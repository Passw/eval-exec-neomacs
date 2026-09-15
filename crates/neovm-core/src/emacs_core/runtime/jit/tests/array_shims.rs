//! `Op::Aref` and `Op::Aset` from compiled code: `neovm_jit_aref` and
//! `neovm_jit_aset` answer the common shapes on a fast path and everything
//! else through the builtin, returning the result's own bits or a
//! `VALUE_SHIM_*` sentinel word. Every case must match the interpreter's
//! opcode arm — result, signal, and what the array looks like afterwards.

use super::*;
use crate::emacs_core::bytecode::Vm;
use crate::emacs_core::eval::Context;
use crate::emacs_core::print::print_value;
use crate::emacs_core::value::LambdaParams;

fn lexical_fn(nargs: u32, ops: Vec<Op>, constants: Vec<Value>) -> ByteCodeFunction {
    let mut f = ByteCodeFunction::new(LambdaParams {
        required: (1..=nargs).map(crate::emacs_core::intern::SymId).collect(),
        optional: Vec::new(),
        rest: None,
    });
    f.lexical = true;
    f.ops = ops;
    f.constants = constants.into();
    f.max_stack = 8;
    f
}

/// `(lambda (a i) (aref a i))`
fn aref_fn() -> ByteCodeFunction {
    lexical_fn(
        2,
        vec![Op::StackRef(1), Op::StackRef(1), Op::Aref, Op::Return],
        vec![],
    )
}

/// `(lambda (a i v) (aset a i v))`
fn aset_fn() -> ByteCodeFunction {
    lexical_fn(
        3,
        vec![
            Op::StackRef(2),
            Op::StackRef(2),
            Op::StackRef(2),
            Op::Aset,
            Op::Return,
        ],
        vec![],
    )
}

fn flow_text(flow: crate::emacs_core::error::Flow) -> String {
    match flow {
        crate::emacs_core::error::Flow::Signal(sig) => format!(
            "signal {} {:?}",
            sig.symbol_name(),
            sig.data.iter().map(print_value).collect::<Vec<_>>()
        ),
        other => format!("{other:?}"),
    }
}

/// What an array looks like: its printed form, and for a string the storage
/// facts a width-changing store would disturb.
fn describe(value: Value) -> String {
    match value.as_lisp_string() {
        Some(s) => format!(
            "{} multibyte={} chars={} bytes={}",
            print_value(&value),
            s.is_multibyte(),
            s.schars(),
            s.sbytes()
        ),
        None => print_value(&value),
    }
}

fn interpret(eval: &mut Context, f: &ByteCodeFunction, args: Vec<Value>) -> String {
    let mut vm = Vm::from_context(eval);
    match vm.execute(f, args) {
        Ok(v) => print_value(&v),
        Err(flow) => flow_text(flow),
    }
}

fn native(ctx_ptr: *mut u8, leaf: &CompiledLeaf, args: &[Value], what: &str) -> String {
    match leaf.call(ctx_ptr, args) {
        NativeRun::Ok(bits) => print_value(&Value::from_bits(bits)),
        NativeRun::Signal => flow_text(take_pending_flow().expect("flow stashed")),
        other => panic!("{what} must not leave native code: {other:?}"),
    }
}

const ARRAYS: &[&str] = &[
    "(vector)",
    "(vector 10)",
    "(vector 10 20 30)",
    "(vector 'x 'y)",
    "(record 'foo 1 2)",
    "(make-bool-vector 5 t)",
    "(make-bool-vector 0 nil)",
    "(make-char-table 'foo 7)",
    "(make-string 3 ?a)",
    "(string-to-multibyte (make-string 3 ?a))",
    "(copy-sequence \"aβc\")",
    "(copy-sequence \"βγδ\")",
    "(string-to-unibyte \"a\\377c\")",
    "(copy-sequence \"\")",
    "(make-hash-table)",
    "(symbol-function 'car)",
    "'sym",
    "nil",
    "(cons 1 2)",
    "42",
];

const INDICES: &[&str] = &[
    "-1",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "97",
    "most-positive-fixnum",
    "most-negative-fixnum",
    "(expt 2 70)",
    "1.0",
    "'x",
    "nil",
];

const VALUES: &[&str] = &[
    "0",
    "65",
    "127",
    "128",
    "255",
    "256",
    "955",
    "#x3fff80",
    "#x3fffff",
    "#x400000",
    "-1",
    "'x",
    "nil",
    "t",
    "(cons 1 2)",
    "1.5",
];

/// Every array shape × index (× value) through the compiled site and the
/// interpreter's opcode arm, each on its own fresh array: the same result or
/// signal, and the same array afterwards. No case may deopt.
#[test]
fn array_sites_match_the_interpreter_natively() {
    let mut eval = Context::new();
    let ctx_ptr = &mut eval as *mut Context as *mut u8;
    let aref = aref_fn();
    let aset = aset_fn();
    let aref_leaf = compile_bytecode_function(&aref).expect("aref compiles");
    let aset_leaf = compile_bytecode_function(&aset).expect("aset compiles");
    let mut checked = 0;
    for array_src in ARRAYS {
        for index_src in INDICES {
            let what = format!("(aref {array_src} {index_src})");
            let fresh = |eval: &mut Context| {
                let pair = eval
                    .eval_str(&format!("(cons {array_src} {index_src})"))
                    .expect("operands");
                vec![pair.cons_car(), pair.cons_cdr()]
            };
            let args = fresh(&mut eval);
            let want = interpret(&mut eval, &aref, args);
            let args = fresh(&mut eval);
            let got = native(ctx_ptr, &aref_leaf, &args, &what);
            assert_eq!(got, want, "{what}");
            checked += 1;

            for value_src in VALUES {
                let what = format!("(aset {array_src} {index_src} {value_src})");
                let fresh = |eval: &mut Context| {
                    let triple = eval
                        .eval_str(&format!("(list {array_src} {index_src} {value_src})"))
                        .expect("operands");
                    let items: Vec<Value> =
                        crate::emacs_core::value::list_to_vec(&triple).expect("list");
                    items
                };
                let args = fresh(&mut eval);
                let array = args[0];
                let want = interpret(&mut eval, &aset, args);
                let want_array = describe(array);
                let args = fresh(&mut eval);
                let array = args[0];
                let got = native(ctx_ptr, &aset_leaf, &args, &what);
                assert_eq!(got, want, "{what}");
                assert_eq!(describe(array), want_array, "{what}: the array afterwards");
                checked += 1;
            }
        }
    }
    assert!(checked > 4000, "checked {checked}");
}

/// The shapes a loop indexes never leave the fast path: a plain vector or
/// record slot, and a character of a unibyte or all-ASCII string.
#[test]
fn indexing_loops_stay_on_the_fast_path() {
    use super::dispatch::ARRAY_SHIM_SLOW_CALLS;
    let mut eval = Context::new();
    let ctx_ptr = &mut eval as *mut Context as *mut u8;
    let aref_leaf = compile_bytecode_function(&aref_fn()).expect("aref compiles");
    let aset_leaf = compile_bytecode_function(&aset_fn()).expect("aset compiles");
    let cases: &[(&str, &str, &str)] = &[
        ("(vector 1 2 3)", "2", "'z"),
        ("(record 'foo 1 2)", "1", "(cons 1 2)"),
        ("(record 'foo 1 2)", "0", "'bar"),
        ("(make-string 4 ?a)", "3", "255"),
        ("(string-to-multibyte (make-string 4 ?a))", "0", "127"),
    ];
    for (array_src, index_src, value_src) in cases {
        let triple = eval
            .eval_str(&format!("(list {array_src} {index_src} {value_src})"))
            .expect("operands");
        let args = crate::emacs_core::value::list_to_vec(&triple).expect("list");
        ARRAY_SHIM_SLOW_CALLS.with(|c| c.set(0));
        let stored = native(ctx_ptr, &aset_leaf, &args, "aset");
        let read = native(ctx_ptr, &aref_leaf, &args[..2], "aref");
        assert_eq!(read, stored, "({array_src}): aref reads what aset stored");
        assert_eq!(
            ARRAY_SHIM_SLOW_CALLS.with(|c| c.get()),
            0,
            "({array_src} {index_src} {value_src}) took the slow path"
        );
    }
    // And a shape the fast path must refuse does reach the builtin.
    let args = [
        eval.eval_str("(make-bool-vector 3 nil)").expect("bv"),
        Value::make_int(1),
        Value::T,
    ];
    ARRAY_SHIM_SLOW_CALLS.with(|c| c.set(0));
    assert_eq!(native(ctx_ptr, &aset_leaf, &args, "bool-vector aset"), "t");
    assert_eq!(
        native(ctx_ptr, &aref_leaf, &args[..2], "bool-vector aref"),
        "t"
    );
    assert_eq!(ARRAY_SHIM_SLOW_CALLS.with(|c| c.get()), 2);
}

/// A redefined `aset` runs, from compiled code, exactly when the interpreter
/// runs it: the shim answers NEED_GENERIC before storing anything.
#[test]
fn a_redefined_aset_runs_from_compiled_code() {
    let mut eval = Context::new();
    let ctx_ptr = &mut eval as *mut Context as *mut u8;
    let aset = aset_fn();
    let aset_leaf = compile_bytecode_function(&aset).expect("aset compiles");
    eval.eval_str(
        "(progn
           (defvar ashim-orig (symbol-function 'aset))
           (defvar ashim-calls 0)
           (fset 'aset (lambda (a i v)
                         (setq ashim-calls (1+ ashim-calls))
                         (funcall ashim-orig a i (list 'wrapped v)))))",
    )
    .expect("redefine");
    let v1 = eval.eval_str("(vector 0 0)").expect("v");
    let want = interpret(
        &mut eval,
        &aset,
        vec![v1, Value::make_int(1), Value::make_int(5)],
    );
    let v2 = eval.eval_str("(vector 0 0)").expect("v");
    let got = native(
        ctx_ptr,
        &aset_leaf,
        &[v2, Value::make_int(1), Value::make_int(5)],
        "redefined aset",
    );
    assert_eq!(got, want);
    assert_eq!(print_value(&v2), print_value(&v1));
    assert_eq!(print_value(&v2), "[0 (wrapped 5)]");
    assert_eq!(
        print_value(&eval.eval_str("ashim-calls").expect("calls")),
        "2"
    );
    // A signal from the redefinition propagates from the general call.
    eval.eval_str("(fset 'aset (lambda (_a _i _v) (signal 'wrong-type-argument '(no-aset))))")
        .expect("redefine");
    let v3 = eval.eval_str("(vector 0 0)").expect("v");
    assert_eq!(
        native(
            ctx_ptr,
            &aset_leaf,
            &[v3, Value::make_int(0), Value::make_int(1)],
            "signalling aset"
        ),
        "signal wrong-type-argument [\"no-aset\"]"
    );
    eval.eval_str("(fset 'aset ashim-orig)").expect("restore");
}

/// Signals from both shims reach a handler in the same body, natively.
///
///     (lambda (a i) (condition-case err (aref a i) (error (list 'caught err))))
///     (lambda (a i) (condition-case err (aset a i 1) (error (list 'caught err))))
#[test]
fn array_signals_are_caught_by_a_leaf_local_handler() {
    let mut eval = Context::new();
    let ctx_ptr = &mut eval as *mut Context as *mut u8;
    for (op, extra) in [(Op::Aref, None), (Op::Aset, Some(Op::Constant(1)))] {
        let mut ops = vec![
            Op::PushConditionCase(0), // patched below
            Op::StackRef(1),
            Op::StackRef(1),
        ];
        if let Some(extra) = extra.clone() {
            ops.push(extra);
        }
        ops.push(op.clone());
        ops.push(Op::PopHandler);
        ops.push(Op::Return);
        let handler = ops.len();
        ops[0] = Op::PushConditionCase(handler as u32);
        ops.extend([Op::Constant(0), Op::StackRef(1), Op::List(2), Op::Return]);
        let f = lexical_fn(2, ops, vec![Value::symbol("caught"), Value::make_int(1)]);
        let leaf = compile_bytecode_function(&f).expect("compiles");
        let v = eval.eval_str("(vector 7 8)").expect("v");
        match leaf.call(ctx_ptr, &[v, Value::make_int(9)]) {
            NativeRun::Ok(bits) => assert_eq!(
                print_value(&Value::from_bits(bits)),
                "(caught (args-out-of-range [7 8] 9))",
                "{op:?}"
            ),
            other => panic!("{op:?}: the handler must catch natively, got {other:?}"),
        }
        match leaf.call(ctx_ptr, &[v, Value::make_int(1)]) {
            NativeRun::Ok(bits) => assert_eq!(
                print_value(&Value::from_bits(bits)),
                if extra.is_some() { "1" } else { "8" },
                "{op:?}"
            ),
            other => panic!("{op:?}: in range must run natively, got {other:?}"),
        }
    }
}

/// Live values below an array site survive what its edges can run: a
/// `signal-hook-function` that collects on the signal edge, and a redefined
/// `aset` that collects on the general-call edge.
///
///     (lambda (a i) (let ((h (cons 1 2))) (condition-case nil (OP a i ...) (error h))))
#[test]
fn array_site_edges_keep_the_residual_alive() {
    let mut eval = Context::new();
    let ctx_ptr = &mut eval as *mut Context as *mut u8;
    for op in [Op::Aref, Op::Aset] {
        let is_aset = matches!(op, Op::Aset);
        let mut ops = vec![
            Op::Constant(0),          // [a i 1]
            Op::Constant(1),          // [a i 1 2]
            Op::Cons,                 // [a i h]
            Op::PushConditionCase(0), // patched
            Op::StackRef(2),          // [a i h a]
            Op::StackRef(2),          // [a i h a i]
        ];
        if is_aset {
            ops.push(Op::Constant(0)); // [a i h a i 1]
        }
        ops.push(op.clone()); // residual [a i h]
        ops.push(Op::Pop); // [a i h]
        ops.push(Op::PopHandler);
        ops.push(Op::Return); // h
        let handler = ops.len();
        ops[3] = Op::PushConditionCase(handler as u32);
        ops.extend([Op::Pop, Op::Return]); // [a i h err] -> h
        let f = lexical_fn(2, ops, vec![Value::make_int(1), Value::make_int(2)]);
        let leaf = compile_bytecode_function(&f).expect("compiles");
        eval.eval_str(
            "(setq signal-hook-function
                   (lambda (_sym _data) (garbage-collect) (make-list 4096 (cons 0 0)) nil))",
        )
        .expect("hook");
        let check = |bits: usize, what: &str| {
            let h = Value::from_bits(bits);
            assert!(h.is_cons(), "{what}: h survived (got {h:?})");
            assert_eq!(h.cons_car(), Value::make_int(1), "{what}: car intact");
            assert_eq!(h.cons_cdr(), Value::make_int(2), "{what}: cdr intact");
        };
        for _ in 0..3 {
            let v = eval.eval_str("(vector 0 0)").expect("v");
            match leaf.call(ctx_ptr, &[v, Value::make_int(5)]) {
                NativeRun::Ok(bits) => check(bits, &format!("{op:?} signal edge")),
                other => panic!("{op:?}: must catch natively, got {other:?}"),
            }
        }
        eval.eval_str("(setq signal-hook-function nil)")
            .expect("unhook");
        if is_aset {
            eval.eval_str(
                "(progn
                   (defvar ashim-orig2 (symbol-function 'aset))
                   (fset 'aset (lambda (a i v)
                                 (garbage-collect)
                                 (make-list 4096 (cons 0 0))
                                 (funcall ashim-orig2 a i v))))",
            )
            .expect("redefine");
            for _ in 0..3 {
                let v = eval.eval_str("(vector 0 0)").expect("v");
                match leaf.call(ctx_ptr, &[v, Value::make_int(1)]) {
                    NativeRun::Ok(bits) => check(bits, "Aset general-call edge"),
                    other => panic!("general call must run natively, got {other:?}"),
                }
                assert_eq!(print_value(&v), "[0 1]");
            }
            eval.eval_str("(fset 'aset ashim-orig2)").expect("restore");
        }
    }
}
