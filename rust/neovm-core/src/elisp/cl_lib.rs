//! CL-lib, seq.el, and JSON built-in functions.
//!
//! Provides Common Lisp compatibility functions, sequence operations,
//! and JSON parsing/serialization for the Elisp interpreter.

use super::error::{signal, EvalResult, Flow};
use super::intern::{intern, resolve_sym};
use super::value::*;
#[cfg(test)]
use std::sync::atomic::{AtomicU64, Ordering};

// ---------------------------------------------------------------------------
// Argument helpers (local copies for this module)
// ---------------------------------------------------------------------------

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}
#[cfg(test)]

fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

#[cfg(test)]
static CL_GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

fn expect_int(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn expect_number_or_marker(val: &Value) -> Result<f64, Flow> {
    match val {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Char(c) => Ok(*c as i64 as f64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

/// Collect elements from any sequence type into a Vec.
fn collect_sequence(val: &Value) -> Vec<Value> {
    match val {
        Value::Nil => Vec::new(),
        Value::Cons(_) => list_to_vec(val).unwrap_or_default(),
        Value::Vector(v) => with_heap(|h| h.get_vector(*v).clone()),
        Value::Str(s) => with_heap(|h| h.get_string(*s).chars().map(Value::Char).collect()),
        _ => vec![val.clone()],
    }
}
#[cfg(test)]

fn cl_list_nth(list: &Value, index: usize) -> EvalResult {
    let mut cursor = list.clone();
    for _ in 0..index {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = read_cons(cell);
                cursor = pair.cdr.clone();
            }
            tail => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), tail],
                ))
            }
        }
    }

    match cursor {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => Ok(with_heap(|h| h.cons_car(cell))),
        tail => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), tail],
        )),
    }
}

/// `(cl-first LIST)` -- return the first element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_first(args: Vec<Value>) -> EvalResult {
    expect_args("cl-first", &args, 1)?;
    cl_list_nth(&args[0], 0)
}

/// `(cl-second LIST)` -- return the second element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_second(args: Vec<Value>) -> EvalResult {
    expect_args("cl-second", &args, 1)?;
    cl_list_nth(&args[0], 1)
}

/// `(cl-third LIST)` -- return the third element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_third(args: Vec<Value>) -> EvalResult {
    expect_args("cl-third", &args, 1)?;
    cl_list_nth(&args[0], 2)
}

/// `(cl-fourth LIST)` -- return the fourth element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_fourth(args: Vec<Value>) -> EvalResult {
    expect_args("cl-fourth", &args, 1)?;
    cl_list_nth(&args[0], 3)
}

/// `(cl-fifth LIST)` -- return the fifth element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_fifth(args: Vec<Value>) -> EvalResult {
    expect_args("cl-fifth", &args, 1)?;
    cl_list_nth(&args[0], 4)
}

/// `(cl-sixth LIST)` -- return the sixth element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_sixth(args: Vec<Value>) -> EvalResult {
    expect_args("cl-sixth", &args, 1)?;
    cl_list_nth(&args[0], 5)
}

/// `(cl-seventh LIST)` -- return the seventh element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_seventh(args: Vec<Value>) -> EvalResult {
    expect_args("cl-seventh", &args, 1)?;
    cl_list_nth(&args[0], 6)
}

/// `(cl-eighth LIST)` -- return the eighth element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_eighth(args: Vec<Value>) -> EvalResult {
    expect_args("cl-eighth", &args, 1)?;
    cl_list_nth(&args[0], 7)
}

/// `(cl-ninth LIST)` -- return the ninth element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_ninth(args: Vec<Value>) -> EvalResult {
    expect_args("cl-ninth", &args, 1)?;
    cl_list_nth(&args[0], 8)
}

/// `(cl-tenth LIST)` -- return the tenth element of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_tenth(args: Vec<Value>) -> EvalResult {
    expect_args("cl-tenth", &args, 1)?;
    cl_list_nth(&args[0], 9)
}

/// `(cl-rest LIST)` -- return the tail (cdr) of LIST.
#[cfg(test)]
pub(crate) fn builtin_cl_rest(args: Vec<Value>) -> EvalResult {
    expect_args("cl-rest", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => Ok(with_heap(|h| h.cons_cdr(*cell))),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), other.clone()],
        )),
    }
}

/// `(cl-evenp N)` -- return t if N is even.
#[cfg(test)]
pub(crate) fn builtin_cl_evenp(args: Vec<Value>) -> EvalResult {
    expect_args("cl-evenp", &args, 1)?;
    let n = expect_int(&args[0])?;
    Ok(Value::bool(n % 2 == 0))
}

/// `(cl-oddp N)` -- return t if N is odd.
#[cfg(test)]
pub(crate) fn builtin_cl_oddp(args: Vec<Value>) -> EvalResult {
    expect_args("cl-oddp", &args, 1)?;
    let n = expect_int(&args[0])?;
    Ok(Value::bool(n % 2 != 0))
}

/// `(cl-plusp N)` -- return t if N is strictly positive.
#[cfg(test)]
pub(crate) fn builtin_cl_plusp(args: Vec<Value>) -> EvalResult {
    expect_args("cl-plusp", &args, 1)?;
    let n = expect_number_or_marker(&args[0])?;
    Ok(Value::bool(n > 0.0))
}

/// `(cl-minusp N)` -- return t if N is strictly negative.
#[cfg(test)]
pub(crate) fn builtin_cl_minusp(args: Vec<Value>) -> EvalResult {
    expect_args("cl-minusp", &args, 1)?;
    let n = expect_number_or_marker(&args[0])?;
    Ok(Value::bool(n < 0.0))
}

/// `(cl-subseq SEQ START &optional END)` -- CL alias for `seq-subseq`.
#[cfg(test)]
pub(crate) fn builtin_cl_subseq(args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-subseq", &args, 2)?;
    expect_max_args("cl-subseq", &args, 3)?;
    builtin_seq_subseq(args)
}

/// `(cl-concatenate TYPE &rest SEQS)` -- CL alias for `seq-concatenate`.
#[cfg(test)]
pub(crate) fn builtin_cl_concatenate(args: Vec<Value>) -> EvalResult {
    builtin_seq_concatenate(args)
}

/// `(cl-member ITEM LIST)` -- CL alias for `member`.
#[cfg(test)]
pub(crate) fn builtin_cl_member(args: Vec<Value>) -> EvalResult {
    super::builtins::builtin_member(args)
}

/// `(cl-coerce OBJECT TYPE)` -- coerce a sequence to TYPE.
#[cfg(test)]
pub(crate) fn builtin_cl_coerce(args: Vec<Value>) -> EvalResult {
    expect_args("cl-coerce", &args, 2)?;
    builtin_seq_concatenate(vec![args[1].clone(), args[0].clone()])
}

/// `(cl-adjoin ITEM LIST)` -- add ITEM to LIST if not already present.
#[cfg(test)]
pub(crate) fn builtin_cl_adjoin(args: Vec<Value>) -> EvalResult {
    expect_args("cl-adjoin", &args, 2)?;
    let item = args[0].clone();
    let list = args[1].clone();
    let found = super::builtins::builtin_member(vec![item.clone(), list.clone()])?;
    if found.is_truthy() {
        Ok(list)
    } else {
        Ok(Value::cons(item, list))
    }
}

/// `(cl-remove ITEM LIST)` -- CL alias for `remove`.
#[cfg(test)]
pub(crate) fn builtin_cl_remove(args: Vec<Value>) -> EvalResult {
    super::builtins_extra::builtin_remove(args)
}

/// `(cl-remove-duplicates SEQ)` -- CL alias for `seq-uniq`.
#[cfg(test)]
pub(crate) fn builtin_cl_remove_duplicates(args: Vec<Value>) -> EvalResult {
    super::builtins_extra::builtin_seq_uniq(args)
}

fn seq_position_list_elements(seq: &Value) -> Result<Vec<Value>, Flow> {
    let mut elements = Vec::new();
    let mut cursor = seq.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(elements),
            Value::Cons(cell) => {
                let pair = read_cons(cell);
                elements.push(pair.car.clone());
                cursor = pair.cdr.clone();
            }
            tail => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), tail],
                ));
            }
        }
    }
}

fn seq_position_elements(seq: &Value) -> Result<Vec<Value>, Flow> {
    match seq {
        Value::Nil => Ok(Vec::new()),
        Value::Cons(_) => seq_position_list_elements(seq),
        Value::Vector(v) => Ok(with_heap(|h| h.get_vector(*v).clone())),
        Value::Str(s) => Ok(with_heap(|h| h.get_string(*s).chars().map(|ch| Value::Int(ch as i64)).collect())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

fn seq_default_match(left: &Value, right: &Value) -> bool {
    if equal_value(left, right, 0) {
        return true;
    }
    match (left, right) {
        (Value::Char(a), Value::Int(b)) => (*a as i64) == *b,
        (Value::Int(a), Value::Char(b)) => *a == (*b as i64),
        _ => false,
    }
}

fn seq_collect_concat_arg(arg: &Value) -> Result<Vec<Value>, Flow> {
    match arg {
        Value::Nil => Ok(Vec::new()),
        Value::Cons(_) => {
            let mut out = Vec::new();
            let mut cursor = arg.clone();
            loop {
                match cursor {
                    Value::Nil => return Ok(out),
                    Value::Cons(cell) => {
                        let pair = read_cons(cell);
                        out.push(pair.car.clone());
                        cursor = pair.cdr.clone();
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ));
                    }
                }
            }
        }
        Value::Vector(v) => Ok(with_heap(|h| h.get_vector(*v).clone())),
        Value::Str(s) => Ok(with_heap(|h| h.get_string(*s).chars().map(|ch| Value::Int(ch as i64)).collect())),
        other => Err(signal(
            "error",
            vec![Value::string(format!(
                "Cannot convert {} into a sequence",
                super::print::print_value(other)
            ))],
        )),
    }
}

// ===========================================================================
// Seq.el pure operations
// ===========================================================================

/// `(seq-reverse SEQ)` — reverse a sequence.
pub(crate) fn builtin_seq_reverse(args: Vec<Value>) -> EvalResult {
    expect_args("seq-reverse", &args, 1)?;
    let mut elems = seq_position_elements(&args[0])?;
    elems.reverse();
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(elems)),
        Value::Str(_) => {
            let mut s = String::new();
            for value in &elems {
                let ch = match value {
                    Value::Char(c) => *c,
                    Value::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
                        signal(
                            "wrong-type-argument",
                            vec![Value::symbol("characterp"), value.clone()],
                        )
                    })?,
                    other => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("characterp"), other.clone()],
                        ));
                    }
                };
                s.push(ch);
            }
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(elems)),
    }
}

/// `(seq-drop SEQ N)` — drop first n elements.
pub(crate) fn builtin_seq_drop(args: Vec<Value>) -> EvalResult {
    expect_args("seq-drop", &args, 2)?;
    let n = expect_int(&args[1])?;

    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Vector(v) => {
            let elems = with_heap(|h| h.get_vector(*v).clone());
            if n <= 0 {
                return Ok(Value::vector(elems.clone()));
            }
            let n = (n as usize).min(elems.len());
            Ok(Value::vector(elems[n..].to_vec()))
        }
        Value::Str(s) => {
            let string = with_heap(|h| h.get_string(*s).clone());
            let chars: Vec<char> = string.chars().collect();
            if n <= 0 {
                return Ok(Value::string(string));
            }
            let n = (n as usize).min(chars.len());
            let out: String = chars[n..].iter().collect();
            Ok(Value::string(out))
        }
        Value::Cons(_) => {
            if n <= 0 {
                return Ok(args[0].clone());
            }
            let mut cursor = args[0].clone();
            let mut remaining = n as usize;
            while remaining > 0 {
                match cursor {
                    Value::Nil => return Ok(Value::Nil),
                    Value::Cons(cell) => {
                        let pair = read_cons(cell);
                        cursor = pair.cdr.clone();
                        remaining -= 1;
                    }
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), args[0].clone()],
                        ));
                    }
                }
            }
            Ok(cursor)
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

/// `(seq-take SEQ N)` — take first n elements.
pub(crate) fn builtin_seq_take(args: Vec<Value>) -> EvalResult {
    expect_args("seq-take", &args, 2)?;
    let n = expect_int(&args[1])?;

    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Vector(v) => {
            let elems = with_heap(|h| h.get_vector(*v).clone());
            if n <= 0 {
                return Ok(Value::vector(Vec::new()));
            }
            let n = (n as usize).min(elems.len());
            Ok(Value::vector(elems[..n].to_vec()))
        }
        Value::Str(s) => {
            let string = with_heap(|h| h.get_string(*s).clone());
            let chars: Vec<char> = string.chars().collect();
            if n <= 0 {
                return Ok(Value::string(""));
            }
            let n = (n as usize).min(chars.len());
            let out: String = chars[..n].iter().collect();
            Ok(Value::string(out))
        }
        Value::Cons(_) => {
            if n <= 0 {
                return Ok(Value::Nil);
            }
            let mut out = Vec::new();
            let mut cursor = args[0].clone();
            let mut remaining = n as usize;
            while remaining > 0 {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let pair = read_cons(cell);
                        out.push(pair.car.clone());
                        cursor = pair.cdr.clone();
                        remaining -= 1;
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ));
                    }
                }
            }
            Ok(Value::list(out))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

fn builtin_seq_subseq_legacy(args: &[Value]) -> EvalResult {
    let elems = collect_sequence(&args[0]);
    let start = expect_int(&args[1])? as usize;
    let end = if args.len() > 2 && !args[2].is_nil() {
        expect_int(&args[2])? as usize
    } else {
        elems.len()
    };
    let start = start.min(elems.len());
    let end = end.min(elems.len());
    if start > end {
        return Ok(Value::Nil);
    }
    let result: Vec<Value> = elems[start..end].to_vec();
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(result)),
        _ => Ok(Value::list(result)),
    }
}

/// `(seq-subseq SEQ START &optional END)` — subsequence.
pub(crate) fn builtin_seq_subseq(args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-subseq", &args, 2)?;
    let start = expect_int(&args[1])?;
    let end = if args.len() > 2 && !args[2].is_nil() {
        Some(expect_int(&args[2])?)
    } else {
        None
    };

    // Preserve existing behavior for negative indices until full seq.el
    // index normalization support lands.
    if start < 0 || end.is_some_and(|v| v < 0) {
        return builtin_seq_subseq_legacy(&args);
    }

    match &args[0] {
        Value::Nil | Value::Cons(_) | Value::Vector(_) | Value::Str(_) => {
            let dropped = builtin_seq_drop(vec![args[0].clone(), Value::Int(start)])?;
            if let Some(end_idx) = end {
                let span = end_idx - start;
                builtin_seq_take(vec![dropped, Value::Int(span)])
            } else {
                Ok(dropped)
            }
        }
        other => Err(signal(
            "error",
            vec![Value::string(format!(
                "Unsupported sequence: {}",
                super::print::print_value(other)
            ))],
        )),
    }
}

/// `(seq-concatenate TYPE &rest SEQS)` — concatenate sequences into target type.
pub(crate) fn builtin_seq_concatenate(args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-concatenate", &args, 1)?;
    let target = match &args[0] {
        Value::Symbol(id) => resolve_sym(*id),
        other => {
            return Err(signal(
                "error",
                vec![Value::string(format!(
                    "Not a sequence type name: {}",
                    super::print::print_value(other)
                ))],
            ));
        }
    };
    if target != "list" && target != "vector" && target != "string" {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Not a sequence type name: {}",
                target
            ))],
        ));
    }

    let mut combined = Vec::new();
    for arg in &args[1..] {
        combined.extend(seq_collect_concat_arg(arg)?);
    }
    match target {
        "list" => Ok(Value::list(combined)),
        "vector" => Ok(Value::vector(combined)),
        "string" => {
            let mut s = String::new();
            for value in &combined {
                let ch = match value {
                    Value::Char(c) => *c,
                    Value::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
                        signal(
                            "wrong-type-argument",
                            vec![Value::symbol("characterp"), value.clone()],
                        )
                    })?,
                    other => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("characterp"), other.clone()],
                        ));
                    }
                };
                s.push(ch);
            }
            Ok(Value::string(s))
        }
        _ => unreachable!(),
    }
}

/// `(seq-empty-p SEQ)` — is sequence empty?
pub(crate) fn builtin_seq_empty_p(args: Vec<Value>) -> EvalResult {
    expect_args("seq-empty-p", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::True),
        Value::Cons(_) => Ok(Value::Nil),
        Value::Str(s) => Ok(Value::bool(with_heap(|h| h.get_string(*s).is_empty()))),
        Value::Vector(v) => Ok(Value::bool(with_heap(|h| h.vector_len(*v)) == 0)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

/// `(seq-min SEQ)` — minimum element (numeric).
pub(crate) fn builtin_seq_min(args: Vec<Value>) -> EvalResult {
    expect_args("seq-min", &args, 1)?;
    let elems = seq_position_elements(&args[0])?;
    if elems.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::Subr(intern("min")), Value::Int(0)],
        ));
    }
    let mut min_val = &elems[0];
    let mut min_num = expect_number_or_marker(min_val)?;
    for e in &elems[1..] {
        let b = expect_number_or_marker(e)?;
        if b < min_num {
            min_num = b;
            min_val = e;
        }
    }
    Ok(min_val.clone())
}

/// `(seq-max SEQ)` — maximum element (numeric).
pub(crate) fn builtin_seq_max(args: Vec<Value>) -> EvalResult {
    expect_args("seq-max", &args, 1)?;
    let elems = seq_position_elements(&args[0])?;
    if elems.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::Subr(intern("max")), Value::Int(0)],
        ));
    }
    let mut max_val = &elems[0];
    let mut max_num = expect_number_or_marker(max_val)?;
    for e in &elems[1..] {
        let b = expect_number_or_marker(e)?;
        if b > max_num {
            max_num = b;
            max_val = e;
        }
    }
    Ok(max_val.clone())
}

// ===========================================================================
// Seq.el eval-dependent operations
// ===========================================================================

/// `(seq-position SEQ ELT &optional TESTFN)` — return first matching index.
pub(crate) fn builtin_seq_position(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("seq-position", &args, 2)?;
    let seq = &args[0];
    let target = args[1].clone();
    let test_fn = if args.len() > 2 && !args[2].is_nil() {
        Some(args[2].clone())
    } else {
        None
    };
    let elements = seq_position_elements(seq)?;

    for (idx, element) in elements.into_iter().enumerate() {
        let matches = if let Some(test) = &test_fn {
            eval.apply(test.clone(), vec![element.clone(), target.clone()])?
                .is_truthy()
        } else {
            seq_default_match(&element, &target)
        };
        if matches {
            return Ok(Value::Int(idx as i64));
        }
    }
    Ok(Value::Nil)
}

/// `(cl-position ITEM SEQ &optional TESTFN)` -- CL argument order wrapper.
#[cfg(test)]
pub(crate) fn builtin_cl_position(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("cl-position", &args, 2)?;
    expect_max_args("cl-position", &args, 3)?;

    let mut forwarded = Vec::with_capacity(args.len());
    forwarded.push(args[1].clone());
    forwarded.push(args[0].clone());
    if args.len() == 3 {
        forwarded.push(args[2].clone());
    }

    builtin_seq_position(eval, forwarded)
}

/// `(cl-reduce FUNCTION SEQ &optional INITIAL-VALUE)` -- CL alias for `seq-reduce`.
#[cfg(test)]
pub(crate) fn builtin_cl_reduce(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    builtin_seq_reduce(eval, args)
}

/// `(cl-count PREDICATE SEQ)` -- CL alias for `seq-count`.
#[cfg(test)]
pub(crate) fn builtin_cl_count(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    builtin_seq_count(eval, args)
}

/// `(cl-count-if PREDICATE SEQ)` -- CL alias for `seq-count`.
#[cfg(test)]
pub(crate) fn builtin_cl_count_if(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    builtin_seq_count(eval, args)
}

/// `(cl-some PREDICATE SEQ)` -- CL alias for `seq-some`.
#[cfg(test)]
pub(crate) fn builtin_cl_some(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    builtin_seq_some(eval, args)
}

/// `(cl-every PREDICATE SEQ)` -- CL alias for `seq-every-p`.
#[cfg(test)]
pub(crate) fn builtin_cl_every(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    builtin_seq_every_p(eval, args)
}

/// `(cl-notany PREDICATE SEQ)` -- true when no element satisfies PREDICATE.
#[cfg(test)]
pub(crate) fn builtin_cl_notany(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    let found = builtin_seq_some(eval, args)?;
    Ok(Value::bool(found.is_nil()))
}

/// `(cl-notevery PREDICATE SEQ)` -- true when not all elements satisfy PREDICATE.
#[cfg(test)]
pub(crate) fn builtin_cl_notevery(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let every = builtin_seq_every_p(eval, args)?;
    Ok(Value::bool(!every.is_truthy()))
}

/// `(cl-gensym &optional PREFIX)` -- generate an uninterned-style symbol name.
#[cfg(test)]
pub(crate) fn builtin_cl_gensym(args: Vec<Value>) -> EvalResult {
    expect_max_args("cl-gensym", &args, 1)?;
    let prefix = match args.first() {
        None => "G".to_string(),
        Some(Value::Nil) => "G".to_string(),
        Some(Value::Str(s)) => with_heap(|h| h.get_string(*s).clone()),
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    let n = CL_GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(Value::symbol(format!("{prefix}{n}")))
}

/// `(cl-find ITEM SEQ)` -- return first element in SEQ equal to ITEM.
#[cfg(test)]
pub(crate) fn builtin_cl_find(args: Vec<Value>) -> EvalResult {
    expect_args("cl-find", &args, 2)?;
    let target = &args[0];
    let elements = seq_position_elements(&args[1])?;
    for element in elements {
        if equal_value(&element, target, 0) {
            return Ok(element);
        }
    }
    Ok(Value::Nil)
}

/// `(cl-find-if PREDICATE SEQ)` -- return first element satisfying PREDICATE.
#[cfg(test)]
pub(crate) fn builtin_cl_find_if(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("cl-find-if", &args, 2)?;
    let pred = args[0].clone();
    let elements = seq_position_elements(&args[1])?;
    for element in elements {
        let matched = eval.apply(pred.clone(), vec![element.clone()])?;
        if matched.is_truthy() {
            return Ok(element);
        }
    }
    Ok(Value::Nil)
}

/// `(cl-subsetp LIST1 LIST2)` -- return t if every element of LIST1 appears in LIST2.
#[cfg(test)]
pub(crate) fn builtin_cl_subsetp(args: Vec<Value>) -> EvalResult {
    expect_args("cl-subsetp", &args, 2)?;
    let left = seq_position_elements(&args[0])?;
    let right = seq_position_elements(&args[1])?;

    for item in left {
        if !right
            .iter()
            .any(|candidate| equal_value(&item, candidate, 0))
        {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::True)
}

/// `(cl-intersection LIST1 LIST2)` -- set-style intersection preserving LIST1 order.
#[cfg(test)]
pub(crate) fn builtin_cl_intersection(args: Vec<Value>) -> EvalResult {
    expect_args("cl-intersection", &args, 2)?;
    let left = seq_position_elements(&args[0])?;
    let right = seq_position_elements(&args[1])?;

    let mut out = Vec::new();
    for item in left {
        let in_right = right
            .iter()
            .any(|candidate| equal_value(&item, candidate, 0));
        let already_in_out = out.iter().any(|seen| equal_value(&item, seen, 0));
        if in_right && !already_in_out {
            out.push(item);
        }
    }
    Ok(Value::list(out))
}

/// `(cl-set-difference LIST1 LIST2)` -- set-style difference preserving LIST1 order.
#[cfg(test)]
pub(crate) fn builtin_cl_set_difference(args: Vec<Value>) -> EvalResult {
    expect_args("cl-set-difference", &args, 2)?;
    let left = seq_position_elements(&args[0])?;
    let right = seq_position_elements(&args[1])?;

    let mut out = Vec::new();
    for item in left {
        let in_right = right
            .iter()
            .any(|candidate| equal_value(&item, candidate, 0));
        let already_in_out = out.iter().any(|seen| equal_value(&item, seen, 0));
        if !in_right && !already_in_out {
            out.push(item);
        }
    }
    Ok(Value::list(out))
}

/// `(cl-union LIST1 LIST2)` -- set-style union preserving left-to-right discovery order.
#[cfg(test)]
pub(crate) fn builtin_cl_union(args: Vec<Value>) -> EvalResult {
    expect_args("cl-union", &args, 2)?;
    let left = seq_position_elements(&args[0])?;
    let right = seq_position_elements(&args[1])?;

    let mut out = Vec::new();
    for item in left.into_iter().chain(right.into_iter()) {
        let already_in_out = out.iter().any(|seen| equal_value(&item, seen, 0));
        if !already_in_out {
            out.push(item);
        }
    }
    Ok(Value::list(out))
}

/// `(cl-substitute NEW OLD SEQ)` -- replace OLD with NEW across SEQ.
#[cfg(test)]
pub(crate) fn builtin_cl_substitute(args: Vec<Value>) -> EvalResult {
    expect_args("cl-substitute", &args, 3)?;
    let new_value = args[0].clone();
    let old_value = &args[1];
    let elements = seq_position_elements(&args[2])?;

    let replaced = elements
        .into_iter()
        .map(|item| {
            if equal_value(&item, old_value, 0) {
                new_value.clone()
            } else {
                item
            }
        })
        .collect::<Vec<_>>();
    Ok(Value::list(replaced))
}

/// `(cl-sort SEQ PREDICATE)` -- CL alias for `sort`.
#[cfg(test)]
pub(crate) fn builtin_cl_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    super::builtins::builtin_sort(eval, args)
}

/// `(cl-stable-sort SEQ PREDICATE)` -- CL alias for stable `sort`.
#[cfg(test)]
pub(crate) fn builtin_cl_stable_sort(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    super::builtins::builtin_sort(eval, args)
}

/// `(cl-remove-if PREDICATE SEQ)` -- remove elements satisfying PREDICATE.
#[cfg(test)]
pub(crate) fn builtin_cl_remove_if(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("cl-remove-if", &args, 2)?;
    let pred = args[0].clone();
    let elements = seq_position_elements(&args[1])?;
    let mut out = Vec::new();

    for element in elements {
        let matched = eval.apply(pred.clone(), vec![element.clone()])?;
        if !matched.is_truthy() {
            out.push(element);
        }
    }
    Ok(Value::list(out))
}

/// `(cl-remove-if-not PREDICATE SEQ)` -- keep elements satisfying PREDICATE.
#[cfg(test)]
pub(crate) fn builtin_cl_remove_if_not(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("cl-remove-if-not", &args, 2)?;
    let pred = args[0].clone();
    let elements = seq_position_elements(&args[1])?;
    let mut out = Vec::new();

    for element in elements {
        let matched = eval.apply(pred.clone(), vec![element.clone()])?;
        if matched.is_truthy() {
            out.push(element);
        }
    }
    Ok(Value::list(out))
}

/// `(cl-map RESULT-TYPE FUNCTION SEQ...)` -- CL map with explicit result type.
#[cfg(test)]
pub(crate) fn builtin_cl_map(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-map", &args, 3)?;
    let result_type = args[0].clone();
    let func = args[1].clone();
    let seqs = args[2..].to_vec();

    let mut forwarded = Vec::with_capacity(1 + seqs.len());
    forwarded.push(func);
    forwarded.extend(seqs);
    let mapped = builtin_seq_mapn(eval, forwarded)?;

    match result_type {
        Value::Symbol(id) if resolve_sym(id) == "list" => Ok(mapped),
        Value::Symbol(id) if resolve_sym(id) == "vector" => {
            let items = list_to_vec(&mapped).ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), mapped.clone()],
                )
            })?;
            Ok(Value::vector(items))
        }
        Value::Symbol(id) if resolve_sym(id) == "string" => {
            let items = list_to_vec(&mapped).ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), mapped.clone()],
                )
            })?;
            let mut out = String::new();
            for item in items {
                let ch =
                    match item {
                        Value::Char(c) => c,
                        Value::Int(n) => u32::try_from(n)
                            .ok()
                            .and_then(char::from_u32)
                            .ok_or_else(|| {
                                signal(
                                    "wrong-type-argument",
                                    vec![Value::symbol("characterp"), Value::Int(n)],
                                )
                            })?,
                        other => {
                            return Err(signal(
                                "wrong-type-argument",
                                vec![Value::symbol("characterp"), other],
                            ))
                        }
                    };
                out.push(ch);
            }
            Ok(Value::string(out))
        }
        other => Err(signal(
            "error",
            vec![Value::string(format!(
                "Unsupported cl-map result type: {}",
                super::print::print_value(&other)
            ))],
        )),
    }
}

/// `(seq-contains-p SEQ ELT &optional TESTFN)` — membership test for sequence.
pub(crate) fn builtin_seq_contains_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if !(2..=3).contains(&args.len()) {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("seq-contains-p"),
                Value::Int(args.len() as i64),
            ],
        ));
    }
    let seq = &args[0];
    let target = args[1].clone();
    let test_fn = if args.len() == 3 && !args[2].is_nil() {
        Some(args[2].clone())
    } else {
        None
    };
    let elements = seq_position_elements(seq)?;

    for element in elements {
        let matches = if let Some(test) = &test_fn {
            eval.apply(test.clone(), vec![element.clone(), target.clone()])?
                .is_truthy()
        } else {
            seq_default_match(&element, &target)
        };
        if matches {
            return Ok(Value::True);
        }
    }
    Ok(Value::Nil)
}

/// `(seq-mapn FN &rest SEQS)` — map over multiple sequences.
pub(crate) fn builtin_seq_mapn(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-mapn", &args, 2)?;
    let func = args[0].clone();
    let seqs: Vec<Vec<Value>> = args[1..].iter().map(|s| collect_sequence(s)).collect();
    if seqs.is_empty() {
        return Ok(Value::Nil);
    }
    let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
    let saved = eval.save_temp_roots();
    let mut results = Vec::new();
    for i in 0..min_len {
        let call_args: Vec<Value> = seqs.iter().map(|s| s[i].clone()).collect();
        match eval.apply(func.clone(), call_args) {
            Ok(val) => {
                eval.push_temp_root(val.clone());
                results.push(val);
            }
            Err(e) => {
                eval.restore_temp_roots(saved);
                return Err(e);
            }
        }
    }
    eval.restore_temp_roots(saved);
    Ok(Value::list(results))
}

/// `(seq-do FN SEQ)` — apply fn for side effects, return nil.
pub(crate) fn builtin_seq_do(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-do", &args, 2)?;
    let func = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        eval.apply(func.clone(), vec![e])?;
    }
    Ok(Value::Nil)
}

/// `(seq-count PRED SEQ)` — count elements matching predicate.
pub(crate) fn builtin_seq_count(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-count", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut count = 0i64;
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_truthy() {
            count += 1;
        }
    }
    Ok(Value::Int(count))
}

/// `(seq-reduce FN SEQ INITIAL)` — reduce with initial value.
pub(crate) fn builtin_seq_reduce(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("seq-reduce", &args, 3)?;
    let func = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut acc = args[2].clone();
    for e in elems {
        acc = eval.apply(func.clone(), vec![acc, e])?;
    }
    Ok(acc)
}

/// `(seq-some PRED SEQ)` — some element matches predicate.
pub(crate) fn builtin_seq_some(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-some", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_truthy() {
            return Ok(r);
        }
    }
    Ok(Value::Nil)
}

/// `(seq-every-p PRED SEQ)` — all elements match predicate.
pub(crate) fn builtin_seq_every_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("seq-every-p", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_nil() {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::True)
}

/// `(seq-sort PRED SEQ)` — sort with predicate.
pub(crate) fn builtin_seq_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-sort", &args, 2)?;
    let pred = args[0].clone();
    let mut items = collect_sequence(&args[1]);

    // Insertion sort (stable, supports fallible predicates)
    for i in 1..items.len() {
        let mut j = i;
        while j > 0 {
            let result = eval.apply(pred.clone(), vec![items[j].clone(), items[j - 1].clone()])?;
            if result.is_truthy() {
                items.swap(j, j - 1);
                j -= 1;
            } else {
                break;
            }
        }
    }
    Ok(Value::list(items))
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // --- Seq.el pure operations ---

    #[test]
    fn seq_reverse_list() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_seq_reverse(vec![list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items[0].as_int(), Some(3));
        assert_eq!(items[2].as_int(), Some(1));
    }

    #[test]
    fn seq_reverse_string() {
        let s = Value::string("abc");
        let result = builtin_seq_reverse(vec![s]).unwrap();
        assert_eq!(result.as_str(), Some("cba"));
    }

    #[test]
    fn cl_first_list() {
        let list = Value::list(vec![Value::symbol("a"), Value::symbol("b")]);
        let result = builtin_cl_first(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "a"));
    }

    #[test]
    fn cl_first_nil() {
        let result = builtin_cl_first(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_first_wrong_type() {
        assert!(builtin_cl_first(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_second_list() {
        let list = Value::list(vec![Value::symbol("a"), Value::symbol("b")]);
        let result = builtin_cl_second(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "b"));
    }

    #[test]
    fn cl_second_nil() {
        let result = builtin_cl_second(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_second_wrong_type() {
        assert!(builtin_cl_second(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_third_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
        ]);
        let result = builtin_cl_third(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "c"));
    }

    #[test]
    fn cl_third_nil() {
        let result = builtin_cl_third(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_third_wrong_type() {
        assert!(builtin_cl_third(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_fourth_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
        ]);
        let result = builtin_cl_fourth(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "d"));
    }

    #[test]
    fn cl_fourth_nil() {
        let result = builtin_cl_fourth(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_fourth_wrong_type() {
        assert!(builtin_cl_fourth(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_fifth_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
            Value::symbol("e"),
        ]);
        let result = builtin_cl_fifth(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "e"));
    }

    #[test]
    fn cl_fifth_nil() {
        let result = builtin_cl_fifth(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_fifth_wrong_type() {
        assert!(builtin_cl_fifth(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_sixth_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
            Value::symbol("e"),
            Value::symbol("f"),
        ]);
        let result = builtin_cl_sixth(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "f"));
    }

    #[test]
    fn cl_sixth_nil() {
        let result = builtin_cl_sixth(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_sixth_wrong_type() {
        assert!(builtin_cl_sixth(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_seventh_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
            Value::symbol("e"),
            Value::symbol("f"),
            Value::symbol("g"),
        ]);
        let result = builtin_cl_seventh(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "g"));
    }

    #[test]
    fn cl_seventh_nil() {
        let result = builtin_cl_seventh(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_seventh_wrong_type() {
        assert!(builtin_cl_seventh(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_eighth_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
            Value::symbol("e"),
            Value::symbol("f"),
            Value::symbol("g"),
            Value::symbol("h"),
        ]);
        let result = builtin_cl_eighth(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "h"));
    }

    #[test]
    fn cl_eighth_nil() {
        let result = builtin_cl_eighth(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_eighth_wrong_type() {
        assert!(builtin_cl_eighth(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_ninth_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
            Value::symbol("e"),
            Value::symbol("f"),
            Value::symbol("g"),
            Value::symbol("h"),
            Value::symbol("i"),
        ]);
        let result = builtin_cl_ninth(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "i"));
    }

    #[test]
    fn cl_ninth_nil() {
        let result = builtin_cl_ninth(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_ninth_wrong_type() {
        assert!(builtin_cl_ninth(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_tenth_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
            Value::symbol("d"),
            Value::symbol("e"),
            Value::symbol("f"),
            Value::symbol("g"),
            Value::symbol("h"),
            Value::symbol("i"),
            Value::symbol("j"),
        ]);
        let result = builtin_cl_tenth(vec![list]).unwrap();
        assert!(matches!(result, Value::Symbol(id) if resolve_sym(id) == "j"));
    }

    #[test]
    fn cl_tenth_nil() {
        let result = builtin_cl_tenth(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_tenth_wrong_type() {
        assert!(builtin_cl_tenth(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_rest_list() {
        let list = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
        ]);
        let result = builtin_cl_rest(vec![list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "b"));
    }

    #[test]
    fn cl_rest_nil() {
        let result = builtin_cl_rest(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_rest_wrong_type() {
        assert!(builtin_cl_rest(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_evenp_true() {
        let result = builtin_cl_evenp(vec![Value::Int(2)]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_evenp_false() {
        let result = builtin_cl_evenp(vec![Value::Int(3)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_evenp_wrong_type() {
        assert!(builtin_cl_evenp(vec![Value::string("x")]).is_err());
    }

    #[test]
    fn cl_oddp_true() {
        let result = builtin_cl_oddp(vec![Value::Int(3)]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_oddp_false() {
        let result = builtin_cl_oddp(vec![Value::Int(2)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_oddp_wrong_type() {
        assert!(builtin_cl_oddp(vec![Value::string("x")]).is_err());
    }

    #[test]
    fn cl_plusp_true() {
        let result = builtin_cl_plusp(vec![Value::Int(1)]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_plusp_false() {
        let result = builtin_cl_plusp(vec![Value::Int(0)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_plusp_wrong_type() {
        assert!(builtin_cl_plusp(vec![Value::string("x")]).is_err());
    }

    #[test]
    fn cl_minusp_true() {
        let result = builtin_cl_minusp(vec![Value::Int(-1)]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_minusp_false() {
        let result = builtin_cl_minusp(vec![Value::Int(0)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_minusp_wrong_type() {
        assert!(builtin_cl_minusp(vec![Value::string("x")]).is_err());
    }

    #[test]
    fn cl_subseq_list() {
        let result = builtin_cl_subseq(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
            ]),
            Value::Int(1),
            Value::Int(3),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("b"), Value::symbol("c")])
        );
    }

    #[test]
    fn cl_subseq_wrong_arity() {
        assert!(builtin_cl_subseq(vec![Value::Int(0)]).is_err());
    }

    #[test]
    fn cl_subseq_wrong_type() {
        assert!(builtin_cl_subseq(vec![Value::Int(0), Value::Int(0)]).is_err());
    }

    #[test]
    fn cl_concatenate_list() {
        let result = builtin_cl_concatenate(vec![
            Value::symbol("list"),
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
            Value::list(vec![Value::symbol("c")]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c")
            ])
        );
    }

    #[test]
    fn cl_concatenate_wrong_type_symbol() {
        assert!(builtin_cl_concatenate(vec![Value::Int(0), Value::Nil]).is_err());
    }

    #[test]
    fn cl_member_found_tail() {
        let result = builtin_cl_member(vec![
            Value::symbol("b"),
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
            ]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("b"), Value::symbol("c")])
        );
    }

    #[test]
    fn cl_member_not_found() {
        let result = builtin_cl_member(vec![
            Value::symbol("z"),
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_member_wrong_arity() {
        assert!(builtin_cl_member(vec![Value::symbol("a")]).is_err());
    }

    #[test]
    fn cl_coerce_list_to_vector() {
        let result = builtin_cl_coerce(vec![
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
            Value::symbol("vector"),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::vector(vec![Value::symbol("a"), Value::symbol("b")])
        );
    }

    #[test]
    fn cl_coerce_wrong_type_name() {
        assert!(builtin_cl_coerce(vec![Value::Nil, Value::Int(0)]).is_err());
    }

    #[test]
    fn cl_adjoin_prepends_when_missing() {
        let result = builtin_cl_adjoin(vec![
            Value::symbol("a"),
            Value::list(vec![Value::symbol("b"), Value::symbol("c")]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c")
            ])
        );
    }

    #[test]
    fn cl_adjoin_keeps_existing() {
        let list = Value::list(vec![Value::symbol("a"), Value::symbol("b")]);
        let result = builtin_cl_adjoin(vec![Value::symbol("a"), list.clone()]).unwrap();
        assert_eq!(result, list);
    }

    #[test]
    fn cl_adjoin_wrong_arity() {
        assert!(builtin_cl_adjoin(vec![Value::symbol("a")]).is_err());
    }

    #[test]
    fn cl_remove_filters_equal_items() {
        let result = builtin_cl_remove(vec![
            Value::symbol("a"),
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("a"),
            ]),
        ])
        .unwrap();
        assert_eq!(result, Value::list(vec![Value::symbol("b")]));
    }

    #[test]
    fn cl_remove_wrong_arity() {
        assert!(builtin_cl_remove(vec![Value::symbol("a")]).is_err());
    }

    #[test]
    fn cl_remove_duplicates_list() {
        let result = builtin_cl_remove_duplicates(vec![Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("a"),
            Value::symbol("c"),
            Value::symbol("b"),
        ])])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c")
            ])
        );
    }

    #[test]
    fn cl_remove_duplicates_wrong_arity() {
        assert!(builtin_cl_remove_duplicates(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn seq_drop_test() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_seq_drop(vec![list, Value::Int(2)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].as_int(), Some(3));
    }

    #[test]
    fn seq_take_test() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_seq_take(vec![list, Value::Int(2)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn seq_subseq_test() {
        let vec = Value::vector(vec![
            Value::Int(10),
            Value::Int(20),
            Value::Int(30),
            Value::Int(40),
        ]);
        let result = builtin_seq_subseq(vec![vec, Value::Int(1), Value::Int(3)]).unwrap();
        if let Value::Vector(v) = result {
            let v = with_heap(|h| h.get_vector(v).clone());
            assert_eq!(v.len(), 2);
            assert_eq!(v[0].as_int(), Some(20));
            assert_eq!(v[1].as_int(), Some(30));
        } else {
            panic!("expected vector");
        }
    }

    #[test]
    fn seq_concatenate_test() {
        let l1 = Value::list(vec![Value::Int(1)]);
        let l2 = Value::list(vec![Value::Int(2)]);
        let result = builtin_seq_concatenate(vec![Value::symbol("list"), l1, l2]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn seq_empty_p_test() {
        assert!(matches!(
            builtin_seq_empty_p(vec![Value::Nil]).unwrap(),
            Value::True
        ));
        assert!(matches!(
            builtin_seq_empty_p(vec![Value::string("")]).unwrap(),
            Value::True
        ));
        assert!(builtin_seq_empty_p(vec![Value::list(vec![Value::Int(1)])])
            .unwrap()
            .is_nil());
    }

    #[test]
    fn seq_min_max_test() {
        let list = Value::list(vec![Value::Int(3), Value::Int(1), Value::Int(2)]);
        assert_eq!(
            builtin_seq_min(vec![list.clone()]).unwrap().as_int(),
            Some(1)
        );
        assert_eq!(builtin_seq_max(vec![list]).unwrap().as_int(), Some(3));
    }

    // --- Eval-dependent tests (using Evaluator) ---

    #[test]
    fn seq_reduce_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("+"));
        let seq = Value::list(vec![Value::Int(10), Value::Int(20)]);
        let result = builtin_seq_reduce(&mut evaluator, vec![func, seq, Value::Int(0)]).unwrap();
        assert_eq!(result.as_int(), Some(30));
    }

    #[test]
    fn seq_count_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::Int(1), Value::string("a"), Value::Int(2)]);
        let result = builtin_seq_count(&mut evaluator, vec![func, seq]).unwrap();
        assert_eq!(result.as_int(), Some(2));
    }

    #[test]
    fn cl_position_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let seq = Value::list(vec![
            Value::symbol("a"),
            Value::symbol("b"),
            Value::symbol("c"),
        ]);
        let result = builtin_cl_position(&mut evaluator, vec![Value::symbol("b"), seq]).unwrap();
        assert_eq!(result.as_int(), Some(1));
    }

    #[test]
    fn cl_position_wrong_arity() {
        let mut evaluator = super::super::eval::Evaluator::new();
        assert!(builtin_cl_position(&mut evaluator, vec![Value::symbol("a")]).is_err());
    }

    #[test]
    fn cl_reduce_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("+"));
        let seq = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_reduce(&mut evaluator, vec![func, seq, Value::Int(0)]).unwrap();
        assert_eq!(result.as_int(), Some(6));
    }

    #[test]
    fn cl_count_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::Int(1), Value::string("x"), Value::Int(2)]);
        let result = builtin_cl_count(&mut evaluator, vec![func, seq]).unwrap();
        assert_eq!(result.as_int(), Some(2));
    }

    #[test]
    fn cl_count_if_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::Int(1), Value::string("x"), Value::Int(2)]);
        let result = builtin_cl_count_if(&mut evaluator, vec![func, seq]).unwrap();
        assert_eq!(result.as_int(), Some(2));
    }

    #[test]
    fn cl_some_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::string("x"), Value::Int(2)]);
        let result = builtin_cl_some(&mut evaluator, vec![func, seq]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_every_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_every(&mut evaluator, vec![func, seq]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_notany_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::string("x"), Value::string("y")]);
        let result = builtin_cl_notany(&mut evaluator, vec![func, seq]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_notevery_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr(intern("numberp"));
        let seq = Value::list(vec![Value::Int(1), Value::string("x")]);
        let result = builtin_cl_notevery(&mut evaluator, vec![func, seq]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_gensym_default_prefix() {
        let result = builtin_cl_gensym(vec![]).unwrap();
        match result {
            Value::Symbol(id) => assert!(resolve_sym(id).starts_with('G')),
            other => panic!("expected symbol, got {other:?}"),
        }
    }

    #[test]
    fn cl_gensym_custom_prefix() {
        let result = builtin_cl_gensym(vec![Value::string("P")]).unwrap();
        match result {
            Value::Symbol(id) => assert!(resolve_sym(id).starts_with('P')),
            other => panic!("expected symbol, got {other:?}"),
        }
    }

    #[test]
    fn cl_gensym_wrong_type() {
        assert!(builtin_cl_gensym(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn cl_find_found() {
        let result = builtin_cl_find(vec![
            Value::symbol("b"),
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
            ]),
        ])
        .unwrap();
        assert_eq!(result, Value::symbol("b"));
    }

    #[test]
    fn cl_find_not_found() {
        let result = builtin_cl_find(vec![
            Value::symbol("z"),
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
            ]),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_find_wrong_arity() {
        assert!(builtin_cl_find(vec![Value::symbol("a")]).is_err());
    }

    #[test]
    fn cl_find_if_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_cl_find_if(
            &mut evaluator,
            vec![
                Value::Subr(intern("numberp")),
                Value::list(vec![Value::string("x"), Value::Int(2)]),
            ],
        )
        .unwrap();
        assert_eq!(result, Value::Int(2));
    }

    #[test]
    fn cl_find_if_wrong_arity() {
        let mut evaluator = super::super::eval::Evaluator::new();
        assert!(
            builtin_cl_find_if(&mut evaluator, vec![Value::Subr(intern("numberp"))]).is_err()
        );
    }

    #[test]
    fn cl_subsetp_true() {
        let result = builtin_cl_subsetp(vec![
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
            Value::list(vec![
                Value::symbol("b"),
                Value::symbol("a"),
                Value::symbol("c"),
            ]),
        ])
        .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_subsetp_false() {
        let result = builtin_cl_subsetp(vec![
            Value::list(vec![Value::symbol("a"), Value::symbol("z")]),
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_subsetp_wrong_arity() {
        assert!(builtin_cl_subsetp(vec![Value::Nil]).is_err());
    }

    #[test]
    fn cl_intersection_basic() {
        let result = builtin_cl_intersection(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
            ]),
            Value::list(vec![
                Value::symbol("b"),
                Value::symbol("d"),
                Value::symbol("c"),
            ]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("b"), Value::symbol("c")])
        );
    }

    #[test]
    fn cl_intersection_no_overlap() {
        let result = builtin_cl_intersection(vec![
            Value::list(vec![Value::symbol("a")]),
            Value::list(vec![Value::symbol("z")]),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_intersection_wrong_arity() {
        assert!(builtin_cl_intersection(vec![Value::Nil]).is_err());
    }

    #[test]
    fn cl_set_difference_basic() {
        let result = builtin_cl_set_difference(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
            ]),
            Value::list(vec![Value::symbol("b"), Value::symbol("d")]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("a"), Value::symbol("c")])
        );
    }

    #[test]
    fn cl_set_difference_all_removed() {
        let result = builtin_cl_set_difference(vec![
            Value::list(vec![Value::symbol("a")]),
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_set_difference_wrong_arity() {
        assert!(builtin_cl_set_difference(vec![Value::Nil]).is_err());
    }

    #[test]
    fn cl_union_basic() {
        let result = builtin_cl_union(vec![
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
            Value::list(vec![Value::symbol("b"), Value::symbol("c")]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c")
            ])
        );
    }

    #[test]
    fn cl_union_empty_left() {
        let result =
            builtin_cl_union(vec![Value::Nil, Value::list(vec![Value::symbol("c")])]).unwrap();
        assert_eq!(result, Value::list(vec![Value::symbol("c")]));
    }

    #[test]
    fn cl_union_wrong_arity() {
        assert!(builtin_cl_union(vec![Value::Nil]).is_err());
    }

    #[test]
    fn cl_substitute_basic() {
        let result = builtin_cl_substitute(vec![
            Value::symbol("x"),
            Value::symbol("b"),
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("b"),
            ]),
        ])
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("x"),
                Value::symbol("c"),
                Value::symbol("x"),
            ])
        );
    }

    #[test]
    fn cl_substitute_wrong_arity() {
        assert!(builtin_cl_substitute(vec![Value::Nil]).is_err());
    }

    #[test]
    fn cl_sort_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let seq = Value::list(vec![Value::Int(3), Value::Int(1), Value::Int(2)]);
        let result =
            builtin_cl_sort(&mut evaluator, vec![seq, Value::Subr(intern("<"))]).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn cl_stable_sort_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let seq = Value::list(vec![Value::Int(3), Value::Int(1), Value::Int(2)]);
        let result =
            builtin_cl_stable_sort(&mut evaluator, vec![seq, Value::Subr(intern("<"))])
                .unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn cl_remove_if_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_cl_remove_if(
            &mut evaluator,
            vec![
                Value::Subr(intern("numberp")),
                Value::list(vec![Value::Int(1), Value::string("x"), Value::Int(2)]),
            ],
        )
        .unwrap();
        assert_eq!(result, Value::list(vec![Value::string("x")]));
    }

    #[test]
    fn cl_remove_if_not_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_cl_remove_if_not(
            &mut evaluator,
            vec![
                Value::Subr(intern("numberp")),
                Value::list(vec![Value::Int(1), Value::string("x"), Value::Int(2)]),
            ],
        )
        .unwrap();
        assert_eq!(result, Value::list(vec![Value::Int(1), Value::Int(2)]));
    }

    #[test]
    fn cl_map_list_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_cl_map(
            &mut evaluator,
            vec![
                Value::symbol("list"),
                Value::Subr(intern("1+")),
                Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
            ],
        )
        .unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::Int(2), Value::Int(3), Value::Int(4)])
        );
    }

    #[test]
    fn cl_map_string_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_cl_map(
            &mut evaluator,
            vec![
                Value::symbol("string"),
                Value::Subr(intern("identity")),
                Value::string("ab"),
            ],
        )
        .unwrap();
        assert_eq!(result, Value::string("ab"));
    }

    #[test]
    fn cl_map_unsupported_type() {
        let mut evaluator = super::super::eval::Evaluator::new();
        assert!(builtin_cl_map(
            &mut evaluator,
            vec![
                Value::symbol("hash-table"),
                Value::Subr(intern("identity")),
                Value::list(vec![Value::Int(1)]),
            ],
        )
        .is_err());
    }
}
