//! Compatibility helpers for Emacs compiled-function reader literals (`#[...]`, `#(...)`).

#[cfg(feature = "legacy-elc-literal")]
use super::bytecode::{ByteCodeFunction, Op};
use super::error::{signal, Flow};
use super::value::Value;
#[cfg(feature = "legacy-elc-literal")]
use super::intern::intern;
#[cfg(feature = "legacy-elc-literal")]
use super::value::{list_to_vec, LambdaParams};
#[cfg(feature = "legacy-elc-literal")]
use std::collections::HashMap;

/// Convert parsed Emacs compiled-function literal vectors into typed
/// `Value::ByteCode` functions when legacy `.elc` compatibility is enabled.
///
/// This path is intentionally non-default behind the `legacy-elc-literal`
/// Cargo feature.
#[cfg(feature = "legacy-elc-literal")]
pub(crate) fn maybe_coerce_compiled_literal_function(value: Value) -> Value {
    let Value::Vector(items_ref) = &value else {
        return value;
    };
    let Some(bytecode) = compiled_literal_vector_to_bytecode(items_ref) else {
        return value;
    };
    Value::make_bytecode(bytecode)
}

/// Default policy: keep parsed `#[...]` values as vectors and do not coerce
/// GNU Emacs `.elc` literal payloads into executable byte-code objects.
#[cfg(not(feature = "legacy-elc-literal"))]
pub(crate) fn maybe_coerce_compiled_literal_function(value: Value) -> Value {
    value
}

/// Build a typed placeholder from a `(byte-code BYTESTR CONSTS MAXDEPTH ...)`
/// form used by some `.elc` payloads.
#[cfg(feature = "legacy-elc-literal")]
#[allow(dead_code)]
pub(crate) fn placeholder_from_byte_code_form(args: &[Value]) -> Result<Value, Flow> {
    if args.len() < 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("byte-code"), Value::Int(args.len() as i64)],
        ));
    }

    if !matches!(args[0], Value::Str(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0]],
        ));
    }
    if !matches!(args[1], Value::Vector(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("vectorp"), args[1]],
        ));
    }
    let max_depth = match args[2] {
        Value::Int(n) if (0..=u16::MAX as i64).contains(&n) => Value::Int(n),
        Value::Int(_) => {
            return Err(signal("args-out-of-range", vec![args[2]]));
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("integerp"), args[2]],
            ));
        }
    };

    let params = if let Some(&value) = args.get(3) {
        if value.is_nil() || list_to_vec(&value).is_some() {
            value
        } else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), value],
            ));
        }
    } else {
        Value::Nil
    };

    let mut items = vec![params, args[0], args[1], max_depth];
    if let Some(&extra) = args.get(4) {
        items.push(extra);
    }

    let coerced = maybe_coerce_compiled_literal_function(Value::vector(items));
    if matches!(coerced, Value::ByteCode(_)) {
        Ok(coerced)
    } else {
        Err(signal(
            "error",
            vec![Value::string("Invalid byte-code object")],
        ))
    }
}

/// Source-only default policy: `(byte-code ...)` forms are unsupported unless
/// legacy `.elc` compatibility is explicitly enabled.
#[cfg(not(feature = "legacy-elc-literal"))]
#[allow(dead_code)]
pub(crate) fn placeholder_from_byte_code_form(_args: &[Value]) -> Result<Value, Flow> {
    Err(signal(
        "error",
        vec![Value::string(
            "Compiled .elc byte-code literal compatibility is disabled",
        )],
    ))
}

#[cfg(feature = "legacy-elc-literal")]
fn compiled_literal_vector_to_bytecode(
    items_ref: &std::sync::Arc<std::sync::Mutex<Vec<Value>>>,
) -> Option<ByteCodeFunction> {
    let items = items_ref.lock().ok()?;
    if items.len() < 4 {
        return None;
    }

    let params = parse_compiled_literal_params(&items[0])?;
    if items[1].as_str().is_none() {
        return None;
    }
    let Value::Vector(constants_ref) = &items[2] else {
        return None;
    };
    let max_stack = match items[3] {
        Value::Int(n) if (0..=u16::MAX as i64).contains(&n) => n as u16,
        _ => return None,
    };

    let byte_stream = items[1].as_str()?;
    let mut bytecode = ByteCodeFunction::new(params);
    bytecode.max_stack = max_stack;
    bytecode.constants = constants_ref.lock().ok()?.clone();
    if let Some(Value::Str(s)) = items.get(4) {
        bytecode.docstring = Some((**s).clone());
    }

    if let Some(decoded) = decode_opcode_subset(byte_stream, bytecode.constants.len()) {
        bytecode.ops = decoded;
    } else {
        let idx = bytecode.add_symbol("%%unimplemented-elc-bytecode");
        bytecode.emit(Op::CallBuiltin(idx, 0));
        bytecode.emit(Op::Return);
    }
    Some(bytecode)
}

#[cfg(feature = "legacy-elc-literal")]
fn decode_opcode_subset(byte_stream: &str, const_len: usize) -> Option<Vec<Op>> {
    enum Pending {
        Op(Op),
        Goto(usize),
        PushConditionCaseRaw(usize),
        PushCatch(usize),
        GotoIfNil(usize),
        GotoIfNotNil(usize),
        GotoIfNilElsePop(usize),
        GotoIfNotNilElsePop(usize),
    }

    let bytes = decode_unibyte_stream(byte_stream)?;
    let mut pending = Vec::with_capacity(bytes.len());
    let mut byte_to_op_index = HashMap::new();
    let mut pc = 0usize;
    while pc < bytes.len() {
        let b = bytes[pc];
        byte_to_op_index.insert(pc, pending.len());
        match b {
            // stack-ref 1..5
            0o001..=0o005 => {
                let n = b as u16;
                pending.push(Pending::Op(Op::StackRef(n)));
                pc += 1;
            }
            // stack-ref (wide 8-bit offset)
            0o006 => {
                let n = *bytes.get(pc + 1)? as u16;
                pending.push(Pending::Op(Op::StackRef(n)));
                pc += 2;
            }
            // stack-ref (wide 16-bit offset)
            0o007 => {
                let n = read_u16_operand(&bytes, pc + 1)?;
                pending.push(Pending::Op(Op::StackRef(n)));
                pc += 3;
            }
            // stack-set (wide 8-bit offset)
            0o262 => {
                let n = *bytes.get(pc + 1)? as u16;
                pending.push(Pending::Op(Op::StackSet(n)));
                pc += 2;
            }
            // stack-set2 (wide 16-bit offset)
            0o263 => {
                let n = read_u16_operand(&bytes, pc + 1)?;
                pending.push(Pending::Op(Op::StackSet(n)));
                pc += 3;
            }
            // discardN (raw immediate encoding; includes preserve-TOS flag bit)
            0o266 => {
                let n = *bytes.get(pc + 1)?;
                pending.push(Pending::Op(Op::DiscardN(n)));
                pc += 2;
            }
            // byte-constant 0..63
            0o300..=0o377 => {
                let idx = (b - 0o300) as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::Constant(idx as u16)));
                pc += 1;
            }
            // varref 0..5
            0o010..=0o015 => {
                let idx = (b - 0o010) as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarRef(idx as u16)));
                pc += 1;
            }
            // varref (wide 8-bit index)
            0o016 => {
                let idx = *bytes.get(pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarRef(idx as u16)));
                pc += 2;
            }
            // varref (wide 16-bit index)
            0o017 => {
                let idx = read_u16_operand(&bytes, pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarRef(idx as u16)));
                pc += 3;
            }
            // varset 0..5
            0o020..=0o025 => {
                let idx = (b - 0o020) as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarSet(idx as u16)));
                pc += 1;
            }
            // varset (wide 8-bit index)
            0o026 => {
                let idx = *bytes.get(pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarSet(idx as u16)));
                pc += 2;
            }
            // varset (wide 16-bit index)
            0o027 => {
                let idx = read_u16_operand(&bytes, pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarSet(idx as u16)));
                pc += 3;
            }
            // varbind 0..5
            0o030..=0o035 => {
                let idx = (b - 0o030) as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarBind(idx as u16)));
                pc += 1;
            }
            // varbind (wide 8-bit index)
            0o036 => {
                let idx = *bytes.get(pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarBind(idx as u16)));
                pc += 2;
            }
            // varbind (wide 16-bit index)
            0o037 => {
                let idx = read_u16_operand(&bytes, pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::VarBind(idx as u16)));
                pc += 3;
            }
            // call 0..7 (function object already pushed on stack)
            0o040..=0o047 => {
                let argc = (b - 0o040) as u8;
                pending.push(Pending::Op(Op::Call(argc)));
                pc += 1;
            }
            // unbind 0..5
            0o050..=0o055 => {
                let n = (b - 0o050) as u16;
                pending.push(Pending::Op(Op::Unbind(n)));
                pc += 1;
            }
            // unbind (wide 8-bit immediate)
            0o056 => {
                let n = *bytes.get(pc + 1)? as u16;
                pending.push(Pending::Op(Op::Unbind(n)));
                pc += 2;
            }
            // unbind (wide 16-bit immediate)
            0o057 => {
                let n = read_u16_operand(&bytes, pc + 1)?;
                pending.push(Pending::Op(Op::Unbind(n)));
                pc += 3;
            }
            // pophandler
            0o060 => {
                pending.push(Pending::Op(Op::PopHandler));
                pc += 1;
            }
            // pushconditioncase (16-bit bytecode stream offset)
            0o061 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::PushConditionCaseRaw(target));
                pc += 3;
            }
            // pushcatch (16-bit bytecode stream offset)
            0o062 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::PushCatch(target));
                pc += 3;
            }
            // goto (16-bit bytecode stream offset)
            0o202 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::Goto(target));
                pc += 3;
            }
            // goto-if-nil (16-bit bytecode stream offset)
            0o203 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::GotoIfNil(target));
                pc += 3;
            }
            // constant2 (wide 16-bit constant index)
            0o201 => {
                let idx = read_u16_operand(&bytes, pc + 1)? as usize;
                if idx >= const_len {
                    return None;
                }
                pending.push(Pending::Op(Op::Constant(idx as u16)));
                pc += 3;
            }
            // goto-if-not-nil (16-bit bytecode stream offset)
            0o204 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::GotoIfNotNil(target));
                pc += 3;
            }
            // goto-if-nil-else-pop (16-bit bytecode stream offset)
            0o205 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::GotoIfNilElsePop(target));
                pc += 3;
            }
            // goto-if-not-nil-else-pop (16-bit bytecode stream offset)
            0o206 => {
                let target = read_u16_operand(&bytes, pc + 1)? as usize;
                pending.push(Pending::GotoIfNotNilElsePop(target));
                pc += 3;
            }
            // not
            0o077 => {
                pending.push(Pending::Op(Op::Not));
                pc += 1;
            }
            // symbolp
            0o071 => {
                pending.push(Pending::Op(Op::Symbolp));
                pc += 1;
            }
            // consp
            0o072 => {
                pending.push(Pending::Op(Op::Consp));
                pc += 1;
            }
            // stringp
            0o073 => {
                pending.push(Pending::Op(Op::Stringp));
                pc += 1;
            }
            // listp
            0o074 => {
                pending.push(Pending::Op(Op::Listp));
                pc += 1;
            }
            // eq
            0o075 => {
                pending.push(Pending::Op(Op::Eq));
                pc += 1;
            }
            // memq
            0o076 => {
                pending.push(Pending::Op(Op::Memq));
                pc += 1;
            }
            // nth
            0o070 => {
                pending.push(Pending::Op(Op::Nth));
                pc += 1;
            }
            // car
            0o100 => {
                pending.push(Pending::Op(Op::Car));
                pc += 1;
            }
            // cdr
            0o101 => {
                pending.push(Pending::Op(Op::Cdr));
                pc += 1;
            }
            // cons
            0o102 => {
                pending.push(Pending::Op(Op::Cons));
                pc += 1;
            }
            // list N (currently covered for N in [1, 4])
            0o103..=0o106 => {
                let n = (b - 0o102) as u16;
                pending.push(Pending::Op(Op::List(n)));
                pc += 1;
            }
            // list N (wide form, 8-bit immediate arity)
            0o257 => {
                let n = *bytes.get(pc + 1)? as u16;
                pending.push(Pending::Op(Op::List(n)));
                pc += 2;
            }
            // length
            0o107 => {
                pending.push(Pending::Op(Op::Length));
                pc += 1;
            }
            // aref
            0o110 => {
                pending.push(Pending::Op(Op::Aref));
                pc += 1;
            }
            // aset
            0o111 => {
                pending.push(Pending::Op(Op::Aset));
                pc += 1;
            }
            // symbol-value
            0o112 => {
                pending.push(Pending::Op(Op::SymbolValue));
                pc += 1;
            }
            // symbol-function
            0o113 => {
                pending.push(Pending::Op(Op::SymbolFunction));
                pc += 1;
            }
            // set
            0o114 => {
                pending.push(Pending::Op(Op::Set));
                pc += 1;
            }
            // fset
            0o115 => {
                pending.push(Pending::Op(Op::Fset));
                pc += 1;
            }
            // get
            0o116 => {
                pending.push(Pending::Op(Op::Get));
                pc += 1;
            }
            // 1- (sub1)
            0o123 => {
                pending.push(Pending::Op(Op::Sub1));
                pc += 1;
            }
            // 1+ (add1)
            0o124 => {
                pending.push(Pending::Op(Op::Add1));
                pc += 1;
            }
            // numeric =
            0o125 => {
                pending.push(Pending::Op(Op::Eqlsign));
                pc += 1;
            }
            // >
            0o126 => {
                pending.push(Pending::Op(Op::Gtr));
                pc += 1;
            }
            // <
            0o127 => {
                pending.push(Pending::Op(Op::Lss));
                pc += 1;
            }
            // <=
            0o130 => {
                pending.push(Pending::Op(Op::Leq));
                pc += 1;
            }
            // >=
            0o131 => {
                pending.push(Pending::Op(Op::Geq));
                pc += 1;
            }
            // substring (STRING FROM TO)
            0o117 => {
                pending.push(Pending::Op(Op::Substring));
                pc += 1;
            }
            // concat N for N in [2, 4]
            0o120..=0o122 => {
                let n = (b - 0o116) as u16;
                pending.push(Pending::Op(Op::Concat(n)));
                pc += 1;
            }
            // concat N (wide form, 8-bit immediate arity)
            0o260 => {
                let n = *bytes.get(pc + 1)? as u16;
                pending.push(Pending::Op(Op::Concat(n)));
                pc += 2;
            }
            // -
            0o132 => {
                pending.push(Pending::Op(Op::Sub));
                pc += 1;
            }
            // unary -
            0o133 => {
                pending.push(Pending::Op(Op::Negate));
                pc += 1;
            }
            // +
            0o134 => {
                pending.push(Pending::Op(Op::Add));
                pc += 1;
            }
            // max
            0o135 => {
                pending.push(Pending::Op(Op::Max));
                pc += 1;
            }
            // min
            0o136 => {
                pending.push(Pending::Op(Op::Min));
                pc += 1;
            }
            // *
            0o137 => {
                pending.push(Pending::Op(Op::Mul));
                pc += 1;
            }
            // /
            0o245 => {
                pending.push(Pending::Op(Op::Div));
                pc += 1;
            }
            // %
            0o246 => {
                pending.push(Pending::Op(Op::Rem));
                pc += 1;
            }
            // nconc (binary; N-ary forms compile as repeated nconc opcodes)
            0o244 => {
                pending.push(Pending::Op(Op::Nconc));
                pc += 1;
            }
            // equal
            0o232 => {
                pending.push(Pending::Op(Op::Equal));
                pc += 1;
            }
            // member
            0o235 => {
                pending.push(Pending::Op(Op::Member));
                pc += 1;
            }
            // string=
            0o230 => {
                pending.push(Pending::Op(Op::StringEqual));
                pc += 1;
            }
            // string-lessp
            0o231 => {
                pending.push(Pending::Op(Op::StringLessp));
                pc += 1;
            }
            // nthcdr
            0o233 => {
                pending.push(Pending::Op(Op::Nthcdr));
                pc += 1;
            }
            // elt
            0o234 => {
                pending.push(Pending::Op(Op::Elt));
                pc += 1;
            }
            // assq
            0o236 => {
                pending.push(Pending::Op(Op::Assq));
                pc += 1;
            }
            // setcar
            0o240 => {
                pending.push(Pending::Op(Op::Setcar));
                pc += 1;
            }
            // setcdr
            0o241 => {
                pending.push(Pending::Op(Op::Setcdr));
                pc += 1;
            }
            // nreverse
            0o237 => {
                pending.push(Pending::Op(Op::Nreverse));
                pc += 1;
            }
            // car-safe
            0o242 => {
                pending.push(Pending::Op(Op::CarSafe));
                pc += 1;
            }
            // cdr-safe
            0o243 => {
                pending.push(Pending::Op(Op::CdrSafe));
                pc += 1;
            }
            // numberp
            0o247 => {
                pending.push(Pending::Op(Op::Numberp));
                pc += 1;
            }
            // integerp
            0o250 => {
                pending.push(Pending::Op(Op::Integerp));
                pc += 1;
            }
            // return
            0o207 => {
                pending.push(Pending::Op(Op::Return));
                pc += 1;
            }
            // discard top of stack
            0o210 => {
                pending.push(Pending::Op(Op::Pop));
                pc += 1;
            }
            // dup top of stack
            0o211 => {
                pending.push(Pending::Op(Op::Dup));
                pc += 1;
            }
            _ => return None,
        }
    }

    if pending.is_empty() {
        return None;
    }

    let mut ops = Vec::with_capacity(pending.len());
    for item in pending {
        let op = match item {
            Pending::Op(op) => op,
            Pending::Goto(target) => Op::Goto(*byte_to_op_index.get(&target)? as u32),
            Pending::PushConditionCaseRaw(target) => {
                Op::PushConditionCaseRaw(*byte_to_op_index.get(&target)? as u32)
            }
            Pending::PushCatch(target) => Op::PushCatch(*byte_to_op_index.get(&target)? as u32),
            Pending::GotoIfNil(target) => Op::GotoIfNil(*byte_to_op_index.get(&target)? as u32),
            Pending::GotoIfNotNil(target) => {
                Op::GotoIfNotNil(*byte_to_op_index.get(&target)? as u32)
            }
            Pending::GotoIfNilElsePop(target) => {
                Op::GotoIfNilElsePop(*byte_to_op_index.get(&target)? as u32)
            }
            Pending::GotoIfNotNilElsePop(target) => {
                Op::GotoIfNotNilElsePop(*byte_to_op_index.get(&target)? as u32)
            }
        };
        ops.push(op);
    }

    Some(ops)
}

#[cfg(feature = "legacy-elc-literal")]
fn read_u16_operand(bytes: &[u8], operand_start: usize) -> Option<u16> {
    if operand_start + 1 >= bytes.len() {
        return None;
    }
    let lo = bytes[operand_start] as u16;
    let hi = bytes[operand_start + 1] as u16;
    Some(lo | (hi << 8))
}

#[cfg(feature = "legacy-elc-literal")]
fn decode_unibyte_stream(byte_stream: &str) -> Option<Vec<u8>> {
    let mut out = Vec::with_capacity(byte_stream.len());
    for ch in byte_stream.chars() {
        let code = ch as u32;
        if code > u8::MAX as u32 {
            return None;
        }
        out.push(code as u8);
    }
    Some(out)
}

#[cfg(feature = "legacy-elc-literal")]
fn parse_compiled_literal_params(value: &Value) -> Option<LambdaParams> {
    if value.is_nil() {
        return Some(LambdaParams::simple(vec![]));
    }
    let items = list_to_vec(value)?;
    let mut required = Vec::new();
    let mut optional = Vec::new();
    let mut rest = None;
    let mut mode = 0_u8; // 0 = required, 1 = optional, 2 = rest

    for item in items {
        let name = item.as_symbol_name()?;
        match name {
            "&optional" => {
                mode = 1;
                continue;
            }
            "&rest" => {
                mode = 2;
                continue;
            }
            _ => {}
        }

        match mode {
            0 => required.push(intern(name)),
            1 => optional.push(intern(name)),
            2 => {
                rest = Some(intern(name));
                break;
            }
            _ => unreachable!(),
        }
    }

    Some(LambdaParams {
        required,
        optional,
        rest,
    })
}

#[cfg(all(test, feature = "legacy-elc-literal"))]
mod tests {
    use super::*;
    use crate::elisp::intern::{intern, resolve_sym};

    #[test]
    fn non_vector_passthrough() {
        let v = Value::Int(42);
        assert_eq!(maybe_coerce_compiled_literal_function(v), v);
    }

    #[test]
    fn invalid_vector_passthrough() {
        let v = Value::vector(vec![Value::Int(1), Value::Int(2)]);
        assert!(matches!(
            maybe_coerce_compiled_literal_function(v),
            Value::Vector(_)
        ));
    }

    #[test]
    fn coerces_compiled_literal_vector_to_placeholder_bytecode() {
        let literal = Value::vector(vec![
            Value::list(vec![
                Value::symbol("x"),
                Value::symbol("&optional"),
                Value::symbol("y"),
                Value::symbol("&rest"),
                Value::symbol("rest"),
            ]),
            Value::string("\u{FF}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::Int(7)]),
            Value::Int(3),
            Value::string("doc"),
        ]);

        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };

        assert_eq!(bc.params.required, vec![intern("x")]);
        assert_eq!(bc.params.optional, vec![intern("y")]);
        assert_eq!(bc.params.rest.map(|id| resolve_sym(id).to_owned()), Some("rest".to_owned()));
        assert_eq!(bc.max_stack, 3);
        assert_eq!(bc.constants[0], Value::symbol("x"));
        assert_eq!(bc.constants[1], Value::Int(7));
        assert_eq!(bc.docstring.as_deref(), Some("doc"));
        match bc.ops.as_slice() {
            [Op::CallBuiltin(idx, 0), Op::Return] => {
                assert_eq!(
                    bc.constants[*idx as usize],
                    Value::symbol("%%unimplemented-elc-bytecode")
                );
            }
            other => panic!("unexpected placeholder ops: {other:?}"),
        }
    }

    #[test]
    fn decodes_constant_return_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{87}"),
            Value::vector(vec![Value::Int(42)]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::Constant(0), Op::Return]);
    }

    #[test]
    fn decodes_constant2_opcode_subset() {
        let mut consts = Vec::new();
        for i in 0..=64 {
            consts.push(Value::Int(i));
        }
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{81}@\u{0}\u{87}"),
            Value::vector(consts),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::Constant(64), Op::Return]);
    }

    #[test]
    fn decodes_varref_return_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(0), Op::Return]);
    }

    #[test]
    fn decodes_varref_add1_return_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}T\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(0), Op::Add1, Op::Return]);
    }

    #[test]
    fn decodes_binary_arith_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\\\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Add, Op::Return]
        );
    }

    #[test]
    fn decodes_car_return_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}@\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(0), Op::Car, Op::Return]);
    }

    #[test]
    fn decodes_if_branch_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}\u{83}\u{6}\u{0}\u{C1}\u{87}\u{C2}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::Int(1), Value::Int(2)]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::GotoIfNil(4),
                Op::Constant(1),
                Op::Return,
                Op::Constant(2),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_goto_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{82}\u{5}\u{0}\u{C1}\u{87}"),
            Value::vector(vec![Value::Int(10), Value::Int(20)]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::Constant(0), Op::Goto(3), Op::Constant(1), Op::Return]
        );
    }

    #[test]
    fn decodes_pushconditioncase_and_pophandler_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{31}\u{6}\u{0}\u{C1}\u{30}\u{87}"),
            Value::vector(vec![Value::Int(10), Value::Int(20)]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::PushConditionCaseRaw(4),
                Op::Constant(1),
                Op::PopHandler,
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_pushcatch_and_pophandler_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{32}\u{6}\u{0}\u{C1}\u{30}\u{87}"),
            Value::vector(vec![Value::Int(1), Value::Int(2)]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::PushCatch(4),
                Op::Constant(1),
                Op::PopHandler,
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_list_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}D\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::List(2), Op::Return]
        );
    }

    #[test]
    fn decodes_comparison_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}W\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Lss, Op::Return]
        );
    }

    #[test]
    fn decodes_setcar_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{A0}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Setcar, Op::Return]
        );
    }

    #[test]
    fn decodes_safe_car_cdr_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}\u{A2}\u{8}\u{A3}D\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::CarSafe,
                Op::VarRef(0),
                Op::CdrSafe,
                Op::List(2),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_concat_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}P\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Concat(2), Op::Return]
        );
    }

    #[test]
    fn decodes_wide_concat_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
            ]),
            Value::string("\u{8}\u{9}\u{A}\u{B}\u{C}\u{B0}\u{5}\u{87}"),
            Value::vector(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
            ]),
            Value::Int(5),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::VarRef(1),
                Op::VarRef(2),
                Op::VarRef(3),
                Op::VarRef(4),
                Op::Concat(5),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_nthcdr_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("n"), Value::symbol("x")]),
            Value::string("\u{8}\u{9}\u{9B}\u{87}"),
            Value::vector(vec![Value::symbol("n"), Value::symbol("x")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Nthcdr, Op::Return]
        );
    }

    #[test]
    fn decodes_elt_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{9C}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Elt, Op::Return]
        );
    }

    #[test]
    fn decodes_integerp_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}\u{A8}\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(0), Op::Integerp, Op::Return]);
    }

    #[test]
    fn decodes_string_compare_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{98}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::StringEqual, Op::Return]
        );
    }

    #[test]
    fn decodes_aref_aset_opcode_subset() {
        let aref = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}H\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(aref);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Aref, Op::Return]
        );

        let aset = Value::vector(vec![
            Value::list(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
            ]),
            Value::string("\u{8}\u{9}\u{A}I\u{87}"),
            Value::vector(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
            ]),
            Value::Int(3),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(aset);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::VarRef(1),
                Op::VarRef(2),
                Op::Aset,
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_div_rem_opcode_subset() {
        let div = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{A5}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(div);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Div, Op::Return]
        );

        let rem = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{A6}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(rem);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Rem, Op::Return]
        );
    }

    #[test]
    fn decodes_wide_list_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
            ]),
            Value::string("\u{8}\u{9}\u{A}\u{C3}\u{C4}\u{C5}\u{AF}\u{6}\u{87}"),
            Value::vector(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
            ]),
            Value::Int(6),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::VarRef(1),
                Op::VarRef(2),
                Op::Constant(3),
                Op::Constant(4),
                Op::Constant(5),
                Op::List(6),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_member_nreverse_opcode_subset() {
        let member = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{9D}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(member);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Member, Op::Return]
        );

        let nreverse = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}\u{9F}\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(nreverse);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(0), Op::Nreverse, Op::Return]);
    }

    #[test]
    fn decodes_generic_call_opcode_subset() {
        let call0 = Value::vector(vec![
            Value::list(vec![]),
            Value::string("\u{C0} \u{87}"),
            Value::vector(vec![Value::symbol("list")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(call0);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::Constant(0), Op::Call(0), Op::Return]);

        let call1 = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{C1}\u{8}!\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("reverse")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(call1);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::Constant(1), Op::VarRef(0), Op::Call(1), Op::Return]
        );

        let call2 = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{C2}\u{8}\u{9}\"\u{87}"),
            Value::vector(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("append"),
            ]),
            Value::Int(3),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(call2);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(2),
                Op::VarRef(0),
                Op::VarRef(1),
                Op::Call(2),
                Op::Return,
            ]
        );

        let call3 = Value::vector(vec![
            Value::list(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
            ]),
            Value::string("\u{C3}\u{8}\u{9}\u{A}#\u{87}"),
            Value::vector(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
                Value::symbol("append"),
            ]),
            Value::Int(4),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(call3);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(3),
                Op::VarRef(0),
                Op::VarRef(1),
                Op::VarRef(2),
                Op::Call(3),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_nconc_opcode_subset() {
        let nconc2 = Value::vector(vec![
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::string("\u{8}\u{9}\u{A4}\u{87}"),
            Value::vector(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(nconc2);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![Op::VarRef(0), Op::VarRef(1), Op::Nconc, Op::Return]
        );

        let nconc3 = Value::vector(vec![
            Value::list(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
            ]),
            Value::string("\u{8}\u{9}\u{A4}\u{A}\u{A4}\u{87}"),
            Value::vector(vec![
                Value::symbol("x"),
                Value::symbol("y"),
                Value::symbol("z"),
            ]),
            Value::Int(3),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(nconc3);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::VarRef(1),
                Op::Nconc,
                Op::VarRef(2),
                Op::Nconc,
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_wide_varref_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
                Value::symbol("f"),
                Value::symbol("g"),
                Value::symbol("h"),
            ]),
            Value::string("\u{8}\u{9}\u{A}\u{B}\u{C}\u{D}\u{E}\u{6}\u{E}\u{7}\u{AF}\u{8}\u{87}"),
            Value::vector(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
                Value::symbol("f"),
                Value::symbol("g"),
                Value::symbol("h"),
            ]),
            Value::Int(8),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::VarRef(1),
                Op::VarRef(2),
                Op::VarRef(3),
                Op::VarRef(4),
                Op::VarRef(5),
                Op::VarRef(6),
                Op::VarRef(7),
                Op::List(8),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_wide16_varref_opcode_subset() {
        let mut consts = Vec::new();
        for i in 0..=300 {
            consts.push(Value::Int(i));
        }
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{F},\u{1}\u{87}"),
            Value::vector(consts),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(300), Op::Return]);
    }

    #[test]
    fn decodes_varset_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
                Value::symbol("f"),
                Value::symbol("g"),
                Value::symbol("h"),
            ]),
            Value::string("\u{9}\u{10}\u{8}\u{E}\u{7}\u{16}\u{6}\u{E}\u{6}\u{87}"),
            Value::vector(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
                Value::symbol("f"),
                Value::symbol("g"),
                Value::symbol("h"),
            ]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(1),
                Op::VarSet(0),
                Op::VarRef(0),
                Op::VarRef(7),
                Op::VarSet(6),
                Op::VarRef(6),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_varbind_unbind_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
                Value::symbol("f"),
                Value::symbol("g"),
                Value::symbol("h"),
            ]),
            Value::string("\u{8}\u{18})\u{E}\u{7}\u{1E}\u{6}.\u{1}/,\u{1}\u{87}"),
            Value::vector(vec![
                Value::symbol("a"),
                Value::symbol("b"),
                Value::symbol("c"),
                Value::symbol("d"),
                Value::symbol("e"),
                Value::symbol("f"),
                Value::symbol("g"),
                Value::symbol("h"),
            ]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::VarRef(0),
                Op::VarBind(0),
                Op::Unbind(1),
                Op::VarRef(7),
                Op::VarBind(6),
                Op::Unbind(1),
                Op::Unbind(300),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_stack_ref_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{1}\u{6}\u{6}\u{7},\u{1}\u{87}"),
            Value::vector(vec![]),
            Value::Int(4),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::StackRef(1),
                Op::StackRef(6),
                Op::StackRef(300),
                Op::Return
            ]
        );
    }

    #[test]
    fn decodes_dup_discard_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{89}\u{88}\u{87}"),
            Value::vector(vec![Value::Int(1)]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::Constant(0), Op::Dup, Op::Pop, Op::Return]);
    }

    #[test]
    fn decodes_stack_set_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{C1}\u{C2}\u{B2}\u{1}D\u{87}"),
            Value::vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
            Value::Int(3),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::StackSet(1),
                Op::List(2),
                Op::Return,
            ]
        );

        let literal_wide = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{C1}\u{C2}\u{C3}\u{B3}\u{2}\u{0}E\u{87}"),
            Value::vector(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4),
            ]),
            Value::Int(4),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal_wide);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::Constant(3),
                Op::StackSet(2),
                Op::List(3),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_discard_n_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{C1}\u{C2}\u{B6}\u{2}\u{87}"),
            Value::vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
            Value::Int(3),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::DiscardN(2),
                Op::Return,
            ]
        );

        let literal_preserve = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{C1}\u{C2}\u{B6}\u{82}\u{87}"),
            Value::vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
            Value::Int(3),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal_preserve);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::DiscardN(0x82),
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_symbol_cell_opcode_subset() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}J\u{C1}K\u{C3}\u{C0}L\u{C0}\u{C2}N\u{87}"),
            Value::vector(vec![
                Value::symbol("x"),
                Value::symbol("car"),
                Value::symbol("p"),
                Value::Int(5),
            ]),
            Value::Int(2),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(
            bc.ops,
            vec![
                Op::Constant(0),
                Op::SymbolValue,
                Op::Constant(1),
                Op::SymbolFunction,
                Op::Constant(3),
                Op::Constant(0),
                Op::Set,
                Op::Constant(0),
                Op::Constant(2),
                Op::Get,
                Op::Return,
            ]
        );
    }

    #[test]
    fn decodes_negate_opcode_subset() {
        let literal = Value::vector(vec![
            Value::list(vec![Value::symbol("x")]),
            Value::string("\u{8}[\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ]);
        let coerced = maybe_coerce_compiled_literal_function(literal);
        let Value::ByteCode(bc) = coerced else {
            panic!("expected Value::ByteCode");
        };
        assert_eq!(bc.ops, vec![Op::VarRef(0), Op::Negate, Op::Return]);
    }

    #[test]
    fn byte_code_form_coerces_to_placeholder() {
        let value = placeholder_from_byte_code_form(&[
            Value::string("\u{8}T\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ])
        .expect("byte-code form should coerce");
        assert!(matches!(value, Value::ByteCode(_)));
    }

    #[test]
    fn byte_code_form_rejects_non_vector_constants() {
        let err = placeholder_from_byte_code_form(&[
            Value::string("\u{8}T\u{87}"),
            Value::Int(1),
            Value::Int(1),
        ])
        .expect_err("non-vector constants should fail");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol_name(), "wrong-type-argument")
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }
}

#[cfg(all(test, not(feature = "legacy-elc-literal")))]
mod default_policy_tests {
    use super::*;

    #[test]
    fn non_vector_passthrough() {
        let v = Value::Int(42);
        assert_eq!(maybe_coerce_compiled_literal_function(v), v);
    }

    #[test]
    fn vector_literal_passthrough() {
        let literal = Value::vector(vec![
            Value::Nil,
            Value::string("\u{C0}\u{87}"),
            Value::vector(vec![Value::Int(7)]),
            Value::Int(1),
        ]);
        assert!(matches!(
            maybe_coerce_compiled_literal_function(literal),
            Value::Vector(_)
        ));
    }

    #[test]
    fn byte_code_form_is_rejected_when_legacy_disabled() {
        let err = placeholder_from_byte_code_form(&[
            Value::string("\u{8}T\u{87}"),
            Value::vector(vec![Value::symbol("x")]),
            Value::Int(1),
        ])
        .expect_err("byte-code form should be rejected");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol_name(), "error"),
            other => panic!("unexpected error: {other:?}"),
        }
    }
}
