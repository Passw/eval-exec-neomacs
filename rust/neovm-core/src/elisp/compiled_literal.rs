//! Compatibility helpers for Emacs compiled-function reader literals (`#[...]`, `#(...)`).

use super::bytecode::{ByteCodeFunction, Op};
use super::value::{list_to_vec, LambdaParams, Value};

/// Convert parsed Emacs compiled-function literal vectors into typed
/// `Value::ByteCode` placeholders. The placeholder is intentionally explicit:
/// calls raise an error until native Emacs bytecode decoding/execution lands.
pub(crate) fn maybe_coerce_compiled_literal_function(value: Value) -> Value {
    let Value::Vector(items_ref) = &value else {
        return value;
    };
    let Some(bytecode) = compiled_literal_vector_to_placeholder_bytecode(items_ref) else {
        return value;
    };
    Value::ByteCode(std::sync::Arc::new(bytecode))
}

fn compiled_literal_vector_to_placeholder_bytecode(
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

    let mut bytecode = ByteCodeFunction::new(params);
    bytecode.max_stack = max_stack;
    bytecode.constants = constants_ref.lock().ok()?.clone();
    if let Some(Value::Str(s)) = items.get(4) {
        bytecode.docstring = Some((**s).clone());
    }

    let idx = bytecode.add_symbol("%%unimplemented-elc-bytecode");
    bytecode.emit(Op::CallBuiltin(idx, 0));
    bytecode.emit(Op::Return);
    Some(bytecode)
}

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
            0 => required.push(name.to_string()),
            1 => optional.push(name.to_string()),
            2 => {
                rest = Some(name.to_string());
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
