//! Code Conversion Language (CCL) stubs.
//!
//! CCL is a low-level bytecode language for efficient character/text conversion.
//! This implementation provides stubs for basic CCL operations:
//! - `ccl-program-p` — basic predicate for vector-shaped CCL program headers
//! - `ccl-execute` — execute CCL program on status vector (stub, returns nil)
//! - `ccl-execute-on-string` — execute CCL program on string (stub, returns string unchanged)
//! - `register-ccl-program` — register a CCL program (stub, returns nil)
//! - `register-code-conversion-map` — register a code conversion map (stub, returns nil)
//!
//! Since the Elisp interpreter doesn't implement the full CCL runtime,
//! all operations are no-ops that satisfy the API contract.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

fn is_integer(value: &Value) -> bool {
    matches!(value, Value::Int(_))
}

fn is_valid_ccl_program(program: &Value) -> bool {
    let Value::Vector(program) = program else {
        return false;
    };

    let program = program.lock().expect("poisoned ccl program vector");
    if program.len() < 3 {
        return false;
    }

    let [first, second, third] = [&program[0], &program[1], &program[2]];

    let first = first.as_int();
    if first.is_none() || first.is_some_and(|n| n < 0) {
        return false;
    }

    is_integer(second) && is_integer(third)
}

// ---------------------------------------------------------------------------
// Argument helpers
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

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (ccl-program-p OBJECT) -> nil
/// This accepts program objects that match the minimum CCL header shape used by Emacs.
pub(crate) fn builtin_ccl_program_p(args: Vec<Value>) -> EvalResult {
    expect_args("ccl-program-p", &args, 1)?;
    Ok(Value::bool(is_valid_ccl_program(&args[0])))
}

/// (ccl-execute CCL-PROGRAM STATUS) -> nil
/// Stub: doesn't actually execute CCL bytecode.
pub(crate) fn builtin_ccl_execute(args: Vec<Value>) -> EvalResult {
    expect_args("ccl-execute", &args, 2)?;
    if !args[1].is_vector() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("vectorp"), args[1].clone()],
        ));
    }

    Ok(Value::Nil)
}

/// (ccl-execute-on-string CCL-PROGRAM STATUS STRING &optional CONTINUE UNIBYTE-P) -> STRING
/// Stub: returns STRING unchanged without processing.
pub(crate) fn builtin_ccl_execute_on_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("ccl-execute-on-string", &args, 3)?;
    expect_max_args("ccl-execute-on-string", &args, 5)?;
    if !args[1].is_vector() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("vectorp"), args[1].clone()],
        ));
    }

    // Arguments:
    //   0: CCL-PROGRAM (we don't use)
    //   1: STATUS vector (we don't use)
    //   2: STRING (return this unchanged)
    //   3: CONTINUE (optional, we don't use)
    //   4: UNIBYTE-P (optional, we don't use)

    // Extract and return the string argument unchanged
    match &args[2] {
        Value::Str(s) => Ok(Value::Str(s.clone())),
        other => {
            // Type error: STRING must be a string or nil
            Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    }
}

/// (register-ccl-program NAME CCL-PROG) -> nil
/// Stub: accepts and discards the CCL program registration.
pub(crate) fn builtin_register_ccl_program(args: Vec<Value>) -> EvalResult {
    expect_args("register-ccl-program", &args, 2)?;
    // Arguments:
    //   0: NAME (symbol name for the program)
    //   1: CCL-PROG (the program definition)
    // We accept both but don't store anything since we don't support CCL
    Ok(Value::Nil)
}

/// (register-code-conversion-map SYMBOL MAP) -> nil
/// Stub: accepts and discards the code conversion map.
pub(crate) fn builtin_register_code_conversion_map(args: Vec<Value>) -> EvalResult {
    expect_args("register-code-conversion-map", &args, 2)?;
    // Arguments:
    //   0: SYMBOL (name for the conversion map)
    //   1: MAP (the conversion map definition, typically a char-table)
    // We accept both but don't store anything since we don't support CCL
    Ok(Value::Nil)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ccl_programp_validates_shape_and_type() {
        let program = Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]);
        let invalid_program = Value::vector(vec![Value::Int(0), Value::Int(0)]);
        let invalid_negative = Value::vector(vec![Value::Int(-1), Value::Int(0), Value::Int(0)]);
        assert_eq!(
            builtin_ccl_program_p(vec![program]).expect("valid program"),
            Value::True
        );
        assert_eq!(
            builtin_ccl_program_p(vec![invalid_program]).expect("invalid program"),
            Value::Nil
        );
        assert_eq!(
            builtin_ccl_program_p(vec![invalid_negative]).expect("invalid program"),
            Value::Nil
        );
    }

    #[test]
    fn ccl_execute_on_string_returns_string_payload() {
        let out = builtin_ccl_execute_on_string(vec![
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::vector(vec![
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
            Value::string("abc"),
        ])
        .expect("string payload should be returned");
        assert_eq!(out, Value::string("abc"));
    }

    #[test]
    fn ccl_execute_on_string_rejects_non_vector_status() {
        let err = builtin_ccl_execute_on_string(vec![
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::Int(1),
            Value::string("abc"),
        ])
        .expect_err("status must be a vector");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn ccl_execute_on_string_rejects_non_string_payload() {
        let err = builtin_ccl_execute_on_string(vec![
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::vector(vec![
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
            Value::Int(1),
        ])
        .expect_err("non-string payload must be rejected");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn ccl_execute_on_string_rejects_over_arity() {
        let err = builtin_ccl_execute_on_string(vec![
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::vector(vec![
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
            Value::string("abc"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ])
        .expect_err("over-arity should signal");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }
}
