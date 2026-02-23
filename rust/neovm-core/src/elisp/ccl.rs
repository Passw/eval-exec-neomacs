//! Code Conversion Language (CCL) stubs.
//!
//! CCL is a low-level bytecode language for efficient character/text conversion.
//! This implementation provides stubs for basic CCL operations:
//! - `ccl-program-p` — basic predicate for vector-shaped CCL program headers
//! - `ccl-execute` — execute CCL program on status vector (stub, validates status/program shape)
//! - `ccl-execute-on-string` — execute CCL program on string (stub, validates status/program shape)
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

    let program = with_heap(|h| h.get_vector(*program).clone());
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

    let status_len = match &args[1] {
        Value::Vector(vec) => with_heap(|h| h.vector_len(*vec)),
        _ => unreachable!("status already validated as vector"),
    };
    if status_len != 8 {
        return Err(signal(
            "error",
            vec![Value::string("Length of vector REGISTERS is not 8")],
        ));
    }

    if is_valid_ccl_program(&args[0]) {
        return Err(signal(
            "error",
            vec![Value::string("Error in CCL program at 4th code")],
        ));
    }

    Err(signal("error", vec![Value::string("Invalid CCL program")]))
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
    let status_len = match &args[1] {
        Value::Vector(vec) => with_heap(|h| h.vector_len(*vec)),
        _ => unreachable!("status already validated as vector"),
    };
    if status_len != 9 {
        return Err(signal(
            "error",
            vec![Value::string("Length of vector STATUS is not 9")],
        ));
    }

    if !is_valid_ccl_program(&args[0]) {
        return Err(signal("error", vec![Value::string("Invalid CCL program")]));
    }

    // Arguments:
    //   0: CCL-PROGRAM (we don't use)
    //   1: STATUS vector (we don't use)
    //   2: STRING (return this unchanged)
    //   3: CONTINUE (optional, we don't use)
    //   4: UNIBYTE-P (optional, we don't use)

    match &args[2] {
        Value::Str(_s) => Err(signal(
            "error",
            vec![Value::string("Error in CCL program at 4th code")],
        )),
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
    if !args[0].is_symbol() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }

    if !is_valid_ccl_program(&args[1]) {
        return Err(signal("error", vec![Value::string("Error in CCL program")]));
    }

    // Arguments:
    //   0: NAME (symbol name for the program)
    //   1: CCL-PROG (the program definition)
    // We accept both but don't store anything since we don't support CCL
    Ok(Value::Int(1))
}

/// (register-code-conversion-map SYMBOL MAP) -> nil
/// Stub: accepts and discards the code conversion map.
pub(crate) fn builtin_register_code_conversion_map(args: Vec<Value>) -> EvalResult {
    expect_args("register-code-conversion-map", &args, 2)?;
    if !args[0].is_symbol() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }

    // Arguments:
    //   0: SYMBOL (name for the conversion map)
    //   1: MAP (the conversion map definition, typically a char-table)
    // We accept both but don't store anything since we don't support CCL
    Ok(Value::Int(0))
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
    fn ccl_execute_requires_registers_vector_length_eight() {
        let err = builtin_ccl_execute(vec![
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::vector(vec![Value::Int(0), Value::Int(0), Value::Int(0)]),
        ])
        .expect_err("registers length should be checked");
        match err {
            Flow::Signal(sig) => assert_eq!(
                sig.data[0],
                Value::string("Length of vector REGISTERS is not 8")
            ),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn ccl_execute_reports_invalid_program_before_success() {
        let err = builtin_ccl_execute(vec![
            Value::Int(1),
            Value::vector(vec![
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
        ])
        .expect_err("non-vector program must be rejected");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.data[0], Value::string("Invalid CCL program")),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn ccl_execute_on_string_requires_status_vector_length_nine() {
        let err = builtin_ccl_execute_on_string(vec![
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::vector(vec![
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
            Value::string("abc"),
        ])
        .expect_err("status length should be checked");
        match err {
            Flow::Signal(sig) => assert_eq!(
                sig.data[0],
                Value::string("Length of vector STATUS is not 9")
            ),
            other => panic!("expected error signal, got {other:?}"),
        }
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
                Value::Int(0),
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
                Value::Int(0),
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

    #[test]
    fn register_ccl_program_requires_symbol_name() {
        let err =
            builtin_register_ccl_program(vec![Value::Int(1), Value::vector(vec![Value::Int(10)])])
                .expect_err("register-ccl-program name must be symbol");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn register_ccl_program_rejects_invalid_program_shape() {
        let err = builtin_register_ccl_program(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(1)]),
        ])
        .expect_err("invalid program must be rejected");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.data[0], Value::string("Error in CCL program"));
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn register_ccl_program_returns_success_code() {
        assert_eq!(
            builtin_register_ccl_program(vec![
                Value::symbol("foo"),
                Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            ])
            .expect("valid registration should succeed"),
            Value::Int(1)
        );
    }

    #[test]
    fn register_code_conversion_map_requires_symbol_name() {
        let err = builtin_register_code_conversion_map(vec![
            Value::Int(1),
            Value::vector(vec![Value::Int(0)]),
        ])
        .expect_err("register-code-conversion-map name must be symbol");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn register_code_conversion_map_returns_success_code() {
        assert_eq!(
            builtin_register_code_conversion_map(vec![
                Value::symbol("foo"),
                Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            ])
            .expect("valid registration should succeed"),
            Value::Int(0)
        );
    }

    #[test]
    fn register_ccl_program_rejects_over_arity() {
        let err = builtin_register_ccl_program(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::Nil,
        ])
        .expect_err("over-arity should signal");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn register_code_conversion_map_rejects_over_arity() {
        let err = builtin_register_code_conversion_map(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
            Value::Nil,
        ])
        .expect_err("over-arity should signal");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }
}
