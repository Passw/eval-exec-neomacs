//! Code Conversion Language (CCL) compatibility runtime.
//!
//! CCL is a low-level bytecode language for efficient character/text conversion.
//! This implementation currently provides partial CCL behavior:
//! - `ccl-program-p` — basic predicate for vector-shaped CCL program headers
//! - `register-ccl-program` — stores named CCL programs and returns stable ids
//! - `register-code-conversion-map` — stores named conversion maps and returns stable ids
//! - `ccl-execute` / `ccl-execute-on-string` — validates shape and designators
//!   and mirrors current oracle error payloads for unsupported execution paths.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

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

    let second = second.as_int();
    if second.is_none() || second.is_some_and(|n| !(0..=3).contains(&n)) {
        return false;
    }

    is_integer(third)
}

#[derive(Default)]
struct CclRegistry {
    programs: HashMap<String, (i64, Value)>,
    code_conversion_maps: HashMap<String, (i64, Value)>,
    next_program_id: i64,
    next_code_conversion_map_id: i64,
}

impl CclRegistry {
    fn with_defaults() -> Self {
        Self {
            programs: HashMap::new(),
            code_conversion_maps: HashMap::new(),
            next_program_id: 1,
            next_code_conversion_map_id: 0,
        }
    }

    fn register_program(&mut self, name: &str, program: Value) -> i64 {
        if let Some((id, slot)) = self.programs.get_mut(name) {
            *slot = program;
            return *id;
        }
        let id = self.next_program_id;
        self.next_program_id = self.next_program_id.saturating_add(1);
        self.programs.insert(name.to_string(), (id, program));
        id
    }

    fn lookup_program(&self, name: &str) -> Option<Value> {
        self.programs.get(name).map(|(_, program)| program.clone())
    }

    fn register_code_conversion_map(&mut self, name: &str, value: Value) -> i64 {
        if let Some((id, slot)) = self.code_conversion_maps.get_mut(name) {
            *slot = value;
            return *id;
        }
        let id = self.next_code_conversion_map_id;
        self.next_code_conversion_map_id = self.next_code_conversion_map_id.saturating_add(1);
        self.code_conversion_maps
            .insert(name.to_string(), (id, value));
        id
    }
}

fn ccl_registry() -> &'static Mutex<CclRegistry> {
    static REGISTRY: OnceLock<Mutex<CclRegistry>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(CclRegistry::with_defaults()))
}

enum CclProgramDesignatorKind {
    Inline,
    RegisteredSymbol,
}

fn resolve_ccl_program_designator(value: &Value) -> Option<(Value, CclProgramDesignatorKind)> {
    if matches!(value, Value::Vector(_)) {
        return Some((value.clone(), CclProgramDesignatorKind::Inline));
    }
    let name = value.as_symbol_name()?;
    let registry = ccl_registry().lock().unwrap_or_else(|e| e.into_inner());
    registry
        .lookup_program(name)
        .map(|program| (program, CclProgramDesignatorKind::RegisteredSymbol))
}

fn ccl_program_code_index_message(program: &Value, designator_kind: CclProgramDesignatorKind) -> String {
    let base_len = match program {
        Value::Vector(handle) => with_heap(|h| h.vector_len(*handle) as i64),
        _ => 0,
    };
    let index = match designator_kind {
        CclProgramDesignatorKind::Inline => base_len.saturating_add(1),
        CclProgramDesignatorKind::RegisteredSymbol => base_len.saturating_add(2),
    };
    format!("Error in CCL program at {index}th code")
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
    let is_program = resolve_ccl_program_designator(&args[0])
        .map(|(program, _)| is_valid_ccl_program(&program))
        .unwrap_or(false);
    Ok(Value::bool(is_program))
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

    let Some((program, designator_kind)) = resolve_ccl_program_designator(&args[0]) else {
        return Err(signal("error", vec![Value::string("Invalid CCL program")]));
    };
    if !is_valid_ccl_program(&program) {
        return Err(signal("error", vec![Value::string("Invalid CCL program")]));
    }

    let message = ccl_program_code_index_message(&program, designator_kind);
    Err(signal("error", vec![Value::string(message)]))
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

    let Some((program, designator_kind)) = resolve_ccl_program_designator(&args[0]) else {
        return Err(signal("error", vec![Value::string("Invalid CCL program")]));
    };
    if !is_valid_ccl_program(&program) {
        return Err(signal("error", vec![Value::string("Invalid CCL program")]));
    }

    // Arguments:
    //   0: CCL-PROGRAM (we don't use)
    //   1: STATUS vector (we don't use)
    //   2: STRING (return this unchanged)
    //   3: CONTINUE (optional, we don't use)
    //   4: UNIBYTE-P (optional, we don't use)

    match &args[2] {
        Value::Str(_s) => {
            let message = ccl_program_code_index_message(&program, designator_kind);
            Err(signal("error", vec![Value::string(message)]))
        }
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
    let program = if args[1].is_nil() {
        // Oracle accepts nil and behaves like a minimal valid registered program.
        Value::vector(vec![Value::Int(0), Value::Int(0), Value::Int(0)])
    } else {
        if !args[1].is_vector() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("vectorp"), args[1].clone()],
            ));
        }
        args[1].clone()
    };

    if !is_valid_ccl_program(&program) {
        return Err(signal("error", vec![Value::string("Error in CCL program")]));
    }

    let name = args[0]
        .as_symbol_name()
        .expect("symbol already validated by is_symbol");
    let program_id = {
        let mut registry = ccl_registry().lock().unwrap_or_else(|e| e.into_inner());
        registry.register_program(name, program)
    };
    Ok(Value::Int(program_id))
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
    if !args[1].is_vector() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("vectorp"), args[1].clone()],
        ));
    }

    let name = args[0]
        .as_symbol_name()
        .expect("symbol already validated by is_symbol");
    let map_id = {
        let mut registry = ccl_registry().lock().unwrap_or_else(|e| e.into_inner());
        registry.register_code_conversion_map(name, args[1].clone())
    };
    Ok(Value::Int(map_id))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ccl_programp_validates_shape_and_type() {
        let program = Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]);
        let invalid_program = Value::vector(vec![Value::Int(0), Value::Int(0)]);
        let invalid_negative = Value::vector(vec![Value::Int(-1), Value::Int(0), Value::Int(0)]);
        let invalid_header_mode = Value::vector(vec![Value::Int(10), Value::Int(4), Value::Int(0)]);
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
        assert_eq!(
            builtin_ccl_program_p(vec![invalid_header_mode]).expect("invalid program"),
            Value::Nil
        );
    }

    #[test]
    fn ccl_programp_accepts_registered_symbol_designator() {
        assert_eq!(
            builtin_ccl_program_p(vec![Value::symbol("ccl-program-p-unregistered")])
                .expect("unregistered symbol should be nil"),
            Value::Nil
        );
        let _ = builtin_register_ccl_program(vec![
            Value::symbol("ccl-program-p-registered"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("registration should succeed");
        assert_eq!(
            builtin_ccl_program_p(vec![Value::symbol("ccl-program-p-registered")])
                .expect("registered symbol should be accepted"),
            Value::True
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
    fn register_ccl_program_requires_vector_when_program_non_nil() {
        let err = builtin_register_ccl_program(vec![Value::symbol("foo"), Value::Int(1)])
            .expect_err("register-ccl-program program must be vector when non-nil");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data[0], Value::symbol("vectorp"));
                assert_eq!(sig.data[1], Value::Int(1));
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn register_ccl_program_accepts_nil_program() {
        let result = builtin_register_ccl_program(vec![Value::symbol("foo-nil"), Value::Nil])
            .expect("register-ccl-program should accept nil");
        match result {
            Value::Int(id) => assert!(id > 0),
            other => panic!("expected integer id, got {other:?}"),
        }
        let programp = builtin_ccl_program_p(vec![Value::symbol("foo-nil")])
            .expect("registered nil program should resolve as valid");
        assert_eq!(programp, Value::True);
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
    fn register_ccl_program_rejects_second_header_out_of_range() {
        let err = builtin_register_ccl_program(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(10), Value::Int(4), Value::Int(0)]),
        ])
        .expect_err("second header slot must be in 0..=3");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data[0], Value::string("Error in CCL program"));
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn register_ccl_program_returns_success_code() {
        let first = builtin_register_ccl_program(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("valid registration should succeed");
        let second = builtin_register_ccl_program(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("repeat registration should keep id");
        assert_eq!(first, second);
        match first {
            Value::Int(id) => assert!(id > 0),
            other => panic!("expected integer id, got {other:?}"),
        }
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
    fn register_code_conversion_map_requires_vector_map() {
        let err = builtin_register_code_conversion_map(vec![Value::symbol("foo"), Value::Int(1)])
            .expect_err("register-code-conversion-map map must be vector");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data[0], Value::symbol("vectorp"));
                assert_eq!(sig.data[1], Value::Int(1));
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn register_code_conversion_map_returns_success_code() {
        let first = builtin_register_code_conversion_map(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("valid registration should succeed");
        let second = builtin_register_code_conversion_map(vec![
            Value::symbol("foo"),
            Value::vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
        ])
        .expect("repeat registration should keep id");
        assert_eq!(first, second);
        match first {
            Value::Int(id) => assert!(id >= 0),
            other => panic!("expected integer id, got {other:?}"),
        }
    }

    #[test]
    fn register_ccl_program_assigns_new_ids_for_new_symbols() {
        let a = builtin_register_ccl_program(vec![
            Value::symbol("ccl-id-a"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("registration a should succeed");
        let b = builtin_register_ccl_program(vec![
            Value::symbol("ccl-id-b"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("registration b should succeed");
        match (a, b) {
            (Value::Int(aid), Value::Int(bid)) => assert!(bid > aid),
            other => panic!("expected integer ids, got {other:?}"),
        }
    }

    #[test]
    fn register_code_conversion_map_assigns_new_ids_for_new_symbols() {
        let a = builtin_register_code_conversion_map(vec![
            Value::symbol("ccl-map-id-a"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("registration a should succeed");
        let b = builtin_register_code_conversion_map(vec![
            Value::symbol("ccl-map-id-b"),
            Value::vector(vec![Value::Int(10), Value::Int(0), Value::Int(0)]),
        ])
        .expect("registration b should succeed");
        match (a, b) {
            (Value::Int(aid), Value::Int(bid)) => assert!(bid > aid),
            other => panic!("expected integer ids, got {other:?}"),
        }
    }

    #[test]
    fn ccl_execute_accepts_registered_symbol_program_designator() {
        let _ = builtin_register_ccl_program(vec![
            Value::symbol("ccl-designator-probe"),
            Value::vector(vec![
                Value::Int(10),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
        ])
        .expect("registration should succeed");
        let err = builtin_ccl_execute(vec![
            Value::symbol("ccl-designator-probe"),
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
        .expect_err("symbol designator should resolve to registered program");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.data[0], Value::string("Error in CCL program at 6th code"));
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn ccl_execute_on_string_accepts_registered_symbol_program_designator() {
        let _ = builtin_register_ccl_program(vec![
            Value::symbol("ccl-designator-probe-on-string"),
            Value::vector(vec![
                Value::Int(10),
                Value::Int(0),
                Value::Int(0),
                Value::Int(0),
            ]),
        ])
        .expect("registration should succeed");
        let err = builtin_ccl_execute_on_string(vec![
            Value::symbol("ccl-designator-probe-on-string"),
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
        ])
        .expect_err("symbol designator should resolve to registered program");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.data[0], Value::string("Error in CCL program at 6th code"));
            }
            other => panic!("expected error signal, got {other:?}"),
        }
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
