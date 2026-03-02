use super::*;

#[test]
fn string_value_to_bytes_basic() {
    let bytes = string_value_to_bytes("ABC");
    assert_eq!(bytes, vec![65, 66, 67]);
}

#[test]
fn string_value_to_bytes_octal_escape() {
    // \300 = 0xC0 = char 192
    let s = "\u{00C0}"; // 192 as char
    let bytes = string_value_to_bytes(s);
    assert_eq!(bytes, vec![0xC0]);
}

#[test]
fn decode_simple_constant_return() {
    // bytecodes: constant(0) return
    // byte 192 = constant 0, byte 135 = return
    let bytecodes = vec![192, 135];
    let mut constants = vec![Value::Int(42)];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::Constant(0), Op::Return]);
}

#[test]
fn decode_car_cdr() {
    // car=64, cdr=65, return=135
    let bytecodes = vec![64, 65, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::Car, Op::Cdr, Op::Return]);
}

#[test]
fn decode_arithmetic() {
    // add=92, sub=90, mul=95, return=135
    let bytecodes = vec![92, 90, 95, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::Add, Op::Sub, Op::Mul, Op::Return]);
}

#[test]
fn decode_varref_immediate() {
    // varref 0 = byte 8, varref 5 = byte 13
    let bytecodes = vec![8, 13, 135];
    let mut constants = vec![Value::symbol("x"), Value::symbol("y"),
                             Value::Nil, Value::Nil, Value::Nil, Value::symbol("z")];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::VarRef(0), Op::VarRef(5), Op::Return]);
}

#[test]
fn decode_goto_jump_patching() {
    // constant(0), goto-if-nil to byte 5, constant(1), return, constant(2), return
    // byte 0: 192 → constant(0) [1 byte]
    // byte 1: 131, 5, 0 → goto-if-nil to byte 5 [3 bytes]
    // byte 4: 193 → constant(1) [1 byte]
    // byte 5: 135 → return [1 byte]
    let bytecodes = vec![192, 131, 5, 0, 193, 135];
    let mut constants = vec![Value::Nil, Value::Int(1)];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    // Instructions: [0] constant(0), [1] goto-if-nil(4), [2] constant(1), [3] return
    // Wait, byte 5 maps to instruction index... let me trace:
    // byte 0 → instr 0: constant(0)
    // byte 1 → instr 1: goto-if-nil(target=byte 5)
    // byte 4 → instr 2: constant(1)
    // byte 5 → instr 3: return
    // So goto-if-nil should jump to instruction 3
    assert_eq!(
        ops,
        vec![
            Op::Constant(0),
            Op::GotoIfNil(3),
            Op::Constant(1),
            Op::Return,
        ]
    );
}

#[test]
fn decode_call_immediate() {
    // call 0 = byte 32, call 3 = byte 35
    let bytecodes = vec![32, 35, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::Call(0), Op::Call(3), Op::Return]);
}

#[test]
fn decode_list_ops() {
    // list1=67, list2=68, cons=66
    let bytecodes = vec![67, 68, 66, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(
        ops,
        vec![Op::List(1), Op::List(2), Op::Cons, Op::Return]
    );
}

#[test]
fn decode_constant_range() {
    // byte 192 = constant(0), byte 255 = constant(63)
    let bytecodes = vec![192, 255, 135];
    let mut constants = (0..64).map(|i| Value::Int(i)).collect();
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(
        ops,
        vec![Op::Constant(0), Op::Constant(63), Op::Return]
    );
}

#[test]
fn decode_unwind_protect_pop() {
    // unwind-protect = byte 142
    let bytecodes = vec![142, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::UnwindProtectPop, Op::Return]);
}

#[test]
fn decode_discard_n() {
    // discardN = byte 182, operand = 3
    let bytecodes = vec![182, 3, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    assert_eq!(ops, vec![Op::DiscardN(3), Op::Return]);
}

#[test]
fn decode_buffer_op_point() {
    // point = byte 96
    let bytecodes = vec![96, 135];
    let mut constants = vec![];
    let ops = decode_gnu_bytecode(&bytecodes, &mut constants).unwrap();
    // Should have injected "point" into constants
    assert!(constants.iter().any(|c| c.as_symbol_name() == Some("point")));
    match &ops[0] {
        Op::CallBuiltin(_, 0) => {} // 0 args
        other => panic!("expected CallBuiltin for point, got {:?}", other),
    }
}

#[test]
fn parse_arglist_descriptor_no_rest() {
    // 2 mandatory, 3 total → 1 optional
    let params = parse_arglist_descriptor((2 << 8) | 3);
    assert_eq!(params.required.len(), 2);
    assert_eq!(params.optional.len(), 1);
    assert!(params.rest.is_none());
}

#[test]
fn parse_arglist_descriptor_with_rest() {
    // 1 mandatory + &rest
    let params = parse_arglist_descriptor(128 | 1);
    assert_eq!(params.required.len(), 1);
    assert_eq!(params.optional.len(), 0);
    assert!(params.rest.is_some());
}

#[test]
fn parse_arglist_descriptor_zero_args() {
    let params = parse_arglist_descriptor(0);
    assert_eq!(params.required.len(), 0);
    assert_eq!(params.optional.len(), 0);
    assert!(params.rest.is_none());
}

#[test]
fn parse_arglist_value_from_list() {
    use crate::emacs_core::intern::intern;
    let arglist = Value::list(vec![
        Value::symbol("x"),
        Value::symbol("&optional"),
        Value::symbol("y"),
        Value::symbol("&rest"),
        Value::symbol("z"),
    ]);
    let params = parse_arglist_value(&arglist);
    assert_eq!(params.required.len(), 1);
    assert_eq!(resolve_sym(params.required[0]), "x");
    assert_eq!(params.optional.len(), 1);
    assert_eq!(resolve_sym(params.optional[0]), "y");
    assert!(params.rest.is_some());
    assert_eq!(resolve_sym(params.rest.unwrap()), "z");
}

#[test]
fn parse_arglist_value_int() {
    let params = parse_arglist_value(&Value::Int((1 << 8) | 2));
    assert_eq!(params.required.len(), 1);
    assert_eq!(params.optional.len(), 1);
    assert!(params.rest.is_none());
}
