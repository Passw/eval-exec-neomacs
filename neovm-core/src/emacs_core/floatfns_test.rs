use super::*;

// Helper to make float comparison with epsilon
fn assert_float_eq(val: &Value, expected: f64, epsilon: f64) {
    match val {
        Value::Float(f, _) => {
            assert!(
                (f - expected).abs() < epsilon,
                "expected {} but got {}",
                expected,
                f
            );
        }
        other => panic!("expected Float, got {:?}", other),
    }
}

fn assert_int_eq(val: &Value, expected: i64) {
    match val {
        Value::Int(n) => assert_eq!(*n, expected, "expected {} but got {}", expected, n),
        other => panic!("expected Int, got {:?}", other),
    }
}

// ===== Classification =====

#[test]
fn test_copysign() {
    let result = builtin_copysign(vec![
        Value::Float(5.0, next_float_id()),
        Value::Float(-1.0, next_float_id()),
    ])
    .unwrap();
    assert_float_eq(&result, -5.0, 1e-10);

    let result = builtin_copysign(vec![
        Value::Float(-5.0, next_float_id()),
        Value::Float(1.0, next_float_id()),
    ])
    .unwrap();
    assert_float_eq(&result, 5.0, 1e-10);
}

#[test]
fn test_frexp() {
    let result = builtin_frexp(vec![Value::Float(8.0, next_float_id())]).unwrap();
    // 8.0 = 0.5 * 2^4
    if let Value::Cons(cell) = &result {
        let pair = read_cons(*cell);
        assert_float_eq(&pair.car, 0.5, 1e-10);
        assert_int_eq(&pair.cdr, 4);
    } else {
        panic!("expected cons");
    }

    // frexp(0.0) = (0.0 . 0)
    let result = builtin_frexp(vec![Value::Float(0.0, next_float_id())]).unwrap();
    if let Value::Cons(cell) = &result {
        let pair = read_cons(*cell);
        assert_float_eq(&pair.car, 0.0, 1e-10);
        assert_int_eq(&pair.cdr, 0);
    } else {
        panic!("expected cons");
    }

    // frexp(-0.0) preserves signed-zero in significand.
    let result = builtin_frexp(vec![Value::Float(-0.0, next_float_id())]).unwrap();
    if let Value::Cons(cell) = &result {
        let pair = read_cons(*cell);
        match pair.car {
            Value::Float(f, _) => {
                assert_eq!(f, 0.0);
                assert!(f.is_sign_negative(), "expected negative zero");
            }
            ref other => panic!("expected Float, got {:?}", other),
        }
        assert_int_eq(&pair.cdr, 0);
    } else {
        panic!("expected cons");
    }
}

#[test]
fn test_frexp_negative() {
    let result = builtin_frexp(vec![Value::Float(-6.0, next_float_id())]).unwrap();
    // -6.0 = -0.75 * 2^3
    if let Value::Cons(cell) = &result {
        let pair = read_cons(*cell);
        assert_float_eq(&pair.car, -0.75, 1e-10);
        assert_int_eq(&pair.cdr, 3);
    } else {
        panic!("expected cons");
    }
}

#[test]
fn test_ldexp() {
    // 0.5 * 2^4 = 8.0
    let result = builtin_ldexp(vec![Value::Float(0.5, next_float_id()), Value::Int(4)]).unwrap();
    assert_float_eq(&result, 8.0, 1e-10);

    // 1.0 * 2^10 = 1024.0
    let result = builtin_ldexp(vec![Value::Float(1.0, next_float_id()), Value::Int(10)]).unwrap();
    assert_float_eq(&result, 1024.0, 1e-10);
}

// ===== logb =====

#[test]
fn test_logb() {
    // logb(8) = 3  (since log2(8) = 3)
    let result = builtin_logb(vec![Value::Float(8.0, next_float_id())]).unwrap();
    assert_int_eq(&result, 3);

    // logb(1) = 0
    let result = builtin_logb(vec![Value::Float(1.0, next_float_id())]).unwrap();
    assert_int_eq(&result, 0);

    // logb(0.5) = -1
    let result = builtin_logb(vec![Value::Float(0.5, next_float_id())]).unwrap();
    assert_int_eq(&result, -1);
}

// ===== Rounding to float =====

#[test]
fn test_fceiling() {
    let result = builtin_fceiling(vec![Value::Float(1.1, next_float_id())]).unwrap();
    assert_float_eq(&result, 2.0, 1e-10);

    let result = builtin_fceiling(vec![Value::Float(-1.1, next_float_id())]).unwrap();
    assert_float_eq(&result, -1.0, 1e-10);
}

#[test]
fn test_ffloor() {
    let result = builtin_ffloor(vec![Value::Float(1.9, next_float_id())]).unwrap();
    assert_float_eq(&result, 1.0, 1e-10);

    let result = builtin_ffloor(vec![Value::Float(-1.1, next_float_id())]).unwrap();
    assert_float_eq(&result, -2.0, 1e-10);
}

#[test]
fn test_fround() {
    let result = builtin_fround(vec![Value::Float(1.4, next_float_id())]).unwrap();
    assert_float_eq(&result, 1.0, 1e-10);

    let result = builtin_fround(vec![Value::Float(1.6, next_float_id())]).unwrap();
    assert_float_eq(&result, 2.0, 1e-10);

    // Banker's rounding
    let result = builtin_fround(vec![Value::Float(0.5, next_float_id())]).unwrap();
    assert_float_eq(&result, 0.0, 1e-10);

    let result = builtin_fround(vec![Value::Float(1.5, next_float_id())]).unwrap();
    assert_float_eq(&result, 2.0, 1e-10);

    let result = builtin_fround(vec![Value::Float(-0.5, next_float_id())]).unwrap();
    match result {
        Value::Float(f, _) => {
            assert_eq!(f, 0.0);
            assert!(f.is_sign_negative(), "expected negative zero");
        }
        other => panic!("expected Float, got {:?}", other),
    }
}

#[test]
fn test_ftruncate() {
    let result = builtin_ftruncate(vec![Value::Float(1.9, next_float_id())]).unwrap();
    assert_float_eq(&result, 1.0, 1e-10);

    let result = builtin_ftruncate(vec![Value::Float(-1.9, next_float_id())]).unwrap();
    assert_float_eq(&result, -1.0, 1e-10);
}

// ===== Wrong type errors =====

#[test]
fn test_wrong_type_errors() {
    assert!(
        builtin_copysign(vec![Value::string("x"), Value::Float(1.0, next_float_id())]).is_err()
    );
    assert!(builtin_copysign(vec![Value::Int(1), Value::Float(1.0, next_float_id())]).is_err());
    assert!(builtin_fceiling(vec![Value::Nil]).is_err());
    assert!(builtin_fceiling(vec![Value::Int(1)]).is_err());
    assert!(builtin_ffloor(vec![Value::Int(1)]).is_err());
    assert!(builtin_fround(vec![Value::Int(1)]).is_err());
    assert!(builtin_ftruncate(vec![Value::Int(1)]).is_err());
    assert!(
        builtin_ldexp(vec![
            Value::Float(1.0, next_float_id()),
            Value::Float(2.0, next_float_id())
        ])
        .is_err()
    );
    assert!(builtin_logb(vec![Value::True]).is_err());
    assert!(builtin_logb(vec![Value::string("y")]).is_err());
}

#[test]
fn test_ldexp_type_check_order_matches_oracle() {
    let err = builtin_ldexp(vec![
        Value::symbol("sym"),
        Value::Float(2.0, next_float_id()),
    ])
    .expect_err("ldexp should reject non-fixnum exponent first");
    match err {
        Flow::Signal(sig) => {
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
            assert_eq!(
                sig.data,
                vec![Value::symbol("fixnump"), Value::Float(2.0, next_float_id())]
            );
        }
        other => panic!("unexpected flow: {other:?}"),
    }

    let err = builtin_ldexp(vec![Value::symbol("sym"), Value::Int(2)])
        .expect_err("ldexp should reject significand after exponent passes");
    match err {
        Flow::Signal(sig) => {
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
            assert_eq!(
                sig.data,
                vec![Value::symbol("numberp"), Value::symbol("sym")]
            );
        }
        other => panic!("unexpected flow: {other:?}"),
    }
}

// ===== Wrong arity errors =====

#[test]
fn test_wrong_arity() {
    assert!(builtin_logb(vec![]).is_err());
    assert!(
        builtin_logb(vec![
            Value::Float(1.0, next_float_id()),
            Value::Float(2.0, next_float_id())
        ])
        .is_err()
    );
    assert!(builtin_copysign(vec![Value::Float(1.0, next_float_id())]).is_err());
    assert!(builtin_ldexp(vec![Value::Float(1.0, next_float_id())]).is_err());
    assert!(builtin_frexp(vec![]).is_err());
}
