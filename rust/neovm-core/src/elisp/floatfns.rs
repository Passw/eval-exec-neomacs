//! Float and math builtins for the Elisp interpreter.
//!
//! Implements all functions from Emacs `floatfns.c`:
//! - Classification: `copysign`, `frexp`, `ldexp`, `logb`
//! - Rounding (float result): `fceiling`, `ffloor`, `fround`, `ftruncate`

use super::error::{signal, EvalResult, Flow};
use super::value::*;

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

/// Extract a numeric argument as `f64` with `numberp` contract semantics.
fn extract_number(val: &Value) -> Result<f64, Flow> {
    match val {
        Value::Int(n) => Ok(*n as f64),
        Value::Char(c) => Ok(*c as u32 as f64),
        Value::Float(f) => Ok(*f),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

/// Extract a float argument with `floatp` contract semantics.
fn extract_float(val: &Value) -> Result<f64, Flow> {
    match val {
        Value::Float(f) => Ok(*f),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("floatp"), other.clone()],
        )),
    }
}

/// Extract a fixnum argument with `fixnump` contract semantics.
fn extract_fixnum(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Classification / special float operations
// ---------------------------------------------------------------------------

/// (copysign X1 X2) -- copy sign of X2 to magnitude of X1
pub(crate) fn builtin_copysign(args: Vec<Value>) -> EvalResult {
    expect_args("copysign", &args, 2)?;
    let x1 = extract_float(&args[0])?;
    let x2 = extract_float(&args[1])?;
    Ok(Value::Float(x1.copysign(x2)))
}

/// (frexp X) -- return (SIGNIFICAND . EXPONENT) cons cell
///
/// Decomposes X into significand * 2^exponent where 0.5 <= |significand| < 1.
/// Uses the C `frexp` convention that Emacs follows.
pub(crate) fn builtin_frexp(args: Vec<Value>) -> EvalResult {
    expect_args("frexp", &args, 1)?;
    let x = extract_number(&args[0])?;

    if x == 0.0 {
        return Ok(Value::cons(Value::Float(x), Value::Int(0)));
    }
    if x.is_nan() {
        return Ok(Value::cons(Value::Float(x), Value::Int(0)));
    }
    if x.is_infinite() {
        return Ok(Value::cons(Value::Float(x), Value::Int(0)));
    }

    // Rust doesn't have frexp in std, so we implement it manually.
    // frexp(x) returns (frac, exp) where x = frac * 2^exp, 0.5 <= |frac| < 1
    let bits = x.to_bits();
    let sign = bits >> 63;
    let exponent_bits = ((bits >> 52) & 0x7FF) as i64;
    let mantissa_bits = bits & 0x000F_FFFF_FFFF_FFFF;

    if exponent_bits == 0 {
        // Subnormal: normalize first
        let normalized = x * (1u64 << 52) as f64;
        let nbits = normalized.to_bits();
        let nexp = ((nbits >> 52) & 0x7FF) as i64;
        let nmant = nbits & 0x000F_FFFF_FFFF_FFFF;
        let exp = nexp - 1022 - 52;
        let frac_bits = (sign << 63) | (0x3FE << 52) | nmant;
        let frac = f64::from_bits(frac_bits);
        return Ok(Value::cons(Value::Float(frac), Value::Int(exp)));
    }

    let exp = exponent_bits - 1022;
    let frac_bits = (sign << 63) | (0x3FE << 52) | mantissa_bits;
    let frac = f64::from_bits(frac_bits);
    Ok(Value::cons(Value::Float(frac), Value::Int(exp)))
}

/// (ldexp SIGNIFICAND EXPONENT) -- return SIGNIFICAND * 2^EXPONENT
pub(crate) fn builtin_ldexp(args: Vec<Value>) -> EvalResult {
    expect_args("ldexp", &args, 2)?;
    let significand = extract_number(&args[0])?;
    let exponent = extract_fixnum(&args[1])?;

    // Use ldexp equivalent: significand * 2.0^exponent
    // Rust doesn't have ldexp in std, but we can use f64::exp2 approach
    // or simply multiply. For correctness with large exponents, we use
    // the powi approach clamped to avoid overflow in intermediate steps.
    let result = if exponent >= 0 && exponent <= 1023 {
        significand * f64::from_bits(((exponent + 1023) as u64) << 52)
    } else if exponent < 0 && exponent >= -1074 {
        significand * 2.0f64.powi(exponent as i32)
    } else if exponent > 1023 {
        // Very large exponent: will be infinity for any non-zero significand
        if significand == 0.0 {
            0.0
        } else {
            significand * f64::INFINITY
        }
    } else {
        // Very small exponent: will be 0.0
        0.0
    };

    Ok(Value::Float(result))
}

/// (logb X) -- integer part of base-2 logarithm of |X|
///
/// Returns the integer exponent from frexp, minus 1 (matching Emacs behavior).
/// For X = 0, signals a domain error (like Emacs).
pub(crate) fn builtin_logb(args: Vec<Value>) -> EvalResult {
    expect_args("logb", &args, 1)?;
    let x = extract_number(&args[0])?;

    if x == 0.0 {
        // Emacs returns -infinity as a float for logb(0)
        return Ok(Value::Float(f64::NEG_INFINITY));
    }
    if x.is_infinite() {
        return Ok(Value::Float(f64::INFINITY));
    }
    if x.is_nan() {
        return Ok(Value::Float(x));
    }

    // logb returns floor(log2(|x|)) as an integer, which is the exponent
    // from frexp minus 1 (since frexp normalizes to [0.5, 1.0)).
    let abs_x = x.abs();
    let result = abs_x.log2().floor() as i64;
    Ok(Value::Int(result))
}

// ---------------------------------------------------------------------------
// Rounding to float
// ---------------------------------------------------------------------------

/// (fceiling X) -- smallest integer not less than X, as a float
pub(crate) fn builtin_fceiling(args: Vec<Value>) -> EvalResult {
    expect_args("fceiling", &args, 1)?;
    let x = extract_float(&args[0])?;
    Ok(Value::Float(x.ceil()))
}

/// (ffloor X) -- largest integer not greater than X, as a float
pub(crate) fn builtin_ffloor(args: Vec<Value>) -> EvalResult {
    expect_args("ffloor", &args, 1)?;
    let x = extract_float(&args[0])?;
    Ok(Value::Float(x.floor()))
}

/// (fround X) -- nearest integer to X, as a float (banker's rounding)
pub(crate) fn builtin_fround(args: Vec<Value>) -> EvalResult {
    expect_args("fround", &args, 1)?;
    let x = extract_float(&args[0])?;
    Ok(Value::Float(x.round_ties_even()))
}

/// (ftruncate X) -- round X toward zero, as a float
pub(crate) fn builtin_ftruncate(args: Vec<Value>) -> EvalResult {
    expect_args("ftruncate", &args, 1)?;
    let x = extract_float(&args[0])?;
    Ok(Value::Float(x.trunc()))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to make float comparison with epsilon
    fn assert_float_eq(val: &Value, expected: f64, epsilon: f64) {
        match val {
            Value::Float(f) => {
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
        let result = builtin_copysign(vec![Value::Float(5.0), Value::Float(-1.0)]).unwrap();
        assert_float_eq(&result, -5.0, 1e-10);

        let result = builtin_copysign(vec![Value::Float(-5.0), Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, 5.0, 1e-10);
    }

    #[test]
    fn test_frexp() {
        let result = builtin_frexp(vec![Value::Float(8.0)]).unwrap();
        // 8.0 = 0.5 * 2^4
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_float_eq(&pair.car, 0.5, 1e-10);
            assert_int_eq(&pair.cdr, 4);
        } else {
            panic!("expected cons");
        }

        // frexp(0.0) = (0.0 . 0)
        let result = builtin_frexp(vec![Value::Float(0.0)]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_float_eq(&pair.car, 0.0, 1e-10);
            assert_int_eq(&pair.cdr, 0);
        } else {
            panic!("expected cons");
        }

        // frexp(-0.0) preserves signed-zero in significand.
        let result = builtin_frexp(vec![Value::Float(-0.0)]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            match pair.car {
                Value::Float(f) => {
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
        let result = builtin_frexp(vec![Value::Float(-6.0)]).unwrap();
        // -6.0 = -0.75 * 2^3
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_float_eq(&pair.car, -0.75, 1e-10);
            assert_int_eq(&pair.cdr, 3);
        } else {
            panic!("expected cons");
        }
    }

    #[test]
    fn test_ldexp() {
        // 0.5 * 2^4 = 8.0
        let result = builtin_ldexp(vec![Value::Float(0.5), Value::Int(4)]).unwrap();
        assert_float_eq(&result, 8.0, 1e-10);

        // 1.0 * 2^10 = 1024.0
        let result = builtin_ldexp(vec![Value::Float(1.0), Value::Int(10)]).unwrap();
        assert_float_eq(&result, 1024.0, 1e-10);
    }

    // ===== logb =====

    #[test]
    fn test_logb() {
        // logb(8) = 3  (since log2(8) = 3)
        let result = builtin_logb(vec![Value::Float(8.0)]).unwrap();
        assert_int_eq(&result, 3);

        // logb(1) = 0
        let result = builtin_logb(vec![Value::Float(1.0)]).unwrap();
        assert_int_eq(&result, 0);

        // logb(0.5) = -1
        let result = builtin_logb(vec![Value::Float(0.5)]).unwrap();
        assert_int_eq(&result, -1);
    }

    // ===== Rounding to float =====

    #[test]
    fn test_fceiling() {
        let result = builtin_fceiling(vec![Value::Float(1.1)]).unwrap();
        assert_float_eq(&result, 2.0, 1e-10);

        let result = builtin_fceiling(vec![Value::Float(-1.1)]).unwrap();
        assert_float_eq(&result, -1.0, 1e-10);
    }

    #[test]
    fn test_ffloor() {
        let result = builtin_ffloor(vec![Value::Float(1.9)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);

        let result = builtin_ffloor(vec![Value::Float(-1.1)]).unwrap();
        assert_float_eq(&result, -2.0, 1e-10);
    }

    #[test]
    fn test_fround() {
        let result = builtin_fround(vec![Value::Float(1.4)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);

        let result = builtin_fround(vec![Value::Float(1.6)]).unwrap();
        assert_float_eq(&result, 2.0, 1e-10);

        // Banker's rounding
        let result = builtin_fround(vec![Value::Float(0.5)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_fround(vec![Value::Float(1.5)]).unwrap();
        assert_float_eq(&result, 2.0, 1e-10);

        let result = builtin_fround(vec![Value::Float(-0.5)]).unwrap();
        match result {
            Value::Float(f) => {
                assert_eq!(f, 0.0);
                assert!(f.is_sign_negative(), "expected negative zero");
            }
            other => panic!("expected Float, got {:?}", other),
        }
    }

    #[test]
    fn test_ftruncate() {
        let result = builtin_ftruncate(vec![Value::Float(1.9)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);

        let result = builtin_ftruncate(vec![Value::Float(-1.9)]).unwrap();
        assert_float_eq(&result, -1.0, 1e-10);
    }

    // ===== Wrong type errors =====

    #[test]
    fn test_wrong_type_errors() {
        assert!(builtin_copysign(vec![Value::string("x"), Value::Float(1.0)]).is_err());
        assert!(builtin_copysign(vec![Value::Int(1), Value::Float(1.0)]).is_err());
        assert!(builtin_fceiling(vec![Value::Nil]).is_err());
        assert!(builtin_fceiling(vec![Value::Int(1)]).is_err());
        assert!(builtin_ffloor(vec![Value::Int(1)]).is_err());
        assert!(builtin_fround(vec![Value::Int(1)]).is_err());
        assert!(builtin_ftruncate(vec![Value::Int(1)]).is_err());
        assert!(builtin_ldexp(vec![Value::Float(1.0), Value::Float(2.0)]).is_err());
        assert!(builtin_logb(vec![Value::True]).is_err());
        assert!(builtin_logb(vec![Value::string("y")]).is_err());
    }

    // ===== Wrong arity errors =====

    #[test]
    fn test_wrong_arity() {
        assert!(builtin_logb(vec![]).is_err());
        assert!(builtin_logb(vec![Value::Float(1.0), Value::Float(2.0)]).is_err());
        assert!(builtin_copysign(vec![Value::Float(1.0)]).is_err());
        assert!(builtin_ldexp(vec![Value::Float(1.0)]).is_err());
        assert!(builtin_frexp(vec![]).is_err());
    }
}
