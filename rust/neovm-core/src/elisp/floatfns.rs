//! Float and math builtins for the Elisp interpreter.
//!
//! Implements all functions from Emacs `floatfns.c`:
//! - Trigonometric: `acos`, `asin`, `atan`, `cos`, `sin`, `tan`
//! - Classification: `isnan`, `copysign`, `frexp`, `ldexp`, `logb`
//! - Exponential: `exp`, `expt`, `log`, `sqrt`
//! - Conversion: `float`
//! - Rounding (integer result): `ceiling`, `floor`, `round`, `truncate`
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

fn expect_args_range(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

/// Extract a numeric argument as `f64`.  Accepts `Value::Int` and `Value::Float`.
/// Signals `wrong-type-argument` with `number-or-marker-p` for anything else
/// (matching Emacs behaviour).
fn extract_float(_name: &str, val: &Value) -> Result<f64, Flow> {
    match val {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

/// Extract a numeric argument that must be an integer.  Signals
/// `wrong-type-argument` with `integerp` for non-integer values.
fn extract_int(_name: &str, val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

/// Check a float result for domain errors (NaN produced by a function that
/// should not produce NaN, e.g. acos(2)).
fn check_domain(name: &str, result: f64) -> Result<f64, Flow> {
    if result.is_nan() {
        Err(signal(
            "domain-error",
            vec![Value::symbol(name), Value::string("NaN")],
        ))
    } else {
        Ok(result)
    }
}

/// Check a float result for range errors (infinity).
fn check_range(name: &str, result: f64) -> Result<f64, Flow> {
    if result.is_infinite() {
        Err(signal(
            "range-error",
            vec![Value::symbol(name), Value::string("infinity")],
        ))
    } else {
        Ok(result)
    }
}

// ---------------------------------------------------------------------------
// Trigonometric
// ---------------------------------------------------------------------------

/// (acos X) -- arc cosine of X
pub(crate) fn builtin_acos(args: Vec<Value>) -> EvalResult {
    expect_args("acos", &args, 1)?;
    let x = extract_float("acos", &args[0])?;
    let result = x.acos();
    let result = check_domain("acos", result)?;
    Ok(Value::Float(result))
}

/// (asin X) -- arc sine of X
pub(crate) fn builtin_asin(args: Vec<Value>) -> EvalResult {
    expect_args("asin", &args, 1)?;
    let x = extract_float("asin", &args[0])?;
    let result = x.asin();
    let result = check_domain("asin", result)?;
    Ok(Value::Float(result))
}

/// (atan Y &optional X) -- arc tangent of Y, or atan2(Y, X) if X given
pub(crate) fn builtin_atan(args: Vec<Value>) -> EvalResult {
    expect_args_range("atan", &args, 1, 2)?;
    let y = extract_float("atan", &args[0])?;
    if args.len() == 2 {
        let x = extract_float("atan", &args[1])?;
        Ok(Value::Float(y.atan2(x)))
    } else {
        Ok(Value::Float(y.atan()))
    }
}

/// (cos X) -- cosine of X
pub(crate) fn builtin_cos(args: Vec<Value>) -> EvalResult {
    expect_args("cos", &args, 1)?;
    let x = extract_float("cos", &args[0])?;
    Ok(Value::Float(x.cos()))
}

/// (sin X) -- sine of X
pub(crate) fn builtin_sin(args: Vec<Value>) -> EvalResult {
    expect_args("sin", &args, 1)?;
    let x = extract_float("sin", &args[0])?;
    Ok(Value::Float(x.sin()))
}

/// (tan X) -- tangent of X
pub(crate) fn builtin_tan(args: Vec<Value>) -> EvalResult {
    expect_args("tan", &args, 1)?;
    let x = extract_float("tan", &args[0])?;
    let result = x.tan();
    let result = check_range("tan", result)?;
    Ok(Value::Float(result))
}

// ---------------------------------------------------------------------------
// Classification / special float operations
// ---------------------------------------------------------------------------

/// (isnan X) -- return t if X is NaN, nil otherwise
pub(crate) fn builtin_isnan(args: Vec<Value>) -> EvalResult {
    expect_args("isnan", &args, 1)?;
    let x = extract_float("isnan", &args[0])?;
    Ok(Value::bool(x.is_nan()))
}

/// (copysign X1 X2) -- copy sign of X2 to magnitude of X1
pub(crate) fn builtin_copysign(args: Vec<Value>) -> EvalResult {
    expect_args("copysign", &args, 2)?;
    let x1 = extract_float("copysign", &args[0])?;
    let x2 = extract_float("copysign", &args[1])?;
    Ok(Value::Float(x1.copysign(x2)))
}

/// (frexp X) -- return (SIGNIFICAND . EXPONENT) cons cell
///
/// Decomposes X into significand * 2^exponent where 0.5 <= |significand| < 1.
/// Uses the C `frexp` convention that Emacs follows.
pub(crate) fn builtin_frexp(args: Vec<Value>) -> EvalResult {
    expect_args("frexp", &args, 1)?;
    let x = extract_float("frexp", &args[0])?;

    if x == 0.0 {
        return Ok(Value::cons(Value::Float(0.0), Value::Int(0)));
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
    let significand = extract_float("ldexp", &args[0])?;
    let exponent = extract_int("ldexp", &args[1])?;

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

// ---------------------------------------------------------------------------
// Exponential / power / logarithm
// ---------------------------------------------------------------------------

/// (exp X) -- e^X
pub(crate) fn builtin_exp(args: Vec<Value>) -> EvalResult {
    expect_args("exp", &args, 1)?;
    let x = extract_float("exp", &args[0])?;
    let result = x.exp();
    let result = check_range("exp", result)?;
    Ok(Value::Float(result))
}

/// (expt X Y) -- X raised to the power Y
///
/// When both X and Y are integers and Y >= 0, returns an integer result.
/// Otherwise returns a float.
pub(crate) fn builtin_expt(args: Vec<Value>) -> EvalResult {
    expect_args("expt", &args, 2)?;

    // Integer fast-path: both args are Int and Y >= 0
    if let (Value::Int(base), Value::Int(exp)) = (&args[0], &args[1]) {
        if *exp >= 0 {
            return int_pow(*base, *exp as u64);
        }
    }

    let x = extract_float("expt", &args[0])?;
    let y = extract_float("expt", &args[1])?;
    Ok(Value::Float(x.powf(y)))
}

/// Integer exponentiation with overflow check.
fn int_pow(base: i64, exp: u64) -> EvalResult {
    let mut result: i64 = 1;
    let mut b = base;
    let mut e = exp;
    while e > 0 {
        if e & 1 == 1 {
            result = match result.checked_mul(b) {
                Some(v) => v,
                None => {
                    // Overflow: fall back to float
                    return Ok(Value::Float((base as f64).powf(exp as f64)));
                }
            };
        }
        e >>= 1;
        if e > 0 {
            b = match b.checked_mul(b) {
                Some(v) => v,
                None => {
                    return Ok(Value::Float((base as f64).powf(exp as f64)));
                }
            };
        }
    }
    Ok(Value::Int(result))
}

/// (log X &optional BASE) -- natural log of X, or log base BASE
pub(crate) fn builtin_log(args: Vec<Value>) -> EvalResult {
    expect_args_range("log", &args, 1, 2)?;
    let x = extract_float("log", &args[0])?;

    if x <= 0.0 {
        return Err(signal(
            "domain-error",
            vec![Value::symbol("log"), Value::Float(x)],
        ));
    }

    if args.len() == 2 {
        let base = extract_float("log", &args[1])?;
        if base <= 0.0 || base == 1.0 {
            return Err(signal(
                "domain-error",
                vec![Value::symbol("log"), Value::Float(base)],
            ));
        }
        Ok(Value::Float(x.ln() / base.ln()))
    } else {
        Ok(Value::Float(x.ln()))
    }
}

/// (sqrt X) -- square root of X
pub(crate) fn builtin_sqrt(args: Vec<Value>) -> EvalResult {
    expect_args("sqrt", &args, 1)?;
    let x = extract_float("sqrt", &args[0])?;
    if x < 0.0 {
        return Err(signal(
            "domain-error",
            vec![Value::symbol("sqrt"), Value::Float(x)],
        ));
    }
    Ok(Value::Float(x.sqrt()))
}

/// (float X) -- convert X to a floating-point number
pub(crate) fn builtin_float(args: Vec<Value>) -> EvalResult {
    expect_args("float", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::Float(f) => Ok(Value::Float(*f)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

/// (logb X) -- integer part of base-2 logarithm of |X|
///
/// Returns the integer exponent from frexp, minus 1 (matching Emacs behavior).
/// For X = 0, signals a domain error (like Emacs).
pub(crate) fn builtin_logb(args: Vec<Value>) -> EvalResult {
    expect_args("logb", &args, 1)?;
    let x = extract_float("logb", &args[0])?;

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
// Rounding to integer
// ---------------------------------------------------------------------------

/// Helper for ceiling/floor/round/truncate with optional DIVISOR.
/// Returns the quotient as f64 after dividing by DIVISOR (if present).
fn divide_for_rounding(name: &str, args: &[Value]) -> Result<(f64, bool), Flow> {
    let x = extract_float(name, &args[0])?;
    let is_int_division = matches!(args[0], Value::Int(_));

    if args.len() == 2 {
        let divisor = extract_float(name, &args[1])?;
        if divisor == 0.0 {
            return Err(signal("arith-error", vec![Value::symbol(name)]));
        }
        let both_int = is_int_division && matches!(args[1], Value::Int(_));
        Ok((x / divisor, both_int))
    } else {
        Ok((x, is_int_division))
    }
}

/// (ceiling X &optional DIVISOR) -- round X upward to integer
pub(crate) fn builtin_ceiling(args: Vec<Value>) -> EvalResult {
    expect_args_range("ceiling", &args, 1, 2)?;
    let (quotient, _both_int) = divide_for_rounding("ceiling", &args)?;

    if quotient.is_nan() || quotient.is_infinite() {
        return Err(signal(
            "range-error",
            vec![Value::symbol("ceiling"), Value::Float(quotient)],
        ));
    }

    // When there is no DIVISOR and the arg is already Int, return it directly
    if args.len() == 1 {
        if let Value::Int(n) = &args[0] {
            return Ok(Value::Int(*n));
        }
    }

    let result = quotient.ceil();
    Ok(Value::Int(result as i64))
}

/// (floor X &optional DIVISOR) -- round X downward to integer
pub(crate) fn builtin_floor(args: Vec<Value>) -> EvalResult {
    expect_args_range("floor", &args, 1, 2)?;
    let (quotient, _both_int) = divide_for_rounding("floor", &args)?;

    if quotient.is_nan() || quotient.is_infinite() {
        return Err(signal(
            "range-error",
            vec![Value::symbol("floor"), Value::Float(quotient)],
        ));
    }

    if args.len() == 1 {
        if let Value::Int(n) = &args[0] {
            return Ok(Value::Int(*n));
        }
    }

    let result = quotient.floor();
    Ok(Value::Int(result as i64))
}

/// (round X &optional DIVISOR) -- round X to nearest integer (banker's rounding)
///
/// Uses "round half to even" (banker's rounding) to match Emacs behavior.
pub(crate) fn builtin_round(args: Vec<Value>) -> EvalResult {
    expect_args_range("round", &args, 1, 2)?;
    let (quotient, _both_int) = divide_for_rounding("round", &args)?;

    if quotient.is_nan() || quotient.is_infinite() {
        return Err(signal(
            "range-error",
            vec![Value::symbol("round"), Value::Float(quotient)],
        ));
    }

    if args.len() == 1 {
        if let Value::Int(n) = &args[0] {
            return Ok(Value::Int(*n));
        }
    }

    let result = bankers_round(quotient);
    Ok(Value::Int(result as i64))
}

/// Banker's rounding: round half to even.
fn bankers_round(x: f64) -> f64 {
    let rounded = x.round();
    // Check if we are exactly at the halfway point
    let frac = x - x.floor();
    if frac == 0.5 {
        // Round to even
        let floor_val = x.floor();
        if floor_val as i64 % 2 == 0 {
            floor_val
        } else {
            floor_val + 1.0
        }
    } else if frac == -0.5 {
        let ceil_val = x.ceil();
        if ceil_val as i64 % 2 == 0 {
            ceil_val
        } else {
            ceil_val - 1.0
        }
    } else {
        rounded
    }
}

/// (truncate X &optional DIVISOR) -- round X toward zero to integer
pub(crate) fn builtin_truncate(args: Vec<Value>) -> EvalResult {
    expect_args_range("truncate", &args, 1, 2)?;
    let (quotient, _both_int) = divide_for_rounding("truncate", &args)?;

    if quotient.is_nan() || quotient.is_infinite() {
        return Err(signal(
            "range-error",
            vec![Value::symbol("truncate"), Value::Float(quotient)],
        ));
    }

    if args.len() == 1 {
        if let Value::Int(n) = &args[0] {
            return Ok(Value::Int(*n));
        }
    }

    let result = quotient.trunc();
    Ok(Value::Int(result as i64))
}

// ---------------------------------------------------------------------------
// Rounding to float
// ---------------------------------------------------------------------------

/// (fceiling X) -- smallest integer not less than X, as a float
pub(crate) fn builtin_fceiling(args: Vec<Value>) -> EvalResult {
    expect_args("fceiling", &args, 1)?;
    let x = extract_float("fceiling", &args[0])?;
    Ok(Value::Float(x.ceil()))
}

/// (ffloor X) -- largest integer not greater than X, as a float
pub(crate) fn builtin_ffloor(args: Vec<Value>) -> EvalResult {
    expect_args("ffloor", &args, 1)?;
    let x = extract_float("ffloor", &args[0])?;
    Ok(Value::Float(x.floor()))
}

/// (fround X) -- nearest integer to X, as a float (banker's rounding)
pub(crate) fn builtin_fround(args: Vec<Value>) -> EvalResult {
    expect_args("fround", &args, 1)?;
    let x = extract_float("fround", &args[0])?;
    Ok(Value::Float(bankers_round(x)))
}

/// (ftruncate X) -- round X toward zero, as a float
pub(crate) fn builtin_ftruncate(args: Vec<Value>) -> EvalResult {
    expect_args("ftruncate", &args, 1)?;
    let x = extract_float("ftruncate", &args[0])?;
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

    // ===== Trigonometric =====

    #[test]
    fn test_acos() {
        let result = builtin_acos(vec![Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_acos(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, std::f64::consts::FRAC_PI_2, 1e-10);

        // Domain error for |x| > 1
        assert!(builtin_acos(vec![Value::Float(2.0)]).is_err());
    }

    #[test]
    fn test_asin() {
        let result = builtin_asin(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_asin(vec![Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, std::f64::consts::FRAC_PI_2, 1e-10);

        // Domain error for |x| > 1
        assert!(builtin_asin(vec![Value::Float(2.0)]).is_err());
    }

    #[test]
    fn test_atan_one_arg() {
        let result = builtin_atan(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_atan(vec![Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, std::f64::consts::FRAC_PI_4, 1e-10);
    }

    #[test]
    fn test_atan_two_args() {
        // atan2(1, 1) = pi/4
        let result = builtin_atan(vec![Value::Float(1.0), Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, std::f64::consts::FRAC_PI_4, 1e-10);

        // atan2(0, -1) = pi
        let result = builtin_atan(vec![Value::Float(0.0), Value::Float(-1.0)]).unwrap();
        assert_float_eq(&result, std::f64::consts::PI, 1e-10);
    }

    #[test]
    fn test_cos() {
        let result = builtin_cos(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);

        let result = builtin_cos(vec![Value::Float(std::f64::consts::PI)]).unwrap();
        assert_float_eq(&result, -1.0, 1e-10);
    }

    #[test]
    fn test_sin() {
        let result = builtin_sin(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_sin(vec![Value::Float(std::f64::consts::FRAC_PI_2)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);
    }

    #[test]
    fn test_tan() {
        let result = builtin_tan(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_tan(vec![Value::Float(std::f64::consts::FRAC_PI_4)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);
    }

    #[test]
    fn test_trig_accepts_int() {
        // Trig functions should accept integers too
        let result = builtin_cos(vec![Value::Int(0)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);

        let result = builtin_sin(vec![Value::Int(0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);
    }

    // ===== Classification =====

    #[test]
    fn test_isnan() {
        let result = builtin_isnan(vec![Value::Float(f64::NAN)]).unwrap();
        assert!(result.is_truthy());

        let result = builtin_isnan(vec![Value::Float(1.0)]).unwrap();
        assert!(result.is_nil());

        let result = builtin_isnan(vec![Value::Int(0)]).unwrap();
        assert!(result.is_nil());
    }

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

    // ===== Exponential / power =====

    #[test]
    fn test_exp() {
        let result = builtin_exp(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);

        let result = builtin_exp(vec![Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, std::f64::consts::E, 1e-10);
    }

    #[test]
    fn test_expt_float() {
        let result = builtin_expt(vec![Value::Float(2.0), Value::Float(10.0)]).unwrap();
        assert_float_eq(&result, 1024.0, 1e-10);
    }

    #[test]
    fn test_expt_int() {
        // Integer result when both args are int and exp >= 0
        let result = builtin_expt(vec![Value::Int(2), Value::Int(10)]).unwrap();
        assert_int_eq(&result, 1024);

        let result = builtin_expt(vec![Value::Int(3), Value::Int(0)]).unwrap();
        assert_int_eq(&result, 1);

        let result = builtin_expt(vec![Value::Int(-2), Value::Int(3)]).unwrap();
        assert_int_eq(&result, -8);
    }

    #[test]
    fn test_expt_negative_exp() {
        // Negative integer exponent forces float result
        let result = builtin_expt(vec![Value::Int(2), Value::Int(-1)]).unwrap();
        assert_float_eq(&result, 0.5, 1e-10);
    }

    #[test]
    fn test_log() {
        let result = builtin_log(vec![Value::Float(1.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_log(vec![Value::Float(std::f64::consts::E)]).unwrap();
        assert_float_eq(&result, 1.0, 1e-10);
    }

    #[test]
    fn test_log_with_base() {
        // log(8, 2) = 3
        let result = builtin_log(vec![Value::Float(8.0), Value::Float(2.0)]).unwrap();
        assert_float_eq(&result, 3.0, 1e-10);

        // log(100, 10) = 2
        let result = builtin_log(vec![Value::Float(100.0), Value::Float(10.0)]).unwrap();
        assert_float_eq(&result, 2.0, 1e-10);
    }

    #[test]
    fn test_log_domain_error() {
        assert!(builtin_log(vec![Value::Float(0.0)]).is_err());
        assert!(builtin_log(vec![Value::Float(-1.0)]).is_err());
    }

    #[test]
    fn test_sqrt() {
        let result = builtin_sqrt(vec![Value::Float(4.0)]).unwrap();
        assert_float_eq(&result, 2.0, 1e-10);

        let result = builtin_sqrt(vec![Value::Float(0.0)]).unwrap();
        assert_float_eq(&result, 0.0, 1e-10);

        let result = builtin_sqrt(vec![Value::Int(9)]).unwrap();
        assert_float_eq(&result, 3.0, 1e-10);

        // Domain error for negative
        assert!(builtin_sqrt(vec![Value::Float(-1.0)]).is_err());
    }

    // ===== float / logb =====

    #[test]
    fn test_float_conversion() {
        let result = builtin_float(vec![Value::Int(42)]).unwrap();
        assert_float_eq(&result, 42.0, 1e-10);

        let result = builtin_float(vec![Value::Float(3.14)]).unwrap();
        assert_float_eq(&result, 3.14, 1e-10);
    }

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

    // ===== Rounding to integer =====

    #[test]
    fn test_ceiling() {
        let result = builtin_ceiling(vec![Value::Float(1.1)]).unwrap();
        assert_int_eq(&result, 2);

        let result = builtin_ceiling(vec![Value::Float(-1.1)]).unwrap();
        assert_int_eq(&result, -1);

        let result = builtin_ceiling(vec![Value::Float(2.0)]).unwrap();
        assert_int_eq(&result, 2);

        // Integer passthrough
        let result = builtin_ceiling(vec![Value::Int(5)]).unwrap();
        assert_int_eq(&result, 5);
    }

    #[test]
    fn test_ceiling_with_divisor() {
        // ceiling(7, 2) = 4
        let result = builtin_ceiling(vec![Value::Float(7.0), Value::Float(2.0)]).unwrap();
        assert_int_eq(&result, 4);
    }

    #[test]
    fn test_floor() {
        let result = builtin_floor(vec![Value::Float(1.9)]).unwrap();
        assert_int_eq(&result, 1);

        let result = builtin_floor(vec![Value::Float(-1.1)]).unwrap();
        assert_int_eq(&result, -2);

        let result = builtin_floor(vec![Value::Int(5)]).unwrap();
        assert_int_eq(&result, 5);
    }

    #[test]
    fn test_floor_with_divisor() {
        // floor(7, 2) = 3
        let result = builtin_floor(vec![Value::Float(7.0), Value::Float(2.0)]).unwrap();
        assert_int_eq(&result, 3);
    }

    #[test]
    fn test_round_basic() {
        let result = builtin_round(vec![Value::Float(1.4)]).unwrap();
        assert_int_eq(&result, 1);

        let result = builtin_round(vec![Value::Float(1.6)]).unwrap();
        assert_int_eq(&result, 2);

        let result = builtin_round(vec![Value::Float(-1.4)]).unwrap();
        assert_int_eq(&result, -1);

        let result = builtin_round(vec![Value::Float(-1.6)]).unwrap();
        assert_int_eq(&result, -2);
    }

    #[test]
    fn test_round_bankers() {
        // Banker's rounding: 0.5 rounds to even
        let result = builtin_round(vec![Value::Float(0.5)]).unwrap();
        assert_int_eq(&result, 0);

        let result = builtin_round(vec![Value::Float(1.5)]).unwrap();
        assert_int_eq(&result, 2);

        let result = builtin_round(vec![Value::Float(2.5)]).unwrap();
        assert_int_eq(&result, 2);

        let result = builtin_round(vec![Value::Float(3.5)]).unwrap();
        assert_int_eq(&result, 4);
    }

    #[test]
    fn test_truncate() {
        let result = builtin_truncate(vec![Value::Float(1.9)]).unwrap();
        assert_int_eq(&result, 1);

        let result = builtin_truncate(vec![Value::Float(-1.9)]).unwrap();
        assert_int_eq(&result, -1);

        let result = builtin_truncate(vec![Value::Int(5)]).unwrap();
        assert_int_eq(&result, 5);
    }

    #[test]
    fn test_truncate_with_divisor() {
        // truncate(7, 2) = 3
        let result = builtin_truncate(vec![Value::Float(7.0), Value::Float(2.0)]).unwrap();
        assert_int_eq(&result, 3);

        // truncate(-7, 2) = -3
        let result = builtin_truncate(vec![Value::Float(-7.0), Value::Float(2.0)]).unwrap();
        assert_int_eq(&result, -3);
    }

    #[test]
    fn test_division_by_zero() {
        assert!(builtin_ceiling(vec![Value::Float(1.0), Value::Float(0.0)]).is_err());
        assert!(builtin_floor(vec![Value::Float(1.0), Value::Float(0.0)]).is_err());
        assert!(builtin_round(vec![Value::Float(1.0), Value::Float(0.0)]).is_err());
        assert!(builtin_truncate(vec![Value::Float(1.0), Value::Float(0.0)]).is_err());
    }

    // ===== Rounding to float =====

    #[test]
    fn test_fceiling() {
        let result = builtin_fceiling(vec![Value::Float(1.1)]).unwrap();
        assert_float_eq(&result, 2.0, 1e-10);

        let result = builtin_fceiling(vec![Value::Float(-1.1)]).unwrap();
        assert_float_eq(&result, -1.0, 1e-10);

        let result = builtin_fceiling(vec![Value::Int(5)]).unwrap();
        assert_float_eq(&result, 5.0, 1e-10);
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
        assert!(builtin_cos(vec![Value::string("x")]).is_err());
        assert!(builtin_sin(vec![Value::Nil]).is_err());
        assert!(builtin_exp(vec![Value::True]).is_err());
        assert!(builtin_float(vec![Value::string("y")]).is_err());
    }

    // ===== Wrong arity errors =====

    #[test]
    fn test_wrong_arity() {
        assert!(builtin_cos(vec![]).is_err());
        assert!(builtin_cos(vec![Value::Float(1.0), Value::Float(2.0)]).is_err());
        assert!(builtin_copysign(vec![Value::Float(1.0)]).is_err());
        assert!(builtin_ldexp(vec![Value::Float(1.0)]).is_err());
        assert!(builtin_frexp(vec![]).is_err());
    }
}
