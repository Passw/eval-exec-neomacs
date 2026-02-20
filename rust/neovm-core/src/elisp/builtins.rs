//! Built-in primitive functions.
//!
//! All functions here take pre-evaluated `Vec<Value>` arguments and return `EvalResult`.
//! The evaluator dispatches here after evaluating the argument expressions.

use super::error::{signal, EvalResult, Flow};
use super::string_escape::{
    bytes_to_storage_string, bytes_to_unibyte_storage_string, decode_storage_char_codes,
    encode_nonunicode_char_for_storage, storage_char_len, storage_string_display_width,
    storage_substring,
};
use super::value::*;
use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, OnceLock};
use strum::EnumString;

/// Expect exactly N arguments.
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

/// Expect at least N arguments.
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

/// Expect at most N arguments.
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

fn expect_range_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

/// Extract an integer, signaling wrong-type-argument if not.
fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn expect_fixnum(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), other.clone()],
        )),
    }
}

fn expect_char_table_index(value: &Value) -> Result<i64, Flow> {
    let idx = expect_fixnum(value)?;
    if !(0..=0x3F_FFFF).contains(&idx) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), value.clone()],
        ));
    }
    Ok(idx)
}

fn expect_char_equal_code(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) if (0..=KEY_CHAR_CODE_MASK).contains(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), other.clone()],
        )),
    }
}

fn char_equal_folded(code: i64) -> Option<String> {
    char::from_u32(code as u32).map(|ch| ch.to_lowercase().collect())
}

/// Extract an integer/marker-ish position value.
///
/// NeoVM does not expose marker values yet, so this currently accepts
/// integers (including char values) and signals with `integer-or-marker-p`.
fn expect_integer_or_marker(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

/// Extract a non-negative integer, signaling `wholenump` on failure.
fn expect_wholenump(value: &Value) -> Result<i64, Flow> {
    let n = match value {
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("wholenump"), value.clone()],
            ))
        }
    };
    if n < 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), value.clone()],
        ));
    }
    Ok(n)
}

enum NumberOrMarker {
    Int(i64),
    Float(f64),
}

fn expect_number_or_marker(value: &Value) -> Result<NumberOrMarker, Flow> {
    match value {
        Value::Int(n) => Ok(NumberOrMarker::Int(*n)),
        Value::Char(c) => Ok(NumberOrMarker::Int(*c as i64)),
        Value::Float(f) => Ok(NumberOrMarker::Float(*f)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

/// Extract a number as f64.
fn expect_number(value: &Value) -> Result<f64, Flow> {
    match value {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(f) => Ok(*f),
        Value::Char(c) => Ok(*c as u32 as f64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

fn expect_number_or_marker_f64(value: &Value) -> Result<f64, Flow> {
    match expect_number_or_marker(value)? {
        NumberOrMarker::Int(n) => Ok(n as f64),
        NumberOrMarker::Float(f) => Ok(f),
    }
}

fn expect_integer_or_marker_after_number_check(value: &Value) -> Result<i64, Flow> {
    match expect_number_or_marker(value)? {
        NumberOrMarker::Int(n) => Ok(n),
        NumberOrMarker::Float(_) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), value.clone()],
        )),
    }
}

/// True if any arg is a float (triggers float arithmetic).
fn has_float(args: &[Value]) -> bool {
    args.iter().any(|v| matches!(v, Value::Float(_)))
}

fn normalize_string_start_arg(string: &str, start: Option<&Value>) -> Result<usize, Flow> {
    let Some(start_val) = start else {
        return Ok(0);
    };
    if start_val.is_nil() {
        return Ok(0);
    }

    let raw_start = expect_int(start_val)?;
    let len = string.chars().count() as i64;
    let normalized = if raw_start < 0 {
        len.checked_add(raw_start)
    } else {
        Some(raw_start)
    };

    let Some(start_idx) = normalized else {
        return Err(signal(
            "args-out-of-range",
            vec![Value::string(string), Value::Int(raw_start)],
        ));
    };

    if !(0..=len).contains(&start_idx) {
        return Err(signal(
            "args-out-of-range",
            vec![Value::string(string), Value::Int(raw_start)],
        ));
    }

    let start_char_idx = start_idx as usize;
    if start_char_idx == len as usize {
        return Ok(string.len());
    }

    Ok(string
        .char_indices()
        .nth(start_char_idx)
        .map(|(byte_idx, _)| byte_idx)
        .unwrap_or(string.len()))
}

fn string_byte_to_char_index(s: &str, byte_idx: usize) -> Option<usize> {
    s.get(..byte_idx).map(|prefix| prefix.chars().count())
}

// ===========================================================================
// Arithmetic
// ===========================================================================

pub(crate) fn builtin_add(args: Vec<Value>) -> EvalResult {
    if has_float(&args) {
        let mut sum = 0.0f64;
        for a in &args {
            sum += expect_number_or_marker_f64(a)?;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum = 0i64;
        for a in &args {
            sum = sum
                .checked_add(expect_integer_or_marker_after_number_check(a)?)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(sum))
    }
}

pub(crate) fn builtin_sub(args: Vec<Value>) -> EvalResult {
    if args.is_empty() {
        return Ok(Value::Int(0));
    }
    if args.len() == 1 {
        // Unary negation
        if has_float(&args) {
            return Ok(Value::Float(-expect_number_or_marker_f64(&args[0])?));
        }
        let n = expect_integer_or_marker_after_number_check(&args[0])?;
        return Ok(Value::Int(
            n.checked_neg()
                .ok_or_else(|| signal("overflow-error", vec![]))?,
        ));
    }
    if has_float(&args) {
        let mut acc = expect_number_or_marker_f64(&args[0])?;
        for a in &args[1..] {
            acc -= expect_number_or_marker_f64(a)?;
        }
        Ok(Value::Float(acc))
    } else {
        let mut acc = expect_integer_or_marker_after_number_check(&args[0])?;
        for a in &args[1..] {
            acc = acc
                .checked_sub(expect_integer_or_marker_after_number_check(a)?)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(acc))
    }
}

pub(crate) fn builtin_mul(args: Vec<Value>) -> EvalResult {
    if has_float(&args) {
        let mut prod = 1.0f64;
        for a in &args {
            prod *= expect_number_or_marker_f64(a)?;
        }
        Ok(Value::Float(prod))
    } else {
        let mut prod = 1i64;
        for a in &args {
            prod = prod
                .checked_mul(expect_integer_or_marker_after_number_check(a)?)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(prod))
    }
}

pub(crate) fn builtin_div(args: Vec<Value>) -> EvalResult {
    expect_min_args("/", &args, 2)?;
    if has_float(&args) {
        let mut acc = expect_number_or_marker_f64(&args[0])?;
        for a in &args[1..] {
            let d = expect_number_or_marker_f64(a)?;
            acc /= d;
            if acc.is_nan() {
                // Emacs prints negative-NaN for float zero-divisor paths.
                acc = f64::from_bits(f64::NAN.to_bits() | (1_u64 << 63));
            }
        }
        Ok(Value::Float(acc))
    } else {
        let mut acc = expect_integer_or_marker_after_number_check(&args[0])?;
        for a in &args[1..] {
            let d = expect_integer_or_marker_after_number_check(a)?;
            if d == 0 {
                return Err(signal("arith-error", vec![]));
            }
            acc = acc
                .checked_div(d)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(acc))
    }
}

pub(crate) fn builtin_percent(args: Vec<Value>) -> EvalResult {
    expect_args("%", &args, 2)?;
    let a = expect_integer_or_marker(&args[0])?;
    let b = expect_integer_or_marker(&args[1])?;
    if b == 0 {
        return Err(signal("arith-error", vec![]));
    }
    Ok(Value::Int(a % b))
}

pub(crate) fn builtin_mod(args: Vec<Value>) -> EvalResult {
    expect_args("mod", &args, 2)?;
    let a_raw = expect_number_or_marker(&args[0])?;
    let b_raw = expect_number_or_marker(&args[1])?;
    match (a_raw, b_raw) {
        (NumberOrMarker::Int(a), NumberOrMarker::Int(b)) => {
            if b == 0 {
                return Err(signal("arith-error", vec![]));
            }
            // Emacs mod: result has sign of divisor.
            let r = a % b;
            let r = if r != 0 && (r < 0) != (b < 0) {
                r + b
            } else {
                r
            };
            Ok(Value::Int(r))
        }
        (a, b) => {
            let a = match a {
                NumberOrMarker::Int(n) => n as f64,
                NumberOrMarker::Float(f) => f,
            };
            let b = match b {
                NumberOrMarker::Int(n) => n as f64,
                NumberOrMarker::Float(f) => f,
            };
            let r = a % b;
            let mut r = if r != 0.0 && (r < 0.0) != (b < 0.0) {
                r + b
            } else {
                r
            };
            if r.is_nan() {
                // Emacs prints negative-NaN for floating mod-by-zero payloads.
                r = f64::from_bits(f64::NAN.to_bits() | (1_u64 << 63));
            }
            Ok(Value::Float(r))
        }
    }
}

pub(crate) fn builtin_add1(args: Vec<Value>) -> EvalResult {
    expect_args("1+", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(
            n.checked_add(1)
                .ok_or_else(|| signal("overflow-error", vec![]))?,
        )),
        Value::Float(f) => Ok(Value::Float(f + 1.0)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_sub1(args: Vec<Value>) -> EvalResult {
    expect_args("1-", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(
            n.checked_sub(1)
                .ok_or_else(|| signal("overflow-error", vec![]))?,
        )),
        Value::Float(f) => Ok(Value::Float(f - 1.0)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_max(args: Vec<Value>) -> EvalResult {
    expect_min_args("max", &args, 1)?;
    let mut best_num = expect_number_or_marker_f64(&args[0])?;
    let mut best_value = args[0].clone();
    for a in &args[1..] {
        let n = expect_number_or_marker_f64(a)?;
        if n > best_num {
            best_num = n;
            best_value = a.clone();
        }
    }
    match best_value {
        Value::Int(_) | Value::Float(_) => Ok(best_value),
        Value::Char(c) => Ok(Value::Int(c as i64)),
        _ => unreachable!("max winner must be numeric"),
    }
}

pub(crate) fn builtin_min(args: Vec<Value>) -> EvalResult {
    expect_min_args("min", &args, 1)?;
    let mut best_num = expect_number_or_marker_f64(&args[0])?;
    let mut best_value = args[0].clone();
    for a in &args[1..] {
        let n = expect_number_or_marker_f64(a)?;
        if n < best_num {
            best_num = n;
            best_value = a.clone();
        }
    }
    match best_value {
        Value::Int(_) | Value::Float(_) => Ok(best_value),
        Value::Char(c) => Ok(Value::Int(c as i64)),
        _ => unreachable!("min winner must be numeric"),
    }
}

pub(crate) fn builtin_abs(args: Vec<Value>) -> EvalResult {
    expect_args("abs", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(
            n.checked_abs()
                .ok_or_else(|| signal("overflow-error", vec![]))?,
        )),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

// ===========================================================================
// Logical / bitwise
// ===========================================================================

pub(crate) fn builtin_logand(args: Vec<Value>) -> EvalResult {
    let mut acc = -1i64; // all bits set
    for a in &args {
        acc &= expect_integer_or_marker_after_number_check(a)?;
    }
    Ok(Value::Int(acc))
}

pub(crate) fn builtin_logior(args: Vec<Value>) -> EvalResult {
    let mut acc = 0i64;
    for a in &args {
        acc |= expect_integer_or_marker_after_number_check(a)?;
    }
    Ok(Value::Int(acc))
}

pub(crate) fn builtin_logxor(args: Vec<Value>) -> EvalResult {
    let mut acc = 0i64;
    for a in &args {
        acc ^= expect_integer_or_marker_after_number_check(a)?;
    }
    Ok(Value::Int(acc))
}

pub(crate) fn builtin_lognot(args: Vec<Value>) -> EvalResult {
    expect_args("lognot", &args, 1)?;
    Ok(Value::Int(!expect_int(&args[0])?))
}

pub(crate) fn builtin_ash(args: Vec<Value>) -> EvalResult {
    expect_args("ash", &args, 2)?;
    let n = expect_int(&args[0])?;
    let count = expect_int(&args[1])?;
    if count >= 0 {
        let shift = u32::try_from(count).unwrap_or(u32::MAX);
        Ok(Value::Int(n.checked_shl(shift).unwrap_or(0)))
    } else {
        let shift = count.unsigned_abs().min(63) as u32;
        Ok(Value::Int(n >> shift))
    }
}

// ===========================================================================
// Comparisons
// ===========================================================================

pub(crate) fn builtin_num_eq(args: Vec<Value>) -> EvalResult {
    expect_min_args("=", &args, 2)?;
    let first = expect_number_or_marker_f64(&args[0])?;
    for a in &args[1..] {
        if expect_number_or_marker_f64(a)? != first {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_lt(args: Vec<Value>) -> EvalResult {
    expect_min_args("<", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number_or_marker_f64(&pair[0])? < expect_number_or_marker_f64(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_le(args: Vec<Value>) -> EvalResult {
    expect_min_args("<=", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number_or_marker_f64(&pair[0])? <= expect_number_or_marker_f64(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_gt(args: Vec<Value>) -> EvalResult {
    expect_min_args(">", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number_or_marker_f64(&pair[0])? > expect_number_or_marker_f64(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_ge(args: Vec<Value>) -> EvalResult {
    expect_min_args(">=", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number_or_marker_f64(&pair[0])? >= expect_number_or_marker_f64(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_ne(args: Vec<Value>) -> EvalResult {
    expect_args("/=", &args, 2)?;
    let a = expect_number_or_marker_f64(&args[0])?;
    let b = expect_number_or_marker_f64(&args[1])?;
    Ok(Value::bool(a != b))
}

// ===========================================================================
// Type predicates
// ===========================================================================

pub(crate) fn builtin_null(args: Vec<Value>) -> EvalResult {
    expect_args("null", &args, 1)?;
    Ok(Value::bool(args[0].is_nil()))
}

pub(crate) fn builtin_atom(args: Vec<Value>) -> EvalResult {
    expect_args("atom", &args, 1)?;
    Ok(Value::bool(!args[0].is_cons()))
}

pub(crate) fn builtin_consp(args: Vec<Value>) -> EvalResult {
    expect_args("consp", &args, 1)?;
    Ok(Value::bool(args[0].is_cons()))
}

pub(crate) fn builtin_listp(args: Vec<Value>) -> EvalResult {
    expect_args("listp", &args, 1)?;
    Ok(Value::bool(args[0].is_list()))
}

pub(crate) fn builtin_nlistp(args: Vec<Value>) -> EvalResult {
    expect_args("nlistp", &args, 1)?;
    Ok(Value::bool(!args[0].is_list()))
}

pub(crate) fn builtin_symbolp(args: Vec<Value>) -> EvalResult {
    expect_args("symbolp", &args, 1)?;
    Ok(Value::bool(args[0].is_symbol()))
}

pub(crate) fn builtin_numberp(args: Vec<Value>) -> EvalResult {
    expect_args("numberp", &args, 1)?;
    Ok(Value::bool(args[0].is_number()))
}

pub(crate) fn builtin_integerp(args: Vec<Value>) -> EvalResult {
    expect_args("integerp", &args, 1)?;
    Ok(Value::bool(args[0].is_integer()))
}

pub(crate) fn builtin_integer_or_marker_p(args: Vec<Value>) -> EvalResult {
    expect_args("integer-or-marker-p", &args, 1)?;
    let is_integer_or_marker =
        matches!(args[0], Value::Int(_) | Value::Char(_)) || super::marker::is_marker(&args[0]);
    Ok(Value::bool(is_integer_or_marker))
}

pub(crate) fn builtin_number_or_marker_p(args: Vec<Value>) -> EvalResult {
    expect_args("number-or-marker-p", &args, 1)?;
    let is_number_or_marker = matches!(args[0], Value::Int(_) | Value::Float(_) | Value::Char(_))
        || super::marker::is_marker(&args[0]);
    Ok(Value::bool(is_number_or_marker))
}

pub(crate) fn builtin_floatp(args: Vec<Value>) -> EvalResult {
    expect_args("floatp", &args, 1)?;
    Ok(Value::bool(args[0].is_float()))
}

pub(crate) fn builtin_stringp(args: Vec<Value>) -> EvalResult {
    expect_args("stringp", &args, 1)?;
    Ok(Value::bool(args[0].is_string()))
}

pub(crate) fn builtin_vectorp(args: Vec<Value>) -> EvalResult {
    expect_args("vectorp", &args, 1)?;
    Ok(Value::bool(args[0].is_vector()))
}

pub(crate) fn builtin_vector_or_char_table_p(args: Vec<Value>) -> EvalResult {
    expect_args("vector-or-char-table-p", &args, 1)?;
    Ok(Value::bool(
        args[0].is_vector() || super::chartable::is_char_table(&args[0]),
    ))
}

pub(crate) fn builtin_characterp(args: Vec<Value>) -> EvalResult {
    expect_args("characterp", &args, 1)?;
    Ok(Value::bool(args[0].is_char()))
}

pub(crate) fn builtin_functionp(args: Vec<Value>) -> EvalResult {
    expect_args("functionp", &args, 1)?;
    let is_function = match &args[0] {
        Value::Symbol(_) => false,
        Value::Cons(_) => is_lambda_form_list(&args[0]),
        other => is_runtime_function_object(other),
    };
    Ok(Value::bool(is_function))
}

fn is_lambda_form_list(value: &Value) -> bool {
    match value {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            pair.car.as_symbol_name() == Some("lambda")
        }
        _ => false,
    }
}

fn is_macro_marker_list(value: &Value) -> bool {
    match value {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            pair.car.as_symbol_name() == Some("macro")
        }
        _ => false,
    }
}

fn is_runtime_function_object(value: &Value) -> bool {
    match value {
        Value::Lambda(_) | Value::ByteCode(_) => true,
        Value::Subr(name) => !super::subr_info::is_special_form(name),
        _ => false,
    }
}

fn autoload_type_of(value: &Value) -> Option<super::autoload::AutoloadType> {
    if !super::autoload::is_autoload_value(value) {
        return None;
    }
    let items = list_to_vec(value)?;
    let type_value = items.get(4).cloned().unwrap_or(Value::Nil);
    Some(super::autoload::AutoloadType::from_value(&type_value))
}

pub(crate) fn builtin_functionp_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("functionp", &args, 1)?;
    let is_function = if let Some(name) = args[0].as_symbol_name() {
        if let Some(function) = resolve_indirect_symbol(eval, name) {
            if let Some(autoload_type) = autoload_type_of(&function) {
                matches!(autoload_type, super::autoload::AutoloadType::Function)
            } else {
                is_runtime_function_object(&function)
            }
        } else {
            false
        }
    } else {
        match &args[0] {
            Value::Lambda(_) | Value::Subr(_) | Value::ByteCode(_) => {
                is_runtime_function_object(&args[0])
            }
            Value::Cons(_) => !is_macro_marker_list(&args[0]) && is_lambda_form_list(&args[0]),
            _ => false,
        }
    };
    Ok(Value::bool(is_function))
}

pub(crate) fn builtin_keywordp(args: Vec<Value>) -> EvalResult {
    expect_args("keywordp", &args, 1)?;
    Ok(Value::bool(args[0].is_keyword()))
}

pub(crate) fn builtin_hash_table_p(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-p", &args, 1)?;
    Ok(Value::bool(args[0].is_hash_table()))
}

pub(crate) fn builtin_type_of(args: Vec<Value>) -> EvalResult {
    expect_args("type-of", &args, 1)?;
    Ok(Value::symbol(args[0].type_name()))
}

pub(crate) fn builtin_cl_type_of(args: Vec<Value>) -> EvalResult {
    expect_args("cl-type-of", &args, 1)?;
    let name = match &args[0] {
        Value::Nil => "null",
        Value::True => "boolean",
        Value::Int(_) | Value::Char(_) => "fixnum",
        Value::Float(_) => "float",
        Value::Str(_) => "string",
        Value::Symbol(_) | Value::Keyword(_) => "symbol",
        Value::Cons(_) => "cons",
        Value::Vector(_) => "vector",
        Value::HashTable(_) => "hash-table",
        Value::Subr(_) => "primitive-function",
        Value::Lambda(_) | Value::Macro(_) => "interpreted-function",
        Value::ByteCode(_) => "byte-code-function",
        Value::Buffer(_) => "buffer",
        Value::Window(_) => "window",
        Value::Frame(_) => "frame",
        Value::Timer(_) => "timer",
    };
    Ok(Value::symbol(name))
}

pub(crate) fn builtin_sequencep(args: Vec<Value>) -> EvalResult {
    expect_args("sequencep", &args, 1)?;
    let is_seq = args[0].is_list() || args[0].is_vector() || args[0].is_string();
    Ok(Value::bool(is_seq))
}

pub(crate) fn builtin_arrayp(args: Vec<Value>) -> EvalResult {
    expect_args("arrayp", &args, 1)?;
    let is_arr = args[0].is_vector() || args[0].is_string();
    Ok(Value::bool(is_arr))
}

// ===========================================================================
// Equality
// ===========================================================================

pub(crate) fn builtin_eq(args: Vec<Value>) -> EvalResult {
    expect_args("eq", &args, 2)?;
    Ok(Value::bool(eq_value(&args[0], &args[1])))
}

pub(crate) fn builtin_eql(args: Vec<Value>) -> EvalResult {
    expect_args("eql", &args, 2)?;
    Ok(Value::bool(eql_value(&args[0], &args[1])))
}

pub(crate) fn builtin_equal(args: Vec<Value>) -> EvalResult {
    expect_args("equal", &args, 2)?;
    Ok(Value::bool(equal_value(&args[0], &args[1], 0)))
}

pub(crate) fn builtin_function_equal(args: Vec<Value>) -> EvalResult {
    expect_args("function-equal", &args, 2)?;
    Ok(Value::bool(eq_value(&args[0], &args[1])))
}

pub(crate) fn builtin_module_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("module-function-p", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_user_ptrp(args: Vec<Value>) -> EvalResult {
    expect_args("user-ptrp", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_symbol_with_pos_p(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-with-pos-p", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_symbol_with_pos_pos(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-with-pos-pos", &args, 1)?;
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("symbol-with-pos-p"), args[0].clone()],
    ))
}

pub(crate) fn builtin_char_equal(eval: &super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("char-equal", &args, 2)?;
    let left = expect_char_equal_code(&args[0])?;
    let right = expect_char_equal_code(&args[1])?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);
    if !case_fold {
        return Ok(Value::bool(left == right));
    }
    match (char_equal_folded(left), char_equal_folded(right)) {
        (Some(a), Some(b)) => Ok(Value::bool(a == b)),
        _ => Ok(Value::bool(left == right)),
    }
}

pub(crate) fn builtin_not(args: Vec<Value>) -> EvalResult {
    expect_args("not", &args, 1)?;
    Ok(Value::bool(args[0].is_nil()))
}

// ===========================================================================
// Cons / List operations
// ===========================================================================

pub(crate) fn builtin_cons(args: Vec<Value>) -> EvalResult {
    expect_args("cons", &args, 2)?;
    Ok(Value::cons(args[0].clone(), args[1].clone()))
}

fn car_value(value: &Value) -> Result<Value, Flow> {
    match value {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").car.clone()),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), value.clone()],
        )),
    }
}

fn cdr_value(value: &Value) -> Result<Value, Flow> {
    match value {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").cdr.clone()),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), value.clone()],
        )),
    }
}

pub(crate) fn builtin_car(args: Vec<Value>) -> EvalResult {
    expect_args("car", &args, 1)?;
    car_value(&args[0])
}

pub(crate) fn builtin_cdr(args: Vec<Value>) -> EvalResult {
    expect_args("cdr", &args, 1)?;
    cdr_value(&args[0])
}

fn apply_cxr(mut value: Value, ops: &[u8]) -> EvalResult {
    for op in ops {
        value = match op {
            b'a' => car_value(&value)?,
            b'd' => cdr_value(&value)?,
            _ => unreachable!("invalid cxr op"),
        };
    }
    Ok(value)
}

pub(crate) fn builtin_caar(args: Vec<Value>) -> EvalResult {
    expect_args("caar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aa")
}

pub(crate) fn builtin_cadr(args: Vec<Value>) -> EvalResult {
    expect_args("cadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"da")
}

pub(crate) fn builtin_cdar(args: Vec<Value>) -> EvalResult {
    expect_args("cdar", &args, 1)?;
    apply_cxr(args[0].clone(), b"ad")
}

pub(crate) fn builtin_cddr(args: Vec<Value>) -> EvalResult {
    expect_args("cddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"dd")
}

pub(crate) fn builtin_caaar(args: Vec<Value>) -> EvalResult {
    expect_args("caaar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aaa")
}

pub(crate) fn builtin_caadr(args: Vec<Value>) -> EvalResult {
    expect_args("caadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"daa")
}

pub(crate) fn builtin_cadar(args: Vec<Value>) -> EvalResult {
    expect_args("cadar", &args, 1)?;
    apply_cxr(args[0].clone(), b"ada")
}

pub(crate) fn builtin_caddr(args: Vec<Value>) -> EvalResult {
    expect_args("caddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"dda")
}

pub(crate) fn builtin_cdaar(args: Vec<Value>) -> EvalResult {
    expect_args("cdaar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aad")
}

pub(crate) fn builtin_cdadr(args: Vec<Value>) -> EvalResult {
    expect_args("cdadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"dad")
}

pub(crate) fn builtin_cddar(args: Vec<Value>) -> EvalResult {
    expect_args("cddar", &args, 1)?;
    apply_cxr(args[0].clone(), b"add")
}

pub(crate) fn builtin_cdddr(args: Vec<Value>) -> EvalResult {
    expect_args("cdddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"ddd")
}

pub(crate) fn builtin_cadddr(args: Vec<Value>) -> EvalResult {
    expect_args("cadddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"ddda")
}

pub(crate) fn builtin_cddddr(args: Vec<Value>) -> EvalResult {
    expect_args("cddddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"dddd")
}

pub(crate) fn builtin_caaaar(args: Vec<Value>) -> EvalResult {
    expect_args("caaaar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aaaa")
}

pub(crate) fn builtin_caaadr(args: Vec<Value>) -> EvalResult {
    expect_args("caaadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"daaa")
}

pub(crate) fn builtin_caadar(args: Vec<Value>) -> EvalResult {
    expect_args("caadar", &args, 1)?;
    apply_cxr(args[0].clone(), b"adaa")
}

pub(crate) fn builtin_caaddr(args: Vec<Value>) -> EvalResult {
    expect_args("caaddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"ddaa")
}

pub(crate) fn builtin_cadaar(args: Vec<Value>) -> EvalResult {
    expect_args("cadaar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aada")
}

pub(crate) fn builtin_cadadr(args: Vec<Value>) -> EvalResult {
    expect_args("cadadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"dada")
}

pub(crate) fn builtin_caddar(args: Vec<Value>) -> EvalResult {
    expect_args("caddar", &args, 1)?;
    apply_cxr(args[0].clone(), b"adda")
}

pub(crate) fn builtin_cdaaar(args: Vec<Value>) -> EvalResult {
    expect_args("cdaaar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aaad")
}

pub(crate) fn builtin_cdaadr(args: Vec<Value>) -> EvalResult {
    expect_args("cdaadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"daad")
}

pub(crate) fn builtin_cdadar(args: Vec<Value>) -> EvalResult {
    expect_args("cdadar", &args, 1)?;
    apply_cxr(args[0].clone(), b"adad")
}

pub(crate) fn builtin_cdaddr(args: Vec<Value>) -> EvalResult {
    expect_args("cdaddr", &args, 1)?;
    apply_cxr(args[0].clone(), b"ddad")
}

pub(crate) fn builtin_cddaar(args: Vec<Value>) -> EvalResult {
    expect_args("cddaar", &args, 1)?;
    apply_cxr(args[0].clone(), b"aadd")
}

pub(crate) fn builtin_cddadr(args: Vec<Value>) -> EvalResult {
    expect_args("cddadr", &args, 1)?;
    apply_cxr(args[0].clone(), b"dadd")
}

pub(crate) fn builtin_cdddar(args: Vec<Value>) -> EvalResult {
    expect_args("cdddar", &args, 1)?;
    apply_cxr(args[0].clone(), b"addd")
}

pub(crate) fn builtin_car_safe(args: Vec<Value>) -> EvalResult {
    expect_args("car-safe", &args, 1)?;
    match &args[0] {
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").car.clone()),
        _ => Ok(Value::Nil),
    }
}

pub(crate) fn builtin_cdr_safe(args: Vec<Value>) -> EvalResult {
    expect_args("cdr-safe", &args, 1)?;
    match &args[0] {
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").cdr.clone()),
        _ => Ok(Value::Nil),
    }
}

pub(crate) fn builtin_setcar(args: Vec<Value>) -> EvalResult {
    expect_args("setcar", &args, 2)?;
    match &args[0] {
        Value::Cons(cell) => {
            cell.lock().expect("poisoned").car = args[1].clone();
            Ok(args[1].clone())
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_setcdr(args: Vec<Value>) -> EvalResult {
    expect_args("setcdr", &args, 2)?;
    match &args[0] {
        Value::Cons(cell) => {
            cell.lock().expect("poisoned").cdr = args[1].clone();
            Ok(args[1].clone())
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_list(args: Vec<Value>) -> EvalResult {
    Ok(Value::list(args))
}

pub(crate) fn builtin_length(args: Vec<Value>) -> EvalResult {
    expect_args("length", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Int(0)),
        Value::Cons(_) => match list_length(&args[0]) {
            Some(n) => Ok(Value::Int(n as i64)),
            None => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), args[0].clone()],
            )),
        },
        Value::Str(s) => Ok(Value::Int(storage_char_len(s) as i64)),
        Value::Vector(v) => Ok(Value::Int(vector_sequence_length(&args[0], v))),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), args[0].clone()],
        )),
    }
}

fn vector_sequence_length(
    sequence: &Value,
    vector: &std::sync::Arc<std::sync::Mutex<Vec<Value>>>,
) -> i64 {
    super::chartable::bool_vector_length(sequence)
        .or_else(|| super::chartable::char_table_length(sequence))
        .unwrap_or_else(|| vector.lock().expect("poisoned").len() as i64)
}

fn sequence_length_less_than(sequence: &Value, target: i64) -> Result<bool, Flow> {
    match sequence {
        Value::Nil => Ok(0 < target),
        Value::Str(s) => Ok((storage_char_len(s) as i64) < target),
        Value::Vector(v) => Ok(vector_sequence_length(sequence, v) < target),
        Value::Cons(_) => {
            if target <= 0 {
                return Ok(false);
            }
            let mut remaining = target;
            let mut cursor = sequence.clone();
            while remaining > 0 {
                match cursor {
                    Value::Cons(cell) => {
                        cursor = cell.lock().expect("poisoned").cdr.clone();
                        remaining -= 1;
                    }
                    _ => return Ok(true),
                }
            }
            Ok(false)
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

fn sequence_length_equal(sequence: &Value, target: i64) -> Result<bool, Flow> {
    match sequence {
        Value::Nil => Ok(target == 0),
        Value::Str(s) => Ok((storage_char_len(s) as i64) == target),
        Value::Vector(v) => Ok(vector_sequence_length(sequence, v) == target),
        Value::Cons(_) => {
            if target < 0 {
                return Ok(false);
            }
            let mut remaining = target;
            let mut cursor = sequence.clone();
            while remaining > 0 {
                match cursor {
                    Value::Cons(cell) => {
                        cursor = cell.lock().expect("poisoned").cdr.clone();
                        remaining -= 1;
                    }
                    _ => return Ok(false),
                }
            }
            Ok(!matches!(cursor, Value::Cons(_)))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

fn sequence_length_greater_than(sequence: &Value, target: i64) -> Result<bool, Flow> {
    match sequence {
        Value::Nil => Ok(0 > target),
        Value::Str(s) => Ok((storage_char_len(s) as i64) > target),
        Value::Vector(v) => Ok(vector_sequence_length(sequence, v) > target),
        Value::Cons(_) => {
            if target < 0 {
                return Ok(true);
            }
            if target == i64::MAX {
                return Ok(false);
            }
            let mut remaining = target + 1;
            let mut cursor = sequence.clone();
            while remaining > 0 {
                match cursor {
                    Value::Cons(cell) => {
                        cursor = cell.lock().expect("poisoned").cdr.clone();
                        remaining -= 1;
                    }
                    _ => return Ok(false),
                }
            }
            Ok(true)
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_length_lt(args: Vec<Value>) -> EvalResult {
    expect_args("length<", &args, 2)?;
    let target = expect_fixnum(&args[1])?;
    Ok(Value::bool(sequence_length_less_than(&args[0], target)?))
}

pub(crate) fn builtin_length_eq(args: Vec<Value>) -> EvalResult {
    expect_args("length=", &args, 2)?;
    let target = expect_fixnum(&args[1])?;
    Ok(Value::bool(sequence_length_equal(&args[0], target)?))
}

pub(crate) fn builtin_length_gt(args: Vec<Value>) -> EvalResult {
    expect_args("length>", &args, 2)?;
    let target = expect_fixnum(&args[1])?;
    Ok(Value::bool(sequence_length_greater_than(&args[0], target)?))
}

pub(crate) fn builtin_nth(args: Vec<Value>) -> EvalResult {
    expect_args("nth", &args, 2)?;
    let n = expect_int(&args[0])?;
    let tail = nthcdr_impl(n, args[1].clone())?;
    match tail {
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").car.clone()),
        Value::Nil => Ok(Value::Nil),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), other],
        )),
    }
}

fn nthcdr_impl(n: i64, list: Value) -> EvalResult {
    if n <= 0 {
        return Ok(list);
    }

    let mut cursor = list.clone();
    for _ in 0..(n as usize) {
        match cursor {
            Value::Cons(cell) => {
                cursor = cell.lock().expect("poisoned").cdr.clone();
            }
            Value::Nil => return Ok(Value::Nil),
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
    Ok(cursor)
}

pub(crate) fn builtin_nthcdr(args: Vec<Value>) -> EvalResult {
    expect_args("nthcdr", &args, 2)?;
    let n = expect_int(&args[0])?;
    nthcdr_impl(n, args[1].clone())
}

pub(crate) fn builtin_append(args: Vec<Value>) -> EvalResult {
    fn extend_from_proper_list(out: &mut Vec<Value>, list: &Value) -> Result<(), Flow> {
        let mut cursor = list.clone();
        loop {
            match cursor {
                Value::Nil => return Ok(()),
                Value::Cons(cell) => {
                    let pair = cell.lock().expect("poisoned");
                    out.push(pair.car.clone());
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
    }

    if args.is_empty() {
        return Ok(Value::Nil);
    }
    if args.len() == 1 {
        return Ok(args[0].clone());
    }

    // Collect all elements from all lists except the last, then use last as tail
    let mut elements: Vec<Value> = Vec::new();
    for arg in &args[..args.len() - 1] {
        match arg {
            Value::Nil => {}
            Value::Cons(_) => extend_from_proper_list(&mut elements, arg)?,
            Value::Vector(v) => elements.extend(v.lock().expect("poisoned").iter().cloned()),
            Value::Str(s) => {
                elements.extend(
                    decode_storage_char_codes(s)
                        .into_iter()
                        .map(|cp| Value::Int(cp as i64)),
                );
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("sequencep"), arg.clone()],
                ))
            }
        }
    }

    let last = &args[args.len() - 1];
    if elements.is_empty() {
        return Ok(last.clone());
    }

    // Build list with last arg as tail (supports improper lists)
    let tail = last.clone();
    Ok(elements
        .into_iter()
        .rev()
        .fold(tail, |acc, item| Value::cons(item, acc)))
}

pub(crate) fn builtin_reverse(args: Vec<Value>) -> EvalResult {
    expect_args("reverse", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            let items = list_to_vec(&args[0]).ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), args[0].clone()],
                )
            })?;
            let mut reversed = items;
            reversed.reverse();
            Ok(Value::list(reversed))
        }
        Value::Vector(v) => {
            let mut items = v.lock().expect("poisoned").clone();
            items.reverse();
            Ok(Value::vector(items))
        }
        Value::Str(s) => {
            let reversed: String = s.chars().rev().collect();
            Ok(Value::string(reversed))
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_nreverse(args: Vec<Value>) -> EvalResult {
    fn dotted_list_prefix(list: &Value) -> Option<Value> {
        let mut cursor = list.clone();
        let mut prefix = Vec::new();
        loop {
            match cursor {
                Value::Cons(cell) => {
                    let pair = cell.lock().expect("poisoned");
                    prefix.push(pair.car.clone());
                    cursor = pair.cdr.clone();
                }
                Value::Nil => return None,
                _ => return Some(Value::list(prefix)),
            }
        }
    }

    expect_args("nreverse", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            // Match Emacs list semantics: reject dotted lists with proper-prefix payload.
            if let Some(prefix) = dotted_list_prefix(&args[0]) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), prefix],
                ));
            }

            let mut prev = Value::Nil;
            let mut current = args[0].clone();
            loop {
                match current {
                    Value::Nil => return Ok(prev),
                    Value::Cons(cell) => {
                        let next = {
                            let mut pair = cell.lock().expect("poisoned");
                            let next = pair.cdr.clone();
                            pair.cdr = prev;
                            next
                        };
                        prev = Value::Cons(cell);
                        current = next;
                    }
                    _ => unreachable!("proper-list check should reject dotted tails"),
                }
            }
        }
        Value::Vector(v) => {
            v.lock().expect("poisoned").reverse();
            Ok(args[0].clone())
        }
        Value::Str(_) => builtin_reverse(args),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_member(args: Vec<Value>) -> EvalResult {
    expect_args("member", &args, 2)?;
    let target = &args[0];
    let list = args[1].clone();
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if equal_value(target, &pair.car, 0) {
                    drop(pair);
                    return Ok(Value::Cons(cell));
                }
                cursor = pair.cdr.clone();
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
}

pub(crate) fn builtin_memq(args: Vec<Value>) -> EvalResult {
    expect_args("memq", &args, 2)?;
    let target = &args[0];
    let list = args[1].clone();
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if eq_value(target, &pair.car) {
                    drop(pair);
                    return Ok(Value::Cons(cell));
                }
                cursor = pair.cdr.clone();
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
}

pub(crate) fn builtin_memql(args: Vec<Value>) -> EvalResult {
    expect_args("memql", &args, 2)?;
    let target = &args[0];
    let list = args[1].clone();
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if eql_value(target, &pair.car) {
                    drop(pair);
                    return Ok(Value::Cons(cell));
                }
                cursor = pair.cdr.clone();
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
}

pub(crate) fn builtin_assoc(args: Vec<Value>) -> EvalResult {
    expect_args("assoc", &args, 2)?;
    let key = &args[0];
    let list = args[1].clone();
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if let Value::Cons(ref entry) = pair.car {
                    let entry_pair = entry.lock().expect("poisoned");
                    if equal_value(key, &entry_pair.car, 0) {
                        return Ok(pair.car.clone());
                    }
                }
                cursor = pair.cdr.clone();
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
}

pub(crate) fn builtin_assoc_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("assoc", &args, 2, 3)?;
    let key = &args[0];
    let list = args[1].clone();
    let test_fn = args.get(2).and_then(|value| {
        if value.is_nil() {
            None
        } else {
            Some(value.clone())
        }
    });
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if let Value::Cons(ref entry) = pair.car {
                    let entry_pair = entry.lock().expect("poisoned");
                    let matches = if let Some(test_fn) = &test_fn {
                        eval.apply(test_fn.clone(), vec![key.clone(), entry_pair.car.clone()])?
                            .is_truthy()
                    } else {
                        equal_value(key, &entry_pair.car, 0)
                    };
                    if matches {
                        return Ok(pair.car.clone());
                    }
                }
                cursor = pair.cdr.clone();
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
}

pub(crate) fn builtin_assq(args: Vec<Value>) -> EvalResult {
    expect_args("assq", &args, 2)?;
    let key = &args[0];
    let list = args[1].clone();
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if let Value::Cons(ref entry) = pair.car {
                    let entry_pair = entry.lock().expect("poisoned");
                    if eq_value(key, &entry_pair.car) {
                        return Ok(pair.car.clone());
                    }
                }
                cursor = pair.cdr.clone();
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), list],
                ))
            }
        }
    }
}

pub(crate) fn builtin_copy_sequence(args: Vec<Value>) -> EvalResult {
    expect_args("copy-sequence", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            let mut items = Vec::new();
            let mut cursor = args[0].clone();
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let pair = cell.lock().expect("poisoned");
                        items.push(pair.car.clone());
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
            Ok(Value::list(items))
        }
        Value::Str(s) => Ok(Value::string((**s).clone())),
        Value::Vector(v) => Ok(Value::vector(v.lock().expect("poisoned").clone())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

// ===========================================================================
// String operations
// ===========================================================================

pub(crate) fn builtin_string_equal(args: Vec<Value>) -> EvalResult {
    expect_args("string-equal", &args, 2)?;
    let a = expect_string(&args[0])?;
    let b = expect_string(&args[1])?;
    Ok(Value::bool(a == b))
}

pub(crate) fn builtin_string_lessp(args: Vec<Value>) -> EvalResult {
    expect_args("string-lessp", &args, 2)?;
    let a = expect_string(&args[0])?;
    let b = expect_string(&args[1])?;
    Ok(Value::bool(a < b))
}

fn substring_impl(name: &str, args: &[Value]) -> EvalResult {
    expect_min_args(name, args, 1)?;
    expect_max_args(name, args, 3)?;
    let s = expect_string(&args[0])?;
    let len = storage_char_len(&s) as i64;

    let normalize_index = |value: &Value, default: i64| -> Result<i64, Flow> {
        let raw = if value.is_nil() {
            default
        } else {
            expect_int(value)?
        };
        let idx = if raw < 0 { len + raw } else { raw };
        if idx < 0 || idx > len {
            return Err(signal(
                "args-out-of-range",
                vec![
                    args[0].clone(),
                    args[1].clone(),
                    args.get(2).cloned().unwrap_or(Value::Nil),
                ],
            ));
        }
        Ok(idx)
    };

    let from = if args.len() > 1 {
        normalize_index(&args[1], 0)?
    } else {
        0
    } as usize;

    let to = if args.len() > 2 {
        normalize_index(&args[2], len)?
    } else {
        len
    } as usize;

    if from > to {
        return Err(signal(
            "args-out-of-range",
            vec![
                args[0].clone(),
                args.get(1).cloned().unwrap_or(Value::Int(0)),
                args.get(2).cloned().unwrap_or(Value::Nil),
            ],
        ));
    }
    let result = storage_substring(&s, from, to).ok_or_else(|| {
        signal(
            "args-out-of-range",
            vec![
                args[0].clone(),
                args.get(1).cloned().unwrap_or(Value::Int(0)),
                args.get(2).cloned().unwrap_or(Value::Nil),
            ],
        )
    })?;
    Ok(Value::string(result))
}

pub(crate) fn builtin_substring(args: Vec<Value>) -> EvalResult {
    substring_impl("substring", &args)
}

pub(crate) fn builtin_substring_no_properties(args: Vec<Value>) -> EvalResult {
    substring_impl("substring-no-properties", &args)
}

pub(crate) fn builtin_concat(args: Vec<Value>) -> EvalResult {
    fn push_concat_int(result: &mut String, n: i64) -> Result<(), Flow> {
        if !(0..=0x3FFFFF).contains(&n) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), Value::Int(n)],
            ));
        }

        let cp = n as u32;
        if let Some(c) = char::from_u32(cp) {
            result.push(c);
            return Ok(());
        }

        // Emacs concat path for raw-byte non-Unicode chars uses byte->multibyte encoding.
        if (0x3FFF00..=0x3FFFFF).contains(&cp) {
            let b = (cp - 0x3FFF00) as u8;
            let bytes = if b < 0x80 {
                vec![b]
            } else {
                vec![0xC0 | ((b >> 6) & 0x01), 0x80 | (b & 0x3F)]
            };
            result.push_str(&bytes_to_storage_string(&bytes));
            return Ok(());
        }

        if let Some(encoded) = encode_nonunicode_char_for_storage(cp) {
            result.push_str(&encoded);
            return Ok(());
        }

        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), Value::Int(n)],
        ))
    }

    fn push_concat_element(result: &mut String, value: &Value) -> Result<(), Flow> {
        match value {
            Value::Char(c) => {
                result.push(*c);
                Ok(())
            }
            Value::Int(n) => push_concat_int(result, *n),
            other => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), other.clone()],
            )),
        }
    }

    let mut result = String::new();
    for arg in &args {
        match arg {
            Value::Str(s) => result.push_str(s),
            Value::Nil => {}
            Value::Cons(_) => {
                let mut cursor = arg.clone();
                loop {
                    match cursor {
                        Value::Nil => break,
                        Value::Cons(cell) => {
                            let pair = cell.lock().expect("poisoned");
                            push_concat_element(&mut result, &pair.car)?;
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
            }
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                for item in items.iter() {
                    push_concat_element(&mut result, item)?;
                }
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("sequencep"), arg.clone()],
                ))
            }
        }
    }
    Ok(Value::string(result))
}

pub(crate) fn builtin_string_to_number(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-to-number", &args, 1)?;
    expect_max_args("string-to-number", &args, 2)?;
    let s = expect_string(&args[0])?;
    let base = if args.len() > 1 {
        expect_int(&args[1])?
    } else {
        10
    };

    if base < 2 || base > 16 {
        return Err(signal("args-out-of-range", vec![Value::Int(base)]));
    }

    let s = s.trim_start();
    if base == 10 {
        let number_prefix =
            regex::Regex::new(r"^[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?")
                .expect("number prefix regexp should compile");
        if let Some(m) = number_prefix.find(s) {
            let token = m.as_str();
            let is_float = token.contains('.') || token.contains('e') || token.contains('E');
            if is_float {
                if let Ok(f) = token.parse::<f64>() {
                    return Ok(Value::Float(f));
                }
            } else if let Ok(n) = token.parse::<i64>() {
                return Ok(Value::Int(n));
            }
        }
    } else {
        let bytes = s.as_bytes();
        let mut pos = 0usize;
        let mut negative = false;
        if pos < bytes.len() {
            if bytes[pos] == b'+' {
                pos += 1;
            } else if bytes[pos] == b'-' {
                negative = true;
                pos += 1;
            }
        }
        let digit_start = pos;
        while pos < bytes.len() {
            let ch = bytes[pos] as char;
            let Some(d) = ch.to_digit(36) else { break };
            if (d as i64) < base {
                pos += 1;
            } else {
                break;
            }
        }
        if pos > digit_start {
            let token = &s[digit_start..pos];
            if let Ok(parsed) = i64::from_str_radix(token, base as u32) {
                return Ok(Value::Int(if negative { -parsed } else { parsed }));
            }
        }
    }
    Ok(Value::Int(0))
}

pub(crate) fn builtin_number_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("number-to-string", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::string(n.to_string())),
        Value::Float(f) => Ok(Value::string(format!("{}", f))),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_upcase(args: Vec<Value>) -> EvalResult {
    expect_args("upcase", &args, 1)?;
    match &args[0] {
        Value::Str(s) => Ok(Value::string(s.to_uppercase())),
        Value::Char(c) => Ok(Value::Char(c.to_uppercase().next().unwrap_or(*c))),
        Value::Int(n) => {
            if let Some(c) = char::from_u32(*n as u32) {
                Ok(Value::Int(c.to_uppercase().next().unwrap_or(c) as i64))
            } else {
                Ok(Value::Int(*n))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_downcase(args: Vec<Value>) -> EvalResult {
    expect_args("downcase", &args, 1)?;
    match &args[0] {
        Value::Str(s) => Ok(Value::string(s.to_lowercase())),
        Value::Char(c) => Ok(Value::Char(c.to_lowercase().next().unwrap_or(*c))),
        Value::Int(n) => {
            if let Some(c) = char::from_u32(*n as u32) {
                Ok(Value::Int(c.to_lowercase().next().unwrap_or(c) as i64))
            } else {
                Ok(Value::Int(*n))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_format(args: Vec<Value>) -> EvalResult {
    expect_min_args("format", &args, 1)?;
    let fmt_str = expect_string(&args[0])?;
    let mut result = String::new();
    let mut arg_idx = 1;
    let mut chars = fmt_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '%' {
            if let Some(&spec) = chars.peek() {
                chars.next();
                match spec {
                    's' => {
                        if arg_idx < args.len() {
                            match &args[arg_idx] {
                                Value::Str(s) => result.push_str(s),
                                other => result.push_str(&super::print::print_value(other)),
                            }
                            arg_idx += 1;
                        }
                    }
                    'S' => {
                        if arg_idx < args.len() {
                            result.push_str(&super::print::print_value(&args[arg_idx]));
                            arg_idx += 1;
                        }
                    }
                    'd' => {
                        if arg_idx < args.len() {
                            if let Ok(n) = expect_int(&args[arg_idx]) {
                                result.push_str(&n.to_string());
                            }
                            arg_idx += 1;
                        }
                    }
                    'f' => {
                        if arg_idx < args.len() {
                            if let Ok(f) = expect_number(&args[arg_idx]) {
                                result.push_str(&format!("{:.6}", f));
                            }
                            arg_idx += 1;
                        }
                    }
                    'c' => {
                        if arg_idx < args.len() {
                            if let Ok(n) = expect_int(&args[arg_idx]) {
                                if let Some(c) = char::from_u32(n as u32) {
                                    result.push(c);
                                }
                            }
                            arg_idx += 1;
                        }
                    }
                    '%' => result.push('%'),
                    _ => {
                        result.push('%');
                        result.push(spec);
                    }
                }
            } else {
                result.push('%');
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::string(result))
}

pub(crate) fn builtin_format_message(args: Vec<Value>) -> EvalResult {
    expect_min_args("format-message", &args, 1)?;
    builtin_format(args)
}

pub(crate) fn builtin_ngettext(args: Vec<Value>) -> EvalResult {
    expect_args("ngettext", &args, 3)?;
    let singular = expect_strict_string(&args[0])?;
    let plural = expect_strict_string(&args[1])?;
    let count = expect_int(&args[2])?;
    if count == 1 {
        Ok(Value::string(singular))
    } else {
        Ok(Value::string(plural))
    }
}

pub(crate) fn builtin_format_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("format", &args, 1)?;
    let fmt_str = expect_string(&args[0])?;
    let mut result = String::new();
    let mut arg_idx = 1;
    let mut chars = fmt_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '%' {
            if let Some(&spec) = chars.peek() {
                chars.next();
                match spec {
                    's' => {
                        if arg_idx < args.len() {
                            match &args[arg_idx] {
                                Value::Str(s) => result.push_str(s),
                                other => result.push_str(&print_value_eval(eval, other)),
                            }
                            arg_idx += 1;
                        }
                    }
                    'S' => {
                        if arg_idx < args.len() {
                            result.push_str(&print_value_eval(eval, &args[arg_idx]));
                            arg_idx += 1;
                        }
                    }
                    'd' => {
                        if arg_idx < args.len() {
                            if let Ok(n) = expect_int(&args[arg_idx]) {
                                result.push_str(&n.to_string());
                            }
                            arg_idx += 1;
                        }
                    }
                    'f' => {
                        if arg_idx < args.len() {
                            if let Ok(f) = expect_number(&args[arg_idx]) {
                                result.push_str(&format!("{:.6}", f));
                            }
                            arg_idx += 1;
                        }
                    }
                    'c' => {
                        if arg_idx < args.len() {
                            if let Ok(n) = expect_int(&args[arg_idx]) {
                                if let Some(c) = char::from_u32(n as u32) {
                                    result.push(c);
                                }
                            }
                            arg_idx += 1;
                        }
                    }
                    '%' => result.push('%'),
                    _ => {
                        result.push('%');
                        result.push(spec);
                    }
                }
            } else {
                result.push('%');
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::string(result))
}

pub(crate) fn builtin_format_message_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("format-message", &args, 1)?;
    builtin_format_eval(eval, args)
}

// ===========================================================================
// Vector operations
// ===========================================================================

pub(crate) fn builtin_make_vector(args: Vec<Value>) -> EvalResult {
    expect_args("make-vector", &args, 2)?;
    let len = expect_wholenump(&args[0])? as usize;
    Ok(Value::vector(vec![args[1].clone(); len]))
}

pub(crate) fn builtin_vector(args: Vec<Value>) -> EvalResult {
    Ok(Value::vector(args))
}

pub(crate) fn builtin_aref(args: Vec<Value>) -> EvalResult {
    expect_args("aref", &args, 2)?;
    let idx_fixnum = expect_fixnum(&args[1])?;
    match &args[0] {
        Value::Vector(_) if super::chartable::is_char_table(&args[0]) => {
            let ch = expect_char_table_index(&args[1])?;
            super::chartable::builtin_char_table_range(vec![args[0].clone(), Value::Int(ch)])
        }
        Value::Vector(v) => {
            let idx = idx_fixnum as usize;
            let items = v.lock().expect("poisoned");
            let is_bool_vector =
                items.len() >= 2 && matches!(&items[0], Value::Symbol(s) if s == "--bool-vector--");
            if is_bool_vector {
                let len = match items.get(1) {
                    Some(Value::Int(n)) if *n >= 0 => *n as usize,
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("bool-vector-p"), args[0].clone()],
                        ));
                    }
                };
                if idx >= len {
                    return Err(signal(
                        "args-out-of-range",
                        vec![args[0].clone(), args[1].clone()],
                    ));
                }
                let bit = items.get(idx + 2).cloned().ok_or_else(|| {
                    signal("args-out-of-range", vec![args[0].clone(), args[1].clone()])
                })?;
                let truthy = match bit {
                    Value::Int(n) => n != 0,
                    Value::Nil => false,
                    other => other.is_truthy(),
                };
                return Ok(Value::bool(truthy));
            }
            items
                .get(idx)
                .cloned()
                .ok_or_else(|| signal("args-out-of-range", vec![args[0].clone(), args[1].clone()]))
        }
        Value::Str(s) => {
            let idx = idx_fixnum as usize;
            let codes = decode_storage_char_codes(s);
            codes
                .get(idx)
                .map(|cp| Value::Int(*cp as i64))
                .ok_or_else(|| signal("args-out-of-range", vec![args[0].clone(), args[1].clone()]))
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), args[0].clone()],
        )),
    }
}

pub(crate) fn aset_string_replacement(
    array: &Value,
    index: &Value,
    new_element: &Value,
) -> Result<Value, Flow> {
    let Value::Str(original) = array else {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), array.clone()],
        ));
    };

    let idx = expect_fixnum(index)? as usize;
    let mut codes = decode_storage_char_codes(original);
    if idx >= codes.len() {
        return Err(signal(
            "args-out-of-range",
            vec![array.clone(), index.clone()],
        ));
    }

    let replacement_code = insert_char_code_from_value(new_element)? as u32;
    codes[idx] = replacement_code;

    let mut rebuilt = String::new();
    for code in codes {
        if let Some(ch) = char::from_u32(code) {
            rebuilt.push(ch);
        } else if let Some(encoded) = encode_nonunicode_char_for_storage(code) {
            rebuilt.push_str(&encoded);
        } else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), new_element.clone()],
            ));
        }
    }
    Ok(Value::string(rebuilt))
}

pub(crate) fn builtin_aset(args: Vec<Value>) -> EvalResult {
    expect_args("aset", &args, 3)?;
    match &args[0] {
        Value::Vector(_) if super::chartable::is_char_table(&args[0]) => {
            let ch = expect_char_table_index(&args[1])?;
            super::chartable::builtin_set_char_table_range(vec![
                args[0].clone(),
                Value::Int(ch),
                args[2].clone(),
            ])
        }
        Value::Vector(v) => {
            let idx = expect_fixnum(&args[1])? as usize;
            let mut items = v.lock().expect("poisoned");
            let is_bool_vector =
                items.len() >= 2 && matches!(&items[0], Value::Symbol(s) if s == "--bool-vector--");
            if is_bool_vector {
                let len = match items.get(1) {
                    Some(Value::Int(n)) if *n >= 0 => *n as usize,
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("bool-vector-p"), args[0].clone()],
                        ));
                    }
                };
                if idx >= len {
                    return Err(signal(
                        "args-out-of-range",
                        vec![args[0].clone(), args[1].clone()],
                    ));
                }
                let store_idx = idx + 2;
                if store_idx >= items.len() {
                    return Err(signal(
                        "args-out-of-range",
                        vec![args[0].clone(), args[1].clone()],
                    ));
                }
                items[store_idx] = Value::Int(if args[2].is_truthy() { 1 } else { 0 });
                return Ok(args[2].clone());
            }
            if idx >= items.len() {
                return Err(signal(
                    "args-out-of-range",
                    vec![args[0].clone(), args[1].clone()],
                ));
            }
            items[idx] = args[2].clone();
            Ok(args[2].clone())
        }
        Value::Str(_) => {
            let _updated = aset_string_replacement(&args[0], &args[1], &args[2])?;
            Ok(args[2].clone())
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_vconcat(args: Vec<Value>) -> EvalResult {
    fn extend_from_proper_list(out: &mut Vec<Value>, list: &Value) -> Result<(), Flow> {
        let mut cursor = list.clone();
        loop {
            match cursor {
                Value::Nil => return Ok(()),
                Value::Cons(cell) => {
                    let pair = cell.lock().expect("poisoned");
                    out.push(pair.car.clone());
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
    }

    let mut result = Vec::new();
    for arg in &args {
        match arg {
            Value::Vector(v) => result.extend(v.lock().expect("poisoned").iter().cloned()),
            Value::Str(s) => {
                result.extend(
                    decode_storage_char_codes(s)
                        .into_iter()
                        .map(|cp| Value::Int(cp as i64)),
                );
            }
            Value::Nil => {}
            Value::Cons(_) => extend_from_proper_list(&mut result, arg)?,
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("sequencep"), arg.clone()],
                ))
            }
        }
    }
    Ok(Value::vector(result))
}

// ===========================================================================
// Hash table operations
// ===========================================================================

fn invalid_hash_table_argument_list(arg: Value) -> Flow {
    signal("error", vec![Value::string("Invalid argument list"), arg])
}

pub(crate) fn builtin_make_hash_table(args: Vec<Value>) -> EvalResult {
    let mut test = HashTableTest::Eql;
    let mut size: i64 = 0;
    let mut weakness: Option<HashTableWeakness> = None;
    let mut seen_test = false;
    let mut seen_size = false;
    let mut seen_weakness = false;
    let mut seen_rehash_size = false;
    let mut seen_rehash_threshold = false;

    let mut i = 0;
    while i < args.len() {
        let Value::Keyword(option) = &args[i] else {
            return Err(invalid_hash_table_argument_list(args[i].clone()));
        };

        match option.as_str() {
            ":test" => {
                if seen_test {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                }
                let Some(value) = args.get(i + 1) else {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                };
                seen_test = true;
                match value {
                    Value::Nil => {
                        return Err(signal(
                            "error",
                            vec![Value::string("Invalid hash table test")],
                        ));
                    }
                    _ => {
                        let Some(name) = value.as_symbol_name() else {
                            return Err(signal(
                                "wrong-type-argument",
                                vec![Value::symbol("symbolp"), value.clone()],
                            ));
                        };
                        test = match name {
                            "eq" => HashTableTest::Eq,
                            "eql" => HashTableTest::Eql,
                            "equal" => HashTableTest::Equal,
                            _ => {
                                if let Some(alias_test) =
                                    super::compat_internal::lookup_hash_table_test_alias(name)
                                {
                                    alias_test
                                } else {
                                    return Err(signal(
                                        "error",
                                        vec![
                                            Value::string("Invalid hash table test"),
                                            value.clone(),
                                        ],
                                    ));
                                }
                            }
                        };
                    }
                }
                i += 2;
            }
            ":size" => {
                if seen_size {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                }
                let Some(value) = args.get(i + 1) else {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                };
                seen_size = true;
                size = match value {
                    Value::Nil => 0,
                    Value::Int(n) if *n >= 0 => *n,
                    _ => {
                        return Err(signal(
                            "error",
                            vec![Value::string("Invalid hash table size"), value.clone()],
                        ));
                    }
                };
                i += 2;
            }
            ":weakness" => {
                if seen_weakness {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                }
                let Some(value) = args.get(i + 1) else {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                };
                seen_weakness = true;
                weakness = match value {
                    Value::Nil => None,
                    Value::True => Some(HashTableWeakness::KeyAndValue),
                    _ => {
                        let Some(name) = value.as_symbol_name() else {
                            return Err(signal(
                                "error",
                                vec![Value::string("Invalid hash table weakness"), value.clone()],
                            ));
                        };
                        Some(match name {
                            "key" => HashTableWeakness::Key,
                            "value" => HashTableWeakness::Value,
                            "key-or-value" => HashTableWeakness::KeyOrValue,
                            "key-and-value" => HashTableWeakness::KeyAndValue,
                            _ => {
                                return Err(signal(
                                    "error",
                                    vec![
                                        Value::string("Invalid hash table weakness"),
                                        value.clone(),
                                    ],
                                ));
                            }
                        })
                    }
                };
                i += 2;
            }
            ":rehash-size" => {
                if seen_rehash_size {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                }
                seen_rehash_size = true;
                if i + 1 >= args.len() {
                    i += 1;
                } else if matches!(
                    &args[i + 1],
                    Value::Keyword(option) if matches!(
                        option.as_str(),
                        ":test" | ":size" | ":weakness" | ":rehash-size" | ":rehash-threshold"
                    )
                ) {
                    i += 1;
                } else {
                    i += 2;
                }
                continue;
            }
            ":rehash-threshold" => {
                if seen_rehash_threshold {
                    return Err(invalid_hash_table_argument_list(args[i].clone()));
                }
                seen_rehash_threshold = true;
                if i + 1 >= args.len() {
                    i += 1;
                } else if matches!(
                    &args[i + 1],
                    Value::Keyword(option) if matches!(
                        option.as_str(),
                        ":test" | ":size" | ":weakness" | ":rehash-size" | ":rehash-threshold"
                    )
                ) {
                    i += 1;
                } else {
                    i += 2;
                }
                continue;
            }
            _ => return Err(invalid_hash_table_argument_list(args[i].clone())),
        }
    }
    Ok(Value::hash_table_with_options(
        test, size, weakness, 1.5, 0.8125,
    ))
}

pub(crate) fn builtin_gethash(args: Vec<Value>) -> EvalResult {
    expect_min_args("gethash", &args, 2)?;
    let default = if args.len() > 2 {
        args[2].clone()
    } else {
        Value::Nil
    };
    match &args[1] {
        Value::HashTable(ht) => {
            let ht = ht.lock().expect("poisoned");
            let key = args[0].to_hash_key(&ht.test);
            Ok(ht.data.get(&key).cloned().unwrap_or(default))
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), args[1].clone()],
        )),
    }
}

pub(crate) fn builtin_puthash(args: Vec<Value>) -> EvalResult {
    expect_args("puthash", &args, 3)?;
    match &args[2] {
        Value::HashTable(ht) => {
            let mut ht = ht.lock().expect("poisoned");
            let key = args[0].to_hash_key(&ht.test);
            ht.data.insert(key, args[1].clone());
            Ok(args[1].clone())
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), args[2].clone()],
        )),
    }
}

pub(crate) fn builtin_remhash(args: Vec<Value>) -> EvalResult {
    expect_args("remhash", &args, 2)?;
    match &args[1] {
        Value::HashTable(ht) => {
            let mut ht = ht.lock().expect("poisoned");
            let key = args[0].to_hash_key(&ht.test);
            ht.data.remove(&key);
            Ok(Value::Nil)
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), args[1].clone()],
        )),
    }
}

pub(crate) fn builtin_clrhash(args: Vec<Value>) -> EvalResult {
    expect_args("clrhash", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            ht.lock().expect("poisoned").data.clear();
            Ok(Value::Nil)
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_hash_table_count(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-count", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => Ok(Value::Int(ht.lock().expect("poisoned").data.len() as i64)),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), args[0].clone()],
        )),
    }
}

// ===========================================================================
// Conversion
// ===========================================================================

pub(crate) fn builtin_float(args: Vec<Value>) -> EvalResult {
    expect_args("float", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::Float(f) => Ok(Value::Float(*f)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_truncate(args: Vec<Value>) -> EvalResult {
    expect_args("truncate", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(*f as i64)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_floor(args: Vec<Value>) -> EvalResult {
    expect_args("floor", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_ceiling(args: Vec<Value>) -> EvalResult {
    expect_args("ceiling", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_round(args: Vec<Value>) -> EvalResult {
    expect_args("round", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.round_ties_even() as i64)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_char_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("char-to-string", &args, 1)?;
    match &args[0] {
        Value::Char(c) => Ok(Value::string(c.to_string())),
        Value::Int(n) => {
            if *n < 0 {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), args[0].clone()],
                ));
            }
            if let Some(c) = char::from_u32(*n as u32) {
                Ok(Value::string(c.to_string()))
            } else if let Some(encoded) = encode_nonunicode_char_for_storage(*n as u32) {
                Ok(Value::string(encoded))
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), args[0].clone()],
                ))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_string_to_char(args: Vec<Value>) -> EvalResult {
    expect_args("string-to-char", &args, 1)?;
    let s = expect_string(&args[0])?;
    let first = decode_storage_char_codes(&s)
        .into_iter()
        .next()
        .unwrap_or(0);
    Ok(Value::Int(first as i64))
}

// ===========================================================================
// Property lists
// ===========================================================================

pub(crate) fn builtin_plist_get(args: Vec<Value>) -> EvalResult {
    expect_args("plist-get", &args, 2)?;
    let mut cursor = args[0].clone();
    loop {
        match cursor {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if eq_value(&pair.car, &args[1]) {
                    // Next element is the value
                    match &pair.cdr {
                        Value::Cons(val_cell) => {
                            return Ok(val_cell.lock().expect("poisoned").car.clone());
                        }
                        _ => return Ok(Value::Nil),
                    }
                }
                // Skip the value entry
                match &pair.cdr {
                    Value::Cons(val_cell) => {
                        cursor = val_cell.lock().expect("poisoned").cdr.clone();
                    }
                    _ => return Ok(Value::Nil),
                }
            }
            _ => return Ok(Value::Nil),
        }
    }
}

pub(crate) fn builtin_plist_put(args: Vec<Value>) -> EvalResult {
    expect_args("plist-put", &args, 3)?;
    let plist = args[0].clone();
    let key = args[1].clone();
    let new_val = args[2].clone();

    if plist.is_nil() {
        return Ok(Value::list(vec![key, new_val]));
    }

    let mut cursor = plist.clone();
    let mut last_value_cell = None;

    loop {
        match cursor {
            Value::Cons(key_cell) => {
                let (entry_key, entry_rest) = {
                    let pair = key_cell.lock().expect("poisoned");
                    (pair.car.clone(), pair.cdr.clone())
                };

                match entry_rest {
                    Value::Cons(value_cell) => {
                        if eq_value(&entry_key, &key) {
                            value_cell.lock().expect("poisoned").car = new_val.clone();
                            return Ok(plist);
                        }
                        cursor = value_cell.lock().expect("poisoned").cdr.clone();
                        last_value_cell = Some(value_cell);
                    }
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("plistp"), plist],
                        ))
                    }
                }
            }
            Value::Nil => {
                if let Some(value_cell) = last_value_cell {
                    value_cell.lock().expect("poisoned").cdr =
                        Value::cons(key, Value::cons(new_val, Value::Nil));
                    return Ok(plist);
                }
                return Ok(Value::list(vec![key, new_val]));
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("plistp"), plist],
                ))
            }
        }
    }
}

pub(crate) fn builtin_plist_member(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("plist-member", &args, 2, 3)?;
    let plist = args[0].clone();
    let prop = args[1].clone();
    let predicate = args.get(2).and_then(|value| {
        if value.is_nil() {
            None
        } else {
            Some(value.clone())
        }
    });

    let mut cursor = plist.clone();
    loop {
        match cursor {
            Value::Cons(key_cell) => {
                let (entry_key, entry_rest) = {
                    let pair = key_cell.lock().expect("poisoned");
                    (pair.car.clone(), pair.cdr.clone())
                };

                let matches = if let Some(predicate) = &predicate {
                    eval.apply(predicate.clone(), vec![entry_key.clone(), prop.clone()])?
                        .is_truthy()
                } else {
                    eq_value(&entry_key, &prop)
                };
                if matches {
                    return Ok(Value::Cons(key_cell));
                }

                match entry_rest {
                    Value::Cons(value_cell) => {
                        cursor = value_cell.lock().expect("poisoned").cdr.clone();
                    }
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("plistp"), plist],
                        ))
                    }
                }
            }
            Value::Nil => return Ok(Value::Nil),
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("plistp"), plist],
                ))
            }
        }
    }
}

// ===========================================================================
// Misc
// ===========================================================================

pub(crate) fn builtin_identity(args: Vec<Value>) -> EvalResult {
    expect_args("identity", &args, 1)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_purecopy(args: Vec<Value>) -> EvalResult {
    expect_args("purecopy", &args, 1)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_prefix_numeric_value(args: Vec<Value>) -> EvalResult {
    expect_args("prefix-numeric-value", &args, 1)?;
    let numeric = match &args[0] {
        Value::Nil => 1,
        Value::Symbol(name) if name == "-" => -1,
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        Value::Cons(cell) => cell.lock().expect("poisoned").car.as_int().unwrap_or(1),
        _ => 1,
    };
    Ok(Value::Int(numeric))
}

pub(crate) fn builtin_ignore(_args: Vec<Value>) -> EvalResult {
    Ok(Value::Nil)
}

pub(crate) fn builtin_message(args: Vec<Value>) -> EvalResult {
    expect_min_args("message", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        match &args[0] {
            Value::Str(s) => (**s).clone(),
            other => super::print::print_value(other),
        }
    } else {
        // Use format
        match builtin_format(args.clone())? {
            Value::Str(s) => (*s).clone(),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_box(args: Vec<Value>) -> EvalResult {
    expect_min_args("message-box", &args, 1)?;
    builtin_message(args)
}

pub(crate) fn builtin_message_or_box(args: Vec<Value>) -> EvalResult {
    expect_min_args("message-or-box", &args, 1)?;
    builtin_message(args)
}

pub(crate) fn builtin_message_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("message", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        match &args[0] {
            Value::Str(s) => (**s).clone(),
            other => print_value_eval(eval, other),
        }
    } else {
        match builtin_format_eval(eval, args.clone())? {
            Value::Str(s) => (*s).clone(),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_box_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("message-box", &args, 1)?;
    builtin_message_eval(eval, args)
}

pub(crate) fn builtin_message_or_box_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("message-or-box", &args, 1)?;
    builtin_message_eval(eval, args)
}

pub(crate) fn builtin_current_message(args: Vec<Value>) -> EvalResult {
    expect_args("current-message", &args, 0)?;
    // Batch mode keeps message display side effects out-of-band.
    Ok(Value::Nil)
}

pub(crate) fn builtin_daemonp(args: Vec<Value>) -> EvalResult {
    expect_args("daemonp", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_daemon_initialized(args: Vec<Value>) -> EvalResult {
    expect_args("daemon-initialized", &args, 0)?;
    Err(signal(
        "error",
        vec![Value::string(
            "This function can only be called if emacs is run as a daemon",
        )],
    ))
}

pub(crate) fn builtin_documentation_stringp(args: Vec<Value>) -> EvalResult {
    expect_args("documentation-stringp", &args, 1)?;
    Ok(Value::bool(matches!(
        args[0],
        Value::Str(_) | Value::Int(_)
    )))
}

pub(crate) fn builtin_flush_standard_output(args: Vec<Value>) -> EvalResult {
    expect_args("flush-standard-output", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_force_mode_line_update(args: Vec<Value>) -> EvalResult {
    expect_max_args("force-mode-line-update", &args, 1)?;
    Ok(args.first().cloned().unwrap_or(Value::Nil))
}

pub(crate) fn builtin_force_window_update(args: Vec<Value>) -> EvalResult {
    expect_max_args("force-window-update", &args, 1)?;
    if args.first().is_some_and(|v| !v.is_nil()) {
        Ok(Value::Nil)
    } else {
        Ok(Value::True)
    }
}

pub(crate) fn builtin_get_internal_run_time(args: Vec<Value>) -> EvalResult {
    expect_args("get-internal-run-time", &args, 0)?;
    use std::time::{SystemTime, UNIX_EPOCH};
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let secs = dur.as_secs() as i64;
    let usecs = dur.subsec_micros() as i64;
    Ok(Value::list(vec![
        Value::Int(secs >> 16),
        Value::Int(secs & 0xFFFF),
        Value::Int(usecs),
        Value::Int(0),
    ]))
}

pub(crate) fn builtin_invocation_directory(args: Vec<Value>) -> EvalResult {
    expect_args("invocation-directory", &args, 0)?;
    let mut dir = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|parent| parent.to_path_buf()))
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|| "/".to_string());
    if !dir.ends_with('/') {
        dir.push('/');
    }
    Ok(Value::string(dir))
}

pub(crate) fn builtin_invocation_name(args: Vec<Value>) -> EvalResult {
    expect_args("invocation-name", &args, 0)?;
    let name = std::env::current_exe()
        .ok()
        .and_then(|p| {
            p.file_name()
                .map(|name| name.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| "emacs".to_string());
    Ok(Value::string(name))
}

pub(crate) fn builtin_error(args: Vec<Value>) -> EvalResult {
    expect_min_args("error", &args, 1)?;
    let msg = match builtin_format(args)? {
        Value::Str(s) => (*s).clone(),
        _ => "error".to_string(),
    };
    Err(signal("error", vec![Value::string(msg)]))
}

pub(crate) fn builtin_error_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("error", &args, 1)?;
    let msg = match builtin_format_eval(eval, args)? {
        Value::Str(s) => (*s).clone(),
        _ => "error".to_string(),
    };
    Err(signal("error", vec![Value::string(msg)]))
}

pub(crate) fn builtin_secure_hash_algorithms(args: Vec<Value>) -> EvalResult {
    expect_args("secure-hash-algorithms", &args, 0)?;
    Ok(Value::list(vec![
        Value::symbol("md5"),
        Value::symbol("sha1"),
        Value::symbol("sha224"),
        Value::symbol("sha256"),
        Value::symbol("sha384"),
        Value::symbol("sha512"),
    ]))
}

pub(crate) fn builtin_symbol_name(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-name", &args, 1)?;
    match args[0].as_symbol_name() {
        Some(name) => Ok(Value::string(name)),
        None => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )),
    }
}

pub(crate) fn builtin_make_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("make-symbol", &args, 1)?;
    let name = expect_string(&args[0])?;
    Ok(Value::Symbol(name))
}

pub(crate) fn builtin_apply(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() < 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("apply"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0].clone();
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();

    // Last argument must be a list, which gets spread
    match last {
        Value::Nil => {}
        Value::Cons(_) => {
            let mut cursor = last.clone();
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let pair = cell.lock().expect("poisoned");
                        call_args.push(pair.car.clone());
                        cursor = pair.cdr.clone();
                    }
                    other => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), other],
                        ))
                    }
                }
            }
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), last.clone()],
            ))
        }
    }

    eval.apply(func, call_args)
}

pub(crate) fn builtin_funcall(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("funcall", &args, 1)?;
    let func = args[0].clone();
    let call_args = args[1..].to_vec();
    eval.apply(func, call_args)
}

pub(crate) fn builtin_funcall_interactively(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("funcall-interactively", &args, 1)?;
    let func = args[0].clone();
    let call_args = args[1..].to_vec();
    eval.apply(func, call_args)
}

pub(crate) fn builtin_funcall_with_delayed_message(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("funcall-with-delayed-message", &args, 3)?;
    let _delay = expect_number(&args[0])?;
    let _message = expect_string(&args[1])?;
    eval.apply(args[2].clone(), vec![])
}

pub(crate) fn builtin_get_pos_property(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("get-pos-property", &args, 2)?;
    expect_max_args("get-pos-property", &args, 3)?;
    let pos = expect_integer_or_marker(&args[0])?;
    let Some(prop) = args[1].as_symbol_name() else {
        return Ok(Value::Nil);
    };

    let buf_id = match args.get(2) {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")])),
        Some(Value::Buffer(id)) => Ok(*id),
        Some(Value::Str(_)) => return Ok(Value::Nil),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("buffer-or-string-p"), other.clone()],
        )),
    }?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let char_pos = if pos > 0 { (pos - 1) as usize } else { 0 };
    let byte_pos = buf.text.char_to_byte(char_pos.min(buf.text.char_count()));
    for ov_id in buf.overlays.overlays_at(byte_pos) {
        if let Some(value) = buf.overlays.overlay_get(ov_id, prop) {
            return Ok(value.clone());
        }
    }
    Ok(Value::Nil)
}

pub(crate) fn builtin_next_char_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("next-char-property-change", &args, 1)?;
    expect_max_args("next-char-property-change", &args, 2)?;
    let result = match args.len() {
        1 => super::textprop::builtin_next_property_change(eval, args)?,
        2 => super::textprop::builtin_next_property_change(
            eval,
            vec![args[0].clone(), Value::Nil, args[1].clone()],
        )?,
        _ => unreachable!(),
    };
    if !result.is_nil() {
        return Ok(result);
    }

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    Ok(Value::Int(
        buf.text.byte_to_char(buf.point_max()) as i64 + 1,
    ))
}

pub(crate) fn builtin_pos_bol(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("pos-bol", &args, 1)?;
    super::navigation::builtin_line_beginning_position(eval, args)
}

pub(crate) fn builtin_pos_eol(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("pos-eol", &args, 1)?;
    super::navigation::builtin_line_end_position(eval, args)
}

pub(crate) fn builtin_previous_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("previous-property-change", &args, 1)?;
    expect_max_args("previous-property-change", &args, 3)?;

    let pos = expect_integer_or_marker(&args[0])?;
    if let Some(Value::Str(_)) = args.get(1) {
        if let Some(limit) = args.get(2) {
            if !limit.is_nil() {
                return Ok(Value::Int(expect_integer_or_marker(limit)?));
            }
        }
        return Ok(Value::Nil);
    }

    let buf_id = match args.get(1) {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")])),
        Some(Value::Buffer(id)) => Ok(*id),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("buffer-or-string-p"), other.clone()],
        )),
    }?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let char_pos = if pos > 0 { (pos - 1) as usize } else { 0 };
    let byte_pos = buf.text.char_to_byte(char_pos.min(buf.text.char_count()));

    let (byte_limit, limit_pos) = match args.get(2) {
        Some(v) if !v.is_nil() => {
            let limit = expect_integer_or_marker(v)?;
            let limit_char = if limit > 0 { (limit - 1) as usize } else { 0 };
            let limit_byte = buf.text.char_to_byte(limit_char.min(buf.text.char_count()));
            let limit_elisp = buf.text.byte_to_char(limit_byte) as i64 + 1;
            (Some(limit_byte), Some(limit_elisp))
        }
        _ => (None, None),
    };

    let ref_byte = if byte_pos > 0 { byte_pos - 1 } else { 0 };
    let current_props = buf.text_props.get_properties(ref_byte);
    let mut cursor = byte_pos;

    loop {
        match buf.text_props.previous_property_change(cursor) {
            Some(prev) => {
                if let (Some(lim_byte), Some(limit_elisp)) = (byte_limit, limit_pos) {
                    if prev < lim_byte {
                        return Ok(Value::Int(limit_elisp));
                    }
                }

                let check = if prev > 0 { prev - 1 } else { 0 };
                let new_props = buf.text_props.get_properties(check);
                if new_props != current_props {
                    return Ok(Value::Int(buf.text.byte_to_char(prev) as i64 + 1));
                }

                if prev == 0 {
                    break;
                }
                cursor = if prev < cursor { prev } else { prev - 1 };
            }
            None => break,
        }
    }

    match limit_pos {
        Some(limit_elisp) => Ok(Value::Int(limit_elisp)),
        None => Ok(Value::Nil),
    }
}

pub(crate) fn builtin_previous_char_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("previous-char-property-change", &args, 1)?;
    expect_max_args("previous-char-property-change", &args, 2)?;

    let mut forwarded = vec![args[0].clone(), Value::Nil];
    if let Some(limit) = args.get(1) {
        forwarded.push(limit.clone());
    }
    let result = builtin_previous_property_change(eval, forwarded)?;
    if !result.is_nil() {
        return Ok(result);
    }

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    Ok(Value::Int(
        buf.text.byte_to_char(buf.point_min()) as i64 + 1,
    ))
}

pub(crate) fn builtin_next_single_char_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("next-single-char-property-change", &args, 2)?;
    expect_max_args("next-single-char-property-change", &args, 4)?;

    if let Some(Value::Str(s)) = args.get(2) {
        if let Some(limit) = args.get(3) {
            if !limit.is_nil() {
                return Ok(Value::Int(expect_integer_or_marker(limit)?));
            }
        }
        return Ok(Value::Int(s.chars().count() as i64));
    }

    let result = super::textprop::builtin_next_single_property_change(eval, args.clone())?;
    if !result.is_nil() {
        return Ok(result);
    }

    let upper = match args.get(2) {
        Some(Value::Buffer(id)) => {
            let buf = eval
                .buffers
                .get(*id)
                .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;
            buf.text.byte_to_char(buf.point_max()) as i64 + 1
        }
        _ => {
            let buf = eval
                .buffers
                .current_buffer()
                .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
            buf.text.byte_to_char(buf.point_max()) as i64 + 1
        }
    };
    Ok(Value::Int(upper))
}

pub(crate) fn builtin_previous_single_char_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("previous-single-char-property-change", &args, 2)?;
    expect_max_args("previous-single-char-property-change", &args, 4)?;

    if let Some(Value::Str(_)) = args.get(2) {
        if let Some(limit) = args.get(3) {
            if !limit.is_nil() {
                return Ok(Value::Int(expect_integer_or_marker(limit)?));
            }
        }
        return Ok(Value::Int(0));
    }

    let result = super::textprop::builtin_previous_single_property_change(eval, args.clone())?;
    if !result.is_nil() {
        return Ok(result);
    }

    let lower = match args.get(2) {
        Some(Value::Buffer(id)) => {
            let buf = eval
                .buffers
                .get(*id)
                .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;
            buf.text.byte_to_char(buf.point_min()) as i64 + 1
        }
        _ => {
            let buf = eval
                .buffers
                .current_buffer()
                .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
            buf.text.byte_to_char(buf.point_min()) as i64 + 1
        }
    };
    Ok(Value::Int(lower))
}

pub(crate) fn builtin_defalias(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("defalias", &args, 2, 3)?;
    eval.defalias_value(args[0].clone(), args[1].clone())
}

pub(crate) fn builtin_provide(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("provide", &args, 1, 2)?;
    eval.provide_value(args[0].clone(), args.get(1).cloned())
}

pub(crate) fn builtin_require(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("require", &args, 1, 3)?;
    eval.require_value(args[0].clone(), args.get(1).cloned(), args.get(2).cloned())
}

// ===========================================================================
// Higher-order
// ===========================================================================

fn for_each_sequence_element<F>(seq: &Value, mut f: F) -> Result<(), Flow>
where
    F: FnMut(Value) -> Result<(), Flow>,
{
    match seq {
        Value::Nil => Ok(()),
        Value::Cons(_) => {
            let mut cursor = seq.clone();
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let pair = cell.lock().expect("poisoned");
                        let item = pair.car.clone();
                        cursor = pair.cdr.clone();
                        drop(pair);
                        f(item)?;
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ))
                    }
                }
            }
            Ok(())
        }
        Value::Vector(v) => {
            for item in v.lock().expect("poisoned").iter().cloned() {
                f(item)?;
            }
            Ok(())
        }
        Value::Str(s) => {
            for cp in decode_storage_char_codes(s) {
                f(Value::Int(cp as i64))?;
            }
            Ok(())
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), seq.clone()],
        )),
    }
}

pub(crate) fn builtin_mapcar(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapcar"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0].clone();
    let mut results = Vec::new();
    for_each_sequence_element(&args[1], |item| {
        results.push(eval.apply(func.clone(), vec![item])?);
        Ok(())
    })?;
    Ok(Value::list(results))
}

pub(crate) fn builtin_mapc(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapc"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0].clone();
    let seq = args[1].clone();
    for_each_sequence_element(&seq, |item| {
        eval.apply(func.clone(), vec![item])?;
        Ok(())
    })?;
    Ok(seq)
}

pub(crate) fn builtin_mapconcat(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapconcat"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0].clone();
    let sequence = args[1].clone();
    let separator = args[2].clone();

    let mut parts = Vec::new();
    for_each_sequence_element(&sequence, |item| {
        parts.push(eval.apply(func.clone(), vec![item])?);
        Ok(())
    })?;

    if parts.is_empty() {
        return Ok(Value::string(""));
    }

    let mut concat_args = Vec::with_capacity(parts.len() * 2 - 1);
    for (index, part) in parts.into_iter().enumerate() {
        if index > 0 {
            concat_args.push(separator.clone());
        }
        concat_args.push(part);
    }
    builtin_concat(concat_args)
}

pub(crate) fn builtin_mapcan(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapcan"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0].clone();
    let sequence = args[1].clone();
    let mut mapped = Vec::new();
    for_each_sequence_element(&sequence, |item| {
        mapped.push(eval.apply(func.clone(), vec![item])?);
        Ok(())
    })?;
    builtin_nconc(mapped)
}

pub(crate) fn builtin_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("sort"), Value::Int(args.len() as i64)],
        ));
    }
    let pred = args[1].clone();
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            let mut cons_cells = Vec::new();
            let mut values = Vec::new();
            let mut cursor = args[0].clone();
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        values.push(cell.lock().expect("poisoned").car.clone());
                        cons_cells.push(cell.clone());
                        cursor = cell.lock().expect("poisoned").cdr.clone();
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ))
                    }
                }
            }

            // Stable insertion sort with dynamic predicate callback.
            for i in 1..values.len() {
                let mut j = i;
                while j > 0 {
                    let result =
                        eval.apply(pred.clone(), vec![values[j].clone(), values[j - 1].clone()])?;
                    if result.is_truthy() {
                        values.swap(j, j - 1);
                        j -= 1;
                    } else {
                        break;
                    }
                }
            }

            for (cell, value) in cons_cells.iter().zip(values.into_iter()) {
                cell.lock().expect("poisoned").car = value;
            }
            Ok(args[0].clone())
        }
        Value::Vector(v) => {
            let mut values = v.lock().expect("poisoned").clone();
            for i in 1..values.len() {
                let mut j = i;
                while j > 0 {
                    let result =
                        eval.apply(pred.clone(), vec![values[j].clone(), values[j - 1].clone()])?;
                    if result.is_truthy() {
                        values.swap(j, j - 1);
                        j -= 1;
                    } else {
                        break;
                    }
                }
            }
            *v.lock().expect("poisoned") = values;
            Ok(args[0].clone())
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("list-or-vector-p"), other.clone()],
        )),
    }
}

// ===========================================================================
// Helpers
// ===========================================================================

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        Value::Symbol(s) => Ok(s.clone()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn expect_strict_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

// ===========================================================================
// Symbol operations (need evaluator for obarray access)
// ===========================================================================

const VARIABLE_ALIAS_PROPERTY: &str = "neovm--variable-alias";
const RAW_SYMBOL_PLIST_PROPERTY: &str = "neovm--raw-symbol-plist";

fn is_internal_symbol_plist_property(property: &str) -> bool {
    property == VARIABLE_ALIAS_PROPERTY || property == RAW_SYMBOL_PLIST_PROPERTY
}

fn resolve_variable_alias_name(eval: &super::eval::Evaluator, name: &str) -> Result<String, Flow> {
    let mut current = name.to_string();
    let mut seen = HashSet::new();

    loop {
        if !seen.insert(current.clone()) {
            return Err(signal(
                "cyclic-variable-indirection",
                vec![Value::symbol(name)],
            ));
        }
        let next = eval
            .obarray()
            .get_property(&current, VARIABLE_ALIAS_PROPERTY)
            .and_then(|value| value.as_symbol_name())
            .map(|value| value.to_string());
        match next {
            Some(next_name) => current = next_name,
            None => return Ok(current),
        }
    }
}

fn would_create_variable_alias_cycle(eval: &super::eval::Evaluator, new: &str, old: &str) -> bool {
    let mut current = old.to_string();
    let mut seen = HashSet::new();

    loop {
        if current == new {
            return true;
        }
        if !seen.insert(current.clone()) {
            return true;
        }
        let next = eval
            .obarray()
            .get_property(&current, VARIABLE_ALIAS_PROPERTY)
            .and_then(|value| value.as_symbol_name())
            .map(|value| value.to_string());
        match next {
            Some(next_name) => current = next_name,
            None => return false,
        }
    }
}

fn symbol_raw_plist_value(eval: &super::eval::Evaluator, name: &str) -> Option<Value> {
    eval.obarray()
        .get_property(name, RAW_SYMBOL_PLIST_PROPERTY)
        .cloned()
}

fn set_symbol_raw_plist(eval: &mut super::eval::Evaluator, name: &str, plist: Value) {
    let sym = eval.obarray_mut().get_or_intern(name);
    let alias = sym.plist.get(VARIABLE_ALIAS_PROPERTY).cloned();
    sym.plist.clear();
    if let Some(value) = alias {
        sym.plist.insert(VARIABLE_ALIAS_PROPERTY.to_string(), value);
    }
    sym.plist
        .insert(RAW_SYMBOL_PLIST_PROPERTY.to_string(), plist);
}

fn plist_lookup_value(plist: &Value, prop: &Value) -> Option<Value> {
    let mut cursor = plist.clone();
    loop {
        match cursor {
            Value::Nil => return None,
            Value::Cons(pair_cell) => {
                let pair = pair_cell.lock().expect("poisoned");
                let key = pair.car.clone();
                let rest = pair.cdr.clone();
                drop(pair);
                let Value::Cons(value_cell) = rest else {
                    return None;
                };
                let value_pair = value_cell.lock().expect("poisoned");
                let value = value_pair.car.clone();
                let next = value_pair.cdr.clone();
                drop(value_pair);
                if eq_value(&key, prop) {
                    return Some(value);
                }
                cursor = next;
            }
            _ => return None,
        }
    }
}

pub(crate) fn builtin_boundp(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("boundp", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    Ok(Value::bool(eval.obarray().boundp(&resolved)))
}

pub(crate) fn builtin_obarrayp_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("obarrayp", &args, 1)?;
    let current_obarray = eval
        .obarray()
        .symbol_value("neovm--obarray-object")
        .or_else(|| eval.obarray().symbol_value("obarray"));
    Ok(Value::bool(
        current_obarray.is_some_and(|obarray| eq_value(obarray, &args[0])),
    ))
}

pub(crate) fn builtin_special_variable_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("special-variable-p", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    Ok(Value::bool(eval.obarray().is_special(&resolved)))
}

pub(crate) fn builtin_default_boundp(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("default-boundp", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    Ok(Value::bool(eval.obarray().boundp(&resolved)))
}

pub(crate) fn builtin_default_toplevel_value(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("default-toplevel-value", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    eval.obarray()
        .symbol_value(&resolved)
        .cloned()
        .ok_or_else(|| signal("void-variable", vec![Value::symbol(name)]))
}

pub(crate) fn builtin_set_default_toplevel_value(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-default-toplevel-value", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    if eval.obarray().is_constant(&resolved) {
        return Err(signal("setting-constant", vec![Value::symbol(name)]));
    }
    eval.obarray.set_symbol_value(&resolved, args[1].clone());
    Ok(Value::Nil)
}

pub(crate) fn builtin_defvaralias_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("defvaralias", &args, 2, 3)?;
    let new_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let old_name = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1].clone()],
        )
    })?;
    if eval.obarray().is_constant(new_name) {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Cannot make a constant an alias: {new_name}"
            ))],
        ));
    }
    if would_create_variable_alias_cycle(eval, new_name, old_name) {
        return Err(signal(
            "cyclic-variable-indirection",
            vec![Value::symbol(old_name)],
        ));
    }
    {
        let sym = eval.obarray_mut().get_or_intern(new_name);
        sym.special = true;
        sym.plist
            .insert(VARIABLE_ALIAS_PROPERTY.to_string(), Value::symbol(old_name));
    }
    eval.obarray_mut().make_special(old_name);
    // GNU Emacs updates `variable-documentation` through plist machinery after
    // installing alias state, so malformed raw plists still raise
    // `(wrong-type-argument plistp ...)` with the alias edge retained.
    let docstring = args.get(2).cloned().unwrap_or(Value::Nil);
    builtin_put(
        eval,
        vec![
            Value::symbol(new_name),
            Value::symbol("variable-documentation"),
            docstring,
        ],
    )?;
    Ok(Value::symbol(old_name))
}

pub(crate) fn builtin_indirect_variable_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("indirect-variable", &args, 1)?;
    let Some(name) = args[0].as_symbol_name() else {
        return Ok(args[0].clone());
    };
    let resolved = resolve_variable_alias_name(eval, name)?;
    Ok(Value::symbol(resolved))
}

pub(crate) fn builtin_fboundp(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("fboundp", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    if eval.obarray().is_function_unbound(name) {
        return Ok(Value::Nil);
    }
    if let Some(function) = eval.obarray().symbol_function(name) {
        return Ok(Value::bool(!function.is_nil()));
    }
    let macro_bound = super::subr_info::is_evaluator_macro_name(name);
    Ok(Value::bool(
        super::subr_info::is_special_form(name)
            || macro_bound
            || super::subr_info::is_evaluator_callable_name(name)
            || super::builtin_registry::is_dispatch_builtin_name(name)
            || name.parse::<PureBuiltinId>().is_ok(),
    ))
}

pub(crate) fn builtin_symbol_value(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("symbol-value", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    // Check dynamic bindings first
    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get(&resolved) {
            return Ok(value.clone());
        }
    }
    // Check current buffer-local binding.
    if let Some(buf) = eval.buffers.current_buffer() {
        if let Some(value) = buf.get_buffer_local(&resolved) {
            return Ok(value.clone());
        }
    }
    eval.obarray()
        .symbol_value(&resolved)
        .cloned()
        .ok_or_else(|| signal("void-variable", vec![Value::symbol(name)]))
}

pub(crate) fn builtin_symbol_function(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("symbol-function", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    if eval.obarray().is_function_unbound(name) {
        return Ok(Value::Nil);
    }

    if let Some(function) = eval.obarray().symbol_function(name) {
        // GNU Emacs exposes this symbol as autoload-shaped in startup state,
        // then subr-shaped after first invocation triggers autoload materialization.
        if name == "kmacro-name-last-macro"
            && matches!(function, Value::Subr(subr) if subr == "kmacro-name-last-macro")
            && eval
                .obarray()
                .get_property("kmacro-name-last-macro", "neovm--kmacro-autoload-promoted")
                .is_none()
        {
            return Ok(Value::list(vec![
                Value::symbol("autoload"),
                Value::string("kmacro"),
                Value::string("Assign a name to the last keyboard macro defined."),
                Value::True,
                Value::Nil,
            ]));
        }
        return Ok(function.clone());
    }

    if let Some(function) = super::subr_info::fallback_macro_value(name) {
        return Ok(function);
    }

    if name == "inline" {
        return Ok(Value::symbol("inline"));
    }

    if super::subr_info::is_special_form(name)
        || super::subr_info::is_evaluator_callable_name(name)
        || super::builtin_registry::is_dispatch_builtin_name(name)
        || name.parse::<PureBuiltinId>().is_ok()
    {
        return Ok(Value::Subr(name.to_string()));
    }

    Ok(Value::Nil)
}

pub(crate) fn builtin_func_arity_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("func-arity", &args, 1)?;

    if let Some(name) = args[0].as_symbol_name() {
        if let Some(function) = resolve_indirect_symbol(eval, name) {
            if function.is_nil() {
                return Err(signal("void-function", vec![Value::symbol(name)]));
            }
            maybe_materialize_thingatpt_word_symbol(eval, name, &function);
            if super::subr_info::is_special_form(name) {
                return super::subr_info::builtin_func_arity(vec![Value::Subr(name.to_string())]);
            }
            if let Some(arity) = dispatch_symbol_func_arity_override(eval, name, &function) {
                return Ok(arity);
            }
            return super::subr_info::builtin_func_arity(vec![function]);
        }
        return Err(signal("void-function", vec![Value::symbol(name)]));
    }

    super::subr_info::builtin_func_arity(vec![args[0].clone()])
}

fn maybe_materialize_thingatpt_word_symbol(
    eval: &mut super::eval::Evaluator,
    name: &str,
    function: &Value,
) {
    if !super::autoload::is_autoload_value(function) {
        return;
    }
    if !matches!(
        name,
        "symbol-at-point" | "thing-at-point" | "bounds-of-thing-at-point"
    ) {
        return;
    }
    let obarray = eval.obarray();
    if obarray.fboundp("word-at-point") {
        return;
    }
    // Respect explicit user-level `fmakunbound` after materialization. Startup
    // masking keeps the symbol uninterned and should still allow first bootstrap.
    if obarray.is_function_unbound("word-at-point")
        && obarray.intern_soft("word-at-point").is_some()
    {
        return;
    }
    eval.set_function("word-at-point", Value::Subr("word-at-point".to_string()));
}

fn has_startup_subr_wrapper(eval: &super::eval::Evaluator, name: &str) -> bool {
    let wrapper = format!("neovm--startup-subr-wrapper-{name}");
    matches!(
        eval.obarray().symbol_function(&wrapper),
        Some(Value::Subr(subr_name)) if subr_name == name
    )
}

fn dispatch_symbol_func_arity_override(
    eval: &super::eval::Evaluator,
    name: &str,
    function: &Value,
) -> Option<Value> {
    if !super::builtin_registry::is_dispatch_builtin_name(name) {
        return None;
    }

    if super::autoload::is_autoload_value(function)
        || (matches!(function, Value::ByteCode(_)) && has_startup_subr_wrapper(eval, name))
    {
        return Some(super::subr_info::dispatch_subr_arity_value(name));
    }

    None
}

pub(crate) fn builtin_set(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    if eval.obarray().is_constant(&resolved) {
        return Err(signal("setting-constant", vec![Value::symbol(name)]));
    }
    let value = args[1].clone();
    eval.assign(&resolved, value.clone());
    Ok(value)
}

pub(crate) fn builtin_fset(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("fset", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    if name == "nil" {
        return Err(signal("setting-constant", vec![Value::symbol("nil")]));
    }
    let def = super::compiled_literal::maybe_coerce_compiled_literal_function(args[1].clone());
    if would_create_function_alias_cycle(eval, name, &def) {
        return Err(signal(
            "cyclic-function-indirection",
            vec![Value::symbol(name)],
        ));
    }
    eval.obarray_mut().set_symbol_function(name, def.clone());
    Ok(def)
}

pub(crate) fn would_create_function_alias_cycle(
    eval: &super::eval::Evaluator,
    target_name: &str,
    def: &Value,
) -> bool {
    let mut current = match def.as_symbol_name() {
        Some(name) => name.to_string(),
        None => return false,
    };
    let mut seen = HashSet::new();

    loop {
        if current == target_name {
            return true;
        }
        if !seen.insert(current.clone()) {
            return true;
        }

        let next = match eval.obarray().symbol_function(&current) {
            Some(function) => {
                if let Some(name) = function.as_symbol_name() {
                    name.to_string()
                } else {
                    return false;
                }
            }
            None => return false,
        };
        current = next;
    }
}

pub(crate) fn builtin_makunbound(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("makunbound", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = resolve_variable_alias_name(eval, name)?;
    if eval.obarray().is_constant(&resolved) {
        return Err(signal("setting-constant", vec![Value::symbol(name)]));
    }
    eval.obarray_mut().makunbound(&resolved);
    Ok(args[0].clone())
}

pub(crate) fn builtin_fmakunbound(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("fmakunbound", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    eval.obarray_mut().fmakunbound(name);
    Ok(args[0].clone())
}

pub(crate) fn builtin_get(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("get", &args, 2)?;
    let sym = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    if let Some(raw) = symbol_raw_plist_value(eval, sym) {
        return Ok(plist_lookup_value(&raw, &args[1]).unwrap_or(Value::Nil));
    }
    let prop = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1].clone()],
        )
    })?;
    if is_internal_symbol_plist_property(prop) {
        return Ok(Value::Nil);
    }
    Ok(eval
        .obarray()
        .get_property(sym, prop)
        .cloned()
        .unwrap_or(Value::Nil))
}

pub(crate) fn builtin_put(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("put", &args, 3)?;
    let sym = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let prop = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1].clone()],
        )
    })?;
    let value = args[2].clone();
    if let Some(raw) = symbol_raw_plist_value(eval, sym) {
        let plist = builtin_plist_put(vec![raw, args[1].clone(), value.clone()])?;
        set_symbol_raw_plist(eval, sym, plist);
        return Ok(value);
    }
    eval.obarray_mut().put_property(sym, prop, value.clone());
    Ok(value)
}

pub(crate) fn builtin_symbol_plist_fn(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("symbol-plist", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    if let Some(raw) = symbol_raw_plist_value(eval, name) {
        return Ok(raw);
    }
    let Some(sym) = eval.obarray().get(name) else {
        return Ok(Value::Nil);
    };
    let mut items = Vec::new();
    for (key, value) in &sym.plist {
        if is_internal_symbol_plist_property(key) {
            continue;
        }
        items.push(Value::symbol(key.clone()));
        items.push(value.clone());
    }
    if items.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::list(items))
    }
}

pub(crate) fn builtin_setplist_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("setplist", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let plist = args[1].clone();
    set_symbol_raw_plist(eval, name, plist.clone());
    Ok(plist)
}

fn macroexpand_environment_binding(env: &Value, name: &str) -> Option<Value> {
    let mut cursor = env.clone();
    loop {
        match cursor {
            Value::Nil => return None,
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let entry = pair.car.clone();
                cursor = pair.cdr.clone();
                drop(pair);
                let Value::Cons(entry_cell) = entry else {
                    continue;
                };
                let entry_pair = entry_cell.lock().expect("poisoned");
                if entry_pair.car.as_symbol_name() == Some(name) {
                    return Some(entry_pair.cdr.clone());
                }
            }
            _ => return None,
        }
    }
}

fn macroexpand_environment_callable(
    eval: &mut super::eval::Evaluator,
    binding: &Value,
) -> Result<Value, Flow> {
    if is_lambda_form_list(binding) {
        let expr = super::eval::value_to_expr_pub(binding);
        return eval.eval(&expr);
    }
    Ok(binding.clone())
}

fn macroexpand_known_fallback_macro(name: &str, args: &[Value]) -> Result<Option<Value>, Flow> {
    match name {
        "when" => {
            if args.is_empty() {
                return Err(signal(
                    "wrong-number-of-arguments",
                    vec![Value::cons(Value::Int(1), Value::Int(1)), Value::Int(0)],
                ));
            }
            if args.len() == 1 {
                return Ok(Some(Value::list(vec![
                    Value::symbol("progn"),
                    args[0].clone(),
                    Value::Nil,
                ])));
            }
            let mut then_forms = Vec::with_capacity(args.len());
            then_forms.push(Value::symbol("progn"));
            then_forms.extend_from_slice(&args[1..]);
            Ok(Some(Value::list(vec![
                Value::symbol("if"),
                args[0].clone(),
                Value::list(then_forms),
            ])))
        }
        "unless" => {
            if args.is_empty() {
                return Err(signal(
                    "wrong-number-of-arguments",
                    vec![Value::cons(Value::Int(1), Value::Int(1)), Value::Int(0)],
                ));
            }
            if args.len() == 1 {
                return Ok(Some(Value::list(vec![
                    Value::symbol("progn"),
                    args[0].clone(),
                    Value::Nil,
                ])));
            }
            let mut forms = Vec::with_capacity(args.len() + 2);
            forms.push(Value::symbol("if"));
            forms.push(args[0].clone());
            forms.push(Value::Nil);
            forms.extend_from_slice(&args[1..]);
            Ok(Some(Value::list(forms)))
        }
        "save-match-data" => {
            let saved = Value::symbol("saved-match-data");
            let binding = Value::list(vec![
                saved.clone(),
                Value::list(vec![Value::symbol("match-data")]),
            ]);
            let mut protected_forms = Vec::with_capacity(args.len() + 1);
            protected_forms.push(Value::symbol("progn"));
            protected_forms.extend_from_slice(args);
            let protected = Value::list(protected_forms);
            let restore = Value::list(vec![
                Value::symbol("set-match-data"),
                saved,
                Value::True,
            ]);
            Ok(Some(Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![binding]),
                Value::list(vec![Value::symbol("unwind-protect"), protected, restore]),
            ])))
        }
        "save-mark-and-excursion" => {
            let saved = Value::symbol("saved-marker");
            let binding = Value::list(vec![
                saved.clone(),
                Value::list(vec![Value::symbol("save-mark-and-excursion--save")]),
            ]);
            let mut protected_forms = Vec::with_capacity(args.len() + 1);
            protected_forms.push(Value::symbol("save-excursion"));
            protected_forms.extend_from_slice(args);
            let protected = Value::list(protected_forms);
            let restore = Value::list(vec![
                Value::symbol("save-mark-and-excursion--restore"),
                saved,
            ]);
            Ok(Some(Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![binding]),
                Value::list(vec![Value::symbol("unwind-protect"), protected, restore]),
            ])))
        }
        "save-window-excursion" => {
            let saved = Value::symbol("wconfig");
            let binding = Value::list(vec![
                saved.clone(),
                Value::list(vec![Value::symbol("current-window-configuration")]),
            ]);
            let mut protected_forms = Vec::with_capacity(args.len() + 1);
            protected_forms.push(Value::symbol("progn"));
            protected_forms.extend_from_slice(args);
            let protected = Value::list(protected_forms);
            let restore = Value::list(vec![Value::symbol("set-window-configuration"), saved]);
            Ok(Some(Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![binding]),
                Value::list(vec![Value::symbol("unwind-protect"), protected, restore]),
            ])))
        }
        "save-selected-window" => {
            let saved = Value::symbol("save-selected-window--state");
            let binding = Value::list(vec![
                saved.clone(),
                Value::list(vec![Value::symbol("internal--before-save-selected-window")]),
            ]);
            let mut protected_forms = Vec::with_capacity(args.len() + 1);
            protected_forms.push(Value::symbol("progn"));
            protected_forms.extend_from_slice(args);
            let protected = Value::list(protected_forms);
            let restore = Value::list(vec![
                Value::symbol("internal--after-save-selected-window"),
                saved,
            ]);
            let unwind = Value::list(vec![Value::symbol("unwind-protect"), protected, restore]);
            Ok(Some(Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![binding]),
                Value::list(vec![Value::symbol("save-current-buffer"), unwind]),
            ])))
        }
        "with-local-quit" => {
            let binding = Value::list(vec![Value::symbol("inhibit-quit"), Value::Nil]);
            let mut let_forms = Vec::with_capacity(args.len() + 2);
            let_forms.push(Value::symbol("let"));
            let_forms.push(Value::list(vec![binding]));
            let_forms.extend_from_slice(args);
            let body = Value::list(let_forms);
            let handler = Value::list(vec![
                Value::symbol("quit"),
                Value::list(vec![
                    Value::symbol("setq"),
                    Value::symbol("quit-flag"),
                    Value::True,
                ]),
                Value::list(vec![
                    Value::symbol("eval"),
                    Value::list(vec![
                        Value::symbol("quote"),
                        Value::list(vec![Value::symbol("ignore"), Value::Nil]),
                    ]),
                    Value::True,
                ]),
            ]);
            Ok(Some(Value::list(vec![
                Value::symbol("condition-case"),
                Value::Nil,
                body,
                handler,
            ])))
        }
        "with-temp-message" => {
            if args.is_empty() {
                return Err(signal(
                    "wrong-number-of-arguments",
                    vec![Value::cons(Value::Int(1), Value::symbol("many")), Value::Int(0)],
                ));
            }

            let temp = Value::symbol("with-temp-message");
            let current = Value::symbol("current-message");
            let bindings = Value::list(vec![
                Value::list(vec![temp.clone(), args[0].clone()]),
                Value::list(vec![current.clone()]),
            ]);

            let when_form = Value::list(vec![
                Value::symbol("when"),
                temp.clone(),
                Value::list(vec![
                    Value::symbol("setq"),
                    current.clone(),
                    Value::list(vec![Value::symbol("current-message")]),
                ]),
                Value::list(vec![
                    Value::symbol("message"),
                    Value::string("%s"),
                    temp.clone(),
                ]),
            ]);

            let mut protected_forms = Vec::with_capacity(args.len() + 1);
            protected_forms.push(Value::symbol("progn"));
            protected_forms.push(when_form);
            protected_forms.extend_from_slice(&args[1..]);
            let protected = Value::list(protected_forms);

            let restore = Value::list(vec![
                Value::symbol("and"),
                temp,
                Value::list(vec![
                    Value::symbol("if"),
                    current.clone(),
                    Value::list(vec![
                        Value::symbol("message"),
                        Value::string("%s"),
                        current,
                    ]),
                    Value::list(vec![Value::symbol("message"), Value::Nil]),
                ]),
            ]);

            Ok(Some(Value::list(vec![
                Value::symbol("let"),
                bindings,
                Value::list(vec![Value::symbol("unwind-protect"), protected, restore]),
            ])))
        }
        _ => Ok(None),
    }
}

fn macroexpand_once_with_environment(
    eval: &mut super::eval::Evaluator,
    form: Value,
    environment: Option<&Value>,
) -> Result<(Value, bool), Flow> {
    let Value::Cons(form_cell) = form.clone() else {
        return Ok((form, false));
    };
    let form_pair = form_cell.lock().expect("poisoned");
    let head = form_pair.car.clone();
    let tail = form_pair.cdr.clone();
    drop(form_pair);
    let Some(head_name) = head.as_symbol_name() else {
        return Ok((form, false));
    };

    let mut env_bound = false;
    let mut function = None;
    if let Some(env) = environment {
        if !env.is_list() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), env.clone()],
            ));
        }
        if let Some(binding) = macroexpand_environment_binding(env, head_name) {
            env_bound = true;
            if !binding.is_nil() {
                function = Some(macroexpand_environment_callable(eval, &binding)?);
            }
        }
    }
    if env_bound && function.is_none() {
        return Ok((form, false));
    }
    let mut resolved_name = head_name.to_string();
    let mut fallback_placeholder = false;
    if function.is_none() {
        if let Some((resolved, global)) = resolve_indirect_symbol_with_name(eval, head_name) {
            if matches!(global, Value::Macro(_)) {
                fallback_placeholder = super::subr_info::has_fallback_macro(&resolved)
                    && eval.obarray().symbol_function(&resolved).is_none();
                resolved_name = resolved;
                function = Some(global);
            }
        }
    }
    let Some(function) = function else {
        return Ok((form, false));
    };
    let args = list_to_vec(&tail).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), tail.clone()],
        )
    })?;
    if fallback_placeholder {
        if let Some(expanded) = macroexpand_known_fallback_macro(&resolved_name, &args)? {
            return Ok((expanded, true));
        }
        return Ok((form, false));
    }
    let expanded = eval.apply(function, args)?;
    Ok((expanded, true))
}

pub(crate) fn builtin_macroexpand_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("macroexpand", &args, 1, 2)?;
    let mut form = args[0].clone();
    let environment = args.get(1);
    loop {
        let (expanded, did_expand) = macroexpand_once_with_environment(eval, form, environment)?;
        if !did_expand {
            return Ok(expanded);
        }
        form = expanded;
    }
}

pub(crate) fn builtin_indirect_function(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("indirect-function", &args, 1)?;
    let _noerror = args.get(1).is_some_and(|value| value.is_truthy());

    if let Some(name) = args[0].as_symbol_name() {
        if let Some(function) = resolve_indirect_symbol(eval, name) {
            return Ok(function);
        }
        return Ok(Value::Nil);
    }

    Ok(args[0].clone())
}

fn resolve_indirect_symbol_with_name(
    eval: &super::eval::Evaluator,
    name: &str,
) -> Option<(String, Value)> {
    let mut current = name.to_string();
    let mut seen = HashSet::new();

    loop {
        if !seen.insert(current.clone()) {
            return None;
        }

        if eval.obarray().is_function_unbound(&current) {
            return None;
        }

        if let Some(function) = eval.obarray().symbol_function(&current) {
            if let Some(next) = function.as_symbol_name() {
                if next == "nil" {
                    return Some(("nil".to_string(), Value::Nil));
                }
                current = next.to_string();
                continue;
            }
            return Some((current, function.clone()));
        }

        if let Some(function) = super::subr_info::fallback_macro_value(&current) {
            return Some((current, function));
        }

        if super::subr_info::is_special_form(&current)
            || super::subr_info::is_evaluator_callable_name(&current)
            || super::builtin_registry::is_dispatch_builtin_name(&current)
            || current.parse::<PureBuiltinId>().is_ok()
        {
            return Some((current.clone(), Value::Subr(current)));
        }

        return None;
    }
}

fn resolve_indirect_symbol(eval: &super::eval::Evaluator, name: &str) -> Option<Value> {
    resolve_indirect_symbol_with_name(eval, name).map(|(_, value)| value)
}

pub(crate) fn builtin_macrop_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("macrop", &args, 1)?;
    if let Some(name) = args[0].as_symbol_name() {
        if let Some(function) = resolve_indirect_symbol(eval, name) {
            return super::subr_info::builtin_macrop(vec![function]);
        }
        return Ok(Value::Nil);
    }

    super::subr_info::builtin_macrop(args)
}

pub(crate) fn builtin_intern_fn(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("intern", &args, 1)?;
    expect_max_args("intern", &args, 2)?;
    if let Some(obarray) = args.get(1) {
        if !obarray.is_nil() && !matches!(obarray, Value::Vector(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("obarrayp"), obarray.clone()],
            ));
        }
    }
    let name = expect_string(&args[0])?;
    eval.obarray_mut().intern(&name);
    Ok(Value::symbol(name))
}

pub(crate) fn builtin_intern_soft(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("intern-soft", &args, 1)?;
    expect_max_args("intern-soft", &args, 2)?;
    if let Some(obarray) = args.get(1) {
        if !obarray.is_nil() && !matches!(obarray, Value::Vector(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("obarrayp"), obarray.clone()],
            ));
        }
    }
    let name = expect_string(&args[0])?;
    if eval.obarray().intern_soft(&name).is_some() {
        Ok(Value::symbol(name))
    } else {
        Ok(Value::Nil)
    }
}

// ===========================================================================
// Hook system (need evaluator)
// ===========================================================================

pub(crate) fn builtin_add_hook(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("add-hook", &args, 2)?;
    let hook_name = args[0]
        .as_symbol_name()
        .ok_or_else(|| {
            signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            )
        })?
        .to_string();
    let function = args[1].clone();
    let append = args.get(2).is_some_and(|v| v.is_truthy());

    // Get current hook value
    let current = eval
        .obarray()
        .symbol_value(&hook_name)
        .cloned()
        .unwrap_or(Value::Nil);
    let mut items = list_to_vec(&current).unwrap_or_default();

    // Don't add duplicates
    if !items.iter().any(|v| eq_value(v, &function)) {
        if append {
            items.push(function);
        } else {
            items.insert(0, function);
        }
    }

    eval.obarray_mut()
        .set_symbol_value(&hook_name, Value::list(items));
    Ok(Value::Nil)
}

pub(crate) fn builtin_remove_hook(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("remove-hook", &args, 2)?;
    let hook_name = args[0]
        .as_symbol_name()
        .ok_or_else(|| {
            signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            )
        })?
        .to_string();
    let function = args[1].clone();

    let current = eval
        .obarray()
        .symbol_value(&hook_name)
        .cloned()
        .unwrap_or(Value::Nil);
    let items = list_to_vec(&current).unwrap_or_default();
    let filtered: Vec<Value> = items
        .into_iter()
        .filter(|v| !eq_value(v, &function))
        .collect();
    eval.obarray_mut()
        .set_symbol_value(&hook_name, Value::list(filtered));
    Ok(Value::Nil)
}

fn symbol_dynamic_buffer_or_global_value(
    eval: &super::eval::Evaluator,
    name: &str,
) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get(name) {
            return Some(value.clone());
        }
    }
    if let Some(buf) = eval.buffers.current_buffer() {
        if let Some(value) = buf.get_buffer_local(name) {
            return Some(value.clone());
        }
    }
    eval.obarray().symbol_value(name).cloned()
}

enum HookControl {
    Continue,
    Return(Value),
}

fn walk_hook_value_with<F>(
    eval: &mut super::eval::Evaluator,
    hook_name: &str,
    hook_value: Value,
    inherit_global: bool,
    callback: &mut F,
) -> Result<HookControl, Flow>
where
    F: FnMut(&mut super::eval::Evaluator, Value) -> Result<HookControl, Flow>,
{
    match hook_value {
        Value::Nil => Ok(HookControl::Continue),
        Value::Cons(_) => {
            // Oracle-compatible traversal: iterate cons cells, ignore improper
            // list tails, and treat `t` as "also run the global value".
            let mut cursor = hook_value;
            let mut saw_global_marker = false;
            while let Value::Cons(cell) = cursor {
                let (func, next) = {
                    let pair = cell.lock().expect("poisoned");
                    (pair.car.clone(), pair.cdr.clone())
                };
                if func.as_symbol_name() == Some("t") {
                    saw_global_marker = true;
                } else {
                    match callback(eval, func)? {
                        HookControl::Continue => {}
                        HookControl::Return(value) => return Ok(HookControl::Return(value)),
                    }
                }
                cursor = next;
            }

            if saw_global_marker && inherit_global {
                let global_value = eval
                    .obarray()
                    .symbol_value(hook_name)
                    .cloned()
                    .unwrap_or(Value::Nil);
                return walk_hook_value_with(eval, hook_name, global_value, false, callback);
            }
            Ok(HookControl::Continue)
        }
        value => callback(eval, value),
    }
}

fn run_hook_value(
    eval: &mut super::eval::Evaluator,
    hook_name: &str,
    hook_value: Value,
    hook_args: &[Value],
    inherit_global: bool,
) -> Result<(), Flow> {
    let mut callback = |eval: &mut super::eval::Evaluator, value: Value| {
        eval.apply(value, hook_args.to_vec())?;
        Ok(HookControl::Continue)
    };
    match walk_hook_value_with(eval, hook_name, hook_value, inherit_global, &mut callback)? {
        HookControl::Continue | HookControl::Return(_) => Ok(()),
    }
}

pub(crate) fn builtin_run_hooks(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    for hook_sym in &args {
        let hook_name = hook_sym.as_symbol_name().ok_or_else(|| {
            signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), hook_sym.clone()],
            )
        })?;
        let hook_value =
            symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
        run_hook_value(eval, hook_name, hook_value, &[], true)?;
    }
    Ok(Value::Nil)
}

pub(crate) fn builtin_run_hook_with_args(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("run-hook-with-args", &args, 1)?;
    let hook_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let hook_args: Vec<Value> = args[1..].to_vec();
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    run_hook_value(eval, hook_name, hook_value, &hook_args, true)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_run_hook_with_args_until_success(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("run-hook-with-args-until-success", &args, 1)?;
    let hook_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let hook_args: Vec<Value> = args[1..].to_vec();
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    let mut callback = |eval: &mut super::eval::Evaluator, func: Value| {
        let value = eval.apply(func, hook_args.clone())?;
        if value.is_truthy() {
            Ok(HookControl::Return(value))
        } else {
            Ok(HookControl::Continue)
        }
    };
    match walk_hook_value_with(eval, hook_name, hook_value, true, &mut callback)? {
        HookControl::Continue => Ok(Value::Nil),
        HookControl::Return(value) => Ok(value),
    }
}

pub(crate) fn builtin_run_hook_with_args_until_failure(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("run-hook-with-args-until-failure", &args, 1)?;
    let hook_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let hook_args: Vec<Value> = args[1..].to_vec();
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    let mut callback = |eval: &mut super::eval::Evaluator, func: Value| {
        let value = eval.apply(func, hook_args.clone())?;
        if value.is_nil() {
            Ok(HookControl::Return(Value::Nil))
        } else {
            Ok(HookControl::Continue)
        }
    };
    match walk_hook_value_with(eval, hook_name, hook_value, true, &mut callback)? {
        HookControl::Continue => Ok(Value::True),
        HookControl::Return(value) => Ok(value),
    }
}

pub(crate) fn builtin_run_hook_wrapped(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("run-hook-wrapped", &args, 2)?;
    let hook_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let wrapper = args[1].clone();
    let wrapped_args: Vec<Value> = args[2..].to_vec();
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    let mut callback = |eval: &mut super::eval::Evaluator, func: Value| {
        let mut call_args = Vec::with_capacity(wrapped_args.len() + 1);
        call_args.push(func);
        call_args.extend(wrapped_args.clone());
        eval.apply(wrapper.clone(), call_args)?;
        Ok(HookControl::Continue)
    };
    let _ = walk_hook_value_with(eval, hook_name, hook_value, true, &mut callback)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_run_mode_hooks(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    builtin_run_hooks(eval, args)
}

pub(crate) fn builtin_run_hook_query_error_with_timeout(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("run-hook-query-error-with-timeout", &args, 1)?;
    let hook_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    match run_hook_value(eval, hook_name, hook_value, &[], true) {
        Ok(()) => Ok(Value::Nil),
        Err(Flow::Signal(_)) => Err(signal(
            "end-of-file",
            vec![Value::string("Error reading from stdin")],
        )),
        Err(flow) => Err(flow),
    }
}

fn expect_optional_live_frame_designator(
    value: &Value,
    eval: &super::eval::Evaluator,
) -> Result<(), Flow> {
    if value.is_nil() {
        return Ok(());
    }
    if let Value::Frame(id) = value {
        if eval.frames.get(crate::window::FrameId(*id)).is_some() {
            return Ok(());
        }
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("frame-live-p"), value.clone()],
    ))
}

fn expect_optional_live_window_designator(
    value: &Value,
    eval: &super::eval::Evaluator,
) -> Result<(), Flow> {
    if value.is_nil() {
        return Ok(());
    }
    if let Value::Window(id) = value {
        if eval.frames.is_live_window_id(crate::window::WindowId(*id)) {
            return Ok(());
        }
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("window-live-p"), value.clone()],
    ))
}

const WINDOW_CONFIGURATION_TAG: &str = "window-configuration";
const SAVE_SELECTED_WINDOW_STATE_TAG: &str = "save-selected-window--state";

#[derive(Clone)]
struct WindowConfigurationSnapshot {
    frame_id: crate::window::FrameId,
    root_window: crate::window::Window,
    selected_window: crate::window::WindowId,
    minibuffer_window: Option<crate::window::WindowId>,
    minibuffer_leaf: Option<crate::window::Window>,
}

fn window_configuration_snapshot_store() -> &'static Mutex<HashMap<i64, WindowConfigurationSnapshot>> {
    static STORE: OnceLock<Mutex<HashMap<i64, WindowConfigurationSnapshot>>> = OnceLock::new();
    STORE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn window_configuration_parts_from_value(value: &Value) -> Option<(Value, i64)> {
    let Value::Vector(data) = value else {
        return None;
    };
    let items = data.lock().expect("poisoned");
    if items.len() != 3 || items[0].as_symbol_name() != Some(WINDOW_CONFIGURATION_TAG) {
        return None;
    }
    match (&items[1], &items[2]) {
        (Value::Frame(_), Value::Int(serial)) => Some((items[1].clone(), *serial)),
        _ => None,
    }
}

fn window_configuration_frame_from_value(value: &Value) -> Option<Value> {
    window_configuration_parts_from_value(value).map(|(frame, _)| frame)
}

fn next_window_configuration_serial() -> i64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT_WINDOW_CONFIGURATION_ID: AtomicU64 = AtomicU64::new(1);
    NEXT_WINDOW_CONFIGURATION_ID.fetch_add(1, Ordering::Relaxed) as i64
}

fn make_window_configuration_value(frame: Value, serial: i64) -> Value {
    Value::vector(vec![
        Value::symbol(WINDOW_CONFIGURATION_TAG),
        frame,
        Value::Int(serial),
    ])
}

fn builtin_window_configuration_p(args: Vec<Value>) -> EvalResult {
    expect_args("window-configuration-p", &args, 1)?;
    Ok(Value::bool(
        window_configuration_frame_from_value(&args[0]).is_some(),
    ))
}

fn builtin_window_configuration_frame(args: Vec<Value>) -> EvalResult {
    expect_args("window-configuration-frame", &args, 1)?;
    window_configuration_frame_from_value(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("window-configuration-p"), args[0].clone()],
        )
    })
}

fn builtin_window_configuration_equal_p(args: Vec<Value>) -> EvalResult {
    expect_args("window-configuration-equal-p", &args, 2)?;
    if window_configuration_frame_from_value(&args[0]).is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("window-configuration-p"), args[0].clone()],
        ));
    }
    if window_configuration_frame_from_value(&args[1]).is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("window-configuration-p"), args[1].clone()],
        ));
    }
    Ok(Value::bool(equal_value(&args[0], &args[1], 0)))
}

pub(crate) fn builtin_current_window_configuration(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("current-window-configuration", &args, 1)?;

    let frame = if let Some(frame) = args.first() {
        expect_optional_live_frame_designator(frame, eval)?;
        if frame.is_nil() {
            super::window_cmds::builtin_selected_frame(eval, vec![])?
        } else {
            frame.clone()
        }
    } else {
        super::window_cmds::builtin_selected_frame(eval, vec![])?
    };

    let Value::Frame(frame_raw_id) = frame.clone() else {
        return Ok(make_window_configuration_value(frame, next_window_configuration_serial()));
    };
    let frame_id = crate::window::FrameId(frame_raw_id);
    if let Some(frame_state) = eval.frames.get(frame_id) {
        let snapshot = WindowConfigurationSnapshot {
            frame_id,
            root_window: frame_state.root_window.clone(),
            selected_window: frame_state.selected_window,
            minibuffer_window: frame_state.minibuffer_window,
            minibuffer_leaf: frame_state.minibuffer_leaf.clone(),
        };
        let serial = next_window_configuration_serial();
        let mut store = window_configuration_snapshot_store()
            .lock()
            .expect("window-configuration snapshot store poisoned");
        store.insert(serial, snapshot);
        if store.len() > 4096 {
            if let Some(oldest) = store.keys().min().copied() {
                store.remove(&oldest);
            }
        }
        return Ok(make_window_configuration_value(frame, serial));
    }

    Ok(make_window_configuration_value(frame, next_window_configuration_serial()))
}

pub(crate) fn builtin_set_window_configuration(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("set-window-configuration", &args, 1, 3)?;
    let Some((_frame, serial)) = window_configuration_parts_from_value(&args[0]) else {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("window-configuration-p"), args[0].clone()],
        ));
    };

    let snapshot = window_configuration_snapshot_store()
        .lock()
        .expect("window-configuration snapshot store poisoned")
        .get(&serial)
        .cloned();

    if let Some(snapshot) = snapshot {
        let selected_buffer = if let Some(frame) = eval.frames.get_mut(snapshot.frame_id) {
            frame.root_window = snapshot.root_window;
            frame.selected_window = snapshot.selected_window;
            frame.minibuffer_window = snapshot.minibuffer_window;
            frame.minibuffer_leaf = snapshot.minibuffer_leaf;
            frame.find_window(frame.selected_window).and_then(|w| w.buffer_id())
        } else {
            None
        };
        if let Some(buffer_id) = selected_buffer {
            eval.buffers.set_current(buffer_id);
        }
    }

    Ok(Value::True)
}

fn save_selected_window_state_from_value(
    value: &Value,
) -> Option<(Value, Value, Option<crate::buffer::BufferId>)> {
    let Value::Vector(data) = value else {
        return None;
    };
    let items = data.lock().expect("poisoned");
    if items.len() != 4 || items[0].as_symbol_name() != Some(SAVE_SELECTED_WINDOW_STATE_TAG) {
        return None;
    }
    let frame = items[1].clone();
    let window = items[2].clone();
    let buffer_id = match items[3] {
        Value::Buffer(id) => Some(id),
        _ => None,
    };
    Some((frame, window, buffer_id))
}

fn builtin_internal_before_save_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("internal--before-save-selected-window", &args, 0)?;
    let frame = super::window_cmds::builtin_selected_frame(eval, vec![])?;
    let window = super::window_cmds::builtin_selected_window(eval, vec![])?;
    let buffer = eval
        .buffers
        .current_buffer()
        .map(|buffer| Value::Buffer(buffer.id))
        .unwrap_or(Value::Nil);
    Ok(Value::vector(vec![
        Value::symbol(SAVE_SELECTED_WINDOW_STATE_TAG),
        frame,
        window,
        buffer,
    ]))
}

fn builtin_internal_after_save_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("internal--after-save-selected-window", &args, 1)?;
    let Some((saved_frame, saved_window, saved_buffer)) =
        save_selected_window_state_from_value(&args[0])
    else {
        return Err(signal(
            "wrong-type-argument",
            vec![
                Value::symbol("vectorp"),
                args.first().cloned().unwrap_or(Value::Nil),
            ],
        ));
    };

    let _ = super::window_cmds::builtin_select_frame(eval, vec![saved_frame, Value::Nil]);
    let _ = super::window_cmds::builtin_select_window(eval, vec![saved_window, Value::Nil]);
    if let Some(buffer_id) = saved_buffer {
        if eval.buffers.get(buffer_id).is_some() {
            eval.buffers.set_current(buffer_id);
        }
    }
    Ok(Value::Nil)
}

pub(crate) fn builtin_run_window_configuration_change_hook(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("run-window-configuration-change-hook", &args, 1)?;
    if let Some(frame) = args.first() {
        expect_optional_live_frame_designator(frame, eval)?;
    }
    let hook_name = "window-configuration-change-hook";
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    run_hook_value(eval, hook_name, hook_value, &[], true)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_run_window_scroll_functions(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("run-window-scroll-functions", &args, 1)?;
    if let Some(window) = args.first() {
        expect_optional_live_window_designator(window, eval)?;
    }

    let window_arg = args.first().cloned().unwrap_or(Value::Nil);
    let window_start = if window_arg.is_nil() {
        super::window_cmds::builtin_window_start(eval, vec![])?
    } else {
        super::window_cmds::builtin_window_start(eval, vec![window_arg.clone()])?
    };

    let hook_name = "window-scroll-functions";
    let hook_value = symbol_dynamic_buffer_or_global_value(eval, hook_name).unwrap_or(Value::Nil);
    run_hook_value(
        eval,
        hook_name,
        hook_value,
        &[window_arg, window_start],
        true,
    )?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_featurep(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("featurep", &args, 1)?;
    expect_max_args("featurep", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    if !eval.feature_present(name) {
        return Ok(Value::Nil);
    }

    let Some(subfeature) = args.get(1) else {
        return Ok(Value::True);
    };
    if subfeature.is_nil() {
        return Ok(Value::True);
    }

    let subfeatures = eval
        .obarray()
        .get_property(name, "subfeatures")
        .cloned()
        .unwrap_or(Value::Nil);
    let items = list_to_vec(&subfeatures).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), subfeatures.clone()],
        )
    })?;
    Ok(Value::bool(items.iter().any(|item| item == subfeature)))
}

// ===========================================================================
// Loading / eval
// ===========================================================================

/// Convert an EvalError back to a Flow for builtins that call load_file.
fn eval_error_to_flow(e: super::error::EvalError) -> Flow {
    match e {
        super::error::EvalError::Signal { symbol, data } => {
            Flow::Signal(super::error::SignalData {
                symbol,
                data,
                raw_data: None,
            })
        }
        super::error::EvalError::UncaughtThrow { tag, value } => Flow::Throw { tag, value },
    }
}

pub(crate) fn builtin_load(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("load", &args, 1)?;
    let file = expect_string(&args[0])?;
    let noerror = args.get(1).is_some_and(|v| v.is_truthy());
    let nosuffix = args.get(3).is_some_and(|v| v.is_truthy());
    let must_suffix = args.get(4).is_some_and(|v| v.is_truthy());
    let prefer_newer = eval
        .obarray
        .symbol_value("load-prefer-newer")
        .is_some_and(|v| v.is_truthy());

    let load_path = super::load::get_load_path(&eval.obarray);
    match super::load::find_file_in_load_path_with_flags(
        &file,
        &load_path,
        nosuffix,
        must_suffix,
        prefer_newer,
    ) {
        Some(path) => super::load::load_file(eval, &path).map_err(eval_error_to_flow),
        None => {
            // Try as absolute path
            if noerror {
                Ok(Value::Nil)
            } else {
                Err(signal(
                    "file-missing",
                    vec![Value::string(format!("Cannot open load file: {}", file))],
                ))
            }
        }
    }
}

pub(crate) fn builtin_load_file(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("load-file", &args, 1)?;
    let file = expect_string(&args[0])?;
    let path = std::path::Path::new(&file);
    super::load::load_file(eval, path).map_err(eval_error_to_flow)
}

/// `(neovm-precompile-file FILE)` -> cache path string
///
/// NeoVM extension: parse source `.el` and emit internal `.neoc` cache sidecar.
pub(crate) fn builtin_neovm_precompile_file(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("neovm-precompile-file", &args, 1)?;
    let file = expect_string(&args[0])?;
    let path = std::path::Path::new(&file);
    let cache = super::load::precompile_source_file(path).map_err(eval_error_to_flow)?;
    Ok(Value::string(cache.to_string_lossy()))
}

pub(crate) fn builtin_eval(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("eval", &args, 1)?;
    expect_max_args("eval", &args, 2)?;
    // Convert value back to expr and evaluate
    let expr = super::eval::value_to_expr_pub(&args[0]);
    eval.eval(&expr) // eval.eval() already returns EvalResult = Result<Value, Flow>
}

// ===========================================================================
// Math functions (pure)
// ===========================================================================

pub(crate) fn builtin_sqrt(args: Vec<Value>) -> EvalResult {
    expect_args("sqrt", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.sqrt()))
}

pub(crate) fn builtin_sin(args: Vec<Value>) -> EvalResult {
    expect_args("sin", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.sin()))
}

pub(crate) fn builtin_cos(args: Vec<Value>) -> EvalResult {
    expect_args("cos", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.cos()))
}

pub(crate) fn builtin_tan(args: Vec<Value>) -> EvalResult {
    expect_args("tan", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.tan()))
}

pub(crate) fn builtin_asin(args: Vec<Value>) -> EvalResult {
    expect_args("asin", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.asin()))
}

pub(crate) fn builtin_acos(args: Vec<Value>) -> EvalResult {
    expect_args("acos", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.acos()))
}

pub(crate) fn builtin_atan(args: Vec<Value>) -> EvalResult {
    expect_min_args("atan", &args, 1)?;
    if args.len() == 2 {
        let y = expect_number(&args[0])?;
        let x = expect_number(&args[1])?;
        Ok(Value::Float(y.atan2(x)))
    } else {
        Ok(Value::Float(expect_number(&args[0])?.atan()))
    }
}

pub(crate) fn builtin_exp(args: Vec<Value>) -> EvalResult {
    expect_args("exp", &args, 1)?;
    Ok(Value::Float(expect_number(&args[0])?.exp()))
}

pub(crate) fn builtin_log(args: Vec<Value>) -> EvalResult {
    expect_min_args("log", &args, 1)?;
    let val = expect_number(&args[0])?;
    if args.len() == 2 {
        let base = expect_number(&args[1])?;
        Ok(Value::Float(val.ln() / base.ln()))
    } else {
        Ok(Value::Float(val.ln()))
    }
}

pub(crate) fn builtin_expt(args: Vec<Value>) -> EvalResult {
    expect_args("expt", &args, 2)?;
    if has_float(&args) {
        let base = expect_number(&args[0])?;
        let exp = expect_number(&args[1])?;
        Ok(Value::Float(base.powf(exp)))
    } else {
        let base = expect_number(&args[0])? as i64;
        let exp = expect_number(&args[1])? as i64;
        if exp < 0 {
            Ok(Value::Float((base as f64).powf(exp as f64)))
        } else {
            Ok(Value::Int(base.wrapping_pow(exp as u32)))
        }
    }
}

pub(crate) fn builtin_random(args: Vec<Value>) -> EvalResult {
    if args.is_empty() {
        // Random integer
        Ok(Value::Int(rand_simple()))
    } else {
        let limit = expect_int(&args[0])?;
        if limit <= 0 {
            return Err(signal("args-out-of-range", vec![args[0].clone()]));
        }
        Ok(Value::Int(rand_simple().unsigned_abs() as i64 % limit))
    }
}

/// Simple PRNG (xorshift64) — not cryptographically secure.
fn rand_simple() -> i64 {
    use std::cell::Cell;
    thread_local! {
        static STATE: Cell<u64> = Cell::new(0x12345678_9abcdef0);
    }
    STATE.with(|s| {
        let mut x = s.get();
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        s.set(x);
        x as i64
    })
}

pub(crate) fn builtin_isnan(args: Vec<Value>) -> EvalResult {
    expect_args("isnan", &args, 1)?;
    match &args[0] {
        Value::Float(f) => Ok(Value::bool(f.is_nan())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("floatp"), other.clone()],
        )),
    }
}

// ===========================================================================
// Extended string operations
// ===========================================================================

pub(crate) fn builtin_string_prefix_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-prefix-p", &args, 2)?;
    expect_max_args("string-prefix-p", &args, 3)?;
    let mut prefix = expect_string(&args[0])?;
    let mut s = expect_string(&args[1])?;
    if args.get(2).is_some_and(|v| v.is_truthy()) {
        prefix = prefix.to_lowercase();
        s = s.to_lowercase();
    }
    Ok(Value::bool(s.starts_with(&prefix)))
}

pub(crate) fn builtin_string_suffix_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-suffix-p", &args, 2)?;
    expect_max_args("string-suffix-p", &args, 3)?;
    let mut suffix = expect_string(&args[0])?;
    let mut s = expect_string(&args[1])?;
    if args.get(2).is_some_and(|v| v.is_truthy()) {
        suffix = suffix.to_lowercase();
        s = s.to_lowercase();
    }
    Ok(Value::bool(s.ends_with(&suffix)))
}

pub(crate) fn builtin_string_join(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-join", &args, 1)?;
    expect_max_args("string-join", &args, 2)?;
    let strs = list_to_vec(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0].clone()],
        )
    })?;
    let sep = match args.get(1) {
        None | Some(Value::Nil) => "".to_string(),
        Some(other) => expect_string(other)?,
    };
    let parts: Result<Vec<String>, _> = strs.iter().map(expect_string).collect();
    Ok(Value::string(parts?.join(&sep)))
}

pub(crate) fn builtin_split_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("split-string", &args, 1)?;
    expect_max_args("split-string", &args, 4)?;
    let s = expect_string(&args[0])?;

    let separator = match args.get(1) {
        None | Some(Value::Nil) => None,
        Some(other) => Some(expect_string(other)?),
    };
    let omit_nulls = args.get(2).is_some_and(|v| v.is_truthy());
    let trim_regex = match args.get(3) {
        None | Some(Value::Nil) => None,
        Some(other) => Some(expect_string(other)?),
    };

    let (splitter, default_omit_nulls) = match separator {
        Some(pattern) => {
            let compiled = regex::Regex::new(&pattern).map_err(|e| {
                signal(
                    "invalid-regexp",
                    vec![Value::string(format!(
                        "Invalid regexp \"{}\": {}",
                        pattern, e
                    ))],
                )
            })?;
            (compiled, false)
        }
        None => (
            regex::Regex::new(r"[ \f\t\n\r\v]+").expect("default split regexp should compile"),
            true,
        ),
    };

    let trimmer = match trim_regex {
        Some(pattern) => Some(regex::Regex::new(&pattern).map_err(|e| {
            signal(
                "invalid-regexp",
                vec![Value::string(format!(
                    "Invalid regexp \"{}\": {}",
                    pattern, e
                ))],
            )
        })?),
        None => None,
    };

    let should_omit_nulls = default_omit_nulls || omit_nulls;
    let mut parts = Vec::new();
    for part in splitter.split(&s) {
        let mut segment = part.to_string();
        if let Some(trim_re) = trimmer.as_ref() {
            loop {
                let Some(m) = trim_re.find(&segment) else {
                    break;
                };
                if m.start() == 0 && m.end() > 0 {
                    segment = segment[m.end()..].to_string();
                } else {
                    break;
                }
            }
            loop {
                let tail = trim_re
                    .find_iter(&segment)
                    .filter(|m| m.end() == segment.len() && m.start() < m.end())
                    .last();
                let Some(m) = tail else { break };
                segment = segment[..m.start()].to_string();
            }
        }
        if should_omit_nulls && segment.is_empty() {
            continue;
        }
        parts.push(Value::string(segment));
    }

    Ok(Value::list(parts))
}

fn compile_trim_regex(name: &str, pattern: &str) -> Result<regex::Regex, Flow> {
    regex::Regex::new(pattern).map_err(|e| {
        signal(
            "invalid-regexp",
            vec![Value::string(format!(
                "Invalid regexp \"{}\" in {}: {}",
                pattern, name, e
            ))],
        )
    })
}

fn trim_leading_with_regex(input: &str, re: &regex::Regex) -> String {
    let mut out = input.to_string();
    loop {
        let Some(m) = re.find(&out) else { break };
        if m.start() == 0 && m.end() > 0 {
            out = out[m.end()..].to_string();
        } else {
            break;
        }
    }
    out
}

fn trim_trailing_with_regex(input: &str, re: &regex::Regex) -> String {
    let mut out = input.to_string();
    loop {
        let tail = re
            .find_iter(&out)
            .filter(|m| m.end() == out.len() && m.start() < m.end())
            .last();
        let Some(m) = tail else { break };
        out = out[..m.start()].to_string();
    }
    out
}

pub(crate) fn builtin_string_trim(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-trim", &args, 1)?;
    expect_max_args("string-trim", &args, 3)?;
    let s = expect_string(&args[0])?;
    let trim_left_pattern = match args.get(1) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_right_pattern = match args.get(2) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_left = compile_trim_regex("string-trim", &trim_left_pattern)?;
    let trim_right = compile_trim_regex("string-trim", &trim_right_pattern)?;
    let left_trimmed = trim_leading_with_regex(&s, &trim_left);
    Ok(Value::string(trim_trailing_with_regex(
        &left_trimmed,
        &trim_right,
    )))
}

pub(crate) fn builtin_string_trim_left(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-trim-left", &args, 1)?;
    expect_max_args("string-trim-left", &args, 2)?;
    let s = expect_string(&args[0])?;
    let trim_left_pattern = match args.get(1) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_left = compile_trim_regex("string-trim-left", &trim_left_pattern)?;
    Ok(Value::string(trim_leading_with_regex(&s, &trim_left)))
}

pub(crate) fn builtin_string_trim_right(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-trim-right", &args, 1)?;
    expect_max_args("string-trim-right", &args, 2)?;
    let s = expect_string(&args[0])?;
    let trim_right_pattern = match args.get(1) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_right = compile_trim_regex("string-trim-right", &trim_right_pattern)?;
    Ok(Value::string(trim_trailing_with_regex(&s, &trim_right)))
}

pub(crate) fn builtin_make_string(args: Vec<Value>) -> EvalResult {
    expect_args("make-string", &args, 2)?;
    let count_raw = expect_int(&args[0])?;
    if count_raw < 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), args[0].clone()],
        ));
    }
    let count = count_raw as usize;

    let ch = match &args[1] {
        Value::Int(c) => {
            if *c < 0 {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), args[1].clone()],
                ));
            }
            match char::from_u32(*c as u32) {
                Some(ch) => ch,
                None => {
                    if let Some(encoded) = encode_nonunicode_char_for_storage(*c as u32) {
                        return Ok(Value::string(encoded.repeat(count)));
                    }
                    // Emacs accepts broader internal character codes. When these
                    // cannot be represented as Unicode scalar values in Rust, emit
                    // replacement characters to keep observable oracle parity.
                    return Ok(Value::string(
                        "\u{FFFD}\u{FFFD}\u{FFFD}\u{FFFD}".repeat(count),
                    ));
                }
            }
        }
        Value::Char(c) => *c,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), other.clone()],
            ))
        }
    };
    Ok(Value::string(
        std::iter::repeat(ch).take(count).collect::<String>(),
    ))
}

pub(crate) fn builtin_string(args: Vec<Value>) -> EvalResult {
    let mut result = String::new();
    for arg in args {
        match arg {
            Value::Char(c) => result.push(c),
            Value::Int(code) => {
                if code < 0 {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("characterp"), Value::Int(code)],
                    ));
                }
                if let Some(ch) = char::from_u32(code as u32) {
                    result.push(ch);
                } else if let Some(encoded) = encode_nonunicode_char_for_storage(code as u32) {
                    result.push_str(&encoded);
                } else {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("characterp"), Value::Int(code)],
                    ));
                }
            }
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), other],
                ));
            }
        }
    }
    Ok(Value::string(result))
}

/// `(unibyte-string &rest BYTES)` -> unibyte storage string.
pub(crate) fn builtin_unibyte_string(args: Vec<Value>) -> EvalResult {
    let mut bytes = Vec::with_capacity(args.len());
    for arg in args {
        let n = match arg {
            Value::Int(v) => v,
            Value::Char(c) => c as i64,
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("integerp"), other],
                ))
            }
        };
        if !(0..=255).contains(&n) {
            return Err(signal(
                "args-out-of-range",
                vec![Value::Int(n), Value::Int(0), Value::Int(255)],
            ));
        }
        bytes.push(n as u8);
    }
    Ok(Value::string(bytes_to_unibyte_storage_string(&bytes)))
}

pub(crate) fn builtin_byte_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("byte-to-string", &args, 1)?;
    let byte = expect_fixnum(&args[0])?;
    if !(0..=255).contains(&byte) {
        return Err(signal("error", vec![Value::string("Invalid byte")]));
    }
    Ok(Value::string(bytes_to_unibyte_storage_string(
        &[byte as u8],
    )))
}

pub(crate) fn builtin_bitmap_spec_p(args: Vec<Value>) -> EvalResult {
    expect_args("bitmap-spec-p", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_clear_face_cache(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-face-cache", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_clear_buffer_auto_save_failure(args: Vec<Value>) -> EvalResult {
    expect_args("clear-buffer-auto-save-failure", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_string_to_list(args: Vec<Value>) -> EvalResult {
    expect_args("string-to-list", &args, 1)?;
    let s = expect_string(&args[0])?;
    let chars: Vec<Value> = decode_storage_char_codes(&s)
        .into_iter()
        .map(|cp| Value::Int(cp as i64))
        .collect();
    Ok(Value::list(chars))
}

pub(crate) fn builtin_string_width(args: Vec<Value>) -> EvalResult {
    expect_args("string-width", &args, 1)?;
    let s = expect_string(&args[0])?;
    Ok(Value::Int(storage_string_display_width(&s) as i64))
}

// ===========================================================================
// Extended list operations
// ===========================================================================

pub(crate) fn builtin_last(args: Vec<Value>) -> EvalResult {
    expect_min_args("last", &args, 1)?;
    let n = if args.len() > 1 && !args[1].is_nil() {
        expect_number_or_marker(&args[1])?
    } else {
        NumberOrMarker::Int(1)
    };

    match n {
        NumberOrMarker::Int(n) => {
            if n < 0 {
                return Ok(Value::Nil);
            }

            let mut lag = args[0].clone();
            let mut lead = args[0].clone();
            for _ in 0..(n as usize) {
                match lead {
                    Value::Cons(cell) => {
                        lead = cell.lock().expect("poisoned").cdr.clone();
                    }
                    _ => return Ok(lag),
                }
            }

            loop {
                match lead {
                    Value::Cons(cell) => {
                        lead = cell.lock().expect("poisoned").cdr.clone();
                        lag = match lag {
                            Value::Cons(lag_cell) => lag_cell.lock().expect("poisoned").cdr.clone(),
                            _ => unreachable!("lag should be a cons while lead is a cons"),
                        };
                    }
                    _ => return Ok(lag),
                }
            }
        }
        NumberOrMarker::Float(n) => {
            if n < 0.0 {
                return Ok(Value::Nil);
            }

            if let Some(len) = list_length(&args[0]) {
                let remaining = len as f64 - n;
                if remaining > 0.0 {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("integerp"), Value::Float(remaining)],
                    ));
                }
            }
            Ok(args[0].clone())
        }
    }
}

pub(crate) fn builtin_butlast(args: Vec<Value>) -> EvalResult {
    expect_min_args("butlast", &args, 1)?;
    let n = if args.len() > 1 && !args[1].is_nil() {
        expect_number_or_marker(&args[1])?
    } else {
        NumberOrMarker::Int(1)
    };

    let n_non_positive = match n {
        NumberOrMarker::Int(v) => v <= 0,
        NumberOrMarker::Float(v) => v <= 0.0,
    };
    if n_non_positive {
        return Ok(args[0].clone());
    }

    match &args[0] {
        Value::Nil | Value::Cons(_) => {}
        Value::Vector(_) | Value::Str(_) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), args[0].clone()],
            ))
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("sequencep"), args[0].clone()],
            ))
        }
    }

    let mut items = Vec::new();
    let mut cursor = args[0].clone();
    loop {
        match cursor {
            Value::Nil => break,
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                items.push(pair.car.clone());
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

    match n {
        NumberOrMarker::Int(v) => {
            let keep = items.len().saturating_sub(v as usize);
            Ok(Value::list(items[..keep].to_vec()))
        }
        NumberOrMarker::Float(v) => Err(signal(
            "wrong-type-argument",
            vec![
                Value::symbol("integerp"),
                Value::Float(items.len() as f64 - v),
            ],
        )),
    }
}

fn delete_from_list_in_place<F>(seq: &Value, should_delete: F) -> Result<Value, Flow>
where
    F: Fn(&Value) -> bool,
{
    let mut probe = seq.clone();
    loop {
        match probe {
            Value::Nil => break,
            Value::Cons(cell) => {
                probe = cell.lock().expect("poisoned").cdr.clone();
            }
            tail => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), tail],
                ))
            }
        }
    }

    let mut head = seq.clone();
    loop {
        match head.clone() {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let remove = {
                    let pair = cell.lock().expect("poisoned");
                    should_delete(&pair.car)
                };
                if remove {
                    head = cell.lock().expect("poisoned").cdr.clone();
                } else {
                    break;
                }
            }
            _ => unreachable!("list shape checked above"),
        }
    }

    let mut prev = match &head {
        Value::Cons(cell) => cell.clone(),
        Value::Nil => return Ok(Value::Nil),
        _ => unreachable!("head must be list"),
    };

    loop {
        let next = prev.lock().expect("poisoned").cdr.clone();
        match next {
            Value::Nil => break,
            Value::Cons(next_cell) => {
                let remove = {
                    let pair = next_cell.lock().expect("poisoned");
                    should_delete(&pair.car)
                };
                if remove {
                    let after = next_cell.lock().expect("poisoned").cdr.clone();
                    prev.lock().expect("poisoned").cdr = after;
                } else {
                    prev = next_cell;
                }
            }
            _ => unreachable!("list shape checked above"),
        }
    }

    Ok(head)
}

pub(crate) fn builtin_delete(args: Vec<Value>) -> EvalResult {
    expect_args("delete", &args, 2)?;
    let elt = &args[0];
    match &args[1] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => delete_from_list_in_place(&args[1], |item| equal_value(elt, item, 0)),
        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            let mut changed = false;
            let mut kept = Vec::with_capacity(items.len());
            for item in items.iter() {
                if equal_value(elt, item, 0) {
                    changed = true;
                } else {
                    kept.push(item.clone());
                }
            }
            if changed {
                Ok(Value::vector(kept))
            } else {
                Ok(args[1].clone())
            }
        }
        Value::Str(s) => {
            let mut changed = false;
            let mut kept = Vec::new();
            for cp in decode_storage_char_codes(s) {
                let ch = Value::Int(cp as i64);
                if equal_value(elt, &ch, 0) {
                    changed = true;
                } else {
                    kept.push(ch);
                }
            }
            if !changed {
                return Ok(args[1].clone());
            }
            builtin_concat(vec![Value::list(kept)])
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_delq(args: Vec<Value>) -> EvalResult {
    expect_args("delq", &args, 2)?;
    let elt = &args[0];
    match &args[1] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => delete_from_list_in_place(&args[1], |item| eq_value(elt, item)),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[1].clone()],
        )),
    }
}

pub(crate) fn builtin_elt(args: Vec<Value>) -> EvalResult {
    expect_args("elt", &args, 2)?;
    match &args[0] {
        Value::Cons(_) | Value::Nil => builtin_nth(vec![args[1].clone(), args[0].clone()]),
        Value::Vector(_) | Value::Str(_) => builtin_aref(vec![args[0].clone(), args[1].clone()]),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

pub(crate) fn builtin_nconc(args: Vec<Value>) -> EvalResult {
    if args.is_empty() {
        return Ok(Value::Nil);
    }

    let mut result_head: Option<Value> = None;
    let mut last_cons: Option<Value> = None;

    for (index, arg) in args.iter().enumerate() {
        let is_last = index + 1 == args.len();

        if is_last {
            if let Some(Value::Cons(cell)) = &last_cons {
                cell.lock().expect("poisoned").cdr = arg.clone();
                return Ok(result_head.unwrap_or_else(|| arg.clone()));
            }
            return Ok(arg.clone());
        }

        match arg {
            Value::Nil => continue,
            Value::Cons(head) => {
                if result_head.is_none() {
                    result_head = Some(arg.clone());
                }
                if let Some(Value::Cons(prev)) = &last_cons {
                    prev.lock().expect("poisoned").cdr = arg.clone();
                }

                let mut tail = head.clone();
                loop {
                    let next = tail.lock().expect("poisoned").cdr.clone();
                    match next {
                        Value::Cons(next_cell) => tail = next_cell,
                        _ => {
                            last_cons = Some(Value::Cons(tail.clone()));
                            break;
                        }
                    }
                }
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("consp"), arg.clone()],
                ))
            }
        }
    }

    Ok(result_head.unwrap_or(Value::Nil))
}

pub(crate) fn builtin_alist_get(args: Vec<Value>) -> EvalResult {
    expect_min_args("alist-get", &args, 2)?;
    let key = &args[0];
    let default = args.get(2).cloned().unwrap_or(Value::Nil);
    let _remove = args.get(3); // not used
    let use_equal = args.get(4).is_some_and(|v| v.is_truthy());

    let mut cursor = args[1].clone();
    loop {
        match cursor {
            Value::Nil => return Ok(default),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let entry = pair.car.clone();
                cursor = pair.cdr.clone();
                drop(pair);

                if let Value::Cons(entry_cell) = entry {
                    let entry_pair = entry_cell.lock().expect("poisoned");
                    let matches = if use_equal {
                        equal_value(key, &entry_pair.car, 0)
                    } else {
                        eq_value(key, &entry_pair.car)
                    };
                    if matches {
                        return Ok(entry_pair.cdr.clone());
                    }
                }
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), args[1].clone()],
                ))
            }
        }
    }
}

pub(crate) fn builtin_alist_get_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("alist-get", &args, 2)?;
    let key = &args[0];
    let default = args.get(2).cloned().unwrap_or(Value::Nil);
    let _remove = args.get(3); // not used
    let test_fn = args.get(4).and_then(|value| {
        if value.is_nil() {
            None
        } else {
            Some(value.clone())
        }
    });

    let mut cursor = args[1].clone();
    loop {
        match cursor {
            Value::Nil => return Ok(default),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let entry = pair.car.clone();
                cursor = pair.cdr.clone();
                drop(pair);

                if let Value::Cons(entry_cell) = entry {
                    let entry_pair = entry_cell.lock().expect("poisoned");
                    let matches = if let Some(test_fn) = &test_fn {
                        eval.apply(test_fn.clone(), vec![key.clone(), entry_pair.car.clone()])?
                            .is_truthy()
                    } else {
                        equal_value(key, &entry_pair.car, 0)
                    };
                    if matches {
                        return Ok(entry_pair.cdr.clone());
                    }
                }
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), args[1].clone()],
                ))
            }
        }
    }
}

pub(crate) fn builtin_number_sequence(args: Vec<Value>) -> EvalResult {
    expect_min_args("number-sequence", &args, 1)?;
    let from = expect_int(&args[0])?;
    let to = if args.len() > 1 {
        match &args[1] {
            Value::Nil => return Ok(Value::list(vec![Value::Int(from)])),
            v => expect_int(v)?,
        }
    } else {
        return Ok(Value::list(vec![Value::Int(from)]));
    };
    let step = if args.len() > 2 {
        expect_int(&args[2])?
    } else if from <= to {
        1
    } else {
        -1
    };

    if step == 0 {
        return Err(signal("args-out-of-range", vec![Value::Int(0)]));
    }

    let mut result = Vec::new();
    let mut i = from;
    if step > 0 {
        while i <= to {
            result.push(Value::Int(i));
            i += step;
        }
    } else {
        while i >= to {
            result.push(Value::Int(i));
            i += step;
        }
    }
    Ok(Value::list(result))
}

// ===========================================================================
// Misc builtins
// ===========================================================================

fn dynamic_or_global_symbol_value(eval: &super::eval::Evaluator, name: &str) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get(name) {
            return Some(value.clone());
        }
    }
    eval.obarray.symbol_value(name).cloned()
}

fn buffer_read_only_active(eval: &super::eval::Evaluator, buf: &crate::buffer::Buffer) -> bool {
    if buf.read_only {
        return true;
    }

    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get("buffer-read-only") {
            return value.is_truthy();
        }
    }

    if let Some(value) = buf.get_buffer_local("buffer-read-only") {
        return value.is_truthy();
    }

    eval.obarray
        .symbol_value("buffer-read-only")
        .is_some_and(|value| value.is_truthy())
}

pub(crate) fn builtin_barf_if_buffer_read_only(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("barf-if-buffer-read-only", &args, 1)?;
    let position = match args.first() {
        None | Some(Value::Nil) => None,
        Some(value) => Some(expect_fixnum(value)?),
    };

    let Some(buf) = eval.buffers.current_buffer() else {
        return Ok(Value::Nil);
    };
    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let read_only = buffer_read_only_active(eval, buf);
    let buffer_name = buf.name.clone();
    if !read_only {
        return Ok(Value::Nil);
    }
    if let Some(pos) = position {
        if pos < point_min {
            return Err(signal(
                "args-out-of-range",
                vec![Value::Int(pos), Value::Int(pos)],
            ));
        }
    }
    Err(signal("buffer-read-only", vec![Value::string(buffer_name)]))
}

pub(crate) fn builtin_bury_buffer_internal(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bury-buffer-internal", &args, 1)?;
    let id = expect_buffer_id(&args[0])?;
    let _ = eval.buffers.get(id);
    Ok(Value::Nil)
}

pub(crate) fn builtin_cancel_kbd_macro_events(args: Vec<Value>) -> EvalResult {
    expect_args("cancel-kbd-macro-events", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_combine_after_change_execute(args: Vec<Value>) -> EvalResult {
    expect_args("combine-after-change-execute", &args, 0)?;
    Ok(Value::Nil)
}

fn resolve_print_target(eval: &super::eval::Evaluator, printcharfun: Option<&Value>) -> Value {
    match printcharfun {
        Some(dest) if !dest.is_nil() => dest.clone(),
        _ => dynamic_or_global_symbol_value(eval, "standard-output").unwrap_or(Value::True),
    }
}

fn write_print_output(
    eval: &mut super::eval::Evaluator,
    printcharfun: Option<&Value>,
    text: &str,
) -> Result<(), Flow> {
    let target = resolve_print_target(eval, printcharfun);
    match target {
        Value::True => Ok(()),
        Value::Buffer(id) => {
            let Some(buf) = eval.buffers.get_mut(id) else {
                return Err(signal(
                    "error",
                    vec![Value::string("Output buffer no longer exists")],
                ));
            };
            buf.insert(text);
            Ok(())
        }
        Value::Str(name) => {
            let Some(id) = eval.buffers.find_buffer_by_name(&name) else {
                return Err(signal(
                    "error",
                    vec![Value::string(format!("No buffer named {name}"))],
                ));
            };
            let Some(buf) = eval.buffers.get_mut(id) else {
                return Err(signal(
                    "error",
                    vec![Value::string("Output buffer no longer exists")],
                ));
            };
            buf.insert(text);
            Ok(())
        }
        _ => Ok(()),
    }
}

fn write_terpri_output(eval: &mut super::eval::Evaluator, target: Value) -> Result<(), Flow> {
    match target {
        Value::True | Value::Nil => Ok(()),
        Value::Buffer(id) => {
            let Some(buf) = eval.buffers.get_mut(id) else {
                return Err(signal(
                    "error",
                    vec![Value::string("Output buffer no longer exists")],
                ));
            };
            buf.insert("\n");
            Ok(())
        }
        Value::Str(name) => {
            let Some(id) = eval.buffers.find_buffer_by_name(&name) else {
                return Err(signal(
                    "error",
                    vec![Value::string(format!("No buffer named {name}"))],
                ));
            };
            let Some(buf) = eval.buffers.get_mut(id) else {
                return Err(signal(
                    "error",
                    vec![Value::string("Output buffer no longer exists")],
                ));
            };
            buf.insert("\n");
            Ok(())
        }
        other => {
            eval.apply(other, vec![Value::Int('\n' as i64)])?;
            Ok(())
        }
    }
}

fn print_threading_handle(eval: &super::eval::Evaluator, value: &Value) -> Option<String> {
    if let Some(handle) = super::display::print_terminal_handle(value) {
        return Some(handle);
    }
    if let Some(id) = eval.threads.thread_id_from_handle(value) {
        return Some(format!("#<thread {id}>"));
    }
    if let Some(id) = eval.threads.mutex_id_from_handle(value) {
        return Some(format!("#<mutex {id}>"));
    }
    if let Some(id) = eval.threads.condition_variable_id_from_handle(value) {
        return Some(format!("#<condvar {id}>"));
    }
    None
}

fn print_value_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    print_threading_handle(eval, value).unwrap_or_else(|| super::print::print_value(value))
}

fn princ_text_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    match value {
        Value::Str(s) => (**s).clone(),
        Value::Char(c) => (*c as u32).to_string(),
        other => print_value_eval(eval, other),
    }
}

fn prin1_to_string_value(value: &Value, noescape: bool) -> String {
    if noescape {
        match value {
            Value::Str(s) => (**s).clone(),
            other => super::print::print_value(other),
        }
    } else {
        bytes_to_storage_string(&super::print::print_value_bytes(value))
    }
}

fn prin1_to_string_value_eval(
    eval: &super::eval::Evaluator,
    value: &Value,
    noescape: bool,
) -> String {
    if noescape {
        match value {
            Value::Str(s) => (**s).clone(),
            other => print_value_eval(eval, other),
        }
    } else {
        if let Some(handle) = print_threading_handle(eval, value) {
            handle
        } else {
            bytes_to_storage_string(&super::print::print_value_bytes(value))
        }
    }
}

pub(crate) fn builtin_princ(args: Vec<Value>) -> EvalResult {
    expect_min_args("princ", &args, 1)?;
    // In real Emacs this prints to standard output; here just return the value
    Ok(args[0].clone())
}

pub(crate) fn builtin_prin1(args: Vec<Value>) -> EvalResult {
    expect_min_args("prin1", &args, 1)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_princ_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("princ", &args, 1)?;
    let text = princ_text_eval(eval, &args[0]);
    write_print_output(eval, args.get(1), &text)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_prin1_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("prin1", &args, 1)?;
    let text = print_value_eval(eval, &args[0]);
    write_print_output(eval, args.get(1), &text)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_prin1_to_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("prin1-to-string", &args, 1)?;
    let noescape = args.get(1).is_some_and(|v| v.is_truthy());
    Ok(Value::string(prin1_to_string_value(&args[0], noescape)))
}

pub(crate) fn builtin_prin1_to_string_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("prin1-to-string", &args, 1)?;
    let noescape = args.get(1).is_some_and(|v| v.is_truthy());
    Ok(Value::string(prin1_to_string_value_eval(
        eval, &args[0], noescape,
    )))
}

pub(crate) fn builtin_print(args: Vec<Value>) -> EvalResult {
    expect_min_args("print", &args, 1)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_terpri(args: Vec<Value>) -> EvalResult {
    expect_max_args("terpri", &args, 2)?;
    Ok(Value::True)
}

pub(crate) fn builtin_print_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("print", &args, 1)?;
    let mut text = String::new();
    text.push('\n');
    text.push_str(&print_value_eval(eval, &args[0]));
    text.push('\n');
    write_print_output(eval, args.get(1), &text)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_terpri_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("terpri", &args, 2)?;
    let target = resolve_print_target(eval, args.first());
    write_terpri_output(eval, target)?;
    Ok(Value::True)
}

fn write_char_rendered_text(char_code: i64) -> Option<String> {
    if !(0..=u32::MAX as i64).contains(&char_code) {
        return None;
    }
    let code = char_code as u32;
    char::from_u32(code)
        .map(|ch| ch.to_string())
        .or_else(|| encode_nonunicode_char_for_storage(code))
}

pub(crate) fn builtin_write_char(args: Vec<Value>) -> EvalResult {
    expect_range_args("write-char", &args, 1, 2)?;
    let char_code = expect_fixnum(&args[0])?;
    Ok(Value::Int(char_code))
}

pub(crate) fn builtin_write_char_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("write-char", &args, 1, 2)?;
    let char_code = expect_fixnum(&args[0])?;
    let target = resolve_print_target(eval, args.get(1));

    match target {
        Value::True | Value::Nil => {}
        Value::Buffer(id) => {
            if let Some(text) = write_char_rendered_text(char_code) {
                let Some(buf) = eval.buffers.get_mut(id) else {
                    return Err(signal(
                        "error",
                        vec![Value::string("Output buffer no longer exists")],
                    ));
                };
                buf.insert(&text);
            }
        }
        Value::Str(name) => {
            if let Some(text) = write_char_rendered_text(char_code) {
                let Some(id) = eval.buffers.find_buffer_by_name(&name) else {
                    return Err(signal(
                        "error",
                        vec![Value::string(format!("No buffer named {name}"))],
                    ));
                };
                let Some(buf) = eval.buffers.get_mut(id) else {
                    return Err(signal(
                        "error",
                        vec![Value::string("Output buffer no longer exists")],
                    ));
                };
                buf.insert(&text);
            }
        }
        other => {
            eval.apply(other, vec![Value::Int(char_code)])?;
        }
    }

    Ok(Value::Int(char_code))
}

pub(crate) fn builtin_propertize(args: Vec<Value>) -> EvalResult {
    expect_min_args("propertize", &args, 1)?;

    let s = match &args[0] {
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ));
        }
    };

    // `propertize` requires an odd argument count: 1 string + plist pairs.
    if args.len() % 2 == 0 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("propertize"), Value::Int(args.len() as i64)],
        ));
    }

    // NeoVM does not attach text properties to string values yet.
    Ok(Value::string(s))
}

pub(crate) fn builtin_gensym(args: Vec<Value>) -> EvalResult {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    expect_max_args("gensym", &args, 1)?;
    let prefix = if !args.is_empty() {
        expect_string(&args[0])?
    } else {
        "g".to_string()
    };
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(Value::Symbol(format!("{}{}", prefix, n)))
}

pub(crate) fn builtin_string_to_syntax(args: Vec<Value>) -> EvalResult {
    super::syntax::builtin_string_to_syntax(args)
}

pub(crate) fn builtin_current_time(args: Vec<Value>) -> EvalResult {
    expect_args("current-time", &args, 0)?;
    use std::time::{SystemTime, UNIX_EPOCH};
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let secs = dur.as_secs() as i64;
    let usecs = dur.subsec_micros() as i64;
    Ok(Value::list(vec![
        Value::Int(secs >> 16),
        Value::Int(secs & 0xFFFF),
        Value::Int(usecs),
    ]))
}

pub(crate) fn builtin_current_cpu_time(args: Vec<Value>) -> EvalResult {
    expect_args("current-cpu-time", &args, 0)?;
    use std::sync::OnceLock;
    use std::time::Instant;
    static CPU_TIME_START: OnceLock<Instant> = OnceLock::new();
    let start = CPU_TIME_START.get_or_init(Instant::now);
    let ticks = start.elapsed().as_micros() as i64;
    Ok(Value::cons(Value::Int(ticks), Value::Int(1_000_000)))
}

pub(crate) fn builtin_current_idle_time(args: Vec<Value>) -> EvalResult {
    expect_args("current-idle-time", &args, 0)?;
    // Batch mode does not track UI idle duration; Oracle returns nil here.
    Ok(Value::Nil)
}

fn number_or_marker_to_f64(value: NumberOrMarker) -> f64 {
    match value {
        NumberOrMarker::Int(n) => n as f64,
        NumberOrMarker::Float(f) => f,
    }
}

fn decode_float_time_arg(value: &Value) -> Result<f64, Flow> {
    let invalid_time_spec = || signal("error", vec![Value::string("Invalid time specification")]);
    let parse_number = |v: &Value| expect_number_or_marker(v).map(number_or_marker_to_f64);

    match value {
        Value::Cons(_) => {
            let items = list_to_vec(value).ok_or_else(invalid_time_spec)?;
            if items.len() < 2 {
                return Err(invalid_time_spec());
            }

            let high = parse_number(&items[0]).map_err(|_| invalid_time_spec())?;
            let low = parse_number(&items[1]).map_err(|_| invalid_time_spec())?;
            let mut seconds = high * 65536.0 + low;
            if let Some(usec) = items.get(2) {
                seconds += parse_number(usec).map_err(|_| invalid_time_spec())? / 1_000_000.0;
            }
            if let Some(psec) = items.get(3) {
                seconds +=
                    parse_number(psec).map_err(|_| invalid_time_spec())? / 1_000_000_000_000.0;
            }
            Ok(seconds)
        }
        _ => Ok(parse_number(value).map_err(|_| invalid_time_spec())?),
    }
}

pub(crate) fn builtin_float_time(args: Vec<Value>) -> EvalResult {
    expect_max_args("float-time", &args, 1)?;
    if let Some(specified_time) = args.first() {
        if !specified_time.is_nil() {
            return Ok(Value::Float(decode_float_time_arg(specified_time)?));
        }
    }
    use std::time::{SystemTime, UNIX_EPOCH};
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    Ok(Value::Float(dur.as_secs_f64()))
}

// ===========================================================================
// Buffer operations (require evaluator for BufferManager access)
// ===========================================================================

use crate::buffer::BufferId;

fn expect_buffer_id(value: &Value) -> Result<BufferId, Flow> {
    match value {
        Value::Buffer(id) => Ok(*id),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("bufferp"), other.clone()],
        )),
    }
}

fn canonicalize_or_self(path: &str) -> String {
    std::fs::canonicalize(path)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| path.to_string())
}

/// (get-buffer-create NAME) → buffer
pub(crate) fn builtin_get_buffer_create(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("get-buffer-create", &args, 1)?;
    expect_max_args("get-buffer-create", &args, 2)?;
    let name = expect_string(&args[0])?;
    if let Some(id) = eval.buffers.find_buffer_by_name(&name) {
        Ok(Value::Buffer(id))
    } else {
        let id = eval.buffers.create_buffer(&name);
        Ok(Value::Buffer(id))
    }
}

/// (get-buffer NAME-OR-BUFFER) → buffer or nil
pub(crate) fn builtin_get_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("get-buffer", &args, 1)?;
    match &args[0] {
        Value::Buffer(_) => Ok(args[0].clone()),
        Value::Str(s) => {
            if let Some(id) = eval.buffers.find_buffer_by_name(s) {
                Ok(Value::Buffer(id))
            } else {
                Ok(Value::Nil)
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// `(find-buffer VARIABLE VALUE)` -> buffer or nil.
///
/// Returns the first live buffer whose VARIABLE value is `eq` to VALUE.
/// Buffer-local bindings take precedence; otherwise dynamic/global bindings are
/// used as fallback. Signals `void-variable` when VARIABLE is unbound.
pub(crate) fn builtin_find_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("find-buffer", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let target_value = args[1].clone();

    let fallback_value = eval
        .dynamic
        .iter()
        .rev()
        .find_map(|frame| frame.get(name).cloned())
        .or_else(|| eval.obarray().symbol_value(name).cloned())
        .ok_or_else(|| signal("void-variable", vec![Value::symbol(name)]))?;

    let mut scan_order = Vec::new();
    let current_id = eval.buffers.current_buffer().map(|buf| buf.id);
    if let Some(id) = current_id {
        scan_order.push(id);
    }
    for id in eval.buffers.buffer_list() {
        if Some(id) != current_id {
            scan_order.push(id);
        }
    }

    for id in scan_order {
        let Some(buf) = eval.buffers.get(id) else {
            continue;
        };
        let observed = buf
            .get_buffer_local(name)
            .cloned()
            .unwrap_or_else(|| fallback_value.clone());
        if eq_value(&observed, &target_value) {
            return Ok(Value::Buffer(id));
        }
    }

    Ok(Value::Nil)
}

/// `(delete-all-overlays &optional BUFFER)` -> nil
///
/// Removes every overlay from BUFFER (or the current buffer when omitted/nil).
pub(crate) fn builtin_delete_all_overlays(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("delete-all-overlays", &args, 1)?;
    let target = if args.is_empty() || args[0].is_nil() {
        eval.buffers.current_buffer().map(|buf| buf.id)
    } else {
        Some(expect_buffer_id(&args[0])?)
    };

    let Some(target_id) = target else {
        return Ok(Value::Nil);
    };
    let Some(buf) = eval.buffers.get_mut(target_id) else {
        // GNU Emacs treats dead buffers as a no-op.
        return Ok(Value::Nil);
    };

    let ids = buf.overlays.overlays_in(buf.point_min(), buf.point_max());
    for ov_id in ids {
        buf.overlays.delete_overlay(ov_id);
    }
    Ok(Value::Nil)
}

/// (buffer-live-p OBJECT) -> t or nil
pub(crate) fn builtin_buffer_live_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("buffer-live-p", &args, 1)?;
    match &args[0] {
        Value::Buffer(id) => Ok(Value::bool(eval.buffers.get(*id).is_some())),
        _ => Ok(Value::Nil),
    }
}

/// (get-file-buffer FILENAME) -> buffer or nil
pub(crate) fn builtin_get_file_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("get-file-buffer", &args, 1)?;
    let filename = expect_string(&args[0])?;
    let resolved = super::fileio::resolve_filename_for_eval(eval, &filename);
    let resolved_true = canonicalize_or_self(&resolved);

    for id in eval.buffers.buffer_list() {
        let Some(buf) = eval.buffers.get(id) else {
            continue;
        };
        let Some(file_name) = &buf.file_name else {
            continue;
        };

        let candidate = super::fileio::resolve_filename_for_eval(eval, file_name);
        if candidate == resolved {
            return Ok(Value::Buffer(id));
        }
        if canonicalize_or_self(&candidate) == resolved_true {
            return Ok(Value::Buffer(id));
        }
    }

    Ok(Value::Nil)
}

/// (kill-buffer &optional BUFFER-OR-NAME) → t or nil
pub(crate) fn builtin_kill_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("kill-buffer", &args, 1)?;
    let id = match args.first() {
        None | Some(Value::Nil) => match eval.buffers.current_buffer() {
            Some(buf) => buf.id,
            None => return Ok(Value::Nil),
        },
        Some(Value::Buffer(id)) => {
            if eval.buffers.get(*id).is_none() {
                return Ok(Value::Nil);
            }
            *id
        }
        Some(Value::Str(name)) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => {
                return Err(signal(
                    "error",
                    vec![Value::string(format!("No buffer named {name}"))],
                ))
            }
        },
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };

    let was_current = eval.buffers.current_buffer().map(|buf| buf.id) == Some(id);
    let replacement = if was_current {
        match builtin_other_buffer(eval, vec![Value::Buffer(id)])? {
            Value::Buffer(next) if next != id => Some(next),
            _ => None,
        }
    } else {
        None
    };

    if !eval.buffers.kill_buffer(id) {
        return Ok(Value::Nil);
    }

    // Ensure dead-buffer windows continue to point at a live fallback buffer.
    let scratch = eval
        .buffers
        .find_buffer_by_name("*scratch*")
        .unwrap_or_else(|| eval.buffers.create_buffer("*scratch*"));
    eval.frames.replace_buffer_in_windows(id, scratch);

    if was_current {
        if let Some(next) = replacement {
            if eval.buffers.get(next).is_some() {
                eval.buffers.set_current(next);
            }
        }
        if eval.buffers.current_buffer().is_none() {
            if let Some(next) = eval.buffers.buffer_list().into_iter().next() {
                eval.buffers.set_current(next);
            } else {
                eval.buffers.set_current(scratch);
            }
        }
    }

    Ok(Value::True)
}

/// (set-buffer BUFFER-OR-NAME) → buffer
pub(crate) fn builtin_set_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-buffer", &args, 1)?;
    let id = match &args[0] {
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_none() {
                return Err(signal(
                    "error",
                    vec![Value::string("Selecting deleted buffer")],
                ));
            }
            *id
        }
        Value::Str(s) => eval
            .buffers
            .find_buffer_by_name(s)
            .ok_or_else(|| signal("error", vec![Value::string(format!("No buffer named {s}"))]))?,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    eval.buffers.set_current(id);
    Ok(Value::Buffer(id))
}

/// (current-buffer) → buffer
pub(crate) fn builtin_current_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("current-buffer", &args, 0)?;
    match eval.buffers.current_buffer() {
        Some(buf) => Ok(Value::Buffer(buf.id)),
        None => Ok(Value::Nil),
    }
}

/// (buffer-name &optional BUFFER) → string
pub(crate) fn builtin_buffer_name(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-name", &args, 1)?;
    let id = if args.is_empty() || matches!(args[0], Value::Nil) {
        match eval.buffers.current_buffer() {
            Some(b) => b.id,
            None => return Ok(Value::Nil),
        }
    } else {
        expect_buffer_id(&args[0])?
    };
    match eval.buffers.get(id) {
        Some(buf) => Ok(Value::string(&buf.name)),
        None => Ok(Value::Nil),
    }
}

/// (buffer-file-name &optional BUFFER) → string or nil
pub(crate) fn builtin_buffer_file_name(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-file-name", &args, 1)?;
    let id = if args.is_empty() || matches!(args[0], Value::Nil) {
        match eval.buffers.current_buffer() {
            Some(b) => b.id,
            None => return Ok(Value::Nil),
        }
    } else {
        expect_buffer_id(&args[0])?
    };
    match eval.buffers.get(id) {
        Some(buf) => match &buf.file_name {
            Some(f) => Ok(Value::string(f)),
            None => Ok(Value::Nil),
        },
        None => Ok(Value::Nil),
    }
}

/// (buffer-base-buffer &optional BUFFER) → buffer or nil
pub(crate) fn builtin_buffer_base_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-base-buffer", &args, 1)?;
    let target = if args.is_empty() || matches!(args[0], Value::Nil) {
        match eval.buffers.current_buffer() {
            Some(buf) => buf.id,
            None => return Ok(Value::Nil),
        }
    } else {
        expect_buffer_id(&args[0])?
    };

    // NeoVM does not currently model indirect buffers; direct and deleted
    // buffers both report no base buffer.
    let _ = target;
    Ok(Value::Nil)
}

/// (buffer-last-name &optional BUFFER) → string or nil
pub(crate) fn builtin_buffer_last_name(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-last-name", &args, 1)?;
    let target = if args.is_empty() || matches!(args[0], Value::Nil) {
        match eval.buffers.current_buffer() {
            Some(buf) => buf.id,
            None => return Ok(Value::Nil),
        }
    } else {
        expect_buffer_id(&args[0])?
    };

    if let Some(buf) = eval.buffers.get(target) {
        if buf.name == "*scratch*" {
            return Ok(Value::Nil);
        }
        return Ok(Value::string(&buf.name));
    }
    if let Some(name) = eval.buffers.dead_buffer_last_name(target) {
        return Ok(Value::string(name));
    }
    Ok(Value::Nil)
}

/// (buffer-string) → string
pub(crate) fn builtin_buffer_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("buffer-string", &args, 0)?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    Ok(Value::string(buf.buffer_string()))
}

/// (buffer-substring START END) → string
pub(crate) fn builtin_buffer_substring(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("buffer-substring", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    if start < point_min || start > point_max || end < point_min || end > point_max {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Buffer(buf.id), Value::Int(start), Value::Int(end)],
        ));
    }
    let start = start as usize;
    let end = end as usize;
    // Emacs uses 1-based positions, convert to 0-based byte positions
    let s = if start > 0 { start - 1 } else { 0 };
    let e = if end > 0 { end - 1 } else { 0 };
    // Convert char positions to byte positions
    let byte_start = buf.text.char_to_byte(s);
    let byte_end = buf.text.char_to_byte(e);
    Ok(Value::string(buf.buffer_substring(byte_start, byte_end)))
}

fn resolve_buffer_designator_allow_nil_current(
    eval: &mut super::eval::Evaluator,
    arg: &Value,
) -> Result<Option<BufferId>, Flow> {
    match arg {
        Value::Nil => eval
            .buffers
            .current_buffer()
            .map(|buf| Some(buf.id))
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")])),
        Value::Buffer(id) => Ok(eval.buffers.get(*id).map(|_| *id)),
        Value::Str(name) => eval
            .buffers
            .find_buffer_by_name(name)
            .map(Some)
            .ok_or_else(|| {
                signal(
                    "error",
                    vec![Value::string(format!("No buffer named {name}"))],
                )
            }),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn buffer_slice_for_char_region(
    eval: &super::eval::Evaluator,
    buffer_id: Option<BufferId>,
    start: i64,
    end: i64,
) -> String {
    let Some(buffer_id) = buffer_id else {
        return String::new();
    };
    let Some(buf) = eval.buffers.get(buffer_id) else {
        return String::new();
    };

    let (from, to) = if start <= end {
        (start, end)
    } else {
        (end, start)
    };
    let from_char = if from > 0 { from as usize - 1 } else { 0 };
    let to_char = if to > 0 { to as usize - 1 } else { 0 };
    let char_count = buf.text.char_count();
    let from_byte = buf.text.char_to_byte(from_char.min(char_count));
    let to_byte = buf.text.char_to_byte(to_char.min(char_count));
    buf.buffer_substring(from_byte, to_byte)
}

fn compare_buffer_substring_strings(left: &str, right: &str) -> i64 {
    let mut pos = 1i64;
    let mut left_iter = left.chars();
    let mut right_iter = right.chars();

    loop {
        match (left_iter.next(), right_iter.next()) {
            (Some(a), Some(b)) => {
                if a != b {
                    return if a < b { -pos } else { pos };
                }
                pos += 1;
            }
            (Some(_), None) => return pos,
            (None, Some(_)) => return -pos,
            (None, None) => return 0,
        }
    }
}

/// `(buffer-line-statistics &optional BUFFER-OR-NAME)` -> (LINES MAX-LEN AVG-LEN)
pub(crate) fn builtin_buffer_line_statistics(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-line-statistics", &args, 1)?;
    let buffer_id = if args.is_empty() {
        resolve_buffer_designator_allow_nil_current(eval, &Value::Nil)?
    } else {
        resolve_buffer_designator_allow_nil_current(eval, &args[0])?
    };

    let text = buffer_id
        .and_then(|id| eval.buffers.get(id).map(|buf| buf.buffer_string()))
        .unwrap_or_default();

    if text.is_empty() {
        return Ok(Value::list(vec![
            Value::Int(0),
            Value::Int(0),
            Value::Float(0.0),
        ]));
    }

    let mut line_count = 0usize;
    let mut max_len = 0usize;
    let mut total_len = 0usize;
    for line in text.lines() {
        line_count += 1;
        let width = line.chars().count();
        max_len = max_len.max(width);
        total_len += width;
    }

    if line_count == 0 {
        return Ok(Value::list(vec![
            Value::Int(0),
            Value::Int(0),
            Value::Float(0.0),
        ]));
    }

    Ok(Value::list(vec![
        Value::Int(line_count as i64),
        Value::Int(max_len as i64),
        Value::Float(total_len as f64 / line_count as f64),
    ]))
}

fn replace_buffer_contents(buf: &mut crate::buffer::Buffer, text: &str) {
    let len = buf.text.len();
    if len > 0 {
        buf.delete_region(0, len);
    }
    buf.widen();
    buf.goto_char(0);
    if !text.is_empty() {
        buf.insert(text);
        buf.goto_char(0);
    }
}

/// `(buffer-swap-text OTHER-BUFFER)` -> nil
pub(crate) fn builtin_buffer_swap_text(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("buffer-swap-text", &args, 1)?;
    let other_id = expect_buffer_id(&args[0])?;
    if eval.buffers.get(other_id).is_none() {
        return Ok(Value::Nil);
    }

    let current_id = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?
        .id;

    if current_id == other_id {
        return Ok(Value::Nil);
    }

    let current_text = eval
        .buffers
        .get(current_id)
        .map(|buf| buf.buffer_string())
        .unwrap_or_default();
    let other_text = eval
        .buffers
        .get(other_id)
        .map(|buf| buf.buffer_string())
        .unwrap_or_default();

    if let Some(buf) = eval.buffers.get_mut(current_id) {
        replace_buffer_contents(buf, &other_text);
    }
    if let Some(buf) = eval.buffers.get_mut(other_id) {
        replace_buffer_contents(buf, &current_text);
    }

    Ok(Value::Nil)
}

/// `(insert-and-inherit &rest ARGS)` -> nil
///
/// Text properties are not modelled separately yet, so this follows `insert`.
pub(crate) fn builtin_insert_and_inherit(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    builtin_insert(eval, args)
}

/// `(insert-before-markers-and-inherit &rest ARGS)` -> nil
///
/// Text property inheritance is currently equivalent to plain insertion.
pub(crate) fn builtin_insert_before_markers_and_inherit(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    super::editfns::builtin_insert_before_markers(eval, args)
}

/// `(insert-buffer-substring BUFFER &optional START END)` -> nil
pub(crate) fn builtin_insert_buffer_substring(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("insert-buffer-substring", &args, 1, 3)?;
    let buffer_id = resolve_buffer_designator_allow_nil_current(eval, &args[0])?;
    let default_end = buffer_id
        .and_then(|id| {
            eval.buffers
                .get(id)
                .map(|buf| buf.text.char_count() as i64 + 1)
        })
        .unwrap_or(1);
    let start = if args.len() > 1 && !args[1].is_nil() {
        expect_integer_or_marker(&args[1])?
    } else {
        1
    };
    let end = if args.len() > 2 && !args[2].is_nil() {
        expect_integer_or_marker(&args[2])?
    } else {
        default_end
    };

    let text = buffer_slice_for_char_region(eval, buffer_id, start, end);
    builtin_insert(eval, vec![Value::string(text)])
}

/// `(kill-all-local-variables &optional KILL-PERMANENT)` -> nil
pub(crate) fn builtin_kill_all_local_variables(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("kill-all-local-variables", &args, 0, 1)?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.properties.clear();
    buf.properties
        .insert("buffer-read-only".to_string(), Value::Nil);
    Ok(Value::Nil)
}

/// `(ntake N LIST)` -> LIST
pub(crate) fn builtin_ntake(args: Vec<Value>) -> EvalResult {
    expect_args("ntake", &args, 2)?;
    let n = expect_int(&args[0])?;
    if n <= 0 {
        return Ok(Value::Nil);
    }

    let head = args[1].clone();
    if matches!(head, Value::Nil) {
        return Ok(Value::Nil);
    }
    if !matches!(head, Value::Cons(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), head],
        ));
    }

    let mut cursor = head.clone();
    for _ in 1..n {
        match cursor {
            Value::Cons(cell) => {
                let next = cell.lock().expect("poisoned").cdr.clone();
                match next {
                    Value::Cons(_) => cursor = next,
                    Value::Nil => return Ok(head),
                    other => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), other],
                        ))
                    }
                }
            }
            Value::Nil => return Ok(head),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), other],
                ))
            }
        }
    }

    match cursor {
        Value::Cons(cell) => {
            cell.lock().expect("poisoned").cdr = Value::Nil;
            Ok(head)
        }
        Value::Nil => Ok(head),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), other],
        )),
    }
}

/// `(replace-buffer-contents SOURCE &optional MAX-SECS MAX-COSTS)` -> t
pub(crate) fn builtin_replace_buffer_contents_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("replace-buffer-contents", &args, 1, 3)?;
    let source_id = resolve_buffer_designator_allow_nil_current(eval, &args[0])?;
    let source_text = source_id
        .and_then(|id| eval.buffers.get(id).map(|buf| buf.buffer_string()))
        .unwrap_or_default();

    let read_only_buffer_name = eval.buffers.current_buffer().and_then(|buf| {
        if buffer_read_only_active(eval, buf) {
            Some(buf.name.clone())
        } else {
            None
        }
    });
    if let Some(name) = read_only_buffer_name {
        return Err(signal("buffer-read-only", vec![Value::string(name)]));
    }

    let current_id = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?
        .id;

    if let Some(buf) = eval.buffers.get_mut(current_id) {
        replace_buffer_contents(buf, &source_text);
    }

    Ok(Value::True)
}

/// `(set-buffer-multibyte FLAG)` -> FLAG
pub(crate) fn builtin_set_buffer_multibyte_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-buffer-multibyte", &args, 1)?;
    let flag = args[0].is_truthy();
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.multibyte = flag;
    Ok(args[0].clone())
}

/// `(split-window-internal WINDOW SIZE SIDE NORMALIZE)` -> window
pub(crate) fn builtin_split_window_internal(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("split-window-internal", &args, 4)?;
    if !args[0].is_nil() {
        let windowp = super::window_cmds::builtin_windowp(eval, vec![args[0].clone()])?;
        if windowp.is_nil() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("windowp"), args[0].clone()],
            ));
        }
    }
    if !args[1].is_nil() {
        let _ = expect_fixnum(&args[1])?;
    }
    if !args[2].is_nil() && !args[2].is_symbol() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[2].clone()],
        ));
    }

    // NORMALIZE is accepted for arity compatibility and ignored in this subset.
    super::window_cmds::builtin_split_window(
        eval,
        vec![args[0].clone(), args[1].clone(), args[2].clone()],
    )
}

/// `(buffer-text-pixel-size &optional BUFFER WINDOW FROM TO)` -> (WIDTH . HEIGHT)
pub(crate) fn builtin_buffer_text_pixel_size(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("buffer-text-pixel-size", &args, 0, 4)?;

    let buffer_id = if args.is_empty() {
        resolve_buffer_designator_allow_nil_current(eval, &Value::Nil)?
    } else {
        resolve_buffer_designator_allow_nil_current(eval, &args[0])?
    };

    if args.len() > 1 {
        let window = &args[1];
        if !window.is_nil() && !matches!(window, Value::Window(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), window.clone()],
            ));
        }
    }

    let from = if args.len() > 2 && !args[2].is_nil() {
        Some(expect_integer_or_marker(&args[2])?)
    } else {
        None
    };
    let to = if args.len() > 3 && !args[3].is_nil() {
        Some(expect_integer_or_marker(&args[3])?)
    } else {
        None
    };

    let text = if let Some(id) = buffer_id {
        if let Some(buf) = eval.buffers.get(id) {
            let default_from = 1i64;
            let default_to = buf.text.char_count() as i64 + 1;
            let from_pos = from.unwrap_or(default_from);
            let to_pos = to.unwrap_or(default_to);
            buffer_slice_for_char_region(eval, Some(id), from_pos, to_pos)
        } else {
            String::new()
        }
    } else {
        String::new()
    };

    if text.is_empty() {
        return Ok(Value::cons(Value::Int(0), Value::Int(0)));
    }

    let mut height = 0usize;
    let mut width = 0usize;
    for line in text.lines() {
        height += 1;
        width = width.max(line.chars().count());
    }

    if height == 0 {
        return Ok(Value::cons(Value::Int(0), Value::Int(0)));
    }
    Ok(Value::cons(
        Value::Int(width as i64),
        Value::Int(height as i64),
    ))
}

/// `(compare-buffer-substrings BUF1 START1 END1 BUF2 START2 END2)` -> integer
pub(crate) fn builtin_compare_buffer_substrings(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("compare-buffer-substrings", &args, 6)?;

    let left_buffer = resolve_buffer_designator_allow_nil_current(eval, &args[0])?;
    let left_start = expect_integer_or_marker(&args[1])?;
    let left_end = expect_integer_or_marker(&args[2])?;
    let right_buffer = resolve_buffer_designator_allow_nil_current(eval, &args[3])?;
    let right_start = expect_integer_or_marker(&args[4])?;
    let right_end = expect_integer_or_marker(&args[5])?;

    let left = buffer_slice_for_char_region(eval, left_buffer, left_start, left_end);
    let right = buffer_slice_for_char_region(eval, right_buffer, right_start, right_end);
    Ok(Value::Int(compare_buffer_substring_strings(&left, &right)))
}

/// `(compute-motion FROM FROMPOS TO TOPOS WIDTH OFFSETS WINDOW)` -> motion tuple
pub(crate) fn builtin_compute_motion(args: Vec<Value>) -> EvalResult {
    expect_args("compute-motion", &args, 7)?;

    let from = expect_integer_or_marker(&args[0])?;
    if !matches!(&args[1], Value::Cons(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), args[1].clone()],
        ));
    }
    let to = expect_integer_or_marker(&args[2])?;
    if !args[3].is_nil() && !matches!(&args[3], Value::Cons(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), args[3].clone()],
        ));
    }
    if !args[4].is_nil() {
        let _ = expect_fixnum(&args[4])?;
    }
    if !args[5].is_nil() && !matches!(&args[5], Value::Cons(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), args[5].clone()],
        ));
    }
    if !args[6].is_nil() && !matches!(&args[6], Value::Window(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("window-live-p"), args[6].clone()],
        ));
    }

    let result = if args[3].is_nil() {
        vec![
            Value::Int(to),
            Value::Int(1),
            Value::Int(0),
            Value::Int(1),
            Value::Nil,
        ]
    } else {
        vec![
            Value::Int(from),
            Value::Int(0),
            Value::Int(0),
            Value::Int(0),
            Value::Nil,
        ]
    };
    Ok(Value::list(result))
}

/// `(coordinates-in-window-p COORDINATES WINDOW)` -> COORDINATES or nil.
///
/// Batch compatibility: returns the coordinate pair when it's inside WINDOW's
/// current character bounds, otherwise nil.
pub(crate) fn builtin_coordinates_in_window_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("coordinates-in-window-p", &args, 2)?;

    let (x, y) = match &args[0] {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            let x = match &pair.car {
                Value::Int(n) => *n as f64,
                Value::Float(f) => *f,
                other => {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("numberp"), other.clone()],
                    ))
                }
            };
            let y = match &pair.cdr {
                Value::Int(n) => *n as f64,
                Value::Float(f) => *f,
                other => {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("numberp"), other.clone()],
                    ))
                }
            };
            (x, y)
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("consp"), other.clone()],
            ))
        }
    };

    expect_optional_live_window_designator(&args[1], eval)?;
    let window_arg = args[1].clone();
    let width =
        match super::window_cmds::builtin_window_total_width(eval, vec![window_arg.clone()])? {
            Value::Int(n) => n as f64,
            _ => 0.0,
        };
    let height = match super::window_cmds::builtin_window_total_height(eval, vec![window_arg])? {
        Value::Int(n) => n as f64,
        _ => 0.0,
    };

    if x >= 0.0 && y >= 0.0 && x < width && y < height {
        Ok(args[0].clone())
    } else {
        Ok(Value::Nil)
    }
}

/// `(constrain-to-field NEW-POS OLD-POS &optional ESCAPE-FROM-EDGE ONLY-IN-LINE INHIBIT-CAPTURE-PROPERTY)`
pub(crate) fn builtin_constrain_to_field(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("constrain-to-field", &args, 2, 5)?;
    let new_pos = if args[0].is_nil() {
        let current = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        current.point_char() as i64 + 1
    } else {
        expect_integer_or_marker(&args[0])?
    };
    let _ = expect_integer_or_marker(&args[1])?;
    Ok(Value::Int(new_pos))
}

fn resolve_field_position(
    eval: &super::eval::Evaluator,
    position_value: Option<&Value>,
) -> Result<(i64, i64, i64), Flow> {
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    let pos = match position_value {
        None | Some(Value::Nil) => buf.text.byte_to_char(buf.pt) as i64 + 1,
        Some(value) => expect_integer_or_marker(value)?,
    };
    if pos < point_min || pos > point_max {
        return Err(signal("args-out-of-range", vec![Value::Int(pos)]));
    }
    Ok((pos, point_min, point_max))
}

/// `(field-beginning &optional POS ESCAPE-FROM-EDGE LIMIT)` -> position
pub(crate) fn builtin_field_beginning(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("field-beginning", &args, 3)?;
    let (_pos, point_min, _point_max) = resolve_field_position(eval, args.first())?;
    if let Some(limit_value) = args.get(2) {
        if !limit_value.is_nil() {
            let limit = expect_integer_or_marker(limit_value)?;
            if limit <= 0 {
                return Err(signal("args-out-of-range", vec![Value::Int(limit)]));
            }
            return Ok(Value::Int(point_min.max(limit)));
        }
    }
    Ok(Value::Int(point_min))
}

/// `(field-end &optional POS ESCAPE-FROM-EDGE LIMIT)` -> position
pub(crate) fn builtin_field_end(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("field-end", &args, 3)?;
    let (_pos, _point_min, point_max) = resolve_field_position(eval, args.first())?;
    if let Some(limit_value) = args.get(2) {
        if !limit_value.is_nil() {
            let limit = expect_integer_or_marker(limit_value)?;
            return Ok(Value::Int(point_max.min(limit)));
        }
    }
    Ok(Value::Int(point_max))
}

/// `(field-string &optional POS)` -> field text at POS.
pub(crate) fn builtin_field_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("field-string", &args, 1)?;
    let (_pos, point_min, point_max) = resolve_field_position(eval, args.first())?;
    builtin_buffer_substring(eval, vec![Value::Int(point_min), Value::Int(point_max)])
}

/// `(field-string-no-properties &optional POS)` -> field text at POS.
pub(crate) fn builtin_field_string_no_properties(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("field-string-no-properties", &args, 1)?;
    let (_pos, point_min, point_max) = resolve_field_position(eval, args.first())?;
    super::editfns::builtin_buffer_substring_no_properties(
        eval,
        vec![Value::Int(point_min), Value::Int(point_max)],
    )
}

/// `(delete-field &optional POS)` -> nil
pub(crate) fn builtin_delete_field(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("delete-field", &args, 1)?;
    let (_pos, point_min, point_max) = resolve_field_position(eval, args.first())?;
    builtin_delete_region(eval, vec![Value::Int(point_min), Value::Int(point_max)])
}

/// `(clear-string STRING)` -> nil
pub(crate) fn builtin_clear_string(args: Vec<Value>) -> EvalResult {
    expect_args("clear-string", &args, 1)?;
    let _ = expect_string(&args[0])?;
    Ok(Value::Nil)
}

/// `(command-error-default-function DATA CONTEXT CALLER)` -> nil
pub(crate) fn builtin_command_error_default_function(args: Vec<Value>) -> EvalResult {
    expect_args("command-error-default-function", &args, 3)?;
    Ok(Value::Nil)
}

/// (point) → integer
pub(crate) fn builtin_point(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("point", &args, 0)?;
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    // Return 1-based char position
    Ok(Value::Int(buf.point_char() as i64 + 1))
}

/// (point-min) → integer
pub(crate) fn builtin_point_min(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("point-min", &args, 0)?;
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    Ok(Value::Int(
        buf.text.byte_to_char(buf.point_min()) as i64 + 1,
    ))
}

/// (point-max) → integer
pub(crate) fn builtin_point_max(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("point-max", &args, 0)?;
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    Ok(Value::Int(
        buf.text.byte_to_char(buf.point_max()) as i64 + 1,
    ))
}

/// (goto-char POS) → POS
pub(crate) fn builtin_goto_char(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("goto-char", &args, 1)?;
    let pos = expect_int(&args[0])?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    // Convert 1-based char pos to 0-based byte pos
    let char_pos = if pos > 0 { pos as usize - 1 } else { 0 };
    let byte_pos = buf.text.char_to_byte(char_pos.min(buf.text.char_count()));
    buf.goto_char(byte_pos);
    Ok(args[0].clone())
}

/// (insert &rest ARGS) → nil
pub(crate) fn builtin_insert(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    let read_only_buffer_name = eval.buffers.current_buffer().and_then(|buf| {
        if buffer_read_only_active(eval, buf) {
            Some(buf.name.clone())
        } else {
            None
        }
    });
    if let Some(name) = read_only_buffer_name {
        return Err(signal("buffer-read-only", vec![Value::string(name)]));
    }

    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    for arg in &args {
        match arg {
            Value::Str(s) => buf.insert(s),
            Value::Char(c) => {
                let mut tmp = [0u8; 4];
                buf.insert(c.encode_utf8(&mut tmp));
            }
            Value::Int(n) => {
                if let Some(c) = char::from_u32(*n as u32) {
                    let mut tmp = [0u8; 4];
                    buf.insert(c.encode_utf8(&mut tmp));
                }
            }
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), other.clone()],
                ))
            }
        }
    }
    Ok(Value::Nil)
}

fn insert_char_code_from_value(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Char(c) => Ok(*c as i64),
        Value::Int(n) if *n < 0 || *n > KEY_CHAR_CODE_MASK => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), value.clone()],
        )),
        Value::Int(n) => Ok(*n),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), other.clone()],
        )),
    }
}

/// `(insert-char CHARACTER &optional COUNT INHERIT)` -> nil
pub(crate) fn builtin_insert_char(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("insert-char", &args, 1, 3)?;
    let char_code = insert_char_code_from_value(&args[0])?;
    let Some(ch) = char::from_u32(char_code as u32) else {
        return Ok(Value::Nil);
    };
    let count = if args.len() > 1 {
        expect_fixnum(&args[1])?
    } else {
        1
    };

    if count <= 0 {
        return Ok(Value::Nil);
    }

    let read_only_buffer_name = eval.buffers.current_buffer().and_then(|buf| {
        if buffer_read_only_active(eval, buf) {
            Some(buf.name.clone())
        } else {
            None
        }
    });
    if let Some(name) = read_only_buffer_name {
        return Err(signal("buffer-read-only", vec![Value::string(name)]));
    }

    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let to_insert = ch.to_string().repeat(count as usize);
    buf.insert(&to_insert);
    Ok(Value::Nil)
}

/// `(insert-byte BYTE COUNT &optional INHERIT)` -> nil
pub(crate) fn builtin_insert_byte(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("insert-byte", &args, 2, 3)?;
    let byte = expect_fixnum(&args[0])?;
    if !(0..=255).contains(&byte) {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(byte), Value::Int(0), Value::Int(255)],
        ));
    }
    let count = expect_fixnum(&args[1])?;
    if count <= 0 {
        return Ok(Value::Nil);
    }

    let read_only_buffer_name = eval.buffers.current_buffer().and_then(|buf| {
        if buffer_read_only_active(eval, buf) {
            Some(buf.name.clone())
        } else {
            None
        }
    });
    if let Some(name) = read_only_buffer_name {
        return Err(signal("buffer-read-only", vec![Value::string(name)]));
    }

    let ch = char::from_u32(byte as u32).expect("byte range maps to a valid codepoint");
    let to_insert = ch.to_string().repeat(count as usize);
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.insert(&to_insert);
    Ok(Value::Nil)
}

/// (delete-region START END) → nil
pub(crate) fn builtin_delete_region(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-region", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let read_only_buffer_name = eval.buffers.current_buffer().and_then(|buf| {
        if buffer_read_only_active(eval, buf) {
            Some(buf.name.clone())
        } else {
            None
        }
    });
    if let Some(name) = read_only_buffer_name {
        return Err(signal("buffer-read-only", vec![Value::string(name)]));
    }

    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    if start < point_min || start > point_max || end < point_min || end > point_max {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Buffer(buf.id), Value::Int(start), Value::Int(end)],
        ));
    }
    let start = start as usize;
    let end = end as usize;
    // Convert 1-based to 0-based char positions, then to byte positions
    let s = if start > 0 { start - 1 } else { 0 };
    let e = if end > 0 { end - 1 } else { 0 };
    let byte_start = buf.text.char_to_byte(s);
    let byte_end = buf.text.char_to_byte(e);
    buf.delete_region(byte_start, byte_end);
    Ok(Value::Nil)
}

/// `(delete-and-extract-region START END)` -> deleted text
pub(crate) fn builtin_delete_and_extract_region(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-and-extract-region", &args, 2)?;
    let start = expect_integer_or_marker(&args[0])?;
    let end = expect_integer_or_marker(&args[1])?;

    let (point_min, point_max, current_buffer) = {
        let buf = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        (
            buf.text.byte_to_char(buf.point_min()) as i64 + 1,
            buf.text.byte_to_char(buf.point_max()) as i64 + 1,
            Value::Buffer(buf.id),
        )
    };

    if start < point_min || start > point_max || end < point_min || end > point_max {
        return Err(signal(
            "args-out-of-range",
            vec![current_buffer, Value::Int(start), Value::Int(end)],
        ));
    }

    let lo = start.min(end);
    let hi = start.max(end);
    let deleted = builtin_buffer_substring(eval, vec![Value::Int(lo), Value::Int(hi)])?;
    let _ = builtin_delete_region(eval, vec![Value::Int(lo), Value::Int(hi)])?;
    Ok(deleted)
}

/// (erase-buffer) → nil
pub(crate) fn builtin_erase_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("erase-buffer", &args, 0)?;
    let read_only_buffer_name = eval.buffers.current_buffer().and_then(|buf| {
        if buffer_read_only_active(eval, buf) {
            Some(buf.name.clone())
        } else {
            None
        }
    });
    if let Some(name) = read_only_buffer_name {
        return Err(signal("buffer-read-only", vec![Value::string(name)]));
    }

    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let len = buf.text.len();
    buf.delete_region(0, len);
    buf.widen();
    Ok(Value::Nil)
}

/// (buffer-enable-undo &optional BUFFER) -> nil
pub(crate) fn builtin_buffer_enable_undo(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 1 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("buffer-enable-undo"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    let id = if args.is_empty() || matches!(args[0], Value::Nil) {
        eval.buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?
            .id
    } else {
        match &args[0] {
            Value::Buffer(id) => {
                if eval.buffers.get(*id).is_none() {
                    return Ok(Value::Nil);
                }
                *id
            }
            Value::Str(name) => eval.buffers.find_buffer_by_name(name).ok_or_else(|| {
                signal(
                    "error",
                    vec![Value::string(format!("No buffer named {name}"))],
                )
            })?,
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), other.clone()],
                ))
            }
        }
    };
    let buf = eval
        .buffers
        .get_mut(id)
        .ok_or_else(|| signal("error", vec![Value::string("Selecting deleted buffer")]))?;
    buf.undo_list.set_enabled(true);
    buf.set_buffer_local("buffer-undo-list", Value::Nil);
    Ok(Value::Nil)
}

/// (buffer-disable-undo &optional BUFFER) -> t
pub(crate) fn builtin_buffer_disable_undo(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 1 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("buffer-disable-undo"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    let id = if args.is_empty() || matches!(args[0], Value::Nil) {
        eval.buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?
            .id
    } else {
        match &args[0] {
            Value::Buffer(id) => {
                if eval.buffers.get(*id).is_none() {
                    return Err(signal(
                        "error",
                        vec![Value::string("Selecting deleted buffer")],
                    ));
                }
                *id
            }
            Value::Str(name) => match eval.buffers.find_buffer_by_name(name) {
                Some(id) => id,
                None => {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("stringp"), Value::Nil],
                    ))
                }
            },
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), other.clone()],
                ))
            }
        }
    };
    let buf = eval
        .buffers
        .get_mut(id)
        .ok_or_else(|| signal("error", vec![Value::string("Selecting deleted buffer")]))?;
    buf.undo_list.set_enabled(false);
    buf.set_buffer_local("buffer-undo-list", Value::True);
    Ok(Value::True)
}

/// (buffer-size &optional BUFFER) → integer
pub(crate) fn builtin_buffer_size(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-size", &args, 1)?;
    if args.is_empty() || matches!(args[0], Value::Nil) {
        let buf = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        return Ok(Value::Int(buf.text.char_count() as i64));
    }

    let id = expect_buffer_id(&args[0])?;
    if let Some(buf) = eval.buffers.get(id) {
        Ok(Value::Int(buf.text.char_count() as i64))
    } else {
        Ok(Value::Int(0))
    }
}

/// (narrow-to-region START END) → nil
pub(crate) fn builtin_narrow_to_region(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("narrow-to-region", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    if start < point_min || start > point_max || end < point_min || end > point_max {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(start), Value::Int(end)],
        ));
    }
    let start = start as usize;
    let end = end as usize;
    let s = if start > 0 { start - 1 } else { 0 };
    let e = if end > 0 { end - 1 } else { 0 };
    let byte_start = buf.text.char_to_byte(s);
    let byte_end = buf.text.char_to_byte(e);
    buf.narrow_to_region(byte_start, byte_end);
    Ok(Value::Nil)
}

/// (widen) → nil
pub(crate) fn builtin_widen(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("widen", &args, 0)?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.widen();
    Ok(Value::Nil)
}

/// (buffer-modified-p &optional BUFFER) → t or nil
pub(crate) fn builtin_buffer_modified_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-modified-p", &args, 1)?;
    if args.is_empty() || matches!(args[0], Value::Nil) {
        let buf = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        return Ok(Value::bool(buf.is_modified()));
    }

    let id = expect_buffer_id(&args[0])?;
    if let Some(buf) = eval.buffers.get(id) {
        Ok(Value::bool(buf.is_modified()))
    } else {
        Ok(Value::Nil)
    }
}

/// (set-buffer-modified-p FLAG) → FLAG
pub(crate) fn builtin_set_buffer_modified_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-buffer-modified-p", &args, 1)?;
    let flag = args[0].is_truthy();
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.set_modified(flag);
    Ok(args[0].clone())
}

fn optional_buffer_tick_target(
    eval: &super::eval::Evaluator,
    name: &str,
    args: &[Value],
) -> Result<Option<BufferId>, Flow> {
    expect_max_args(name, args, 1)?;
    if args.is_empty() || matches!(args[0], Value::Nil) {
        Ok(eval.buffers.current_buffer().map(|buf| buf.id))
    } else {
        Ok(Some(expect_buffer_id(&args[0])?))
    }
}

/// (buffer-modified-tick &optional BUFFER) → integer
pub(crate) fn builtin_buffer_modified_tick(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let target = optional_buffer_tick_target(eval, "buffer-modified-tick", &args)?;
    if let Some(id) = target {
        if let Some(buf) = eval.buffers.get(id) {
            return Ok(Value::Int(buf.modified_tick));
        }
    }
    Ok(Value::Int(1))
}

/// (buffer-chars-modified-tick &optional BUFFER) → integer
pub(crate) fn builtin_buffer_chars_modified_tick(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let target = optional_buffer_tick_target(eval, "buffer-chars-modified-tick", &args)?;
    if let Some(id) = target {
        if let Some(buf) = eval.buffers.get(id) {
            return Ok(Value::Int(buf.chars_modified_tick));
        }
    }
    Ok(Value::Int(1))
}

/// (buffer-list) → list of buffers
pub(crate) fn builtin_buffer_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-list", &args, 1)?;
    let ids = eval.buffers.buffer_list();
    let vals: Vec<Value> = ids.into_iter().map(Value::Buffer).collect();
    Ok(Value::list(vals))
}

/// (other-buffer &optional BUFFER VISIBLE-OK FRAME) → buffer
///
/// Batch-friendly behavior:
/// - prefers `*Messages*` when available and distinct from BUFFER
/// - otherwise returns a live buffer distinct from BUFFER when possible
/// - falls back to BUFFER/current buffer when no alternative exists
pub(crate) fn builtin_other_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("other-buffer", &args, 3)?;

    let current_id = eval.buffers.current_buffer().map(|buf| buf.id);
    let avoid_id = match args.first() {
        None | Some(Value::Nil) => current_id,
        Some(Value::Buffer(id)) => Some(*id),
        Some(Value::Str(name)) => eval.buffers.find_buffer_by_name(name),
        // GNU Emacs is permissive for non-buffer designators here; treat as
        // unspecified and still return a live buffer.
        Some(_) => current_id,
    };

    if let Some(messages_id) = eval.buffers.find_buffer_by_name("*Messages*") {
        if Some(messages_id) != avoid_id {
            return Ok(Value::Buffer(messages_id));
        }
    }

    if let Some(id) = eval
        .buffers
        .buffer_list()
        .into_iter()
        .find(|id| Some(*id) != avoid_id)
    {
        return Ok(Value::Buffer(id));
    }

    if let Some(id) = avoid_id.or(current_id) {
        return Ok(Value::Buffer(id));
    }

    Ok(Value::Nil)
}

/// (generate-new-buffer-name BASE) → string
pub(crate) fn builtin_generate_new_buffer_name(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("generate-new-buffer-name", &args, 1)?;
    expect_max_args("generate-new-buffer-name", &args, 2)?;
    if args.len() == 2
        && !matches!(
            &args[1],
            Value::Nil | Value::True | Value::Str(_) | Value::Symbol(_) | Value::Keyword(_)
        )
    {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[1].clone()],
        ));
    }
    let base = expect_string(&args[0])?;
    Ok(Value::string(eval.buffers.generate_new_buffer_name(&base)))
}

/// (generate-new-buffer NAME) → buffer
pub(crate) fn builtin_generate_new_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("generate-new-buffer", &args, 1)?;
    expect_max_args("generate-new-buffer", &args, 2)?;
    let base = expect_string(&args[0])?;
    let name = eval.buffers.generate_new_buffer_name(&base);
    let id = eval.buffers.create_buffer(&name);
    Ok(Value::Buffer(id))
}

/// (bufferp OBJECT) → t or nil
pub(crate) fn builtin_bufferp(args: Vec<Value>) -> EvalResult {
    expect_args("bufferp", &args, 1)?;
    Ok(Value::bool(matches!(args[0], Value::Buffer(_))))
}

/// (char-after &optional POS) → integer or nil
pub(crate) fn builtin_char_after(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let byte_pos = if args.is_empty() || matches!(args[0], Value::Nil) {
        buf.point()
    } else {
        let pos = expect_int(&args[0])?;
        if pos <= 0 {
            return Ok(Value::Nil);
        }
        let pos = pos as usize;
        let char_pos = if pos > 0 { pos - 1 } else { 0 };
        buf.text.char_to_byte(char_pos.min(buf.text.char_count()))
    };
    match buf.char_after(byte_pos) {
        Some(c) => Ok(Value::Int(c as i64)),
        None => Ok(Value::Nil),
    }
}

/// (char-before &optional POS) → integer or nil
pub(crate) fn builtin_char_before(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let byte_pos = if args.is_empty() || matches!(args[0], Value::Nil) {
        buf.point()
    } else {
        let pos = expect_int(&args[0])?;
        if pos <= 0 {
            return Ok(Value::Nil);
        }
        let pos = pos as usize;
        let char_pos = if pos > 0 { pos - 1 } else { 0 };
        buf.text.char_to_byte(char_pos.min(buf.text.char_count()))
    };
    match buf.char_before(byte_pos) {
        Some(c) => Ok(Value::Int(c as i64)),
        None => Ok(Value::Nil),
    }
}

fn is_unibyte_storage_string(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|ch| (0xE300..=0xE3FF).contains(&(ch as u32)))
}

fn get_byte_from_multibyte_char_code(code: u32) -> EvalResult {
    if code <= 0x7F {
        return Ok(Value::Int(code as i64));
    }
    if (0x3FFF80..=0x3FFFFF).contains(&code) {
        return Ok(Value::Int((code - 0x3FFF00) as i64));
    }
    Err(signal(
        "error",
        vec![Value::string(format!(
            "Not an ASCII nor an 8-bit character: {code}"
        ))],
    ))
}

/// `(byte-to-position BYTEPOS)` -- map a 1-based byte position to 1-based char position.
pub(crate) fn builtin_byte_to_position(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("byte-to-position", &args, 1)?;
    let byte_pos = expect_fixnum(&args[0])?;
    if byte_pos <= 0 {
        return Ok(Value::Nil);
    }

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let byte_len = buf.text.len();
    let byte_pos0 = (byte_pos - 1) as usize;
    if byte_pos0 > byte_len {
        return Ok(Value::Nil);
    }

    // Emacs maps interior UTF-8 continuation bytes to the containing character.
    let mut boundary = byte_pos0;
    while boundary > 0 && boundary < byte_len {
        let b = buf.text.byte_at(boundary);
        if (b & 0b1100_0000) != 0b1000_0000 {
            break;
        }
        boundary -= 1;
    }

    Ok(Value::Int(buf.text.byte_to_char(boundary) as i64 + 1))
}

/// `(position-bytes POSITION)` -- map a 1-based char position to a 1-based byte position.
pub(crate) fn builtin_position_bytes(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("position-bytes", &args, 1)?;
    let pos = expect_integer_or_marker(&args[0])?;

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let max_char_pos = buf.text.char_count() as i64 + 1;
    if pos <= 0 || pos > max_char_pos {
        return Ok(Value::Nil);
    }

    let byte_pos = buf.text.char_to_byte((pos - 1) as usize);
    Ok(Value::Int(byte_pos as i64 + 1))
}

/// `(get-byte &optional POSITION STRING)` -- return a byte value at point or in STRING.
pub(crate) fn builtin_get_byte(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("get-byte", &args, 2)?;

    // STRING path: POSITION is a zero-based character index.
    if args.get(1).is_some_and(|v| !v.is_nil()) {
        let string_value = args[1].clone();
        let s = expect_string(&args[1])?;
        let pos = if args.is_empty() || args[0].is_nil() {
            0usize
        } else {
            expect_wholenump(&args[0])? as usize
        };

        let char_len = storage_char_len(&s);
        if pos >= char_len && !s.is_empty() {
            return Err(signal(
                "args-out-of-range",
                vec![string_value, Value::Int(pos as i64)],
            ));
        }

        // Emacs returns 0 for the terminating NUL when indexing an empty string.
        if char_len == 0 {
            return Ok(Value::Int(0));
        }

        let code = decode_storage_char_codes(&s)[pos];
        if is_unibyte_storage_string(&s) {
            return Ok(Value::Int((code & 0xFF) as i64));
        }
        return get_byte_from_multibyte_char_code(code);
    }

    // Buffer path: POSITION is a 1-based character position.
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let byte_pos = if args.is_empty() || args[0].is_nil() {
        buf.point()
    } else {
        let pos = expect_integer_or_marker(&args[0])?;
        let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
        let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
        if pos < point_min || pos >= point_max {
            return Err(signal(
                "args-out-of-range",
                vec![
                    args[0].clone(),
                    Value::Int(point_min),
                    Value::Int(point_max),
                ],
            ));
        }
        buf.text.char_to_byte((pos - 1) as usize)
    };

    if byte_pos >= buf.text.len() {
        return Ok(Value::Int(0));
    }

    if !buf.multibyte {
        return Ok(Value::Int(buf.text.byte_at(byte_pos) as i64));
    }

    let code = match buf.char_after(byte_pos) {
        Some(ch) => ch as u32,
        None => return Ok(Value::Int(0)),
    };

    if (0xE080..=0xE0FF).contains(&code) {
        return Ok(Value::Int((code - 0xE000) as i64));
    }
    if (0xE300..=0xE3FF).contains(&code) {
        return Ok(Value::Int((code - 0xE300) as i64));
    }

    get_byte_from_multibyte_char_code(code)
}

/// (buffer-local-value VARIABLE BUFFER) → value
pub(crate) fn builtin_buffer_local_value(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("buffer-local-value", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let id = expect_buffer_id(&args[1])?;
    let buf = eval
        .buffers
        .get(id)
        .ok_or_else(|| signal("error", vec![Value::string("No such buffer")]))?;
    match buf.get_buffer_local(name) {
        Some(v) => Ok(v.clone()),
        None => eval
            .obarray()
            .symbol_value(name)
            .cloned()
            .ok_or_else(|| signal("void-variable", vec![Value::symbol(name)])),
    }
}

/// (with-current-buffer BUFFER-OR-NAME &rest BODY) is a special form handled
/// in eval.rs, but we provide the utility of switching and restoring here.
// Search / regex builtins are defined at the end of this file.

// ===========================================================================
// Keymap builtins
// ===========================================================================
use super::keymap::{
    decode_keymap_handle, encode_keymap_handle, KeyBinding, KeyEvent, KeymapManager,
};

/// Extract a keymap id from a Value, signaling wrong-type-argument if invalid.
fn expect_keymap_id(eval: &super::eval::Evaluator, value: &Value) -> Result<u64, Flow> {
    match value {
        Value::Int(n) => {
            let Some(id) = decode_keymap_handle(*n) else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("keymapp"), value.clone()],
                ));
            };
            if eval.keymaps.is_keymap(id) {
                Ok(id)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("keymapp"), value.clone()],
                ))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("keymapp"), other.clone()],
        )),
    }
}

/// Convert a KeyBinding to a Value for returning to Lisp.
fn key_binding_to_value(binding: &KeyBinding) -> Value {
    match binding {
        KeyBinding::Command(name) => Value::symbol(name.clone()),
        KeyBinding::Prefix(id) => Value::Int(encode_keymap_handle(*id)),
        KeyBinding::LispValue(v) => v.clone(),
    }
}

/// Convert a Value to a KeyBinding.
fn value_to_key_binding(eval: &super::eval::Evaluator, value: &Value) -> KeyBinding {
    match value {
        Value::Symbol(name) => KeyBinding::Command(name.clone()),
        Value::Nil => KeyBinding::Command("nil".to_string()),
        Value::Int(n) => {
            if let Some(id) = decode_keymap_handle(*n) {
                if eval.keymaps.is_keymap(id) {
                    return KeyBinding::Prefix(id);
                }
            }
            KeyBinding::LispValue(value.clone())
        }
        other => KeyBinding::LispValue(other.clone()),
    }
}

fn key_event_to_value(event: &KeyEvent) -> Value {
    match event {
        KeyEvent::Char {
            code,
            ctrl,
            meta,
            shift,
            super_,
        } if !ctrl && !meta && !shift && !super_ => Value::Int(*code as i64),
        _ => Value::symbol(KeymapManager::format_key_event(event)),
    }
}

fn key_sequence_to_value(seq: &[KeyEvent]) -> Value {
    Value::vector(seq.iter().map(key_event_to_value).collect())
}

fn collect_accessible_keymap_paths(
    eval: &super::eval::Evaluator,
    map_id: u64,
    prefix: &mut Vec<KeyEvent>,
    out: &mut Vec<Value>,
    seen: &mut HashSet<u64>,
    filter: Option<&[KeyEvent]>,
) {
    let include_current = match filter {
        None => true,
        Some(filter_seq) => filter_seq.is_empty() || prefix.starts_with(filter_seq),
    };
    let should_descend = match filter {
        None => true,
        Some(filter_seq) => {
            filter_seq.is_empty()
                || filter_seq.starts_with(prefix.as_slice())
                || prefix.starts_with(filter_seq)
        }
    };
    if !should_descend {
        return;
    }

    if include_current {
        out.push(Value::cons(
            key_sequence_to_value(prefix),
            Value::Int(encode_keymap_handle(map_id)),
        ));
    }

    if !seen.insert(map_id) {
        return;
    }

    let Some(map) = eval.keymaps.get(map_id) else {
        return;
    };
    let mut prefixes: Vec<(String, KeyEvent, u64)> = map
        .bindings
        .iter()
        .filter_map(|(event, binding)| match binding {
            KeyBinding::Prefix(child_id) => Some((
                KeymapManager::format_key_event(event),
                event.clone(),
                *child_id,
            )),
            _ => None,
        })
        .collect();
    prefixes.sort_by(|a, b| a.0.cmp(&b.0));

    for (_, event, child_id) in prefixes {
        prefix.push(event);
        collect_accessible_keymap_paths(eval, child_id, prefix, out, seen, filter);
        prefix.pop();
    }
}

/// `(accessible-keymaps KEYMAP &optional PREFIXES)` -> list of accessible keymaps.
fn builtin_accessible_keymaps(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("accessible-keymaps", &args, 1)?;
    expect_max_args("accessible-keymaps", &args, 2)?;
    let map_id = expect_keymap_id(eval, &args[0])?;

    let filter = match args.get(1) {
        None | Some(Value::Nil) => None,
        Some(value) if value.is_vector() => {
            let is_empty = match value {
                Value::Vector(vec) => vec.lock().expect("poisoned").is_empty(),
                _ => false,
            };
            if is_empty {
                Some(Vec::new())
            } else {
                Some(expect_key_description(value)?)
            }
        }
        Some(value @ Value::Str(s)) => {
            if s.is_empty() {
                Some(Vec::new())
            } else {
                Some(expect_key_description(value)?)
            }
        }
        Some(value) if value.is_list() => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("arrayp"), value.clone()],
            ))
        }
        Some(value) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("sequencep"), value.clone()],
            ))
        }
    };

    let mut out = Vec::new();
    let mut prefix = Vec::new();
    let mut seen = HashSet::new();
    collect_accessible_keymap_paths(
        eval,
        map_id,
        &mut prefix,
        &mut out,
        &mut seen,
        filter.as_deref(),
    );
    Ok(Value::list(out))
}

/// Parse a key description from a Value (must be a string).
fn expect_key_description(value: &Value) -> Result<Vec<KeyEvent>, Flow> {
    match super::kbd::key_events_from_designator(value) {
        Ok(events) => Ok(events),
        Err(super::kbd::KeyDesignatorError::WrongType(other)) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), other],
        )),
        Err(super::kbd::KeyDesignatorError::Parse(msg)) => {
            Err(signal("error", vec![Value::string(msg)]))
        }
    }
}

/// Helper: define a key in a keymap, auto-creating prefix maps for multi-key sequences.
fn define_key_in_map(
    eval: &mut super::eval::Evaluator,
    map_id: u64,
    keys: Vec<KeyEvent>,
    binding: KeyBinding,
) {
    if keys.len() == 1 {
        eval.keymaps
            .define_key(map_id, keys.into_iter().next().unwrap(), binding);
    } else {
        let mut current_map = map_id;
        for (i, key) in keys.iter().enumerate() {
            if i == keys.len() - 1 {
                eval.keymaps
                    .define_key(current_map, key.clone(), binding.clone());
            } else {
                match eval.keymaps.lookup_key(current_map, key).cloned() {
                    Some(KeyBinding::Prefix(next_map)) => {
                        current_map = next_map;
                    }
                    _ => {
                        let prefix_map = eval.keymaps.make_sparse_keymap(None);
                        eval.keymaps.define_key(
                            current_map,
                            key.clone(),
                            KeyBinding::Prefix(prefix_map),
                        );
                        current_map = prefix_map;
                    }
                }
            }
        }
    }
}

/// (make-keymap) -> keymap-id
fn builtin_make_keymap(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("make-keymap", &args, 1)?;
    let id = eval.keymaps.make_keymap();
    Ok(Value::Int(encode_keymap_handle(id)))
}

/// (make-sparse-keymap &optional NAME) -> keymap-id
fn builtin_make_sparse_keymap(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("make-sparse-keymap", &args, 1)?;
    let name = if !args.is_empty() {
        match &args[0] {
            Value::Str(s) => Some((**s).clone()),
            Value::Nil => None,
            _ => None,
        }
    } else {
        None
    };
    let id = eval.keymaps.make_sparse_keymap(name);
    Ok(Value::Int(encode_keymap_handle(id)))
}

fn clone_keymap_tree(
    eval: &mut super::eval::Evaluator,
    source_id: u64,
    seen: &mut std::collections::HashMap<u64, u64>,
) -> Result<u64, Flow> {
    if let Some(existing) = seen.get(&source_id) {
        return Ok(*existing);
    }

    let source = eval.keymaps.get(source_id).cloned().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![
                Value::symbol("keymapp"),
                Value::Int(encode_keymap_handle(source_id)),
            ],
        )
    })?;

    let clone_id = eval.keymaps.make_sparse_keymap(source.name.clone());
    seen.insert(source_id, clone_id);

    let mut cloned_bindings = std::collections::HashMap::with_capacity(source.bindings.len());
    for (event, binding) in &source.bindings {
        let copied = match binding {
            KeyBinding::Prefix(child_id) => {
                KeyBinding::Prefix(clone_keymap_tree(eval, *child_id, seen)?)
            }
            _ => binding.clone(),
        };
        cloned_bindings.insert(event.clone(), copied);
    }

    let cloned_default = source
        .default_binding
        .as_ref()
        .map(|binding| match binding.as_ref() {
            KeyBinding::Prefix(child_id) => {
                clone_keymap_tree(eval, *child_id, seen).map(KeyBinding::Prefix)
            }
            _ => Ok(binding.as_ref().clone()),
        })
        .transpose()?
        .map(Box::new);

    if let Some(target) = eval.keymaps.get_mut(clone_id) {
        // Oracle keeps parent links shared instead of recursively cloning
        // parent chains; only nested prefix maps are copied.
        target.parent = source.parent;
        target.bindings = cloned_bindings;
        target.default_binding = cloned_default;
    }
    Ok(clone_id)
}

/// `(copy-keymap KEYMAP)` -> keymap copy.
fn builtin_copy_keymap(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("copy-keymap", &args, 1)?;
    let keymap_id = expect_keymap_id(eval, &args[0])?;
    let mut seen = std::collections::HashMap::new();
    let copied = clone_keymap_tree(eval, keymap_id, &mut seen)?;
    Ok(Value::Int(encode_keymap_handle(copied)))
}

/// (define-key KEYMAP KEY DEF &optional REMOVE) -> DEF
fn builtin_define_key(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("define-key", &args, 3)?;
    expect_max_args("define-key", &args, 4)?;
    let keymap_id = expect_keymap_id(eval, &args[0])?;
    let keys = expect_key_description(&args[1])?;
    let binding = value_to_key_binding(eval, &args[2]);
    define_key_in_map(eval, keymap_id, keys, binding);
    Ok(args[2].clone())
}

/// (lookup-key KEYMAP KEY) -> binding or nil
fn builtin_lookup_key(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("lookup-key", &args, 2)?;
    let keymap_id = expect_keymap_id(eval, &args[0])?;
    let keys = expect_key_description(&args[1])?;

    if keys.is_empty() {
        // Oracle returns the original keymap object for empty key sequences.
        return Ok(Value::Int(encode_keymap_handle(keymap_id)));
    }

    if keys.len() == 1 {
        return match eval.keymaps.lookup_key(keymap_id, &keys[0]) {
            Some(binding) => Ok(key_binding_to_value(binding)),
            None => Ok(Value::Nil),
        };
    }

    let mut current_map = keymap_id;
    for (i, key) in keys.iter().enumerate() {
        let Some(binding) = eval.keymaps.lookup_key(current_map, key) else {
            // Oracle lookup-key returns 1 when the first key in a multi-key
            // sequence has no binding; deeper misses return nil.
            return if i == 0 {
                Ok(Value::Int(1))
            } else {
                Ok(Value::Nil)
            };
        };

        if i == keys.len() - 1 {
            return Ok(key_binding_to_value(binding));
        }

        match binding {
            KeyBinding::Prefix(next_map) => {
                current_map = *next_map;
            }
            _ => {
                // Over-specified sequence past a complete binding: return the
                // matched prefix length.
                return Ok(Value::Int((i + 1) as i64));
            }
        }
    }

    Ok(Value::Nil)
}

/// (global-set-key KEY COMMAND)
fn builtin_global_set_key(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("global-set-key", &args, 2)?;
    let global_id = match eval.keymaps.global_map() {
        Some(id) => id,
        None => {
            let id = eval.keymaps.make_keymap();
            eval.keymaps.set_global_map(id);
            id
        }
    };
    let keys = expect_key_description(&args[0])?;
    let binding = value_to_key_binding(eval, &args[1]);
    define_key_in_map(eval, global_id, keys, binding);
    Ok(args[1].clone())
}

/// (local-set-key KEY COMMAND)
fn builtin_local_set_key(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("local-set-key", &args, 2)?;
    let local_id = match eval.current_local_map {
        Some(id) => id,
        None => {
            let id = eval.keymaps.make_sparse_keymap(None);
            eval.current_local_map = Some(id);
            id
        }
    };
    let keys = expect_key_description(&args[0])?;
    let binding = value_to_key_binding(eval, &args[1]);
    define_key_in_map(eval, local_id, keys, binding);
    Ok(args[1].clone())
}

/// (use-local-map KEYMAP)
fn builtin_use_local_map(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("use-local-map", &args, 1)?;
    if args[0].is_nil() {
        eval.current_local_map = None;
    } else {
        let id = expect_keymap_id(eval, &args[0])?;
        eval.current_local_map = Some(id);
    }
    Ok(Value::Nil)
}

/// (use-global-map KEYMAP)
fn builtin_use_global_map(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("use-global-map", &args, 1)?;
    let id = expect_keymap_id(eval, &args[0])?;
    eval.keymaps.set_global_map(id);
    Ok(Value::Nil)
}

/// (current-local-map) -> keymap-id or nil
fn builtin_current_local_map(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("current-local-map", &args, 0)?;
    match eval.current_local_map {
        Some(id) => Ok(Value::Int(encode_keymap_handle(id))),
        None => Ok(Value::Nil),
    }
}

/// (current-global-map) -> keymap-id or nil
fn builtin_current_global_map(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("current-global-map", &args, 0)?;
    let id = match eval.keymaps.global_map() {
        Some(id) => id,
        None => {
            let id = eval.keymaps.make_keymap();
            eval.keymaps.set_global_map(id);
            id
        }
    };
    Ok(Value::Int(encode_keymap_handle(id)))
}

/// `(current-active-maps &optional OLP POSITION)` -> list of active keymaps.
fn builtin_current_active_maps(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("current-active-maps", &args, 2)?;

    let mut maps = Vec::new();
    if let Some(id) = eval.current_local_map {
        maps.push(Value::Int(encode_keymap_handle(id)));
    }

    let global_id = match eval.keymaps.global_map() {
        Some(id) => id,
        None => {
            let id = eval.keymaps.make_keymap();
            eval.keymaps.set_global_map(id);
            id
        }
    };
    maps.push(Value::Int(encode_keymap_handle(global_id)));

    Ok(Value::list(maps))
}

fn builtin_current_minor_mode_maps(args: Vec<Value>) -> EvalResult {
    expect_args("current-minor-mode-maps", &args, 0)?;
    Ok(Value::Nil)
}

/// (keymap-parent KEYMAP) -> keymap-id or nil
fn builtin_keymap_parent(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("keymap-parent", &args, 1)?;
    let id = expect_keymap_id(eval, &args[0])?;
    match eval.keymaps.keymap_parent(id) {
        Some(parent_id) => Ok(Value::Int(encode_keymap_handle(parent_id))),
        None => Ok(Value::Nil),
    }
}

/// (set-keymap-parent KEYMAP PARENT) -> PARENT
fn builtin_set_keymap_parent(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set-keymap-parent", &args, 2)?;
    let id = expect_keymap_id(eval, &args[0])?;
    let parent = if args[1].is_nil() {
        None
    } else {
        Some(expect_keymap_id(eval, &args[1])?)
    };
    eval.keymaps.set_keymap_parent(id, parent);
    Ok(args[1].clone())
}

fn is_lisp_keymap_object(value: &Value) -> bool {
    let Value::Cons(cell) = value else {
        return false;
    };
    let pair = cell.lock().expect("poisoned");
    pair.car.as_symbol_name() == Some("keymap")
}

/// (keymapp OBJ) -> t or nil
fn builtin_keymapp(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("keymapp", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::bool(
            decode_keymap_handle(*n).is_some_and(|id| eval.keymaps.is_keymap(id)),
        )),
        Value::Cons(_) => Ok(Value::bool(is_lisp_keymap_object(&args[0]))),
        _ => Ok(Value::Nil),
    }
}

/// (kbd STRING) -> string-or-vector
/// Parses key description text and returns Emacs-style event encoding.
fn builtin_kbd(args: Vec<Value>) -> EvalResult {
    expect_args("kbd", &args, 1)?;
    let desc = match &args[0] {
        Value::Str(s) => s.as_str(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ));
        }
    };
    super::kbd::parse_kbd_string(desc).map_err(|msg| signal("error", vec![Value::string(msg)]))
}

const KEY_CHAR_META: i64 = 0x8000000;
const KEY_CHAR_CTRL: i64 = 0x4000000;
const KEY_CHAR_SHIFT: i64 = 0x2000000;
const KEY_CHAR_SUPER: i64 = 0x0800000;
const KEY_CHAR_HYPER: i64 = 0x1000000;
const KEY_CHAR_ALT: i64 = 0x0400000;
const KEY_CHAR_MOD_MASK: i64 =
    KEY_CHAR_META | KEY_CHAR_CTRL | KEY_CHAR_SHIFT | KEY_CHAR_SUPER | KEY_CHAR_HYPER | KEY_CHAR_ALT;
const KEY_CHAR_CODE_MASK: i64 = 0x3FFFFF;

fn invalid_single_key_error() -> Flow {
    signal(
        "error",
        vec![Value::string(
            "KEY must be an integer, cons, symbol, or string",
        )],
    )
}

fn control_char_suffix(code: i64) -> Option<char> {
    match code {
        0 => Some('@'),
        1..=26 => char::from_u32((code as u32) + 96),
        28 => Some('\\'),
        29 => Some(']'),
        30 => Some('^'),
        31 => Some('_'),
        _ => None,
    }
}

fn named_char_name(code: i64) -> Option<&'static str> {
    match code {
        9 => Some("TAB"),
        13 => Some("RET"),
        27 => Some("ESC"),
        32 => Some("SPC"),
        127 => Some("DEL"),
        _ => None,
    }
}

fn split_symbol_modifiers(mut name: &str) -> (String, &str) {
    let mut prefix = String::new();
    let is_single_char = |s: &str| {
        let mut chars = s.chars();
        chars.next().is_some() && chars.next().is_none()
    };
    loop {
        if let Some(rest) = name.strip_prefix("C-") {
            if is_single_char(rest) {
                break;
            }
            prefix.push_str("C-");
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("M-") {
            if is_single_char(rest) {
                break;
            }
            prefix.push_str("M-");
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("S-") {
            if is_single_char(rest) {
                break;
            }
            prefix.push_str("S-");
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("s-") {
            if is_single_char(rest) {
                break;
            }
            prefix.push_str("s-");
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("H-") {
            if is_single_char(rest) {
                break;
            }
            prefix.push_str("H-");
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("A-") {
            if is_single_char(rest) {
                break;
            }
            prefix.push_str("A-");
            name = rest;
            continue;
        }
        break;
    }
    (prefix, name)
}

fn describe_symbol_key(name: &str, no_angles: bool) -> String {
    let (prefix, base) = split_symbol_modifiers(name);
    if no_angles {
        return format!("{prefix}{base}");
    }
    format!("{prefix}<{base}>")
}

fn describe_int_key(code: i64) -> Result<String, Flow> {
    let mods = code & KEY_CHAR_MOD_MASK;
    let base = code & !KEY_CHAR_MOD_MASK;
    if !(0..=0x10FFFF).contains(&base) {
        return Err(invalid_single_key_error());
    }

    let ctrl = (mods & KEY_CHAR_CTRL) != 0;
    let meta = (mods & KEY_CHAR_META) != 0;
    let shift = (mods & KEY_CHAR_SHIFT) != 0;
    let super_ = (mods & KEY_CHAR_SUPER) != 0;

    let push_prefixes = |out: &mut String, with_ctrl: bool| {
        if with_ctrl {
            out.push_str("C-");
        }
        if meta {
            out.push_str("M-");
        }
        if shift {
            out.push_str("S-");
        }
        if super_ {
            out.push_str("s-");
        }
        if (mods & KEY_CHAR_HYPER) != 0 {
            out.push_str("H-");
        }
        if (mods & KEY_CHAR_ALT) != 0 {
            out.push_str("A-");
        }
    };

    let mut out = String::new();

    if let Some(name) = named_char_name(base) {
        push_prefixes(&mut out, ctrl);
        out.push_str(name);
        return Ok(out);
    }

    if let Some(sfx) = control_char_suffix(base) {
        push_prefixes(&mut out, true);
        out.push(sfx.to_ascii_lowercase());
        return Ok(out);
    }

    let Some(ch) = char::from_u32(base as u32) else {
        return Err(invalid_single_key_error());
    };
    push_prefixes(&mut out, ctrl);
    out.push(ch);
    Ok(out)
}

fn describe_single_key_value(value: &Value, no_angles: bool) -> Result<String, Flow> {
    match value {
        Value::Int(n) => describe_int_key(*n),
        Value::Char(c) => describe_int_key(*c as i64),
        Value::Symbol(name) => Ok(describe_symbol_key(name, no_angles)),
        Value::True => Ok(describe_symbol_key("t", no_angles)),
        Value::Nil => Ok(describe_symbol_key("nil", no_angles)),
        Value::Str(s) => Ok((**s).clone()),
        Value::Cons(_) => {
            let items = list_to_vec(value).ok_or_else(invalid_single_key_error)?;
            if items.len() != 1 {
                return Err(invalid_single_key_error());
            }
            describe_single_key_value(&items[0], no_angles)
        }
        _ => Err(invalid_single_key_error()),
    }
}

fn key_sequence_values(value: &Value) -> Result<Vec<Value>, Flow> {
    match value {
        Value::Nil => Ok(vec![]),
        Value::Str(s) => Ok(s.chars().map(|ch| Value::Int(ch as i64)).collect()),
        Value::Vector(v) => Ok(v.lock().expect("vector lock poisoned").clone()),
        Value::Cons(_) => list_to_vec(value).ok_or_else(|| {
            signal(
                "wrong-type-argument",
                vec![Value::symbol("sequencep"), value.clone()],
            )
        }),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), value.clone()],
        )),
    }
}

fn resolve_control_code(code: i64) -> Option<i64> {
    match code {
        32 => Some(0),               // SPC
        63 => Some(127),             // ?
        64 => Some(0),               // @
        65..=90 => Some(code - 64),  // A-Z
        91 => Some(27),              // [
        92 => Some(28),              // \
        93 => Some(29),              // ]
        94 => Some(30),              // ^
        95 => Some(31),              // _
        97..=122 => Some(code - 96), // a-z
        _ => None,
    }
}

fn event_modifier_bit(symbol: &str) -> Option<i64> {
    match symbol {
        "control" => Some(KEY_CHAR_CTRL),
        "meta" => Some(KEY_CHAR_META),
        "shift" => Some(KEY_CHAR_SHIFT),
        "super" => Some(KEY_CHAR_SUPER),
        "hyper" => Some(KEY_CHAR_HYPER),
        "alt" => Some(KEY_CHAR_ALT),
        _ => None,
    }
}

fn event_modifier_prefix(bits: i64) -> String {
    let mut out = String::new();
    if (bits & KEY_CHAR_CTRL) != 0 {
        out.push_str("C-");
    }
    if (bits & KEY_CHAR_META) != 0 {
        out.push_str("M-");
    }
    if (bits & KEY_CHAR_SHIFT) != 0 {
        out.push_str("S-");
    }
    if (bits & KEY_CHAR_SUPER) != 0 {
        out.push_str("s-");
    }
    if (bits & KEY_CHAR_HYPER) != 0 {
        out.push_str("H-");
    }
    if (bits & KEY_CHAR_ALT) != 0 {
        out.push_str("A-");
    }
    out
}

fn basic_char_code(mut code: i64) -> i64 {
    code &= KEY_CHAR_CODE_MASK;
    match code {
        0 => 64,
        1..=26 => code + 96,
        27..=31 => code + 64,
        65..=90 => code + 32,
        _ => code,
    }
}

fn symbol_has_modifier_prefix(name: &str) -> bool {
    name.starts_with("C-")
        || name.starts_with("M-")
        || name.starts_with("S-")
        || name.starts_with("s-")
        || name.starts_with("H-")
        || name.starts_with("A-")
}

/// `(event-convert-list EVENT-DESC)` -> event object or nil
fn builtin_event_convert_list(args: Vec<Value>) -> EvalResult {
    expect_args("event-convert-list", &args, 1)?;
    let Some(items) = list_to_vec(&args[0]) else {
        return Ok(Value::Nil);
    };
    if items.is_empty() {
        return Ok(Value::Nil);
    }
    if items.len() == 1 {
        return Ok(items[0].clone());
    }

    let mut mod_bits = 0i64;
    let mut base: Option<Value> = None;
    for item in items {
        if base.is_none() {
            if let Some(sym) = item.as_symbol_name() {
                if let Some(bit) = event_modifier_bit(sym) {
                    mod_bits |= bit;
                    continue;
                }
            }
            base = Some(item);
        } else {
            return Err(signal(
                "error",
                vec![Value::string("Invalid event description")],
            ));
        }
    }

    let Some(base) = base else {
        return Ok(Value::Nil);
    };

    match base {
        Value::Int(_) | Value::Char(_) => {
            let mut code = match base {
                Value::Int(i) => i,
                Value::Char(c) => c as i64,
                _ => unreachable!(),
            };

            let ctrl = (mod_bits & KEY_CHAR_CTRL) != 0;
            let shift = (mod_bits & KEY_CHAR_SHIFT) != 0;

            if shift && !ctrl && (97..=122).contains(&code) {
                code -= 32;
                mod_bits &= !KEY_CHAR_SHIFT;
            }
            if ctrl && code <= 31 {
                mod_bits &= !KEY_CHAR_CTRL;
            }
            if ctrl && code != 32 && code != 63 {
                if let Some(resolved) = resolve_control_code(code) {
                    if (65..=90).contains(&code) {
                        mod_bits |= KEY_CHAR_SHIFT;
                    }
                    code = resolved;
                    mod_bits &= !KEY_CHAR_CTRL;
                }
            }
            Ok(Value::Int(code | mod_bits))
        }
        Value::Symbol(name) => {
            if mod_bits == 0 {
                Ok(Value::symbol(name))
            } else {
                Ok(Value::symbol(format!(
                    "{}{}",
                    event_modifier_prefix(mod_bits),
                    name
                )))
            }
        }
        Value::Nil | Value::True => {
            if mod_bits == 0 {
                Ok(base)
            } else {
                Err(signal(
                    "error",
                    vec![Value::string("Invalid event description")],
                ))
            }
        }
        _ => Err(signal(
            "error",
            vec![Value::string("Invalid event description")],
        )),
    }
}

/// `(event-basic-type EVENT)` -> base event type
fn builtin_event_basic_type(args: Vec<Value>) -> EvalResult {
    expect_args("event-basic-type", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(basic_char_code(*n))),
        Value::Char(c) => Ok(Value::Int(basic_char_code(*c as i64))),
        Value::Str(_) => Ok(Value::Nil),
        Value::Nil => Ok(Value::Nil),
        Value::True => Ok(Value::True),
        Value::Vector(_) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), args[0].clone()],
        )),
        Value::Symbol(name) => {
            if symbol_has_modifier_prefix(name) {
                Ok(Value::Nil)
            } else {
                Ok(args[0].clone())
            }
        }
        Value::Cons(_) => {
            let items = list_to_vec(&args[0]).unwrap_or_default();
            if let Some(first) = items.first() {
                builtin_event_basic_type(vec![first.clone()])
            } else {
                Ok(Value::Nil)
            }
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), args[0].clone()],
        )),
    }
}

/// `(text-char-description CHARACTER)` -> printable text description.
fn builtin_text_char_description(args: Vec<Value>) -> EvalResult {
    expect_args("text-char-description", &args, 1)?;
    let code = match &args[0] {
        Value::Int(n) if (0..=KEY_CHAR_CODE_MASK).contains(n) => *n,
        Value::Char(c) => *c as i64,
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), args[0].clone()],
            ))
        }
    };
    if (code & !KEY_CHAR_CODE_MASK) != 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), args[0].clone()],
        ));
    }

    let rendered = match code {
        0 => "^@".to_string(),
        1..=26 => format!("^{}", char::from_u32((code as u32) + 64).unwrap_or('?')),
        27 => "^[".to_string(),
        28 => "^\\\\".to_string(),
        29 => "^]".to_string(),
        30 => "^^".to_string(),
        31 => "^_".to_string(),
        127 => "^?".to_string(),
        _ => match char::from_u32(code as u32) {
            Some(ch) => ch.to_string(),
            None => "\u{FFFD}\u{FFFD}\u{FFFD}\u{FFFD}".to_string(),
        },
    };
    Ok(Value::string(rendered))
}

fn parse_event_symbol_prefixes(mut name: &str) -> (Vec<Value>, &str) {
    let mut mods = Vec::new();
    loop {
        if let Some(rest) = name.strip_prefix("C-") {
            mods.push(Value::symbol("control"));
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("M-") {
            mods.push(Value::symbol("meta"));
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("S-") {
            mods.push(Value::symbol("shift"));
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("s-") {
            mods.push(Value::symbol("super"));
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("H-") {
            mods.push(Value::symbol("hyper"));
            name = rest;
            continue;
        }
        if let Some(rest) = name.strip_prefix("A-") {
            mods.push(Value::symbol("alt"));
            name = rest;
            continue;
        }
        break;
    }
    (mods, name)
}

fn mouse_event_kind_symbol(name: &str) -> Option<Value> {
    if name.starts_with("down-mouse-") {
        return Some(Value::symbol("down"));
    }
    if name.starts_with("drag-mouse-") {
        return Some(Value::symbol("drag"));
    }
    if name.starts_with("double-mouse-") {
        return Some(Value::symbol("double"));
    }
    if name.starts_with("triple-mouse-") {
        return Some(Value::symbol("triple"));
    }
    if name.contains("mouse-") {
        return Some(Value::symbol("click"));
    }
    None
}

/// `(eventp OBJECT)` -> non-nil if OBJECT is an event.
fn builtin_eventp(args: Vec<Value>) -> EvalResult {
    expect_args("eventp", &args, 1)?;
    let is_event = match &args[0] {
        Value::Int(_) | Value::Char(_) | Value::True | Value::Symbol(_) => true,
        Value::Cons(_) => match list_to_vec(&args[0]) {
            Some(items) => matches!(items.first(), Some(Value::Symbol(_)) | Some(Value::True)),
            None => false,
        },
        _ => false,
    };
    Ok(Value::bool(is_event))
}

/// `(event-modifiers EVENT)` -> list of event modifier symbols.
fn builtin_event_modifiers(args: Vec<Value>) -> EvalResult {
    expect_args("event-modifiers", &args, 1)?;
    match &args[0] {
        Value::Int(n) => {
            let code = *n & KEY_CHAR_CODE_MASK;
            let mods = *n & !KEY_CHAR_CODE_MASK;
            let mut out = Vec::new();
            if (mods & KEY_CHAR_SHIFT) != 0 {
                out.push(Value::symbol("shift"));
            }
            if (mods & KEY_CHAR_CTRL) != 0 || code <= 31 {
                out.push(Value::symbol("control"));
            }
            if (mods & KEY_CHAR_SUPER) != 0 {
                out.push(Value::symbol("super"));
            }
            if (mods & KEY_CHAR_META) != 0 {
                out.push(Value::symbol("meta"));
            }
            if (mods & KEY_CHAR_HYPER) != 0 {
                out.push(Value::symbol("hyper"));
            }
            if (mods & KEY_CHAR_ALT) != 0 {
                out.push(Value::symbol("alt"));
            }
            Ok(Value::list(out))
        }
        Value::Char(c) => builtin_event_modifiers(vec![Value::Int(*c as i64)]),
        Value::Symbol(name) => {
            let (mut out, base) = parse_event_symbol_prefixes(name);
            if let Some(kind) = mouse_event_kind_symbol(base) {
                out.push(kind);
            }
            Ok(Value::list(out))
        }
        Value::Cons(_) => {
            let items = list_to_vec(&args[0]).unwrap_or_default();
            if let Some(first) = items.first() {
                builtin_event_modifiers(vec![first.clone()])
            } else {
                Ok(Value::Nil)
            }
        }
        _ => Ok(Value::Nil),
    }
}

/// `(event-apply-modifier EVENT MODIFIER LSHIFTBY PREFIX)` -- apply one key
/// modifier operation to EVENT.
fn builtin_event_apply_modifier(args: Vec<Value>) -> EvalResult {
    expect_args("event-apply-modifier", &args, 4)?;

    let event = match &args[0] {
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        Value::Nil | Value::True | Value::Symbol(_) | Value::Cons(_) => return Ok(args[0].clone()),
        Value::Str(_) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), args[0].clone()],
            ))
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("integer-or-marker-p"), args[0].clone()],
            ))
        }
    };

    let modifier = args[1].as_symbol_name();
    if modifier == Some("control") {
        let code = event & KEY_CHAR_CODE_MASK;
        let mut mod_bits = event & !KEY_CHAR_CODE_MASK;
        if code <= 31 {
            return Ok(Value::Int(mod_bits | code));
        }
        if code != 32 && code != 63 {
            if let Some(resolved) = resolve_control_code(code) {
                if (65..=90).contains(&code) || (mod_bits & KEY_CHAR_SHIFT) != 0 {
                    mod_bits |= KEY_CHAR_SHIFT;
                }
                return Ok(Value::Int(mod_bits | resolved));
            }
        }
        return Ok(Value::Int(mod_bits | (code | 1)));
    }

    if modifier == Some("shift") {
        let code = event & KEY_CHAR_CODE_MASK;
        let mod_bits = event & !KEY_CHAR_CODE_MASK;
        let shifted = match code {
            0 => 1,
            1..=26 => code + 64,
            28 => 29,
            30 => 31,
            32 => 33,
            c if (34..=62).contains(&c) && c % 2 == 0 => c + 1,
            64 => 65,
            92 => 93,
            94 => 95,
            96 => 97,
            124 => 125,
            126 => 127,
            97..=122 => code - 32,
            _ => code,
        };
        return Ok(Value::Int(mod_bits | shifted));
    }

    let lshiftby = match &args[2] {
        Value::Int(n) => *n,
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("integerp"), args[2].clone()],
            ))
        }
    };

    if !(0..63).contains(&lshiftby) {
        return Ok(Value::Int(event));
    }

    Ok(Value::Int(event | (1i64 << lshiftby)))
}

/// `(listify-key-sequence SEQUENCE)` -> list representation of key sequence.
fn builtin_listify_key_sequence(args: Vec<Value>) -> EvalResult {
    expect_args("listify-key-sequence", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Str(s) => Ok(Value::list(
            s.chars().map(|ch| Value::Int(ch as i64)).collect(),
        )),
        Value::Vector(v) => Ok(Value::list(v.lock().expect("vector lock poisoned").clone())),
        Value::Cons(_) => {
            let items = list_to_vec(&args[0]).ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("sequencep"), args[0].clone()],
                )
            })?;
            for item in &items {
                if !matches!(item, Value::Int(_) | Value::Char(_)) {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("number-or-marker-p"), item.clone()],
                    ));
                }
            }
            Ok(Value::list(items))
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), args[0].clone()],
        )),
    }
}

fn key_valid_token(token: &str) -> bool {
    let mut rest = token;
    loop {
        if let Some(next) = rest.strip_prefix("C-") {
            if next.is_empty() {
                return false;
            }
            rest = next;
            continue;
        }
        if let Some(next) = rest.strip_prefix("M-") {
            if next.is_empty() {
                return false;
            }
            rest = next;
            continue;
        }
        if let Some(next) = rest.strip_prefix("S-") {
            if next.is_empty() {
                return false;
            }
            rest = next;
            continue;
        }
        if let Some(next) = rest.strip_prefix("s-") {
            if next.is_empty() {
                return false;
            }
            rest = next;
            continue;
        }
        if let Some(next) = rest.strip_prefix("H-") {
            if next.is_empty() {
                return false;
            }
            rest = next;
            continue;
        }
        if let Some(next) = rest.strip_prefix("A-") {
            if next.is_empty() {
                return false;
            }
            rest = next;
            continue;
        }
        break;
    }
    if rest.is_empty() {
        return false;
    }

    if let Some(inner) = rest.strip_prefix('<').and_then(|s| s.strip_suffix('>')) {
        return !inner.is_empty();
    }

    if matches!(rest, "RET" | "TAB" | "SPC" | "ESC" | "DEL" | "return") {
        return true;
    }

    let mut chars = rest.chars();
    if chars.next().is_some() && chars.next().is_none() {
        return true;
    }

    false
}

/// `(key-valid-p KEY-DESC)` -> non-nil when KEY-DESC is a valid key description string.
fn builtin_key_valid_p(args: Vec<Value>) -> EvalResult {
    expect_args("key-valid-p", &args, 1)?;
    let Value::Str(s) = &args[0] else {
        return Ok(Value::Nil);
    };
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return Ok(Value::Nil);
    }
    let valid = trimmed.split_whitespace().all(key_valid_token);
    Ok(Value::bool(valid))
}

/// `(single-key-description KEY &optional NO-ANGLES)` -> string
fn builtin_single_key_description(args: Vec<Value>) -> EvalResult {
    expect_range_args("single-key-description", &args, 1, 2)?;
    let no_angles = args.get(1).is_some_and(Value::is_truthy);
    Ok(Value::string(describe_single_key_value(
        &args[0], no_angles,
    )?))
}

/// `(key-description KEYS &optional PREFIX)` -> string
fn builtin_key_description(args: Vec<Value>) -> EvalResult {
    expect_range_args("key-description", &args, 1, 2)?;
    let mut events = if let Some(prefix) = args.get(1) {
        key_sequence_values(prefix)?
    } else {
        vec![]
    };
    events.extend(key_sequence_values(&args[0])?);
    let rendered: Result<Vec<String>, Flow> = events
        .iter()
        .map(|event| describe_single_key_value(event, false))
        .collect();
    Ok(Value::string(rendered?.join(" ")))
}

/// `(help-key-description TRANSLATED UNTRANSLATED)` -> key description for help output.
fn event_ascii_latin_letter_name(event: &Value) -> Option<String> {
    let ch = match event {
        Value::Char(c) => *c,
        Value::Int(n) if (0..=0x7f).contains(n) => char::from_u32(*n as u32)?,
        _ => return None,
    };
    if ch.is_ascii_lowercase() {
        Some(format!("LATIN SMALL LETTER {}", ch.to_ascii_uppercase()))
    } else if ch.is_ascii_uppercase() {
        Some(format!("LATIN CAPITAL LETTER {ch}"))
    } else {
        None
    }
}

fn builtin_help_key_description(args: Vec<Value>) -> EvalResult {
    expect_args("help-key-description", &args, 2)?;

    let translated = &args[0];
    let untranslated = &args[1];

    if untranslated.is_nil() {
        if translated.is_nil() {
            return Ok(Value::Nil);
        }
        let rendered: Result<Vec<String>, Flow> = key_sequence_values(translated)?
            .iter()
            .map(|event| describe_single_key_value(event, false))
            .collect();
        return Ok(Value::string(rendered?.join(" ")));
    }

    let untranslated_events = match untranslated {
        Value::Str(_) | Value::Vector(_) => key_sequence_values(untranslated)?,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("arrayp"), other.clone()],
            ))
        }
    };
    if untranslated_events.is_empty() {
        return Err(signal(
            "args-out-of-range",
            vec![untranslated.clone(), Value::Int(0)],
        ));
    }

    let translated_events = if translated.is_nil() {
        None
    } else {
        Some(key_sequence_values(translated)?)
    };
    let translated_desc = match translated_events.as_ref() {
        Some(events) => {
            let rendered: Result<Vec<String>, Flow> = events
                .iter()
                .map(|event| describe_single_key_value(event, false))
                .collect();
            rendered?.join(" ")
        }
        None => "nil".to_string(),
    };

    let untranslated_desc = {
        let rendered: Result<Vec<String>, Flow> = untranslated_events
            .iter()
            .map(|event| describe_single_key_value(event, false))
            .collect();
        rendered?.join(" ")
    };

    if translated_desc == untranslated_desc {
        Ok(Value::string(translated_desc))
    } else {
        if let Some(events) = translated_events.as_ref() {
            if events.len() == 1 && untranslated_events.len() == 1 {
                if let Some(name) = event_ascii_latin_letter_name(&events[0]) {
                    return Ok(Value::string(format!(
                        "{translated_desc} '{name}' (translated from {untranslated_desc})"
                    )));
                }
            }
        }
        Ok(Value::string(format!(
            "{translated_desc} (translated from {untranslated_desc})"
        )))
    }
}

/// `(recent-keys &optional INCLUDE-CMDS)` -> vector of recent input events.
fn builtin_recent_keys(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("recent-keys", &args, 1)?;
    Ok(Value::vector(eval.recent_input_events().to_vec()))
}

// ===========================================================================
// Dispatch table
// ===========================================================================

#[derive(EnumString)]
enum PureBuiltinId {
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Sub,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
    #[strum(serialize = "%")]
    Percent,
    #[strum(serialize = "mod")]
    Mod,
    #[strum(serialize = "1+")]
    Add1,
    #[strum(serialize = "1-")]
    Sub1,
    #[strum(serialize = "=")]
    NumEq,
    #[strum(serialize = "<")]
    NumLt,
    #[strum(serialize = "<=")]
    NumLe,
    #[strum(serialize = ">")]
    NumGt,
    #[strum(serialize = ">=")]
    NumGe,
    #[strum(serialize = "/=")]
    NumNe,
    #[strum(serialize = "max")]
    Max,
    #[strum(serialize = "min")]
    Min,
    #[strum(serialize = "abs")]
    Abs,
    #[strum(serialize = "logand")]
    LogAnd,
    #[strum(serialize = "logior")]
    LogIor,
    #[strum(serialize = "logxor")]
    LogXor,
    #[strum(serialize = "lognot")]
    LogNot,
    #[strum(serialize = "ash")]
    Ash,
    #[strum(serialize = "null")]
    Null,
    #[strum(serialize = "not")]
    Not,
    #[strum(serialize = "ignore")]
    Ignore,
    #[strum(serialize = "atom")]
    Atom,
    #[strum(serialize = "consp")]
    Consp,
    #[strum(serialize = "listp")]
    Listp,
    #[strum(serialize = "nlistp")]
    NListp,
    #[strum(serialize = "symbolp")]
    Symbolp,
    #[strum(serialize = "numberp")]
    Numberp,
    #[strum(serialize = "integerp")]
    Integerp,
    #[strum(serialize = "floatp")]
    Floatp,
    #[strum(serialize = "stringp")]
    Stringp,
    #[strum(serialize = "vectorp")]
    Vectorp,
    #[strum(serialize = "characterp")]
    Characterp,
    #[strum(serialize = "functionp")]
    Functionp,
    #[strum(serialize = "keywordp")]
    Keywordp,
    #[strum(serialize = "hash-table-p")]
    HashTablep,
    #[strum(serialize = "bufferp")]
    Bufferp,
    #[strum(serialize = "type-of")]
    TypeOf,
    #[strum(serialize = "sequencep")]
    Sequencep,
    #[strum(serialize = "arrayp")]
    Arrayp,
    #[strum(serialize = "eq")]
    Eq,
    #[strum(serialize = "eql")]
    Eql,
    #[strum(serialize = "equal")]
    Equal,
    #[strum(serialize = "cons")]
    Cons,
    #[strum(serialize = "car")]
    Car,
    #[strum(serialize = "cdr")]
    Cdr,
    #[strum(serialize = "caar")]
    Caar,
    #[strum(serialize = "cadr")]
    Cadr,
    #[strum(serialize = "cdar")]
    Cdar,
    #[strum(serialize = "cddr")]
    Cddr,
    #[strum(serialize = "caaar")]
    Caaar,
    #[strum(serialize = "caadr")]
    Caadr,
    #[strum(serialize = "cadar")]
    Cadar,
    #[strum(serialize = "caddr")]
    Caddr,
    #[strum(serialize = "cdaar")]
    Cdaar,
    #[strum(serialize = "cdadr")]
    Cdadr,
    #[strum(serialize = "cddar")]
    Cddar,
    #[strum(serialize = "cdddr")]
    Cdddr,
    #[strum(serialize = "cadddr")]
    Cadddr,
    #[strum(serialize = "cddddr")]
    Cddddr,
    #[strum(serialize = "caaaar")]
    Caaaar,
    #[strum(serialize = "caaadr")]
    Caaadr,
    #[strum(serialize = "caadar")]
    Caadar,
    #[strum(serialize = "caaddr")]
    Caaddr,
    #[strum(serialize = "cadaar")]
    Cadaar,
    #[strum(serialize = "cadadr")]
    Cadadr,
    #[strum(serialize = "caddar")]
    Caddar,
    #[strum(serialize = "cdaaar")]
    Cdaaar,
    #[strum(serialize = "cdaadr")]
    Cdaadr,
    #[strum(serialize = "cdadar")]
    Cdadar,
    #[strum(serialize = "cdaddr")]
    Cdaddr,
    #[strum(serialize = "cddaar")]
    Cddaar,
    #[strum(serialize = "cddadr")]
    Cddadr,
    #[strum(serialize = "cdddar")]
    Cdddar,
    #[strum(serialize = "car-safe")]
    CarSafe,
    #[strum(serialize = "cdr-safe")]
    CdrSafe,
    #[strum(serialize = "setcar")]
    Setcar,
    #[strum(serialize = "setcdr")]
    Setcdr,
    #[strum(serialize = "list")]
    List,
    #[strum(serialize = "length")]
    Length,
    #[strum(serialize = "nth")]
    Nth,
    #[strum(serialize = "nthcdr")]
    Nthcdr,
    #[strum(serialize = "append")]
    Append,
    #[strum(serialize = "reverse")]
    Reverse,
    #[strum(serialize = "nreverse")]
    Nreverse,
    #[strum(serialize = "member")]
    Member,
    #[strum(serialize = "memq")]
    Memq,
    #[strum(serialize = "assoc")]
    Assoc,
    #[strum(serialize = "assq")]
    Assq,
    #[strum(serialize = "copy-sequence")]
    CopySequence,
    #[strum(serialize = "string-equal", serialize = "string=")]
    StringEqual,
    #[strum(serialize = "string-lessp", serialize = "string<")]
    StringLessp,
    #[strum(serialize = "substring")]
    Substring,
    #[strum(serialize = "concat")]
    Concat,
    #[strum(serialize = "string")]
    String,
    #[strum(serialize = "unibyte-string")]
    UnibyteString,
    #[strum(serialize = "string-to-number")]
    StringToNumber,
    #[strum(serialize = "number-to-string")]
    NumberToString,
    #[strum(serialize = "upcase")]
    Upcase,
    #[strum(serialize = "downcase")]
    Downcase,
    #[strum(serialize = "format")]
    Format,
    #[strum(serialize = "make-vector")]
    MakeVector,
    #[strum(serialize = "vector")]
    Vector,
    #[strum(serialize = "aref")]
    Aref,
    #[strum(serialize = "aset")]
    Aset,
    #[strum(serialize = "vconcat")]
    Vconcat,
    #[strum(serialize = "float")]
    Float,
    #[strum(serialize = "truncate")]
    Truncate,
    #[strum(serialize = "floor")]
    Floor,
    #[strum(serialize = "ceiling")]
    Ceiling,
    #[strum(serialize = "round")]
    Round,
    #[strum(serialize = "char-to-string")]
    CharToString,
    #[strum(serialize = "string-to-char")]
    StringToChar,
    #[strum(serialize = "make-hash-table")]
    MakeHashTable,
    #[strum(serialize = "gethash")]
    Gethash,
    #[strum(serialize = "puthash")]
    Puthash,
    #[strum(serialize = "remhash")]
    Remhash,
    #[strum(serialize = "clrhash")]
    Clrhash,
    #[strum(serialize = "hash-table-count")]
    HashTableCount,
    #[strum(serialize = "plist-get")]
    PlistGet,
    #[strum(serialize = "plist-put")]
    PlistPut,
    #[strum(serialize = "symbol-name")]
    SymbolName,
    #[strum(serialize = "make-symbol")]
    MakeSymbol,
    #[strum(serialize = "sqrt")]
    Sqrt,
    #[strum(serialize = "sin")]
    Sin,
    #[strum(serialize = "cos")]
    Cos,
    #[strum(serialize = "tan")]
    Tan,
    #[strum(serialize = "asin")]
    Asin,
    #[strum(serialize = "acos")]
    Acos,
    #[strum(serialize = "atan")]
    Atan,
    #[strum(serialize = "exp")]
    Exp,
    #[strum(serialize = "log")]
    Log,
    #[strum(serialize = "expt")]
    Expt,
    #[strum(serialize = "random")]
    Random,
    #[strum(serialize = "isnan")]
    Isnan,
    #[strum(serialize = "string-prefix-p")]
    StringPrefixP,
    #[strum(serialize = "string-suffix-p")]
    StringSuffixP,
    #[strum(serialize = "string-join")]
    StringJoin,
    #[strum(serialize = "split-string")]
    SplitString,
    #[strum(serialize = "string-trim")]
    StringTrim,
    #[strum(serialize = "string-trim-left")]
    StringTrimLeft,
    #[strum(serialize = "string-trim-right")]
    StringTrimRight,
    #[strum(serialize = "make-string")]
    MakeString,
    #[strum(serialize = "string-to-list")]
    StringToList,
    #[strum(serialize = "string-width")]
    StringWidth,
    #[strum(serialize = "last")]
    Last,
    #[strum(serialize = "butlast")]
    Butlast,
    #[strum(serialize = "delete")]
    Delete,
    #[strum(serialize = "delq")]
    Delq,
    #[strum(serialize = "elt")]
    Elt,
    #[strum(serialize = "nconc")]
    Nconc,
    #[strum(serialize = "alist-get")]
    AlistGet,
    #[strum(serialize = "number-sequence")]
    NumberSequence,
    #[strum(serialize = "bitmap-spec-p")]
    BitmapSpecP,
    #[strum(serialize = "byte-to-string")]
    ByteToString,
    #[strum(serialize = "clear-buffer-auto-save-failure")]
    ClearBufferAutoSaveFailure,
    #[strum(serialize = "clear-face-cache")]
    ClearFaceCache,
}

fn dispatch_builtin_id_pure(id: PureBuiltinId, args: Vec<Value>) -> EvalResult {
    match id {
        PureBuiltinId::Add => builtin_add(args),
        PureBuiltinId::Sub => builtin_sub(args),
        PureBuiltinId::Mul => builtin_mul(args),
        PureBuiltinId::Div => builtin_div(args),
        PureBuiltinId::Percent => builtin_percent(args),
        PureBuiltinId::Mod => builtin_mod(args),
        PureBuiltinId::Add1 => builtin_add1(args),
        PureBuiltinId::Sub1 => builtin_sub1(args),
        PureBuiltinId::NumEq => builtin_num_eq(args),
        PureBuiltinId::NumLt => builtin_num_lt(args),
        PureBuiltinId::NumLe => builtin_num_le(args),
        PureBuiltinId::NumGt => builtin_num_gt(args),
        PureBuiltinId::NumGe => builtin_num_ge(args),
        PureBuiltinId::NumNe => builtin_num_ne(args),
        PureBuiltinId::Max => builtin_max(args),
        PureBuiltinId::Min => builtin_min(args),
        PureBuiltinId::Abs => builtin_abs(args),
        PureBuiltinId::LogAnd => builtin_logand(args),
        PureBuiltinId::LogIor => builtin_logior(args),
        PureBuiltinId::LogXor => builtin_logxor(args),
        PureBuiltinId::LogNot => builtin_lognot(args),
        PureBuiltinId::Ash => builtin_ash(args),
        PureBuiltinId::Null => builtin_null(args),
        PureBuiltinId::Not => builtin_not(args),
        PureBuiltinId::Ignore => builtin_ignore(args),
        PureBuiltinId::Atom => builtin_atom(args),
        PureBuiltinId::Consp => builtin_consp(args),
        PureBuiltinId::Listp => builtin_listp(args),
        PureBuiltinId::NListp => builtin_nlistp(args),
        PureBuiltinId::Symbolp => builtin_symbolp(args),
        PureBuiltinId::Numberp => builtin_numberp(args),
        PureBuiltinId::Integerp => builtin_integerp(args),
        PureBuiltinId::Floatp => builtin_floatp(args),
        PureBuiltinId::Stringp => builtin_stringp(args),
        PureBuiltinId::Vectorp => builtin_vectorp(args),
        PureBuiltinId::Characterp => builtin_characterp(args),
        PureBuiltinId::Functionp => builtin_functionp(args),
        PureBuiltinId::Keywordp => builtin_keywordp(args),
        PureBuiltinId::HashTablep => builtin_hash_table_p(args),
        PureBuiltinId::Bufferp => builtin_bufferp(args),
        PureBuiltinId::TypeOf => builtin_type_of(args),
        PureBuiltinId::Sequencep => builtin_sequencep(args),
        PureBuiltinId::Arrayp => builtin_arrayp(args),
        PureBuiltinId::Eq => builtin_eq(args),
        PureBuiltinId::Eql => builtin_eql(args),
        PureBuiltinId::Equal => builtin_equal(args),
        PureBuiltinId::Cons => builtin_cons(args),
        PureBuiltinId::Car => builtin_car(args),
        PureBuiltinId::Cdr => builtin_cdr(args),
        PureBuiltinId::Caar => builtin_caar(args),
        PureBuiltinId::Cadr => builtin_cadr(args),
        PureBuiltinId::Cdar => builtin_cdar(args),
        PureBuiltinId::Cddr => builtin_cddr(args),
        PureBuiltinId::Caaar => builtin_caaar(args),
        PureBuiltinId::Caadr => builtin_caadr(args),
        PureBuiltinId::Cadar => builtin_cadar(args),
        PureBuiltinId::Caddr => builtin_caddr(args),
        PureBuiltinId::Cdaar => builtin_cdaar(args),
        PureBuiltinId::Cdadr => builtin_cdadr(args),
        PureBuiltinId::Cddar => builtin_cddar(args),
        PureBuiltinId::Cdddr => builtin_cdddr(args),
        PureBuiltinId::Cadddr => builtin_cadddr(args),
        PureBuiltinId::Cddddr => builtin_cddddr(args),
        PureBuiltinId::Caaaar => builtin_caaaar(args),
        PureBuiltinId::Caaadr => builtin_caaadr(args),
        PureBuiltinId::Caadar => builtin_caadar(args),
        PureBuiltinId::Caaddr => builtin_caaddr(args),
        PureBuiltinId::Cadaar => builtin_cadaar(args),
        PureBuiltinId::Cadadr => builtin_cadadr(args),
        PureBuiltinId::Caddar => builtin_caddar(args),
        PureBuiltinId::Cdaaar => builtin_cdaaar(args),
        PureBuiltinId::Cdaadr => builtin_cdaadr(args),
        PureBuiltinId::Cdadar => builtin_cdadar(args),
        PureBuiltinId::Cdaddr => builtin_cdaddr(args),
        PureBuiltinId::Cddaar => builtin_cddaar(args),
        PureBuiltinId::Cddadr => builtin_cddadr(args),
        PureBuiltinId::Cdddar => builtin_cdddar(args),
        PureBuiltinId::CarSafe => builtin_car_safe(args),
        PureBuiltinId::CdrSafe => builtin_cdr_safe(args),
        PureBuiltinId::Setcar => builtin_setcar(args),
        PureBuiltinId::Setcdr => builtin_setcdr(args),
        PureBuiltinId::List => builtin_list(args),
        PureBuiltinId::Length => builtin_length(args),
        PureBuiltinId::Nth => builtin_nth(args),
        PureBuiltinId::Nthcdr => builtin_nthcdr(args),
        PureBuiltinId::Append => builtin_append(args),
        PureBuiltinId::Reverse => builtin_reverse(args),
        PureBuiltinId::Nreverse => builtin_nreverse(args),
        PureBuiltinId::Member => builtin_member(args),
        PureBuiltinId::Memq => builtin_memq(args),
        PureBuiltinId::Assoc => builtin_assoc(args),
        PureBuiltinId::Assq => builtin_assq(args),
        PureBuiltinId::CopySequence => builtin_copy_sequence(args),
        PureBuiltinId::StringEqual => builtin_string_equal(args),
        PureBuiltinId::StringLessp => builtin_string_lessp(args),
        PureBuiltinId::Substring => builtin_substring(args),
        PureBuiltinId::Concat => builtin_concat(args),
        PureBuiltinId::String => builtin_string(args),
        PureBuiltinId::UnibyteString => builtin_unibyte_string(args),
        PureBuiltinId::StringToNumber => builtin_string_to_number(args),
        PureBuiltinId::NumberToString => builtin_number_to_string(args),
        PureBuiltinId::Upcase => builtin_upcase(args),
        PureBuiltinId::Downcase => builtin_downcase(args),
        PureBuiltinId::Format => builtin_format(args),
        PureBuiltinId::MakeVector => builtin_make_vector(args),
        PureBuiltinId::Vector => builtin_vector(args),
        PureBuiltinId::Aref => builtin_aref(args),
        PureBuiltinId::Aset => builtin_aset(args),
        PureBuiltinId::Vconcat => builtin_vconcat(args),
        PureBuiltinId::Float => builtin_float(args),
        PureBuiltinId::Truncate => builtin_truncate(args),
        PureBuiltinId::Floor => builtin_floor(args),
        PureBuiltinId::Ceiling => builtin_ceiling(args),
        PureBuiltinId::Round => builtin_round(args),
        PureBuiltinId::CharToString => builtin_char_to_string(args),
        PureBuiltinId::StringToChar => builtin_string_to_char(args),
        PureBuiltinId::MakeHashTable => builtin_make_hash_table(args),
        PureBuiltinId::Gethash => builtin_gethash(args),
        PureBuiltinId::Puthash => builtin_puthash(args),
        PureBuiltinId::Remhash => builtin_remhash(args),
        PureBuiltinId::Clrhash => builtin_clrhash(args),
        PureBuiltinId::HashTableCount => builtin_hash_table_count(args),
        PureBuiltinId::PlistGet => builtin_plist_get(args),
        PureBuiltinId::PlistPut => builtin_plist_put(args),
        PureBuiltinId::SymbolName => builtin_symbol_name(args),
        PureBuiltinId::MakeSymbol => builtin_make_symbol(args),
        PureBuiltinId::Sqrt => builtin_sqrt(args),
        PureBuiltinId::Sin => builtin_sin(args),
        PureBuiltinId::Cos => builtin_cos(args),
        PureBuiltinId::Tan => builtin_tan(args),
        PureBuiltinId::Asin => builtin_asin(args),
        PureBuiltinId::Acos => builtin_acos(args),
        PureBuiltinId::Atan => builtin_atan(args),
        PureBuiltinId::Exp => builtin_exp(args),
        PureBuiltinId::Log => builtin_log(args),
        PureBuiltinId::Expt => builtin_expt(args),
        PureBuiltinId::Random => builtin_random(args),
        PureBuiltinId::Isnan => builtin_isnan(args),
        PureBuiltinId::StringPrefixP => builtin_string_prefix_p(args),
        PureBuiltinId::StringSuffixP => builtin_string_suffix_p(args),
        PureBuiltinId::StringJoin => builtin_string_join(args),
        PureBuiltinId::SplitString => builtin_split_string(args),
        PureBuiltinId::StringTrim => builtin_string_trim(args),
        PureBuiltinId::StringTrimLeft => builtin_string_trim_left(args),
        PureBuiltinId::StringTrimRight => builtin_string_trim_right(args),
        PureBuiltinId::MakeString => builtin_make_string(args),
        PureBuiltinId::StringToList => builtin_string_to_list(args),
        PureBuiltinId::StringWidth => builtin_string_width(args),
        PureBuiltinId::Last => builtin_last(args),
        PureBuiltinId::Butlast => builtin_butlast(args),
        PureBuiltinId::Delete => builtin_delete(args),
        PureBuiltinId::Delq => builtin_delq(args),
        PureBuiltinId::Elt => builtin_elt(args),
        PureBuiltinId::Nconc => builtin_nconc(args),
        PureBuiltinId::AlistGet => builtin_alist_get(args),
        PureBuiltinId::NumberSequence => builtin_number_sequence(args),
        PureBuiltinId::BitmapSpecP => builtin_bitmap_spec_p(args),
        PureBuiltinId::ByteToString => builtin_byte_to_string(args),
        PureBuiltinId::ClearBufferAutoSaveFailure => builtin_clear_buffer_auto_save_failure(args),
        PureBuiltinId::ClearFaceCache => builtin_clear_face_cache(args),
    }
}

/// Try to dispatch a builtin function by name. Returns None if not a known builtin.
pub(crate) fn dispatch_builtin(
    eval: &mut super::eval::Evaluator,
    name: &str,
    args: Vec<Value>,
) -> Option<EvalResult> {
    // Functions that need the evaluator (higher-order / obarray access)
    match name {
        "apply" => return Some(builtin_apply(eval, args)),
        "funcall" => return Some(builtin_funcall(eval, args)),
        "funcall-interactively" => return Some(builtin_funcall_interactively(eval, args)),
        "funcall-with-delayed-message" => {
            return Some(builtin_funcall_with_delayed_message(eval, args))
        }
        "defalias" => return Some(builtin_defalias(eval, args)),
        "provide" => return Some(builtin_provide(eval, args)),
        "require" => return Some(builtin_require(eval, args)),
        "mapcan" => return Some(builtin_mapcan(eval, args)),
        "mapcar" => return Some(builtin_mapcar(eval, args)),
        "mapc" => return Some(builtin_mapc(eval, args)),
        "mapconcat" => return Some(builtin_mapconcat(eval, args)),
        "sort" => return Some(builtin_sort(eval, args)),
        "functionp" => return Some(builtin_functionp_eval(eval, args)),
        "macrop" => return Some(builtin_macrop_eval(eval, args)),
        // Symbol/obarray
        "defvaralias" => return Some(builtin_defvaralias_eval(eval, args)),
        "boundp" => return Some(builtin_boundp(eval, args)),
        "default-boundp" => return Some(builtin_default_boundp(eval, args)),
        "default-toplevel-value" => return Some(builtin_default_toplevel_value(eval, args)),
        "fboundp" => return Some(builtin_fboundp(eval, args)),
        "indirect-variable" => return Some(builtin_indirect_variable_eval(eval, args)),
        "symbol-value" => return Some(builtin_symbol_value(eval, args)),
        "symbol-function" => return Some(builtin_symbol_function(eval, args)),
        "set" => return Some(builtin_set(eval, args)),
        "fset" => return Some(builtin_fset(eval, args)),
        "makunbound" => return Some(builtin_makunbound(eval, args)),
        "fmakunbound" => return Some(builtin_fmakunbound(eval, args)),
        "macroexpand" => return Some(builtin_macroexpand_eval(eval, args)),
        "get" => return Some(builtin_get(eval, args)),
        "put" => return Some(builtin_put(eval, args)),
        "setplist" => return Some(builtin_setplist_eval(eval, args)),
        "symbol-plist" => return Some(builtin_symbol_plist_fn(eval, args)),
        "indirect-function" => return Some(builtin_indirect_function(eval, args)),
        "obarrayp" => return Some(builtin_obarrayp_eval(eval, args)),
        "special-variable-p" => return Some(builtin_special_variable_p(eval, args)),
        "intern" => return Some(builtin_intern_fn(eval, args)),
        "intern-soft" => return Some(builtin_intern_soft(eval, args)),
        // Hooks
        "add-hook" => return Some(builtin_add_hook(eval, args)),
        "remove-hook" => return Some(builtin_remove_hook(eval, args)),
        "run-hooks" => return Some(builtin_run_hooks(eval, args)),
        "run-hook-with-args" => return Some(builtin_run_hook_with_args(eval, args)),
        "run-hook-with-args-until-success" => {
            return Some(builtin_run_hook_with_args_until_success(eval, args))
        }
        "run-hook-with-args-until-failure" => {
            return Some(builtin_run_hook_with_args_until_failure(eval, args))
        }
        "run-hook-wrapped" => return Some(builtin_run_hook_wrapped(eval, args)),
        "run-hook-query-error-with-timeout" => {
            return Some(builtin_run_hook_query_error_with_timeout(eval, args))
        }
        "run-mode-hooks" => return Some(builtin_run_mode_hooks(eval, args)),
        "run-window-configuration-change-hook" => {
            return Some(builtin_run_window_configuration_change_hook(eval, args))
        }
        "run-window-scroll-functions" => {
            return Some(builtin_run_window_scroll_functions(eval, args))
        }
        "featurep" => return Some(builtin_featurep(eval, args)),
        // Loading
        "load" => return Some(builtin_load(eval, args)),
        "load-file" => return Some(builtin_load_file(eval, args)),
        "symbol-file" => return Some(super::autoload::builtin_symbol_file_eval(eval, args)),
        "neovm-precompile-file" => return Some(builtin_neovm_precompile_file(eval, args)),
        "eval" => return Some(builtin_eval(eval, args)),
        // Buffer operations
        "get-buffer-create" => return Some(builtin_get_buffer_create(eval, args)),
        "get-buffer" => return Some(builtin_get_buffer(eval, args)),
        "find-buffer" => return Some(builtin_find_buffer(eval, args)),
        "buffer-live-p" => return Some(builtin_buffer_live_p(eval, args)),
        "barf-if-buffer-read-only" => return Some(builtin_barf_if_buffer_read_only(eval, args)),
        "bury-buffer-internal" => return Some(builtin_bury_buffer_internal(eval, args)),
        "get-file-buffer" => return Some(builtin_get_file_buffer(eval, args)),
        "kill-buffer" => return Some(builtin_kill_buffer(eval, args)),
        "set-buffer" => return Some(builtin_set_buffer(eval, args)),
        "current-buffer" => return Some(builtin_current_buffer(eval, args)),
        "buffer-name" => return Some(builtin_buffer_name(eval, args)),
        "buffer-file-name" => return Some(builtin_buffer_file_name(eval, args)),
        "buffer-base-buffer" => return Some(builtin_buffer_base_buffer(eval, args)),
        "buffer-last-name" => return Some(builtin_buffer_last_name(eval, args)),
        "buffer-string" => return Some(builtin_buffer_string(eval, args)),
        "buffer-line-statistics" => return Some(builtin_buffer_line_statistics(eval, args)),
        "buffer-text-pixel-size" => return Some(builtin_buffer_text_pixel_size(eval, args)),
        "base64-encode-region" => {
            return Some(super::fns::builtin_base64_encode_region_eval(eval, args))
        }
        "base64-decode-region" => {
            return Some(super::fns::builtin_base64_decode_region_eval(eval, args))
        }
        "base64url-encode-region" => {
            return Some(super::fns::builtin_base64url_encode_region_eval(eval, args))
        }
        "md5" => return Some(super::fns::builtin_md5_eval(eval, args)),
        "secure-hash" => return Some(super::fns::builtin_secure_hash_eval(eval, args)),
        "buffer-hash" => return Some(super::fns::builtin_buffer_hash_eval(eval, args)),
        "buffer-substring" => return Some(builtin_buffer_substring(eval, args)),
        "compare-buffer-substrings" => return Some(builtin_compare_buffer_substrings(eval, args)),
        "point" => return Some(builtin_point(eval, args)),
        "point-min" => return Some(builtin_point_min(eval, args)),
        "point-max" => return Some(builtin_point_max(eval, args)),
        "goto-char" => return Some(builtin_goto_char(eval, args)),
        "field-beginning" => return Some(builtin_field_beginning(eval, args)),
        "field-end" => return Some(builtin_field_end(eval, args)),
        "field-string" => return Some(builtin_field_string(eval, args)),
        "field-string-no-properties" => {
            return Some(builtin_field_string_no_properties(eval, args))
        }
        "constrain-to-field" => return Some(builtin_constrain_to_field(eval, args)),
        "insert" => return Some(builtin_insert(eval, args)),
        "insert-and-inherit" => return Some(builtin_insert_and_inherit(eval, args)),
        "insert-before-markers-and-inherit" => {
            return Some(builtin_insert_before_markers_and_inherit(eval, args))
        }
        "insert-buffer-substring" => return Some(builtin_insert_buffer_substring(eval, args)),
        "insert-char" => return Some(builtin_insert_char(eval, args)),
        "insert-byte" => return Some(builtin_insert_byte(eval, args)),
        "replace-buffer-contents" => return Some(builtin_replace_buffer_contents_eval(eval, args)),
        "set-buffer-multibyte" => return Some(builtin_set_buffer_multibyte_eval(eval, args)),
        "kill-all-local-variables" => return Some(builtin_kill_all_local_variables(eval, args)),
        "buffer-swap-text" => return Some(builtin_buffer_swap_text(eval, args)),
        "delete-region" => return Some(builtin_delete_region(eval, args)),
        "delete-and-extract-region" => return Some(builtin_delete_and_extract_region(eval, args)),
        "delete-field" => return Some(builtin_delete_field(eval, args)),
        "delete-all-overlays" => return Some(builtin_delete_all_overlays(eval, args)),
        "erase-buffer" => return Some(builtin_erase_buffer(eval, args)),
        "buffer-enable-undo" => return Some(builtin_buffer_enable_undo(eval, args)),
        "buffer-disable-undo" => return Some(builtin_buffer_disable_undo(eval, args)),
        "buffer-size" => return Some(builtin_buffer_size(eval, args)),
        "narrow-to-region" => return Some(builtin_narrow_to_region(eval, args)),
        "widen" => return Some(builtin_widen(eval, args)),
        // set-mark and mark are now in navigation module (below)
        "buffer-modified-p" => return Some(builtin_buffer_modified_p(eval, args)),
        "set-buffer-modified-p" => return Some(builtin_set_buffer_modified_p(eval, args)),
        "buffer-modified-tick" => return Some(builtin_buffer_modified_tick(eval, args)),
        "buffer-chars-modified-tick" => {
            return Some(builtin_buffer_chars_modified_tick(eval, args))
        }
        "buffer-list" => return Some(builtin_buffer_list(eval, args)),
        "other-buffer" => return Some(builtin_other_buffer(eval, args)),
        "generate-new-buffer-name" => return Some(builtin_generate_new_buffer_name(eval, args)),
        "generate-new-buffer" => return Some(builtin_generate_new_buffer(eval, args)),
        "char-after" => return Some(builtin_char_after(eval, args)),
        "char-before" => return Some(builtin_char_before(eval, args)),
        "byte-to-position" => return Some(builtin_byte_to_position(eval, args)),
        "position-bytes" => return Some(builtin_position_bytes(eval, args)),
        "get-byte" => return Some(builtin_get_byte(eval, args)),
        "buffer-local-value" => return Some(builtin_buffer_local_value(eval, args)),
        "ntake" => return Some(builtin_ntake(args)),
        // Search / regex operations
        "search-forward" => return Some(builtin_search_forward(eval, args)),
        "search-backward" => return Some(builtin_search_backward(eval, args)),
        "re-search-forward" => return Some(builtin_re_search_forward(eval, args)),
        "re-search-backward" => return Some(builtin_re_search_backward(eval, args)),
        "search-forward-regexp" => return Some(builtin_search_forward_regexp(eval, args)),
        "search-backward-regexp" => return Some(builtin_search_backward_regexp(eval, args)),
        "isearch-forward" => return Some(super::isearch::builtin_isearch_forward(args)),
        "isearch-backward" => return Some(super::isearch::builtin_isearch_backward(args)),
        "looking-at" => return Some(builtin_looking_at(eval, args)),
        "looking-at-p" => return Some(builtin_looking_at_p(eval, args)),
        "posix-looking-at" => return Some(builtin_posix_looking_at(eval, args)),
        "string-match" => return Some(builtin_string_match_eval(eval, args)),
        "string-match-p" => return Some(builtin_string_match_p_eval(eval, args)),
        "posix-string-match" => return Some(builtin_posix_string_match(eval, args)),
        "match-string" => return Some(builtin_match_string(eval, args)),
        "match-beginning" => return Some(builtin_match_beginning(eval, args)),
        "match-end" => return Some(builtin_match_end(eval, args)),
        "match-data" => return Some(builtin_match_data_eval(eval, args)),
        "set-match-data" => return Some(builtin_set_match_data_eval(eval, args)),
        "replace-match" => return Some(builtin_replace_match(eval, args)),
        "replace-regexp-in-string" => {
            return Some(super::search::builtin_replace_regexp_in_string_eval(
                eval, args,
            ))
        }
        "query-replace" => return Some(super::isearch::builtin_query_replace_eval(eval, args)),
        "query-replace-regexp" => {
            return Some(super::isearch::builtin_query_replace_regexp_eval(
                eval, args,
            ))
        }
        "replace-string" => return Some(super::isearch::builtin_replace_string_eval(eval, args)),
        "replace-regexp" => return Some(super::isearch::builtin_replace_regexp_eval(eval, args)),
        "how-many" => return Some(super::isearch::builtin_how_many_eval(eval, args)),
        "count-matches" => return Some(super::isearch::builtin_count_matches_eval(eval, args)),
        "keep-lines" => return Some(super::isearch::builtin_keep_lines_eval(eval, args)),
        "flush-lines" => return Some(super::isearch::builtin_flush_lines_eval(eval, args)),
        // charset (evaluator-dependent)
        "find-charset-region" => {
            return Some(super::charset::builtin_find_charset_region_eval(eval, args))
        }
        "charset-after" => return Some(super::charset::builtin_charset_after_eval(eval, args)),
        // composite (evaluator-dependent)
        "compose-region-internal" => {
            return Some(super::composite::builtin_compose_region_internal_eval(
                eval, args,
            ))
        }
        // xdisp (evaluator-dependent)
        "format-mode-line" => return Some(super::xdisp::builtin_format_mode_line_eval(eval, args)),
        "window-text-pixel-size" => {
            return Some(super::xdisp::builtin_window_text_pixel_size_eval(
                eval, args,
            ))
        }
        "pos-visible-in-window-p" => {
            return Some(super::xdisp::builtin_pos_visible_in_window_p_eval(
                eval, args,
            ))
        }
        "coordinates-in-window-p" => return Some(builtin_coordinates_in_window_p(eval, args)),
        "tool-bar-height" => return Some(super::xdisp::builtin_tool_bar_height_eval(eval, args)),
        "tab-bar-height" => return Some(super::xdisp::builtin_tab_bar_height_eval(eval, args)),

        // Font (evaluator-dependent — frame designator validation)
        "list-fonts" => return Some(super::font::builtin_list_fonts_eval(eval, args)),
        "find-font" => return Some(super::font::builtin_find_font_eval(eval, args)),
        "font-family-list" => return Some(super::font::builtin_font_family_list_eval(eval, args)),

        // File I/O (evaluator-dependent)
        "access-file" => return Some(super::fileio::builtin_access_file_eval(eval, args)),
        "expand-file-name" => {
            return Some(super::fileio::builtin_expand_file_name_eval(eval, args))
        }
        "file-truename" => return Some(super::fileio::builtin_file_truename_eval(eval, args)),
        "insert-file-contents" => {
            return Some(super::fileio::builtin_insert_file_contents(eval, args))
        }
        "write-region" => return Some(super::fileio::builtin_write_region(eval, args)),
        "delete-file" => return Some(super::fileio::builtin_delete_file_eval(eval, args)),
        "delete-file-internal" => {
            return Some(super::fileio::builtin_delete_file_internal_eval(eval, args))
        }
        "delete-directory" => {
            return Some(super::fileio::builtin_delete_directory_eval(eval, args))
        }
        "delete-directory-internal" => {
            return Some(super::fileio::builtin_delete_directory_internal_eval(
                eval, args,
            ))
        }
        "rename-file" => return Some(super::fileio::builtin_rename_file_eval(eval, args)),
        "copy-file" => return Some(super::fileio::builtin_copy_file_eval(eval, args)),
        "add-name-to-file" => {
            return Some(super::fileio::builtin_add_name_to_file_eval(eval, args))
        }
        "make-symbolic-link" => {
            return Some(super::fileio::builtin_make_symbolic_link_eval(eval, args))
        }
        "make-directory" => return Some(super::fileio::builtin_make_directory_eval(eval, args)),
        "make-directory-internal" => {
            return Some(super::fileio::builtin_make_directory_internal_eval(
                eval, args,
            ))
        }
        "make-temp-file" => return Some(super::fileio::builtin_make_temp_file_eval(eval, args)),
        "make-nearby-temp-file" => {
            return Some(super::fileio::builtin_make_nearby_temp_file_eval(
                eval, args,
            ))
        }
        "find-file-noselect" => return Some(super::fileio::builtin_find_file_noselect(eval, args)),
        "directory-files" => return Some(super::fileio::builtin_directory_files_eval(eval, args)),
        "directory-files-and-attributes" => {
            return Some(super::dired::builtin_directory_files_and_attributes_eval(
                eval, args,
            ))
        }
        "find-file-name-handler" => {
            return Some(super::fileio::builtin_find_file_name_handler_eval(
                eval, args,
            ))
        }
        "file-name-completion" => {
            return Some(super::dired::builtin_file_name_completion_eval(eval, args))
        }
        "file-name-all-completions" => {
            return Some(super::dired::builtin_file_name_all_completions_eval(
                eval, args,
            ))
        }
        "file-attributes" => return Some(super::dired::builtin_file_attributes_eval(eval, args)),
        "file-exists-p" => return Some(super::fileio::builtin_file_exists_p_eval(eval, args)),
        "file-readable-p" => return Some(super::fileio::builtin_file_readable_p_eval(eval, args)),
        "file-writable-p" => return Some(super::fileio::builtin_file_writable_p_eval(eval, args)),
        "file-acl" => return Some(super::fileio::builtin_file_acl_eval(eval, args)),
        "file-accessible-directory-p" => {
            return Some(super::fileio::builtin_file_accessible_directory_p_eval(
                eval, args,
            ))
        }
        "file-executable-p" => {
            return Some(super::fileio::builtin_file_executable_p_eval(eval, args))
        }
        "file-locked-p" => return Some(super::fileio::builtin_file_locked_p_eval(eval, args)),
        "file-selinux-context" => {
            return Some(super::fileio::builtin_file_selinux_context_eval(eval, args))
        }
        "file-system-info" => {
            return Some(super::fileio::builtin_file_system_info_eval(eval, args))
        }
        "file-directory-p" => {
            return Some(super::fileio::builtin_file_directory_p_eval(eval, args))
        }
        "file-regular-p" => return Some(super::fileio::builtin_file_regular_p_eval(eval, args)),
        "file-symlink-p" => return Some(super::fileio::builtin_file_symlink_p_eval(eval, args)),
        "file-name-case-insensitive-p" => {
            return Some(super::fileio::builtin_file_name_case_insensitive_p_eval(
                eval, args,
            ))
        }
        "file-newer-than-file-p" => {
            return Some(super::fileio::builtin_file_newer_than_file_p_eval(
                eval, args,
            ))
        }
        "file-equal-p" => return Some(super::fileio::builtin_file_equal_p_eval(eval, args)),
        "file-in-directory-p" => {
            return Some(super::fileio::builtin_file_in_directory_p_eval(eval, args))
        }
        "file-modes" => return Some(super::fileio::builtin_file_modes_eval(eval, args)),
        "set-file-modes" => return Some(super::fileio::builtin_set_file_modes_eval(eval, args)),
        "set-file-times" => return Some(super::fileio::builtin_set_file_times_eval(eval, args)),
        "verify-visited-file-modtime" => {
            return Some(super::fileio::builtin_verify_visited_file_modtime(
                eval, args,
            ))
        }
        "set-visited-file-modtime" => {
            return Some(super::fileio::builtin_set_visited_file_modtime(eval, args))
        }
        "default-file-modes" => return Some(super::fileio::builtin_default_file_modes(args)),
        "set-default-file-modes" => {
            return Some(super::fileio::builtin_set_default_file_modes(args))
        }
        // Keymap operations
        "make-keymap" => return Some(builtin_make_keymap(eval, args)),
        "make-sparse-keymap" => return Some(builtin_make_sparse_keymap(eval, args)),
        "copy-keymap" => return Some(builtin_copy_keymap(eval, args)),
        "define-key" => return Some(builtin_define_key(eval, args)),
        "lookup-key" => return Some(builtin_lookup_key(eval, args)),
        "global-set-key" => return Some(builtin_global_set_key(eval, args)),
        "local-set-key" => return Some(builtin_local_set_key(eval, args)),
        "use-local-map" => return Some(builtin_use_local_map(eval, args)),
        "use-global-map" => return Some(builtin_use_global_map(eval, args)),
        "current-local-map" => return Some(builtin_current_local_map(eval, args)),
        "current-global-map" => return Some(builtin_current_global_map(eval, args)),
        "current-active-maps" => return Some(builtin_current_active_maps(eval, args)),
        "current-minor-mode-maps" => return Some(builtin_current_minor_mode_maps(args)),
        "keymap-parent" => return Some(builtin_keymap_parent(eval, args)),
        "set-keymap-parent" => return Some(builtin_set_keymap_parent(eval, args)),
        "keymapp" => return Some(builtin_keymapp(eval, args)),
        "accessible-keymaps" => return Some(builtin_accessible_keymaps(eval, args)),
        // Process operations (evaluator-dependent)
        "backquote-delay-process" => {
            return Some(super::process::builtin_backquote_delay_process(eval, args))
        }
        "backquote-process" => return Some(super::process::builtin_backquote_process(eval, args)),
        "clone-process" => return Some(super::process::builtin_clone_process(eval, args)),
        "internal-default-interrupt-process" => {
            return Some(super::process::builtin_internal_default_interrupt_process(
                eval, args,
            ))
        }
        "internal-default-process-filter" => {
            return Some(super::process::builtin_internal_default_process_filter(
                eval, args,
            ))
        }
        "internal-default-process-sentinel" => {
            return Some(super::process::builtin_internal_default_process_sentinel(
                eval, args,
            ))
        }
        "internal-default-signal-process" => {
            return Some(super::process::builtin_internal_default_signal_process(
                eval, args,
            ))
        }
        "isearch-process-search-char" => {
            return Some(super::process::builtin_isearch_process_search_char(
                eval, args,
            ))
        }
        "isearch-process-search-string" => {
            return Some(super::process::builtin_isearch_process_search_string(
                eval, args,
            ))
        }
        "minibuffer--sort-preprocess-history" => {
            return Some(super::process::builtin_minibuffer_sort_preprocess_history(
                eval, args,
            ))
        }
        "print--preprocess" => return Some(super::process::builtin_print_preprocess(eval, args)),
        "syntax-propertize--in-process-p" => {
            return Some(super::process::builtin_syntax_propertize_in_process_p(
                eval, args,
            ))
        }
        "tooltip-process-prompt-regexp" => {
            return Some(super::process::builtin_tooltip_process_prompt_regexp(
                eval, args,
            ))
        }
        "window--adjust-process-windows" => {
            return Some(super::process::builtin_window_adjust_process_windows(
                eval, args,
            ))
        }
        "window--process-window-list" => {
            return Some(super::process::builtin_window_process_window_list(
                eval, args,
            ))
        }
        "window-adjust-process-window-size" => {
            return Some(super::process::builtin_window_adjust_process_window_size(
                eval, args,
            ))
        }
        "window-adjust-process-window-size-largest" => {
            return Some(
                super::process::builtin_window_adjust_process_window_size_largest(eval, args),
            )
        }
        "window-adjust-process-window-size-smallest" => {
            return Some(
                super::process::builtin_window_adjust_process_window_size_smallest(eval, args),
            )
        }
        "format-network-address" => {
            return Some(super::process::builtin_format_network_address(eval, args))
        }
        "network-interface-list" => {
            return Some(super::process::builtin_network_interface_list(eval, args))
        }
        "network-interface-info" => {
            return Some(super::process::builtin_network_interface_info(eval, args))
        }
        "network-lookup-address-info" => {
            return Some(super::process::builtin_network_lookup_address_info(
                eval, args,
            ))
        }
        "signal-names" => return Some(super::process::builtin_signal_names(eval, args)),
        "accept-process-output" => {
            return Some(super::process::builtin_accept_process_output(eval, args))
        }
        "list-system-processes" => {
            return Some(super::process::builtin_list_system_processes(eval, args))
        }
        "num-processors" => return Some(super::process::builtin_num_processors(eval, args)),
        "list-processes" => return Some(super::process::builtin_list_processes(eval, args)),
        "list-processes--refresh" => {
            return Some(super::process::builtin_list_processes_refresh(eval, args))
        }
        "make-process" => return Some(super::process::builtin_make_process(eval, args)),
        "make-network-process" => {
            return Some(super::process::builtin_make_network_process(eval, args))
        }
        "make-pipe-process" => return Some(super::process::builtin_make_pipe_process(eval, args)),
        "make-serial-process" => {
            return Some(super::process::builtin_make_serial_process(eval, args))
        }
        "serial-process-configure" => {
            return Some(super::process::builtin_serial_process_configure(eval, args))
        }
        "set-network-process-option" => {
            return Some(super::process::builtin_set_network_process_option(
                eval, args,
            ))
        }
        "start-process" => return Some(super::process::builtin_start_process(eval, args)),
        "start-process-shell-command" => {
            return Some(super::process::builtin_start_process_shell_command(
                eval, args,
            ))
        }
        "start-file-process" => {
            return Some(super::process::builtin_start_file_process(eval, args))
        }
        "start-file-process-shell-command" => {
            return Some(super::process::builtin_start_file_process_shell_command(
                eval, args,
            ))
        }
        "call-process" => return Some(super::process::builtin_call_process(eval, args)),
        "call-process-shell-command" => {
            return Some(super::process::builtin_call_process_shell_command(
                eval, args,
            ))
        }
        "process-file" => return Some(super::process::builtin_process_file(eval, args)),
        "process-file-shell-command" => {
            return Some(super::process::builtin_process_file_shell_command(
                eval, args,
            ))
        }
        "call-process-region" => {
            return Some(super::process::builtin_call_process_region(eval, args))
        }
        "continue-process" => return Some(super::process::builtin_continue_process(eval, args)),
        "delete-process" => return Some(super::process::builtin_delete_process(eval, args)),
        "interrupt-process" => return Some(super::process::builtin_interrupt_process(eval, args)),
        "kill-process" => return Some(super::process::builtin_kill_process(eval, args)),
        "quit-process" => return Some(super::process::builtin_quit_process(eval, args)),
        "signal-process" => return Some(super::process::builtin_signal_process(eval, args)),
        "stop-process" => return Some(super::process::builtin_stop_process(eval, args)),
        "get-process" => return Some(super::process::builtin_get_process(eval, args)),
        "get-buffer-process" => {
            return Some(super::process::builtin_get_buffer_process(eval, args))
        }
        "process-attributes" => {
            return Some(super::process::builtin_process_attributes(eval, args))
        }
        "process-live-p" => return Some(super::process::builtin_process_live_p(eval, args)),
        "processp" => return Some(super::process::builtin_processp(eval, args)),
        "process-id" => return Some(super::process::builtin_process_id(eval, args)),
        "process-query-on-exit-flag" => {
            return Some(super::process::builtin_process_query_on_exit_flag(
                eval, args,
            ))
        }
        "set-process-query-on-exit-flag" => {
            return Some(super::process::builtin_set_process_query_on_exit_flag(
                eval, args,
            ))
        }
        "process-command" => return Some(super::process::builtin_process_command(eval, args)),
        "process-contact" => return Some(super::process::builtin_process_contact(eval, args)),
        "process-filter" => return Some(super::process::builtin_process_filter(eval, args)),
        "set-process-filter" => {
            return Some(super::process::builtin_set_process_filter(eval, args))
        }
        "process-sentinel" => return Some(super::process::builtin_process_sentinel(eval, args)),
        "set-process-sentinel" => {
            return Some(super::process::builtin_set_process_sentinel(eval, args))
        }
        "process-coding-system" => {
            return Some(super::process::builtin_process_coding_system(eval, args))
        }
        "process-datagram-address" => {
            return Some(super::process::builtin_process_datagram_address(eval, args))
        }
        "process-inherit-coding-system-flag" => {
            return Some(super::process::builtin_process_inherit_coding_system_flag(
                eval, args,
            ))
        }
        "set-process-buffer" => {
            return Some(super::process::builtin_set_process_buffer(eval, args))
        }
        "set-process-coding-system" => {
            return Some(super::process::builtin_set_process_coding_system(
                eval, args,
            ))
        }
        "set-process-datagram-address" => {
            return Some(super::process::builtin_set_process_datagram_address(
                eval, args,
            ))
        }
        "set-process-inherit-coding-system-flag" => {
            return Some(super::process::builtin_set_process_inherit_coding_system_flag(eval, args))
        }
        "set-process-thread" => {
            return Some(super::process::builtin_set_process_thread(eval, args))
        }
        "set-process-window-size" => {
            return Some(super::process::builtin_set_process_window_size(eval, args))
        }
        "set-buffer-process-coding-system" => {
            return Some(super::process::builtin_set_buffer_process_coding_system(
                eval, args,
            ))
        }
        "process-lines" => return Some(super::process::builtin_process_lines(eval, args)),
        "process-lines-ignore-status" => {
            return Some(super::process::builtin_process_lines_ignore_status(
                eval, args,
            ))
        }
        "process-lines-handling-status" => {
            return Some(super::process::builtin_process_lines_handling_status(
                eval, args,
            ))
        }
        "process-kill-buffer-query-function" => {
            return Some(super::process::builtin_process_kill_buffer_query_function(
                args,
            ))
        }
        "process-menu-delete-process" => {
            return Some(super::process::builtin_process_menu_delete_process(
                eval, args,
            ))
        }
        "process-menu-visit-buffer" => {
            return Some(super::process::builtin_process_menu_visit_buffer(
                eval, args,
            ))
        }
        "process-menu-mode" => return Some(super::process::builtin_process_menu_mode(args)),
        "process-tty-name" => return Some(super::process::builtin_process_tty_name(eval, args)),
        "process-plist" => return Some(super::process::builtin_process_plist(eval, args)),
        "set-process-plist" => return Some(super::process::builtin_set_process_plist(eval, args)),
        "process-put" => return Some(super::process::builtin_process_put(eval, args)),
        "process-get" => return Some(super::process::builtin_process_get(eval, args)),
        "process-mark" => return Some(super::process::builtin_process_mark(eval, args)),
        "process-type" => return Some(super::process::builtin_process_type(eval, args)),
        "process-thread" => return Some(super::process::builtin_process_thread(eval, args)),
        "process-running-child-p" => {
            return Some(super::process::builtin_process_running_child_p(eval, args))
        }
        "process-send-region" => {
            return Some(super::process::builtin_process_send_region(eval, args))
        }
        "process-send-eof" => return Some(super::process::builtin_process_send_eof(eval, args)),
        "process-send-string" => {
            return Some(super::process::builtin_process_send_string(eval, args))
        }
        "process-status" => return Some(super::process::builtin_process_status(eval, args)),
        "process-exit-status" => {
            return Some(super::process::builtin_process_exit_status(eval, args))
        }
        "process-list" => return Some(super::process::builtin_process_list(eval, args)),
        "process-name" => return Some(super::process::builtin_process_name(eval, args)),
        "process-buffer" => return Some(super::process::builtin_process_buffer(eval, args)),
        // Timer operations (evaluator-dependent)
        "add-timeout" => return Some(super::timer::builtin_add_timeout(eval, args)),
        "run-at-time" => return Some(super::timer::builtin_run_at_time(eval, args)),
        "run-with-timer" => return Some(super::timer::builtin_run_with_timer(eval, args)),
        "run-with-idle-timer" => {
            return Some(super::timer::builtin_run_with_idle_timer(eval, args))
        }
        "cancel-timer" => return Some(super::timer::builtin_cancel_timer(eval, args)),
        "timer-activate" => return Some(super::timer::builtin_timer_activate(eval, args)),
        "sleep-for" => return Some(super::timer::builtin_sleep_for(args)),
        // Advice system
        "advice-add" => return Some(super::advice::builtin_advice_add(eval, args)),
        "advice-remove" => return Some(super::advice::builtin_advice_remove(eval, args)),
        "advice-member-p" => return Some(super::advice::builtin_advice_member_p(eval, args)),
        // Variable watchers
        "add-variable-watcher" => {
            return Some(super::advice::builtin_add_variable_watcher(eval, args))
        }
        "remove-variable-watcher" => {
            return Some(super::advice::builtin_remove_variable_watcher(eval, args))
        }
        // Syntax table operations (evaluator-dependent)
        "modify-syntax-entry" => {
            return Some(super::syntax::builtin_modify_syntax_entry(eval, args))
        }
        "syntax-table" => return Some(super::syntax::builtin_syntax_table(eval, args)),
        "set-syntax-table" => return Some(super::syntax::builtin_set_syntax_table(eval, args)),
        "char-syntax" => return Some(super::syntax::builtin_char_syntax(eval, args)),
        "syntax-after" => return Some(super::syntax::builtin_syntax_after(eval, args)),
        "forward-comment" => return Some(super::syntax::builtin_forward_comment(eval, args)),
        "backward-prefix-chars" => {
            return Some(super::syntax::builtin_backward_prefix_chars(eval, args))
        }
        "forward-word" => return Some(super::syntax::builtin_forward_word(eval, args)),
        "backward-word" => return Some(super::syntax::builtin_backward_word(eval, args)),
        "forward-sexp" => return Some(super::syntax::builtin_forward_sexp(eval, args)),
        "backward-sexp" => return Some(super::syntax::builtin_backward_sexp(eval, args)),
        "scan-lists" => return Some(super::syntax::builtin_scan_lists(eval, args)),
        "scan-sexps" => return Some(super::syntax::builtin_scan_sexps(eval, args)),
        "parse-partial-sexp" => return Some(super::syntax::builtin_parse_partial_sexp(eval, args)),
        "syntax-ppss" => return Some(super::syntax::builtin_syntax_ppss(eval, args)),
        "syntax-ppss-flush-cache" => {
            return Some(super::syntax::builtin_syntax_ppss_flush_cache(eval, args))
        }
        "skip-syntax-forward" => {
            return Some(super::syntax::builtin_skip_syntax_forward(eval, args))
        }
        "skip-syntax-backward" => {
            return Some(super::syntax::builtin_skip_syntax_backward(eval, args))
        }
        // Register operations (evaluator-dependent)
        "copy-to-register" => return Some(super::register::builtin_copy_to_register(eval, args)),
        "insert-register" => return Some(super::register::builtin_insert_register(eval, args)),
        "point-to-register" => return Some(super::register::builtin_point_to_register(eval, args)),
        "number-to-register" => {
            return Some(super::register::builtin_number_to_register(eval, args))
        }
        "increment-register" => {
            return Some(super::register::builtin_increment_register(eval, args))
        }
        "view-register" => return Some(super::register::builtin_view_register(eval, args)),
        "get-register" => return Some(super::register::builtin_get_register(eval, args)),
        "set-register" => return Some(super::register::builtin_set_register(eval, args)),
        // Keyboard macro operations (evaluator-dependent)
        "cancel-kbd-macro-events" => return Some(builtin_cancel_kbd_macro_events(args)),
        "defining-kbd-macro" => return Some(super::kmacro::builtin_defining_kbd_macro(eval, args)),
        "start-kbd-macro" => return Some(super::kmacro::builtin_start_kbd_macro(eval, args)),
        "end-kbd-macro" => return Some(super::kmacro::builtin_end_kbd_macro(eval, args)),
        "call-last-kbd-macro" => {
            return Some(super::kmacro::builtin_call_last_kbd_macro(eval, args))
        }
        "execute-kbd-macro" => return Some(super::kmacro::builtin_execute_kbd_macro(eval, args)),
        "name-last-kbd-macro" => {
            return Some(super::kmacro::builtin_name_last_kbd_macro(eval, args))
        }
        "kmacro-name-last-macro" => {
            eval.obarray_mut().put_property(
                "kmacro-name-last-macro",
                "neovm--kmacro-autoload-promoted",
                Value::True,
            );
            return Some(super::kmacro::builtin_kmacro_name_last_macro(eval, args));
        }
        "insert-kbd-macro" => return Some(super::kmacro::builtin_insert_kbd_macro(eval, args)),
        "kbd-macro-query" => return Some(super::kmacro::builtin_kbd_macro_query(eval, args)),
        "store-kbd-macro-event" => {
            return Some(super::kmacro::builtin_store_kbd_macro_event(eval, args))
        }
        // Bookmark operations (evaluator-dependent)
        "bookmark-set" => return Some(super::bookmark::builtin_bookmark_set(eval, args)),
        "bookmark-jump" => return Some(super::bookmark::builtin_bookmark_jump(eval, args)),
        "bookmark-delete" => return Some(super::bookmark::builtin_bookmark_delete(eval, args)),
        "bookmark-rename" => return Some(super::bookmark::builtin_bookmark_rename(eval, args)),
        "bookmark-save" => return Some(super::bookmark::builtin_bookmark_save(eval, args)),
        "bookmark-load" => return Some(super::bookmark::builtin_bookmark_load(eval, args)),
        // Abbreviation operations (evaluator-dependent)
        "define-abbrev" => return Some(super::abbrev::builtin_define_abbrev(eval, args)),
        "expand-abbrev" => return Some(super::abbrev::builtin_expand_abbrev(eval, args)),
        "abbrev-mode" => return Some(super::abbrev::builtin_abbrev_mode(eval, args)),
        "define-abbrev-table" => {
            return Some(super::abbrev::builtin_define_abbrev_table(eval, args))
        }
        "clear-abbrev-table" => return Some(super::abbrev::builtin_clear_abbrev_table(eval, args)),
        "abbrev-expansion" => return Some(super::abbrev::builtin_abbrev_expansion(eval, args)),
        "insert-abbrev-table-description" => {
            return Some(super::abbrev::builtin_insert_abbrev_table_description(
                eval, args,
            ))
        }
        "abbrev-table-p" => return Some(super::abbrev::builtin_abbrev_table_p(eval, args)),

        // Text property operations (evaluator-dependent — buffer access)
        "put-text-property" => return Some(super::textprop::builtin_put_text_property(eval, args)),
        "get-text-property" => return Some(super::textprop::builtin_get_text_property(eval, args)),
        "get-char-property" => return Some(super::textprop::builtin_get_char_property(eval, args)),
        "get-pos-property" => return Some(builtin_get_pos_property(eval, args)),
        "add-face-text-property" => {
            return Some(super::textprop::builtin_add_face_text_property(eval, args))
        }
        "add-text-properties" => {
            return Some(super::textprop::builtin_add_text_properties(eval, args))
        }
        "set-text-properties" => {
            return Some(super::textprop::builtin_set_text_properties(eval, args))
        }
        "remove-text-properties" => {
            return Some(super::textprop::builtin_remove_text_properties(eval, args))
        }
        "remove-list-of-text-properties" => {
            return Some(super::textprop::builtin_remove_list_of_text_properties(
                eval, args,
            ))
        }
        "text-properties-at" => {
            return Some(super::textprop::builtin_text_properties_at(eval, args))
        }
        "get-char-property-and-overlay" => {
            return Some(super::textprop::builtin_get_char_property_and_overlay(
                eval, args,
            ))
        }
        "get-display-property" => {
            return Some(super::textprop::builtin_get_display_property(eval, args))
        }
        "next-single-property-change" => {
            return Some(super::textprop::builtin_next_single_property_change(
                eval, args,
            ))
        }
        "next-single-char-property-change" => {
            return Some(builtin_next_single_char_property_change(eval, args))
        }
        "previous-single-property-change" => {
            return Some(super::textprop::builtin_previous_single_property_change(
                eval, args,
            ))
        }
        "previous-single-char-property-change" => {
            return Some(builtin_previous_single_char_property_change(eval, args))
        }
        "next-property-change" => {
            return Some(super::textprop::builtin_next_property_change(eval, args))
        }
        "next-char-property-change" => return Some(builtin_next_char_property_change(eval, args)),
        "previous-property-change" => return Some(builtin_previous_property_change(eval, args)),
        "previous-char-property-change" => {
            return Some(builtin_previous_char_property_change(eval, args))
        }
        "text-property-any" => return Some(super::textprop::builtin_text_property_any(eval, args)),
        "text-property-not-all" => {
            return Some(super::textprop::builtin_text_property_not_all(eval, args))
        }
        "next-overlay-change" => {
            return Some(super::textprop::builtin_next_overlay_change(eval, args))
        }
        "previous-overlay-change" => {
            return Some(super::textprop::builtin_previous_overlay_change(eval, args))
        }
        "make-overlay" => return Some(super::textprop::builtin_make_overlay(eval, args)),
        "delete-overlay" => return Some(super::textprop::builtin_delete_overlay(eval, args)),
        "overlay-put" => return Some(super::textprop::builtin_overlay_put(eval, args)),
        "overlay-get" => return Some(super::textprop::builtin_overlay_get(eval, args)),
        "overlays-at" => return Some(super::textprop::builtin_overlays_at(eval, args)),
        "overlays-in" => return Some(super::textprop::builtin_overlays_in(eval, args)),
        "move-overlay" => return Some(super::textprop::builtin_move_overlay(eval, args)),
        "overlay-start" => return Some(super::textprop::builtin_overlay_start(eval, args)),
        "overlay-end" => return Some(super::textprop::builtin_overlay_end(eval, args)),
        "overlay-buffer" => return Some(super::textprop::builtin_overlay_buffer(eval, args)),
        "overlay-properties" => {
            return Some(super::textprop::builtin_overlay_properties(eval, args))
        }
        "remove-overlays" => return Some(super::textprop::builtin_remove_overlays(eval, args)),
        "overlayp" => return Some(super::textprop::builtin_overlayp(eval, args)),

        // Navigation / mark / region (evaluator-dependent — buffer access)
        "bobp" => return Some(super::navigation::builtin_bobp(eval, args)),
        "eobp" => return Some(super::navigation::builtin_eobp(eval, args)),
        "bolp" => return Some(super::navigation::builtin_bolp(eval, args)),
        "eolp" => return Some(super::navigation::builtin_eolp(eval, args)),
        "line-beginning-position" => {
            return Some(super::navigation::builtin_line_beginning_position(
                eval, args,
            ))
        }
        "pos-bol" => return Some(builtin_pos_bol(eval, args)),
        "line-end-position" => {
            return Some(super::navigation::builtin_line_end_position(eval, args))
        }
        "pos-eol" => return Some(builtin_pos_eol(eval, args)),
        "line-number-at-pos" => {
            return Some(super::navigation::builtin_line_number_at_pos(eval, args))
        }
        "count-lines" => return Some(super::navigation::builtin_count_lines(eval, args)),
        "forward-line" => return Some(super::navigation::builtin_forward_line(eval, args)),
        "next-line" => return Some(super::navigation::builtin_next_line(eval, args)),
        "previous-line" => return Some(super::navigation::builtin_previous_line(eval, args)),
        "beginning-of-line" => {
            return Some(super::navigation::builtin_beginning_of_line(eval, args))
        }
        "beginning-of-buffer" => {
            return Some(super::navigation::builtin_beginning_of_buffer(eval, args))
        }
        "move-beginning-of-line" => {
            return Some(super::navigation::builtin_beginning_of_line(eval, args))
        }
        "end-of-line" => return Some(super::navigation::builtin_end_of_line(eval, args)),
        "end-of-buffer" => return Some(super::navigation::builtin_end_of_buffer(eval, args)),
        "move-end-of-line" => return Some(super::navigation::builtin_end_of_line(eval, args)),
        "goto-line" => return Some(super::navigation::builtin_goto_line(eval, args)),
        "forward-char" => return Some(super::navigation::builtin_forward_char(eval, args)),
        "backward-char" => return Some(super::navigation::builtin_backward_char(eval, args)),
        "skip-chars-forward" => {
            return Some(super::navigation::builtin_skip_chars_forward(eval, args))
        }
        "skip-chars-backward" => {
            return Some(super::navigation::builtin_skip_chars_backward(eval, args))
        }
        "push-mark" => return Some(super::navigation::builtin_push_mark(eval, args)),
        "pop-mark" => return Some(super::navigation::builtin_pop_mark(eval, args)),
        "set-mark" => return Some(super::navigation::builtin_set_mark_nav(eval, args)),
        "mark" => return Some(super::navigation::builtin_mark_nav(eval, args)),
        "mark-marker" => return Some(super::marker::builtin_mark_marker(eval, args)),
        "region-beginning" => return Some(super::navigation::builtin_region_beginning(eval, args)),
        "region-end" => return Some(super::navigation::builtin_region_end(eval, args)),
        "use-region-p" => return Some(super::navigation::builtin_use_region_p(eval, args)),
        "deactivate-mark" => return Some(super::navigation::builtin_deactivate_mark(eval, args)),
        "activate-mark" => return Some(super::navigation::builtin_activate_mark(eval, args)),
        "exchange-point-and-mark" => {
            return Some(super::navigation::builtin_exchange_point_and_mark(
                eval, args,
            ))
        }
        "transient-mark-mode" => {
            return Some(super::navigation::builtin_transient_mark_mode(eval, args))
        }

        // Custom system (evaluator-dependent)
        "custom-variable-p" => return Some(super::custom::builtin_custom_variable_p(eval, args)),
        "custom-set-variables" => {
            return Some(super::custom::builtin_custom_set_variables(eval, args))
        }
        "make-variable-buffer-local" => {
            return Some(super::custom::builtin_make_variable_buffer_local(
                eval, args,
            ))
        }
        "make-local-variable" => {
            return Some(super::custom::builtin_make_local_variable(eval, args))
        }
        "local-variable-p" => return Some(super::custom::builtin_local_variable_p(eval, args)),
        "buffer-local-variables" => {
            return Some(super::custom::builtin_buffer_local_variables(eval, args))
        }
        "kill-local-variable" => {
            return Some(super::custom::builtin_kill_local_variable(eval, args))
        }
        "default-value" => return Some(super::custom::builtin_default_value(eval, args)),
        "set-default" => return Some(super::custom::builtin_set_default(eval, args)),
        "set-default-toplevel-value" => {
            return Some(builtin_set_default_toplevel_value(eval, args))
        }

        // Autoload (evaluator-dependent)
        "autoload" => return Some(super::autoload::builtin_autoload(eval, args)),
        "autoload-do-load" => return Some(super::autoload::builtin_autoload_do_load(eval, args)),

        // Kill ring / text editing (evaluator-dependent — buffer access)
        "kill-new" => return Some(super::kill_ring::builtin_kill_new(eval, args)),
        "kill-append" => return Some(super::kill_ring::builtin_kill_append(eval, args)),
        "current-kill" => return Some(super::kill_ring::builtin_current_kill(eval, args)),
        "kill-region" => return Some(super::kill_ring::builtin_kill_region(eval, args)),
        "kill-ring-save" => return Some(super::kill_ring::builtin_kill_ring_save(eval, args)),
        "copy-region-as-kill" => {
            return Some(super::kill_ring::builtin_copy_region_as_kill(eval, args))
        }
        "kill-line" => return Some(super::kill_ring::builtin_kill_line(eval, args)),
        "kill-whole-line" => return Some(super::kill_ring::builtin_kill_whole_line(eval, args)),
        "kill-word" => return Some(super::kill_ring::builtin_kill_word(eval, args)),
        "backward-kill-word" => {
            return Some(super::kill_ring::builtin_backward_kill_word(eval, args))
        }
        "yank" => return Some(super::kill_ring::builtin_yank(eval, args)),
        "yank-pop" => return Some(super::kill_ring::builtin_yank_pop(eval, args)),
        "downcase-region" => return Some(super::kill_ring::builtin_downcase_region(eval, args)),
        "upcase-region" => return Some(super::kill_ring::builtin_upcase_region(eval, args)),
        "capitalize-region" => {
            return Some(super::kill_ring::builtin_capitalize_region(eval, args))
        }
        "downcase-word" => return Some(super::kill_ring::builtin_downcase_word(eval, args)),
        "upcase-word" => return Some(super::kill_ring::builtin_upcase_word(eval, args)),
        "capitalize-word" => return Some(super::kill_ring::builtin_capitalize_word(eval, args)),
        "transpose-chars" => return Some(super::kill_ring::builtin_transpose_chars(eval, args)),
        "transpose-sexps" => return Some(super::kill_ring::builtin_transpose_sexps(eval, args)),
        "transpose-sentences" => {
            return Some(super::kill_ring::builtin_transpose_sentences(eval, args))
        }
        "transpose-paragraphs" => {
            return Some(super::kill_ring::builtin_transpose_paragraphs(eval, args))
        }
        "transpose-words" => return Some(super::kill_ring::builtin_transpose_words(eval, args)),
        "transpose-lines" => return Some(super::kill_ring::builtin_transpose_lines(eval, args)),
        "indent-line-to" => return Some(super::kill_ring::builtin_indent_line_to(eval, args)),
        "indent-to" => return Some(super::kill_ring::builtin_indent_to(eval, args)),
        "newline" => return Some(super::kill_ring::builtin_newline(eval, args)),
        "newline-and-indent" => {
            return Some(super::kill_ring::builtin_newline_and_indent(eval, args))
        }
        "open-line" => return Some(super::kill_ring::builtin_open_line(eval, args)),
        "delete-horizontal-space" => {
            return Some(super::kill_ring::builtin_delete_horizontal_space(
                eval, args,
            ))
        }
        "just-one-space" => return Some(super::kill_ring::builtin_just_one_space(eval, args)),
        "delete-indentation" => {
            return Some(super::kill_ring::builtin_delete_indentation(eval, args))
        }
        "tab-to-tab-stop" => return Some(super::kill_ring::builtin_tab_to_tab_stop(eval, args)),
        "indent-rigidly" => return Some(super::kill_ring::builtin_indent_rigidly(eval, args)),

        // Rectangle operations (evaluator-dependent — buffer access)
        "extract-rectangle" => return Some(super::rect::builtin_extract_rectangle(eval, args)),
        "delete-rectangle" => return Some(super::rect::builtin_delete_rectangle(eval, args)),
        "kill-rectangle" => return Some(super::rect::builtin_kill_rectangle(eval, args)),
        "yank-rectangle" => return Some(super::rect::builtin_yank_rectangle(eval, args)),
        "insert-rectangle" => return Some(super::rect::builtin_insert_rectangle(eval, args)),
        "open-rectangle" => return Some(super::rect::builtin_open_rectangle(eval, args)),
        "clear-rectangle" => return Some(super::rect::builtin_clear_rectangle(eval, args)),
        "string-rectangle" => return Some(super::rect::builtin_string_rectangle(eval, args)),
        "delete-extract-rectangle" => {
            return Some(super::rect::builtin_delete_extract_rectangle(eval, args))
        }
        "replace-rectangle" => return Some(super::rect::builtin_replace_rectangle(eval, args)),

        // Window/frame operations (evaluator-dependent)
        "selected-window" => return Some(super::window_cmds::builtin_selected_window(eval, args)),
        "active-minibuffer-window" => {
            return Some(super::window_cmds::builtin_active_minibuffer_window(args))
        }
        "minibuffer-window" => {
            return Some(super::window_cmds::builtin_minibuffer_window(eval, args))
        }
        "minibuffer-selected-window" => {
            return Some(super::window_cmds::builtin_minibuffer_selected_window(args))
        }
        "minibuffer-window-active-p" => {
            return Some(super::window_cmds::builtin_minibuffer_window_active_p(args))
        }
        "window-parameter" => {
            return Some(super::window_cmds::builtin_window_parameter(eval, args))
        }
        "set-window-parameter" => {
            return Some(super::window_cmds::builtin_set_window_parameter(eval, args))
        }
        "window-parameters" => {
            return Some(super::window_cmds::builtin_window_parameters(eval, args))
        }
        "window-display-table" => {
            return Some(super::window_cmds::builtin_window_display_table(eval, args))
        }
        "window-size-fixed-p" => {
            return Some(super::window_cmds::builtin_window_size_fixed_p(eval, args))
        }
        "window-preserve-size" => {
            return Some(super::window_cmds::builtin_window_preserve_size(eval, args))
        }
        "window-resizable" => {
            return Some(super::window_cmds::builtin_window_resizable(eval, args))
        }
        "window-cursor-type" => {
            return Some(super::window_cmds::builtin_window_cursor_type(eval, args))
        }
        "window-buffer" => return Some(super::window_cmds::builtin_window_buffer(eval, args)),
        "window-start" => return Some(super::window_cmds::builtin_window_start(eval, args)),
        "window-group-start" => {
            return Some(super::window_cmds::builtin_window_group_start(eval, args))
        }
        "window-end" => return Some(super::window_cmds::builtin_window_end(eval, args)),
        "window-point" => return Some(super::window_cmds::builtin_window_point(eval, args)),
        "window-height" => return Some(super::window_cmds::builtin_window_height(eval, args)),
        "window-width" => return Some(super::window_cmds::builtin_window_width(eval, args)),
        "window-use-time" => return Some(super::window_cmds::builtin_window_use_time(eval, args)),
        "window-old-point" => {
            return Some(super::window_cmds::builtin_window_old_point(eval, args))
        }
        "window-old-buffer" => {
            return Some(super::window_cmds::builtin_window_old_buffer(eval, args))
        }
        "window-prev-buffers" => {
            return Some(super::window_cmds::builtin_window_prev_buffers(eval, args))
        }
        "window-next-buffers" => {
            return Some(super::window_cmds::builtin_window_next_buffers(eval, args))
        }
        "window-left-column" => {
            return Some(super::window_cmds::builtin_window_left_column(eval, args))
        }
        "window-top-line" => return Some(super::window_cmds::builtin_window_top_line(eval, args)),
        "window-hscroll" => return Some(super::window_cmds::builtin_window_hscroll(eval, args)),
        "window-vscroll" => return Some(super::window_cmds::builtin_window_vscroll(eval, args)),
        "window-margins" => return Some(super::window_cmds::builtin_window_margins(eval, args)),
        "window-fringes" => return Some(super::window_cmds::builtin_window_fringes(eval, args)),
        "window-scroll-bars" => {
            return Some(super::window_cmds::builtin_window_scroll_bars(eval, args))
        }
        "window-mode-line-height" => {
            return Some(super::window_cmds::builtin_window_mode_line_height(
                eval, args,
            ))
        }
        "window-header-line-height" => {
            return Some(super::window_cmds::builtin_window_header_line_height(
                eval, args,
            ))
        }
        "window-pixel-height" => {
            return Some(super::window_cmds::builtin_window_pixel_height(eval, args))
        }
        "window-pixel-width" => {
            return Some(super::window_cmds::builtin_window_pixel_width(eval, args))
        }
        "window-body-height" => {
            return Some(super::window_cmds::builtin_window_body_height(eval, args))
        }
        "window-body-width" => {
            return Some(super::window_cmds::builtin_window_body_width(eval, args))
        }
        "window-text-height" => {
            return Some(super::window_cmds::builtin_window_text_height(eval, args))
        }
        "window-text-width" => {
            return Some(super::window_cmds::builtin_window_text_width(eval, args))
        }
        "window-body-pixel-edges" => {
            return Some(super::window_cmds::builtin_window_body_pixel_edges(
                eval, args,
            ))
        }
        "window-body-edges" => {
            return Some(super::window_cmds::builtin_window_body_edges(eval, args))
        }
        "window-pixel-edges" => {
            return Some(super::window_cmds::builtin_window_pixel_edges(eval, args))
        }
        "window-edges" => return Some(super::window_cmds::builtin_window_edges(eval, args)),
        "window-total-height" => {
            return Some(super::window_cmds::builtin_window_total_height(eval, args))
        }
        "window-total-width" => {
            return Some(super::window_cmds::builtin_window_total_width(eval, args))
        }
        "window-list" => return Some(super::window_cmds::builtin_window_list(eval, args)),
        "get-buffer-window" => {
            return Some(super::window_cmds::builtin_get_buffer_window(eval, args))
        }
        "get-buffer-window-list" => {
            return Some(super::window_cmds::builtin_get_buffer_window_list(
                eval, args,
            ))
        }
        "fit-window-to-buffer" => {
            return Some(super::window_cmds::builtin_fit_window_to_buffer(eval, args))
        }
        "window-dedicated-p" => {
            return Some(super::window_cmds::builtin_window_dedicated_p(eval, args))
        }
        "window-minibuffer-p" => {
            return Some(super::window_cmds::builtin_window_minibuffer_p(eval, args))
        }
        "window-live-p" => return Some(super::window_cmds::builtin_window_live_p(eval, args)),
        "set-window-start" => {
            return Some(super::window_cmds::builtin_set_window_start(eval, args))
        }
        "set-window-group-start" => {
            return Some(super::window_cmds::builtin_set_window_group_start(
                eval, args,
            ))
        }
        "set-window-hscroll" => {
            return Some(super::window_cmds::builtin_set_window_hscroll(eval, args))
        }
        "set-window-margins" => {
            return Some(super::window_cmds::builtin_set_window_margins(eval, args))
        }
        "set-window-fringes" => {
            return Some(super::window_cmds::builtin_set_window_fringes(eval, args))
        }
        "set-window-display-table" => {
            return Some(super::window_cmds::builtin_set_window_display_table(
                eval, args,
            ))
        }
        "set-window-cursor-type" => {
            return Some(super::window_cmds::builtin_set_window_cursor_type(
                eval, args,
            ))
        }
        "set-window-scroll-bars" => {
            return Some(super::window_cmds::builtin_set_window_scroll_bars(
                eval, args,
            ))
        }
        "set-window-vscroll" => {
            return Some(super::window_cmds::builtin_set_window_vscroll(eval, args))
        }
        "set-window-point" => {
            return Some(super::window_cmds::builtin_set_window_point(eval, args))
        }
        "set-window-next-buffers" => {
            return Some(super::window_cmds::builtin_set_window_next_buffers(
                eval, args,
            ))
        }
        "set-window-prev-buffers" => {
            return Some(super::window_cmds::builtin_set_window_prev_buffers(
                eval, args,
            ))
        }
        "set-window-dedicated-p" => {
            return Some(super::window_cmds::builtin_set_window_dedicated_p(
                eval, args,
            ))
        }
        "split-window" => return Some(super::window_cmds::builtin_split_window(eval, args)),
        "split-window-internal" => return Some(builtin_split_window_internal(eval, args)),
        "delete-window" => return Some(super::window_cmds::builtin_delete_window(eval, args)),
        "delete-window-internal" => {
            return Some(super::window_cmds::builtin_delete_window_internal(
                eval, args,
            ))
        }
        "delete-other-windows" => {
            return Some(super::window_cmds::builtin_delete_other_windows(eval, args))
        }
        "delete-other-windows-internal" => {
            return Some(super::window_cmds::builtin_delete_other_windows_internal(
                eval, args,
            ))
        }
        "select-window" => return Some(super::window_cmds::builtin_select_window(eval, args)),
        "other-window" => return Some(super::window_cmds::builtin_other_window(eval, args)),
        "scroll-up-command" => {
            return Some(super::window_cmds::builtin_scroll_up_command(eval, args))
        }
        "scroll-down-command" => {
            return Some(super::window_cmds::builtin_scroll_down_command(eval, args))
        }
        "scroll-up" => return Some(super::window_cmds::builtin_scroll_up(eval, args)),
        "scroll-down" => return Some(super::window_cmds::builtin_scroll_down(eval, args)),
        "scroll-left" => return Some(super::window_cmds::builtin_scroll_left(eval, args)),
        "scroll-right" => return Some(super::window_cmds::builtin_scroll_right(eval, args)),
        "recenter-top-bottom" => {
            return Some(super::window_cmds::builtin_recenter_top_bottom(eval, args))
        }
        "recenter" => return Some(super::window_cmds::builtin_recenter(eval, args)),
        "other-window-for-scrolling" => {
            return Some(super::window_cmds::builtin_other_window_for_scrolling(
                eval, args,
            ))
        }
        "next-window" => return Some(super::window_cmds::builtin_next_window(eval, args)),
        "previous-window" => return Some(super::window_cmds::builtin_previous_window(eval, args)),
        "set-window-buffer" => {
            return Some(super::window_cmds::builtin_set_window_buffer(eval, args))
        }
        "switch-to-buffer" => {
            return Some(super::window_cmds::builtin_switch_to_buffer(eval, args))
        }
        "display-buffer" => return Some(super::window_cmds::builtin_display_buffer(eval, args)),
        "pop-to-buffer" => return Some(super::window_cmds::builtin_pop_to_buffer(eval, args)),
        "current-window-configuration" => {
            return Some(builtin_current_window_configuration(eval, args))
        }
        "set-window-configuration" => return Some(builtin_set_window_configuration(eval, args)),
        "window-configuration-p" => return Some(builtin_window_configuration_p(args)),
        "window-configuration-frame" => return Some(builtin_window_configuration_frame(args)),
        "window-configuration-equal-p" => return Some(builtin_window_configuration_equal_p(args)),
        "selected-frame" => return Some(super::window_cmds::builtin_selected_frame(eval, args)),
        "select-frame" => return Some(super::window_cmds::builtin_select_frame(eval, args)),
        "select-frame-set-input-focus" => {
            return Some(super::window_cmds::builtin_select_frame_set_input_focus(
                eval, args,
            ))
        }
        "last-nonminibuffer-frame" => {
            return Some(super::window_cmds::builtin_selected_frame(eval, args))
        }
        "visible-frame-list" => {
            return Some(super::window_cmds::builtin_visible_frame_list(eval, args))
        }
        "frame-list" => return Some(super::window_cmds::builtin_frame_list(eval, args)),
        "make-frame" => return Some(super::window_cmds::builtin_make_frame(eval, args)),
        "make-frame-visible" => {
            return Some(super::window_cmds::builtin_make_frame_visible(eval, args))
        }
        "iconify-frame" => return Some(super::window_cmds::builtin_iconify_frame(eval, args)),
        "delete-frame" => return Some(super::window_cmds::builtin_delete_frame(eval, args)),
        "frame-char-height" => {
            return Some(super::window_cmds::builtin_frame_char_height(eval, args))
        }
        "frame-char-width" => {
            return Some(super::window_cmds::builtin_frame_char_width(eval, args))
        }
        "frame-native-height" => {
            return Some(super::window_cmds::builtin_frame_native_height(eval, args))
        }
        "frame-native-width" => {
            return Some(super::window_cmds::builtin_frame_native_width(eval, args))
        }
        "frame-text-cols" => return Some(super::window_cmds::builtin_frame_text_cols(eval, args)),
        "frame-text-height" => {
            return Some(super::window_cmds::builtin_frame_text_height(eval, args))
        }
        "frame-text-lines" => {
            return Some(super::window_cmds::builtin_frame_text_lines(eval, args))
        }
        "frame-text-width" => {
            return Some(super::window_cmds::builtin_frame_text_width(eval, args))
        }
        "frame-total-cols" => {
            return Some(super::window_cmds::builtin_frame_total_cols(eval, args))
        }
        "frame-total-lines" => {
            return Some(super::window_cmds::builtin_frame_total_lines(eval, args))
        }
        "frame-position" => return Some(super::window_cmds::builtin_frame_position(eval, args)),
        "frame-parameter" => return Some(super::window_cmds::builtin_frame_parameter(eval, args)),
        "frame-parameters" => {
            return Some(super::window_cmds::builtin_frame_parameters(eval, args))
        }
        "modify-frame-parameters" => {
            return Some(super::window_cmds::builtin_modify_frame_parameters(
                eval, args,
            ))
        }
        "set-frame-height" => {
            return Some(super::window_cmds::builtin_set_frame_height(eval, args))
        }
        "set-frame-width" => return Some(super::window_cmds::builtin_set_frame_width(eval, args)),
        "set-frame-size" => return Some(super::window_cmds::builtin_set_frame_size(eval, args)),
        "set-frame-position" => {
            return Some(super::window_cmds::builtin_set_frame_position(eval, args))
        }
        "frame-visible-p" => return Some(super::window_cmds::builtin_frame_visible_p(eval, args)),
        "frame-live-p" => return Some(super::window_cmds::builtin_frame_live_p(eval, args)),
        "frame-first-window" => {
            return Some(super::window_cmds::builtin_frame_first_window(eval, args))
        }
        "frame-root-window" => {
            return Some(super::window_cmds::builtin_frame_root_window(eval, args))
        }
        "windowp" => return Some(super::window_cmds::builtin_windowp(eval, args)),
        "window-valid-p" => return Some(super::window_cmds::builtin_window_valid_p(eval, args)),
        "framep" => return Some(super::window_cmds::builtin_framep(eval, args)),
        "window-frame" => return Some(super::window_cmds::builtin_window_frame(eval, args)),
        "frame-selected-window" => {
            return Some(super::window_cmds::builtin_frame_selected_window(
                eval, args,
            ))
        }
        "display-graphic-p" => {
            return Some(super::display::builtin_display_graphic_p_eval(eval, args))
        }
        "send-string-to-terminal" => {
            return Some(super::display::builtin_send_string_to_terminal_eval(
                eval, args,
            ))
        }
        "internal-show-cursor" => {
            return Some(super::display::builtin_internal_show_cursor_eval(
                eval, args,
            ))
        }
        "internal-show-cursor-p" => {
            return Some(super::display::builtin_internal_show_cursor_p_eval(
                eval, args,
            ))
        }
        "redraw-frame" => return Some(super::display::builtin_redraw_frame_eval(eval, args)),
        "display-color-p" => return Some(super::display::builtin_display_color_p_eval(eval, args)),
        "display-grayscale-p" => {
            return Some(super::display::builtin_display_grayscale_p_eval(eval, args))
        }
        "display-mouse-p" => return Some(super::display::builtin_display_mouse_p_eval(eval, args)),
        "display-popup-menus-p" => {
            return Some(super::display::builtin_display_popup_menus_p_eval(
                eval, args,
            ))
        }
        "display-symbol-keys-p" => {
            return Some(super::display::builtin_display_symbol_keys_p_eval(
                eval, args,
            ))
        }
        "display-pixel-width" => {
            return Some(super::display::builtin_display_pixel_width_eval(eval, args))
        }
        "display-pixel-height" => {
            return Some(super::display::builtin_display_pixel_height_eval(
                eval, args,
            ))
        }
        "window-system" => return Some(super::display::builtin_window_system_eval(eval, args)),
        "frame-edges" => return Some(super::display::builtin_frame_edges_eval(eval, args)),
        "display-mm-width" => {
            return Some(super::display::builtin_display_mm_width_eval(eval, args))
        }
        "display-mm-height" => {
            return Some(super::display::builtin_display_mm_height_eval(eval, args))
        }
        "display-screens" => return Some(super::display::builtin_display_screens_eval(eval, args)),
        "display-color-cells" => {
            return Some(super::display::builtin_display_color_cells_eval(eval, args))
        }
        "display-planes" => return Some(super::display::builtin_display_planes_eval(eval, args)),
        "display-visual-class" => {
            return Some(super::display::builtin_display_visual_class_eval(
                eval, args,
            ))
        }
        "display-backing-store" => {
            return Some(super::display::builtin_display_backing_store_eval(
                eval, args,
            ))
        }
        "display-save-under" => {
            return Some(super::display::builtin_display_save_under_eval(eval, args))
        }
        "display-selections-p" => {
            return Some(super::display::builtin_display_selections_p_eval(
                eval, args,
            ))
        }
        "display-images-p" => {
            return Some(super::display::builtin_display_images_p_eval(eval, args))
        }
        "display-supports-face-attributes-p" => {
            return Some(
                super::display::builtin_display_supports_face_attributes_p_eval(eval, args),
            )
        }
        "terminal-name" => return Some(super::display::builtin_terminal_name_eval(eval, args)),
        "terminal-live-p" => return Some(super::display::builtin_terminal_live_p_eval(eval, args)),
        "terminal-parameter" => {
            return Some(super::display::builtin_terminal_parameter_eval(eval, args))
        }
        "terminal-parameters" => {
            return Some(super::display::builtin_terminal_parameters_eval(eval, args))
        }
        "set-terminal-parameter" => {
            return Some(super::display::builtin_set_terminal_parameter_eval(
                eval, args,
            ))
        }
        "tty-type" => return Some(super::display::builtin_tty_type_eval(eval, args)),
        "tty-top-frame" => return Some(super::display::builtin_tty_top_frame_eval(eval, args)),
        "tty-display-color-p" => {
            return Some(super::display::builtin_tty_display_color_p_eval(eval, args))
        }
        "tty-display-color-cells" => {
            return Some(super::display::builtin_tty_display_color_cells_eval(
                eval, args,
            ))
        }
        "tty-no-underline" => {
            return Some(super::display::builtin_tty_no_underline_eval(eval, args))
        }
        "controlling-tty-p" => {
            return Some(super::display::builtin_controlling_tty_p_eval(eval, args))
        }
        "suspend-tty" => return Some(super::display::builtin_suspend_tty_eval(eval, args)),
        "resume-tty" => return Some(super::display::builtin_resume_tty_eval(eval, args)),
        "frame-terminal" => return Some(super::display::builtin_frame_terminal_eval(eval, args)),
        "display-monitor-attributes-list" => {
            return Some(super::display::builtin_display_monitor_attributes_list_eval(eval, args))
        }
        "frame-monitor-attributes" => {
            return Some(super::display::builtin_frame_monitor_attributes_eval(
                eval, args,
            ))
        }
        "x-display-pixel-width" => {
            return Some(super::display::builtin_x_display_pixel_width_eval(
                eval, args,
            ))
        }
        "x-display-pixel-height" => {
            return Some(super::display::builtin_x_display_pixel_height_eval(
                eval, args,
            ))
        }
        "x-server-version" => {
            return Some(super::display::builtin_x_server_version_eval(eval, args))
        }
        "x-server-max-request-size" => {
            return Some(super::display::builtin_x_server_max_request_size_eval(
                eval, args,
            ))
        }
        "x-server-input-extension-version" => {
            return Some(super::display::builtin_x_server_input_extension_version_eval(eval, args))
        }
        "x-server-vendor" => return Some(super::display::builtin_x_server_vendor_eval(eval, args)),
        "x-display-grayscale-p" => {
            return Some(super::display::builtin_x_display_grayscale_p_eval(
                eval, args,
            ))
        }
        "x-display-backing-store" => {
            return Some(super::display::builtin_x_display_backing_store_eval(
                eval, args,
            ))
        }
        "x-display-color-cells" => {
            return Some(super::display::builtin_x_display_color_cells_eval(
                eval, args,
            ))
        }
        "x-display-mm-height" => {
            return Some(super::display::builtin_x_display_mm_height_eval(eval, args))
        }
        "x-display-mm-width" => {
            return Some(super::display::builtin_x_display_mm_width_eval(eval, args))
        }
        "x-display-monitor-attributes-list" => {
            return Some(super::display::builtin_x_display_monitor_attributes_list_eval(eval, args))
        }
        "x-display-planes" => {
            return Some(super::display::builtin_x_display_planes_eval(eval, args))
        }
        "x-display-save-under" => {
            return Some(super::display::builtin_x_display_save_under_eval(
                eval, args,
            ))
        }
        "x-display-screens" => {
            return Some(super::display::builtin_x_display_screens_eval(eval, args))
        }
        "x-display-visual-class" => {
            return Some(super::display::builtin_x_display_visual_class_eval(
                eval, args,
            ))
        }
        "x-display-color-p" => {
            return Some(super::display::builtin_x_display_color_p_eval(eval, args))
        }
        "x-close-connection" => {
            return Some(super::display::builtin_x_close_connection_eval(eval, args))
        }

        // Interactive / command system (evaluator-dependent)
        "call-interactively" => {
            return Some(super::interactive::builtin_call_interactively(eval, args))
        }
        "interactive-p" => return Some(super::interactive::builtin_interactive_p(eval, args)),
        "called-interactively-p" => {
            return Some(super::interactive::builtin_called_interactively_p(
                eval, args,
            ))
        }
        "commandp" => return Some(super::interactive::builtin_commandp_interactive(eval, args)),
        "command-remapping" => {
            return Some(super::interactive::builtin_command_remapping(eval, args))
        }
        "command-execute" => return Some(super::interactive::builtin_command_execute(eval, args)),
        "find-file" => return Some(super::interactive::builtin_find_file_command(eval, args)),
        "save-buffer" => return Some(super::interactive::builtin_save_buffer_command(eval, args)),
        "set-mark-command" => {
            return Some(super::interactive::builtin_set_mark_command(eval, args))
        }
        "eval-expression" => return Some(super::interactive::builtin_eval_expression(eval, args)),
        "self-insert-command" => {
            return Some(super::interactive::builtin_self_insert_command(eval, args))
        }
        "keyboard-quit" => return Some(super::interactive::builtin_keyboard_quit(eval, args)),
        "quoted-insert" => {
            return Some(super::interactive::builtin_quoted_insert_command(
                eval, args,
            ))
        }
        "universal-argument" => {
            return Some(super::interactive::builtin_universal_argument_command(
                eval, args,
            ))
        }
        "execute-extended-command" => {
            return Some(super::interactive::builtin_execute_extended_command(
                eval, args,
            ))
        }
        "key-binding" => return Some(super::interactive::builtin_key_binding(eval, args)),
        "local-key-binding" => {
            return Some(super::interactive::builtin_local_key_binding(eval, args))
        }
        "global-key-binding" => {
            return Some(super::interactive::builtin_global_key_binding(eval, args))
        }
        "minor-mode-key-binding" => {
            return Some(super::interactive::builtin_minor_mode_key_binding(
                eval, args,
            ))
        }
        "where-is-internal" => {
            return Some(super::interactive::builtin_where_is_internal(eval, args))
        }
        "substitute-command-keys" => {
            return Some(super::interactive::builtin_substitute_command_keys(
                eval, args,
            ))
        }
        "describe-key-briefly" => {
            return Some(super::interactive::builtin_describe_key_briefly(eval, args))
        }
        "this-command-keys" => {
            return Some(super::interactive::builtin_this_command_keys(eval, args))
        }
        "this-command-keys-vector" => {
            return Some(super::interactive::builtin_this_command_keys_vector(
                eval, args,
            ))
        }
        "clear-this-command-keys" => {
            return Some(super::interactive::builtin_clear_this_command_keys(
                eval, args,
            ))
        }
        "thing-at-point" => return Some(super::interactive::builtin_thing_at_point(eval, args)),
        "bounds-of-thing-at-point" => {
            return Some(super::interactive::builtin_bounds_of_thing_at_point(
                eval, args,
            ))
        }
        "symbol-at-point" => return Some(super::interactive::builtin_symbol_at_point(eval, args)),
        "word-at-point" => return Some(super::interactive::builtin_word_at_point(eval, args)),
        // Error hierarchy (evaluator-dependent — reads obarray)
        "error-message-string" => {
            return Some(super::errors::builtin_error_message_string(eval, args))
        }

        // Reader/printer (evaluator-dependent)
        "format" => return Some(builtin_format_eval(eval, args)),
        "format-message" => return Some(builtin_format_message_eval(eval, args)),
        "message" => return Some(builtin_message_eval(eval, args)),
        "message-box" => return Some(builtin_message_box_eval(eval, args)),
        "message-or-box" => return Some(builtin_message_or_box_eval(eval, args)),
        "error" => return Some(builtin_error_eval(eval, args)),
        "read-from-string" => return Some(super::reader::builtin_read_from_string(eval, args)),
        "read" => return Some(super::reader::builtin_read(eval, args)),
        "read-from-minibuffer" => {
            return Some(super::reader::builtin_read_from_minibuffer(eval, args))
        }
        "read-string" => return Some(super::reader::builtin_read_string(eval, args)),
        "read-number" => return Some(super::reader::builtin_read_number(eval, args)),
        "read-passwd" => return Some(super::reader::builtin_read_passwd(eval, args)),
        "completing-read" => return Some(super::reader::builtin_completing_read(eval, args)),
        "read-file-name" => return Some(super::minibuffer::builtin_read_file_name(args)),
        "read-directory-name" => return Some(super::minibuffer::builtin_read_directory_name(args)),
        "read-buffer" => return Some(super::minibuffer::builtin_read_buffer(args)),
        "read-command" => return Some(super::minibuffer::builtin_read_command(args)),
        "read-variable" => return Some(super::minibuffer::builtin_read_variable(args)),
        "try-completion" => return Some(super::minibuffer::builtin_try_completion(args)),
        "all-completions" => return Some(super::minibuffer::builtin_all_completions(args)),
        "test-completion" => return Some(super::minibuffer::builtin_test_completion(args)),
        "input-pending-p" => return Some(super::reader::builtin_input_pending_p(eval, args)),
        "discard-input" => return Some(super::reader::builtin_discard_input(eval, args)),
        "current-input-mode" => return Some(super::reader::builtin_current_input_mode(eval, args)),
        "set-input-mode" => return Some(super::reader::builtin_set_input_mode(eval, args)),
        "set-input-interrupt-mode" => {
            return Some(super::reader::builtin_set_input_interrupt_mode(eval, args))
        }
        "set-input-meta-mode" => return Some(super::reader::builtin_set_input_meta_mode(args)),
        "set-output-flow-control" => {
            return Some(super::reader::builtin_set_output_flow_control(args))
        }
        "set-quit-char" => return Some(super::reader::builtin_set_quit_char(args)),
        "waiting-for-user-input-p" => {
            return Some(super::reader::builtin_waiting_for_user_input_p(args))
        }
        "read-char" => return Some(super::reader::builtin_read_char(eval, args)),
        "read-key" => return Some(super::reader::builtin_read_key(eval, args)),
        "read-key-sequence" => return Some(super::reader::builtin_read_key_sequence(eval, args)),
        "read-key-sequence-vector" => {
            return Some(super::reader::builtin_read_key_sequence_vector(eval, args))
        }
        "recent-keys" => return Some(builtin_recent_keys(eval, args)),
        "minibufferp" => return Some(super::minibuffer::builtin_minibufferp(args)),
        "minibuffer-prompt" => return Some(super::minibuffer::builtin_minibuffer_prompt(args)),
        "minibuffer-contents" => {
            return Some(super::minibuffer::builtin_minibuffer_contents(eval, args))
        }
        "minibuffer-contents-no-properties" => {
            return Some(super::minibuffer::builtin_minibuffer_contents_no_properties(eval, args))
        }
        "exit-minibuffer" => return Some(super::minibuffer::builtin_exit_minibuffer(args)),
        "minibuffer-depth" => return Some(super::minibuffer::builtin_minibuffer_depth(args)),
        "princ" => return Some(builtin_princ_eval(eval, args)),
        "prin1" => return Some(builtin_prin1_eval(eval, args)),
        "prin1-to-string" => return Some(builtin_prin1_to_string_eval(eval, args)),
        "print" => return Some(builtin_print_eval(eval, args)),
        "terpri" => return Some(builtin_terpri_eval(eval, args)),
        "write-char" => return Some(builtin_write_char_eval(eval, args)),

        // Misc (evaluator-dependent)
        "backtrace--frames-from-thread" => {
            return Some(super::misc::builtin_backtrace_frames_from_thread(
                eval, args,
            ))
        }
        "backtrace--locals" => return Some(super::misc::builtin_backtrace_locals(eval, args)),
        "backtrace-debug" => return Some(super::misc::builtin_backtrace_debug(eval, args)),
        "backtrace-eval" => return Some(super::misc::builtin_backtrace_eval(eval, args)),
        "backtrace-frame--internal" => {
            return Some(super::misc::builtin_backtrace_frame_internal(eval, args))
        }
        "backtrace-frame" => return Some(super::misc::builtin_backtrace_frame(eval, args)),
        "recursion-depth" => return Some(super::misc::builtin_recursion_depth(eval, args)),
        "top-level" => return Some(super::minibuffer::builtin_top_level(args)),
        "recursive-edit" => return Some(super::minibuffer::builtin_recursive_edit(args)),
        "exit-recursive-edit" => return Some(super::minibuffer::builtin_exit_recursive_edit(args)),
        "abort-recursive-edit" => {
            return Some(super::minibuffer::builtin_abort_recursive_edit(args))
        }
        "abort-minibuffers" => return Some(super::minibuffer::builtin_abort_minibuffers(args)),

        // Threading (evaluator-dependent)
        "make-thread" => return Some(super::threads::builtin_make_thread(eval, args)),
        "thread-join" => return Some(super::threads::builtin_thread_join(eval, args)),
        "thread-yield" => return Some(super::threads::builtin_thread_yield(eval, args)),
        "thread-name" => return Some(super::threads::builtin_thread_name(eval, args)),
        "thread-live-p" => return Some(super::threads::builtin_thread_live_p(eval, args)),
        "threadp" => return Some(super::threads::builtin_threadp(eval, args)),
        "thread-signal" => return Some(super::threads::builtin_thread_signal(eval, args)),
        "current-thread" => return Some(super::threads::builtin_current_thread(eval, args)),
        "all-threads" => return Some(super::threads::builtin_all_threads(eval, args)),
        "thread-last-error" => return Some(super::threads::builtin_thread_last_error(eval, args)),
        "make-mutex" => return Some(super::threads::builtin_make_mutex(eval, args)),
        "mutex-name" => return Some(super::threads::builtin_mutex_name(eval, args)),
        "mutex-lock" => return Some(super::threads::builtin_mutex_lock(eval, args)),
        "mutex-unlock" => return Some(super::threads::builtin_mutex_unlock(eval, args)),
        "mutexp" => return Some(super::threads::builtin_mutexp(eval, args)),
        "make-condition-variable" => {
            return Some(super::threads::builtin_make_condition_variable(eval, args))
        }
        "condition-variable-p" => {
            return Some(super::threads::builtin_condition_variable_p(eval, args))
        }
        "condition-name" => return Some(super::threads::builtin_condition_name(eval, args)),
        "condition-mutex" => return Some(super::threads::builtin_condition_mutex(eval, args)),
        "condition-wait" => return Some(super::threads::builtin_condition_wait(eval, args)),
        "condition-notify" => return Some(super::threads::builtin_condition_notify(eval, args)),

        // Undo system (evaluator-dependent)
        "undo-boundary" => return Some(super::undo::builtin_undo_boundary_eval(eval, args)),
        "undo" => return Some(super::undo::builtin_undo(eval, args)),

        // Hash-table / obarray (evaluator-dependent)
        "maphash" => return Some(super::hashtab::builtin_maphash(eval, args)),
        "mapatoms" => return Some(super::hashtab::builtin_mapatoms(eval, args)),
        "unintern" => return Some(super::hashtab::builtin_unintern(eval, args)),

        // Marker (evaluator-dependent)
        "set-marker" => return Some(super::marker::builtin_set_marker(eval, args)),
        "point-marker" => return Some(super::marker::builtin_point_marker(eval, args)),
        "point-min-marker" => return Some(super::marker::builtin_point_min_marker(eval, args)),
        "point-max-marker" => return Some(super::marker::builtin_point_max_marker(eval, args)),

        // Case table (evaluator-dependent)
        "current-case-table" => {
            return Some(super::casetab::builtin_current_case_table_eval(eval, args))
        }
        "standard-case-table" => {
            return Some(super::casetab::builtin_standard_case_table_eval(eval, args))
        }
        "set-case-table" => return Some(super::casetab::builtin_set_case_table_eval(eval, args)),
        "set-standard-case-table" => {
            return Some(super::casetab::builtin_set_standard_case_table_eval(
                eval, args,
            ))
        }

        // Category (evaluator-dependent)
        "define-category" => {
            return Some(super::category::builtin_define_category_eval(eval, args))
        }
        "category-docstring" => {
            return Some(super::category::builtin_category_docstring_eval(eval, args))
        }
        "get-unused-category" => {
            return Some(super::category::builtin_get_unused_category_eval(
                eval, args,
            ))
        }
        "modify-category-entry" => {
            return Some(super::category::builtin_modify_category_entry(eval, args))
        }
        "char-category-set" => return Some(super::category::builtin_char_category_set(eval, args)),
        "category-table" => return Some(super::category::builtin_category_table_eval(eval, args)),
        "standard-category-table" => {
            return Some(super::category::builtin_standard_category_table_eval(
                eval, args,
            ))
        }
        "set-category-table" => {
            return Some(super::category::builtin_set_category_table_eval(eval, args))
        }

        // Char-table (evaluator-dependent — applies function)
        "map-char-table" => return Some(super::chartable::builtin_map_char_table(eval, args)),

        // Coding system (evaluator-dependent — uses coding_systems manager)
        "coding-system-list" => {
            return Some(super::coding::builtin_coding_system_list(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-aliases" => {
            return Some(super::coding::builtin_coding_system_aliases(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-get" => {
            return Some(super::coding::builtin_coding_system_get(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-plist" => {
            return Some(super::coding::builtin_coding_system_plist(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-put" => {
            return Some(super::coding::builtin_coding_system_put(
                &mut eval.coding_systems,
                args,
            ))
        }
        "coding-system-base" => {
            return Some(super::coding::builtin_coding_system_base(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-eol-type" => {
            return Some(super::coding::builtin_coding_system_eol_type(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-type" => {
            return Some(super::coding::builtin_coding_system_type(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-change-eol-conversion" => {
            return Some(super::coding::builtin_coding_system_change_eol_conversion(
                &eval.coding_systems,
                args,
            ))
        }
        "coding-system-change-text-conversion" => {
            return Some(super::coding::builtin_coding_system_change_text_conversion(
                &eval.coding_systems,
                args,
            ))
        }
        "detect-coding-string" => {
            return Some(super::coding::builtin_detect_coding_string(
                &eval.coding_systems,
                args,
            ))
        }
        "detect-coding-region" => {
            return Some(super::coding::builtin_detect_coding_region(
                &eval.coding_systems,
                args,
            ))
        }
        "keyboard-coding-system" => {
            return Some(super::coding::builtin_keyboard_coding_system(
                &eval.coding_systems,
                args,
            ))
        }
        "terminal-coding-system" => {
            return Some(super::coding::builtin_terminal_coding_system(
                &eval.coding_systems,
                args,
            ))
        }
        "set-keyboard-coding-system" => {
            return Some(super::coding::builtin_set_keyboard_coding_system(
                &mut eval.coding_systems,
                args,
            ))
        }
        "set-terminal-coding-system" => {
            return Some(super::coding::builtin_set_terminal_coding_system(
                &mut eval.coding_systems,
                args,
            ))
        }
        "coding-system-priority-list" => {
            return Some(super::coding::builtin_coding_system_priority_list(
                &eval.coding_systems,
                args,
            ))
        }
        "seq-position" => return Some(super::cl_lib::builtin_seq_position(eval, args)),
        "seq-contains-p" => return Some(super::cl_lib::builtin_seq_contains_p(eval, args)),
        "seq-mapn" => return Some(super::cl_lib::builtin_seq_mapn(eval, args)),
        "seq-do" => return Some(super::cl_lib::builtin_seq_do(eval, args)),
        "seq-count" => return Some(super::cl_lib::builtin_seq_count(eval, args)),
        "seq-reduce" => return Some(super::cl_lib::builtin_seq_reduce(eval, args)),
        "seq-some" => return Some(super::cl_lib::builtin_seq_some(eval, args)),
        "seq-every-p" => return Some(super::cl_lib::builtin_seq_every_p(eval, args)),
        "seq-sort" => return Some(super::cl_lib::builtin_seq_sort(eval, args)),
        "assoc" => return Some(builtin_assoc_eval(eval, args)),
        "alist-get" => return Some(builtin_alist_get_eval(eval, args)),
        "plist-member" => return Some(builtin_plist_member(eval, args)),
        "json-parse-buffer" => return Some(super::json::builtin_json_parse_buffer(eval, args)),
        "json-insert" => return Some(super::json::builtin_json_insert(eval, args)),

        // Documentation/help (evaluator-dependent)
        "documentation" => return Some(super::doc::builtin_documentation(eval, args)),
        "describe-function" => return Some(super::doc::builtin_describe_function(eval, args)),
        "describe-variable" => return Some(super::doc::builtin_describe_variable(eval, args)),
        "documentation-stringp" => return Some(builtin_documentation_stringp(args)),
        "documentation-property" => {
            return Some(super::doc::builtin_documentation_property_eval(eval, args))
        }

        // Indentation (evaluator-dependent)
        "current-indentation" => {
            return Some(super::indent::builtin_current_indentation_eval(eval, args))
        }
        "current-column" => return Some(super::indent::builtin_current_column_eval(eval, args)),
        "move-to-column" => return Some(super::indent::builtin_move_to_column_eval(eval, args)),
        "indent-region" => return Some(super::indent::builtin_indent_region(eval, args)),
        "reindent-then-newline-and-indent" => {
            return Some(super::indent::builtin_reindent_then_newline_and_indent(
                eval, args,
            ))
        }
        "indent-for-tab-command" => {
            return Some(super::indent::builtin_indent_for_tab_command(eval, args))
        }
        "indent-according-to-mode" => {
            return Some(super::indent::builtin_indent_according_to_mode(eval, args))
        }
        "back-to-indentation" => {
            return Some(super::indent::builtin_back_to_indentation(eval, args))
        }

        // Case/char (evaluator-dependent)
        "char-equal" => return Some(builtin_char_equal(eval, args)),
        "upcase-initials-region" => {
            return Some(super::kill_ring::builtin_upcase_initials_region(eval, args))
        }

        // Search (evaluator-dependent)
        "posix-search-forward" => {
            // Reuse regex search engine for now; this replaces nil-stub behavior.
            return Some(builtin_re_search_forward(eval, args));
        }
        "posix-search-backward" => {
            // Reuse regex search engine for now; this replaces nil-stub behavior.
            return Some(builtin_re_search_backward(eval, args));
        }
        "word-search-forward" => {
            // Literal fallback keeps search state/point semantics instead of stub nil.
            return Some(builtin_search_forward(eval, args));
        }
        "word-search-backward" => {
            // Literal fallback keeps search state/point semantics instead of stub nil.
            return Some(builtin_search_backward(eval, args));
        }

        // Lread (evaluator-dependent)
        "eval-buffer" => return Some(super::lread::builtin_eval_buffer(eval, args)),
        "eval-region" => return Some(super::lread::builtin_eval_region(eval, args)),
        "read-event" => return Some(super::lread::builtin_read_event(eval, args)),
        "read-char-exclusive" => {
            return Some(super::lread::builtin_read_char_exclusive(eval, args))
        }

        // Editfns (evaluator-dependent)
        "insert-before-markers" => {
            return Some(super::editfns::builtin_insert_before_markers(eval, args))
        }
        "delete-char" => return Some(super::editfns::builtin_delete_char(eval, args)),
        "buffer-substring-no-properties" => {
            return Some(super::editfns::builtin_buffer_substring_no_properties(
                eval, args,
            ))
        }
        "following-char" => return Some(super::editfns::builtin_following_char(eval, args)),
        "preceding-char" => return Some(super::editfns::builtin_preceding_char(eval, args)),

        _ => {}
    }

    if let Ok(id) = name.parse::<PureBuiltinId>() {
        return Some(dispatch_builtin_id_pure(id, args));
    }

    // Pure builtins (no evaluator needed)
    Some(match name {
        // Arithmetic
        "+" => builtin_add(args),
        "-" => builtin_sub(args),
        "*" => builtin_mul(args),
        "/" => builtin_div(args),
        "%" => builtin_percent(args),
        "mod" => builtin_mod(args),
        "1+" => builtin_add1(args),
        "1-" => builtin_sub1(args),
        "max" => builtin_max(args),
        "min" => builtin_min(args),
        "abs" => builtin_abs(args),

        // Logical / bitwise
        "logand" => builtin_logand(args),
        "logior" => builtin_logior(args),
        "logxor" => builtin_logxor(args),
        "lognot" => builtin_lognot(args),
        "ash" => builtin_ash(args),

        // Numeric comparisons
        "=" => builtin_num_eq(args),
        "<" => builtin_num_lt(args),
        "<=" => builtin_num_le(args),
        ">" => builtin_num_gt(args),
        ">=" => builtin_num_ge(args),
        "/=" => builtin_num_ne(args),

        // Type predicates (typed subset is dispatched above)
        // Type predicates (typed subset is dispatched above)
        // Type predicates (typed subset is dispatched above)
        "integer-or-marker-p" => builtin_integer_or_marker_p(args),
        "number-or-marker-p" => builtin_number_or_marker_p(args),
        "vector-or-char-table-p" => builtin_vector_or_char_table_p(args),
        "module-function-p" => builtin_module_function_p(args),
        "user-ptrp" => builtin_user_ptrp(args),
        "symbol-with-pos-p" => builtin_symbol_with_pos_p(args),
        "symbol-with-pos-pos" => builtin_symbol_with_pos_pos(args),

        // Equality (typed subset is dispatched above)
        "function-equal" => builtin_function_equal(args),

        // Cons / List
        "cons" => builtin_cons(args),
        "car" => builtin_car(args),
        "cdr" => builtin_cdr(args),
        "caar" => builtin_caar(args),
        "cadr" => builtin_cadr(args),
        "cdar" => builtin_cdar(args),
        "cddr" => builtin_cddr(args),
        "caaar" => builtin_caaar(args),
        "caadr" => builtin_caadr(args),
        "cadar" => builtin_cadar(args),
        "caddr" => builtin_caddr(args),
        "cdaar" => builtin_cdaar(args),
        "cdadr" => builtin_cdadr(args),
        "cddar" => builtin_cddar(args),
        "cdddr" => builtin_cdddr(args),
        "cadddr" => builtin_cadddr(args),
        "cddddr" => builtin_cddddr(args),
        "caaaar" => builtin_caaaar(args),
        "caaadr" => builtin_caaadr(args),
        "caadar" => builtin_caadar(args),
        "caaddr" => builtin_caaddr(args),
        "cadaar" => builtin_cadaar(args),
        "cadadr" => builtin_cadadr(args),
        "caddar" => builtin_caddar(args),
        "cdaaar" => builtin_cdaaar(args),
        "cdaadr" => builtin_cdaadr(args),
        "cdadar" => builtin_cdadar(args),
        "cdaddr" => builtin_cdaddr(args),
        "cddaar" => builtin_cddaar(args),
        "cddadr" => builtin_cddadr(args),
        "cdddar" => builtin_cdddar(args),
        "car-safe" => builtin_car_safe(args),
        "cdr-safe" => builtin_cdr_safe(args),
        "setcar" => builtin_setcar(args),
        "setcdr" => builtin_setcdr(args),
        "list" => builtin_list(args),
        "length" => builtin_length(args),
        "length<" => builtin_length_lt(args),
        "length=" => builtin_length_eq(args),
        "length>" => builtin_length_gt(args),
        "nth" => builtin_nth(args),
        "nthcdr" => builtin_nthcdr(args),
        "append" => builtin_append(args),
        "reverse" => builtin_reverse(args),
        "nreverse" => builtin_nreverse(args),
        "member" => builtin_member(args),
        "memq" => builtin_memq(args),
        "memql" => builtin_memql(args),
        "assoc" => builtin_assoc(args),
        "assq" => builtin_assq(args),
        "copy-sequence" => builtin_copy_sequence(args),
        "purecopy" => builtin_purecopy(args),
        "substring-no-properties" => builtin_substring_no_properties(args),

        // String (typed subset is dispatched above)

        // Vector (typed subset is dispatched above)

        // Hash table (typed subset is dispatched above)

        // Conversion (typed subset is dispatched above)

        // Property lists
        "plist-get" => builtin_plist_get(args),
        "plist-put" => builtin_plist_put(args),

        // Symbol (pure)
        "cl-type-of" => builtin_cl_type_of(args),
        "symbol-name" => builtin_symbol_name(args),
        "make-symbol" => builtin_make_symbol(args),

        // Math (typed subset is dispatched above)

        // Extended string (typed subset is dispatched above)

        // Extended list (typed subset is dispatched above)

        // Output / misc
        "identity" => builtin_identity(args),
        "format-message" => builtin_format_message(args),
        "message" => builtin_message(args),
        "message-box" => builtin_message_box(args),
        "message-or-box" => builtin_message_or_box(args),
        "current-message" => builtin_current_message(args),
        "ngettext" => builtin_ngettext(args),
        "secure-hash-algorithms" => builtin_secure_hash_algorithms(args),
        "error" => builtin_error(args),
        "prefix-numeric-value" => builtin_prefix_numeric_value(args),
        "command-error-default-function" => builtin_command_error_default_function(args),
        "compute-motion" => builtin_compute_motion(args),
        "clear-string" => builtin_clear_string(args),
        "combine-after-change-execute" => builtin_combine_after_change_execute(args),
        "princ" => builtin_princ(args),
        "prin1" => builtin_prin1(args),
        "prin1-to-string" => builtin_prin1_to_string(args),
        "print" => builtin_print(args),
        "terpri" => builtin_terpri(args),
        "write-char" => builtin_write_char(args),
        "propertize" => builtin_propertize(args),
        "gensym" => builtin_gensym(args),
        "string-to-syntax" => builtin_string_to_syntax(args),
        "syntax-class-to-char" => super::syntax::builtin_syntax_class_to_char(args),
        "matching-paren" => super::syntax::builtin_matching_paren(args),
        "make-syntax-table" => super::syntax::builtin_make_syntax_table(args),
        "copy-syntax-table" => super::syntax::builtin_copy_syntax_table(args),
        "syntax-table-p" => super::syntax::builtin_syntax_table_p(args),
        "standard-syntax-table" => super::syntax::builtin_standard_syntax_table(args),
        "current-time" => builtin_current_time(args),
        "current-cpu-time" => builtin_current_cpu_time(args),
        "current-idle-time" => builtin_current_idle_time(args),
        "get-internal-run-time" => builtin_get_internal_run_time(args),
        "float-time" => builtin_float_time(args),
        "daemonp" => builtin_daemonp(args),
        "daemon-initialized" => builtin_daemon_initialized(args),
        "flush-standard-output" => builtin_flush_standard_output(args),
        "force-mode-line-update" => builtin_force_mode_line_update(args),
        "force-window-update" => builtin_force_window_update(args),
        "invocation-directory" => builtin_invocation_directory(args),
        "invocation-name" => builtin_invocation_name(args),

        // File I/O (pure)
        "access-file" => super::fileio::builtin_access_file(args),
        "expand-file-name" => super::fileio::builtin_expand_file_name(args),
        "file-truename" => super::fileio::builtin_file_truename(args),
        "file-name-directory" => super::fileio::builtin_file_name_directory(args),
        "file-name-nondirectory" => super::fileio::builtin_file_name_nondirectory(args),
        "file-name-extension" => super::fileio::builtin_file_name_extension(args),
        "file-name-sans-extension" => super::fileio::builtin_file_name_sans_extension(args),
        "file-name-as-directory" => super::fileio::builtin_file_name_as_directory(args),
        "directory-file-name" => super::fileio::builtin_directory_file_name(args),
        "file-name-concat" => super::fileio::builtin_file_name_concat(args),
        "file-name-absolute-p" => super::fileio::builtin_file_name_absolute_p(args),
        "directory-name-p" => super::fileio::builtin_directory_name_p(args),
        "substitute-in-file-name" => super::fileio::builtin_substitute_in_file_name(args),
        "file-acl" => super::fileio::builtin_file_acl(args),
        "file-exists-p" => super::fileio::builtin_file_exists_p(args),
        "file-readable-p" => super::fileio::builtin_file_readable_p(args),
        "file-writable-p" => super::fileio::builtin_file_writable_p(args),
        "file-accessible-directory-p" => super::fileio::builtin_file_accessible_directory_p(args),
        "file-executable-p" => super::fileio::builtin_file_executable_p(args),
        "file-locked-p" => super::fileio::builtin_file_locked_p(args),
        "file-selinux-context" => super::fileio::builtin_file_selinux_context(args),
        "file-system-info" => super::fileio::builtin_file_system_info(args),
        "file-directory-p" => super::fileio::builtin_file_directory_p(args),
        "file-regular-p" => super::fileio::builtin_file_regular_p(args),
        "file-symlink-p" => super::fileio::builtin_file_symlink_p(args),
        "file-name-case-insensitive-p" => super::fileio::builtin_file_name_case_insensitive_p(args),
        "file-newer-than-file-p" => super::fileio::builtin_file_newer_than_file_p(args),
        "file-equal-p" => super::fileio::builtin_file_equal_p(args),
        "file-in-directory-p" => super::fileio::builtin_file_in_directory_p(args),
        "file-modes" => super::fileio::builtin_file_modes(args),
        "set-file-modes" => super::fileio::builtin_set_file_modes(args),
        "set-file-times" => super::fileio::builtin_set_file_times(args),
        "set-file-acl" => super::fileio::builtin_set_file_acl(args),
        "set-file-selinux-context" => super::fileio::builtin_set_file_selinux_context(args),
        "visited-file-modtime" => super::fileio::builtin_visited_file_modtime(args),
        "default-file-modes" => super::fileio::builtin_default_file_modes(args),
        "set-default-file-modes" => super::fileio::builtin_set_default_file_modes(args),
        "delete-file" => super::fileio::builtin_delete_file(args),
        "delete-file-internal" => super::fileio::builtin_delete_file_internal(args),
        "delete-directory" => super::fileio::builtin_delete_directory(args),
        "delete-directory-internal" => super::fileio::builtin_delete_directory_internal(args),
        "rename-file" => super::fileio::builtin_rename_file(args),
        "copy-file" => super::fileio::builtin_copy_file(args),
        "add-name-to-file" => super::fileio::builtin_add_name_to_file(args),
        "make-symbolic-link" => super::fileio::builtin_make_symbolic_link(args),
        "make-directory" => super::fileio::builtin_make_directory(args),
        "make-directory-internal" => super::fileio::builtin_make_directory_internal(args),
        "make-temp-file" => super::fileio::builtin_make_temp_file(args),
        "make-temp-name" => super::fileio::builtin_make_temp_name(args),
        "make-nearby-temp-file" => super::fileio::builtin_make_nearby_temp_file(args),
        "next-read-file-uses-dialog-p" => super::fileio::builtin_next_read_file_uses_dialog_p(args),
        "unhandled-file-name-directory" => {
            super::fileio::builtin_unhandled_file_name_directory(args)
        }
        "get-truename-buffer" => super::fileio::builtin_get_truename_buffer(args),
        "directory-files" => super::fileio::builtin_directory_files(args),
        "find-file-name-handler" => super::fileio::builtin_find_file_name_handler(args),
        "file-attributes" => super::dired::builtin_file_attributes(args),

        // Keymap (pure — no evaluator needed)
        "kbd" => builtin_kbd(args),
        "single-key-description" => builtin_single_key_description(args),
        "key-description" => builtin_key_description(args),
        "help-key-description" => builtin_help_key_description(args),
        "event-convert-list" => builtin_event_convert_list(args),
        "event-basic-type" => builtin_event_basic_type(args),
        "event-apply-modifier" => builtin_event_apply_modifier(args),
        "eventp" => builtin_eventp(args),
        "event-modifiers" => builtin_event_modifiers(args),
        "listify-key-sequence" => builtin_listify_key_sequence(args),
        "key-valid-p" => builtin_key_valid_p(args),
        "text-char-description" => builtin_text_char_description(args),

        // Process (pure — no evaluator needed)
        "shell-command-to-string" => super::process::builtin_shell_command_to_string(args),
        "getenv" => super::process::builtin_getenv(args),
        "getenv-internal" => super::process::builtin_getenv_internal(args),
        "setenv" => super::process::builtin_setenv(args),
        "set-binary-mode" => super::process::builtin_set_binary_mode(args),

        // Timer (pure — no evaluator needed)
        "timerp" => super::timer::builtin_timerp(args),
        "sit-for" => super::timer::builtin_sit_for(args),

        // Undo system (pure — no evaluator needed)
        "undo-boundary" => super::undo::builtin_undo_boundary(args),
        "primitive-undo" => super::undo::builtin_primitive_undo(args),

        // Keyboard macro (pure — no evaluator needed)

        // Case table (pure)
        "case-table-p" => super::casetab::builtin_case_table_p(args),
        "current-case-table" => super::casetab::builtin_current_case_table(args),
        "standard-case-table" => super::casetab::builtin_standard_case_table(args),
        "set-case-table" => super::casetab::builtin_set_case_table(args),
        "set-standard-case-table" => super::casetab::builtin_set_standard_case_table(args),
        "upcase-char" => super::casetab::builtin_upcase_char(args),

        // Category (pure)
        "define-category" => super::category::builtin_define_category(args),
        "category-docstring" => super::category::builtin_category_docstring(args),
        "get-unused-category" => super::category::builtin_get_unused_category(args),
        "copy-category-table" => super::category::builtin_copy_category_table(args),
        "category-table-p" => super::category::builtin_category_table_p(args),
        "category-table" => super::category::builtin_category_table(args),
        "standard-category-table" => super::category::builtin_standard_category_table(args),
        "make-category-table" => super::category::builtin_make_category_table(args),
        "set-category-table" => super::category::builtin_set_category_table(args),
        "make-category-set" => super::category::builtin_make_category_set(args),
        "category-set-mnemonics" => super::category::builtin_category_set_mnemonics(args),

        // Display/terminal (pure)
        "redraw-frame" => super::display::builtin_redraw_frame(args),
        "redraw-display" => super::display::builtin_redraw_display(args),
        "open-termscript" => super::display::builtin_open_termscript(args),
        "ding" => super::display::builtin_ding(args),
        "send-string-to-terminal" => super::display::builtin_send_string_to_terminal(args),
        "internal-show-cursor" => super::display::builtin_internal_show_cursor(args),
        "internal-show-cursor-p" => super::display::builtin_internal_show_cursor_p(args),
        "display-graphic-p" => super::display::builtin_display_graphic_p(args),
        "display-color-p" => super::display::builtin_display_color_p(args),
        "display-grayscale-p" => super::display::builtin_display_grayscale_p(args),
        "display-mouse-p" => super::display::builtin_display_mouse_p(args),
        "display-popup-menus-p" => super::display::builtin_display_popup_menus_p(args),
        "display-symbol-keys-p" => super::display::builtin_display_symbol_keys_p(args),
        "display-pixel-width" => super::display::builtin_display_pixel_width(args),
        "display-pixel-height" => super::display::builtin_display_pixel_height(args),
        "display-mm-width" => super::display::builtin_display_mm_width(args),
        "display-mm-height" => super::display::builtin_display_mm_height(args),
        "display-screens" => super::display::builtin_display_screens(args),
        "display-color-cells" => super::display::builtin_display_color_cells(args),
        "display-planes" => super::display::builtin_display_planes(args),
        "display-visual-class" => super::display::builtin_display_visual_class(args),
        "display-backing-store" => super::display::builtin_display_backing_store(args),
        "display-save-under" => super::display::builtin_display_save_under(args),
        "display-selections-p" => super::display::builtin_display_selections_p(args),
        "gui-get-primary-selection" => super::display::builtin_gui_get_primary_selection(args),
        "gui-get-selection" => super::display::builtin_gui_get_selection(args),
        "gui-select-text" => super::display::builtin_gui_select_text(args),
        "gui-selection-value" => super::display::builtin_gui_selection_value(args),
        "gui-set-selection" => super::display::builtin_gui_set_selection(args),
        "x-apply-session-resources" => super::display::builtin_x_apply_session_resources(args),
        "x-export-frames" => super::display::builtin_x_export_frames(args),
        "x-backspace-delete-keys-p" => super::display::builtin_x_backspace_delete_keys_p(args),
        "x-change-window-property" => super::display::builtin_x_change_window_property(args),
        "x-clear-preedit-text" => super::display::builtin_x_clear_preedit_text(args),
        "x-clipboard-yank" => super::display::builtin_x_clipboard_yank(args),
        "x-focus-frame" => super::display::builtin_x_focus_frame(args),
        "x-get-clipboard" => super::display::builtin_x_get_clipboard(args),
        "x-get-input-coding-system" => super::display::builtin_x_get_input_coding_system(args),
        "x-get-local-selection" => super::display::builtin_x_get_local_selection(args),
        "x-get-modifier-masks" => super::display::builtin_x_get_modifier_masks(args),
        "x-get-selection-internal" => super::display::builtin_x_get_selection_internal(args),
        "x-display-list" => super::display::builtin_x_display_list(args),
        "x-disown-selection-internal" => super::display::builtin_x_disown_selection_internal(args),
        "x-device-class" => super::display::builtin_x_device_class(args),
        "x-delete-window-property" => super::display::builtin_x_delete_window_property(args),
        "x-frame-edges" => super::display::builtin_x_frame_edges(args),
        "x-frame-geometry" => super::display::builtin_x_frame_geometry(args),
        "x-frame-list-z-order" => super::display::builtin_x_frame_list_z_order(args),
        "x-frame-restack" => super::display::builtin_x_frame_restack(args),
        "x-family-fonts" => super::display::builtin_x_family_fonts(args),
        "x-get-atom-name" => super::display::builtin_x_get_atom_name(args),
        "x-mouse-absolute-pixel-position" => {
            super::display::builtin_x_mouse_absolute_pixel_position(args)
        }
        "x-get-resource" => super::display::builtin_x_get_resource(args),
        "x-list-fonts" => super::display::builtin_x_list_fonts(args),
        "x-open-connection" => super::display::builtin_x_open_connection(args),
        "x-parse-geometry" => super::display::builtin_x_parse_geometry(args),
        "x-own-selection-internal" => super::display::builtin_x_own_selection_internal(args),
        "x-popup-dialog" => super::display::builtin_x_popup_dialog(args),
        "x-popup-menu" => super::display::builtin_x_popup_menu(args),
        "x-register-dnd-atom" => super::display::builtin_x_register_dnd_atom(args),
        "x-selection-exists-p" => super::display::builtin_x_selection_exists_p(args),
        "x-selection-owner-p" => super::display::builtin_x_selection_owner_p(args),
        "x-hide-tip" => super::display::builtin_x_hide_tip(args),
        "x-internal-focus-input-context" => {
            super::display::builtin_x_internal_focus_input_context(args)
        }
        "x-preedit-text" => super::display::builtin_x_preedit_text(args),
        "x-send-client-message" => super::display::builtin_x_send_client_message(args),
        "x-show-tip" => super::display::builtin_x_show_tip(args),
        "x-setup-function-keys" => super::display::builtin_x_setup_function_keys(args),
        "x-set-mouse-absolute-pixel-position" => {
            super::display::builtin_x_set_mouse_absolute_pixel_position(args)
        }
        "x-synchronize" => super::display::builtin_x_synchronize(args),
        "x-translate-coordinates" => super::display::builtin_x_translate_coordinates(args),
        "x-uses-old-gtk-dialog" => super::display::builtin_x_uses_old_gtk_dialog(args),
        "x-close-connection" => super::display::builtin_x_close_connection(args),
        "x-display-pixel-width" => super::display::builtin_x_display_pixel_width(args),
        "x-display-pixel-height" => super::display::builtin_x_display_pixel_height(args),
        "x-display-color-p" => super::display::builtin_x_display_color_p(args),
        "x-window-property" => super::display::builtin_x_window_property(args),
        "x-window-property-attributes" => {
            super::display::builtin_x_window_property_attributes(args)
        }
        "terminal-name" => super::display::builtin_terminal_name(args),
        "terminal-list" => super::display::builtin_terminal_list(args),
        "frame-terminal" => super::display::builtin_frame_terminal(args),
        "terminal-live-p" => super::display::builtin_terminal_live_p(args),
        "terminal-parameter" => super::display::builtin_terminal_parameter(args),
        "terminal-parameters" => super::display::builtin_terminal_parameters(args),
        "set-terminal-parameter" => super::display::builtin_set_terminal_parameter(args),
        "tty-type" => super::display::builtin_tty_type(args),
        "tty-top-frame" => super::display::builtin_tty_top_frame(args),
        "tty-display-color-p" => super::display::builtin_tty_display_color_p(args),
        "tty-display-color-cells" => super::display::builtin_tty_display_color_cells(args),
        "tty-no-underline" => super::display::builtin_tty_no_underline(args),
        "controlling-tty-p" => super::display::builtin_controlling_tty_p(args),
        "suspend-tty" => super::display::builtin_suspend_tty(args),
        "resume-tty" => super::display::builtin_resume_tty(args),
        "display-monitor-attributes-list" => {
            super::display::builtin_display_monitor_attributes_list(args)
        }
        "frame-monitor-attributes" => super::display::builtin_frame_monitor_attributes(args),
        "window-system" => super::display::builtin_window_system(args),
        "frame-edges" => super::display::builtin_frame_edges(args),
        "display-images-p" => super::display::builtin_display_images_p(args),
        "display-supports-face-attributes-p" => {
            super::display::builtin_display_supports_face_attributes_p(args)
        }
        "x-server-version" => super::display::builtin_x_server_version(args),
        "x-server-max-request-size" => super::display::builtin_x_server_max_request_size(args),
        "x-server-input-extension-version" => {
            super::display::builtin_x_server_input_extension_version(args)
        }
        "x-server-vendor" => super::display::builtin_x_server_vendor(args),
        "x-display-grayscale-p" => super::display::builtin_x_display_grayscale_p(args),
        "x-display-backing-store" => super::display::builtin_x_display_backing_store(args),
        "x-display-color-cells" => super::display::builtin_x_display_color_cells(args),
        "x-display-mm-height" => super::display::builtin_x_display_mm_height(args),
        "x-display-mm-width" => super::display::builtin_x_display_mm_width(args),
        "x-display-monitor-attributes-list" => {
            super::display::builtin_x_display_monitor_attributes_list(args)
        }
        "x-display-planes" => super::display::builtin_x_display_planes(args),
        "x-display-save-under" => super::display::builtin_x_display_save_under(args),
        "x-display-screens" => super::display::builtin_x_display_screens(args),
        "x-display-set-last-user-time" => {
            super::display::builtin_x_display_set_last_user_time(args)
        }
        "x-display-visual-class" => super::display::builtin_x_display_visual_class(args),
        "x-win-suspend-error" => super::display::builtin_x_win_suspend_error(args),
        "x-wm-set-size-hint" => super::display::builtin_x_wm_set_size_hint(args),

        // Image (pure)
        "image-type-available-p" => super::image::builtin_image_type_available_p(args),
        "create-image" => super::image::builtin_create_image(args),
        "image-size" => super::image::builtin_image_size(args),
        "image-mask-p" => super::image::builtin_image_mask_p(args),
        "put-image" => super::image::builtin_put_image(args),
        "insert-image" => super::image::builtin_insert_image(args),
        "remove-images" => super::image::builtin_remove_images(args),
        "image-flush" => super::image::builtin_image_flush(args),
        "clear-image-cache" => super::image::builtin_clear_image_cache(args),
        "image-cache-size" => super::image::builtin_image_cache_size(args),
        "image-metadata" => super::image::builtin_image_metadata(args),
        "imagep" => super::image::builtin_imagep(args),
        "image-type" => super::image::builtin_image_type(args),
        "image-transforms-p" => super::image::builtin_image_transforms_p(args),

        // Character encoding
        "char-width" => crate::encoding::builtin_char_width(args),
        "string-bytes" => crate::encoding::builtin_string_bytes(args),
        "multibyte-string-p" => crate::encoding::builtin_multibyte_string_p(args),
        "encode-coding-string" => crate::encoding::builtin_encode_coding_string(args),
        "decode-coding-string" => crate::encoding::builtin_decode_coding_string(args),
        "char-or-string-p" => crate::encoding::builtin_char_or_string_p(args),
        "max-char" => crate::encoding::builtin_max_char(args),

        // Extra builtins
        "remove" => super::builtins_extra::builtin_remove(args),
        "remq" => super::builtins_extra::builtin_remq(args),
        "flatten-tree" => super::builtins_extra::builtin_flatten_tree(args),
        "take" => super::builtins_extra::builtin_take(args),
        "assoc-string" => super::builtins_extra::builtin_assoc_string(args),
        "seq-uniq" => super::builtins_extra::builtin_seq_uniq(args),
        "seq-length" => super::builtins_extra::builtin_seq_length(args),
        "seq-into" => super::builtins_extra::builtin_seq_into(args),
        "string-empty-p" => super::builtins_extra::builtin_string_empty_p(args),
        "string-blank-p" => super::builtins_extra::builtin_string_blank_p(args),
        "string-replace" => super::builtins_extra::builtin_string_replace(args),
        "string-search" => super::builtins_extra::builtin_string_search(args),
        "string-to-vector" => super::builtins_extra::builtin_string_to_vector(args),
        "bare-symbol" => super::builtins_extra::builtin_bare_symbol(args),
        "bare-symbol-p" => super::builtins_extra::builtin_bare_symbol_p(args),
        "byteorder" => super::builtins_extra::builtin_byteorder(args),
        "car-less-than-car" => super::builtins_extra::builtin_car_less_than_car(args),
        "proper-list-p" => super::builtins_extra::builtin_proper_list_p(args),
        "subrp" => super::builtins_extra::builtin_subrp(args),
        "byte-code-function-p" => super::builtins_extra::builtin_byte_code_function_p(args),
        "compiled-function-p" => super::builtins_extra::builtin_compiled_function_p(args),
        "closurep" => super::builtins_extra::builtin_closurep(args),
        "natnump" => super::builtins_extra::builtin_natnump(args),
        "fixnump" => super::builtins_extra::builtin_fixnump(args),
        "bignump" => super::builtins_extra::builtin_bignump(args),
        "wholenump" => super::builtins_extra::builtin_wholenump(args),
        "zerop" => super::builtins_extra::builtin_zerop(args),
        "user-login-name" => super::builtins_extra::builtin_user_login_name(args),
        "user-real-login-name" => super::builtins_extra::builtin_user_real_login_name(args),
        "user-full-name" => super::builtins_extra::builtin_user_full_name(args),
        "system-name" => super::builtins_extra::builtin_system_name(args),
        "emacs-version" => super::builtins_extra::builtin_emacs_version(args),
        "emacs-pid" => super::builtins_extra::builtin_emacs_pid(args),
        "garbage-collect" => super::builtins_extra::builtin_garbage_collect(args),
        "memory-use-counts" => super::builtins_extra::builtin_memory_use_counts(args),
        // Note: overlayp is in the eval-dependent section above

        // Autoload (pure)
        "autoloadp" => super::autoload::builtin_autoloadp(args),
        "symbol-file" => super::autoload::builtin_symbol_file(args),

        // Time/date (pure)
        "time-add" => super::timefns::builtin_time_add(args),
        "time-subtract" => super::timefns::builtin_time_subtract(args),
        "time-less-p" => super::timefns::builtin_time_less_p(args),
        "time-equal-p" => super::timefns::builtin_time_equal_p(args),
        "current-time-string" => super::timefns::builtin_current_time_string(args),
        "current-time-zone" => super::timefns::builtin_current_time_zone(args),
        "encode-time" => super::timefns::builtin_encode_time(args),
        "decode-time" => super::timefns::builtin_decode_time(args),
        "time-convert" => super::timefns::builtin_time_convert(args),
        "set-time-zone-rule" => super::timefns::builtin_set_time_zone_rule(args),
        "safe-date-to-time" => super::timefns::builtin_safe_date_to_time(args),

        // Float/math (pure)
        "copysign" => super::floatfns::builtin_copysign(args),
        "frexp" => super::floatfns::builtin_frexp(args),
        "ldexp" => super::floatfns::builtin_ldexp(args),
        "logb" => super::floatfns::builtin_logb(args),
        "fceiling" => super::floatfns::builtin_fceiling(args),
        "ffloor" => super::floatfns::builtin_ffloor(args),
        "fround" => super::floatfns::builtin_fround(args),
        "ftruncate" => super::floatfns::builtin_ftruncate(args),

        // Case/char (pure)
        "capitalize" => super::casefiddle::builtin_capitalize(args),
        "upcase-initials" => super::casefiddle::builtin_upcase_initials(args),
        "char-resolve-modifiers" => super::casefiddle::builtin_char_resolve_modifiers(args),

        // Font/face (pure)
        "fontp" => super::font::builtin_fontp(args),
        "font-spec" => super::font::builtin_font_spec(args),
        "font-get" => super::font::builtin_font_get(args),
        "font-put" => super::font::builtin_font_put(args),
        "list-fonts" => super::font::builtin_list_fonts(args),
        "find-font" => super::font::builtin_find_font(args),
        "clear-font-cache" => super::font::builtin_clear_font_cache(args),
        "font-family-list" => super::font::builtin_font_family_list(args),
        "font-xlfd-name" => super::font::builtin_font_xlfd_name(args),
        "close-font" => super::font::builtin_close_font(args),
        "internal-make-lisp-face" => super::font::builtin_internal_make_lisp_face(args),
        "internal-lisp-face-p" => super::font::builtin_internal_lisp_face_p(args),
        "internal-copy-lisp-face" => super::font::builtin_internal_copy_lisp_face(args),
        "internal-set-lisp-face-attribute" => {
            super::font::builtin_internal_set_lisp_face_attribute(args)
        }
        "internal-get-lisp-face-attribute" => {
            super::font::builtin_internal_get_lisp_face_attribute(args)
        }
        "internal-lisp-face-attribute-values" => {
            super::font::builtin_internal_lisp_face_attribute_values(args)
        }
        "internal-lisp-face-equal-p" => super::font::builtin_internal_lisp_face_equal_p(args),
        "internal-lisp-face-empty-p" => super::font::builtin_internal_lisp_face_empty_p(args),
        "internal-merge-in-global-face" => super::font::builtin_internal_merge_in_global_face(args),
        "face-attribute-relative-p" => super::font::builtin_face_attribute_relative_p(args),
        "merge-face-attribute" => super::font::builtin_merge_face_attribute(args),
        "face-list" => super::font::builtin_face_list(args),
        "color-defined-p" => super::font::builtin_color_defined_p(args),
        "color-gray-p" => super::font::builtin_color_gray_p(args),
        "color-supported-p" => super::font::builtin_color_supported_p(args),
        "color-distance" => super::font::builtin_color_distance(args),
        "color-values" => super::font::builtin_color_values(args),
        "color-values-from-color-spec" => super::font::builtin_color_values_from_color_spec(args),
        "defined-colors" => super::font::builtin_defined_colors(args),
        "face-id" => super::font::builtin_face_id(args),
        "face-font" => super::font::builtin_face_font(args),
        "internal-face-x-get-resource" => super::font::builtin_internal_face_x_get_resource(args),
        "internal-set-font-selection-order" => {
            super::font::builtin_internal_set_font_selection_order(args)
        }
        "internal-set-alternative-font-family-alist" => {
            super::font::builtin_internal_set_alternative_font_family_alist(args)
        }
        "internal-set-alternative-font-registry-alist" => {
            super::font::builtin_internal_set_alternative_font_registry_alist(args)
        }

        // Directory/file attributes (pure)
        "directory-files-and-attributes" => {
            super::dired::builtin_directory_files_and_attributes(args)
        }
        "file-name-completion" => super::dired::builtin_file_name_completion(args),
        "file-name-all-completions" => super::dired::builtin_file_name_all_completions(args),
        "file-attributes-lessp" => super::dired::builtin_file_attributes_lessp(args),
        "system-users" => super::dired::builtin_system_users(args),
        "system-groups" => super::dired::builtin_system_groups(args),

        // Display engine (pure)
        "format-mode-line" => super::xdisp::builtin_format_mode_line(args),
        "invisible-p" => super::xdisp::builtin_invisible_p(args),
        "line-pixel-height" => super::xdisp::builtin_line_pixel_height(args),
        "window-text-pixel-size" => super::xdisp::builtin_window_text_pixel_size(args),
        "pos-visible-in-window-p" => super::xdisp::builtin_pos_visible_in_window_p(args),
        "move-point-visually" => super::xdisp::builtin_move_point_visually(args),
        "lookup-image-map" => super::xdisp::builtin_lookup_image_map(args),
        "current-bidi-paragraph-direction" => {
            super::xdisp::builtin_current_bidi_paragraph_direction(args)
        }
        "bidi-resolved-levels" => super::xdisp::builtin_bidi_resolved_levels(args),
        "bidi-find-overridden-directionality" => {
            super::xdisp::builtin_bidi_find_overridden_directionality(args)
        }
        "move-to-window-line" => super::xdisp::builtin_move_to_window_line(args),
        "tool-bar-height" => super::xdisp::builtin_tool_bar_height(args),
        "tab-bar-height" => super::xdisp::builtin_tab_bar_height(args),
        "line-number-display-width" => super::xdisp::builtin_line_number_display_width(args),
        "long-line-optimizations-p" => super::xdisp::builtin_long_line_optimizations_p(args),

        // Charset (pure)
        "charsetp" => super::charset::builtin_charsetp(args),
        "charset-priority-list" => super::charset::builtin_charset_priority_list(args),
        "set-charset-priority" => super::charset::builtin_set_charset_priority(args),
        "char-charset" => super::charset::builtin_char_charset(args),
        "charset-plist" => super::charset::builtin_charset_plist(args),
        "charset-id-internal" => super::charset::builtin_charset_id_internal(args),
        "define-charset-alias" => super::charset::builtin_define_charset_alias(args),
        "define-charset-internal" => super::charset::builtin_define_charset_internal(args),
        "declare-equiv-charset" => super::charset::builtin_declare_equiv_charset(args),
        "find-charset-region" => super::charset::builtin_find_charset_region(args),
        "find-charset-string" => super::charset::builtin_find_charset_string(args),
        "decode-big5-char" => super::charset::builtin_decode_big5_char(args),
        "decode-char" => super::charset::builtin_decode_char(args),
        "decode-sjis-char" => super::charset::builtin_decode_sjis_char(args),
        "encode-big5-char" => super::charset::builtin_encode_big5_char(args),
        "encode-char" => super::charset::builtin_encode_char(args),
        "encode-sjis-char" => super::charset::builtin_encode_sjis_char(args),
        "get-unused-iso-final-char" => super::charset::builtin_get_unused_iso_final_char(args),
        "clear-charset-maps" => super::charset::builtin_clear_charset_maps(args),
        "charset-after" => super::charset::builtin_charset_after(args),

        // CCL (pure)
        "ccl-program-p" => super::ccl::builtin_ccl_program_p(args),
        "ccl-execute" => super::ccl::builtin_ccl_execute(args),
        "ccl-execute-on-string" => super::ccl::builtin_ccl_execute_on_string(args),
        "register-ccl-program" => super::ccl::builtin_register_ccl_program(args),
        "register-code-conversion-map" => super::ccl::builtin_register_code_conversion_map(args),

        // XML/decompress (pure)
        "libxml-parse-html-region" => super::xml::builtin_libxml_parse_html_region(args),
        "libxml-parse-xml-region" => super::xml::builtin_libxml_parse_xml_region(args),
        "libxml-available-p" => super::xml::builtin_libxml_available_p(args),
        "zlib-available-p" => super::xml::builtin_zlib_available_p(args),
        "zlib-decompress-region" => super::xml::builtin_zlib_decompress_region(args),

        // Custom system (pure)
        "custom-set-faces" => super::custom::builtin_custom_set_faces(args),

        // Internal compatibility surface (pure)
        "define-fringe-bitmap" => super::compat_internal::builtin_define_fringe_bitmap(args),
        "destroy-fringe-bitmap" => super::compat_internal::builtin_destroy_fringe_bitmap(args),
        "display--line-is-continued-p" => {
            super::compat_internal::builtin_display_line_is_continued_p(args)
        }
        "display--update-for-mouse-movement" => {
            super::compat_internal::builtin_display_update_for_mouse_movement(args)
        }
        "do-auto-save" => super::compat_internal::builtin_do_auto_save(args),
        "external-debugging-output" => {
            super::compat_internal::builtin_external_debugging_output(args)
        }
        "describe-buffer-bindings" => {
            super::compat_internal::builtin_describe_buffer_bindings(args)
        }
        "describe-vector" => super::compat_internal::builtin_describe_vector(args),
        "delete-terminal" => super::compat_internal::builtin_delete_terminal(args),
        "face-attributes-as-vector" => {
            super::compat_internal::builtin_face_attributes_as_vector(args)
        }
        "font-at" => super::compat_internal::builtin_font_at(args),
        "font-face-attributes" => super::compat_internal::builtin_font_face_attributes(args),
        "font-get-glyphs" => super::compat_internal::builtin_font_get_glyphs(args),
        "font-get-system-font" => super::compat_internal::builtin_font_get_system_font(args),
        "font-get-system-normal-font" => {
            super::compat_internal::builtin_font_get_system_normal_font(args)
        }
        "font-has-char-p" => super::compat_internal::builtin_font_has_char_p(args),
        "font-info" => super::compat_internal::builtin_font_info(args),
        "font-match-p" => super::compat_internal::builtin_font_match_p(args),
        "font-shape-gstring" => super::compat_internal::builtin_font_shape_gstring(args),
        "font-variation-glyphs" => super::compat_internal::builtin_font_variation_glyphs(args),
        "fontset-font" => super::compat_internal::builtin_fontset_font(args),
        "fontset-info" => super::compat_internal::builtin_fontset_info(args),
        "fontset-list" => super::compat_internal::builtin_fontset_list(args),
        "frame--set-was-invisible" => super::compat_internal::builtin_frame_set_was_invisible(args),
        "frame-after-make-frame" => super::compat_internal::builtin_frame_after_make_frame(args),
        "frame-ancestor-p" => super::compat_internal::builtin_frame_ancestor_p(args),
        "frame-bottom-divider-width" => {
            super::compat_internal::builtin_frame_bottom_divider_width(args)
        }
        "frame-child-frame-border-width" => {
            super::compat_internal::builtin_frame_child_frame_border_width(args)
        }
        "frame-focus" => super::compat_internal::builtin_frame_focus(args),
        "frame-font-cache" => super::compat_internal::builtin_frame_font_cache(args),
        "frame--face-hash-table" => super::compat_internal::builtin_frame_face_hash_table(args),
        "frame-fringe-width" => super::compat_internal::builtin_frame_fringe_width(args),
        "frame-internal-border-width" => {
            super::compat_internal::builtin_frame_internal_border_width(args)
        }
        "frame-old-selected-window" => {
            super::compat_internal::builtin_frame_old_selected_window(args)
        }
        "frame-or-buffer-changed-p" => {
            super::compat_internal::builtin_frame_or_buffer_changed_p(args)
        }
        "frame-parent" => super::compat_internal::builtin_frame_parent(args),
        "frame-pointer-visible-p" => super::compat_internal::builtin_frame_pointer_visible_p(args),
        "frame-right-divider-width" => {
            super::compat_internal::builtin_frame_right_divider_width(args)
        }
        "frame-scale-factor" => super::compat_internal::builtin_frame_scale_factor(args),
        "frame-scroll-bar-height" => super::compat_internal::builtin_frame_scroll_bar_height(args),
        "frame-scroll-bar-width" => super::compat_internal::builtin_frame_scroll_bar_width(args),
        "frame-window-state-change" => {
            super::compat_internal::builtin_frame_window_state_change(args)
        }
        "fringe-bitmaps-at-pos" => super::compat_internal::builtin_fringe_bitmaps_at_pos(args),
        "gap-position" => super::compat_internal::builtin_gap_position(args),
        "gap-size" => super::compat_internal::builtin_gap_size(args),
        "garbage-collect-maybe" => super::compat_internal::builtin_garbage_collect_maybe(args),
        "get-unicode-property-internal" => {
            super::compat_internal::builtin_get_unicode_property_internal(args)
        }
        "get-variable-watchers" => super::compat_internal::builtin_get_variable_watchers(args),
        "gnutls-available-p" => super::compat_internal::builtin_gnutls_available_p(args),
        "gnutls-asynchronous-parameters" => {
            super::compat_internal::builtin_gnutls_asynchronous_parameters(args)
        }
        "gnutls-boot" => super::compat_internal::builtin_gnutls_boot(args),
        "gnutls-bye" => super::compat_internal::builtin_gnutls_bye(args),
        "gnutls-ciphers" => super::compat_internal::builtin_gnutls_ciphers(args),
        "gnutls-deinit" => super::compat_internal::builtin_gnutls_deinit(args),
        "gnutls-digests" => super::compat_internal::builtin_gnutls_digests(args),
        "gnutls-error-fatalp" => super::compat_internal::builtin_gnutls_error_fatalp(args),
        "gnutls-error-string" => super::compat_internal::builtin_gnutls_error_string(args),
        "gnutls-errorp" => super::compat_internal::builtin_gnutls_errorp(args),
        "gnutls-format-certificate" => {
            super::compat_internal::builtin_gnutls_format_certificate(args)
        }
        "gnutls-get-initstage" => super::compat_internal::builtin_gnutls_get_initstage(args),
        "gnutls-hash-digest" => super::compat_internal::builtin_gnutls_hash_digest(args),
        "gnutls-hash-mac" => super::compat_internal::builtin_gnutls_hash_mac(args),
        "gnutls-macs" => super::compat_internal::builtin_gnutls_macs(args),
        "gnutls-peer-status" => super::compat_internal::builtin_gnutls_peer_status(args),
        "gnutls-peer-status-warning-describe" => {
            super::compat_internal::builtin_gnutls_peer_status_warning_describe(args)
        }
        "gnutls-symmetric-decrypt" => {
            super::compat_internal::builtin_gnutls_symmetric_decrypt(args)
        }
        "gnutls-symmetric-encrypt" => {
            super::compat_internal::builtin_gnutls_symmetric_encrypt(args)
        }
        "gpm-mouse-start" => super::compat_internal::builtin_gpm_mouse_start(args),
        "gpm-mouse-stop" => super::compat_internal::builtin_gpm_mouse_stop(args),
        "handle-save-session" => super::compat_internal::builtin_handle_save_session(args),
        "handle-switch-frame" => super::compat_internal::builtin_handle_switch_frame(args),
        "help--describe-vector" => super::compat_internal::builtin_help_describe_vector(args),
        "init-image-library" => super::compat_internal::builtin_init_image_library(args),
        "internal--define-uninitialized-variable" => {
            super::compat_internal::builtin_internal_define_uninitialized_variable(args)
        }
        "internal--labeled-narrow-to-region" => {
            super::compat_internal::builtin_internal_labeled_narrow_to_region(args)
        }
        "internal--labeled-widen" => super::compat_internal::builtin_internal_labeled_widen(args),
        "internal--obarray-buckets" => {
            super::compat_internal::builtin_internal_obarray_buckets(args)
        }
        "internal--set-buffer-modified-tick" => {
            super::compat_internal::builtin_internal_set_buffer_modified_tick(args)
        }
        "internal--before-save-selected-window" => {
            builtin_internal_before_save_selected_window(eval, args)
        }
        "internal--after-save-selected-window" => {
            builtin_internal_after_save_selected_window(eval, args)
        }
        "internal--track-mouse" => super::compat_internal::builtin_internal_track_mouse(args),
        "internal-char-font" => super::compat_internal::builtin_internal_char_font(args),
        "internal-complete-buffer" => {
            super::compat_internal::builtin_internal_complete_buffer(args)
        }
        "internal-describe-syntax-value" => {
            super::compat_internal::builtin_internal_describe_syntax_value(args)
        }
        "internal-event-symbol-parse-modifiers" => {
            super::compat_internal::builtin_internal_event_symbol_parse_modifiers(args)
        }
        "internal-handle-focus-in" => {
            super::compat_internal::builtin_internal_handle_focus_in(args)
        }
        "internal-make-var-non-special" => {
            super::compat_internal::builtin_internal_make_var_non_special(args)
        }
        "internal-set-lisp-face-attribute-from-resource" => {
            super::compat_internal::builtin_internal_set_lisp_face_attribute_from_resource(args)
        }
        "internal-stack-stats" => super::compat_internal::builtin_internal_stack_stats(args),
        "internal-subr-documentation" => {
            super::compat_internal::builtin_internal_subr_documentation(args)
        }
        "byte-code" => super::compat_internal::builtin_byte_code(args),
        "decode-coding-region" => super::compat_internal::builtin_decode_coding_region(args),
        "defconst-1" => super::compat_internal::builtin_defconst_1(args),
        "define-coding-system-internal" => {
            super::compat_internal::builtin_define_coding_system_internal(args)
        }
        "defvar-1" => super::compat_internal::builtin_defvar_1(args),
        "defvaralias" => super::compat_internal::builtin_defvaralias(args),
        "dump-emacs-portable" => super::compat_internal::builtin_dump_emacs_portable(args),
        "dump-emacs-portable--sort-predicate" => {
            super::compat_internal::builtin_dump_emacs_portable_sort_predicate(args)
        }
        "dump-emacs-portable--sort-predicate-copied" => {
            super::compat_internal::builtin_dump_emacs_portable_sort_predicate_copied(args)
        }
        "encode-coding-region" => super::compat_internal::builtin_encode_coding_region(args),
        "find-operation-coding-system" => {
            super::compat_internal::builtin_find_operation_coding_system(args)
        }
        "handler-bind-1" => super::compat_internal::builtin_handler_bind_1(args),
        "indirect-variable" => super::compat_internal::builtin_indirect_variable(args),
        "insert-and-inherit" => super::compat_internal::builtin_insert_and_inherit(args),
        "insert-before-markers-and-inherit" => {
            super::compat_internal::builtin_insert_before_markers_and_inherit(args)
        }
        "insert-buffer-substring" => super::compat_internal::builtin_insert_buffer_substring(args),
        "iso-charset" => super::compat_internal::builtin_iso_charset(args),
        "keymap--get-keyelt" => super::compat_internal::builtin_keymap_get_keyelt(args),
        "keymap-prompt" => super::compat_internal::builtin_keymap_prompt(args),
        "kill-all-local-variables" => {
            super::compat_internal::builtin_kill_all_local_variables(args)
        }
        "kill-emacs" => super::compat_internal::builtin_kill_emacs(args),
        "lower-frame" => super::compat_internal::builtin_lower_frame(args),
        "lread--substitute-object-in-subtree" => {
            super::compat_internal::builtin_lread_substitute_object_in_subtree(args)
        }
        "macroexpand" => super::compat_internal::builtin_macroexpand(args),
        "malloc-info" => super::compat_internal::builtin_malloc_info(args),
        "malloc-trim" => super::compat_internal::builtin_malloc_trim(args),
        "make-byte-code" => super::compat_internal::builtin_make_byte_code(args),
        "make-char" => super::compat_internal::builtin_make_char(args),
        "make-closure" => super::compat_internal::builtin_make_closure(args),
        "make-finalizer" => super::compat_internal::builtin_make_finalizer(args),
        "marker-last-position" => super::compat_internal::builtin_marker_last_position(args),
        "make-indirect-buffer" => super::compat_internal::builtin_make_indirect_buffer(args),
        "make-interpreted-closure" => {
            super::compat_internal::builtin_make_interpreted_closure(args)
        }
        "make-record" => super::compat_internal::builtin_make_record(args),
        "make-temp-file-internal" => super::compat_internal::builtin_make_temp_file_internal(args),
        "map-charset-chars" => super::compat_internal::builtin_map_charset_chars(args),
        "map-keymap" => super::compat_internal::builtin_map_keymap(args),
        "map-keymap-internal" => super::compat_internal::builtin_map_keymap_internal(args),
        "mapbacktrace" => super::compat_internal::builtin_mapbacktrace(args),
        "match-data--translate" => super::compat_internal::builtin_match_data_translate(args),
        "memory-info" => super::compat_internal::builtin_memory_info(args),
        "make-frame-invisible" => super::compat_internal::builtin_make_frame_invisible(args),
        "make-terminal-frame" => super::compat_internal::builtin_make_terminal_frame(args),
        "menu-bar-menu-at-x-y" => super::compat_internal::builtin_menu_bar_menu_at_x_y(args),
        "menu-or-popup-active-p" => super::compat_internal::builtin_menu_or_popup_active_p(args),
        "minibuffer-innermost-command-loop-p" => {
            super::compat_internal::builtin_minibuffer_innermost_command_loop_p(args)
        }
        "minibuffer-prompt-end" => super::compat_internal::builtin_minibuffer_prompt_end(args),
        "module-load" => super::compat_internal::builtin_module_load(args),
        "mouse-pixel-position" => super::compat_internal::builtin_mouse_pixel_position(args),
        "mouse-position" => super::compat_internal::builtin_mouse_position(args),
        "newline-cache-check" => super::compat_internal::builtin_newline_cache_check(args),
        "native-comp-available-p" => super::compat_internal::builtin_native_comp_available_p(args),
        "native-comp-unit-file" => super::compat_internal::builtin_native_comp_unit_file(args),
        "native-comp-unit-set-file" => {
            super::compat_internal::builtin_native_comp_unit_set_file(args)
        }
        "native-elisp-load" => super::compat_internal::builtin_native_elisp_load(args),
        "new-fontset" => super::compat_internal::builtin_new_fontset(args),
        "next-frame" => super::compat_internal::builtin_next_frame(args),
        "ntake" => super::compat_internal::builtin_ntake(args),
        "obarray-clear" => super::compat_internal::builtin_obarray_clear(args),
        "obarray-make" => super::compat_internal::builtin_obarray_make(args),
        "object-intervals" => super::compat_internal::builtin_object_intervals(args),
        "old-selected-frame" => super::compat_internal::builtin_old_selected_frame(args),
        "old-selected-window" => super::compat_internal::builtin_old_selected_window(args),
        "open-dribble-file" => super::compat_internal::builtin_open_dribble_file(args),
        "open-font" => super::compat_internal::builtin_open_font(args),
        "optimize-char-table" => super::compat_internal::builtin_optimize_char_table(args),
        "overlay-lists" => super::compat_internal::builtin_overlay_lists(args),
        "overlay-recenter" => super::compat_internal::builtin_overlay_recenter(args),
        "pdumper-stats" => super::compat_internal::builtin_pdumper_stats(args),
        "play-sound-internal" => super::compat_internal::builtin_play_sound_internal(args),
        "position-symbol" => super::compat_internal::builtin_position_symbol(args),
        "posn-at-point" => super::compat_internal::builtin_posn_at_point(args),
        "posn-at-x-y" => super::compat_internal::builtin_posn_at_x_y(args),
        "previous-frame" => super::compat_internal::builtin_previous_frame(args),
        "profiler-cpu-log" => super::compat_internal::builtin_profiler_cpu_log(args),
        "profiler-cpu-running-p" => super::compat_internal::builtin_profiler_cpu_running_p(args),
        "profiler-cpu-start" => super::compat_internal::builtin_profiler_cpu_start(args),
        "profiler-cpu-stop" => super::compat_internal::builtin_profiler_cpu_stop(args),
        "profiler-memory-log" => super::compat_internal::builtin_profiler_memory_log(args),
        "profiler-memory-running-p" => {
            super::compat_internal::builtin_profiler_memory_running_p(args)
        }
        "profiler-memory-start" => super::compat_internal::builtin_profiler_memory_start(args),
        "profiler-memory-stop" => super::compat_internal::builtin_profiler_memory_stop(args),
        "put-unicode-property-internal" => {
            super::compat_internal::builtin_put_unicode_property_internal(args)
        }
        "query-font" => super::compat_internal::builtin_query_font(args),
        "query-fontset" => super::compat_internal::builtin_query_fontset(args),
        "raise-frame" => super::compat_internal::builtin_raise_frame(args),
        "read-positioning-symbols" => {
            super::compat_internal::builtin_read_positioning_symbols(args)
        }
        "re--describe-compiled" => super::compat_internal::builtin_re_describe_compiled(args),
        "recent-auto-save-p" => super::compat_internal::builtin_recent_auto_save_p(args),
        "redisplay" => super::compat_internal::builtin_redisplay(args),
        "record" => super::compat_internal::builtin_record(args),
        "recordp" => super::compat_internal::builtin_recordp(args),
        "reconsider-frame-fonts" => super::compat_internal::builtin_reconsider_frame_fonts(args),
        "redirect-debugging-output" => {
            super::compat_internal::builtin_redirect_debugging_output(args)
        }
        "redirect-frame-focus" => super::compat_internal::builtin_redirect_frame_focus(args),
        "remove-pos-from-symbol" => super::compat_internal::builtin_remove_pos_from_symbol(args),
        "rename-buffer" => super::compat_internal::builtin_rename_buffer(args),
        "replace-buffer-contents" => super::compat_internal::builtin_replace_buffer_contents(args),
        "resize-mini-window-internal" => {
            super::compat_internal::builtin_resize_mini_window_internal(args)
        }
        "restore-buffer-modified-p" => {
            super::compat_internal::builtin_restore_buffer_modified_p(args)
        }
        "set--this-command-keys" => super::compat_internal::builtin_set_this_command_keys(args),
        "set-buffer-auto-saved" => super::compat_internal::builtin_set_buffer_auto_saved(args),
        "set-buffer-major-mode" => super::compat_internal::builtin_set_buffer_major_mode(args),
        "set-buffer-multibyte" => super::compat_internal::builtin_set_buffer_multibyte(args),
        "set-buffer-redisplay" => super::compat_internal::builtin_set_buffer_redisplay(args),
        "set-charset-plist" => super::compat_internal::builtin_set_charset_plist(args),
        "set-fontset-font" => super::compat_internal::builtin_set_fontset_font(args),
        "set-frame-selected-window" => {
            super::compat_internal::builtin_set_frame_selected_window(args)
        }
        "set-frame-window-state-change" => {
            super::compat_internal::builtin_set_frame_window_state_change(args)
        }
        "set-fringe-bitmap-face" => super::compat_internal::builtin_set_fringe_bitmap_face(args),
        "set-minibuffer-window" => super::compat_internal::builtin_set_minibuffer_window(args),
        "set-mouse-pixel-position" => {
            super::compat_internal::builtin_set_mouse_pixel_position(args)
        }
        "set-mouse-position" => super::compat_internal::builtin_set_mouse_position(args),
        "set-window-combination-limit" => {
            super::compat_internal::builtin_set_window_combination_limit(args)
        }
        "set-window-new-normal" => super::compat_internal::builtin_set_window_new_normal(args),
        "set-window-new-pixel" => super::compat_internal::builtin_set_window_new_pixel(args),
        "set-window-new-total" => super::compat_internal::builtin_set_window_new_total(args),
        "setplist" => super::compat_internal::builtin_setplist(args),
        "sort-charsets" => super::compat_internal::builtin_sort_charsets(args),
        "split-char" => super::compat_internal::builtin_split_char(args),
        "split-window-internal" => super::compat_internal::builtin_split_window_internal(args),
        "string-distance" => super::compat_internal::builtin_string_distance(args),
        "subst-char-in-region" => super::compat_internal::builtin_subst_char_in_region(args),
        "subr-native-comp-unit" => super::compat_internal::builtin_subr_native_comp_unit(args),
        "subr-native-lambda-list" => super::compat_internal::builtin_subr_native_lambda_list(args),
        "subr-type" => super::compat_internal::builtin_subr_type(args),
        "suspend-emacs" => super::compat_internal::builtin_suspend_emacs(args),
        "this-single-command-keys" => {
            super::compat_internal::builtin_this_single_command_keys(args)
        }
        "this-single-command-raw-keys" => {
            super::compat_internal::builtin_this_single_command_raw_keys(args)
        }
        "thread--blocker" => super::compat_internal::builtin_thread_blocker(args),
        "tool-bar-get-system-style" => {
            super::compat_internal::builtin_tool_bar_get_system_style(args)
        }
        "tool-bar-pixel-width" => super::compat_internal::builtin_tool_bar_pixel_width(args),
        "translate-region-internal" => {
            super::compat_internal::builtin_translate_region_internal(args)
        }
        "transpose-regions" => super::compat_internal::builtin_transpose_regions(args),
        "tty--output-buffer-size" => super::compat_internal::builtin_tty_output_buffer_size(args),
        "tty--set-output-buffer-size" => {
            super::compat_internal::builtin_tty_set_output_buffer_size(args)
        }
        "tty-suppress-bold-inverse-default-colors" => {
            super::compat_internal::builtin_tty_suppress_bold_inverse_default_colors(args)
        }
        "unencodable-char-position" => {
            super::compat_internal::builtin_unencodable_char_position(args)
        }
        "unicode-property-table-internal" => {
            super::compat_internal::builtin_unicode_property_table_internal(args)
        }
        "unify-charset" => super::compat_internal::builtin_unify_charset(args),
        "unix-sync" => super::compat_internal::builtin_unix_sync(args),
        "value<" => super::compat_internal::builtin_value_lt(args),
        "variable-binding-locus" => super::compat_internal::builtin_variable_binding_locus(args),
        "vertical-motion" => super::compat_internal::builtin_vertical_motion(args),
        "x-begin-drag" => super::compat_internal::builtin_x_begin_drag(args),
        "x-create-frame" => super::compat_internal::builtin_x_create_frame(args),
        "x-double-buffered-p" => super::compat_internal::builtin_x_double_buffered_p(args),
        "x-menu-bar-open-internal" => {
            super::compat_internal::builtin_x_menu_bar_open_internal(args)
        }
        "xw-color-defined-p" => super::compat_internal::builtin_xw_color_defined_p(args),
        "xw-color-values" => super::compat_internal::builtin_xw_color_values(args),
        "xw-display-color-p" => super::compat_internal::builtin_xw_display_color_p(args),
        "innermost-minibuffer-p" => super::compat_internal::builtin_innermost_minibuffer_p(args),
        "interactive-form" => super::compat_internal::builtin_interactive_form(args),
        "inotify-add-watch" => super::compat_internal::builtin_inotify_add_watch(args),
        "inotify-rm-watch" => super::compat_internal::builtin_inotify_rm_watch(args),
        "inotify-valid-p" => super::compat_internal::builtin_inotify_valid_p(args),
        "local-variable-if-set-p" => super::compat_internal::builtin_local_variable_if_set_p(args),
        "lock-buffer" => super::compat_internal::builtin_lock_buffer(args),
        "lock-file" => super::compat_internal::builtin_lock_file(args),
        "lossage-size" => super::compat_internal::builtin_lossage_size(args),
        "unlock-buffer" => super::compat_internal::builtin_unlock_buffer(args),
        "unlock-file" => super::compat_internal::builtin_unlock_file(args),
        "window-at" => super::compat_internal::builtin_window_at(args),
        "window-bottom-divider-width" => {
            super::compat_internal::builtin_window_bottom_divider_width(args)
        }
        "window-bump-use-time" => super::compat_internal::builtin_window_bump_use_time(args),
        "window-combination-limit" => {
            super::compat_internal::builtin_window_combination_limit(args)
        }
        "window-left-child" => super::compat_internal::builtin_window_left_child(args),
        "window-line-height" => super::compat_internal::builtin_window_line_height(args),
        "window-lines-pixel-dimensions" => {
            super::compat_internal::builtin_window_lines_pixel_dimensions(args)
        }
        "window-list-1" => super::compat_internal::builtin_window_list_1(args),
        "window-new-normal" => super::compat_internal::builtin_window_new_normal(args),
        "window-new-pixel" => super::compat_internal::builtin_window_new_pixel(args),
        "window-new-total" => super::compat_internal::builtin_window_new_total(args),
        "window-next-sibling" => super::compat_internal::builtin_window_next_sibling(args),
        "window-normal-size" => super::compat_internal::builtin_window_normal_size(args),
        "window-old-body-pixel-height" => {
            super::compat_internal::builtin_window_old_body_pixel_height(args)
        }
        "window-old-body-pixel-width" => {
            super::compat_internal::builtin_window_old_body_pixel_width(args)
        }
        "window-old-pixel-height" => super::compat_internal::builtin_window_old_pixel_height(args),
        "window-old-pixel-width" => super::compat_internal::builtin_window_old_pixel_width(args),
        "window-parent" => super::compat_internal::builtin_window_parent(args),
        "window-pixel-left" => super::compat_internal::builtin_window_pixel_left(args),
        "window-pixel-top" => super::compat_internal::builtin_window_pixel_top(args),
        "window-prev-sibling" => super::compat_internal::builtin_window_prev_sibling(args),
        "window-resize-apply" => super::compat_internal::builtin_window_resize_apply(args),
        "window-resize-apply-total" => {
            super::compat_internal::builtin_window_resize_apply_total(args)
        }
        "window-right-divider-width" => {
            super::compat_internal::builtin_window_right_divider_width(args)
        }
        "window-scroll-bar-height" => {
            super::compat_internal::builtin_window_scroll_bar_height(args)
        }
        "window-scroll-bar-width" => super::compat_internal::builtin_window_scroll_bar_width(args),
        "window-tab-line-height" => super::compat_internal::builtin_window_tab_line_height(args),
        "window-top-child" => super::compat_internal::builtin_window_top_child(args),
        "treesit-available-p" => super::compat_internal::builtin_treesit_available_p(args),
        "treesit-compiled-query-p" => {
            super::compat_internal::builtin_treesit_compiled_query_p(args)
        }
        "treesit-induce-sparse-tree" => {
            super::compat_internal::builtin_treesit_induce_sparse_tree(args)
        }
        "treesit-language-abi-version" => {
            super::compat_internal::builtin_treesit_language_abi_version(args)
        }
        "treesit-language-available-p" => {
            super::compat_internal::builtin_treesit_language_available_p(args)
        }
        "treesit-library-abi-version" => {
            super::compat_internal::builtin_treesit_library_abi_version(args)
        }
        "treesit-node-check" => super::compat_internal::builtin_treesit_node_check(args),
        "treesit-node-child" => super::compat_internal::builtin_treesit_node_child(args),
        "treesit-node-child-by-field-name" => {
            super::compat_internal::builtin_treesit_node_child_by_field_name(args)
        }
        "treesit-node-child-count" => {
            super::compat_internal::builtin_treesit_node_child_count(args)
        }
        "treesit-node-descendant-for-range" => {
            super::compat_internal::builtin_treesit_node_descendant_for_range(args)
        }
        "treesit-node-end" => super::compat_internal::builtin_treesit_node_end(args),
        "treesit-node-eq" => super::compat_internal::builtin_treesit_node_eq(args),
        "treesit-node-field-name-for-child" => {
            super::compat_internal::builtin_treesit_node_field_name_for_child(args)
        }
        "treesit-node-first-child-for-pos" => {
            super::compat_internal::builtin_treesit_node_first_child_for_pos(args)
        }
        "treesit-node-match-p" => super::compat_internal::builtin_treesit_node_match_p(args),
        "treesit-node-next-sibling" => {
            super::compat_internal::builtin_treesit_node_next_sibling(args)
        }
        "treesit-node-p" => super::compat_internal::builtin_treesit_node_p(args),
        "treesit-node-parent" => super::compat_internal::builtin_treesit_node_parent(args),
        "treesit-node-parser" => super::compat_internal::builtin_treesit_node_parser(args),
        "treesit-node-prev-sibling" => {
            super::compat_internal::builtin_treesit_node_prev_sibling(args)
        }
        "treesit-node-start" => super::compat_internal::builtin_treesit_node_start(args),
        "treesit-node-string" => super::compat_internal::builtin_treesit_node_string(args),
        "treesit-node-type" => super::compat_internal::builtin_treesit_node_type(args),
        "treesit-parser-add-notifier" => {
            super::compat_internal::builtin_treesit_parser_add_notifier(args)
        }
        "treesit-parser-buffer" => super::compat_internal::builtin_treesit_parser_buffer(args),
        "treesit-parser-create" => super::compat_internal::builtin_treesit_parser_create(args),
        "treesit-parser-delete" => super::compat_internal::builtin_treesit_parser_delete(args),
        "treesit-parser-included-ranges" => {
            super::compat_internal::builtin_treesit_parser_included_ranges(args)
        }
        "treesit-parser-language" => super::compat_internal::builtin_treesit_parser_language(args),
        "treesit-parser-list" => super::compat_internal::builtin_treesit_parser_list(args),
        "treesit-parser-notifiers" => {
            super::compat_internal::builtin_treesit_parser_notifiers(args)
        }
        "treesit-parser-p" => super::compat_internal::builtin_treesit_parser_p(args),
        "treesit-parser-remove-notifier" => {
            super::compat_internal::builtin_treesit_parser_remove_notifier(args)
        }
        "treesit-parser-root-node" => {
            super::compat_internal::builtin_treesit_parser_root_node(args)
        }
        "treesit-parser-set-included-ranges" => {
            super::compat_internal::builtin_treesit_parser_set_included_ranges(args)
        }
        "treesit-parser-tag" => super::compat_internal::builtin_treesit_parser_tag(args),
        "treesit-pattern-expand" => super::compat_internal::builtin_treesit_pattern_expand(args),
        "treesit-query-capture" => super::compat_internal::builtin_treesit_query_capture(args),
        "treesit-query-compile" => super::compat_internal::builtin_treesit_query_compile(args),
        "treesit-query-expand" => super::compat_internal::builtin_treesit_query_expand(args),
        "treesit-query-language" => super::compat_internal::builtin_treesit_query_language(args),
        "treesit-query-p" => super::compat_internal::builtin_treesit_query_p(args),
        "treesit-search-forward" => super::compat_internal::builtin_treesit_search_forward(args),
        "treesit-search-subtree" => super::compat_internal::builtin_treesit_search_subtree(args),
        "treesit-subtree-stat" => super::compat_internal::builtin_treesit_subtree_stat(args),
        "sqlite-available-p" => super::compat_internal::builtin_sqlite_available_p(args),
        "sqlite-close" => super::compat_internal::builtin_sqlite_close(args),
        "sqlite-columns" => super::compat_internal::builtin_sqlite_columns(args),
        "sqlite-commit" => super::compat_internal::builtin_sqlite_commit(args),
        "sqlite-execute" => super::compat_internal::builtin_sqlite_execute(args),
        "sqlite-execute-batch" => super::compat_internal::builtin_sqlite_execute_batch(args),
        "sqlite-finalize" => super::compat_internal::builtin_sqlite_finalize(args),
        "sqlite-load-extension" => super::compat_internal::builtin_sqlite_load_extension(args),
        "sqlite-more-p" => super::compat_internal::builtin_sqlite_more_p(args),
        "sqlite-next" => super::compat_internal::builtin_sqlite_next(args),
        "sqlite-open" => super::compat_internal::builtin_sqlite_open(args),
        "sqlite-pragma" => super::compat_internal::builtin_sqlite_pragma(args),
        "sqlite-rollback" => super::compat_internal::builtin_sqlite_rollback(args),
        "sqlite-select" => super::compat_internal::builtin_sqlite_select(args),
        "sqlite-transaction" => super::compat_internal::builtin_sqlite_transaction(args),
        "sqlite-version" => super::compat_internal::builtin_sqlite_version(args),
        "sqlitep" => super::compat_internal::builtin_sqlitep(args),
        "fillarray" => super::compat_internal::builtin_fillarray(args),
        "define-hash-table-test" => super::compat_internal::builtin_define_hash_table_test(args),
        "find-coding-systems-region-internal" => {
            super::compat_internal::builtin_find_coding_systems_region_internal(args)
        }

        // Native compilation compatibility (pure)
        "comp--compile-ctxt-to-file0" => super::comp::builtin_comp_compile_ctxt_to_file0(args),
        "comp--init-ctxt" => super::comp::builtin_comp_init_ctxt(args),
        "comp--install-trampoline" => super::comp::builtin_comp_install_trampoline(args),
        "comp--late-register-subr" => super::comp::builtin_comp_late_register_subr(args),
        "comp--register-lambda" => super::comp::builtin_comp_register_lambda(args),
        "comp--register-subr" => super::comp::builtin_comp_register_subr(args),
        "comp--release-ctxt" => super::comp::builtin_comp_release_ctxt(args),
        "comp--subr-signature" => super::comp::builtin_comp_subr_signature(args),
        "comp-el-to-eln-filename" => super::comp::builtin_comp_el_to_eln_filename(args),
        "comp-el-to-eln-rel-filename" => super::comp::builtin_comp_el_to_eln_rel_filename(args),
        "comp-libgccjit-version" => super::comp::builtin_comp_libgccjit_version(args),
        "comp-native-compiler-options-effective-p" => {
            super::comp::builtin_comp_native_compiler_options_effective_p(args)
        }
        "comp-native-driver-options-effective-p" => {
            super::comp::builtin_comp_native_driver_options_effective_p(args)
        }

        // DBus compatibility (pure)
        "dbus--init-bus" => super::dbus::builtin_dbus_init_bus(args),
        "dbus-get-unique-name" => super::dbus::builtin_dbus_get_unique_name(args),
        "dbus-message-internal" => super::dbus::builtin_dbus_message_internal(args),

        // Documentation/help (pure)
        "documentation-property" => super::doc::builtin_documentation_property(args),
        "Snarf-documentation" => super::doc::builtin_snarf_documentation(args),
        "substitute-command-keys" => super::doc::builtin_substitute_command_keys(args),
        "help-function-arglist" => super::doc::builtin_help_function_arglist_eval(eval, args),

        // JSON (pure)
        "json-serialize" => super::json::builtin_json_serialize(args),
        "json-parse-string" => super::json::builtin_json_parse_string(args),

        // Subr introspection (pure)
        "subr-name" => super::subr_info::builtin_subr_name(args),
        "subr-arity" => super::subr_info::builtin_subr_arity(args),
        "subr-native-elisp-p" => super::subr_info::builtin_subr_native_elisp_p(args),
        "native-comp-function-p" => super::subr_info::builtin_native_comp_function_p(args),
        "subr-primitive-p" => super::subr_info::builtin_subr_primitive_p(args),
        "interpreted-function-p" => super::subr_info::builtin_interpreted_function_p(args),
        "special-form-p" => super::subr_info::builtin_special_form_p(args),
        "macrop" => super::subr_info::builtin_macrop(args),
        "commandp" => super::subr_info::builtin_commandp(args),
        "command-modes" => super::interactive::builtin_command_modes(args),
        "func-arity" => builtin_func_arity_eval(eval, args),

        // Format/string utilities (pure)
        "format-spec" => super::format::builtin_format_spec(args),
        "format-time-string" => super::format::builtin_format_time_string(args),
        "format-seconds" => super::format::builtin_format_seconds(args),
        "string-pad" => super::format::builtin_string_pad(args),
        "string-fill" => super::format::builtin_string_fill(args),
        "string-limit" => super::format::builtin_string_limit(args),
        "string-chop-newline" => super::format::builtin_string_chop_newline(args),
        "string-lines" => super::format::builtin_string_lines(args),
        "string-clean-whitespace" => super::format::builtin_string_clean_whitespace(args),
        "string-pixel-width" => super::format::builtin_string_pixel_width(args),
        "string-glyph-split" => super::format::builtin_string_glyph_split(args),
        "string-equal-ignore-case" => super::format::builtin_string_equal_ignore_case(args),

        // Marker (pure)
        "markerp" => super::marker::builtin_markerp(args),
        "marker-position" => super::marker::builtin_marker_position(args),
        "marker-buffer" => super::marker::builtin_marker_buffer(args),
        "marker-insertion-type" => super::marker::builtin_marker_insertion_type(args),
        "set-marker-insertion-type" => super::marker::builtin_set_marker_insertion_type(args),
        "copy-marker" => super::marker::builtin_copy_marker(args),
        "make-marker" => super::marker::builtin_make_marker(args),

        // Composite (pure)
        "compose-region-internal" => super::composite::builtin_compose_region_internal(args),
        "compose-string-internal" => super::composite::builtin_compose_string_internal(args),
        "find-composition-internal" => super::composite::builtin_find_composition_internal(args),
        "composition-get-gstring" => super::composite::builtin_composition_get_gstring(args),
        "clear-composition-cache" => super::composite::builtin_clear_composition_cache(args),
        "composition-sort-rules" => super::composite::builtin_composition_sort_rules(args),
        "auto-composition-mode" => super::composite::builtin_auto_composition_mode(args),

        // Indentation (pure)
        "current-indentation" => super::indent::builtin_current_indentation(args),
        "indent-to" => super::indent::builtin_indent_to(args),
        "current-column" => super::indent::builtin_current_column(args),
        "move-to-column" => super::indent::builtin_move_to_column(args),

        // Error hierarchy (pure)
        "signal" => super::errors::builtin_signal(args),

        // Hash-table extended (pure)
        "hash-table-test" => super::hashtab::builtin_hash_table_test(args),
        "hash-table-size" => super::hashtab::builtin_hash_table_size(args),
        "hash-table-rehash-size" => super::hashtab::builtin_hash_table_rehash_size(args),
        "hash-table-rehash-threshold" => super::hashtab::builtin_hash_table_rehash_threshold(args),
        "hash-table-weakness" => super::hashtab::builtin_hash_table_weakness(args),
        "copy-hash-table" => super::hashtab::builtin_copy_hash_table(args),
        "sxhash-eq" => super::hashtab::builtin_sxhash_eq(args),
        "sxhash-eql" => super::hashtab::builtin_sxhash_eql(args),
        "sxhash-equal" => super::hashtab::builtin_sxhash_equal(args),
        "sxhash-equal-including-properties" => {
            super::hashtab::builtin_sxhash_equal_including_properties(args)
        }
        "internal--hash-table-buckets" => super::hashtab::builtin_internal_hash_table_buckets(args),
        "internal--hash-table-histogram" => {
            super::hashtab::builtin_internal_hash_table_histogram(args)
        }
        "internal--hash-table-index-size" => {
            super::hashtab::builtin_internal_hash_table_index_size(args)
        }

        // Threading (pure)
        // Misc (pure)
        "copy-alist" => super::misc::builtin_copy_alist(args),
        "rassoc" => super::misc::builtin_rassoc(args),
        "rassq" => super::misc::builtin_rassq(args),
        "assoc-default" => super::misc::builtin_assoc_default(args),
        "make-list" => super::misc::builtin_make_list(args),
        "safe-length" => super::misc::builtin_safe_length(args),
        "subst-char-in-string" => super::misc::builtin_subst_char_in_string(args),
        "string-to-multibyte" => super::misc::builtin_string_to_multibyte(args),
        "string-to-unibyte" => super::misc::builtin_string_to_unibyte(args),
        "string-as-unibyte" => super::misc::builtin_string_as_unibyte(args),
        "string-as-multibyte" => super::misc::builtin_string_as_multibyte(args),
        "unibyte-char-to-multibyte" => super::misc::builtin_unibyte_char_to_multibyte(args),
        "multibyte-char-to-unibyte" => super::misc::builtin_multibyte_char_to_unibyte(args),
        "define-coding-system-alias" => {
            super::coding::builtin_define_coding_system_alias(&mut eval.coding_systems, args)
        }
        "coding-system-p" => super::coding::builtin_coding_system_p(&eval.coding_systems, args),
        "check-coding-system" => {
            super::coding::builtin_check_coding_system(&eval.coding_systems, args)
        }
        "check-coding-systems-region" => {
            super::coding::builtin_check_coding_systems_region(&eval.coding_systems, args)
        }
        "set-coding-system-priority" => {
            super::coding::builtin_set_coding_system_priority(&mut eval.coding_systems, args)
        }
        "set-keyboard-coding-system-internal" => {
            super::coding::builtin_set_keyboard_coding_system_internal(
                &mut eval.coding_systems,
                args,
            )
        }
        "set-safe-terminal-coding-system-internal" => {
            super::coding::builtin_set_safe_terminal_coding_system_internal(
                &mut eval.coding_systems,
                args,
            )
        }
        "set-terminal-coding-system-internal" => {
            super::coding::builtin_set_terminal_coding_system_internal(
                &mut eval.coding_systems,
                args,
            )
        }
        "set-text-conversion-style" => super::coding::builtin_set_text_conversion_style(args),
        "text-quoting-style" => super::coding::builtin_text_quoting_style(args),
        "locale-info" => super::misc::builtin_locale_info(args),
        // Reader/printer (pure)
        "y-or-n-p" => super::reader::builtin_y_or_n_p(eval, args),
        "yes-or-no-p" => super::reader::builtin_yes_or_no_p(eval, args),

        // Char-table / bool-vector (pure)
        "make-char-table" => super::chartable::builtin_make_char_table(args),
        "char-table-p" => super::chartable::builtin_char_table_p(args),
        "set-char-table-range" => super::chartable::builtin_set_char_table_range(args),
        "char-table-range" => super::chartable::builtin_char_table_range(args),
        "char-table-parent" => super::chartable::builtin_char_table_parent(args),
        "set-char-table-parent" => super::chartable::builtin_set_char_table_parent(args),
        "char-table-extra-slot" => super::chartable::builtin_char_table_extra_slot(args),
        "set-char-table-extra-slot" => super::chartable::builtin_set_char_table_extra_slot(args),
        "char-table-subtype" => super::chartable::builtin_char_table_subtype(args),
        "bool-vector" => super::chartable::builtin_bool_vector(args),
        "make-bool-vector" => super::chartable::builtin_make_bool_vector(args),
        "bool-vector-p" => super::chartable::builtin_bool_vector_p(args),
        "bool-vector-count-population" => {
            super::chartable::builtin_bool_vector_count_population(args)
        }
        "bool-vector-count-consecutive" => {
            super::chartable::builtin_bool_vector_count_consecutive(args)
        }
        "bool-vector-intersection" => super::chartable::builtin_bool_vector_intersection(args),
        "bool-vector-not" => super::chartable::builtin_bool_vector_not(args),
        "bool-vector-set-difference" => super::chartable::builtin_bool_vector_set_difference(args),
        "bool-vector-union" => super::chartable::builtin_bool_vector_union(args),
        "bool-vector-exclusive-or" => super::chartable::builtin_bool_vector_exclusive_or(args),
        "bool-vector-subsetp" => super::chartable::builtin_bool_vector_subsetp(args),

        // Note: windowp and framep are in the eval-dependent section above
        "seq-reverse" => super::cl_lib::builtin_seq_reverse(args),
        "seq-drop" => super::cl_lib::builtin_seq_drop(args),
        "seq-take" => super::cl_lib::builtin_seq_take(args),
        "seq-subseq" => super::cl_lib::builtin_seq_subseq(args),
        "seq-concatenate" => super::cl_lib::builtin_seq_concatenate(args),
        "seq-empty-p" => super::cl_lib::builtin_seq_empty_p(args),
        "seq-min" => super::cl_lib::builtin_seq_min(args),
        "seq-max" => super::cl_lib::builtin_seq_max(args),
        // Search (pure)
        "string-match" => super::search::builtin_string_match(args),
        "string-match-p" => super::search::builtin_string_match_p(args),
        "regexp-quote" => super::search::builtin_regexp_quote(args),
        "match-beginning" => super::search::builtin_match_beginning(args),
        "match-end" => super::search::builtin_match_end(args),
        "match-data" => super::search::builtin_match_data(args),
        "set-match-data" => super::search::builtin_set_match_data(args),
        "looking-at" => super::search::builtin_looking_at(args),
        "looking-at-p" => super::search::builtin_looking_at_p(args),
        "replace-regexp-in-string" => super::search::builtin_replace_regexp_in_string(args),

        // Lread (pure)
        "get-load-suffixes" => super::lread::builtin_get_load_suffixes(args),
        "locate-file" => super::lread::builtin_locate_file(args),
        "locate-file-internal" => super::lread::builtin_locate_file_internal(args),
        "read-coding-system" => super::lread::builtin_read_coding_system(args),
        "read-non-nil-coding-system" => super::lread::builtin_read_non_nil_coding_system(args),

        // Editfns (pure)
        "user-uid" => super::editfns::builtin_user_uid(args),
        "user-real-uid" => super::editfns::builtin_user_real_uid(args),
        "group-name" => super::editfns::builtin_group_name(args),
        "group-gid" => super::editfns::builtin_group_gid(args),
        "group-real-gid" => super::editfns::builtin_group_real_gid(args),
        "load-average" => super::editfns::builtin_load_average(args),
        "logcount" => super::editfns::builtin_logcount(args),

        // Fns (pure)
        "base64-encode-string" => super::fns::builtin_base64_encode_string(args),
        "base64-decode-string" => super::fns::builtin_base64_decode_string(args),
        "base64url-encode-string" => super::fns::builtin_base64url_encode_string(args),
        "md5" => super::fns::builtin_md5(args),
        "secure-hash" => super::fns::builtin_secure_hash(args),
        "equal-including-properties" => super::fns::builtin_equal_including_properties(args),
        "widget-get" => super::fns::builtin_widget_get(args),
        "widget-put" => super::fns::builtin_widget_put(args),
        "widget-apply" => super::fns::builtin_widget_apply(args),
        "string-make-multibyte" => super::fns::builtin_string_make_multibyte(args),
        "string-make-unibyte" => super::fns::builtin_string_make_unibyte(args),
        "compare-strings" => super::fns::builtin_compare_strings(args),
        "string-version-lessp" => super::fns::builtin_string_version_lessp(args),
        "string-collate-lessp" => super::fns::builtin_string_collate_lessp(args),
        "string-collate-equalp" => super::fns::builtin_string_collate_equalp(args),

        _ => return None,
    })
}

/// Dispatch to pure builtins that don't need evaluator access.
/// Used by the bytecode VM.
pub(crate) fn dispatch_builtin_pure(name: &str, args: Vec<Value>) -> Option<EvalResult> {
    match name {
        "assoc" | "alist-get" | "plist-member" => return None,
        _ => {}
    }

    if let Ok(id) = name.parse::<PureBuiltinId>() {
        return Some(dispatch_builtin_id_pure(id, args));
    }

    Some(match name {
        // Arithmetic (typed subset is dispatched above)
        // Type predicates and equality (typed subset is dispatched above)
        "integer-or-marker-p" => builtin_integer_or_marker_p(args),
        "number-or-marker-p" => builtin_number_or_marker_p(args),
        "vector-or-char-table-p" => builtin_vector_or_char_table_p(args),
        "function-equal" => builtin_function_equal(args),
        "module-function-p" => builtin_module_function_p(args),
        "user-ptrp" => builtin_user_ptrp(args),
        "symbol-with-pos-p" => builtin_symbol_with_pos_p(args),
        "symbol-with-pos-pos" => builtin_symbol_with_pos_pos(args),
        // Cons/List (typed subset is dispatched above)
        "length<" => builtin_length_lt(args),
        "length=" => builtin_length_eq(args),
        "length>" => builtin_length_gt(args),
        "substring-no-properties" => builtin_substring_no_properties(args),
        // String (typed subset is dispatched above)
        // Vector/hash/conversion/plist/symbol (typed subset is dispatched above)
        // Math
        "sqrt" => builtin_sqrt(args),
        "sin" => builtin_sin(args),
        "cos" => builtin_cos(args),
        "tan" => builtin_tan(args),
        "asin" => builtin_asin(args),
        "acos" => builtin_acos(args),
        "atan" => builtin_atan(args),
        "exp" => builtin_exp(args),
        "log" => builtin_log(args),
        "expt" => builtin_expt(args),
        "random" => builtin_random(args),
        "isnan" => builtin_isnan(args),
        // Extended string
        "string-prefix-p" => builtin_string_prefix_p(args),
        "string-suffix-p" => builtin_string_suffix_p(args),
        "string-join" => builtin_string_join(args),
        "split-string" => builtin_split_string(args),
        "string-trim" => builtin_string_trim(args),
        "string-trim-left" => builtin_string_trim_left(args),
        "string-trim-right" => builtin_string_trim_right(args),
        "make-string" => builtin_make_string(args),
        "string" => builtin_string(args),
        "string-to-list" => builtin_string_to_list(args),
        "string-width" => builtin_string_width(args),
        // Extended list
        "last" => builtin_last(args),
        "butlast" => builtin_butlast(args),
        "delete" => builtin_delete(args),
        "delq" => builtin_delq(args),
        "elt" => builtin_elt(args),
        "memql" => builtin_memql(args),
        "nconc" => builtin_nconc(args),
        "alist-get" => builtin_alist_get(args),
        "number-sequence" => builtin_number_sequence(args),
        // Output / misc
        "identity" => builtin_identity(args),
        "purecopy" => builtin_purecopy(args),
        "format-message" => builtin_format_message(args),
        "message" => builtin_message(args),
        "message-box" => builtin_message_box(args),
        "message-or-box" => builtin_message_or_box(args),
        "current-message" => builtin_current_message(args),
        "ngettext" => builtin_ngettext(args),
        "secure-hash-algorithms" => builtin_secure_hash_algorithms(args),
        "error" => builtin_error(args),
        "prefix-numeric-value" => builtin_prefix_numeric_value(args),
        "princ" => builtin_princ(args),
        "prin1" => builtin_prin1(args),
        "prin1-to-string" => builtin_prin1_to_string(args),
        "print" => builtin_print(args),
        "terpri" => builtin_terpri(args),
        "write-char" => builtin_write_char(args),
        "propertize" => builtin_propertize(args),
        "gensym" => builtin_gensym(args),
        "string-to-syntax" => builtin_string_to_syntax(args),
        "syntax-class-to-char" => super::syntax::builtin_syntax_class_to_char(args),
        "matching-paren" => super::syntax::builtin_matching_paren(args),
        "make-syntax-table" => super::syntax::builtin_make_syntax_table(args),
        "copy-syntax-table" => super::syntax::builtin_copy_syntax_table(args),
        "syntax-table-p" => super::syntax::builtin_syntax_table_p(args),
        "standard-syntax-table" => super::syntax::builtin_standard_syntax_table(args),
        "current-time" => builtin_current_time(args),
        "current-cpu-time" => builtin_current_cpu_time(args),
        "current-idle-time" => builtin_current_idle_time(args),
        "get-internal-run-time" => builtin_get_internal_run_time(args),
        "float-time" => builtin_float_time(args),
        "daemonp" => builtin_daemonp(args),
        "daemon-initialized" => builtin_daemon_initialized(args),
        "flush-standard-output" => builtin_flush_standard_output(args),
        "force-mode-line-update" => builtin_force_mode_line_update(args),
        "force-window-update" => builtin_force_window_update(args),
        "invocation-directory" => builtin_invocation_directory(args),
        "invocation-name" => builtin_invocation_name(args),
        // File I/O (pure)
        "expand-file-name" => super::fileio::builtin_expand_file_name(args),
        "file-truename" => super::fileio::builtin_file_truename(args),
        "file-name-directory" => super::fileio::builtin_file_name_directory(args),
        "file-name-nondirectory" => super::fileio::builtin_file_name_nondirectory(args),
        "file-name-extension" => super::fileio::builtin_file_name_extension(args),
        "file-name-sans-extension" => super::fileio::builtin_file_name_sans_extension(args),
        "file-name-as-directory" => super::fileio::builtin_file_name_as_directory(args),
        "directory-file-name" => super::fileio::builtin_directory_file_name(args),
        "file-name-concat" => super::fileio::builtin_file_name_concat(args),
        "file-name-absolute-p" => super::fileio::builtin_file_name_absolute_p(args),
        "directory-name-p" => super::fileio::builtin_directory_name_p(args),
        "substitute-in-file-name" => super::fileio::builtin_substitute_in_file_name(args),
        "file-acl" => super::fileio::builtin_file_acl(args),
        "file-exists-p" => super::fileio::builtin_file_exists_p(args),
        "file-readable-p" => super::fileio::builtin_file_readable_p(args),
        "file-writable-p" => super::fileio::builtin_file_writable_p(args),
        "file-accessible-directory-p" => super::fileio::builtin_file_accessible_directory_p(args),
        "file-executable-p" => super::fileio::builtin_file_executable_p(args),
        "file-locked-p" => super::fileio::builtin_file_locked_p(args),
        "file-selinux-context" => super::fileio::builtin_file_selinux_context(args),
        "file-system-info" => super::fileio::builtin_file_system_info(args),
        "file-directory-p" => super::fileio::builtin_file_directory_p(args),
        "file-regular-p" => super::fileio::builtin_file_regular_p(args),
        "file-symlink-p" => super::fileio::builtin_file_symlink_p(args),
        "file-name-case-insensitive-p" => super::fileio::builtin_file_name_case_insensitive_p(args),
        "file-newer-than-file-p" => super::fileio::builtin_file_newer_than_file_p(args),
        "file-equal-p" => super::fileio::builtin_file_equal_p(args),
        "file-in-directory-p" => super::fileio::builtin_file_in_directory_p(args),
        "file-modes" => super::fileio::builtin_file_modes(args),
        "set-file-modes" => super::fileio::builtin_set_file_modes(args),
        "set-file-times" => super::fileio::builtin_set_file_times(args),
        "set-file-acl" => super::fileio::builtin_set_file_acl(args),
        "set-file-selinux-context" => super::fileio::builtin_set_file_selinux_context(args),
        "visited-file-modtime" => super::fileio::builtin_visited_file_modtime(args),
        "default-file-modes" => super::fileio::builtin_default_file_modes(args),
        "set-default-file-modes" => super::fileio::builtin_set_default_file_modes(args),
        "delete-file" => super::fileio::builtin_delete_file(args),
        "delete-file-internal" => super::fileio::builtin_delete_file_internal(args),
        "delete-directory" => super::fileio::builtin_delete_directory(args),
        "delete-directory-internal" => super::fileio::builtin_delete_directory_internal(args),
        "rename-file" => super::fileio::builtin_rename_file(args),
        "copy-file" => super::fileio::builtin_copy_file(args),
        "add-name-to-file" => super::fileio::builtin_add_name_to_file(args),
        "make-symbolic-link" => super::fileio::builtin_make_symbolic_link(args),
        "make-directory" => super::fileio::builtin_make_directory(args),
        "make-directory-internal" => super::fileio::builtin_make_directory_internal(args),
        "make-temp-file" => super::fileio::builtin_make_temp_file(args),
        "make-temp-name" => super::fileio::builtin_make_temp_name(args),
        "make-nearby-temp-file" => super::fileio::builtin_make_nearby_temp_file(args),
        "next-read-file-uses-dialog-p" => super::fileio::builtin_next_read_file_uses_dialog_p(args),
        "unhandled-file-name-directory" => {
            super::fileio::builtin_unhandled_file_name_directory(args)
        }
        "get-truename-buffer" => super::fileio::builtin_get_truename_buffer(args),
        "directory-files" => super::fileio::builtin_directory_files(args),
        "find-file-name-handler" => super::fileio::builtin_find_file_name_handler(args),
        "file-attributes" => super::dired::builtin_file_attributes(args),
        // Keymap (pure)
        "kbd" => builtin_kbd(args),
        "single-key-description" => builtin_single_key_description(args),
        "key-description" => builtin_key_description(args),
        "help-key-description" => builtin_help_key_description(args),
        "event-convert-list" => builtin_event_convert_list(args),
        "event-basic-type" => builtin_event_basic_type(args),
        "event-apply-modifier" => builtin_event_apply_modifier(args),
        "eventp" => builtin_eventp(args),
        "event-modifiers" => builtin_event_modifiers(args),
        "listify-key-sequence" => builtin_listify_key_sequence(args),
        "key-valid-p" => builtin_key_valid_p(args),
        "text-char-description" => builtin_text_char_description(args),
        // Process (pure)
        "shell-command-to-string" => super::process::builtin_shell_command_to_string(args),
        "getenv" => super::process::builtin_getenv(args),
        "getenv-internal" => super::process::builtin_getenv_internal(args),
        "setenv" => super::process::builtin_setenv(args),
        "set-binary-mode" => super::process::builtin_set_binary_mode(args),
        // Editfns (pure)
        "group-name" => super::editfns::builtin_group_name(args),
        "group-gid" => super::editfns::builtin_group_gid(args),
        "group-real-gid" => super::editfns::builtin_group_real_gid(args),
        "load-average" => super::editfns::builtin_load_average(args),
        "logcount" => super::editfns::builtin_logcount(args),
        // Timer (pure)
        "timerp" => super::timer::builtin_timerp(args),
        "sit-for" => super::timer::builtin_sit_for(args),
        // Undo system (pure)
        "undo-boundary" => super::undo::builtin_undo_boundary(args),
        "primitive-undo" => super::undo::builtin_primitive_undo(args),
        // Keyboard macro (pure)
        // Character encoding (pure)
        "char-width" => crate::encoding::builtin_char_width(args),
        "string-bytes" => crate::encoding::builtin_string_bytes(args),
        "multibyte-string-p" => crate::encoding::builtin_multibyte_string_p(args),
        "encode-coding-string" => crate::encoding::builtin_encode_coding_string(args),
        "decode-coding-string" => crate::encoding::builtin_decode_coding_string(args),
        "char-or-string-p" => crate::encoding::builtin_char_or_string_p(args),
        "max-char" => crate::encoding::builtin_max_char(args),
        // Display/terminal (pure)
        "display-graphic-p" => super::display::builtin_display_graphic_p(args),
        "display-color-p" => super::display::builtin_display_color_p(args),
        "display-pixel-width" => super::display::builtin_display_pixel_width(args),
        "display-pixel-height" => super::display::builtin_display_pixel_height(args),
        "terminal-name" => super::display::builtin_terminal_name(args),
        "terminal-live-p" => super::display::builtin_terminal_live_p(args),
        // Internal compatibility surface (pure)
        "define-fringe-bitmap" => super::compat_internal::builtin_define_fringe_bitmap(args),
        "destroy-fringe-bitmap" => super::compat_internal::builtin_destroy_fringe_bitmap(args),
        "display--line-is-continued-p" => {
            super::compat_internal::builtin_display_line_is_continued_p(args)
        }
        "display--update-for-mouse-movement" => {
            super::compat_internal::builtin_display_update_for_mouse_movement(args)
        }
        "do-auto-save" => super::compat_internal::builtin_do_auto_save(args),
        "external-debugging-output" => {
            super::compat_internal::builtin_external_debugging_output(args)
        }
        "describe-buffer-bindings" => {
            super::compat_internal::builtin_describe_buffer_bindings(args)
        }
        "describe-vector" => super::compat_internal::builtin_describe_vector(args),
        "delete-terminal" => super::compat_internal::builtin_delete_terminal(args),
        "face-attributes-as-vector" => {
            super::compat_internal::builtin_face_attributes_as_vector(args)
        }
        "font-at" => super::compat_internal::builtin_font_at(args),
        "font-face-attributes" => super::compat_internal::builtin_font_face_attributes(args),
        "font-get-glyphs" => super::compat_internal::builtin_font_get_glyphs(args),
        "font-get-system-font" => super::compat_internal::builtin_font_get_system_font(args),
        "font-get-system-normal-font" => {
            super::compat_internal::builtin_font_get_system_normal_font(args)
        }
        "font-has-char-p" => super::compat_internal::builtin_font_has_char_p(args),
        "font-info" => super::compat_internal::builtin_font_info(args),
        "font-match-p" => super::compat_internal::builtin_font_match_p(args),
        "font-shape-gstring" => super::compat_internal::builtin_font_shape_gstring(args),
        "font-variation-glyphs" => super::compat_internal::builtin_font_variation_glyphs(args),
        "fontset-font" => super::compat_internal::builtin_fontset_font(args),
        "fontset-info" => super::compat_internal::builtin_fontset_info(args),
        "fontset-list" => super::compat_internal::builtin_fontset_list(args),
        "frame--set-was-invisible" => super::compat_internal::builtin_frame_set_was_invisible(args),
        "frame-after-make-frame" => super::compat_internal::builtin_frame_after_make_frame(args),
        "frame-ancestor-p" => super::compat_internal::builtin_frame_ancestor_p(args),
        "frame-bottom-divider-width" => {
            super::compat_internal::builtin_frame_bottom_divider_width(args)
        }
        "frame-child-frame-border-width" => {
            super::compat_internal::builtin_frame_child_frame_border_width(args)
        }
        "frame-focus" => super::compat_internal::builtin_frame_focus(args),
        "frame-font-cache" => super::compat_internal::builtin_frame_font_cache(args),
        "frame--face-hash-table" => super::compat_internal::builtin_frame_face_hash_table(args),
        "frame-fringe-width" => super::compat_internal::builtin_frame_fringe_width(args),
        "frame-internal-border-width" => {
            super::compat_internal::builtin_frame_internal_border_width(args)
        }
        "frame-old-selected-window" => {
            super::compat_internal::builtin_frame_old_selected_window(args)
        }
        "frame-or-buffer-changed-p" => {
            super::compat_internal::builtin_frame_or_buffer_changed_p(args)
        }
        "frame-parent" => super::compat_internal::builtin_frame_parent(args),
        "frame-pointer-visible-p" => super::compat_internal::builtin_frame_pointer_visible_p(args),
        "frame-right-divider-width" => {
            super::compat_internal::builtin_frame_right_divider_width(args)
        }
        "frame-scale-factor" => super::compat_internal::builtin_frame_scale_factor(args),
        "frame-scroll-bar-height" => super::compat_internal::builtin_frame_scroll_bar_height(args),
        "frame-scroll-bar-width" => super::compat_internal::builtin_frame_scroll_bar_width(args),
        "frame-window-state-change" => {
            super::compat_internal::builtin_frame_window_state_change(args)
        }
        "fringe-bitmaps-at-pos" => super::compat_internal::builtin_fringe_bitmaps_at_pos(args),
        "gap-position" => super::compat_internal::builtin_gap_position(args),
        "gap-size" => super::compat_internal::builtin_gap_size(args),
        "garbage-collect-maybe" => super::compat_internal::builtin_garbage_collect_maybe(args),
        "get-unicode-property-internal" => {
            super::compat_internal::builtin_get_unicode_property_internal(args)
        }
        "get-variable-watchers" => super::compat_internal::builtin_get_variable_watchers(args),
        "gnutls-available-p" => super::compat_internal::builtin_gnutls_available_p(args),
        "gnutls-asynchronous-parameters" => {
            super::compat_internal::builtin_gnutls_asynchronous_parameters(args)
        }
        "gnutls-boot" => super::compat_internal::builtin_gnutls_boot(args),
        "gnutls-bye" => super::compat_internal::builtin_gnutls_bye(args),
        "gnutls-ciphers" => super::compat_internal::builtin_gnutls_ciphers(args),
        "gnutls-deinit" => super::compat_internal::builtin_gnutls_deinit(args),
        "gnutls-digests" => super::compat_internal::builtin_gnutls_digests(args),
        "gnutls-error-fatalp" => super::compat_internal::builtin_gnutls_error_fatalp(args),
        "gnutls-error-string" => super::compat_internal::builtin_gnutls_error_string(args),
        "gnutls-errorp" => super::compat_internal::builtin_gnutls_errorp(args),
        "gnutls-format-certificate" => {
            super::compat_internal::builtin_gnutls_format_certificate(args)
        }
        "gnutls-get-initstage" => super::compat_internal::builtin_gnutls_get_initstage(args),
        "gnutls-hash-digest" => super::compat_internal::builtin_gnutls_hash_digest(args),
        "gnutls-hash-mac" => super::compat_internal::builtin_gnutls_hash_mac(args),
        "gnutls-macs" => super::compat_internal::builtin_gnutls_macs(args),
        "gnutls-peer-status" => super::compat_internal::builtin_gnutls_peer_status(args),
        "gnutls-peer-status-warning-describe" => {
            super::compat_internal::builtin_gnutls_peer_status_warning_describe(args)
        }
        "gnutls-symmetric-decrypt" => {
            super::compat_internal::builtin_gnutls_symmetric_decrypt(args)
        }
        "gnutls-symmetric-encrypt" => {
            super::compat_internal::builtin_gnutls_symmetric_encrypt(args)
        }
        "gpm-mouse-start" => super::compat_internal::builtin_gpm_mouse_start(args),
        "gpm-mouse-stop" => super::compat_internal::builtin_gpm_mouse_stop(args),
        "handle-save-session" => super::compat_internal::builtin_handle_save_session(args),
        "handle-switch-frame" => super::compat_internal::builtin_handle_switch_frame(args),
        "help--describe-vector" => super::compat_internal::builtin_help_describe_vector(args),
        "init-image-library" => super::compat_internal::builtin_init_image_library(args),
        "internal--define-uninitialized-variable" => {
            super::compat_internal::builtin_internal_define_uninitialized_variable(args)
        }
        "internal--labeled-narrow-to-region" => {
            super::compat_internal::builtin_internal_labeled_narrow_to_region(args)
        }
        "internal--labeled-widen" => super::compat_internal::builtin_internal_labeled_widen(args),
        "internal--obarray-buckets" => {
            super::compat_internal::builtin_internal_obarray_buckets(args)
        }
        "internal--set-buffer-modified-tick" => {
            super::compat_internal::builtin_internal_set_buffer_modified_tick(args)
        }
        "internal--track-mouse" => super::compat_internal::builtin_internal_track_mouse(args),
        "internal-char-font" => super::compat_internal::builtin_internal_char_font(args),
        "internal-complete-buffer" => {
            super::compat_internal::builtin_internal_complete_buffer(args)
        }
        "internal-describe-syntax-value" => {
            super::compat_internal::builtin_internal_describe_syntax_value(args)
        }
        "internal-event-symbol-parse-modifiers" => {
            super::compat_internal::builtin_internal_event_symbol_parse_modifiers(args)
        }
        "internal-handle-focus-in" => {
            super::compat_internal::builtin_internal_handle_focus_in(args)
        }
        "internal-make-var-non-special" => {
            super::compat_internal::builtin_internal_make_var_non_special(args)
        }
        "internal-set-lisp-face-attribute-from-resource" => {
            super::compat_internal::builtin_internal_set_lisp_face_attribute_from_resource(args)
        }
        "internal-stack-stats" => super::compat_internal::builtin_internal_stack_stats(args),
        "internal-subr-documentation" => {
            super::compat_internal::builtin_internal_subr_documentation(args)
        }
        "byte-code" => super::compat_internal::builtin_byte_code(args),
        "decode-coding-region" => super::compat_internal::builtin_decode_coding_region(args),
        "defconst-1" => super::compat_internal::builtin_defconst_1(args),
        "define-coding-system-internal" => {
            super::compat_internal::builtin_define_coding_system_internal(args)
        }
        "defvar-1" => super::compat_internal::builtin_defvar_1(args),
        "defvaralias" => super::compat_internal::builtin_defvaralias(args),
        "dump-emacs-portable" => super::compat_internal::builtin_dump_emacs_portable(args),
        "dump-emacs-portable--sort-predicate" => {
            super::compat_internal::builtin_dump_emacs_portable_sort_predicate(args)
        }
        "dump-emacs-portable--sort-predicate-copied" => {
            super::compat_internal::builtin_dump_emacs_portable_sort_predicate_copied(args)
        }
        "encode-coding-region" => super::compat_internal::builtin_encode_coding_region(args),
        "find-operation-coding-system" => {
            super::compat_internal::builtin_find_operation_coding_system(args)
        }
        "handler-bind-1" => super::compat_internal::builtin_handler_bind_1(args),
        "indirect-variable" => super::compat_internal::builtin_indirect_variable(args),
        "insert-and-inherit" => super::compat_internal::builtin_insert_and_inherit(args),
        "insert-before-markers-and-inherit" => {
            super::compat_internal::builtin_insert_before_markers_and_inherit(args)
        }
        "insert-buffer-substring" => super::compat_internal::builtin_insert_buffer_substring(args),
        "iso-charset" => super::compat_internal::builtin_iso_charset(args),
        "keymap--get-keyelt" => super::compat_internal::builtin_keymap_get_keyelt(args),
        "keymap-prompt" => super::compat_internal::builtin_keymap_prompt(args),
        "kill-all-local-variables" => {
            super::compat_internal::builtin_kill_all_local_variables(args)
        }
        "kill-emacs" => super::compat_internal::builtin_kill_emacs(args),
        "lower-frame" => super::compat_internal::builtin_lower_frame(args),
        "lread--substitute-object-in-subtree" => {
            super::compat_internal::builtin_lread_substitute_object_in_subtree(args)
        }
        "macroexpand" => super::compat_internal::builtin_macroexpand(args),
        "malloc-info" => super::compat_internal::builtin_malloc_info(args),
        "malloc-trim" => super::compat_internal::builtin_malloc_trim(args),
        "make-byte-code" => super::compat_internal::builtin_make_byte_code(args),
        "make-char" => super::compat_internal::builtin_make_char(args),
        "make-closure" => super::compat_internal::builtin_make_closure(args),
        "make-finalizer" => super::compat_internal::builtin_make_finalizer(args),
        "marker-last-position" => super::compat_internal::builtin_marker_last_position(args),
        "make-indirect-buffer" => super::compat_internal::builtin_make_indirect_buffer(args),
        "make-interpreted-closure" => {
            super::compat_internal::builtin_make_interpreted_closure(args)
        }
        "make-record" => super::compat_internal::builtin_make_record(args),
        "make-temp-file-internal" => super::compat_internal::builtin_make_temp_file_internal(args),
        "map-charset-chars" => super::compat_internal::builtin_map_charset_chars(args),
        "map-keymap" => super::compat_internal::builtin_map_keymap(args),
        "map-keymap-internal" => super::compat_internal::builtin_map_keymap_internal(args),
        "mapbacktrace" => super::compat_internal::builtin_mapbacktrace(args),
        "match-data--translate" => super::compat_internal::builtin_match_data_translate(args),
        "memory-info" => super::compat_internal::builtin_memory_info(args),
        "make-frame-invisible" => super::compat_internal::builtin_make_frame_invisible(args),
        "make-terminal-frame" => super::compat_internal::builtin_make_terminal_frame(args),
        "menu-bar-menu-at-x-y" => super::compat_internal::builtin_menu_bar_menu_at_x_y(args),
        "menu-or-popup-active-p" => super::compat_internal::builtin_menu_or_popup_active_p(args),
        "minibuffer-innermost-command-loop-p" => {
            super::compat_internal::builtin_minibuffer_innermost_command_loop_p(args)
        }
        "minibuffer-prompt-end" => super::compat_internal::builtin_minibuffer_prompt_end(args),
        "module-load" => super::compat_internal::builtin_module_load(args),
        "mouse-pixel-position" => super::compat_internal::builtin_mouse_pixel_position(args),
        "mouse-position" => super::compat_internal::builtin_mouse_position(args),
        "newline-cache-check" => super::compat_internal::builtin_newline_cache_check(args),
        "native-comp-available-p" => super::compat_internal::builtin_native_comp_available_p(args),
        "native-comp-unit-file" => super::compat_internal::builtin_native_comp_unit_file(args),
        "native-comp-unit-set-file" => {
            super::compat_internal::builtin_native_comp_unit_set_file(args)
        }
        "native-elisp-load" => super::compat_internal::builtin_native_elisp_load(args),
        "new-fontset" => super::compat_internal::builtin_new_fontset(args),
        "next-frame" => super::compat_internal::builtin_next_frame(args),
        "ntake" => super::compat_internal::builtin_ntake(args),
        "obarray-clear" => super::compat_internal::builtin_obarray_clear(args),
        "obarray-make" => super::compat_internal::builtin_obarray_make(args),
        "object-intervals" => super::compat_internal::builtin_object_intervals(args),
        "old-selected-frame" => super::compat_internal::builtin_old_selected_frame(args),
        "old-selected-window" => super::compat_internal::builtin_old_selected_window(args),
        "open-dribble-file" => super::compat_internal::builtin_open_dribble_file(args),
        "open-font" => super::compat_internal::builtin_open_font(args),
        "optimize-char-table" => super::compat_internal::builtin_optimize_char_table(args),
        "overlay-lists" => super::compat_internal::builtin_overlay_lists(args),
        "overlay-recenter" => super::compat_internal::builtin_overlay_recenter(args),
        "pdumper-stats" => super::compat_internal::builtin_pdumper_stats(args),
        "play-sound-internal" => super::compat_internal::builtin_play_sound_internal(args),
        "position-symbol" => super::compat_internal::builtin_position_symbol(args),
        "posn-at-point" => super::compat_internal::builtin_posn_at_point(args),
        "posn-at-x-y" => super::compat_internal::builtin_posn_at_x_y(args),
        "previous-frame" => super::compat_internal::builtin_previous_frame(args),
        "profiler-cpu-log" => super::compat_internal::builtin_profiler_cpu_log(args),
        "profiler-cpu-running-p" => super::compat_internal::builtin_profiler_cpu_running_p(args),
        "profiler-cpu-start" => super::compat_internal::builtin_profiler_cpu_start(args),
        "profiler-cpu-stop" => super::compat_internal::builtin_profiler_cpu_stop(args),
        "profiler-memory-log" => super::compat_internal::builtin_profiler_memory_log(args),
        "profiler-memory-running-p" => {
            super::compat_internal::builtin_profiler_memory_running_p(args)
        }
        "profiler-memory-start" => super::compat_internal::builtin_profiler_memory_start(args),
        "profiler-memory-stop" => super::compat_internal::builtin_profiler_memory_stop(args),
        "put-unicode-property-internal" => {
            super::compat_internal::builtin_put_unicode_property_internal(args)
        }
        "query-font" => super::compat_internal::builtin_query_font(args),
        "query-fontset" => super::compat_internal::builtin_query_fontset(args),
        "raise-frame" => super::compat_internal::builtin_raise_frame(args),
        "read-positioning-symbols" => {
            super::compat_internal::builtin_read_positioning_symbols(args)
        }
        "re--describe-compiled" => super::compat_internal::builtin_re_describe_compiled(args),
        "recent-auto-save-p" => super::compat_internal::builtin_recent_auto_save_p(args),
        "redisplay" => super::compat_internal::builtin_redisplay(args),
        "record" => super::compat_internal::builtin_record(args),
        "recordp" => super::compat_internal::builtin_recordp(args),
        "reconsider-frame-fonts" => super::compat_internal::builtin_reconsider_frame_fonts(args),
        "redirect-debugging-output" => {
            super::compat_internal::builtin_redirect_debugging_output(args)
        }
        "redirect-frame-focus" => super::compat_internal::builtin_redirect_frame_focus(args),
        "remove-pos-from-symbol" => super::compat_internal::builtin_remove_pos_from_symbol(args),
        "rename-buffer" => super::compat_internal::builtin_rename_buffer(args),
        "replace-buffer-contents" => super::compat_internal::builtin_replace_buffer_contents(args),
        "resize-mini-window-internal" => {
            super::compat_internal::builtin_resize_mini_window_internal(args)
        }
        "restore-buffer-modified-p" => {
            super::compat_internal::builtin_restore_buffer_modified_p(args)
        }
        "set--this-command-keys" => super::compat_internal::builtin_set_this_command_keys(args),
        "set-buffer-auto-saved" => super::compat_internal::builtin_set_buffer_auto_saved(args),
        "set-buffer-major-mode" => super::compat_internal::builtin_set_buffer_major_mode(args),
        "set-buffer-multibyte" => super::compat_internal::builtin_set_buffer_multibyte(args),
        "set-buffer-redisplay" => super::compat_internal::builtin_set_buffer_redisplay(args),
        "set-charset-plist" => super::compat_internal::builtin_set_charset_plist(args),
        "set-fontset-font" => super::compat_internal::builtin_set_fontset_font(args),
        "set-frame-selected-window" => {
            super::compat_internal::builtin_set_frame_selected_window(args)
        }
        "set-frame-window-state-change" => {
            super::compat_internal::builtin_set_frame_window_state_change(args)
        }
        "set-fringe-bitmap-face" => super::compat_internal::builtin_set_fringe_bitmap_face(args),
        "set-minibuffer-window" => super::compat_internal::builtin_set_minibuffer_window(args),
        "set-mouse-pixel-position" => {
            super::compat_internal::builtin_set_mouse_pixel_position(args)
        }
        "set-mouse-position" => super::compat_internal::builtin_set_mouse_position(args),
        "set-window-combination-limit" => {
            super::compat_internal::builtin_set_window_combination_limit(args)
        }
        "set-window-new-normal" => super::compat_internal::builtin_set_window_new_normal(args),
        "set-window-new-pixel" => super::compat_internal::builtin_set_window_new_pixel(args),
        "set-window-new-total" => super::compat_internal::builtin_set_window_new_total(args),
        "setplist" => super::compat_internal::builtin_setplist(args),
        "sort-charsets" => super::compat_internal::builtin_sort_charsets(args),
        "split-char" => super::compat_internal::builtin_split_char(args),
        "split-window-internal" => super::compat_internal::builtin_split_window_internal(args),
        "string-distance" => super::compat_internal::builtin_string_distance(args),
        "subst-char-in-region" => super::compat_internal::builtin_subst_char_in_region(args),
        "subr-native-comp-unit" => super::compat_internal::builtin_subr_native_comp_unit(args),
        "subr-native-lambda-list" => super::compat_internal::builtin_subr_native_lambda_list(args),
        "subr-type" => super::compat_internal::builtin_subr_type(args),
        "suspend-emacs" => super::compat_internal::builtin_suspend_emacs(args),
        "this-single-command-keys" => {
            super::compat_internal::builtin_this_single_command_keys(args)
        }
        "this-single-command-raw-keys" => {
            super::compat_internal::builtin_this_single_command_raw_keys(args)
        }
        "thread--blocker" => super::compat_internal::builtin_thread_blocker(args),
        "tool-bar-get-system-style" => {
            super::compat_internal::builtin_tool_bar_get_system_style(args)
        }
        "tool-bar-pixel-width" => super::compat_internal::builtin_tool_bar_pixel_width(args),
        "translate-region-internal" => {
            super::compat_internal::builtin_translate_region_internal(args)
        }
        "transpose-regions" => super::compat_internal::builtin_transpose_regions(args),
        "tty--output-buffer-size" => super::compat_internal::builtin_tty_output_buffer_size(args),
        "tty--set-output-buffer-size" => {
            super::compat_internal::builtin_tty_set_output_buffer_size(args)
        }
        "tty-suppress-bold-inverse-default-colors" => {
            super::compat_internal::builtin_tty_suppress_bold_inverse_default_colors(args)
        }
        "unencodable-char-position" => {
            super::compat_internal::builtin_unencodable_char_position(args)
        }
        "unicode-property-table-internal" => {
            super::compat_internal::builtin_unicode_property_table_internal(args)
        }
        "unify-charset" => super::compat_internal::builtin_unify_charset(args),
        "unix-sync" => super::compat_internal::builtin_unix_sync(args),
        "value<" => super::compat_internal::builtin_value_lt(args),
        "variable-binding-locus" => super::compat_internal::builtin_variable_binding_locus(args),
        "vertical-motion" => super::compat_internal::builtin_vertical_motion(args),
        "x-begin-drag" => super::compat_internal::builtin_x_begin_drag(args),
        "x-create-frame" => super::compat_internal::builtin_x_create_frame(args),
        "x-double-buffered-p" => super::compat_internal::builtin_x_double_buffered_p(args),
        "x-menu-bar-open-internal" => {
            super::compat_internal::builtin_x_menu_bar_open_internal(args)
        }
        "xw-color-defined-p" => super::compat_internal::builtin_xw_color_defined_p(args),
        "xw-color-values" => super::compat_internal::builtin_xw_color_values(args),
        "xw-display-color-p" => super::compat_internal::builtin_xw_display_color_p(args),
        "innermost-minibuffer-p" => super::compat_internal::builtin_innermost_minibuffer_p(args),
        "interactive-form" => super::compat_internal::builtin_interactive_form(args),
        "inotify-add-watch" => super::compat_internal::builtin_inotify_add_watch(args),
        "inotify-rm-watch" => super::compat_internal::builtin_inotify_rm_watch(args),
        "inotify-valid-p" => super::compat_internal::builtin_inotify_valid_p(args),
        "local-variable-if-set-p" => super::compat_internal::builtin_local_variable_if_set_p(args),
        "lock-buffer" => super::compat_internal::builtin_lock_buffer(args),
        "lock-file" => super::compat_internal::builtin_lock_file(args),
        "lossage-size" => super::compat_internal::builtin_lossage_size(args),
        "unlock-buffer" => super::compat_internal::builtin_unlock_buffer(args),
        "unlock-file" => super::compat_internal::builtin_unlock_file(args),
        "window-at" => super::compat_internal::builtin_window_at(args),
        "window-bottom-divider-width" => {
            super::compat_internal::builtin_window_bottom_divider_width(args)
        }
        "window-bump-use-time" => super::compat_internal::builtin_window_bump_use_time(args),
        "window-combination-limit" => {
            super::compat_internal::builtin_window_combination_limit(args)
        }
        "window-left-child" => super::compat_internal::builtin_window_left_child(args),
        "window-line-height" => super::compat_internal::builtin_window_line_height(args),
        "window-lines-pixel-dimensions" => {
            super::compat_internal::builtin_window_lines_pixel_dimensions(args)
        }
        "window-list-1" => super::compat_internal::builtin_window_list_1(args),
        "window-new-normal" => super::compat_internal::builtin_window_new_normal(args),
        "window-new-pixel" => super::compat_internal::builtin_window_new_pixel(args),
        "window-new-total" => super::compat_internal::builtin_window_new_total(args),
        "window-next-sibling" => super::compat_internal::builtin_window_next_sibling(args),
        "window-normal-size" => super::compat_internal::builtin_window_normal_size(args),
        "window-old-body-pixel-height" => {
            super::compat_internal::builtin_window_old_body_pixel_height(args)
        }
        "window-old-body-pixel-width" => {
            super::compat_internal::builtin_window_old_body_pixel_width(args)
        }
        "window-old-pixel-height" => super::compat_internal::builtin_window_old_pixel_height(args),
        "window-old-pixel-width" => super::compat_internal::builtin_window_old_pixel_width(args),
        "window-parent" => super::compat_internal::builtin_window_parent(args),
        "window-pixel-left" => super::compat_internal::builtin_window_pixel_left(args),
        "window-pixel-top" => super::compat_internal::builtin_window_pixel_top(args),
        "window-prev-sibling" => super::compat_internal::builtin_window_prev_sibling(args),
        "window-resize-apply" => super::compat_internal::builtin_window_resize_apply(args),
        "window-resize-apply-total" => {
            super::compat_internal::builtin_window_resize_apply_total(args)
        }
        "window-right-divider-width" => {
            super::compat_internal::builtin_window_right_divider_width(args)
        }
        "window-scroll-bar-height" => {
            super::compat_internal::builtin_window_scroll_bar_height(args)
        }
        "window-scroll-bar-width" => super::compat_internal::builtin_window_scroll_bar_width(args),
        "window-tab-line-height" => super::compat_internal::builtin_window_tab_line_height(args),
        "window-top-child" => super::compat_internal::builtin_window_top_child(args),
        "treesit-available-p" => super::compat_internal::builtin_treesit_available_p(args),
        "treesit-compiled-query-p" => {
            super::compat_internal::builtin_treesit_compiled_query_p(args)
        }
        "treesit-induce-sparse-tree" => {
            super::compat_internal::builtin_treesit_induce_sparse_tree(args)
        }
        "treesit-language-abi-version" => {
            super::compat_internal::builtin_treesit_language_abi_version(args)
        }
        "treesit-language-available-p" => {
            super::compat_internal::builtin_treesit_language_available_p(args)
        }
        "treesit-library-abi-version" => {
            super::compat_internal::builtin_treesit_library_abi_version(args)
        }
        "treesit-node-check" => super::compat_internal::builtin_treesit_node_check(args),
        "treesit-node-child" => super::compat_internal::builtin_treesit_node_child(args),
        "treesit-node-child-by-field-name" => {
            super::compat_internal::builtin_treesit_node_child_by_field_name(args)
        }
        "treesit-node-child-count" => {
            super::compat_internal::builtin_treesit_node_child_count(args)
        }
        "treesit-node-descendant-for-range" => {
            super::compat_internal::builtin_treesit_node_descendant_for_range(args)
        }
        "treesit-node-end" => super::compat_internal::builtin_treesit_node_end(args),
        "treesit-node-eq" => super::compat_internal::builtin_treesit_node_eq(args),
        "treesit-node-field-name-for-child" => {
            super::compat_internal::builtin_treesit_node_field_name_for_child(args)
        }
        "treesit-node-first-child-for-pos" => {
            super::compat_internal::builtin_treesit_node_first_child_for_pos(args)
        }
        "treesit-node-match-p" => super::compat_internal::builtin_treesit_node_match_p(args),
        "treesit-node-next-sibling" => {
            super::compat_internal::builtin_treesit_node_next_sibling(args)
        }
        "treesit-node-p" => super::compat_internal::builtin_treesit_node_p(args),
        "treesit-node-parent" => super::compat_internal::builtin_treesit_node_parent(args),
        "treesit-node-parser" => super::compat_internal::builtin_treesit_node_parser(args),
        "treesit-node-prev-sibling" => {
            super::compat_internal::builtin_treesit_node_prev_sibling(args)
        }
        "treesit-node-start" => super::compat_internal::builtin_treesit_node_start(args),
        "treesit-node-string" => super::compat_internal::builtin_treesit_node_string(args),
        "treesit-node-type" => super::compat_internal::builtin_treesit_node_type(args),
        "treesit-parser-add-notifier" => {
            super::compat_internal::builtin_treesit_parser_add_notifier(args)
        }
        "treesit-parser-buffer" => super::compat_internal::builtin_treesit_parser_buffer(args),
        "treesit-parser-create" => super::compat_internal::builtin_treesit_parser_create(args),
        "treesit-parser-delete" => super::compat_internal::builtin_treesit_parser_delete(args),
        "treesit-parser-included-ranges" => {
            super::compat_internal::builtin_treesit_parser_included_ranges(args)
        }
        "treesit-parser-language" => super::compat_internal::builtin_treesit_parser_language(args),
        "treesit-parser-list" => super::compat_internal::builtin_treesit_parser_list(args),
        "treesit-parser-notifiers" => {
            super::compat_internal::builtin_treesit_parser_notifiers(args)
        }
        "treesit-parser-p" => super::compat_internal::builtin_treesit_parser_p(args),
        "treesit-parser-remove-notifier" => {
            super::compat_internal::builtin_treesit_parser_remove_notifier(args)
        }
        "treesit-parser-root-node" => {
            super::compat_internal::builtin_treesit_parser_root_node(args)
        }
        "treesit-parser-set-included-ranges" => {
            super::compat_internal::builtin_treesit_parser_set_included_ranges(args)
        }
        "treesit-parser-tag" => super::compat_internal::builtin_treesit_parser_tag(args),
        "treesit-pattern-expand" => super::compat_internal::builtin_treesit_pattern_expand(args),
        "treesit-query-capture" => super::compat_internal::builtin_treesit_query_capture(args),
        "treesit-query-compile" => super::compat_internal::builtin_treesit_query_compile(args),
        "treesit-query-expand" => super::compat_internal::builtin_treesit_query_expand(args),
        "treesit-query-language" => super::compat_internal::builtin_treesit_query_language(args),
        "treesit-query-p" => super::compat_internal::builtin_treesit_query_p(args),
        "treesit-search-forward" => super::compat_internal::builtin_treesit_search_forward(args),
        "treesit-search-subtree" => super::compat_internal::builtin_treesit_search_subtree(args),
        "treesit-subtree-stat" => super::compat_internal::builtin_treesit_subtree_stat(args),
        "sqlite-available-p" => super::compat_internal::builtin_sqlite_available_p(args),
        "sqlite-close" => super::compat_internal::builtin_sqlite_close(args),
        "sqlite-columns" => super::compat_internal::builtin_sqlite_columns(args),
        "sqlite-commit" => super::compat_internal::builtin_sqlite_commit(args),
        "sqlite-execute" => super::compat_internal::builtin_sqlite_execute(args),
        "sqlite-execute-batch" => super::compat_internal::builtin_sqlite_execute_batch(args),
        "sqlite-finalize" => super::compat_internal::builtin_sqlite_finalize(args),
        "sqlite-load-extension" => super::compat_internal::builtin_sqlite_load_extension(args),
        "sqlite-more-p" => super::compat_internal::builtin_sqlite_more_p(args),
        "sqlite-next" => super::compat_internal::builtin_sqlite_next(args),
        "sqlite-open" => super::compat_internal::builtin_sqlite_open(args),
        "sqlite-pragma" => super::compat_internal::builtin_sqlite_pragma(args),
        "sqlite-rollback" => super::compat_internal::builtin_sqlite_rollback(args),
        "sqlite-select" => super::compat_internal::builtin_sqlite_select(args),
        "sqlite-transaction" => super::compat_internal::builtin_sqlite_transaction(args),
        "sqlite-version" => super::compat_internal::builtin_sqlite_version(args),
        "sqlitep" => super::compat_internal::builtin_sqlitep(args),
        "fillarray" => super::compat_internal::builtin_fillarray(args),
        "define-hash-table-test" => super::compat_internal::builtin_define_hash_table_test(args),
        "find-coding-systems-region-internal" => {
            super::compat_internal::builtin_find_coding_systems_region_internal(args)
        }
        _ => return None,
    })
}

// ===========================================================================
// Search / Regex builtins (evaluator-dependent)
// ===========================================================================

pub(crate) fn builtin_search_forward(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("search-forward", &args, 1, 4)?;
    let pattern = expect_string(&args[0])?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let opts = parse_search_options(buf, &args, SearchKind::ForwardLiteral)?;
    let start_pt = buf.pt;

    if opts.steps == 0 {
        return Ok(Value::Int(buf.text.byte_to_char(buf.pt) as i64 + 1));
    }

    let mut last_pos = None;
    for _ in 0..opts.steps {
        let result = match opts.direction {
            SearchDirection::Forward => super::regex::search_forward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
            SearchDirection::Backward => super::regex::search_backward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
        };
        match result {
            Ok(Some(pos)) => last_pos = Some(pos),
            Ok(None) => {
                // regex::search_* with `noerror = false` never returns None.
                return Err(signal("search-failed", vec![Value::string(pattern)]));
            }
            Err(_) => {
                return handle_search_failure(
                    buf,
                    &pattern,
                    opts,
                    start_pt,
                    SearchErrorKind::NotFound,
                );
            }
        }
    }

    let end = last_pos.expect("search loop should produce at least one match");
    Ok(Value::Int(buf.text.byte_to_char(end) as i64 + 1))
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SearchDirection {
    Forward,
    Backward,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SearchNoErrorMode {
    Signal,
    KeepPoint,
    MoveToBound,
}

#[derive(Clone, Copy)]
enum SearchKind {
    ForwardLiteral,
    BackwardLiteral,
    ForwardRegexp,
    BackwardRegexp,
}

#[derive(Clone, Copy)]
enum SearchErrorKind {
    NotFound,
}

#[derive(Clone, Copy)]
struct SearchOptions {
    bound: Option<usize>,
    direction: SearchDirection,
    noerror_mode: SearchNoErrorMode,
    steps: usize,
}

fn search_count_arg(args: &[Value]) -> Result<i64, Flow> {
    match args.get(3) {
        None | Some(Value::Nil) => Ok(1),
        Some(Value::Int(n)) => Ok(*n),
        Some(Value::Char(c)) => Ok(*c as i64),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), other.clone()],
        )),
    }
}

fn search_bound_to_byte(buf: &crate::buffer::Buffer, value: &Value) -> Result<usize, Flow> {
    let pos = expect_integer_or_marker(value)?;
    let char_pos = if pos > 0 { pos as usize - 1 } else { 0 };
    let byte = buf.text.char_to_byte(char_pos.min(buf.text.char_count()));
    Ok(byte.clamp(buf.begv, buf.zv))
}

fn parse_search_options(
    buf: &crate::buffer::Buffer,
    args: &[Value],
    kind: SearchKind,
) -> Result<SearchOptions, Flow> {
    let count = search_count_arg(args)?;
    let noerror_mode = match args.get(2) {
        None | Some(Value::Nil) => SearchNoErrorMode::Signal,
        Some(Value::True) => SearchNoErrorMode::KeepPoint,
        Some(_) => SearchNoErrorMode::MoveToBound,
    };
    let (bound_lisp, bound) = match args.get(1) {
        Some(v) if !v.is_nil() => {
            let raw = expect_integer_or_marker(v)?;
            let byte = search_bound_to_byte(buf, v)?;
            (Some(raw), Some(byte))
        }
        _ => (None, None),
    };

    let direction = match kind {
        SearchKind::ForwardLiteral | SearchKind::ForwardRegexp => {
            if count > 0 {
                SearchDirection::Forward
            } else {
                SearchDirection::Backward
            }
        }
        SearchKind::BackwardLiteral | SearchKind::BackwardRegexp => {
            if count < 0 {
                SearchDirection::Forward
            } else {
                SearchDirection::Backward
            }
        }
    };
    let steps = count.unsigned_abs() as usize;

    if let Some(limit) = bound_lisp {
        let point_lisp = buf.text.byte_to_char(buf.pt) as i64 + 1;
        match direction {
            SearchDirection::Forward if limit < point_lisp => {
                return Err(signal(
                    "error",
                    vec![Value::string("Invalid search bound (wrong side of point)")],
                ));
            }
            SearchDirection::Backward if limit > point_lisp => {
                return Err(signal(
                    "error",
                    vec![Value::string("Invalid search bound (wrong side of point)")],
                ));
            }
            _ => {}
        }
    }

    Ok(SearchOptions {
        bound,
        direction,
        noerror_mode,
        steps,
    })
}

fn search_failure_position(buf: &crate::buffer::Buffer, opts: SearchOptions) -> usize {
    match opts.bound {
        Some(limit) => limit.clamp(buf.begv, buf.zv),
        None => match opts.direction {
            SearchDirection::Forward => buf.zv,
            SearchDirection::Backward => buf.begv,
        },
    }
}

fn handle_search_failure(
    buf: &mut crate::buffer::Buffer,
    pattern: &str,
    opts: SearchOptions,
    start_pt: usize,
    kind: SearchErrorKind,
) -> EvalResult {
    match kind {
        SearchErrorKind::NotFound => match opts.noerror_mode {
            SearchNoErrorMode::Signal => {
                buf.goto_char(start_pt);
                Err(signal("search-failed", vec![Value::string(pattern)]))
            }
            SearchNoErrorMode::KeepPoint => {
                buf.goto_char(start_pt);
                Ok(Value::Nil)
            }
            SearchNoErrorMode::MoveToBound => {
                let target = search_failure_position(buf, opts);
                buf.goto_char(target);
                Ok(Value::Nil)
            }
        },
    }
}

pub(crate) fn builtin_search_backward(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("search-backward", &args, 1, 4)?;
    let pattern = expect_string(&args[0])?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let opts = parse_search_options(buf, &args, SearchKind::BackwardLiteral)?;
    let start_pt = buf.pt;

    if opts.steps == 0 {
        return Ok(Value::Int(buf.text.byte_to_char(buf.pt) as i64 + 1));
    }

    let mut last_pos = None;
    for _ in 0..opts.steps {
        let result = match opts.direction {
            SearchDirection::Forward => super::regex::search_forward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
            SearchDirection::Backward => super::regex::search_backward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
        };
        match result {
            Ok(Some(pos)) => last_pos = Some(pos),
            Ok(None) => {
                return Err(signal("search-failed", vec![Value::string(pattern)]));
            }
            Err(_) => {
                return handle_search_failure(
                    buf,
                    &pattern,
                    opts,
                    start_pt,
                    SearchErrorKind::NotFound,
                );
            }
        }
    }

    let end = last_pos.expect("search loop should produce at least one match");
    Ok(Value::Int(buf.text.byte_to_char(end) as i64 + 1))
}

pub(crate) fn builtin_re_search_forward(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("re-search-forward", &args, 1, 4)?;
    let pattern = expect_string(&args[0])?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let opts = parse_search_options(buf, &args, SearchKind::ForwardRegexp)?;
    let start_pt = buf.pt;

    if opts.steps == 0 {
        return Ok(Value::Int(buf.text.byte_to_char(buf.pt) as i64 + 1));
    }

    let mut last_pos = None;
    for _ in 0..opts.steps {
        let result = match opts.direction {
            SearchDirection::Forward => super::regex::re_search_forward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
            SearchDirection::Backward => super::regex::re_search_backward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
        };

        match result {
            Ok(Some(pos)) => last_pos = Some(pos),
            Ok(None) => {
                return Err(signal("search-failed", vec![Value::string(pattern)]));
            }
            Err(msg) if msg.starts_with("Invalid regexp:") => {
                buf.goto_char(start_pt);
                return Err(signal("invalid-regexp", vec![Value::string(msg)]));
            }
            Err(_) => {
                return handle_search_failure(
                    buf,
                    &pattern,
                    opts,
                    start_pt,
                    SearchErrorKind::NotFound,
                );
            }
        }
    }

    let end = last_pos.expect("search loop should produce at least one match");
    Ok(Value::Int(buf.text.byte_to_char(end) as i64 + 1))
}

pub(crate) fn builtin_re_search_backward(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("re-search-backward", &args, 1, 4)?;
    let pattern = expect_string(&args[0])?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let opts = parse_search_options(buf, &args, SearchKind::BackwardRegexp)?;
    let start_pt = buf.pt;

    if opts.steps == 0 {
        return Ok(Value::Int(buf.text.byte_to_char(buf.pt) as i64 + 1));
    }

    let mut last_pos = None;
    for _ in 0..opts.steps {
        let result = match opts.direction {
            SearchDirection::Forward => super::regex::re_search_forward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
            SearchDirection::Backward => super::regex::re_search_backward(
                buf,
                &pattern,
                opts.bound,
                false,
                case_fold,
                &mut eval.match_data,
            ),
        };

        match result {
            Ok(Some(pos)) => last_pos = Some(pos),
            Ok(None) => {
                return Err(signal("search-failed", vec![Value::string(pattern)]));
            }
            Err(msg) if msg.starts_with("Invalid regexp:") => {
                buf.goto_char(start_pt);
                return Err(signal("invalid-regexp", vec![Value::string(msg)]));
            }
            Err(_) => {
                return handle_search_failure(
                    buf,
                    &pattern,
                    opts,
                    start_pt,
                    SearchErrorKind::NotFound,
                );
            }
        }
    }

    let end = last_pos.expect("search loop should produce at least one match");
    Ok(Value::Int(buf.text.byte_to_char(end) as i64 + 1))
}

pub(crate) fn builtin_search_forward_regexp(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("search-forward-regexp", &args, 1, 4)?;
    builtin_re_search_forward(eval, args)
}

pub(crate) fn builtin_search_backward_regexp(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("search-backward-regexp", &args, 1, 4)?;
    builtin_re_search_backward(eval, args)
}

pub(crate) fn builtin_looking_at(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("looking-at", &args, 1, 2)?;
    let pattern = expect_string(&args[0])?;
    let inhibit_modify = args.get(1).is_some_and(|arg| !arg.is_nil());

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);

    let result = if inhibit_modify {
        let mut preserved_match_data = eval.match_data.clone();
        super::regex::looking_at(buf, &pattern, case_fold, &mut preserved_match_data)
    } else {
        super::regex::looking_at(buf, &pattern, case_fold, &mut eval.match_data)
    };

    match result {
        Ok(matched) => Ok(Value::bool(matched)),
        Err(msg) => Err(signal("invalid-regexp", vec![Value::string(msg)])),
    }
}

pub(crate) fn builtin_looking_at_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("looking-at-p", &args, 1)?;
    let pattern = expect_string(&args[0])?;

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);

    let mut throwaway_match_data = None;
    match super::regex::looking_at(buf, &pattern, case_fold, &mut throwaway_match_data) {
        Ok(matched) => Ok(Value::bool(matched)),
        Err(msg) => Err(signal("invalid-regexp", vec![Value::string(msg)])),
    }
}

pub(crate) fn builtin_posix_looking_at(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("posix-looking-at", &args, 1, 2)?;
    builtin_looking_at(eval, args)
}

/// Evaluator-dependent `string-match`: updates match data on the evaluator.
pub(crate) fn builtin_string_match_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("string-match", &args, 2, 4)?;
    let pattern = expect_string(&args[0])?;
    let s = expect_string(&args[1])?;
    let start = normalize_string_start_arg(&s, args.get(2))?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);

    match super::regex::string_match_full_with_case_fold(
        &pattern,
        &s,
        start,
        case_fold,
        &mut eval.match_data,
    ) {
        Ok(Some(pos)) => Ok(Value::Int(s[..pos].chars().count() as i64)),
        Ok(None) => Ok(Value::Nil),
        Err(msg) => Err(signal("invalid-regexp", vec![Value::string(msg)])),
    }
}

pub(crate) fn builtin_string_match_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("string-match-p", &args, 2, 3)?;
    let pattern = expect_string(&args[0])?;
    let s = expect_string(&args[1])?;
    let start = normalize_string_start_arg(&s, args.get(2))?;
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|v| !v.is_nil())
        .unwrap_or(true);
    let mut throwaway = None;

    match super::regex::string_match_full_with_case_fold(
        &pattern,
        &s,
        start,
        case_fold,
        &mut throwaway,
    ) {
        Ok(Some(pos)) => Ok(Value::Int(s[..pos].chars().count() as i64)),
        Ok(None) => Ok(Value::Nil),
        Err(msg) => Err(signal("invalid-regexp", vec![Value::string(msg)])),
    }
}

pub(crate) fn builtin_posix_string_match(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("posix-string-match", &args, 2, 4)?;
    builtin_string_match_eval(eval, args)
}

pub(crate) fn builtin_match_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("match-string", &args, 1, 2)?;
    let group = expect_int(&args[0])?;
    if group < 0 {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(group), Value::Int(0)],
        ));
    }
    let group = group as usize;

    let md = match &eval.match_data {
        Some(md) => md,
        None => return Ok(Value::Nil),
    };

    let (start, end) = match md.groups.get(group) {
        Some(Some(pair)) => *pair,
        _ => return Ok(Value::Nil),
    };

    // If the match was against a string, use that string
    if let Some(ref searched) = md.searched_string {
        if end <= searched.len() {
            return Ok(Value::string(&searched[start..end]));
        }
        return Ok(Value::Nil);
    }

    // Otherwise use current buffer
    // If an optional second arg is a string, use that
    if args.len() > 1 {
        if let Some(s) = args[1].as_str() {
            if end <= s.len() {
                return Ok(Value::string(&s[start..end]));
            }
            return Ok(Value::Nil);
        }
    }

    let buf = match eval.buffers.current_buffer() {
        Some(b) => b,
        None => return Ok(Value::Nil),
    };
    if end <= buf.text.len() {
        Ok(Value::string(buf.text.text_range(start, end)))
    } else {
        Ok(Value::Nil)
    }
}

pub(crate) fn builtin_match_beginning(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("match-beginning", &args, 1)?;
    let group = expect_int(&args[0])?;
    if group < 0 {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(group), Value::Int(0)],
        ));
    }
    let group = group as usize;

    let md = match &eval.match_data {
        Some(md) => md,
        None => return Ok(Value::Nil),
    };

    match md.groups.get(group) {
        Some(Some((start, _end))) => {
            if let Some(searched) = md.searched_string.as_ref() {
                match string_byte_to_char_index(searched, *start) {
                    Some(pos) => Ok(Value::Int(pos as i64)),
                    None => Ok(Value::Nil),
                }
            } else if let Some(buf) = eval.buffers.current_buffer() {
                // Buffer positions are 1-based character positions.
                let pos = buf.text.byte_to_char(*start) as i64 + 1;
                Ok(Value::Int(pos))
            } else {
                Ok(Value::Int(*start as i64))
            }
        }
        Some(None) => Ok(Value::Nil),
        None => Ok(Value::Nil),
    }
}

pub(crate) fn builtin_match_end(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("match-end", &args, 1)?;
    let group = expect_int(&args[0])?;
    if group < 0 {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(group), Value::Int(0)],
        ));
    }
    let group = group as usize;

    let md = match &eval.match_data {
        Some(md) => md,
        None => return Ok(Value::Nil),
    };

    match md.groups.get(group) {
        Some(Some((_start, end))) => {
            if let Some(searched) = md.searched_string.as_ref() {
                match string_byte_to_char_index(searched, *end) {
                    Some(pos) => Ok(Value::Int(pos as i64)),
                    None => Ok(Value::Nil),
                }
            } else if let Some(buf) = eval.buffers.current_buffer() {
                let pos = buf.text.byte_to_char(*end) as i64 + 1;
                Ok(Value::Int(pos))
            } else {
                Ok(Value::Int(*end as i64))
            }
        }
        Some(None) => Ok(Value::Nil),
        None => Ok(Value::Nil),
    }
}

pub(crate) fn builtin_match_data_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("match-data"), Value::Int(args.len() as i64)],
        ));
    }

    let Some(md) = &eval.match_data else {
        return Ok(Value::Nil);
    };

    // Emacs trims trailing unmatched groups from match-data output.
    let mut trailing = md.groups.len();
    while trailing > 0 && md.groups[trailing - 1].is_none() {
        trailing -= 1;
    }

    let mut flat: Vec<Value> = Vec::with_capacity(trailing * 2);
    for grp in md.groups.iter().take(trailing) {
        match grp {
            Some((start, end)) => {
                if let Some(searched) = md.searched_string.as_ref() {
                    let start_char = string_byte_to_char_index(searched, *start);
                    let end_char = string_byte_to_char_index(searched, *end);
                    match (start_char, end_char) {
                        (Some(s), Some(e)) => {
                            flat.push(Value::Int(s as i64));
                            flat.push(Value::Int(e as i64));
                        }
                        _ => {
                            flat.push(Value::Nil);
                            flat.push(Value::Nil);
                        }
                    }
                } else {
                    flat.push(Value::Int(*start as i64));
                    flat.push(Value::Int(*end as i64));
                }
            }
            None => {
                flat.push(Value::Nil);
                flat.push(Value::Nil);
            }
        }
    }
    Ok(Value::list(flat))
}

pub(crate) fn builtin_set_match_data_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-match-data", &args, 1)?;
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("set-match-data"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    if args[0].is_nil() {
        eval.match_data = None;
        return Ok(Value::Nil);
    }

    let items = list_to_vec(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0].clone()],
        )
    })?;

    let mut groups: Vec<Option<(usize, usize)>> = Vec::with_capacity(items.len() / 2);
    let mut i = 0usize;
    while i + 1 < items.len() {
        let start_v = &items[i];
        let end_v = &items[i + 1];

        if start_v.is_nil() && end_v.is_nil() {
            groups.push(None);
            i += 2;
            continue;
        }

        let start = expect_integer_or_marker(start_v)?;
        let end = expect_integer_or_marker(end_v)?;

        // Emacs treats negative marker positions as an end sentinel and
        // truncates remaining groups.
        if start < 0 || end < 0 {
            break;
        }

        groups.push(Some((start as usize, end as usize)));
        i += 2;
    }

    if groups.is_empty() {
        eval.match_data = None;
    } else {
        eval.match_data = Some(super::regex::MatchData {
            groups,
            searched_string: None,
        });
    }

    Ok(Value::Nil)
}

pub(crate) fn builtin_replace_match(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("replace-match", &args, 1)?;
    if args.len() > 5 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("replace-match"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    let newtext = expect_strict_string(&args[0])?;
    let fixedcase = args.len() > 1 && args[1].is_truthy();
    let literal = args.len() > 2 && args[2].is_truthy();
    let string_arg = if args.len() > 3 && !args[3].is_nil() {
        Some(expect_strict_string(&args[3])?)
    } else {
        None
    };
    let subexp = if args.len() > 4 && !args[4].is_nil() {
        let n = expect_int(&args[4])?;
        if n < 0 {
            return if let Some(source) = string_arg.as_ref() {
                Err(signal(
                    "args-out-of-range",
                    vec![
                        Value::Int(n),
                        Value::Int(0),
                        Value::Int(source.chars().count() as i64),
                    ],
                ))
            } else {
                Err(signal("args-out-of-range", vec![Value::Int(n)]))
            };
        }
        n as usize
    } else {
        0usize
    };

    // Clone match_data to avoid borrow conflict
    let md = eval.match_data.clone();
    let missing_subexp_error = super::regex::REPLACE_MATCH_SUBEXP_MISSING;

    if let Some(source) = string_arg {
        if md
            .as_ref()
            .and_then(|m| m.groups.first())
            .and_then(|g| *g)
            .is_none()
            && subexp == 0
        {
            return Err(signal("args-out-of-range", vec![Value::Int(0)]));
        }
        return match super::regex::replace_match_string(
            &source, &newtext, fixedcase, literal, subexp, &md,
        ) {
            Ok(result) => Ok(Value::string(result)),
            Err(msg) if msg == missing_subexp_error && subexp == 0 => {
                Err(signal("args-out-of-range", vec![Value::Int(0)]))
            }
            Err(msg) if msg == missing_subexp_error => {
                if md.as_ref().is_some_and(|m| subexp > m.groups.len()) {
                    Err(signal(
                        "args-out-of-range",
                        vec![
                            Value::Int(subexp as i64),
                            Value::Int(0),
                            Value::Int(source.chars().count() as i64),
                        ],
                    ))
                } else {
                    Err(signal(
                        "error",
                        vec![Value::string(msg), Value::Int(subexp as i64)],
                    ))
                }
            }
            Err(msg) => Err(signal("error", vec![Value::string(msg)])),
        };
    }

    if md.as_ref().is_some_and(|m| m.searched_string.is_some()) {
        return Err(signal("args-out-of-range", vec![Value::Int(0)]));
    }

    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    match super::regex::replace_match_buffer(buf, &newtext, fixedcase, literal, subexp, &md) {
        Ok(()) => Ok(Value::Nil), // Emacs returns nil on buffer replacement
        Err(msg) if msg == missing_subexp_error && subexp == 0 => {
            Err(signal("args-out-of-range", vec![Value::Int(0)]))
        }
        Err(msg) if msg == missing_subexp_error => {
            if md.as_ref().is_some_and(|m| subexp > m.groups.len()) {
                Err(signal(
                    "args-out-of-range",
                    vec![
                        Value::Int(subexp as i64),
                        Value::Int(0),
                        Value::Int(buf.text.char_count() as i64),
                    ],
                ))
            } else {
                Err(signal(
                    "error",
                    vec![Value::string(msg), Value::Int(subexp as i64)],
                ))
            }
        }
        Err(msg) => Err(signal("error", vec![Value::string(msg)])),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::value::{LambdaData, LambdaParams};
    use std::sync::Arc;

    #[test]
    fn pure_dispatch_typed_add_still_works() {
        let result = dispatch_builtin_pure("+", vec![Value::Int(2), Value::Int(3)])
            .expect("builtin + should resolve")
            .expect("builtin + should evaluate");
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn pure_dispatch_typed_percent_and_mod_follow_emacs_sign_rules() {
        let percent = dispatch_builtin_pure("%", vec![Value::Int(-5), Value::Int(2)])
            .expect("builtin % should resolve")
            .expect("builtin % should evaluate");
        let mod_name = dispatch_builtin_pure("mod", vec![Value::Int(-5), Value::Int(2)])
            .expect("builtin mod should resolve")
            .expect("builtin mod should evaluate");
        assert_eq!(percent, Value::Int(-1));
        assert_eq!(mod_name, Value::Int(1));
    }

    #[test]
    fn pure_dispatch_typed_mod_zero_remainder_with_negative_divisor_stays_zero() {
        let int_mod = dispatch_builtin_pure("mod", vec![Value::Int(0), Value::Int(-3)])
            .expect("builtin mod should resolve")
            .expect("builtin mod should evaluate");
        assert_eq!(int_mod, Value::Int(0));

        let float_mod = dispatch_builtin_pure("mod", vec![Value::Float(0.5), Value::Float(-0.5)])
            .expect("builtin mod should resolve")
            .expect("builtin mod should evaluate");
        match float_mod {
            Value::Float(f) => {
                assert_eq!(f, 0.0);
                assert!(!f.is_sign_negative(), "expected +0.0");
            }
            other => panic!("expected float, got {other:?}"),
        }

        let neg_zero_mod =
            dispatch_builtin_pure("mod", vec![Value::Float(-0.5), Value::Float(-0.5)])
                .expect("builtin mod should resolve")
                .expect("builtin mod should evaluate");
        match neg_zero_mod {
            Value::Float(f) => {
                assert_eq!(f, 0.0);
                assert!(f.is_sign_negative(), "expected -0.0");
            }
            other => panic!("expected float, got {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_max_min_preserve_selected_operand_type() {
        let max_int = dispatch_builtin_pure("max", vec![Value::Float(-2.5), Value::Int(1)])
            .expect("builtin max should resolve")
            .expect("builtin max should evaluate");
        assert_eq!(max_int, Value::Int(1));

        let min_int = dispatch_builtin_pure("min", vec![Value::Int(1), Value::Float(1.0)])
            .expect("builtin min should resolve")
            .expect("builtin min should evaluate");
        assert_eq!(min_int, Value::Int(1));

        let max_float = dispatch_builtin_pure("max", vec![Value::Float(1.0), Value::Int(1)])
            .expect("builtin max should resolve")
            .expect("builtin max should evaluate");
        assert_eq!(max_float, Value::Float(1.0));
    }

    #[test]
    fn pure_dispatch_typed_percent_rejects_float_args() {
        let err = dispatch_builtin_pure("%", vec![Value::Float(1.5), Value::Int(2)])
            .expect("builtin % should resolve")
            .expect_err("builtin % should reject non-integer args");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("integer-or-marker-p"), Value::Float(1.5)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_log_bitops_reject_with_integer_or_marker_p() {
        for name in ["logand", "logior", "logxor"] {
            let err = dispatch_builtin_pure(name, vec![Value::Int(1), Value::Float(2.0)])
                .expect("builtin should resolve")
                .expect_err("bit operation should reject non-integer args");
            match err {
                Flow::Signal(sig) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(
                        sig.data,
                        vec![Value::symbol("integer-or-marker-p"), Value::Float(2.0)]
                    );
                }
                other => panic!("unexpected flow: {other:?}"),
            }
        }
    }

    #[test]
    fn pure_dispatch_typed_numeric_symbol_rejections_use_number_or_marker_p() {
        let symbol_arg = Value::symbol("a");
        let cases = [
            ("+", vec![Value::Int(1), symbol_arg.clone()]),
            ("mod", vec![Value::Int(1), symbol_arg.clone()]),
            ("logand", vec![Value::Int(1), symbol_arg.clone()]),
            ("=", vec![Value::Int(1), symbol_arg.clone()]),
        ];

        for (name, args) in cases {
            let err = dispatch_builtin_pure(name, args)
                .expect("builtin should resolve")
                .expect_err("numeric builtin should reject non-numeric symbols");
            match err {
                Flow::Signal(sig) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(
                        sig.data,
                        vec![Value::symbol("number-or-marker-p"), symbol_arg.clone()]
                    );
                }
                other => panic!("unexpected flow: {other:?}"),
            }
        }
    }

    #[test]
    fn pure_dispatch_typed_div_float_zero_uses_ieee_results() {
        let pos_inf = dispatch_builtin_pure("/", vec![Value::Float(1.0), Value::Float(0.0)])
            .expect("builtin / should resolve")
            .expect("float division should evaluate");
        match pos_inf {
            Value::Float(f) => assert!(f.is_infinite() && f.is_sign_positive()),
            other => panic!("expected float, got {other:?}"),
        }

        let neg_inf = dispatch_builtin_pure("/", vec![Value::Float(-1.0), Value::Float(0.0)])
            .expect("builtin / should resolve")
            .expect("float division should evaluate");
        match neg_inf {
            Value::Float(f) => assert!(f.is_infinite() && f.is_sign_negative()),
            other => panic!("expected float, got {other:?}"),
        }

        let neg_nan = dispatch_builtin_pure("/", vec![Value::Float(0.0), Value::Float(0.0)])
            .expect("builtin / should resolve")
            .expect("float division should evaluate");
        match neg_nan {
            Value::Float(f) => assert!(f.is_nan() && f.is_sign_negative()),
            other => panic!("expected float, got {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_ash_handles_extreme_negative_shift_counts() {
        let right = dispatch_builtin_pure("ash", vec![Value::Int(3), Value::Int(i64::MIN)])
            .expect("builtin ash should resolve")
            .expect("builtin ash should evaluate");
        assert_eq!(right, Value::Int(0));

        let right_neg = dispatch_builtin_pure("ash", vec![Value::Int(-3), Value::Int(i64::MIN)])
            .expect("builtin ash should resolve")
            .expect("builtin ash should evaluate");
        assert_eq!(right_neg, Value::Int(-1));
    }

    #[test]
    fn pure_dispatch_typed_abs_min_fixnum_signals_overflow_error() {
        let err = dispatch_builtin_pure("abs", vec![Value::Int(i64::MIN)])
            .expect("builtin abs should resolve")
            .expect_err("abs on i64::MIN should not panic");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "overflow-error"),
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_eq_returns_truthy_for_same_symbol() {
        let sym = Value::symbol("typed-dispatch-test");
        let result = dispatch_builtin_pure("eq", vec![sym.clone(), sym])
            .expect("builtin eq should resolve")
            .expect("builtin eq should evaluate");
        assert!(result.is_truthy());
    }

    #[test]
    fn pure_dispatch_typed_append_concatenates_lists() {
        let left = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let right = Value::list(vec![Value::Int(3), Value::Int(4)]);
        let result = dispatch_builtin_pure("append", vec![left, right])
            .expect("builtin append should resolve")
            .expect("builtin append should evaluate");
        assert_eq!(
            result,
            Value::list(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4)
            ])
        );
    }

    #[test]
    fn pure_dispatch_typed_string_equal_aliases_match() {
        let a = Value::string("neo");
        let b = Value::string("neo");
        let full = dispatch_builtin_pure("string-equal", vec![a.clone(), b.clone()])
            .expect("builtin string-equal should resolve")
            .expect("builtin string-equal should evaluate");
        let short = dispatch_builtin_pure("string=", vec![a, b])
            .expect("builtin string= should resolve")
            .expect("builtin string= should evaluate");
        assert_eq!(full, short);
        assert!(full.is_truthy());
    }

    #[test]
    fn keymapp_accepts_lisp_keymap_cons_cells() {
        let mut eval = super::super::eval::Evaluator::new();

        let proper = Value::list(vec![Value::symbol("keymap")]);
        assert_eq!(
            builtin_keymapp(&mut eval, vec![proper]).unwrap(),
            Value::True
        );

        let proper_with_entry = Value::cons(
            Value::symbol("keymap"),
            Value::cons(
                Value::cons(Value::Int(97), Value::symbol("ignore")),
                Value::Nil,
            ),
        );
        assert_eq!(
            builtin_keymapp(&mut eval, vec![proper_with_entry]).unwrap(),
            Value::True
        );

        let improper = Value::cons(Value::symbol("keymap"), Value::symbol("tail"));
        assert_eq!(
            builtin_keymapp(&mut eval, vec![improper]).unwrap(),
            Value::True
        );

        let non_keymap = Value::list(vec![Value::symbol("foo"), Value::symbol("keymap")]);
        assert_eq!(
            builtin_keymapp(&mut eval, vec![non_keymap]).unwrap(),
            Value::Nil
        );
    }

    #[test]
    fn keymapp_rejects_non_keymap_integer_designators() {
        let mut eval = super::super::eval::Evaluator::new();
        let keymap = builtin_make_sparse_keymap(&mut eval, vec![]).unwrap();
        assert_eq!(
            builtin_keymapp(&mut eval, vec![keymap]).unwrap(),
            Value::True
        );
        assert_eq!(
            builtin_keymapp(&mut eval, vec![Value::Int(16)]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_keymapp(&mut eval, vec![Value::Int(999_999)]).unwrap(),
            Value::Nil
        );
    }

    #[test]
    fn accessible_keymaps_reports_root_and_prefix_paths() {
        let mut eval = super::super::eval::Evaluator::new();
        let root = builtin_make_sparse_keymap(&mut eval, vec![]).unwrap();
        let child = builtin_make_sparse_keymap(&mut eval, vec![]).unwrap();
        builtin_define_key(
            &mut eval,
            vec![root.clone(), Value::string("C-x"), child.clone()],
        )
        .unwrap();

        let all = builtin_accessible_keymaps(&mut eval, vec![root.clone()]).unwrap();
        let all_items = list_to_vec(&all).expect("accessible-keymaps should return list");
        assert_eq!(all_items.len(), 2);

        let first = match &all_items[0] {
            Value::Cons(cell) => cell.lock().expect("poisoned").clone(),
            other => panic!("expected cons cell, got {other:?}"),
        };
        assert_eq!(first.car, Value::vector(vec![]));
        assert_eq!(
            builtin_keymapp(&mut eval, vec![first.cdr.clone()]).unwrap(),
            Value::True
        );

        let filtered = builtin_accessible_keymaps(
            &mut eval,
            vec![root.clone(), Value::vector(vec![Value::Int(24)])],
        )
        .unwrap();
        let filtered_items = list_to_vec(&filtered).expect("filtered accessible-keymaps list");
        assert_eq!(filtered_items.len(), 1);
        let only = match &filtered_items[0] {
            Value::Cons(cell) => cell.lock().expect("poisoned").clone(),
            other => panic!("expected cons cell, got {other:?}"),
        };
        assert_eq!(only.car, Value::vector(vec![Value::Int(24)]));

        let no_match =
            builtin_accessible_keymaps(&mut eval, vec![root, Value::vector(vec![Value::Int(97)])])
                .unwrap();
        assert!(no_match.is_nil());
    }

    #[test]
    fn accessible_keymaps_prefix_type_errors_match_oracle_shape() {
        let mut eval = super::super::eval::Evaluator::new();
        let map = builtin_make_sparse_keymap(&mut eval, vec![]).unwrap();

        let sequence_err =
            builtin_accessible_keymaps(&mut eval, vec![map.clone(), Value::True]).unwrap_err();
        match sequence_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("sequencep"), Value::True]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let array_err =
            builtin_accessible_keymaps(&mut eval, vec![map, Value::list(vec![Value::symbol("a")])])
                .unwrap_err();
        match array_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![
                        Value::symbol("arrayp"),
                        Value::list(vec![Value::symbol("a")])
                    ]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn key_description_renders_super_prefixed_symbol_events_with_expected_angles() {
        let super_only = builtin_key_description(vec![Value::vector(vec![Value::symbol("s-f1")])])
            .expect("key-description should succeed");
        assert_eq!(super_only, Value::string("s-<f1>"));

        let ctrl_super =
            builtin_key_description(vec![Value::vector(vec![Value::symbol("C-s-f1")])])
                .expect("key-description should succeed");
        assert_eq!(ctrl_super, Value::string("C-s-<f1>"));

        let single = builtin_single_key_description(vec![Value::symbol("s-f1")])
            .expect("single-key-description should succeed");
        assert_eq!(single, Value::string("s-<f1>"));
    }

    #[test]
    fn key_description_symbol_modifier_edges_match_emacs() {
        assert_eq!(
            builtin_single_key_description(vec![Value::symbol("M-a")])
                .expect("single-key-description should succeed"),
            Value::string("<M-a>")
        );
        assert_eq!(
            builtin_single_key_description(vec![Value::symbol("C-a")])
                .expect("single-key-description should succeed"),
            Value::string("<C-a>")
        );
        assert_eq!(
            builtin_single_key_description(vec![Value::symbol("C-M-a")])
                .expect("single-key-description should succeed"),
            Value::string("C-<M-a>")
        );
        assert_eq!(
            builtin_single_key_description(vec![Value::symbol("M-a"), Value::True])
                .expect("single-key-description should succeed"),
            Value::string("M-a")
        );
        assert_eq!(
            builtin_key_description(vec![Value::vector(vec![Value::symbol("M-a")])])
                .expect("key-description should succeed"),
            Value::string("<M-a>")
        );
        assert_eq!(
            builtin_key_description(vec![Value::vector(vec![Value::symbol("C-s-f1")])])
                .expect("key-description should succeed"),
            Value::string("C-s-<f1>")
        );
    }

    #[test]
    fn eval_get_file_buffer_matches_visited_paths() {
        let mut eval = super::super::eval::Evaluator::new();
        let id = eval.buffers.create_buffer("gfb");

        let path =
            std::env::temp_dir().join(format!("neovm-gfb-{}-{}", std::process::id(), "eval"));
        std::fs::write(&path, b"gfb").expect("write test file");
        let file = path.to_string_lossy().to_string();
        eval.buffers.get_mut(id).unwrap().file_name = Some(file.clone());

        let exact = builtin_get_file_buffer(&mut eval, vec![Value::string(&file)]).unwrap();
        assert_eq!(exact, Value::Buffer(id));

        let truename = std::fs::canonicalize(&path)
            .expect("canonicalize file")
            .to_string_lossy()
            .to_string();
        let true_match = builtin_get_file_buffer(&mut eval, vec![Value::string(truename)]).unwrap();
        assert_eq!(true_match, Value::Buffer(id));

        let default_dir = format!("{}/", path.parent().unwrap().to_string_lossy());
        let basename = path.file_name().unwrap().to_string_lossy().to_string();
        eval.obarray
            .set_symbol_value("default-directory", Value::string(default_dir));
        let relative = builtin_get_file_buffer(&mut eval, vec![Value::string(basename)]).unwrap();
        assert_eq!(relative, Value::Buffer(id));

        let _ = std::fs::remove_file(path);
    }

    #[test]
    fn eval_get_file_buffer_type_and_missing_paths() {
        let mut eval = super::super::eval::Evaluator::new();
        let missing = builtin_get_file_buffer(
            &mut eval,
            vec![Value::string("/tmp/neovm-no-such-file-for-gfb")],
        )
        .unwrap();
        assert!(missing.is_nil());
        assert!(builtin_get_file_buffer(&mut eval, vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn eval_builtin_rejects_too_many_args() {
        let mut eval = super::super::eval::Evaluator::new();
        let err = builtin_eval(
            &mut eval,
            vec![Value::Int(1), Value::Nil, Value::symbol("ignored")],
        )
        .expect_err("eval should reject more than two arguments");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(sig.data, vec![Value::symbol("eval"), Value::Int(3)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn eval_buffer_live_p_tracks_killed_buffers() {
        let mut eval = super::super::eval::Evaluator::new();
        let buf = builtin_get_buffer_create(&mut eval, vec![Value::string("*blp*")]).unwrap();
        let live = builtin_buffer_live_p(&mut eval, vec![buf.clone()]).unwrap();
        assert_eq!(live, Value::True);

        let _ = builtin_kill_buffer(&mut eval, vec![buf.clone()]).unwrap();
        let dead = builtin_buffer_live_p(&mut eval, vec![buf]).unwrap();
        assert_eq!(dead, Value::Nil);
    }

    #[test]
    fn kill_buffer_optional_arg_and_error_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        let a = builtin_get_buffer_create(&mut eval, vec![Value::string("*kb-opt-a*")]).unwrap();
        let b = builtin_get_buffer_create(&mut eval, vec![Value::string("*kb-opt-b*")]).unwrap();
        let _ = builtin_set_buffer(&mut eval, vec![a.clone()]).unwrap();

        // Optional argument omitted kills current buffer and selects another.
        let killed_current = builtin_kill_buffer(&mut eval, vec![]).unwrap();
        assert_eq!(killed_current, Value::True);
        assert_eq!(
            builtin_buffer_live_p(&mut eval, vec![a.clone()]).unwrap(),
            Value::Nil
        );
        assert!(matches!(
            builtin_current_buffer(&mut eval, vec![]).unwrap(),
            Value::Buffer(_)
        ));

        // Missing buffer name signals `(error "No buffer named ...")`.
        let missing = builtin_kill_buffer(&mut eval, vec![Value::string("*kb-opt-missing*")])
            .expect_err("kill-buffer should signal on missing name");
        match missing {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("No buffer named *kb-opt-missing*")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        // Dead buffer object returns nil.
        let dead =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*kb-opt-dead*")]).unwrap();
        assert_eq!(
            builtin_kill_buffer(&mut eval, vec![dead.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            builtin_kill_buffer(&mut eval, vec![dead]).unwrap(),
            Value::Nil
        );

        // Non-buffer/non-string designators signal `wrong-type-argument`.
        let type_err = builtin_kill_buffer(&mut eval, vec![Value::Int(1)])
            .expect_err("kill-buffer should reject non-string designator");
        match type_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let _ = builtin_kill_buffer(&mut eval, vec![b]).unwrap();
    }

    #[test]
    fn set_buffer_rejects_deleted_buffer_object() {
        let mut eval = super::super::eval::Evaluator::new();
        let dead =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*sb-dead*")]).unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead.clone()]).unwrap();

        let err = builtin_set_buffer(&mut eval, vec![dead])
            .expect_err("set-buffer should reject deleted buffer objects");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Selecting deleted buffer")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn eval_buffer_live_p_non_buffer_objects_return_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let by_name = builtin_buffer_live_p(&mut eval, vec![Value::string("*scratch*")]).unwrap();
        assert_eq!(by_name, Value::Nil);
        let nil_arg = builtin_buffer_live_p(&mut eval, vec![Value::Nil]).unwrap();
        assert_eq!(nil_arg, Value::Nil);
    }

    #[test]
    fn get_buffer_create_accepts_optional_second_arg() {
        let mut eval = super::super::eval::Evaluator::new();
        let first =
            builtin_get_buffer_create(&mut eval, vec![Value::string("*gbc-opt*"), Value::Int(7)])
                .unwrap();
        let second =
            builtin_get_buffer_create(&mut eval, vec![Value::string("*gbc-opt*"), Value::Nil])
                .unwrap();
        assert_eq!(first, second);

        let err = builtin_get_buffer_create(
            &mut eval,
            vec![Value::string("*gbc-opt*"), Value::Nil, Value::Nil],
        )
        .expect_err("get-buffer-create should reject more than two args");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("get-buffer-create"), Value::Int(3)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn buffer_creation_helpers_reject_missing_required_name_arg() {
        let mut eval = super::super::eval::Evaluator::new();

        let err = builtin_get_buffer_create(&mut eval, vec![])
            .expect_err("get-buffer-create should reject missing required arg");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("get-buffer-create"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let err = builtin_generate_new_buffer_name(&mut eval, vec![])
            .expect_err("generate-new-buffer-name should reject missing required arg");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("generate-new-buffer-name"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let err = builtin_generate_new_buffer(&mut eval, vec![])
            .expect_err("generate-new-buffer should reject missing required arg");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("generate-new-buffer"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn get_buffer_rejects_non_string_non_buffer_designators() {
        let mut eval = super::super::eval::Evaluator::new();
        for bad in [Value::Int(1), Value::Nil, Value::symbol("foo")] {
            let err = builtin_get_buffer(&mut eval, vec![bad.clone()])
                .expect_err("get-buffer should reject non-string/non-buffer args");
            match err {
                Flow::Signal(sig) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(sig.data, vec![Value::symbol("stringp"), bad.clone()]);
                }
                other => panic!("unexpected flow: {other:?}"),
            }
        }

        let dead =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*gb-dead*")]).unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead.clone()]).unwrap();
        assert_eq!(
            builtin_get_buffer(&mut eval, vec![dead.clone()]).unwrap(),
            dead
        );
    }

    #[test]
    fn generate_new_buffer_accepts_optional_second_arg() {
        let mut eval = super::super::eval::Evaluator::new();
        let one =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*gnb-opt*"), Value::Nil])
                .unwrap();
        let two =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*gnb-opt*"), Value::Int(1)])
                .unwrap();
        assert!(matches!(one, Value::Buffer(_)));
        assert!(matches!(two, Value::Buffer(_)));
        assert_ne!(one, two);

        let err = builtin_generate_new_buffer(
            &mut eval,
            vec![Value::string("*gnb-opt*"), Value::Nil, Value::Nil],
        )
        .expect_err("generate-new-buffer should reject more than two args");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("generate-new-buffer"), Value::Int(3)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn generate_new_buffer_name_optional_arg_matches_expected_types() {
        let mut eval = super::super::eval::Evaluator::new();
        let _ = builtin_get_buffer_create(&mut eval, vec![Value::string("*gnbn-opt*")]).unwrap();

        let with_nil = builtin_generate_new_buffer_name(
            &mut eval,
            vec![Value::string("*gnbn-opt*"), Value::Nil],
        )
        .unwrap();
        let with_true = builtin_generate_new_buffer_name(
            &mut eval,
            vec![Value::string("*gnbn-opt*"), Value::True],
        )
        .unwrap();
        let with_symbol = builtin_generate_new_buffer_name(
            &mut eval,
            vec![Value::string("*gnbn-opt*"), Value::symbol("ignored")],
        )
        .unwrap();
        let with_keyword = builtin_generate_new_buffer_name(
            &mut eval,
            vec![
                Value::string("*gnbn-opt*"),
                Value::Keyword("ignored".to_string()),
            ],
        )
        .unwrap();
        let with_string = builtin_generate_new_buffer_name(
            &mut eval,
            vec![Value::string("*gnbn-opt*"), Value::string("*gnbn-opt*<9>")],
        )
        .unwrap();

        assert_eq!(with_nil, Value::string("*gnbn-opt*<2>"));
        assert_eq!(with_true, Value::string("*gnbn-opt*<2>"));
        assert_eq!(with_symbol, Value::string("*gnbn-opt*<2>"));
        assert_eq!(with_keyword, Value::string("*gnbn-opt*<2>"));
        assert_eq!(with_string, Value::string("*gnbn-opt*<2>"));

        let err = builtin_generate_new_buffer_name(
            &mut eval,
            vec![
                Value::string("*gnbn-opt*"),
                Value::list(vec![Value::Int(1)]),
            ],
        )
        .expect_err("generate-new-buffer-name should reject non string/symbol optional arg");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("stringp"), Value::list(vec![Value::Int(1)])]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn buffer_size_and_modified_p_return_defaults_for_deleted_buffer_objects() {
        let mut eval = super::super::eval::Evaluator::new();

        let dead_for_size =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*bs-dead*")]).unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead_for_size.clone()]).unwrap();
        let size = builtin_buffer_size(&mut eval, vec![dead_for_size]).unwrap();
        assert_eq!(size, Value::Int(0));

        let dead_for_modified =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*bm-dead*")]).unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead_for_modified.clone()]).unwrap();
        let modified = builtin_buffer_modified_p(&mut eval, vec![dead_for_modified]).unwrap();
        assert_eq!(modified, Value::Nil);
    }

    #[test]
    fn buffer_base_buffer_and_last_name_semantics() {
        let mut eval = super::super::eval::Evaluator::new();

        assert_eq!(
            builtin_buffer_base_buffer(&mut eval, vec![]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_buffer_last_name(&mut eval, vec![]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_buffer_base_buffer(&mut eval, vec![Value::Nil]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_buffer_last_name(&mut eval, vec![Value::Nil]).unwrap(),
            Value::Nil
        );

        let base_type = builtin_buffer_base_buffer(&mut eval, vec![Value::symbol("x")])
            .expect_err("buffer-base-buffer should reject non-buffer, non-nil optional arg");
        match base_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("bufferp"), Value::symbol("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let last_type = builtin_buffer_last_name(&mut eval, vec![Value::symbol("x")])
            .expect_err("buffer-last-name should reject non-buffer, non-nil optional arg");
        match last_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("bufferp"), Value::symbol("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let base_arity = builtin_buffer_base_buffer(&mut eval, vec![Value::Nil, Value::Nil])
            .expect_err("buffer-base-buffer should reject >1 args");
        match base_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("buffer-base-buffer"), Value::Int(2)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let last_arity = builtin_buffer_last_name(&mut eval, vec![Value::Nil, Value::Nil])
            .expect_err("buffer-last-name should reject >1 args");
        match last_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("buffer-last-name"), Value::Int(2)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let dead =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*bln-dead*")]).unwrap();
        let live_name = builtin_buffer_name(&mut eval, vec![dead.clone()]).unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead.clone()]).unwrap();

        assert_eq!(
            builtin_buffer_base_buffer(&mut eval, vec![dead.clone()]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_buffer_last_name(&mut eval, vec![dead]).unwrap(),
            live_name
        );
    }

    #[test]
    fn buffer_modified_tick_semantics() {
        let mut eval = super::super::eval::Evaluator::new();

        assert_eq!(
            builtin_buffer_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            builtin_buffer_chars_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(1)
        );

        builtin_insert(&mut eval, vec![Value::string("x")]).unwrap();
        assert_eq!(
            builtin_buffer_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            builtin_buffer_chars_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(2)
        );

        builtin_set_buffer_modified_p(&mut eval, vec![Value::Nil]).unwrap();
        assert_eq!(
            builtin_buffer_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            builtin_buffer_chars_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(2)
        );

        builtin_delete_region(&mut eval, vec![Value::Int(1), Value::Int(2)]).unwrap();
        assert_eq!(
            builtin_buffer_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            builtin_buffer_chars_modified_tick(&mut eval, vec![]).unwrap(),
            Value::Int(3)
        );

        let dead =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*ticks-dead*")]).unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead.clone()]).unwrap();
        assert_eq!(
            builtin_buffer_modified_tick(&mut eval, vec![dead.clone()]).unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            builtin_buffer_chars_modified_tick(&mut eval, vec![dead]).unwrap(),
            Value::Int(1)
        );

        let type_error = builtin_buffer_modified_tick(&mut eval, vec![Value::symbol("x")])
            .expect_err("buffer-modified-tick should reject non-buffer optional arg");
        match type_error {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("bufferp"), Value::symbol("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let arity_error =
            builtin_buffer_chars_modified_tick(&mut eval, vec![Value::Nil, Value::Nil])
                .expect_err("buffer-chars-modified-tick should reject >1 args");
        match arity_error {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("buffer-chars-modified-tick"), Value::Int(2)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn insert_inherit_variants_reuse_insert_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        assert_eq!(
            builtin_insert_and_inherit(
                &mut eval,
                vec![Value::string("a"), Value::Char('b'), Value::Int('c' as i64)],
            )
            .unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_insert_before_markers_and_inherit(&mut eval, vec![Value::string("d")]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_buffer_string(&mut eval, vec![]).unwrap(),
            Value::string("abcd")
        );

        let type_error =
            builtin_insert_and_inherit(&mut eval, vec![Value::list(vec![Value::Int(1)])])
                .expect_err("insert-and-inherit should reject non char/string values");
        match type_error {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![
                        Value::symbol("char-or-string-p"),
                        Value::list(vec![Value::Int(1)])
                    ]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn insert_buffer_substring_inserts_source_region() {
        let mut eval = super::super::eval::Evaluator::new();
        let source_id = eval.buffers.create_buffer("*ibs-source*");
        eval.buffers.set_current(source_id);
        builtin_insert(&mut eval, vec![Value::string("abcdef")]).unwrap();

        let dest_id = eval.buffers.create_buffer("*ibs-dest*");
        eval.buffers.set_current(dest_id);
        builtin_insert(&mut eval, vec![Value::string("start:")]).unwrap();

        assert_eq!(
            builtin_insert_buffer_substring(
                &mut eval,
                vec![Value::Buffer(source_id), Value::Int(2), Value::Int(5)],
            )
            .unwrap(),
            Value::Nil
        );
        assert_eq!(
            eval.buffers
                .get(dest_id)
                .expect("destination buffer should exist")
                .buffer_string(),
            "start:bcd"
        );

        let bad_designator = builtin_insert_buffer_substring(&mut eval, vec![Value::Int(9)])
            .expect_err("insert-buffer-substring should reject non-buffer designators");
        match bad_designator {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(9)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let bad_start = builtin_insert_buffer_substring(
            &mut eval,
            vec![Value::Buffer(source_id), Value::string("x")],
        )
        .expect_err("insert-buffer-substring should reject non integer-or-marker START");
        match bad_start {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("integer-or-marker-p"), Value::string("x")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn kill_all_local_variables_clears_buffer_locals() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval.buffers.current_buffer_mut().unwrap();
            buf.set_buffer_local("tab-width", Value::Int(8));
            buf.set_buffer_local("fill-column", Value::Int(80));
        }

        assert_eq!(
            builtin_kill_all_local_variables(&mut eval, vec![]).unwrap(),
            Value::Nil
        );

        let buf = eval.buffers.current_buffer().unwrap();
        assert!(buf.get_buffer_local("tab-width").is_none());
        assert!(buf.get_buffer_local("fill-column").is_none());
        assert_eq!(buf.get_buffer_local("buffer-read-only"), Some(&Value::Nil));
    }

    #[test]
    fn ntake_destructively_truncates_lists() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
        ]);
        let kept = builtin_ntake(vec![Value::Int(2), list.clone()]).unwrap();
        assert_eq!(kept, Value::list(vec![Value::Int(1), Value::Int(2)]));
        assert_eq!(
            list_to_vec(&list).expect("list should stay proper after ntake"),
            vec![Value::Int(1), Value::Int(2)]
        );

        let unchanged = Value::list(vec![Value::Int(5), Value::Int(6)]);
        assert_eq!(
            builtin_ntake(vec![Value::Int(10), unchanged.clone()]).unwrap(),
            unchanged
        );
        assert_eq!(
            builtin_ntake(vec![Value::Int(0), list.clone()]).unwrap(),
            Value::Nil
        );

        let type_error = builtin_ntake(vec![Value::Int(1), Value::Int(3)])
            .expect_err("ntake should reject non-list arguments");
        match type_error {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("listp"), Value::Int(3)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn replace_buffer_contents_and_set_buffer_multibyte_runtime_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        let source_id = eval.buffers.create_buffer("*rbc-source*");
        eval.buffers.set_current(source_id);
        builtin_insert(&mut eval, vec![Value::string("source-text")]).unwrap();

        let dest_id = eval.buffers.create_buffer("*rbc-dest*");
        eval.buffers.set_current(dest_id);
        builtin_insert(&mut eval, vec![Value::string("dest-text")]).unwrap();

        assert_eq!(
            builtin_replace_buffer_contents_eval(&mut eval, vec![Value::Buffer(source_id)])
                .unwrap(),
            Value::True
        );
        assert_eq!(
            eval.buffers
                .get(dest_id)
                .expect("destination buffer should exist")
                .buffer_string(),
            "source-text"
        );

        assert_eq!(
            builtin_set_buffer_multibyte_eval(&mut eval, vec![Value::Nil]).unwrap(),
            Value::Nil
        );
        assert!(!eval.buffers.current_buffer().unwrap().multibyte);

        assert_eq!(
            builtin_set_buffer_multibyte_eval(&mut eval, vec![Value::symbol("foo")]).unwrap(),
            Value::symbol("foo")
        );
        assert!(eval.buffers.current_buffer().unwrap().multibyte);
    }

    #[test]
    fn split_window_internal_validates_core_argument_types() {
        let mut eval = super::super::eval::Evaluator::new();
        let split = builtin_split_window_internal(
            &mut eval,
            vec![Value::Nil, Value::Nil, Value::symbol("below"), Value::Nil],
        )
        .unwrap();
        assert!(matches!(split, Value::Window(_)));

        let window_type = builtin_split_window_internal(
            &mut eval,
            vec![
                Value::symbol("not-a-window"),
                Value::Nil,
                Value::symbol("below"),
                Value::Nil,
            ],
        )
        .expect_err("split-window-internal should reject non-window objects");
        match window_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("windowp"), Value::symbol("not-a-window")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let size_type = builtin_split_window_internal(
            &mut eval,
            vec![
                Value::Nil,
                Value::string("bad"),
                Value::symbol("below"),
                Value::Nil,
            ],
        )
        .expect_err("split-window-internal should reject non-fixnum sizes");
        match size_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("fixnump"), Value::string("bad")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let side_type = builtin_split_window_internal(
            &mut eval,
            vec![Value::Nil, Value::Nil, Value::Int(9), Value::Nil],
        )
        .expect_err("split-window-internal should reject non-symbol SIDE");
        match side_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("symbolp"), Value::Int(9)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn barf_bury_char_equal_cl_type_and_cancel_semantics() {
        let mut eval = super::super::eval::Evaluator::new();

        assert!(
            builtin_char_equal(&eval, vec![Value::Int(97), Value::Int(65)])
                .unwrap()
                .is_truthy()
        );
        eval.obarray
            .set_symbol_value("case-fold-search", Value::Nil);
        assert!(
            builtin_char_equal(&eval, vec![Value::Int(97), Value::Int(65)])
                .unwrap()
                .is_nil()
        );
        eval.obarray
            .set_symbol_value("case-fold-search", Value::True);

        let char_type = builtin_char_equal(&eval, vec![Value::Int(1), Value::string("a")])
            .expect_err("char-equal should reject non-character args");
        match char_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("characterp"), Value::string("a")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        assert_eq!(
            builtin_cl_type_of(vec![Value::Nil]).unwrap(),
            Value::symbol("null")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::True]).unwrap(),
            Value::symbol("boolean")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::Int(1)]).unwrap(),
            Value::symbol("fixnum")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::Float(1.0)]).unwrap(),
            Value::symbol("float")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::string("x")]).unwrap(),
            Value::symbol("string")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::symbol("foo")]).unwrap(),
            Value::symbol("symbol")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::cons(Value::Int(1), Value::Int(2))]).unwrap(),
            Value::symbol("cons")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::vector(vec![Value::Int(1)])]).unwrap(),
            Value::symbol("vector")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::hash_table(HashTableTest::Equal)]).unwrap(),
            Value::symbol("hash-table")
        );
        assert_eq!(
            builtin_cl_type_of(vec![Value::Subr("car".to_string())]).unwrap(),
            Value::symbol("primitive-function")
        );
        let lambda = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["x".to_string()]),
            body: Vec::new(),
            env: None,
            docstring: None,
        }));
        assert_eq!(
            builtin_cl_type_of(vec![lambda]).unwrap(),
            Value::symbol("interpreted-function")
        );

        assert_eq!(builtin_cancel_kbd_macro_events(vec![]).unwrap(), Value::Nil);
        let cancel_arity = builtin_cancel_kbd_macro_events(vec![Value::Nil])
            .expect_err("cancel-kbd-macro-events should reject args");
        match cancel_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("cancel-kbd-macro-events"), Value::Int(1)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let barf_buffer = builtin_get_buffer_create(&mut eval, vec![Value::string("*barf*")])
            .expect("create buffer for barf-if-buffer-read-only tests");
        let _ = builtin_set_buffer(&mut eval, vec![barf_buffer]).expect("select barf test buffer");

        assert_eq!(
            builtin_barf_if_buffer_read_only(&mut eval, vec![Value::Int(0)]).unwrap(),
            Value::Nil
        );
        if let Some(buf) = eval.buffers.current_buffer_mut() {
            buf.set_buffer_local("buffer-read-only", Value::True);
        }
        let barf_read_only = builtin_barf_if_buffer_read_only(&mut eval, vec![])
            .expect_err("barf-if-buffer-read-only should signal on read-only buffers");
        match barf_read_only {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "buffer-read-only"),
            other => panic!("unexpected flow: {other:?}"),
        }

        let barf_range = builtin_barf_if_buffer_read_only(&mut eval, vec![Value::Int(0)])
            .expect_err("barf-if-buffer-read-only should check lower-bound positions");
        match barf_range {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(0), Value::Int(0)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let barf_type = builtin_barf_if_buffer_read_only(&mut eval, vec![Value::string("x")])
            .expect_err("barf-if-buffer-read-only should reject non-fixnum positions");
        match barf_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("fixnump"), Value::string("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let barf_arity = builtin_barf_if_buffer_read_only(&mut eval, vec![Value::Nil, Value::Nil])
            .expect_err("barf-if-buffer-read-only should reject >1 args");
        match barf_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("barf-if-buffer-read-only"), Value::Int(2)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
        if let Some(buf) = eval.buffers.current_buffer_mut() {
            buf.set_buffer_local("buffer-read-only", Value::Nil);
        }

        let buffer = builtin_generate_new_buffer(&mut eval, vec![Value::string("*bury*")]).unwrap();
        assert_eq!(
            builtin_bury_buffer_internal(&mut eval, vec![buffer]).unwrap(),
            Value::Nil
        );
        let bury_type = builtin_bury_buffer_internal(&mut eval, vec![Value::symbol("x")])
            .expect_err("bury-buffer-internal should reject non-buffer values");
        match bury_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("bufferp"), Value::symbol("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
        let bury_arity = builtin_bury_buffer_internal(&mut eval, vec![])
            .expect_err("bury-buffer-internal should reject wrong arity");
        match bury_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("bury-buffer-internal"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn byte_position_and_clear_bitmap_semantics() {
        let mut eval = super::super::eval::Evaluator::new();

        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(1)]).unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(2)]).unwrap(),
            Value::Nil
        );

        builtin_erase_buffer(&mut eval, vec![]).unwrap();
        builtin_insert(&mut eval, vec![Value::string("a\u{00E9}")]).unwrap();

        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(1)]).unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(2)]).unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(3)]).unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(4)]).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(5)]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(0)]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_byte_to_position(&mut eval, vec![Value::Int(-1)]).unwrap(),
            Value::Nil
        );

        let byte_to_position_type = builtin_byte_to_position(&mut eval, vec![Value::string("x")])
            .expect_err("byte-to-position should enforce fixnum input");
        match byte_to_position_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("fixnump"), Value::string("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let byte_to_position_arity =
            builtin_byte_to_position(&mut eval, vec![Value::Int(1), Value::Int(2)])
                .expect_err("byte-to-position should reject wrong arity");
        match byte_to_position_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("byte-to-position"), Value::Int(2)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let byte_to_string = builtin_byte_to_string(vec![Value::Int(255)]).unwrap();
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(0), byte_to_string]).unwrap(),
            Value::Int(255)
        );

        let byte_to_string_type = builtin_byte_to_string(vec![Value::symbol("x")])
            .expect_err("byte-to-string should enforce fixnum input");
        match byte_to_string_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("fixnump"), Value::symbol("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let byte_to_string_range = builtin_byte_to_string(vec![Value::Int(256)])
            .expect_err("byte-to-string should reject bytes above 255");
        match byte_to_string_range {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Invalid byte")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        assert_eq!(builtin_bitmap_spec_p(vec![Value::Nil]).unwrap(), Value::Nil);
        let bitmap_arity =
            builtin_bitmap_spec_p(vec![]).expect_err("bitmap-spec-p should reject wrong arity");
        match bitmap_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("bitmap-spec-p"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        assert_eq!(builtin_clear_face_cache(vec![]).unwrap(), Value::Nil);
        assert_eq!(
            builtin_clear_face_cache(vec![Value::symbol("all")]).unwrap(),
            Value::Nil
        );
        let clear_face_arity = builtin_clear_face_cache(vec![Value::Nil, Value::Nil])
            .expect_err("clear-face-cache should reject >1 args");
        match clear_face_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("clear-face-cache"), Value::Int(2)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        assert_eq!(
            builtin_clear_buffer_auto_save_failure(vec![]).unwrap(),
            Value::Nil
        );
        let clear_auto_save_arity = builtin_clear_buffer_auto_save_failure(vec![Value::Nil])
            .expect_err("clear-buffer-auto-save-failure should reject args");
        match clear_auto_save_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![
                        Value::symbol("clear-buffer-auto-save-failure"),
                        Value::Int(1)
                    ]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn buffer_undo_designators_match_deleted_and_missing_buffer_semantics() {
        let mut eval = super::super::eval::Evaluator::new();

        let enable_missing_name =
            builtin_buffer_enable_undo(&mut eval, vec![Value::string("*undo-enable-missing*")])
                .expect_err("buffer-enable-undo missing string should signal");
        match enable_missing_name {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("No buffer named *undo-enable-missing*")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let disable_missing_name =
            builtin_buffer_disable_undo(&mut eval, vec![Value::string("*undo-disable-missing*")])
                .expect_err("buffer-disable-undo missing string should signal wrong-type-argument");
        match disable_missing_name {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Nil]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let dead_for_enable =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*undo-enable-deleted*")])
                .unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead_for_enable.clone()]).unwrap();
        let enable_deleted = builtin_buffer_enable_undo(&mut eval, vec![dead_for_enable]).unwrap();
        assert_eq!(enable_deleted, Value::Nil);

        let dead_for_disable =
            builtin_generate_new_buffer(&mut eval, vec![Value::string("*undo-disable-deleted*")])
                .unwrap();
        let _ = builtin_kill_buffer(&mut eval, vec![dead_for_disable.clone()]).unwrap();
        let disable_deleted = builtin_buffer_disable_undo(&mut eval, vec![dead_for_disable])
            .expect_err("buffer-disable-undo should reject deleted buffer objects");
        match disable_deleted {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Selecting deleted buffer")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn other_buffer_prefers_live_alternative_and_enforces_arity() {
        let mut eval = super::super::eval::Evaluator::new();
        let _ = builtin_get_buffer_create(&mut eval, vec![Value::string("*Messages*")]).unwrap();
        let avoid = builtin_get_buffer_create(&mut eval, vec![Value::string("*ob-avoid*")])
            .expect("create avoid buffer");

        let other = builtin_other_buffer(&mut eval, vec![avoid.clone()]).expect("other-buffer");
        assert!(matches!(other, Value::Buffer(_)));
        assert_ne!(other, avoid);

        let from_non_buffer =
            builtin_other_buffer(&mut eval, vec![Value::Int(1)]).expect("other-buffer int");
        assert!(matches!(from_non_buffer, Value::Buffer(_)));

        let from_missing_name = builtin_other_buffer(&mut eval, vec![Value::string("*missing*")])
            .expect("other-buffer missing name");
        assert!(matches!(from_missing_name, Value::Buffer(_)));

        let err = builtin_other_buffer(
            &mut eval,
            vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil],
        )
        .expect_err("other-buffer should reject more than three args");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(sig.data, vec![Value::symbol("other-buffer"), Value::Int(4)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn featurep_accepts_optional_subfeature_arg() {
        let mut eval = super::super::eval::Evaluator::new();
        eval.set_variable(
            "features",
            Value::list(vec![Value::symbol("vm-featurep-present")]),
        );
        eval.obarray_mut().put_property(
            "vm-featurep-present",
            "subfeatures",
            Value::list(vec![Value::symbol("vm-sub"), Value::Int(1)]),
        );

        let base = builtin_featurep(&mut eval, vec![Value::symbol("vm-featurep-present")]).unwrap();
        assert_eq!(base, Value::True);

        let with_nil = builtin_featurep(
            &mut eval,
            vec![Value::symbol("vm-featurep-present"), Value::Nil],
        )
        .unwrap();
        assert_eq!(with_nil, Value::True);

        let with_sub = builtin_featurep(
            &mut eval,
            vec![
                Value::symbol("vm-featurep-present"),
                Value::symbol("vm-sub"),
            ],
        )
        .unwrap();
        assert_eq!(with_sub, Value::True);

        let with_other = builtin_featurep(
            &mut eval,
            vec![
                Value::symbol("vm-featurep-present"),
                Value::symbol("vm-other"),
            ],
        )
        .unwrap();
        assert_eq!(with_other, Value::Nil);
    }

    #[test]
    fn featurep_subfeatures_property_must_be_list() {
        let mut eval = super::super::eval::Evaluator::new();
        eval.set_variable(
            "features",
            Value::list(vec![Value::symbol("vm-featurep-present")]),
        );
        eval.obarray_mut()
            .put_property("vm-featurep-present", "subfeatures", Value::Int(1));

        let err = builtin_featurep(
            &mut eval,
            vec![
                Value::symbol("vm-featurep-present"),
                Value::symbol("vm-sub"),
            ],
        )
        .expect_err("featurep should signal listp when subfeatures is not a list");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("listp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn featurep_rejects_more_than_two_args() {
        let mut eval = super::super::eval::Evaluator::new();
        let err = builtin_featurep(
            &mut eval,
            vec![
                Value::symbol("vm-featurep-present"),
                Value::Nil,
                Value::symbol("extra"),
            ],
        )
        .expect_err("featurep should reject more than two arguments");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(sig.data, vec![Value::symbol("featurep"), Value::Int(3)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_string_constructor_builds_string() {
        let result = dispatch_builtin_pure(
            "string",
            vec![Value::Int(65), Value::Int(66), Value::Char('C')],
        )
        .expect("builtin string should resolve")
        .expect("builtin string should evaluate");
        assert_eq!(result, Value::string("ABC"));
    }

    #[test]
    fn pure_dispatch_typed_propertize_validates_and_returns_string() {
        let result = dispatch_builtin_pure(
            "propertize",
            vec![
                Value::string("x"),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .expect("builtin propertize should resolve")
        .expect("builtin propertize should evaluate");
        assert_eq!(result, Value::string("x"));
    }

    #[test]
    fn pure_dispatch_typed_propertize_non_string_signals_stringp() {
        let result = dispatch_builtin_pure("propertize", vec![Value::Int(1)])
            .expect("builtin propertize should resolve")
            .expect_err("propertize should reject non-string first arg");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_propertize_odd_property_list_signals_arity() {
        let result = dispatch_builtin_pure(
            "propertize",
            vec![Value::string("x"), Value::symbol("face")],
        )
        .expect("builtin propertize should resolve")
        .expect_err("propertize should reject odd property argument count");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(sig.data, vec![Value::symbol("propertize"), Value::Int(2)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_propertize_accepts_non_symbol_property_keys() {
        let result = dispatch_builtin_pure(
            "propertize",
            vec![Value::string("x"), Value::Int(1), Value::symbol("v")],
        )
        .expect("builtin propertize should resolve")
        .expect("builtin propertize should evaluate");
        assert_eq!(result, Value::string("x"));
    }

    #[test]
    fn pure_dispatch_typed_unibyte_string_round_trips_bytes() {
        let s = dispatch_builtin_pure(
            "unibyte-string",
            vec![Value::Int(65), Value::Int(255), Value::Int(66)],
        )
        .expect("builtin unibyte-string should resolve")
        .expect("builtin unibyte-string should evaluate");

        let len = dispatch_builtin_pure("string-bytes", vec![s.clone()])
            .expect("builtin string-bytes should resolve")
            .expect("builtin string-bytes should evaluate");
        assert_eq!(len, Value::Int(3));

        let a = dispatch_builtin_pure("aref", vec![s.clone(), Value::Int(0)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert_eq!(a, Value::Int(65));

        let ff = dispatch_builtin_pure("aref", vec![s.clone(), Value::Int(1)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert_eq!(ff, Value::Int(255));

        let b = dispatch_builtin_pure("aref", vec![s, Value::Int(2)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert_eq!(b, Value::Int(66));
    }

    #[test]
    fn pure_dispatch_typed_unibyte_string_validates_range_and_type() {
        let out_of_range = dispatch_builtin_pure("unibyte-string", vec![Value::Int(256)])
            .expect("builtin unibyte-string should resolve")
            .expect_err("expected args-out-of-range");
        match out_of_range {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(
                    sig.data,
                    vec![Value::Int(256), Value::Int(0), Value::Int(255)]
                );
            }
            other => panic!("expected signal flow, got {other:?}"),
        }

        let wrong_type = dispatch_builtin_pure("unibyte-string", vec![Value::string("x")])
            .expect("builtin unibyte-string should resolve")
            .expect_err("expected wrong-type-argument");
        match wrong_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("integerp"), Value::string("x")]
                );
            }
            other => panic!("expected signal flow, got {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_vector_builds_vector() {
        let result = dispatch_builtin_pure("vector", vec![Value::Int(7), Value::Int(9)])
            .expect("builtin vector should resolve")
            .expect("builtin vector should evaluate");
        assert_eq!(result, Value::vector(vec![Value::Int(7), Value::Int(9)]));
    }

    #[test]
    fn pure_dispatch_typed_make_vector_validates_wholenump_length() {
        let ok = dispatch_builtin_pure("make-vector", vec![Value::Int(3), Value::symbol("x")])
            .expect("builtin make-vector should resolve")
            .expect("builtin make-vector should evaluate");
        assert_eq!(
            ok,
            Value::vector(vec![Value::symbol("x"), Value::symbol("x"), Value::symbol("x")])
        );

        for bad_len in [Value::Int(-1), Value::Float(1.5), Value::symbol("foo")] {
            let err = dispatch_builtin_pure("make-vector", vec![bad_len.clone(), Value::Nil])
                .expect("builtin make-vector should resolve")
                .expect_err("invalid lengths should signal");
            match err {
                Flow::Signal(sig) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(sig.data, vec![Value::symbol("wholenump"), bad_len]);
                }
                other => panic!("expected signal flow, got {other:?}"),
            }
        }
    }

    #[test]
    fn pure_dispatch_typed_aref_bool_vector_returns_boolean_bits() {
        let bv = Value::vector(vec![
            Value::symbol("--bool-vector--"),
            Value::Int(4),
            Value::Int(0),
            Value::Int(0),
            Value::Int(0),
            Value::Int(0),
        ]);

        let initial = dispatch_builtin_pure("aref", vec![bv.clone(), Value::Int(2)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert!(initial.is_nil());

        let _ = dispatch_builtin_pure("aset", vec![bv.clone(), Value::Int(2), Value::True])
            .expect("builtin aset should resolve")
            .expect("builtin aset should evaluate");

        let updated = dispatch_builtin_pure("aref", vec![bv, Value::Int(2)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert!(updated.is_truthy());
    }

    #[test]
    fn pure_dispatch_typed_aref_aset_char_table_uses_character_index_semantics() {
        let ct = Value::vector(vec![
            Value::symbol("--char-table--"),
            Value::Nil,
            Value::Nil,
            Value::symbol("syntax-table"),
            Value::Int(0),
            Value::Int(i64::MIN + 1),
            Value::Nil,
        ]);

        let initial = dispatch_builtin_pure("aref", vec![ct.clone(), Value::Int(0)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert_eq!(initial, Value::Nil);

        let _ = dispatch_builtin_pure("aset", vec![ct.clone(), Value::Int(0x3F_FFFF), Value::Int(9)])
            .expect("builtin aset should resolve")
            .expect("builtin aset should evaluate");

        let edge = dispatch_builtin_pure("aref", vec![ct.clone(), Value::Int(0x3F_FFFF)])
            .expect("builtin aref should resolve")
            .expect("builtin aref should evaluate");
        assert_eq!(edge, Value::Int(9));

        let elt = dispatch_builtin_pure("elt", vec![ct.clone(), Value::Int(0x3F_FFFF)])
            .expect("builtin elt should resolve")
            .expect("builtin elt should evaluate");
        assert_eq!(elt, Value::Int(9));

        let negative = dispatch_builtin_pure("aref", vec![ct.clone(), Value::Int(-1)])
            .expect("builtin aref should resolve")
            .expect_err("negative char-table index should fail");
        match negative {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("characterp"), Value::Int(-1)],
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let too_large = dispatch_builtin_pure("aset", vec![ct, Value::Int(0x40_0000), Value::Int(1)])
            .expect("builtin aset should resolve")
            .expect_err("out-of-range char-table index should fail");
        match too_large {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("characterp"), Value::Int(0x40_0000)],
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_length_family_uses_bool_vector_logical_length() {
        let bv = Value::vector(vec![
            Value::symbol("--bool-vector--"),
            Value::Int(3),
            Value::Int(1),
            Value::Int(0),
            Value::Int(1),
        ]);

        let len = dispatch_builtin_pure("length", vec![bv.clone()])
            .expect("builtin length should resolve")
            .expect("builtin length should evaluate");
        assert_eq!(len, Value::Int(3));

        let lt = dispatch_builtin_pure("length<", vec![bv.clone(), Value::Int(4)])
            .expect("builtin length< should resolve")
            .expect("builtin length< should evaluate");
        assert_eq!(lt, Value::True);

        let eq = dispatch_builtin_pure("length=", vec![bv.clone(), Value::Int(3)])
            .expect("builtin length= should resolve")
            .expect("builtin length= should evaluate");
        assert_eq!(eq, Value::True);

        let gt = dispatch_builtin_pure("length>", vec![bv, Value::Int(2)])
            .expect("builtin length> should resolve")
            .expect("builtin length> should evaluate");
        assert_eq!(gt, Value::True);
    }

    #[test]
    fn pure_dispatch_typed_length_family_uses_char_table_logical_length() {
        let ct = Value::vector(vec![
            Value::symbol("--char-table--"),
            Value::Nil,
            Value::Nil,
            Value::symbol("syntax-table"),
            Value::Int(0),
            Value::Int(i64::MIN + 1),
            Value::Nil,
        ]);

        let len = dispatch_builtin_pure("length", vec![ct.clone()])
            .expect("builtin length should resolve")
            .expect("builtin length should evaluate");
        assert_eq!(len, Value::Int(0x3F_FFFF));

        let lt = dispatch_builtin_pure("length<", vec![ct.clone(), Value::Int(100)])
            .expect("builtin length< should resolve")
            .expect("builtin length< should evaluate");
        assert_eq!(lt, Value::Nil);

        let eq = dispatch_builtin_pure("length=", vec![ct.clone(), Value::Int(0x3F_FFFF)])
            .expect("builtin length= should resolve")
            .expect("builtin length= should evaluate");
        assert_eq!(eq, Value::True);

        let gt = dispatch_builtin_pure("length>", vec![ct, Value::Int(0)])
            .expect("builtin length> should resolve")
            .expect("builtin length> should evaluate");
        assert_eq!(gt, Value::True);
    }

    #[test]
    fn pure_dispatch_typed_aset_string_returns_new_element_and_computes_replacement() {
        let result = dispatch_builtin_pure(
            "aset",
            vec![Value::string("abc"), Value::Int(1), Value::Int(120)],
        )
        .expect("builtin aset should resolve")
        .expect("builtin aset should evaluate");
        assert_eq!(result, Value::Int(120));

        let replacement =
            aset_string_replacement(&Value::string("abc"), &Value::Int(1), &Value::Int(120))
                .expect("string replacement should succeed");
        assert_eq!(replacement, Value::string("axc"));
    }

    #[test]
    fn pure_dispatch_typed_aset_string_errors_match_oracle() {
        let out_of_range = dispatch_builtin_pure(
            "aset",
            vec![Value::string("abc"), Value::Int(-1), Value::Int(120)],
        )
        .expect("builtin aset should resolve")
        .expect_err("aset should reject negative index");
        match out_of_range {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::string("abc"), Value::Int(-1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let wrong_type = dispatch_builtin_pure(
            "aset",
            vec![Value::string("abc"), Value::Int(1), Value::Nil],
        )
        .expect("builtin aset should resolve")
        .expect_err("aset should validate replacement character");
        match wrong_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("characterp"), Value::Nil]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_char_string_conversions_work() {
        let as_code = dispatch_builtin_pure("string-to-char", vec![Value::string("A")])
            .expect("builtin string-to-char should resolve")
            .expect("builtin string-to-char should evaluate");
        assert_eq!(as_code, Value::Int(65));

        let as_string = dispatch_builtin_pure("char-to-string", vec![Value::Int(65)])
            .expect("builtin char-to-string should resolve")
            .expect("builtin char-to-string should evaluate");
        assert_eq!(as_string, Value::string("A"));
    }

    #[test]
    fn pure_dispatch_typed_hash_table_round_trip() {
        let table = dispatch_builtin_pure(
            "make-hash-table",
            vec![Value::keyword(":test"), Value::symbol("equal")],
        )
        .expect("builtin make-hash-table should resolve")
        .expect("builtin make-hash-table should evaluate");

        dispatch_builtin_pure(
            "puthash",
            vec![Value::string("answer"), Value::Int(42), table.clone()],
        )
        .expect("builtin puthash should resolve")
        .expect("builtin puthash should evaluate");

        let value = dispatch_builtin_pure("gethash", vec![Value::string("answer"), table.clone()])
            .expect("builtin gethash should resolve")
            .expect("builtin gethash should evaluate");
        assert_eq!(value, Value::Int(42));

        let count = dispatch_builtin_pure("hash-table-count", vec![table])
            .expect("builtin hash-table-count should resolve")
            .expect("builtin hash-table-count should evaluate");
        assert_eq!(count, Value::Int(1));
    }

    #[test]
    fn pure_dispatch_typed_plist_and_symbol_round_trip() {
        let plist = dispatch_builtin_pure(
            "plist-put",
            vec![Value::Nil, Value::keyword(":lang"), Value::string("rust")],
        )
        .expect("builtin plist-put should resolve")
        .expect("builtin plist-put should evaluate");

        let lang = dispatch_builtin_pure("plist-get", vec![plist, Value::keyword(":lang")])
            .expect("builtin plist-get should resolve")
            .expect("builtin plist-get should evaluate");
        assert_eq!(lang, Value::string("rust"));

        let sym = dispatch_builtin_pure("make-symbol", vec![Value::string("neo-vm")])
            .expect("builtin make-symbol should resolve")
            .expect("builtin make-symbol should evaluate");
        let name = dispatch_builtin_pure("symbol-name", vec![sym])
            .expect("builtin symbol-name should resolve")
            .expect("builtin symbol-name should evaluate");
        assert_eq!(name, Value::string("neo-vm"));
    }

    #[test]
    fn pure_dispatch_typed_math_ops_work() {
        let sqrt = dispatch_builtin_pure("sqrt", vec![Value::Int(4)])
            .expect("builtin sqrt should resolve")
            .expect("builtin sqrt should evaluate");
        assert_eq!(sqrt, Value::Float(2.0));

        let expt = dispatch_builtin_pure("expt", vec![Value::Int(2), Value::Int(8)])
            .expect("builtin expt should resolve")
            .expect("builtin expt should evaluate");
        assert_eq!(expt, Value::Int(256));

        let nan_check = dispatch_builtin_pure("isnan", vec![Value::Float(f64::NAN)])
            .expect("builtin isnan should resolve")
            .expect("builtin isnan should evaluate");
        assert!(nan_check.is_truthy());
    }

    #[test]
    fn pure_dispatch_typed_expt_and_isnan_type_errors_match_oracle() {
        let expt_base = dispatch_builtin_pure("expt", vec![Value::symbol("a"), Value::Int(2)])
            .expect("builtin expt should resolve")
            .expect_err("expt should reject non-numeric base");
        match expt_base {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("numberp"), Value::symbol("a")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let expt_exp = dispatch_builtin_pure("expt", vec![Value::Int(2), Value::symbol("a")])
            .expect("builtin expt should resolve")
            .expect_err("expt should reject non-numeric exponent");
        match expt_exp {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("numberp"), Value::symbol("a")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let isnan_non_float = dispatch_builtin_pure("isnan", vec![Value::Int(1)])
            .expect("builtin isnan should resolve")
            .expect_err("isnan should reject non-floats");
        match isnan_non_float {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("floatp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn pure_dispatch_typed_round_half_ties_to_even() {
        let positive_half = dispatch_builtin_pure("round", vec![Value::Float(2.5)])
            .expect("builtin round should resolve")
            .expect("builtin round should evaluate");
        assert_eq!(positive_half, Value::Int(2));

        let negative_half = dispatch_builtin_pure("round", vec![Value::Float(-2.5)])
            .expect("builtin round should resolve")
            .expect("builtin round should evaluate");
        assert_eq!(negative_half, Value::Int(-2));

        let zero_half = dispatch_builtin_pure("round", vec![Value::Float(0.5)])
            .expect("builtin round should resolve")
            .expect("builtin round should evaluate");
        assert_eq!(zero_half, Value::Int(0));

        let negative_zero_half = dispatch_builtin_pure("round", vec![Value::Float(-0.5)])
            .expect("builtin round should resolve")
            .expect("builtin round should evaluate");
        assert_eq!(negative_zero_half, Value::Int(0));
    }

    #[test]
    fn pure_dispatch_typed_extended_string_ops_work() {
        let prefix = dispatch_builtin_pure(
            "string-prefix-p",
            vec![Value::string("neo"), Value::string("neovm")],
        )
        .expect("builtin string-prefix-p should resolve")
        .expect("builtin string-prefix-p should evaluate");
        assert!(prefix.is_truthy());

        let trimmed = dispatch_builtin_pure("string-trim", vec![Value::string("  vm  ")])
            .expect("builtin string-trim should resolve")
            .expect("builtin string-trim should evaluate");
        assert_eq!(trimmed, Value::string("vm"));

        let width = dispatch_builtin_pure("string-width", vec![Value::string("ab")])
            .expect("builtin string-width should resolve")
            .expect("builtin string-width should evaluate");
        assert_eq!(width, Value::Int(2));

        let bytes = dispatch_builtin_pure("string-bytes", vec![Value::string("ab")])
            .expect("builtin string-bytes should resolve")
            .expect("builtin string-bytes should evaluate");
        assert_eq!(bytes, Value::Int(2));
    }

    #[test]
    fn pure_dispatch_typed_extended_list_ops_work() {
        let seq = dispatch_builtin_pure("number-sequence", vec![Value::Int(1), Value::Int(4)])
            .expect("builtin number-sequence should resolve")
            .expect("builtin number-sequence should evaluate");
        assert_eq!(
            seq,
            Value::list(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
                Value::Int(4)
            ])
        );

        let last = dispatch_builtin_pure("last", vec![seq])
            .expect("builtin last should resolve")
            .expect("builtin last should evaluate");
        assert_eq!(last, Value::list(vec![Value::Int(4)]));
    }

    #[test]
    fn pure_dispatch_typed_ignore_accepts_any_arity() {
        let zero = dispatch_builtin_pure("ignore", vec![])
            .expect("builtin ignore should resolve")
            .expect("builtin ignore should evaluate");
        assert!(zero.is_nil());

        let many = dispatch_builtin_pure(
            "ignore",
            vec![Value::Int(1), Value::string("x"), Value::symbol("foo")],
        )
        .expect("builtin ignore should resolve")
        .expect("builtin ignore should evaluate");
        assert!(many.is_nil());
    }

    #[test]
    fn match_data_round_trip_with_nil_groups() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        builtin_set_match_data_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::Int(0),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
                Value::Int(5),
                Value::Int(7),
            ])],
        )
        .expect("set-match-data should succeed");

        let md = builtin_match_data_eval(&mut eval, vec![]).expect("match-data should succeed");
        assert_eq!(
            md,
            Value::list(vec![
                Value::Int(0),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
                Value::Int(5),
                Value::Int(7)
            ])
        );
    }

    #[test]
    fn match_beginning_end_return_nil_without_match_data() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        builtin_set_match_data_eval(&mut eval, vec![Value::Nil]).expect("set-match-data nil");

        let beg = builtin_match_beginning(&mut eval, vec![Value::Int(0)])
            .expect("match-beginning should not error");
        let end =
            builtin_match_end(&mut eval, vec![Value::Int(0)]).expect("match-end should not error");
        assert!(beg.is_nil());
        assert!(end.is_nil());
    }

    #[test]
    fn negative_match_group_signals_args_out_of_range() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let match_string_err = builtin_match_string(&mut eval, vec![Value::Int(-1)])
            .expect_err("negative subgroup should signal");
        match match_string_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(-1), Value::Int(0)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let match_beginning_err = builtin_match_beginning(&mut eval, vec![Value::Int(-1)])
            .expect_err("negative subgroup should signal");
        match match_beginning_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(-1), Value::Int(0)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let match_end_err = builtin_match_end(&mut eval, vec![Value::Int(-1)])
            .expect_err("negative subgroup should signal");
        match match_end_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(-1), Value::Int(0)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn buffer_region_negative_bounds_signal_without_panicking() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        builtin_insert(&mut eval, vec![Value::string("abc")]).expect("insert should succeed");
        let current = builtin_current_buffer(&mut eval, vec![]).expect("current-buffer should work");

        let substring_err = builtin_buffer_substring(&mut eval, vec![Value::Int(-1), Value::Int(2)])
            .expect_err("negative start should signal");
        match substring_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![current.clone(), Value::Int(-1), Value::Int(2)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let delete_err = builtin_delete_region(&mut eval, vec![Value::Int(-1), Value::Int(2)])
            .expect_err("negative start should signal");
        match delete_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![current, Value::Int(-1), Value::Int(2)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let narrow_err = builtin_narrow_to_region(&mut eval, vec![Value::Int(-1), Value::Int(2)])
            .expect_err("negative start should signal");
        match narrow_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(-1), Value::Int(2)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        assert_eq!(
            builtin_char_after(&mut eval, vec![Value::Int(-1)]).expect("char-after should succeed"),
            Value::Nil
        );
        assert_eq!(
            builtin_char_before(&mut eval, vec![Value::Int(0)]).expect("char-before should succeed"),
            Value::Nil
        );
    }

    #[test]
    fn string_match_start_handles_nil_and_negative_offsets() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let with_nil = builtin_string_match_eval(
            &mut eval,
            vec![Value::string("a"), Value::string("ba"), Value::Nil],
        )
        .expect("string-match with nil start");
        assert_eq!(with_nil, Value::Int(1));

        let with_negative = builtin_string_match_eval(
            &mut eval,
            vec![Value::string("a"), Value::string("ba"), Value::Int(-1)],
        )
        .expect("string-match with negative start");
        assert_eq!(with_negative, Value::Int(1));

        let out_of_range = builtin_string_match_eval(
            &mut eval,
            vec![Value::string("a"), Value::string("ba"), Value::Int(3)],
        );
        assert!(out_of_range.is_err());
    }

    #[test]
    fn search_match_runtime_arity_edges_match_oracle_contracts() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let search_over_arity = builtin_search_forward(
            &mut eval,
            vec![
                Value::string("a"),
                Value::Nil,
                Value::Nil,
                Value::Int(1),
                Value::Nil,
            ],
        );
        assert!(matches!(
            search_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let regex_over_arity = builtin_re_search_forward(
            &mut eval,
            vec![
                Value::string("a"),
                Value::Nil,
                Value::Nil,
                Value::Int(1),
                Value::Nil,
            ],
        );
        assert!(matches!(
            regex_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let looking_at_optional_second =
            builtin_looking_at(&mut eval, vec![Value::string("a"), Value::True]);
        assert!(looking_at_optional_second.is_ok());

        let looking_at_over_arity =
            builtin_looking_at(&mut eval, vec![Value::string("a"), Value::Nil, Value::Nil]);
        assert!(matches!(
            looking_at_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let looking_at_p_over_arity =
            builtin_looking_at_p(&mut eval, vec![Value::string("a"), Value::Nil]);
        assert!(matches!(
            looking_at_p_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let looking_at_p_bad_type = builtin_looking_at_p(&mut eval, vec![Value::Int(1)]);
        assert!(matches!(
            looking_at_p_bad_type,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));

        let match_string_over_arity = builtin_match_string(
            &mut eval,
            vec![Value::Int(0), Value::string("a"), Value::Nil],
        );
        assert!(matches!(
            match_string_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let replace_match_over_arity = builtin_replace_match(
            &mut eval,
            vec![
                Value::string("x"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            replace_match_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let string_match_over_arity = builtin_string_match_eval(
            &mut eval,
            vec![
                Value::string("a"),
                Value::string("a"),
                Value::Int(0),
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            string_match_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let string_match_p_over_arity = builtin_string_match_p_eval(
            &mut eval,
            vec![
                Value::string("a"),
                Value::string("a"),
                Value::Int(0),
                Value::Nil,
            ],
        );
        assert!(matches!(
            string_match_p_over_arity,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn set_match_data_rejects_non_list() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let result = builtin_set_match_data_eval(&mut eval, vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn looking_at_inhibit_modify_preserves_match_data() {
        use crate::elisp::eval::Evaluator;

        let mut eval = Evaluator::new();
        {
            let buffer = eval.buffers.current_buffer_mut().expect("scratch buffer");
            buffer.insert("abc");
            buffer.goto_char(0);
        }

        let baseline = Value::list(vec![Value::Int(10), Value::Int(11)]);
        builtin_set_match_data_eval(&mut eval, vec![baseline.clone()])
            .expect("setting baseline match-data");
        let result = builtin_looking_at(&mut eval, vec![Value::string("a"), Value::True]);
        assert!(result.is_ok());

        let observed = builtin_match_data_eval(&mut eval, vec![]).expect("read match-data");
        assert_eq!(observed, baseline);
    }

    #[test]
    fn looking_at_updates_match_data_when_allowed() {
        use crate::elisp::eval::Evaluator;

        let mut eval = Evaluator::new();
        {
            let buffer = eval.buffers.current_buffer_mut().expect("scratch buffer");
            buffer.insert("abc");
            buffer.goto_char(0);
        }

        builtin_set_match_data_eval(&mut eval, vec![Value::Nil]).expect("clear match-data");
        let result = builtin_looking_at(&mut eval, vec![Value::string("a"), Value::Nil]);
        assert!(result.is_ok());

        let observed = builtin_match_data_eval(&mut eval, vec![]).expect("read match-data");
        assert_eq!(observed, Value::list(vec![Value::Int(0), Value::Int(1)]));
    }

    #[test]
    fn looking_at_p_preserves_match_data() {
        use crate::elisp::eval::Evaluator;

        let mut eval = Evaluator::new();
        {
            let buffer = eval.buffers.current_buffer_mut().expect("scratch buffer");
            buffer.insert("abc");
            buffer.goto_char(0);
        }

        let baseline = Value::list(vec![Value::Int(1), Value::Int(2)]);
        builtin_set_match_data_eval(&mut eval, vec![baseline.clone()]).expect("seed baseline");
        let _ = builtin_looking_at_p(&mut eval, vec![Value::string("z")])
            .expect("looking-at-p handles non-match");
        let observed = builtin_match_data_eval(&mut eval, vec![]).expect("read match-data");
        assert_eq!(observed, baseline);
    }

    #[test]
    fn looking_at_p_respects_case_fold_search() {
        use crate::elisp::eval::Evaluator;

        let mut eval = Evaluator::new();
        {
            let buffer = eval.buffers.current_buffer_mut().expect("scratch buffer");
            buffer.insert("A");
            buffer.goto_char(0);
        }

        eval.set_variable("case-fold-search", Value::Nil);
        let sensitive = builtin_looking_at(&mut eval, vec![Value::string("a"), Value::Nil])
            .expect("looking-at with case-fold-search=nil");
        let sensitive_p = builtin_looking_at_p(&mut eval, vec![Value::string("a")])
            .expect("looking-at-p with case-fold-search=nil");
        assert!(sensitive.is_nil());
        assert!(sensitive_p.is_nil());

        eval.set_variable("case-fold-search", Value::True);
        let insensitive = builtin_looking_at(&mut eval, vec![Value::string("a"), Value::Nil])
            .expect("looking-at with case-fold-search=t");
        let insensitive_p = builtin_looking_at_p(&mut eval, vec![Value::string("a")])
            .expect("looking-at-p with case-fold-search=t");
        assert!(insensitive.is_truthy());
        assert!(insensitive_p.is_truthy());
    }

    #[test]
    fn dispatch_builtin_resolves_typed_only_pure_names() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let result = dispatch_builtin(
            &mut eval,
            "string-equal",
            vec![Value::string("neo"), Value::string("neo")],
        )
        .expect("dispatch_builtin should resolve string-equal")
        .expect("dispatch_builtin should evaluate string-equal");
        assert!(result.is_truthy());
    }

    #[test]
    fn prin1_to_string_prints_canonical_threading_handles_as_opaque() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let thread = dispatch_builtin(&mut eval, "current-thread", vec![])
            .expect("current-thread should resolve")
            .expect("current-thread should evaluate");
        let thread_text = dispatch_builtin(&mut eval, "prin1-to-string", vec![thread])
            .expect("prin1-to-string should resolve for thread")
            .expect("prin1-to-string should evaluate for thread");
        assert_eq!(thread_text, Value::string("#<thread 0>"));

        let mutex = dispatch_builtin(&mut eval, "make-mutex", vec![])
            .expect("make-mutex should resolve")
            .expect("make-mutex should evaluate");
        let mutex_text = dispatch_builtin(&mut eval, "prin1-to-string", vec![mutex.clone()])
            .expect("prin1-to-string should resolve for mutex")
            .expect("prin1-to-string should evaluate for mutex");
        assert_eq!(mutex_text, Value::string("#<mutex 1>"));

        let condvar = dispatch_builtin(&mut eval, "make-condition-variable", vec![mutex])
            .expect("make-condition-variable should resolve")
            .expect("make-condition-variable should evaluate");
        let condvar_text = dispatch_builtin(&mut eval, "prin1-to-string", vec![condvar])
            .expect("prin1-to-string should resolve for condvar")
            .expect("prin1-to-string should evaluate for condvar");
        assert_eq!(condvar_text, Value::string("#<condvar 1>"));
    }

    #[test]
    fn prin1_to_string_keeps_forged_threading_handles_as_cons() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let forged_thread = Value::cons(Value::symbol("thread"), Value::Int(0));
        let thread_text = dispatch_builtin(&mut eval, "prin1-to-string", vec![forged_thread])
            .expect("prin1-to-string should resolve for forged thread")
            .expect("prin1-to-string should evaluate for forged thread");
        assert_eq!(thread_text, Value::string("(thread . 0)"));

        let forged_mutex = Value::cons(Value::symbol("mutex"), Value::Int(1));
        let mutex_text = dispatch_builtin(&mut eval, "prin1-to-string", vec![forged_mutex])
            .expect("prin1-to-string should resolve for forged mutex")
            .expect("prin1-to-string should evaluate for forged mutex");
        assert_eq!(mutex_text, Value::string("(mutex . 1)"));

        let forged_condvar = Value::cons(Value::symbol("condition-variable"), Value::Int(1));
        let condvar_text = dispatch_builtin(&mut eval, "prin1-to-string", vec![forged_condvar])
            .expect("prin1-to-string should resolve for forged condvar")
            .expect("prin1-to-string should evaluate for forged condvar");
        assert_eq!(condvar_text, Value::string("(condition-variable . 1)"));
    }

    #[test]
    fn prin1_to_string_supports_noescape_for_strings() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let value = Value::string("a\nb");

        let escaped = dispatch_builtin(&mut eval, "prin1-to-string", vec![value.clone()])
            .expect("prin1-to-string should resolve")
            .expect("prin1-to-string should evaluate");
        assert_eq!(escaped, Value::string("\"a\\nb\""));

        let noescape = dispatch_builtin(&mut eval, "prin1-to-string", vec![value, Value::True])
            .expect("prin1-to-string should resolve with noescape")
            .expect("prin1-to-string should evaluate with noescape");
        assert_eq!(noescape, Value::string("a\nb"));
    }

    #[test]
    fn prin1_to_string_ignores_extra_args_for_compat() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let result = dispatch_builtin(
            &mut eval,
            "prin1-to-string",
            vec![Value::Int(1), Value::Nil, Value::Nil],
        )
        .expect("prin1-to-string should resolve with extra args")
        .expect("prin1-to-string should evaluate with extra args");
        assert_eq!(result, Value::string("1"));
    }

    #[test]
    fn format_prints_thread_handles_as_opaque_in_eval_dispatch() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let thread = dispatch_builtin(&mut eval, "current-thread", vec![])
            .expect("current-thread should resolve")
            .expect("current-thread should evaluate");

        let upper = dispatch_builtin(
            &mut eval,
            "format",
            vec![Value::string("%S"), thread.clone()],
        )
        .expect("format should resolve for %S")
        .expect("format should evaluate for %S");
        assert!(upper.as_str().is_some_and(|s| s.starts_with("#<thread")));

        let lower = dispatch_builtin(&mut eval, "format", vec![Value::string("%s"), thread])
            .expect("format should resolve for %s")
            .expect("format should evaluate for %s");
        assert!(lower.as_str().is_some_and(|s| s.starts_with("#<thread")));
    }

    #[test]
    fn message_prints_thread_handles_as_opaque_in_eval_dispatch() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let thread = dispatch_builtin(&mut eval, "current-thread", vec![])
            .expect("current-thread should resolve")
            .expect("current-thread should evaluate");
        let message = dispatch_builtin(&mut eval, "message", vec![Value::string("%S"), thread])
            .expect("message should resolve")
            .expect("message should evaluate");
        assert!(message.as_str().is_some_and(|s| s.starts_with("#<thread")));
    }

    #[test]
    fn message_nil_returns_nil() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let raw = builtin_message(vec![Value::Nil]).expect("message should accept nil");
        assert!(raw.is_nil());

        let eval_result = builtin_message_eval(&mut eval, vec![Value::Nil])
            .expect("message eval should accept nil");
        assert!(eval_result.is_nil());
    }

    #[test]
    fn internal_save_selected_window_helpers_restore_selected_window() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let selected = dispatch_builtin(&mut eval, "selected-window", vec![])
            .expect("selected-window should resolve")
            .expect("selected-window should evaluate");
        let split = dispatch_builtin(&mut eval, "split-window", vec![selected.clone()])
            .expect("split-window should resolve")
            .expect("split-window should evaluate");
        let state = dispatch_builtin(
            &mut eval,
            "internal--before-save-selected-window",
            vec![],
        )
        .expect("before helper should resolve")
        .expect("before helper should evaluate");

        let _ = dispatch_builtin(&mut eval, "select-window", vec![split.clone()])
            .expect("select-window should resolve")
            .expect("select-window should evaluate");
        let switched = dispatch_builtin(&mut eval, "selected-window", vec![])
            .expect("selected-window should resolve")
            .expect("selected-window should evaluate");
        assert_eq!(switched, split);

        let restored = dispatch_builtin(
            &mut eval,
            "internal--after-save-selected-window",
            vec![state],
        )
        .expect("after helper should resolve")
        .expect("after helper should evaluate");
        assert!(restored.is_nil());

        let selected_again = dispatch_builtin(&mut eval, "selected-window", vec![])
            .expect("selected-window should resolve")
            .expect("selected-window should evaluate");
        assert_eq!(selected_again, selected);
    }

    #[test]
    fn fboundp_recognizes_dispatch_and_typed_builtin_names() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let message = builtin_fboundp(&mut eval, vec![Value::symbol("message")])
            .expect("fboundp should succeed for message");
        assert!(message.is_truthy());

        let load = builtin_fboundp(&mut eval, vec![Value::symbol("load")])
            .expect("fboundp should succeed for load");
        assert!(load.is_truthy());

        let symbol_value = builtin_fboundp(&mut eval, vec![Value::symbol("symbol-value")])
            .expect("fboundp should succeed for symbol-value");
        assert!(symbol_value.is_truthy());

        let random = builtin_fboundp(&mut eval, vec![Value::symbol("random")])
            .expect("fboundp should succeed for random");
        assert!(random.is_truthy());

        let read_key = builtin_fboundp(&mut eval, vec![Value::symbol("read-key")])
            .expect("fboundp should succeed for read-key");
        assert!(read_key.is_truthy());

        let read_key_sequence_vector =
            builtin_fboundp(&mut eval, vec![Value::symbol("read-key-sequence-vector")])
                .expect("fboundp should succeed for read-key-sequence-vector");
        assert!(read_key_sequence_vector.is_truthy());

        let read_event = builtin_fboundp(&mut eval, vec![Value::symbol("read-event")])
            .expect("fboundp should succeed for read-event");
        assert!(read_event.is_truthy());

        let read_char_exclusive =
            builtin_fboundp(&mut eval, vec![Value::symbol("read-char-exclusive")])
                .expect("fboundp should succeed for read-char-exclusive");
        assert!(read_char_exclusive.is_truthy());

        let when_macro = builtin_fboundp(&mut eval, vec![Value::symbol("when")])
            .expect("fboundp should succeed for when");
        assert!(when_macro.is_truthy());
        let save_match_data = builtin_fboundp(&mut eval, vec![Value::symbol("save-match-data")])
            .expect("fboundp should succeed for save-match-data");
        assert!(save_match_data.is_truthy());
        let save_mark_and_excursion =
            builtin_fboundp(&mut eval, vec![Value::symbol("save-mark-and-excursion")])
                .expect("fboundp should succeed for save-mark-and-excursion");
        assert!(save_mark_and_excursion.is_truthy());
        let save_window_excursion =
            builtin_fboundp(&mut eval, vec![Value::symbol("save-window-excursion")])
                .expect("fboundp should succeed for save-window-excursion");
        assert!(save_window_excursion.is_truthy());
        let save_selected_window =
            builtin_fboundp(&mut eval, vec![Value::symbol("save-selected-window")])
                .expect("fboundp should succeed for save-selected-window");
        assert!(save_selected_window.is_truthy());
        let with_local_quit = builtin_fboundp(&mut eval, vec![Value::symbol("with-local-quit")])
            .expect("fboundp should succeed for with-local-quit");
        assert!(with_local_quit.is_truthy());
        let with_temp_message =
            builtin_fboundp(&mut eval, vec![Value::symbol("with-temp-message")])
                .expect("fboundp should succeed for with-temp-message");
        assert!(with_temp_message.is_truthy());

        let with_temp_buffer = builtin_fboundp(&mut eval, vec![Value::symbol("with-temp-buffer")])
            .expect("fboundp should succeed for with-temp-buffer");
        assert!(with_temp_buffer.is_truthy());

        let declare = builtin_fboundp(&mut eval, vec![Value::symbol("declare")])
            .expect("fboundp should succeed for declare");
        assert!(declare.is_truthy());

        let inline = builtin_fboundp(&mut eval, vec![Value::symbol("inline")])
            .expect("fboundp should succeed for inline");
        assert!(inline.is_truthy());

        let throw_fn = builtin_fboundp(&mut eval, vec![Value::symbol("throw")])
            .expect("fboundp should succeed for throw");
        assert!(throw_fn.is_truthy());
        for name in ["funcall", "defalias", "provide", "require"] {
            let result = builtin_fboundp(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("fboundp should succeed for {name}"));
            assert!(result.is_truthy(), "expected {name} to be fboundp");
        }
    }

    #[test]
    fn functionp_eval_matches_symbol_and_lambda_form_semantics() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let builtin_symbol = builtin_functionp_eval(&mut eval, vec![Value::symbol("message")])
            .expect("functionp should accept builtin symbol");
        assert!(builtin_symbol.is_truthy());

        let quoted_lambda = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x")]),
            Value::symbol("x"),
        ]);
        let lambda_result = builtin_functionp_eval(&mut eval, vec![quoted_lambda])
            .expect("functionp should accept quoted lambda list");
        assert!(lambda_result.is_truthy());

        let improper_lambda = Value::cons(Value::symbol("lambda"), Value::Int(1));
        let improper_result = builtin_functionp_eval(&mut eval, vec![improper_lambda])
            .expect("functionp should accept improper lambda forms");
        assert!(improper_result.is_truthy());

        let quoted_closure = Value::list(vec![
            Value::symbol("closure"),
            Value::list(vec![Value::True]),
            Value::list(vec![Value::symbol("x")]),
            Value::symbol("x"),
        ]);
        let closure_result = builtin_functionp_eval(&mut eval, vec![quoted_closure])
            .expect("functionp should reject quoted closure lists");
        assert!(closure_result.is_nil());

        let special_symbol = builtin_functionp_eval(&mut eval, vec![Value::symbol("if")])
            .expect("functionp should reject special-form symbols");
        assert!(special_symbol.is_nil());

        let macro_symbol = builtin_functionp_eval(&mut eval, vec![Value::symbol("when")])
            .expect("functionp should reject macro symbols");
        assert!(macro_symbol.is_nil());
        let save_match_data_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("save-match-data")])
                .expect("functionp should reject save-match-data macro symbol");
        assert!(save_match_data_symbol.is_nil());
        let save_mark_and_excursion_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("save-mark-and-excursion")])
                .expect("functionp should reject save-mark-and-excursion macro symbol");
        assert!(save_mark_and_excursion_symbol.is_nil());
        let save_window_excursion_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("save-window-excursion")])
                .expect("functionp should reject save-window-excursion macro symbol");
        assert!(save_window_excursion_symbol.is_nil());
        let save_selected_window_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("save-selected-window")])
                .expect("functionp should reject save-selected-window macro symbol");
        assert!(save_selected_window_symbol.is_nil());
        let with_local_quit_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("with-local-quit")])
                .expect("functionp should reject with-local-quit macro symbol");
        assert!(with_local_quit_symbol.is_nil());
        let with_temp_message_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("with-temp-message")])
                .expect("functionp should reject with-temp-message macro symbol");
        assert!(with_temp_message_symbol.is_nil());
        let declare_symbol = builtin_functionp_eval(&mut eval, vec![Value::symbol("declare")])
            .expect("functionp should reject declare symbol");
        assert!(declare_symbol.is_nil());
        let inline_symbol = builtin_functionp_eval(&mut eval, vec![Value::symbol("inline")])
            .expect("functionp should reject inline symbol");
        assert!(inline_symbol.is_nil());
        let throw_symbol = builtin_functionp_eval(&mut eval, vec![Value::symbol("throw")])
            .expect("functionp should accept throw symbol");
        assert!(throw_symbol.is_truthy());
        for name in ["funcall", "defalias", "provide", "require"] {
            let result = builtin_functionp_eval(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("functionp should accept {name} symbol"));
            assert!(result.is_truthy(), "expected {name} to satisfy functionp");
        }
        let macro_marker_cons = builtin_functionp_eval(
            &mut eval,
            vec![Value::cons(Value::symbol("macro"), Value::True)],
        )
        .expect("functionp should reject dotted macro marker cons");
        assert!(macro_marker_cons.is_nil());
        let macro_marker_list = builtin_functionp_eval(
            &mut eval,
            vec![Value::list(vec![Value::symbol("macro"), Value::True])],
        )
        .expect("functionp should reject macro marker lists");
        assert!(macro_marker_list.is_nil());

        let special_subr = builtin_functionp_eval(&mut eval, vec![Value::Subr("if".to_string())])
            .expect("functionp should reject special-form subr objects");
        assert!(special_subr.is_nil());

        let autoload_function_forms = crate::elisp::parser::parse_forms(
            r#"(autoload 'vm-test-auto-fn "vm-test-file" nil t)"#,
        )
        .expect("autoload function form should parse");
        for form in &autoload_function_forms {
            eval.eval(form).expect("autoload function should register");
        }
        let autoload_function_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("vm-test-auto-fn")])
                .expect("functionp should recognize autoload function symbol");
        assert!(autoload_function_symbol.is_truthy());
        let autoload_function_cell = eval
            .obarray()
            .symbol_function("vm-test-auto-fn")
            .expect("autoload function cell exists")
            .clone();
        let autoload_function_cell =
            builtin_functionp_eval(&mut eval, vec![autoload_function_cell])
                .expect("functionp should reject raw autoload function cell object");
        assert!(autoload_function_cell.is_nil());

        let autoload_macro_forms = crate::elisp::parser::parse_forms(
            r#"(autoload 'vm-test-auto-macro "vm-test-file" nil nil 'macro)"#,
        )
        .expect("autoload macro form should parse");
        for form in &autoload_macro_forms {
            eval.eval(form).expect("autoload macro should register");
        }
        let autoload_macro_symbol =
            builtin_functionp_eval(&mut eval, vec![Value::symbol("vm-test-auto-macro")])
                .expect("functionp should reject autoload macro symbol");
        assert!(autoload_macro_symbol.is_nil());
    }

    #[test]
    fn functionp_eval_resolves_t_and_keyword_symbol_designators() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let keyword = Value::keyword(":vm-functionp-keyword");
        let orig_t = builtin_symbol_function(&mut eval, vec![Value::True])
            .expect("symbol-function should read t cell");
        let orig_keyword = builtin_symbol_function(&mut eval, vec![keyword.clone()])
            .expect("symbol-function should read keyword cell");

        builtin_fset(&mut eval, vec![Value::True, Value::symbol("car")])
            .expect("fset should bind t function cell");
        builtin_fset(&mut eval, vec![keyword.clone(), Value::symbol("car")])
            .expect("fset should bind keyword function cell");

        let t_result = builtin_functionp_eval(&mut eval, vec![Value::True])
            .expect("functionp should accept t");
        assert!(t_result.is_truthy());
        let keyword_result = builtin_functionp_eval(&mut eval, vec![keyword.clone()])
            .expect("functionp should accept keyword designator");
        assert!(keyword_result.is_truthy());

        builtin_fset(&mut eval, vec![Value::True, orig_t]).expect("restore t function cell");
        builtin_fset(&mut eval, vec![keyword, orig_keyword])
            .expect("restore keyword function cell");
    }

    #[test]
    fn symbol_function_resolves_builtin_and_special_names() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let message = builtin_symbol_function(&mut eval, vec![Value::symbol("message")])
            .expect("symbol-function should resolve message");
        assert_eq!(message, Value::Subr("message".to_string()));

        let load = builtin_symbol_function(&mut eval, vec![Value::symbol("load")])
            .expect("symbol-function should resolve load");
        assert_eq!(load, Value::Subr("load".to_string()));

        let if_sf = builtin_symbol_function(&mut eval, vec![Value::symbol("if")])
            .expect("symbol-function should resolve if");
        assert_eq!(if_sf, Value::Subr("if".to_string()));

        let typed = builtin_symbol_function(&mut eval, vec![Value::symbol("car")])
            .expect("symbol-function should resolve car");
        assert_eq!(typed, Value::Subr("car".to_string()));

        let read_key_sequence_vector =
            builtin_symbol_function(&mut eval, vec![Value::symbol("read-key-sequence-vector")])
                .expect("symbol-function should resolve read-key-sequence-vector");
        assert_eq!(
            read_key_sequence_vector,
            Value::Subr("read-key-sequence-vector".to_string())
        );

        let x_display_color =
            builtin_symbol_function(&mut eval, vec![Value::symbol("x-display-color-p")])
                .expect("symbol-function should resolve x-display-color-p alias");
        assert_eq!(x_display_color, Value::symbol("display-color-p"));

        let window_height =
            builtin_symbol_function(&mut eval, vec![Value::symbol("window-height")])
                .expect("symbol-function should resolve window-height alias");
        assert_eq!(window_height, Value::symbol("window-total-height"));

        let window_width = builtin_symbol_function(&mut eval, vec![Value::symbol("window-width")])
            .expect("symbol-function should resolve window-width alias");
        assert_eq!(window_width, Value::symbol("window-body-width"));

        let inside_pixel_edges =
            builtin_symbol_function(&mut eval, vec![Value::symbol("window-inside-pixel-edges")])
                .expect("symbol-function should resolve window-inside-pixel-edges alias");
        assert_eq!(inside_pixel_edges, Value::symbol("window-body-pixel-edges"));

        let inside_edges =
            builtin_symbol_function(&mut eval, vec![Value::symbol("window-inside-edges")])
                .expect("symbol-function should resolve window-inside-edges alias");
        assert_eq!(inside_edges, Value::symbol("window-body-edges"));

        let count_matches =
            builtin_symbol_function(&mut eval, vec![Value::symbol("count-matches")])
                .expect("symbol-function should resolve count-matches alias");
        assert_eq!(count_matches, Value::symbol("how-many"));

        let replace_rectangle =
            builtin_symbol_function(&mut eval, vec![Value::symbol("replace-rectangle")])
                .expect("symbol-function should resolve replace-rectangle alias");
        assert_eq!(replace_rectangle, Value::symbol("string-rectangle"));

        let wholenump = builtin_symbol_function(&mut eval, vec![Value::symbol("wholenump")])
            .expect("symbol-function should resolve wholenump alias");
        assert_eq!(wholenump, Value::symbol("natnump"));

        let subr_native =
            builtin_symbol_function(&mut eval, vec![Value::symbol("subr-native-elisp-p")])
                .expect("symbol-function should resolve subr-native-elisp-p alias");
        assert_eq!(subr_native, Value::symbol("native-comp-function-p"));

        let name_last =
            builtin_symbol_function(&mut eval, vec![Value::symbol("name-last-kbd-macro")])
                .expect("symbol-function should resolve name-last-kbd-macro alias");
        assert_eq!(name_last, Value::symbol("kmacro-name-last-macro"));

        let subr_primitive =
            builtin_symbol_function(&mut eval, vec![Value::symbol("subr-primitive-p")])
                .expect("symbol-function should resolve subr-primitive-p wrapper");
        assert!(matches!(
            subr_primitive,
            Value::Lambda(_) | Value::ByteCode(_)
        ));

        let bookmark_delete =
            builtin_symbol_function(&mut eval, vec![Value::symbol("bookmark-delete")])
                .expect("symbol-function should resolve bookmark-delete autoload");
        assert!(crate::elisp::autoload::is_autoload_value(&bookmark_delete));
        for name in [
            "format-seconds",
            "format-spec",
            "string-clean-whitespace",
            "string-glyph-split",
            "upcase-char",
            "bounds-of-thing-at-point",
            "thing-at-point",
            "symbol-at-point",
            "safe-date-to-time",
            "read-passwd",
            "clear-rectangle",
            "delete-extract-rectangle",
            "delete-rectangle",
            "describe-function",
            "describe-variable",
            "extract-rectangle",
            "insert-kbd-macro",
            "insert-rectangle",
            "kbd-macro-query",
            "kill-rectangle",
            "open-rectangle",
            "string-pixel-width",
            "string-rectangle",
            "yank-rectangle",
        ] {
            let value = builtin_symbol_function(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("symbol-function should resolve {name} autoload"));
            assert!(crate::elisp::autoload::is_autoload_value(&value));
        }
        for name in [
            "autoloadp",
            "seq-count",
            "seq-concatenate",
            "seq-contains-p",
            "seq-drop",
            "seq-do",
            "seq-empty-p",
            "seq-every-p",
            "seq-into",
            "seq-length",
            "seq-mapn",
            "seq-max",
            "seq-min",
            "seq-position",
            "seq-reduce",
            "seq-reverse",
            "seq-some",
            "seq-sort",
            "seq-subseq",
            "seq-take",
            "seq-uniq",
            "looking-at-p",
            "string-match-p",
            "string-blank-p",
            "string-empty-p",
            "string-equal-ignore-case",
            "string-to-vector",
        ] {
            let value = builtin_symbol_function(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("symbol-function should resolve {name} wrapper"));
            assert!(matches!(value, Value::Lambda(_) | Value::ByteCode(_)));
        }

        let throw_fn = builtin_symbol_function(&mut eval, vec![Value::symbol("throw")])
            .expect("symbol-function should resolve throw as callable subr");
        assert_eq!(throw_fn, Value::Subr("throw".to_string()));
        for name in ["funcall", "defalias", "provide", "require"] {
            let result = builtin_symbol_function(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("symbol-function should resolve {name}"));
            assert_eq!(result, Value::Subr(name.to_string()));
        }

        let when_macro = builtin_symbol_function(&mut eval, vec![Value::symbol("when")])
            .expect("symbol-function should resolve when as a macro");
        assert!(matches!(when_macro, Value::Macro(_)));
        let save_match_data_macro =
            builtin_symbol_function(&mut eval, vec![Value::symbol("save-match-data")])
                .expect("symbol-function should resolve save-match-data as a macro");
        assert!(matches!(save_match_data_macro, Value::Macro(_)));
        let save_mark_and_excursion_macro =
            builtin_symbol_function(&mut eval, vec![Value::symbol("save-mark-and-excursion")])
                .expect("symbol-function should resolve save-mark-and-excursion as a macro");
        assert!(matches!(save_mark_and_excursion_macro, Value::Macro(_)));
        let save_window_excursion_macro =
            builtin_symbol_function(&mut eval, vec![Value::symbol("save-window-excursion")])
                .expect("symbol-function should resolve save-window-excursion as a macro");
        assert!(matches!(save_window_excursion_macro, Value::Macro(_)));
        let save_selected_window_macro =
            builtin_symbol_function(&mut eval, vec![Value::symbol("save-selected-window")])
                .expect("symbol-function should resolve save-selected-window as a macro");
        assert!(matches!(save_selected_window_macro, Value::Macro(_)));
        let with_local_quit_macro =
            builtin_symbol_function(&mut eval, vec![Value::symbol("with-local-quit")])
                .expect("symbol-function should resolve with-local-quit as a macro");
        assert!(matches!(with_local_quit_macro, Value::Macro(_)));
        let with_temp_message_macro =
            builtin_symbol_function(&mut eval, vec![Value::symbol("with-temp-message")])
                .expect("symbol-function should resolve with-temp-message as a macro");
        assert!(matches!(with_temp_message_macro, Value::Macro(_)));

        let declare_macro = builtin_symbol_function(&mut eval, vec![Value::symbol("declare")])
            .expect("symbol-function should resolve declare as a macro");
        assert!(matches!(declare_macro, Value::Macro(_)));

        let inline_symbol = builtin_symbol_function(&mut eval, vec![Value::symbol("inline")])
            .expect("symbol-function should resolve inline as a symbol marker");
        assert_eq!(inline_symbol, Value::symbol("inline"));

        let unresolved =
            builtin_symbol_function(&mut eval, vec![Value::symbol("definitely-not-a-function")])
                .expect("symbol-function should return nil for unresolved symbols");
        assert!(unresolved.is_nil());

        let nil_symbol = builtin_symbol_function(&mut eval, vec![Value::symbol("nil")])
            .expect("symbol-function should return nil for symbol nil");
        assert!(nil_symbol.is_nil());

        let t_symbol = builtin_symbol_function(&mut eval, vec![Value::symbol("t")])
            .expect("symbol-function should return nil for symbol t");
        assert!(t_symbol.is_nil());
    }

    #[test]
    fn fmakunbound_masks_builtin_special_and_evaluator_callables() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        builtin_fmakunbound(&mut eval, vec![Value::symbol("car")])
            .expect("fmakunbound should accept builtin name");
        let car_bound = builtin_fboundp(&mut eval, vec![Value::symbol("car")])
            .expect("fboundp should accept builtin name");
        assert!(car_bound.is_nil());
        let car_fn = builtin_symbol_function(&mut eval, vec![Value::symbol("car")])
            .expect("symbol-function should return nil after fmakunbound");
        assert!(car_fn.is_nil());
        let car_functionp = builtin_functionp_eval(&mut eval, vec![Value::symbol("car")])
            .expect("functionp should accept symbol");
        assert!(car_functionp.is_nil());

        builtin_fmakunbound(&mut eval, vec![Value::symbol("if")])
            .expect("fmakunbound should accept special form name");
        let if_bound = builtin_fboundp(&mut eval, vec![Value::symbol("if")])
            .expect("fboundp should accept special form name");
        assert!(if_bound.is_nil());
        let if_fn = builtin_symbol_function(&mut eval, vec![Value::symbol("if")])
            .expect("symbol-function should return nil after fmakunbound special form");
        assert!(if_fn.is_nil());

        builtin_fmakunbound(&mut eval, vec![Value::symbol("throw")])
            .expect("fmakunbound should accept evaluator callable name");
        let throw_bound = builtin_fboundp(&mut eval, vec![Value::symbol("throw")])
            .expect("fboundp should accept evaluator callable name");
        assert!(throw_bound.is_nil());
        let throw_fn = builtin_symbol_function(&mut eval, vec![Value::symbol("throw")])
            .expect("symbol-function should return nil after fmakunbound evaluator callable");
        assert!(throw_fn.is_nil());
        let throw_functionp = builtin_functionp_eval(&mut eval, vec![Value::symbol("throw")])
            .expect("functionp should accept symbol");
        assert!(throw_functionp.is_nil());
        for name in ["funcall", "defalias", "provide", "require"] {
            builtin_fmakunbound(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("fmakunbound should accept {name}"));
            let bound = builtin_fboundp(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("fboundp should accept {name}"));
            assert!(
                bound.is_nil(),
                "expected {name} to be unbound after fmakunbound"
            );
            let fn_cell = builtin_symbol_function(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("symbol-function should accept {name}"));
            assert!(
                fn_cell.is_nil(),
                "expected symbol-function {name} to be nil"
            );
            let functionp = builtin_functionp_eval(&mut eval, vec![Value::symbol(name)])
                .unwrap_or_else(|_| panic!("functionp should accept {name}"));
            assert!(
                functionp.is_nil(),
                "expected functionp {name} to be nil after fmakunbound"
            );
        }
    }

    #[test]
    fn fset_nil_clears_fboundp_for_regular_and_fallback_names() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let regular = builtin_fset(&mut eval, vec![Value::symbol("vm-fsetnil"), Value::Nil])
            .expect("fset should accept nil definition payload");
        assert!(regular.is_nil());
        let regular_bound = builtin_fboundp(&mut eval, vec![Value::symbol("vm-fsetnil")])
            .expect("fboundp should accept symbol");
        assert!(regular_bound.is_nil());
        let regular_fn = builtin_symbol_function(&mut eval, vec![Value::symbol("vm-fsetnil")])
            .expect("symbol-function should accept symbol");
        assert!(regular_fn.is_nil());

        let fallback = builtin_fset(&mut eval, vec![Value::symbol("length"), Value::Nil])
            .expect("fset should accept nil for fallback builtin name");
        assert!(fallback.is_nil());
        let fallback_bound = builtin_fboundp(&mut eval, vec![Value::symbol("length")])
            .expect("fboundp should honor explicit nil function cell");
        assert!(fallback_bound.is_nil());
    }

    #[test]
    fn func_arity_eval_resolves_symbol_inputs_and_void_edges() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let car_arity = builtin_func_arity_eval(&mut eval, vec![Value::symbol("car")])
            .expect("func-arity should resolve builtin symbols");
        match &car_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(1));
                assert_eq!(pair.cdr, Value::Int(1));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }

        let when_arity = builtin_func_arity_eval(&mut eval, vec![Value::symbol("when")])
            .expect("func-arity should resolve fallback macro symbols");
        match &when_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(1));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }
        let save_match_data_arity =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("save-match-data")])
                .expect("func-arity should resolve save-match-data macro symbol");
        match &save_match_data_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(0));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }
        let save_mark_and_excursion_arity =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("save-mark-and-excursion")])
                .expect("func-arity should resolve save-mark-and-excursion macro symbol");
        match &save_mark_and_excursion_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(0));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }
        let save_window_excursion_arity =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("save-window-excursion")])
                .expect("func-arity should resolve save-window-excursion macro symbol");
        match &save_window_excursion_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(0));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }
        let save_selected_window_arity =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("save-selected-window")])
                .expect("func-arity should resolve save-selected-window macro symbol");
        match &save_selected_window_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(0));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }
        let with_local_quit_arity =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("with-local-quit")])
                .expect("func-arity should resolve with-local-quit macro symbol");
        match &with_local_quit_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(0));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }
        let with_temp_message_arity =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("with-temp-message")])
                .expect("func-arity should resolve with-temp-message macro symbol");
        match &with_temp_message_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(1));
                assert_eq!(pair.cdr, Value::symbol("many"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }

        let inline_arity = builtin_func_arity_eval(&mut eval, vec![Value::symbol("inline")])
            .expect("func-arity should resolve inline special-form symbol");
        match &inline_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(0));
                assert_eq!(pair.cdr, Value::symbol("unevalled"));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }

        let missing_err =
            builtin_func_arity_eval(&mut eval, vec![Value::symbol("definitely-not-a-function")])
                .expect_err("func-arity should signal void-function for unresolved symbols");
        match missing_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "void-function");
                assert_eq!(sig.data, vec![Value::symbol("definitely-not-a-function")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let nil_err = builtin_func_arity_eval(&mut eval, vec![Value::Nil])
            .expect_err("func-arity should signal void-function for nil");
        match nil_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "void-function");
                assert_eq!(sig.data, vec![Value::symbol("nil")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn func_arity_eval_resolves_symbol_designators_and_nil_cells() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let keyword = Value::keyword(":vm-func-arity-keyword");
        let vm_nil = Value::symbol("vm-func-arity-nil-cell");
        let orig_t = builtin_symbol_function(&mut eval, vec![Value::True])
            .expect("symbol-function should read t function cell");
        let orig_keyword = builtin_symbol_function(&mut eval, vec![keyword.clone()])
            .expect("symbol-function should read keyword function cell");
        let orig_vm_nil = builtin_symbol_function(&mut eval, vec![vm_nil.clone()])
            .expect("symbol-function should read symbol function cell");

        builtin_fset(&mut eval, vec![Value::True, Value::symbol("car")])
            .expect("fset should bind t function cell");
        builtin_fset(&mut eval, vec![keyword.clone(), Value::symbol("car")])
            .expect("fset should bind keyword function cell");
        builtin_fset(&mut eval, vec![vm_nil.clone(), Value::Nil])
            .expect("fset should bind explicit nil function cell");

        let t_arity = builtin_func_arity_eval(&mut eval, vec![Value::True])
            .expect("func-arity should resolve t designator");
        match &t_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(1));
                assert_eq!(pair.cdr, Value::Int(1));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }

        let keyword_arity = builtin_func_arity_eval(&mut eval, vec![keyword.clone()])
            .expect("func-arity should resolve keyword designator");
        match &keyword_arity {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car, Value::Int(1));
                assert_eq!(pair.cdr, Value::Int(1));
            }
            other => panic!("expected cons arity pair, got {other:?}"),
        }

        let nil_cell_err = builtin_func_arity_eval(&mut eval, vec![vm_nil.clone()])
            .expect_err("func-arity should signal void-function for nil function cell");
        match nil_cell_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "void-function");
                assert_eq!(sig.data, vec![vm_nil.clone()]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        builtin_fset(&mut eval, vec![Value::True, orig_t]).expect("restore t function cell");
        builtin_fset(&mut eval, vec![keyword, orig_keyword])
            .expect("restore keyword function cell");
        builtin_fset(&mut eval, vec![vm_nil, orig_vm_nil]).expect("restore symbol function cell");
    }

    #[test]
    fn indirect_function_resolves_builtin_and_special_names() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let message = builtin_indirect_function(&mut eval, vec![Value::symbol("message")])
            .expect("indirect-function should resolve message");
        assert_eq!(message, Value::Subr("message".to_string()));

        let load = builtin_indirect_function(&mut eval, vec![Value::symbol("load")])
            .expect("indirect-function should resolve load");
        assert_eq!(load, Value::Subr("load".to_string()));

        let if_sf = builtin_indirect_function(&mut eval, vec![Value::symbol("if")])
            .expect("indirect-function should resolve if");
        assert_eq!(if_sf, Value::Subr("if".to_string()));

        let typed = builtin_indirect_function(&mut eval, vec![Value::symbol("car")])
            .expect("indirect-function should resolve car");
        assert_eq!(typed, Value::Subr("car".to_string()));

        let read_key_sequence_vector =
            builtin_indirect_function(&mut eval, vec![Value::symbol("read-key-sequence-vector")])
                .expect("indirect-function should resolve read-key-sequence-vector");
        assert_eq!(
            read_key_sequence_vector,
            Value::Subr("read-key-sequence-vector".to_string())
        );

        let when_macro = builtin_indirect_function(&mut eval, vec![Value::symbol("when")])
            .expect("indirect-function should resolve when as a macro");
        assert!(matches!(when_macro, Value::Macro(_)));
        let save_match_data_macro =
            builtin_indirect_function(&mut eval, vec![Value::symbol("save-match-data")])
                .expect("indirect-function should resolve save-match-data as a macro");
        assert!(matches!(save_match_data_macro, Value::Macro(_)));
        let save_mark_and_excursion_macro =
            builtin_indirect_function(&mut eval, vec![Value::symbol("save-mark-and-excursion")])
                .expect("indirect-function should resolve save-mark-and-excursion as a macro");
        assert!(matches!(save_mark_and_excursion_macro, Value::Macro(_)));
        let save_window_excursion_macro =
            builtin_indirect_function(&mut eval, vec![Value::symbol("save-window-excursion")])
                .expect("indirect-function should resolve save-window-excursion as a macro");
        assert!(matches!(save_window_excursion_macro, Value::Macro(_)));
        let save_selected_window_macro =
            builtin_indirect_function(&mut eval, vec![Value::symbol("save-selected-window")])
                .expect("indirect-function should resolve save-selected-window as a macro");
        assert!(matches!(save_selected_window_macro, Value::Macro(_)));
        let with_local_quit_macro =
            builtin_indirect_function(&mut eval, vec![Value::symbol("with-local-quit")])
                .expect("indirect-function should resolve with-local-quit as a macro");
        assert!(matches!(with_local_quit_macro, Value::Macro(_)));
        let with_temp_message_macro =
            builtin_indirect_function(&mut eval, vec![Value::symbol("with-temp-message")])
                .expect("indirect-function should resolve with-temp-message as a macro");
        assert!(matches!(with_temp_message_macro, Value::Macro(_)));

        eval.obarray_mut()
            .set_symbol_function("alias-car", Value::symbol("car"));
        let alias_car = builtin_indirect_function(&mut eval, vec![Value::symbol("alias-car")])
            .expect("indirect-function should resolve alias chains ending in builtins");
        assert_eq!(alias_car, Value::Subr("car".to_string()));
    }

    #[test]
    fn indirect_function_follows_t_and_keyword_alias_values() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let keyword = Value::keyword(":vm-indirect-keyword-alias");
        let t_alias = Value::symbol("vm-indirect-through-t");
        let keyword_alias = Value::symbol("vm-indirect-through-keyword");
        let orig_t = builtin_symbol_function(&mut eval, vec![Value::True])
            .expect("symbol-function should read t function cell");
        let orig_keyword = builtin_symbol_function(&mut eval, vec![keyword.clone()])
            .expect("symbol-function should read keyword function cell");
        let orig_t_alias = builtin_symbol_function(&mut eval, vec![t_alias.clone()])
            .expect("symbol-function should read alias function cell");
        let orig_keyword_alias = builtin_symbol_function(&mut eval, vec![keyword_alias.clone()])
            .expect("symbol-function should read alias function cell");

        builtin_fset(&mut eval, vec![Value::True, Value::symbol("car")])
            .expect("fset should bind t function cell");
        builtin_fset(&mut eval, vec![keyword.clone(), Value::symbol("car")])
            .expect("fset should bind keyword function cell");
        builtin_fset(&mut eval, vec![t_alias.clone(), Value::True])
            .expect("fset should bind alias to t symbol designator");
        builtin_fset(&mut eval, vec![keyword_alias.clone(), keyword.clone()])
            .expect("fset should bind alias to keyword designator");

        let resolved_t_alias = builtin_indirect_function(&mut eval, vec![t_alias.clone()])
            .expect("indirect-function should resolve alias through t");
        assert_eq!(resolved_t_alias, Value::Subr("car".to_string()));
        let resolved_keyword_alias =
            builtin_indirect_function(&mut eval, vec![keyword_alias.clone()])
                .expect("indirect-function should resolve alias through keyword");
        assert_eq!(resolved_keyword_alias, Value::Subr("car".to_string()));

        builtin_fset(&mut eval, vec![Value::True, orig_t]).expect("restore t function cell");
        builtin_fset(&mut eval, vec![keyword, orig_keyword])
            .expect("restore keyword function cell");
        builtin_fset(&mut eval, vec![t_alias, orig_t_alias]).expect("restore alias function cell");
        builtin_fset(&mut eval, vec![keyword_alias, orig_keyword_alias])
            .expect("restore alias function cell");
    }

    #[test]
    fn macrop_eval_recognizes_symbol_function_and_marker_forms() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let when_symbol = builtin_macrop_eval(&mut eval, vec![Value::symbol("when")])
            .expect("macrop should handle symbol input");
        assert!(when_symbol.is_truthy());
        let save_match_data_symbol =
            builtin_macrop_eval(&mut eval, vec![Value::symbol("save-match-data")])
                .expect("macrop should handle save-match-data symbol input");
        assert!(save_match_data_symbol.is_truthy());
        let save_mark_and_excursion_symbol =
            builtin_macrop_eval(&mut eval, vec![Value::symbol("save-mark-and-excursion")])
                .expect("macrop should handle save-mark-and-excursion symbol input");
        assert!(save_mark_and_excursion_symbol.is_truthy());
        let save_window_excursion_symbol =
            builtin_macrop_eval(&mut eval, vec![Value::symbol("save-window-excursion")])
                .expect("macrop should handle save-window-excursion symbol input");
        assert!(save_window_excursion_symbol.is_truthy());
        let save_selected_window_symbol =
            builtin_macrop_eval(&mut eval, vec![Value::symbol("save-selected-window")])
                .expect("macrop should handle save-selected-window symbol input");
        assert!(save_selected_window_symbol.is_truthy());
        let with_local_quit_symbol =
            builtin_macrop_eval(&mut eval, vec![Value::symbol("with-local-quit")])
                .expect("macrop should handle with-local-quit symbol input");
        assert!(with_local_quit_symbol.is_truthy());
        let with_temp_message_symbol =
            builtin_macrop_eval(&mut eval, vec![Value::symbol("with-temp-message")])
                .expect("macrop should handle with-temp-message symbol input");
        assert!(with_temp_message_symbol.is_truthy());

        let plain_symbol = builtin_macrop_eval(&mut eval, vec![Value::symbol("if")])
            .expect("macrop should handle non-macro symbols");
        assert!(plain_symbol.is_nil());

        let marker = Value::cons(Value::symbol("macro"), Value::Int(1));
        let marker_result =
            builtin_macrop_eval(&mut eval, vec![marker]).expect("macrop should accept markers");
        assert!(marker_result.is_truthy());
    }

    #[test]
    fn macrop_eval_resolves_keyword_designators() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let keyword = Value::keyword(":vm-macrop-keyword");
        let orig_keyword = builtin_symbol_function(&mut eval, vec![keyword.clone()])
            .expect("symbol-function should read keyword function cell");
        let when_macro = builtin_symbol_function(&mut eval, vec![Value::symbol("when")])
            .expect("symbol-function should read when macro");

        builtin_fset(&mut eval, vec![keyword.clone(), when_macro])
            .expect("fset should bind keyword function cell");
        let keyword_result = builtin_macrop_eval(&mut eval, vec![keyword.clone()])
            .expect("macrop should resolve keyword designator");
        assert!(keyword_result.is_truthy());

        builtin_fset(&mut eval, vec![keyword, orig_keyword])
            .expect("restore keyword function cell");
    }

    #[test]
    fn macroexpand_runtime_expands_global_and_nested_macros() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let expanded = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("when"),
                Value::True,
                Value::Int(1),
            ])],
        )
        .expect("macroexpand should expand top-level when");
        assert_eq!(
            expanded,
            Value::list(vec![
                Value::symbol("if"),
                Value::True,
                Value::list(vec![Value::symbol("progn"), Value::Int(1)]),
            ])
        );

        let nested = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("when"),
                Value::True,
                Value::list(vec![Value::symbol("when"), Value::True, Value::Int(1)]),
            ])],
        )
        .expect("macroexpand should not recursively walk nested bodies");
        assert_eq!(
            nested,
            Value::list(vec![
                Value::symbol("if"),
                Value::True,
                Value::list(vec![
                    Value::symbol("progn"),
                    Value::list(vec![Value::symbol("when"), Value::True, Value::Int(1)]),
                ]),
            ])
        );

        let save_match_data = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("save-match-data"),
                Value::list(vec![
                    Value::symbol("string-match"),
                    Value::string("a"),
                    Value::string("a"),
                ]),
            ])],
        )
        .expect("macroexpand should expand save-match-data");
        assert_eq!(
            save_match_data,
            Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![Value::list(vec![
                    Value::symbol("saved-match-data"),
                    Value::list(vec![Value::symbol("match-data")]),
                ])]),
                Value::list(vec![
                    Value::symbol("unwind-protect"),
                    Value::list(vec![
                        Value::symbol("progn"),
                        Value::list(vec![
                            Value::symbol("string-match"),
                            Value::string("a"),
                            Value::string("a"),
                        ]),
                    ]),
                    Value::list(vec![
                        Value::symbol("set-match-data"),
                        Value::symbol("saved-match-data"),
                        Value::True,
                    ]),
                ]),
            ])
        );

        let save_mark_and_excursion = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("save-mark-and-excursion"),
                Value::list(vec![Value::symbol("setq"), Value::symbol("x"), Value::Int(1)]),
            ])],
        )
        .expect("macroexpand should expand save-mark-and-excursion");
        assert_eq!(
            save_mark_and_excursion,
            Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![Value::list(vec![
                    Value::symbol("saved-marker"),
                    Value::list(vec![Value::symbol("save-mark-and-excursion--save")]),
                ])]),
                Value::list(vec![
                    Value::symbol("unwind-protect"),
                    Value::list(vec![
                        Value::symbol("save-excursion"),
                        Value::list(vec![
                            Value::symbol("setq"),
                            Value::symbol("x"),
                            Value::Int(1),
                        ]),
                    ]),
                    Value::list(vec![
                        Value::symbol("save-mark-and-excursion--restore"),
                        Value::symbol("saved-marker"),
                    ]),
                ]),
            ])
        );

        let save_window_excursion = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("save-window-excursion"),
                Value::list(vec![Value::symbol("selected-window")]),
            ])],
        )
        .expect("macroexpand should expand save-window-excursion");
        assert_eq!(
            save_window_excursion,
            Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![Value::list(vec![
                    Value::symbol("wconfig"),
                    Value::list(vec![Value::symbol("current-window-configuration")]),
                ])]),
                Value::list(vec![
                    Value::symbol("unwind-protect"),
                    Value::list(vec![
                        Value::symbol("progn"),
                        Value::list(vec![Value::symbol("selected-window")]),
                    ]),
                    Value::list(vec![
                        Value::symbol("set-window-configuration"),
                        Value::symbol("wconfig"),
                    ]),
                ]),
            ])
        );

        let save_selected_window = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("save-selected-window"),
                Value::list(vec![Value::symbol("selected-window")]),
            ])],
        )
        .expect("macroexpand should expand save-selected-window");
        assert_eq!(
            save_selected_window,
            Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![Value::list(vec![
                    Value::symbol("save-selected-window--state"),
                    Value::list(vec![Value::symbol("internal--before-save-selected-window")]),
                ])]),
                Value::list(vec![
                    Value::symbol("save-current-buffer"),
                    Value::list(vec![
                        Value::symbol("unwind-protect"),
                        Value::list(vec![
                            Value::symbol("progn"),
                            Value::list(vec![Value::symbol("selected-window")]),
                        ]),
                        Value::list(vec![
                            Value::symbol("internal--after-save-selected-window"),
                            Value::symbol("save-selected-window--state"),
                        ]),
                    ]),
                ]),
            ])
        );

        let with_local_quit = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("with-local-quit"),
                Value::list(vec![Value::symbol("selected-window")]),
            ])],
        )
        .expect("macroexpand should expand with-local-quit");
        assert_eq!(
            with_local_quit,
            Value::list(vec![
                Value::symbol("condition-case"),
                Value::Nil,
                Value::list(vec![
                    Value::symbol("let"),
                    Value::list(vec![Value::list(vec![
                        Value::symbol("inhibit-quit"),
                        Value::Nil,
                    ])]),
                    Value::list(vec![Value::symbol("selected-window")]),
                ]),
                Value::list(vec![
                    Value::symbol("quit"),
                    Value::list(vec![
                        Value::symbol("setq"),
                        Value::symbol("quit-flag"),
                        Value::True,
                    ]),
                    Value::list(vec![
                        Value::symbol("eval"),
                        Value::list(vec![
                            Value::symbol("quote"),
                            Value::list(vec![Value::symbol("ignore"), Value::Nil]),
                        ]),
                        Value::True,
                    ]),
                ]),
            ])
        );

        let with_temp_message = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol("with-temp-message"),
                Value::string("x"),
                Value::list(vec![Value::symbol("selected-window")]),
            ])],
        )
        .expect("macroexpand should expand with-temp-message");
        assert_eq!(
            with_temp_message,
            Value::list(vec![
                Value::symbol("let"),
                Value::list(vec![
                    Value::list(vec![Value::symbol("with-temp-message"), Value::string("x")]),
                    Value::list(vec![Value::symbol("current-message")]),
                ]),
                Value::list(vec![
                    Value::symbol("unwind-protect"),
                    Value::list(vec![
                        Value::symbol("progn"),
                        Value::list(vec![
                            Value::symbol("when"),
                            Value::symbol("with-temp-message"),
                            Value::list(vec![
                                Value::symbol("setq"),
                                Value::symbol("current-message"),
                                Value::list(vec![Value::symbol("current-message")]),
                            ]),
                            Value::list(vec![
                                Value::symbol("message"),
                                Value::string("%s"),
                                Value::symbol("with-temp-message"),
                            ]),
                        ]),
                        Value::list(vec![Value::symbol("selected-window")]),
                    ]),
                    Value::list(vec![
                        Value::symbol("and"),
                        Value::symbol("with-temp-message"),
                        Value::list(vec![
                            Value::symbol("if"),
                            Value::symbol("current-message"),
                            Value::list(vec![
                                Value::symbol("message"),
                                Value::string("%s"),
                                Value::symbol("current-message"),
                            ]),
                            Value::list(vec![Value::symbol("message"), Value::Nil]),
                        ]),
                    ]),
                ]),
            ])
        );

        let passthrough = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::list(vec![Value::symbol("+"), Value::Int(1)])],
        )
        .expect("macroexpand should leave non-macro heads unchanged");
        assert_eq!(
            passthrough,
            Value::list(vec![Value::symbol("+"), Value::Int(1)])
        );
    }

    #[test]
    fn macroexpand_runtime_environment_overrides_and_shadows_global_macros() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let env_lambda = Value::list(vec![Value::list(vec![
            Value::symbol("vm-env"),
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x")]),
            Value::list(vec![
                Value::symbol("list"),
                Value::list(vec![Value::symbol("quote"), Value::symbol("when")]),
                Value::symbol("x"),
                Value::Int(1),
            ]),
        ])]);
        let expanded = builtin_macroexpand_eval(
            &mut eval,
            vec![
                Value::list(vec![Value::symbol("vm-env"), Value::True]),
                env_lambda,
            ],
        )
        .expect("macroexpand should apply lambda environment expanders");
        assert_eq!(
            expanded,
            Value::list(vec![
                Value::symbol("if"),
                Value::True,
                Value::list(vec![Value::symbol("progn"), Value::Int(1)]),
            ])
        );

        let shadow = builtin_macroexpand_eval(
            &mut eval,
            vec![
                Value::list(vec![Value::symbol("when"), Value::True, Value::Int(1)]),
                Value::list(vec![Value::list(vec![Value::symbol("when")])]),
            ],
        )
        .expect("environment shadow entries should suppress global macro expansion");
        assert_eq!(
            shadow,
            Value::list(vec![Value::symbol("when"), Value::True, Value::Int(1)])
        );
    }

    #[test]
    fn macroexpand_runtime_environment_type_and_payload_edges_match_oracle() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let atom_ignores_bad_env =
            builtin_macroexpand_eval(&mut eval, vec![Value::symbol("x"), Value::Int(1)])
                .expect("non-list forms should ignore non-list environments");
        assert_eq!(atom_ignores_bad_env, Value::symbol("x"));

        let nonsymbol_head_ignores_bad_env = builtin_macroexpand_eval(
            &mut eval,
            vec![
                Value::list(vec![
                    Value::list(vec![Value::symbol("lambda")]),
                    Value::Int(1),
                ]),
                Value::Int(1),
            ],
        )
        .expect("list forms without symbol heads should ignore non-list env");
        assert_eq!(
            nonsymbol_head_ignores_bad_env,
            Value::list(vec![
                Value::list(vec![Value::symbol("lambda")]),
                Value::Int(1)
            ])
        );

        let env_type_err = builtin_macroexpand_eval(
            &mut eval,
            vec![
                Value::list(vec![Value::symbol("foo"), Value::Int(1)]),
                Value::Int(1),
            ],
        )
        .expect_err("symbol-headed forms should validate environment list-ness");
        match env_type_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("listp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let invalid_env_function = builtin_macroexpand_eval(
            &mut eval,
            vec![
                Value::list(vec![Value::symbol("vm-f"), Value::Int(1)]),
                Value::list(vec![Value::cons(Value::symbol("vm-f"), Value::Int(42))]),
            ],
        )
        .expect_err("environment entries with non-callables should surface invalid-function");
        match invalid_env_function {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "invalid-function");
                assert_eq!(sig.data, vec![Value::Int(42)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn macroexpand_runtime_improper_lists_match_oracle_error_behavior() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let not_macro = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::cons(Value::symbol("foo"), Value::Int(1))],
        )
        .expect("non-macro improper forms should pass through unchanged");
        assert_eq!(not_macro, Value::cons(Value::symbol("foo"), Value::Int(1)));

        let improper_macro = builtin_macroexpand_eval(
            &mut eval,
            vec![Value::cons(Value::symbol("when"), Value::Int(1))],
        )
        .expect_err("macro expansion should reject improper argument lists");
        match improper_macro {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("listp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn indirect_function_nil_and_non_symbol_behavior() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let noerror = builtin_indirect_function(
            &mut eval,
            vec![Value::symbol("definitely-not-a-function"), Value::True],
        )
        .expect("indirect-function should return nil when noerror is non-nil");
        assert!(noerror.is_nil());

        let unresolved =
            builtin_indirect_function(&mut eval, vec![Value::symbol("definitely-not-a-function")])
                .expect("indirect-function should return nil for unresolved function");
        assert!(unresolved.is_nil());

        let nil_input = builtin_indirect_function(&mut eval, vec![Value::Nil])
            .expect("indirect-function should return nil for nil input");
        assert!(nil_input.is_nil());

        let true_input = builtin_indirect_function(&mut eval, vec![Value::True])
            .expect("indirect-function should treat t as a symbol and return nil");
        assert!(true_input.is_nil());

        let keyword_input = builtin_indirect_function(
            &mut eval,
            vec![Value::Keyword(":vm-indirect-keyword".to_string())],
        )
        .expect("indirect-function should treat keywords as symbols and return nil");
        assert!(keyword_input.is_nil());

        let passthrough = builtin_indirect_function(&mut eval, vec![Value::Int(42)])
            .expect("non-symbol should be returned as-is");
        assert_eq!(passthrough, Value::Int(42));
    }

    #[test]
    fn indirect_function_resolves_deep_alias_chain_without_depth_cutoff() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let depth = 120;

        for idx in 0..depth {
            let name = format!("vm-test-deep-alias-{idx}");
            let target = if idx == depth - 1 {
                Value::symbol("car")
            } else {
                Value::symbol(format!("vm-test-deep-alias-{}", idx + 1))
            };
            eval.obarray_mut().set_symbol_function(&name, target);
        }

        let resolved =
            builtin_indirect_function(&mut eval, vec![Value::symbol("vm-test-deep-alias-0")])
                .expect("indirect-function should resolve deep alias chains");
        assert_eq!(resolved, Value::Subr("car".to_string()));
    }

    #[test]
    fn fset_rejects_self_alias_cycle() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let err = builtin_fset(
            &mut eval,
            vec![
                Value::symbol("vm-test-fset-cycle-self"),
                Value::symbol("vm-test-fset-cycle-self"),
            ],
        )
        .expect_err("fset should reject self-referential alias cycles");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "cyclic-function-indirection");
                assert_eq!(sig.data, vec![Value::symbol("vm-test-fset-cycle-self")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let unresolved =
            builtin_indirect_function(&mut eval, vec![Value::symbol("vm-test-fset-cycle-self")])
                .expect("indirect-function should still resolve");
        assert!(unresolved.is_nil());
    }

    #[test]
    fn fset_rejects_two_node_alias_cycle() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let first = builtin_fset(
            &mut eval,
            vec![
                Value::symbol("vm-test-fset-cycle-a"),
                Value::symbol("vm-test-fset-cycle-b"),
            ],
        )
        .expect("first alias should be accepted");
        assert_eq!(first, Value::symbol("vm-test-fset-cycle-b"));

        let err = builtin_fset(
            &mut eval,
            vec![
                Value::symbol("vm-test-fset-cycle-b"),
                Value::symbol("vm-test-fset-cycle-a"),
            ],
        )
        .expect_err("fset should reject second edge that closes alias cycle");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "cyclic-function-indirection");
                assert_eq!(sig.data, vec![Value::symbol("vm-test-fset-cycle-b")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn fset_rejects_keyword_and_t_alias_cycles() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let first = builtin_fset(
            &mut eval,
            vec![Value::keyword(":vmk2"), Value::keyword(":vmk3")],
        )
        .expect("first keyword alias should be accepted");
        assert_eq!(first, Value::keyword(":vmk3"));

        let keyword_cycle = builtin_fset(
            &mut eval,
            vec![Value::keyword(":vmk3"), Value::keyword(":vmk2")],
        )
        .expect_err("second keyword edge should close cycle");
        match keyword_cycle {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "cyclic-function-indirection");
                assert_eq!(sig.data, vec![Value::symbol(":vmk3")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        builtin_fset(&mut eval, vec![Value::True, Value::keyword(":vmk")])
            .expect("fset should allow binding t to keyword alias");

        let t_cycle = builtin_fset(&mut eval, vec![Value::keyword(":vmk"), Value::True])
            .expect_err("keyword->t edge should be rejected when t->keyword exists");
        match t_cycle {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "cyclic-function-indirection");
                assert_eq!(sig.data, vec![Value::symbol(":vmk")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn fset_nil_signals_setting_constant() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let err = builtin_fset(&mut eval, vec![Value::Nil, Value::symbol("car")])
            .expect_err("fset should reject writing nil's function cell");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "setting-constant");
                assert_eq!(sig.data, vec![Value::symbol("nil")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn fset_t_accepts_symbol_cell_updates() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let result = builtin_fset(&mut eval, vec![Value::True, Value::symbol("car")])
            .expect("fset should allow writing t's function cell");
        assert_eq!(result, Value::symbol("car"));

        let resolved = builtin_indirect_function(&mut eval, vec![Value::True])
            .expect("indirect-function should resolve t after fset");
        assert_eq!(resolved, Value::Subr("car".to_string()));
    }

    #[test]
    fn defvaralias_and_indirect_variable_follow_runtime_aliases() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let aliased = builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-new"),
                Value::symbol("vm-defvaralias-old"),
                Value::string("vm-doc"),
            ],
        )
        .expect("defvaralias should succeed");
        assert_eq!(aliased, Value::symbol("vm-defvaralias-old"));

        let doc = builtin_get(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-new"),
                Value::symbol("variable-documentation"),
            ],
        )
        .expect("get should return variable doc");
        assert_eq!(doc, Value::string("vm-doc"));

        let direct =
            builtin_indirect_variable_eval(&mut eval, vec![Value::symbol("vm-defvaralias-new")])
                .expect("indirect-variable should resolve aliases");
        assert_eq!(direct, Value::symbol("vm-defvaralias-old"));

        let special_new =
            builtin_special_variable_p(&mut eval, vec![Value::symbol("vm-defvaralias-new")])
                .expect("special-variable-p should accept alias");
        assert!(special_new.is_truthy());
        let special_old =
            builtin_special_variable_p(&mut eval, vec![Value::symbol("vm-defvaralias-old")])
                .expect("special-variable-p should mark target special");
        assert!(special_old.is_truthy());

        let set_value = builtin_set(
            &mut eval,
            vec![Value::symbol("vm-defvaralias-new"), Value::Int(7)],
        )
        .expect("set should assign through aliases");
        assert_eq!(set_value, Value::Int(7));
        let old_value = builtin_symbol_value(&mut eval, vec![Value::symbol("vm-defvaralias-old")])
            .expect("symbol-value should read aliased target");
        assert_eq!(old_value, Value::Int(7));

        builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-new"),
                Value::symbol("vm-defvaralias-old"),
            ],
        )
        .expect("defvaralias without doc should clear variable-documentation");
        let cleared_doc = builtin_get(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-new"),
                Value::symbol("variable-documentation"),
            ],
        )
        .expect("get should read cleared documentation");
        assert!(cleared_doc.is_nil());

        let unbound = builtin_makunbound(&mut eval, vec![Value::symbol("vm-defvaralias-new")])
            .expect("makunbound should clear target through alias");
        assert_eq!(unbound, Value::symbol("vm-defvaralias-new"));
        let bound_old = builtin_boundp(&mut eval, vec![Value::symbol("vm-defvaralias-old")])
            .expect("boundp should read aliased target");
        assert!(bound_old.is_nil());
    }

    #[test]
    fn defvaralias_rejects_invalid_inputs_and_cycles() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let constant_err = builtin_defvaralias_eval(
            &mut eval,
            vec![Value::symbol("nil"), Value::symbol("vm-defvaralias-x")],
        )
        .expect_err("defvaralias should reject constant aliases");
        match constant_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Cannot make a constant an alias: nil")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let type_err = builtin_defvaralias_eval(
            &mut eval,
            vec![Value::symbol("vm-defvaralias-bad"), Value::Int(1)],
        )
        .expect_err("defvaralias should validate OLD-BASE");
        match type_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("symbolp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-a"),
                Value::symbol("vm-defvaralias-b"),
            ],
        )
        .expect("first alias edge should succeed");
        let cycle_err = builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-b"),
                Value::symbol("vm-defvaralias-a"),
            ],
        )
        .expect_err("second alias edge should be rejected as a cycle");
        match cycle_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "cyclic-variable-indirection");
                assert_eq!(sig.data, vec![Value::symbol("vm-defvaralias-a")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn setplist_runtime_controls_get_put_and_symbol_plist_edges() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        let initial_plist = Value::list(vec![
            Value::symbol("a"),
            Value::Int(1),
            Value::symbol("b"),
            Value::Int(2),
        ]);
        let stored = builtin_setplist_eval(
            &mut eval,
            vec![Value::symbol("vm-setplist"), initial_plist.clone()],
        )
        .expect("setplist should store plist values");
        assert_eq!(stored, initial_plist);

        let read_plist = builtin_symbol_plist_fn(&mut eval, vec![Value::symbol("vm-setplist")])
            .expect("symbol-plist should return stored raw plist");
        assert_eq!(
            read_plist,
            Value::list(vec![
                Value::symbol("a"),
                Value::Int(1),
                Value::symbol("b"),
                Value::Int(2),
            ])
        );

        let lookup = builtin_get(
            &mut eval,
            vec![Value::symbol("vm-setplist"), Value::symbol("a")],
        )
        .expect("get should read entries from raw plist");
        assert_eq!(lookup, Value::Int(1));

        let put = builtin_put(
            &mut eval,
            vec![
                Value::symbol("vm-setplist"),
                Value::symbol("a"),
                Value::Int(5),
            ],
        )
        .expect("put should update raw plist entries");
        assert_eq!(put, Value::Int(5));
        let updated = builtin_symbol_plist_fn(&mut eval, vec![Value::symbol("vm-setplist")])
            .expect("symbol-plist should reflect updated plist values");
        assert_eq!(
            updated,
            Value::list(vec![
                Value::symbol("a"),
                Value::Int(5),
                Value::symbol("b"),
                Value::Int(2),
            ])
        );

        builtin_setplist_eval(&mut eval, vec![Value::symbol("vm-setplist"), Value::Int(1)])
            .expect("setplist should accept non-list plist values");
        let non_list = builtin_symbol_plist_fn(&mut eval, vec![Value::symbol("vm-setplist")])
            .expect("symbol-plist should return raw non-list values");
        assert_eq!(non_list, Value::Int(1));

        let missing = builtin_get(
            &mut eval,
            vec![Value::symbol("vm-setplist"), Value::symbol("a")],
        )
        .expect("get should treat non-list plist as missing keys");
        assert!(missing.is_nil());

        let put_err = builtin_put(
            &mut eval,
            vec![
                Value::symbol("vm-setplist"),
                Value::symbol("a"),
                Value::Int(8),
            ],
        )
        .expect_err("put should fail on non-plist raw values");
        match put_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("plistp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn variable_alias_to_constant_reports_alias_in_setting_constant_errors() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-alias-constant"),
                Value::symbol("nil"),
                Value::Nil,
            ],
        )
        .expect("defvaralias should allow aliasing to nil");

        let set_err = builtin_set(
            &mut eval,
            vec![Value::symbol("vm-alias-constant"), Value::Int(1)],
        )
        .expect_err("set should reject writes through nil aliases");
        match set_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "setting-constant");
                assert_eq!(sig.data, vec![Value::symbol("vm-alias-constant")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let default_err = builtin_set_default_toplevel_value(
            &mut eval,
            vec![Value::symbol("vm-alias-constant"), Value::Int(1)],
        )
        .expect_err("set-default-toplevel-value should reject nil aliases");
        match default_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "setting-constant");
                assert_eq!(sig.data, vec![Value::symbol("vm-alias-constant")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let unbind_err = builtin_makunbound(&mut eval, vec![Value::symbol("vm-alias-constant")])
            .expect_err("makunbound should reject nil aliases");
        match unbind_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "setting-constant");
                assert_eq!(sig.data, vec![Value::symbol("vm-alias-constant")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn defvaralias_raises_plistp_errors_when_symbol_plist_is_non_list() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        builtin_setplist_eval(
            &mut eval,
            vec![Value::symbol("vm-defvaralias-bad-plist"), Value::Int(1)],
        )
        .expect("setplist should seed malformed symbol plist value");

        let err = builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-defvaralias-bad-plist"),
                Value::symbol("vm-defvaralias-target"),
                Value::string("doc"),
            ],
        )
        .expect_err("defvaralias should preserve put-style plistp failures");
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("plistp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let unresolved = builtin_indirect_variable_eval(
            &mut eval,
            vec![Value::symbol("vm-defvaralias-bad-plist")],
        )
        .expect("failed defvaralias should still install alias edges");
        assert_eq!(unresolved, Value::symbol("vm-defvaralias-target"));
    }

    #[test]
    fn neovm_precompile_file_builtin_writes_cache_and_returns_path() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let mut eval = crate::elisp::eval::Evaluator::new();
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-builtin-precompile-ok-{unique}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        let source = dir.join("probe.el");
        fs::write(
            &source,
            ";;; -*- lexical-binding: t; -*-\n(setq vm-builtins-precompile 1)\n",
        )
        .expect("write source");

        let result =
            builtin_neovm_precompile_file(&mut eval, vec![Value::string(source.to_string_lossy())])
                .expect("precompile builtin should succeed");
        let cache_path = result
            .as_str()
            .expect("result should be a string path")
            .to_string();
        assert!(
            cache_path.ends_with(".neoc"),
            "cache path should end with .neoc"
        );
        assert!(
            std::path::Path::new(&cache_path).exists(),
            "cache file should exist"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn neovm_precompile_file_builtin_rejects_compiled_inputs() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let mut eval = crate::elisp::eval::Evaluator::new();
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-builtin-precompile-reject-{unique}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        let compiled = dir.join("probe.elc");
        fs::write(&compiled, "compiled").expect("write compiled artifact");

        let err = builtin_neovm_precompile_file(
            &mut eval,
            vec![Value::string(compiled.to_string_lossy())],
        )
        .expect_err("precompile builtin should reject .elc");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "file-error"),
            other => panic!("unexpected flow: {other:?}"),
        }

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn get_byte_string_semantics_match_oracle_edges() {
        let mut eval = crate::elisp::eval::Evaluator::new();

        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(0), Value::string("abc")]).unwrap(),
            Value::Int(97)
        );
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(1), Value::string("abc")]).unwrap(),
            Value::Int(98)
        );
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Nil, Value::string("abc")]).unwrap(),
            Value::Int(97)
        );

        let out_of_range =
            builtin_get_byte(&mut eval, vec![Value::Int(3), Value::string("abc")]).unwrap_err();
        match out_of_range {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::string("abc"), Value::Int(3)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let negative =
            builtin_get_byte(&mut eval, vec![Value::Int(-1), Value::string("abc")]).unwrap_err();
        match negative {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("wholenump"), Value::Int(-1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let non_ascii = builtin_get_byte(&mut eval, vec![Value::Int(0), Value::string("é")])
            .expect_err("multibyte non-byte8 should signal");
        match non_ascii {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Not an ASCII nor an 8-bit character: 233")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn get_byte_buffer_semantics_match_oracle_edges() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        builtin_erase_buffer(&mut eval, vec![]).unwrap();
        builtin_insert(&mut eval, vec![Value::string("abc")]).unwrap();

        assert_eq!(builtin_get_byte(&mut eval, vec![]).unwrap(), Value::Int(0));
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(1)]).unwrap(),
            Value::Int(97)
        );
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(2)]).unwrap(),
            Value::Int(98)
        );
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(3)]).unwrap(),
            Value::Int(99)
        );

        builtin_goto_char(&mut eval, vec![Value::Int(2)]).unwrap();
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Nil]).unwrap(),
            Value::Int(98)
        );

        let zero = builtin_get_byte(&mut eval, vec![Value::Int(0)]).unwrap_err();
        match zero {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(0), Value::Int(1), Value::Int(4)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let end = builtin_get_byte(&mut eval, vec![Value::Int(4)]).unwrap_err();
        match end {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(sig.data, vec![Value::Int(4), Value::Int(1), Value::Int(4)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn get_byte_unibyte_string_returns_raw_byte_values() {
        let mut eval = crate::elisp::eval::Evaluator::new();
        let s = builtin_unibyte_string(vec![Value::Int(255), Value::Int(65)]).unwrap();

        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(0), s.clone()]).unwrap(),
            Value::Int(255)
        );
        assert_eq!(
            builtin_get_byte(&mut eval, vec![Value::Int(1), s]).unwrap(),
            Value::Int(65)
        );
    }
}
