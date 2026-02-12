//! Built-in primitive functions.
//!
//! All functions here take pre-evaluated `Vec<Value>` arguments and return `EvalResult`.
//! The evaluator dispatches here after evaluating the argument expressions.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

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

/// True if any arg is a float (triggers float arithmetic).
fn has_float(args: &[Value]) -> bool {
    args.iter().any(|v| matches!(v, Value::Float(_)))
}

/// Return a numeric result: int if all were ints, float otherwise.
fn numeric_result(f: f64, was_float: bool) -> Value {
    if was_float {
        Value::Float(f)
    } else {
        Value::Int(f as i64)
    }
}

// ===========================================================================
// Arithmetic
// ===========================================================================

pub(crate) fn builtin_add(args: Vec<Value>) -> EvalResult {
    if has_float(&args) {
        let mut sum = 0.0f64;
        for a in &args {
            sum += expect_number(a)?;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum = 0i64;
        for a in &args {
            sum = sum
                .checked_add(expect_int(a)?)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(sum))
    }
}

pub(crate) fn builtin_sub(args: Vec<Value>) -> EvalResult {
    expect_min_args("-", &args, 1)?;
    if args.len() == 1 {
        // Unary negation
        if has_float(&args) {
            return Ok(Value::Float(-expect_number(&args[0])?));
        }
        return Ok(Value::Int(-expect_int(&args[0])?));
    }
    if has_float(&args) {
        let mut acc = expect_number(&args[0])?;
        for a in &args[1..] {
            acc -= expect_number(a)?;
        }
        Ok(Value::Float(acc))
    } else {
        let mut acc = expect_int(&args[0])?;
        for a in &args[1..] {
            acc = acc
                .checked_sub(expect_int(a)?)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(acc))
    }
}

pub(crate) fn builtin_mul(args: Vec<Value>) -> EvalResult {
    if has_float(&args) {
        let mut prod = 1.0f64;
        for a in &args {
            prod *= expect_number(a)?;
        }
        Ok(Value::Float(prod))
    } else {
        let mut prod = 1i64;
        for a in &args {
            prod = prod
                .checked_mul(expect_int(a)?)
                .ok_or_else(|| signal("overflow-error", vec![]))?;
        }
        Ok(Value::Int(prod))
    }
}

pub(crate) fn builtin_div(args: Vec<Value>) -> EvalResult {
    expect_min_args("/", &args, 2)?;
    if has_float(&args) {
        let mut acc = expect_number(&args[0])?;
        for a in &args[1..] {
            let d = expect_number(a)?;
            if d == 0.0 {
                return Err(signal("arith-error", vec![]));
            }
            acc /= d;
        }
        Ok(Value::Float(acc))
    } else {
        let mut acc = expect_int(&args[0])?;
        for a in &args[1..] {
            let d = expect_int(a)?;
            if d == 0 {
                return Err(signal("arith-error", vec![]));
            }
            acc /= d; // Truncating division like Emacs
        }
        Ok(Value::Int(acc))
    }
}

pub(crate) fn builtin_mod(args: Vec<Value>) -> EvalResult {
    expect_args("%", &args, 2)?;
    if has_float(&args) {
        let a = expect_number(&args[0])?;
        let b = expect_number(&args[1])?;
        if b == 0.0 {
            return Err(signal("arith-error", vec![]));
        }
        // Emacs mod: result has sign of divisor
        let r = a % b;
        let r = if (r < 0.0) != (b < 0.0) { r + b } else { r };
        Ok(Value::Float(r))
    } else {
        let a = expect_int(&args[0])?;
        let b = expect_int(&args[1])?;
        if b == 0 {
            return Err(signal("arith-error", vec![]));
        }
        // Emacs mod: result has sign of divisor
        let r = a % b;
        let r = if (r < 0) != (b < 0) { r + b } else { r };
        Ok(Value::Int(r))
    }
}

pub(crate) fn builtin_add1(args: Vec<Value>) -> EvalResult {
    expect_args("1+", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.checked_add(1).ok_or_else(|| signal("overflow-error", vec![]))?)),
        Value::Float(f) => Ok(Value::Float(f + 1.0)),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("number-or-marker-p"), other.clone()])),
    }
}

pub(crate) fn builtin_sub1(args: Vec<Value>) -> EvalResult {
    expect_args("1-", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.checked_sub(1).ok_or_else(|| signal("overflow-error", vec![]))?)),
        Value::Float(f) => Ok(Value::Float(f - 1.0)),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("number-or-marker-p"), other.clone()])),
    }
}

pub(crate) fn builtin_max(args: Vec<Value>) -> EvalResult {
    expect_min_args("max", &args, 1)?;
    let float = has_float(&args);
    let mut best = expect_number(&args[0])?;
    for a in &args[1..] {
        let n = expect_number(a)?;
        if n > best { best = n; }
    }
    Ok(numeric_result(best, float))
}

pub(crate) fn builtin_min(args: Vec<Value>) -> EvalResult {
    expect_min_args("min", &args, 1)?;
    let float = has_float(&args);
    let mut best = expect_number(&args[0])?;
    for a in &args[1..] {
        let n = expect_number(a)?;
        if n < best { best = n; }
    }
    Ok(numeric_result(best, float))
}

pub(crate) fn builtin_abs(args: Vec<Value>) -> EvalResult {
    expect_args("abs", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
    }
}

// ===========================================================================
// Logical / bitwise
// ===========================================================================

pub(crate) fn builtin_logand(args: Vec<Value>) -> EvalResult {
    let mut acc = -1i64; // all bits set
    for a in &args {
        acc &= expect_int(a)?;
    }
    Ok(Value::Int(acc))
}

pub(crate) fn builtin_logior(args: Vec<Value>) -> EvalResult {
    let mut acc = 0i64;
    for a in &args {
        acc |= expect_int(a)?;
    }
    Ok(Value::Int(acc))
}

pub(crate) fn builtin_logxor(args: Vec<Value>) -> EvalResult {
    let mut acc = 0i64;
    for a in &args {
        acc ^= expect_int(a)?;
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
        Ok(Value::Int(n.checked_shl(count as u32).unwrap_or(0)))
    } else {
        Ok(Value::Int(n >> (-count).min(63) as u32))
    }
}

// ===========================================================================
// Comparisons
// ===========================================================================

pub(crate) fn builtin_num_eq(args: Vec<Value>) -> EvalResult {
    expect_min_args("=", &args, 2)?;
    let first = expect_number(&args[0])?;
    for a in &args[1..] {
        if expect_number(a)? != first {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_lt(args: Vec<Value>) -> EvalResult {
    expect_min_args("<", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number(&pair[0])? < expect_number(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_le(args: Vec<Value>) -> EvalResult {
    expect_min_args("<=", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number(&pair[0])? <= expect_number(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_gt(args: Vec<Value>) -> EvalResult {
    expect_min_args(">", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number(&pair[0])? > expect_number(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_ge(args: Vec<Value>) -> EvalResult {
    expect_min_args(">=", &args, 2)?;
    for pair in args.windows(2) {
        if !(expect_number(&pair[0])? >= expect_number(&pair[1])?) {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::t())
}

pub(crate) fn builtin_num_ne(args: Vec<Value>) -> EvalResult {
    expect_args("/=", &args, 2)?;
    let a = expect_number(&args[0])?;
    let b = expect_number(&args[1])?;
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

pub(crate) fn builtin_characterp(args: Vec<Value>) -> EvalResult {
    expect_args("characterp", &args, 1)?;
    Ok(Value::bool(args[0].is_char()))
}

pub(crate) fn builtin_functionp(args: Vec<Value>) -> EvalResult {
    expect_args("functionp", &args, 1)?;
    Ok(Value::bool(args[0].is_function()))
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

pub(crate) fn builtin_car(args: Vec<Value>) -> EvalResult {
    expect_args("car", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").car.clone()),
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("listp"), args[0].clone()])),
    }
}

pub(crate) fn builtin_cdr(args: Vec<Value>) -> EvalResult {
    expect_args("cdr", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").cdr.clone()),
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("listp"), args[0].clone()])),
    }
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
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("consp"), args[0].clone()])),
    }
}

pub(crate) fn builtin_setcdr(args: Vec<Value>) -> EvalResult {
    expect_args("setcdr", &args, 2)?;
    match &args[0] {
        Value::Cons(cell) => {
            cell.lock().expect("poisoned").cdr = args[1].clone();
            Ok(args[1].clone())
        }
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("consp"), args[0].clone()])),
    }
}

pub(crate) fn builtin_list(args: Vec<Value>) -> EvalResult {
    Ok(Value::list(args))
}

pub(crate) fn builtin_length(args: Vec<Value>) -> EvalResult {
    expect_args("length", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Int(0)),
        Value::Cons(_) => {
            match list_length(&args[0]) {
                Some(n) => Ok(Value::Int(n as i64)),
                None => Err(signal("wrong-type-argument", vec![Value::symbol("listp"), args[0].clone()])),
            }
        }
        Value::Str(s) => Ok(Value::Int(s.chars().count() as i64)),
        Value::Vector(v) => Ok(Value::Int(v.lock().expect("poisoned").len() as i64)),
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("sequencep"), args[0].clone()])),
    }
}

pub(crate) fn builtin_nth(args: Vec<Value>) -> EvalResult {
    expect_args("nth", &args, 2)?;
    let n = expect_int(&args[0])? as usize;
    let mut cursor = args[1].clone();
    for _ in 0..n {
        match cursor {
            Value::Cons(cell) => cursor = cell.lock().expect("poisoned").cdr.clone(),
            _ => return Ok(Value::Nil),
        }
    }
    match cursor {
        Value::Cons(cell) => Ok(cell.lock().expect("poisoned").car.clone()),
        _ => Ok(Value::Nil),
    }
}

pub(crate) fn builtin_nthcdr(args: Vec<Value>) -> EvalResult {
    expect_args("nthcdr", &args, 2)?;
    let n = expect_int(&args[0])? as usize;
    let mut cursor = args[1].clone();
    for _ in 0..n {
        match cursor {
            Value::Cons(cell) => cursor = cell.lock().expect("poisoned").cdr.clone(),
            _ => return Ok(Value::Nil),
        }
    }
    Ok(cursor)
}

pub(crate) fn builtin_append(args: Vec<Value>) -> EvalResult {
    if args.is_empty() {
        return Ok(Value::Nil);
    }
    if args.len() == 1 {
        return Ok(args[0].clone());
    }

    // Collect all elements from all lists except the last, then use last as tail
    let mut elements: Vec<Value> = Vec::new();
    for arg in &args[..args.len() - 1] {
        if let Some(items) = list_to_vec(arg) {
            elements.extend(items);
        }
    }

    let last = &args[args.len() - 1];
    if elements.is_empty() {
        return Ok(last.clone());
    }

    // Build list with last arg as tail (supports improper lists)
    let tail = last.clone();
    Ok(elements.into_iter().rev().fold(tail, |acc, item| {
        Value::cons(item, acc)
    }))
}

pub(crate) fn builtin_reverse(args: Vec<Value>) -> EvalResult {
    expect_args("reverse", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            let items = list_to_vec(&args[0])
                .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("listp"), args[0].clone()]))?;
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
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("sequencep"), args[0].clone()])),
    }
}

pub(crate) fn builtin_nreverse(args: Vec<Value>) -> EvalResult {
    // For simplicity, same as reverse (we'd need to destructively modify cons cells)
    builtin_reverse(args)
}

pub(crate) fn builtin_member(args: Vec<Value>) -> EvalResult {
    expect_args("member", &args, 2)?;
    let target = &args[0];
    let mut cursor = args[1].clone();
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
            _ => return Ok(Value::Nil),
        }
    }
}

pub(crate) fn builtin_memq(args: Vec<Value>) -> EvalResult {
    expect_args("memq", &args, 2)?;
    let target = &args[0];
    let mut cursor = args[1].clone();
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
            _ => return Ok(Value::Nil),
        }
    }
}

pub(crate) fn builtin_assoc(args: Vec<Value>) -> EvalResult {
    expect_args("assoc", &args, 2)?;
    let key = &args[0];
    let mut cursor = args[1].clone();
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
            _ => return Ok(Value::Nil),
        }
    }
}

pub(crate) fn builtin_assq(args: Vec<Value>) -> EvalResult {
    expect_args("assq", &args, 2)?;
    let key = &args[0];
    let mut cursor = args[1].clone();
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
            _ => return Ok(Value::Nil),
        }
    }
}

pub(crate) fn builtin_copy_sequence(args: Vec<Value>) -> EvalResult {
    expect_args("copy-sequence", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            let items = list_to_vec(&args[0])
                .ok_or_else(|| signal("wrong-type-argument", vec![]))?;
            Ok(Value::list(items))
        }
        Value::Str(s) => Ok(Value::string((**s).clone())),
        Value::Vector(v) => Ok(Value::vector(v.lock().expect("poisoned").clone())),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("sequencep"), other.clone()])),
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

pub(crate) fn builtin_substring(args: Vec<Value>) -> EvalResult {
    expect_min_args("substring", &args, 1)?;
    let s = expect_string(&args[0])?;
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len() as i64;

    let from = if args.len() > 1 {
        let n = expect_int(&args[1])?;
        if n < 0 { (len + n).max(0) as usize } else { n as usize }
    } else {
        0
    };

    let to = if args.len() > 2 {
        if args[2].is_nil() {
            chars.len()
        } else {
            let n = expect_int(&args[2])?;
            if n < 0 { (len + n).max(0) as usize } else { n as usize }
        }
    } else {
        chars.len()
    };

    let from = from.min(chars.len());
    let to = to.min(chars.len());
    if from > to {
        return Ok(Value::string(""));
    }
    let result: String = chars[from..to].iter().collect();
    Ok(Value::string(result))
}

pub(crate) fn builtin_concat(args: Vec<Value>) -> EvalResult {
    let mut result = String::new();
    for arg in &args {
        match arg {
            Value::Str(s) => result.push_str(s),
            Value::Nil => {}
            Value::Cons(_) => {
                if let Some(items) = list_to_vec(arg) {
                    for item in items {
                        if let Value::Char(c) = item {
                            result.push(c);
                        } else if let Value::Int(n) = item {
                            if let Some(c) = char::from_u32(n as u32) {
                                result.push(c);
                            }
                        }
                    }
                }
            }
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                for item in items.iter() {
                    if let Value::Char(c) = item {
                        result.push(*c);
                    } else if let Value::Int(n) = item {
                        if let Some(c) = char::from_u32(*n as u32) {
                            result.push(c);
                        }
                    }
                }
            }
            _ => return Err(signal("wrong-type-argument", vec![Value::symbol("sequencep"), arg.clone()])),
        }
    }
    Ok(Value::string(result))
}

pub(crate) fn builtin_string_to_number(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-to-number", &args, 1)?;
    let s = expect_string(&args[0])?;
    let base = if args.len() > 1 { expect_int(&args[1])? as u32 } else { 10 };

    let s = s.trim();
    if base == 10 {
        if let Ok(n) = s.parse::<i64>() {
            return Ok(Value::Int(n));
        }
        if let Ok(f) = s.parse::<f64>() {
            return Ok(Value::Float(f));
        }
    } else if let Ok(n) = i64::from_str_radix(s, base) {
        return Ok(Value::Int(n));
    }
    Ok(Value::Int(0))
}

pub(crate) fn builtin_number_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("number-to-string", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::string(n.to_string())),
        Value::Float(f) => Ok(Value::string(format!("{}", f))),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
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
        other => Err(signal("wrong-type-argument", vec![Value::symbol("char-or-string-p"), other.clone()])),
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
        other => Err(signal("wrong-type-argument", vec![Value::symbol("char-or-string-p"), other.clone()])),
    }
}

pub(crate) fn builtin_string_match(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-match", &args, 2)?;
    let _pattern = expect_string(&args[0])?;
    let _s = expect_string(&args[1])?;
    // TODO: implement regex matching
    Ok(Value::Nil)
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

// ===========================================================================
// Vector operations
// ===========================================================================

pub(crate) fn builtin_make_vector(args: Vec<Value>) -> EvalResult {
    expect_args("make-vector", &args, 2)?;
    let len = expect_int(&args[0])? as usize;
    Ok(Value::vector(vec![args[1].clone(); len]))
}

pub(crate) fn builtin_vector(args: Vec<Value>) -> EvalResult {
    Ok(Value::vector(args))
}

pub(crate) fn builtin_aref(args: Vec<Value>) -> EvalResult {
    expect_args("aref", &args, 2)?;
    let idx = expect_int(&args[1])? as usize;
    match &args[0] {
        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            items.get(idx).cloned().ok_or_else(|| {
                signal("args-out-of-range", vec![args[0].clone(), args[1].clone()])
            })
        }
        Value::Str(s) => {
            s.chars().nth(idx).map(Value::Char).ok_or_else(|| {
                signal("args-out-of-range", vec![args[0].clone(), args[1].clone()])
            })
        }
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("arrayp"), args[0].clone()])),
    }
}

pub(crate) fn builtin_aset(args: Vec<Value>) -> EvalResult {
    expect_args("aset", &args, 3)?;
    let idx = expect_int(&args[1])? as usize;
    match &args[0] {
        Value::Vector(v) => {
            let mut items = v.lock().expect("poisoned");
            if idx >= items.len() {
                return Err(signal("args-out-of-range", vec![args[0].clone(), args[1].clone()]));
            }
            items[idx] = args[2].clone();
            Ok(args[2].clone())
        }
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("arrayp"), args[0].clone()])),
    }
}

pub(crate) fn builtin_vconcat(args: Vec<Value>) -> EvalResult {
    let mut result = Vec::new();
    for arg in &args {
        match arg {
            Value::Vector(v) => result.extend(v.lock().expect("poisoned").iter().cloned()),
            Value::Nil => {}
            Value::Cons(_) => {
                if let Some(items) = list_to_vec(arg) {
                    result.extend(items);
                }
            }
            _ => return Err(signal("wrong-type-argument", vec![Value::symbol("sequencep"), arg.clone()])),
        }
    }
    Ok(Value::vector(result))
}

// ===========================================================================
// Hash table operations
// ===========================================================================

pub(crate) fn builtin_make_hash_table(args: Vec<Value>) -> EvalResult {
    let mut test = HashTableTest::Eql;
    let mut i = 0;
    while i < args.len() {
        if let Value::Keyword(ref k) = args[i] {
            if k == ":test" && i + 1 < args.len() {
                test = match args[i + 1].as_symbol_name() {
                    Some("eq") => HashTableTest::Eq,
                    Some("eql") => HashTableTest::Eql,
                    Some("equal") => HashTableTest::Equal,
                    _ => HashTableTest::Eql,
                };
                i += 2;
                continue;
            }
        }
        i += 1;
    }
    Ok(Value::hash_table(test))
}

pub(crate) fn builtin_gethash(args: Vec<Value>) -> EvalResult {
    expect_min_args("gethash", &args, 2)?;
    let default = if args.len() > 2 { args[2].clone() } else { Value::Nil };
    match &args[1] {
        Value::HashTable(ht) => {
            let ht = ht.lock().expect("poisoned");
            let key = args[0].to_hash_key(&ht.test);
            Ok(ht.data.get(&key).cloned().unwrap_or(default))
        }
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("hash-table-p"), args[1].clone()])),
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
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("hash-table-p"), args[2].clone()])),
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
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("hash-table-p"), args[1].clone()])),
    }
}

pub(crate) fn builtin_clrhash(args: Vec<Value>) -> EvalResult {
    expect_args("clrhash", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            ht.lock().expect("poisoned").data.clear();
            Ok(Value::Nil)
        }
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("hash-table-p"), args[0].clone()])),
    }
}

pub(crate) fn builtin_hash_table_count(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-count", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => Ok(Value::Int(ht.lock().expect("poisoned").data.len() as i64)),
        _ => Err(signal("wrong-type-argument", vec![Value::symbol("hash-table-p"), args[0].clone()])),
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
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
    }
}

pub(crate) fn builtin_truncate(args: Vec<Value>) -> EvalResult {
    expect_args("truncate", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(*f as i64)),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
    }
}

pub(crate) fn builtin_floor(args: Vec<Value>) -> EvalResult {
    expect_args("floor", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
    }
}

pub(crate) fn builtin_ceiling(args: Vec<Value>) -> EvalResult {
    expect_args("ceiling", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
    }
}

pub(crate) fn builtin_round(args: Vec<Value>) -> EvalResult {
    expect_args("round", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(f) => Ok(Value::Int(f.round() as i64)),
        other => Err(signal("wrong-type-argument", vec![Value::symbol("numberp"), other.clone()])),
    }
}

pub(crate) fn builtin_char_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("char-to-string", &args, 1)?;
    match &args[0] {
        Value::Char(c) => Ok(Value::string(c.to_string())),
        Value::Int(n) => {
            char::from_u32(*n as u32)
                .map(|c| Value::string(c.to_string()))
                .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("characterp"), args[0].clone()]))
        }
        other => Err(signal("wrong-type-argument", vec![Value::symbol("characterp"), other.clone()])),
    }
}

pub(crate) fn builtin_string_to_char(args: Vec<Value>) -> EvalResult {
    expect_args("string-to-char", &args, 1)?;
    let s = expect_string(&args[0])?;
    Ok(Value::Int(s.chars().next().map(|c| c as i64).unwrap_or(0)))
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
    // Simple implementation: rebuild plist with new key/value
    let plist = &args[0];
    let key = &args[1];
    let new_val = &args[2];

    let mut items = Vec::new();
    let mut found = false;
    let mut cursor = plist.clone();
    loop {
        match cursor {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let k = pair.car.clone();
                match &pair.cdr {
                    Value::Cons(val_cell) => {
                        let val_pair = val_cell.lock().expect("poisoned");
                        if eq_value(&k, key) {
                            items.push(k);
                            items.push(new_val.clone());
                            found = true;
                        } else {
                            items.push(k);
                            items.push(val_pair.car.clone());
                        }
                        cursor = val_pair.cdr.clone();
                    }
                    _ => break,
                }
            }
            _ => break,
        }
    }

    if !found {
        items.push(key.clone());
        items.push(new_val.clone());
    }

    Ok(Value::list(items))
}

// ===========================================================================
// Misc
// ===========================================================================

pub(crate) fn builtin_identity(args: Vec<Value>) -> EvalResult {
    expect_args("identity", &args, 1)?;
    Ok(args[0].clone())
}

pub(crate) fn builtin_message(args: Vec<Value>) -> EvalResult {
    expect_min_args("message", &args, 1)?;
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

pub(crate) fn builtin_error(args: Vec<Value>) -> EvalResult {
    expect_min_args("error", &args, 1)?;
    let msg = match builtin_format(args)? {
        Value::Str(s) => (*s).clone(),
        _ => "error".to_string(),
    };
    Err(signal("error", vec![Value::string(msg)]))
}

pub(crate) fn builtin_symbol_name(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-name", &args, 1)?;
    match args[0].as_symbol_name() {
        Some(name) => Ok(Value::string(name)),
        None => Err(signal("wrong-type-argument", vec![Value::symbol("symbolp"), args[0].clone()])),
    }
}

pub(crate) fn builtin_intern(args: Vec<Value>) -> EvalResult {
    expect_args("intern", &args, 1)?;
    let name = expect_string(&args[0])?;
    Ok(Value::symbol(name))
}

pub(crate) fn builtin_make_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("make-symbol", &args, 1)?;
    let name = expect_string(&args[0])?;
    Ok(Value::Symbol(name))
}

pub(crate) fn builtin_apply(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() < 2 {
        return Err(signal("wrong-number-of-arguments", vec![Value::symbol("apply"), Value::Int(args.len() as i64)]));
    }
    let func = args[0].clone();
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();

    // Last argument must be a list, which gets spread
    match last {
        Value::Nil => {}
        Value::Cons(_) => {
            if let Some(items) = list_to_vec(last) {
                call_args.extend(items);
            }
        }
        _ => return Err(signal("wrong-type-argument", vec![Value::symbol("listp"), last.clone()])),
    }

    eval.apply(func, call_args)
}

// ===========================================================================
// Higher-order
// ===========================================================================

pub(crate) fn builtin_mapcar(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal("wrong-number-of-arguments", vec![Value::symbol("mapcar"), Value::Int(args.len() as i64)]));
    }
    let func = args[0].clone();
    let mut results = Vec::new();
    let mut cursor = args[1].clone();
    loop {
        match cursor {
            Value::Nil => break,
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let item = pair.car.clone();
                cursor = pair.cdr.clone();
                drop(pair);
                results.push(eval.apply(func.clone(), vec![item])?);
            }
            _ => return Err(signal("wrong-type-argument", vec![Value::symbol("listp"), cursor])),
        }
    }
    Ok(Value::list(results))
}

pub(crate) fn builtin_mapc(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal("wrong-number-of-arguments", vec![Value::symbol("mapc"), Value::Int(args.len() as i64)]));
    }
    let func = args[0].clone();
    let list_val = args[1].clone();
    let mut cursor = list_val.clone();
    loop {
        match cursor {
            Value::Nil => break,
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let item = pair.car.clone();
                cursor = pair.cdr.clone();
                drop(pair);
                eval.apply(func.clone(), vec![item])?;
            }
            _ => return Err(signal("wrong-type-argument", vec![Value::symbol("listp"), cursor])),
        }
    }
    Ok(list_val)
}

pub(crate) fn builtin_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal("wrong-number-of-arguments", vec![Value::symbol("sort"), Value::Int(args.len() as i64)]));
    }
    let pred = args[1].clone();
    let mut items = list_to_vec(&args[0])
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("listp"), args[0].clone()]))?;

    // Simple insertion sort (stable sort with predicate)
    // We can't use sort_by because the predicate can fail
    for i in 1..items.len() {
        let mut j = i;
        while j > 0 {
            let result = eval.apply(pred.clone(), vec![items[j].clone(), items[j - 1].clone()])?;
            if result.is_truthy() {
                items.swap(j, j - 1);
                j -= 1;
            } else {
                break;
            }
        }
    }

    Ok(Value::list(items))
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

// ===========================================================================
// Dispatch table
// ===========================================================================

/// Try to dispatch a builtin function by name. Returns None if not a known builtin.
pub(crate) fn dispatch_builtin(
    eval: &mut super::eval::Evaluator,
    name: &str,
    args: Vec<Value>,
) -> Option<EvalResult> {
    // Functions that need the evaluator (higher-order)
    match name {
        "apply" => return Some(builtin_apply(eval, args)),
        "mapcar" => return Some(builtin_mapcar(eval, args)),
        "mapc" => return Some(builtin_mapc(eval, args)),
        "sort" => return Some(builtin_sort(eval, args)),
        _ => {}
    }

    // Pure builtins (no evaluator needed)
    Some(match name {
        // Arithmetic
        "+" => builtin_add(args),
        "-" => builtin_sub(args),
        "*" => builtin_mul(args),
        "/" => builtin_div(args),
        "%" | "mod" => builtin_mod(args),
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

        // Type predicates
        "null" => builtin_null(args),
        "not" => builtin_not(args),
        "atom" => builtin_atom(args),
        "consp" => builtin_consp(args),
        "listp" => builtin_listp(args),
        "nlistp" => builtin_nlistp(args),
        "symbolp" => builtin_symbolp(args),
        "numberp" => builtin_numberp(args),
        "integerp" => builtin_integerp(args),
        "floatp" => builtin_floatp(args),
        "stringp" => builtin_stringp(args),
        "vectorp" => builtin_vectorp(args),
        "characterp" => builtin_characterp(args),
        "functionp" => builtin_functionp(args),
        "keywordp" => builtin_keywordp(args),
        "hash-table-p" => builtin_hash_table_p(args),
        "type-of" => builtin_type_of(args),
        "sequencep" => builtin_sequencep(args),
        "arrayp" => builtin_arrayp(args),

        // Equality
        "eq" => builtin_eq(args),
        "eql" => builtin_eql(args),
        "equal" => builtin_equal(args),

        // Cons / List
        "cons" => builtin_cons(args),
        "car" => builtin_car(args),
        "cdr" => builtin_cdr(args),
        "car-safe" => builtin_car_safe(args),
        "cdr-safe" => builtin_cdr_safe(args),
        "setcar" => builtin_setcar(args),
        "setcdr" => builtin_setcdr(args),
        "list" => builtin_list(args),
        "length" => builtin_length(args),
        "nth" => builtin_nth(args),
        "nthcdr" => builtin_nthcdr(args),
        "append" => builtin_append(args),
        "reverse" => builtin_reverse(args),
        "nreverse" => builtin_nreverse(args),
        "member" => builtin_member(args),
        "memq" => builtin_memq(args),
        "assoc" => builtin_assoc(args),
        "assq" => builtin_assq(args),
        "copy-sequence" => builtin_copy_sequence(args),

        // String
        "string-equal" | "string=" => builtin_string_equal(args),
        "string-lessp" | "string<" => builtin_string_lessp(args),
        "substring" => builtin_substring(args),
        "concat" => builtin_concat(args),
        "string-to-number" => builtin_string_to_number(args),
        "number-to-string" => builtin_number_to_string(args),
        "upcase" => builtin_upcase(args),
        "downcase" => builtin_downcase(args),
        "string-match" => builtin_string_match(args),
        "format" => builtin_format(args),

        // Vector
        "make-vector" => builtin_make_vector(args),
        "vector" => builtin_vector(args),
        "aref" => builtin_aref(args),
        "aset" => builtin_aset(args),
        "vconcat" => builtin_vconcat(args),

        // Hash table
        "make-hash-table" => builtin_make_hash_table(args),
        "gethash" => builtin_gethash(args),
        "puthash" => builtin_puthash(args),
        "remhash" => builtin_remhash(args),
        "clrhash" => builtin_clrhash(args),
        "hash-table-count" => builtin_hash_table_count(args),

        // Conversion
        "float" => builtin_float(args),
        "truncate" => builtin_truncate(args),
        "floor" => builtin_floor(args),
        "ceiling" => builtin_ceiling(args),
        "round" => builtin_round(args),
        "char-to-string" => builtin_char_to_string(args),
        "string-to-char" => builtin_string_to_char(args),

        // Property lists
        "plist-get" => builtin_plist_get(args),
        "plist-put" => builtin_plist_put(args),

        // Symbol
        "symbol-name" => builtin_symbol_name(args),
        "intern" => builtin_intern(args),
        "make-symbol" => builtin_make_symbol(args),

        // Misc
        "identity" => builtin_identity(args),
        "message" => builtin_message(args),
        "error" => builtin_error(args),

        _ => return None,
    })
}
