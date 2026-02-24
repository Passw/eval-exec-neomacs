//! Char-table and bool-vector types.
//!
//! Since we cannot add new `Value` variants, these types are represented using
//! existing `Value` infrastructure:
//!
//! - **Char-table**: A `Value::Vector` whose first element is the tag symbol
//!   `--char-table--`.  The layout is:
//!   `[--char-table-- DEFAULT PARENT SUB-TYPE EXTRA-SLOTS-COUNT ...EXTRA-SLOTS... ...DATA-PAIRS...]`
//!   where DATA-PAIRS are stored as consecutive `(char-code, value)` pairs
//!   starting after the extra slots.  For efficiency, lookups walk the data
//!   pairs linearly (fine for the typical sparse char-table).
//!
//! - **Bool-vector**: A `Value::Vector` whose first element is the tag symbol
//!   `--bool-vector--`.  The layout is:
//!   `[--bool-vector-- SIZE ...BITS...]`
//!   where SIZE is `Value::Int(length)` and each subsequent element is
//!   `Value::Int(0)` or `Value::Int(1)`.

use super::error::{signal, EvalResult, Flow};
use super::eval::Evaluator;
use super::intern::resolve_sym;
use super::value::*;

// ---------------------------------------------------------------------------
// Tag constants
// ---------------------------------------------------------------------------

const CHAR_TABLE_TAG: &str = "--char-table--";
const BOOL_VECTOR_TAG: &str = "--bool-vector--";

// Char-table fixed-layout indices (after the tag at index 0):
const CT_DEFAULT: usize = 1; // default value
const CT_PARENT: usize = 2; // parent char-table or nil
const CT_SUBTYPE: usize = 3; // sub-type symbol
const CT_EXTRA_COUNT: usize = 4; // Value::Int — number of extra slots
const CT_EXTRA_START: usize = 5; // first extra slot (if any)
const CT_ALL_CHARS_SENTINEL: i64 = i64::MIN; // wildcard range set via RANGE=t
const CT_BASE_FALLBACK_SENTINEL: i64 = i64::MIN + 1; // initial/default char fallback
const CT_LOGICAL_LENGTH: i64 = 0x3F_FFFF;

// Bool-vector fixed-layout indices:
const BV_SIZE: usize = 1; // Value::Int — logical length

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

/// Return `true` if `v` is a char-table (tagged vector).
pub fn is_char_table(v: &Value) -> bool {
    if let Value::Vector(arc) = v {
        let vec = with_heap(|h| h.get_vector(*arc).clone());
        vec.len() >= CT_EXTRA_START && matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == CHAR_TABLE_TAG)
    } else {
        false
    }
}

/// Return `true` if `v` is a bool-vector (tagged vector).
pub fn is_bool_vector(v: &Value) -> bool {
    if let Value::Vector(arc) = v {
        let vec = with_heap(|h| h.get_vector(*arc).clone());
        vec.len() >= 2 && matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == BOOL_VECTOR_TAG)
    } else {
        false
    }
}

/// Return the logical bit length if `v` is a bool-vector.
pub(crate) fn bool_vector_length(v: &Value) -> Option<i64> {
    let Value::Vector(arc) = v else {
        return None;
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());
    if vec.len() < 2 || !matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == BOOL_VECTOR_TAG) {
        return None;
    }
    Some(match &vec[BV_SIZE] {
        Value::Int(n) => *n,
        _ => 0,
    })
}

/// Return the logical sequence length if `v` is a char-table.
pub(crate) fn char_table_length(v: &Value) -> Option<i64> {
    let Value::Vector(arc) = v else {
        return None;
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());
    if vec.len() >= CT_EXTRA_START && matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == CHAR_TABLE_TAG) {
        Some(CT_LOGICAL_LENGTH)
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Expect exactly N arguments, or signal `wrong-number-of-arguments`.
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

/// Signal `wrong-type-argument` with a predicate name.
fn wrong_type(pred: &str, got: &Value) -> Flow {
    signal(
        "wrong-type-argument",
        vec![Value::symbol(pred), got.clone()],
    )
}

/// Extract an integer (Int or Char), signal otherwise.
fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(wrong_type("integerp", other)),
    }
}

/// Extract a non-negative integer (for index-like args), signaling with
/// `wholenump` on any mismatch.
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

/// Data-pairs region start index for a char-table vector.
fn ct_data_start(vec: &[Value]) -> usize {
    let extra_count = match &vec[CT_EXTRA_COUNT] {
        Value::Int(n) => *n as usize,
        _ => 0,
    };
    CT_EXTRA_START + extra_count
}

// ---------------------------------------------------------------------------
// Char-table builtins
// ---------------------------------------------------------------------------

/// `(make-char-table SUB-TYPE &optional DEFAULT)` -- create a char-table.
pub(crate) fn builtin_make_char_table(args: Vec<Value>) -> EvalResult {
    expect_min_args("make-char-table", &args, 1)?;
    expect_max_args("make-char-table", &args, 2)?;
    let sub_type = args[0].clone();
    let default = if args.len() > 1 {
        args[1].clone()
    } else {
        Value::Nil
    };
    let mut vec = vec![
        Value::symbol(CHAR_TABLE_TAG),
        default.clone(), // CT_DEFAULT
        Value::Nil,      // CT_PARENT
        sub_type,        // CT_SUBTYPE
        Value::Int(0),   // CT_EXTRA_COUNT
    ];
    // Keep a dedicated char-lookup fallback separate from the NIL range
    // default slot so `(set-char-table-range TABLE nil VALUE)` can update NIL
    // lookups without retroactively changing existing character fallback.
    vec.push(Value::Int(CT_BASE_FALLBACK_SENTINEL));
    vec.push(default);
    Ok(Value::vector(vec))
}

/// `(char-table-p OBJ)` -- return t if OBJ is a char-table.
pub(crate) fn builtin_char_table_p(args: Vec<Value>) -> EvalResult {
    expect_args("char-table-p", &args, 1)?;
    Ok(Value::bool(is_char_table(&args[0])))
}

/// `(set-char-table-range CHAR-TABLE RANGE VALUE)` -- set entries.
///
/// RANGE may be:
/// - a character (integer/char) -- set that single entry
/// - a cons `(MIN . MAX)` -- set all characters MIN..=MAX
/// - `nil` -- set the default value
/// - `t` -- set all character entries (without changing the default slot)
pub(crate) fn builtin_set_char_table_range(args: Vec<Value>) -> EvalResult {
    expect_args("set-char-table-range", &args, 3)?;
    let table = &args[0];
    let range = &args[1];
    let value = &args[2];

    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };

    let mut vec = with_heap(|h| h.get_vector(*arc).clone());

    match range {
        // nil -> set default
        Value::Nil => {
            vec[CT_DEFAULT] = value.clone();
        }
        // t -> set all characters (but keep default slot unchanged).
        Value::True => {
            ct_set_char(&mut vec, CT_ALL_CHARS_SENTINEL, value.clone());
        }
        // Single character
        Value::Int(_) | Value::Char(_) => {
            let ch = expect_int(range)?;
            ct_set_char(&mut vec, ch, value.clone());
        }
        // Range cons (MIN . MAX)
        Value::Cons(cell) => {
            let pair = read_cons(*cell);
            let min = expect_int(&pair.car)?;
            let max = expect_int(&pair.cdr)?;
            drop(pair);
            if min > max {
                return Err(signal(
                    "args-out-of-range",
                    vec![Value::Int(min), Value::Int(max)],
                ));
            }
            for ch in min..=max {
                ct_set_char(&mut vec, ch, value.clone());
            }
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("char-table-range"), range.clone()],
            ));
        }
    }

    with_heap_mut(|h| *h.get_vector_mut(*arc) = vec);

    Ok(value.clone())
}

/// Set a single character entry in the char-table's data pairs.
fn ct_set_char(vec: &mut Vec<Value>, ch: i64, value: Value) {
    let start = ct_data_start(vec);
    // Search for an existing entry.
    let mut i = start;
    while i + 1 < vec.len() {
        if let Value::Int(existing) = &vec[i] {
            if *existing == ch {
                vec[i + 1] = value;
                return;
            }
        }
        i += 2;
    }
    // Not found — append a new pair.
    vec.push(Value::Int(ch));
    vec.push(value);
}

/// Look up a single character in the data pairs (no parent fallback).
fn ct_get_char(vec: &[Value], ch: i64) -> Option<Value> {
    let start = ct_data_start(vec);
    let mut i = start;
    while i + 1 < vec.len() {
        if let Value::Int(existing) = &vec[i] {
            if *existing == ch {
                return Some(vec[i + 1].clone());
            }
        }
        i += 2;
    }
    None
}

/// `(char-table-range CHAR-TABLE RANGE)` -- look up a value.
///
/// RANGE may be:
/// - a character -- look up that character (with parent fallback)
/// - `nil` -- return the default value
pub(crate) fn builtin_char_table_range(args: Vec<Value>) -> EvalResult {
    expect_args("char-table-range", &args, 2)?;
    let table = &args[0];
    let range = &args[1];

    if !is_char_table(table) {
        return Err(wrong_type("char-table-p", table));
    }

    match range {
        Value::Nil => {
            // Return the default value.
            let arc = match table {
                Value::Vector(a) => a,
                _ => unreachable!(),
            };
            let vec = with_heap(|h| h.get_vector(*arc).clone());
            Ok(vec[CT_DEFAULT].clone())
        }
        Value::True => Err(signal(
            "error",
            vec![Value::string(
                "Invalid RANGE argument to `char-table-range'",
            )],
        )),
        Value::Int(_) | Value::Char(_) => {
            let ch = expect_int(range)?;
            ct_lookup(table, ch)
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-table-range"), range.clone()],
        )),
    }
}

/// Recursive char-table lookup: check own entries, then all-char wildcard,
/// then char fallback/default, then parent chain.
fn ct_lookup(table: &Value, ch: i64) -> EvalResult {
    let arc = match table {
        Value::Vector(a) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());

    if let Some(val) = ct_get_char(&vec, ch) {
        return Ok(val);
    }
    if let Some(val) = ct_get_char(&vec, CT_ALL_CHARS_SENTINEL) {
        return Ok(val);
    }
    if let Some(val) = ct_get_char(&vec, CT_BASE_FALLBACK_SENTINEL) {
        if !val.is_nil() {
            return Ok(val);
        }
    }

    let parent = vec[CT_PARENT].clone();
    let default = vec[CT_DEFAULT].clone();

    if !default.is_nil() {
        Ok(default)
    } else if is_char_table(&parent) {
        ct_lookup(&parent, ch)
    } else {
        Ok(Value::Nil)
    }
}

/// `(char-table-parent CHAR-TABLE)` -- return the parent table (or nil).
pub(crate) fn builtin_char_table_parent(args: Vec<Value>) -> EvalResult {
    expect_args("char-table-parent", &args, 1)?;
    let table = &args[0];
    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());
    Ok(vec[CT_PARENT].clone())
}

/// `(set-char-table-parent CHAR-TABLE PARENT)` -- set the parent table.
pub(crate) fn builtin_set_char_table_parent(args: Vec<Value>) -> EvalResult {
    expect_args("set-char-table-parent", &args, 2)?;
    let table = &args[0];
    let parent = &args[1];

    // parent must be nil or a char-table.
    if !parent.is_nil() && !is_char_table(parent) {
        return Err(wrong_type("char-table-p", parent));
    }

    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    with_heap_mut(|h| h.vector_set(*arc, CT_PARENT, parent.clone()));
    Ok(parent.clone())
}

/// `(map-char-table FUNCTION CHAR-TABLE)` -- call FUNCTION for each
/// explicitly set entry.  FUNCTION receives `(CHAR VALUE)`.
/// Returns nil.
pub(crate) fn builtin_map_char_table(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("map-char-table", &args, 2)?;
    let func = args[0].clone();
    let table = &args[1];

    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };

    // Collect entries (char, value) from a snapshot, then iterate so the
    // callback can modify the table.
    let entries: Vec<(i64, Value)> = {
        let vec = with_heap(|h| h.get_vector(*arc).clone());
        let start = ct_data_start(&vec);
        let mut result = Vec::new();
        let mut i = start;
        while i + 1 < vec.len() {
            if let Value::Int(ch) = &vec[i] {
                result.push((*ch, vec[i + 1].clone()));
            }
            i += 2;
        }
        result
    };

    for (ch, val) in entries {
        eval.apply(func.clone(), vec![Value::Int(ch), val])?;
    }
    Ok(Value::Nil)
}

/// `(char-table-extra-slot TABLE N)` -- get extra slot N (0-based).
pub(crate) fn builtin_char_table_extra_slot(args: Vec<Value>) -> EvalResult {
    expect_args("char-table-extra-slot", &args, 2)?;
    let table = &args[0];
    let n = expect_int(&args[1])?;

    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    let v = with_heap(|h| h.get_vector(*arc).clone());
    let extra_count = match &v[CT_EXTRA_COUNT] {
        Value::Int(c) => *c,
        _ => 0,
    };

    if n < 0 || n >= extra_count {
        return Err(signal(
            "args-out-of-range",
            vec![args[1].clone(), Value::Int(extra_count)],
        ));
    }

    Ok(v[CT_EXTRA_START + n as usize].clone())
}

/// `(set-char-table-extra-slot TABLE N VALUE)` -- set extra slot N.
pub(crate) fn builtin_set_char_table_extra_slot(args: Vec<Value>) -> EvalResult {
    expect_args("set-char-table-extra-slot", &args, 3)?;
    let table = &args[0];
    let n = expect_int(&args[1])?;
    let value = &args[2];

    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    let mut v = with_heap(|h| h.get_vector(*arc).clone());
    let extra_count = match &v[CT_EXTRA_COUNT] {
        Value::Int(c) => *c,
        _ => 0,
    };

    if n < 0 || n >= extra_count {
        // Grow extra slots if needed (Emacs allows up to 10).
        let needed = (n + 1) as usize;
        if needed > 10 {
            return Err(signal(
                "args-out-of-range",
                vec![args[1].clone(), Value::Int(10)],
            ));
        }
        // We need to shift data pairs to make room for new extra slots.
        let old_start = ct_data_start(&v);
        let old_extra = extra_count as usize;
        let new_extra = needed;
        let grow = new_extra - old_extra;
        // Insert `grow` nil slots at position CT_EXTRA_START + old_extra.
        let insert_pos = CT_EXTRA_START + old_extra;
        for _ in 0..grow {
            v.insert(insert_pos, Value::Nil);
        }
        v[CT_EXTRA_COUNT] = Value::Int(new_extra as i64);
        // old_start is now shifted by `grow`.
        let _ = old_start;
    }

    v[CT_EXTRA_START + n as usize] = value.clone();
    with_heap_mut(|h| *h.get_vector_mut(*arc) = v);
    Ok(value.clone())
}

/// `(char-table-subtype TABLE)` -- return the sub-type symbol.
pub(crate) fn builtin_char_table_subtype(args: Vec<Value>) -> EvalResult {
    expect_args("char-table-subtype", &args, 1)?;
    let table = &args[0];
    let arc = match table {
        Value::Vector(a) if is_char_table(table) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());
    Ok(vec[CT_SUBTYPE].clone())
}

// ---------------------------------------------------------------------------
// Bool-vector builtins
// ---------------------------------------------------------------------------

/// `(make-bool-vector LENGTH INIT)` -- create a bool vector of LENGTH bits,
/// each initialized to INIT (nil or non-nil).
pub(crate) fn builtin_make_bool_vector(args: Vec<Value>) -> EvalResult {
    expect_args("make-bool-vector", &args, 2)?;
    let length = expect_int(&args[0])?;
    if length < 0 {
        return Err(signal("args-out-of-range", vec![args[0].clone()]));
    }
    let init_val = if args[1].is_truthy() {
        Value::Int(1)
    } else {
        Value::Int(0)
    };
    let len = length as usize;
    let mut vec = Vec::with_capacity(2 + len);
    vec.push(Value::symbol(BOOL_VECTOR_TAG));
    vec.push(Value::Int(length));
    for _ in 0..len {
        vec.push(init_val.clone());
    }
    Ok(Value::vector(vec))
}

/// `(bool-vector &rest OBJECTS)` -- create a bool-vector from OBJECTS
/// truthiness.
pub(crate) fn builtin_bool_vector(args: Vec<Value>) -> EvalResult {
    let bits: Vec<bool> = args.into_iter().map(|v| v.is_truthy()).collect();
    Ok(bv_from_bits(&bits))
}

/// `(bool-vector-p OBJ)` -- return t if OBJ is a bool-vector.
pub(crate) fn builtin_bool_vector_p(args: Vec<Value>) -> EvalResult {
    expect_args("bool-vector-p", &args, 1)?;
    Ok(Value::bool(is_bool_vector(&args[0])))
}

/// Helper: extract a bool-vector's length.
fn bv_length(vec: &[Value]) -> i64 {
    match &vec[BV_SIZE] {
        Value::Int(n) => *n,
        _ => 0,
    }
}

/// Helper: extract the bits of a bool-vector as a `Vec<bool>`.
fn bv_bits(vec: &[Value]) -> Vec<bool> {
    let len = bv_length(vec) as usize;
    let mut bits = Vec::with_capacity(len);
    for i in 0..len {
        let v = &vec[2 + i];
        bits.push(matches!(v, Value::Int(n) if *n != 0));
    }
    bits
}

/// `(bool-vector-count-population BV)` -- count the number of true values.
pub(crate) fn builtin_bool_vector_count_population(args: Vec<Value>) -> EvalResult {
    expect_args("bool-vector-count-population", &args, 1)?;
    let (bits, _len) = extract_bv_bits(&args[0])?;
    let count = bits.iter().filter(|&&b| b).count();
    Ok(Value::Int(count as i64))
}

fn extract_bv_bits(value: &Value) -> Result<(Vec<bool>, i64), Flow> {
    let arc = match value {
        Value::Vector(arc) if is_bool_vector(value) => arc,
        _ => return Err(wrong_type("bool-vector-p", value)),
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());
    let len = bv_length(&vec);
    let bits = bv_bits(&vec);
    Ok((bits, len))
}

/// Build a bool-vector `Value` from a slice of bools.
fn bv_from_bits(bits: &[bool]) -> Value {
    let len = bits.len();
    let mut vec = Vec::with_capacity(2 + len);
    vec.push(Value::symbol(BOOL_VECTOR_TAG));
    vec.push(Value::Int(len as i64));
    for &b in bits {
        vec.push(Value::Int(if b { 1 } else { 0 }));
    }
    Value::vector(vec)
}

/// `(bool-vector-intersection A B &optional C)` -- bitwise AND.
/// If C is provided, store result in C and return C; otherwise return a new
/// bool-vector.
pub(crate) fn builtin_bool_vector_intersection(args: Vec<Value>) -> EvalResult {
    expect_min_args("bool-vector-intersection", &args, 2)?;
    expect_max_args("bool-vector-intersection", &args, 3)?;
    let (bits_a, len_a) = extract_bv_bits(&args[0])?;
    let (bits_b, len_b) = extract_bv_bits(&args[1])?;
    if len_a != len_b {
        return Err(signal(
            "wrong-length-argument",
            vec![Value::Int(len_a), Value::Int(len_b)],
        ));
    }
    let result_bits: Vec<bool> = bits_a
        .iter()
        .zip(bits_b.iter())
        .map(|(&a, &b)| a && b)
        .collect();

    if args.len() == 3 {
        store_bv_result_with_expected_lengths(&args[2], &result_bits, &[len_a, len_b])?;
        Ok(args[2].clone())
    } else {
        Ok(bv_from_bits(&result_bits))
    }
}

/// `(bool-vector-union A B &optional C)` -- bitwise OR.
pub(crate) fn builtin_bool_vector_union(args: Vec<Value>) -> EvalResult {
    expect_min_args("bool-vector-union", &args, 2)?;
    expect_max_args("bool-vector-union", &args, 3)?;
    let (bits_a, len_a) = extract_bv_bits(&args[0])?;
    let (bits_b, len_b) = extract_bv_bits(&args[1])?;
    if len_a != len_b {
        return Err(signal(
            "wrong-length-argument",
            vec![Value::Int(len_a), Value::Int(len_b)],
        ));
    }
    let result_bits: Vec<bool> = bits_a
        .iter()
        .zip(bits_b.iter())
        .map(|(&a, &b)| a || b)
        .collect();

    if args.len() == 3 {
        store_bv_result_with_expected_lengths(&args[2], &result_bits, &[len_a, len_b])?;
        Ok(args[2].clone())
    } else {
        Ok(bv_from_bits(&result_bits))
    }
}

/// `(bool-vector-exclusive-or A B &optional C)` -- bitwise XOR.
pub(crate) fn builtin_bool_vector_exclusive_or(args: Vec<Value>) -> EvalResult {
    expect_min_args("bool-vector-exclusive-or", &args, 2)?;
    expect_max_args("bool-vector-exclusive-or", &args, 3)?;
    let (bits_a, len_a) = extract_bv_bits(&args[0])?;
    let (bits_b, len_b) = extract_bv_bits(&args[1])?;
    if len_a != len_b {
        return Err(signal(
            "wrong-length-argument",
            vec![Value::Int(len_a), Value::Int(len_b)],
        ));
    }
    let result_bits: Vec<bool> = bits_a
        .iter()
        .zip(bits_b.iter())
        .map(|(&a, &b)| a ^ b)
        .collect();

    if args.len() == 3 {
        store_bv_result_with_expected_lengths(&args[2], &result_bits, &[len_a, len_b])?;
        Ok(args[2].clone())
    } else {
        Ok(bv_from_bits(&result_bits))
    }
}

/// `(bool-vector-not A &optional B)` -- bitwise NOT.
///
/// If B is provided, store result in B and return B; otherwise return a new
/// bool-vector.
pub(crate) fn builtin_bool_vector_not(args: Vec<Value>) -> EvalResult {
    expect_min_args("bool-vector-not", &args, 1)?;
    expect_max_args("bool-vector-not", &args, 2)?;
    let (bits, len_a) = extract_bv_bits(&args[0])?;
    let result_bits: Vec<bool> = bits.into_iter().map(|b| !b).collect();
    if args.len() == 2 {
        store_bv_result_with_expected_lengths(&args[1], &result_bits, &[len_a])?;
        Ok(args[1].clone())
    } else {
        Ok(bv_from_bits(&result_bits))
    }
}

/// `(bool-vector-set-difference A B &optional C)` -- `A & (not B)`.
pub(crate) fn builtin_bool_vector_set_difference(args: Vec<Value>) -> EvalResult {
    expect_min_args("bool-vector-set-difference", &args, 2)?;
    expect_max_args("bool-vector-set-difference", &args, 3)?;
    let (bits_a, len_a) = extract_bv_bits(&args[0])?;
    let (bits_b, len_b) = extract_bv_bits(&args[1])?;
    if len_a != len_b {
        return Err(signal(
            "wrong-length-argument",
            vec![Value::Int(len_a), Value::Int(len_b)],
        ));
    }
    let result_bits: Vec<bool> = bits_a
        .iter()
        .zip(bits_b.iter())
        .map(|(&a, &b)| a && !b)
        .collect();
    if args.len() == 3 {
        store_bv_result_with_expected_lengths(&args[2], &result_bits, &[len_a, len_b])?;
        Ok(args[2].clone())
    } else {
        Ok(bv_from_bits(&result_bits))
    }
}

/// `(bool-vector-count-consecutive BV BOOL START)` -- count matching bits from
/// START until the first non-matching bit or the end.
pub(crate) fn builtin_bool_vector_count_consecutive(args: Vec<Value>) -> EvalResult {
    expect_args("bool-vector-count-consecutive", &args, 3)?;
    let (bits, len) = extract_bv_bits(&args[0])?;
    let target = args[1].is_truthy();
    let start = expect_wholenump(&args[2])?;
    if start > len {
        return Err(signal(
            "args-out-of-range",
            vec![args[0].clone(), Value::Int(start)],
        ));
    }
    let mut count = 0usize;
    for bit in bits.iter().skip(start as usize) {
        if *bit != target {
            break;
        }
        count += 1;
    }
    Ok(Value::Int(count as i64))
}

/// `(bool-vector-subsetp A B)` -- return t if every true bit in A is also true
/// in B.
pub(crate) fn builtin_bool_vector_subsetp(args: Vec<Value>) -> EvalResult {
    expect_args("bool-vector-subsetp", &args, 2)?;
    let (bits_a, len_a) = extract_bv_bits(&args[0])?;
    let (bits_b, len_b) = extract_bv_bits(&args[1])?;
    if len_a != len_b {
        return Err(signal(
            "wrong-length-argument",
            vec![Value::Int(len_a), Value::Int(len_b), Value::Int(len_b)],
        ));
    }
    let is_subset = bits_a.iter().zip(bits_b.iter()).all(|(&a, &b)| !a || b);
    Ok(Value::bool(is_subset))
}

/// Store bits into an existing bool-vector (for the optional dest argument).
fn store_bv_result_with_expected_lengths(
    dest: &Value,
    bits: &[bool],
    expected_lengths: &[i64],
) -> Result<(), Flow> {
    let arc = match dest {
        Value::Vector(a) if is_bool_vector(dest) => a,
        _ => return Err(wrong_type("bool-vector-p", dest)),
    };
    let mut v = with_heap(|h| h.get_vector(*arc).clone());
    let len = bv_length(&v) as usize;
    if len != bits.len() {
        let mut payload: Vec<Value> = expected_lengths.iter().copied().map(Value::Int).collect();
        payload.push(Value::Int(len as i64));
        return Err(signal("wrong-length-argument", payload));
    }
    for (i, &b) in bits.iter().enumerate() {
        v[2 + i] = Value::Int(if b { 1 } else { 0 });
    }
    with_heap_mut(|h| *h.get_vector_mut(*arc) = v);
    Ok(())
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // Char-table tests
    // -----------------------------------------------------------------------

    #[test]
    fn make_char_table_basic() {
        let ct = builtin_make_char_table(vec![Value::symbol("syntax-table")]).unwrap();
        assert!(is_char_table(&ct));
        assert!(!is_bool_vector(&ct));
    }

    #[test]
    fn make_char_table_with_default() {
        let ct =
            builtin_make_char_table(vec![Value::symbol("syntax-table"), Value::Int(42)]).unwrap();
        assert!(is_char_table(&ct));
        // Default lookup should return the default.
        let def = builtin_char_table_range(vec![ct.clone(), Value::Nil]).unwrap();
        assert!(matches!(def, Value::Int(42)));
    }

    #[test]
    fn char_table_p_predicate() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        assert!(matches!(
            builtin_char_table_p(vec![ct]).unwrap(),
            Value::True
        ));
        assert!(matches!(
            builtin_char_table_p(vec![Value::Int(5)]).unwrap(),
            Value::Nil
        ));
        assert!(matches!(
            builtin_char_table_p(vec![Value::Nil]).unwrap(),
            Value::Nil
        ));
    }

    #[test]
    fn set_and_get_single_char() {
        let ct = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::Int(65), Value::symbol("letter-a")])
            .unwrap();
        let val = builtin_char_table_range(vec![ct.clone(), Value::Int(65)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="letter-a"));
    }

    #[test]
    fn lookup_falls_back_to_default() {
        let ct = builtin_make_char_table(vec![Value::symbol("test"), Value::symbol("default-val")])
            .unwrap();
        // No entry for char 90.
        let val = builtin_char_table_range(vec![ct.clone(), Value::Int(90)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="default-val"));
    }

    #[test]
    fn set_range_cons() {
        let ct = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        // Set chars 65..=67 (A, B, C)
        let range = Value::cons(Value::Int(65), Value::Int(67));
        builtin_set_char_table_range(vec![ct.clone(), range, Value::symbol("abc")]).unwrap();
        for ch in 65..=67 {
            let val = builtin_char_table_range(vec![ct.clone(), Value::Int(ch)]).unwrap();
            assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="abc"));
        }
        // Char 68 should be nil (default).
        let val = builtin_char_table_range(vec![ct.clone(), Value::Int(68)]).unwrap();
        assert!(val.is_nil());
    }

    #[test]
    fn set_default_via_range_nil() {
        let ct = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::Nil, Value::Int(999)]).unwrap();
        let def = builtin_char_table_range(vec![ct.clone(), Value::Nil]).unwrap();
        assert!(matches!(def, Value::Int(999)));
    }

    #[test]
    fn set_range_t_sets_all_chars_without_changing_default() {
        let ct = builtin_make_char_table(vec![Value::symbol("test"), Value::Int(0)]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::True, Value::Int(5)]).unwrap();

        let a = builtin_char_table_range(vec![ct.clone(), Value::Int('a' as i64)]).unwrap();
        let b = builtin_char_table_range(vec![ct.clone(), Value::Int('b' as i64)]).unwrap();
        let def = builtin_char_table_range(vec![ct.clone(), Value::Nil]).unwrap();
        assert!(matches!(a, Value::Int(5)));
        assert!(matches!(b, Value::Int(5)));
        assert!(matches!(def, Value::Int(0)));
    }

    #[test]
    fn set_range_t_wildcard_allows_single_char_override() {
        let ct = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::True, Value::Int(5)]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::Int('a' as i64), Value::Int(9)])
            .unwrap();

        let a = builtin_char_table_range(vec![ct.clone(), Value::Int('a' as i64)]).unwrap();
        let b = builtin_char_table_range(vec![ct.clone(), Value::Int('b' as i64)]).unwrap();
        let def = builtin_char_table_range(vec![ct.clone(), Value::Nil]).unwrap();
        assert!(matches!(a, Value::Int(9)));
        assert!(matches!(b, Value::Int(5)));
        assert!(def.is_nil());
    }

    #[test]
    fn parent_chain_lookup() {
        let parent = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        builtin_set_char_table_range(vec![
            parent.clone(),
            Value::Int(65),
            Value::symbol("from-parent"),
        ])
        .unwrap();
        let child = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        builtin_set_char_table_parent(vec![child.clone(), parent.clone()]).unwrap();

        // Lookup in child falls through to parent.
        let val = builtin_char_table_range(vec![child.clone(), Value::Int(65)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="from-parent"));

        // Child override takes priority.
        builtin_set_char_table_range(vec![
            child.clone(),
            Value::Int(65),
            Value::symbol("child-val"),
        ])
        .unwrap();
        let val = builtin_char_table_range(vec![child.clone(), Value::Int(65)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="child-val"));
    }

    #[test]
    fn char_table_parent_get_set() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        // Initially nil.
        let p = builtin_char_table_parent(vec![ct.clone()]).unwrap();
        assert!(p.is_nil());

        let parent = builtin_make_char_table(vec![Value::symbol("parent")]).unwrap();
        builtin_set_char_table_parent(vec![ct.clone(), parent.clone()]).unwrap();
        let p = builtin_char_table_parent(vec![ct.clone()]).unwrap();
        assert!(is_char_table(&p));
    }

    #[test]
    fn set_char_table_parent_nil() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        let parent = builtin_make_char_table(vec![Value::symbol("parent")]).unwrap();
        builtin_set_char_table_parent(vec![ct.clone(), parent.clone()]).unwrap();
        builtin_set_char_table_parent(vec![ct.clone(), Value::Nil]).unwrap();
        let p = builtin_char_table_parent(vec![ct.clone()]).unwrap();
        assert!(p.is_nil());
    }

    #[test]
    fn set_char_table_parent_wrong_type() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        let result = builtin_set_char_table_parent(vec![ct.clone(), Value::Int(5)]);
        assert!(result.is_err());
    }

    #[test]
    fn char_table_extra_slot_basic() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        // Initially 0 extra slots -- should error.
        let result = builtin_char_table_extra_slot(vec![ct.clone(), Value::Int(0)]);
        assert!(result.is_err());

        // Set extra slot 0 -- grows the table.
        builtin_set_char_table_extra_slot(vec![ct.clone(), Value::Int(0), Value::symbol("extra0")])
            .unwrap();
        let val = builtin_char_table_extra_slot(vec![ct.clone(), Value::Int(0)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="extra0"));
    }

    #[test]
    fn char_table_extra_slot_preserves_data() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        // Set a char entry first.
        builtin_set_char_table_range(vec![ct.clone(), Value::Int(65), Value::symbol("a-val")])
            .unwrap();
        // Now grow extra slots.
        builtin_set_char_table_extra_slot(vec![ct.clone(), Value::Int(0), Value::symbol("e0")])
            .unwrap();
        // The char entry should still be intact.
        let val = builtin_char_table_range(vec![ct.clone(), Value::Int(65)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="a-val"));
        // Extra slot should be readable.
        let es = builtin_char_table_extra_slot(vec![ct.clone(), Value::Int(0)]).unwrap();
        assert!(matches!(es, Value::Symbol(ref id) if resolve_sym(*id) =="e0"));
    }

    #[test]
    fn char_table_subtype() {
        let ct = builtin_make_char_table(vec![Value::symbol("syntax-table")]).unwrap();
        let st = builtin_char_table_subtype(vec![ct]).unwrap();
        assert!(matches!(st, Value::Symbol(ref id) if resolve_sym(*id) =="syntax-table"));
    }

    #[test]
    fn char_table_overwrite_entry() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::Int(65), Value::Int(1)]).unwrap();
        builtin_set_char_table_range(vec![ct.clone(), Value::Int(65), Value::Int(2)]).unwrap();
        let val = builtin_char_table_range(vec![ct.clone(), Value::Int(65)]).unwrap();
        assert!(matches!(val, Value::Int(2)));
    }

    #[test]
    fn char_table_p_on_plain_vector() {
        // A plain vector should not be detected as a char-table.
        let v = Value::vector(vec![Value::Int(1), Value::Int(2)]);
        assert!(!is_char_table(&v));
    }

    #[test]
    fn char_table_wrong_type_signals() {
        let result = builtin_char_table_range(vec![Value::Int(5), Value::Int(65)]);
        assert!(result.is_err());
        let result = builtin_set_char_table_range(vec![Value::Nil, Value::Int(65), Value::Int(1)]);
        assert!(result.is_err());
        let result = builtin_char_table_parent(vec![Value::string("not-a-table")]);
        assert!(result.is_err());
    }

    #[test]
    fn char_table_wrong_arg_count() {
        assert!(builtin_make_char_table(vec![]).is_err());
        assert!(builtin_char_table_p(vec![]).is_err());
        assert!(builtin_char_table_range(vec![Value::Nil]).is_err());
        assert!(builtin_set_char_table_range(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn char_table_char_key() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        // Use Value::Char for setting.
        builtin_set_char_table_range(vec![ct.clone(), Value::Char('Z'), Value::symbol("zee")])
            .unwrap();
        // Look up with Int.
        let val = builtin_char_table_range(vec![ct.clone(), Value::Int('Z' as i64)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="zee"));
    }

    #[test]
    fn parent_default_fallback() {
        // Parent has default but no explicit entry.
        let parent =
            builtin_make_char_table(vec![Value::symbol("test"), Value::symbol("parent-default")])
                .unwrap();
        let child = builtin_make_char_table(vec![Value::symbol("test"), Value::Nil]).unwrap();
        builtin_set_char_table_parent(vec![child.clone(), parent.clone()]).unwrap();

        // Child has no entry, parent has no entry, parent default is used.
        let val = builtin_char_table_range(vec![child.clone(), Value::Int(100)]).unwrap();
        assert!(matches!(val, Value::Symbol(ref id) if resolve_sym(*id) =="parent-default"));
    }

    #[test]
    fn non_nil_child_default_overrides_parent_lookup() {
        let parent = builtin_make_char_table(vec![Value::symbol("test"), Value::Int(8)]).unwrap();
        let child = builtin_make_char_table(vec![Value::symbol("test"), Value::Int(0)]).unwrap();
        builtin_set_char_table_parent(vec![child.clone(), parent]).unwrap();

        let val = builtin_char_table_range(vec![child, Value::Int('a' as i64)]).unwrap();
        assert!(matches!(val, Value::Int(0)));
    }

    // -----------------------------------------------------------------------
    // Bool-vector tests
    // -----------------------------------------------------------------------

    #[test]
    fn make_bool_vector_basic() {
        let bv = builtin_make_bool_vector(vec![Value::Int(5), Value::Nil]).unwrap();
        assert!(is_bool_vector(&bv));
        assert!(!is_char_table(&bv));
    }

    #[test]
    fn bool_vector_constructor_from_rest_args() {
        let bv = builtin_bool_vector(vec![
            Value::True,
            Value::Nil,
            Value::Int(0),
            Value::symbol("x"),
        ])
        .unwrap();
        assert!(is_bool_vector(&bv));
        assert_bv_bits(&bv, &[true, false, true, true]);

        let empty = builtin_bool_vector(vec![]).unwrap();
        assert!(is_bool_vector(&empty));
        assert_bv_bits(&empty, &[]);
    }

    #[test]
    fn make_bool_vector_all_true() {
        let bv = builtin_make_bool_vector(vec![Value::Int(4), Value::True]).unwrap();
        let count = builtin_bool_vector_count_population(vec![bv]).unwrap();
        assert!(matches!(count, Value::Int(4)));
    }

    #[test]
    fn make_bool_vector_all_false() {
        let bv = builtin_make_bool_vector(vec![Value::Int(4), Value::Nil]).unwrap();
        let count = builtin_bool_vector_count_population(vec![bv]).unwrap();
        assert!(matches!(count, Value::Int(0)));
    }

    #[test]
    fn bool_vector_p_predicate() {
        let bv = builtin_make_bool_vector(vec![Value::Int(3), Value::Nil]).unwrap();
        assert!(matches!(
            builtin_bool_vector_p(vec![bv]).unwrap(),
            Value::True
        ));
        assert!(matches!(
            builtin_bool_vector_p(vec![Value::Int(0)]).unwrap(),
            Value::Nil
        ));
    }

    #[test]
    fn bool_vector_intersection() {
        // a = [1, 1, 0, 0], b = [1, 0, 1, 0] -> AND = [1, 0, 0, 0]
        let a = make_bv(&[true, true, false, false]);
        let b = make_bv(&[true, false, true, false]);
        let result = builtin_bool_vector_intersection(vec![a, b]).unwrap();
        assert_bv_bits(&result, &[true, false, false, false]);
    }

    #[test]
    fn bool_vector_union() {
        let a = make_bv(&[true, true, false, false]);
        let b = make_bv(&[true, false, true, false]);
        let result = builtin_bool_vector_union(vec![a, b]).unwrap();
        assert_bv_bits(&result, &[true, true, true, false]);
    }

    #[test]
    fn bool_vector_exclusive_or() {
        let a = make_bv(&[true, true, false, false]);
        let b = make_bv(&[true, false, true, false]);
        let result = builtin_bool_vector_exclusive_or(vec![a, b]).unwrap();
        assert_bv_bits(&result, &[false, true, true, false]);
    }

    #[test]
    fn bool_vector_not() {
        let a = make_bv(&[true, false, true, false]);
        let result = builtin_bool_vector_not(vec![a]).unwrap();
        assert_bv_bits(&result, &[false, true, false, true]);
    }

    #[test]
    fn bool_vector_not_into_dest() {
        let a = make_bv(&[false, false, true]);
        let dest = make_bv(&[false, false, false]);
        let result = builtin_bool_vector_not(vec![a, dest.clone()]).unwrap();
        assert_eq!(result, dest);
        assert_bv_bits(&dest, &[true, true, false]);
    }

    #[test]
    fn bool_vector_set_difference() {
        let a = make_bv(&[true, true, false, true]);
        let b = make_bv(&[false, true, true, false]);
        let result = builtin_bool_vector_set_difference(vec![a, b]).unwrap();
        assert_bv_bits(&result, &[true, false, false, true]);
    }

    #[test]
    fn bool_vector_count_consecutive() {
        let bv = make_bv(&[true, true, false, false, true, true]);
        let count_true_start =
            builtin_bool_vector_count_consecutive(vec![bv.clone(), Value::True, Value::Int(0)])
                .unwrap();
        let count_false_middle =
            builtin_bool_vector_count_consecutive(vec![bv.clone(), Value::Nil, Value::Int(2)])
                .unwrap();
        let count_true_mismatch =
            builtin_bool_vector_count_consecutive(vec![bv.clone(), Value::True, Value::Int(2)])
                .unwrap();
        assert!(matches!(count_true_start, Value::Int(2)));
        assert!(matches!(count_false_middle, Value::Int(2)));
        assert!(matches!(count_true_mismatch, Value::Int(0)));
    }

    #[test]
    fn bool_vector_subsetp_true() {
        let a = make_bv(&[true, false, false]);
        let b = make_bv(&[true, true, false]);
        let result = builtin_bool_vector_subsetp(vec![a, b]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn bool_vector_subsetp_false() {
        let a = make_bv(&[true, false, true]);
        let b = make_bv(&[true, true, false]);
        let result = builtin_bool_vector_subsetp(vec![a, b]).unwrap();
        assert!(matches!(result, Value::Nil));
    }

    #[test]
    fn bool_vector_count_population_mixed() {
        let bv = make_bv(&[true, false, true, true, false]);
        let count = builtin_bool_vector_count_population(vec![bv]).unwrap();
        assert!(matches!(count, Value::Int(3)));
    }

    #[test]
    fn bool_vector_empty() {
        let bv = builtin_make_bool_vector(vec![Value::Int(0), Value::Nil]).unwrap();
        assert!(is_bool_vector(&bv));
        let count = builtin_bool_vector_count_population(vec![bv]).unwrap();
        assert!(matches!(count, Value::Int(0)));
    }

    #[test]
    fn bool_vector_negative_length() {
        let result = builtin_make_bool_vector(vec![Value::Int(-1), Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn bool_vector_wrong_type_signals() {
        let result = builtin_bool_vector_count_population(vec![Value::Int(0)]);
        assert!(result.is_err());
    }

    #[test]
    fn bool_vector_mismatched_length() {
        let a = make_bv(&[true, false]);
        let b = make_bv(&[true]);
        let result = builtin_bool_vector_intersection(vec![a, b]);
        assert!(result.is_err());
    }

    #[test]
    fn bool_vector_intersection_into_dest() {
        let a = make_bv(&[true, true, false]);
        let b = make_bv(&[false, true, true]);
        let dest = make_bv(&[false, false, false]);
        let result = builtin_bool_vector_intersection(vec![a, b, dest.clone()]).unwrap();
        // Result should be the same object as dest.
        assert_bv_bits(&result, &[false, true, false]);
        // Dest should have been mutated.
        assert_bv_bits(&dest, &[false, true, false]);
    }

    #[test]
    fn bool_vector_union_into_dest() {
        let a = make_bv(&[true, false, false]);
        let b = make_bv(&[false, true, false]);
        let dest = make_bv(&[false, false, false]);
        builtin_bool_vector_union(vec![a, b, dest.clone()]).unwrap();
        assert_bv_bits(&dest, &[true, true, false]);
    }

    #[test]
    fn is_predicates_disjoint() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        let bv = builtin_make_bool_vector(vec![Value::Int(3), Value::Nil]).unwrap();
        let v = Value::vector(vec![Value::Int(1)]);
        assert!(is_char_table(&ct));
        assert!(!is_bool_vector(&ct));
        assert!(!is_char_table(&bv));
        assert!(is_bool_vector(&bv));
        assert!(!is_char_table(&v));
        assert!(!is_bool_vector(&v));
    }

    #[test]
    fn bool_vector_wrong_arg_count() {
        assert!(builtin_make_bool_vector(vec![]).is_err());
        assert!(builtin_bool_vector_p(vec![]).is_err());
        assert!(builtin_bool_vector_subsetp(vec![Value::Nil]).is_err());
        assert!(builtin_bool_vector_not(vec![]).is_err());
        assert!(builtin_bool_vector_not(vec![Value::Nil, Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn char_table_range_invalid_range_type() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        let result =
            builtin_set_char_table_range(vec![ct.clone(), Value::string("invalid"), Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn char_table_range_reverse_cons_errors() {
        let ct = builtin_make_char_table(vec![Value::symbol("test")]).unwrap();
        let range = Value::cons(Value::Int(70), Value::Int(65)); // min > max
        let result = builtin_set_char_table_range(vec![ct, range, Value::Int(1)]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // Test helpers
    // -----------------------------------------------------------------------

    /// Build a bool-vector from a slice of bools (test helper).
    fn make_bv(bits: &[bool]) -> Value {
        bv_from_bits(bits)
    }

    /// Assert that a bool-vector has the expected bits.
    fn assert_bv_bits(bv: &Value, expected: &[bool]) {
        let arc = match bv {
            Value::Vector(a) => a,
            _ => panic!("expected a vector"),
        };
        let vec = with_heap(|h| h.get_vector(*arc).clone());
        let len = bv_length(&vec) as usize;
        assert_eq!(len, expected.len(), "bool-vector length mismatch");
        let bits = bv_bits(&vec);
        assert_eq!(bits, expected);
    }
}
