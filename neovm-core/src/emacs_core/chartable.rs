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

use super::error::{EvalResult, Flow, signal};
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
const CT_LOGICAL_LENGTH: i64 = 0x3F_FFFF;
/// Maximum valid Unicode code point.
const MAX_CHAR: i64 = 0x3F_FFFF;

// Bool-vector fixed-layout indices:
const BV_SIZE: usize = 1; // Value::Int — logical length

// ---------------------------------------------------------------------------
// Predicates
// ---------------------------------------------------------------------------

/// Return `true` if `v` is a char-table (tagged vector).
pub fn is_char_table(v: &Value) -> bool {
    if let Value::Vector(arc) = v {
        let vec = with_heap(|h| h.get_vector(*arc).clone());
        vec.len() >= CT_EXTRA_START
            && matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == CHAR_TABLE_TAG)
    } else {
        false
    }
}

/// Return `true` if `v` is a bool-vector (tagged vector).
pub fn is_bool_vector(v: &Value) -> bool {
    if let Value::Vector(arc) = v {
        let vec = with_heap(|h| h.get_vector(*arc).clone());
        vec.len() >= 2
            && matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == BOOL_VECTOR_TAG)
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
    if vec.len() < 2 || !matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == BOOL_VECTOR_TAG)
    {
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
    if vec.len() >= CT_EXTRA_START
        && matches!(&vec[0], Value::Symbol(id) if resolve_sym(*id) == CHAR_TABLE_TAG)
    {
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
    signal("wrong-type-argument", vec![Value::symbol(pred), *got])
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
                vec![Value::symbol("wholenump"), *value],
            ));
        }
    };
    if n < 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), *value],
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

/// Create a char-table `Value` directly (for use in bootstrap code).
pub fn make_char_table_value(sub_type: Value, default: Value) -> Value {
    make_char_table_with_extra_slots(sub_type, default, 0)
}

/// Create a char-table with a specified number of extra slots.
pub fn make_char_table_with_extra_slots(sub_type: Value, default: Value, n_extras: i64) -> Value {
    let mut vec = vec![
        Value::symbol(CHAR_TABLE_TAG),
        default,              // CT_DEFAULT
        Value::Nil,           // CT_PARENT
        sub_type,             // CT_SUBTYPE
        Value::Int(n_extras), // CT_EXTRA_COUNT
    ];
    // Allocate extra slots initialised to nil.
    for _ in 0..n_extras {
        vec.push(Value::Nil);
    }
    Value::vector(vec)
}

/// Set a single character entry in a char-table Value (for bootstrap code).
/// Panics if `table` is not a char-table Vector.
pub fn ct_set_single(table: &Value, ch: i64, value: Value) {
    if let Value::Vector(arc) = table {
        with_heap_mut(|h| {
            let vec = h.get_vector_mut(*arc);
            ct_set_char(vec, ch, value);
        });
    } else {
        panic!("ct_set_single: expected char-table Vector");
    }
}

/// `(make-char-table SUB-TYPE &optional DEFAULT)` -- create a char-table.
///
/// If SUB-TYPE has a `char-table-extra-slots` property, its value
/// specifies how many extra slots the char-table has (0..10).
pub(crate) fn builtin_make_char_table(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("make-char-table", &args, 1)?;
    expect_max_args("make-char-table", &args, 2)?;
    let sub_type = args[0];
    let default = if args.len() > 1 { args[1] } else { Value::Nil };
    // Read char-table-extra-slots property from the sub-type symbol,
    // matching GNU Emacs chartab.c:Fmake_char_table.
    let n_extras = if let Some(name) = sub_type.as_symbol_name() {
        eval.obarray()
            .get_property(name, "char-table-extra-slots")
            .and_then(|v| v.as_int())
            .unwrap_or(0)
    } else {
        0
    };
    Ok(make_char_table_with_extra_slots(
        sub_type, default, n_extras,
    ))
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
/// - `t` -- set the default value for all characters
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
            vec[CT_DEFAULT] = *value;
        }
        // t -> set the default value (same as nil in GNU Emacs).
        Value::True => {
            vec[CT_DEFAULT] = *value;
        }
        // Single character
        Value::Int(_) | Value::Char(_) => {
            let ch = expect_int(range)?;
            ct_set_char(&mut vec, ch, *value);
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
            ct_set_range(&mut vec, min, max, *value);
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("char-table-range"), *range],
            ));
        }
    }

    with_heap_mut(|h| *h.get_vector_mut(*arc) = vec);

    Ok(*value)
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

/// Set a range entry in the char-table's data pairs.
/// The range is stored as a `Cons(min . max)` key.
fn ct_set_range(vec: &mut Vec<Value>, min: i64, max: i64, value: Value) {
    let start = ct_data_start(vec);
    // Search for an existing range entry with the same bounds.
    let mut i = start;
    while i + 1 < vec.len() {
        if let Value::Cons(cell) = &vec[i] {
            let pair = read_cons(*cell);
            if matches!((&pair.car, &pair.cdr), (Value::Int(m1), Value::Int(m2)) if *m1 == min && *m2 == max)
            {
                vec[i + 1] = value;
                return;
            }
        }
        i += 2;
    }
    // Not found — append a new range entry.
    vec.push(Value::cons(Value::Int(min), Value::Int(max)));
    vec.push(value);
}

/// Look up a single character in the data pairs (no parent/default fallback).
/// The last assignment that covers the character wins, matching GNU Emacs
/// `set-char-table-range` overwrite semantics for both single-char and range
/// entries.
fn ct_get_char(vec: &[Value], ch: i64) -> Option<Value> {
    let start = ct_data_start(vec);
    let mut i = start;
    let mut match_value: Option<Value> = None;
    while i + 1 < vec.len() {
        match &vec[i] {
            Value::Int(existing) => {
                if *existing == ch {
                    match_value = Some(vec[i + 1]);
                }
            }
            Value::Cons(cell) => {
                // Range entry: key is (MIN . MAX)
                let pair = read_cons(*cell);
                if let (Value::Int(min), Value::Int(max)) = (&pair.car, &pair.cdr) {
                    if ch >= *min && ch <= *max {
                        match_value = Some(vec[i + 1]);
                    }
                }
            }
            _ => {}
        }
        i += 2;
    }
    match_value
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
        Value::Nil | Value::True => {
            // Return the default value.
            let arc = match table {
                Value::Vector(a) => a,
                _ => unreachable!(),
            };
            let vec = with_heap(|h| h.get_vector(*arc).clone());
            Ok(vec[CT_DEFAULT])
        }
        Value::Int(_) | Value::Char(_) => {
            let ch = expect_int(range)?;
            ct_lookup(table, ch)
        }
        Value::Cons(cell) => {
            // Cons range (FROM . TO): In official Emacs, this returns the
            // value for FROM (the first character in the range).
            let pair = read_cons(*cell);
            let from = expect_int(&pair.car)?;
            ct_lookup(table, from)
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-table-range"), *range],
        )),
    }
}

/// Recursive char-table lookup: check own entries, then default, then parent.
///
/// This matches GNU Emacs semantics:
/// 1. Look up the character in the char-table's data pairs
/// 2. If the local entry is nil or absent, use the char-table's default value
/// 3. If default is nil, recursively check the parent char-table
pub(crate) fn ct_lookup(table: &Value, ch: i64) -> EvalResult {
    let arc = match table {
        Value::Vector(a) => a,
        _ => return Err(wrong_type("char-table-p", table)),
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());

    if let Some(val) = ct_get_char(&vec, ch) {
        if !val.is_nil() {
            return Ok(val);
        }
    }

    let default = vec[CT_DEFAULT];
    let parent = vec[CT_PARENT];

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
    Ok(vec[CT_PARENT])
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
    with_heap_mut(|h| h.vector_set(*arc, CT_PARENT, *parent));
    Ok(*parent)
}

/// `(map-char-table FUNCTION CHAR-TABLE)` -- call FUNCTION for each
/// entry with a non-nil value.  FUNCTION receives `(KEY VALUE)` where
/// KEY is either a character (integer) or a cons `(FROM . TO)` for ranges.
///
/// This resolves overlapping ranges: later (more specific) entries override
/// earlier (broader) entries, and the result is reported as non-overlapping
/// segments, matching GNU Emacs behavior.
/// Returns nil.
pub(crate) fn builtin_map_char_table(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("map-char-table", &args, 2)?;
    let func = args[0];
    let table = &args[1];

    match table {
        Value::Vector(_) if is_char_table(table) => {}
        _ => return Err(wrong_type("char-table-p", table)),
    }

    let entries = ct_resolved_entries(table);

    for (key, val) in entries {
        eval.apply(func, vec![key, val])?;
    }
    Ok(Value::Nil)
}

/// Resolve a char-table into non-overlapping effective `(key, value)` entries.
///
/// This mirrors GNU Emacs `map-char-table` semantics:
/// - later local assignments override earlier ones regardless of whether they
///   are single-char or range writes
/// - an explicit local `nil` means "inherit", not "mask the parent"
/// - the table default fills uncovered regions before parent inheritance
/// - parent/default/local ranges are coalesced into maximal same-value runs
fn ct_resolved_entries(table: &Value) -> Vec<(Value, Value)> {
    let Value::Vector(arc) = table else {
        return Vec::new();
    };
    let vec = with_heap(|h| h.get_vector(*arc).clone());
    let raws = ct_collect_raw_entries(&vec);
    let default = vec[CT_DEFAULT];
    let parent_entries = match vec[CT_PARENT] {
        parent if is_char_table(&parent) => ct_resolved_entries(&parent),
        _ => Vec::new(),
    };

    if raws.is_empty() && default.is_nil() && parent_entries.is_empty() {
        return Vec::new();
    }

    let mut boundaries = std::collections::BTreeSet::new();
    for raw in &raws {
        boundaries.insert(raw.start);
        boundaries.insert(raw.end.saturating_add(1));
    }
    for (key, _) in &parent_entries {
        let (start, end) = resolved_key_bounds(key);
        boundaries.insert(start);
        boundaries.insert(end.saturating_add(1));
    }
    if !default.is_nil() {
        boundaries.insert(0);
        boundaries.insert(MAX_CHAR.saturating_add(1));
    }

    let boundary_vec = boundaries
        .into_iter()
        .filter(|b| *b >= 0 && *b <= MAX_CHAR.saturating_add(1))
        .collect::<Vec<_>>();

    let mut result = Vec::new();
    let mut run_start: Option<i64> = None;
    let mut run_end = 0i64;
    let mut run_value = Value::Nil;

    for window in boundary_vec.windows(2) {
        let start = window[0];
        let end_exclusive = window[1];
        if start > MAX_CHAR || end_exclusive <= start {
            continue;
        }
        let interval_end = end_exclusive.saturating_sub(1).min(MAX_CHAR);
        let value = ct_effective_value_at(&raws, default, &parent_entries, start);
        if value.is_nil() {
            if let Some(existing_start) = run_start.take() {
                emit_run(&mut result, existing_start, run_end, run_value);
            }
            continue;
        }

        match run_start {
            Some(existing_start) if run_value == value && start == run_end.saturating_add(1) => {
                let _ = existing_start;
                run_end = interval_end;
            }
            Some(existing_start) => {
                emit_run(&mut result, existing_start, run_end, run_value);
                run_start = Some(start);
                run_end = interval_end;
                run_value = value;
            }
            None => {
                run_start = Some(start);
                run_end = interval_end;
                run_value = value;
            }
        }
    }

    if let Some(existing_start) = run_start {
        emit_run(&mut result, existing_start, run_end, run_value);
    }

    result
}

#[derive(Clone, Copy)]
struct RawEntry {
    start: i64,
    end: i64,
    value: Value,
}

fn ct_collect_raw_entries(vec: &[Value]) -> Vec<RawEntry> {
    let start = ct_data_start(vec);
    let mut raws = Vec::new();
    let mut i = start;
    while i + 1 < vec.len() {
        match &vec[i] {
            Value::Int(ch) => raws.push(RawEntry {
                start: *ch,
                end: *ch,
                value: vec[i + 1],
            }),
            Value::Cons(cell) => {
                let pair = read_cons(*cell);
                if let (Value::Int(min), Value::Int(max)) = (&pair.car, &pair.cdr) {
                    raws.push(RawEntry {
                        start: *min,
                        end: *max,
                        value: vec[i + 1],
                    });
                }
            }
            _ => {}
        }
        i += 2;
    }
    raws
}

fn ct_local_value_at(raws: &[RawEntry], ch: i64) -> Option<Value> {
    let mut value = None;
    for raw in raws {
        if ch >= raw.start && ch <= raw.end {
            value = Some(raw.value);
        }
    }
    value
}

fn ct_effective_value_at(
    raws: &[RawEntry],
    default: Value,
    parent_entries: &[(Value, Value)],
    ch: i64,
) -> Value {
    if let Some(local) = ct_local_value_at(raws, ch) {
        if !local.is_nil() {
            return local;
        }
    }
    if !default.is_nil() {
        return default;
    }
    resolved_entries_value_at(parent_entries, ch).unwrap_or(Value::Nil)
}

fn resolved_entries_value_at(entries: &[(Value, Value)], ch: i64) -> Option<Value> {
    for (key, value) in entries {
        let (start, end) = resolved_key_bounds(key);
        if ch >= start && ch <= end {
            return Some(*value);
        }
    }
    None
}

fn resolved_key_bounds(key: &Value) -> (i64, i64) {
    match key {
        Value::Int(ch) => (*ch, *ch),
        Value::Cons(cell) => {
            let pair = read_cons(*cell);
            match (&pair.car, &pair.cdr) {
                (Value::Int(start), Value::Int(end)) => (*start, *end),
                _ => (0, -1),
            }
        }
        _ => (0, -1),
    }
}

/// Emit a coalesced run as either a single character or a range cons.
fn emit_run(result: &mut Vec<(Value, Value)>, start: i64, end: i64, val: Value) {
    if start == end {
        result.push((Value::Int(start), val));
    } else {
        result.push((Value::cons(Value::Int(start), Value::Int(end)), val));
    }
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
            vec![args[1], Value::Int(extra_count)],
        ));
    }

    Ok(v[CT_EXTRA_START + n as usize])
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
        return Err(signal(
            "args-out-of-range",
            vec![args[1], Value::Int(extra_count)],
        ));
    }

    v[CT_EXTRA_START + n as usize] = *value;
    with_heap_mut(|h| *h.get_vector_mut(*arc) = v);
    Ok(*value)
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
    Ok(vec[CT_SUBTYPE])
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
        return Err(signal("args-out-of-range", vec![args[0]]));
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
        vec.push(init_val);
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
        Ok(args[2])
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
        Ok(args[2])
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
        Ok(args[2])
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
        Ok(args[1])
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
        Ok(args[2])
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
            vec![args[0], Value::Int(start)],
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
#[path = "chartable_test.rs"]
mod tests;
