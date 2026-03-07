//! Abbreviation system -- text abbreviation expansion.
//!
//! Implements GNU Emacs-compatible obarray-based abbrev tables:
//! - An abbrev table is an obarray (vector) with a special symbol `"0"` storing
//!   table properties on its plist.
//! - `abbrev-table-p` checks for the `abbrev-table` property on `"0"`.
//! - `define-abbrev` interns symbols into the obarray with expansion as
//!   `symbol-value` and hook as `symbol-function`.
//! - `abbrev-symbol` / `abbrev-expansion` look up symbols in the obarray.
//! - `clear-abbrev-table` resets all buckets, re-interns `"0"` with preserved
//!   properties.
//! - `abbrev-table-get` / `abbrev-table-put` access the `"0"` symbol's plist.
//! - `abbrev-get` / `abbrev-put` access abbrev symbol's plist.

use std::collections::HashMap;

use super::error::{EvalResult, Flow, signal};
use super::intern::resolve_sym;
use super::value::{Value, list_to_vec, with_heap, with_heap_mut};
use crate::gc::types::ObjId;

// ---------------------------------------------------------------------------
// AbbrevManager -- kept for backward compat (eval.rs, pdump)
// ---------------------------------------------------------------------------

/// A single abbreviation entry (kept for pdump compatibility).
#[derive(Clone, Debug)]
pub struct Abbrev {
    pub expansion: String,
    pub hook: Option<String>,
    pub count: usize,
    pub system: bool,
}

/// A named table of abbreviations (kept for pdump compatibility).
#[derive(Clone, Debug)]
pub struct AbbrevTable {
    pub name: String,
    pub abbrevs: HashMap<String, Abbrev>,
    pub parent: Option<String>,
    pub case_fixed: bool,
    pub enable_quoting: bool,
}

impl AbbrevTable {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            abbrevs: HashMap::new(),
            parent: None,
            case_fixed: false,
            enable_quoting: false,
        }
    }
}

/// Central registry -- now only holds the abbrev-mode flag.
/// The tables HashMap is kept for pdump compatibility but no longer used by builtins.
#[derive(Clone, Debug)]
pub struct AbbrevManager {
    tables: HashMap<String, AbbrevTable>,
    global_table_name: String,
    abbrev_mode: bool,
}

impl Default for AbbrevManager {
    fn default() -> Self {
        Self::new()
    }
}

impl AbbrevManager {
    pub fn new() -> Self {
        let global_name = "global-abbrev-table".to_string();
        let mut tables = HashMap::new();
        tables.insert(global_name.clone(), AbbrevTable::new(&global_name));
        Self {
            tables,
            global_table_name: global_name,
            abbrev_mode: false,
        }
    }

    pub fn define_abbrev(&mut self, table: &str, abbrev: &str, expansion: &str) {
        let tbl = self
            .tables
            .entry(table.to_string())
            .or_insert_with(|| AbbrevTable::new(table));
        let key = abbrev.to_lowercase();
        tbl.abbrevs.insert(
            key,
            Abbrev {
                expansion: expansion.to_string(),
                hook: None,
                count: 0,
                system: false,
            },
        );
    }

    pub fn define_abbrev_full(
        &mut self,
        table: &str,
        abbrev: &str,
        expansion: &str,
        hook: Option<String>,
        system: bool,
    ) {
        let tbl = self
            .tables
            .entry(table.to_string())
            .or_insert_with(|| AbbrevTable::new(table));
        let key = abbrev.to_lowercase();
        tbl.abbrevs.insert(
            key,
            Abbrev {
                expansion: expansion.to_string(),
                hook,
                count: 0,
                system,
            },
        );
    }

    pub fn expand_abbrev(&mut self, table: &str, word: &str) -> Option<String> {
        let key = word.to_lowercase();
        if let Some(tbl) = self.tables.get_mut(table) {
            if let Some(ab) = tbl.abbrevs.get_mut(&key) {
                ab.count += 1;
                let expansion = apply_case(&ab.expansion, word, tbl.case_fixed);
                return Some(expansion);
            }
        }
        let parent = self.tables.get(table).and_then(|t| t.parent.clone());
        if let Some(parent_name) = parent {
            return self.expand_abbrev(&parent_name, word);
        }
        if table != self.global_table_name {
            let global = self.global_table_name.clone();
            return self.expand_abbrev(&global, word);
        }
        None
    }

    pub fn create_table(&mut self, name: &str) -> &mut AbbrevTable {
        self.tables
            .entry(name.to_string())
            .or_insert_with(|| AbbrevTable::new(name))
    }

    pub fn get_table(&self, name: &str) -> Option<&AbbrevTable> {
        self.tables.get(name)
    }

    pub fn list_abbrevs(&self, table: &str) -> Vec<(&str, &str)> {
        match self.tables.get(table) {
            Some(tbl) => {
                let mut entries: Vec<(&str, &str)> = tbl
                    .abbrevs
                    .iter()
                    .map(|(k, v)| (k.as_str(), v.expansion.as_str()))
                    .collect();
                entries.sort_by_key(|(k, _)| *k);
                entries
            }
            None => Vec::new(),
        }
    }

    pub fn clear_table(&mut self, table: &str) {
        if let Some(tbl) = self.tables.get_mut(table) {
            tbl.abbrevs.clear();
        }
    }

    pub fn is_enabled(&self) -> bool {
        self.abbrev_mode
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.abbrev_mode = enabled;
    }

    pub fn global_table_name(&self) -> &str {
        &self.global_table_name
    }

    pub fn all_table_names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.tables.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    // pdump accessors
    pub(crate) fn dump_tables(&self) -> &HashMap<String, AbbrevTable> {
        &self.tables
    }
    pub(crate) fn dump_global_table_name(&self) -> &str {
        &self.global_table_name
    }
    pub(crate) fn dump_abbrev_mode(&self) -> bool {
        self.abbrev_mode
    }
    pub(crate) fn from_dump(
        tables: HashMap<String, AbbrevTable>,
        global_table_name: String,
        abbrev_mode: bool,
    ) -> Self {
        Self {
            tables,
            global_table_name,
            abbrev_mode,
        }
    }
}

// ---------------------------------------------------------------------------
// Case handling
// ---------------------------------------------------------------------------

fn apply_case(expansion: &str, word: &str, case_fixed: bool) -> String {
    if case_fixed || word.is_empty() || expansion.is_empty() {
        return expansion.to_string();
    }
    let all_upper = word.chars().all(|c| !c.is_alphabetic() || c.is_uppercase());
    let first_upper = word
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false);
    if all_upper && word.chars().any(|c| c.is_alphabetic()) {
        expansion.to_uppercase()
    } else if first_upper {
        let mut chars = expansion.chars();
        match chars.next() {
            Some(first) => {
                let mut result = first.to_uppercase().to_string();
                result.extend(chars);
                result
            }
            None => expansion.to_string(),
        }
    } else {
        expansion.to_string()
    }
}

// ===========================================================================
// Obarray helpers (shared with builtins/symbols.rs logic)
// ===========================================================================

/// Default obarray size for abbrev tables (same as GNU Emacs).
const ABBREV_TABLE_DEFAULT_SIZE: usize = 59;

/// Hash a string for obarray bucket lookup.
fn obarray_hash(s: &str, len: usize) -> usize {
    let hash = s
        .bytes()
        .fold(0u64, |h, b| h.wrapping_mul(31).wrapping_add(b as u64));
    hash as usize % len
}

/// Search a bucket chain (cons list) for a symbol with the given name.
fn obarray_bucket_find(bucket: Value, name: &str) -> Option<Value> {
    let mut current = bucket;
    loop {
        match current {
            Value::Nil => return None,
            Value::Cons(id) => {
                let (car, cdr) = with_heap(|h| (h.cons_car(id), h.cons_cdr(id)));
                if let Some(sym_name) = car.as_symbol_name() {
                    if sym_name == name {
                        return Some(car);
                    }
                }
                current = cdr;
            }
            _ => return None,
        }
    }
}

/// Intern a symbol into a custom obarray (vector). Returns the symbol Value.
fn obarray_intern(vec_id: ObjId, name: &str) -> Value {
    let vec_len = with_heap(|h| h.get_vector(vec_id).len());
    if vec_len == 0 {
        // Shouldn't happen for abbrev tables, but safety.
        let sym = Value::symbol(name);
        return sym;
    }
    let bucket_idx = obarray_hash(name, vec_len);
    let bucket = with_heap(|h| h.get_vector(vec_id)[bucket_idx]);

    // Check if already interned
    if let Some(sym) = obarray_bucket_find(bucket, name) {
        return sym;
    }

    // Not found: create symbol and prepend to bucket chain
    let sym = Value::symbol(name);
    let new_bucket = Value::cons(sym, bucket);
    with_heap_mut(|h| {
        h.get_vector_mut(vec_id)[bucket_idx] = new_bucket;
    });
    sym
}

/// Look up a symbol in a custom obarray (vector) without interning.
fn obarray_lookup(vec_id: ObjId, name: &str) -> Option<Value> {
    let vec_len = with_heap(|h| h.get_vector(vec_id).len());
    if vec_len == 0 {
        return None;
    }
    let bucket_idx = obarray_hash(name, vec_len);
    let bucket = with_heap(|h| h.get_vector(vec_id)[bucket_idx]);
    obarray_bucket_find(bucket, name)
}

/// Check if a Value is an abbrev table (obarray with "0" symbol having
/// `abbrev-table` property set to non-nil).
fn is_abbrev_table(eval: &super::eval::Evaluator, value: &Value) -> bool {
    let vec_id = match value {
        Value::Vector(id) => *id,
        _ => return false,
    };
    let vec_len = with_heap(|h| h.get_vector(vec_id).len());
    if vec_len == 0 {
        return false;
    }
    // Look for the "0" symbol
    if let Some(_sym) = obarray_lookup(vec_id, "0") {
        // Check if it has the `abbrev-table` property
        if let Some(val) = eval.obarray().get_property("0", "abbrev-table") {
            return val.is_truthy();
        }
    }
    false
}

/// Collect all symbols from an obarray into a Vec.
fn obarray_all_symbols(vec_id: ObjId) -> Vec<Value> {
    let all_slots = with_heap(|h| h.get_vector(vec_id).clone());
    let mut symbols = Vec::new();
    for slot in &all_slots {
        let mut current = *slot;
        loop {
            match current {
                Value::Nil => break,
                Value::Cons(id) => {
                    let (car, cdr) = with_heap(|h| (h.cons_car(id), h.cons_cdr(id)));
                    symbols.push(car);
                    current = cdr;
                }
                _ => break,
            }
        }
    }
    symbols
}

// ===========================================================================
// Builtin helpers
// ===========================================================================

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

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(id) => Ok(with_heap(|h| h.get_string(*id).clone())),
        Value::Symbol(id) => Ok(resolve_sym(*id).to_owned()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), *other],
        )),
    }
}

fn expect_abbrev_table<'a>(name: &str, value: &Value) -> Result<ObjId, Flow> {
    match value {
        Value::Vector(id) => Ok(*id),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol(name), *value],
        )),
    }
}

// ===========================================================================
// Obarray-based builtins
// ===========================================================================

/// (make-abbrev-table &optional PROPS) -> obarray
///
/// Create a new empty abbrev table (obarray with a "0" symbol).
pub(crate) fn builtin_make_abbrev_table(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    // Create vector of ABBREV_TABLE_DEFAULT_SIZE nil slots
    let table = Value::vector(vec![Value::Nil; ABBREV_TABLE_DEFAULT_SIZE]);
    let vec_id = match table {
        Value::Vector(id) => id,
        _ => unreachable!(),
    };

    // Intern the "0" symbol and set abbrev-table property
    obarray_intern(vec_id, "0");
    eval.obarray_mut()
        .put_property("0", "abbrev-table", Value::True);

    // Process optional property list
    if let Some(props_val) = args.first() {
        if !props_val.is_nil() {
            if let Some(props) = list_to_vec(props_val) {
                let mut i = 0;
                while i + 1 < props.len() {
                    let prop = &props[i];
                    let val = props[i + 1];
                    if let Some(prop_name) = prop.as_symbol_name() {
                        eval.obarray_mut().put_property("0", prop_name, val);
                    }
                    i += 2;
                }
            }
        }
    }

    Ok(table)
}

/// (abbrev-table-p OBJ) -> t or nil
///
/// Return t if OBJ is an abbrev table (obarray with "0" symbol having
/// `abbrev-table` property).
pub(crate) fn builtin_abbrev_table_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("abbrev-table-p", &args, 1)?;
    Ok(Value::bool(is_abbrev_table(eval, &args[0])))
}

/// (define-abbrev TABLE NAME EXPANSION &optional HOOK &rest PROPS) -> name-symbol
///
/// TABLE is an abbrev table (obarray).
/// NAME is a string. EXPANSION is a string or nil.
pub(crate) fn builtin_define_abbrev(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("define-abbrev", &args, 3)?;
    let vec_id = expect_abbrev_table("abbrev-table-p", &args[0])?;
    let name = expect_string(&args[1])?;

    // Intern the abbreviation symbol into the obarray
    let sym = obarray_intern(vec_id, &name);

    // Set symbol-value to expansion (the expansion string or nil)
    let expansion = args[2];
    let sym_name = sym.as_symbol_name().unwrap();
    eval.obarray_mut().set_symbol_value(sym_name, expansion);

    // Set symbol-function to hook (4th arg), or nil
    let hook = if args.len() > 3 { args[3] } else { Value::Nil };
    eval.obarray_mut().set_symbol_function(sym_name, hook);

    // Process keyword properties (remaining args after hook)
    // Default :count to 0 if not specified
    let mut count_set = false;
    let mut system_set = false;
    if args.len() > 4 {
        let mut i = 4;
        while i + 1 < args.len() {
            let prop = &args[i];
            let val = args[i + 1];
            if let Some(prop_name) = prop.as_symbol_name() {
                eval.obarray_mut().put_property(sym_name, prop_name, val);
                if prop_name == ":count" {
                    count_set = true;
                }
                if prop_name == ":system" {
                    system_set = true;
                }
            }
            i += 2;
        }
    }

    // Set default :count to 0 if not explicitly provided
    if !count_set {
        eval.obarray_mut()
            .put_property(sym_name, ":count", Value::Int(0));
    }
    // Set default :system to nil if not explicitly provided
    if !system_set {
        eval.obarray_mut()
            .put_property(sym_name, ":system", Value::Nil);
    }

    Ok(sym)
}

/// (abbrev-symbol ABBREV &optional TABLE) -> symbol or nil
///
/// Look up ABBREV in TABLE (or the local/global abbrev tables).
pub(crate) fn builtin_abbrev_symbol(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("abbrev-symbol", &args, 1)?;
    let name = expect_string(&args[0])?;

    // If TABLE is provided
    if let Some(table_val) = args.get(1) {
        if !table_val.is_nil() {
            let vec_id = expect_abbrev_table("abbrev-table-p", table_val)?;
            // Look up in this table
            if let Some(sym) = obarray_lookup(vec_id, &name) {
                // Skip the "0" symbol and symbols without expansions
                if sym.as_symbol_name() != Some("0") {
                    return Ok(sym);
                }
            }
            // Check parent tables
            if let Some(parents) = get_table_property(eval, vec_id, ":parents") {
                if let Some(parent_list) = list_to_vec(&parents) {
                    for parent in &parent_list {
                        if let Value::Vector(pid) = parent {
                            if let Some(sym) = obarray_lookup(*pid, &name) {
                                if sym.as_symbol_name() != Some("0") {
                                    return Ok(sym);
                                }
                            }
                        }
                    }
                }
            }
            return Ok(Value::Nil);
        }
    }

    // Fall back to global-abbrev-table
    let global_table = eval
        .obarray()
        .symbol_value("global-abbrev-table")
        .cloned()
        .unwrap_or(Value::Nil);
    if let Value::Vector(vec_id) = global_table {
        if let Some(sym) = obarray_lookup(vec_id, &name) {
            if sym.as_symbol_name() != Some("0") {
                return Ok(sym);
            }
        }
    }
    Ok(Value::Nil)
}

/// (abbrev-expansion ABBREV &optional TABLE) -> string or nil
///
/// Look up the expansion of ABBREV without expanding it.
pub(crate) fn builtin_abbrev_expansion(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("abbrev-expansion", &args, 1)?;
    let name = expect_string(&args[0])?;

    // If TABLE is provided
    if let Some(table_val) = args.get(1) {
        if !table_val.is_nil() {
            let vec_id = expect_abbrev_table("abbrev-table-p", table_val)?;
            if let Some(sym) = obarray_lookup(vec_id, &name) {
                let sym_name = sym.as_symbol_name().unwrap();
                if sym_name != "0" {
                    if let Some(val) = eval.obarray().symbol_value(sym_name).cloned() {
                        if !val.is_nil() {
                            return Ok(val);
                        }
                    }
                }
            }
            // Check parent tables
            if let Some(parents) = get_table_property(eval, vec_id, ":parents") {
                if let Some(parent_list) = list_to_vec(&parents) {
                    for parent in &parent_list {
                        if let Value::Vector(pid) = parent {
                            if let Some(sym) = obarray_lookup(*pid, &name) {
                                let sym_name = sym.as_symbol_name().unwrap();
                                if sym_name != "0" {
                                    if let Some(val) =
                                        eval.obarray().symbol_value(sym_name).cloned()
                                    {
                                        if !val.is_nil() {
                                            return Ok(val);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return Ok(Value::Nil);
        }
    }

    // Fall back to global-abbrev-table
    let global_table = eval
        .obarray()
        .symbol_value("global-abbrev-table")
        .cloned()
        .unwrap_or(Value::Nil);
    if let Value::Vector(vec_id) = global_table {
        if let Some(sym) = obarray_lookup(vec_id, &name) {
            let sym_name = sym.as_symbol_name().unwrap();
            if sym_name != "0" {
                if let Some(val) = eval.obarray().symbol_value(sym_name).cloned() {
                    if !val.is_nil() {
                        return Ok(val);
                    }
                }
            }
        }
    }
    Ok(Value::Nil)
}

/// (clear-abbrev-table TABLE) -> nil
///
/// Reset all symbols in TABLE except the "0" property symbol.
/// In GNU Emacs, system abbrevs are kept but with empty expansion.
pub(crate) fn builtin_clear_abbrev_table(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("clear-abbrev-table", &args, 1)?;
    let vec_id = expect_abbrev_table("abbrev-table-p", &args[0])?;

    // Collect all symbols and their properties
    let symbols = obarray_all_symbols(vec_id);

    // For each non-"0" symbol, check if it's a system abbrev
    for sym in &symbols {
        if let Some(sym_name) = sym.as_symbol_name() {
            if sym_name == "0" {
                continue;
            }
            // Check if system abbrev
            let is_system = eval
                .obarray()
                .get_property(sym_name, ":system")
                .map(|v| v.is_truthy())
                .unwrap_or(false);

            if is_system {
                // System abbrevs: set expansion to "" and count to 0
                eval.obarray_mut()
                    .set_symbol_value(sym_name, Value::string(""));
                eval.obarray_mut()
                    .put_property(sym_name, ":count", Value::Int(0));
            } else {
                // Non-system: set expansion to nil, hook to nil, count to 0
                eval.obarray_mut().set_symbol_value(sym_name, Value::Nil);
                eval.obarray_mut().set_symbol_function(sym_name, Value::Nil);
                eval.obarray_mut()
                    .put_property(sym_name, ":count", Value::Int(0));
            }
        }
    }

    // Clear all buckets and re-intern "0" and system abbrevs
    let vec_len = with_heap(|h| h.get_vector(vec_id).len());
    with_heap_mut(|h| {
        let v = h.get_vector_mut(vec_id);
        for i in 0..vec_len {
            v[i] = Value::Nil;
        }
    });

    // Re-intern "0" (its properties are on the global obarray, unchanged)
    obarray_intern(vec_id, "0");

    // Re-intern system abbrevs
    for sym in &symbols {
        if let Some(sym_name) = sym.as_symbol_name() {
            if sym_name == "0" {
                continue;
            }
            let is_system = eval
                .obarray()
                .get_property(sym_name, ":system")
                .map(|v| v.is_truthy())
                .unwrap_or(false);
            if is_system {
                obarray_intern(vec_id, sym_name);
            }
        }
    }

    Ok(Value::Nil)
}

/// (abbrev-table-get TABLE PROP) -> value
///
/// Get property PROP from the "0" symbol of TABLE.
pub(crate) fn builtin_abbrev_table_get(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("abbrev-table-get", &args, 2)?;
    let vec_id = expect_abbrev_table("abbrev-table-p", &args[0])?;
    let prop = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1]],
        )
    })?;
    Ok(get_table_property(eval, vec_id, prop).unwrap_or(Value::Nil))
}

/// (abbrev-table-put TABLE PROP VAL) -> VAL
///
/// Set property PROP to VAL on the "0" symbol of TABLE.
pub(crate) fn builtin_abbrev_table_put(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("abbrev-table-put", &args, 3)?;
    let _vec_id = expect_abbrev_table("abbrev-table-p", &args[0])?;
    let prop = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1]],
        )
    })?;
    eval.obarray_mut().put_property("0", prop, args[2]);
    Ok(args[2])
}

/// (abbrev-get SYMBOL PROP) -> value
///
/// Get property PROP from abbrev SYMBOL's plist.
pub(crate) fn builtin_abbrev_get(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("abbrev-get", &args, 2)?;
    let sym_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0]],
        )
    })?;
    let prop = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1]],
        )
    })?;
    Ok(eval
        .obarray()
        .get_property(sym_name, prop)
        .cloned()
        .unwrap_or(Value::Nil))
}

/// (abbrev-put SYMBOL PROP VAL) -> VAL
///
/// Set property PROP to VAL on abbrev SYMBOL's plist.
pub(crate) fn builtin_abbrev_put(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("abbrev-put", &args, 3)?;
    let sym_name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0]],
        )
    })?;
    let prop = args[1].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1]],
        )
    })?;
    eval.obarray_mut().put_property(sym_name, prop, args[2]);
    Ok(args[2])
}

/// Helper: get a property from the "0" symbol of an obarray.
fn get_table_property(eval: &super::eval::Evaluator, _vec_id: ObjId, prop: &str) -> Option<Value> {
    eval.obarray().get_property("0", prop).cloned()
}

/// (define-abbrev-table NAME DEFS &optional DOCSTRING &rest PROPS) -> nil
///
/// NAME is a symbol. Creates the table as an obarray, sets it as NAME's value,
/// and adds NAME to `abbrev-table-name-list`.
pub(crate) fn builtin_define_abbrev_table(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("define-abbrev-table", &args, 2)?;

    // NAME must be a symbol (quoted)
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0]],
        )
    })?;

    // Check if table already exists
    let table = if let Some(existing) = eval.obarray().symbol_value(name).cloned() {
        if is_abbrev_table(eval, &existing) {
            existing
        } else {
            // Create new table
            builtin_make_abbrev_table(eval, vec![])?
        }
    } else {
        builtin_make_abbrev_table(eval, vec![])?
    };

    let vec_id = match table {
        Value::Vector(id) => id,
        _ => unreachable!(),
    };

    // Set as symbol value
    eval.obarray_mut().set_symbol_value(name, table);

    // Add to abbrev-table-name-list if not already present
    let name_sym = Value::symbol(name);
    let current_list = eval
        .obarray()
        .symbol_value("abbrev-table-name-list")
        .cloned()
        .unwrap_or(Value::Nil);

    // Check if already in list
    let mut already_in_list = false;
    if let Some(items) = list_to_vec(&current_list) {
        for item in &items {
            if let (Some(a), Some(b)) = (item.as_symbol_name(), name_sym.as_symbol_name()) {
                if a == b {
                    already_in_list = true;
                    break;
                }
            }
        }
    }

    if !already_in_list {
        let new_list = Value::cons(name_sym, current_list);
        eval.obarray_mut()
            .set_symbol_value("abbrev-table-name-list", new_list);
    }

    // Process DOCSTRING (3rd arg) -- store as table property if it's a string
    if let Some(docstring) = args.get(2) {
        if let Value::Str(_) = docstring {
            eval.obarray_mut()
                .put_property("0", ":docstring", *docstring);
        }
    }

    // Process properties (PROPS after docstring)
    // In GNU Emacs: (define-abbrev-table 'name defs "doc" :prop1 val1 :prop2 val2 ...)
    if args.len() > 3 {
        let mut i = 3;
        while i + 1 < args.len() {
            let prop = &args[i];
            let val = args[i + 1];
            if let Some(prop_name) = prop.as_symbol_name() {
                eval.obarray_mut().put_property("0", prop_name, val);
            }
            i += 2;
        }
    }

    // Process DEFS (2nd arg) -- list of (name expansion hook &rest props)
    let defs = &args[1];
    if !defs.is_nil() {
        if let Some(def_list) = list_to_vec(defs) {
            for def_val in &def_list {
                if let Some(def_items) = list_to_vec(def_val) {
                    if def_items.len() >= 2 {
                        let abbrev_name = expect_string(&def_items[0])?;
                        let expansion = def_items[1];
                        let hook = if def_items.len() > 2 {
                            def_items[2]
                        } else {
                            Value::Nil
                        };

                        // Build args for define-abbrev
                        let mut da_args = vec![table, Value::string(&abbrev_name), expansion, hook];

                        // Append remaining items as keyword properties
                        if def_items.len() > 3 {
                            da_args.extend_from_slice(&def_items[3..]);
                        }

                        builtin_define_abbrev(eval, da_args)?;
                    }
                }
            }
        }
    }

    Ok(Value::Nil)
}

/// (expand-abbrev) -> string or nil
///
/// NeoVM stub: returns nil in batch/non-interactive use.
pub(crate) fn builtin_expand_abbrev(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("expand-abbrev", &args, 0)?;
    Ok(Value::Nil)
}

/// (abbrev-mode &optional ARG) -> t or nil
pub(crate) fn builtin_abbrev_mode(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.is_empty() {
        let new_state = !eval.abbrevs.is_enabled();
        eval.abbrevs.set_enabled(new_state);
        Ok(Value::bool(new_state))
    } else {
        match &args[0] {
            Value::Int(n) => {
                let enabled = *n > 0;
                eval.abbrevs.set_enabled(enabled);
                Ok(Value::bool(enabled))
            }
            Value::Nil => {
                eval.abbrevs.set_enabled(false);
                Ok(Value::Nil)
            }
            _ => {
                eval.abbrevs.set_enabled(true);
                Ok(Value::True)
            }
        }
    }
}

/// (insert-abbrev-table-description NAME &optional READABLE) -> nil
///
/// Insert a description of the abbrev table named NAME into the current buffer.
/// This is a simplified version that inserts into the current buffer.
pub(crate) fn builtin_insert_abbrev_table_description(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("insert-abbrev-table-description", &args, 1)?;

    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0]],
        )
    })?;

    // Get the table value
    let table_val = eval
        .obarray()
        .symbol_value(name)
        .cloned()
        .unwrap_or(Value::Nil);

    let vec_id = match table_val {
        Value::Vector(id) => id,
        _ => {
            // Insert empty table description
            let text = format!("(define-abbrev-table '{})\n", name);
            if let Some(buf) = eval.buffers.current_buffer_mut() {
                buf.insert(&text);
            }
            return Ok(Value::Nil);
        }
    };

    // Collect all abbrev symbols (not "0")
    let symbols = obarray_all_symbols(vec_id);
    let mut entries: Vec<(String, String, i64, Option<String>)> = Vec::new();

    for sym in &symbols {
        if let Some(sym_name) = sym.as_symbol_name() {
            if sym_name == "0" || sym_name.is_empty() {
                continue;
            }
            let expansion = eval
                .obarray()
                .symbol_value(sym_name)
                .cloned()
                .unwrap_or(Value::Nil);
            if expansion.is_nil() {
                continue;
            }
            let exp_str = match &expansion {
                Value::Str(id) => with_heap(|h| h.get_string(*id).clone()),
                _ => continue,
            };
            let count = eval
                .obarray()
                .get_property(sym_name, ":count")
                .and_then(|v| {
                    if let Value::Int(n) = v {
                        Some(*n)
                    } else {
                        None
                    }
                })
                .unwrap_or(0);
            let hook_fn = eval
                .obarray()
                .symbol_function(sym_name)
                .cloned()
                .and_then(|v| {
                    if v.is_nil() {
                        None
                    } else {
                        v.as_symbol_name().map(|s| s.to_string())
                    }
                });
            entries.push((sym_name.to_string(), exp_str, count, hook_fn));
        }
    }

    entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut text = format!("(define-abbrev-table '{}\n  '(\n", name);
    for (abbrev, expansion, count, hook) in &entries {
        let hook_str = match hook {
            Some(h) => format!(" '{}", h),
            None => String::new(),
        };
        text.push_str(&format!(
            "    (\"{}\"{} \"{}\" {})\n",
            abbrev, hook_str, expansion, count
        ));
    }
    text.push_str("   ))\n\n");

    // Insert into current buffer
    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.insert(&text);
    }
    Ok(Value::Nil)
}

// ===========================================================================
// Tests
// ===========================================================================
#[cfg(test)]
#[path = "abbrev_test.rs"]
mod tests;
