//! Case-table support for Emacs case conversion.
//!
//! Provides a `CaseTable` struct holding upcase/downcase/canonicalize/equivalences
//! mappings, a `CaseTableManager` with standard ASCII case tables pre-initialized,
//! and pure builtins for case-table predicates and character case conversion.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static STANDARD_CASE_TABLE_OBJECT: RefCell<Option<Value>> = const { RefCell::new(None) };
}

// ---------------------------------------------------------------------------
// CaseTable
// ---------------------------------------------------------------------------

/// A case table holding four character mappings.
#[derive(Clone, Debug)]
pub struct CaseTable {
    /// Maps lowercase characters to their uppercase equivalents.
    pub upcase: HashMap<char, char>,
    /// Maps uppercase characters to their lowercase equivalents.
    pub downcase: HashMap<char, char>,
    /// Maps characters to a canonical form (used for case-insensitive comparison).
    pub canonicalize: HashMap<char, char>,
    /// Maps characters to the next character in the equivalence class cycle.
    pub equivalences: HashMap<char, char>,
}

impl CaseTable {
    /// Create an empty case table with no mappings.
    pub fn empty() -> Self {
        Self {
            upcase: HashMap::new(),
            downcase: HashMap::new(),
            canonicalize: HashMap::new(),
            equivalences: HashMap::new(),
        }
    }

    /// Create the standard ASCII case table (a-z <-> A-Z).
    pub fn standard_ascii() -> Self {
        let mut upcase = HashMap::new();
        let mut downcase = HashMap::new();
        let mut canonicalize = HashMap::new();
        let mut equivalences = HashMap::new();

        for lower in b'a'..=b'z' {
            let upper = lower - b'a' + b'A';
            let lc = lower as char;
            let uc = upper as char;

            // Upcase: lowercase -> uppercase
            upcase.insert(lc, uc);
            // Downcase: uppercase -> lowercase
            downcase.insert(uc, lc);

            // Canonicalize: both map to lowercase
            canonicalize.insert(uc, lc);
            canonicalize.insert(lc, lc);

            // Equivalences: cycle upper -> lower -> upper
            equivalences.insert(uc, lc);
            equivalences.insert(lc, uc);
        }

        Self {
            upcase,
            downcase,
            canonicalize,
            equivalences,
        }
    }
}

// ---------------------------------------------------------------------------
// CaseTableManager
// ---------------------------------------------------------------------------

/// Manages case tables, providing standard ASCII case conversion by default.
#[derive(Clone, Debug)]
pub struct CaseTableManager {
    /// The standard (immutable) case table.
    standard: CaseTable,
    /// The current buffer-local case table.
    current: CaseTable,
}

impl CaseTableManager {
    /// Create a new manager with the standard ASCII case table.
    pub fn new() -> Self {
        let table = CaseTable::standard_ascii();
        Self {
            standard: table.clone(),
            current: table,
        }
    }

    /// Convert a character to uppercase using the current case table.
    /// Returns the character unchanged if no upcase mapping exists.
    pub fn upcase_char(&self, c: char) -> char {
        *self.current.upcase.get(&c).unwrap_or(&c)
    }

    /// Convert a character to lowercase using the current case table.
    /// Returns the character unchanged if no downcase mapping exists.
    pub fn downcase_char(&self, c: char) -> char {
        *self.current.downcase.get(&c).unwrap_or(&c)
    }

    /// Convert an entire string to uppercase.
    pub fn upcase_string(&self, s: &str) -> String {
        s.chars().map(|c| self.upcase_char(c)).collect()
    }

    /// Convert an entire string to lowercase.
    pub fn downcase_string(&self, s: &str) -> String {
        s.chars().map(|c| self.downcase_char(c)).collect()
    }

    /// Return a reference to the standard case table.
    pub fn standard_table(&self) -> &CaseTable {
        &self.standard
    }

    /// Return a reference to the current case table.
    pub fn current_table(&self) -> &CaseTable {
        &self.current
    }

    /// Set the current case table.
    pub fn set_current(&mut self, table: CaseTable) {
        self.current = table;
    }

    /// Set the standard case table.
    pub fn set_standard(&mut self, table: CaseTable) {
        self.standard = table;
    }
}

impl Default for CaseTableManager {
    fn default() -> Self {
        Self::new()
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

/// Signal `wrong-type-argument` with a predicate name.
fn wrong_type(pred: &str, got: &Value) -> Flow {
    signal(
        "wrong-type-argument",
        vec![Value::symbol(pred), got.clone()],
    )
}

/// Extract a character from a Value (Int or Char), signal otherwise.
fn expect_char(value: &Value) -> Result<char, Flow> {
    match value {
        Value::Char(c) => Ok(*c),
        Value::Int(n) => {
            if let Some(c) = char::from_u32(*n as u32) {
                Ok(c)
            } else {
                Err(wrong_type("characterp", value))
            }
        }
        other => Err(wrong_type("characterp", other)),
    }
}

// ---------------------------------------------------------------------------
// Builtins
// ---------------------------------------------------------------------------

/// `(case-table-p OBJ)` -- return t if OBJ is a case table.
///
/// In a full implementation, a case table is a char-table with `case-table`
/// sub-type.  This stub checks for a tagged vector with the `--case-table--`
/// symbol, or returns nil otherwise.
pub(crate) fn builtin_case_table_p(args: Vec<Value>) -> EvalResult {
    expect_args("case-table-p", &args, 1)?;
    Ok(Value::bool(is_case_table(&args[0])))
}

/// `(current-case-table)` -- return the current case table.
///
/// Pure fallback returns the standard case table object.
pub(crate) fn builtin_current_case_table(args: Vec<Value>) -> EvalResult {
    expect_args("current-case-table", &args, 0)?;
    ensure_standard_case_table_object()
}

/// `(standard-case-table)` -- return the standard case table.
///
/// Returns the process-wide standard case table object.
pub(crate) fn builtin_standard_case_table(args: Vec<Value>) -> EvalResult {
    expect_args("standard-case-table", &args, 0)?;
    ensure_standard_case_table_object()
}

/// `(set-case-table TABLE)` -- set the current case table.
///
/// Pure fallback: validate TABLE and return it.
pub(crate) fn builtin_set_case_table(args: Vec<Value>) -> EvalResult {
    expect_args("set-case-table", &args, 1)?;
    if !is_case_table(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("case-table-p"), args[0].clone()],
        ));
    }
    Ok(args[0].clone())
}

/// `(set-standard-case-table TABLE)` -- set the standard case table.
///
/// Pure fallback: validate TABLE and return it.
pub(crate) fn builtin_set_standard_case_table(args: Vec<Value>) -> EvalResult {
    expect_args("set-standard-case-table", &args, 1)?;
    if !is_case_table(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("case-table-p"), args[0].clone()],
        ));
    }
    STANDARD_CASE_TABLE_OBJECT.with(|slot| {
        *slot.borrow_mut() = Some(args[0].clone());
    });
    Ok(args[0].clone())
}

/// `(current-case-table)` -- evaluator-backed current buffer case table object.
pub(crate) fn builtin_current_case_table_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("current-case-table", &args, 0)?;
    current_case_table_for_buffer(eval)
}

/// `(standard-case-table)` -- evaluator-backed standard case table object.
pub(crate) fn builtin_standard_case_table_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("standard-case-table", &args, 0)?;
    ensure_standard_case_table_object_eval(eval)
}

/// `(set-case-table TABLE)` -- evaluator-backed current buffer case table set.
pub(crate) fn builtin_set_case_table_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-case-table", &args, 1)?;
    if !is_case_table(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("case-table-p"), args[0].clone()],
        ));
    }
    let table = args[0].clone();
    set_current_case_table_for_buffer(eval, table.clone())?;
    Ok(table)
}

/// `(set-standard-case-table TABLE)` -- evaluator-backed standard table set.
pub(crate) fn builtin_set_standard_case_table_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let table = builtin_set_standard_case_table(args)?;
    eval.obarray
        .set_symbol_value(STANDARD_CASE_TABLE_SYMBOL, table.clone());
    Ok(table)
}

/// `(upcase CHAR)` -- convert a character to uppercase.
///
/// If the argument is an integer or character, returns the uppercase version
/// using the standard ASCII case table.  Characters outside a-z are returned
/// unchanged.
pub(crate) fn builtin_upcase_char(args: Vec<Value>) -> EvalResult {
    expect_args("upcase", &args, 1)?;
    let c = expect_char(&args[0])?;
    let manager = CaseTableManager::new();
    let result = manager.upcase_char(c);
    Ok(Value::Int(result as i64))
}

/// `(downcase CHAR)` -- convert a character to lowercase.
///
/// If the argument is an integer or character, returns the lowercase version
/// using the standard ASCII case table. Characters outside A-Z are returned
/// unchanged.
#[cfg(test)]
pub(crate) fn builtin_downcase_char(args: Vec<Value>) -> EvalResult {
    expect_args("downcase", &args, 1)?;
    let c = expect_char(&args[0])?;
    let manager = CaseTableManager::new();
    let result = manager.downcase_char(c);
    Ok(Value::Int(result as i64))
}

// ---------------------------------------------------------------------------
// Case-table tag and predicate
// ---------------------------------------------------------------------------

const CASE_TABLE_TAG: &str = "--case-table--";
const CURRENT_CASE_TABLE_PROPERTY: &str = "case-table";
const STANDARD_CASE_TABLE_SYMBOL: &str = "neovm--standard-case-table-object";

fn make_case_table_value() -> Value {
    Value::vector(vec![
        Value::symbol(CASE_TABLE_TAG),
        Value::Nil,
        Value::Nil,
        Value::Nil,
        Value::Nil,
    ])
}

fn ensure_standard_case_table_object() -> EvalResult {
    STANDARD_CASE_TABLE_OBJECT.with(|slot| {
        if let Some(value) = slot.borrow().as_ref() {
            return Ok(value.clone());
        }
        let table = make_case_table_value();
        *slot.borrow_mut() = Some(table.clone());
        Ok(table)
    })
}

fn ensure_standard_case_table_object_eval(eval: &mut super::eval::Evaluator) -> EvalResult {
    if let Some(value) = eval
        .obarray
        .symbol_value(STANDARD_CASE_TABLE_SYMBOL)
        .cloned()
    {
        if is_case_table(&value) {
            return Ok(value);
        }
    }
    let table = make_case_table_value();
    eval.obarray
        .set_symbol_value(STANDARD_CASE_TABLE_SYMBOL, table.clone());
    Ok(table)
}

fn current_case_table_for_buffer(eval: &mut super::eval::Evaluator) -> Result<Value, Flow> {
    let fallback = ensure_standard_case_table_object_eval(eval)?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    if let Some(value) = buf.properties.get(CURRENT_CASE_TABLE_PROPERTY) {
        if is_case_table(value) {
            return Ok(value.clone());
        }
    }

    buf.properties
        .insert(CURRENT_CASE_TABLE_PROPERTY.to_string(), fallback.clone());
    Ok(fallback)
}

fn set_current_case_table_for_buffer(
    eval: &mut super::eval::Evaluator,
    table: Value,
) -> Result<(), Flow> {
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.properties
        .insert(CURRENT_CASE_TABLE_PROPERTY.to_string(), table);
    Ok(())
}

/// Return `true` if `v` is a case table (tagged vector).
pub fn is_case_table(v: &Value) -> bool {
    if let Value::Vector(arc) = v {
        let vec = arc.lock().expect("poisoned");
        vec.len() >= 5 && matches!(&vec[0], Value::Symbol(s) if s == CASE_TABLE_TAG)
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // CaseTable tests
    // -----------------------------------------------------------------------

    #[test]
    fn standard_ascii_upcase() {
        let table = CaseTable::standard_ascii();
        assert_eq!(table.upcase.get(&'a'), Some(&'A'));
        assert_eq!(table.upcase.get(&'z'), Some(&'Z'));
        assert_eq!(table.upcase.get(&'m'), Some(&'M'));
        // Uppercase letters should have no upcase mapping.
        assert_eq!(table.upcase.get(&'A'), None);
    }

    #[test]
    fn standard_ascii_downcase() {
        let table = CaseTable::standard_ascii();
        assert_eq!(table.downcase.get(&'A'), Some(&'a'));
        assert_eq!(table.downcase.get(&'Z'), Some(&'z'));
        assert_eq!(table.downcase.get(&'M'), Some(&'m'));
        // Lowercase letters should have no downcase mapping.
        assert_eq!(table.downcase.get(&'a'), None);
    }

    #[test]
    fn standard_ascii_canonicalize() {
        let table = CaseTable::standard_ascii();
        // Both upper and lower should canonicalize to lowercase.
        assert_eq!(table.canonicalize.get(&'A'), Some(&'a'));
        assert_eq!(table.canonicalize.get(&'a'), Some(&'a'));
        assert_eq!(table.canonicalize.get(&'Z'), Some(&'z'));
        assert_eq!(table.canonicalize.get(&'z'), Some(&'z'));
    }

    #[test]
    fn standard_ascii_equivalences() {
        let table = CaseTable::standard_ascii();
        // Equivalences form a cycle: A -> a -> A.
        assert_eq!(table.equivalences.get(&'A'), Some(&'a'));
        assert_eq!(table.equivalences.get(&'a'), Some(&'A'));
    }

    #[test]
    fn empty_table_has_no_mappings() {
        let table = CaseTable::empty();
        assert!(table.upcase.is_empty());
        assert!(table.downcase.is_empty());
        assert!(table.canonicalize.is_empty());
        assert!(table.equivalences.is_empty());
    }

    // -----------------------------------------------------------------------
    // CaseTableManager tests
    // -----------------------------------------------------------------------

    #[test]
    fn manager_upcase_char() {
        let mgr = CaseTableManager::new();
        assert_eq!(mgr.upcase_char('a'), 'A');
        assert_eq!(mgr.upcase_char('z'), 'Z');
        assert_eq!(mgr.upcase_char('A'), 'A'); // already uppercase, no mapping
        assert_eq!(mgr.upcase_char('0'), '0'); // non-letter unchanged
        assert_eq!(mgr.upcase_char(' '), ' ');
    }

    #[test]
    fn manager_downcase_char() {
        let mgr = CaseTableManager::new();
        assert_eq!(mgr.downcase_char('A'), 'a');
        assert_eq!(mgr.downcase_char('Z'), 'z');
        assert_eq!(mgr.downcase_char('a'), 'a'); // already lowercase, no mapping
        assert_eq!(mgr.downcase_char('5'), '5'); // non-letter unchanged
    }

    #[test]
    fn manager_upcase_string() {
        let mgr = CaseTableManager::new();
        assert_eq!(mgr.upcase_string("hello"), "HELLO");
        assert_eq!(mgr.upcase_string("Hello World"), "HELLO WORLD");
        assert_eq!(mgr.upcase_string("ABC"), "ABC");
        assert_eq!(mgr.upcase_string(""), "");
        assert_eq!(mgr.upcase_string("a1b2c3"), "A1B2C3");
    }

    #[test]
    fn manager_downcase_string() {
        let mgr = CaseTableManager::new();
        assert_eq!(mgr.downcase_string("HELLO"), "hello");
        assert_eq!(mgr.downcase_string("Hello World"), "hello world");
        assert_eq!(mgr.downcase_string("abc"), "abc");
        assert_eq!(mgr.downcase_string(""), "");
        assert_eq!(mgr.downcase_string("A1B2C3"), "a1b2c3");
    }

    #[test]
    fn manager_default() {
        let mgr = CaseTableManager::default();
        assert_eq!(mgr.upcase_char('a'), 'A');
        assert_eq!(mgr.downcase_char('A'), 'a');
    }

    #[test]
    fn manager_set_current() {
        let mut mgr = CaseTableManager::new();
        let mut custom = CaseTable::empty();
        // Map 'x' to 'Y' for upcase.
        custom.upcase.insert('x', 'Y');
        mgr.set_current(custom);
        assert_eq!(mgr.upcase_char('x'), 'Y');
        // 'a' no longer has an upcase mapping in the custom table.
        assert_eq!(mgr.upcase_char('a'), 'a');
    }

    #[test]
    fn manager_set_standard() {
        let mut mgr = CaseTableManager::new();
        let custom = CaseTable::empty();
        mgr.set_standard(custom);
        assert!(mgr.standard_table().upcase.is_empty());
    }

    // -----------------------------------------------------------------------
    // Builtin tests
    // -----------------------------------------------------------------------

    #[test]
    fn builtin_case_table_p_on_non_table() {
        assert!(matches!(
            builtin_case_table_p(vec![Value::Nil]).unwrap(),
            Value::Nil
        ));
        assert!(matches!(
            builtin_case_table_p(vec![Value::Int(42)]).unwrap(),
            Value::Nil
        ));
        assert!(matches!(
            builtin_case_table_p(vec![Value::string("hello")]).unwrap(),
            Value::Nil
        ));
    }

    #[test]
    fn builtin_case_table_p_on_tagged_vector() {
        // A properly tagged case-table vector.
        let ct = Value::vector(vec![
            Value::Symbol(CASE_TABLE_TAG.to_string()),
            Value::Nil, // upcase
            Value::Nil, // downcase
            Value::Nil, // canonicalize
            Value::Nil, // equivalences
        ]);
        assert!(matches!(
            builtin_case_table_p(vec![ct]).unwrap(),
            Value::True
        ));
    }

    #[test]
    fn builtin_case_table_p_wrong_arg_count() {
        assert!(builtin_case_table_p(vec![]).is_err());
        assert!(builtin_case_table_p(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_current_case_table_returns_case_table() {
        let result = builtin_current_case_table(vec![]).unwrap();
        assert!(is_case_table(&result));
    }

    #[test]
    fn builtin_current_case_table_wrong_args() {
        assert!(builtin_current_case_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_standard_case_table_returns_case_table() {
        let result = builtin_standard_case_table(vec![]).unwrap();
        assert!(is_case_table(&result));
    }

    #[test]
    fn builtin_standard_case_table_wrong_args() {
        assert!(builtin_standard_case_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_set_case_table_returns_arg() {
        let table = make_case_table_value();
        let result = builtin_set_case_table(vec![table.clone()]).unwrap();
        assert_eq!(result, table);
    }

    #[test]
    fn builtin_set_case_table_rejects_non_table() {
        assert!(builtin_set_case_table(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn builtin_set_case_table_wrong_args() {
        assert!(builtin_set_case_table(vec![]).is_err());
        assert!(builtin_set_case_table(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_set_standard_case_table_returns_arg() {
        let table = make_case_table_value();
        let result = builtin_set_standard_case_table(vec![table.clone()]).unwrap();
        assert_eq!(result, table);
    }

    #[test]
    fn builtin_set_standard_case_table_rejects_non_table() {
        assert!(builtin_set_standard_case_table(vec![Value::Int(1)]).is_err());
    }

    #[test]
    fn builtin_set_standard_case_table_wrong_args() {
        assert!(builtin_set_standard_case_table(vec![]).is_err());
    }

    #[test]
    fn evaluator_case_table_roundtrip_and_isolation() {
        let mut eval = super::super::eval::Evaluator::new();
        let standard = builtin_standard_case_table_eval(&mut eval, vec![]).unwrap();
        let current = builtin_current_case_table_eval(&mut eval, vec![]).unwrap();
        assert_eq!(standard, current);

        let current_id = eval.buffers.current_buffer().expect("current buffer").id;
        let other_id = eval.buffers.create_buffer("*case-other*");

        let custom = make_case_table_value();
        builtin_set_case_table_eval(&mut eval, vec![custom.clone()]).unwrap();
        let after_set = builtin_current_case_table_eval(&mut eval, vec![]).unwrap();
        assert_eq!(after_set, custom);

        eval.buffers.set_current(other_id);
        let other_current = builtin_current_case_table_eval(&mut eval, vec![]).unwrap();
        assert_eq!(other_current, standard);

        eval.buffers.set_current(current_id);
        let restored = builtin_current_case_table_eval(&mut eval, vec![]).unwrap();
        assert_eq!(restored, custom);
    }

    #[test]
    fn builtin_upcase_char_lowercase() {
        // (upcase ?a) -> 65 (i.e., ?A)
        let result = builtin_upcase_char(vec![Value::Char('a')]).unwrap();
        assert!(matches!(result, Value::Int(65)));
    }

    #[test]
    fn builtin_upcase_char_uppercase_unchanged() {
        // (upcase ?A) -> 65
        let result = builtin_upcase_char(vec![Value::Char('A')]).unwrap();
        assert!(matches!(result, Value::Int(65)));
    }

    #[test]
    fn builtin_upcase_char_non_letter() {
        // (upcase ?0) -> 48
        let result = builtin_upcase_char(vec![Value::Char('0')]).unwrap();
        assert!(matches!(result, Value::Int(48)));
    }

    #[test]
    fn builtin_upcase_char_from_int() {
        // (upcase 97) -> 65 (97 = ?a, 65 = ?A)
        let result = builtin_upcase_char(vec![Value::Int(97)]).unwrap();
        assert!(matches!(result, Value::Int(65)));
    }

    #[test]
    fn builtin_upcase_char_wrong_type() {
        assert!(builtin_upcase_char(vec![Value::string("a")]).is_err());
        assert!(builtin_upcase_char(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_upcase_char_wrong_arg_count() {
        assert!(builtin_upcase_char(vec![]).is_err());
        assert!(builtin_upcase_char(vec![Value::Char('a'), Value::Char('b')]).is_err());
    }

    #[test]
    fn builtin_downcase_char_uppercase() {
        // (downcase ?A) -> 97 (i.e., ?a)
        let result = builtin_downcase_char(vec![Value::Char('A')]).unwrap();
        assert!(matches!(result, Value::Int(97)));
    }

    #[test]
    fn builtin_downcase_char_lowercase_unchanged() {
        // (downcase ?a) -> 97
        let result = builtin_downcase_char(vec![Value::Char('a')]).unwrap();
        assert!(matches!(result, Value::Int(97)));
    }

    #[test]
    fn builtin_downcase_char_from_int() {
        // (downcase 65) -> 97 (65 = ?A, 97 = ?a)
        let result = builtin_downcase_char(vec![Value::Int(65)]).unwrap();
        assert!(matches!(result, Value::Int(97)));
    }

    #[test]
    fn builtin_downcase_char_wrong_type() {
        assert!(builtin_downcase_char(vec![Value::string("A")]).is_err());
        assert!(builtin_downcase_char(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_downcase_char_wrong_arg_count() {
        assert!(builtin_downcase_char(vec![]).is_err());
        assert!(builtin_downcase_char(vec![Value::Char('A'), Value::Char('B')]).is_err());
    }

    #[test]
    fn upcase_all_letters() {
        let mgr = CaseTableManager::new();
        for lower in b'a'..=b'z' {
            let lc = lower as char;
            let uc = (lower - b'a' + b'A') as char;
            assert_eq!(mgr.upcase_char(lc), uc);
        }
    }

    #[test]
    fn downcase_all_letters() {
        let mgr = CaseTableManager::new();
        for upper in b'A'..=b'Z' {
            let uc = upper as char;
            let lc = (upper - b'A' + b'a') as char;
            assert_eq!(mgr.downcase_char(uc), lc);
        }
    }

    #[test]
    fn roundtrip_upcase_downcase() {
        let mgr = CaseTableManager::new();
        for lower in b'a'..=b'z' {
            let lc = lower as char;
            let uc = mgr.upcase_char(lc);
            let back = mgr.downcase_char(uc);
            assert_eq!(back, lc);
        }
    }

    #[test]
    fn string_roundtrip() {
        let mgr = CaseTableManager::new();
        let original = "Hello World";
        let upper = mgr.upcase_string(original);
        let lower = mgr.downcase_string(&upper);
        assert_eq!(lower, "hello world");
    }

    #[test]
    fn non_ascii_chars_unchanged() {
        let mgr = CaseTableManager::new();
        // Non-ASCII characters should pass through unchanged with the ASCII table.
        assert_eq!(mgr.upcase_char('\u{00e9}'), '\u{00e9}'); // e-acute
        assert_eq!(mgr.downcase_char('\u{00c9}'), '\u{00c9}'); // E-acute
        assert_eq!(mgr.upcase_string("\u{00e9}"), "\u{00e9}");
    }

    #[test]
    fn is_case_table_on_short_vector() {
        // A vector too short to be a case table.
        let v = Value::vector(vec![Value::Symbol(CASE_TABLE_TAG.to_string()), Value::Nil]);
        assert!(!is_case_table(&v));
    }

    #[test]
    fn is_case_table_wrong_tag() {
        let v = Value::vector(vec![
            Value::Symbol("not-a-case-table".to_string()),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]);
        assert!(!is_case_table(&v));
    }
}
