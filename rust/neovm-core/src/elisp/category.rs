//! Character category system for the Elisp VM.
//!
//! Implements Emacs-compatible character categories.  Each character can
//! belong to zero or more *categories*, where a category is a single ASCII
//! letter (`a`-`z`, `A`-`Z`).  Categories are organized into *category
//! tables*; a `CategoryManager` keeps track of named tables and the
//! current table.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::collections::{HashMap, HashSet};

// ===========================================================================
// CategoryTable
// ===========================================================================

/// A single category table mapping characters to sets of category letters
/// and storing per-category descriptions.
#[derive(Clone, Debug)]
pub struct CategoryTable {
    /// Char -> set of category letters the char belongs to.
    pub entries: HashMap<char, HashSet<char>>,
    /// Category letter -> human-readable description string.
    pub descriptions: HashMap<char, String>,
}

impl CategoryTable {
    /// Create a new, empty category table.
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            descriptions: HashMap::new(),
        }
    }

    /// Define a category letter with a description.
    ///
    /// Returns an error string if `cat` is not a valid category letter.
    pub fn define_category(&mut self, cat: char, docstring: &str) -> Result<(), String> {
        if !is_category_letter(cat) {
            return Err(format!(
                "Invalid category character '{}': must be a-z or A-Z",
                cat
            ));
        }
        self.descriptions.insert(cat, docstring.to_string());
        Ok(())
    }

    /// Return the description for a category letter, or `None`.
    pub fn category_docstring(&self, cat: char) -> Option<&str> {
        self.descriptions.get(&cat).map(|s| s.as_str())
    }

    /// Find an unused category letter (one that has no description defined).
    /// Searches `a`-`z` then `A`-`Z`.  Returns `None` if all 52 are in use.
    pub fn get_unused_category(&self) -> Option<char> {
        for ch in 'a'..='z' {
            if !self.descriptions.contains_key(&ch) {
                return Some(ch);
            }
        }
        for ch in 'A'..='Z' {
            if !self.descriptions.contains_key(&ch) {
                return Some(ch);
            }
        }
        None
    }

    /// Add `cat` to the category set of `ch`.
    pub fn modify_entry(&mut self, ch: char, cat: char, reset: bool) -> Result<(), String> {
        if !is_category_letter(cat) {
            return Err(format!(
                "Invalid category character '{}': must be a-z or A-Z",
                cat
            ));
        }
        let set = self.entries.entry(ch).or_insert_with(HashSet::new);
        if reset {
            set.remove(&cat);
        } else {
            set.insert(cat);
        }
        Ok(())
    }

    /// Return the set of category letters for `ch` (empty set if none).
    pub fn char_category_set(&self, ch: char) -> HashSet<char> {
        self.entries.get(&ch).cloned().unwrap_or_default()
    }
}

impl Default for CategoryTable {
    fn default() -> Self {
        Self::new()
    }
}

// ===========================================================================
// CategoryManager
// ===========================================================================

/// Manages named category tables and tracks the current table.
#[derive(Clone, Debug)]
pub struct CategoryManager {
    /// Named tables.  The `"standard"` table is always present.
    pub tables: HashMap<String, CategoryTable>,
    /// Name of the currently active table.
    pub current_table: String,
}

impl CategoryManager {
    /// Create a new manager with a pre-created `"standard"` table.
    pub fn new() -> Self {
        let mut tables = HashMap::new();
        tables.insert("standard".to_string(), CategoryTable::new());
        Self {
            tables,
            current_table: "standard".to_string(),
        }
    }

    /// Return a reference to the current table.
    pub fn current(&self) -> &CategoryTable {
        self.tables
            .get(&self.current_table)
            .expect("current_table must exist in tables")
    }

    /// Return a mutable reference to the current table.
    pub fn current_mut(&mut self) -> &mut CategoryTable {
        self.tables
            .get_mut(&self.current_table)
            .expect("current_table must exist in tables")
    }

    /// Return a reference to the standard table.
    pub fn standard(&self) -> &CategoryTable {
        self.tables
            .get("standard")
            .expect("standard table must always exist")
    }

    /// Return a mutable reference to the standard table.
    pub fn standard_mut(&mut self) -> &mut CategoryTable {
        self.tables
            .get_mut("standard")
            .expect("standard table must always exist")
    }
}

impl Default for CategoryManager {
    fn default() -> Self {
        Self::new()
    }
}

// ===========================================================================
// Helpers
// ===========================================================================

/// Return `true` if `ch` is a valid category letter (`a`-`z` or `A`-`Z`).
fn is_category_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

/// Extract a character argument from a `Value`, accepting both `Char` and
/// `Int` (code-point) forms.
fn extract_char(value: &Value, fn_name: &str) -> Result<char, Flow> {
    match value {
        Value::Char(c) => Ok(*c),
        Value::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
            signal(
                "error",
                vec![Value::string(&format!(
                    "{}: Invalid character code: {}",
                    fn_name, n
                ))],
            )
        }),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), other.clone()],
        )),
    }
}

/// Expect at least `min` arguments, signalling `wrong-number-of-arguments`
/// otherwise.
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

/// Expect exactly `n` arguments.
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

/// Expect at most `max` arguments.
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

// ===========================================================================
// Pure builtins (no evaluator needed)
// ===========================================================================

/// `(define-category CHAR DOCSTRING &optional TABLE)`
///
/// Define category CHAR (a single letter) with the given DOCSTRING.
/// TABLE is currently ignored (uses the standard table).
/// Returns DOCSTRING.
pub(crate) fn builtin_define_category(args: Vec<Value>) -> EvalResult {
    expect_min_args("define-category", &args, 2)?;
    expect_max_args("define-category", &args, 3)?;

    let cat = extract_char(&args[0], "define-category")?;
    let docstring = match &args[1] {
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ));
        }
    };

    if !is_category_letter(cat) {
        return Err(signal(
            "error",
            vec![Value::string(&format!(
                "Invalid category character '{}': must be a-z or A-Z",
                cat
            ))],
        ));
    }

    // NOTE: Without access to the evaluator (and thus the CategoryManager),
    // this pure builtin records the intent but cannot actually mutate the
    // manager.  In practice, registration wires this through the evaluator.
    // For now, return the docstring as Emacs does.
    Ok(Value::string(&docstring))
}

/// `(category-docstring CATEGORY &optional TABLE)`
///
/// Return the docstring of CATEGORY.  Stub: returns nil since we lack
/// evaluator access in pure builtins.
pub(crate) fn builtin_category_docstring(args: Vec<Value>) -> EvalResult {
    expect_min_args("category-docstring", &args, 1)?;
    expect_max_args("category-docstring", &args, 2)?;

    let _cat = extract_char(&args[0], "category-docstring")?;
    // Without evaluator access, return nil.
    Ok(Value::Nil)
}

/// `(get-unused-category &optional TABLE)`
///
/// Return an unused category letter, or nil if all 52 are used.
/// Stub: returns ?a since pure builtins cannot access the manager.
pub(crate) fn builtin_get_unused_category(args: Vec<Value>) -> EvalResult {
    expect_max_args("get-unused-category", &args, 1)?;
    // Without evaluator access we cannot inspect the actual table.
    // Return ?a as a best-effort placeholder.
    Ok(Value::Char('a'))
}

/// `(category-table-p OBJ)`
///
/// Return t if OBJ is a category table.  In this implementation, category
/// tables are not first-class values (they live in the CategoryManager),
/// so this always returns nil for any Lisp value and t for the symbol
/// `category-table`.
pub(crate) fn builtin_category_table_p(args: Vec<Value>) -> EvalResult {
    expect_args("category-table-p", &args, 1)?;
    // Category tables are not first-class values in this implementation.
    // Accept `t` and the symbol `category-table` as representing one.
    let result = match &args[0] {
        Value::True => true,
        Value::Symbol(s) if s == "category-table" => true,
        _ => false,
    };
    Ok(Value::bool(result))
}

/// `(category-table)`
///
/// Return the current buffer's category table.  Stub: returns t.
pub(crate) fn builtin_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("category-table", &args, 0)?;
    Ok(Value::True)
}

/// `(standard-category-table)`
///
/// Return the standard category table.  Stub: returns t.
pub(crate) fn builtin_standard_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("standard-category-table", &args, 0)?;
    Ok(Value::True)
}

/// `(make-category-table)`
///
/// Create a new (empty) category table.  Stub: returns t.
pub(crate) fn builtin_make_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("make-category-table", &args, 0)?;
    Ok(Value::True)
}

/// `(set-category-table TABLE)`
///
/// Set the current buffer's category table to TABLE.  Stub: returns TABLE.
pub(crate) fn builtin_set_category_table(args: Vec<Value>) -> EvalResult {
    expect_args("set-category-table", &args, 1)?;
    Ok(args[0].clone())
}

/// `(make-category-set CATEGORIES)`
///
/// Return a bool-vector representing a set of categories.
/// CATEGORIES is a string of category letters.
/// The resulting bool-vector has 128 slots (one per ASCII code);
/// positions corresponding to the given category letters are set to t.
pub(crate) fn builtin_make_category_set(args: Vec<Value>) -> EvalResult {
    expect_args("make-category-set", &args, 1)?;

    let cats = match &args[0] {
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ));
        }
    };

    // Build a bool-vector of 128 slots (matching Emacs behavior).
    let mut bits = vec![Value::Int(0); 128];
    for ch in cats.chars() {
        if is_category_letter(ch) {
            let idx = ch as usize;
            if idx < 128 {
                bits[idx] = Value::Int(1);
            }
        }
    }

    // Return as a plain list of 0/1 values wrapped in a cons.
    // In Emacs this would be a bool-vector, but for compatibility with our
    // bool-vector implementation we construct one using the tagged-vector
    // convention from chartable.rs.
    let mut vec = Vec::with_capacity(2 + 128);
    vec.push(Value::Symbol("--bool-vector--".to_string()));
    vec.push(Value::Int(128));
    vec.extend(bits);
    Ok(Value::vector(vec))
}

// ===========================================================================
// Eval-dependent builtins (require evaluator / CategoryManager access)
// ===========================================================================

/// `(modify-category-entry CHAR CATEGORY &optional TABLE RESET)`
///
/// Add (or remove when RESET is non-nil) CATEGORY from the category set
/// of CHAR in the current category table.
pub(crate) fn builtin_modify_category_entry(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("modify-category-entry", &args, 2)?;
    expect_max_args("modify-category-entry", &args, 4)?;

    let ch = extract_char(&args[0], "modify-category-entry")?;
    let cat = extract_char(&args[1], "modify-category-entry")?;

    // TABLE (arg 2) is ignored — we always use the current table.
    let reset = if args.len() >= 4 {
        args[3].is_truthy()
    } else {
        false
    };

    if !is_category_letter(cat) {
        return Err(signal(
            "error",
            vec![Value::string(&format!(
                "Invalid category character '{}': must be a-z or A-Z",
                cat
            ))],
        ));
    }

    eval.category_manager
        .current_mut()
        .modify_entry(ch, cat, reset)
        .map_err(|msg| signal("error", vec![Value::string(&msg)]))?;

    Ok(Value::Nil)
}

/// `(char-category-set CHAR)`
///
/// Return a bool-vector of 128 elements indicating which categories CHAR
/// belongs to in the current category table.
pub(crate) fn builtin_char_category_set(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("char-category-set", &args, 1)?;

    let ch = extract_char(&args[0], "char-category-set")?;

    let cats = eval.category_manager.current().char_category_set(ch);

    // Build a 128-element bool-vector.
    let mut bits = vec![Value::Int(0); 128];
    for &cat in &cats {
        let idx = cat as usize;
        if idx < 128 {
            bits[idx] = Value::Int(1);
        }
    }

    let mut vec = Vec::with_capacity(2 + 128);
    vec.push(Value::Symbol("--bool-vector--".to_string()));
    vec.push(Value::Int(128));
    vec.extend(bits);
    Ok(Value::vector(vec))
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // CategoryTable
    // -----------------------------------------------------------------------

    #[test]
    fn new_table_is_empty() {
        let table = CategoryTable::new();
        assert!(table.entries.is_empty());
        assert!(table.descriptions.is_empty());
    }

    #[test]
    fn define_category_stores_description() {
        let mut table = CategoryTable::new();
        table.define_category('a', "ASCII letters").unwrap();
        assert_eq!(table.category_docstring('a'), Some("ASCII letters"));
    }

    #[test]
    fn define_category_rejects_non_letter() {
        let mut table = CategoryTable::new();
        assert!(table.define_category('1', "digits").is_err());
        assert!(table.define_category(' ', "space").is_err());
        assert!(table.define_category('!', "bang").is_err());
    }

    #[test]
    fn define_category_accepts_upper_and_lower() {
        let mut table = CategoryTable::new();
        table.define_category('a', "lower a").unwrap();
        table.define_category('Z', "upper Z").unwrap();
        assert_eq!(table.category_docstring('a'), Some("lower a"));
        assert_eq!(table.category_docstring('Z'), Some("upper Z"));
    }

    #[test]
    fn category_docstring_returns_none_for_undefined() {
        let table = CategoryTable::new();
        assert_eq!(table.category_docstring('x'), None);
    }

    #[test]
    fn get_unused_category_empty_table() {
        let table = CategoryTable::new();
        // First unused is 'a'.
        assert_eq!(table.get_unused_category(), Some('a'));
    }

    #[test]
    fn get_unused_category_skips_defined() {
        let mut table = CategoryTable::new();
        table.define_category('a', "used").unwrap();
        assert_eq!(table.get_unused_category(), Some('b'));
    }

    #[test]
    fn get_unused_category_all_lower_used() {
        let mut table = CategoryTable::new();
        for ch in 'a'..='z' {
            table.define_category(ch, "used").unwrap();
        }
        // Should return 'A' (first uppercase).
        assert_eq!(table.get_unused_category(), Some('A'));
    }

    #[test]
    fn get_unused_category_all_used() {
        let mut table = CategoryTable::new();
        for ch in 'a'..='z' {
            table.define_category(ch, "used").unwrap();
        }
        for ch in 'A'..='Z' {
            table.define_category(ch, "used").unwrap();
        }
        assert_eq!(table.get_unused_category(), None);
    }

    #[test]
    fn modify_entry_adds_category() {
        let mut table = CategoryTable::new();
        table.modify_entry('X', 'a', false).unwrap();
        let cats = table.char_category_set('X');
        assert!(cats.contains(&'a'));
    }

    #[test]
    fn modify_entry_removes_category_with_reset() {
        let mut table = CategoryTable::new();
        table.modify_entry('X', 'a', false).unwrap();
        table.modify_entry('X', 'b', false).unwrap();
        table.modify_entry('X', 'a', true).unwrap();
        let cats = table.char_category_set('X');
        assert!(!cats.contains(&'a'));
        assert!(cats.contains(&'b'));
    }

    #[test]
    fn modify_entry_rejects_non_letter() {
        let mut table = CategoryTable::new();
        assert!(table.modify_entry('X', '1', false).is_err());
    }

    #[test]
    fn char_category_set_empty_for_unknown() {
        let table = CategoryTable::new();
        let cats = table.char_category_set('Z');
        assert!(cats.is_empty());
    }

    #[test]
    fn char_category_set_multiple_categories() {
        let mut table = CategoryTable::new();
        table.modify_entry('!', 'a', false).unwrap();
        table.modify_entry('!', 'b', false).unwrap();
        table.modify_entry('!', 'c', false).unwrap();
        let cats = table.char_category_set('!');
        assert_eq!(cats.len(), 3);
        assert!(cats.contains(&'a'));
        assert!(cats.contains(&'b'));
        assert!(cats.contains(&'c'));
    }

    // -----------------------------------------------------------------------
    // CategoryManager
    // -----------------------------------------------------------------------

    #[test]
    fn manager_new_has_standard_table() {
        let mgr = CategoryManager::new();
        assert_eq!(mgr.current_table, "standard");
        assert!(mgr.tables.contains_key("standard"));
    }

    #[test]
    fn manager_current_returns_standard() {
        let mgr = CategoryManager::new();
        // Should not panic.
        let _table = mgr.current();
    }

    #[test]
    fn manager_current_mut_allows_modification() {
        let mut mgr = CategoryManager::new();
        mgr.current_mut()
            .define_category('a', "test")
            .unwrap();
        assert_eq!(mgr.current().category_docstring('a'), Some("test"));
    }

    #[test]
    fn manager_standard_and_current_are_same_initially() {
        let mut mgr = CategoryManager::new();
        mgr.standard_mut()
            .define_category('z', "zed")
            .unwrap();
        // Since current == standard, current should see it too.
        assert_eq!(mgr.current().category_docstring('z'), Some("zed"));
    }

    // -----------------------------------------------------------------------
    // Pure builtins
    // -----------------------------------------------------------------------

    #[test]
    fn builtin_define_category_basic() {
        let result = builtin_define_category(vec![
            Value::Char('a'),
            Value::string("ASCII letters"),
        ]);
        assert!(result.is_ok());
        if let Ok(Value::Str(s)) = &result {
            assert_eq!(**s, "ASCII letters");
        } else {
            panic!("Expected string result");
        }
    }

    #[test]
    fn builtin_define_category_wrong_args() {
        // Too few.
        assert!(builtin_define_category(vec![Value::Char('a')]).is_err());
        // Too many.
        assert!(builtin_define_category(vec![
            Value::Char('a'),
            Value::string("doc"),
            Value::Nil,
            Value::Nil,
        ])
        .is_err());
    }

    #[test]
    fn builtin_define_category_invalid_char() {
        let result = builtin_define_category(vec![
            Value::Char('1'),
            Value::string("digits"),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_define_category_wrong_type_docstring() {
        let result = builtin_define_category(vec![
            Value::Char('a'),
            Value::Int(42),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_category_docstring_basic() {
        let result = builtin_category_docstring(vec![Value::Char('a')]);
        assert!(result.is_ok());
        // Pure builtin returns nil without evaluator.
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_category_docstring_wrong_args() {
        assert!(builtin_category_docstring(vec![]).is_err());
        assert!(builtin_category_docstring(vec![
            Value::Char('a'),
            Value::Nil,
            Value::Nil,
        ])
        .is_err());
    }

    #[test]
    fn builtin_get_unused_category_returns_char() {
        let result = builtin_get_unused_category(vec![]).unwrap();
        assert!(matches!(result, Value::Char(_)));
    }

    #[test]
    fn builtin_get_unused_category_wrong_args() {
        assert!(builtin_get_unused_category(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_category_table_p_true_for_t() {
        let result = builtin_category_table_p(vec![Value::True]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn builtin_category_table_p_true_for_symbol() {
        let result = builtin_category_table_p(vec![Value::symbol("category-table")]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn builtin_category_table_p_false_for_int() {
        let result = builtin_category_table_p(vec![Value::Int(5)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn builtin_category_table_p_wrong_args() {
        assert!(builtin_category_table_p(vec![]).is_err());
        assert!(builtin_category_table_p(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_category_table_returns_t() {
        let result = builtin_category_table(vec![]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn builtin_category_table_wrong_args() {
        assert!(builtin_category_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_standard_category_table_returns_t() {
        let result = builtin_standard_category_table(vec![]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn builtin_standard_category_table_wrong_args() {
        assert!(builtin_standard_category_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_make_category_table_returns_t() {
        let result = builtin_make_category_table(vec![]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn builtin_make_category_table_wrong_args() {
        assert!(builtin_make_category_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_set_category_table_returns_arg() {
        let result = builtin_set_category_table(vec![Value::True]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn builtin_set_category_table_wrong_args() {
        assert!(builtin_set_category_table(vec![]).is_err());
        assert!(builtin_set_category_table(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_make_category_set_basic() {
        let result = builtin_make_category_set(vec![Value::string("abc")]).unwrap();
        // Should be a bool-vector of length 128.
        if let Value::Vector(arc) = &result {
            let vec = arc.lock().unwrap();
            // Tag + size + 128 bits = 130 elements.
            assert_eq!(vec.len(), 130);
            assert!(matches!(&vec[0], Value::Symbol(s) if s == "--bool-vector--"));
            assert!(matches!(&vec[1], Value::Int(128)));
            // 'a' = 97, 'b' = 98, 'c' = 99 should be set.
            assert!(matches!(&vec[2 + 97], Value::Int(1)));
            assert!(matches!(&vec[2 + 98], Value::Int(1)));
            assert!(matches!(&vec[2 + 99], Value::Int(1)));
            // 'd' = 100 should NOT be set.
            assert!(matches!(&vec[2 + 100], Value::Int(0)));
        } else {
            panic!("Expected vector result");
        }
    }

    #[test]
    fn builtin_make_category_set_empty_string() {
        let result = builtin_make_category_set(vec![Value::string("")]).unwrap();
        if let Value::Vector(arc) = &result {
            let vec = arc.lock().unwrap();
            assert_eq!(vec.len(), 130);
            // All bits should be 0.
            for i in 2..130 {
                assert!(matches!(&vec[i], Value::Int(0)));
            }
        } else {
            panic!("Expected vector result");
        }
    }

    #[test]
    fn builtin_make_category_set_uppercase() {
        let result = builtin_make_category_set(vec![Value::string("AZ")]).unwrap();
        if let Value::Vector(arc) = &result {
            let vec = arc.lock().unwrap();
            // 'A' = 65, 'Z' = 90
            assert!(matches!(&vec[2 + 65], Value::Int(1)));
            assert!(matches!(&vec[2 + 90], Value::Int(1)));
            assert!(matches!(&vec[2 + 66], Value::Int(0)));
        } else {
            panic!("Expected vector result");
        }
    }

    #[test]
    fn builtin_make_category_set_ignores_non_letters() {
        let result = builtin_make_category_set(vec![Value::string("a1b!c")]).unwrap();
        if let Value::Vector(arc) = &result {
            let vec = arc.lock().unwrap();
            // 'a', 'b', 'c' set; '1' and '!' not set.
            assert!(matches!(&vec[2 + 97], Value::Int(1)));  // 'a'
            assert!(matches!(&vec[2 + 98], Value::Int(1)));  // 'b'
            assert!(matches!(&vec[2 + 99], Value::Int(1)));  // 'c'
            assert!(matches!(&vec[2 + 49], Value::Int(0)));  // '1'
            assert!(matches!(&vec[2 + 33], Value::Int(0)));  // '!'
        } else {
            panic!("Expected vector result");
        }
    }

    #[test]
    fn builtin_make_category_set_wrong_type() {
        assert!(builtin_make_category_set(vec![Value::Int(42)]).is_err());
    }

    #[test]
    fn builtin_make_category_set_wrong_args() {
        assert!(builtin_make_category_set(vec![]).is_err());
        assert!(builtin_make_category_set(vec![
            Value::string("a"),
            Value::string("b"),
        ])
        .is_err());
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    #[test]
    fn is_category_letter_valid() {
        assert!(is_category_letter('a'));
        assert!(is_category_letter('z'));
        assert!(is_category_letter('A'));
        assert!(is_category_letter('Z'));
        assert!(is_category_letter('m'));
        assert!(is_category_letter('M'));
    }

    #[test]
    fn is_category_letter_invalid() {
        assert!(!is_category_letter('0'));
        assert!(!is_category_letter('9'));
        assert!(!is_category_letter(' '));
        assert!(!is_category_letter('!'));
        assert!(!is_category_letter('\n'));
    }

    #[test]
    fn extract_char_from_char_value() {
        let result = extract_char(&Value::Char('x'), "test");
        assert_eq!(result.unwrap(), 'x');
    }

    #[test]
    fn extract_char_from_int_value() {
        let result = extract_char(&Value::Int(65), "test");
        assert_eq!(result.unwrap(), 'A');
    }

    #[test]
    fn extract_char_wrong_type() {
        let result = extract_char(&Value::string("not a char"), "test");
        assert!(result.is_err());
    }
}
