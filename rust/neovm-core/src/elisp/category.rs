//! Character category system for the Elisp VM.
//!
//! Implements Emacs-compatible character categories.  Each character can
//! belong to zero or more *categories*, where a category is a single ASCII
//! letter (`a`-`z`, `A`-`Z`).  Categories are organized into *category
//! tables*; a `CategoryManager` keeps track of named tables and the
//! current table.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

thread_local! {
    static STANDARD_CATEGORY_TABLE: RefCell<Option<Value>> = const { RefCell::new(None) };
    static PURE_CATEGORY_MANAGER: RefCell<CategoryManager> = RefCell::new(CategoryManager::new());
}

const CATEGORY_TABLE_PROPERTY: &str = "category-table";

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
        let mut descriptions = HashMap::new();
        // Emacs ships pre-defined category entries, including digit classes.
        descriptions.insert('1', "decimal digit".to_string());
        Self {
            entries: HashMap::new(),
            descriptions,
        }
    }

    /// Define a category letter with a description.
    ///
    /// Returns an error string if `cat` is not a valid category letter.
    pub fn define_category(&mut self, cat: char, docstring: &str) -> Result<(), String> {
        if !is_category_letter(cat) {
            return Err(format!(
                "Invalid category character '{}': must be ASCII graphic",
                cat
            ));
        }
        if self.descriptions.contains_key(&cat) {
            return Err(format!("Category ‘{}’ is already defined", cat));
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
                "Invalid category character '{}': must be ASCII graphic",
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

/// Return `true` if `ch` is a valid category code (ASCII graphic).
fn is_category_letter(ch: char) -> bool {
    ch.is_ascii_graphic()
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

fn make_category_table_object() -> EvalResult {
    super::chartable::builtin_make_char_table(vec![Value::symbol("category-table")])
}

fn is_category_table_value(value: &Value) -> Result<bool, Flow> {
    let is_char_table = super::chartable::builtin_char_table_p(vec![value.clone()])?;
    if !is_char_table.is_truthy() {
        return Ok(false);
    }
    let subtype = super::chartable::builtin_char_table_subtype(vec![value.clone()])?;
    Ok(matches!(subtype, Value::Symbol(name) if name == "category-table"))
}

fn clone_char_table_object(value: &Value) -> EvalResult {
    match value {
        Value::Vector(v) => Ok(Value::vector(
            with_heap(|h| h.get_vector(*v).clone()),
        )),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("category-table-p"), other.clone()],
        )),
    }
}

fn ensure_standard_category_table() -> EvalResult {
    STANDARD_CATEGORY_TABLE.with(|slot| {
        if let Some(table) = slot.borrow().as_ref() {
            return Ok(table.clone());
        }

        let table = make_category_table_object()?;
        *slot.borrow_mut() = Some(table.clone());
        Ok(table)
    })
}

fn category_table_pointer_eq(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Vector(a), Value::Vector(b)) => a == b,
        _ => false,
    }
}

fn current_buffer_category_table(eval: &mut super::eval::Evaluator) -> Result<Value, Flow> {
    let fallback = ensure_standard_category_table()?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    if let Some(table) = buf.properties.get(CATEGORY_TABLE_PROPERTY) {
        if is_category_table_value(table)? {
            return Ok(table.clone());
        }
    }

    buf.properties
        .insert(CATEGORY_TABLE_PROPERTY.to_string(), fallback.clone());
    Ok(fallback)
}

fn set_current_buffer_category_table(
    eval: &mut super::eval::Evaluator,
    table: Value,
) -> Result<(), Flow> {
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.properties
        .insert(CATEGORY_TABLE_PROPERTY.to_string(), table);
    Ok(())
}

// ===========================================================================
// Pure builtins (no evaluator needed)
// ===========================================================================

/// `(define-category CHAR DOCSTRING &optional TABLE)`
///
/// Define category CHAR (a single letter) with the given DOCSTRING.
/// TABLE is currently ignored (uses the standard table).
/// Returns nil.
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
                "Invalid category character '{}': must be ASCII graphic",
                cat
            ))],
        ));
    }

    // TABLE (arg 2) is currently ignored; category tables are not first-class.
    PURE_CATEGORY_MANAGER.with(|slot| {
        slot.borrow_mut()
            .current_mut()
            .define_category(cat, &docstring)
            .map_err(|msg| signal("error", vec![Value::string(msg)]))
    })?;

    Ok(Value::Nil)
}

/// `(category-docstring CATEGORY &optional TABLE)`
///
/// Return the docstring of CATEGORY from the pure category manager.
pub(crate) fn builtin_category_docstring(args: Vec<Value>) -> EvalResult {
    expect_min_args("category-docstring", &args, 1)?;
    expect_max_args("category-docstring", &args, 2)?;

    let cat = extract_char(&args[0], "category-docstring")?;
    // TABLE (arg 1) is currently ignored; category tables are not first-class.
    let _ = args.get(1);

    PURE_CATEGORY_MANAGER.with(|slot| {
        let doc = slot.borrow();
        match doc.current().category_docstring(cat) {
            Some(text) => Ok(Value::string(text)),
            None => Ok(Value::Nil),
        }
    })
}

/// `(get-unused-category &optional TABLE)`
///
/// Return an unused category letter, or nil if all 52 are used.
pub(crate) fn builtin_get_unused_category(args: Vec<Value>) -> EvalResult {
    expect_max_args("get-unused-category", &args, 1)?;
    // TABLE (arg 0) is currently ignored; category tables are not first-class.
    let _ = args.first();

    PURE_CATEGORY_MANAGER.with(|slot| match slot.borrow().current().get_unused_category() {
        Some(cat) => Ok(Value::Char(cat)),
        None => Ok(Value::Nil),
    })
}

/// `(category-table-p OBJ)`
///
/// Return t if OBJ is a category table.  In this implementation, category
/// tables are represented as char-tables with subtype `category-table`.
pub(crate) fn builtin_category_table_p(args: Vec<Value>) -> EvalResult {
    expect_args("category-table-p", &args, 1)?;
    Ok(Value::bool(is_category_table_value(&args[0])?))
}

/// `(category-table)`
///
/// Return the standard category table in pure mode.
pub(crate) fn builtin_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("category-table", &args, 0)?;
    ensure_standard_category_table()
}

/// `(standard-category-table)`
///
/// Return the standard category table.
pub(crate) fn builtin_standard_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("standard-category-table", &args, 0)?;
    ensure_standard_category_table()
}

/// `(make-category-table)`
///
/// Create a new (empty) category table.
pub(crate) fn builtin_make_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("make-category-table", &args, 0)?;
    make_category_table_object()
}

/// `(copy-category-table &optional TABLE)`
///
/// Return a fresh copy of TABLE.  When TABLE is omitted or nil, copy the
/// process-wide standard category table.
pub(crate) fn builtin_copy_category_table(args: Vec<Value>) -> EvalResult {
    expect_max_args("copy-category-table", &args, 1)?;

    let source = match args.first() {
        None | Some(Value::Nil) => ensure_standard_category_table()?,
        Some(table) => {
            if !is_category_table_value(table)? {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("category-table-p"), table.clone()],
                ));
            }
            table.clone()
        }
    };

    clone_char_table_object(&source)
}

/// `(set-category-table TABLE)`
///
/// Pure-mode fallback: validate TABLE and return it.
pub(crate) fn builtin_set_category_table(args: Vec<Value>) -> EvalResult {
    expect_args("set-category-table", &args, 1)?;
    if args[0].is_nil() {
        return ensure_standard_category_table();
    }
    if !is_category_table_value(&args[0])? {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("category-table-p"), args[0].clone()],
        ));
    }
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

/// `(category-set-mnemonics CATEGORY-SET)`
///
/// Return CATEGORY-SET as a sorted mnemonic string.
pub(crate) fn builtin_category_set_mnemonics(args: Vec<Value>) -> EvalResult {
    expect_args("category-set-mnemonics", &args, 1)?;

    let Value::Vector(bits_arc) = &args[0] else {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("categorysetp"), args[0].clone()],
        ));
    };

    let bits = with_heap(|h| h.get_vector(*bits_arc).clone());
    let valid_shape = bits.len() >= 130
        && matches!(&bits[0], Value::Symbol(tag) if tag == "--bool-vector--")
        && matches!(&bits[1], Value::Int(128));
    if !valid_shape {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("categorysetp"), args[0].clone()],
        ));
    }

    let mut out = String::new();
    for idx in 0..128usize {
        let is_set = match &bits[2 + idx] {
            Value::Nil => false,
            Value::Int(0) => false,
            _ => true,
        };
        if is_set {
            let ch = idx as u8 as char;
            if is_category_letter(ch) {
                out.push(ch);
            }
        }
    }

    Ok(Value::string(&out))
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
                "Invalid category character '{}': must be ASCII graphic",
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

/// `(define-category CHAR DOCSTRING &optional TABLE)` (evaluator-backed).
///
/// Stores the category docstring in the active category manager table and
/// returns nil.
pub(crate) fn builtin_define_category_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
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
                "Invalid category character '{}': must be ASCII graphic",
                cat
            ))],
        ));
    }

    // TABLE (arg 2) is currently ignored; category tables are not first-class.
    eval.category_manager
        .current_mut()
        .define_category(cat, &docstring)
        .map_err(|msg| signal("error", vec![Value::string(&msg)]))?;

    let _ = docstring;
    Ok(Value::Nil)
}

/// `(category-docstring CATEGORY &optional TABLE)` (evaluator-backed).
///
/// Returns the category docstring from the active table, or nil when absent.
pub(crate) fn builtin_category_docstring_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("category-docstring", &args, 1)?;
    expect_max_args("category-docstring", &args, 2)?;

    let cat = extract_char(&args[0], "category-docstring")?;
    // TABLE (arg 1) is currently ignored; category tables are not first-class.
    let _ = args.get(1);

    match eval.category_manager.current().category_docstring(cat) {
        Some(doc) => Ok(Value::string(doc)),
        None => Ok(Value::Nil),
    }
}

/// `(get-unused-category &optional TABLE)` (evaluator-backed).
///
/// Returns the first unused category letter in the active table, or nil.
pub(crate) fn builtin_get_unused_category_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("get-unused-category", &args, 1)?;
    // TABLE (arg 0) is currently ignored; category tables are not first-class.
    let _ = args.first();

    match eval.category_manager.current().get_unused_category() {
        Some(cat) => Ok(Value::Char(cat)),
        None => Ok(Value::Nil),
    }
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

/// `(category-table)` (evaluator-backed).
///
/// Return the current buffer's category table object.
pub(crate) fn builtin_category_table_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("category-table", &args, 0)?;
    current_buffer_category_table(eval)
}

/// `(standard-category-table)` (evaluator-backed).
///
/// Return the process-wide standard category table object.
pub(crate) fn builtin_standard_category_table_eval(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("standard-category-table", &args, 0)?;
    ensure_standard_category_table()
}

/// `(set-category-table TABLE)` (evaluator-backed).
///
/// Install TABLE in the current buffer and return the installed table.
pub(crate) fn builtin_set_category_table_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-category-table", &args, 1)?;

    let installed = if args[0].is_nil() {
        let current = current_buffer_category_table(eval)?;
        let standard = ensure_standard_category_table()?;
        if category_table_pointer_eq(&current, &standard) {
            standard
        } else {
            clone_char_table_object(&standard)?
        }
    } else {
        if !is_category_table_value(&args[0])? {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("category-table-p"), args[0].clone()],
            ));
        }
        args[0].clone()
    };

    set_current_buffer_category_table(eval, installed.clone())?;
    Ok(installed)
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn reset_pure_category_manager_for_tests() {
        PURE_CATEGORY_MANAGER.with(|slot| {
            *slot.borrow_mut() = CategoryManager::new();
        });
    }

    // -----------------------------------------------------------------------
    // CategoryTable
    // -----------------------------------------------------------------------

    #[test]
    fn new_table_is_empty() {
        let table = CategoryTable::new();
        assert!(table.entries.is_empty());
        assert!(table.descriptions.contains_key(&'1'));
    }

    #[test]
    fn define_category_stores_description() {
        let mut table = CategoryTable::new();
        table.define_category('a', "ASCII letters").unwrap();
        assert_eq!(table.category_docstring('a'), Some("ASCII letters"));
    }

    #[test]
    fn define_category_rejects_non_graphic() {
        let mut table = CategoryTable::new();
        assert!(table.define_category('1', "digits").is_err());
        assert!(table.define_category(' ', "space").is_err());
        assert!(table.define_category('\n', "newline").is_err());
    }

    #[test]
    fn define_category_accepts_upper_and_lower() {
        let mut table = CategoryTable::new();
        table.define_category('a', "lower a").unwrap();
        table.define_category('Z', "upper Z").unwrap();
        table.define_category('!', "bang").unwrap();
        assert_eq!(table.category_docstring('a'), Some("lower a"));
        assert_eq!(table.category_docstring('Z'), Some("upper Z"));
        assert_eq!(table.category_docstring('!'), Some("bang"));
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
    fn modify_entry_rejects_non_graphic() {
        let mut table = CategoryTable::new();
        assert!(table.modify_entry('X', ' ', false).is_err());
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
        mgr.current_mut().define_category('a', "test").unwrap();
        assert_eq!(mgr.current().category_docstring('a'), Some("test"));
    }

    #[test]
    fn manager_standard_and_current_are_same_initially() {
        let mut mgr = CategoryManager::new();
        mgr.standard_mut().define_category('z', "zed").unwrap();
        // Since current == standard, current should see it too.
        assert_eq!(mgr.current().category_docstring('z'), Some("zed"));
    }

    // -----------------------------------------------------------------------
    // Pure builtins
    // -----------------------------------------------------------------------

    #[test]
    fn builtin_define_category_basic() {
        reset_pure_category_manager_for_tests();
        let result =
            builtin_define_category(vec![Value::Char('a'), Value::string("ASCII letters")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
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
        let result = builtin_define_category(vec![Value::Char(' '), Value::string("space")]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_define_category_wrong_type_docstring() {
        let result = builtin_define_category(vec![Value::Char('a'), Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_category_docstring_basic() {
        reset_pure_category_manager_for_tests();
        builtin_define_category(vec![Value::Char('a'), Value::string("ASCII letters")]).unwrap();
        let result = builtin_category_docstring(vec![Value::Char('a')]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("ASCII letters"));
    }

    #[test]
    fn builtin_category_docstring_wrong_args() {
        assert!(builtin_category_docstring(vec![]).is_err());
        assert!(
            builtin_category_docstring(vec![Value::Char('a'), Value::Nil, Value::Nil,]).is_err()
        );
    }

    #[test]
    fn builtin_get_unused_category_returns_char() {
        reset_pure_category_manager_for_tests();
        let result = builtin_get_unused_category(vec![]).unwrap();
        assert!(matches!(result, Value::Char(_)));
    }

    #[test]
    fn builtin_get_unused_category_wrong_args() {
        assert!(builtin_get_unused_category(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_category_table_p_nil_for_t() {
        let result = builtin_category_table_p(vec![Value::True]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn builtin_category_table_p_true_for_category_char_table() {
        let table = builtin_make_category_table(vec![]).unwrap();
        let result = builtin_category_table_p(vec![table]).unwrap();
        assert!(result.is_truthy());
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
    fn builtin_category_table_returns_category_table() {
        let result = builtin_category_table(vec![]).unwrap();
        assert!(builtin_category_table_p(vec![result]).unwrap().is_truthy());
    }

    #[test]
    fn builtin_category_table_wrong_args() {
        assert!(builtin_category_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_standard_category_table_returns_category_table() {
        let result = builtin_standard_category_table(vec![]).unwrap();
        assert!(builtin_category_table_p(vec![result]).unwrap().is_truthy());
    }

    #[test]
    fn builtin_standard_category_table_wrong_args() {
        assert!(builtin_standard_category_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_make_category_table_returns_category_table() {
        let result = builtin_make_category_table(vec![]).unwrap();
        assert!(builtin_category_table_p(vec![result]).unwrap().is_truthy());
    }

    #[test]
    fn builtin_make_category_table_wrong_args() {
        assert!(builtin_make_category_table(vec![Value::Nil]).is_err());
    }

    #[test]
    fn builtin_set_category_table_returns_arg() {
        let table = builtin_make_category_table(vec![]).unwrap();
        let result = builtin_set_category_table(vec![table.clone()]).unwrap();
        assert!(equal_value(&result, &table, 0));
    }

    #[test]
    fn builtin_set_category_table_nil_returns_standard() {
        let result = builtin_set_category_table(vec![Value::Nil]).unwrap();
        let standard = builtin_standard_category_table(vec![]).unwrap();
        assert!(equal_value(&result, &standard, 0));
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
            let vec = with_heap(|h| h.get_vector(*arc).clone());
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
            let vec = with_heap(|h| h.get_vector(*arc).clone());
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
            let vec = with_heap(|h| h.get_vector(*arc).clone());
            // 'A' = 65, 'Z' = 90
            assert!(matches!(&vec[2 + 65], Value::Int(1)));
            assert!(matches!(&vec[2 + 90], Value::Int(1)));
            assert!(matches!(&vec[2 + 66], Value::Int(0)));
        } else {
            panic!("Expected vector result");
        }
    }

    #[test]
    fn builtin_make_category_set_includes_ascii_graphic_symbols() {
        let result = builtin_make_category_set(vec![Value::string("a1b!c")]).unwrap();
        if let Value::Vector(arc) = &result {
            let vec = with_heap(|h| h.get_vector(*arc).clone());
            // 'a', '1', 'b', '!', 'c' all set.
            assert!(matches!(&vec[2 + 97], Value::Int(1))); // 'a'
            assert!(matches!(&vec[2 + 98], Value::Int(1))); // 'b'
            assert!(matches!(&vec[2 + 99], Value::Int(1))); // 'c'
            assert!(matches!(&vec[2 + 49], Value::Int(1))); // '1'
            assert!(matches!(&vec[2 + 33], Value::Int(1))); // '!'
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
        assert!(builtin_make_category_set(vec![Value::string("a"), Value::string("b"),]).is_err());
    }

    #[test]
    fn builtin_category_set_mnemonics_round_trip() {
        let set = builtin_make_category_set(vec![Value::string("a1!")]).unwrap();
        assert_eq!(
            builtin_category_set_mnemonics(vec![set]).unwrap(),
            Value::string("!1a")
        );
    }

    #[test]
    fn builtin_category_set_mnemonics_rejects_non_category_set() {
        let nil_err = builtin_category_set_mnemonics(vec![Value::Nil]).unwrap_err();
        match nil_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("categorysetp"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument, got {other:?}"),
        }

        let non_category_set = Value::vector(vec![
            Value::symbol("--bool-vector--"),
            Value::Int(2),
            Value::Int(1),
            Value::Int(0),
        ]);
        let short_err = builtin_category_set_mnemonics(vec![non_category_set.clone()]).unwrap_err();
        match short_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("categorysetp"), non_category_set]
                );
            }
            other => panic!("expected wrong-type-argument, got {other:?}"),
        }
    }

    #[test]
    fn builtin_category_set_mnemonics_wrong_args() {
        assert!(builtin_category_set_mnemonics(vec![]).is_err());
        assert!(builtin_category_set_mnemonics(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn builtin_define_category_eval_sets_docstring() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_define_category_eval(
            &mut eval,
            vec![Value::Char('Z'), Value::string("neovm-category-doc")],
        )
        .unwrap();
        assert!(result.is_nil());

        let doc = builtin_category_docstring_eval(&mut eval, vec![Value::Char('Z')]).unwrap();
        assert_eq!(doc.as_str(), Some("neovm-category-doc"));
    }

    #[test]
    fn builtin_get_unused_category_eval_tracks_defined_values() {
        let mut eval = super::super::eval::Evaluator::new();
        let first = builtin_get_unused_category_eval(&mut eval, vec![]).unwrap();
        assert!(matches!(first, Value::Char('a')));

        builtin_define_category_eval(&mut eval, vec![Value::Char('a'), Value::string("used")])
            .unwrap();
        let second = builtin_get_unused_category_eval(&mut eval, vec![]).unwrap();
        assert!(matches!(second, Value::Char('b')));
    }

    #[test]
    fn builtin_category_table_eval_defaults_to_standard() {
        let mut eval = super::super::eval::Evaluator::new();
        let current = builtin_category_table_eval(&mut eval, vec![]).unwrap();
        let standard = builtin_standard_category_table_eval(&mut eval, vec![]).unwrap();
        assert!(equal_value(&current, &standard, 0));
    }

    #[test]
    fn builtin_set_category_table_eval_roundtrip() {
        let mut eval = super::super::eval::Evaluator::new();
        let custom = builtin_make_category_table(vec![]).unwrap();

        let out = builtin_set_category_table_eval(&mut eval, vec![custom.clone()]).unwrap();
        assert!(equal_value(&out, &custom, 0));

        let current = builtin_category_table_eval(&mut eval, vec![]).unwrap();
        assert!(equal_value(&current, &custom, 0));
    }

    #[test]
    fn builtin_set_category_table_eval_nil_after_custom_clones_standard() {
        let mut eval = super::super::eval::Evaluator::new();
        let standard = builtin_standard_category_table_eval(&mut eval, vec![]).unwrap();
        let custom = builtin_make_category_table(vec![]).unwrap();

        builtin_set_category_table_eval(&mut eval, vec![custom]).unwrap();
        let restored = builtin_set_category_table_eval(&mut eval, vec![Value::Nil]).unwrap();

        assert!(builtin_category_table_p(vec![restored.clone()])
            .unwrap()
            .is_truthy());
        assert!(!category_table_pointer_eq(&restored, &standard));
    }

    #[test]
    fn builtin_set_category_table_eval_rejects_non_tables() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_set_category_table_eval(&mut eval, vec![Value::Int(1)]);
        assert!(result.is_err());
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
        assert!(is_category_letter('0'));
        assert!(is_category_letter('9'));
        assert!(is_category_letter('!'));
    }

    #[test]
    fn is_category_letter_invalid() {
        assert!(!is_category_letter(' '));
        assert!(!is_category_letter('\n'));
        assert!(!is_category_letter('é'));
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
