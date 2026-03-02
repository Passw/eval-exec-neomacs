use super::*;

// -----------------------------------------------------------------------
// AbbrevManager unit tests
// -----------------------------------------------------------------------

#[test]
fn define_and_expand() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev("global-abbrev-table", "btw", "by the way");

    let result = mgr.expand_abbrev("global-abbrev-table", "btw");
    assert_eq!(result, Some("by the way".to_string()));

    // Check count incremented
    let tbl = mgr.get_table("global-abbrev-table").unwrap();
    assert_eq!(tbl.abbrevs.get("btw").unwrap().count, 1);

    // Expand again
    let result = mgr.expand_abbrev("global-abbrev-table", "btw");
    assert_eq!(result, Some("by the way".to_string()));

    let tbl = mgr.get_table("global-abbrev-table").unwrap();
    assert_eq!(tbl.abbrevs.get("btw").unwrap().count, 2);
}

#[test]
fn expand_nonexistent() {
    let mut mgr = AbbrevManager::new();
    let result = mgr.expand_abbrev("global-abbrev-table", "nope");
    assert!(result.is_none());
}

#[test]
fn case_insensitive_lookup() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev("global-abbrev-table", "BTW", "by the way");

    // Stored as lowercase key "btw"
    let result = mgr.expand_abbrev("global-abbrev-table", "btw");
    assert_eq!(result, Some("by the way".to_string()));

    let result = mgr.expand_abbrev("global-abbrev-table", "BTW");
    // All-uppercase input -> all-uppercase expansion
    assert_eq!(result, Some("BY THE WAY".to_string()));
}

#[test]
fn case_capitalized() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev("global-abbrev-table", "btw", "by the way");

    // Capitalized input -> capitalized expansion
    let result = mgr.expand_abbrev("global-abbrev-table", "Btw");
    assert_eq!(result, Some("By the way".to_string()));
}

#[test]
fn case_fixed() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev("global-abbrev-table", "btw", "by the way");
    mgr.tables
        .get_mut("global-abbrev-table")
        .unwrap()
        .case_fixed = true;

    // With case_fixed, expansion is returned verbatim regardless of input case
    let result = mgr.expand_abbrev("global-abbrev-table", "BTW");
    assert_eq!(result, Some("by the way".to_string()));
}

#[test]
fn table_inheritance() {
    let mut mgr = AbbrevManager::new();

    // Define in global
    mgr.define_abbrev("global-abbrev-table", "btw", "by the way");

    // Create a child table with parent
    let child = mgr.create_table("lisp-mode-abbrev-table");
    child.parent = Some("global-abbrev-table".to_string());

    // Define a local abbrev in child
    mgr.define_abbrev("lisp-mode-abbrev-table", "df", "defun");

    // Child table can find its own abbrevs
    let result = mgr.expand_abbrev("lisp-mode-abbrev-table", "df");
    assert_eq!(result, Some("defun".to_string()));

    // Child table inherits from parent
    let result = mgr.expand_abbrev("lisp-mode-abbrev-table", "btw");
    assert_eq!(result, Some("by the way".to_string()));
}

#[test]
fn fallback_to_global() {
    let mut mgr = AbbrevManager::new();

    mgr.define_abbrev("global-abbrev-table", "teh", "the");
    mgr.create_table("text-mode-abbrev-table");
    // No parent set, but should still fall back to global
    let result = mgr.expand_abbrev("text-mode-abbrev-table", "teh");
    assert_eq!(result, Some("the".to_string()));
}

#[test]
fn list_abbrevs_sorted() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev("global-abbrev-table", "zz", "sleep");
    mgr.define_abbrev("global-abbrev-table", "aa", "alpha");
    mgr.define_abbrev("global-abbrev-table", "mm", "middle");

    let list = mgr.list_abbrevs("global-abbrev-table");
    assert_eq!(list.len(), 3);
    assert_eq!(list[0], ("aa", "alpha"));
    assert_eq!(list[1], ("mm", "middle"));
    assert_eq!(list[2], ("zz", "sleep"));
}

#[test]
fn list_abbrevs_nonexistent_table() {
    let mgr = AbbrevManager::new();
    let list = mgr.list_abbrevs("no-such-table");
    assert!(list.is_empty());
}

#[test]
fn clear_table() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev("global-abbrev-table", "a", "alpha");
    mgr.define_abbrev("global-abbrev-table", "b", "beta");
    assert_eq!(mgr.list_abbrevs("global-abbrev-table").len(), 2);

    mgr.clear_table("global-abbrev-table");
    assert_eq!(mgr.list_abbrevs("global-abbrev-table").len(), 0);
}

#[test]
fn enable_disable() {
    let mut mgr = AbbrevManager::new();
    assert!(!mgr.is_enabled());

    mgr.set_enabled(true);
    assert!(mgr.is_enabled());

    mgr.set_enabled(false);
    assert!(!mgr.is_enabled());
}

#[test]
fn define_abbrev_full_with_hook_and_system() {
    let mut mgr = AbbrevManager::new();
    mgr.define_abbrev_full(
        "global-abbrev-table",
        "hw",
        "hello world",
        Some("my-hook".to_string()),
        true,
    );

    let tbl = mgr.get_table("global-abbrev-table").unwrap();
    let ab = tbl.abbrevs.get("hw").unwrap();
    assert_eq!(ab.expansion, "hello world");
    assert_eq!(ab.hook.as_deref(), Some("my-hook"));
    assert!(ab.system);
    assert_eq!(ab.count, 0);
}

#[test]
fn all_table_names() {
    let mut mgr = AbbrevManager::new();
    mgr.create_table("z-table");
    mgr.create_table("a-table");

    let names = mgr.all_table_names();
    // Should include global + the two we created, sorted
    assert!(names.contains(&"a-table"));
    assert!(names.contains(&"global-abbrev-table"));
    assert!(names.contains(&"z-table"));
    // Verify sorting
    for i in 1..names.len() {
        assert!(names[i - 1] <= names[i]);
    }
}

// -----------------------------------------------------------------------
// apply_case unit tests
// -----------------------------------------------------------------------

#[test]
fn test_apply_case() {
    // Lowercase word -> as-is
    assert_eq!(apply_case("hello world", "hw", false), "hello world");

    // Capitalized word -> capitalize expansion
    assert_eq!(apply_case("hello world", "Hw", false), "Hello world");

    // All-uppercase word -> uppercase expansion
    assert_eq!(apply_case("hello world", "HW", false), "HELLO WORLD");

    // case_fixed -> always as-is
    assert_eq!(apply_case("hello world", "HW", true), "hello world");

    // Empty word/expansion
    assert_eq!(apply_case("", "HW", false), "");
    assert_eq!(apply_case("hello", "", false), "hello");
}

// -----------------------------------------------------------------------
// Builtin-level tests
// -----------------------------------------------------------------------

#[test]
fn test_builtin_define_and_expand() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    // define-abbrev
    let result = builtin_define_abbrev(
        &mut eval,
        vec![
            Value::string("global-abbrev-table"),
            Value::string("btw"),
            Value::string("by the way"),
        ],
    );
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());

    // Manager expansion behavior is exercised directly.
    let expanded = eval.abbrevs.expand_abbrev("global-abbrev-table", "btw");
    assert_eq!(expanded.as_deref(), Some("by the way"));

    // expand nonexistent
    let expanded = eval.abbrevs.expand_abbrev("global-abbrev-table", "xyz");
    assert!(expanded.is_none());
}

#[test]
fn test_builtin_abbrev_mode() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    // Initially off
    assert!(!eval.abbrevs.is_enabled());

    // Toggle on
    let result = builtin_abbrev_mode(&mut eval, vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
    assert!(eval.abbrevs.is_enabled());

    // Toggle off
    let result = builtin_abbrev_mode(&mut eval, vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
    assert!(!eval.abbrevs.is_enabled());

    // Explicit enable
    let result = builtin_abbrev_mode(&mut eval, vec![Value::Int(1)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());

    // Explicit disable
    let result = builtin_abbrev_mode(&mut eval, vec![Value::Int(0)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_builtin_define_abbrev_table() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    // Create a table without parent
    let result = builtin_define_abbrev_table(
        &mut eval,
        vec![Value::string("my-mode-abbrev-table"), Value::Nil],
    );
    assert!(result.is_ok());
    assert!(eval.abbrevs.get_table("my-mode-abbrev-table").is_some());

    // Create a table with parent
    let result = builtin_define_abbrev_table(
        &mut eval,
        vec![
            Value::string("child-table"),
            Value::string("my-mode-abbrev-table"),
        ],
    );
    assert!(result.is_ok());
    let child = eval.abbrevs.get_table("child-table").unwrap();
    assert_eq!(child.parent.as_deref(), Some("my-mode-abbrev-table"));

    // DEFS list form should be accepted for compatibility (currently ignored).
    let result = builtin_define_abbrev_table(
        &mut eval,
        vec![
            Value::string("defs-table"),
            Value::list(vec![Value::list(vec![
                Value::string("hw"),
                Value::string("hello world"),
            ])]),
        ],
    );
    assert!(result.is_ok());
    assert!(eval.abbrevs.get_table("defs-table").is_some());

    // Symbol docstring + trailing arg should be treated as property list
    // head and therefore remain accepted.
    let result = builtin_define_abbrev_table(
        &mut eval,
        vec![
            Value::string("props-table"),
            Value::Nil,
            Value::symbol(":foo"),
            Value::True,
        ],
    );
    assert!(result.is_ok());

    // Nil docstring with one trailing property key must signal like GNU Emacs.
    let result = builtin_define_abbrev_table(
        &mut eval,
        vec![
            Value::string("missing-prop-value-table"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ],
    )
    .expect_err("define-abbrev-table should reject missing property values");
    match result {
        Flow::Signal(sig) => {
            assert_eq!(sig.symbol_name(), "error");
            assert_eq!(
                sig.data,
                vec![Value::string("Missing value for property nil")]
            );
        }
        other => panic!("unexpected flow: {other:?}"),
    }
}

#[test]
fn test_builtin_clear_abbrev_table() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    // Define some abbrevs
    builtin_define_abbrev(
        &mut eval,
        vec![
            Value::string("global-abbrev-table"),
            Value::string("a"),
            Value::string("alpha"),
        ],
    )
    .unwrap();

    // Clear
    let result =
        builtin_clear_abbrev_table(&mut eval, vec![Value::string("global-abbrev-table")]);
    assert!(result.is_ok());

    // Verify empty
    let entries = eval.abbrevs.list_abbrevs("global-abbrev-table");
    assert!(entries.is_empty());
}

#[test]
fn test_builtin_abbrev_expansion() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    eval.abbrevs
        .define_abbrev("global-abbrev-table", "teh", "the");

    // Look up expansion without expanding (count should not change)
    let result = builtin_abbrev_expansion(&mut eval, vec![Value::string("teh")]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some("the"));

    // Verify count was NOT incremented
    let tbl = eval.abbrevs.get_table("global-abbrev-table").unwrap();
    assert_eq!(tbl.abbrevs.get("teh").unwrap().count, 0);

    // Look up in specific table
    let result = builtin_abbrev_expansion(
        &mut eval,
        vec![Value::string("teh"), Value::string("global-abbrev-table")],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some("the"));

    // Nonexistent
    let result = builtin_abbrev_expansion(&mut eval, vec![Value::string("xyz")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_builtin_abbrev_table_p() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    let result = builtin_abbrev_table_p(&mut eval, vec![Value::string("global-abbrev-table")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());

    let result = builtin_abbrev_table_p(&mut eval, vec![Value::string("no-such-table")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_builtin_insert_abbrev_table_description() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    // Empty table
    let result = builtin_insert_abbrev_table_description(
        &mut eval,
        vec![Value::string("global-abbrev-table")],
    );
    assert!(result.is_ok());
    let desc = result.unwrap();
    assert!(desc.as_str().unwrap().contains("define-abbrev-table"));

    // With entries
    eval.abbrevs
        .define_abbrev("global-abbrev-table", "hw", "hello world");
    let result = builtin_insert_abbrev_table_description(
        &mut eval,
        vec![Value::string("global-abbrev-table")],
    );
    assert!(result.is_ok());
    let desc = result.unwrap();
    assert!(desc.as_str().unwrap().contains("hw"));
    assert!(desc.as_str().unwrap().contains("hello world"));
}

#[test]
fn test_builtin_define_abbrev_with_hook() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    let result = builtin_define_abbrev(
        &mut eval,
        vec![
            Value::string("global-abbrev-table"),
            Value::string("hw"),
            Value::string("hello world"),
            Value::string("my-hook-fn"),
            Value::True, // system
        ],
    );
    assert!(result.is_ok());

    let tbl = eval.abbrevs.get_table("global-abbrev-table").unwrap();
    let ab = tbl.abbrevs.get("hw").unwrap();
    assert_eq!(ab.hook.as_deref(), Some("my-hook-fn"));
    assert!(ab.system);
}

#[test]
fn test_wrong_arg_count() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();

    // define-abbrev needs at least 3 args
    let result = builtin_define_abbrev(&mut eval, vec![Value::string("t"), Value::string("a")]);
    assert!(result.is_err());

    // define-abbrev-table needs at least 2 args
    let result = builtin_define_abbrev_table(&mut eval, vec![Value::string("my-table")]);
    assert!(result.is_err());

    // expand-abbrev needs exactly 0 args
    let result = builtin_expand_abbrev(&mut eval, vec![Value::string("t")]);
    assert!(result.is_err());
    let result = builtin_expand_abbrev(&mut eval, vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());

    // clear-abbrev-table needs exactly 1
    let result = builtin_clear_abbrev_table(&mut eval, vec![]);
    assert!(result.is_err());
}
