use super::*;

#[test]
fn file_user_uid_matches_user_uid() {
    let user_uid = builtin_user_uid(vec![]).expect("user-uid should succeed");
    let file_user_uid = builtin_file_user_uid(vec![]).expect("file-user-uid should succeed");
    assert_eq!(file_user_uid, user_uid);
    assert!(matches!(file_user_uid, Value::Int(_)));
}

#[test]
fn file_user_uid_arity_errors() {
    assert!(builtin_file_user_uid(vec![Value::Nil]).is_err());
}

#[test]
fn file_group_gid_matches_group_gid() {
    let group_gid = builtin_group_gid(vec![]).expect("group-gid should succeed");
    let file_group_gid = builtin_file_group_gid(vec![]).expect("file-group-gid should succeed");
    assert_eq!(file_group_gid, group_gid);
    assert!(matches!(file_group_gid, Value::Int(_)));
}

#[test]
fn file_group_gid_arity_errors() {
    assert!(builtin_file_group_gid(vec![Value::Nil]).is_err());
}

// -- expect_args / expect_min_args / expect_max_args ----------------------

#[test]
fn expect_args_exact_match() {
    assert!(expect_args("test", &[Value::Nil, Value::Nil], 2).is_ok());
}

#[test]
fn expect_args_wrong_count() {
    let err = expect_args("test", &[Value::Nil], 2);
    assert!(err.is_err());
}

#[test]
fn expect_min_args_at_min() {
    assert!(expect_min_args("test", &[Value::Nil], 1).is_ok());
}

#[test]
fn expect_min_args_below_min() {
    assert!(expect_min_args("test", &[], 1).is_err());
}

#[test]
fn expect_max_args_at_max() {
    assert!(expect_max_args("test", &[Value::Nil, Value::Nil], 2).is_ok());
}

#[test]
fn expect_max_args_above_max() {
    assert!(expect_max_args("test", &[Value::Nil, Value::Nil, Value::Nil], 2).is_err());
}

// -- expect_integer -------------------------------------------------------

#[test]
fn expect_integer_from_int() {
    assert_eq!(expect_integer("test", &Value::Int(42)).unwrap(), 42);
}

#[test]
fn expect_integer_from_non_int() {
    assert!(expect_integer("test", &Value::Nil).is_err());
}

// -- collect_insert_text --------------------------------------------------

#[test]
fn collect_insert_text_strings_and_chars() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);
    let mut heap = Box::new(crate::gc::heap::LispHeap::new());
    super::super::value::set_current_heap(&mut heap);

    let s = Value::string("hello");
    let c = Value::Char('!');
    let result = collect_insert_text("insert", &[s, c]).unwrap();
    assert_eq!(result, "hello!");
}

#[test]
fn collect_insert_text_int_as_char() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);
    let mut heap = Box::new(crate::gc::heap::LispHeap::new());
    super::super::value::set_current_heap(&mut heap);

    // ASCII 65 = 'A'
    let result = collect_insert_text("insert", &[Value::Int(65)]).unwrap();
    assert_eq!(result, "A");
}

#[test]
fn collect_insert_text_wrong_type() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);
    let mut heap = Box::new(crate::gc::heap::LispHeap::new());
    super::super::value::set_current_heap(&mut heap);

    assert!(collect_insert_text("insert", &[Value::Nil]).is_err());
}

// -- builtin_logcount -----------------------------------------------------

#[test]
fn logcount_positive() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);

    // 7 = 0b111 → 3 bits
    let result = builtin_logcount(vec![Value::Int(7)]).unwrap();
    assert_eq!(result, Value::Int(3));
}

#[test]
fn logcount_zero() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);

    let result = builtin_logcount(vec![Value::Int(0)]).unwrap();
    assert_eq!(result, Value::Int(0));
}

#[test]
fn logcount_negative() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);

    // -1 = all 1s → !(-1) = 0 → count_ones = 0
    let result = builtin_logcount(vec![Value::Int(-1)]).unwrap();
    assert_eq!(result, Value::Int(0));

    // -2 = ...1110 → !(-2) = 1 → count_ones = 1
    let result = builtin_logcount(vec![Value::Int(-2)]).unwrap();
    assert_eq!(result, Value::Int(1));
}

#[test]
fn logcount_wrong_type() {
    use super::super::intern::StringInterner;
    let mut interner = Box::new(StringInterner::new());
    super::super::intern::set_current_interner(&mut interner);

    assert!(builtin_logcount(vec![Value::Nil]).is_err());
}
