//! Oracle parity tests for `insert`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_insert_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_insert_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(progn (erase-buffer) (insert \"ab\" 99) (buffer-string))");
    assert_ok_eq("\"abc\"", &oracle, &neovm);
}

#[test]
fn oracle_prop_insert_wrong_type_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_insert_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(insert '(1 2))");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_insert_two_ascii_ints(
        a in b'a'..=b'z',
        b in b'a'..=b'z',
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn (erase-buffer) (insert {} {}) (buffer-string))",
            a, b
        );
        let expected = format!("\"{}{}\"", a as char, b as char);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
