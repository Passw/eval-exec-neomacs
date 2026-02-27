//! Oracle parity tests for `buffer-string`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_buffer_string_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_buffer_string_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(progn (erase-buffer) (insert "abc") (buffer-string))"#);
    assert_ok_eq("\"abc\"", &oracle, &neovm);
}

#[test]
fn oracle_prop_buffer_string_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_buffer_string_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(buffer-string nil)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_buffer_string_roundtrip(
        s in proptest::string::string_regex(r"[a-z0-9 ]{0,20}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn (erase-buffer) (insert {:?}) (buffer-string))",
            s
        );
        let expected = format!("{:?}", s);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
