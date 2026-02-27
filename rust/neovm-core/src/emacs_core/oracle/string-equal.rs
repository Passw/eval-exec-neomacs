//! Oracle parity tests for `string-equal`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, oracle_prop_enabled,
    ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_string_equal_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_string_equal_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-equal "a")"#);
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

#[test]
fn oracle_prop_string_equal_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_string_equal_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-equal "a" 1)"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_string_equal_alias_smoke() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_string_equal_alias_smoke: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    assert_oracle_parity(r#"(string= "abc" "abc")"#);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_string_equal_operator(
        a in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
        b in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let expected = if a == b { "t" } else { "nil" };
        let form = format!("(string-equal {:?} {:?})", a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected, &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_string_equal_alias_operator(
        a in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
        b in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let expected = if a == b { "t" } else { "nil" };
        let form = format!("(string= {:?} {:?})", a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected, &oracle, &neovm);
    }
}
