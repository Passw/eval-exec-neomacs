//! Oracle parity tests for `string-to-number`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_string_to_number_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_string_to_number_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_decimal, neovm_decimal) = eval_oracle_and_neovm(r#"(string-to-number "42")"#);
    assert_ok_eq("42", &oracle_decimal, &neovm_decimal);

    let (oracle_hex, neovm_hex) = eval_oracle_and_neovm(r#"(string-to-number "ff" 16)"#);
    assert_ok_eq("255", &oracle_hex, &neovm_hex);
}

#[test]
fn oracle_prop_string_to_number_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_string_to_number_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(string-to-number 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_string_to_number_decimal_roundtrip(
        n in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let input = n.to_string();
        let form = format!(r#"(string-to-number "{}")"#, input);
        let expected = n.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
