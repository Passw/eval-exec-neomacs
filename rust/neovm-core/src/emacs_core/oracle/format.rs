//! Oracle parity tests for `format`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_format_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_format_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(format "%s-%d" "a" 3)"#);
    assert_ok_eq("\"a-3\"", &oracle, &neovm);
}

#[test]
fn oracle_prop_format_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_format_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(format 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_format_two_ints(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(r#"(format "%d:%d" {} {})"#, a, b);
        let expected = format!("\"{}:{}\"", a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
