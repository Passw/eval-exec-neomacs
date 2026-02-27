//! Oracle parity tests for `throw`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_throw_to_matching_catch() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_throw_to_matching_catch: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(catch 'tag (throw 'tag 99))");
    assert_ok_eq("99", &oracle, &neovm);
}

#[test]
fn oracle_prop_throw_without_catch_errors() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_throw_without_catch_errors: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(throw 'tag 1)");
    assert_err_kind(&oracle, &neovm, "no-catch");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_throw_returns_caught_value(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(catch 'tag (throw 'tag {}))", a);
        let expected = a.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
