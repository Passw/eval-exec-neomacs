//! Oracle parity tests for `not`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_not_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_not_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_t, neovm_t) = eval_oracle_and_neovm("(not nil)");
    assert_ok_eq("t", &oracle_t, &neovm_t);

    let (oracle_nil, neovm_nil) = eval_oracle_and_neovm("(not 1)");
    assert_ok_eq("nil", &oracle_nil, &neovm_nil);
}

#[test]
fn oracle_prop_not_wrong_arity_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_not_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(not)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_not_boolean_behavior(
        cond in any::<bool>(),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let arg = if cond { "t" } else { "nil" };
        let expected = if cond { "nil" } else { "t" };
        let form = format!("(not {})", arg);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected, &oracle, &neovm);
    }
}
