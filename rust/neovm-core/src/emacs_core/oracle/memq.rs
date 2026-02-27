//! Oracle parity tests for `memq`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_memq_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_memq_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_found, neovm_found) = eval_oracle_and_neovm("(memq 'b '(a b c))");
    assert_ok_eq("(b c)", &oracle_found, &neovm_found);

    let (oracle_missing, neovm_missing) = eval_oracle_and_neovm("(memq 'z '(a b c))");
    assert_ok_eq("nil", &oracle_missing, &neovm_missing);
}

#[test]
fn oracle_prop_memq_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_memq_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(memq 'a 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_memq_head_match(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(memq {} (list {} {}))", a, a, b);
        let expected = format!("({} {})", a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
