//! Oracle parity tests for `nthcdr`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_nthcdr_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_nthcdr_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (o, n) = eval_oracle_and_neovm("(nthcdr 2 '(a b c d e))");
    assert_ok_eq("(c d e)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nthcdr 0 '(10 20 30))");
    assert_ok_eq("(10 20 30)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nthcdr 5 '(1 2))");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nthcdr 0 nil)");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nthcdr 1 '(solo))");
    assert_ok_eq("nil", &o, &n);
}

#[test]
fn oracle_prop_nthcdr_wrong_type() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_nthcdr_wrong_type: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(nthcdr 'x '(1 2))");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_nthcdr_random_offset(
        offset in 0i64..6i64,
        a in -500i64..500i64,
        b in -500i64..500i64,
        c in -500i64..500i64,
        d in -500i64..500i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(nthcdr {} (list {} {} {} {}))", offset, a, b, c, d);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_eq!(neovm, oracle, "nthcdr parity failed for: {form}");
    }
}
