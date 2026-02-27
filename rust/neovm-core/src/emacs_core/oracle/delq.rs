//! Oracle parity tests for `delq`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_delq_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_delq_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (o, n) = eval_oracle_and_neovm("(delq 3 '(1 3 5 3 7))");
    assert_ok_eq("(1 5 7)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(delq 'x '(x x x))");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(delq 99 '(10 20 30))");
    assert_ok_eq("(10 20 30)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(delq 5 nil)");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(delq 'a '(b c a d a e))");
    assert_ok_eq("(b c d e)", &o, &n);
}

#[test]
fn oracle_prop_delq_wrong_type() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_delq_wrong_type: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(delq 1 42)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_delq_integer_removal(
        target in -200i64..200i64,
        a in -200i64..200i64,
        b in -200i64..200i64,
        c in -200i64..200i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(delq {} (list {} {} {}))", target, a, b, c);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_eq!(neovm, oracle, "delq parity failed for: {form}");
    }
}
