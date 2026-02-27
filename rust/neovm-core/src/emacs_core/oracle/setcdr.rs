//! Oracle parity tests for `setcdr`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_setcdr_basic() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_setcdr_basic: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x (cons 1 2))) (setcdr x 9) x)");
    assert_ok_eq("(1 . 9)", &oracle, &neovm);
}

#[test]
fn oracle_prop_setcdr_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_setcdr_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(setcdr 1 2)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_setcdr_updates_tail(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(let ((x (cons {} {}))) (setcdr x {}) x)", a, b, c);
        let expected = format!("({} . {})", a, c);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
