//! Oracle parity tests for `setq`.

use proptest::prelude::*;

use super::common::{assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_setq_basic() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 1)) (setq x 2) x)");
    assert_ok_eq("2", &oracle, &neovm);
}

#[test]
fn oracle_prop_setq_odd_arity_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm("(setq x)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_setq_updates_value(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!("(let ((x {})) (setq x {}) x)", a, b);
        let expected = b.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
