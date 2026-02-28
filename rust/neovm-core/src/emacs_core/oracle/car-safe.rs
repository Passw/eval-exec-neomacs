//! Oracle parity tests for `car-safe`.

use proptest::prelude::*;

use super::common::{assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_car_safe_basics() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle_cons, neovm_cons) = eval_oracle_and_neovm("(car-safe (cons 1 2))");
    assert_ok_eq("1", &oracle_cons, &neovm_cons);

    let (oracle_atom, neovm_atom) = eval_oracle_and_neovm("(car-safe 1)");
    assert_ok_eq("nil", &oracle_atom, &neovm_atom);
}

#[test]
fn oracle_prop_car_safe_wrong_arity_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm("(car-safe)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_car_safe_cons_head(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!("(car-safe (cons {} {}))", a, b);
        let expected = a.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
