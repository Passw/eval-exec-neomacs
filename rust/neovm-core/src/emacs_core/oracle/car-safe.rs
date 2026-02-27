//! Oracle parity tests for `car-safe`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_car_safe_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_car_safe_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_cons, neovm_cons) = eval_oracle_and_neovm("(car-safe (cons 1 2))");
    assert_ok_eq("1", &oracle_cons, &neovm_cons);

    let (oracle_atom, neovm_atom) = eval_oracle_and_neovm("(car-safe 1)");
    assert_ok_eq("nil", &oracle_atom, &neovm_atom);
}

#[test]
fn oracle_prop_car_safe_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_car_safe_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

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
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(car-safe (cons {} {}))", a, b);
        let expected = a.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
