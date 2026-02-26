//! Oracle parity tests for `let`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_let_scoping_basic() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_let_scoping_basic: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 1)) (let ((x 2)) x))");
    assert_ok_eq("2", &oracle, &neovm);
}

#[test]
fn oracle_prop_let_parallel_binding_error() {
    if !oracle_prop_enabled() {
        eprintln!(
            "skipping oracle_prop_let_parallel_binding_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 1) (y (+ x 2))) y)");
    assert_err_kind(&oracle, &neovm, "void-variable");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_let_returns_bound_value(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(let ((x {})) x)", a);
        let expected = a.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
