//! Oracle parity tests for `point-min`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_point_min_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_point_min_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(point-min)");
    assert_ok_eq("1", &oracle, &neovm);
}

#[test]
fn oracle_prop_point_min_wrong_arity_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_point_min_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(point-min nil)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_point_min_is_stable(
        pos in 1usize..27usize,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn (erase-buffer) (insert \"abcdefghijklmnopqrstuvwxyz\") (goto-char {}) (point-min))",
            pos
        );
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq("1", &oracle, &neovm);
    }
}
