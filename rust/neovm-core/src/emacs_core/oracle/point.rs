//! Oracle parity tests for `point`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_point_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_point_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(point)");
    assert_ok_eq("1", &oracle, &neovm);
}

#[test]
fn oracle_prop_point_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_point_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(point nil)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_point_after_goto_char(
        pos in 1usize..27usize,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn (erase-buffer) (insert \"abcdefghijklmnopqrstuvwxyz\") (goto-char {}) (point))",
            pos
        );
        let expected = pos.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
