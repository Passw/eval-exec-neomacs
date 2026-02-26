//! Oracle parity tests for `point-max`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_point_max_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_point_max_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(point-max)");
    assert_ok_eq("1", &oracle, &neovm);
}

#[test]
fn oracle_prop_point_max_wrong_arity_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_point_max_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(point-max nil)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_point_max_tracks_buffer_end(
        len in 0usize..20usize,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let content = "x".repeat(len);
        let form = format!(
            "(progn (erase-buffer) (insert \"{}\") (point-max))",
            content
        );
        let expected = (len + 1).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
