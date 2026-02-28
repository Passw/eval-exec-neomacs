//! Oracle parity tests for `point`.

use proptest::prelude::*;

use super::common::{assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_point_basics() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm("(point)");
    assert_ok_eq("1", &oracle, &neovm);
}

#[test]
fn oracle_prop_point_wrong_arity_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm("(point nil)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_point_after_goto_char(
        pos in 1usize..27usize,
    ) {
        crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn (erase-buffer) (insert \"abcdefghijklmnopqrstuvwxyz\") (goto-char {}) (point))",
            pos
        );
        let expected = pos.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
