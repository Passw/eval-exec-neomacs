//! Oracle parity tests for `forward-line`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_forward_line_basics() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm(
        "(progn (erase-buffer) (insert \"a\\nb\\nc\\n\") (goto-char 1) (forward-line 1))",
    );
    assert_ok_eq("0", &oracle, &neovm);

    assert_oracle_parity(
        "(progn (erase-buffer) (insert \"a\\nb\\nc\\n\") (goto-char 1) (forward-line 10))",
    );
}

#[test]
fn oracle_prop_forward_line_wrong_type_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(forward-line "x")"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_forward_line_remainder_and_point(
        n in -10i64..10i64,
    ) {
        crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn (erase-buffer) (insert \"a\\nb\\nc\\n\") (goto-char 1) (list (forward-line {}) (point)))",
            n
        );
        assert_oracle_parity(&form);
    }
}
