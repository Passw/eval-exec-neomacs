//! Oracle parity tests for `forward-char`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, oracle_prop_enabled,
    ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_forward_char_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_forward_char_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) =
        eval_oracle_and_neovm("(progn (erase-buffer) (insert \"abcd\") (goto-char 1) (forward-char 2) (point))");
    assert_ok_eq("3", &oracle, &neovm);
}

#[test]
fn oracle_prop_forward_char_error_cases() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_forward_char_error_cases: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (type_oracle, type_neovm) = eval_oracle_and_neovm(r#"(forward-char "x")"#);
    assert_err_kind(&type_oracle, &type_neovm, "wrong-type-argument");

    let (eob_oracle, eob_neovm) =
        eval_oracle_and_neovm("(progn (erase-buffer) (insert \"a\") (goto-char 1) (forward-char 10))");
    assert_err_kind(&eob_oracle, &eob_neovm, "end-of-buffer");

    let (bob_oracle, bob_neovm) =
        eval_oracle_and_neovm("(progn (erase-buffer) (insert \"a\") (goto-char 1) (forward-char -1))");
    assert_err_kind(&bob_oracle, &bob_neovm, "beginning-of-buffer");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_forward_char_parity_with_normalized_error(
        n in -8i64..8i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn
               (erase-buffer)
               (insert \"abcd\")
               (goto-char 2)
               (condition-case err
                   (progn (forward-char {}) (list 'ok (point)))
                 (error (list 'err (car err) (point)))))",
            n
        );
        assert_oracle_parity(&form);
    }
}
