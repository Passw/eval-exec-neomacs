//! Oracle parity tests for `goto-char`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_goto_char_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_goto_char_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_ret, neovm_ret) = eval_oracle_and_neovm("(progn (erase-buffer) (insert \"abc\") (goto-char 2))");
    assert_ok_eq("2", &oracle_ret, &neovm_ret);

    let (oracle_point, neovm_point) =
        eval_oracle_and_neovm("(progn (erase-buffer) (insert \"abc\") (goto-char 2) (point))");
    assert_ok_eq("2", &oracle_point, &neovm_point);
}

#[test]
fn oracle_prop_goto_char_error_cases() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_goto_char_error_cases: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (arity_oracle, arity_neovm) = eval_oracle_and_neovm("(goto-char)");
    assert_err_kind(&arity_oracle, &arity_neovm, "wrong-number-of-arguments");

    let (type_oracle, type_neovm) = eval_oracle_and_neovm(r#"(goto-char "x")"#);
    assert_err_kind(&type_oracle, &type_neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_goto_char_updates_point(
        pos in 1usize..5usize,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn (erase-buffer) (insert \"abcd\") (goto-char {}) (point))",
            pos
        );
        let expected = pos.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
