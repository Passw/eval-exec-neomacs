//! Oracle parity tests for `end-of-line`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, oracle_prop_enabled,
    ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_end_of_line_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_end_of_line_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) =
        eval_oracle_and_neovm("(progn (erase-buffer) (insert \"abc\") (goto-char 1) (end-of-line) (point))");
    assert_ok_eq("4", &oracle, &neovm);
}

#[test]
fn oracle_prop_end_of_line_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_end_of_line_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(end-of-line "x")"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_end_of_line_optional_n_parity(
        n in 1i64..5i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(progn (erase-buffer) (insert \"a\\nb\\nc\\nd\\n\") (goto-char 5) (list (end-of-line {}) (point)))",
            n
        );
        assert_oracle_parity(&form);
    }
}
