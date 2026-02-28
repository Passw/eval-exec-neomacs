//! Oracle parity tests for `re-search-forward`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_re_search_forward_basics() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_re_search_forward_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_ret, neovm_ret) = eval_oracle_and_neovm(
        r#"(progn (erase-buffer) (insert "abc xyz") (goto-char 1) (re-search-forward "xyz"))"#,
    );
    assert_ok_eq("8", &oracle_ret, &neovm_ret);

    let (oracle_point, neovm_point) = eval_oracle_and_neovm(
        r#"(progn (erase-buffer) (insert "abc xyz") (goto-char 1) (re-search-forward "xyz") (point))"#,
    );
    assert_ok_eq("8", &oracle_point, &neovm_point);
}

#[test]
fn oracle_prop_re_search_forward_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_re_search_forward_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(re-search-forward 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_re_search_forward_multibyte_match_positions() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_re_search_forward_multibyte_match_positions: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = r#"(progn (erase-buffer) (insert "αβc") (goto-char 1) (re-search-forward "c") (list (match-beginning 0) (match-end 0) (point)))"#;
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(3 4 4)", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_re_search_forward_returns_match_end(
        n in 0usize..20usize,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let haystack = format!("{}abc", "b".repeat(n));
        let form = format!(
            r#"(progn (erase-buffer) (insert "{}") (goto-char 1) (re-search-forward "abc"))"#,
            haystack
        );
        let expected = (n + 4).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
