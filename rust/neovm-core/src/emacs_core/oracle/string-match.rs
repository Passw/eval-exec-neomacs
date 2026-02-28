//! Oracle parity tests for `string-match`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_string_match_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_string_match_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_hit, neovm_hit) = eval_oracle_and_neovm(r#"(string-match "b+" "abbb")"#);
    assert_ok_eq("1", &oracle_hit, &neovm_hit);

    let (oracle_miss, neovm_miss) = eval_oracle_and_neovm(r#"(string-match "z+" "abbb")"#);
    assert_ok_eq("nil", &oracle_miss, &neovm_miss);
}

#[test]
fn oracle_prop_string_match_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_string_match_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-match 1 "abc")"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_string_match_char_class_edge_cases() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_string_match_char_class_edge_cases: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-match "[z-a]" "z")"#);
    assert_ok_eq("nil", &oracle, &neovm);

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-match "[^z-a]" "x")"#);
    assert_ok_eq("0", &oracle, &neovm);

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-match "[]a]+" "]aa")"#);
    assert_ok_eq("0", &oracle, &neovm);

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-match "[[]+" "[[[")"#);
    assert_ok_eq("0", &oracle, &neovm);

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-match "[\\]" "\\")"#);
    assert_ok_eq("0", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_string_match_index_for_simple_prefix(
        n in 0usize..20usize,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let haystack = format!("{}a", "b".repeat(n));
        let form = format!(r#"(string-match "a" "{}")"#, haystack);
        let expected = n.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
