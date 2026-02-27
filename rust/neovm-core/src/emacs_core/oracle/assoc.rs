//! Oracle parity tests for `assoc`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_assoc_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_assoc_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_found, neovm_found) =
        eval_oracle_and_neovm(r#"(assoc "b" '(("a" . 1) ("b" . 2)))"#);
    assert_ok_eq("(\"b\" . 2)", &oracle_found, &neovm_found);

    let (oracle_missing, neovm_missing) =
        eval_oracle_and_neovm(r#"(assoc "z" '(("a" . 1) ("b" . 2)))"#);
    assert_ok_eq("nil", &oracle_missing, &neovm_missing);
}

#[test]
fn oracle_prop_assoc_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_assoc_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(assoc "a" 1)"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_assoc_equal_key(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(r#"(assoc "k" (list (cons "x" {}) (cons (concat "k") {})))"#, a, b);
        let expected = format!("(\"k\" . {})", b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
