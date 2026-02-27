//! Oracle parity tests for `condition-case`.

use super::common::{
    assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_condition_case_handles_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_condition_case_handles_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(condition-case nil (/ 1 0) (arith-error 42))");
    assert_ok_eq("42", &oracle, &neovm);
}

#[test]
fn oracle_prop_condition_case_no_error_passthrough() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_condition_case_no_error_passthrough: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(condition-case nil (+ 1 2) (error 0))");
    assert_ok_eq("3", &oracle, &neovm);
}

#[test]
fn oracle_prop_condition_case_error_symbol_binding() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_condition_case_error_symbol_binding: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(condition-case err (/ 1 0) (arith-error (car err)))");
}
