//! Oracle parity tests for `defvar`.

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, oracle_prop_enabled};

#[test]
fn oracle_prop_defvar_with_value() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_defvar_with_value: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    // defvar with initial value
    let (o, n) = eval_oracle_and_neovm("(progn (defvar test--dv-x 42) test--dv-x)");
    assert_ok_eq("42", &o, &n);

    // defvar does not overwrite existing value
    let (o, n) = eval_oracle_and_neovm(
        "(progn (defvar test--dv-y 1) (defvar test--dv-y 2) test--dv-y)",
    );
    assert_ok_eq("1", &o, &n);
}

#[test]
fn oracle_prop_defvar_without_value() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_defvar_without_value: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    // defvar without init value should NOT bind the variable in batch mode
    assert_oracle_parity("(progn (defvar test--dv-z) (boundp 'test--dv-z))");
}

#[test]
fn oracle_prop_defvar_dynamic_binding() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_defvar_dynamic_binding: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    // dynamic scoping with let
    let (o, n) = eval_oracle_and_neovm(
        "(progn (defvar test--dv-dyn 10) (let ((test--dv-dyn 77)) test--dv-dyn))",
    );
    assert_ok_eq("77", &o, &n);

    // dynamic binding restores after let
    let (o, n) = eval_oracle_and_neovm(
        "(progn (defvar test--dv-restore 5) (let ((test--dv-restore 99)) nil) test--dv-restore)",
    );
    assert_ok_eq("5", &o, &n);
}
