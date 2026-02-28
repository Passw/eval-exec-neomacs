//! Oracle parity tests for keymap primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_keymap_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_keymap_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(keymapp (make-keymap))");
    assert_ok_eq("t", &oracle, &neovm);

    let (oracle, neovm) = eval_oracle_and_neovm("(keymapp (make-sparse-keymap))");
    assert_ok_eq("t", &oracle, &neovm);
}

#[test]
fn oracle_prop_keymap_copy_and_lookup() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_keymap_copy_and_lookup: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let* ((m (make-sparse-keymap))) (define-key m [24] 'foo) (let ((c (copy-keymap m))) (list (lookup-key c [24]) (keymapp c))))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(foo t)", &oracle, &neovm);
}

#[test]
fn oracle_prop_keymap_parent_roundtrip() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_keymap_parent_roundtrip: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let* ((p (make-sparse-keymap)) (c (make-sparse-keymap))) (set-keymap-parent c p) (eq (keymap-parent c) p))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("t", &oracle, &neovm);
}

#[test]
fn oracle_prop_keymap_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_keymap_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_copy, neovm_copy) = eval_oracle_and_neovm("(copy-keymap 1)");
    assert_err_kind(&oracle_copy, &neovm_copy, "wrong-type-argument");

    let (oracle_parent, neovm_parent) = eval_oracle_and_neovm("(set-keymap-parent 1 2)");
    assert_err_kind(&oracle_parent, &neovm_parent, "wrong-type-argument");
}
