//! Oracle parity tests for char-table parent/subtype/extra-slot primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled,
};

#[test]
fn oracle_prop_char_table_parent_and_subtype_basics() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_parent_and_subtype_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let* ((p (make-char-table 'generic 'p)) (c (make-char-table 'generic 'c))) (set-char-table-parent c p) (list (eq (char-table-parent c) p) (char-table-subtype c)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(t generic)", &oracle, &neovm);
}

#[test]
fn oracle_prop_char_table_parent_fallback_lookup() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_parent_fallback_lookup: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity(
        "(let* ((p (make-char-table 'generic 'p)) (c (make-char-table 'generic nil))) (set-char-table-parent c p) (char-table-range c ?A))",
    );
}

#[test]
fn oracle_prop_char_table_extra_slot_out_of_range_get() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_extra_slot_out_of_range_get: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_generic, neovm_generic) =
        eval_oracle_and_neovm("(char-table-extra-slot (make-char-table 'generic) 0)");
    assert_err_kind(&oracle_generic, &neovm_generic, "args-out-of-range");

    let (oracle_syntax, neovm_syntax) =
        eval_oracle_and_neovm("(char-table-extra-slot (make-char-table 'syntax-table) 0)");
    assert_err_kind(&oracle_syntax, &neovm_syntax, "args-out-of-range");
}

#[test]
fn oracle_prop_char_table_extra_slot_out_of_range_set() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_extra_slot_out_of_range_set: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_generic, neovm_generic) =
        eval_oracle_and_neovm("(set-char-table-extra-slot (make-char-table 'generic) 0 'x)");
    assert_err_kind(&oracle_generic, &neovm_generic, "args-out-of-range");

    let (oracle_syntax, neovm_syntax) =
        eval_oracle_and_neovm("(set-char-table-extra-slot (make-char-table 'syntax-table) 0 'x)");
    assert_err_kind(&oracle_syntax, &neovm_syntax, "args-out-of-range");
}

#[test]
fn oracle_prop_char_table_parent_and_extra_slot_wrong_type_errors() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_parent_and_extra_slot_wrong_type_errors: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_parent, neovm_parent) = eval_oracle_and_neovm("(char-table-parent 1)");
    assert_err_kind(&oracle_parent, &neovm_parent, "wrong-type-argument");

    let (oracle_set_parent, neovm_set_parent) = eval_oracle_and_neovm("(set-char-table-parent 1 nil)");
    assert_err_kind(&oracle_set_parent, &neovm_set_parent, "wrong-type-argument");

    let (oracle_slot, neovm_slot) = eval_oracle_and_neovm("(char-table-extra-slot 1 0)");
    assert_err_kind(&oracle_slot, &neovm_slot, "wrong-type-argument");
}
