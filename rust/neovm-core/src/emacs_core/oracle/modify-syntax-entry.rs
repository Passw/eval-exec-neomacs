//! Oracle parity tests for `modify-syntax-entry`.

use super::common::{
    assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_modify_syntax_entry_cons_pair_range() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_modify_syntax_entry_cons_pair_range: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((st (make-syntax-table))) (modify-syntax-entry '(?a . ?z) \"w\" st) (list (aref st ?a) (aref st ?m) (aref st ?z) (aref st ?A)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_modify_syntax_entry_digit_range() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_modify_syntax_entry_digit_range: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((st (make-syntax-table))) (modify-syntax-entry '(?0 . ?9) \".\" st) (list (aref st ?0) (aref st ?5) (aref st ?9)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_modify_syntax_entry_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_modify_syntax_entry_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(modify-syntax-entry 1 \"w\")");
    assert_ok_eq("nil", &oracle, &neovm);
}

#[test]
fn oracle_prop_make_syntax_table_inherits_standard_entries() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_make_syntax_table_inherits_standard_entries: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((st (make-syntax-table))) (list (aref st ?A) (aref st ?0) (aref st ?\\n)))";
    assert_oracle_parity(form);
}
