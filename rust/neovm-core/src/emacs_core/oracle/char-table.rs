//! Oracle parity tests for char-table primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_char_table_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_char_table_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_ct, neovm_ct) = eval_oracle_and_neovm("(char-table-p (make-char-table 'generic))");
    assert_ok_eq("t", &oracle_ct, &neovm_ct);

    let (oracle_vec_or_ct, neovm_vec_or_ct) =
        eval_oracle_and_neovm("(vector-or-char-table-p (make-char-table 'generic))");
    assert_ok_eq("t", &oracle_vec_or_ct, &neovm_vec_or_ct);
}

#[test]
fn oracle_prop_char_table_set_range_cons_pair() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_set_range_cons_pair: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((ct (make-char-table 'generic nil))) (set-char-table-range ct '(?a . ?c) 'x) (list (char-table-range ct ?a) (char-table-range ct ?b) (char-table-range ct ?c) (char-table-range ct ?d)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(x x x nil)", &oracle, &neovm);
}

#[test]
fn oracle_prop_char_table_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_char_table_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(char-table-range 1 ?a)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}
