//! Oracle parity tests for coding-system metadata primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled,
};

#[test]
fn oracle_prop_coding_system_aliases_utf8_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_coding_system_aliases_utf8_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form =
        "(let ((a (coding-system-aliases 'utf-8))) (list (consp a) (memq 'utf-8 a) (memq 'utf-8-unix a)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_coding_system_plist_utf8_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_coding_system_plist_utf8_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((p (coding-system-plist 'utf-8))) (list (plist-get p :name) (integerp (plist-get p :mnemonic)) (plist-get p :eol-type)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(utf-8 t nil)", &oracle, &neovm);
}

#[test]
fn oracle_prop_coding_system_priority_list_nonempty() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_coding_system_priority_list_nonempty: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity(
        "(let ((p (coding-system-priority-list))) (list (consp p) (coding-system-p (car p))))",
    );
}

#[test]
fn oracle_prop_coding_system_put_roundtrip() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_coding_system_put_roundtrip: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity(
        "(let ((_ (coding-system-put 'utf-8 :neovm-oracle-test 'ok))) (plist-get (coding-system-plist 'utf-8) :neovm-oracle-test))",
    );
}

#[test]
fn oracle_prop_coding_system_metadata_unknown_symbol_errors() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_coding_system_metadata_unknown_symbol_errors: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_aliases, neovm_aliases) =
        eval_oracle_and_neovm("(coding-system-aliases 'neovm-no-such-coding)");
    assert_err_kind(&oracle_aliases, &neovm_aliases, "coding-system-error");

    let (oracle_plist, neovm_plist) =
        eval_oracle_and_neovm("(coding-system-plist 'neovm-no-such-coding)");
    assert_err_kind(&oracle_plist, &neovm_plist, "coding-system-error");
}

#[test]
fn oracle_prop_coding_system_put_error_shapes() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_coding_system_put_error_shapes: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_type, neovm_type) = eval_oracle_and_neovm("(coding-system-put 1 :x t)");
    assert_err_kind(&oracle_type, &neovm_type, "wrong-type-argument");

    let (oracle_unknown, neovm_unknown) =
        eval_oracle_and_neovm("(coding-system-put 'neovm-no-such-coding :x t)");
    assert_err_kind(&oracle_unknown, &neovm_unknown, "coding-system-error");
}
