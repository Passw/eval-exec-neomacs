//! Oracle parity tests for coding-system metadata primitives.

use super::common::{assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

#[test]
fn oracle_prop_coding_system_aliases_utf8_shape() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let form =
        "(let ((a (coding-system-aliases 'utf-8))) (list (consp a) (memq 'utf-8 a) (memq 'utf-8-unix a)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_coding_system_plist_utf8_shape() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((p (coding-system-plist 'utf-8))) (list (plist-get p :name) (integerp (plist-get p :mnemonic)) (plist-get p :eol-type)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(utf-8 t nil)", &oracle, &neovm);
}

#[test]
fn oracle_prop_coding_system_priority_list_nonempty() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    assert_oracle_parity(
        "(let ((p (coding-system-priority-list))) (list (consp p) (coding-system-p (car p))))",
    );
}

#[test]
fn oracle_prop_coding_system_put_roundtrip() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    assert_oracle_parity(
        "(let ((_ (coding-system-put 'utf-8 :neovm-oracle-test 'ok))) (plist-get (coding-system-plist 'utf-8) :neovm-oracle-test))",
    );
}

#[test]
fn oracle_prop_coding_system_metadata_unknown_symbol_errors() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle_aliases, neovm_aliases) =
        eval_oracle_and_neovm("(coding-system-aliases 'neovm-no-such-coding)");
    assert_err_kind(&oracle_aliases, &neovm_aliases, "coding-system-error");

    let (oracle_plist, neovm_plist) =
        eval_oracle_and_neovm("(coding-system-plist 'neovm-no-such-coding)");
    assert_err_kind(&oracle_plist, &neovm_plist, "coding-system-error");
}

#[test]
fn oracle_prop_coding_system_put_error_shapes() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle_type, neovm_type) = eval_oracle_and_neovm("(coding-system-put 1 :x t)");
    assert_err_kind(&oracle_type, &neovm_type, "wrong-type-argument");

    let (oracle_unknown, neovm_unknown) =
        eval_oracle_and_neovm("(coding-system-put 'neovm-no-such-coding :x t)");
    assert_err_kind(&oracle_unknown, &neovm_unknown, "coding-system-error");
}
