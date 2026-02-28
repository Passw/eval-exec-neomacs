//! Oracle parity tests for coding string conversion primitives.

use super::common::{assert_err_kind, assert_ok_eq, eval_oracle_and_neovm};

#[test]
fn oracle_prop_coding_string_basics() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(list (decode-coding-string \"abc\" 'utf-8) (encode-coding-string \"abc\" 'utf-8))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(\"abc\" \"abc\")", &oracle, &neovm);
}

#[test]
fn oracle_prop_encode_coding_string_wrong_type_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm("(encode-coding-string 1 'utf-8)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_decode_coding_string_unknown_coding_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) =
        eval_oracle_and_neovm("(decode-coding-string \"a\" 'neovm-no-such-coding)");
    assert_err_kind(&oracle, &neovm, "coding-system-error");
}

#[test]
fn oracle_prop_encode_coding_string_unknown_coding_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) =
        eval_oracle_and_neovm("(encode-coding-string \"a\" 'neovm-no-such-coding)");
    assert_err_kind(&oracle, &neovm, "coding-system-error");
}
