//! Oracle parity tests for coding string conversion primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_coding_string_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_coding_string_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = "(list (decode-coding-string \"abc\" 'utf-8) (encode-coding-string \"abc\" 'utf-8))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(\"abc\" \"abc\")", &oracle, &neovm);
}

#[test]
fn oracle_prop_encode_coding_string_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_encode_coding_string_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(encode-coding-string 1 'utf-8)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_decode_coding_string_unknown_coding_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_decode_coding_string_unknown_coding_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(decode-coding-string \"a\" 'neovm-no-such-coding)");
    assert_err_kind(&oracle, &neovm, "coding-system-error");
}
