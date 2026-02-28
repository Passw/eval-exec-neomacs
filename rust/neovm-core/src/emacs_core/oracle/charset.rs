//! Oracle parity tests for charset primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled,
};

#[test]
fn oracle_prop_charset_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_charset_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form =
        "(list (char-charset ?A) (charsetp (char-charset ?A)) (encode-char ?A 'ucs) (decode-char 'ucs #x41) (encode-char ?😀 'ucs) (decode-char 'ucs #x1F600))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(ascii t 65 65 128512 128512)", &oracle, &neovm);
}

#[test]
fn oracle_prop_encode_char_unknown_charset_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_encode_char_unknown_charset_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(encode-char ?A 'neovm-no-such-charset)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_decode_char_out_of_range_error_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_decode_char_out_of_range_error_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(decode-char 'ucs -1)");
}
