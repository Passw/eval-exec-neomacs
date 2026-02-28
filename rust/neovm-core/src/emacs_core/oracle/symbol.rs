//! Oracle parity tests for symbol primitives.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_symbol_name_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_symbol_name_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(symbol-name 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_intern_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_intern_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(intern 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_fboundp_car() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_fboundp_car: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(fboundp 'car)");
    assert_ok_eq("t", &oracle, &neovm);
}

#[test]
fn oracle_prop_boundp_nil() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_boundp_nil: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(boundp 'nil)");
    assert_ok_eq("t", &oracle, &neovm);
}

#[test]
fn oracle_prop_symbolp_basic() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_symbolp_basic: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    assert_oracle_parity(r#"(symbolp "x")"#);
    assert_oracle_parity("(symbolp 'x)");
}

#[test]
fn oracle_prop_bare_colon_keyword_self_evaluates() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_bare_colon_keyword_self_evaluates: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x :)) (list (eq x :) (keywordp x) (symbolp x)))");
    assert_ok_eq("(t t t)", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_intern_symbol_name_roundtrip(
        name in proptest::string::string_regex(r"[a-z][a-z0-9-]{0,12}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(r#"(symbol-name (intern {:?}))"#, name);
        let expected = format!("{:?}", name);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_symbolp_interned_symbol(
        name in proptest::string::string_regex(r"[a-z][a-z0-9-]{0,12}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(r#"(symbolp (intern {:?}))"#, name);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq("t", &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_intern_eq_idempotent(
        name in proptest::string::string_regex(r"[a-z][a-z0-9-]{0,12}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(r#"(eq (intern {:?}) (intern {:?}))"#, name, name);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq("t", &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_fboundp_unknown_symbol(
        name in proptest::string::string_regex(r"[a-z][a-z0-9-]{0,10}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let symbol_name = format!("neovm-oracle-unknown-fn-{name}");
        let form = format!(r#"(fboundp (intern {:?}))"#, symbol_name);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq("nil", &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_boundp_unknown_symbol(
        name in proptest::string::string_regex(r"[a-z][a-z0-9-]{0,10}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let symbol_name = format!("neovm-oracle-unknown-var-{name}");
        let form = format!(r#"(boundp (intern {:?}))"#, symbol_name);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq("nil", &oracle, &neovm);
    }
}
