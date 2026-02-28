//! Oracle parity tests for key description and modifier parsing primitives.

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_single_key_description_modifier_outputs() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_single_key_description_modifier_outputs: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(list (single-key-description (event-convert-list '(control ?x))) (single-key-description (event-convert-list '(meta control ?x))))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(\"C-x\" \"C-M-x\")", &oracle, &neovm);
}

#[test]
fn oracle_prop_key_description_sequence() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_key_description_sequence: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form =
        "(key-description (vector (event-convert-list '(control ?x)) (event-convert-list '(meta control ?x))))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("\"C-x C-M-x\"", &oracle, &neovm);
}

#[test]
fn oracle_prop_internal_event_symbol_parse_modifiers() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_internal_event_symbol_parse_modifiers: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(internal-event-symbol-parse-modifiers 'C-M-x)");
    assert_ok_eq("(x meta control)", &oracle, &neovm);
}

#[test]
fn oracle_prop_internal_event_symbol_parse_modifiers_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_internal_event_symbol_parse_modifiers_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(internal-event-symbol-parse-modifiers 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_key_description_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_key_description_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(key-description 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}
