//! Oracle parity tests for `event-convert-list`.

use proptest::prelude::*;

use super::common::{
    assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_event_convert_list_basics() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_event_convert_list_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(event-convert-list '(control ?x))");
    assert_oracle_parity("(event-convert-list '(meta control ?x))");
}

#[test]
fn oracle_prop_event_convert_list_lookup_key_roundtrip() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_event_convert_list_lookup_key_roundtrip: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((m (make-sparse-keymap))) (define-key m (vector (event-convert-list '(control ?x))) 'foo) (lookup-key m (vector (event-convert-list '(control ?x)))))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("foo", &oracle, &neovm);
}

#[test]
fn oracle_prop_event_convert_list_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_event_convert_list_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(event-convert-list 1)");
    assert_ok_eq("nil", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_event_convert_list_control_ascii_lower(
        ch in 97u32..123u32, // a-z
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(event-convert-list (list 'control {}))", ch);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        prop_assert_eq!(neovm, oracle, "event-convert-list parity failed for: {}", form);
    }
}
