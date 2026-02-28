//! Oracle parity tests for advice functions.

use super::common::{assert_oracle_parity, oracle_prop_enabled};

#[test]
fn oracle_prop_advice_add_remove_member_lifecycle() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_add_remove_member_lifecycle: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-target) (adv 'neovm--adv-fn)) (fset target (lambda (x) x)) (fset adv (lambda (&rest _) nil)) (unwind-protect (progn (advice-add target :before adv) (list (advice-member-p adv target) (progn (advice-remove target adv) (advice-member-p adv target)))) (fmakunbound target) (fmakunbound adv)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_advice_unknown_where_keyword_error_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_unknown_where_keyword_error_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity(
        "(condition-case err (advice-add 'car :neovm-unknown #'ignore) (error err))",
    );
}

#[test]
fn oracle_prop_advice_wrong_arity_error_shapes() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_wrong_arity_error_shapes: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(condition-case err (advice-add 'car :before) (error err))");
    assert_oracle_parity("(condition-case err (advice-remove 'car) (error err))");
    assert_oracle_parity("(condition-case err (advice-member-p 'ignore) (error err))");
}

#[test]
fn oracle_prop_advice_target_type_error_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_target_type_error_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(condition-case err (advice-add 1 :before #'ignore) (error err))");
}
