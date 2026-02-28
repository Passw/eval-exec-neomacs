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

#[test]
fn oracle_prop_advice_before_observes_call_arguments() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_before_observes_call_arguments: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-target) (before 'neovm--adv-before) (log nil)) (fset target (lambda (x) (setq log (cons (list 'orig x) log)) x)) (fset before (lambda (&rest args) (setq log (cons (cons 'before args) log)))) (unwind-protect (progn (advice-add target :before before) (funcall target 7) (nreverse log)) (fmakunbound target) (fmakunbound before)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_advice_around_wraps_original_result() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_around_wraps_original_result: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-around-target) (around 'neovm--adv-around)) (fset target (lambda (x) (* x 2))) (fset around (lambda (orig x) (+ 10 (funcall orig (1+ x))))) (unwind-protect (progn (advice-add target :around around) (funcall target 3)) (fmakunbound target) (fmakunbound around)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_advice_override_replaces_original_function() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_override_replaces_original_function: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-override-target) (override 'neovm--adv-override)) (fset target (lambda (x) (+ x 1))) (fset override (lambda (&rest _) 'override-hit)) (unwind-protect (progn (advice-add target :override override) (funcall target 11)) (fmakunbound target) (fmakunbound override)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_advice_filter_args_rewrites_argument_list() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_filter_args_rewrites_argument_list: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-filter-args-target) (filter 'neovm--adv-filter-args)) (fset target (lambda (a b) (+ a b))) (fset filter (lambda (args) (list (* 2 (car args)) (* 3 (car (cdr args)))))) (unwind-protect (progn (advice-add target :filter-args filter) (funcall target 2 5)) (fmakunbound target) (fmakunbound filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_advice_filter_return_rewrites_result() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_filter_return_rewrites_result: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-filter-ret-target) (filter 'neovm--adv-filter-ret)) (fset target (lambda (x) (* x 2))) (fset filter (lambda (ret) (+ ret 9))) (unwind-protect (progn (advice-add target :filter-return filter) (funcall target 3)) (fmakunbound target) (fmakunbound filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_advice_runs_when_target_is_called_via_apply() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_advice_runs_when_target_is_called_via_apply: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((target 'neovm--adv-apply-target) (before 'neovm--adv-apply-before) (log nil)) (fset target (lambda (a b) (setq log (cons (list 'orig a b) log)) (+ a b))) (fset before (lambda (&rest args) (setq log (cons (cons 'before args) log)))) (unwind-protect (progn (advice-add target :before before) (list (apply target '(4 9)) (nreverse log))) (fmakunbound target) (fmakunbound before)))";
    assert_oracle_parity(form);
}
