//! Oracle parity tests for `eval`.

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled,
};

#[test]
fn oracle_prop_eval_lexical_flag_controls_closure_capture() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_lexical_flag_controls_closure_capture: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_default, neovm_default) =
        eval_oracle_and_neovm("(let ((f (eval '(let ((x 1)) (lambda () x))))) (funcall f))");
    assert_err_kind(&oracle_default, &neovm_default, "void-variable");

    let (oracle_nil, neovm_nil) =
        eval_oracle_and_neovm("(let ((f (eval '(let ((x 1)) (lambda () x)) nil))) (funcall f))");
    assert_err_kind(&oracle_nil, &neovm_nil, "void-variable");

    let (oracle_lex, neovm_lex) =
        eval_oracle_and_neovm("(let ((f (eval '(let ((x 1)) (lambda () x)) t))) (funcall f))");
    assert_ok_eq("1", &oracle_lex, &neovm_lex);
}

#[test]
fn oracle_prop_eval_nil_resets_dynamic_mode_after_lexical_eval() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_nil_resets_dynamic_mode_after_lexical_eval: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((_ (eval '(let ((x 7)) (lambda () x)) t))) (condition-case nil (let ((f (eval '(let ((x 9)) (lambda () x)) nil))) (funcall f)) (void-variable 'dynamic)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("dynamic", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_eval_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(eval)");
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}
