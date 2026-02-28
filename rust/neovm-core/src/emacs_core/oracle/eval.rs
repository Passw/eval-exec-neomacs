//! Oracle parity tests for `eval`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled, ORACLE_PROP_CASES,
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

#[test]
fn oracle_prop_eval_lexenv_list_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_eval_lexenv_list_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(eval '(list x y) '((x . 1) (y . 2)))");
    assert_ok_eq("(1 2)", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_lexenv_shadowing_outer_dynamic_binding() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_lexenv_shadowing_outer_dynamic_binding: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 10)) (eval 'x '((x . 3))))");
    assert_ok_eq("3", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_lexenv_duplicate_binding_first_wins() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_lexenv_duplicate_binding_first_wins: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(eval 'x '((x . 1) (x . 2)))");
    assert_ok_eq("1", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_lexenv_binding_with_implicit_nil() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_lexenv_binding_with_implicit_nil: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(eval 'x '((x)))");
    assert_ok_eq("nil", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_lexenv_captured_by_lambda() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_lexenv_captured_by_lambda: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(let ((f (eval '(lambda () x) '((x . 99))))) (funcall f))");
    assert_oracle_parity("(let ((f (eval '(lambda () x) '((x . 99))))) (let ((x 3)) (funcall f)))");
}

#[test]
fn oracle_prop_eval_macro_expansion_with_lexenv() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_macro_expansion_with_lexenv: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(eval '(when x y) '((x . t) (y . 9)))");
    assert_ok_eq("9", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_lexenv_argument_shape_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_lexenv_argument_shape_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(eval 'x '(x . 1))");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_eval_error_does_not_leak_lexical_mode() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_error_does_not_leak_lexical_mode: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((_ (condition-case nil (eval '(+ 1 \"x\") t) (error 'err)))) (let ((f (eval '(let ((x 9)) (lambda () x)) nil))) (condition-case nil (funcall f) (void-variable 'dynamic))))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("dynamic", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_nested_mode_switch_with_inner_lexical_eval() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_nested_mode_switch_with_inner_lexical_eval: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) =
        eval_oracle_and_neovm("(let ((f (eval '(eval '(let ((x 7)) (lambda () x)) t) nil))) (funcall f))");
    assert_ok_eq("7", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_dynamic_setq_side_effect() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_dynamic_setq_side_effect: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 1)) (eval '(setq x 2)) x)");
    assert_ok_eq("2", &oracle, &neovm);
}

#[test]
fn oracle_prop_eval_quote_and_function_forms() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_quote_and_function_forms: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_quote, neovm_quote) = eval_oracle_and_neovm("(let ((x 1)) (eval '(quote x)))");
    assert_ok_eq("x", &oracle_quote, &neovm_quote);

    let (oracle_fn, neovm_fn) =
        eval_oracle_and_neovm("(let ((f (eval '(function (lambda (x) (+ x 1)))))) (funcall f 41))");
    assert_ok_eq("42", &oracle_fn, &neovm_fn);
}

#[test]
fn oracle_prop_eval_error_passthrough_via_condition_case() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_eval_error_passthrough_via_condition_case: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(condition-case nil (eval '(car 1)) (wrong-type-argument 'caught))");
    assert_ok_eq("caught", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_eval_lexenv_integer_addition(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(eval '(+ x y) '((x . {}) (y . {})))", a, b);
        let expected = (a + b).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_eval_lexenv_shadows_outer_dynamic_binding(
        outer in -100_000i64..100_000i64,
        inner in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(let ((x {})) (eval 'x '((x . {}))))", outer, inner);
        let expected = inner.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_eval_runtime_constructed_form_addition(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(eval (list '+ {} {}))", a, b);
        let expected = (a + b).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_eval_dynamic_setq_updates_binding(
        initial in -100_000i64..100_000i64,
        updated in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(let ((x {})) (eval '(setq x {})) x)", initial, updated);
        let expected = updated.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
