//! Oracle parity tests for `apply`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm,
    oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_apply_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_apply_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_sum, neovm_sum) = eval_oracle_and_neovm("(apply '+ '(1 2 3 4))");
    assert_ok_eq("10", &oracle_sum, &neovm_sum);

    let (oracle_list, neovm_list) = eval_oracle_and_neovm("(apply 'list 1 2 '(3 4))");
    assert_ok_eq("(1 2 3 4)", &oracle_list, &neovm_list);

    let (oracle_vec, neovm_vec) = eval_oracle_and_neovm("(apply 'vector 1 '(2 3))");
    assert_ok_eq("[1 2 3]", &oracle_vec, &neovm_vec);
}

#[test]
fn oracle_prop_apply_wrong_type_error_for_last_arg() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_wrong_type_error_for_last_arg: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(apply '+ 1 2)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_apply_empty_tail_and_runtime_function_objects() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_empty_tail_and_runtime_function_objects: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle_sum, neovm_sum) = eval_oracle_and_neovm("(apply '+ nil)");
    assert_ok_eq("0", &oracle_sum, &neovm_sum);

    let (oracle_list, neovm_list) = eval_oracle_and_neovm("(apply 'list nil)");
    assert_ok_eq("nil", &oracle_list, &neovm_list);

    let (oracle_car, neovm_car) = eval_oracle_and_neovm("(apply #'car '((1 2)))");
    assert_ok_eq("1", &oracle_car, &neovm_car);
}

#[test]
fn oracle_prop_apply_lambda_optional_and_rest_binding() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_lambda_optional_and_rest_binding: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form_full = "(apply (lambda (a &optional b &rest xs) (list a b xs)) '(1 2 3 4))";
    let (oracle_full, neovm_full) = eval_oracle_and_neovm(form_full);
    assert_ok_eq("(1 2 (3 4))", &oracle_full, &neovm_full);

    let form_short = "(apply (lambda (a &optional b &rest xs) (list a b xs)) '(1))";
    let (oracle_short, neovm_short) = eval_oracle_and_neovm(form_short);
    assert_ok_eq("(1 nil nil)", &oracle_short, &neovm_short);
}

#[test]
fn oracle_prop_apply_improper_tail_error_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_improper_tail_error_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(condition-case err (apply 'list '(1 . 2)) (error err))");
}

#[test]
fn oracle_prop_apply_nil_t_and_special_form_call_targets() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_nil_t_and_special_form_call_targets: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity("(condition-case err (apply nil nil) (error err))");
    assert_oracle_parity("(condition-case err (apply t nil) (error err))");
    assert_oracle_parity("(condition-case err (apply 'if '(t 1 2)) (error err))");
}

#[test]
fn oracle_prop_apply_autoload_object_error_payload_shape() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_autoload_object_error_payload_shape: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(condition-case err (apply '(autoload \"x\" nil nil nil) '(3)) (wrong-type-argument (list (car err) (nth 1 err) (autoloadp (nth 2 err)))))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(wrong-type-argument symbolp t)", &oracle, &neovm);
}

#[test]
fn oracle_prop_apply_keyword_function_cell_controls_behavior() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_keyword_function_cell_controls_behavior: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    assert_oracle_parity(
        "(let ((orig (symbol-function :k))) (unwind-protect (progn (fset :k 'car) (apply :k '((1 2)))) (fset :k orig)))",
    );
    assert_oracle_parity(
        "(let ((orig (symbol-function :k))) (unwind-protect (progn (fset :k 1) (condition-case err (apply :k nil) (error err))) (fset :k orig)))",
    );
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_apply_splices_last_list_argument(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(apply 'list {} (list {} {}))", a, b, c);
        let expected = format!("({} {} {})", a, b, c);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_apply_mixed_prefix_and_spread_sum(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(apply '+ {} (list {} {}))", a, b, c);
        let expected = (a + b + c).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_apply_list_prefix_and_nested_values(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(apply 'list {} (list (list {}) (list {})))", a, a, b);
        let expected = format!("({} ({}) ({}))", a, a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
