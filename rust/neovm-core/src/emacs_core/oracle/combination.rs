//! Oracle parity tests for cross-feature combinations.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use proptest::prelude::*;
use std::sync::OnceLock;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, ORACLE_PROP_CASES};

fn oracle_combination_proptest_failure_path() -> &'static str {
    static PATH: OnceLock<&'static str> = OnceLock::new();
    PATH.get_or_init(|| {
        let target_dir = std::env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| "target".to_string());
        Box::leak(
            format!("{target_dir}/proptest-regressions/emacs_core/oracle/combination.txt")
                .into_boxed_str(),
        )
    })
}

#[test]
fn oracle_prop_combination_macro_advice_apply_roundtrip() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((target 'neovm--combo-target)
                      (around 'neovm--combo-around))
                  (progn
                    (defmacro neovm--combo-call-twice (f x)
                      (list 'list
                            (list 'funcall f x)
                            (list 'apply f (list 'list x))))
                    (fset target (lambda (x) (+ x 1)))
                    (fset around (lambda (orig x) (* 2 (funcall orig x))))
                    (unwind-protect
                        (progn
                          (advice-add target :around around)
                          (neovm--combo-call-twice target 5))
                      (condition-case nil (advice-remove target around) (error nil))
                      (fmakunbound target)
                      (fmakunbound around)
                      (fmakunbound 'neovm--combo-call-twice))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_from_advised_function_keeps_log_order() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((target 'neovm--combo-throw-target)
                      (before 'neovm--combo-throw-before)
                      (log nil))
                  (fset target (lambda (x)
                                 (setq log (cons (list 'orig x) log))
                                 (throw 'neovm--combo-tag (+ x 1))))
                  (fset before (lambda (&rest args)
                                 (setq log (cons (cons 'before args) log))))
                  (unwind-protect
                      (progn
                        (advice-add target :before before)
                        (list (catch 'neovm--combo-tag (funcall target 4))
                              (nreverse log)))
                    (condition-case nil (advice-remove target before) (error nil))
                    (fmakunbound target)
                    (fmakunbound before)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_cleanup_error_overrides_throw() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(condition-case err
                  (catch 'neovm--combo-tag
                    (unwind-protect
                        (throw 'neovm--combo-tag 'ok)
                      (car 1)))
                (wrong-type-argument (car err)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("wrong-type-argument", &oracle, &neovm);
}

#[test]
fn oracle_prop_combination_macro_expansion_side_effect_count() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((expands 0))
                  (defmacro neovm--combo-expander (x)
                    (setq expands (1+ expands))
                    `(condition-case nil (car ,x) (wrong-type-argument 'bad)))
                  (unwind-protect
                      (list (neovm--combo-expander '(1 2))
                            (neovm--combo-expander 1)
                            expands)
                    (fmakunbound 'neovm--combo-expander)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_eval_defmacro_then_expand_and_eval() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (eval '(defmacro neovm--combo-eval-m (op x y) (list op x y)))
                  (unwind-protect
                      (list
                        (macroexpand '(neovm--combo-eval-m + 2 3))
                        (neovm--combo-eval-m + 2 3)
                        (funcall (lambda (f) (neovm--combo-eval-m f 10 4)) '-))
                    (fmakunbound 'neovm--combo-eval-m)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_nested_condition_case_throw_and_cleanup() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((state nil))
                  (list
                    (condition-case err
                        (catch 'neovm--combo-done
                          (unwind-protect
                              (progn
                                (setq state (cons 'enter state))
                                (condition-case nil
                                    (car 1)
                                  (wrong-type-argument
                                   (setq state (cons 'handled state))
                                   (throw 'neovm--combo-done 'thrown)))
                                'tail)
                            (setq state (cons 'cleanup state))))
                      (error (list 'err (car err))))
                    (nreverse state)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macroexpand_env_override_then_eval() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-mx (x) (list '+ x 1))
                  (unwind-protect
                      (list
                        (macroexpand '(neovm--combo-mx 7))
                        (eval (macroexpand '(neovm--combo-mx 7)
                                           '((neovm--combo-mx . (lambda (x) (list '- x 1))))))
                        (eval (macroexpand '(neovm--combo-mx 7))))
                    (fmakunbound 'neovm--combo-mx)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_apply_with_symbol_function_mutation() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((sym 'neovm--combo-fn))
                  (fset sym (lambda (x) (+ x 1)))
                  (unwind-protect
                      (let ((orig (symbol-function sym)))
                        (list
                          (apply sym '(2))
                          (progn
                            (fset sym (lambda (x) (* x 10)))
                            (apply sym '(2)))
                          (apply orig '(2))))
                    (fmakunbound sym)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_nested_unwind_cleanup_stack_order() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((log nil))
                  (list
                    (catch 'neovm--combo-tag
                      (unwind-protect
                          (unwind-protect
                              (progn
                                (setq log (cons 'body log))
                                (throw 'neovm--combo-tag 'done))
                            (setq log (cons 'inner-clean log)))
                        (setq log (cons 'outer-clean log))))
                    log))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_guards_apply_with_condition_case() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-guarded-call (fn args)
                    `(condition-case err
                         (apply ,fn ,args)
                       (wrong-type-argument (list 'wta (car err)))
                       (error (list 'err (car err)))))
                  (unwind-protect
                      (list
                        (neovm--combo-guarded-call '+ '(1 2 3))
                        (neovm--combo-guarded-call 'car '(1)))
                    (fmakunbound 'neovm--combo-guarded-call)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macroexpand_and_filter_return_advice() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-call-target (x) `(neovm--combo-target ,x))
                  (fset 'neovm--combo-target (lambda (x) (+ x 1)))
                  (fset 'neovm--combo-filter-ret (lambda (ret) (* ret 3)))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-target :filter-return 'neovm--combo-filter-ret)
                        (list
                          (macroexpand '(neovm--combo-call-target 4))
                          (neovm--combo-call-target 4)))
                    (condition-case nil
                        (advice-remove 'neovm--combo-target 'neovm--combo-filter-ret)
                      (error nil))
                    (fmakunbound 'neovm--combo-target)
                    (fmakunbound 'neovm--combo-filter-ret)
                    (fmakunbound 'neovm--combo-call-target)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_eval_macro_with_lexenv_shadowing() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-eval-env (sym) sym)
                  (unwind-protect
                      (let ((x 7))
                        (list
                          (eval '(neovm--combo-eval-env x))
                          (eval '(neovm--combo-eval-env x) '((x . 11)))))
                    (fmakunbound 'neovm--combo-eval-env)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_apply_with_filter_chain_and_log() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((target 'neovm--combo-chain-target)
                      (fargs 'neovm--combo-chain-fargs)
                      (fret 'neovm--combo-chain-fret)
                      (log nil))
                  (fset target (lambda (a b) (setq log (cons (list 'orig a b) log)) (+ a b)))
                  (fset fargs (lambda (args)
                                (list (1+ (car args))
                                      (1+ (car (cdr args))))))
                  (fset fret (lambda (ret)
                               (setq log (cons (list 'ret ret) log))
                               (* ret 2)))
                  (unwind-protect
                      (progn
                        (advice-add target :filter-args fargs)
                        (advice-add target :filter-return fret)
                        (list (apply target '(2 5)) (nreverse log)))
                    (condition-case nil (advice-remove target fargs) (error nil))
                    (condition-case nil (advice-remove target fret) (error nil))
                    (fmakunbound target)
                    (fmakunbound fargs)
                    (fmakunbound fret)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_generated_unwind_with_nonlocal_exit() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-wrap (body cleanup)
                    `(unwind-protect ,body ,cleanup))
                  (let ((x 0))
                    (unwind-protect
                        (list
                          (catch 'neovm--combo-tag
                            (funcall
                              (lambda ()
                                (neovm--combo-wrap
                                  (progn
                                    (setq x 1)
                                    (throw 'neovm--combo-tag 'stop))
                                  (setq x 2)))))
                          x)
                      (fmakunbound 'neovm--combo-wrap))))";
    assert_oracle_parity(form);
}

proptest! {
    #![proptest_config({
        let mut config = proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES);
        config.failure_persistence = Some(Box::new(
            proptest::test_runner::FileFailurePersistence::Direct(
                oracle_combination_proptest_failure_path(),
            ),
        ));
        config
    })]

    #[test]
    fn oracle_prop_combination_eval_macro_apply_arithmetic(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn
               (defmacro neovm--combo-prop (x y z)
                 (list '+ x (list 'apply (list 'quote '+) (list 'list y z))))
               (unwind-protect
                   (eval '(neovm--combo-prop {} {} {}))
                 (fmakunbound 'neovm--combo-prop)))",
            a, b, c
        );
        let expected = (a + b + c).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_nonlocal_exit_cleanup_state(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
        throw_now in any::<bool>(),
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let flow = if throw_now {
            "(throw 'neovm--combo-tag x)"
        } else {
            "(+ x C)"
        };
        let form = format!(
            "(let ((x {a}))
               (list
                 (catch 'neovm--combo-tag
                   (unwind-protect
                       (progn
                         (setq x (+ x {b}))
                         {flow})
                     (setq x (+ x 1))))
                 x))",
            a = a,
            b = b,
            flow = flow.replace("C", &c.to_string()),
        );

        let protected = if throw_now { a + b } else { a + b + c };
        let x_after_cleanup = a + b + 1;
        let expected = format!("({protected} {x_after_cleanup})");
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_throw_cleanup_updates_multiple_cells(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
        d in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(let ((x {a}) (y {b}))
               (list
                 (catch 'neovm--combo-tag
                   (unwind-protect
                       (progn
                         (setq x (+ x y))
                         (throw 'neovm--combo-tag (+ x {c})))
                     (setq y (- y {d}))))
                 x
                 y))",
            a = a,
            b = b,
            c = c,
            d = d,
        );

        let x_after = a + b;
        let y_after = b - d;
        let caught = x_after + c;
        let expected = format!("({caught} {x_after} {y_after})");
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
