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

#[test]
fn oracle_prop_combination_filter_return_advice_call_path_matrix() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (fset 'neovm--combo-path-target (lambda (x) (+ x 1)))
                  (fset 'neovm--combo-path-filter (lambda (ret) (* ret 3)))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-path-target :filter-return 'neovm--combo-path-filter)
                        (list
                          (funcall 'neovm--combo-path-target 4)
                          (apply 'neovm--combo-path-target '(4))
                          (neovm--combo-path-target 4)
                          (eval '(neovm--combo-path-target 4))))
                    (condition-case nil
                        (advice-remove 'neovm--combo-path-target 'neovm--combo-path-filter)
                      (error nil))
                    (fmakunbound 'neovm--combo-path-target)
                    (fmakunbound 'neovm--combo-path-filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_before_advice_call_path_logging_matrix() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((log nil))
                  (fset 'neovm--combo-before-path-target
                        (lambda (x) (setq log (cons (list 'orig x) log)) x))
                  (fset 'neovm--combo-before-path
                        (lambda (&rest args)
                          (setq log (cons (cons 'before args) log))))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-before-path-target :before 'neovm--combo-before-path)
                        (list
                          (funcall 'neovm--combo-before-path-target 1)
                          (apply 'neovm--combo-before-path-target '(2))
                          (neovm--combo-before-path-target 3)
                          (eval '(neovm--combo-before-path-target 4))
                          (nreverse log)))
                    (condition-case nil
                        (advice-remove 'neovm--combo-before-path-target 'neovm--combo-before-path)
                      (error nil))
                    (fmakunbound 'neovm--combo-before-path-target)
                    (fmakunbound 'neovm--combo-before-path)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_direct_vs_funcall_under_advice() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-call-direct (x) `(neovm--combo-macro-path-target ,x))
                  (defmacro neovm--combo-call-funcall (x) `(funcall 'neovm--combo-macro-path-target ,x))
                  (fset 'neovm--combo-macro-path-target (lambda (x) (+ x 1)))
                  (fset 'neovm--combo-macro-path-filter (lambda (ret) (* ret 3)))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-macro-path-target :filter-return 'neovm--combo-macro-path-filter)
                        (list
                          (neovm--combo-call-direct 5)
                          (neovm--combo-call-funcall 5)
                          (macroexpand '(neovm--combo-call-direct 5))
                          (macroexpand '(neovm--combo-call-funcall 5))))
                    (condition-case nil
                        (advice-remove 'neovm--combo-macro-path-target 'neovm--combo-macro-path-filter)
                      (error nil))
                    (fmakunbound 'neovm--combo-call-direct)
                    (fmakunbound 'neovm--combo-call-funcall)
                    (fmakunbound 'neovm--combo-macro-path-target)
                    (fmakunbound 'neovm--combo-macro-path-filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_filter_args_advice_call_path_matrix() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (fset 'neovm--combo-fargs-path-target (lambda (a b) (+ a b)))
                  (fset 'neovm--combo-fargs-path
                        (lambda (args)
                          (list (+ 10 (car args))
                                (+ 20 (car (cdr args))))))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-fargs-path-target :filter-args 'neovm--combo-fargs-path)
                        (list
                          (funcall 'neovm--combo-fargs-path-target 1 2)
                          (apply 'neovm--combo-fargs-path-target '(1 2))
                          (neovm--combo-fargs-path-target 1 2)
                          (eval '(neovm--combo-fargs-path-target 1 2))))
                    (condition-case nil
                        (advice-remove 'neovm--combo-fargs-path-target 'neovm--combo-fargs-path)
                      (error nil))
                    (fmakunbound 'neovm--combo-fargs-path-target)
                    (fmakunbound 'neovm--combo-fargs-path)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_override_advice_call_path_matrix() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (fset 'neovm--combo-override-path-target (lambda (x) (+ x 1)))
                  (fset 'neovm--combo-override-path (lambda (&rest _args) 99))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-override-path-target :override 'neovm--combo-override-path)
                        (list
                          (funcall 'neovm--combo-override-path-target 7)
                          (apply 'neovm--combo-override-path-target '(7))
                          (neovm--combo-override-path-target 7)
                          (eval '(neovm--combo-override-path-target 7))))
                    (condition-case nil
                        (advice-remove 'neovm--combo-override-path-target 'neovm--combo-override-path)
                      (error nil))
                    (fmakunbound 'neovm--combo-override-path-target)
                    (fmakunbound 'neovm--combo-override-path)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_after_advice_call_path_matrix_logging() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((log nil))
                  (fset 'neovm--combo-after-path-target
                        (lambda (x) (setq log (cons (list 'orig x) log)) x))
                  (fset 'neovm--combo-after-path
                        (lambda (&rest args)
                          (setq log (cons (cons 'after args) log))))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-after-path-target :after 'neovm--combo-after-path)
                        (list
                          (funcall 'neovm--combo-after-path-target 1)
                          (apply 'neovm--combo-after-path-target '(2))
                          (neovm--combo-after-path-target 3)
                          (eval '(neovm--combo-after-path-target 4))
                          (nreverse log)))
                    (condition-case nil
                        (advice-remove 'neovm--combo-after-path-target 'neovm--combo-after-path)
                      (error nil))
                    (fmakunbound 'neovm--combo-after-path-target)
                    (fmakunbound 'neovm--combo-after-path)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_eval_macroexpand_error_recovery() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-bad-macro (x) (list 'car x))
                  (unwind-protect
                      (list
                        (condition-case err
                            (eval (macroexpand '(neovm--combo-bad-macro 1)))
                          (wrong-type-argument (car err)))
                        (condition-case err
                            (eval (macroexpand '(neovm--combo-bad-macro '(9 8))))
                          (error (car err))))
                    (fmakunbound 'neovm--combo-bad-macro)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_unwind_cleanup_with_mutating_closure_state() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((x 0)
                      (f (let ((cell 0))
                           (lambda (delta)
                             (setq cell (+ cell delta))
                             cell))))
                  (list
                    (unwind-protect
                        (progn
                          (funcall f 3)
                          (funcall f 4))
                      (setq x (funcall f 10)))
                    x
                    (funcall f 1)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_generated_tags_and_nonlocal_exit() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-with-tag (tag value)
                    `(catch ,tag
                       (condition-case err
                           (throw ,tag ,value)
                         (error (list 'err (car err))))))
                  (unwind-protect
                      (list
                        (neovm--combo-with-tag 'neovm--t1 11)
                        (neovm--combo-with-tag 'neovm--t2 22))
                    (fmakunbound 'neovm--combo-with-tag)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_runtime_macro_definition_lifecycle() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (eval '(defmacro neovm--combo-runtime (x) (list 'list x x)))
                  (unwind-protect
                      (list
                        (eval '(neovm--combo-runtime 5))
                        (macroexpand '(neovm--combo-runtime 9)))
                    (fmakunbound 'neovm--combo-runtime)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_direct_dynamic_tag_catch_throw() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-dyn-tag))
                  (list
                    (catch 'neovm--combo-lit-tag (throw 'neovm--combo-lit-tag 10))
                    (catch tag (throw tag 20))
                    (condition-case err
                        (catch tag (throw 'neovm--combo-other 1))
                      (no-catch (car err)))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_parameterized_tag_simple() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-catch-throw-param (tag value)
                    `(catch ,tag (throw ,tag ,value)))
                  (unwind-protect
                      (list
                        (neovm--combo-catch-throw-param 'neovm--combo-a 3)
                        (let ((tg 'neovm--combo-b))
                          (neovm--combo-catch-throw-param tg 4)))
                    (fmakunbound 'neovm--combo-catch-throw-param)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macroexpanded_tag_form_eval_roundtrip() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-catch-throw-param (tag value)
                    `(catch ,tag (throw ,tag ,value)))
                  (unwind-protect
                      (let ((expanded
                              (macroexpand
                                '(neovm--combo-catch-throw-param 'neovm--combo-c 9))))
                        (list expanded (eval expanded)))
                    (fmakunbound 'neovm--combo-catch-throw-param)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_eval_constructed_catch_throw_runtime_tag() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let* ((tag 'neovm--combo-rt-tag)
                       (form (list 'catch
                                   (list 'quote tag)
                                   (list 'throw (list 'quote tag) 77))))
                  (eval form))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_apply_eval_macro_generated_throw() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-build-throw (tag val)
                    `(throw ,tag ,val))
                  (unwind-protect
                      (catch 'neovm--combo-ap
                        (apply (lambda (frm) (eval frm))
                               (list (macroexpand
                                       '(neovm--combo-build-throw
                                          'neovm--combo-ap
                                          13)))))
                    (fmakunbound 'neovm--combo-build-throw)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_dynamic_tag_with_condition_case_without_macro() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-cond-tag))
                  (catch tag
                    (condition-case err
                        (throw tag 31)
                      (error (list 'err (car err))))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_tag_with_condition_case_expansion_and_eval() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-with-tag-cc (tag value)
                    `(catch ,tag
                       (condition-case err
                           (throw ,tag ,value)
                         (error (list 'err (car err))))))
                  (unwind-protect
                      (let ((a (macroexpand '(neovm--combo-with-tag-cc 'neovm--combo-c1 41)))
                            (b (macroexpand '(neovm--combo-with-tag-cc 'neovm--combo-c2 42))))
                        (list a b (eval a) (eval b)))
                    (fmakunbound 'neovm--combo-with-tag-cc)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_eval_macroexpanded_lambda_with_lexenv() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-make-adder (x)
                    `(lambda (y) (+ ,x y)))
                  (unwind-protect
                      (let ((f (eval '(neovm--combo-make-adder x) '((x . 9)))))
                        (list (funcall f 1) (apply f '(2))))
                    (fmakunbound 'neovm--combo-make-adder)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_unwind_cleanup_rebinds_function_seen_by_eval() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((sym 'neovm--combo-rebind))
                  (fset sym (lambda (x) (+ x 1)))
                  (unwind-protect
                      (list
                        (funcall sym 3)
                        (unwind-protect
                            (catch 'neovm--combo-rb-tag
                              (throw 'neovm--combo-rb-tag (funcall sym 4)))
                          (fset sym (lambda (x) (* x 10))))
                        (funcall sym 3)
                        (eval (list sym 3)))
                    (fmakunbound sym)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_runtime_redefinition_changes_future_expansion() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-rdef (x) (list '+ x 1))
                  (unwind-protect
                      (let ((first (macroexpand '(neovm--combo-rdef 7))))
                        (fset 'neovm--combo-rdef (cons 'macro (lambda (x) (list '- x 1))))
                        (list
                          first
                          (macroexpand '(neovm--combo-rdef 7))
                          (neovm--combo-rdef 7)))
                    (fmakunbound 'neovm--combo-rdef)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_dynamic_tag_throw_inside_unwind_protect() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-tag-u))
                  (catch tag
                    (unwind-protect
                        (throw tag 55)
                      'cleanup)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_dynamic_tag_throw_in_condition_case_inside_unwind() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-tag-uc))
                  (catch tag
                    (unwind-protect
                        (condition-case err
                            (throw tag 66)
                          (error (list 'err (car err))))
                      'cleanup)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_no_catch_handler_rethrows_to_outer_catch() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-tag-r))
                  (catch tag
                    (condition-case err
                        (throw 'neovm--combo-other 1)
                      (no-catch
                       (throw tag (list 'rescued (car err)))))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_macro_wrapped_condition_case_throw_with_dynamic_tag() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (defmacro neovm--combo-cc-throw (tag v)
                    `(condition-case err
                         (throw ,tag ,v)
                       (error (list 'err (car err)))))
                  (unwind-protect
                      (let ((tag 'neovm--combo-mtag))
                        (catch tag
                          (neovm--combo-cc-throw tag 77)))
                    (fmakunbound 'neovm--combo-cc-throw)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_apply_throw_with_dynamic_tag() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-apply-tag))
                  (catch tag
                    (apply #'throw (list tag 88))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_through_condition_case_unrelated_error_handlers() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-cc-pass-tag))
                  (catch tag
                    (condition-case nil
                        (progn (throw tag 91) 'tail)
                      (arith-error 'arith)
                      (wrong-type-argument 'wta))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_through_multiple_condition_case_layers() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-cc-nest-tag))
                  (catch tag
                    (condition-case nil
                        (condition-case nil
                            (throw tag 92)
                          (wrong-type-argument 'inner))
                      (arith-error 'outer))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_through_condition_case_and_unwind_cleanup() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-cc-unwind-tag)
                      (x 0))
                  (list
                    (catch tag
                      (condition-case nil
                          (unwind-protect
                              (throw tag 93)
                            (setq x 1))
                        (error 'caught)))
                    x))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_apply_throw_inside_condition_case_to_outer_catch() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag 'neovm--combo-cc-apply-tag))
                  (catch tag
                    (condition-case nil
                        (apply #'throw (list tag 94))
                      (error 'caught))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_not_caught_by_condition_case_error_clause() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(catch 'neovm--combo-cc-basic-tag
                  (condition-case nil
                      (throw 'neovm--combo-cc-basic-tag 95)
                    (error 'caught)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_symbol_function_after_advice_call_paths() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (fset 'neovm--combo-sf-target (lambda (x) (+ x 1)))
                  (fset 'neovm--combo-sf-filter (lambda (ret) (* ret 3)))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-sf-target :filter-return 'neovm--combo-sf-filter)
                        (let ((f (symbol-function 'neovm--combo-sf-target)))
                          (list
                            (funcall f 5)
                            (apply f '(5))
                            (funcall 'neovm--combo-sf-target 5)
                            (neovm--combo-sf-target 5))))
                    (condition-case nil
                        (advice-remove 'neovm--combo-sf-target 'neovm--combo-sf-filter)
                      (error nil))
                    (fmakunbound 'neovm--combo-sf-target)
                    (fmakunbound 'neovm--combo-sf-filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_fset_after_advice_add_keeps_wrapping_behavior() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (fset 'neovm--combo-fset-target (lambda (x) (+ x 1)))
                  (fset 'neovm--combo-fset-filter (lambda (ret) (* ret 2)))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-fset-target :filter-return 'neovm--combo-fset-filter)
                        (fset 'neovm--combo-fset-target (lambda (x) (+ x 10)))
                        (list
                          (funcall 'neovm--combo-fset-target 3)
                          (neovm--combo-fset-target 3)))
                    (condition-case nil
                        (advice-remove 'neovm--combo-fset-target 'neovm--combo-fset-filter)
                      (error nil))
                    (fmakunbound 'neovm--combo-fset-target)
                    (fmakunbound 'neovm--combo-fset-filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_defalias_to_advised_symbol_call_paths() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(progn
                  (fset 'neovm--combo-alias-target (lambda (x) (+ x 1)))
                  (defalias 'neovm--combo-alias 'neovm--combo-alias-target)
                  (fset 'neovm--combo-alias-filter (lambda (ret) (* ret 3)))
                  (unwind-protect
                      (progn
                        (advice-add 'neovm--combo-alias-target :filter-return 'neovm--combo-alias-filter)
                        (list
                          (funcall 'neovm--combo-alias-target 2)
                          (neovm--combo-alias-target 2)
                          (funcall 'neovm--combo-alias 2)
                          (neovm--combo-alias 2)))
                    (condition-case nil
                        (advice-remove 'neovm--combo-alias-target 'neovm--combo-alias-filter)
                      (error nil))
                    (fmakunbound 'neovm--combo-alias-target)
                    (fmakunbound 'neovm--combo-alias)
                    (fmakunbound 'neovm--combo-alias-filter)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_catch_throw_non_symbol_tag_basics() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(list
                  (catch 1 (throw 1 'int-tag))
                  (let ((tag (list 'a)))
                    (catch tag (throw tag 'cons-tag))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_catch_throw_tag_identity_uses_eq() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(condition-case err
                  (let ((tag (list 'a)))
                    (catch tag (throw (list 'a) 'mismatch)))
                (no-catch (car err)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_catch_tag_expression_evaluated_once() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((n 0))
                  (list
                    (catch (progn (setq n (1+ n)) 'neovm--combo-once-tag)
                      (throw 'neovm--combo-once-tag n))
                    n))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_non_symbol_tag_throw_through_condition_case() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag (list 'neovm--combo-nsym)))
                  (catch tag
                    (condition-case nil
                        (throw tag 96)
                      (error 'caught))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_through_condition_case_with_no_catch_clause() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(catch 'neovm--combo-nc-tag
                  (condition-case err
                      (throw 'neovm--combo-nc-tag 97)
                    (no-catch (list 'handled (car err)))
                    (error 'caught)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_non_symbol_throw_with_no_catch_clause() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((tag (list 'neovm--combo-nc-nsym)))
                  (catch tag
                    (condition-case err
                        (throw tag 98)
                      (no-catch (list 'handled (car err)))
                      (error 'caught))))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_from_funcall_inside_condition_case() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(catch 'neovm--combo-funcall-tag
                  (condition-case nil
                      (funcall (lambda () (throw 'neovm--combo-funcall-tag 99)))
                    (error 'caught)))";
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_combination_throw_from_while_inside_condition_case() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((i 0))
                  (catch 'neovm--combo-while-tag
                    (condition-case nil
                        (progn
                          (while t
                            (setq i (1+ i))
                            (if (= i 3)
                                (throw 'neovm--combo-while-tag i))))
                      (error 'caught))))";
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

    #[test]
    fn oracle_prop_combination_around_advice_call_path_matrix_consistency(
        n in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn
               (fset 'neovm--combo-around-path-target (lambda (x) (* 2 x)))
               (fset 'neovm--combo-around-path
                     (lambda (orig x) (+ 1 (funcall orig x))))
               (unwind-protect
                   (progn
                     (advice-add 'neovm--combo-around-path-target :around 'neovm--combo-around-path)
                     (list
                       (funcall 'neovm--combo-around-path-target {n})
                       (apply 'neovm--combo-around-path-target (list {n}))
                       (neovm--combo-around-path-target {n})
                       (eval '(neovm--combo-around-path-target {n}))))
                 (condition-case nil
                     (advice-remove 'neovm--combo-around-path-target 'neovm--combo-around-path)
                   (error nil))
                 (fmakunbound 'neovm--combo-around-path-target)
                 (fmakunbound 'neovm--combo-around-path)))",
            n = n,
        );

        let expected = 2 * n + 1;
        let expected_payload = format!("({expected} {expected} {expected} {expected})");
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected_payload.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_filter_args_call_path_matrix_consistency(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn
               (fset 'neovm--combo-fargs-prop-target (lambda (x y) (+ x y)))
               (fset 'neovm--combo-fargs-prop
                     (lambda (args)
                       (list (+ {a} (car args))
                             (+ {b} (car (cdr args))))))
               (unwind-protect
                   (progn
                     (advice-add 'neovm--combo-fargs-prop-target :filter-args 'neovm--combo-fargs-prop)
                     (list
                       (funcall 'neovm--combo-fargs-prop-target 3 4)
                       (apply 'neovm--combo-fargs-prop-target '(3 4))
                       (neovm--combo-fargs-prop-target 3 4)
                       (eval '(neovm--combo-fargs-prop-target 3 4))))
                 (condition-case nil
                     (advice-remove 'neovm--combo-fargs-prop-target 'neovm--combo-fargs-prop)
                   (error nil))
                 (fmakunbound 'neovm--combo-fargs-prop-target)
                 (fmakunbound 'neovm--combo-fargs-prop)))",
            a = a,
            b = b,
        );

        let expected = (a + 3) + (b + 4);
        let expected_payload = format!("({expected} {expected} {expected} {expected})");
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected_payload.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_error_or_throw_with_cleanup_state(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
        d in -10_000i64..10_000i64,
        signal_arith in any::<bool>(),
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let flow = if signal_arith {
            "(/ 1 0)"
        } else {
            "(throw 'neovm--combo-tag (+ x C))"
        };
        let form = format!(
            "(let ((x {a}))
               (list
                 (condition-case _err
                     (catch 'neovm--combo-tag
                       (unwind-protect
                           (progn
                             (setq x (+ x {b}))
                             {flow})
                         (setq x (+ x {d}))))
                   (arith-error 'arith))
                 x))",
            a = a,
            b = b,
            d = d,
            flow = flow.replace("C", &c.to_string()),
        );

        let x_after = a + b + d;
        let first = if signal_arith {
            "arith".to_string()
        } else {
            (a + b + c).to_string()
        };
        let expected = format!("({first} {x_after})");
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_macro_parameterized_tag_value_roundtrip(
        v in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn
               (defmacro neovm--combo-catch-throw-param (tag value)
                 `(catch ,tag (throw ,tag ,value)))
               (unwind-protect
                   (neovm--combo-catch-throw-param 'neovm--combo-prop-tag {})
                 (fmakunbound 'neovm--combo-catch-throw-param)))",
            v
        );
        let expected = v.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_macro_tag_condition_case_roundtrip(
        v in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn
               (defmacro neovm--combo-with-tag-cc (tag value)
                 `(catch ,tag
                    (condition-case err
                        (throw ,tag ,value)
                      (error (list 'err (car err))))))
               (unwind-protect
                   (neovm--combo-with-tag-cc 'neovm--combo-prop-cc {})
                 (fmakunbound 'neovm--combo-with-tag-cc)))",
            v
        );
        let expected = v.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_macro_wrapped_condition_case_throw_roundtrip(
        v in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(progn
               (defmacro neovm--combo-cc-throw (tag value)
                 `(condition-case err
                      (throw ,tag ,value)
                    (error (list 'err (car err)))))
               (unwind-protect
                   (let ((tag 'neovm--combo-prop-dyn-tag))
                     (catch tag
                       (neovm--combo-cc-throw tag {})))
                 (fmakunbound 'neovm--combo-cc-throw)))",
            v
        );
        let expected = v.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_throw_through_condition_case_roundtrip(
        v in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(let ((tag 'neovm--combo-prop-cc-pass-tag))
               (catch tag
                 (condition-case nil
                     (progn (throw tag {}) 'tail)
                   (arith-error 'arith)
                   (wrong-type-argument 'wta))))",
            v
        );
        let expected = v.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_throw_not_caught_by_condition_case_roundtrip(
        v in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(catch 'neovm--combo-cc-prop-tag
               (condition-case nil
                   (throw 'neovm--combo-cc-prop-tag {})
                 (error 'caught)))",
            v
        );
        let expected = v.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_integer_tag_throw_roundtrip(
        tag in -1000i64..1000i64,
        value in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!("(catch {} (throw {} {}))", tag, tag, value);
        let expected = value.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_integer_tag_throw_through_condition_case_roundtrip(
        tag in -1000i64..1000i64,
        value in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(catch {}
               (condition-case nil
                   (throw {} {})
                 (error 'caught)))",
            tag, tag, value
        );
        let expected = value.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_combination_throw_from_funcall_through_condition_case_roundtrip(
        value in -10_000i64..10_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let form = format!(
            "(catch 'neovm--combo-funcall-prop-tag
               (condition-case nil
                   (funcall (lambda () (throw 'neovm--combo-funcall-prop-tag {})))
                 (error 'caught)))",
            value
        );
        let expected = value.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
