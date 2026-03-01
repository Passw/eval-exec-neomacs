//! Complex oracle tests for macro-heavy patterns: hygiene simulation,
//! anaphoric macros, iteration macros, macro-based DSLs,
//! and macro-generated data structures.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

// ---------------------------------------------------------------------------
// Anaphoric macros (using `it` binding)
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_macro_anaphoric_when() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (defmacro neovm--test-awhen (test &rest body)
                      `(let ((it ,test))
                         (when it ,@body)))
                    (defmacro neovm--test-aif (test then &optional else)
                      `(let ((it ,test))
                         (if it ,then ,else)))
                    (unwind-protect
                        (list
                         ;; awhen: `it` bound to test result
                         (neovm--test-awhen (assq 'x '((x . 42) (y . 99)))
                           (cdr it))
                         ;; awhen: nil test → nil
                         (neovm--test-awhen (assq 'z '((x . 42)))
                           (cdr it))
                         ;; aif: true branch
                         (neovm--test-aif (memq 3 '(1 2 3 4 5))
                           (length it)
                           'not-found)
                         ;; aif: false branch
                         (neovm--test-aif (memq 9 '(1 2 3))
                           (length it)
                           'not-found))
                      (fmakunbound 'neovm--test-awhen)
                      (fmakunbound 'neovm--test-aif)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Macro: with-gensyms for hygiene
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_macro_with_gensyms() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (defmacro neovm--test-once-only (var expr &rest body)
                      (let ((g (make-symbol "val")))
                        `(let ((,g ,expr))
                           (let ((,var ,g))
                             ,@body))))
                    (unwind-protect
                        (let ((counter 0))
                          (neovm--test-once-only x (progn
                                                     (setq counter (1+ counter))
                                                     42)
                            ;; x should be 42, counter should be 1
                            ;; even though we reference x twice
                            (list x x counter)))
                      (fmakunbound 'neovm--test-once-only)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Macro: accumulation loop
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_macro_loop_collect() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (defmacro neovm--test-collecting (&rest body)
                      `(let ((neovm--collect-acc nil))
                         (cl-flet ((collect (x)
                                     (setq neovm--collect-acc
                                           (cons x neovm--collect-acc))))
                           ,@body)
                         (nreverse neovm--collect-acc)))
                    (unwind-protect
                        (neovm--test-collecting
                         (dolist (x '(1 2 3 4 5 6 7 8 9 10))
                           (when (= (% x 2) 0)
                             (collect (* x x)))))
                      (fmakunbound 'neovm--test-collecting)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Macro: bind-keys DSL
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_macro_bind_keys_dsl() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Macro that generates a keymap from a binding spec
    let form = r#"(progn
                    (defmacro neovm--test-defkeys (name &rest bindings)
                      `(let ((map (make-sparse-keymap)))
                         ,@(mapcar
                            (lambda (b)
                              `(define-key map ,(car b) ,(cadr b)))
                            bindings)
                         (fset ',name (lambda () map))
                         ',name))
                    (unwind-protect
                        (progn
                          (neovm--test-defkeys neovm--test-my-keys
                            ("a" 'self-insert-command)
                            ("b" 'backward-char))
                          (let ((map (funcall 'neovm--test-my-keys)))
                            (list (keymapp map)
                                  (lookup-key map "a")
                                  (lookup-key map "b"))))
                      (fmakunbound 'neovm--test-defkeys)
                      (fmakunbound 'neovm--test-my-keys)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Macro: define-struct (record constructor)
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_macro_define_struct() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Macro that generates constructor and accessors for a simple struct
    let form = r#"(progn
                    (defmacro neovm--test-defstruct (name &rest fields)
                      (let ((constructor
                             (intern (concat "make-" (symbol-name name))))
                            (accessors
                             (let ((i 0) (acc nil))
                               (dolist (f fields)
                                 (setq acc
                                       (cons (list f i) acc)
                                       i (1+ i)))
                               (nreverse acc))))
                        `(progn
                           (fset ',constructor
                                 (lambda (&rest args)
                                   (apply #'vector ',name args)))
                           ,@(mapcar
                              (lambda (a)
                                (let ((accessor
                                       (intern (concat (symbol-name name)
                                                       "-"
                                                       (symbol-name (car a))))))
                                  `(fset ',accessor
                                         (lambda (obj)
                                           (aref obj ,(1+ (cadr a)))))))
                              accessors)
                           ',name)))
                    (unwind-protect
                        (progn
                          (neovm--test-defstruct point x y z)
                          (let ((p (funcall 'make-point 1 2 3)))
                            (list p
                                  (funcall 'point-x p)
                                  (funcall 'point-y p)
                                  (funcall 'point-z p))))
                      (fmakunbound 'neovm--test-defstruct)
                      (fmakunbound 'make-point)
                      (fmakunbound 'point-x)
                      (fmakunbound 'point-y)
                      (fmakunbound 'point-z)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Macro: chained let (threading macro)
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_macro_thread_first() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Thread-first macro: (-> x (f a) (g b)) => (g (f x a) b)
    let form = r#"(progn
                    (defmacro neovm--test--> (x &rest forms)
                      (let ((result x))
                        (dolist (form forms)
                          (if (listp form)
                              (setq result
                                    (append (list (car form) result)
                                            (cdr form)))
                            (setq result (list form result))))
                        result))
                    (unwind-protect
                        (list
                         (neovm--test--> 5
                           (+ 3)
                           (* 2)
                           (- 1))
                         ;; (- (* (+ 5 3) 2) 1) = 15
                         (neovm--test--> "hello"
                           (concat " world")
                           upcase
                           (concat "!" )))
                      (fmakunbound 'neovm--test-->)))"#;
    assert_oracle_parity(form);
}
