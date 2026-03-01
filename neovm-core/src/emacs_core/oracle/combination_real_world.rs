//! Complex oracle tests simulating real-world Emacs Lisp patterns.
//!
//! Tests patterns from actual Emacs packages: mode-line formatters,
//! font-lock keyword builders, completion systems, and configuration
//! management.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

// ---------------------------------------------------------------------------
// Mode-line formatter
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_mode_line_formatter() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let ((segments
                     (list
                      (lambda (info)
                        (plist-get info :buffer-name))
                      (lambda (info)
                        (format "[%s]"
                                (if (plist-get info :modified)
                                    "+" "-")))
                      (lambda (info)
                        (format "L%d"
                                (plist-get info :line)))
                      (lambda (info)
                        (format "(%s)"
                                (plist-get info :mode))))))
                    (let ((info '(:buffer-name "test.el"
                                  :modified t
                                  :line 42
                                  :mode "emacs-lisp")))
                      (mapconcat
                       (lambda (seg) (funcall seg info))
                       segments " ")))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Configuration system
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_config_system() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Layered configuration: defaults < user < project
    let form = r#"(let ((defaults (make-hash-table :test 'equal))
                        (user (make-hash-table :test 'equal))
                        (project (make-hash-table :test 'equal)))
                    ;; Defaults
                    (puthash "indent" 4 defaults)
                    (puthash "tabs" nil defaults)
                    (puthash "encoding" "utf-8" defaults)
                    (puthash "line-width" 80 defaults)
                    ;; User overrides
                    (puthash "indent" 2 user)
                    (puthash "theme" "dark" user)
                    ;; Project overrides
                    (puthash "line-width" 100 project)
                    (puthash "tabs" t project)
                    ;; Lookup: project > user > defaults
                    (let ((get-config
                           (lambda (key)
                             (let ((val (gethash key project 'miss)))
                               (when (eq val 'miss)
                                 (setq val (gethash key user 'miss)))
                               (when (eq val 'miss)
                                 (setq val (gethash key defaults nil)))
                               val))))
                      (list (funcall get-config "indent")
                            (funcall get-config "tabs")
                            (funcall get-config "encoding")
                            (funcall get-config "line-width")
                            (funcall get-config "theme"))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Hook system
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_hook_system() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Simple hook system like Emacs hooks
    let form = "(let ((hooks (make-hash-table))
                      (log nil))
                  (let ((add-hook
                         (lambda (hook fn)
                           (puthash hook
                                    (append (gethash hook hooks nil)
                                            (list fn))
                                    hooks)))
                        (run-hooks
                         (lambda (hook &rest args)
                           (dolist (fn (gethash hook hooks nil))
                             (apply fn args)))))
                    ;; Add hooks
                    (funcall add-hook 'before-save
                             (lambda ()
                               (setq log (cons 'trim-whitespace log))))
                    (funcall add-hook 'before-save
                             (lambda ()
                               (setq log (cons 'add-newline log))))
                    (funcall add-hook 'after-save
                             (lambda ()
                               (setq log (cons 'compile log))))
                    ;; Run hooks
                    (funcall run-hooks 'before-save)
                    (funcall run-hooks 'after-save)
                    (funcall run-hooks 'undefined-hook)
                    (nreverse log)))";
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Text property-based highlighting rules
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_keyword_highlight() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Simulate font-lock keyword matching and counting
    let form = r#"(with-temp-buffer
                    (insert "defun hello (x y)\n  (let ((z 10))\n")
                    (insert "    (if (> x y) z nil)))\n")
                    (goto-char (point-min))
                    (let ((keywords
                           '("defun" "let" "if" "nil" "t"
                             "when" "unless" "cond"))
                          (found nil))
                      (dolist (kw keywords)
                        (goto-char (point-min))
                        (let ((count 0))
                          (while (re-search-forward
                                  (concat "\\b" (regexp-quote kw) "\\b")
                                  nil t)
                            (setq count (1+ count)))
                          (when (> count 0)
                            (setq found
                                  (cons (cons kw count) found)))))
                      (sort (nreverse found)
                            (lambda (a b)
                              (string-lessp (car a) (car b))))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Undo/redo system
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_undo_redo() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(let ((state 0)
                      (undo-stack nil)
                      (redo-stack nil))
                  (let ((do-action
                         (lambda (new-state)
                           (setq undo-stack (cons state undo-stack)
                                 redo-stack nil
                                 state new-state)))
                        (undo
                         (lambda ()
                           (when undo-stack
                             (setq redo-stack (cons state redo-stack)
                                   state (car undo-stack)
                                   undo-stack (cdr undo-stack)))))
                        (redo
                         (lambda ()
                           (when redo-stack
                             (setq undo-stack (cons state undo-stack)
                                   state (car redo-stack)
                                   redo-stack (cdr redo-stack))))))
                    (funcall do-action 1)
                    (funcall do-action 2)
                    (funcall do-action 3)
                    (let ((after-3 state))
                      (funcall undo)
                      (let ((after-undo-1 state))
                        (funcall undo)
                        (let ((after-undo-2 state))
                          (funcall redo)
                          (let ((after-redo state))
                            ;; New action clears redo stack
                            (funcall do-action 99)
                            (funcall redo)
                            (list after-3 after-undo-1 after-undo-2
                                  after-redo state
                                  (length redo-stack))))))))";
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Simple S-expression formatter
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_sexp_formatter() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Pretty-print an S-expression with indentation
    let form = "(progn
  (fset 'neovm--test-pp
    (lambda (form indent)
      (cond
        ((null form) \"nil\")
        ((atom form) (prin1-to-string form))
        ((and (consp form)
              (memq (car form) '(let progn if)))
         (let ((parts (list (format \"%s(%s\"
                                    (make-string indent ?\\s)
                                    (symbol-name (car form))))))
           (dolist (sub (cdr form))
             (setq parts
                   (cons (funcall 'neovm--test-pp
                                  sub (+ indent 2))
                         parts)))
           (setq parts (cons \")\" parts))
           (mapconcat #'identity (nreverse parts) \"\\n\")))
        (t (format \"%s%s\"
                   (make-string indent ?\\s)
                   (prin1-to-string form))))))
  (unwind-protect
      (funcall 'neovm--test-pp
               '(let ((x 1)) (if x (progn x) nil))
               0)
    (fmakunbound 'neovm--test-pp)))";
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Dependency graph resolver
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_rw_dependency_ordering() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Simple topological sort for package dependencies
    let form = "(let ((deps (make-hash-table)))
                  (puthash 'app '(lib-a lib-b) deps)
                  (puthash 'lib-a '(core utils) deps)
                  (puthash 'lib-b '(core) deps)
                  (puthash 'core nil deps)
                  (puthash 'utils '(core) deps)
                  ;; Topological sort via DFS
                  (let ((visited (make-hash-table))
                        (order nil))
                    (fset 'neovm--test-dfs
                      (lambda (node)
                        (unless (gethash node visited)
                          (puthash node t visited)
                          (dolist (dep (gethash node deps nil))
                            (funcall 'neovm--test-dfs dep))
                          (setq order (cons node order)))))
                    (unwind-protect
                        (progn
                          (dolist (pkg '(app lib-a lib-b core utils))
                            (funcall 'neovm--test-dfs pkg))
                          (nreverse order))
                      (fmakunbound 'neovm--test-dfs))))";
    assert_oracle_parity(form);
}
