//! Complex oracle tests for recursion patterns: mutual recursion,
//! tree traversal, recursive descent parsing, recursive data
//! transformers, and recursion with memoization.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

// ---------------------------------------------------------------------------
// Mutual recursion: even/odd
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_recursion_mutual() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (fset 'neovm--test-my-even
                      (lambda (n)
                        (if (= n 0) t
                          (funcall 'neovm--test-my-odd (1- n)))))
                    (fset 'neovm--test-my-odd
                      (lambda (n)
                        (if (= n 0) nil
                          (funcall 'neovm--test-my-even (1- n)))))
                    (unwind-protect
                        (list
                         (funcall 'neovm--test-my-even 0)
                         (funcall 'neovm--test-my-even 1)
                         (funcall 'neovm--test-my-even 10)
                         (funcall 'neovm--test-my-odd 7)
                         (funcall 'neovm--test-my-odd 8))
                      (fmakunbound 'neovm--test-my-even)
                      (fmakunbound 'neovm--test-my-odd)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Tree operations
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_recursion_tree_depth() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (fset 'neovm--test-tree-depth
                      (lambda (tree)
                        (if (atom tree) 0
                          (1+ (max (funcall 'neovm--test-tree-depth
                                            (car tree))
                                   (funcall 'neovm--test-tree-depth
                                            (cdr tree)))))))
                    (unwind-protect
                        (list
                         (funcall 'neovm--test-tree-depth nil)
                         (funcall 'neovm--test-tree-depth '(a))
                         (funcall 'neovm--test-tree-depth '(a (b (c))))
                         (funcall 'neovm--test-tree-depth
                                  '((a b) (c (d e)) f)))
                      (fmakunbound 'neovm--test-tree-depth)))"#;
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_recursion_tree_flatten() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (fset 'neovm--test-flatten
                      (lambda (tree)
                        (cond
                         ((null tree) nil)
                         ((atom tree) (list tree))
                         (t (append
                             (funcall 'neovm--test-flatten (car tree))
                             (funcall 'neovm--test-flatten (cdr tree)))))))
                    (unwind-protect
                        (list
                         (funcall 'neovm--test-flatten nil)
                         (funcall 'neovm--test-flatten '(1 2 3))
                         (funcall 'neovm--test-flatten '(1 (2 (3 (4))) 5))
                         (funcall 'neovm--test-flatten
                                  '((a b) ((c)) (d (e (f))))))
                      (fmakunbound 'neovm--test-flatten)))"#;
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_recursion_tree_map() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Map over a tree, transforming every leaf
    let form = r#"(progn
                    (fset 'neovm--test-tree-map
                      (lambda (fn tree)
                        (cond
                         ((null tree) nil)
                         ((atom tree) (funcall fn tree))
                         (t (cons (funcall 'neovm--test-tree-map
                                           fn (car tree))
                                  (funcall 'neovm--test-tree-map
                                           fn (cdr tree)))))))
                    (unwind-protect
                        (list
                         (funcall 'neovm--test-tree-map
                                  #'1+ '(1 (2 (3)) (4 5)))
                         (funcall 'neovm--test-tree-map
                                  #'symbol-name '(a (b c) (d (e f)))))
                      (fmakunbound 'neovm--test-tree-map)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Recursive descent: arithmetic expression parser
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_recursion_arith_parser() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Parse and evaluate simple arithmetic: +, *, parens, numbers
    let form = r#"(progn
                    (defvar neovm--test-parse-tokens nil)
                    (fset 'neovm--test-peek
                      (lambda () (car neovm--test-parse-tokens)))
                    (fset 'neovm--test-consume
                      (lambda ()
                        (prog1 (car neovm--test-parse-tokens)
                          (setq neovm--test-parse-tokens
                                (cdr neovm--test-parse-tokens)))))
                    ;; expr = term (('+' term)*)
                    (fset 'neovm--test-parse-expr
                      (lambda ()
                        (let ((left (funcall 'neovm--test-parse-term)))
                          (while (eq (funcall 'neovm--test-peek) '+)
                            (funcall 'neovm--test-consume)
                            (setq left (+ left (funcall 'neovm--test-parse-term))))
                          left)))
                    ;; term = factor (('*' factor)*)
                    (fset 'neovm--test-parse-term
                      (lambda ()
                        (let ((left (funcall 'neovm--test-parse-factor)))
                          (while (eq (funcall 'neovm--test-peek) '*)
                            (funcall 'neovm--test-consume)
                            (setq left (* left (funcall 'neovm--test-parse-factor))))
                          left)))
                    ;; factor = number | '(' expr ')'
                    (fset 'neovm--test-parse-factor
                      (lambda ()
                        (let ((tok (funcall 'neovm--test-peek)))
                          (cond
                           ((numberp tok)
                            (funcall 'neovm--test-consume))
                           ((eq tok 'lp)
                            (funcall 'neovm--test-consume)
                            (let ((val (funcall 'neovm--test-parse-expr)))
                              (funcall 'neovm--test-consume) ;; rp
                              val))))))
                    (unwind-protect
                        (list
                         ;; 2 + 3 * 4 = 14
                         (progn
                           (setq neovm--test-parse-tokens
                                 '(2 + 3 * 4))
                           (funcall 'neovm--test-parse-expr))
                         ;; (2 + 3) * 4 = 20
                         (progn
                           (setq neovm--test-parse-tokens
                                 '(lp 2 + 3 rp * 4))
                           (funcall 'neovm--test-parse-expr))
                         ;; 1 + 2 + 3 + 4 = 10
                         (progn
                           (setq neovm--test-parse-tokens
                                 '(1 + 2 + 3 + 4))
                           (funcall 'neovm--test-parse-expr)))
                      (fmakunbound 'neovm--test-peek)
                      (fmakunbound 'neovm--test-consume)
                      (fmakunbound 'neovm--test-parse-expr)
                      (fmakunbound 'neovm--test-parse-term)
                      (fmakunbound 'neovm--test-parse-factor)
                      (makunbound 'neovm--test-parse-tokens)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Recursive JSON-like pretty printer
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_recursion_pretty_print() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (fset 'neovm--test-pp-json
                      (lambda (obj indent)
                        (let ((pad (make-string (* indent 2) ?\ )))
                          (cond
                           ((null obj) "null")
                           ((eq obj t) "true")
                           ((numberp obj) (number-to-string obj))
                           ((stringp obj) (format "\"%s\"" obj))
                           ;; alist → object
                           ((and (consp obj) (consp (car obj))
                                 (symbolp (caar obj)))
                            (concat "{\n"
                                    (mapconcat
                                     (lambda (pair)
                                       (format "%s  \"%s\": %s"
                                               pad
                                               (symbol-name (car pair))
                                               (funcall 'neovm--test-pp-json
                                                        (cdr pair)
                                                        (1+ indent))))
                                     obj
                                     (concat ",\n"))
                                    "\n" pad "}"))
                           ;; list → array
                           ((listp obj)
                            (concat "[\n"
                                    (mapconcat
                                     (lambda (elem)
                                       (concat pad "  "
                                               (funcall 'neovm--test-pp-json
                                                        elem
                                                        (1+ indent))))
                                     obj
                                     (concat ",\n"))
                                    "\n" pad "]"))
                           (t "?")))))
                    (unwind-protect
                        (funcall 'neovm--test-pp-json
                                 '((name . "Alice")
                                   (age . 30)
                                   (scores . (95 87 92))
                                   (active . t))
                                 0)
                      (fmakunbound 'neovm--test-pp-json)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Recursive substitution (like subst-char-in-string for trees)
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_recursion_tree_subst() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(progn
                    (fset 'neovm--test-tree-subst
                      (lambda (old new tree)
                        (cond
                         ((equal tree old) new)
                         ((atom tree) tree)
                         (t (cons (funcall 'neovm--test-tree-subst
                                           old new (car tree))
                                  (funcall 'neovm--test-tree-subst
                                           old new (cdr tree)))))))
                    (unwind-protect
                        (list
                         (funcall 'neovm--test-tree-subst
                                  'x 42 '(x (y x) (x z)))
                         (funcall 'neovm--test-tree-subst
                                  '(a b) '(replaced)
                                  '(start (a b) middle (a b) end)))
                      (fmakunbound 'neovm--test-tree-subst)))"#;
    assert_oracle_parity(form);
}
