//! Oracle parity tests for advanced `progn`, `prog1`, `prog2`,
//! `progn` as implicit body, and complex sequential evaluation
//! patterns.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

// ---------------------------------------------------------------------------
// prog1 / prog2
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_prog1_basic() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let ((log nil))
                    (list
                     ;; prog1 returns first form's value
                     (prog1 42
                       (setq log (cons 'a log))
                       (setq log (cons 'b log)))
                     ;; But all forms are evaluated
                     (nreverse log)))"#;
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_prog2_basic() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let ((log nil))
                    (list
                     ;; prog2 returns second form's value
                     (prog2
                       (setq log (cons 'first log))
                       42
                       (setq log (cons 'third log)))
                     (nreverse log)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// prog1 for pop-like operations
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_prog1_pop_pattern() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Common pattern: return head and advance to tail
    let form = r#"(let ((stack '(a b c d e)))
                    (let ((items nil))
                      (dotimes (_ 3)
                        (setq items
                              (cons (prog1 (car stack)
                                      (setq stack (cdr stack)))
                                    items)))
                      (list (nreverse items) stack)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// progn as body in various constructs
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_progn_in_cond() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let ((x 15) (log nil))
                    (cond
                     ((< x 10)
                      (setq log (cons 'small log))
                      'small)
                     ((< x 20)
                      (setq log (cons 'check-1 log))
                      (setq log (cons 'check-2 log))
                      (setq log (cons (format "x=%d" x) log))
                      'medium)
                     (t 'large))
                    (nreverse log))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: sequential pipeline with intermediate results
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_progn_pipeline_with_logging() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let ((log nil))
                    (let ((result
                           (let ((data '(5 3 8 1 9 2 7)))
                             ;; Step 1: sort
                             (let ((sorted (sort (copy-sequence data) #'<)))
                               (setq log (cons (list 'sorted sorted) log))
                               ;; Step 2: take top 3
                               (let ((top3 (list (nth 0 sorted)
                                                 (nth 1 sorted)
                                                 (nth 2 sorted))))
                                 (setq log (cons (list 'top3 top3) log))
                                 ;; Step 3: compute mean
                                 (let ((mean (/ (float (apply #'+ top3))
                                                (length top3))))
                                   (setq log (cons (list 'mean mean) log))
                                   mean))))))
                      (list result (nreverse log))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: transaction-like pattern with prog1
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_prog1_transaction() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Return result but ensure cleanup happens
    let form = r#"(let ((resource-open nil)
                        (operations nil))
                    (let ((result
                           (prog1
                               (progn
                                 ;; "Acquire"
                                 (setq resource-open t)
                                 (setq operations
                                       (cons 'acquire operations))
                                 ;; "Do work"
                                 (setq operations
                                       (cons 'work operations))
                                 ;; Return value
                                 42)
                             ;; "Release" in cleanup
                             (setq operations
                                   (cons 'release operations))
                             (setq resource-open nil))))
                      (list result
                            resource-open
                            (nreverse operations))))"#;
    assert_oracle_parity(form);
}
