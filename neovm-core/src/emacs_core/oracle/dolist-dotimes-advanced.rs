//! Oracle parity tests for advanced `dolist` and `dotimes` patterns:
//! result forms, nested loops, early return, accumulation,
//! and complex iteration patterns.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use super::common::{assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm};

// ---------------------------------------------------------------------------
// dolist with result form
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dolist_result_form() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // dolist returns the RESULT form value
    let form = r#"(let ((sum 0))
                    (dolist (x '(1 2 3 4 5) sum)
                      (setq sum (+ sum x))))"#;
    let (o, n) = eval_oracle_and_neovm(form);
    assert_ok_eq("15", &o, &n);
}

#[test]
fn oracle_prop_dolist_result_complex() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Result form can reference the loop variable (it's nil after loop)
    let form = r#"(let ((collected nil))
                    (dolist (x '(a b c d) (list x (nreverse collected)))
                      (setq collected (cons x collected))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// dotimes with result form
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dotimes_result_form() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let ((result nil))
                    (dotimes (i 5 (nreverse result))
                      (setq result (cons (* i i) result))))"#;
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_dotimes_result_var_value() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // In result form, loop var equals the count
    let form = r#"(let ((log nil))
                    (dotimes (i 4 (list i (nreverse log)))
                      (setq log (cons i log))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Nested dolist/dotimes
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dolist_nested_cartesian() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Cartesian product of two lists
    let form = r#"(let ((result nil))
                    (dolist (x '(a b c))
                      (dolist (y '(1 2 3))
                        (setq result (cons (list x y) result))))
                    (nreverse result))"#;
    assert_oracle_parity(form);
}

#[test]
fn oracle_prop_dotimes_nested_matrix() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Build a matrix using dotimes
    let form = r#"(let ((rows nil))
                    (dotimes (i 3)
                      (let ((row nil))
                        (dotimes (j 4)
                          (setq row (cons (+ (* i 4) j) row)))
                        (setq rows (cons (nreverse row) rows))))
                    (nreverse rows))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// dolist with filtering
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dolist_filter_partition() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Partition list by predicate
    let form = r#"(let ((evens nil) (odds nil))
                    (dolist (x '(1 2 3 4 5 6 7 8 9 10))
                      (if (= (% x 2) 0)
                          (setq evens (cons x evens))
                        (setq odds (cons x odds))))
                    (list (nreverse evens) (nreverse odds)))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// dotimes for string building
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dotimes_string_builder() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Build a diamond pattern
    let form = r#"(let ((lines nil) (size 5))
                    ;; Top half
                    (dotimes (i size)
                      (let ((spaces (make-string (- size i 1) ?\ ))
                            (stars (make-string (1+ (* 2 i)) ?*)))
                        (setq lines (cons (concat spaces stars) lines))))
                    ;; Bottom half (excluding middle)
                    (dotimes (i (1- size))
                      (let* ((row (- size i 2))
                             (spaces (make-string (- size row 1) ?\ ))
                             (stars (make-string (1+ (* 2 row)) ?*)))
                        (setq lines (cons (concat spaces stars) lines))))
                    (mapconcat #'identity (nreverse lines) "\n"))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: dolist with accumulator patterns
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dolist_group_consecutive() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Group consecutive equal elements
    let form = r#"(let ((input '(a a a b b c a a d d d d))
                        (groups nil)
                        (current nil)
                        (count 0))
                    (dolist (x input)
                      (if (eq x current)
                          (setq count (1+ count))
                        (when current
                          (setq groups (cons (list current count) groups)))
                        (setq current x count 1)))
                    ;; Don't forget last group
                    (when current
                      (setq groups (cons (list current count) groups)))
                    (nreverse groups))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: dotimes for prime sieve
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dotimes_sieve() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = r#"(let* ((limit 50)
                         (sieve (make-vector (1+ limit) t)))
                    (aset sieve 0 nil)
                    (aset sieve 1 nil)
                    (dotimes (i (1+ limit))
                      (when (and (>= i 2) (aref sieve i))
                        (let ((j (* i i)))
                          (while (<= j limit)
                            (aset sieve j nil)
                            (setq j (+ j i))))))
                    ;; Collect primes
                    (let ((primes nil))
                      (dotimes (i (1+ limit) (nreverse primes))
                        (when (aref sieve i)
                          (setq primes (cons i primes))))))"#;
    assert_oracle_parity(form);
}

// ---------------------------------------------------------------------------
// Complex: dolist with multiple accumulators and early termination
// ---------------------------------------------------------------------------

#[test]
fn oracle_prop_dolist_multi_accumulator() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    // Process records with multiple aggregators
    let form = r#"(let ((data '((sale "Alice" 100)
                                 (refund "Bob" 50)
                                 (sale "Carol" 200)
                                 (sale "Alice" 150)
                                 (refund "Dave" 30)
                                 (sale "Bob" 80))))
                    (let ((total-sales 0)
                          (total-refunds 0)
                          (by-person (make-hash-table :test 'equal)))
                      (dolist (rec data)
                        (let ((type (nth 0 rec))
                              (name (nth 1 rec))
                              (amount (nth 2 rec)))
                          (if (eq type 'sale)
                              (setq total-sales (+ total-sales amount))
                            (setq total-refunds (+ total-refunds amount)))
                          (let ((sign (if (eq type 'sale) 1 -1)))
                            (puthash name
                                     (+ (gethash name by-person 0)
                                        (* sign amount))
                                     by-person))))
                      ;; Collect per-person totals
                      (let ((person-totals nil))
                        (maphash (lambda (k v)
                                   (setq person-totals
                                         (cons (cons k v) person-totals)))
                                 by-person)
                        (list total-sales total-refunds
                              (- total-sales total-refunds)
                              (sort person-totals
                                    (lambda (a b)
                                      (string< (car a) (car b))))))))"#;
    assert_oracle_parity(form);
}
