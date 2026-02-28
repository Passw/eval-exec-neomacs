//! Oracle parity tests for hash-table operations.

use super::common::{assert_ok_eq, eval_oracle_and_neovm};

#[test]
fn oracle_prop_hash_table_put_get() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table :test 'equal))) (puthash \"key\" 42 h) (gethash \"key\" h))",
    );
    assert_ok_eq("42", &o, &n);

    // missing key returns nil
    let (o, n) = eval_oracle_and_neovm("(let ((h (make-hash-table))) (gethash 'missing h))");
    assert_ok_eq("nil", &o, &n);

    // missing key with default
    let (o, n) =
        eval_oracle_and_neovm("(let ((h (make-hash-table))) (gethash 'missing h 'fallback))");
    assert_ok_eq("fallback", &o, &n);

    // overwrite existing key
    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table))) (puthash 'k 1 h) (puthash 'k 2 h) (gethash 'k h))",
    );
    assert_ok_eq("2", &o, &n);
}

#[test]
fn oracle_prop_hash_table_remhash() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table))) (puthash 'a 1 h) (remhash 'a h) (gethash 'a h))",
    );
    assert_ok_eq("nil", &o, &n);

    // remhash on missing key is harmless
    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table))) (remhash 'gone h) (hash-table-count h))",
    );
    assert_ok_eq("0", &o, &n);
}

#[test]
fn oracle_prop_hash_table_count() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table))) (puthash 'a 1 h) (puthash 'b 2 h) (puthash 'c 3 h) (hash-table-count h))",
    );
    assert_ok_eq("3", &o, &n);

    // empty table
    let (o, n) = eval_oracle_and_neovm("(hash-table-count (make-hash-table))");
    assert_ok_eq("0", &o, &n);
}

#[test]
fn oracle_prop_hash_table_p() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (o, n) = eval_oracle_and_neovm("(hash-table-p (make-hash-table))");
    assert_ok_eq("t", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(hash-table-p '(not a table))");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(hash-table-p 42)");
    assert_ok_eq("nil", &o, &n);
}

#[test]
fn oracle_prop_hash_table_clrhash() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table))) (puthash 'x 1 h) (puthash 'y 2 h) (clrhash h) (hash-table-count h))",
    );
    assert_ok_eq("0", &o, &n);
}

#[test]
fn oracle_prop_hash_table_equal_structural_keys() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table :test 'equal))) (puthash (list 1 2 3) 'hit h) (gethash (list 1 2 3) h))",
    );
    assert_ok_eq("hit", &o, &n);

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table :test 'equal))) (puthash [1 2 3] 'vec h) (gethash [1 2 3] h))",
    );
    assert_ok_eq("vec", &o, &n);

    let (o, n) = eval_oracle_and_neovm(
        "(let ((h (make-hash-table :test 'equal))) (puthash (list 1 2) 'a h) (puthash (list 1 2) 'b h) (list (hash-table-count h) (gethash (list 1 2) h)))",
    );
    assert_ok_eq("(1 b)", &o, &n);
}
