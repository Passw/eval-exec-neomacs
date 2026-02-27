//! Oracle parity tests for `copy-alist`.

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled};

#[test]
fn oracle_prop_copy_alist_basics() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_copy_alist_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (o, n) = eval_oracle_and_neovm("(copy-alist '((a . 1) (b . 2) (c . 3)))");
    assert_ok_eq("((a . 1) (b . 2) (c . 3))", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(copy-alist nil)");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(copy-alist '((x . 10)))");
    assert_ok_eq("((x . 10))", &o, &n);

    // verify it's a distinct copy (setcdr on copy doesn't affect original)
    let (o, n) = eval_oracle_and_neovm(
        "(let* ((orig '((k . 1))) (cp (copy-alist orig))) (setcdr (car cp) 99) (cdar orig))",
    );
    assert_ok_eq("1", &o, &n);
}
