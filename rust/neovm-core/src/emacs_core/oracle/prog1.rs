//! Oracle parity tests for `prog1` and `prog2`.

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled};

#[test]
fn oracle_prop_prog1_basics() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_prog1_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (o, n) = eval_oracle_and_neovm("(prog1 10 20 30)");
    assert_ok_eq("10", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(prog1 'first)");
    assert_ok_eq("first", &o, &n);

    // side effects still happen
    let (o, n) = eval_oracle_and_neovm(
        "(let ((x 0)) (prog1 x (setq x 99)) )",
    );
    assert_ok_eq("0", &o, &n);

    // prog1 with no body forms
    let (o, n) = eval_oracle_and_neovm("(prog1 42)");
    assert_ok_eq("42", &o, &n);
}

#[test]
fn oracle_prop_prog2_basics() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_prog2_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (o, n) = eval_oracle_and_neovm("(prog2 10 20 30)");
    assert_ok_eq("20", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(prog2 'a 'b)");
    assert_ok_eq("b", &o, &n);

    // side effects in first form
    let (o, n) = eval_oracle_and_neovm(
        "(let ((x 0)) (list (prog2 (setq x 1) (+ x 10) (setq x 99)) x))",
    );
    assert_ok_eq("(11 99)", &o, &n);
}
