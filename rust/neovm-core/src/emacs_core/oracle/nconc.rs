//! Oracle parity tests for `nconc`.

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled};

#[test]
fn oracle_prop_nconc_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_nconc_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (o, n) = eval_oracle_and_neovm("(nconc '(1 2) '(3 4))");
    assert_ok_eq("(1 2 3 4)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nconc '(a b) '(c) '(d e f))");
    assert_ok_eq("(a b c d e f)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nconc nil '(5 6))");
    assert_ok_eq("(5 6)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nconc '(7 8) nil)");
    assert_ok_eq("(7 8)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nconc nil)");
    assert_ok_eq("nil", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nconc '(99))");
    assert_ok_eq("(99)", &o, &n);

    let (o, n) = eval_oracle_and_neovm("(nconc nil nil nil '(1))");
    assert_ok_eq("(1)", &o, &n);
}
