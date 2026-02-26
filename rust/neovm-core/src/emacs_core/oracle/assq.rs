//! Oracle parity tests for `assq`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_assq_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_assq_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_found, neovm_found) = eval_oracle_and_neovm("(assq 'b '((a . 1) (b . 2)))");
    assert_ok_eq("(b . 2)", &oracle_found, &neovm_found);

    let (oracle_missing, neovm_missing) = eval_oracle_and_neovm("(assq 'z '((a . 1) (b . 2)))");
    assert_ok_eq("nil", &oracle_missing, &neovm_missing);
}

#[test]
fn oracle_prop_assq_wrong_type_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_assq_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(assq 'a 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_assq_symbol_key(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(assq 'k (list (cons 'x {}) (cons 'k {})))", a, b);
        let expected = format!("(k . {})", b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
