//! Oracle parity tests for `put`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_put_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_put_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_return, neovm_return) = eval_oracle_and_neovm("(let ((s 'oracle-prop-put)) (put s 'k 99))");
    assert_ok_eq("99", &oracle_return, &neovm_return);

    let (oracle_get, neovm_get) =
        eval_oracle_and_neovm("(let ((s 'oracle-prop-put)) (put s 'k 99) (get s 'k))");
    assert_ok_eq("99", &oracle_get, &neovm_get);
}

#[test]
fn oracle_prop_put_wrong_type_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_put_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(put 1 'k 2)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_put_returns_value(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(let ((s 'oracle-prop-put-rand)) (put s 'k {}))", a);
        let expected = a.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
