//! Oracle parity tests for `get`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_get_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_get_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_set, neovm_set) =
        eval_oracle_and_neovm("(let ((s 'oracle-prop-get)) (put s 'k 12) (get s 'k))");
    assert_ok_eq("12", &oracle_set, &neovm_set);

    let (oracle_missing, neovm_missing) =
        eval_oracle_and_neovm("(let ((s 'oracle-prop-get-missing)) (get s 'k))");
    assert_ok_eq("nil", &oracle_missing, &neovm_missing);
}

#[test]
fn oracle_prop_get_wrong_type_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_get_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(get 1 'k)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_get_latest_value(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!(
            "(let ((s 'oracle-prop-get-rand)) (put s 'k {}) (put s 'k {}) (get s 'k))",
            a, b
        );
        let expected = b.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
