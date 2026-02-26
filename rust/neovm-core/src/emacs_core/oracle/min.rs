//! Oracle parity tests for `min`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_min_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_min_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_int, neovm_int) = eval_oracle_and_neovm("(min 1 9 -3)");
    assert_ok_eq("-3", &oracle_int, &neovm_int);

    let (oracle_mixed, neovm_mixed) = eval_oracle_and_neovm("(min 1 2.5)");
    assert_ok_eq("1", &oracle_mixed, &neovm_mixed);
}

#[test]
fn oracle_prop_min_error_cases() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_min_error_cases: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (arity_oracle, arity_neovm) = eval_oracle_and_neovm("(min)");
    assert_err_kind(&arity_oracle, &arity_neovm, "wrong-number-of-arguments");

    let (type_oracle, type_neovm) = eval_oracle_and_neovm(r#"(min 1 "x")"#);
    assert_err_kind(&type_oracle, &type_neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_min_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(min {} {})", a, b);
        let expected = format!("OK {}", std::cmp::min(a, b));
        let (oracle, neovm) = eval_oracle_and_neovm(&form);

        prop_assert_eq!(oracle.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), expected.as_str());
        prop_assert_eq!(neovm, oracle);
    }
}
