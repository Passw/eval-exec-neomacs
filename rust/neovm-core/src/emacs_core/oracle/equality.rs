//! Oracle parity tests for equality primitives.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, oracle_prop_enabled, run_neovm_eval, run_oracle_eval, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_eq_symbol_identity() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_eq_symbol_identity: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = "(eq 'x 'x)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_eq!(oracle.as_str(), "OK t");
    assert_eq!(neovm.as_str(), "OK t");
    assert_eq!(neovm, oracle);
}

#[test]
fn oracle_prop_eq_symbol_distinct() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_eq_symbol_distinct: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = "(eq 'x 'y)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_eq!(oracle.as_str(), "OK nil");
    assert_eq!(neovm.as_str(), "OK nil");
    assert_eq!(neovm, oracle);
}

#[test]
fn oracle_prop_eq_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_eq_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = "(eq 1)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_eq_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(eq {} {})", a, b);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_eql_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(eql {} {})", a, b);
        let expected = if a == b { "OK t" } else { "OK nil" };
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), expected);
        prop_assert_eq!(neovm.as_str(), expected);
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_equal_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(equal (list {} {}) (list {} {}))", a, b, a, b);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_equal_operator_distinct_lists(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }
        prop_assume!(a != c || b != c);

        let form = format!("(equal (list {} {}) (list {} {}))", a, b, c, c);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }
}
