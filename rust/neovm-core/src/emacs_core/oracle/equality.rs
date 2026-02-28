//! Oracle parity tests for equality primitives.

use super::common::return_if_neovm_enable_oracle_proptest_not_set;

use proptest::prelude::*;

use super::common::{assert_err_kind, run_neovm_eval, run_oracle_eval, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_eq_symbol_identity() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(eq 'x 'x)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_eq!(oracle.as_str(), "OK t");
    assert_eq!(neovm.as_str(), "OK t");
    assert_eq!(neovm, oracle);
}

#[test]
fn oracle_prop_eq_symbol_distinct() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(eq 'x 'y)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_eq!(oracle.as_str(), "OK nil");
    assert_eq!(neovm.as_str(), "OK nil");
    assert_eq!(neovm, oracle);
}

#[test]
fn oracle_prop_eq_wrong_arity_error() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(eq 1)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

#[test]
fn oracle_prop_eq_float_corner_cases() {
    return_if_neovm_enable_oracle_proptest_not_set!();

    let form = "(list (eq 1.0 1.0) (let ((x 1.0)) (eq x x)) (eq 0.0 -0.0) (eql 0.0 -0.0))";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_eq!(oracle.as_str(), "OK (nil t nil nil)");
    assert_eq!(neovm.as_str(), "OK (nil t nil nil)");
    assert_eq!(neovm, oracle);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_eq_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

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
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

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
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

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
        return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));
        prop_assume!(a != c || b != c);

        let form = format!("(equal (list {} {}) (list {} {}))", a, b, c, c);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }
}
