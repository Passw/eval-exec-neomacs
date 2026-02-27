//! Oracle parity tests for predicate primitives.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, oracle_prop_enabled, run_neovm_eval, run_oracle_eval, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_numberp_wrong_arity_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_numberp_wrong_arity_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = "(numberp)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

#[test]
fn oracle_prop_null_fixed_cases() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_null_fixed_cases: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    for (form, expected) in [("(null nil)", "OK t"), ("(null 1)", "OK nil"), ("(listp nil)", "OK t")] {
        let oracle = run_oracle_eval(form).expect("oracle eval should run");
        let neovm = run_neovm_eval(form).expect("neovm eval should run");
        assert_eq!(oracle.as_str(), expected);
        assert_eq!(neovm.as_str(), expected);
        assert_eq!(neovm, oracle);
    }
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_numberp_int(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(numberp {})", a);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_numberp_float(
        a in -100_000.0f64..100_000.0f64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(numberp {})", a);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_integerp_int(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(integerp {})", a);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_integerp_float(
        a in -100_000.0f64..100_000.0f64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(integerp {})", a);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK nil");
        prop_assert_eq!(neovm.as_str(), "OK nil");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_floatp_int(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(floatp {})", a);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK nil");
        prop_assert_eq!(neovm.as_str(), "OK nil");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_floatp_float(
        a in -100_000.0f64..100_000.0f64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(floatp {})", a);
        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), "OK t");
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_consp_atom_listp(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let checks = [
            (format!("(consp (cons {} {}))", a, b), "OK t"),
            (format!("(listp (cons {} {}))", a, b), "OK t"),
            (format!("(atom (cons {} {}))", a, b), "OK nil"),
            (format!("(atom {})", a), "OK t"),
        ];

        for (form, expected) in &checks {
            let oracle = run_oracle_eval(form).expect("oracle eval should succeed");
            let neovm = run_neovm_eval(form).expect("neovm eval should succeed");
            prop_assert_eq!(oracle.as_str(), *expected);
            prop_assert_eq!(neovm.as_str(), *expected);
            prop_assert_eq!(neovm.as_str(), oracle.as_str());
        }
    }
}
