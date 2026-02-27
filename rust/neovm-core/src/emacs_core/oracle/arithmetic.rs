//! Oracle parity tests for arithmetic primitives.

use proptest::prelude::*;
use proptest::test_runner::TestCaseError;

use super::common::{
    assert_err_kind, oracle_prop_enabled, run_neovm_eval, run_oracle_eval, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_plus_1_3() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_plus_1_3: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let mut runner = proptest::test_runner::TestRunner::new(proptest::test_runner::Config {
        cases: 1,
        ..proptest::test_runner::Config::default()
    });
    let strategy = Just("(+ 1 3)".to_string());

    runner
        .run(&strategy, |form| {
            let oracle = run_oracle_eval(&form).map_err(TestCaseError::fail)?;
            let neovm = run_neovm_eval(&form).map_err(TestCaseError::fail)?;
            prop_assert_eq!(oracle.as_str(), "OK 4");
            prop_assert_eq!(neovm.as_str(), "OK 4");
            prop_assert_eq!(neovm, oracle);
            Ok(())
        })
        .expect("property parity for (+ 1 3) should pass");
}

#[test]
fn oracle_prop_plus_error_message() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_plus_error_message: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = r#"(+ 1 "x")"#;
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_divide_by_zero_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_divide_by_zero_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = "(/ 1 0)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "arith-error");
}

#[test]
fn oracle_prop_add1_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_add1_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = r#"(1+ "x")"#;
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_percent_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_percent_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(% 1.5 2)";
    let oracle = run_oracle_eval(form).expect("oracle eval should run");
    let neovm = run_neovm_eval(form).expect("neovm eval should run");

    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_plus_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(+ {} {})", a, b);
        let expected = format!("OK {}", a + b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_plus_operator_mixed_int_float(
        a in -100_000i64..100_000i64,
        b in -100_000.0f64..100_000.0f64,
        int_first in any::<bool>(),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = if int_first {
            format!("(+ {} {})", a, b)
        } else {
            format!("(+ {} {})", b, a)
        };

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert!(oracle.starts_with("OK "), "oracle should succeed: {}", oracle);
        prop_assert!(neovm.starts_with("OK "), "neovm should succeed: {}", neovm);
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_mul_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(* {} {})", a, b);
        let expected = format!("OK {}", a * b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_div_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }
        prop_assume!(b != 0);

        let form = format!("(/ {} {})", a, b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert!(oracle.starts_with("OK "), "oracle should succeed: {}", oracle);
        prop_assert!(neovm.starts_with("OK "), "neovm should succeed: {}", neovm);
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_add1_operator(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(1+ {})", a);
        let expected = format!("OK {}", a + 1);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_sub1_operator(
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(1- {})", a);
        let expected = format!("OK {}", a - 1);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_percent_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }
        prop_assume!(b != 0);

        let form = format!("(% {} {})", a, b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert!(oracle.starts_with("OK "), "oracle should succeed: {}", oracle);
        prop_assert!(neovm.starts_with("OK "), "neovm should succeed: {}", neovm);
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_mod_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }
        prop_assume!(b != 0);

        let form = format!("(mod {} {})", a, b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert!(oracle.starts_with("OK "), "oracle should succeed: {}", oracle);
        prop_assert!(neovm.starts_with("OK "), "neovm should succeed: {}", neovm);
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_mod_float_operator(
        a in -100_000.0f64..100_000.0f64,
        b in -100_000.0f64..100_000.0f64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }
        prop_assume!(b.abs() > 1e-9);

        let form = format!("(mod {} {})", a, b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert!(oracle.starts_with("OK "), "oracle should succeed: {}", oracle);
        prop_assert!(neovm.starts_with("OK "), "neovm should succeed: {}", neovm);
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }

    #[test]
    fn oracle_prop_minus_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(- {} {})", a, b);
        let expected = format!("OK {}", a - b);

        let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
        let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

        prop_assert_eq!(oracle.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), expected.as_str());
        prop_assert_eq!(neovm.as_str(), oracle.as_str());
    }
}
