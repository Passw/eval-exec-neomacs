//! Oracle-backed property tests for evaluator parity.
//!
//! These tests are intentionally opt-in because they require GNU Emacs
//! available on PATH (or via `NEOVM_FORCE_ORACLE_PATH`).

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use proptest::test_runner::TestCaseError;

    use crate::elisp::oracle::{oracle_prop_enabled, run_neovm_eval, run_oracle_eval};

    const ORACLE_PROP_CASES: u32 = 10;

    #[test]
    fn oracle_prop_plus_1_3() {
        if !oracle_prop_enabled() {
            eprintln!("skipping oracle_prop_plus_1_3: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
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
            eprintln!("skipping oracle_prop_plus_error_message: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
            return;
        }

        let form = r#"(+ 1 "x")"#;
        let oracle = run_oracle_eval(form).expect("oracle eval should run");
        let neovm = run_neovm_eval(form).expect("neovm eval should run");

        assert!(oracle.starts_with("ERR "), "oracle should return an error: {oracle}");
        assert!(neovm.starts_with("ERR "), "neovm should return an error: {neovm}");
        let oracle_payload = oracle
            .strip_prefix("ERR ")
            .expect("oracle payload should have ERR prefix")
            .trim();
        let neovm_payload = neovm
            .strip_prefix("ERR ")
            .expect("neovm payload should have ERR prefix")
            .trim();

        assert!(!oracle_payload.is_empty(), "oracle error should include a message");
        assert!(!neovm_payload.is_empty(), "neovm error should include a message");
        assert!(
            oracle_payload.contains("wrong-type-argument"),
            "oracle error kind should be present: {oracle_payload}"
        );
        assert!(
            neovm_payload.contains("wrong-type-argument"),
            "neovm error kind should be present: {neovm_payload}"
        );
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
}
