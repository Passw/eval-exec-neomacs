//! Oracle parity tests for `unwind-protect`.

use proptest::prelude::*;

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_unwind_protect_runs_cleanup_on_success() {
    if !oracle_prop_enabled() {
        eprintln!(
            "skipping oracle_prop_unwind_protect_runs_cleanup_on_success: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 0)) (unwind-protect 1 (setq x 2)) x)");
    assert_ok_eq("2", &oracle, &neovm);
}

#[test]
fn oracle_prop_unwind_protect_runs_cleanup_on_error() {
    if !oracle_prop_enabled() {
        eprintln!(
            "skipping oracle_prop_unwind_protect_runs_cleanup_on_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let form = "(let ((x 0)) (condition-case nil (unwind-protect (/ 1 0) (setq x 7)) (error x)))";
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("7", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_unwind_protect_returns_protected_value(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(unwind-protect {} {})", a, b);
        let expected = a.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
