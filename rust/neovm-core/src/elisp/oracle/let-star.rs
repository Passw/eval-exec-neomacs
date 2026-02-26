//! Oracle parity tests for `let*`.

use proptest::prelude::*;

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_let_star_sequential_binding() {
    if !oracle_prop_enabled() {
        eprintln!(
            "skipping oracle_prop_let_star_sequential_binding: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let* ((x 1) (y (+ x 2))) y)");
    assert_ok_eq("3", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_let_star_depends_on_prior_binding(
        a in -50_000i64..50_000i64,
        b in -50_000i64..50_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(let* ((x {}) (y (+ x {}))) y)", a, b);
        let expected = (a + b).to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
