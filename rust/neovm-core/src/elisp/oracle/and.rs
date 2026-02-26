//! Oracle parity tests for `and`.

use proptest::prelude::*;

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_and_short_circuit() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_and_short_circuit: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(let ((x 0)) (and nil (setq x 1)) x)");
    assert_ok_eq("0", &oracle, &neovm);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_and_returns_last_truthy(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(and {} {})", a, b);
        let expected = b.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
