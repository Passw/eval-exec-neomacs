//! Oracle parity tests for `unless`.

use proptest::prelude::*;

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_unless_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_unless_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_t, neovm_t) = eval_oracle_and_neovm("(unless t 7)");
    assert_ok_eq("nil", &oracle_t, &neovm_t);

    let (oracle_nil, neovm_nil) = eval_oracle_and_neovm("(unless nil 7)");
    assert_ok_eq("7", &oracle_nil, &neovm_nil);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_unless_branching(
        cond in any::<bool>(),
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let cond_form = if cond { "t" } else { "nil" };
        let form = format!("(unless {} {})", cond_form, a);
        let expected = if cond { "nil".to_string() } else { a.to_string() };
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
