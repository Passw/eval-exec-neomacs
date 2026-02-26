//! Oracle parity tests for `when`.

use proptest::prelude::*;

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_when_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_when_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_t, neovm_t) = eval_oracle_and_neovm("(when t 7)");
    assert_ok_eq("7", &oracle_t, &neovm_t);

    let (oracle_nil, neovm_nil) = eval_oracle_and_neovm("(when nil 7)");
    assert_ok_eq("nil", &oracle_nil, &neovm_nil);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_when_branching(
        cond in any::<bool>(),
        a in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let cond_form = if cond { "t" } else { "nil" };
        let form = format!("(when {} {})", cond_form, a);
        let expected = if cond { a.to_string() } else { "nil".to_string() };
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
