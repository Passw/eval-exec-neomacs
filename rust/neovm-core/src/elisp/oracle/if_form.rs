//! Oracle parity tests for `if`.

use proptest::prelude::*;

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES};

#[test]
fn oracle_prop_if_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_if_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_t, neovm_t) = eval_oracle_and_neovm("(if t 1 2)");
    assert_ok_eq("1", &oracle_t, &neovm_t);

    let (oracle_nil, neovm_nil) = eval_oracle_and_neovm("(if nil 1 2)");
    assert_ok_eq("2", &oracle_nil, &neovm_nil);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_if_branching(
        cond in any::<bool>(),
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let cond_form = if cond { "t" } else { "nil" };
        let form = format!("(if {} {} {})", cond_form, a, b);
        let expected = if cond { a } else { b }.to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
