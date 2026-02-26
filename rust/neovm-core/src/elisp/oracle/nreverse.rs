//! Oracle parity tests for `nreverse`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_nreverse_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_nreverse_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_list, neovm_list) = eval_oracle_and_neovm("(nreverse '(1 2 3))");
    assert_ok_eq("(3 2 1)", &oracle_list, &neovm_list);

    let (oracle_nil, neovm_nil) = eval_oracle_and_neovm("(nreverse nil)");
    assert_ok_eq("nil", &oracle_nil, &neovm_nil);
}

#[test]
fn oracle_prop_nreverse_wrong_type_error() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_nreverse_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(nreverse 1)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_nreverse_three_element_list(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(nreverse (list {} {} {}))", a, b, c);
        let expected = format!("({} {} {})", c, b, a);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
