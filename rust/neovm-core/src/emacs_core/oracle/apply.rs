//! Oracle parity tests for `apply`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_apply_basics() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_apply_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle_sum, neovm_sum) = eval_oracle_and_neovm("(apply '+ '(1 2 3 4))");
    assert_ok_eq("10", &oracle_sum, &neovm_sum);

    let (oracle_list, neovm_list) = eval_oracle_and_neovm("(apply 'list 1 2 '(3 4))");
    assert_ok_eq("(1 2 3 4)", &oracle_list, &neovm_list);

    let (oracle_vec, neovm_vec) = eval_oracle_and_neovm("(apply 'vector 1 '(2 3))");
    assert_ok_eq("[1 2 3]", &oracle_vec, &neovm_vec);
}

#[test]
fn oracle_prop_apply_wrong_type_error_for_last_arg() {
    if !oracle_prop_enabled() {
        tracing::info!(
            "skipping oracle_prop_apply_wrong_type_error_for_last_arg: set NEOVM_ENABLE_ORACLE_PROPTEST=1"
        );
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(apply '+ 1 2)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_apply_splices_last_list_argument(
        a in -10_000i64..10_000i64,
        b in -10_000i64..10_000i64,
        c in -10_000i64..10_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(apply 'list {} (list {} {}))", a, b, c);
        let expected = format!("({} {} {})", a, b, c);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
