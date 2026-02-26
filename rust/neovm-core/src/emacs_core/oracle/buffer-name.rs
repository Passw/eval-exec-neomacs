//! Oracle parity tests for `buffer-name`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_buffer_name_basics() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_buffer_name_basics: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(stringp (buffer-name))");
    assert_ok_eq("t", &oracle, &neovm);
}

#[test]
fn oracle_prop_buffer_name_error_cases() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_buffer_name_error_cases: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (arity_oracle, arity_neovm) = eval_oracle_and_neovm("(buffer-name nil nil)");
    assert_err_kind(&arity_oracle, &arity_neovm, "wrong-number-of-arguments");

    let (type_oracle, type_neovm) = eval_oracle_and_neovm("(buffer-name 1)");
    assert_err_kind(&type_oracle, &type_neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_buffer_name_after_set_buffer(
        suffix in proptest::string::string_regex(r"[a-z0-9-]{1,10}").expect("regex should compile"),
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let name = format!("*neovm-oracle-buffer-name-{}*", suffix);
        let form = format!(
            "(let ((b (get-buffer-create {:?}))) (set-buffer b) (buffer-name))",
            name
        );
        let expected = format!("{:?}", name);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }
}
