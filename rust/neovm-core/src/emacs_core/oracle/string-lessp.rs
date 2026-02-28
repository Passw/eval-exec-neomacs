//! Oracle parity tests for `string-lessp`.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, assert_oracle_parity, eval_oracle_and_neovm, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_string_lessp_wrong_arity_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-lessp "a")"#);
    assert_err_kind(&oracle, &neovm, "wrong-number-of-arguments");
}

#[test]
fn oracle_prop_string_lessp_wrong_type_error() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(string-lessp "a" 1)"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_string_lessp_alias_smoke() {
    crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!();

    assert_oracle_parity(r#"(string< "abc" "abd")"#);
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_string_lessp_operator(
        a in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
        b in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
    ) {
        crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let expected = if a < b { "t" } else { "nil" };
        let form = format!("(string-lessp {:?} {:?})", a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected, &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_string_lessp_alias_operator(
        a in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
        b in proptest::string::string_regex(r"[A-Za-z0-9 _-]{0,24}").expect("regex should compile"),
    ) {
        crate::emacs_core::oracle::common::return_if_neovm_enable_oracle_proptest_not_set!(Ok(()));

        let expected = if a < b { "t" } else { "nil" };
        let form = format!("(string< {:?} {:?})", a, b);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected, &oracle, &neovm);
    }
}
