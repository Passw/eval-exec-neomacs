//! Oracle parity tests for vector primitives.

use proptest::prelude::*;

use super::common::{
    assert_err_kind, assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled, ORACLE_PROP_CASES,
};

#[test]
fn oracle_prop_aref_wrong_index_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_aref_wrong_index_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm(r#"(aref [1 2] "x")"#);
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

#[test]
fn oracle_prop_aref_out_of_range_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_aref_out_of_range_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(aref [1 2] 10)");
    assert_err_kind(&oracle, &neovm, "args-out-of-range");
}

#[test]
fn oracle_prop_aset_wrong_type_error() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_aset_wrong_type_error: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(aset 1 0 2)");
    assert_err_kind(&oracle, &neovm, "wrong-type-argument");
}

proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(ORACLE_PROP_CASES))]

    #[test]
    fn oracle_prop_vector_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(vector {} {} {})", a, b, c);
        let expected = format!("[{} {} {}]", a, b, c);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_aref_operator(
        idx in 0usize..5usize,
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
        d in -100_000i64..100_000i64,
        e in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let values = [a, b, c, d, e];
        let form = format!("(aref [{} {} {} {} {}] {})", a, b, c, d, e, idx);
        let expected = values[idx].to_string();
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_aset_operator(
        idx in 0usize..5usize,
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
        d in -100_000i64..100_000i64,
        e in -100_000i64..100_000i64,
        x in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let mut values = [a, b, c, d, e];
        values[idx] = x;
        let form = format!(
            "(let ((v [{} {} {} {} {}])) (aset v {} {}) v)",
            a, b, c, d, e, idx, x
        );
        let expected = format!(
            "[{} {} {} {} {}]",
            values[0], values[1], values[2], values[3], values[4]
        );
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq(expected.as_str(), &oracle, &neovm);
    }

    #[test]
    fn oracle_prop_length_vector_operator(
        a in -100_000i64..100_000i64,
        b in -100_000i64..100_000i64,
        c in -100_000i64..100_000i64,
        d in -100_000i64..100_000i64,
    ) {
        if !oracle_prop_enabled() {
            return Ok(());
        }

        let form = format!("(length [{} {} {} {}])", a, b, c, d);
        let (oracle, neovm) = eval_oracle_and_neovm(&form);
        assert_ok_eq("4", &oracle, &neovm);
    }
}
