//! Oracle parity tests for character-literal parsing (`?x`, `?\M-x`, etc.).

use super::common::{assert_ok_eq, eval_oracle_and_neovm, oracle_prop_enabled};

#[test]
fn oracle_prop_char_literal_modifier_bits() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_char_literal_modifier_bits: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let form = r#"(list ?\M-a ?\C-a ?\M-\C-a ?\S-a)"#;
    let (oracle, neovm) = eval_oracle_and_neovm(form);
    assert_ok_eq("(134217825 1 134217729 33554529)", &oracle, &neovm);
}

#[test]
fn oracle_prop_char_literal_unicode_codepoints() {
    if !oracle_prop_enabled() {
        tracing::info!("skipping oracle_prop_char_literal_unicode_codepoints: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let (oracle, neovm) = eval_oracle_and_neovm("(list ?😀 ?𐌀)");
    assert_ok_eq("(128512 66304)", &oracle, &neovm);
}
