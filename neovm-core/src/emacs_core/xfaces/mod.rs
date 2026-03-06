//! Bootstrap-facing subset of GNU Emacs's `xfaces.c`.
//!
//! Face-related builtins are still mostly implemented in `face.rs` and
//! `font.rs`, but GNU startup also relies on a small set of C-level
//! variables from `xfaces.c` being bound before Lisp runs. Keep those
//! defaults here so Rust startup matches the same ownership boundary.

use crate::emacs_core::symbol::Obarray;
use crate::emacs_core::value::{HashTableTest, Value};

/// Register bootstrap variables owned by the face subsystem.
pub fn register_bootstrap_vars(obarray: &mut Obarray) {
    obarray.set_symbol_value("face-filters-always-match", Value::Nil);
    obarray.set_symbol_value(
        "face--new-frame-defaults",
        Value::hash_table(HashTableTest::Eq),
    );
    obarray.set_symbol_value("face-default-stipple", Value::string("gray3"));
    obarray.set_symbol_value("tty-defined-color-alist", Value::Nil);
    obarray.set_symbol_value("scalable-fonts-allowed", Value::Nil);
    obarray.set_symbol_value("face-ignored-fonts", Value::Nil);
    obarray.set_symbol_value("face-remapping-alist", Value::Nil);
    obarray.set_symbol_value("face-font-rescale-alist", Value::Nil);
    obarray.set_symbol_value("face-near-same-color-threshold", Value::Int(30_000));
    obarray.set_symbol_value("face-font-lax-matched-attributes", Value::True);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::emacs_core::value::with_heap;

    #[test]
    fn register_bootstrap_vars_matches_gnu_defaults() {
        let mut obarray = Obarray::new();
        register_bootstrap_vars(&mut obarray);

        assert_eq!(
            obarray.symbol_value("face-default-stipple").copied(),
            Some(Value::string("gray3"))
        );
        assert_eq!(
            obarray
                .symbol_value("face-near-same-color-threshold")
                .copied(),
            Some(Value::Int(30_000))
        );
        assert_eq!(
            obarray
                .symbol_value("face-font-lax-matched-attributes")
                .copied(),
            Some(Value::True)
        );

        let table = obarray
            .symbol_value("face--new-frame-defaults")
            .copied()
            .expect("face--new-frame-defaults");
        let Value::HashTable(id) = table else {
            panic!("face--new-frame-defaults must be a hash table");
        };
        let test = with_heap(|heap| heap.get_hash_table(id).test.clone());
        assert_eq!(test, HashTableTest::Eq);
    }
}
