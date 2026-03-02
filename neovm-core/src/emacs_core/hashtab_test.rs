use super::*;
use crate::emacs_core::builtins::{
    builtin_gethash, builtin_hash_table_count, builtin_make_hash_table, builtin_puthash,
};

#[test]
fn hash_table_keys_values_basics() {
    let table = Value::hash_table(HashTableTest::Equal);
    if let Value::HashTable(ht) = &table {
        with_heap_mut(|h| {
            let raw = h.get_hash_table_mut(*ht);
            let test = raw.test.clone();
            raw.data
                .insert(Value::symbol("alpha").to_hash_key(&test), Value::Int(1));
            raw.data
                .insert(Value::symbol("beta").to_hash_key(&test), Value::Int(2));
        });
    } else {
        panic!("expected hash table");
    }

    let keys = builtin_hash_table_keys(vec![table]).unwrap();
    let keys = list_to_vec(&keys).expect("proper list");
    assert_eq!(keys.len(), 2);
    assert!(keys.iter().any(|v| v.as_symbol_name() == Some("alpha")));
    assert!(keys.iter().any(|v| v.as_symbol_name() == Some("beta")));

    let values = builtin_hash_table_values(vec![table]).unwrap();
    let values = list_to_vec(&values).expect("proper list");
    assert_eq!(values.len(), 2);
    assert!(values.iter().any(|v| v.as_int() == Some(1)));
    assert!(values.iter().any(|v| v.as_int() == Some(2)));
}

#[test]
fn hash_table_keys_values_errors() {
    assert!(builtin_hash_table_keys(vec![]).is_err());
    assert!(builtin_hash_table_values(vec![]).is_err());
    assert!(builtin_hash_table_keys(vec![Value::Nil]).is_err());
    assert!(builtin_hash_table_values(vec![Value::Nil]).is_err());
}

#[test]
fn hash_table_rehash_defaults() {
    let table = builtin_make_hash_table(vec![]).unwrap();
    let size = builtin_hash_table_rehash_size(vec![table]).unwrap();
    let threshold = builtin_hash_table_rehash_threshold(vec![table]).unwrap();

    assert_eq!(size, Value::Float(1.5, next_float_id()));
    assert_eq!(threshold, Value::Float(0.8125, next_float_id()));
}

#[test]
fn hash_table_rehash_options_are_ignored() {
    let table = builtin_make_hash_table(vec![
        Value::keyword(":rehash-size"),
        Value::Float(2.0, next_float_id()),
        Value::keyword(":rehash-threshold"),
        Value::Float(0.9, next_float_id()),
    ])
    .unwrap();

    let size = builtin_hash_table_rehash_size(vec![table]).unwrap();
    let threshold = builtin_hash_table_rehash_threshold(vec![table]).unwrap();

    assert_eq!(size, Value::Float(1.5, next_float_id()));
    assert_eq!(threshold, Value::Float(0.8125, next_float_id()));

    assert!(builtin_make_hash_table(vec![
        Value::keyword(":rehash-size"),
        Value::string("x"),
        Value::keyword(":rehash-threshold"),
        Value::Float(1.5, next_float_id()),
    ])
    .is_ok());
    assert!(builtin_make_hash_table(vec![
        Value::keyword(":rehash-threshold"),
        Value::string("x"),
        Value::keyword(":rehash-size"),
        Value::Float(1.5, next_float_id()),
    ])
    .is_ok());
}

#[test]
fn sxhash_variants_return_fixnums_and_preserve_hash_contracts() {
    assert!(matches!(
        builtin_sxhash_eq(vec![Value::symbol("foo")]),
        Ok(Value::Int(_))
    ));
    assert!(matches!(
        builtin_sxhash_eql(vec![Value::symbol("foo")]),
        Ok(Value::Int(_))
    ));
    assert!(matches!(
        builtin_sxhash_equal(vec![Value::symbol("foo")]),
        Ok(Value::Int(_))
    ));
    assert!(matches!(
        builtin_sxhash_equal_including_properties(vec![Value::symbol("foo")]),
        Ok(Value::Int(_))
    ));

    let left = Value::string("x");
    let right = Value::string("x");
    assert_eq!(
        builtin_sxhash_equal(vec![left]).unwrap(),
        builtin_sxhash_equal(vec![right]).unwrap()
    );
    assert_eq!(
        builtin_sxhash_equal_including_properties(vec![left]).unwrap(),
        builtin_sxhash_equal_including_properties(vec![right]).unwrap()
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::list(vec![Value::Int(1), Value::Int(2)])]).unwrap(),
        builtin_sxhash_equal(vec![Value::list(vec![Value::Int(1), Value::Int(2)])]).unwrap()
    );
}

#[test]
fn sxhash_equal_matches_oracle_for_small_int_and_string_values() {
    assert_eq!(
        builtin_sxhash_equal(vec![Value::string("a")]).unwrap(),
        Value::Int(109)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::string("b")]).unwrap(),
        Value::Int(110)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::string("ab")]).unwrap(),
        Value::Int(31265)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Int(1)]).unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Int(2)]).unwrap(),
        Value::Int(2)
    );
}

#[test]
fn sxhash_eq_eql_fixnum_and_char_match_oracle_values() {
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(1)]).unwrap(),
        Value::Int(6)
    );
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(2)]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(3)]).unwrap(),
        Value::Int(4)
    );
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(65)]).unwrap(),
        Value::Int(86)
    );
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(97)]).unwrap(),
        Value::Int(126)
    );
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(-1)]).unwrap(),
        Value::Int(-1_152_921_504_606_846_969)
    );
    assert_eq!(
        builtin_sxhash_eq(vec![Value::Int(-2)]).unwrap(),
        Value::Int(-1_152_921_504_606_846_973)
    );

    assert_eq!(
        builtin_sxhash_eql(vec![Value::Int(65)]).unwrap(),
        Value::Int(86)
    );
    assert_eq!(
        builtin_sxhash_eql(vec![Value::Char('A')]).unwrap(),
        Value::Int(86)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Int(65)]).unwrap(),
        Value::Int(81)
    );
}

#[test]
fn sxhash_float_matches_oracle_fixnum_values() {
    assert_eq!(
        builtin_sxhash_eql(vec![Value::Float(1.0, next_float_id())]).unwrap(),
        Value::Int(-1_149_543_804_886_319_104)
    );
    assert_eq!(
        builtin_sxhash_eql(vec![Value::Float(2.0, next_float_id())]).unwrap(),
        Value::Int(1_152_921_504_606_846_976)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Float(1.0, next_float_id())]).unwrap(),
        Value::Int(-1_149_543_804_886_319_104)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Float(2.0, next_float_id())]).unwrap(),
        Value::Int(1_152_921_504_606_846_976)
    );
}

#[test]
fn sxhash_float_signed_zero_and_nan_semantics_match_oracle() {
    assert_eq!(
        builtin_sxhash_eql(vec![Value::Float(0.0, next_float_id())]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(
        builtin_sxhash_eql(vec![Value::Float(-0.0, next_float_id())]).unwrap(),
        Value::Int(-2_305_843_009_213_693_952)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Float(0.0, next_float_id())]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(
        builtin_sxhash_equal(vec![Value::Float(-0.0, next_float_id())]).unwrap(),
        Value::Int(-2_305_843_009_213_693_952)
    );

    let nan = Value::Float(0.0_f64 / 0.0_f64, next_float_id());
    let nan_eql = builtin_sxhash_eql(vec![nan]).unwrap();
    let nan_equal = builtin_sxhash_equal(vec![nan]).unwrap();
    assert_eq!(nan_eql, nan_equal);

    for test_name in ["eql", "equal"] {
        let table =
            builtin_make_hash_table(vec![Value::keyword(":test"), Value::symbol(test_name)])
                .expect("hash table");
        let _ = builtin_puthash(vec![
            Value::Float(0.0, next_float_id()),
            Value::symbol("zero"),
            table,
        ])
        .expect("puthash zero");
        assert_eq!(
            builtin_gethash(vec![
                Value::Float(-0.0, next_float_id()),
                table,
                Value::symbol("miss")
            ])
            .expect("gethash -0.0"),
            Value::symbol("miss")
        );

        let _ = builtin_puthash(vec![nan, Value::symbol("nan"), table])
            .expect("puthash nan");
        assert_eq!(
            builtin_gethash(vec![nan, table, Value::symbol("miss")])
                .expect("gethash nan"),
            Value::symbol("nan")
        );
    }
}

#[test]
fn hash_table_nan_payloads_remain_distinct_for_eql_and_equal() {
    let nan_a = Value::Float(f64::from_bits(0x7ff8_0000_0000_0000), next_float_id());
    let nan_b = Value::Float(f64::from_bits(0x7ff8_0000_0000_0001), next_float_id());
    assert_eq!(
        builtin_sxhash_eql(vec![nan_a]).unwrap(),
        builtin_sxhash_equal(vec![nan_a]).unwrap()
    );
    assert_eq!(
        builtin_sxhash_eql(vec![nan_b]).unwrap(),
        builtin_sxhash_equal(vec![nan_b]).unwrap()
    );
    assert_ne!(
        builtin_sxhash_eql(vec![nan_a]).unwrap(),
        builtin_sxhash_eql(vec![nan_b]).unwrap()
    );

    for test_name in ["eql", "equal"] {
        let table = builtin_make_hash_table(vec![
            Value::keyword(":test"),
            Value::symbol(test_name),
            Value::keyword(":size"),
            Value::Int(5),
        ])
        .expect("hash table");
        let _ = builtin_puthash(vec![nan_a, Value::symbol("a"), table])
            .expect("puthash nan-a");
        let _ = builtin_puthash(vec![nan_b, Value::symbol("b"), table])
            .expect("puthash nan-b");
        assert_eq!(
            builtin_hash_table_count(vec![table]).expect("hash-table-count"),
            Value::Int(2)
        );
        assert_eq!(
            builtin_gethash(vec![nan_a, table, Value::symbol("miss")])
                .expect("gethash nan-a"),
            Value::symbol("a")
        );
        assert_eq!(
            builtin_gethash(vec![nan_b, table, Value::symbol("miss")])
                .expect("gethash nan-b"),
            Value::symbol("b")
        );

        let buckets =
            builtin_internal_hash_table_buckets(vec![table]).expect("bucket diagnostics");
        let outer = list_to_vec(&buckets).expect("outer list");
        let mut hashes = Vec::new();
        for bucket in outer {
            let entries = list_to_vec(&bucket).expect("bucket alist");
            for entry in entries {
                let Value::Cons(cell) = entry else {
                    panic!("expected alist cons entry");
                };
                let pair = read_cons(cell);
                hashes.push(pair.cdr.as_int().expect("diagnostic hash integer"));
            }
        }
        hashes.sort_unstable();
        assert_eq!(hashes.len(), 2);
        assert_ne!(hashes[0], hashes[1]);
    }
}

#[test]
fn internal_hash_table_introspection_empty_defaults() {
    let table = builtin_make_hash_table(vec![]).unwrap();
    assert_eq!(
        builtin_internal_hash_table_buckets(vec![table]).unwrap(),
        Value::Nil
    );
    assert_eq!(
        builtin_internal_hash_table_histogram(vec![table]).unwrap(),
        Value::Nil
    );
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![table]).unwrap(),
        Value::Int(1)
    );
}

#[test]
fn internal_hash_table_index_size_uses_declared_size() {
    let table_one = builtin_make_hash_table(vec![Value::keyword(":size"), Value::Int(1)])
        .expect("size 1 table");
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![table_one]).unwrap(),
        Value::Int(2)
    );

    let table_mid = builtin_make_hash_table(vec![Value::keyword(":size"), Value::Int(37)])
        .expect("size 37 table");
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![table_mid]).unwrap(),
        Value::Int(64)
    );
}

#[test]
fn internal_hash_table_index_size_tracks_growth_boundaries() {
    let tiny = builtin_make_hash_table(vec![Value::keyword(":size"), Value::Int(1)])
        .expect("size 1 table");
    let _ = builtin_puthash(vec![Value::Int(1), Value::symbol("x"), tiny])
        .expect("puthash for first tiny entry");
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![tiny]).unwrap(),
        Value::Int(2)
    );
    let _ = builtin_puthash(vec![Value::Int(2), Value::symbol("y"), tiny])
        .expect("puthash for second tiny entry");
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![tiny]).unwrap(),
        Value::Int(32)
    );

    let default_table = builtin_make_hash_table(vec![]).expect("default table");
    let _ = builtin_puthash(vec![
        Value::Int(1),
        Value::symbol("x"),
        default_table,
    ])
    .expect("puthash for default table");
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![default_table]).unwrap(),
        Value::Int(8)
    );

    let mid = builtin_make_hash_table(vec![Value::keyword(":size"), Value::Int(10)])
        .expect("size 10 table");
    for i in 0..10 {
        let i = i as i64;
        let _ = builtin_puthash(vec![Value::Int(i), Value::Int(i), mid])
            .expect("puthash while filling size 10 table");
    }
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![mid]).unwrap(),
        Value::Int(16)
    );
    let _ = builtin_puthash(vec![Value::Int(10), Value::Int(10), mid])
        .expect("puthash crossing size 10 threshold");
    assert_eq!(
        builtin_internal_hash_table_index_size(vec![mid]).unwrap(),
        Value::Int(64)
    );
}

#[test]
fn hash_table_size_tracks_growth_boundaries() {
    let tiny = builtin_make_hash_table(vec![Value::keyword(":size"), Value::Int(1)])
        .expect("size 1 table");
    let _ = builtin_puthash(vec![Value::Int(1), Value::symbol("x"), tiny])
        .expect("puthash for first tiny entry");
    assert_eq!(
        builtin_hash_table_size(vec![tiny]).unwrap(),
        Value::Int(1)
    );
    let _ = builtin_puthash(vec![Value::Int(2), Value::symbol("y"), tiny])
        .expect("puthash for second tiny entry");
    assert_eq!(builtin_hash_table_size(vec![tiny]).unwrap(), Value::Int(24));

    let default_table = builtin_make_hash_table(vec![]).expect("default table");
    let _ = builtin_puthash(vec![
        Value::Int(1),
        Value::symbol("default-value"),
        default_table,
    ])
    .expect("puthash for default table");
    assert_eq!(
        builtin_hash_table_size(vec![default_table]).unwrap(),
        Value::Int(6)
    );

    let mid = builtin_make_hash_table(vec![Value::keyword(":size"), Value::Int(10)])
        .expect("size 10 table");
    for i in 0..11 {
        let i = i as i64;
        let _ = builtin_puthash(vec![Value::Int(i), Value::Int(i), mid])
            .expect("puthash while filling size 10 table");
    }
    assert_eq!(builtin_hash_table_size(vec![mid]).unwrap(), Value::Int(40));
}

#[test]
fn internal_hash_table_buckets_report_hash_diagnostics() {
    let table = builtin_make_hash_table(vec![
        Value::keyword(":test"),
        Value::symbol("equal"),
        Value::keyword(":size"),
        Value::Int(3),
    ])
    .expect("hash table");
    if let Value::HashTable(ht) = &table {
        with_heap_mut(|h| {
            let raw = h.get_hash_table_mut(*ht);
            let test = raw.test.clone();
            raw.data.insert(
                Value::string("a").to_hash_key(&test),
                Value::symbol("value-a"),
            );
            raw.data.insert(
                Value::string("b").to_hash_key(&test),
                Value::symbol("value-b"),
            );
        });
    } else {
        panic!("expected hash table");
    }

    let buckets = builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists");
    let outer = list_to_vec(&buckets).expect("outer list");
    let mut seen = std::collections::BTreeMap::new();
    for bucket in outer {
        let entries = list_to_vec(&bucket).expect("bucket alist");
        for entry in entries {
            let Value::Cons(cell) = entry else {
                panic!("expected alist cons entry");
            };
            let pair = read_cons(cell);
            let key = pair.car.as_str().expect("string key").to_string();
            let hash = pair.cdr.as_int().expect("diagnostic hash integer");
            seen.insert(key, hash);
        }
    }

    assert_eq!(seen.len(), 2);
    assert!(seen.contains_key("a"));
    assert!(seen.contains_key("b"));
}

#[test]
fn internal_hash_table_buckets_match_oracle_small_string_hashes() {
    let table = builtin_make_hash_table(vec![
        Value::keyword(":test"),
        Value::symbol("equal"),
        Value::keyword(":size"),
        Value::Int(3),
    ])
    .expect("hash table");
    let _ = builtin_puthash(vec![Value::string("a"), Value::Int(1), table])
        .expect("puthash a");
    let _ = builtin_puthash(vec![Value::string("b"), Value::Int(2), table])
        .expect("puthash b");

    assert_eq!(
        builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists"),
        Value::list(vec![
            Value::list(vec![Value::cons(Value::string("b"), Value::Int(114))]),
            Value::list(vec![Value::cons(Value::string("a"), Value::Int(113))]),
        ])
    );
}

#[test]
fn internal_hash_table_buckets_match_oracle_eq_eql_fixnum_hashes() {
    for test_name in ["eq", "eql"] {
        let table = builtin_make_hash_table(vec![
            Value::keyword(":test"),
            Value::symbol(test_name),
            Value::keyword(":size"),
            Value::Int(3),
        ])
        .expect("hash table");
        let _ = builtin_puthash(vec![Value::Char('A'), Value::symbol("char"), table])
            .expect("puthash char");
        assert_eq!(
            builtin_gethash(vec![Value::Int(65), table, Value::symbol("miss")])
                .expect("gethash int"),
            Value::symbol("char")
        );
        assert_eq!(
            builtin_gethash(vec![Value::Char('A'), table, Value::symbol("miss")])
                .expect("gethash char"),
            Value::symbol("char")
        );
        assert_eq!(
            builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists"),
            Value::list(vec![Value::list(vec![Value::cons(
                Value::Int(65),
                Value::Int(71)
            )])])
        );
    }

    let table = builtin_make_hash_table(vec![
        Value::keyword(":test"),
        Value::symbol("equal"),
        Value::keyword(":size"),
        Value::Int(3),
    ])
    .expect("hash table");
    let _ = builtin_puthash(vec![Value::Char('A'), Value::symbol("char"), table])
        .expect("puthash char");
    assert_eq!(
        builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists"),
        Value::list(vec![Value::list(vec![Value::cons(
            Value::Int(65),
            Value::Int(65)
        )])])
    );
}

#[test]
fn internal_hash_table_buckets_eq_pointer_keys_keep_distinct_hashes() {
    let table = builtin_make_hash_table(vec![
        Value::keyword(":test"),
        Value::symbol("eq"),
        Value::keyword(":size"),
        Value::Int(5),
    ])
    .expect("hash table");
    let key_a = Value::string("x");
    let key_b = Value::string("x");
    let _ = builtin_puthash(vec![key_a, Value::symbol("a"), table])
        .expect("puthash key-a");
    let _ = builtin_puthash(vec![key_b, Value::symbol("b"), table])
        .expect("puthash key-b");
    assert_eq!(
        builtin_hash_table_count(vec![table]).expect("hash-table-count"),
        Value::Int(2)
    );
    assert_eq!(
        builtin_gethash(vec![key_a, table, Value::symbol("miss")]).expect("gethash a"),
        Value::symbol("a")
    );
    assert_eq!(
        builtin_gethash(vec![key_b, table, Value::symbol("miss")]).expect("gethash b"),
        Value::symbol("b")
    );

    let buckets = builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists");
    let outer = list_to_vec(&buckets).expect("outer list");
    let mut hashes = Vec::new();
    let mut keys = Vec::new();
    for bucket in outer {
        let entries = list_to_vec(&bucket).expect("bucket alist");
        for entry in entries {
            let Value::Cons(cell) = entry else {
                panic!("expected alist cons entry");
            };
            let pair = read_cons(cell);
            keys.push(pair.car);
            hashes.push(pair.cdr.as_int().expect("diagnostic hash integer"));
        }
    }
    assert_eq!(keys.len(), 2);
    assert!(keys.iter().all(|key| key.as_str().is_some()));
    hashes.sort_unstable();
    assert_eq!(hashes.len(), 2);
    assert_ne!(hashes[0], hashes[1]);
}

#[test]
fn internal_hash_table_buckets_equal_preserve_first_key_identity_on_overwrite() {
    let table = builtin_make_hash_table(vec![
        Value::keyword(":test"),
        Value::symbol("equal"),
        Value::keyword(":size"),
        Value::Int(5),
    ])
    .expect("hash table");
    let key_a = Value::string("x");
    let key_b = Value::string("x");
    let _ = builtin_puthash(vec![key_a, Value::symbol("a"), table])
        .expect("puthash key-a");
    let _ = builtin_puthash(vec![key_b, Value::symbol("b"), table])
        .expect("puthash key-b overwrite");
    assert_eq!(
        builtin_hash_table_count(vec![table]).expect("hash-table-count"),
        Value::Int(1)
    );
    assert_eq!(
        builtin_gethash(vec![
            Value::string("x"),
            table,
            Value::symbol("miss")
        ])
        .expect("gethash x"),
        Value::symbol("b")
    );

    let buckets = builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists");
    let outer = list_to_vec(&buckets).expect("outer list");
    assert_eq!(outer.len(), 1);
    let entries = list_to_vec(&outer[0]).expect("bucket alist");
    assert_eq!(entries.len(), 1);
    let Value::Cons(cell) = &entries[0] else {
        panic!("expected alist cons entry");
    };
    let pair = read_cons(*cell);
    assert_eq!(pair.car.as_str(), Some("x"));
    assert!(eq_value(&pair.car, &key_a));
    assert!(!eq_value(&pair.car, &key_b));
}

#[test]
fn internal_hash_table_buckets_match_oracle_small_float_hashes() {
    fn collect_float_hashes(table: Value) -> std::collections::BTreeMap<u64, i64> {
        let buckets = builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists");
        let outer = list_to_vec(&buckets).expect("outer list");
        let mut seen = std::collections::BTreeMap::new();
        for bucket in outer {
            let entries = list_to_vec(&bucket).expect("bucket alist");
            for entry in entries {
                let Value::Cons(cell) = entry else {
                    panic!("expected alist cons entry");
                };
                let pair = read_cons(cell);
                let key_bits = pair.car.as_float().expect("float key").to_bits();
                let hash = pair.cdr.as_int().expect("diagnostic hash integer");
                seen.insert(key_bits, hash);
            }
        }
        seen
    }

    let expected = std::collections::BTreeMap::from([
        (1.0_f64.to_bits(), 1_072_693_248_i64),
        (2.0_f64.to_bits(), 1_073_741_824_i64),
    ]);

    for test_name in ["eql", "equal"] {
        let table = builtin_make_hash_table(vec![
            Value::keyword(":test"),
            Value::symbol(test_name),
            Value::keyword(":size"),
            Value::Int(3),
        ])
        .expect("hash table");
        let _ = builtin_puthash(vec![Value::Float(1.0, next_float_id()), Value::Int(1), table])
            .expect("puthash 1.0");
        let _ = builtin_puthash(vec![Value::Float(2.0, next_float_id()), Value::Int(2), table])
            .expect("puthash 2.0");

        assert_eq!(collect_float_hashes(table), expected);
    }
}

#[test]
fn internal_hash_table_buckets_match_oracle_float_special_hashes() {
    fn collect_hashes(table: Value) -> Vec<i64> {
        let buckets = builtin_internal_hash_table_buckets(vec![table]).expect("bucket alists");
        let outer = list_to_vec(&buckets).expect("outer list");
        let mut seen = Vec::new();
        for bucket in outer {
            let entries = list_to_vec(&bucket).expect("bucket alist");
            for entry in entries {
                let Value::Cons(cell) = entry else {
                    panic!("expected alist cons entry");
                };
                let pair = read_cons(cell);
                let hash = pair.cdr.as_int().expect("diagnostic hash integer");
                seen.push(hash);
            }
        }
        seen.sort_unstable();
        seen
    }

    let expected = vec![0_i64, 2_146_959_360_i64, 2_147_483_648_i64];
    for test_name in ["eql", "equal"] {
        let table = builtin_make_hash_table(vec![
            Value::keyword(":test"),
            Value::symbol(test_name),
            Value::keyword(":size"),
            Value::Int(5),
        ])
        .expect("hash table");
        let _ = builtin_puthash(vec![
            Value::Float(-0.0, next_float_id()),
            Value::symbol("neg"),
            table,
        ])
        .expect("puthash -0.0");
        let _ = builtin_puthash(vec![Value::Float(0.0, next_float_id()), Value::symbol("pos"), table])
            .expect("puthash 0.0");
        let _ = builtin_puthash(vec![
            Value::Float(f64::NAN, next_float_id()),
            Value::symbol("nan"),
            table,
        ])
        .expect("puthash nan");
        assert_eq!(collect_hashes(table), expected);
    }
}

#[test]
fn internal_hash_table_introspection_type_errors() {
    assert!(builtin_internal_hash_table_buckets(vec![Value::Nil]).is_err());
    assert!(builtin_internal_hash_table_histogram(vec![Value::Nil]).is_err());
    assert!(builtin_internal_hash_table_index_size(vec![Value::Nil]).is_err());
}
