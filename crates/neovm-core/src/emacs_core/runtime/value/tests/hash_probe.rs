//! The in-place hash-table probe must find exactly what the materialized
//! [`HashKey`] finds: same hash stream, same equivalence relation.
use super::super::*;
use crate::emacs_core::eval::Context;
use crate::emacs_core::intern::intern;
use std::hash::{Hash, Hasher};

fn fx_hash<T: Hash + ?Sized>(t: &T) -> u64 {
    let mut hasher = rustc_hash::FxHasher::default();
    t.hash(&mut hasher);
    hasher.finish()
}

fn list(items: &[Value]) -> Value {
    items
        .iter()
        .rev()
        .fold(Value::NIL, |tail, item| Value::cons(*item, tail))
}

/// Values covering every arm of `to_eq_key` / `to_eql_key` /
/// `to_equal_key_depth_swp` the probe admits, plus ones it must decline.
fn corpus() -> Vec<Value> {
    let sym = Value::from_sym_id(intern("hash-probe-sym"));
    let other_sym = Value::from_sym_id(intern("hash-probe-other"));
    let mut deep = Value::fixnum(0);
    for _ in 0..250 {
        deep = Value::cons(deep, Value::NIL);
    }
    vec![
        Value::NIL,
        Value::T,
        Value::fixnum(0),
        Value::fixnum(-1),
        Value::fixnum(1 << 40),
        sym,
        other_sym,
        Value::keyword("hash-probe-key"),
        Value::string(""),
        Value::string("abc"),
        Value::string("abc"),
        Value::string("ünïcødé"),
        Value::make_float(1.5),
        Value::make_float(1.5),
        Value::make_float(-0.0),
        Value::make_float(0.0),
        Value::make_float(f64::NAN),
        Value::cons(Value::fixnum(1), Value::fixnum(2)),
        Value::cons(Value::fixnum(1), Value::fixnum(2)),
        list(&[Value::fixnum(1), Value::fixnum(2)]),
        list(&[Value::fixnum(1), Value::fixnum(2), Value::fixnum(3)]),
        list(&[sym, Value::string("s"), Value::make_float(2.5)]),
        list(&[sym, Value::string("s"), Value::make_float(2.5)]),
        Value::cons(list(&[Value::fixnum(1)]), list(&[Value::fixnum(2)])),
        Value::cons(Value::string("a"), Value::string("b")),
        Value::vector(vec![Value::fixnum(1), Value::fixnum(2)]),
        Value::vector(vec![Value::fixnum(1), Value::fixnum(2)]),
        Value::cons(Value::vector(vec![Value::fixnum(1)]), Value::NIL),
        deep,
    ]
}

const TESTS: [HashTableTest; 3] = [HashTableTest::Eq, HashTableTest::Eql, HashTableTest::Equal];

#[test]
fn probe_hash_and_equivalence_match_the_materialized_key() {
    let values = corpus();
    for test in TESTS {
        let keys: Vec<HashKey> = values
            .iter()
            .map(|v| v.to_hash_key_swp(&test, false))
            .collect();
        for (i, value) in values.iter().enumerate() {
            let Some(probe) = ValueKeyProbe::new(*value, test, false) else {
                continue;
            };
            assert_eq!(
                fx_hash(&probe),
                fx_hash(&keys[i]),
                "hash stream differs for {value:?} under {test:?}"
            );
            for (j, key) in keys.iter().enumerate() {
                assert_eq!(
                    hashbrown::Equivalent::equivalent(&probe, key),
                    keys[i] == *key,
                    "equivalence differs for {value:?} vs {:?} under {test:?}",
                    values[j]
                );
            }
        }
    }
}

#[test]
fn probe_admits_structural_shapes_and_declines_the_rest() {
    let values = corpus();
    let supported = |v: &Value, test| ValueKeyProbe::new(*v, test, false).is_some();
    // Everything keys by identity under `eq`, so every value is admitted.
    assert!(values.iter().all(|v| supported(v, HashTableTest::Eq)));
    // Under `equal`, vectors and 250-deep lists take the materializing path.
    assert!(!supported(&values[values.len() - 1], HashTableTest::Equal));
    assert!(!supported(
        &Value::vector(vec![Value::fixnum(1)]),
        HashTableTest::Equal
    ));
    assert!(!supported(
        &Value::cons(Value::vector(vec![]), Value::NIL),
        HashTableTest::Equal
    ));
    let wide = list(&vec![Value::fixnum(7); FAST_PROBE_NODE_BUDGET + 1]);
    assert!(!supported(&wide, HashTableTest::Equal));
    assert!(supported(
        &list(&[Value::fixnum(1), Value::string("s")]),
        HashTableTest::Equal
    ));
    assert!(supported(&Value::string("s"), HashTableTest::Equal));
}

#[test]
fn storage_lookups_by_value_agree_with_lookups_by_key() {
    let values = corpus();
    for test in TESTS {
        let mut storage = HashTableStorage::default();
        for (i, value) in values.iter().enumerate() {
            storage.insert(
                value.to_hash_key_swp(&test, false),
                *value,
                Value::fixnum(i as i64),
            );
        }
        for value in &values {
            let by_key = storage.get(&value.to_hash_key_swp(&test, false)).copied();
            assert_eq!(
                storage.lookup(*value, test, false).copied(),
                by_key,
                "{value:?} under {test:?}"
            );
        }
        let fresh = list(&[Value::fixnum(1), Value::fixnum(2)]);
        let expect_hit = matches!(test, HashTableTest::Equal);
        assert_eq!(
            storage.get_by_value(fresh, test, false).is_some(),
            expect_hit
        );
        if expect_hit {
            *storage.get_mut_by_value(fresh, test, false).unwrap() = Value::T;
            assert_eq!(
                storage.get(&fresh.to_hash_key_swp(&test, false)).copied(),
                Some(Value::T)
            );
            assert_eq!(storage.remove_by_value(fresh, test, false), Some(Value::T));
            assert!(storage.get(&fresh.to_hash_key_swp(&test, false)).is_none());
        }
    }
}

#[test]
fn gethash_puthash_remhash_match_gnu() {
    let mut eval = Context::new();
    // Expectation taken from GNU Emacs 31.0.90 --batch.
    let result = eval
        .eval_str(
            r#"(let ((h (make-hash-table :test 'equal)) (e (make-hash-table :test 'eql)) (q (make-hash-table)))
                 (puthash (list 1 2) 'x h) (puthash "s" 'y h) (puthash 1.5 'f h)
                 (puthash 1.5 'g e) (puthash 'sym 's q) (puthash 7 'seven q)
                 (format "%S" (list (gethash (list 1 2) h) (gethash "s" h) (gethash (list 1 2 3) h)
                                    (gethash 1.5 h) (gethash 1.5 e) (gethash 1.5 q) (gethash 'sym q)
                                    (gethash 7 q) (gethash "s" q)
                                    (progn (remhash (list 1 2) h) (gethash (list 1 2) h 'gone))
                                    (progn (puthash "s" 'z h) (gethash "s" h))
                                    (hash-table-count h))))"#,
        )
        .expect("hash table forms evaluate");
    assert_eq!(
        result.as_utf8_str(),
        Some("(x y nil f g nil s seven nil gone z 2)")
    );
}

/// Small `eq`/`eql` tables answer fixnum and symbol lookups by scanning
/// slots for the key's bits (`small_identity_scan`). Across every table size
/// up to and past the scan limit, with removals leaving holes and mixed key
/// shapes, the answer must be the hashed lookup's.
#[test]
fn small_table_identity_scans_agree_with_hashed_lookups() {
    struct Rng(u64);
    impl Rng {
        fn below(&mut self, n: usize) -> usize {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            (self.0 % n as u64) as usize
        }
    }
    let mut rng = Rng(0x243f_6a88_85a3_08d3);
    let syms: Vec<Value> = (0..24)
        .map(|i| Value::from_sym_id(intern(&format!("small-scan-sym-{i}"))))
        .collect();
    let pool = |rng: &mut Rng| -> Value {
        match rng.below(7) {
            0 | 1 => Value::fixnum(rng.below(40) as i64 - 5),
            2 | 3 => syms[rng.below(syms.len())],
            4 => Value::make_float(rng.below(4) as f64),
            5 => Value::string(&format!("k{}", rng.below(4))),
            _ => [Value::NIL, Value::T, Value::keyword("small-scan-kw")][rng.below(3)],
        }
    };
    let mut checked = 0;
    for test in [HashTableTest::Eq, HashTableTest::Eql] {
        for size in 0..40 {
            let mut storage = HashTableStorage::default();
            for i in 0..size {
                let key = pool(&mut rng);
                storage.insert(
                    key.to_hash_key_swp(&test, false),
                    key,
                    Value::fixnum(i as i64),
                );
                if rng.below(5) == 0 {
                    let gone = pool(&mut rng);
                    storage.remove(&gone.to_hash_key_swp(&test, false));
                }
            }
            for _ in 0..60 {
                let probe = pool(&mut rng);
                let by_key = storage.get(&probe.to_hash_key_swp(&test, false)).copied();
                assert_eq!(
                    storage
                        .lookup(probe, test, false)
                        .copied()
                        .map(|v| v.bits()),
                    by_key.map(|v| v.bits()),
                    "{probe:?} under {test:?} in a {size}-entry table"
                );
                checked += 1;
            }
        }
    }
    assert!(checked > 4000, "checked {checked}");
}

/// With `symbols-with-pos-enabled`, a positioned key `eq`s its bare symbol,
/// which no bit comparison sees: the scan must decline.
#[test]
fn small_table_scans_decline_under_symbols_with_pos() {
    let mut ctx = Context::new();
    let sym = Value::from_sym_id(intern("small-scan-positioned"));
    let positioned = ctx.tagged_heap.alloc_symbol_with_pos(sym, Value::fixnum(3));
    for test in [HashTableTest::Eq, HashTableTest::Eql] {
        let mut storage = HashTableStorage::default();
        storage.insert(
            positioned.to_hash_key_swp(&test, true),
            positioned,
            Value::fixnum(7),
        );
        let by_key = storage.get(&sym.to_hash_key_swp(&test, true)).copied();
        assert_eq!(
            storage.lookup(sym, test, true).copied().map(|v| v.bits()),
            by_key.map(|v| v.bits()),
            "bare symbol against a positioned key under {test:?}"
        );
    }
}
