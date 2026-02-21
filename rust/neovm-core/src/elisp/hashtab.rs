//! Extended hash-table and obarray builtins.
//!
//! Supplements the basic hash-table operations in `builtins.rs` with:
//! - `maphash`
//! - `hash-table-test`, `hash-table-size`, `hash-table-rehash-size`,
//!   `hash-table-rehash-threshold`, `hash-table-weakness`
//! - `copy-hash-table`
//! - `mapatoms`, `unintern`

use super::error::{signal, EvalResult, Flow};
use super::print::print_value;
use super::value::*;
use std::collections::{hash_map::DefaultHasher, BTreeMap};
use std::hash::{Hash, Hasher};

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn validate_optional_obarray_arg(args: &[Value]) -> Result<(), Flow> {
    if let Some(obarray) = args.get(1) {
        if !obarray.is_nil() && !matches!(obarray, Value::Vector(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("obarrayp"), obarray.clone()],
            ));
        }
    }
    Ok(())
}

/// Convert a `HashKey` back into a `Value`.
fn hash_key_to_value(key: &HashKey) -> Value {
    match key {
        HashKey::Nil => Value::Nil,
        HashKey::True => Value::True,
        HashKey::Int(n) => Value::Int(*n),
        HashKey::Float(bits) => Value::Float(f64::from_bits(*bits)),
        HashKey::Symbol(s) => Value::symbol(s.clone()),
        HashKey::Keyword(s) => Value::Keyword(s.clone()),
        HashKey::Str(s) => Value::string(s.clone()),
        HashKey::Char(c) => Value::Char(*c),
        HashKey::Window(id) => Value::Window(*id),
        HashKey::Frame(id) => Value::Frame(*id),
        HashKey::Ptr(_) => Value::Nil, // can't reconstruct from pointer
    }
}

fn hash_value_for_equal(value: &Value, hasher: &mut DefaultHasher, depth: usize) {
    if depth > 4096 {
        0_u8.hash(hasher);
        return;
    }
    match value {
        Value::Nil => 0_u8.hash(hasher),
        Value::True => 1_u8.hash(hasher),
        Value::Int(n) => {
            // `equal` treats chars and ints with same codepoint as equal.
            2_u8.hash(hasher);
            n.hash(hasher);
        }
        Value::Char(c) => {
            2_u8.hash(hasher);
            (*c as i64).hash(hasher);
        }
        Value::Float(f) => {
            3_u8.hash(hasher);
            f.to_bits().hash(hasher);
        }
        Value::Symbol(s) => {
            4_u8.hash(hasher);
            s.hash(hasher);
        }
        Value::Keyword(s) => {
            5_u8.hash(hasher);
            s.hash(hasher);
        }
        Value::Str(s) => {
            6_u8.hash(hasher);
            s.hash(hasher);
        }
        Value::Cons(cons) => {
            7_u8.hash(hasher);
            let pair = cons.lock().expect("poisoned");
            hash_value_for_equal(&pair.car, hasher, depth + 1);
            hash_value_for_equal(&pair.cdr, hasher, depth + 1);
        }
        Value::Vector(vec) => {
            8_u8.hash(hasher);
            let items = vec.lock().expect("poisoned");
            items.len().hash(hasher);
            for item in items.iter() {
                hash_value_for_equal(item, hasher, depth + 1);
            }
        }
        Value::Window(id) => {
            9_u8.hash(hasher);
            id.hash(hasher);
        }
        Value::Frame(id) => {
            10_u8.hash(hasher);
            id.hash(hasher);
        }
        Value::Buffer(id) => {
            11_u8.hash(hasher);
            id.0.hash(hasher);
        }
        Value::Timer(id) => {
            12_u8.hash(hasher);
            id.hash(hasher);
        }
        Value::Subr(name) => {
            13_u8.hash(hasher);
            name.hash(hasher);
        }
        Value::Lambda(lambda) => {
            14_u8.hash(hasher);
            (std::sync::Arc::as_ptr(lambda) as usize).hash(hasher);
        }
        Value::Macro(macro_fn) => {
            15_u8.hash(hasher);
            (std::sync::Arc::as_ptr(macro_fn) as usize).hash(hasher);
        }
        Value::HashTable(table) => {
            16_u8.hash(hasher);
            (std::sync::Arc::as_ptr(table) as usize).hash(hasher);
        }
        Value::ByteCode(bytecode) => {
            17_u8.hash(hasher);
            (std::sync::Arc::as_ptr(bytecode) as usize).hash(hasher);
        }
    }
}

fn sxhash_for(value: &Value, test: HashTableTest) -> i64 {
    let mut hasher = DefaultHasher::new();
    match test {
        HashTableTest::Equal => hash_value_for_equal(value, &mut hasher, 0),
        _ => value.to_hash_key(&test).hash(&mut hasher),
    }
    // Emacs returns fixnums; keep the top bit clear to stay non-negative.
    (hasher.finish() & (i64::MAX as u64)) as i64
}

fn internal_hash_table_index_size(table: &LispHashTable) -> usize {
    let requested = if table.size <= 0 {
        1_u64
    } else {
        (table.size as u64).saturating_add(1)
    };
    let from_entries = (table.data.len() as u64).saturating_add(1);
    let needed = requested.max(from_entries);
    let pow2 = needed.next_power_of_two();
    pow2.min(usize::MAX as u64) as usize
}

fn internal_hash_table_nonempty_buckets(table: &LispHashTable) -> Vec<Vec<(Value, Value)>> {
    if table.data.is_empty() {
        return Vec::new();
    }
    let bucket_count = internal_hash_table_index_size(table).max(1);
    let mut buckets: Vec<Vec<(Value, Value)>> = vec![Vec::new(); bucket_count];
    for (key, value) in &table.data {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let index = (hasher.finish() as usize) % bucket_count;
        buckets[index].push((hash_key_to_value(key), value.clone()));
    }
    for bucket in &mut buckets {
        bucket.sort_by_key(|(key, value)| (print_value(key), print_value(value)));
    }
    buckets
        .into_iter()
        .filter(|bucket| !bucket.is_empty())
        .collect()
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (hash-table-test TABLE) -> symbol
pub(crate) fn builtin_hash_table_test(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-test", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let sym = match table.test {
                HashTableTest::Eq => "eq",
                HashTableTest::Eql => "eql",
                HashTableTest::Equal => "equal",
            };
            Ok(Value::symbol(sym))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-size TABLE) -> integer
pub(crate) fn builtin_hash_table_size(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-size", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(Value::Int(table.size))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-rehash-size TABLE) -> float
pub(crate) fn builtin_hash_table_rehash_size(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-rehash-size", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(Value::Float(table.rehash_size))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-rehash-threshold TABLE) -> float
pub(crate) fn builtin_hash_table_rehash_threshold(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-rehash-threshold", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(Value::Float(table.rehash_threshold))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-weakness TABLE) -> nil | symbol
pub(crate) fn builtin_hash_table_weakness(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-weakness", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(match table.weakness {
                None => Value::Nil,
                Some(HashTableWeakness::Key) => Value::symbol("key"),
                Some(HashTableWeakness::Value) => Value::symbol("value"),
                Some(HashTableWeakness::KeyOrValue) => Value::symbol("key-or-value"),
                Some(HashTableWeakness::KeyAndValue) => Value::symbol("key-and-value"),
            })
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (copy-hash-table TABLE) -> new hash table with same entries
pub(crate) fn builtin_copy_hash_table(args: Vec<Value>) -> EvalResult {
    expect_args("copy-hash-table", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let new_table = table.clone();
            Ok(Value::HashTable(std::sync::Arc::new(
                std::sync::Mutex::new(new_table),
            )))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-keys TABLE) -> list of keys
#[cfg(test)]
pub(crate) fn builtin_hash_table_keys(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-keys", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let keys: Vec<Value> = table.data.keys().map(hash_key_to_value).collect();
            Ok(Value::list(keys))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// (hash-table-values TABLE) -> list of values
#[cfg(test)]
pub(crate) fn builtin_hash_table_values(args: Vec<Value>) -> EvalResult {
    expect_args("hash-table-values", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let values: Vec<Value> = table.data.values().cloned().collect();
            Ok(Value::list(values))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// `(sxhash-eq OBJECT)` -- hash OBJECT according to `eq` semantics.
pub(crate) fn builtin_sxhash_eq(args: Vec<Value>) -> EvalResult {
    expect_args("sxhash-eq", &args, 1)?;
    Ok(Value::Int(sxhash_for(&args[0], HashTableTest::Eq)))
}

/// `(sxhash-eql OBJECT)` -- hash OBJECT according to `eql` semantics.
pub(crate) fn builtin_sxhash_eql(args: Vec<Value>) -> EvalResult {
    expect_args("sxhash-eql", &args, 1)?;
    Ok(Value::Int(sxhash_for(&args[0], HashTableTest::Eql)))
}

/// `(sxhash-equal OBJECT)` -- hash OBJECT according to `equal` semantics.
pub(crate) fn builtin_sxhash_equal(args: Vec<Value>) -> EvalResult {
    expect_args("sxhash-equal", &args, 1)?;
    Ok(Value::Int(sxhash_for(&args[0], HashTableTest::Equal)))
}

/// `(sxhash-equal-including-properties OBJECT)` -- hash OBJECT like
/// `equal-including-properties`. NeoVM currently has no text properties, so
/// this matches `sxhash-equal`.
pub(crate) fn builtin_sxhash_equal_including_properties(args: Vec<Value>) -> EvalResult {
    expect_args("sxhash-equal-including-properties", &args, 1)?;
    Ok(Value::Int(sxhash_for(&args[0], HashTableTest::Equal)))
}

/// `(internal--hash-table-index-size TABLE)` -- report hash index width.
pub(crate) fn builtin_internal_hash_table_index_size(args: Vec<Value>) -> EvalResult {
    expect_args("internal--hash-table-index-size", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            Ok(Value::Int(
                internal_hash_table_index_size(&table).min(i64::MAX as usize) as i64,
            ))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// `(internal--hash-table-buckets TABLE)` -- return non-empty bucket alists.
pub(crate) fn builtin_internal_hash_table_buckets(args: Vec<Value>) -> EvalResult {
    expect_args("internal--hash-table-buckets", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let buckets = internal_hash_table_nonempty_buckets(&table);
            if buckets.is_empty() {
                return Ok(Value::Nil);
            }
            let rendered = buckets
                .into_iter()
                .map(|bucket| {
                    let alist_items: Vec<Value> = bucket
                        .into_iter()
                        .map(|(key, value)| Value::cons(key, value))
                        .collect();
                    Value::list(alist_items)
                })
                .collect();
            Ok(Value::list(rendered))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

/// `(internal--hash-table-histogram TABLE)` -- return (bucket-size . count)
/// alist for non-empty buckets.
pub(crate) fn builtin_internal_hash_table_histogram(args: Vec<Value>) -> EvalResult {
    expect_args("internal--hash-table-histogram", &args, 1)?;
    match &args[0] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let buckets = internal_hash_table_nonempty_buckets(&table);
            if buckets.is_empty() {
                return Ok(Value::Nil);
            }
            let mut histogram: BTreeMap<i64, i64> = BTreeMap::new();
            for bucket in buckets {
                let size = bucket.len() as i64;
                *histogram.entry(size).or_insert(0) += 1;
            }
            let entries: Vec<Value> = histogram
                .into_iter()
                .map(|(size, count)| Value::cons(Value::Int(size), Value::Int(count)))
                .collect();
            Ok(Value::list(entries))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("hash-table-p"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// (maphash FUNCTION TABLE) — call FUNCTION with each (KEY VALUE) pair.
pub(crate) fn builtin_maphash(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("maphash", &args, 2)?;
    let func = args[0].clone();
    let entries: Vec<(Value, Value)> = match &args[1] {
        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            table
                .data
                .iter()
                .map(|(k, v)| (hash_key_to_value(k), v.clone()))
                .collect()
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("hash-table-p"), other.clone()],
            ));
        }
    };
    for (key, val) in entries {
        eval.apply(func.clone(), vec![key, val])?;
    }
    Ok(Value::Nil)
}

/// (mapatoms FUNCTION &optional OBARRAY) — call FUNCTION with each interned symbol.
pub(crate) fn builtin_mapatoms(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("mapatoms", &args, 1)?;
    expect_max_args("mapatoms", &args, 2)?;
    validate_optional_obarray_arg(&args)?;
    let func = args[0].clone();
    // Collect symbol names to avoid borrowing obarray during eval
    let symbols: Vec<String> = eval
        .obarray
        .all_symbols()
        .iter()
        .map(|s| s.to_string())
        .collect();
    for sym in symbols {
        eval.apply(func.clone(), vec![Value::symbol(sym)])?;
    }
    Ok(Value::Nil)
}

/// (unintern NAME &optional OBARRAY) — remove symbol from obarray.
pub(crate) fn builtin_unintern(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("unintern", &args, 1)?;
    expect_max_args("unintern", &args, 2)?;
    validate_optional_obarray_arg(&args)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    let removed = eval.obarray.unintern(&name);
    Ok(if removed { Value::True } else { Value::Nil })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::builtins::builtin_make_hash_table;

    #[test]
    fn hash_table_keys_values_basics() {
        let table = Value::hash_table(HashTableTest::Equal);
        if let Value::HashTable(ht) = &table {
            let mut raw = ht.lock().expect("poisoned");
            let test = raw.test.clone();
            raw.data
                .insert(Value::symbol("alpha").to_hash_key(&test), Value::Int(1));
            raw.data
                .insert(Value::symbol("beta").to_hash_key(&test), Value::Int(2));
        } else {
            panic!("expected hash table");
        }

        let keys = builtin_hash_table_keys(vec![table.clone()]).unwrap();
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
        let size = builtin_hash_table_rehash_size(vec![table.clone()]).unwrap();
        let threshold = builtin_hash_table_rehash_threshold(vec![table]).unwrap();

        assert_eq!(size, Value::Float(1.5));
        assert_eq!(threshold, Value::Float(0.8125));
    }

    #[test]
    fn hash_table_rehash_options_are_ignored() {
        let table = builtin_make_hash_table(vec![
            Value::keyword(":rehash-size"),
            Value::Float(2.0),
            Value::keyword(":rehash-threshold"),
            Value::Float(0.9),
        ])
        .unwrap();

        let size = builtin_hash_table_rehash_size(vec![table.clone()]).unwrap();
        let threshold = builtin_hash_table_rehash_threshold(vec![table]).unwrap();

        assert_eq!(size, Value::Float(1.5));
        assert_eq!(threshold, Value::Float(0.8125));

        assert!(builtin_make_hash_table(vec![
            Value::keyword(":rehash-size"),
            Value::string("x"),
            Value::keyword(":rehash-threshold"),
            Value::Float(1.5),
        ])
        .is_ok());
        assert!(builtin_make_hash_table(vec![
            Value::keyword(":rehash-threshold"),
            Value::string("x"),
            Value::keyword(":rehash-size"),
            Value::Float(1.5),
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
            builtin_sxhash_equal(vec![left.clone()]).unwrap(),
            builtin_sxhash_equal(vec![right.clone()]).unwrap()
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
    fn internal_hash_table_introspection_empty_defaults() {
        let table = builtin_make_hash_table(vec![]).unwrap();
        assert_eq!(
            builtin_internal_hash_table_buckets(vec![table.clone()]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_internal_hash_table_histogram(vec![table.clone()]).unwrap(),
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
    fn internal_hash_table_introspection_type_errors() {
        assert!(builtin_internal_hash_table_buckets(vec![Value::Nil]).is_err());
        assert!(builtin_internal_hash_table_histogram(vec![Value::Nil]).is_err());
        assert!(builtin_internal_hash_table_index_size(vec![Value::Nil]).is_err());
    }
}
