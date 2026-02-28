# Float Allocation ID: `Value::Float(f64, u32)`

## Problem

NeoVM stores floats as immediate `Value::Float(f64)`. GNU Emacs
heap-allocates floats, so `eq` is pointer identity. This causes
semantic divergence:

- `(funcall 'eq 1.0 1.0)` → NeoVM: `t`, Emacs: `nil`
- `(memq 1.0 '(1.0 2.0))` → NeoVM: finds match, Emacs: `nil`
- `(eq (+ 0.5 0.5) 1.0)` → NeoVM: `t`, Emacs: `nil`
- eq-test hash tables with float keys match by value, not identity
- The macroexpand infinite loop in faces.el was one symptom (fixed
  with an AST heuristic in `eval_eq_call`, which this design replaces)

## Solution

Change `Value::Float(f64)` to `Value::Float(f64, u32)`. The `u32` is a
thread-local monotonic counter bumped each time a float is created.
Two floats are `eq` only when both the f64 bits AND the allocation ID
match.

### Key properties

- **Value stays 16 bytes.** The u32 fits in existing enum padding
  (verified with rustc).
- **Perfect GNU Emacs `eq` semantics** for all call paths.
- **No `unsafe` code.** Safe Rust enum with pattern matching preserved.
- **No API signature changes.** Thread-local counter needs no parameter
  threading.
- **Deletes `eval_eq_call`.** The AST heuristic is no longer needed.

## Detailed Changes

### 1. Value enum (`value.rs`)

```rust
// Before:
Float(f64),
// After:
Float(f64, u32),
```

### 2. Thread-local allocation counter (`value.rs`)

```rust
use std::cell::Cell;

thread_local! {
    static FLOAT_ALLOC_ID: Cell<u32> = const { Cell::new(0) };
}

pub fn next_float_id() -> u32 {
    FLOAT_ALLOC_ID.with(|c| {
        let id = c.get();
        c.set(id.wrapping_add(1));
        id
    })
}
```

### 3. Equality functions (`value.rs`)

**`eq_value`** — compare ID + bits (identity):
```rust
(Value::Float(a, id_a), Value::Float(b, id_b)) =>
    id_a == id_b && a.to_bits() == b.to_bits(),
```

**`eql_value`** — compare bits only (numeric value equality):
```rust
(Value::Float(a, _), Value::Float(b, _)) => a.to_bits() == b.to_bits(),
```

**`equal_value`** — compare bits only:
```rust
(Value::Float(a, _), Value::Float(b, _)) => a.to_bits() == b.to_bits(),
```

### 4. Hash keys (`value.rs`, `hashtab.rs`)

- `to_eq_key`: include ID →
  `Value::Float(f, id) => HashKey::FloatEq(f.to_bits(), id)`
- `to_eql_key`: ignore ID →
  `Value::Float(f, _) => HashKey::Float(f.to_bits())`
- `to_equal_key`: ignore ID →
  `Value::Float(f, _) => HashKey::Float(f.to_bits())`

Add a `HashKey::FloatEq(u64, u32)` variant for eq-test hash tables.

### 5. Delete `eval_eq_call` (`eval.rs`)

Remove the entire function and its call sites. The AST-level float
interception is no longer needed — `eq_value` now handles all paths
correctly.

### 6. Two dangerous reconstruction sites

**`arithmetic.rs:344`** — `(float X)` when X is already a float must
preserve the original ID:
```rust
// Before:
Value::Float(f) => Ok(Value::Float(*f)),
// After:
Value::Float(f, id) => Ok(Value::Float(*f, *id)),
```

**`hashtab.rs:82`** — `hash_key_to_value` reconstructs a float from
stored bits. The `HashKey` must carry the allocation ID so it can be
restored.

### 7. Mechanical changes

| Category | Count | Change |
|----------|-------|--------|
| New float creation | ~63 | Add `, next_float_id()` |
| Read-only / pattern match | ~81 | Add `, _` to pattern |
| Test code | ~148 | Mechanical update |
| Dangerous reconstruction | 2 | Preserve original ID |

## Correctness

| Expression | Before | After | GNU Emacs |
|------------|--------|-------|-----------|
| `(eq 1.0 1.0)` | nil | nil | nil |
| `(let ((x 1.0)) (eq x x))` | t | t | t |
| `(eq form (setq nf (identity form)))` | t | t | t |
| `(funcall 'eq 1.0 1.0)` | **t** | nil | nil |
| `(memq 1.0 '(1.0 2.0))` | **matches** | nil | nil |
| `(eq (+ 0.5 0.5) 1.0)` | **t** | nil | nil |
| Bytecode `(eq 1.0 1.0)` | **t** | nil | nil |

## Performance

- Float creation: +1-3 ns (thread-local counter increment)
- Float reads: zero overhead (`, _` optimized away)
- Memory: zero impact (Value stays 16 bytes)
- No heap allocation, no GC pressure, no indirection
