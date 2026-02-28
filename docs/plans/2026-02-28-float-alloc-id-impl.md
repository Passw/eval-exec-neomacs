# Float Allocation ID Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `u32` allocation ID to `Value::Float` so that `eq` on floats matches GNU Emacs pointer-identity semantics.

**Architecture:** Change `Value::Float(f64)` to `Value::Float(f64, u32)` where the `u32` is a thread-local monotonic counter. `eq_value` compares both ID and bits. `eql_value`/`equal_value` compare bits only. Delete the `eval_eq_call` AST heuristic.

**Tech Stack:** Rust, neovm-core crate

**Design doc:** `docs/plans/2026-02-28-float-alloc-id-design.md`

---

### Task 1: Add `next_float_id()` and change `Value::Float(f64)` to `Value::Float(f64, u32)`

This is the core change. Everything else follows from it.

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/value.rs:302` (enum variant)
- Modify: `rust/neovm-core/src/emacs_core/value.rs` (add `next_float_id` function)

**Step 1: Add the thread-local counter**

Add after the existing `use` block near the top of `value.rs`:

```rust
use std::cell::Cell;

thread_local! {
    static FLOAT_ALLOC_ID: Cell<u32> = const { Cell::new(0) };
}

/// Allocate a fresh float identity. Each call returns a unique u32
/// (within the current thread), matching GNU Emacs's `make_float`
/// semantics where every float creation produces a distinct object.
pub fn next_float_id() -> u32 {
    FLOAT_ALLOC_ID.with(|c| {
        let id = c.get();
        c.set(id.wrapping_add(1));
        id
    })
}
```

**Step 2: Change the Float variant**

At line 302, change:
```rust
Float(f64),
```
to:
```rust
Float(f64, u32),
```

**Step 3: Update the size assertion**

At line 1221-1225, the test asserts `size_of::<Value>() == 16`. This should still pass (the u32 fits in existing padding), but verify by running:

```
cargo test --release -p neovm-core value_is_copy_and_16_bytes
```

Expected: PASS (Value is still 16 bytes).

**Step 4: DO NOT commit yet** — the crate won't compile until all `Value::Float` sites are updated in subsequent tasks.

---

### Task 2: Update equality functions in `value.rs`

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/value.rs:962-1014`

**Step 1: Update `eq_value` (line 967)**

Change:
```rust
(Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
```
to:
```rust
(Value::Float(a, id_a), Value::Float(b, id_b)) => id_a == id_b && a.to_bits() == b.to_bits(),
```

**Step 2: Update `eql_value` (line 993)**

Change:
```rust
(Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
```
to:
```rust
(Value::Float(a, _), Value::Float(b, _)) => a.to_bits() == b.to_bits(),
```

**Step 3: Update `equal_value` (line 1009)**

Change:
```rust
(Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
```
to:
```rust
(Value::Float(a, _), Value::Float(b, _)) => a.to_bits() == b.to_bits(),
```

---

### Task 3: Update `HashKey` and hash key functions in `value.rs`

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/value.rs:418-500`

**Step 1: Add `FloatEq` variant to `HashKey` (after line 422)**

Change:
```rust
Float(u64), // bits
```
to:
```rust
Float(u64),          // bits (for eql/equal hash tables)
FloatEq(u64, u32),   // bits + alloc ID (for eq hash tables)
```

**Step 2: Update `Hash` impl for `HashKey` (line 446)**

After the `HashKey::Float(bits)` arm, add:
```rust
HashKey::FloatEq(bits, id) => {
    bits.hash(state);
    id.hash(state);
}
```

**Step 3: Update `PartialEq` impl for `HashKey` (line 475)**

After the `HashKey::Float(a, b)` arm, add:
```rust
(HashKey::FloatEq(a, id_a), HashKey::FloatEq(b, id_b)) => a == b && id_a == id_b,
```

**Step 4: Update `to_eq_key` (line 888)**

Change:
```rust
Value::Float(f) => HashKey::Float(f.to_bits()),
```
to:
```rust
Value::Float(f, id) => HashKey::FloatEq(f.to_bits(), *id),
```

**Step 5: Update `to_eql_key` (line 909)**

Change:
```rust
Value::Float(f) => HashKey::Float(f.to_bits()),
```
to:
```rust
Value::Float(f, _) => HashKey::Float(f.to_bits()),
```

**Step 6: Update `to_equal_key_depth` (line 928)**

Change:
```rust
Value::Float(f) => HashKey::Float(f.to_bits()),
```
to:
```rust
Value::Float(f, _) => HashKey::Float(f.to_bits()),
```

---

### Task 4: Update all remaining `Value::Float` sites in `value.rs`

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/value.rs` (all remaining Float patterns)

Update every remaining `Value::Float(f)` pattern in value.rs to `Value::Float(f, _)` for reads, or `Value::Float(val, next_float_id())` for creations. Key sites:

- `is_number()`, `is_float()`, `type_name()`: change `Value::Float(_)` to `Value::Float(_, _)`
- `as_number_f64()`, `as_float()`: change `Value::Float(f)` to `Value::Float(f, _)`
- Test code in `#[cfg(test)]`: change `Value::Float(x)` to `Value::Float(x, next_float_id())`

Use `cargo check -p neovm-core 2>&1 | head -50` after each batch to verify progress.

---

### Task 5: Update `hashtab.rs` — fix dangerous reconstruction site

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/hashtab.rs:82` (Category B: preserve ID)
- Modify: all other `Value::Float` patterns in `hashtab.rs`

**Step 1: Fix `hash_key_to_value` (line 82)**

This is a Category B (dangerous reconstruction) site. The `HashKey` now carries the float allocation ID via `FloatEq`, so:

Change:
```rust
HashKey::Float(bits) => Value::Float(f64::from_bits(*bits)),
```
to:
```rust
HashKey::Float(bits) => Value::Float(f64::from_bits(*bits), next_float_id()),
HashKey::FloatEq(bits, id) => Value::Float(f64::from_bits(*bits), *id),
```

**Step 2: Update all remaining `Value::Float` patterns in `hashtab.rs`**

Read-only patterns: add `, _`. Test code: add `, next_float_id()`.

---

### Task 6: Update `arithmetic.rs` — fix dangerous reconstruction + creation sites

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/builtins/arithmetic.rs`

**Step 1: Fix `builtin_float` (line 344) — Category B: preserve ID**

Change:
```rust
Value::Float(f) => Ok(Value::Float(*f)),
```
to:
```rust
Value::Float(f, id) => Ok(Value::Float(*f, *id)),
```

**Step 2: Update all Category A sites (new float creation)**

All arithmetic results get `next_float_id()`. Examples:
```rust
// Before:
Value::Float(sum)
// After:
Value::Float(sum, next_float_id())
```

**Step 3: Update all Category C sites (read-only patterns)**

Add `, _` to destructure patterns. Example:
```rust
// Before:
Value::Float(f) => Ok(*f)
// After:
Value::Float(f, _) => Ok(*f)
```

---

### Task 7: Update `floatfns.rs`

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/floatfns.rs`

All Category A (new float creation): add `, next_float_id()`.
All Category C (read-only): add `, _`.
All Category D (test code): add `, next_float_id()` for creations, `, _` for patterns.

---

### Task 8: Update `bytecode/vm.rs`

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/bytecode/vm.rs`

All 7 sites are Category A (arithmetic results). Change each:
```rust
Value::Float(a + b)
```
to:
```rust
Value::Float(a + b, next_float_id())
```

Also update any `Value::Float(f)` destructure patterns to `Value::Float(f, _)`.

---

### Task 9: Update `eval.rs` — delete `eval_eq_call` and update Float sites

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/eval.rs`

**Step 1: Delete `eval_eq_call` (lines 2197-2234)**

Remove the entire function.

**Step 2: Remove call sites for `eval_eq_call`**

At line 2354-2356, remove:
```rust
if resolve_sym(*bound_name) == "eq" {
    return self.eval_eq_call(tail);
}
```

At line 2420-2422, remove:
```rust
if name == "eq" {
    return self.eval_eq_call(tail);
}
```

**Step 3: Update float literal evaluation (line 2048)**

Change:
```rust
Expr::Float(v) => Ok(Value::Float(*v)),
```
to:
```rust
Expr::Float(v) => Ok(Value::Float(*v, next_float_id())),
```

**Step 4: Update `quote_to_value` (line 5119)**

Change:
```rust
Expr::Float(v) => Value::Float(*v),
```
to:
```rust
Expr::Float(v) => Value::Float(*v, next_float_id()),
```

**Step 5: Update all literal constant initializations**

Lines 689, 982, 986: add `, next_float_id()`.

**Step 6: Update all remaining `Value::Float` patterns**

Add `, _` to any remaining read-only patterns.

---

### Task 10: Update all remaining files (batch)

**Files:**
- All other files with `Value::Float` patterns (see list below)

Use `cargo check -p neovm-core 2>&1 | grep "error\[E"` to find remaining sites. Update each:

- `builtins/tests.rs` (32 sites — all test code)
- `json.rs` (11 sites)
- `print.rs` (8 sites)
- `timefns.rs` (9 sites)
- `timer.rs` (9 sites)
- `window_cmds.rs` (8 sites)
- `kill_ring.rs` (5 sites)
- `builtins/buffers.rs` (5 sites)
- `format.rs` (3 sites)
- `fileio.rs` (3 sites)
- `editfns.rs` (3 sites)
- `interactive.rs` (3 sites)
- `builtins_extra.rs` (3 sites)
- `builtins/higher_order.rs` (3 sites)
- `builtins/strings.rs` (2 sites)
- `builtins/types.rs` (2 sites)
- `builtins/cons_list.rs` (2 sites)
- `builtins/misc_eval.rs` (2 sites)
- `builtins/stubs.rs` (2 sites)
- `builtins/mod.rs` (3 sites)
- `process.rs` (2 sites)
- `font.rs` (2 sites)
- `lread.rs` (2 sites)
- `rect.rs` (2 sites)
- `undo.rs` (2 sites)
- `bytecode/compiler.rs` (2 sites)
- Remaining 1-site files: `navigation.rs`, `reader.rs`, `charset.rs`,
  `compat_regressions.rs`, `xdisp.rs`, `cl_lib.rs`, `coding.rs`,
  `register.rs`, `builtins/symbols.rs`, `misc.rs`

Rule: creations get `next_float_id()`, patterns get `, _`.

---

### Task 11: Compile and fix any remaining errors

**Step 1: Full build check**

Run: `cargo check -p neovm-core 2>&1 | grep "error"`
Expected: zero errors.

**Step 2: Run full test suite**

Run: `cargo nextest run --release -p neovm-core 2>&1 | tail -5`
Expected: all tests pass.

**Step 3: Commit**

```
git add -A
git commit -m "feat(neovm-core): add float allocation ID for eq identity

Change Value::Float(f64) to Value::Float(f64, u32) where the u32
is a thread-local monotonic counter. eq_value now compares both
ID and bits, matching GNU Emacs pointer-identity semantics.

Delete eval_eq_call AST heuristic — no longer needed.
Value stays at 16 bytes (u32 fits in existing enum padding)."
```

---

### Task 12: Update oracle equality tests

**Files:**
- Modify: `rust/neovm-core/src/emacs_core/oracle/equality.rs`

**Step 1: Update existing neovm-only tests**

The neovm-only tests added in the previous fix should still pass
since they test correct behavior. Update any `Value::Float` patterns
if needed.

**Step 2: Add new tests for previously-divergent cases**

```rust
/// funcall 'eq on two float literals must return nil (neovm-only).
#[test]
fn neovm_eq_float_funcall_literals() {
    let neovm = run_neovm_eval(
        r#"(funcall 'eq 1.0 1.0)"#
    ).expect("neovm eval should run");
    assert_eq!(neovm.as_str(), "OK nil");
}

/// funcall 'eq on same variable must return t (neovm-only).
#[test]
fn neovm_eq_float_funcall_same_var() {
    let neovm = run_neovm_eval(
        r#"(let ((x 1.0)) (funcall 'eq x x))"#
    ).expect("neovm eval should run");
    assert_eq!(neovm.as_str(), "OK t");
}

/// Arithmetic results with same bits but different allocations
/// must not be eq (neovm-only).
#[test]
fn neovm_eq_float_arithmetic_distinct() {
    let neovm = run_neovm_eval(
        r#"(eq (+ 0.5 0.5) (- 2.0 1.0))"#
    ).expect("neovm eval should run");
    assert_eq!(neovm.as_str(), "OK nil");
}

/// memq must not find float by value (neovm-only).
#[test]
fn neovm_memq_float_not_found() {
    let neovm = run_neovm_eval(
        r#"(memq 1.0 '(1.0 2.0 3.0))"#
    ).expect("neovm eval should run");
    assert_eq!(neovm.as_str(), "OK nil");
}

/// memq must find float by identity (same variable) (neovm-only).
#[test]
fn neovm_memq_float_same_identity() {
    let neovm = run_neovm_eval(
        r#"(let ((x 1.0)) (memq x (list x 2.0 3.0)))"#
    ).expect("neovm eval should run");
    assert_eq!(neovm.as_str(), "OK (1.0 2.0 3.0)");
}
```

**Step 3: Run tests**

Run: `cargo nextest run --release -p neovm-core oracle::equality 2>&1 | tail -10`
Expected: all tests pass.

**Step 4: Commit**

```
git add rust/neovm-core/src/emacs_core/oracle/equality.rs
git commit -m "test(neovm-core): add float alloc ID eq/memq oracle tests"
```

---

### Task 13: Run loadup bootstrap test

**Step 1: Verify the loadup test still works**

Run: `NEOVM_LOADUP_TEST=1 cargo nextest run --release -p neovm-core neovm_loadup_bootstrap --nocapture 2>&1 | tail -10`

Expected: completes in <60s, no infinite loop in macroexpand.

**Step 2: Push**

```
git push
```
