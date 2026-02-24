# NeoVM GC Design: Arena-Owned Values with Controlled Safe Points

**Date**: 2026-02-23
**Status**: Proposal

## Problem

NeoVM's evaluator currently uses `Arc<Mutex<T>>` for all heap-allocated Lisp values
(cons cells, vectors, hash tables, closures). This has three critical problems:

1. **No cycle collection** — `Arc` leaks circular structures (common in Elisp via
   `setcar`/`setcdr`)
2. **High overhead** — Every cons cell is `Arc<Mutex<ConsCell>>`: 16 bytes of reference
   count + a mutex + heap allocation. Every clone bumps an atomic counter.
3. **Deep cloning** — Closures clone entire lexical environments
   (`self.lexenv.clone()`), environment lookups clone values.

The GC module (`rust/neovm-core/src/gc/`) exists but isn't integrated. It implements
Cheney's semi-space copying collector with opaque `GcRef(u64)` handles and a
`HashMap<u64, usize>` for indirection — the HashMap lookup on every object access would
be unacceptably slow.

## Survey of Existing Approaches

### Rune (CeleritasCelery/rune)

Lifetime-branded `Gc<'cx, T>` pointers. GC requires `&mut Context`, allocation returns
references tied to `&Context`. Borrow checker statically proves no live references exist
during collection. `root!` macro for values surviving GC points.

**Pros**: Compile-time safety, zero-cost pointers.
**Cons**: `'gc` lifetime propagates through every function signature. The piccolo author
(gc-arena) abandoned his Lua VM for 4 years due to unsolvable problems with this approach.

### gc-arena (kyren/gc-arena)

Arena-scoped mutation with HRTB lifetime branding. All operations inside
`arena.mutate(|mc, root| { ... })` callbacks. Incremental mark-and-sweep (Lua 5.4 style).
Used in production by Ruffle (Flash emulator).

**Pros**: Proven incremental GC, production-tested.
**Cons**: Callback-based API, values can't escape arena, HRTB syntax unfamiliar.

### shifgrethor (withoutboats)

Pin-based rooting. `Root<'root>` lifetime parameter propagates through all GC'd types.
Eliminates context parameter passing.

**Cons**: Manual rooting, lifetime propagation.

### Alloy (2025 OOPSLA paper)

Modified rustc with conservative GC (BDWGC). Automatic finalizer reuse from Rust
destructors. Compiler-integrated finalizer safety analysis.

**Pros**: Zero API burden, automatic finalizers.
**Cons**: Requires forked compiler, conservative scanning.

### The Common Problem

All approaches assume the GC is a **library defending itself against arbitrary Rust code**.
They use Rust's type system to build walls (lifetimes, callbacks, HRTB) — and those walls
impose massive API costs on 18,000+ Value callsites.

## Proposed Design: Arena-Owned Values with Controlled Safe Points

### Key Insight

NeoVM controls the **entire execution environment**. The evaluator is the only thing that
allocates, mutates, and determines when collection runs. We don't need the borrow checker
to enforce GC safety at every callsite — we can enforce it structurally.

### Value Representation

```rust
// Value is a small, Copy-able tagged union. No lifetimes. No Arc.
#[derive(Clone, Copy, PartialEq)]
enum Value {
    Nil,
    True,
    Int(i64),
    Float(f64),
    Char(char),
    // Heap types are indices into the arena
    Cons(ObjId),
    Str(ObjId),
    Symbol(ObjId),
    Vector(ObjId),
    HashTable(ObjId),
    Lambda(ObjId),
    // ...
}

// 8 bytes. Copy. No reference counting. No lifetime.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ObjId(u32, u32);  // (index, generation)
```

### Arena (Heap)

```rust
struct LispHeap {
    // Object storage — contiguous, cache-friendly
    objects: Vec<HeapObject>,
    // Free list for reuse after sweep
    free_list: Vec<u32>,
    // Generation counter (incremented each GC cycle)
    generation: u32,
    // Incremental mark state (tri-color)
    mark_state: MarkState,
}
```

### Evaluator as Root Set

```rust
struct Evaluator {
    heap: LispHeap,
    // All of these are scanned as roots:
    dynamic: Vec<HashMap<String, Value>>,
    lexenv: Vec<HashMap<String, Value>>,
    obarray: Obarray,
    bytecode_stack: Vec<Value>,
    // ...

    fn alloc_cons(&mut self, car: Value, cdr: Value) -> Value {
        let id = self.heap.alloc(HeapObject::Cons(car, cdr));
        Value::Cons(id)
    }

    fn cons_car(&self, id: ObjId) -> Value {
        match self.heap.get(id) {
            HeapObject::Cons(car, _) => *car,
            _ => panic!("type error"),
        }
    }

    fn set_car(&mut self, id: ObjId, val: Value) {
        match self.heap.get_mut(id) {
            HeapObject::Cons(car, _) => {
                *car = val;
                self.heap.write_barrier(id); // for incremental GC
            }
            _ => panic!("type error"),
        }
    }

    fn gc_collect(&mut self) {
        // &mut self guarantees no other code is running
        self.heap.collect(self.root_iter());
    }
}
```

### Collection Algorithm

Incremental mark-and-sweep (same as gc-arena / Lua 5.4):

1. **Mark phase** (incremental): Tri-color marking. Start from roots (Evaluator fields),
   mark reachable objects. Spread across multiple allocation cycles to keep pauses small.
2. **Write barriers**: On `setcar`/`setcdr`/`aset`/`puthash` — mark modified objects as
   gray (needs re-scanning). Small, known set of mutation points.
3. **Sweep phase**: Walk object table, free unmarked objects to free list.
4. **Bump generation**: Increment generation counter so stale ObjIds are detectable.

### Safe Points

GC runs at controlled points chosen by the evaluator:
- Between top-level form evaluations
- At bytecode VM yield points (every N opcodes)
- When allocation pressure exceeds threshold

No safe-point annotation needed in user code — the evaluator decides.

## Comparison with Rune

| Aspect | Rune | Proposed |
|--------|------|----------|
| Pointer type | `Gc<'gc, T>` (lifetime-branded) | `ObjId` (generation-checked index) |
| Safety enforcement | Compile-time (borrow checker) | Structural (`&mut self`) + runtime (generation) |
| API burden | `'gc` on every function | None — `Value` is `Copy` |
| Root management | `root!` macro at every safe point | One `root_iter()` function |
| Migration cost | Rewrite every function signature | Mechanical find-and-replace |
| Unsafe surface | Small (GC internals) | Small (heap module, ~500 lines) |
| Collection | Stop-the-world mark-and-sweep | Incremental mark-and-sweep |
| Interior mutability | GC-specific Lock/RefLock types | Direct `get_mut()` (single-threaded) |
| Allocation | Direct pointer (zero-cost) | Index lookup (bounds check) |

## Safety Argument

**Rune's guarantee**: "You can't hold a GC reference during collection" — proven at every
callsite via lifetimes.

**Our guarantee**: "`gc_collect()` takes `&mut Evaluator`, so no other code is executing
when GC runs" — proven once, structurally.

### Defense Against Root Enumeration Bugs

The remaining risk: did `root_iter()` enumerate all roots correctly? Defenses:

1. **Generation counters**: `ObjId(index, generation)` — accessing a collected object
   panics immediately. Catches 100% of use-after-collected bugs at runtime.
2. **Oracle fuzzing**: 1,800+ compatibility tests run identical programs in NeoVM and GNU
   Emacs. Any GC bug affecting observable behavior is caught.
3. **Single audit point**: Root enumeration is ONE function, not 18,000 callsites with
   lifetime annotations.

### Contained Unsafety

Unsafe code is limited to the heap module (~500 lines). The 18,000+ lines of evaluator
code use only safe abstractions: `Value`, `ObjId`, `evaluator.alloc_cons()`,
`evaluator.cons_car()`.

## Migration Path

Each step is mechanical, no structural changes:

1. Replace `Arc<Mutex<ConsCell>>` → `ObjId` in the `Value` enum
2. Add `alloc_cons()`, `cons_car()`, `cons_set_car()` methods to Evaluator
3. Find-and-replace `cell.lock().unwrap().car.clone()` → `evaluator.cons_car(id)`
4. Remove all `Arc::new(Mutex::new(...))` → `evaluator.alloc_cons(...)`
5. Write `root_iter()` — enumerate all Value-holding fields in Evaluator
6. Add `gc_collect()` calls at safe points
7. Implement incremental mark-and-sweep in LispHeap
8. Add write barriers to mutation primitives (`setcar`, `setcdr`, `aset`, `puthash`)

## Performance Expectations

- **Allocation**: Bump pointer or free-list reuse vs current `Arc::new(Mutex::new(...))`
  heap allocation — order of magnitude faster
- **Access**: Bounds-checked index lookup vs current `HashMap` or `Arc` deref — comparable
  or faster (no atomic operations)
- **Clone/Copy**: 8-byte memcpy vs current atomic increment — much faster
- **GC pauses**: Incremental marking keeps pauses proportional to mutation rate, not heap
  size — suitable for interactive editor
- **Memory**: No reference counts, no mutex, no Arc overhead — roughly 50% less memory per
  cons cell

## References

- [Rune: Rust VM for Emacs](https://github.com/CeleritasCelery/rune)
- [gc-arena: Incremental GC from safe Rust](https://github.com/kyren/gc-arena)
- [Techniques for Safe GC in Rust](https://kyju.org/blog/rust-safe-garbage-collection/)
- [A Tour of Safe Tracing GC Designs in Rust](https://manishearth.github.io/blog/2021/04/05/a-tour-of-safe-tracing-gc-designs-in-rust/)
- [Piccolo: A Stackless Lua Interpreter](https://kyju.org/blog/piccolo-a-stackless-lua-interpreter/)
- [Alloy: GC for Rust — The Finalizer Frontier (OOPSLA 2025)](https://soft-dev.org/pubs/html/hughes_tratt__garbage_collection_for_rust_the_finalizer_frontier/)
- [Safe GC in Rust (Rune blog)](https://coredumped.dev/2022/04/11/implementing-a-safe-garbage-collector-in-rust/)
- [Design of Emacs in Rust (Rune blog)](https://coredumped.dev/2023/01/17/design-of-emacs-in-rust/)
