//! Garbage Collector for the NeoVM Elisp runtime.
//!
//! # Architecture
//!
//! Arena-based mark-and-sweep collector:
//!
//! - **LispHeap**: Arena that owns all cycle-forming objects (cons, vector, hash-table).
//! - **ObjId**: Lightweight 8-byte handle (index + generation) replacing `Arc<Mutex<T>>`.
//! - **Thread-local access**: The evaluator sets a thread-local pointer before evaluation;
//!   `Value` constructors and accessors use it transparently.
//! - **Mark-and-sweep**: Iterative worklist marking from root set, sweep frees unmarked objects.
//! - **Generation counters**: Catch use-after-collected bugs at runtime (stale ObjId panics).

pub mod heap;
pub mod types;

pub use heap::LispHeap;
pub use types::{HeapObject, ObjId};
