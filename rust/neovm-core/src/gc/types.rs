//! GC heap object types and handles.

use crate::elisp::value::{LispHashTable, Value};

/// Handle to a heap-allocated object.  Copy-able, 8 bytes.
///
/// `index` selects the slot in `LispHeap::objects`.
/// `generation` detects use-after-free (stale handles panic on access).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjId {
    pub(crate) index: u32,
    pub(crate) generation: u32,
}

impl std::fmt::Debug for ObjId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ObjId({}/{})", self.index, self.generation)
    }
}

/// The concrete object stored on the managed heap.
///
/// Only cycle-forming types live here: cons cells, vectors, and hash tables.
/// Strings, lambdas, macros, and bytecode stay as `Arc` (they can't form
/// reference cycles on their own).
pub enum HeapObject {
    Cons { car: Value, cdr: Value },
    Vector(Vec<Value>),
    HashTable(LispHashTable),
    /// Freed slot, available for reuse.
    Free,
}

impl HeapObject {
    /// Iterate over all `Value` references contained in this object (for GC marking).
    pub fn trace_values(&self) -> Box<dyn Iterator<Item = &Value> + '_> {
        match self {
            HeapObject::Cons { car, cdr } => Box::new([car, cdr].into_iter()),
            HeapObject::Vector(v) => Box::new(v.iter()),
            HeapObject::HashTable(ht) => {
                Box::new(ht.data.values().chain(ht.key_snapshots.values()))
            }
            HeapObject::Free => Box::new(std::iter::empty()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn objid_copy_eq_hash() {
        let a = ObjId {
            index: 1,
            generation: 0,
        };
        let b = a; // Copy
        assert_eq!(a, b);

        use std::collections::HashSet;
        let mut set = HashSet::new();
        set.insert(a);
        assert!(set.contains(&b));
    }
}
