//! GC heap object types and handles.

use crate::elisp::bytecode::ByteCodeFunction;
use crate::elisp::value::{LambdaData, LispHashTable, Value};

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
/// All heap-allocated Lisp types live here: cons cells, vectors, hash tables,
/// strings, lambdas, macros, and bytecode functions.
pub enum HeapObject {
    Cons { car: Value, cdr: Value },
    Vector(Vec<Value>),
    HashTable(LispHashTable),
    Str(String),
    Lambda(LambdaData),
    Macro(LambdaData),
    ByteCode(ByteCodeFunction),
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
            HeapObject::Str(_) => Box::new(std::iter::empty()),
            HeapObject::Lambda(d) | HeapObject::Macro(d) => {
                Box::new(d.env.iter().flat_map(|env| env.iter().flat_map(|scope| scope.values())))
            }
            HeapObject::ByteCode(bc) => {
                let constants = bc.constants.iter();
                let env_vals = bc.env.iter().flat_map(|env| env.iter().flat_map(|scope| scope.values()));
                Box::new(constants.chain(env_vals))
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
