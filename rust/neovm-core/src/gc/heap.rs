//! Arena-based heap with mark-and-sweep collection.

use super::types::{HeapObject, ObjId};
use crate::elisp::value::{HashTableTest, LispHashTable, Value};

/// The managed heap for cycle-forming Lisp objects.
pub struct LispHeap {
    objects: Vec<HeapObject>,
    generations: Vec<u32>,
    marks: Vec<bool>,
    free_list: Vec<u32>,
    allocated_count: usize,
    gc_threshold: usize,
}

impl LispHeap {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            generations: Vec::new(),
            marks: Vec::new(),
            free_list: Vec::new(),
            allocated_count: 0,
            gc_threshold: 8192,
        }
    }

    // -----------------------------------------------------------------------
    // Allocation
    // -----------------------------------------------------------------------

    fn alloc(&mut self, obj: HeapObject) -> ObjId {
        self.allocated_count += 1;
        if let Some(idx) = self.free_list.pop() {
            let i = idx as usize;
            self.generations[i] = self.generations[i].wrapping_add(1);
            self.objects[i] = obj;
            ObjId {
                index: idx,
                generation: self.generations[i],
            }
        } else {
            let idx = self.objects.len() as u32;
            self.objects.push(obj);
            self.generations.push(0);
            self.marks.push(false);
            ObjId {
                index: idx,
                generation: 0,
            }
        }
    }

    pub fn alloc_cons(&mut self, car: Value, cdr: Value) -> ObjId {
        self.alloc(HeapObject::Cons { car, cdr })
    }

    pub fn alloc_vector(&mut self, items: Vec<Value>) -> ObjId {
        self.alloc(HeapObject::Vector(items))
    }

    pub fn alloc_hash_table(&mut self, test: HashTableTest) -> ObjId {
        self.alloc(HeapObject::HashTable(LispHashTable::new(test)))
    }

    pub fn alloc_hash_table_with_options(
        &mut self,
        test: HashTableTest,
        size: i64,
        weakness: Option<crate::elisp::value::HashTableWeakness>,
        rehash_size: f64,
        rehash_threshold: f64,
    ) -> ObjId {
        self.alloc(HeapObject::HashTable(LispHashTable::new_with_options(
            test,
            size,
            weakness,
            rehash_size,
            rehash_threshold,
        )))
    }

    pub fn alloc_hash_table_raw(&mut self, ht: LispHashTable) -> ObjId {
        self.alloc(HeapObject::HashTable(ht))
    }

    /// Current allocation threshold used by opportunistic GC call sites.
    pub fn gc_threshold(&self) -> usize {
        self.gc_threshold
    }

    /// Update the allocation threshold used by opportunistic GC call sites.
    /// Clamp to 1 so callers never disable threshold checks with zero.
    pub fn set_gc_threshold(&mut self, threshold: usize) {
        self.gc_threshold = threshold.max(1);
    }

    /// True when allocated objects reached the configured threshold.
    pub fn should_collect(&self) -> bool {
        self.allocated_count >= self.gc_threshold
    }

    // -----------------------------------------------------------------------
    // Checked access
    // -----------------------------------------------------------------------

    #[inline]
    fn check(&self, id: ObjId) {
        let i = id.index as usize;
        assert!(
            i < self.objects.len() && self.generations[i] == id.generation,
            "stale ObjId: {:?} (current gen={})",
            id,
            if i < self.generations.len() {
                self.generations[i]
            } else {
                u32::MAX
            }
        );
    }

    pub fn get(&self, id: ObjId) -> &HeapObject {
        self.check(id);
        &self.objects[id.index as usize]
    }

    pub fn get_mut(&mut self, id: ObjId) -> &mut HeapObject {
        self.check(id);
        &mut self.objects[id.index as usize]
    }

    // -----------------------------------------------------------------------
    // Cons accessors
    // -----------------------------------------------------------------------

    pub fn cons_car(&self, id: ObjId) -> Value {
        match self.get(id) {
            HeapObject::Cons { car, .. } => car.clone(),
            _ => panic!("cons_car on non-cons"),
        }
    }

    pub fn cons_cdr(&self, id: ObjId) -> Value {
        match self.get(id) {
            HeapObject::Cons { cdr, .. } => cdr.clone(),
            _ => panic!("cons_cdr on non-cons"),
        }
    }

    pub fn set_car(&mut self, id: ObjId, val: Value) {
        match self.get_mut(id) {
            HeapObject::Cons { car, .. } => *car = val,
            _ => panic!("set_car on non-cons"),
        }
    }

    pub fn set_cdr(&mut self, id: ObjId, val: Value) {
        match self.get_mut(id) {
            HeapObject::Cons { cdr, .. } => *cdr = val,
            _ => panic!("set_cdr on non-cons"),
        }
    }

    // -----------------------------------------------------------------------
    // Vector accessors
    // -----------------------------------------------------------------------

    pub fn vector_ref(&self, id: ObjId, index: usize) -> Value {
        match self.get(id) {
            HeapObject::Vector(v) => v[index].clone(),
            _ => panic!("vector_ref on non-vector"),
        }
    }

    pub fn vector_set(&mut self, id: ObjId, index: usize, val: Value) {
        match self.get_mut(id) {
            HeapObject::Vector(v) => v[index] = val,
            _ => panic!("vector_set on non-vector"),
        }
    }

    pub fn vector_len(&self, id: ObjId) -> usize {
        match self.get(id) {
            HeapObject::Vector(v) => v.len(),
            _ => panic!("vector_len on non-vector"),
        }
    }

    pub fn get_vector(&self, id: ObjId) -> &Vec<Value> {
        match self.get(id) {
            HeapObject::Vector(v) => v,
            _ => panic!("get_vector on non-vector"),
        }
    }

    pub fn get_vector_mut(&mut self, id: ObjId) -> &mut Vec<Value> {
        match self.get_mut(id) {
            HeapObject::Vector(v) => v,
            _ => panic!("get_vector_mut on non-vector"),
        }
    }

    // -----------------------------------------------------------------------
    // HashTable accessors
    // -----------------------------------------------------------------------

    pub fn get_hash_table(&self, id: ObjId) -> &LispHashTable {
        match self.get(id) {
            HeapObject::HashTable(ht) => ht,
            _ => panic!("get_hash_table on non-hash-table"),
        }
    }

    pub fn get_hash_table_mut(&mut self, id: ObjId) -> &mut LispHashTable {
        match self.get_mut(id) {
            HeapObject::HashTable(ht) => ht,
            _ => panic!("get_hash_table_mut on non-hash-table"),
        }
    }

    // -----------------------------------------------------------------------
    // List helpers
    // -----------------------------------------------------------------------

    pub fn list_to_vec(&self, value: &Value) -> Option<Vec<Value>> {
        let mut result = Vec::new();
        let mut cursor = value.clone();
        loop {
            match cursor {
                Value::Nil => return Some(result),
                Value::Cons(id) => {
                    result.push(self.cons_car(id));
                    cursor = self.cons_cdr(id);
                }
                _ => return None,
            }
        }
    }

    pub fn list_length(&self, value: &Value) -> Option<usize> {
        let mut len = 0;
        let mut cursor = value.clone();
        loop {
            match cursor {
                Value::Nil => return Some(len),
                Value::Cons(id) => {
                    len += 1;
                    cursor = self.cons_cdr(id);
                }
                _ => return None,
            }
        }
    }

    // -----------------------------------------------------------------------
    // Structural equality
    // -----------------------------------------------------------------------

    pub fn equal_value(&self, a: &Value, b: &Value, depth: usize) -> bool {
        if depth > 4096 {
            return false;
        }
        match (a, b) {
            (Value::Cons(ai), Value::Cons(bi)) => {
                if ai == bi {
                    return true;
                }
                let a_car = self.cons_car(*ai);
                let a_cdr = self.cons_cdr(*ai);
                let b_car = self.cons_car(*bi);
                let b_cdr = self.cons_cdr(*bi);
                self.equal_value(&a_car, &b_car, depth + 1)
                    && self.equal_value(&a_cdr, &b_cdr, depth + 1)
            }
            (Value::Vector(ai), Value::Vector(bi)) => {
                if ai == bi {
                    return true;
                }
                let av = self.get_vector(*ai);
                let bv = self.get_vector(*bi);
                av.len() == bv.len()
                    && av
                        .iter()
                        .zip(bv.iter())
                        .all(|(x, y)| self.equal_value(x, y, depth + 1))
            }
            _ => crate::elisp::value::equal_value(a, b, depth),
        }
    }

    // -----------------------------------------------------------------------
    // Mark-and-sweep collection
    // -----------------------------------------------------------------------

    /// Collect garbage. `roots` must yield every Value that is reachable.
    pub fn collect(&mut self, roots: impl Iterator<Item = Value>) {
        // Clear marks
        for m in self.marks.iter_mut() {
            *m = false;
        }
        // Ensure marks vec covers all objects
        self.marks.resize(self.objects.len(), false);

        // Mark phase — iterative worklist
        let mut worklist: Vec<ObjId> = Vec::new();
        for root in roots {
            Self::push_value_ids(&root, &mut worklist);
            // Also trace Arc-held values (Lambda/Macro/ByteCode captured envs)
            Self::trace_arc_values(&root, &mut worklist);
        }

        while let Some(id) = worklist.pop() {
            let i = id.index as usize;
            if i >= self.marks.len() || self.marks[i] {
                continue;
            }
            if self.generations[i] != id.generation {
                continue; // stale
            }
            self.marks[i] = true;
            // Trace children
            match &self.objects[i] {
                HeapObject::Cons { car, cdr } => {
                    Self::push_value_ids(car, &mut worklist);
                    Self::push_value_ids(cdr, &mut worklist);
                    Self::trace_arc_values(car, &mut worklist);
                    Self::trace_arc_values(cdr, &mut worklist);
                }
                HeapObject::Vector(v) => {
                    for val in v {
                        Self::push_value_ids(val, &mut worklist);
                        Self::trace_arc_values(val, &mut worklist);
                    }
                }
                HeapObject::HashTable(ht) => {
                    for v in ht.data.values() {
                        Self::push_value_ids(v, &mut worklist);
                        Self::trace_arc_values(v, &mut worklist);
                    }
                    for v in ht.key_snapshots.values() {
                        Self::push_value_ids(v, &mut worklist);
                        Self::trace_arc_values(v, &mut worklist);
                    }
                }
                HeapObject::Free => {}
            }
        }

        // Sweep phase
        for i in 0..self.objects.len() {
            if !self.marks[i] {
                if !matches!(self.objects[i], HeapObject::Free) {
                    self.objects[i] = HeapObject::Free;
                    self.generations[i] = self.generations[i].wrapping_add(1);
                    self.free_list.push(i as u32);
                    self.allocated_count = self.allocated_count.saturating_sub(1);
                }
            }
        }

        // Adapt threshold: next GC triggers at 2x surviving objects, minimum 8192
        self.gc_threshold = self.allocated_count.saturating_mul(2).max(8192);
    }

    fn push_value_ids(val: &Value, worklist: &mut Vec<ObjId>) {
        match val {
            Value::Cons(id) | Value::Vector(id) | Value::HashTable(id) => worklist.push(*id),
            _ => {}
        }
    }

    fn trace_arc_values(val: &Value, worklist: &mut Vec<ObjId>) {
        match val {
            Value::Lambda(data) | Value::Macro(data) => {
                if let Some(env) = &data.env {
                    for scope in env {
                        for v in scope.values() {
                            Self::push_value_ids(v, worklist);
                            Self::trace_arc_values(v, worklist);
                        }
                    }
                }
            }
            Value::ByteCode(bc) => {
                for c in &bc.constants {
                    Self::push_value_ids(c, worklist);
                    Self::trace_arc_values(c, worklist);
                }
            }
            _ => {}
        }
    }

    // -----------------------------------------------------------------------
    // Stats
    // -----------------------------------------------------------------------

    pub fn allocated_count(&self) -> usize {
        self.allocated_count
    }
}

impl Default for LispHeap {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_cons_read() {
        let mut heap = LispHeap::new();
        let id = heap.alloc_cons(Value::Int(1), Value::Int(2));
        assert_eq!(heap.cons_car(id), Value::Int(1));
        assert_eq!(heap.cons_cdr(id), Value::Int(2));
    }

    #[test]
    fn alloc_cons_mutate() {
        let mut heap = LispHeap::new();
        let id = heap.alloc_cons(Value::Int(1), Value::Int(2));
        heap.set_car(id, Value::Int(10));
        assert_eq!(heap.cons_car(id), Value::Int(10));
    }

    #[test]
    fn free_list_reuse() {
        let mut heap = LispHeap::new();
        let id1 = heap.alloc_cons(Value::Nil, Value::Nil);
        let idx = id1.index;
        // Simulate free by collecting with no roots
        heap.collect(std::iter::empty());
        // Next alloc should reuse the slot
        let id2 = heap.alloc_cons(Value::Int(42), Value::Nil);
        assert_eq!(id2.index, idx);
        assert_ne!(id2.generation, id1.generation);
    }

    #[test]
    #[should_panic(expected = "stale ObjId")]
    fn stale_id_panics() {
        let mut heap = LispHeap::new();
        let id = heap.alloc_cons(Value::Nil, Value::Nil);
        heap.collect(std::iter::empty());
        let _ = heap.cons_car(id); // should panic — stale
    }

    #[test]
    fn collect_unreachable() {
        let mut heap = LispHeap::new();
        let _a = heap.alloc_cons(Value::Int(1), Value::Nil);
        let b = heap.alloc_cons(Value::Int(2), Value::Nil);
        assert_eq!(heap.allocated_count(), 2);
        // Only b is a root
        heap.collect([Value::Cons(b)].into_iter());
        assert_eq!(heap.allocated_count(), 1);
        assert_eq!(heap.cons_car(b), Value::Int(2));
    }

    #[test]
    fn collect_nested() {
        let mut heap = LispHeap::new();
        let inner = heap.alloc_cons(Value::Int(1), Value::Nil);
        let outer = heap.alloc_cons(Value::Cons(inner), Value::Nil);
        heap.collect([Value::Cons(outer)].into_iter());
        assert_eq!(heap.allocated_count(), 2);
        // inner is reachable through outer
        assert_eq!(heap.cons_car(inner), Value::Int(1));
    }

    #[test]
    fn collect_cycle() {
        let mut heap = LispHeap::new();
        let a = heap.alloc_cons(Value::Int(1), Value::Nil);
        let b = heap.alloc_cons(Value::Int(2), Value::Cons(a));
        heap.set_cdr(a, Value::Cons(b)); // create cycle a <-> b

        // Both reachable from a
        heap.collect([Value::Cons(a)].into_iter());
        assert_eq!(heap.allocated_count(), 2);

        // Remove root — both should be collected
        heap.collect(std::iter::empty());
        assert_eq!(heap.allocated_count(), 0);
    }

    #[test]
    fn vector_ops() {
        let mut heap = LispHeap::new();
        let id = heap.alloc_vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(heap.vector_len(id), 3);
        assert_eq!(heap.vector_ref(id, 1), Value::Int(2));
        heap.vector_set(id, 1, Value::Int(20));
        assert_eq!(heap.vector_ref(id, 1), Value::Int(20));
    }

    #[test]
    fn list_helpers() {
        let mut heap = LispHeap::new();
        let c3 = heap.alloc_cons(Value::Int(3), Value::Nil);
        let c2 = heap.alloc_cons(Value::Int(2), Value::Cons(c3));
        let c1 = heap.alloc_cons(Value::Int(1), Value::Cons(c2));
        let list = Value::Cons(c1);

        let vec = heap.list_to_vec(&list).unwrap();
        assert_eq!(vec, vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(heap.list_length(&list), Some(3));
    }

    #[test]
    fn structural_equality() {
        let mut heap = LispHeap::new();
        let a = heap.alloc_cons(Value::Int(1), Value::Int(2));
        let b = heap.alloc_cons(Value::Int(1), Value::Int(2));
        assert!(heap.equal_value(&Value::Cons(a), &Value::Cons(b), 0));
        let c = heap.alloc_cons(Value::Int(1), Value::Int(3));
        assert!(!heap.equal_value(
            &Value::Cons(a),
            &Value::Cons(c),
            0
        ));
    }

    #[test]
    fn hash_table_basic() {
        let mut heap = LispHeap::new();
        let id = heap.alloc_hash_table(HashTableTest::Equal);
        let ht = heap.get_hash_table(id);
        assert_eq!(ht.data.len(), 0);
    }

    #[test]
    fn gc_threshold_is_configurable_and_clamped() {
        let mut heap = LispHeap::new();
        assert_eq!(heap.gc_threshold(), 8192);
        heap.set_gc_threshold(0);
        assert_eq!(heap.gc_threshold(), 1);
        heap.set_gc_threshold(64);
        assert_eq!(heap.gc_threshold(), 64);
    }

    #[test]
    fn should_collect_tracks_allocations_against_threshold() {
        let mut heap = LispHeap::new();
        heap.set_gc_threshold(2);
        assert!(!heap.should_collect());
        let _ = heap.alloc_cons(Value::Int(1), Value::Nil);
        assert!(!heap.should_collect());
        let _ = heap.alloc_cons(Value::Int(2), Value::Nil);
        assert!(heap.should_collect());
    }
}
