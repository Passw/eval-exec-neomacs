//! Arena-based heap with incremental tri-color mark-and-sweep collection.

use super::types::{HeapObject, ObjId};
use crate::elisp::bytecode::ByteCodeFunction;
use crate::elisp::value::{HashTableTest, LambdaData, LispHashTable, Value};

/// GC collection phase (tri-color incremental).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum GcPhase {
    /// No collection in progress.
    Idle,
    /// Incremental mark phase — processing the gray worklist.
    Marking,
    /// Sweep phase — freeing unmarked objects.
    Sweeping,
}

/// The managed heap for cycle-forming Lisp objects.
pub struct LispHeap {
    objects: Vec<HeapObject>,
    generations: Vec<u32>,
    /// Tri-color mark bits: `true` = black (fully scanned), `false` = white.
    marks: Vec<bool>,
    free_list: Vec<u32>,
    allocated_count: usize,
    gc_threshold: usize,
    /// Current GC phase for incremental collection.
    gc_phase: GcPhase,
    /// Gray worklist — objects marked but whose children haven't been scanned.
    gray_queue: Vec<ObjId>,
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
            gc_phase: GcPhase::Idle,
            gray_queue: Vec::new(),
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
            self.marks[i] = false;
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

    pub fn alloc_string(&mut self, s: String) -> ObjId {
        self.alloc(HeapObject::Str(s))
    }

    pub fn alloc_lambda(&mut self, data: LambdaData) -> ObjId {
        self.alloc(HeapObject::Lambda(data))
    }

    pub fn alloc_macro(&mut self, data: LambdaData) -> ObjId {
        self.alloc(HeapObject::Macro(data))
    }

    pub fn alloc_bytecode(&mut self, bc: ByteCodeFunction) -> ObjId {
        self.alloc(HeapObject::ByteCode(bc))
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

    /// True when an incremental marking cycle is in progress.
    pub fn is_marking(&self) -> bool {
        self.gc_phase == GcPhase::Marking
    }

    /// Begin an incremental marking cycle: clear marks, seed gray queue from
    /// roots, and set the phase to `Marking`.  Does NOT drain the queue.
    pub fn begin_marking(&mut self, roots: impl Iterator<Item = Value>) {
        self.gc_phase = GcPhase::Marking;
        for m in self.marks.iter_mut() {
            *m = false;
        }
        self.marks.resize(self.objects.len(), false);
        self.gray_queue.clear();
        for root in roots {
            Self::push_value_ids(&root, &mut self.gray_queue);
        }
    }

    /// Finish an incremental collection cycle: sweep unmarked objects,
    /// adapt the threshold, and return to `Idle`.
    pub fn finish_collection(&mut self) {
        self.gc_phase = GcPhase::Sweeping;
        self.sweep_all();
        self.gc_phase = GcPhase::Idle;
        self.gc_threshold = self.allocated_count.saturating_mul(2).max(8192);
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
    // Write barrier
    // -----------------------------------------------------------------------

    /// Barrier-back write barrier: if `id` is black (marked) during the
    /// marking phase, push it back to gray so its new children get scanned.
    #[inline]
    fn write_barrier(&mut self, id: ObjId) {
        if self.gc_phase == GcPhase::Marking {
            let i = id.index as usize;
            if i < self.marks.len() && self.marks[i] {
                // Push back to gray — will be re-scanned.
                self.marks[i] = false;
                self.gray_queue.push(id);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Cons accessors
    // -----------------------------------------------------------------------

    pub fn cons_car(&self, id: ObjId) -> Value {
        match self.get(id) {
            HeapObject::Cons { car, .. } => *car,
            _ => panic!("cons_car on non-cons"),
        }
    }

    pub fn cons_cdr(&self, id: ObjId) -> Value {
        match self.get(id) {
            HeapObject::Cons { cdr, .. } => *cdr,
            _ => panic!("cons_cdr on non-cons"),
        }
    }

    pub fn set_car(&mut self, id: ObjId, val: Value) {
        self.write_barrier(id);
        match self.get_mut(id) {
            HeapObject::Cons { car, .. } => *car = val,
            _ => panic!("set_car on non-cons"),
        }
    }

    pub fn set_cdr(&mut self, id: ObjId, val: Value) {
        self.write_barrier(id);
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
            HeapObject::Vector(v) => v[index],
            _ => panic!("vector_ref on non-vector"),
        }
    }

    pub fn vector_set(&mut self, id: ObjId, index: usize, val: Value) {
        self.write_barrier(id);
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
        self.write_barrier(id);
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
        self.write_barrier(id);
        match self.get_mut(id) {
            HeapObject::HashTable(ht) => ht,
            _ => panic!("get_hash_table_mut on non-hash-table"),
        }
    }

    // -----------------------------------------------------------------------
    // String accessors
    // -----------------------------------------------------------------------

    pub fn get_string(&self, id: ObjId) -> &String {
        match self.get(id) {
            HeapObject::Str(s) => s,
            _ => panic!("get_string on non-string"),
        }
    }

    pub fn get_string_mut(&mut self, id: ObjId) -> &mut String {
        self.write_barrier(id);
        match self.get_mut(id) {
            HeapObject::Str(s) => s,
            _ => panic!("get_string_mut on non-string"),
        }
    }

    // -----------------------------------------------------------------------
    // Lambda / Macro accessors
    // -----------------------------------------------------------------------

    pub fn get_lambda(&self, id: ObjId) -> &LambdaData {
        match self.get(id) {
            HeapObject::Lambda(d) => d,
            _ => panic!("get_lambda on non-lambda"),
        }
    }

    pub fn get_lambda_mut(&mut self, id: ObjId) -> &mut LambdaData {
        self.write_barrier(id);
        match self.get_mut(id) {
            HeapObject::Lambda(d) => d,
            _ => panic!("get_lambda_mut on non-lambda"),
        }
    }

    pub fn get_macro_data(&self, id: ObjId) -> &LambdaData {
        match self.get(id) {
            HeapObject::Macro(d) => d,
            _ => panic!("get_macro_data on non-macro"),
        }
    }

    pub fn get_macro_data_mut(&mut self, id: ObjId) -> &mut LambdaData {
        self.write_barrier(id);
        match self.get_mut(id) {
            HeapObject::Macro(d) => d,
            _ => panic!("get_macro_data_mut on non-macro"),
        }
    }

    // -----------------------------------------------------------------------
    // ByteCode accessors
    // -----------------------------------------------------------------------

    pub fn get_bytecode(&self, id: ObjId) -> &ByteCodeFunction {
        match self.get(id) {
            HeapObject::ByteCode(bc) => bc,
            _ => panic!("get_bytecode on non-bytecode"),
        }
    }

    pub fn get_bytecode_mut(&mut self, id: ObjId) -> &mut ByteCodeFunction {
        self.write_barrier(id);
        match self.get_mut(id) {
            HeapObject::ByteCode(bc) => bc,
            _ => panic!("get_bytecode_mut on non-bytecode"),
        }
    }

    // -----------------------------------------------------------------------
    // List helpers
    // -----------------------------------------------------------------------

    pub fn list_to_vec(&self, value: &Value) -> Option<Vec<Value>> {
        let mut result = Vec::new();
        let mut cursor = *value;
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
        let mut cursor = *value;
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
    // Incremental tri-color mark-and-sweep collection
    // -----------------------------------------------------------------------

    /// Collect garbage. `roots` must yield every Value that is reachable.
    ///
    /// Runs a complete mark-and-sweep cycle (mark all, then sweep all).
    /// Write barriers protect against mutations during future incremental
    /// collection where marking is interleaved with mutator execution.
    pub fn collect(&mut self, roots: impl Iterator<Item = Value>) {
        // -- Begin mark phase --
        self.gc_phase = GcPhase::Marking;

        // Clear marks
        for m in self.marks.iter_mut() {
            *m = false;
        }
        self.marks.resize(self.objects.len(), false);
        self.gray_queue.clear();

        // Seed gray queue from roots
        for root in roots {
            Self::push_value_ids(&root, &mut self.gray_queue);
        }

        // Drain gray queue (full mark)
        self.mark_all();

        // -- Sweep phase --
        self.gc_phase = GcPhase::Sweeping;
        self.sweep_all();

        // -- Done --
        self.gc_phase = GcPhase::Idle;

        // Adapt threshold: next GC triggers at 2x surviving objects, minimum 8192
        self.gc_threshold = self.allocated_count.saturating_mul(2).max(8192);
    }

    /// Process gray objects until the worklist is empty.
    fn mark_all(&mut self) {
        while let Some(id) = self.gray_queue.pop() {
            let i = id.index as usize;
            if i >= self.marks.len() || self.marks[i] {
                continue;
            }
            if self.generations[i] != id.generation {
                continue; // stale
            }
            self.marks[i] = true;

            // Collect children into a local vec, then extend gray_queue.
            // This avoids borrow conflicts with self.objects / self.gray_queue.
            let mut children = Vec::new();
            Self::trace_heap_object(&self.objects[i], &mut children);
            self.gray_queue.extend(children);
        }
    }

    /// Process up to `work_limit` gray objects. Returns `true` when the gray
    /// queue is empty (marking complete).
    ///
    /// This enables future incremental collection: call `mark_some()` at
    /// safe points to spread marking work across multiple mutator pauses.
    pub fn mark_some(&mut self, work_limit: usize) -> bool {
        for _ in 0..work_limit {
            let Some(id) = self.gray_queue.pop() else {
                return true; // done
            };
            let i = id.index as usize;
            if i >= self.marks.len() || self.marks[i] {
                continue;
            }
            if self.generations[i] != id.generation {
                continue; // stale
            }
            self.marks[i] = true;

            let mut children = Vec::new();
            Self::trace_heap_object(&self.objects[i], &mut children);
            self.gray_queue.extend(children);
        }
        self.gray_queue.is_empty()
    }

    /// Sweep all unmarked objects in one pass.
    fn sweep_all(&mut self) {
        for i in 0..self.objects.len() {
            if !self.marks[i]
                && !matches!(self.objects[i], HeapObject::Free) {
                    self.objects[i] = HeapObject::Free;
                    self.generations[i] = self.generations[i].wrapping_add(1);
                    self.free_list.push(i as u32);
                    self.allocated_count = self.allocated_count.saturating_sub(1);
                }
        }
    }

    fn push_value_ids(val: &Value, worklist: &mut Vec<ObjId>) {
        match val {
            Value::Cons(id) | Value::Vector(id) | Value::HashTable(id)
            | Value::Str(id) | Value::Lambda(id) | Value::Macro(id) | Value::ByteCode(id)
                => worklist.push(*id),
            _ => {}
        }
    }

    /// Trace all Value children inside a HeapObject, pushing their ObjIds onto
    /// the worklist.  Used by both `mark_all()` and `mark_some()`.
    fn trace_heap_object(obj: &HeapObject, children: &mut Vec<ObjId>) {
        match obj {
            HeapObject::Cons { car, cdr } => {
                Self::push_value_ids(car, children);
                Self::push_value_ids(cdr, children);
            }
            HeapObject::Vector(v) => {
                for val in v {
                    Self::push_value_ids(val, children);
                }
            }
            HeapObject::HashTable(ht) => {
                for v in ht.data.values() {
                    Self::push_value_ids(v, children);
                }
                for v in ht.key_snapshots.values() {
                    Self::push_value_ids(v, children);
                }
            }
            HeapObject::Str(_) => {} // no Value children
            HeapObject::Lambda(d) | HeapObject::Macro(d) => {
                if let Some(env) = &d.env {
                    for scope in env {
                        for v in scope.values() {
                            Self::push_value_ids(v, children);
                        }
                    }
                }
            }
            HeapObject::ByteCode(bc) => {
                for c in &bc.constants {
                    Self::push_value_ids(c, children);
                }
                if let Some(env) = &bc.env {
                    for scope in env {
                        for v in scope.values() {
                            Self::push_value_ids(v, children);
                        }
                    }
                }
            }
            HeapObject::Free => {}
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

    #[test]
    fn mark_some_incremental() {
        let mut heap = LispHeap::new();
        let a = heap.alloc_cons(Value::Int(1), Value::Nil);
        let b = heap.alloc_cons(Value::Int(2), Value::Cons(a));
        let c = heap.alloc_cons(Value::Int(3), Value::Cons(b));

        // Manually start marking
        heap.gc_phase = GcPhase::Marking;
        for m in heap.marks.iter_mut() {
            *m = false;
        }
        heap.marks.resize(heap.objects.len(), false);
        heap.gray_queue.clear();
        LispHeap::push_value_ids(&Value::Cons(c), &mut heap.gray_queue);

        // Process one object at a time
        let done = heap.mark_some(1);
        assert!(!done, "should have more work after 1 step");

        // Finish marking
        let done = heap.mark_some(100);
        assert!(done, "should be done after draining queue");

        // All 3 should be marked
        assert!(heap.marks[a.index as usize]);
        assert!(heap.marks[b.index as usize]);
        assert!(heap.marks[c.index as usize]);
    }

    #[test]
    fn write_barrier_regays_black_object() {
        let mut heap = LispHeap::new();
        let a = heap.alloc_cons(Value::Int(1), Value::Nil);
        let new_child = heap.alloc_cons(Value::Int(99), Value::Nil);

        // Simulate marking phase: mark `a` as black
        heap.gc_phase = GcPhase::Marking;
        heap.marks.resize(heap.objects.len(), false);
        heap.marks[a.index as usize] = true;

        // Mutate `a` — write barrier should push it back to gray
        heap.set_cdr(a, Value::Cons(new_child));

        assert!(
            !heap.marks[a.index as usize],
            "write barrier should have cleared mark"
        );
        assert!(
            heap.gray_queue.contains(&a),
            "write barrier should have added to gray queue"
        );

        // After re-scanning, new_child should be discovered
        heap.mark_all();
        assert!(heap.marks[new_child.index as usize]);
    }

    #[test]
    fn write_barrier_noop_when_idle() {
        let mut heap = LispHeap::new();
        let a = heap.alloc_cons(Value::Int(1), Value::Nil);

        // Outside marking phase, write barrier is a no-op
        assert_eq!(heap.gc_phase, GcPhase::Idle);
        heap.set_car(a, Value::Int(42));
        assert!(heap.gray_queue.is_empty());
    }
}
