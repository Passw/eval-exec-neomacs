//! Semi-space copying garbage collector.
//!
//! Two memory regions (semi-spaces). Allocation bumps a pointer in the active
//! space. When full, live objects are copied to the other space (Cheney's
//! algorithm), then spaces are swapped.
//!
//! Advantages:
//! - O(live objects) collection, not O(heap size)
//! - Automatic compaction — no fragmentation
//! - Bump allocation is ~2 instructions
//! - Cache-friendly due to compaction
//!
//! Limitations:
//! - 50% memory overhead (two semi-spaces)
//! - All live objects copied each GC (mitigated by generational extension later)

use super::types::*;
use std::collections::HashMap;
use std::ptr;

/// Opaque handle to a GC-managed object.
///
/// Internally an index/offset into the heap. NOT a raw pointer — remains stable
/// across GC cycles because the handle table is updated during collection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct GcRef(pub(crate) u64);

impl GcRef {
    pub const NIL: GcRef = GcRef(0);

    pub fn is_nil(self) -> bool {
        self.0 == 0
    }
}

/// The GC heap — semi-space copying collector.
pub struct GcHeap {
    /// Active semi-space.
    from_space: Vec<u8>,
    /// Reserve semi-space (used during collection).
    to_space: Vec<u8>,
    /// Bump pointer — next free byte in from_space.
    alloc_ptr: usize,
    /// Capacity of each semi-space.
    space_size: usize,
    /// Handle table: GcRef → offset in from_space.
    /// Updated during GC to point to new locations in to_space.
    handles: HashMap<u64, usize>,
    /// Next handle ID.
    next_handle: u64,
    /// Root set: handles that are GC roots (stack frames, globals).
    roots: Vec<GcRef>,
    /// Statistics.
    pub stats: GcStats,
}

#[derive(Clone, Debug, Default)]
pub struct GcStats {
    pub total_allocated: usize,
    pub total_collections: usize,
    pub total_copied_bytes: usize,
    pub live_bytes: usize,
    pub heap_size: usize,
}

impl GcHeap {
    /// Create a new heap with the given semi-space size (each space).
    pub fn new(space_size: usize) -> Self {
        let space_size = space_size.max(4096); // minimum 4KB
        Self {
            from_space: vec![0u8; space_size],
            to_space: vec![0u8; space_size],
            alloc_ptr: 0,
            space_size,
            handles: HashMap::new(),
            next_handle: 1, // 0 = NIL
            roots: Vec::new(),
            stats: GcStats {
                heap_size: space_size,
                ..Default::default()
            },
        }
    }

    /// Default heap: 4MB per semi-space.
    pub fn default_heap() -> Self {
        Self::new(4 * 1024 * 1024)
    }

    // -----------------------------------------------------------------------
    // Allocation
    // -----------------------------------------------------------------------

    /// Allocate `size` bytes in the heap. Returns offset in from_space.
    /// Triggers GC if needed.
    fn alloc_raw(&mut self, size: usize) -> Result<usize, GcError> {
        // Align to 8 bytes
        let aligned_size = (size + 7) & !7;

        if self.alloc_ptr + aligned_size > self.space_size {
            // Try GC first
            self.collect()?;
            if self.alloc_ptr + aligned_size > self.space_size {
                // Still no room — grow the heap
                self.grow(aligned_size)?;
            }
        }

        let offset = self.alloc_ptr;
        self.alloc_ptr += aligned_size;
        self.stats.total_allocated += aligned_size;
        Ok(offset)
    }

    /// Allocate an object and return a handle.
    fn alloc_object(&mut self, tag: GcTag, payload_size: usize) -> Result<GcRef, GcError> {
        let total_size = ObjHeader::SIZE + payload_size;
        let offset = self.alloc_raw(total_size)?;

        // Write header
        let header = ObjHeader::new(tag, total_size as u32);
        self.write_header(offset, &header);

        // Create handle
        let handle_id = self.next_handle;
        self.next_handle += 1;
        self.handles.insert(handle_id, offset);
        Ok(GcRef(handle_id))
    }

    /// Allocate a cons cell. Returns handle.
    pub fn alloc_cons(&mut self, car: GcRef, cdr: GcRef) -> Result<GcRef, GcError> {
        let handle = self.alloc_object(GcTag::Cons, 2 * std::mem::size_of::<u64>())?;
        let offset = self.handles[&handle.0];
        let payload_offset = offset + ObjHeader::SIZE;
        self.write_u64(payload_offset, car.0);
        self.write_u64(payload_offset + 8, cdr.0);
        Ok(handle)
    }

    /// Read the car of a cons cell.
    pub fn cons_car(&self, handle: GcRef) -> GcRef {
        if handle.is_nil() {
            return GcRef::NIL;
        }
        let offset = match self.handles.get(&handle.0) {
            Some(&off) => off,
            None => return GcRef::NIL,
        };
        let payload_offset = offset + ObjHeader::SIZE;
        GcRef(self.read_u64(payload_offset))
    }

    /// Read the cdr of a cons cell.
    pub fn cons_cdr(&self, handle: GcRef) -> GcRef {
        if handle.is_nil() {
            return GcRef::NIL;
        }
        let offset = match self.handles.get(&handle.0) {
            Some(&off) => off,
            None => return GcRef::NIL,
        };
        let payload_offset = offset + ObjHeader::SIZE;
        GcRef(self.read_u64(payload_offset + 8))
    }

    /// Set the car of a cons cell.
    pub fn set_cons_car(&mut self, handle: GcRef, car: GcRef) {
        if handle.is_nil() {
            return;
        }
        if let Some(&offset) = self.handles.get(&handle.0) {
            let payload_offset = offset + ObjHeader::SIZE;
            self.write_u64(payload_offset, car.0);
        }
    }

    /// Set the cdr of a cons cell.
    pub fn set_cons_cdr(&mut self, handle: GcRef, cdr: GcRef) {
        if handle.is_nil() {
            return;
        }
        if let Some(&offset) = self.handles.get(&handle.0) {
            let payload_offset = offset + ObjHeader::SIZE;
            self.write_u64(payload_offset + 8, cdr.0);
        }
    }

    /// Allocate a string. Returns handle.
    pub fn alloc_string(&mut self, s: &str) -> Result<GcRef, GcError> {
        let bytes = s.as_bytes();
        // Layout: [len: u64][bytes...]
        let payload_size = 8 + bytes.len();
        let handle = self.alloc_object(GcTag::String, payload_size)?;
        let offset = self.handles[&handle.0];
        let payload_offset = offset + ObjHeader::SIZE;
        self.write_u64(payload_offset, bytes.len() as u64);
        self.from_space[payload_offset + 8..payload_offset + 8 + bytes.len()]
            .copy_from_slice(bytes);
        Ok(handle)
    }

    /// Read a string from a handle.
    pub fn get_string(&self, handle: GcRef) -> Option<String> {
        if handle.is_nil() {
            return None;
        }
        let offset = *self.handles.get(&handle.0)?;
        let header = self.read_header(offset);
        if header.gc_tag() != Some(GcTag::String) {
            return None;
        }
        let payload_offset = offset + ObjHeader::SIZE;
        let len = self.read_u64(payload_offset) as usize;
        let bytes = &self.from_space[payload_offset + 8..payload_offset + 8 + len];
        String::from_utf8(bytes.to_vec()).ok()
    }

    /// Allocate a vector. Returns handle.
    pub fn alloc_vector(&mut self, elements: &[GcRef]) -> Result<GcRef, GcError> {
        // Layout: [len: u64][elem0: u64][elem1: u64]...
        let payload_size = 8 + elements.len() * 8;
        let handle = self.alloc_object(GcTag::Vector, payload_size)?;
        let offset = self.handles[&handle.0];
        let payload_offset = offset + ObjHeader::SIZE;
        self.write_u64(payload_offset, elements.len() as u64);
        for (i, elem) in elements.iter().enumerate() {
            self.write_u64(payload_offset + 8 + i * 8, elem.0);
        }
        Ok(handle)
    }

    /// Read a vector element.
    pub fn vector_ref(&self, handle: GcRef, index: usize) -> Option<GcRef> {
        if handle.is_nil() {
            return None;
        }
        let offset = *self.handles.get(&handle.0)?;
        let payload_offset = offset + ObjHeader::SIZE;
        let len = self.read_u64(payload_offset) as usize;
        if index >= len {
            return None;
        }
        Some(GcRef(self.read_u64(payload_offset + 8 + index * 8)))
    }

    /// Set a vector element.
    pub fn vector_set(&mut self, handle: GcRef, index: usize, value: GcRef) -> bool {
        if handle.is_nil() {
            return false;
        }
        let offset = match self.handles.get(&handle.0) {
            Some(&off) => off,
            None => return false,
        };
        let payload_offset = offset + ObjHeader::SIZE;
        let len = self.read_u64(payload_offset) as usize;
        if index >= len {
            return false;
        }
        self.write_u64(payload_offset + 8 + index * 8, value.0);
        true
    }

    /// Vector length.
    pub fn vector_len(&self, handle: GcRef) -> usize {
        if handle.is_nil() {
            return 0;
        }
        let offset = match self.handles.get(&handle.0) {
            Some(&off) => off,
            None => return 0,
        };
        let payload_offset = offset + ObjHeader::SIZE;
        self.read_u64(payload_offset) as usize
    }

    /// Get the tag of an object.
    pub fn tag(&self, handle: GcRef) -> Option<GcTag> {
        if handle.is_nil() {
            return None;
        }
        let offset = *self.handles.get(&handle.0)?;
        let header = self.read_header(offset);
        header.gc_tag()
    }

    // -----------------------------------------------------------------------
    // Root management
    // -----------------------------------------------------------------------

    /// Add a GC root. Returns an index that can be used to remove it.
    pub fn add_root(&mut self, handle: GcRef) -> usize {
        let idx = self.roots.len();
        self.roots.push(handle);
        idx
    }

    /// Remove a GC root by index.
    pub fn remove_root(&mut self, idx: usize) {
        if idx < self.roots.len() {
            self.roots[idx] = GcRef::NIL;
        }
    }

    /// Replace a root's value.
    pub fn update_root(&mut self, idx: usize, handle: GcRef) {
        if idx < self.roots.len() {
            self.roots[idx] = handle;
        }
    }

    // -----------------------------------------------------------------------
    // Collection (Cheney's copying algorithm)
    // -----------------------------------------------------------------------

    /// Run garbage collection.
    pub fn collect(&mut self) -> Result<(), GcError> {
        self.stats.total_collections += 1;

        // to_space is our target
        self.to_space.fill(0);
        let mut to_ptr: usize = 0;

        // Map: old handle_id → new offset in to_space
        let mut new_offsets: HashMap<u64, usize> = HashMap::new();

        // Phase 1: Copy root-reachable objects (BFS with Cheney scan pointer)
        let mut scan_ptr: usize = 0;

        // Copy all roots (clone to avoid borrow conflict with copy_object)
        let roots_snapshot: Vec<GcRef> = self.roots.clone();
        for root in &roots_snapshot {
            if !root.is_nil() {
                self.copy_object(root.0, &mut to_ptr, &mut new_offsets);
            }
        }

        // Phase 2: Scan copied objects for references (Cheney's algorithm)
        while scan_ptr < to_ptr {
            let header = read_header_from(&self.to_space, scan_ptr);
            let aligned_size = ((header.size as usize) + 7) & !7;

            match header.gc_tag() {
                Some(GcTag::Cons) => {
                    let payload = scan_ptr + ObjHeader::SIZE;
                    // car
                    let car_id = read_u64_from(&self.to_space, payload);
                    if car_id != 0 {
                        let new_id = self.copy_object(car_id, &mut to_ptr, &mut new_offsets);
                        write_u64_to(&mut self.to_space, payload, new_id);
                    }
                    // cdr
                    let cdr_id = read_u64_from(&self.to_space, payload + 8);
                    if cdr_id != 0 {
                        let new_id = self.copy_object(cdr_id, &mut to_ptr, &mut new_offsets);
                        write_u64_to(&mut self.to_space, payload + 8, new_id);
                    }
                }
                Some(GcTag::Vector) => {
                    let payload = scan_ptr + ObjHeader::SIZE;
                    let len = read_u64_from(&self.to_space, payload) as usize;
                    for i in 0..len {
                        let elem_offset = payload + 8 + i * 8;
                        let elem_id = read_u64_from(&self.to_space, elem_offset);
                        if elem_id != 0 {
                            let new_id = self.copy_object(elem_id, &mut to_ptr, &mut new_offsets);
                            write_u64_to(&mut self.to_space, elem_offset, new_id);
                        }
                    }
                }
                Some(GcTag::String) | Some(GcTag::Symbol) => {
                    // No GC references in strings/symbols
                }
                _ => {}
            }

            scan_ptr += aligned_size;
        }

        // Phase 3: Swap spaces
        std::mem::swap(&mut self.from_space, &mut self.to_space);
        self.alloc_ptr = to_ptr;

        // Phase 4: Update handle table
        self.handles = new_offsets;

        self.stats.total_copied_bytes += to_ptr;
        self.stats.live_bytes = to_ptr;

        Ok(())
    }

    /// Copy a single object from from_space to to_space if not already copied.
    /// Returns the handle ID (unchanged — we reuse handle IDs).
    fn copy_object(
        &mut self,
        handle_id: u64,
        to_ptr: &mut usize,
        new_offsets: &mut HashMap<u64, usize>,
    ) -> u64 {
        if handle_id == 0 {
            return 0;
        }

        // Already copied?
        if new_offsets.contains_key(&handle_id) {
            return handle_id;
        }

        // Find in from_space
        let old_offset = match self.handles.get(&handle_id) {
            Some(&off) => off,
            None => return handle_id, // Unknown handle — leave as-is
        };

        let header = read_header_from(&self.from_space, old_offset);
        let size = header.size as usize;
        let aligned_size = (size + 7) & !7;

        // Copy to to_space
        let new_offset = *to_ptr;
        if new_offset + aligned_size > self.to_space.len() {
            // Out of space — this shouldn't happen if we sized correctly
            return handle_id;
        }

        self.to_space[new_offset..new_offset + size]
            .copy_from_slice(&self.from_space[old_offset..old_offset + size]);

        *to_ptr += aligned_size;
        new_offsets.insert(handle_id, new_offset);

        handle_id
    }

    /// Grow the heap (double both semi-spaces).
    fn grow(&mut self, min_extra: usize) -> Result<(), GcError> {
        let new_size = (self.space_size * 2).max(self.space_size + min_extra);
        if new_size > 1024 * 1024 * 1024 {
            return Err(GcError::OutOfMemory);
        }

        let mut new_from = vec![0u8; new_size];
        new_from[..self.alloc_ptr].copy_from_slice(&self.from_space[..self.alloc_ptr]);
        self.from_space = new_from;
        self.to_space = vec![0u8; new_size];
        self.space_size = new_size;
        self.stats.heap_size = new_size;
        Ok(())
    }

    // -----------------------------------------------------------------------
    // Low-level memory access
    // -----------------------------------------------------------------------

    fn write_header(&mut self, offset: usize, header: &ObjHeader) {
        let bytes = unsafe {
            std::slice::from_raw_parts(header as *const ObjHeader as *const u8, ObjHeader::SIZE)
        };
        self.from_space[offset..offset + ObjHeader::SIZE].copy_from_slice(bytes);
    }

    fn read_header(&self, offset: usize) -> ObjHeader {
        let mut header = ObjHeader::new(GcTag::Cons, 0);
        unsafe {
            ptr::copy_nonoverlapping(
                self.from_space[offset..].as_ptr(),
                &mut header as *mut ObjHeader as *mut u8,
                ObjHeader::SIZE,
            );
        }
        header
    }

    fn write_u64(&mut self, offset: usize, value: u64) {
        self.from_space[offset..offset + 8].copy_from_slice(&value.to_ne_bytes());
    }

    fn read_u64(&self, offset: usize) -> u64 {
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&self.from_space[offset..offset + 8]);
        u64::from_ne_bytes(bytes)
    }

    /// Current allocation pointer (bytes used in active space).
    pub fn bytes_used(&self) -> usize {
        self.alloc_ptr
    }

    /// Total handles alive.
    pub fn handle_count(&self) -> usize {
        self.handles.len()
    }
}

// ---------------------------------------------------------------------------
// Free functions for reading/writing to arbitrary buffers
// ---------------------------------------------------------------------------

fn read_header_from(buf: &[u8], offset: usize) -> ObjHeader {
    let mut header = ObjHeader::new(GcTag::Cons, 0);
    unsafe {
        ptr::copy_nonoverlapping(
            buf[offset..].as_ptr(),
            &mut header as *mut ObjHeader as *mut u8,
            ObjHeader::SIZE,
        );
    }
    header
}

fn read_u64_from(buf: &[u8], offset: usize) -> u64 {
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&buf[offset..offset + 8]);
    u64::from_ne_bytes(bytes)
}

fn write_u64_to(buf: &mut [u8], offset: usize, value: u64) {
    buf[offset..offset + 8].copy_from_slice(&value.to_ne_bytes());
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum GcError {
    OutOfMemory,
}

impl std::fmt::Display for GcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GcError::OutOfMemory => write!(f, "GC: out of memory"),
        }
    }
}

impl std::error::Error for GcError {}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_and_read_cons() {
        let mut heap = GcHeap::new(4096);
        let a = heap.alloc_string("hello").unwrap();
        let b = heap.alloc_string("world").unwrap();
        let cons = heap.alloc_cons(a, b).unwrap();

        assert_eq!(heap.cons_car(cons), a);
        assert_eq!(heap.cons_cdr(cons), b);
        assert_eq!(heap.get_string(a), Some("hello".to_string()));
        assert_eq!(heap.get_string(b), Some("world".to_string()));
    }

    #[test]
    fn alloc_and_read_vector() {
        let mut heap = GcHeap::new(4096);
        let a = heap.alloc_string("a").unwrap();
        let b = heap.alloc_string("b").unwrap();
        let vec = heap.alloc_vector(&[a, b]).unwrap();

        assert_eq!(heap.vector_len(vec), 2);
        assert_eq!(heap.vector_ref(vec, 0), Some(a));
        assert_eq!(heap.vector_ref(vec, 1), Some(b));
        assert_eq!(heap.vector_ref(vec, 2), None);
    }

    #[test]
    fn gc_collects_unreachable() {
        let mut heap = GcHeap::new(4096);

        // Allocate some objects
        let a = heap.alloc_string("keep").unwrap();
        let _b = heap.alloc_string("garbage").unwrap(); // no root

        // Only root 'a'
        heap.add_root(a);

        let before = heap.bytes_used();
        heap.collect().unwrap();
        let after = heap.bytes_used();

        // After GC, only the rooted string should remain
        assert!(after < before, "GC should reclaim unreachable objects");
        assert_eq!(heap.get_string(a), Some("keep".to_string()));
    }

    #[test]
    fn gc_preserves_cons_structure() {
        let mut heap = GcHeap::new(4096);

        let s1 = heap.alloc_string("foo").unwrap();
        let s2 = heap.alloc_string("bar").unwrap();
        let cons = heap.alloc_cons(s1, s2).unwrap();
        heap.add_root(cons);

        // Allocate garbage
        for i in 0..50 {
            let _ = heap.alloc_string(&format!("garbage-{}", i));
        }

        heap.collect().unwrap();

        // Cons and its children should survive
        assert_eq!(heap.tag(cons), Some(GcTag::Cons));
        let car = heap.cons_car(cons);
        let cdr = heap.cons_cdr(cons);
        assert_eq!(heap.get_string(car), Some("foo".to_string()));
        assert_eq!(heap.get_string(cdr), Some("bar".to_string()));
    }

    #[test]
    fn gc_preserves_deep_list() {
        let mut heap = GcHeap::new(8192);

        // Build a list: (1 . (2 . (3 . nil)))
        // Using string representations for simplicity
        let s3 = heap.alloc_string("3").unwrap();
        let c3 = heap.alloc_cons(s3, GcRef::NIL).unwrap();
        let s2 = heap.alloc_string("2").unwrap();
        let c2 = heap.alloc_cons(s2, c3).unwrap();
        let s1 = heap.alloc_string("1").unwrap();
        let c1 = heap.alloc_cons(s1, c2).unwrap();

        heap.add_root(c1);
        heap.collect().unwrap();

        // Walk the list
        let car1 = heap.cons_car(c1);
        assert_eq!(heap.get_string(car1), Some("1".to_string()));
        let cdr1 = heap.cons_cdr(c1);
        let car2 = heap.cons_car(cdr1);
        assert_eq!(heap.get_string(car2), Some("2".to_string()));
        let cdr2 = heap.cons_cdr(cdr1);
        let car3 = heap.cons_car(cdr2);
        assert_eq!(heap.get_string(car3), Some("3".to_string()));
        assert!(heap.cons_cdr(cdr2).is_nil());
    }

    #[test]
    fn gc_vector_references_survive() {
        let mut heap = GcHeap::new(4096);

        let s1 = heap.alloc_string("one").unwrap();
        let s2 = heap.alloc_string("two").unwrap();
        let vec = heap.alloc_vector(&[s1, s2]).unwrap();
        heap.add_root(vec);

        heap.collect().unwrap();

        assert_eq!(heap.vector_len(vec), 2);
        let e0 = heap.vector_ref(vec, 0).unwrap();
        let e1 = heap.vector_ref(vec, 1).unwrap();
        assert_eq!(heap.get_string(e0), Some("one".to_string()));
        assert_eq!(heap.get_string(e1), Some("two".to_string()));
    }

    #[test]
    fn heap_grows_when_needed() {
        let mut heap = GcHeap::new(4096);

        // Allocate more than 4KB of data
        let mut roots = Vec::new();
        for i in 0..200 {
            let s = heap.alloc_string(&format!("string-{:04}", i)).unwrap();
            let idx = heap.add_root(s);
            roots.push(idx);
        }

        // Should have grown
        assert!(heap.stats.heap_size > 4096);
    }

    #[test]
    fn mutation_after_gc() {
        let mut heap = GcHeap::new(4096);

        let s1 = heap.alloc_string("original").unwrap();
        let s2 = heap.alloc_string("replaced").unwrap();
        let cons = heap.alloc_cons(s1, GcRef::NIL).unwrap();
        heap.add_root(cons);
        heap.add_root(s2);

        heap.collect().unwrap();

        // Mutate the cons cell after GC
        heap.set_cons_car(cons, s2);
        let new_car = heap.cons_car(cons);
        assert_eq!(heap.get_string(new_car), Some("replaced".to_string()));
    }

    #[test]
    fn stats_tracking() {
        let mut heap = GcHeap::new(4096);

        assert_eq!(heap.stats.total_collections, 0);
        assert_eq!(heap.stats.total_allocated, 0);

        let s = heap.alloc_string("test").unwrap();
        assert!(heap.stats.total_allocated > 0);

        heap.add_root(s);
        heap.collect().unwrap();
        assert_eq!(heap.stats.total_collections, 1);
        assert!(heap.stats.live_bytes > 0);
    }
}
