/// Marker system for tracking buffer positions.
///
/// Markers are named position trackers that automatically adjust when text is
/// inserted or deleted. Each marker has an [`InsertionType`] that controls
/// whether it stays before or moves after text inserted at its exact position.

/// Controls marker behavior when text is inserted exactly at the marker's position.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsertionType {
    /// Marker stays before the newly inserted text.
    /// The marker's byte position does not change.
    Before,
    /// Marker moves to after the newly inserted text.
    /// The marker's byte position increases by the inserted length.
    After,
}

/// A named position tracker within a buffer.
///
/// Each marker has a unique `id`, a `byte_pos` indicating its current byte
/// offset in the buffer, and an `insertion_type` that governs how it adjusts
/// when an insertion occurs at its exact position.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Marker {
    /// Unique identifier for this marker.
    pub id: u64,
    /// Current byte offset in the buffer.
    pub byte_pos: usize,
    /// How this marker behaves when text is inserted at its exact position.
    pub insertion_type: InsertionType,
}

/// A collection of markers associated with a buffer.
///
/// `MarkerSet` provides efficient operations for adding, removing, querying,
/// and bulk-adjusting markers in response to buffer edits (insertions and
/// deletions).
#[derive(Debug, Clone)]
pub struct MarkerSet {
    markers: Vec<Marker>,
}

impl MarkerSet {
    /// Creates a new, empty `MarkerSet`.
    pub fn new() -> Self {
        Self {
            markers: Vec::new(),
        }
    }

    /// Adds a marker with the given `id`, `byte_pos`, and `insertion_type`.
    ///
    /// If a marker with the same `id` already exists, it is replaced.
    pub fn add(&mut self, id: u64, byte_pos: usize, insertion_type: InsertionType) {
        // Remove any existing marker with this id to enforce uniqueness.
        self.remove(id);
        self.markers.push(Marker {
            id,
            byte_pos,
            insertion_type,
        });
    }

    /// Removes the marker with the given `id`.
    ///
    /// Returns `true` if a marker was found and removed, `false` otherwise.
    pub fn remove(&mut self, id: u64) -> bool {
        let before = self.markers.len();
        self.markers.retain(|m| m.id != id);
        self.markers.len() < before
    }

    /// Returns a reference to the marker with the given `id`, if it exists.
    pub fn get(&self, id: u64) -> Option<&Marker> {
        self.markers.iter().find(|m| m.id == id)
    }

    /// Returns the byte position of the marker with the given `id`, if it exists.
    pub fn position(&self, id: u64) -> Option<usize> {
        self.get(id).map(|m| m.byte_pos)
    }

    /// Sets the byte position of the marker with the given `id`.
    ///
    /// Does nothing if no marker with that `id` exists.
    pub fn set_position(&mut self, id: u64, pos: usize) {
        if let Some(m) = self.markers.iter_mut().find(|m| m.id == id) {
            m.byte_pos = pos;
        }
    }

    /// Adjusts all markers in response to a text insertion.
    ///
    /// `insert_pos` is the byte offset where the insertion occurred, and `len`
    /// is the number of bytes inserted.
    ///
    /// - Markers strictly after `insert_pos` are shifted forward by `len`.
    /// - Markers exactly at `insert_pos` with [`InsertionType::After`] are
    ///   shifted forward by `len` (they move to after the new text).
    /// - Markers exactly at `insert_pos` with [`InsertionType::Before`] are
    ///   left unchanged (they stay before the new text).
    /// - Markers before `insert_pos` are unaffected.
    pub fn adjust_for_insert(&mut self, insert_pos: usize, len: usize) {
        if len == 0 {
            return;
        }
        for marker in &mut self.markers {
            if marker.byte_pos > insert_pos {
                // Strictly after the insertion point: always shift.
                marker.byte_pos += len;
            } else if marker.byte_pos == insert_pos {
                // Exactly at the insertion point: depends on insertion type.
                if marker.insertion_type == InsertionType::After {
                    marker.byte_pos += len;
                }
                // InsertionType::Before: stays put.
            }
            // Before the insertion point: no change.
        }
    }

    /// Adjusts all markers in response to a text deletion.
    ///
    /// `start` is the byte offset of the first deleted byte, and `end` is one
    /// past the last deleted byte (i.e., the half-open range `[start, end)` is
    /// removed).
    ///
    /// - Markers before `start` are unaffected.
    /// - Markers within `[start, end)` are clamped to `start`.
    /// - Markers at or after `end` are shifted back by `end - start`.
    pub fn adjust_for_delete(&mut self, start: usize, end: usize) {
        if start >= end {
            return;
        }
        let deleted_len = end - start;
        for marker in &mut self.markers {
            if marker.byte_pos >= end {
                // After the deleted region: shift back.
                marker.byte_pos -= deleted_len;
            } else if marker.byte_pos >= start {
                // Inside the deleted region: clamp to start.
                marker.byte_pos = start;
            }
            // Before the deleted region: no change.
        }
    }

    /// Returns all `(id, byte_pos)` pairs in the order markers were added.
    pub fn all_positions(&self) -> Vec<(u64, usize)> {
        self.markers.iter().map(|m| (m.id, m.byte_pos)).collect()
    }

    /// Returns the number of markers in this set.
    pub fn len(&self) -> usize {
        self.markers.len()
    }

    /// Returns `true` if the set contains no markers.
    pub fn is_empty(&self) -> bool {
        self.markers.is_empty()
    }
}

impl Default for MarkerSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // Basic operations
    // -----------------------------------------------------------------------

    #[test]
    fn new_marker_set_is_empty() {
        let set = MarkerSet::new();
        assert_eq!(set.len(), 0);
        assert!(set.is_empty());
        assert!(set.all_positions().is_empty());
    }

    #[test]
    fn add_and_get() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);

        assert_eq!(set.len(), 1);
        let m = set.get(1).expect("marker 1 should exist");
        assert_eq!(m.id, 1);
        assert_eq!(m.byte_pos, 10);
        assert_eq!(m.insertion_type, InsertionType::Before);
    }

    #[test]
    fn add_replaces_existing_id() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.add(1, 20, InsertionType::After);

        assert_eq!(set.len(), 1);
        let m = set.get(1).unwrap();
        assert_eq!(m.byte_pos, 20);
        assert_eq!(m.insertion_type, InsertionType::After);
    }

    #[test]
    fn position_returns_byte_pos() {
        let mut set = MarkerSet::new();
        set.add(5, 42, InsertionType::Before);
        assert_eq!(set.position(5), Some(42));
        assert_eq!(set.position(99), None);
    }

    #[test]
    fn set_position_updates_marker() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.set_position(1, 99);
        assert_eq!(set.position(1), Some(99));
    }

    #[test]
    fn set_position_noop_for_missing_id() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.set_position(99, 50); // no such marker
        assert_eq!(set.len(), 1);
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn remove_existing() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        assert!(set.remove(1));
        assert_eq!(set.len(), 0);
        assert!(set.get(1).is_none());
    }

    #[test]
    fn remove_missing_returns_false() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        assert!(!set.remove(99));
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn all_positions_returns_all_markers() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.add(2, 20, InsertionType::After);
        set.add(3, 30, InsertionType::Before);

        let positions = set.all_positions();
        assert_eq!(positions.len(), 3);
        assert!(positions.contains(&(1, 10)));
        assert!(positions.contains(&(2, 20)));
        assert!(positions.contains(&(3, 30)));
    }

    // -----------------------------------------------------------------------
    // Insert adjustments
    // -----------------------------------------------------------------------

    #[test]
    fn insert_shifts_markers_after_insert_pos() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.add(2, 20, InsertionType::Before);

        // Insert 5 bytes at position 15.
        set.adjust_for_insert(15, 5);

        assert_eq!(set.position(1), Some(10)); // before insert: unchanged
        assert_eq!(set.position(2), Some(25)); // after insert: shifted by 5
    }

    #[test]
    fn insert_at_marker_position_before_type() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);

        // Insert 3 bytes exactly at position 10.
        set.adjust_for_insert(10, 3);

        // InsertionType::Before: marker stays at 10 (before the new text).
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn insert_at_marker_position_after_type() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::After);

        // Insert 3 bytes exactly at position 10.
        set.adjust_for_insert(10, 3);

        // InsertionType::After: marker moves to 13 (after the new text).
        assert_eq!(set.position(1), Some(13));
    }

    #[test]
    fn insert_zero_length_is_noop() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::After);

        set.adjust_for_insert(10, 0);
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn insert_before_all_markers_shifts_all() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.add(2, 20, InsertionType::After);

        set.adjust_for_insert(0, 5);

        assert_eq!(set.position(1), Some(15));
        assert_eq!(set.position(2), Some(25));
    }

    #[test]
    fn insert_after_all_markers_changes_none() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.add(2, 20, InsertionType::After);

        set.adjust_for_insert(100, 5);

        assert_eq!(set.position(1), Some(10));
        assert_eq!(set.position(2), Some(20));
    }

    #[test]
    fn insert_multiple_markers_same_position_different_types() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);
        set.add(2, 10, InsertionType::After);

        set.adjust_for_insert(10, 7);

        // Before stays, After moves.
        assert_eq!(set.position(1), Some(10));
        assert_eq!(set.position(2), Some(17));
    }

    // -----------------------------------------------------------------------
    // Delete adjustments
    // -----------------------------------------------------------------------

    #[test]
    fn delete_shifts_markers_after_region() {
        let mut set = MarkerSet::new();
        set.add(1, 20, InsertionType::Before);

        // Delete bytes [5, 10).
        set.adjust_for_delete(5, 10);

        // Marker after region: shifted back by 5.
        assert_eq!(set.position(1), Some(15));
    }

    #[test]
    fn delete_clamps_markers_inside_region_to_start() {
        let mut set = MarkerSet::new();
        set.add(1, 7, InsertionType::Before);

        // Delete bytes [5, 10).
        set.adjust_for_delete(5, 10);

        // Marker inside deleted region: clamped to start (5).
        assert_eq!(set.position(1), Some(5));
    }

    #[test]
    fn delete_leaves_markers_before_region_unchanged() {
        let mut set = MarkerSet::new();
        set.add(1, 3, InsertionType::Before);

        // Delete bytes [5, 10).
        set.adjust_for_delete(5, 10);

        assert_eq!(set.position(1), Some(3));
    }

    #[test]
    fn delete_marker_at_region_start() {
        let mut set = MarkerSet::new();
        set.add(1, 5, InsertionType::Before);

        // Delete bytes [5, 10). Marker at start of deleted region.
        set.adjust_for_delete(5, 10);

        // Clamped to start = 5 (which is start itself).
        assert_eq!(set.position(1), Some(5));
    }

    #[test]
    fn delete_marker_at_region_end() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);

        // Delete bytes [5, 10). Marker at end boundary (one past last deleted).
        set.adjust_for_delete(5, 10);

        // Marker at `end`: shifted back by 5 (10 - 5 = 5).
        assert_eq!(set.position(1), Some(5));
    }

    #[test]
    fn delete_zero_length_is_noop() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);

        set.adjust_for_delete(5, 5);
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn delete_inverted_range_is_noop() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);

        // start > end: should be a no-op (defensive).
        set.adjust_for_delete(10, 5);
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn delete_multiple_markers_mixed_positions() {
        let mut set = MarkerSet::new();
        set.add(1, 3, InsertionType::Before);  // before region
        set.add(2, 7, InsertionType::After);    // inside region
        set.add(3, 15, InsertionType::Before);  // after region

        // Delete bytes [5, 10).
        set.adjust_for_delete(5, 10);

        assert_eq!(set.position(1), Some(3));  // unchanged
        assert_eq!(set.position(2), Some(5));  // clamped to start
        assert_eq!(set.position(3), Some(10)); // shifted back by 5
    }

    // -----------------------------------------------------------------------
    // Combined insert + delete sequences
    // -----------------------------------------------------------------------

    #[test]
    fn sequential_insert_then_delete() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::After);

        // Insert 5 bytes at position 10. Marker moves to 15 (After type).
        set.adjust_for_insert(10, 5);
        assert_eq!(set.position(1), Some(15));

        // Delete the 5 bytes we just inserted: [10, 15).
        set.adjust_for_delete(10, 15);
        // Marker at 15 (== end): shifted back by 5 -> 10.
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn multiple_insertions_accumulate() {
        let mut set = MarkerSet::new();
        set.add(1, 0, InsertionType::Before);

        // Insert at position 0 three times. Before type: marker stays at 0.
        set.adjust_for_insert(0, 5);
        assert_eq!(set.position(1), Some(0));
        set.adjust_for_insert(0, 5);
        assert_eq!(set.position(1), Some(0));
        set.adjust_for_insert(0, 5);
        assert_eq!(set.position(1), Some(0));
    }

    #[test]
    fn multiple_insertions_after_type_accumulate() {
        let mut set = MarkerSet::new();
        set.add(1, 0, InsertionType::After);

        set.adjust_for_insert(0, 5);
        assert_eq!(set.position(1), Some(5));

        // Next insert at the marker's new position (5).
        set.adjust_for_insert(5, 3);
        assert_eq!(set.position(1), Some(8));
    }

    // -----------------------------------------------------------------------
    // Edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn marker_at_position_zero() {
        let mut set = MarkerSet::new();
        set.add(1, 0, InsertionType::Before);
        set.add(2, 0, InsertionType::After);

        set.adjust_for_insert(0, 10);

        assert_eq!(set.position(1), Some(0));
        assert_eq!(set.position(2), Some(10));
    }

    #[test]
    fn delete_entire_buffer_clamps_all_markers() {
        let mut set = MarkerSet::new();
        set.add(1, 0, InsertionType::Before);
        set.add(2, 50, InsertionType::After);
        set.add(3, 100, InsertionType::Before);

        // Delete everything: [0, 200).
        set.adjust_for_delete(0, 200);

        assert_eq!(set.position(1), Some(0));
        assert_eq!(set.position(2), Some(0));
        assert_eq!(set.position(3), Some(0));
    }

    #[test]
    fn large_number_of_markers() {
        let mut set = MarkerSet::new();
        for i in 0..1000u64 {
            set.add(
                i,
                i as usize * 10,
                if i % 2 == 0 {
                    InsertionType::Before
                } else {
                    InsertionType::After
                },
            );
        }
        assert_eq!(set.len(), 1000);

        // Insert 100 bytes at position 5000 (marker id=500 is at 5000).
        set.adjust_for_insert(5000, 100);

        // Markers before 5000 are unchanged.
        assert_eq!(set.position(499), Some(4990));

        // Marker 500 at exactly 5000: Before type -> stays at 5000.
        assert_eq!(set.position(500), Some(5000));

        // Marker 501 at 5010: After type, strictly after insert -> shifted.
        assert_eq!(set.position(501), Some(5110));

        // Marker 999 at 9990 -> 10090.
        assert_eq!(set.position(999), Some(10090));
    }

    #[test]
    fn default_creates_empty_set() {
        let set = MarkerSet::default();
        assert!(set.is_empty());
        assert_eq!(set.len(), 0);
    }

    #[test]
    fn get_nonexistent_returns_none() {
        let set = MarkerSet::new();
        assert!(set.get(42).is_none());
        assert!(set.position(42).is_none());
    }

    #[test]
    fn insert_at_position_between_markers() {
        let mut set = MarkerSet::new();
        set.add(1, 5, InsertionType::Before);
        set.add(2, 10, InsertionType::After);
        set.add(3, 15, InsertionType::Before);

        // Insert 3 bytes at position 10.
        set.adjust_for_insert(10, 3);

        assert_eq!(set.position(1), Some(5));  // before: unchanged
        assert_eq!(set.position(2), Some(13)); // at insert, After: moved
        assert_eq!(set.position(3), Some(18)); // after: shifted
    }

    #[test]
    fn delete_single_byte_at_marker() {
        let mut set = MarkerSet::new();
        set.add(1, 10, InsertionType::Before);

        // Delete exactly the byte at position 10: [10, 11).
        set.adjust_for_delete(10, 11);

        // Marker inside deleted region: clamped to 10.
        assert_eq!(set.position(1), Some(10));
    }

    #[test]
    fn insert_then_delete_restores_relative_order() {
        let mut set = MarkerSet::new();
        set.add(1, 5, InsertionType::Before);
        set.add(2, 10, InsertionType::Before);
        set.add(3, 15, InsertionType::Before);

        // Insert 10 bytes at position 8.
        set.adjust_for_insert(8, 10);
        assert_eq!(set.position(1), Some(5));
        assert_eq!(set.position(2), Some(20));
        assert_eq!(set.position(3), Some(25));

        // Delete those 10 bytes: [8, 18).
        set.adjust_for_delete(8, 18);
        assert_eq!(set.position(1), Some(5));
        assert_eq!(set.position(2), Some(10));
        assert_eq!(set.position(3), Some(15));
    }
}
