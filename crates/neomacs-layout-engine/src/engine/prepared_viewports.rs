//! Bounded storage for accepted viewports outside the current window.
//!
//! Entries own their original face namespace and are admitted only after the
//! full layout key matches. This is content reuse, not a replay of old input or
//! old chrome. Opaque evaluator-side leases retain captured source objects;
//! they grant lifetime, while the read certificate grants freshness.

mod export;

use super::*;
use crate::frame_face_arena::PreparedFaceSnapshot;
use crate::incremental_layout::WindowDelta;
use neovm_core::tagged::collection_reads::CollectionReads;
use std::collections::VecDeque;

const MAX_VIEWPORTS: usize = 8;
const MAX_ROWS: usize = 512;
const MAX_GLYPHS: usize = 65_536;

struct PreparedViewport {
    frame: neovm_core::window::FrameId,
    window: DisplayWindowId,
    retained: RetainedWindowMatrix,
    faces: PreparedFaceSnapshot,
    rows: usize,
    glyphs: usize,
    computed: bool,
    // Partial worker pages may extend coverage, but cannot replace a viewport.
    complete_viewport: bool,
    reads: Option<CollectionReads>,
    _source_roots: Vec<neovm_core::emacs_core::owned_roots::OwnedRoots>,
}

impl PreparedViewport {
    fn dependencies_valid(&self) -> bool {
        self.retained.key.fontset_generation
            == neovm_core::emacs_core::fontset::fontset_generation()
            && self
                .reads
                .as_ref()
                .is_none_or(CollectionReads::unchanged_and_observe)
    }
}

#[derive(Default)]
pub(super) struct PreparedViewports {
    entries: VecDeque<PreparedViewport>,
    pub(super) invalidated: Vec<(neovm_core::window::FrameId, DisplayWindowId)>,
    export_epochs: Vec<(
        neovm_core::window::FrameId,
        DisplayWindowId,
        RetainedWindowKey,
        u64,
    )>,
}

impl PreparedViewports {
    pub(super) fn retire_invalid_dependencies(&mut self) {
        self.entries.retain(|entry| {
            if entry.dependencies_valid() {
                return true;
            }
            let owner = (entry.frame, entry.window);
            if !self.invalidated.contains(&owner) {
                self.invalidated.push(owner);
            }
            self.export_epochs
                .retain(|(frame, window, _, _)| (*frame, *window) != owner);
            false
        });
    }

    pub(super) fn has_computed(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
    ) -> bool {
        self.entries.iter().any(|entry| {
            entry.computed
                && entry.frame == frame
                && entry.window == window
                && entry.dependencies_valid()
        })
    }

    pub(super) fn insert_computed(
        &mut self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        retained: RetainedWindowMatrix,
        faces: PreparedFaceSnapshot,
        reads: CollectionReads,
        source_roots: Vec<neovm_core::emacs_core::owned_roots::OwnedRoots>,
        complete_viewport: bool,
    ) {
        // Retargeting can revisit a page that is already prepared. A preview
        // must never shorten its still-valid coverage while the rest is being
        // captured again. Freshness and geometry must match before retaining
        // the old source roots and face namespace.
        if !complete_viewport
            && self.entries.iter().any(|entry| {
                entry.computed
                    && entry.complete_viewport
                    && entry.frame == frame
                    && entry.window == window
                    && entry.retained.key.window_start == retained.key.window_start
                    && RetainedWindowKey::row_content_eligible(&entry.retained.key, &retained.key)
                    && entry.dependencies_valid()
            })
        {
            return;
        }
        let rows = retained.matrix.rows.len();
        let glyphs = retained
            .matrix
            .rows
            .iter()
            .flat_map(|row| &row.glyphs)
            .map(Vec::len)
            .sum();
        if rows > MAX_ROWS || glyphs > MAX_GLYPHS {
            return;
        }
        self.entries.retain(|entry| {
            entry.frame != frame
                || entry.window != window
                || entry.retained.key.window_start != retained.key.window_start
        });
        self.entries.push_back(PreparedViewport {
            frame,
            window,
            retained,
            faces,
            rows,
            glyphs,
            computed: true,
            complete_viewport,
            reads: Some(reads),
            _source_roots: source_roots,
        });
        self.trim();
    }

    fn trim(&mut self) {
        while self.entries.len() > MAX_VIEWPORTS
            || self.entries.iter().map(|entry| entry.rows).sum::<usize>() > MAX_ROWS
            || self.entries.iter().map(|entry| entry.glyphs).sum::<usize>() > MAX_GLYPHS
        {
            self.entries.pop_front();
        }
    }

    pub(super) fn replay(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        key: &RetainedWindowKey,
        force_start: bool,
    ) -> Option<(CursorOnlyReplay, PreparedFaceSnapshot)> {
        self.entries.iter().rev().find_map(|entry| {
            if !entry.complete_viewport
                || !entry.dependencies_valid()
                || entry.frame != frame
                || entry.window != window
                || entry.retained.key.window_start != key.window_start
            {
                return None;
            }
            let replay = entry
                .retained
                .cursor_only_replay_with_forced_start(key, force_start)
                .ok()?;
            // The cursor builder deliberately leaves chrome empty. Even when
            // revisiting a page, mode-line Lisp must run against today's state.
            Some((replay, entry.faces.clone()))
        })
    }

    /// Complete a small forward scroll from retained prefix rows and a worker
    /// page beginning inside that prefix. The overlap proves the source seam;
    /// placement uses measured heights, never a default-font row estimate.
    pub(super) fn complete_forward_scroll(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        key: &RetainedWindowKey,
        prefix: &ScrollReplay,
        geometry: crate::buffer_source::window_geometry::BufferWindowGeometry,
        arena: &FrameFaceArena,
        force_start: bool,
    ) -> Option<(CursorOnlyReplay, PreparedFaceSnapshot)> {
        if prefix.sync.is_some() || prefix.edit || prefix.face_generation != arena.generation() {
            return None;
        }
        self.entries.iter().rev().find_map(|entry| {
            if !entry.dependencies_valid()
                || !entry.computed
                || entry.frame != frame
                || entry.window != window
                || entry.retained.key.window_start <= key.window_start
                || !RetainedWindowKey::row_content_eligible(&entry.retained.key, key)
            {
                return None;
            }
            let seam = entry.retained.key.window_start as usize;
            let above: Vec<_> = prefix
                .reused_rows
                .iter()
                .take_while(|(_, row)| row.start_charpos < seam)
                .collect();
            let (last_index, last_row) = *above.last()?;
            if last_row.next_buffer_row_start()? != seam {
                return None;
            }
            let mut candidate = entry.retained.clone();
            candidate.key.window_start = key.window_start;
            candidate.key.vscroll = key.vscroll;
            candidate.matrix.resize(
                geometry.display_text_row_base + geometry.max_rows,
                candidate.matrix.ncols,
            );
            candidate.presented_cursor = None;
            let snapshot = &mut candidate.display_snapshot;
            snapshot.logical_cursor = None;
            snapshot.phys_cursor = None;
            snapshot.layout_freshness = None;
            snapshot.window_end_record = None;
            snapshot.rows = prefix
                .reused_row_snapshots
                .iter()
                .filter(|row| row.row <= *last_index as i64)
                .cloned()
                .collect();
            snapshot.points = prefix
                .reused_points
                .iter()
                .filter(|point| point.row <= *last_index as i64)
                .cloned()
                .collect();
            for row in &mut candidate.matrix.rows {
                if !RetainedWindowMatrix::is_chrome_role(row.role) {
                    let mut disabled = neomacs_display_protocol::glyph_matrix::GlyphRow::new(
                        neomacs_display_protocol::frame_glyphs::GlyphRowRole::Text,
                    );
                    disabled.enabled = false;
                    *row = neomacs_display_protocol::glyph_matrix::MatrixRow::new(disabled);
                }
            }
            for (index, row) in above {
                // A fractional offset can reduce the destination's row
                // capacity while the retained prefix still includes its old
                // bottom row. Such a prefix cannot be completed here.
                let destination = candidate.matrix.rows.get_mut(*index)?;
                if RetainedWindowMatrix::is_chrome_role(destination.role) {
                    return None;
                }
                *destination = row.clone();
            }
            let mut index = last_index + 1;
            let mut y = last_row.pixel_y + last_row.height_px;
            let bottom = snapshot.regions.text_body.bottom() - snapshot.regions.outer.y;
            let mut next = seam;
            let mut remap = rustc_hash::FxHashMap::default();
            for (source_index, row) in entry
                .retained
                .matrix
                .rows
                .iter()
                .enumerate()
                .filter(|(_, row)| row.enabled && !RetainedWindowMatrix::is_chrome_role(row.role))
            {
                if row.start_charpos != next {
                    return None;
                }
                if y >= bottom || index >= geometry.display_text_row_base + geometry.max_rows {
                    break;
                }
                let destination = candidate.matrix.rows.get_mut(index)?;
                if RetainedWindowMatrix::is_chrome_role(destination.role) {
                    return None;
                }
                let dy = y - row.pixel_y;
                let mut shifted = row.as_ref().clone();
                shifted.pixel_y = y;
                shifted.cursor_col = None;
                shifted.cursor_type = None;
                *destination = neomacs_display_protocol::glyph_matrix::MatrixRow::new(shifted);
                remap.insert(source_index as i64, (index as i64, dy.round() as i64));
                next = row.next_buffer_row_start()?;
                y += row.height_px;
                index += 1;
            }
            // Any row intersecting the body is laid out in full, then clipped.
            if (y < bottom && index < geometry.display_text_row_base + geometry.max_rows)
                || index == last_index + 1
            {
                return None;
            }
            snapshot.rows.extend(
                entry
                    .retained
                    .display_snapshot
                    .rows
                    .iter()
                    .filter_map(|row| {
                        let &(index, dy) = remap.get(&row.row)?;
                        let mut row = row.clone();
                        row.row = index;
                        row.y += dy;
                        Some(row)
                    }),
            );
            snapshot
                .points
                .extend(
                    entry
                        .retained
                        .display_snapshot
                        .points
                        .iter()
                        .filter_map(|point| {
                            let &(index, dy) = remap.get(&point.row)?;
                            let mut point = point.clone();
                            point.row = index;
                            point.y += dy;
                            Some(point)
                        }),
                );
            let replay = candidate
                .cursor_only_replay_with_forced_start(key, force_start)
                .ok()?;
            Some((replay, arena.prepared_with_retained(&entry.faces).ok()?))
        })
    }

    /// Reuse coverage from an older viewport when it supplies more rows than
    /// the immediately preceding presentation. In particular, a backward
    /// scroll can be a forward replay within an older prepared page.
    pub(super) fn scroll_replay(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        key: &RetainedWindowKey,
        minimum_reused: usize,
    ) -> Option<(ScrollReplay, PreparedFaceSnapshot)> {
        self.entries.iter().rev().find_map(|entry| {
            if !entry.dependencies_valid()
                || entry.frame != frame
                || entry.window != window
                || entry.retained.key.window_start >= key.window_start
            {
                return None;
            }
            // Avoid allocating shifted glyph rows unless this entry can beat
            // the current plan. The canonical replay builder still decides
            // eligibility, including row boundaries, fringes and wrapping.
            let potential = entry
                .retained
                .matrix
                .rows
                .iter()
                .filter(|row| {
                    row.enabled
                        && !RetainedWindowMatrix::is_chrome_role(row.role)
                        && row.start_charpos as i64 >= key.window_start
                })
                .count();
            if potential <= minimum_reused {
                return None;
            }
            let mut replay = entry.retained.scroll_replay(key)?;
            if !entry.complete_viewport {
                // The synchronous walk must fill the rest of the viewport,
                // not merely replace the rows shifted out of this prefix.
                replay.exposed_row_count = entry
                    .retained
                    .matrix
                    .rows
                    .len()
                    .checked_sub(replay.exposed_row_base)?;
            }
            (replay.reused_rows.len() > minimum_reused).then(|| (replay, entry.faces.clone()))
        })
    }

    pub(super) fn accept(
        &mut self,
        frame: neovm_core::window::FrameId,
        previous: rustc_hash::FxHashMap<DisplayWindowId, RetainedWindowMatrix>,
        next: &rustc_hash::FxHashMap<DisplayWindowId, RetainedWindowMatrix>,
        faces: Option<&FrameFaceArena>,
    ) {
        self.retire_invalid_dependencies();
        self.entries.retain(|entry| {
            if entry.frame != frame {
                return true;
            }
            next.get(&entry.window).is_some_and(|current| {
                if entry.computed {
                    RetainedWindowKey::row_content_eligible(&entry.retained.key, &current.key)
                } else {
                    let delta = WindowDelta::between(&entry.retained.key, &current.key);
                    !delta.text_changed
                        && !delta.properties_changed
                        && !delta.overlays_changed
                        && !delta.other_changed
                        && delta.window_start_moved
                }
            })
        });
        let Some(faces) = faces else {
            return;
        };
        for (window, retained) in previous {
            let Some(current) = next.get(&window) else {
                continue;
            };
            if retained.validity != MatrixValidity::Valid
                || !RetainedWindowKey::scroll_eligible(&retained.key, &current.key)
            {
                continue;
            }
            let rows = retained.matrix.rows.len();
            let glyphs = retained
                .matrix
                .rows
                .iter()
                .flat_map(|row| row.glyphs.iter())
                .map(Vec::len)
                .sum::<usize>();
            if rows > MAX_ROWS || glyphs > MAX_GLYPHS {
                continue;
            }
            self.entries.retain(|entry| {
                entry.frame != frame
                    || entry.window != window
                    || entry.retained.key.window_start != retained.key.window_start
            });
            self.entries.push_back(PreparedViewport {
                frame,
                window,
                retained,
                faces: faces.prepared_snapshot(),
                rows,
                glyphs,
                computed: false,
                complete_viewport: true,
                reads: None,
                _source_roots: Vec::new(),
            });
        }
        self.trim();
    }
}
