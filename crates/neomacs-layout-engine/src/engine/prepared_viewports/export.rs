//! Export certified contiguous rows without publishing speculative query data.

use super::*;
use neomacs_display_protocol::glyph_matrix::{GlyphMatrix, MatrixRow};
use neomacs_display_protocol::scroll_coverage::ScrollCoverage;
use neomacs_display_protocol::{
    FrameDisplayState, FrameRect, GlyphRowRole, PresentedHitIndex, PresentedHitRegion,
    PresentedRegionKind, Rect,
};
use neovm_core::window::{PresentedBodyRowSnapshot, WindowDisplaySnapshot};
use std::sync::Arc;

impl PreparedViewports {
    pub(in crate::engine) fn export(
        &mut self,
        frame: neovm_core::window::FrameId,
        retained: &rustc_hash::FxHashMap<DisplayWindowId, RetainedWindowMatrix>,
        state: &mut FrameDisplayState,
        arena: &FrameFaceArena,
        metrics: &mut Option<crate::font::metrics::FontMetricsService>,
    ) {
        self.retire_invalid_dependencies();
        self.export_epochs.retain(|(old_frame, window, _, _)| {
            *old_frame == frame && retained.contains_key(window)
        });
        for (window, current) in retained {
            let epoch = match self
                .export_epochs
                .iter_mut()
                .find(|(f, w, _, _)| *f == frame && w == window)
            {
                Some((_, _, key, epoch))
                    if RetainedWindowKey::row_content_eligible(key, &current.key) =>
                {
                    *epoch
                }
                Some((_, _, key, epoch)) => {
                    *key = current.key.clone();
                    *epoch = next_epoch();
                    *epoch
                }
                None => {
                    let epoch = next_epoch();
                    self.export_epochs
                        .push((frame, *window, current.key.clone(), epoch));
                    epoch
                }
            };
            if let Some(coverage) =
                self.export_window(frame, *window, current, state, arena, metrics, epoch)
            {
                state.scroll_coverage.push(Arc::new(coverage));
            }
        }
    }

    fn export_window(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        current: &RetainedWindowMatrix,
        state: &FrameDisplayState,
        arena: &FrameFaceArena,
        metrics: &mut Option<crate::font::metrics::FontMetricsService>,
        epoch: u64,
    ) -> Option<ScrollCoverage> {
        let entries: Vec<_> = self
            .entries
            .iter()
            .filter(|entry| {
                entry.computed
                    && entry.dependencies_valid()
                    && entry.frame == frame
                    && entry.window == window
                    && RetainedWindowKey::row_content_eligible(&entry.retained.key, &current.key)
            })
            .collect();
        // Build an extra scroll surface only when it adds prepared content.
        // A current-only surface repeats resource and hit-index work on every
        // frame for just the clipped remainder of the visible bottom row.
        if entries.is_empty() {
            return None;
        }
        let original = state
            .window_matrices
            .iter()
            .find(|entry| entry.window_id == window)?;
        let body: Vec<_> = current
            .matrix
            .rows
            .iter()
            .enumerate()
            .filter(|(_, row)| row.enabled && row.role == GlyphRowRole::Text)
            .collect();
        let (_, anchor) = *body.first()?;
        let mut rows = std::collections::BTreeMap::new();
        let mut attempt = arena.begin_attempt();
        attempt
            .admit_prepared(
                body.iter().flat_map(|(_, row)| scroll_row_faces(row)),
                &arena.prepared_snapshot(),
                arena,
            )
            .ok()?;
        // Complete physical rows and certified visual continuations may extend this surface.
        // Source continuity is proved again after selecting the connected set.
        for (index, row) in &body {
            if rows.insert(row.start_charpos, (current, *index)).is_some() {
                return None;
            }
        }
        for entry in entries {
            attempt
                .admit_prepared(
                    entry
                        .retained
                        .matrix
                        .rows
                        .iter()
                        .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
                        .flat_map(|row| scroll_row_faces(row)),
                    &entry.faces,
                    arena,
                )
                .ok()?;
            for (index, row) in entry
                .retained
                .matrix
                .rows
                .iter()
                .enumerate()
                .filter(|(_, row)| row.enabled && row.role == GlyphRowRole::Text)
            {
                if let Some((source, old_index)) = rows.get(&row.start_charpos) {
                    let old = &source.matrix.rows[*old_index];
                    if old.height_px != row.height_px
                        || old.end_charpos != row.end_charpos
                        || old.continued != row.continued
                    {
                        return None;
                    }
                }
                rows.insert(row.start_charpos, (&entry.retained, index));
            }
        }
        let all: Vec<_> = rows.into_values().collect();
        let anchor_index = all.iter().position(|(source, index)| {
            source.matrix.rows[*index].start_charpos == anchor.start_charpos
        })?;
        let adjacent = |left: usize, right: usize| {
            let (a, ai) = all[left];
            let (b, bi) = all[right];
            a.matrix.rows[ai].next_buffer_row_start() == Some(b.matrix.rows[bi].start_charpos)
        };
        let mut begin = anchor_index;
        while begin > 0 && adjacent(begin - 1, begin) {
            begin -= 1;
        }
        let mut end = anchor_index + 1;
        while end < all.len() && adjacent(end - 1, end) {
            end += 1;
        }
        if end - begin > 192 {
            return None;
        }
        let mut y = anchor.pixel_y
            - all[begin..anchor_index]
                .iter()
                .map(|(source, index)| source.matrix.rows[*index].height_px)
                .sum::<f32>();
        let top = original.text_pixel_bounds.y + y;
        let origin = -top;
        let mut matrix = GlyphMatrix::new(end - begin, current.matrix.ncols);
        let mut snapshot = WindowDisplaySnapshot::default();
        snapshot.regions = current.display_snapshot.regions;
        // Group points once per source, preserving per-face hit-test heights.
        let mut points = rustc_hash::FxHashMap::<(usize, i64), Vec<_>>::default();
        for (source, _) in &all[begin..end] {
            let identity = *source as *const RetainedWindowMatrix as usize;
            if points.contains_key(&(identity, -1)) {
                continue;
            }
            points.insert((identity, -1), Vec::new());
            for point in &source.display_snapshot.points {
                points.entry((identity, point.row)).or_default().push(point);
            }
        }
        for (output_row, (source, index)) in all[begin..end].iter().enumerate() {
            let original_row = &source.matrix.rows[*index];
            let mut row = original_row.as_ref().clone();
            row.pixel_y = y + origin;
            row.cursor_col = None;
            row.cursor_type = None;
            let mut row_snapshot = source
                .display_snapshot
                .rows
                .iter()
                .find(|row| row.row == *index as i64)?
                .clone();
            row_snapshot.row = output_row as i64;
            row_snapshot.y = y.round() as i64;
            snapshot.rows.push(row_snapshot);
            snapshot.body_rows.push(PresentedBodyRowSnapshot {
                output_row: output_row as i64,
                body_row: output_row as i64 - (anchor_index - begin) as i64,
                body_y: (original.text_pixel_bounds.y + y - top).round() as i64,
            });
            let identity = *source as *const RetainedWindowMatrix as usize;
            for point in points.get(&(identity, *index as i64)).into_iter().flatten() {
                let mut point = (*point).clone();
                point.row = output_row as i64;
                point.y = y.round() as i64;
                snapshot.points.push(point);
            }
            y += row.height_px;
            matrix.rows[output_row] = MatrixRow::new(row);
        }
        let viewport = current.display_snapshot.regions.text_body;
        let bounds = Rect::new(
            viewport.x,
            top,
            viewport.width,
            original.text_pixel_bounds.y + y - top,
        );
        if top > viewport.y
            || bounds.bottom() < viewport.bottom()
            || (top == viewport.y && bounds.bottom() == viewport.bottom())
        {
            return None;
        }
        let bounds = Rect::new(bounds.x, 0.0, bounds.width, bounds.height);
        snapshot.regions.text_body = bounds;
        let positions =
            crate::presentation::spatial::body_text_positions(window, &snapshot, bounds).ok()?;
        let hit_index = PresentedHitIndex::from_parts(
            state.presentation_id,
            vec![PresentedHitRegion::new(
                Some(window),
                PresentedRegionKind::TextBody,
                FrameRect::new(bounds.x, bounds.y, bounds.width, bounds.height).ok()?,
                0,
            )],
            positions,
        )
        .ok()?;
        let mut content = original.clone();
        content.matrix = matrix;
        content.text_clip_bounds = Some(bounds);
        let mut fonts = FrameDisplayState::new(0, 0, state.char_width, state.char_height);
        fonts.font_catalog_generation = state.font_catalog_generation;
        fonts.font_pixel_size = state.font_pixel_size;
        fonts.faces = attempt.faces();
        fonts.window_matrices.push(content.clone());
        crate::font::metrics::realize_frame_fonts(&mut fonts, metrics);
        let pointer_source = crate::presentation::pointer::window_pointer_source_map(&fonts).ok()?;
        Some(ScrollCoverage {
            epoch,
            predict_pixels: false,
            compositor_enabled: false,
            anchor_row: anchor_index - begin,
            viewport,
            origin,
            content,
            faces: fonts.faces,
            fonts: fonts.fonts,
            char_fonts: fonts.char_fonts,
            shaped_clusters: fonts.shaped_clusters,
            hit_index,
            pointer_source,
        })
    }
}

fn next_epoch() -> u64 {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

fn scroll_row_faces(
    row: &neomacs_display_protocol::GlyphRow,
) -> impl Iterator<Item = neomacs_display_protocol::FaceId> + '_ {
    row.referenced_face_ids().chain(
        [
            row.left_fringe_bitmap,
            row.right_fringe_bitmap,
            row.overlay_arrow_bitmap,
        ]
        .into_iter()
        .flatten()
        .map(|bitmap| bitmap.face_id),
    )
}
