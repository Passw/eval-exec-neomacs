//! Evaluator ownership of worker admission. Tickets route a result to its
//! captured full key; the ordinary retained-row validator grants reuse.

use super::*;
use crate::buffer_source::face_resolution::BufferSourceFaceResolutionContext;
use crate::buffer_source::owned_capture::capture_physical_line;
use crate::display_row::face_state::{
    DisplayRowFaceRealizer, DisplayRowGlyphMeasurer, DisplayRowMeasurementMode,
    DisplayRowMeasurementPolicy, stable_face_id_for_resolved,
};
use crate::display_row::metrics::DisplayRowFallbackMetrics;
use crate::frame_face_arena::{FrameFaceAttempt, PreparedFaceSnapshot};
use crate::glyph_advance::GlyphAdvanceQuantization;
use crate::neovm_bridge::{BorrowedLayoutBuffer, LayoutBufferView, LayoutVar};
use crate::row_layout::program::{
    RowProgram, RowProgramError, RowProgramGeometry, RowProgramLimits,
};
use crate::row_layout::worker::{RowJobTicket, RowWorker};
use crate::window_output::prepared_body::position_buffer_rows;
use neovm_core::buffer::CharPos0;
use neovm_core::tagged::collection_reads::{self, CollectionReads};
use neovm_core::window::{FrameId, WindowId};

/// Keep speculative work bounded, including cyclic user lists. Recheck at
/// admission because custom marker variables need not bump display ticks.
pub(super) fn inactive_overlay_arrows(evaluator: &neovm_core::emacs_core::Context) -> bool {
    let mut tail = evaluator
        .obarray()
        .symbol_value("overlay-arrow-variable-list")
        .copied()
        .unwrap_or(Value::NIL);
    for _ in 0..32 {
        if !tail.is_cons() {
            return true;
        }
        if let Some(sym) = tail.cons_car().as_symbol_id()
            && evaluator
                .obarray()
                .symbol_value_id(sym)
                .is_some_and(|value| !value.is_nil())
        {
            return false;
        }
        tail = tail.cons_cdr();
    }
    !tail.is_cons()
}

struct Admission {
    reads: CollectionReads,
    frame: FrameId,
    window: DisplayWindowId,
    ticket: RowJobTicket,
    retained: RetainedWindowMatrix,
    faces: PreparedFaceSnapshot,
    row_base: usize,
}

struct Capture {
    reads: Option<CollectionReads>,
    frame: FrameId,
    window: WindowId,
    retained: RetainedWindowMatrix,
    attempt: FrameFaceAttempt,
    faces: Option<PreparedFaceSnapshot>,
    row_base: usize,
    row_count: usize,
    position: CharPos0,
    programs: Vec<RowProgram>,
    font_snapshots:
        Vec<std::sync::Arc<crate::row_layout::font_measurement::FontMeasurementSnapshot>>,
    font_bytes: usize,
}

struct WindowCoverage {
    key: RetainedWindowKey,
    targets: Vec<CharPos0>,
}

#[derive(Default)]
pub(super) struct ScrollCoverage {
    worker: RowWorker,
    admission: Option<Admission>,
    capture: Option<Capture>,
    frame: Option<FrameId>,
    // At most two targets per retained live window. Glyph storage remains
    // subject to PreparedViewports' global byte/row limits.
    windows: rustc_hash::FxHashMap<DisplayWindowId, WindowCoverage>,
    last_window: Option<DisplayWindowId>,
    publication_pending: bool,
}

impl ScrollCoverage {
    pub(super) fn cancel(&mut self) {
        self.cancel_active();
        self.frame = None;
        self.windows.clear();
        self.last_window = None;
    }

    fn cancel_active(&mut self) {
        self.worker.cancel();
        self.admission = None;
        self.capture = None;
    }

    fn active_window(&self) -> Option<DisplayWindowId> {
        self.capture
            .as_ref()
            .map(|capture| DisplayWindowId::new(capture.window.0 as i64))
            .or_else(|| self.admission.as_ref().map(|admission| admission.window))
    }

    pub(super) fn drain(
        &mut self,
        destination: &mut PreparedViewports,
    ) -> Result<bool, RowProgramError> {
        let Some(result) = self.worker.take_completed() else {
            return Ok(false);
        };
        let Some(mut admission) = self.admission.take() else {
            return Err(RowProgramError::Cancelled);
        };
        if admission.ticket != result.ticket
            || admission.retained.key.fontset_generation
                != neovm_core::emacs_core::fontset::fontset_generation()
            || !admission.reads.unchanged()
        {
            return Err(RowProgramError::Cancelled);
        }
        let mut rows = result.rows?;
        let regions = &admission.retained.display_snapshot.regions;
        let mut height = 0.0;
        let visible = rows
            .iter()
            .take_while(|row| {
                let take = height < regions.text_body.height;
                if take {
                    height += row.row.height_px;
                }
                take
            })
            .count();
        let complete_viewport = height >= regions.text_body.height
            || rows.len() == admission.retained.matrix.rows.len() - admission.row_base;
        rows.truncate(visible);
        let body = position_buffer_rows(
            rows,
            admission.row_base,
            regions.text_body.x,
            regions.text_body.y,
            regions.outer.y,
            admission.window.get() as u64,
            regions.outer,
            admission.retained.matrix.ncols,
        )?;
        let mut rows = body.glyph_rows.into_iter();
        for (index, row) in admission.retained.matrix.rows.iter_mut().enumerate() {
            if index < admission.row_base || RetainedWindowMatrix::is_chrome_role(row.role) {
                continue;
            }
            let computed = rows.next().unwrap_or_else(|| {
                let mut row = neomacs_display_protocol::glyph_matrix::GlyphRow::new(
                    neomacs_display_protocol::frame_glyphs::GlyphRowRole::Text,
                );
                row.enabled = false;
                row
            });
            *row = neomacs_display_protocol::glyph_matrix::MatrixRow::new(computed);
        }
        if rows.next().is_some() {
            return Err(RowProgramError::Unsupported);
        }
        let snapshot = &mut admission.retained.display_snapshot;
        snapshot.points = body.geometry.points;
        snapshot.rows = body.geometry.rows;
        snapshot.body_rows = body.geometry.body_rows;
        snapshot.logical_cursor = None;
        snapshot.phys_cursor = None;
        // Prepared geometry is private coverage, never a query certificate.
        // Redisplay reconstructs these fields after full-key admission.
        snapshot.layout_freshness = None;
        snapshot.window_end_record = None;
        admission.retained.presented_cursor = None;
        self.publication_pending = true;
        destination.insert_computed(
            admission.frame,
            admission.window,
            admission.retained,
            admission.faces,
            admission.reads,
            complete_viewport,
        );
        Ok(true)
    }
}

impl LayoutEngine {
    /// A completed idle page needs one fresh immutable transport publication.
    pub fn take_scroll_coverage_publication(&mut self) -> bool {
        std::mem::take(&mut self.scroll_coverage.publication_pending)
    }

    /// Perform one bounded off-screen acquisition step during a command-loop
    /// idle wait. Never evaluate Lisp, publish a viewport, or wait for a worker.
    pub fn maintain_scroll_coverage(
        &mut self,
        evaluator: &neovm_core::emacs_core::Context,
    ) -> Option<std::time::Duration> {
        use std::time::Duration;
        self.prepared_viewports.retire_invalid_dependencies();
        if !self.prepared_viewports.invalidated.is_empty() {
            // Retire the old render-thread certificate even before a new
            // worker page is ready. Cache eviction alone cannot revoke it.
            self.scroll_coverage.publication_pending = true;
        }
        for (_, window) in self.prepared_viewports.invalidated.drain(..) {
            self.scroll_coverage.windows.remove(&window);
        }
        let frame = evaluator.frame_manager().selected_frame()?;
        if self.retained_frame != Some(frame.id) || self.font_metrics.is_none() {
            self.scroll_coverage.cancel();
            return None;
        }
        if self.scroll_coverage.frame != Some(frame.id) {
            self.scroll_coverage.cancel();
            self.scroll_coverage.frame = Some(frame.id);
        }
        // Remove deleted/replaced window owners before consulting in-flight
        // work. A split or selection change must not cancel another window's
        // still-valid page.
        let mut owners: Vec<_> = self
            .retained_window_matrices
            .keys()
            .copied()
            .filter(|owner| {
                let window = WindowId(owner.get() as u64);
                frame.minibuffer_window != Some(window)
                    && frame
                        .find_window(window)
                        .and_then(|window| window.buffer_id())
                        .is_some()
            })
            .collect();
        owners.sort_unstable_by_key(|owner| owner.get());
        self.scroll_coverage.windows.retain(|owner, _| {
            owners
                .binary_search_by_key(&owner.get(), |owner| owner.get())
                .is_ok()
        });
        if self.scroll_coverage.active_window().is_some_and(|owner| {
            owners
                .binary_search_by_key(&owner.get(), |owner| owner.get())
                .is_err()
        }) {
            self.scroll_coverage.cancel_active();
        }
        // One worker owns a whole bounded page. Between pages, round-robin
        // through pending/changed windows, starting with the selected one.
        let start = self
            .scroll_coverage
            .last_window
            .map(|last| owners.partition_point(|owner| owner.get() <= last.get()))
            .unwrap_or_else(|| {
                owners
                    .iter()
                    .position(|owner| owner.get() == frame.selected_window.0 as i64)
                    .unwrap_or(0)
            });
        if !owners.is_empty() {
            let count = owners.len();
            owners.rotate_left(start % count);
        }
        let window_id = self.scroll_coverage.active_window().or_else(|| {
            owners.iter().copied().find(|owner| {
                let retained = &self.retained_window_matrices[owner];
                self.scroll_coverage
                    .windows
                    .get(owner)
                    .is_none_or(|observed| {
                        !observed.targets.is_empty()
                            || !RetainedWindowKey::row_content_eligible(
                                &observed.key,
                                &retained.key,
                            )
                            || observed.key.window_start != retained.key.window_start
                    })
            })
        })?;
        let window = WindowId(window_id.get() as u64);
        let retained = self.retained_window_matrices.get(&window_id)?;
        let compatible = self
            .scroll_coverage
            .windows
            .get(&window_id)
            .is_some_and(|observed| {
                RetainedWindowKey::row_content_eligible(&observed.key, &retained.key)
            });
        let moved = self
            .scroll_coverage
            .windows
            .get(&window_id)
            .is_some_and(|observed| observed.key.window_start != retained.key.window_start);
        if !compatible {
            self.scroll_coverage.cancel_active();
        }
        if !compatible || moved {
            // Placement-only changes retarget future work without starving
            // the page already being captured during continuous scrolling.
            let mut targets = Vec::with_capacity(2);
            let buffer = evaluator
                .buffer_manager()
                .get(neovm_core::buffer::BufferId(retained.key.buffer_id))?;
            let rows: Vec<_> = retained
                .matrix
                .rows
                .iter()
                .filter(|row| row.enabled && !RetainedWindowMatrix::is_chrome_role(row.role))
                .collect();
            // Backward acquisition scans at most 8 KiB, regardless of buffer
            // length. It stops only on a complete physical-line boundary.
            let start = CharPos0::new(retained.key.window_start.max(0) as usize);
            let start_byte = buffer.char_pos_to_emacs_byte_pos_clamped(start).get();
            let begin = buffer.point_min_emacs_byte_pos().get();
            let lower = start_byte.saturating_sub(8192).max(begin);
            let mut position = start_byte;
            let mut lines = 0;
            while position > lower {
                position -= 1;
                if buffer.emacs_byte_at_pos(neovm_core::buffer::EmacsBytePos::new(position))
                    == Some(b'\n')
                {
                    lines += 1;
                    if lines > rows.len().saturating_sub(2).max(1) {
                        position += 1;
                        break;
                    }
                }
            }
            if position < start_byte
                && (position == begin || lines > rows.len().saturating_sub(2).max(1))
            {
                targets.push(buffer.emacs_byte_pos_to_char_pos_clamped(
                    neovm_core::buffer::EmacsBytePos::new(position),
                ));
            }
            // Two-row overlap matches the usual page movement and also leaves
            // reusable rows for smaller wheel motions into the next page.
            if let Some(row) = rows.get(rows.len().saturating_sub(2)) {
                let forward = CharPos0::new(row.start_charpos);
                if forward > start {
                    targets.push(forward);
                }
            }
            self.scroll_coverage.windows.insert(
                window_id,
                WindowCoverage {
                    key: retained.key.clone(),
                    targets,
                },
            );
        }
        match self.scroll_coverage.drain(&mut self.prepared_viewports) {
            Ok(true) => {
                tracing::debug!(target: "neomacs_layout_engine::scroll_coverage", "worker page ready");
                self.scroll_coverage.last_window = Some(window_id);
                return Some(Duration::from_millis(1));
            }
            Err(error) => {
                tracing::debug!(target: "neomacs_layout_engine::scroll_coverage", ?error, "worker page rejected");
                self.scroll_coverage.last_window = Some(window_id);
                return Some(Duration::from_millis(1));
            }
            Ok(false) => {}
        }
        if self.scroll_coverage.admission.is_some() {
            return Some(Duration::from_millis(4));
        }
        if self.scroll_coverage.capture.is_none() {
            let Some(start) = self
                .scroll_coverage
                .windows
                .get_mut(&window_id)?
                .targets
                .pop()
            else {
                self.scroll_coverage.last_window = Some(window_id);
                return Some(Duration::from_millis(1));
            };
            if let Err(error) = self.begin_scroll_coverage(evaluator, frame.id, window, start) {
                tracing::debug!(target: "neomacs_layout_engine::scroll_coverage", ?error, start = start.get(), "capture not eligible");
                self.scroll_coverage.last_window = Some(window_id);
                return Some(Duration::from_millis(1));
            }
        }
        if let Err(error) = self.capture_scroll_step(evaluator) {
            tracing::debug!(target: "neomacs_layout_engine::scroll_coverage", ?error, "row capture rejected");
            self.scroll_coverage.last_window = Some(window_id);
        }
        Some(Duration::from_millis(1))
    }

    /// Capture a bounded unseen page. No live start, point or presentation is
    /// changed. A failure leaves the normal synchronous path authoritative.
    pub(super) fn begin_scroll_coverage(
        &mut self,
        _evaluator: &neovm_core::emacs_core::Context,
        frame: FrameId,
        window: WindowId,
        start: CharPos0,
    ) -> Result<(), RowProgramError> {
        if self.retained_frame != Some(frame) {
            return Err(RowProgramError::Unsupported);
        }
        let window_id = DisplayWindowId::new(window.0 as i64);
        let retained = self
            .retained_window_matrices
            .get(&window_id)
            .ok_or(RowProgramError::Unsupported)?;
        let key = &retained.key;
        if key.hscroll != 0
            || key.selective_display != 0
            || key.show_trailing_whitespace
            || key.display_line_numbers != crate::types::DisplayLineNumbersMode::Off
            || !key.line_prefix.is_empty()
            || !key.wrap_prefix.is_empty()
            || key.indicate_empty_lines != 0
            || key.display_table != Default::default()
            || key.tab_stop_list.len() > 32
        {
            return Err(RowProgramError::Unsupported);
        }
        let row_base = retained
            .matrix
            .rows
            .iter()
            .position(|row| row.enabled && !RetainedWindowMatrix::is_chrome_role(row.role))
            .ok_or(RowProgramError::Unsupported)?;
        // Retained matrices can keep spare rows after geometry changes and
        // incremental walks. Their allocation is not the visible row count.
        // Match the canonical window's row capacity, including its partially
        // visible frontier. Measured rows can stop earlier at the pixel bottom.
        let needed = (retained.display_snapshot.regions.text_body.height / key.char_height).ceil();
        if !needed.is_finite() || needed < 1.0 || needed > 64.0 || row_base > 64 {
            return Err(RowProgramError::Budget);
        }
        let row_count = needed as usize;
        let attempt = self
            .frame_face_arenas
            .get(&frame)
            .ok_or(RowProgramError::Unsupported)?
            .begin_attempt();
        // Copy only fixed window inputs, not spare matrix capacity, old glyphs,
        // points or Lisp chrome strings. This private template is never a
        // synchronous query certificate or a published window presentation.
        let snapshot = &retained.display_snapshot;
        let mut matrix = neomacs_display_protocol::glyph_matrix::GlyphMatrix::new(
            row_base + row_count,
            retained.matrix.ncols,
        );
        matrix.matrix_x = retained.matrix.matrix_x;
        matrix.matrix_y = retained.matrix.matrix_y;
        matrix.header_line = retained.matrix.header_line;
        matrix.tab_line = retained.matrix.tab_line;
        let mut retained = RetainedWindowMatrix {
            matrix,
            key: key.clone(),
            validity: retained.validity,
            display_snapshot: neovm_core::window::WindowDisplaySnapshot {
                window_id: window,
                cell_origin: snapshot.cell_origin,
                regions: snapshot.regions,
                text_area_left_offset: snapshot.text_area_left_offset,
                mode_line_height: snapshot.mode_line_height,
                header_line_height: snapshot.header_line_height,
                tab_line_height: snapshot.tab_line_height,
                ..Default::default()
            },
            presented_cursor: None,
            face_generation: retained.face_generation,
            chrome_uses_column: false,
            chrome_modified_flag: retained.chrome_modified_flag,
            chrome_fingerprints: None,
        };
        retained.key.window_start = start.get() as i64;
        retained.key.vscroll = 0;
        self.scroll_coverage.worker.cancel();
        self.scroll_coverage.admission = None;
        self.scroll_coverage.capture = Some(Capture {
            reads: None,
            frame,
            window,
            retained,
            attempt,
            faces: None,
            row_base,
            row_count,
            position: start,
            programs: Vec::with_capacity(row_count),
            font_snapshots: Vec::new(),
            font_bytes: 0,
        });
        Ok(())
    }

    #[cfg(test)]
    pub(super) fn request_scroll_coverage(
        &mut self,
        evaluator: &neovm_core::emacs_core::Context,
        frame: FrameId,
        window: WindowId,
        start: CharPos0,
    ) -> Result<(), RowProgramError> {
        self.begin_scroll_coverage(evaluator, frame, window, start)?;
        while self.capture_scroll_step(evaluator)? {}
        Ok(())
    }

    /// Consume at most one bounded physical line. All evaluator references
    /// are dropped before returning to the command-input wait loop.
    pub(super) fn capture_scroll_step(
        &mut self,
        evaluator: &neovm_core::emacs_core::Context,
    ) -> Result<bool, RowProgramError> {
        let Some(mut capture) = self.scroll_coverage.capture.take() else {
            return Ok(false);
        };
        let arena = self
            .frame_face_arenas
            .get(&capture.frame)
            .ok_or(RowProgramError::Unsupported)?;
        capture.attempt = match &capture.faces {
            Some(faces) => arena
                .resume_prepared(faces)
                .map_err(|_| RowProgramError::Unsupported)?,
            None => arena.begin_attempt(),
        };
        // Each idle step extends the exact main-thread read certificate.
        // No Lisp values or collection identities are sent to the worker.
        let (result, reads) = collection_reads::capture(|| {
            if capture
                .reads
                .as_ref()
                .is_some_and(|reads| !reads.unchanged_and_observe())
            {
                return Err(RowProgramError::Cancelled);
            }
            self.capture_scroll_row(evaluator, &mut capture)
        });
        // Unsupported source syntax ends this page, not its already complete
        // prefix. Cancellation and a failed dependency certificate still
        // reject the whole page, including work captured in earlier steps.
        let frontier = match result {
            Ok(()) => false,
            Err(
                RowProgramError::Unsupported
                | RowProgramError::Budget
                | RowProgramError::Incomplete,
            ) if !capture.programs.is_empty() => true,
            Err(error) => return Err(error),
        };
        capture.reads = Some(reads.ok_or(RowProgramError::Budget)?);
        // Reserve after every bounded step: an intervening timer redisplay
        // may seal another arena generation before the next idle wake.
        let faces = self
            .frame_face_arenas
            .get_mut(&capture.frame)
            .ok_or(RowProgramError::Unsupported)?
            .reserve_prepared(&capture.attempt)
            .map_err(|_| RowProgramError::Unsupported)?;
        if !frontier && capture.programs.len() < capture.row_count {
            capture.faces = Some(faces);
            self.scroll_coverage.capture = Some(capture);
            return Ok(true);
        }
        tracing::debug!(target: "neomacs_layout_engine::scroll_coverage", start = capture.retained.key.window_start, rows = capture.programs.len(), "submitting unseen page");
        let ticket = self.scroll_coverage.worker.submit(capture.programs)?;
        self.scroll_coverage.admission = Some(Admission {
            reads: capture.reads.ok_or(RowProgramError::Unsupported)?,
            frame: capture.frame,
            window: DisplayWindowId::new(capture.window.0 as i64),
            ticket,
            retained: capture.retained,
            faces,
            row_base: capture.row_base,
        });
        Ok(false)
    }

    fn capture_scroll_row(
        &mut self,
        evaluator: &neovm_core::emacs_core::Context,
        capture: &mut Capture,
    ) -> Result<(), RowProgramError> {
        let frame = capture.frame;
        let window = capture.window;
        let key = &capture.retained.key;
        let frame_data = evaluator
            .frame_manager()
            .get(frame)
            .ok_or(RowProgramError::Unsupported)?;
        let ws = frame_data
            .effective_window_system()
            .and_then(|value| value.as_symbol_name().map(str::to_owned));
        if ws.is_none() {
            return Err(RowProgramError::Unsupported);
        }
        let bootstrap = crate::neovm_bridge::frame_params_from_neovm(
            frame_data,
            evaluator.face_table(),
            evaluator.obarray(),
        );
        let resolver = crate::neovm_bridge::FaceResolver::new_with_font_sizing(
            evaluator.face_table(),
            0xffffff,
            bootstrap.background,
            key.font_pixel_size,
            ws,
            self.font_sizing,
        );
        resolver.set_current_window_parameters(
            evaluator.frame_manager().window_parameters_pairs(window),
        );
        resolver.set_current_window_id(Some(window.0));
        let base_id = stable_face_id_for_resolved(&mut capture.attempt, resolver.default_face());
        if self.font_metrics.is_none() {
            return Err(RowProgramError::Unsupported);
        }
        let default_face = DisplayRowFaceRealizer::new(&mut self.font_metrics).realize_face(
            base_id,
            resolver.default_face(),
            key.char_width,
            key.char_height,
            key.char_height,
        );
        let metrics = DisplayRowFallbackMetrics::from_default_face_extents(
            key.char_width,
            key.char_height,
            default_face.metrics.ascent_px(),
        );
        let buffer_id = neovm_core::buffer::BufferId(key.buffer_id);
        let buffer = evaluator
            .buffer_manager()
            .get(buffer_id)
            .ok_or(RowProgramError::Unsupported)?;
        // A command or timer may have run between idle capture steps. Never
        // combine measurements from different buffer/display revisions into
        // one job, even though final replay also validates the complete key.
        if neovm_core::emacs_core::symbol::SymbolPropertyRevision::current()
            != key.symbol_property_revision
            || neovm_core::emacs_core::fontset::fontset_generation() != key.fontset_generation
            || buffer.chars_modified_tick() != key.chars_modified_tick
            || buffer.props_modified_tick() != key.props_modified_tick
            || buffer.overlay_modified_tick() != key.overlay_modified_tick
            || buffer.point_max_char_pos().get() as i64 != key.buffer_size
            || buffer.point_min_char_pos().get() as i64 != key.buffer_begv
            || evaluator.face_change_count != key.face_change_count
            || evaluator.display_var_change_count != key.display_var_change_count
            || evaluator.media_generation() != key.media_generation
            || evaluator
                .frame_manager()
                .get(frame)
                .and_then(|frame| frame.find_window(window))
                .and_then(|window| window.buffer_id())
                != Some(buffer_id)
        {
            return Err(RowProgramError::Cancelled);
        }
        let view = BorrowedLayoutBuffer::for_window(
            buffer,
            evaluator.obarray(),
            capture.position,
            128,
            crate::display_property::DisplayPropertyTarget::Graphical,
        );
        if !inactive_overlay_arrows(evaluator) {
            return Err(RowProgramError::Unsupported);
        }
        // The complete body kernel will widen this domain; these policies
        // still belong to the buffer loop, not the item writer.
        for variable in [
            LayoutVar::FaceRemappingAlist,
            LayoutVar::LinePrefix,
            LayoutVar::WrapPrefix,
            LayoutVar::DisplayFillColumnIndicator,
            LayoutVar::IndicateBufferBoundaries,
            LayoutVar::OverlayArrowPosition,
            LayoutVar::BufferDisplayTable,
        ] {
            if view
                .layout_buffer_local_value(variable)
                .is_some_and(|value| !value.is_nil())
            {
                return Err(RowProgramError::Unsupported);
            }
        }
        let context = BufferSourceFaceResolutionContext::new(
            &view,
            &resolver,
            DisplayRowMeasurementPolicy::for_mode(DisplayRowMeasurementMode::ConcreteFont),
            resolver.default_face(),
            base_id,
            metrics,
            metrics,
            Default::default(),
        );
        let captured = capture_physical_line(
            buffer_id,
            window.0,
            capture.position,
            128,
            32,
            context,
            &mut capture.attempt,
            || false,
        )?;
        capture.position = captured.end;
        let mut realizer = DisplayRowFaceRealizer::new(&mut self.font_metrics);
        let mut faces = vec![realizer.realize_face(
            base_id,
            resolver.default_face(),
            metrics.char_width(),
            metrics.ascent(),
            metrics.row_height(),
        )];
        for pending in captured.faces {
            if !faces.iter().any(|face| face.face_id == pending.face_id()) {
                faces.push(realizer.realize_face(
                    pending.face_id(),
                    pending.resolved(),
                    metrics.char_width(),
                    metrics.ascent(),
                    metrics.row_height(),
                ));
            }
        }
        for face in &faces {
            let mut rendered = face.render_face();
            if let Some(fonts) = realizer.font_metrics_service_mut() {
                crate::font::metrics::realize_face_font(&mut rendered, fonts)
                    .ok_or(RowProgramError::Unsupported)?;
            }
            capture
                .attempt
                .import_face(rendered)
                .map_err(|_| RowProgramError::Unsupported)?;
        }
        let geometry = RowProgramGeometry {
            inherited_line_spacing: key.extra_line_spacing,
            width: key.partition.text_body().width,
            metrics,
            tabs: crate::display_row::builder::DisplayTabPolicy::from_tab_width_and_stops(
                0.0,
                key.tab_width,
                &key.tab_stop_list,
            ),
            base_face: base_id,
            background: Color::from_pixel(bootstrap.background),
        };
        let limits = RowProgramLimits {
            items: 32,
            text_bytes: 512,
            glyphs: 256,
        };
        let deferred = if RowProgram::supports_deferred_text(&captured.items) {
            crate::row_layout::font_measurement::FontMeasurementSnapshot::capture(
                &faces,
                realizer
                    .font_metrics_service_mut()
                    .ok_or(RowProgramError::Unsupported)?,
                &captured.items,
            )
            .ok()
        } else {
            None
        };
        let program = if let Some(fonts) = deferred {
            let fonts = if let Some(shared) = capture
                .font_snapshots
                .iter()
                .find(|old| old.as_ref() == &fonts)
            {
                shared.clone()
            } else {
                let bytes = capture.font_bytes.saturating_add(fonts.bytes());
                if bytes > crate::row_layout::worker::MAX_FONT_BYTES {
                    return Err(RowProgramError::Budget);
                }
                let shared = std::sync::Arc::new(fonts);
                capture.font_bytes = bytes;
                capture.font_snapshots.push(shared.clone());
                shared
            };
            RowProgram::capture_deferred(geometry, captured.items, faces, fonts, limits)?
        } else {
            let mut measurer = DisplayRowGlyphMeasurer::with_mode(
                &faces,
                realizer.font_metrics_service_mut(),
                metrics.char_width(),
                GlyphAdvanceQuantization::PreserveLogicalPixels,
                DisplayRowMeasurementMode::ConcreteFont,
            );
            RowProgram::capture(
                geometry,
                captured.items,
                faces.clone(),
                &mut measurer,
                limits,
            )?
        };
        capture.programs.push(program);
        Ok(())
    }
}
