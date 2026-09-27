use super::*;
use crate::buffer_source::face_resolution::BufferSourceFaceResolutionContext;
use crate::buffer_source::owned_capture::capture_physical_line;
use crate::display_row::face_state::{
    DisplayRowFaceRealizer, DisplayRowGlyphMeasurer, DisplayRowMeasurementMode,
    DisplayRowMeasurementPolicy, stable_face_id_for_resolved,
};
use crate::display_row::metrics::DisplayRowFallbackMetrics;
use crate::glyph_advance::GlyphAdvanceQuantization;
use crate::row_layout::program::{RowProgram, RowProgramGeometry, RowProgramLimits};

#[test]
fn unseen_row_worker_glyphs_match_canonical_window_body() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let display_window = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
    let retained = engine.retained_window_matrices[&display_window].clone();
    let key = &retained.key;
    let sample = retained
        .matrix
        .rows
        .iter()
        .find(|row| row.enabled && row.role == GlyphRowRole::Text)
        .unwrap();
    let metrics = DisplayRowFallbackMetrics::from_default_face_extents(
        key.char_width,
        sample.height_px,
        sample.ascent_px,
    );
    let frame_data = eval.frame_manager().get(frame).unwrap();
    let ws = frame_data
        .effective_window_system()
        .and_then(|value| value.as_symbol_name().map(str::to_owned));
    let mode = DisplayRowMeasurementMode::from_frame_window_system(ws.is_some());
    let resolver = crate::neovm_bridge::FaceResolver::new_with_font_sizing(
        eval.face_table(),
        0xffffff,
        0,
        key.font_pixel_size,
        ws,
        engine.font_sizing,
    );
    let mut attempt = engine.frame_face_arenas[&frame].begin_attempt();
    let base_id = stable_face_id_for_resolved(&mut attempt, resolver.default_face());
    let start = CharPos0::new(120 * line.len());
    let snapshot = crate::neovm_bridge::BorrowedLayoutBuffer::for_window(
        eval.buffer_manager().get(buffer).unwrap(),
        eval.obarray(),
        start,
        128,
        crate::display_property::DisplayPropertyTarget::for_window_system(
            mode.uses_concrete_font_geometry(),
        ),
    );
    let context = BufferSourceFaceResolutionContext::new(
        &snapshot,
        &resolver,
        DisplayRowMeasurementPolicy::for_mode(mode),
        resolver.default_face(),
        base_id,
        metrics,
        metrics,
        Default::default(),
    );
    let captured = capture_physical_line(
        buffer,
        window.0,
        start,
        128,
        32,
        context,
        &mut attempt,
        || false,
    )
    .unwrap();
    let mut realizer = DisplayRowFaceRealizer::new(&mut engine.font_metrics);
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
    let mut measurer = DisplayRowGlyphMeasurer::with_mode(
        &faces,
        realizer.font_metrics_service_mut(),
        metrics.char_width(),
        GlyphAdvanceQuantization::PreserveLogicalPixels,
        mode,
    );
    let program = RowProgram::capture(
        RowProgramGeometry {
            width: key.partition.text_body().width,
            metrics,
            tabs: crate::display_row::builder::DisplayTabPolicy::from_tab_width_and_stops(
                0.0,
                key.tab_width,
                &key.tab_stop_list,
            ),
            base_face: base_id,
            background: neomacs_display_protocol::types::Color::BLACK,
        },
        captured.items,
        faces.clone(),
        &mut measurer,
        RowProgramLimits {
            items: 32,
            text_bytes: 512,
            glyphs: 512,
        },
    )
    .unwrap();
    let region = retained.display_snapshot.regions.text_body;
    let window_top = retained.display_snapshot.regions.outer.y;
    let row_base = retained
        .matrix
        .rows
        .iter()
        .position(|row| row.enabled && row.role == GlyphRowRole::Text)
        .unwrap();
    let window_bounds = retained.display_snapshot.regions.outer;
    let ncols = retained.matrix.ncols;
    let actual = std::thread::spawn(move || {
        let row = program.compute(|| false)?;
        crate::window_output::prepared_body::position_buffer_rows(
            vec![row],
            row_base,
            region.x,
            region.y,
            window_top,
            window.0,
            window_bounds,
            ncols,
        )
    })
    .join()
    .unwrap()
    .unwrap();
    scroll_window_to(
        &mut eval,
        frame,
        window,
        buffer,
        start.get() as i64 + 1,
        start.get() + 5 * line.len(),
    );
    if let neovm_core::window::Window::Leaf { force_start, .. } = eval
        .frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .find_window_mut(window)
        .unwrap()
    {
        *force_start = true;
    }
    let mut fresh = LayoutEngine::new();
    fresh.layout_frame_rust(&mut eval, frame);
    let expected = fresh.retained_window_matrices[&display_window]
        .matrix
        .rows
        .iter()
        .find(|row| row.enabled && row.role == GlyphRowRole::Text)
        .unwrap();
    // Compare glyph semantics independently of speculative face numbering.
    let mut actual_row = actual.glyph_rows[0].clone();
    for glyph in actual_row.glyphs.iter_mut().flatten() {
        glyph.face_id = expected.glyphs[1][0].face_id;
    }
    assert_eq!(
        actual_row.glyphs[1].len(),
        expected.glyphs[1].len(),
        "actual text {:?}; expected text {:?}; expected row {}..{}",
        actual_row.glyphs[1]
            .iter()
            .map(|g| &g.glyph_type)
            .collect::<Vec<_>>(),
        expected.glyphs[1]
            .iter()
            .map(|g| &g.glyph_type)
            .collect::<Vec<_>>(),
        expected.start_charpos,
        expected.end_charpos
    );
    for (i, (actual, expected)) in actual_row.glyphs[1]
        .iter()
        .zip(&expected.glyphs[1])
        .enumerate()
    {
        assert_eq!(actual, expected, "glyph {i}");
    }
    assert_eq!(actual_row.height_px, expected.height_px);
    assert_eq!(actual_row.ascent_px, expected.ascent_px);
    assert_eq!(actual_row.start_charpos, expected.start_charpos);
    assert_eq!(actual_row.end_charpos, expected.end_charpos);
    let expected_snapshot = &fresh.retained_window_matrices[&display_window].display_snapshot;
    let expected_points: Vec<_> = expected_snapshot
        .points
        .iter()
        .filter(|point| point.row == row_base as i64)
        .cloned()
        .collect();
    assert_eq!(actual.geometry.points, expected_points);
    assert_eq!(actual.geometry.rows[0], expected_snapshot.rows[row_base]);
}

#[test]
fn first_visit_to_worker_prepared_page_reuses_rows_and_matches_fresh_layout() {
    first_visit(None, "ordinary offscreen text\n", None);
}

#[test]
fn first_visit_to_worker_prepared_mixed_height_faces_matches_fresh_layout() {
    first_visit(
        Some("(:height 150 :foreground \"red\")"),
        "ordinary offscreen text\n",
        None,
    );
}

#[test]
fn first_visit_to_worker_prepared_unicode_page_matches_fresh_layout() {
    first_visit(None, "office café 好 á שלום سلام\n", None);
}

fn first_visit(face: Option<&str>, line: &str, change: Option<&str>) {
    first_visit_shifted(face, line, change, 0);
}

#[test]
fn first_visit_shifted_within_worker_page_matches_fresh_layout() {
    first_visit_shifted(None, "ordinary offscreen text\n", None, 1);
}

fn first_visit_shifted(face: Option<&str>, line: &str, change: Option<&str>, shift: usize) {
    let (mut eval, frame, buffer, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let line_chars = line.chars().count();
    let start = 120 * line_chars;
    let start_byte = 120 * line.len();
    if let Some(face) = face {
        eval.eval_str(&format!(
            "(put-text-property {} {} 'face '{face})",
            start + 1,
            start + 12 * line_chars
        ))
        .unwrap();
    }
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    engine
        .request_scroll_coverage(&eval, frame, window, CharPos0::new(start))
        .unwrap();
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while !engine
        .scroll_coverage
        .drain(&mut engine.prepared_viewports)
        .expect("worker coverage failed")
    {
        assert!(
            std::time::Instant::now() < deadline,
            "unseen coverage was not admitted"
        );
        std::thread::yield_now();
    }
    let start = start + shift * line_chars;
    let start_byte = start_byte + shift * line.len();
    let display_window = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
    let mut requested_key = engine.retained_window_matrices[&display_window].key.clone();
    requested_key.window_start = start as i64;
    requested_key.point = (start + 5 * line_chars) as i64;
    if shift == 0 {
        let (replay, faces) = engine
            .prepared_viewports
            .replay(frame, display_window, &requested_key)
            .expect("ready coverage must satisfy the requested key");
        let arena = &engine.frame_face_arenas[&frame];
        arena
            .begin_attempt()
            .admit_prepared(
                replay
                    .body_rows
                    .iter()
                    .flat_map(|(_, row)| row.glyphs.iter().flatten().map(|glyph| glyph.face_id)),
                &faces,
                arena,
            )
            .expect("prepared face namespace");
    }
    if let Some(change) = change {
        eval.eval_str(change).unwrap();
    }
    scroll_window_to(
        &mut eval,
        frame,
        window,
        buffer,
        start as i64 + 1,
        start_byte + 5 * line.len(),
    );
    if let neovm_core::window::Window::Leaf { force_start, .. } = eval
        .frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .find_window_mut(window)
        .unwrap()
    {
        *force_start = true;
    }
    engine.layout_frame_rust(&mut eval, frame);
    assert_eq!(
        engine.last_layout_stats().prepared_windows,
        usize::from(change.is_none()),
        "requested {requested_key:?}, actual {:?}",
        engine.retained_window_matrices[&display_window].key
    );
    if change.is_none() {
        assert!(
            engine.last_layout_stats().reused_rows + engine.last_layout_stats().reused_shifted_rows
                > 10
        );
    }
    let actual = selected_window_layout_trace(&eval, &engine, frame);
    let mut fresh = LayoutEngine::new();
    fresh.layout_frame_rust(&mut eval, frame);
    assert_eq!(actual, selected_window_layout_trace(&eval, &fresh, frame));
}

#[test]
fn worker_page_is_rejected_after_text_edit() {
    first_visit(
        None,
        "ordinary offscreen text\n",
        Some("(progn (goto-char 3000) (delete-region 3000 3001) (insert \"X\"))"),
    );
}

#[test]
fn worker_page_is_rejected_after_face_property_change() {
    first_visit(
        None,
        "ordinary offscreen text\n",
        Some("(put-text-property 2900 3000 'face '(:height 175))"),
    );
}

#[test]
fn worker_page_is_rejected_after_overlay_change() {
    first_visit(
        None,
        "ordinary offscreen text\n",
        Some("(overlay-put (make-overlay 2900 3000) 'face '(:background \"red\"))"),
    );
}

#[test]
fn worker_page_is_rejected_after_narrowing() {
    first_visit(
        None,
        "ordinary offscreen text\n",
        Some("(narrow-to-region 1 5000)"),
    );
}

#[test]
fn idle_capture_yields_between_rows_and_cancels_after_a_revision_change() {
    let (mut eval, frame, _buffer, window) =
        incr_editing_frame(&"ordinary text\n".repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    engine
        .begin_scroll_coverage(&eval, frame, window, CharPos0::new(120 * 14))
        .unwrap();
    assert!(
        engine.capture_scroll_step(&eval).unwrap(),
        "one step must leave the rest of the page for later"
    );
    eval.eval_str("(put-text-property 2000 2010 'face '(:height 175))")
        .unwrap();
    assert_eq!(
        engine.capture_scroll_step(&eval),
        Err(crate::row_layout::program::RowProgramError::Cancelled)
    );
    assert!(
        !engine.capture_scroll_step(&eval).unwrap(),
        "failed capture must be retired"
    );
    assert!(
        !engine
            .scroll_coverage
            .drain(&mut engine.prepared_viewports)
            .unwrap()
    );
}

#[test]
fn idle_maintenance_prepares_an_unseen_page_without_changing_the_live_viewport() {
    idle_first_visit(false, false);
}

#[test]
fn idle_maintenance_prepares_an_unseen_backward_page() {
    idle_first_visit(true, false);
}

#[test]
fn idle_capture_survives_redisplays_with_unchanged_layout_inputs() {
    idle_first_visit(false, true);
}

fn idle_first_visit(backward: bool, redisplay_between_steps: bool) {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    if backward {
        scroll_window_to(
            &mut eval,
            frame,
            window,
            buffer,
            (120 * line.len() + 1) as i64,
            125 * line.len(),
        );
        if let neovm_core::window::Window::Leaf { force_start, .. } = eval
            .frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .find_window_mut(window)
            .unwrap()
        {
            *force_start = true;
        }
    }
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let before = selected_window_layout_trace(&eval, &engine, frame);
    let display_window = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
    let rows: Vec<_> = engine.retained_window_matrices[&display_window]
        .matrix
        .rows
        .iter()
        .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
        .collect();
    let start = if backward {
        (120 - rows.len().saturating_sub(2).max(1)) * line.len()
    } else {
        rows[rows.len() - 2].start_charpos
    };
    let row_count = rows.len();
    drop(rows);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let mut steps = 0;
    while engine.maintain_scroll_coverage(&eval).is_some() {
        if redisplay_between_steps {
            engine.layout_frame_rust(&mut eval, frame);
        }
        steps += 1;
        assert!(std::time::Instant::now() < deadline);
        std::thread::yield_now();
    }
    assert!(
        steps + 1 >= row_count,
        "acquisition must yield between rows"
    );
    assert_eq!(before, selected_window_layout_trace(&eval, &engine, frame));
    scroll_window_to(
        &mut eval,
        frame,
        window,
        buffer,
        start as i64 + 1,
        start + 5 * line.len(),
    );
    if let neovm_core::window::Window::Leaf { force_start, .. } = eval
        .frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .find_window_mut(window)
        .unwrap()
    {
        *force_start = true;
    }
    engine.layout_frame_rust(&mut eval, frame);
    assert_eq!(engine.last_layout_stats().prepared_windows, 1);
    let actual = selected_window_layout_trace(&eval, &engine, frame);
    let mut fresh = LayoutEngine::new();
    fresh.layout_frame_rust(&mut eval, frame);
    assert_eq!(actual, selected_window_layout_trace(&eval, &fresh, frame));
}

#[test]
fn worker_page_is_rejected_after_font_change() {
    first_visit(
        None,
        "ordinary offscreen text\n",
        Some("(internal-set-lisp-face-attribute 'default :height 180 (selected-frame))"),
    );
}

#[test]
fn worker_page_is_rejected_after_horizontal_scroll() {
    first_visit(
        None,
        "ordinary offscreen text\n",
        Some("(set-window-hscroll nil 2)"),
    );
}

#[test]
fn worker_page_with_font_family_weight_and_slant_matches_fresh_layout() {
    first_visit(
        Some("(:family \"DejaVu Serif\" :weight bold :slant italic :height 125)"),
        "ordinary offscreen text\n",
        None,
    );
}

#[test]
fn worker_page_with_box_and_extended_background_matches_fresh_layout() {
    first_visit(
        Some("(:box (:line-width 2 :color \"blue\") :background \"red\" :extend t)"),
        "ordinary offscreen text\n",
        None,
    );
}

#[test]
fn worker_page_with_combined_decorations_matches_fresh_layout() {
    first_visit(
        Some(
            "(:underline (:style wave :color \"blue\") :overline t :strike-through t :inverse-video t)",
        ),
        "ordinary offscreen text\n",
        None,
    );
}

#[test]
fn worker_page_with_smaller_font_and_tabs_matches_fresh_layout() {
    first_visit(
        Some("(:family \"DejaVu Serif\" :height 125)"),
        "font\ttext\n",
        None,
    );
}
