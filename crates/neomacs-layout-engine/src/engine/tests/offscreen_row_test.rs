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
    first_visit_at(face, line, change, shift, 5);
}

#[test]
fn first_visit_page_command_reuses_worker_rows_with_point_at_page_start() {
    first_visit_at(None, "ordinary offscreen text\n", None, 0, 0);
}

fn first_visit_at(
    face: Option<&str>,
    line: &str,
    change: Option<&str>,
    shift: usize,
    point_row: usize,
) {
    first_visit_with_setup(face, line, change, shift, point_row, None);
}

#[test]
fn first_visit_worker_page_accepts_inactive_startup_overlay_arrows() {
    first_visit_with_setup(
        None,
        "ordinary offscreen text\n",
        None,
        0,
        5,
        Some(
            "(setq overlay-arrow-variable-list '(next-error-overlay-arrow-position overlay-arrow-position) next-error-overlay-arrow-position nil)",
        ),
    );
}

#[test]
fn worker_page_is_rejected_when_custom_overlay_arrow_becomes_active() {
    first_visit_with_setup(
        None,
        "ordinary offscreen text\n",
        Some("(setq worker-custom-arrow (copy-marker 2761))"),
        0,
        5,
        Some("(setq overlay-arrow-variable-list '(worker-custom-arrow) worker-custom-arrow nil)"),
    );
}

#[test]
fn first_visit_worker_page_preserves_overlapping_face_and_pointer_overlays() {
    first_visit_with_setup(
        None,
        "ordinary offscreen text\n",
        None,
        0,
        5,
        Some(
            "(let ((a (make-overlay 2761 3100)) (b (make-overlay 2780 3060)))
                 (overlay-put a 'face '(:family \"DejaVu Serif\" :height 150 :foreground \"red\"))
                 (overlay-put a 'mouse-face 'highlight)
                 (overlay-put a 'help-echo \"offscreen help\")
                 (overlay-put b 'priority 12)
                 (overlay-put b 'face '(:weight bold :underline t)))",
        ),
    );
}

#[test]
fn first_visit_worker_page_resolves_overlay_category_faces() {
    first_visit_with_setup(
        None,
        "ordinary offscreen text\n",
        None,
        0,
        5,
        Some(
            "(progn (put 'worker-overlay-category 'face '(:height 125 :slant italic))
                 (overlay-put (make-overlay 2761 3100) 'category 'worker-overlay-category))",
        ),
    );
}

#[test]
fn worker_capture_rejects_overlay_replacements_and_excessive_overlap() {
    use crate::row_layout::program::RowProgramError;
    for (setup, expected) in [
        (
            "(setq overlay-arrow-variable-list (make-list 33 'worker-custom-arrow))",
            RowProgramError::Unsupported,
        ),
        (
            "(setq overlay-arrow-variable-list '(worker-custom-arrow) worker-custom-arrow (copy-marker 2761))",
            RowProgramError::Unsupported,
        ),
        (
            "(overlay-put (make-overlay 2761 3100) 'before-string \"prefix\")",
            RowProgramError::Unsupported,
        ),
        (
            "(overlay-put (make-overlay 2761 3100) 'display \"replacement\")",
            RowProgramError::Unsupported,
        ),
        (
            "(progn (put 'worker-overlay-category 'after-string \"suffix\")
                 (overlay-put (make-overlay 2761 3100) 'category 'worker-overlay-category))",
            RowProgramError::Unsupported,
        ),
        (
            "(let ((i 0)) (while (< i 33) (make-overlay 2761 3100) (setq i (1+ i))))",
            RowProgramError::Budget,
        ),
        (
            "(put-text-property 2761 3100 'display '(when t (raise 0.25)))",
            RowProgramError::Unsupported,
        ),
        (
            "(put-text-property 2761 3100 'display '(raise (+ 1 2)))",
            RowProgramError::Unsupported,
        ),
    ] {
        let (mut eval, frame, _, window) =
            incr_editing_frame(&"ordinary offscreen text\n".repeat(300), 800, 600);
        eval.frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .window_system = Some(Value::symbol("neomacs"));
        eval.eval_str(setup).unwrap();
        let mut engine = LayoutEngine::new();
        engine.layout_frame_rust(&mut eval, frame);
        assert_eq!(
            engine.request_scroll_coverage(&eval, frame, window, CharPos0::new(2760)),
            Err(expected),
            "{setup}"
        );
    }
}

fn first_visit_with_setup(
    face: Option<&str>,
    line: &str,
    change: Option<&str>,
    shift: usize,
    point_row: usize,
    setup: Option<&str>,
) {
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
    if let Some(setup) = setup {
        eval.eval_str(setup).unwrap();
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
    requested_key.point = (start + point_row * line_chars) as i64;
    if shift == 0 {
        if point_row == 0 {
            assert!(
                engine
                    .prepared_viewports
                    .replay(frame, display_window, &requested_key, false)
                    .is_none(),
                "ordinary point motion must still run viewport resolution"
            );
        }
        let (replay, faces) = engine
            .prepared_viewports
            .replay(frame, display_window, &requested_key, true)
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
    // Native scroll commands synchronously ask for window geometry before
    // publishing their new start. These queries must preserve idle coverage.
    for _ in 0..3 {
        engine
            .query_window_layout(
                &mut eval,
                frame,
                window,
                neovm_core::window::WindowLayoutQueryScope::Viewport,
            )
            .unwrap();
    }
    scroll_window_to(
        &mut eval,
        frame,
        window,
        buffer,
        start as i64 + 1,
        start_byte + point_row * line.len(),
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
fn raised_buffer_rows_preserve_glyph_extents_with_and_without_wrapping() {
    for width in [120, 800] {
        let text = format!("{}\n", "raised words ".repeat(30));
        let (mut eval, frame, _, window) = incr_editing_frame(&text, width, 600);
        eval.frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .window_system = Some(Value::symbol("neomacs"));
        eval.eval_str(
            "(setq truncate-lines nil word-wrap t)
            (put-text-property 1 361 'face '(:height 150))
            (put-text-property 1 361 'display '(raise 0.25))",
        )
        .unwrap();
        let mut engine = LayoutEngine::new();
        engine.layout_frame_rust(&mut eval, frame);
        let owner = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
        let rows = &engine.retained_window_matrices[&owner].matrix.rows;
        let mut raised_rows = 0;
        for row in rows
            .iter()
            .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
        {
            for glyph in row.glyphs.iter().flatten().filter(|glyph| !glyph.padding) {
                if glyph.vertical_offset_px == 0.0 {
                    continue;
                }
                raised_rows += 1;
                let ascent = (glyph.pixel_ascent - glyph.vertical_offset_px).max(0.0);
                let descent =
                    (glyph.pixel_height - glyph.pixel_ascent + glyph.vertical_offset_px).max(0.0);
                assert!(
                    row.ascent_px >= ascent,
                    "width={width}, row baseline lost raised glyph ascent"
                );
                assert!(
                    row.height_px - row.ascent_px >= descent,
                    "width={width}, row lost lowered glyph descent"
                );
            }
        }
        assert!(raised_rows > 0);
    }
}

#[test]
fn unchanged_raised_cursor_reuses_its_authoritative_presentation() {
    let (mut eval, frame, _, _) =
        incr_editing_frame(&"ordinary offscreen text\n".repeat(30), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str("(put-text-property 1 20 'display '(raise 0.25))")
        .unwrap();
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let expected = selected_window_layout_trace(&eval, &engine, frame);
    engine.layout_frame_rust(&mut eval, frame);
    assert_eq!(engine.last_layout_stats().cursor_only_windows, 1);
    assert_eq!(
        selected_window_layout_trace(&eval, &engine, frame),
        expected
    );
}

#[test]
fn worker_page_preserves_literal_raised_text_and_overlay_geometry() {
    for setup in [
        "(put-text-property 2761 3100 'display '(raise 0.25))",
        "(overlay-put (make-overlay 2761 3100) 'display '(raise -0.25))",
    ] {
        first_visit_with_setup(
            Some("(:height 150 :weight bold)"),
            "ordinary offscreen text\n",
            None,
            0,
            16,
            Some(setup),
        );
    }
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
fn offscreen_capture_budget_ignores_unused_retained_matrix_capacity() {
    let (mut eval, frame, _, window) =
        incr_editing_frame(&"ordinary offscreen text\n".repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let window_id = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
    let retained = engine.retained_window_matrices.get_mut(&window_id).unwrap();
    retained.matrix.resize(1000, retained.matrix.ncols);
    engine
        .request_scroll_coverage(&eval, frame, window, CharPos0::new(120 * 23))
        .expect("unused matrix slots are not source rows to precompute");
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while !engine
        .scroll_coverage
        .drain(&mut engine.prepared_viewports)
        .unwrap()
    {
        assert!(std::time::Instant::now() < deadline);
        std::thread::yield_now();
    }
    let mut key = engine.retained_window_matrices[&window_id].key.clone();
    key.window_start = 120 * 23;
    key.point = 125 * 23;
    let (replay, _) = engine
        .prepared_viewports
        .replay(frame, window_id, &key, false)
        .expect("unused slots must not exceed the prepared-cache capacity either");
    assert!(replay.body_rows.len() > 10);
}

#[test]
fn unchanged_redisplays_keep_retained_matrix_capacity_bounded() {
    let (mut eval, frame, _, window) =
        incr_editing_frame(&"ordinary offscreen text\n".repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let window_id = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
    let rows = engine.retained_window_matrices[&window_id]
        .matrix
        .rows
        .len();
    for _ in 0..100 {
        engine.layout_frame_rust(&mut eval, frame);
    }
    assert_eq!(
        engine.retained_window_matrices[&window_id]
            .matrix
            .rows
            .len(),
        rows
    );
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
    idle_first_visit_step(backward, redisplay_between_steps, None);
}

#[test]
fn idle_worker_rows_complete_small_forward_scrolls() {
    for rows in [1, 3] {
        idle_first_visit_step(false, false, Some(rows));
    }
}

fn idle_first_visit_step(backward: bool, redisplay_between_steps: bool, step: Option<usize>) {
    idle_first_visit_styled(backward, redisplay_between_steps, step, false);
}

#[test]
fn idle_worker_rows_complete_small_scrolls_with_distinct_prefix_faces() {
    idle_first_visit_styled(false, false, Some(1), true);
    idle_first_visit_styled(false, false, Some(3), true);
}

fn idle_first_visit_styled(
    backward: bool,
    redisplay_between_steps: bool,
    step: Option<usize>,
    styled: bool,
) {
    idle_first_visit_projected(backward, redisplay_between_steps, step, styled, 0);
}

#[test]
fn idle_worker_rows_cover_fractional_scroll_placement() {
    for hidden in [1, 4, 12] {
        for step in [0, 1, 3] {
            idle_first_visit_projected(false, false, Some(step), false, hidden);
        }
    }
}

#[test]
fn idle_worker_fractional_scroll_preserves_mixed_face_geometry() {
    for step in [0, 1, 3] {
        idle_first_visit_projected(false, false, Some(step), true, 4);
    }
}

fn idle_first_visit_projected(
    backward: bool,
    redisplay_between_steps: bool,
    step: Option<usize>,
    styled: bool,
    hidden: i32,
) {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    if styled {
        eval.eval_str(
            "(progn
            (put-text-property 1 70 'face '(:height 150 :family \"DejaVu Serif\"))
            (put-text-property 277 690 'face '(:height 125 :weight bold))
            (overlay-put (make-overlay 277 690) 'mouse-face 'highlight))",
        )
        .unwrap();
    }
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
    let start = if let Some(step) = step {
        step * line.len()
    } else if backward {
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
    let offsets = if hidden == 0 {
        vec![0]
    } else {
        vec![hidden, 2, 14, 1, 0, 6]
    };
    for hidden in offsets {
        if let neovm_core::window::Window::Leaf {
            force_start,
            vscroll,
            ..
        } = eval
            .frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .find_window_mut(window)
            .unwrap()
        {
            *force_start = true;
            *vscroll = -hidden;
        }
        engine.layout_frame_rust(&mut eval, frame);
        assert_eq!(engine.last_layout_stats().prepared_windows, 1);
        let actual = selected_window_layout_trace(&eval, &engine, frame);
        let mut fresh = LayoutEngine::new();
        fresh.layout_frame_rust(&mut eval, frame);
        assert_eq!(actual, selected_window_layout_trace(&eval, &fresh, frame));
    }
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

#[test]
fn repeated_fractional_scroll_across_prepared_pages_matches_fresh_layout() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, window) = incr_editing_frame(&line.repeat(400), 1000, 700);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.frame_manager_mut().get_mut(frame).unwrap().char_height = 17.0;
    let mut engine = LayoutEngine::new();
    for pixels in (0..=96)
        .map(|step| step * 4)
        .chain((0..96).rev().map(|step| step * 4))
    {
        let start = (pixels / 17) as usize * line.len();
        scroll_window_to(
            &mut eval,
            frame,
            window,
            buffer,
            start as i64 + 1,
            start + 5 * line.len(),
        );
        if let neovm_core::window::Window::Leaf {
            force_start,
            vscroll,
            ..
        } = eval
            .frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .find_window_mut(window)
            .unwrap()
        {
            *force_start = true;
            *vscroll = -(pixels % 17);
        }
        engine.layout_frame_rust(&mut eval, frame);
        let actual = selected_window_layout_trace(&eval, &engine, frame);
        let mut fresh = LayoutEngine::new();
        fresh.layout_frame_rust(&mut eval, frame);
        assert_eq!(
            actual,
            selected_window_layout_trace(&eval, &fresh, frame),
            "offset {pixels}"
        );
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        while engine.maintain_scroll_coverage(&eval).is_some() {
            assert!(std::time::Instant::now() < deadline);
            std::thread::yield_now();
        }
    }
}

#[test]
fn idle_capture_finishes_while_the_viewport_keeps_moving() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, window) = incr_editing_frame(&line.repeat(400), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let display_window = neomacs_display_protocol::types::DisplayWindowId::new(window.0 as i64);
    let mut completed = false;
    // Each service opportunity is separated by a real viewport change and
    // redisplay. A continuously moving viewport must not restart acquisition
    // at its first row forever.
    for step in 0..150 {
        engine.maintain_scroll_coverage(&eval);
        if engine
            .prepared_viewports
            .has_computed(frame, display_window)
        {
            completed = true;
            break;
        }
        let start = (step % 3 + 1) * line.len();
        scroll_window_to(
            &mut eval,
            frame,
            window,
            buffer,
            start as i64 + 1,
            start + 5 * line.len(),
        );
        engine.layout_frame_rust(&mut eval, frame);
        std::thread::yield_now();
    }
    assert!(completed, "viewport motion starved the offscreen worker");
    let actual = selected_window_layout_trace(&eval, &engine, frame);
    let mut fresh = LayoutEngine::new();
    fresh.layout_frame_rust(&mut eval, frame);
    assert_eq!(actual, selected_window_layout_trace(&eval, &fresh, frame));
}

#[test]
fn worker_page_is_rejected_after_category_symbol_properties_change() {
    first_visit_with_setup(
        None,
        "ordinary offscreen text\n",
        Some("(put 'worker-overlay-category 'face '(:height 200))"),
        0,
        5,
        Some(
            "(progn (put 'worker-overlay-category 'face '(:height 125)) (overlay-put (make-overlay 2761 3100) 'category 'worker-overlay-category))",
        ),
    );
}

#[test]
fn idle_preparation_covers_unselected_windows_even_when_selected_rows_are_unsupported() {
    for unsupported_selected in [false, true] {
        let line = "ordinary offscreen text\n";
        let (mut eval, frame, buffer, selected) = incr_editing_frame(&line.repeat(300), 800, 600);
        let other_buffer = eval.buffer_manager_mut().create_buffer("offscreen-other");
        eval.buffer_manager_mut()
            .get_mut(other_buffer)
            .unwrap()
            .insert(&line.repeat(300));
        let other = eval
            .frame_manager_mut()
            .split_window(
                frame,
                selected,
                neovm_core::window::SplitDirection::Horizontal,
                other_buffer,
                None,
                neovm_core::window::SplitPlacement::AfterTarget,
            )
            .unwrap();
        eval.frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .window_system = Some(Value::symbol("neomacs"));
        if unsupported_selected {
            eval.eval_str("(put-text-property 1 (point-max) 'display \"replacement\")")
                .unwrap();
        }
        let mut engine = LayoutEngine::new();
        engine.layout_frame_rust(&mut eval, frame);
        let before = selected_window_layout_trace(&eval, &engine, frame);
        let owner = neomacs_display_protocol::types::DisplayWindowId::new(other.0 as i64);
        let rows: Vec<_> = engine.retained_window_matrices[&owner]
            .matrix
            .rows
            .iter()
            .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
            .collect();
        let target = rows[rows.len() - 2].start_charpos;
        let mut key = engine.retained_window_matrices[&owner].key.clone();
        key.window_start = target as i64;
        key.point = (target + line.len()) as i64;
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        while engine.maintain_scroll_coverage(&eval).is_some() {
            assert!(std::time::Instant::now() < deadline);
            std::thread::yield_now();
        }
        assert!(
            engine
                .prepared_viewports
                .replay(frame, owner, &key, false)
                .is_some(),
            "unselected window must have prepared coverage; unsupported_selected={unsupported_selected}"
        );
        assert_eq!(before, selected_window_layout_trace(&eval, &engine, frame));
        assert_eq!(
            eval.frame_manager().get(frame).unwrap().selected_window,
            selected
        );
        assert_eq!(eval.buffer_manager().current_buffer().unwrap().id(), buffer);
    }
}

#[test]
fn moving_selected_window_does_not_starve_other_window_preparation() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, selected) = incr_editing_frame(&line.repeat(300), 800, 600);
    let other = eval
        .frame_manager_mut()
        .split_window(
            frame,
            selected,
            neovm_core::window::SplitDirection::Horizontal,
            buffer,
            None,
            neovm_core::window::SplitPlacement::AfterTarget,
        )
        .unwrap();
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    let owner = neomacs_display_protocol::types::DisplayWindowId::new(other.0 as i64);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let mut steps = 0;
    while !engine.prepared_viewports.has_computed(frame, owner) {
        assert!(engine.maintain_scroll_coverage(&eval).is_some());
        assert!(std::time::Instant::now() < deadline);
        // Change the selected viewport while its bounded page is in flight.
        // A global "observed window" would continually retarget that window.
        {
            let start = (steps % 60 + 1) * line.len();
            scroll_window_to(&mut eval, frame, selected, buffer, start as i64 + 1, start);
            if let neovm_core::window::Window::Leaf { force_start, .. } = eval
                .frame_manager_mut()
                .get_mut(frame)
                .unwrap()
                .find_window_mut(selected)
                .unwrap()
            {
                *force_start = true;
            }
            engine.layout_frame_rust(&mut eval, frame);
        }
        steps += 1;
        std::thread::yield_now();
    }
}

#[test]
fn deleting_capture_owner_allows_remaining_window_preparation() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame, buffer, selected) = incr_editing_frame(&line.repeat(300), 800, 600);
    let other = eval
        .frame_manager_mut()
        .split_window(
            frame,
            selected,
            neovm_core::window::SplitDirection::Horizontal,
            buffer,
            None,
            neovm_core::window::SplitPlacement::AfterTarget,
        )
        .unwrap();
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    assert!(engine.maintain_scroll_coverage(&eval).is_some());
    assert!(eval.frame_manager_mut().delete_window(frame, selected));
    engine.layout_frame_rust(&mut eval, frame);
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while engine.maintain_scroll_coverage(&eval).is_some() {
        assert!(std::time::Instant::now() < deadline);
        std::thread::yield_now();
    }
    assert!(engine.prepared_viewports.has_computed(
        frame,
        neomacs_display_protocol::types::DisplayWindowId::new(other.0 as i64)
    ));
    assert!(!engine.prepared_viewports.has_computed(
        frame,
        neomacs_display_protocol::types::DisplayWindowId::new(selected.0 as i64)
    ));
}

#[test]
fn worker_page_is_rejected_after_in_place_display_property_mutation() {
    first_visit_with_setup(
        None,
        "ordinary offscreen text\n",
        Some("(setcar (cdr worker-raise-spec) 0.75)"),
        0,
        16,
        Some(
            "(progn (setq worker-raise-spec (list 'raise 0.25)) (put-text-property 2761 3100 'display worker-raise-spec))",
        ),
    );
}

#[test]
fn worker_capture_rejects_mutation_between_idle_steps() {
    let (mut eval, frame, _, window) =
        incr_editing_frame(&"ordinary offscreen text\n".repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str("(progn (setq worker-raise-spec (list 'raise 0.25)) (put-text-property 2761 3500 'display worker-raise-spec))").unwrap();
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    engine
        .begin_scroll_coverage(&eval, frame, window, CharPos0::new(2880))
        .unwrap();
    assert_eq!(engine.capture_scroll_step(&eval), Ok(true));
    eval.eval_str("(setcar (cdr worker-raise-spec) 0.75)")
        .unwrap();
    assert_eq!(
        engine.capture_scroll_step(&eval),
        Err(crate::row_layout::program::RowProgramError::Cancelled)
    );
}

#[test]
fn worker_admission_rejects_mutation_after_capture() {
    let (mut eval, frame, _, window) =
        incr_editing_frame(&"ordinary offscreen text\n".repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str("(progn (setq worker-raise-spec (list 'raise 0.25)) (put-text-property 2761 3500 'display worker-raise-spec))").unwrap();
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame);
    engine
        .request_scroll_coverage(&eval, frame, window, CharPos0::new(2880))
        .unwrap();
    eval.eval_str("(setcar (cdr worker-raise-spec) 0.75)")
        .unwrap();
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        match engine.scroll_coverage.drain(&mut engine.prepared_viewports) {
            Ok(false) => {
                assert!(std::time::Instant::now() < deadline);
                std::thread::yield_now();
            }
            result => {
                assert_eq!(
                    result,
                    Err(crate::row_layout::program::RowProgramError::Cancelled)
                );
                break;
            }
        }
    }
}
