use super::*;

fn await_coverage(engine: &mut LayoutEngine) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    while !engine
        .scroll_coverage
        .drain(&mut engine.prepared_viewports)
        .unwrap()
    {
        assert!(std::time::Instant::now() < deadline);
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

#[test]
fn exported_scroll_surface_moves_paint_and_source_hits_without_changing_viewport() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame_id, buffer, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame_id)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame_id);
    let window_id = DisplayWindowId::new(window.0 as i64);
    let old = engine.retained_window_matrices[&window_id].clone();
    let rows: Vec<_> = old
        .matrix
        .rows
        .iter()
        .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
        .collect();
    let target = rows[rows.len() - 2].start_charpos;
    let row_height = rows[0].height_px;
    engine
        .request_scroll_coverage(&eval, frame_id, window, CharPos0::new(target))
        .unwrap();
    await_coverage(&mut engine);
    engine.layout_frame_rust(&mut eval, frame_id);
    let state = engine.last_frame_display_state.as_ref().unwrap();
    assert_eq!(
        state.scroll_coverage.len(),
        1,
        "a joined worker page must be exported"
    );
    let mut frame = state.materialize();
    assert_eq!(
        frame.scroll_surfaces.len(),
        1,
        "valid coverage must materialize"
    );
    let surface = frame.scroll_surfaces[0].clone();
    let viewport = surface.coverage().viewport;
    let before_hit = state.presented_hit_index.clone();
    assert_eq!(
        engine.retained_window_matrices[&window_id].key.window_start,
        old.key.window_start
    );
    assert_eq!(
        engine.retained_window_matrices[&window_id]
            .display_snapshot
            .rows,
        old.display_snapshot.rows
    );
    let offset = surface.clamp_offset(row_height);
    assert_eq!(offset, row_height);
    surface.paint(&mut frame, offset);
    let point = settled_point(frame.presentation_id, viewport.x + 2.0, viewport.y + 2.0);
    let hit = surface.hit(point, offset).unwrap().unwrap();
    assert_eq!(
        hit.text_position().unwrap().buffer_position(),
        line.len() as i64 + 1
    );
    assert_eq!(hit.text_position().unwrap().row(), 0);
    assert_eq!(hit.text_position().unwrap().bounds().y(), viewport.y);
    assert_eq!(hit.region().bounds().raw(), viewport);
    assert_eq!(
        before_hit
            .resolve(neomacs_display_protocol::PresentedHitQuery::new(point))
            .unwrap()
            .unwrap()
            .text_position()
            .unwrap()
            .buffer_position(),
        1
    );
    assert!(
        surface
            .hit(
                settled_point(
                    frame.presentation_id,
                    viewport.x + 2.0,
                    viewport.bottom() + 1.0
                ),
                offset
            )
            .unwrap()
            .is_none()
    );
    assert_eq!(surface.clamp_offset(-100_000.0), 0.0);
    assert!(surface.clamp_offset(100_000.0) > row_height);
    assert_eq!(surface.clamp_offset(f32::NAN), 0.0);

    scroll_window_to(
        &mut eval,
        frame_id,
        window,
        buffer,
        line.len() as i64 + 1,
        line.len(),
    );
    let mut canonical = LayoutEngine::new();
    canonical.layout_frame_rust(&mut eval, frame_id);
    let expected = canonical
        .last_frame_display_state
        .as_ref()
        .unwrap()
        .materialize();
    let chars = |frame: &neomacs_display_protocol::FrameGlyphBuffer| {
        frame
            .glyphs
            .iter()
            .filter_map(|glyph| match glyph {
                neomacs_display_protocol::FrameGlyph::Char {
                    window_id: owner,
                    row_role: GlyphRowRole::Text,
                    char: ch,
                    x,
                    y,
                    baseline,
                    width,
                    height,
                    ..
                } if *owner == window_id && *y < viewport.bottom() && *y + *height > viewport.y => {
                    Some((*ch, *x, *y, *baseline, *width, *height))
                }
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(chars(&frame), chars(&expected));
}

#[test]
fn exported_scroll_surface_rejects_non_contiguous_or_foreign_hit_geometry() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame_id, _, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame_id)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame_id);
    let window_id = DisplayWindowId::new(window.0 as i64);
    let rows: Vec<_> = engine.retained_window_matrices[&window_id]
        .matrix
        .rows
        .iter()
        .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
        .collect();
    let start = rows[rows.len() - 2].start_charpos;
    engine
        .request_scroll_coverage(&eval, frame_id, window, CharPos0::new(start))
        .unwrap();
    await_coverage(&mut engine);
    engine.layout_frame_rust(&mut eval, frame_id);
    let state = engine.last_frame_display_state.as_ref().unwrap();
    let original = state.scroll_coverage[0].clone();
    let mut broken = (*original).clone();
    neomacs_display_protocol::glyph_matrix::MatrixRow::make_mut(
        &mut broken.content.matrix.rows[1],
    )
    .pixel_y += 1.0;
    assert!(std::sync::Arc::new(broken).materialize(state).is_none());
    let mut broken = (*original).clone();
    broken.content.window_id = DisplayWindowId::new(9_999);
    assert!(std::sync::Arc::new(broken).materialize(state).is_none());
    eval.eval_str("(goto-char 1) (insert \"changed\")").unwrap();
    engine.layout_frame_rust(&mut eval, frame_id);
    assert!(
        engine
            .last_frame_display_state
            .as_ref()
            .unwrap()
            .scroll_coverage
            .is_empty()
    );
}

#[test]
fn exported_backward_coverage_keeps_nonnegative_storage_and_visible_row_hit_coordinates() {
    let line = "ordinary offscreen text\n";
    let (mut eval, frame_id, buffer, window) = incr_editing_frame(&line.repeat(300), 800, 600);
    eval.frame_manager_mut()
        .get_mut(frame_id)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str(&format!(
        "(put-text-property {} {} 'face '(:height 180 :family \"monospace\" :weight bold))",
        50 * line.len() + 1,
        53 * line.len()
    ))
    .unwrap();
    scroll_window_to(
        &mut eval,
        frame_id,
        window,
        buffer,
        40 * line.len() as i64 + 1,
        40 * line.len(),
    );
    let mut engine = LayoutEngine::new();
    engine.layout_frame_rust(&mut eval, frame_id);
    engine
        .request_scroll_coverage(&eval, frame_id, window, CharPos0::new(30 * line.len()))
        .unwrap();
    await_coverage(&mut engine);
    let window_id = DisplayWindowId::new(window.0 as i64);
    let rows: Vec<_> = engine.retained_window_matrices[&window_id]
        .matrix
        .rows
        .iter()
        .filter(|row| row.enabled && row.role == GlyphRowRole::Text)
        .collect();
    let target = rows[rows.len() - 2].start_charpos;
    engine
        .request_scroll_coverage(&eval, frame_id, window, CharPos0::new(target))
        .unwrap();
    await_coverage(&mut engine);
    engine.layout_frame_rust(&mut eval, frame_id);
    let state = engine.last_frame_display_state.as_ref().unwrap();
    assert_eq!(state.scroll_coverage.len(), 1);
    let frame = state.materialize();
    let surface = &frame.scroll_surfaces[0];
    assert!(surface.coverage().origin > 0.0);
    assert!(surface.clamp_offset(-1000.0) < -4.0);
    let viewport = surface.coverage().viewport;
    let point = settled_point(frame.presentation_id, viewport.x + 2.0, viewport.y + 1.0);
    let hit = surface.hit(point, -4.0).unwrap().unwrap();
    assert_eq!(
        hit.text_position().unwrap().buffer_position(),
        39 * line.len() as i64 + 1
    );
    assert_eq!(hit.text_position().unwrap().row(), 0);
    assert_eq!(hit.text_position().unwrap().bounds().y(), viewport.y);
    assert_eq!(hit.region().bounds().raw(), viewport);
    assert!(
        surface
            .coverage()
            .content
            .matrix
            .rows
            .iter()
            .any(|row| row.height_px > frame.char_height)
    );
}
