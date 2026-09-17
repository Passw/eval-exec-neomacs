//! Freshness must agree with the geometry produced by a full layout.
use super::*;

fn assert_prefix_mutation_invalidates_geometry(variable: &str, mutation: &str) {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .unwrap()
        .insert(&"abcdefghij".repeat(20));
    let frame = eval
        .frame_manager_mut()
        .create_frame("prefix-freshness", 160, 160, buffer);
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str(&format!(
        r#"(progn (goto-char 1) (setq {variable} (copy-sequence "  ")))"#
    ))
    .unwrap();
    let mut engine = LayoutEngine::new_without_font_metrics();
    engine.layout_frame_rust(&mut eval, frame);
    let original = eval
        .fresh_window_display_snapshot(frame, window, buffer)
        .unwrap()
        .points
        .clone();

    engine.layout_frame_rust(&mut eval, frame);
    assert!(
        engine.last_layout_stats().reused_rows > 0,
        "unchanged prefixes retain row reuse"
    );
    let before = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    eval.eval_str(mutation).unwrap();
    let after = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    assert_ne!(
        before, after,
        "in-flight attempts detect the same prefix mutation"
    );
    let stale_accepted = eval
        .fresh_window_display_snapshot(frame, window, buffer)
        .is_some();
    let mut query_engine = WindowLayoutQueryEngine::new_without_font_metrics();
    let measured = query_engine
        .query_window_layout(
            &mut eval,
            frame,
            window,
            neovm_core::window::WindowLayoutQueryScope::Viewport,
        )
        .unwrap()
        .into_geometry()
        .unwrap()
        .points;
    engine.layout_frame_rust(&mut eval, frame);
    assert!(
        engine.last_layout_stats().full_windows > 0,
        "prefix changes invalidate row reuse"
    );
    let incremental = eval
        .fresh_window_display_snapshot(frame, window, buffer)
        .unwrap()
        .points
        .clone();
    let mut full = LayoutEngine::new_without_font_metrics();
    full.layout_frame_rust(&mut eval, frame);
    let reference = &eval
        .fresh_window_display_snapshot(frame, window, buffer)
        .unwrap()
        .points;
    assert_ne!(
        &original, reference,
        "the mutation must change actual layout geometry"
    );
    assert_eq!(
        &incremental, reference,
        "retained engine agrees with a fresh full layout"
    );
    assert_eq!(
        &measured, reference,
        "synchronous query agrees with redisplay"
    );
    assert!(
        !stale_accepted,
        "geometry from before a prefix width change must not be reported fresh"
    );
}

#[test]
fn retained_geometry_rejects_in_place_line_prefix_changes() {
    assert_prefix_mutation_invalidates_geometry("line-prefix", "(aset line-prefix 0 9)");
}

#[test]
fn retained_geometry_rejects_in_place_wrap_prefix_changes() {
    assert_prefix_mutation_invalidates_geometry("wrap-prefix", "(aset wrap-prefix 0 9)");
}

#[test]
fn retained_geometry_rejects_prefix_text_property_changes() {
    assert_prefix_mutation_invalidates_geometry(
        "line-prefix",
        r#"(put-text-property 0 1 'display '(space :width 6) line-prefix)"#,
    );
}

#[test]
fn restoring_prefix_bytes_restores_attempt_validity() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    let frame = eval
        .frame_manager_mut()
        .create_frame("prefix-restoration", 160, 160, buffer);
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str(r#"(setq line-prefix (copy-sequence "  "))"#)
        .unwrap();
    let before = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    eval.eval_str("(aset line-prefix 0 9)").unwrap();
    assert_ne!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(aset line-prefix 0 32)").unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
}
