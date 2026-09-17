//! Freshness must agree with the geometry produced by a full layout.
use super::*;

fn assert_prefix_mutation_invalidates_geometry(variable: &str, mutation: &str) {
    assert_prefix_spec_mutation_invalidates_geometry(variable, r#"(copy-sequence "  ")"#, mutation);
}

fn assert_prefix_spec_mutation_invalidates_geometry(variable: &str, initial: &str, mutation: &str) {
    assert_layout_mutation_invalidates_geometry(&format!("(setq {variable} {initial})"), mutation);
}

fn assert_layout_mutation_invalidates_geometry(setup: &str, mutation: &str) {
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
    eval.eval_str(&format!(r#"(progn (goto-char 1) {setup})"#))
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
        "unchanged inputs retain row reuse"
    );
    let before = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    eval.eval_str(mutation).unwrap();
    let after = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
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
    let full_windows = engine.last_layout_stats().full_windows;
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
    assert_ne!(
        before, after,
        "in-flight attempts detect the same input mutation"
    );
    assert!(full_windows > 0, "input changes invalidate row reuse");
    assert_eq!(
        &measured, reference,
        "synchronous query agrees with redisplay"
    );
    assert!(
        !stale_accepted,
        "geometry from before a layout input change must not be reported fresh"
    );
}

#[test]
fn retained_geometry_rejects_mutated_invisibility_membership() {
    assert_layout_mutation_invalidates_geometry(
        "(setq buffer-invisibility-spec (list 'hidden)) (put-text-property 1 11 'invisible 'hidden)",
        "(setcar buffer-invisibility-spec 'visible)",
    );
}

#[test]
fn retained_geometry_rejects_mutated_invisibility_ellipsis() {
    assert_layout_mutation_invalidates_geometry(
        "(setq category (cons 'hidden nil) buffer-invisibility-spec (list category)) (put-text-property 1 31 'invisible 'hidden)",
        "(setcdr category t)",
    );
}

#[test]
fn invisibility_freshness_tracks_ellipsis_truthiness_not_identity() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    let frame = eval
        .frame_manager_mut()
        .create_frame("invisibility", 160, 160, buffer);
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str("(setq category (cons 'hidden t) buffer-invisibility-spec (list category))")
        .unwrap();
    let before = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    eval.eval_str("(setcdr category 'another-non-nil-value)")
        .unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcdr category nil)").unwrap();
    assert_ne!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcdr category t)").unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
}

#[test]
fn retained_geometry_rejects_in_place_line_prefix_changes() {
    assert_prefix_mutation_invalidates_geometry("line-prefix", "(aset line-prefix 0 9)");
}

#[test]
fn retained_geometry_rejects_mutated_nested_space_expression() {
    assert_prefix_spec_mutation_invalidates_geometry(
        "line-prefix",
        "(progn (setq dimension (list 16)) (list 'space :width (list '+ dimension 1)))",
        "(setcar dimension 48)",
    );
}

#[test]
fn retained_geometry_rejects_mutated_scaled_space_expression() {
    assert_prefix_spec_mutation_invalidates_geometry(
        "wrap-prefix",
        "(progn (setq dimension (cons 2 1)) (list 'space :width (list '- dimension 1)))",
        "(setcdr dimension 3)",
    );
}

#[test]
fn cyclic_pixel_input_capture_terminates_and_detects_operand_changes() {
    // Only capture freshness: evaluating cyclic arithmetic is not supported.
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    let frame = eval
        .frame_manager_mut()
        .create_frame("cyclic-prefix", 160, 160, buffer);
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str("(progn (setq dimension (list 1) line-prefix (list 'space :width (cons '+ dimension))) (setcdr dimension dimension))").unwrap();
    let before = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcar dimension 2)").unwrap();
    assert_ne!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcar dimension 1)").unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
}

#[test]
fn retained_geometry_rejects_mutated_space_prefix_width() {
    assert_prefix_spec_mutation_invalidates_geometry(
        "line-prefix",
        "(list 'space :width 2)",
        "(setcar (cdr (cdr line-prefix)) 6)",
    );
}

#[test]
fn retained_geometry_rejects_mutated_wrap_space_prefix_width() {
    assert_prefix_spec_mutation_invalidates_geometry(
        "wrap-prefix",
        "(list 'space :width 2)",
        "(setcar (cdr (cdr wrap-prefix)) 6)",
    );
}

#[test]
fn space_prefix_freshness_ignores_shadowed_and_unknown_operands() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    let frame = eval
        .frame_manager_mut()
        .create_frame("space-prefix", 160, 160, buffer);
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str("(setq tail (list :width 8 :unknown 9) line-prefix (cons 'space (cons :width (cons 2 tail))))").unwrap();
    let before = eval
        .window_layout_attempt_freshness(frame, window, buffer)
        .unwrap();
    eval.eval_str("(setcar (cdr tail) 20)").unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcar (cdr (cdr (cdr tail))) 30)").unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcar (cdr (cdr line-prefix)) 6)").unwrap();
    assert_ne!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
    eval.eval_str("(setcar (cdr (cdr line-prefix)) 2)").unwrap();
    assert_eq!(
        before,
        eval.window_layout_attempt_freshness(frame, window, buffer)
            .unwrap()
    );
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
