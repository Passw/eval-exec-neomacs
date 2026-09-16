//! Lisp-visible motion through the real row producer, not a synthetic matrix.

use super::*;
use neovm_core::window::WindowLayoutQueryOutcome;

#[test]
fn vertical_motion_leaves_a_wrapped_replacement_before_taking_another_step() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert("ABC\n\nDEF\n");
    eval.frame_manager_mut()
        .create_frame("wrapped-display-motion", 400, 240, buffer);
    eval.eval_str(
        "(put-text-property 1 2 'display (make-string (1+ (* 2 (window-body-width))) ?X))",
    )
    .expect("wrapped replacement");
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window| {
        match query.query_window_layout(eval, frame, window) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
                 (mapcar (lambda (n)
                           (goto-char 1)
                           (list (vertical-motion n) (point)))
                         '(1 2)))"#,
        )
        .expect("motion leaves a wrapped replacement");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((2 2) (3 5))"
    );
}

#[test]
fn vertical_motion_remeasures_display_rows_after_a_display_property_change() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert("AAA\n\nBBB\n");
    let frame = eval
        .frame_manager_mut()
        .create_frame("display-motion", 400, 240, buffer);
    eval.eval_str("(goto-char 1)").expect("initial point");
    let mut display = LayoutEngine::new_without_font_metrics();
    display.layout_frame_rust(&mut eval, frame);
    let presentation = activate_last_engine_presentation(&mut eval, &display, frame);
    let presented_frame = eval.frame_manager().get(frame).expect("frame");
    let window = presented_frame.selected_window;
    let retained = presented_frame.redisplay_snapshot(window).cloned();

    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window| {
        match query.query_window_layout(eval, frame, window) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(progn
                 (put-text-property 1 5 'display "X\n")
                 (let ((noninteractive nil))
                   (list (vertical-motion 2) (point) (window-start))))"#,
        )
        .expect("interactive motion through changed display text");

    // GNU's interactive iterator counts X, the blank line, then BBB.
    // The column-only scanner swallows the display-string newline and lands
    // at 10 instead. Changing point must not publish a speculative viewport.
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(2 6 1)"
    );
    assert_eq!(
        eval.frame_manager()
            .get(frame)
            .expect("frame")
            .active_presentation(),
        Some(presentation),
        "a motion query must not replace the renderer's presentation"
    );
    let presented_frame = eval.frame_manager().get(frame).expect("frame");
    assert_eq!(
        presented_frame.redisplay_snapshot(window),
        retained.as_ref()
    );
    assert!(!presented_frame.has_prepared_display_presentations());
    // These expectations come from a real GNU -nw session. Binding Lisp's
    // `noninteractive` under GNU --batch does not switch its C motion engine.
    let zero = eval
        .eval_str(
            "(progn (goto-char 1) (let ((noninteractive nil)) (list (vertical-motion 0) (point))))",
        )
        .expect("start of a row occupied by a replacement");
    assert_eq!(neovm_core::emacs_core::print::print_value(&zero), "(0 5)");
    let multi_line = eval
        .eval_str(
            r#"(progn
                 (put-text-property 1 5 'display "X\nY\n")
                 (let ((noninteractive nil))
                   (mapcar (lambda (n)
                             (goto-char 1)
                             (list (vertical-motion n) (point)))
                           '(0 1 2 3 4))))"#,
        )
        .expect("motion across multiple display-string newlines");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&multi_line),
        "((0 5) (2 5) (3 6) (4 10) (4 10))"
    );
    let backward = eval
        .eval_str(
            r#"(let ((noninteractive nil))
                 (mapcar (lambda (n)
                           (goto-char (point-max))
                           (list (vertical-motion n) (point)))
                         '(-1 -2 -3 -4 -5)))"#,
        )
        .expect("backward motion keeps physical display-row distances");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&backward),
        "((-1 6) (-2 5) (-3 1) (-4 1) (-4 1))"
    );
    let mixed_row_goals = eval
        .eval_str(
            r#"(progn
                 (put-text-property 1 5 'display "X\nY")
                 (let ((noninteractive nil))
                   (mapcar (lambda (goal)
                             (goto-char 1)
                             (list (vertical-motion goal) (point)))
                           '(1 (0 . 1) (1 . 1) (2 . 1)))))"#,
        )
        .expect("goal column cannot return inside a replacement just left");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&mixed_row_goals),
        "((1 5) (1 5) (1 5) (1 5))"
    );
    let mixed_row_zero = eval
        .eval_str(
            r#"(let ((noninteractive nil))
                 (mapcar (lambda (goal)
                           (goto-char 5)
                           (list (vertical-motion goal) (point)))
                         '(0 (0 . 0) (1 . 0) (2 . 0))))"#,
        )
        .expect("zero motion at the buffer text following a replacement");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&mixed_row_zero),
        "((0 5) (0 5) (0 5) (0 5))"
    );
}

#[test]
fn vertical_motion_counts_measured_rows_at_accessible_boundaries() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert("AAA\n\nBBB\n");
    eval.frame_manager_mut()
        .create_frame("display-motion", 400, 240, buffer);
    eval.eval_str(r#"(put-text-property 1 5 'display "X\n")"#)
        .expect("display replacement");
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window| {
        match query.query_window_layout(eval, frame, window) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
                 (list
                   (progn (goto-char 1)
                          (list (vertical-motion 4) (point)))
                   (progn (goto-char (point-max))
                          (list (vertical-motion -4) (point)))))"#,
        )
        .expect("motion through accessible boundaries");

    // GNU counts the rendered replacement newline in both directions, even
    // when fewer rows remain than were requested.
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((3 10) (-3 1))"
    );
    let goal = eval
        .eval_str(
            r#"(progn
                 (remove-text-properties (point-min) (point-max) '(display nil))
                 (goto-char (point-max))
                 (let ((noninteractive nil))
                   (list (vertical-motion '(2 . -4)) (point))))"#,
        )
        .expect("goal column at the accessible beginning");
    assert_eq!(neovm_core::emacs_core::print::print_value(&goal), "(-3 3)");
}

#[test]
fn vertical_motion_does_not_treat_measured_viewport_edges_as_buffer_boundaries() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"line\n".repeat(80));
    let frame = eval
        .frame_manager_mut()
        .create_frame("display-motion", 400, 80, buffer);
    // Start halfway through a buffer much taller than the measured viewport.
    eval.eval_str("(progn (goto-char 201) (set-window-start nil 201 t))")
        .expect("middle viewport");
    let mut display = LayoutEngine::new_without_font_metrics();
    display.layout_frame_rust(&mut eval, frame);
    activate_last_engine_presentation(&mut eval, &display, frame);
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
                 (list (list (vertical-motion -20) (point))
                       (progn (goto-char 201)
                              (list (vertical-motion 20) (point)))
                       (window-start)))"#,
        )
        .expect("motion beyond measured rows");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((-20 101) (20 301) 201)"
    );
}

#[test]
fn vertical_motion_preserves_a_labeled_accessible_region_during_measurement() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert("a\nb\nc\nd\ne\n");
    eval.frame_manager_mut()
        .create_frame("display-motion", 400, 240, buffer);
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window| {
        match query.query_window_layout(eval, frame, window) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(progn
                 (goto-char 5)
                 (internal--labeled-narrow-to-region 5 7 'motion-test)
                 (let ((noninteractive nil))
                   (list (point-min) (point-max) (vertical-motion 2) (point))))"#,
        )
        .expect("motion within a labeled restriction");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(5 7 1 7)"
    );
}
