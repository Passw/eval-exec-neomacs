//! Lisp-visible motion through the real row producer, not a synthetic matrix.

use super::*;
use neovm_core::window::WindowLayoutQueryOutcome;

#[test]
fn scrolling_restarts_after_fontification_moves_source_positions() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"row\n".repeat(40));
    let frame = eval
        .frame_manager_mut()
        .create_frame("scroll-fontification", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str(
        "(progn (set-window-buffer nil (current-buffer)) (goto-char 5) (set-window-start nil 5 t))",
    )
    .expect("initial marker-backed viewport");
    let mut display = LayoutEngine::new_without_font_metrics();
    display.layout_frame_rust(&mut eval, frame);
    activate_last_engine_presentation(&mut eval, &display, frame);
    eval.eval_str(
        r#"(progn (setq motion-fontified nil)
        (setq fontification-functions
          (list (lambda (_start)
            (if motion-fontified nil
              (progn (setq motion-fontified t)
                (save-excursion (goto-char 1) (insert "new\n"))))
            (put-text-property (point-min) (point-max) 'fontified t)))))"#,
    )
    .expect("fontification edits source");
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
        (scroll-up 0) (list motion-fontified (window-start) (point)))"#,
        )
        .expect("scroll survives fontification");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(t 9 9)"
    );
}

#[test]
fn measured_motion_distinguishes_overlay_insertions_from_replacements() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert("AAA\n\nBBB\n");
    eval.frame_manager_mut()
        .create_frame("overlay-motion", 400, 240, buffer);
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval.eval_str(r#"(let ((noninteractive nil))
        (mapcar (lambda (property)
          (let ((overlay (make-overlay 1 2)))
            (overlay-put overlay property "X\nY\n")
            (prog1
              (list
                (mapcar (lambda (n) (goto-char 1) (list (vertical-motion n) (point))) '(0 1 2 3 4 5))
                (mapcar (lambda (n) (goto-char 10) (list (vertical-motion n) (point))) '(-1 -2 -3 -4 -5)))
              (delete-overlay overlay)))) '(before-string after-string)))"#).expect("motion through overlay insertions");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((((0 1) (1 5) (2 6) (3 10) (3 10) (3 10)) ((-1 6) (-2 5) (-3 1) (-4 1) (-5 1))) (((0 1) (2 2) (3 5) (4 6) (5 10) (5 10)) ((-1 6) (-2 5) (-3 2) (-4 2) (-5 1))))"
    );
}

#[test]
fn measured_motion_reaches_an_invisible_accessible_beginning() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"row\n".repeat(100));
    eval.frame_manager_mut()
        .create_frame("hidden-beginning", 400, 160, buffer);
    eval.eval_str(
        r#"(progn (setq buffer-invisibility-spec '(hidden))
        (put-text-property 1 3 'invisible 'hidden) (goto-char 101))"#,
    )
    .expect("hidden prefix");
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
        (list (vertical-motion -1000) (point)))"#,
        )
        .expect("back past beginning");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(-25 1)"
    );
}

#[test]
fn page_scrolling_back_to_a_tall_row_keeps_point_in_the_new_viewport() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"row\n".repeat(40));
    let frame = eval
        .frame_manager_mut()
        .create_frame("pixel-paging", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str(
        r#"(progn
        (setq auto-window-vscroll nil scroll-preserve-screen-position nil)
        (put-text-property 1 2 'display '(space :height 20))
        (goto-char 5) (set-window-start nil 5 t))"#,
    )
    .expect("start below a tall row");
    let mut display = LayoutEngine::new_without_font_metrics();
    display.layout_frame_rust(&mut eval, frame);
    activate_last_engine_presentation(&mut eval, &display, frame);
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
        (scroll-down)
        (list (window-start) (point) (window-vscroll nil t)))"#,
        )
        .expect("back over tall row");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(1 1 0)"
    );
    display.layout_frame_rust(&mut eval, frame);
    activate_last_engine_presentation(&mut eval, &display, frame);
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
        (list (condition-case nil (scroll-down) (beginning-of-buffer 'at-start))
              (window-start) (point)))"#,
        )
        .expect("stay at beginning after redisplay");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(at-start 1 1)"
    );
}

#[test]
fn page_scrolling_a_tall_first_row_uses_pixels_and_returns_to_the_same_viewport() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"row\n".repeat(40));
    let frame = eval
        .frame_manager_mut()
        .create_frame("pixel-paging", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str(
        r#"(progn
        (setq auto-window-vscroll t scroll-preserve-screen-position nil)
        (put-text-property 1 2 'display '(space :height 20))
        (goto-char 1) (set-window-start nil 1 t))"#,
    )
    .expect("tall row");
    let mut display = LayoutEngine::new_without_font_metrics();
    display.layout_frame_rust(&mut eval, frame);
    activate_last_engine_presentation(&mut eval, &display, frame);
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil))
        (scroll-up)
        (let ((forward (list (window-start) (> (window-vscroll nil t) 0))))
          (scroll-down)
          (list forward (window-start) (window-vscroll nil t) (point))))"#,
        )
        .expect("page inside a tall row and back");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((1 t) 1 0 1)"
    );
}

#[test]
fn vertical_motion_measures_replacements_outside_the_presented_viewport() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"line\n".repeat(100));
    let frame = eval
        .frame_manager_mut()
        .create_frame("offscreen-motion", 400, 80, buffer);
    eval.eval_str(
        r#"(progn
        (put-text-property 201 206 'display "X\nY\n")
        (goto-char 1) (set-window-start nil 1 t))"#,
    )
    .expect("offscreen replacement");
    let mut display = LayoutEngine::new_without_font_metrics();
    display.layout_frame_rust(&mut eval, frame);
    let presentation = activate_last_engine_presentation(&mut eval, &display, frame);
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    // GNU -nw in a real PTY: the first step leaves both replacement rows;
    // the second reaches source line 43, with three physical rows crossed.
    let result = eval
        .eval_str(
            r#"(progn (goto-char 201)
        (let ((noninteractive nil))
          (list (vertical-motion 2) (point) (window-start))))"#,
        )
        .expect("offscreen motion");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(3 211 1)"
    );
    assert_eq!(
        eval.frame_manager()
            .get(frame)
            .expect("frame")
            .active_presentation(),
        Some(presentation)
    );
    let zero = eval
        .eval_str(
            r#"(progn (goto-char 201)
        (let ((noninteractive nil)) (list (vertical-motion 0) (point))))"#,
        )
        .expect("zero motion at a replacement following a buffer newline");
    assert_eq!(neovm_core::emacs_core::print::print_value(&zero), "(0 201)");
}

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
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
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
                         '(0 1 2)))"#,
        )
        .expect("motion leaves a wrapped replacement");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((0 5) (2 2) (3 5))"
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
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
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
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
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
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
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
