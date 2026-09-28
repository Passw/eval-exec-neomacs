//! Lisp-visible motion through the real row producer, not a synthetic matrix.

use super::*;
use neovm_core::window::WindowLayoutQueryOutcome;

#[test]
fn redisplay_keeps_a_fully_visible_cursor_row_when_its_source_line_continues() {
    use neovm_core::window::WindowLayoutQueryScope;
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    eval.buffer_manager_mut().get_mut(buffer).unwrap().insert(&format!("head\n{}\n", "x".repeat(2000)));
    let frame = eval.frame_manager_mut().create_frame("visible-wrap-cursor", 160, 160, buffer);
    eval.frame_manager_mut().get_mut(frame).unwrap().window_system = Some(Value::symbol("neomacs"));
    eval.eval_str("(setq mode-line-format nil header-line-format nil tab-line-format nil) (goto-char 1) (set-window-start nil 1 t)").unwrap();
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    let snapshot = query.query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport).unwrap().into_geometry().unwrap();
    let body = snapshot.regions.text_body;
    let outer = snapshot.regions.outer;
    let row = snapshot.rows.iter().rev().find(|row| row.start_buffer_pos.is_some()
        && row.y as f32 + outer.y >= body.y
        && (row.y + row.height) as f32 + outer.y <= body.bottom()).unwrap();
    let point = row.start_buffer_pos.unwrap();
    assert!(point.as_i64() > 6 && point.as_i64() < 2000);
    eval.eval_str(&format!("(goto-char {}) (set-window-start nil 1 t)", point.as_i64())).unwrap();
    let mut engine = LayoutEngine::new_without_font_metrics();
    engine.layout_frame_rust(&mut eval, frame);
    assert_eq!(eval.eval_str("(window-start)").unwrap(), Value::fixnum(1),
        "a complete visible screen row must not scroll to expose the rest of its physical line");
    assert_eq!(eval.eval_str("(point)").unwrap(), Value::fixnum(point.as_i64()));
}

#[test]
fn small_vertical_motion_measures_only_the_needed_rows() {
    use neovm_core::window::WindowLayoutQueryScope;
    use std::{cell::RefCell, rc::Rc};
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .unwrap()
        .insert(&"row\n".repeat(1000));
    let frame = eval
        .frame_manager_mut()
        .create_frame("motion-budget", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let counts = Rc::new(RefCell::new(Vec::new()));
    let observed = counts.clone();
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        if let WindowLayoutQueryScope::Rows { count, .. } = &scope {
            observed.borrow_mut().push(count.get());
        }
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval.eval_str("(progn (goto-char 2001) (let ((noninteractive nil)) (list (vertical-motion 1) (point) (vertical-motion -1) (point))))").unwrap();
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(1 2005 -1 2001)"
    );
    let counts = counts.borrow();
    assert!(
        !counts.is_empty(),
        "must exercise offscreen row measurement"
    );
    assert!(
        counts.iter().all(|count| *count <= 4),
        "one-row motion overmeasured: {counts:?}"
    );
}

#[test]
fn small_backward_pixel_measurement_starts_with_bounded_rows() {
    use neovm_core::window::WindowLayoutQueryScope;
    use std::{cell::RefCell, rc::Rc};
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    eval.buffer_manager_mut().get_mut(buffer).unwrap().insert(&"row\n".repeat(1000));
    let frame = eval.frame_manager_mut().create_frame("pixel-budget", 400, 160, buffer);
    eval.frame_manager_mut().get_mut(frame).unwrap().window_system = Some(Value::symbol("neomacs"));
    let counts = Rc::new(RefCell::new(Vec::new()));
    let observed = counts.clone();
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        if let WindowLayoutQueryScope::Rows { count, .. } = &scope {
            observed.borrow_mut().push(count.get());
        }
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval.eval_str("(progn (goto-char 2001) (cdr (window-text-pixel-size nil '(2001 . -1) 2001 nil nil nil t)))").unwrap();
    assert_eq!(neovm_core::emacs_core::print::print_value(&result), "(16 1997)");
    assert!(!counts.borrow().is_empty());
    assert!(counts.borrow().iter().all(|count| *count <= 4), "one-pixel measurement overmeasured: {:?}", counts.borrow());
}

#[test]
fn backward_pixel_measurement_uses_the_offscreen_rows_actual_height() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .unwrap()
        .insert("row\nnext\n");
    let frame = eval
        .frame_manager_mut()
        .create_frame("pixel-height", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str("(progn (put-text-property 1 2 'display '(space :height 4)) (goto-char 5) (set-window-start nil 5 t))").unwrap();
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval.eval_str("(list (mapcar (lambda (offset) (cdr (window-text-pixel-size nil (cons 5 offset) 5 nil nil nil t))) '(-1 -63 -64 -65)) (window-start) (point))").unwrap();
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(((64 1) (64 1) (64 1) (64 1)) 5 5)"
    );
    let result = eval.eval_str("(progn (put-text-property 1 2 'display '(space :height 2)) (cdr (window-text-pixel-size nil '(5 . -1) 5 nil nil nil t)))").unwrap();
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(32 1)"
    );
}

#[test]
fn backward_pixel_measurement_expands_past_wrapped_source_lines() {
    use neovm_core::window::WindowLayoutQueryScope;
    use std::num::NonZeroUsize;
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .unwrap()
        .insert(&format!("{}\nnext\n", "x".repeat(2000)));
    let frame = eval
        .frame_manager_mut()
        .create_frame("pixel-wrap", 160, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    let snapshot = query
        .query_window_layout(
            &mut eval,
            frame,
            window,
            WindowLayoutQueryScope::Rows {
                start: LispCharPos1::ONE,
                count: NonZeroUsize::new(256).unwrap(),
            },
        )
        .unwrap()
        .into_geometry()
        .unwrap();
    let rows: Vec<_> = snapshot
        .rows
        .iter()
        .filter(|row| row.start_buffer_pos.is_some())
        .collect();
    let anchor = rows
        .iter()
        .position(|row| row.start_buffer_pos == Some(LispCharPos1::new(2002)))
        .unwrap();
    assert!(
        anchor > 16,
        "exercise expansion beyond the initial query budget"
    );
    let preceding = rows[anchor - 1];
    let expected = format!(
        "({} {})",
        rows[anchor].y - preceding.y,
        preceding.start_buffer_pos.unwrap().as_i64()
    );
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str("(cdr (window-text-pixel-size nil '(2002 . -1) 2002 nil nil nil t))")
        .unwrap();
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        expected
    );
    // The same ambiguity occurs at a continuation boundary shared with the
    // preceding row's end, not only after the source newline.
    let origin = preceding.start_buffer_pos.unwrap().as_i64();
    let earlier = rows[anchor - 2];
    let result = eval
        .eval_str(&format!(
            "(cdr (window-text-pixel-size nil '({origin} . -1) {origin} nil nil nil t))"
        ))
        .unwrap();
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        format!(
            "({} {})",
            preceding.y - earlier.y,
            earlier.start_buffer_pos.unwrap().as_i64()
        )
    );
}

#[test]
fn consecutive_scroll_commands_preserve_the_original_goal_past_short_rows() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"abcdefghij\nx\nabcdefghij\n".repeat(10));
    let frame = eval
        .frame_manager_mut()
        .create_frame("scroll-goal", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil)
      (scroll-preserve-screen-position 'always) (last-command nil))
      (put 'scroll-up 'scroll-command t)
      (goto-char 6) (set-window-start nil 1 t)
      (scroll-up 1)
      (let ((short-row (list (window-start) (point))))
        (setq last-command 'scroll-up)
        (scroll-up 1)
        (let ((long-row (list (window-start) (point))))
          (goto-char 16) (setq last-command 'forward-char)
          (scroll-up 1)
          (list short-row long-row (list (window-start) (point))))))"#,
        )
        .expect("retain the original pixel goal across scroll commands");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((12 13) (14 19) (25 27))"
    );
}

#[test]
fn graphical_scrolling_honors_screen_position_and_scroll_margin() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"row\n".repeat(40));
    let frame = eval
        .frame_manager_mut()
        .create_frame("scroll-policy", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
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
      (setq scroll-preserve-screen-position 'always scroll-margin 0)
      (goto-char 10) (set-window-start nil 1 t) (scroll-up 1)
      (let ((preserved (list (window-start) (point))))
        (setq scroll-preserve-screen-position nil scroll-margin 2 maximum-scroll-margin 0.5)
        (goto-char 1) (set-window-start nil 1 t) (scroll-up 1)
        (list preserved (list (window-start) (point)))))"#,
        )
        .expect("scroll policy");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((5 14) (5 13))"
    );
}

#[test]
fn motion_backtracking_does_not_reseat_inside_a_multiline_replacement() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert("AAA\n\nBBB\nCCC\n");
    eval.frame_manager_mut()
        .create_frame("source-boundary", 400, 160, buffer);
    eval.eval_str(r#"(put-text-property 1 7 'display "X")"#)
        .expect("replacement covers newlines");
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(progn (goto-char 8)
        (let ((noninteractive nil)) (list (vertical-motion 0) (point))))"#,
        )
        .expect("backtrack over replacement owner");
    assert_eq!(neovm_core::emacs_core::print::print_value(&result), "(0 1)");
}

#[test]
fn offscreen_row_queries_preserve_automatic_composition_metrics() {
    use neovm_core::window::{DisplayPointRole, WindowLayoutQueryScope};
    use std::num::NonZeroUsize;
    let mut eval = Context::new();
    crate::test_composition::install_rules(&mut eval);
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    let text = format!("{}👩‍💻Z\n", format!("{}\n", "a".repeat(40)).repeat(240));
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&text);
    let frame = eval
        .frame_manager_mut()
        .create_frame("offscreen-composition", 400, 160, buffer);
    let window = eval
        .frame_manager()
        .get(frame)
        .expect("frame")
        .selected_window;
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    let mut widths = Vec::new();
    for (start, count) in [(9841, 2), (1, 256)] {
        let snapshot = query
            .query_window_layout(
                &mut eval,
                frame,
                window,
                WindowLayoutQueryScope::Rows {
                    start: LispCharPos1::new(start),
                    count: NonZeroUsize::new(count).expect("row budget"),
                },
            )
            .expect("canonical row query")
            .into_geometry()
            .expect("geometry");
        widths.push(
            snapshot
                .points
                .iter()
                .find(|point| {
                    point.role == DisplayPointRole::Glyph
                        && point.buffer_pos == LispCharPos1::new(9841)
                })
                .expect("emoji source position in measured rows")
                .width,
        );
    }
    assert_eq!(
        widths,
        vec![32, 32],
        "query distance must not change composition"
    );
}

#[test]
fn scrolling_restarts_after_fontification_moves_source_positions() {
    for window_system in [None, Some(Value::symbol("neomacs"))] {
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
            .window_system = window_system;
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
fn scrolling_promotes_a_bottom_clipped_point_row_before_pixel_scrolling() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&"row\n".repeat(40));
    let frame = eval
        .frame_manager_mut()
        .create_frame("clipped-point-row", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str(
        r#"(progn
      (setq auto-window-vscroll t scroll-preserve-screen-position nil)
      (put-text-property 9 10 'display '(space :height 20))
      (goto-char 9) (set-window-start nil 1 t))"#,
    )
    .expect("point on clipped third row");
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
      (scroll-up 1)
      (let ((promoted (list (window-start) (point) (window-vscroll nil t))))
        (scroll-up 1)
        (list promoted (window-start) (point) (> (window-vscroll nil t) 0))))"#,
        )
        .expect("promote then pixel-scroll the tall point row");
    // GNU window_scroll_pixel_based first moves start to the clipped point
    // row, keeping point; only the next scroll changes its pixel offset.
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((9 9 0) 9 9 t)"
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

/// A point past the right margin of a truncated line is on that line's row for
/// the scrolling planner too.
///
/// The planner asks the same "which row holds this position" question motion
/// does.  A position it cannot place reads as a point that has left the window:
/// the plan recenters the viewport around it, or -- when the measured rows reach
/// the end of the buffer -- fails with "Scroll origin is outside measured source
/// coverage".  GNU walks the display iterator from the start of the origin's
/// line and backtracks when the walk overshoots a line truncated on the right
/// (src/indent.c:2393-2400, the same walk `window_scroll_pixel_based` uses),
/// which puts such a point on the truncated row.
#[test]
fn scrolling_places_a_point_past_the_right_margin_on_the_truncated_row() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    // A long truncated line early, and enough lines after it that the window can
    // start well below point: the buffer's own end is then inside the rows the
    // measurement reaches, which is what turns the unplaced origin into a
    // failure rather than one more measurement.
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&format!(
            "line\n{}\n{}",
            "x".repeat(3000),
            "line\n".repeat(60)
        ));
    let frame = eval
        .frame_manager_mut()
        .create_frame("scroll-truncated-line", 400, 240, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .expect("frame")
        .window_system = Some(Value::symbol("neomacs"));
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil) (truncate-lines t))
                 (set-window-hscroll nil 50)
                 (goto-char 2500)
                 (set-window-start nil 307 t)
                 (scroll-down 1)
                 (list (window-start) (point)))"#,
        )
        .expect("scroll with a point past the right margin of a truncated line");
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "(1 2500)"
    );
}

/// A position past the right margin of a truncated line belongs to that line's
/// row.
///
/// GNU never asks which row holds a position: `Fvertical_motion` reseats at the
/// start of the origin's line, walks forward to the origin, and when that walk
/// overshoots a line truncated on the right (`it.line_wrap == TRUNCATE &&
/// it.current_x >= it.last_visible_x`) backtracks one line, landing back on the
/// truncated row itself (src/indent.c:2393-2400).  Nothing implemented that for
/// rows, so the newline ending a truncated line was in no row at all, and
/// `C-e` -- whose `line-move-1` walks to `(line-end-position)` and then asks for
/// a screen line -- was answered with "Display motion origin is outside measured
/// source coverage" instead of reaching the end of its line.
#[test]
fn vertical_motion_reaches_the_end_of_a_truncated_line() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .expect("buffer")
        .insert(&format!("{}\n", "x".repeat(300)));
    // Narrow enough that the line cannot fit at any plausible character width,
    // so redisplay truncates it -- which is the premise of this case.
    eval.frame_manager_mut()
        .create_frame("display-motion", 160, 240, buffer);
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    eval.install_window_layout_query(move |eval, frame, window, scope| {
        match query.query_window_layout(eval, frame, window, scope) {
            Ok(query) => WindowLayoutQueryOutcome::Ready(query),
            Err(error) => WindowLayoutQueryOutcome::Failed(error),
        }
    });
    let result = eval
        .eval_str(
            r#"(let ((noninteractive nil) (truncate-lines t))
                 (list
                   (progn (goto-char (point-min))
                          (list (vertical-motion 1) (point)))
                   (progn (goto-char 301)
                          (list (vertical-motion 0) (point)))
                   (progn (goto-char 301)
                          (list (vertical-motion 1) (point)))))"#,
        )
        .expect("screen-line motion from inside a truncated line");

    // Measured against GNU Emacs 31 on the same buffer: one screen line down from
    // the beginning is the empty line after the newline, zero lines from the
    // newline answers the start of its own screen line, and one line from it
    // reaches that empty line.  Before the row lookup learned the overflow rule,
    // the zero-line probe signalled instead of answering.
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&result),
        "((1 302) (0 1) (1 302))"
    );
}

#[test]
fn graphical_posn_queries_follow_live_scroll_before_presentation() {
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().expect("buffer").id();
    let text: String = (0..400)
        .map(|i| format!("Line {i:03} -- native scrolling diagnostic\n"))
        .collect();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .unwrap()
        .insert(&text);
    let frame = eval
        .frame_manager_mut()
        .create_frame("live-posn", 1000, 700, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    eval.eval_str(
        "(progn (goto-char 881) (set-window-start nil 801 t) (set-window-vscroll nil 12 t))",
    )
    .unwrap();
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
    let observe = |eval: &mut Context| {
        let value = eval.eval_str(
            "(let ((noninteractive nil)) (list (nth 1 (posn-at-x-y 0 100 (selected-window))) (nth 2 (posn-at-point 881))))",
        ).unwrap();
        neovm_core::emacs_core::print::print_value(&value)
    };
    let old = observe(&mut eval);
    eval.eval_str("(progn (set-window-start nil 721 t) (set-window-vscroll nil 14 t))")
        .unwrap();
    let before_presentation = observe(&mut eval);
    let positions = eval
        .eval_str("(list (window-start) (window-vscroll nil t) (point))")
        .unwrap();
    assert_eq!(
        neovm_core::emacs_core::print::print_value(&positions),
        "(721 14 881)"
    );
    display.layout_frame_rust(&mut eval, frame);
    let after_layout = observe(&mut eval);
    activate_last_engine_presentation(&mut eval, &display, frame);
    let after_presentation = observe(&mut eval);
    assert_ne!(old, after_presentation, "the viewport really moved");
    assert_eq!(
        after_layout, after_presentation,
        "completed redisplay rows must answer before renderer activation"
    );
    assert_eq!(
        before_presentation, after_presentation,
        "Lisp coordinate queries must follow live scrolling before renderer acknowledgement"
    );
}

#[test]
fn identical_geometry_queries_reuse_rows_and_mutations_force_a_new_walk() {
    use crate::engine::viewport_retry_depth_probe as probe;
    use neovm_core::window::WindowLayoutQueryScope;
    use std::num::NonZeroUsize;
    for scope in [
        WindowLayoutQueryScope::Viewport,
        WindowLayoutQueryScope::Rows {
            start: LispCharPos1::ONE,
            count: NonZeroUsize::new(8).unwrap(),
        },
    ] {
        let mut eval = Context::new();
        let buffer = eval.buffer_manager().current_buffer().unwrap().id();
        eval.buffer_manager_mut()
            .get_mut(buffer)
            .unwrap()
            .insert(&"ordinary row\n".repeat(100));
        let frame = eval
            .frame_manager_mut()
            .create_frame("query-cache", 400, 160, buffer);
        eval.frame_manager_mut()
            .get_mut(frame)
            .unwrap()
            .window_system = Some(Value::symbol("neomacs"));
        let window = eval.frame_manager().get(frame).unwrap().selected_window;
        let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
        let initial = query
            .query_window_layout(&mut eval, frame, window, scope)
            .unwrap();
        probe::reset();
        let repeated = query
            .query_window_layout(&mut eval, frame, window, scope)
            .unwrap();
        assert_eq!(initial.end(), repeated.end());
        assert_eq!(
            probe::max_depth(),
            0,
            "identical query walked the body again: {scope:?}"
        );
        for change in [
            "(put-text-property 1 8 'face '(:height 175))",
            r#"(overlay-put (make-overlay 1 20) 'display "replacement")"#,
            "(goto-char 20)",
            "(set-window-vscroll nil 3 t)",
            "(setq tab-width 3)",
            "(narrow-to-region 1 100)",
            "(progn (overlay-put (make-overlay 30 40) 'category 'query-face-category) (put 'query-face-category 'face '(:height 125)))",
            "(put 'query-face-category 'face '(:height 200))",
            r#"(progn (setq query-replacement (copy-sequence "a")) (put-text-property 50 51 'display query-replacement))"#,
            "(aset query-replacement 0 9)",
            "(progn (setq query-face (list :height 100)) (put-text-property 50 51 'face query-face))",
            "(setcar (cdr query-face) 200)",
        ] {
            eval.eval_str(change).unwrap();
            probe::reset();
            let actual = query
                .query_window_layout(&mut eval, frame, window, scope)
                .unwrap();
            assert!(
                probe::max_depth() > 0,
                "mutation reused stale rows: {change}"
            );
            let mut fresh = WindowLayoutQueryEngine::new_without_font_metrics();
            let expected = fresh
                .query_window_layout(&mut eval, frame, window, scope)
                .unwrap();
            assert_eq!(actual.end(), expected.end(), "{change}");
            let actual = actual.into_geometry().unwrap();
            let expected = expected.into_geometry().unwrap();
            assert_eq!(actual.rows, expected.rows, "{change}");
            assert_eq!(actual.points, expected.points, "{change}");
        }
    }
}

#[test]
fn geometry_queries_reevaluate_conditional_display_without_source_edits() {
    use neovm_core::window::WindowLayoutQueryScope;
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    eval.buffer_manager_mut()
        .get_mut(buffer)
        .unwrap()
        .insert("ordinary row\nnext row\n");
    let frame = eval
        .frame_manager_mut()
        .create_frame("query-conditions", 400, 160, buffer);
    eval.frame_manager_mut()
        .get_mut(frame)
        .unwrap()
        .window_system = Some(Value::symbol("neomacs"));
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str(r#"(progn (setq query-condition t query-calls 0) (put-text-property 1 2 'display '(when (progn (setq query-calls (1+ query-calls)) query-condition) . "REPLACED")))"#).unwrap();
    let mut query = WindowLayoutQueryEngine::new_without_font_metrics();
    query
        .query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport)
        .unwrap();
    let before = eval.eval_str("query-calls").unwrap().as_fixnum().unwrap();
    eval.eval_str("(setq query-condition nil)").unwrap();
    let actual = query
        .query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport)
        .unwrap()
        .into_geometry()
        .unwrap();
    let after = eval.eval_str("query-calls").unwrap().as_fixnum().unwrap();
    assert!(
        after > before,
        "query cache skipped Lisp display conditions"
    );
    let mut fresh = WindowLayoutQueryEngine::new_without_font_metrics();
    let expected = fresh
        .query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport)
        .unwrap()
        .into_geometry()
        .unwrap();
    assert_eq!(actual.rows, expected.rows);
    assert_eq!(actual.points, expected.points);
}

#[test]
fn pixel_only_queries_reuse_complete_row_geometry() {
    use crate::engine::viewport_retry_depth_probe as probe;
    use neovm_core::window::WindowLayoutQueryScope;
    let mut eval = Context::new();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    eval.buffer_manager_mut().get_mut(buffer).unwrap().insert(&"ordinary row\n".repeat(100));
    let frame = eval.frame_manager_mut().create_frame("query-pixel-placement", 400, 170, buffer);
    eval.frame_manager_mut().get_mut(frame).unwrap().window_system = Some(Value::symbol("neomacs"));
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    eval.eval_str("(setq mode-line-format nil header-line-format nil tab-line-format nil) (goto-char 30) (set-window-vscroll nil 2 t t)").unwrap();
    let mut query = LayoutEngine::new_without_font_metrics();
    query.query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport).unwrap();
    eval.eval_str("(set-window-vscroll nil 3 t t)").unwrap();
    probe::reset();
    let actual = query.query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport).unwrap();
    let walks = probe::max_depth();
    let mut fresh = WindowLayoutQueryEngine::new_without_font_metrics();
    let expected = fresh.query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport).unwrap();
    assert_eq!(actual.end(), expected.end());
    assert_eq!(actual.geometry(), expected.geometry());
    assert_eq!(walks, 0, "pixel placement rewalked unchanged complete rows");
}

#[test]
fn pixel_query_placement_matches_fresh_wrapped_and_decorated_rows() {
    use crate::engine::viewport_retry_depth_probe as probe;
    use neovm_core::window::WindowLayoutQueryScope;
    for decoration in [
        "nil",
        "(put-text-property 1 121 'face '(:height 1.5))",
        r#"(progn (put-text-property 1 121 'line-height 1.3)
            (put-text-property 31 35 'display '(raise 0.2))
            (let ((o (make-overlay 90 110)))
              (overlay-put o 'before-string "prefix")
              (overlay-put o 'after-string "suffix")
              (overlay-put o 'face '(:height 1.2))))"#,
    ] {
        let mut eval = Context::new();
        let buffer = eval.buffer_manager().current_buffer().unwrap().id();
        eval.buffer_manager_mut().get_mut(buffer).unwrap().insert(&format!("{}\n", "abc def ghi ".repeat(20)).repeat(30));
        let frame = eval.frame_manager_mut().create_frame("query-pixel-differential", 180, 200, buffer);
        eval.frame_manager_mut().get_mut(frame).unwrap().window_system = Some(Value::symbol("neomacs"));
        let window = eval.frame_manager().get(frame).unwrap().selected_window;
        eval.eval_str("(setq mode-line-format nil header-line-format nil tab-line-format nil word-wrap t) (goto-char 55)").unwrap();
        eval.eval_str(decoration).unwrap();
        let mut query = LayoutEngine::new_without_font_metrics();
        let mut reused = 0;
        for pixels in (0..32).chain((0..32).rev()) {
            eval.eval_str(&format!("(set-window-vscroll nil {pixels} t t)")).unwrap();
            probe::reset();
            let actual = query.query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport).unwrap();
            reused += usize::from(probe::max_depth() == 0);
            let mut fresh = WindowLayoutQueryEngine::new_without_font_metrics();
            let expected = fresh.query_window_layout(&mut eval, frame, window, WindowLayoutQueryScope::Viewport).unwrap();
            assert_eq!(actual.end(), expected.end(), "{decoration} pixels={pixels}");
            assert_eq!(actual.geometry(), expected.geometry(), "{decoration} pixels={pixels}");
        }
        assert!(reused > 4, "no pixel placement reuse: {decoration}, reused={reused}");
    }
}
