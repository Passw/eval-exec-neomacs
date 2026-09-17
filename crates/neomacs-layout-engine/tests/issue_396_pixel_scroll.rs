//! Diagnostic: repeated precise scroll input must survive actual redisplay.
use neomacs_layout_engine::LayoutEngine;
use neovm_core::emacs_core::Context;
use neovm_core::window::{FrameId, Window, WindowId};

fn viewport(eval: &Context, frame: FrameId, window: WindowId) -> (i64, i32) {
    match eval
        .frame_manager()
        .get(frame)
        .unwrap()
        .find_window(window)
        .unwrap()
    {
        Window::Leaf {
            window_start,
            vscroll,
            ..
        } => (window_start.as_i64(), -*vscroll),
        _ => panic!("expected leaf"),
    }
}

#[test]
fn repeated_pixel_scroll_survives_redisplay_without_jitter() {
    let mut eval = Context::new();
    eval.eval_str(
        r#"
        (progn
          (let ((i 0))
            (while (< i 400)
              (insert (format "Line %03d scrolling diagnostic\n" i))
              (setq i (1+ i))))
          (goto-char (point-min)))
    "#,
    )
    .unwrap();
    let buffer = eval.buffer_manager().current_buffer().unwrap().id();
    let frame = eval
        .frame_manager_mut()
        .create_frame("issue-396", 800, 600, buffer);
    let window = eval.frame_manager().get(frame).unwrap().selected_window;
    let mut engine = LayoutEngine::new();
    let _ = engine.redisplay_frame_attempt(&mut eval, frame);
    let mut previous = viewport(&eval, frame, window);
    for step in 0..40 {
        assert_eq!(engine.pixel_scroll_window(&mut eval, window, 3), Some(()));
        let applied = viewport(&eval, frame, window);
        let _ = engine.redisplay_frame_attempt(&mut eval, frame);
        let presented = viewport(&eval, frame, window);
        eprintln!("step={step} before={previous:?} applied={applied:?} presented={presented:?}");
        assert!(
            presented > previous,
            "downward gesture must advance, not reset or jitter: step={step}, before={previous:?}, applied={applied:?}, presented={presented:?}"
        );
        previous = presented;
    }
}
