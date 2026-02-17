//! Display engine builtins for the Elisp interpreter.
//!
//! Implements display-related functions from Emacs `xdisp.c`:
//! - `format-mode-line` — format a mode line string
//! - `invisible-p` — check if a position or property is invisible
//! - `line-pixel-height` — get line height in pixels
//! - `window-text-pixel-size` — calculate text pixel dimensions
//! - `pos-visible-in-window-p` — check if position is visible
//! - `move-point-visually` — move point in visual order
//! - `lookup-image-map` — lookup image map coordinates
//! - `current-bidi-paragraph-direction` — get bidi paragraph direction
//! - `move-to-window-line` — move to a specific window line
//! - `tool-bar-height` — get tool bar height
//! - `tab-bar-height` — get tab bar height
//! - `line-number-display-width` — get line number display width
//! - `long-line-optimizations-p` — check if long-line optimizations are enabled

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use crate::window::{FrameId, WindowId};

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_args_range(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_integer_or_marker(arg: &Value) -> Result<(), Flow> {
    match arg {
        Value::Int(_) | Value::Char(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

fn expect_fixnum_arg(name: &str, arg: &Value) -> Result<(), Flow> {
    match arg {
        Value::Int(_) | Value::Char(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol(name), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (format-mode-line &optional FORMAT FACE WINDOW BUFFER) -> string
///
/// Batch-compatible behavior: accepts 1..4 args and returns an empty string.
pub(crate) fn builtin_format_mode_line(args: Vec<Value>) -> EvalResult {
    expect_args_range("format-mode-line", &args, 1, 4)?;
    Ok(Value::string(""))
}

/// `(format-mode-line &optional FORMAT FACE WINDOW BUFFER)` evaluator-backed variant.
///
/// Batch mode still returns the empty string, but validates optional WINDOW and
/// BUFFER designators like Emacs.
pub(crate) fn builtin_format_mode_line_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args_range("format-mode-line", &args, 1, 4)?;
    validate_optional_window_designator(eval, args.get(2), "windowp")?;
    validate_optional_buffer_designator(eval, args.get(3))?;
    Ok(Value::string(""))
}

/// (invisible-p POS-OR-PROP) -> boolean
///
/// Batch semantics: symbols are considered invisible properties, while
/// numeric positions are not invisible by default.
pub(crate) fn builtin_invisible_p(args: Vec<Value>) -> EvalResult {
    expect_args("invisible-p", &args, 1)?;
    Ok(Value::bool(matches!(
        &args[0],
        Value::Symbol(_) | Value::True
    )))
}

/// (line-pixel-height) -> integer
///
/// Batch-compatible behavior returns 1.
pub(crate) fn builtin_line_pixel_height(args: Vec<Value>) -> EvalResult {
    expect_args("line-pixel-height", &args, 0)?;
    Ok(Value::Int(1))
}

/// (window-text-pixel-size &optional WINDOW FROM TO X-LIMIT Y-LIMIT MODE) -> (WIDTH . HEIGHT)
///
/// Batch-compatible behavior returns `(0 . 0)` and enforces argument
/// validation for WINDOW / FROM / TO.
pub(crate) fn builtin_window_text_pixel_size(args: Vec<Value>) -> EvalResult {
    expect_args_range("window-text-pixel-size", &args, 0, 7)?;

    if let Some(window) = args.first() {
        if !window.is_nil() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), window.clone()],
            ));
        }
    }
    if let Some(from) = args.get(1) {
        if !from.is_nil() {
            expect_integer_or_marker(from)?;
        }
    }
    if let Some(to) = args.get(2) {
        if !to.is_nil() {
            expect_integer_or_marker(to)?;
        }
    }

    Ok(Value::cons(Value::Int(0), Value::Int(0)))
}

/// `(window-text-pixel-size &optional WINDOW FROM TO X-LIMIT Y-LIMIT MODE)` evaluator-backed variant.
///
/// Batch mode returns `(0 . 0)` and validates optional WINDOW / FROM / TO
/// designators against evaluator state.
pub(crate) fn builtin_window_text_pixel_size_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args_range("window-text-pixel-size", &args, 0, 7)?;
    validate_optional_window_designator(eval, args.first(), "window-live-p")?;
    if let Some(from) = args.get(1) {
        if !from.is_nil() {
            expect_integer_or_marker(from)?;
        }
    }
    if let Some(to) = args.get(2) {
        if !to.is_nil() {
            expect_integer_or_marker(to)?;
        }
    }
    Ok(Value::cons(Value::Int(0), Value::Int(0)))
}

/// (pos-visible-in-window-p &optional POS WINDOW PARTIALLY) -> boolean
///
/// Batch-compatible behavior: no window visibility is reported, so this
/// returns nil.
pub(crate) fn builtin_pos_visible_in_window_p(args: Vec<Value>) -> EvalResult {
    expect_args_range("pos-visible-in-window-p", &args, 0, 3)?;
    Ok(Value::Nil)
}

/// `(pos-visible-in-window-p &optional POS WINDOW PARTIALLY)` evaluator-backed variant.
///
/// Batch mode reports no visibility (`nil`), but validates WINDOW designators.
pub(crate) fn builtin_pos_visible_in_window_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args_range("pos-visible-in-window-p", &args, 0, 3)?;
    validate_optional_window_designator(eval, args.get(1), "window-live-p")?;
    Ok(Value::Nil)
}

/// (move-point-visually DIRECTION) -> boolean
///
/// Batch semantics: direction is validated as a fixnum and reports whether
/// movement would reach the buffer edge in the stub-only VM context.
pub(crate) fn builtin_move_point_visually(args: Vec<Value>) -> EvalResult {
    expect_args("move-point-visually", &args, 1)?;
    match &args[0] {
        Value::Int(v) => {
            if *v > 0 {
                Ok(Value::symbol("end-of-buffer"))
            } else {
                Ok(Value::symbol("beginning-of-buffer"))
            }
        }
        Value::Char(_) => Ok(Value::symbol("end-of-buffer")),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), other.clone()],
        )),
    }
}

/// (lookup-image-map MAP X Y) -> symbol or nil
///
/// Lookup an image map at coordinates. Stub implementation
/// returns nil while preserving arity validation.
pub(crate) fn builtin_lookup_image_map(args: Vec<Value>) -> EvalResult {
    expect_args("lookup-image-map", &args, 3)?;
    if !args[0].is_nil() {
        expect_fixnum_arg("fixnump", &args[1])?;
        expect_fixnum_arg("fixnump", &args[2])?;
    }
    Ok(Value::Nil)
}

/// (current-bidi-paragraph-direction &optional BUFFER) -> symbol
///
/// Get the bidi paragraph direction. Returns the symbol 'left-to-right.
pub(crate) fn builtin_current_bidi_paragraph_direction(args: Vec<Value>) -> EvalResult {
    expect_args_range("current-bidi-paragraph-direction", &args, 0, 1)?;
    if let Some(bufferish) = args.first() {
        if !bufferish.is_nil() && !matches!(bufferish, Value::Buffer(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("bufferp"), bufferish.clone()],
            ));
        }
    }
    // Return 'left-to-right
    Ok(Value::symbol("left-to-right"))
}

/// (move-to-window-line ARG) -> integer or nil
///
/// Batch semantics: no related live window is available.
pub(crate) fn builtin_move_to_window_line(args: Vec<Value>) -> EvalResult {
    expect_args("move-to-window-line", &args, 1)?;
    Err(signal(
        "error",
        vec![Value::string(
            "move-to-window-line called from unrelated buffer",
        )],
    ))
}

/// (tool-bar-height &optional FRAME PIXELWISE) -> integer
///
/// Get the height of the tool bar. Returns 0 (no tool bar).
pub(crate) fn builtin_tool_bar_height(args: Vec<Value>) -> EvalResult {
    expect_args_range("tool-bar-height", &args, 0, 2)?;
    // Return 0 (no tool bar)
    Ok(Value::Int(0))
}

/// `(tool-bar-height &optional FRAME PIXELWISE)` evaluator-backed variant.
///
/// Accepts nil or a live frame designator for FRAME.
pub(crate) fn builtin_tool_bar_height_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args_range("tool-bar-height", &args, 0, 2)?;
    validate_optional_frame_designator(eval, args.first())?;
    Ok(Value::Int(0))
}

/// (tab-bar-height &optional FRAME PIXELWISE) -> integer
///
/// Get the height of the tab bar. Returns 0 (no tab bar).
pub(crate) fn builtin_tab_bar_height(args: Vec<Value>) -> EvalResult {
    expect_args_range("tab-bar-height", &args, 0, 2)?;
    // Return 0 (no tab bar)
    Ok(Value::Int(0))
}

/// `(tab-bar-height &optional FRAME PIXELWISE)` evaluator-backed variant.
///
/// Accepts nil or a live frame designator for FRAME.
pub(crate) fn builtin_tab_bar_height_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args_range("tab-bar-height", &args, 0, 2)?;
    validate_optional_frame_designator(eval, args.first())?;
    Ok(Value::Int(0))
}

/// (line-number-display-width &optional ON-DISPLAY) -> integer
///
/// Get the width of the line number display. Returns 0 (no line numbers).
pub(crate) fn builtin_line_number_display_width(args: Vec<Value>) -> EvalResult {
    expect_args_range("line-number-display-width", &args, 0, 1)?;
    // Return 0 (no line numbers)
    Ok(Value::Int(0))
}

/// (long-line-optimizations-p) -> boolean
///
/// Check if long-line optimizations are enabled. Returns nil.
pub(crate) fn builtin_long_line_optimizations_p(args: Vec<Value>) -> EvalResult {
    expect_args("long-line-optimizations-p", &args, 0)?;
    // Return nil (optimizations not enabled)
    Ok(Value::Nil)
}

fn validate_optional_frame_designator(
    eval: &super::eval::Evaluator,
    value: Option<&Value>,
) -> Result<(), Flow> {
    let Some(frameish) = value else {
        return Ok(());
    };
    if frameish.is_nil() {
        return Ok(());
    }
    if let Value::Int(id) = frameish {
        if *id >= 0 && eval.frames.get(FrameId(*id as u64)).is_some() {
            return Ok(());
        }
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("framep"), frameish.clone()],
    ))
}

fn validate_optional_window_designator(
    eval: &super::eval::Evaluator,
    value: Option<&Value>,
    predicate: &str,
) -> Result<(), Flow> {
    let Some(windowish) = value else {
        return Ok(());
    };
    if windowish.is_nil() {
        return Ok(());
    }
    if let Value::Int(id) = windowish {
        if *id >= 0 {
            let wid = WindowId(*id as u64);
            for fid in eval.frames.frame_list() {
                if let Some(frame) = eval.frames.get(fid) {
                    if frame.find_window(wid).is_some() {
                        return Ok(());
                    }
                }
            }
        }
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol(predicate), windowish.clone()],
    ))
}

fn validate_optional_buffer_designator(
    eval: &super::eval::Evaluator,
    value: Option<&Value>,
) -> Result<(), Flow> {
    let Some(bufferish) = value else {
        return Ok(());
    };
    if bufferish.is_nil() {
        return Ok(());
    }
    if let Value::Buffer(id) = bufferish {
        if eval.buffers.get(*id).is_some() {
            return Ok(());
        }
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("bufferp"), bufferish.clone()],
    ))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_mode_line() {
        let result =
            builtin_format_mode_line(vec![Value::string("test"), Value::symbol("default")])
                .unwrap();
        assert_eq!(result, Value::string(""));

        assert!(builtin_format_mode_line(vec![]).is_err());
    }

    #[test]
    fn test_format_mode_line_eval_optional_designators() {
        let mut eval = super::super::eval::Evaluator::new();
        let buffer_id = eval.buffers.current_buffer().expect("current buffer").id;
        let frame_id = eval.frames.create_frame("xdisp-format", 80, 24, buffer_id);
        let window_id = eval.frames.get(frame_id).expect("frame").selected_window.0 as i64;

        let ok = builtin_format_mode_line_eval(
            &mut eval,
            vec![
                Value::string("%b"),
                Value::Nil,
                Value::Int(window_id),
                Value::Buffer(buffer_id),
            ],
        )
        .unwrap();
        assert_eq!(ok, Value::string(""));

        let err = builtin_format_mode_line_eval(
            &mut eval,
            vec![Value::string("%b"), Value::Nil, Value::string("x")],
        )
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        let err = builtin_format_mode_line_eval(
            &mut eval,
            vec![
                Value::string("%b"),
                Value::Nil,
                Value::Nil,
                Value::string("x"),
            ],
        )
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }
    }

    #[test]
    fn test_invisible_p() {
        let result = builtin_invisible_p(vec![Value::Int(1)]).unwrap();
        assert!(result.is_nil());

        let result = builtin_invisible_p(vec![Value::symbol("invisible")]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn test_line_pixel_height() {
        let result = builtin_line_pixel_height(vec![]).unwrap();
        assert_eq!(result, Value::Int(1));
    }

    #[test]
    fn test_window_text_pixel_size() {
        let result = builtin_window_text_pixel_size(vec![]).unwrap();
        if let Value::Cons(cell) = result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car, Value::Int(0));
            assert_eq!(pair.cdr, Value::Int(0));
        } else {
            panic!("expected cons");
        }
    }

    #[test]
    fn test_window_text_pixel_size_arg_validation() {
        let err = builtin_window_text_pixel_size(vec![Value::Int(1)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        let err = builtin_window_text_pixel_size(vec![Value::Nil, Value::symbol("x")]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        let err = builtin_window_text_pixel_size(vec![Value::Nil, Value::Nil, Value::symbol("x")])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        // X-LIMIT / Y-LIMIT / MODE / PIXELWISE are accepted without strict type checks.
        assert!(builtin_window_text_pixel_size(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::symbol("x"),
            Value::symbol("y"),
            Value::symbol("z"),
            Value::symbol("m"),
        ])
        .is_ok());
    }

    #[test]
    fn test_window_text_pixel_size_eval_window_validation() {
        let mut eval = super::super::eval::Evaluator::new();
        let buf_id = eval.buffers.current_buffer().expect("current buffer").id;
        let frame_id = eval.frames.create_frame("xdisp-test", 80, 24, buf_id);
        let selected_window = eval.frames.get(frame_id).expect("frame").selected_window.0 as i64;

        let ok = builtin_window_text_pixel_size_eval(&mut eval, vec![Value::Int(selected_window)])
            .unwrap();
        match ok {
            Value::Cons(_) => {}
            other => panic!("expected cons return, got {other:?}"),
        }

        let err =
            builtin_window_text_pixel_size_eval(&mut eval, vec![Value::Int(999_999)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }
    }

    #[test]
    fn test_pos_visible_in_window_p() {
        let result = builtin_pos_visible_in_window_p(vec![]).unwrap();
        assert!(result.is_nil());

        let result =
            builtin_pos_visible_in_window_p(vec![Value::Int(100), Value::symbol("window")])
                .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn test_pos_visible_in_window_p_eval_window_validation() {
        let mut eval = super::super::eval::Evaluator::new();
        let err =
            builtin_pos_visible_in_window_p_eval(&mut eval, vec![Value::Nil, Value::string("x")])
                .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        let ok = builtin_pos_visible_in_window_p_eval(&mut eval, vec![Value::Int(1)]).unwrap();
        assert!(ok.is_nil());
    }

    #[test]
    fn test_move_point_visually() {
        let result = builtin_move_point_visually(vec![Value::Int(1)]).unwrap();
        assert_eq!(result, Value::symbol("end-of-buffer"));

        let result = builtin_move_point_visually(vec![Value::Int(0)]).unwrap();
        assert_eq!(result, Value::symbol("beginning-of-buffer"));

        let result = builtin_move_point_visually(vec![Value::Int(-1)]).unwrap();
        assert_eq!(result, Value::symbol("beginning-of-buffer"));

        let result = builtin_move_point_visually(vec![Value::Char('a')]).unwrap();
        assert_eq!(result, Value::symbol("end-of-buffer"));

        let err = builtin_move_point_visually(vec![Value::symbol("left")]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }
    }

    #[test]
    fn test_lookup_image_map() {
        let result =
            builtin_lookup_image_map(vec![Value::symbol("map"), Value::Int(10), Value::Int(20)])
                .unwrap();
        assert!(result.is_nil());

        let err = builtin_lookup_image_map(vec![
            Value::symbol("image"),
            Value::string("x"),
            Value::symbol("y"),
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        let err = builtin_lookup_image_map(vec![
            Value::symbol("image"),
            Value::Int(1),
            Value::symbol("y"),
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }

        let result =
            builtin_lookup_image_map(vec![Value::Nil, Value::Int(1), Value::string("y")]).unwrap();
        assert!(result.is_nil());

        let err = builtin_lookup_image_map(vec![]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments, got {:?}", other),
        }
    }

    #[test]
    fn test_current_bidi_paragraph_direction() {
        let result = builtin_current_bidi_paragraph_direction(vec![]).unwrap();
        assert_eq!(result, Value::symbol("left-to-right"));

        let result = builtin_current_bidi_paragraph_direction(vec![Value::Buffer(
            crate::buffer::BufferId(1),
        )])
        .unwrap();
        assert_eq!(result, Value::symbol("left-to-right"));

        let err =
            builtin_current_bidi_paragraph_direction(vec![Value::symbol("buffer")]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }
    }

    #[test]
    fn test_move_to_window_line() {
        let err = builtin_move_to_window_line(vec![Value::Int(0)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {:?}", other),
        }

        let err = builtin_move_to_window_line(vec![Value::Int(5)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {:?}", other),
        }
    }

    #[test]
    fn test_tool_bar_height() {
        let result = builtin_tool_bar_height(vec![]).unwrap();
        assert_eq!(result, Value::Int(0));

        let result = builtin_tool_bar_height(vec![Value::symbol("frame")]).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn test_tool_bar_height_eval_frame_validation() {
        let mut eval = super::super::eval::Evaluator::new();
        let buf_id = eval.buffers.current_buffer().expect("current buffer").id;
        let frame_id = eval.frames.create_frame("xdisp-test", 80, 24, buf_id);

        let result =
            builtin_tool_bar_height_eval(&mut eval, vec![Value::Int(frame_id.0 as i64)]).unwrap();
        assert_eq!(result, Value::Int(0));

        let err = builtin_tool_bar_height_eval(&mut eval, vec![Value::string("x")]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }
    }

    #[test]
    fn test_tab_bar_height() {
        let result = builtin_tab_bar_height(vec![]).unwrap();
        assert_eq!(result, Value::Int(0));

        let result = builtin_tab_bar_height(vec![Value::symbol("frame")]).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn test_tab_bar_height_eval_frame_validation() {
        let mut eval = super::super::eval::Evaluator::new();
        let buf_id = eval.buffers.current_buffer().expect("current buffer").id;
        let frame_id = eval.frames.create_frame("xdisp-test", 80, 24, buf_id);

        let result =
            builtin_tab_bar_height_eval(&mut eval, vec![Value::Int(frame_id.0 as i64)]).unwrap();
        assert_eq!(result, Value::Int(0));

        let err = builtin_tab_bar_height_eval(&mut eval, vec![Value::string("x")]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {:?}", other),
        }
    }

    #[test]
    fn test_line_number_display_width() {
        let result = builtin_line_number_display_width(vec![]).unwrap();
        assert_eq!(result, Value::Int(0));

        let result = builtin_line_number_display_width(vec![Value::True]).unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn test_long_line_optimizations_p() {
        let result = builtin_long_line_optimizations_p(vec![]).unwrap();
        assert!(result.is_nil());
    }

    // Test wrong arity errors
    #[test]
    fn test_wrong_arity() {
        assert!(builtin_line_pixel_height(vec![Value::Int(1)]).is_err());
        assert!(builtin_invisible_p(vec![]).is_err());
        assert!(builtin_move_point_visually(vec![]).is_err());
        assert!(builtin_lookup_image_map(vec![Value::Int(1), Value::Int(2)]).is_err());
        assert!(builtin_move_to_window_line(vec![]).is_err());
    }

    // Test optional args
    #[test]
    fn test_optional_args() {
        // format-mode-line allows 1-4 args
        assert!(builtin_format_mode_line(vec![]).is_err());
        assert!(builtin_format_mode_line(vec![Value::string("fmt")]).is_ok());
        assert!(builtin_format_mode_line(vec![
            Value::string("fmt"),
            Value::symbol("face"),
            Value::symbol("window"),
            Value::symbol("buffer"),
        ])
        .is_ok());
        assert!(builtin_format_mode_line(vec![
            Value::string("fmt"),
            Value::symbol("face"),
            Value::symbol("window"),
            Value::symbol("buffer"),
            Value::symbol("extra"),
        ])
        .is_err());

        // window-text-pixel-size allows 0-7 args
        assert!(builtin_window_text_pixel_size(vec![]).is_ok());
        assert!(builtin_window_text_pixel_size(vec![
            Value::Nil,
            Value::Int(1),
            Value::Int(100),
            Value::Int(500),
            Value::Int(300),
            Value::symbol("mode"),
            Value::symbol("pixelwise"),
        ])
        .is_ok());
        assert!(builtin_window_text_pixel_size(vec![Value::Int(1); 8]).is_err());
    }
}
