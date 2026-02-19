//! Window, frame, and display-related builtins for the Elisp VM.
//!
//! Bridges the `FrameManager` (in `crate::window`) to Elisp by exposing
//! builtins such as `selected-window`, `split-window`, `selected-frame`, etc.
//! Windows and frames are represented as integer IDs in Lisp.

use super::error::{signal, EvalResult, Flow};
use super::value::Value;
use crate::buffer::BufferId;
use crate::window::{FrameId, FrameManager, SplitDirection, Window, WindowId};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Expect exactly N arguments.
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

/// Expect at least N arguments.
fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

/// Expect at most N arguments.
fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

/// Extract an integer from a Value.
fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

/// Resolve an optional window designator.
///
/// - nil/omitted => selected window of selected frame
/// - non-nil invalid designator => `(wrong-type-argument PRED VALUE)`
fn resolve_window_id_with_pred(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
    pred: &str,
) -> Result<(FrameId, WindowId), Flow> {
    match arg {
        None | Some(Value::Nil) => {
            let frame_id = ensure_selected_frame_id(eval);
            let frame = eval
                .frames
                .get(frame_id)
                .ok_or_else(|| signal("error", vec![Value::string("No selected frame")]))?;
            Ok((frame_id, frame.selected_window))
        }
        Some(val) => {
            let wid = match val {
                Value::Int(n) => WindowId(*n as u64),
                _ => {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol(pred), val.clone()],
                    ))
                }
            };
            if let Some(frame_id) = eval.frames.find_window_frame_id(wid) {
                Ok((frame_id, wid))
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(pred), val.clone()],
                ))
            }
        }
    }
}

fn resolve_window_id(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
) -> Result<(FrameId, WindowId), Flow> {
    resolve_window_id_with_pred(eval, arg, "window-live-p")
}

/// Resolve a window designator for mutation-style window ops.
///
/// GNU Emacs uses generic `error` signaling for invalid designators in some
/// split/delete window builtins, rather than `wrong-type-argument`.
fn resolve_window_id_or_error(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
) -> Result<(FrameId, WindowId), Flow> {
    match arg {
        None | Some(Value::Nil) => resolve_window_id(eval, arg),
        Some(Value::Int(n)) => {
            let wid = WindowId(*n as u64);
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                Ok((fid, wid))
            } else {
                Err(signal("error", vec![Value::string("Invalid window")]))
            }
        }
        Some(_) => Err(signal("error", vec![Value::string("Invalid window")])),
    }
}

/// Resolve a frame designator, signaling predicate-shaped type errors.
///
/// When ARG is nil/omitted, GNU Emacs resolves against the selected frame.
/// In batch compatibility mode we bootstrap that frame on demand.
fn resolve_frame_id(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
    predicate: &str,
) -> Result<FrameId, Flow> {
    match arg {
        None | Some(Value::Nil) => Ok(ensure_selected_frame_id(eval)),
        Some(Value::Int(n)) => {
            let fid = FrameId(*n as u64);
            if eval.frames.get(fid).is_some() {
                Ok(fid)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(predicate), Value::Int(*n)],
                ))
            }
        }
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol(predicate), other.clone()],
        )),
    }
}

/// Helper: get a reference to a leaf window by id.
fn get_leaf<'a>(frames: &'a FrameManager, fid: FrameId, wid: WindowId) -> Result<&'a Window, Flow> {
    let frame = frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    frame
        .find_window(wid)
        .ok_or_else(|| signal("error", vec![Value::string("Window not found")]))
}

/// Ensure a selected frame exists and return its id.
///
/// In batch compatibility mode, GNU Emacs still has an initial frame (`F1`).
/// When the evaluator has no frame yet, synthesize one on demand.
pub(crate) fn ensure_selected_frame_id(eval: &mut super::eval::Evaluator) -> FrameId {
    if let Some(fid) = eval.frames.selected_frame().map(|f| f.id) {
        return fid;
    }

    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .unwrap_or_else(|| eval.buffers.create_buffer("*scratch*"));
    // Batch GNU Emacs startup exposes an initial ~80x24 text window plus
    // a minibuffer line; frame parameters report 80x25.
    // With our default 8x16 char metrics the text area corresponds to 640x384.
    let fid = eval.frames.create_frame("F1", 640, 384, buf_id);
    let minibuffer_buf_id = eval
        .buffers
        .find_buffer_by_name(" *Minibuf-0*")
        .unwrap_or_else(|| eval.buffers.create_buffer(" *Minibuf-0*"));
    if let Some(frame) = eval.frames.get_mut(fid) {
        frame.parameters.insert("width".to_string(), Value::Int(80));
        frame
            .parameters
            .insert("height".to_string(), Value::Int(25));
        if let Some(Window::Leaf {
            window_start,
            point,
            ..
        }) = frame.find_window_mut(frame.selected_window)
        {
            // Batch-mode startup in GNU Emacs reports point/window-start as 1.
            *window_start = 1;
            *point = 1;
        }
        if let Some(minibuffer_leaf) = frame.minibuffer_leaf.as_mut() {
            // Keep minibuffer window accessors aligned with GNU Emacs batch startup.
            minibuffer_leaf.set_buffer(minibuffer_buf_id);
        }
    }
    fid
}

/// Compute the height of a window in lines.
fn window_height_lines(w: &Window, char_height: f32) -> i64 {
    let h = w.bounds().height;
    if char_height > 0.0 {
        (h / char_height) as i64
    } else {
        0
    }
}

/// Compute the width of a window in columns.
fn window_width_cols(w: &Window, char_width: f32) -> i64 {
    let cw = w.bounds().width;
    if char_width > 0.0 {
        (cw / char_width) as i64
    } else {
        0
    }
}

// ===========================================================================
// Window queries
// ===========================================================================

/// `(selected-window)` -> window id (int).
pub(crate) fn builtin_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("selected-window", &args, 0)?;
    let fid = ensure_selected_frame_id(eval);
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("No selected frame")]))?;
    Ok(Value::Int(frame.selected_window.0 as i64))
}

/// `(frame-selected-window &optional FRAME)` -> selected window of FRAME.
pub(crate) fn builtin_frame_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-selected-window", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame.selected_window.0 as i64))
}

/// `(window-frame &optional WINDOW)` -> frame of WINDOW.
pub(crate) fn builtin_window_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-frame", &args, 1)?;
    let (fid, _wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    Ok(Value::Int(fid.0 as i64))
}

/// `(window-buffer &optional WINDOW)` -> buffer object.
pub(crate) fn builtin_window_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-buffer", &args, 1)?;
    let resolve_buffer = |frames: &FrameManager, fid: FrameId, wid: WindowId| -> EvalResult {
        let w = get_leaf(frames, fid, wid)?;
        match w.buffer_id() {
            Some(bid) => Ok(Value::Buffer(bid)),
            None => Ok(Value::Nil),
        }
    };

    match args.first() {
        None | Some(Value::Nil) => {
            let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "windowp")?;
            resolve_buffer(&eval.frames, fid, wid)
        }
        Some(Value::Int(n)) => {
            let wid = WindowId(*n as u64);
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                return resolve_buffer(&eval.frames, fid, wid);
            }
            if eval.frames.is_window_object_id(wid) {
                return Ok(Value::Nil);
            }
            Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("windowp"), Value::Int(*n)],
            ))
        }
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("windowp"), other.clone()],
        )),
    }
}

/// `(window-start &optional WINDOW)` -> integer position.
pub(crate) fn builtin_window_start(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-start", &args, 1)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf { window_start, .. } => Ok(Value::Int(*window_start as i64)),
        _ => Ok(Value::Int(0)),
    }
}

/// `(window-end &optional WINDOW UPDATE)` -> integer position.
///
/// We approximate window-end as window-start since we don't have real
/// display layout.  The UPDATE argument is ignored.
pub(crate) fn builtin_window_end(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-end", &args, 2)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf {
            window_start,
            bounds,
            buffer_id,
            ..
        } => {
            // Clamp the display estimate to the buffer's end position so empty
            // buffers report their 1-based start/end as GNU Emacs does.
            let frame = eval.frames.get(fid).unwrap();
            let lines = (bounds.height / frame.char_height) as usize;
            let cols = (bounds.width / frame.char_width) as usize;
            let estimated_end = window_start.saturating_add(lines.saturating_mul(cols));
            let buffer_end = eval
                .buffers
                .get(*buffer_id)
                .map(|buf| buf.text.char_count().saturating_add(1))
                .unwrap_or(*window_start);
            let clamped_end = estimated_end.min(buffer_end.max(*window_start));
            Ok(Value::Int(clamped_end as i64))
        }
        _ => Ok(Value::Int(0)),
    }
}

/// `(window-point &optional WINDOW)` -> integer position.
pub(crate) fn builtin_window_point(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-point", &args, 1)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf { point, .. } => Ok(Value::Int(*point as i64)),
        _ => Ok(Value::Int(0)),
    }
}

/// `(set-window-start WINDOW POS &optional NOFORCE)` -> POS.
pub(crate) fn builtin_set_window_start(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-start", &args, 2)?;
    expect_max_args("set-window-start", &args, 3)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let pos = expect_int(&args[1])? as usize;
    if let Some(frame) = eval.frames.get_mut(fid) {
        if frame.minibuffer_window == Some(wid) {
            if let Some(Window::Leaf { window_start, .. }) = frame.find_window(wid) {
                return Ok(Value::Int(*window_start as i64));
            }
            return Ok(Value::Int(1));
        }
    }
    if let Some(w) = eval.frames.get_mut(fid).and_then(|f| f.find_window_mut(wid)) {
        if let Window::Leaf { window_start, .. } = w {
            *window_start = pos;
        }
    }
    Ok(Value::Int(pos as i64))
}

/// `(set-window-point WINDOW POS)` -> POS.
pub(crate) fn builtin_set_window_point(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-point", &args, 2)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let pos = expect_int(&args[1])? as usize;
    if let Some(frame) = eval.frames.get_mut(fid) {
        if frame.minibuffer_window == Some(wid) {
            if let Some(Window::Leaf { point, .. }) = frame.find_window(wid) {
                return Ok(Value::Int(*point as i64));
            }
            return Ok(Value::Int(1));
        }
    }
    if let Some(w) = eval.frames.get_mut(fid).and_then(|f| f.find_window_mut(wid)) {
        if let Window::Leaf { point, .. } = w {
            *point = pos;
        }
    }
    Ok(Value::Int(pos as i64))
}

/// `(window-height &optional WINDOW)` -> integer (lines).
pub(crate) fn builtin_window_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-height", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let ch = eval.frames.get(fid).map(|f| f.char_height).unwrap_or(16.0);
    Ok(Value::Int(window_height_lines(w, ch)))
}

/// `(window-width &optional WINDOW)` -> integer (columns).
pub(crate) fn builtin_window_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-width", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let cw = eval.frames.get(fid).map(|f| f.char_width).unwrap_or(8.0);
    Ok(Value::Int(window_width_cols(w, cw)))
}

/// `(window-body-height &optional WINDOW PIXELWISE)` -> integer.
pub(crate) fn builtin_window_body_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-body-height", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let _pixelwise = args.get(1);
    // Batch GNU Emacs returns character-height values even when PIXELWISE is non-nil.
    // The body area excludes one mode-line row in the default window.
    let ch = eval.frames.get(fid).map(|f| f.char_height).unwrap_or(16.0);
    Ok(Value::Int(window_height_lines(w, ch).saturating_sub(1)))
}

/// `(window-body-width &optional WINDOW PIXELWISE)` -> integer.
pub(crate) fn builtin_window_body_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-body-width", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let _pixelwise = args.get(1);
    // Batch GNU Emacs returns character-width values even when PIXELWISE is non-nil.
    let cw = eval.frames.get(fid).map(|f| f.char_width).unwrap_or(8.0);
    Ok(Value::Int(window_width_cols(w, cw)))
}

/// `(window-total-height &optional WINDOW ROUND)` -> integer.
///
pub(crate) fn builtin_window_total_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-total-height", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let ch = eval.frames.get(fid).map(|f| f.char_height).unwrap_or(16.0);
    Ok(Value::Int(window_height_lines(w, ch)))
}

/// `(window-total-width &optional WINDOW ROUND)` -> integer.
///
pub(crate) fn builtin_window_total_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-total-width", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let cw = eval.frames.get(fid).map(|f| f.char_width).unwrap_or(8.0);
    Ok(Value::Int(window_width_cols(w, cw)))
}

/// `(window-list &optional FRAME MINIBUF ALL-FRAMES)` -> list of window ids.
pub(crate) fn builtin_window_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-list", &args, 3)?;
    let selected_fid = ensure_selected_frame_id(eval);
    // GNU Emacs validates ALL-FRAMES before FRAME mismatch checks.
    let all_frames_fid = match args.get(2) {
        None | Some(Value::Nil) => None,
        Some(Value::Int(n)) => {
            let wid = WindowId(*n as u64);
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                Some(fid)
            } else if eval.frames.is_window_object_id(wid) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("window-live-p"), Value::Int(*n)],
                ));
            } else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("windowp"), Value::Int(*n)],
                ));
            }
        }
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("windowp"), other.clone()],
            ))
        }
    };
    let mut fid = match args.first() {
        None | Some(Value::Nil) => selected_fid,
        Some(Value::Int(n)) => {
            let fid = FrameId(*n as u64);
            if eval.frames.get(fid).is_some() {
                fid
            } else {
                return Err(signal(
                    "error",
                    vec![Value::string("Window is on a different frame")],
                ));
            }
        }
        Some(_) => {
            return Err(signal(
                "error",
                vec![Value::string("Window is on a different frame")],
            ))
        }
    };
    if let Some(all_frames_fid) = all_frames_fid {
        fid = all_frames_fid;
    }
    let include_minibuffer = matches!(args.get(1), Some(Value::True));
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let mut ids: Vec<Value> = frame
        .window_list()
        .into_iter()
        .map(|wid| Value::Int(wid.0 as i64))
        .collect();
    if include_minibuffer {
        if let Some(minibuffer_wid) = frame.minibuffer_window {
            ids.push(Value::Int(minibuffer_wid.0 as i64));
        }
    }
    Ok(Value::list(ids))
}

/// `(get-buffer-window &optional BUFFER-OR-NAME ALL-FRAMES)` -> window or nil.
///
/// Batch-compatible behavior: search the selected frame for a window showing
/// the requested buffer.
pub(crate) fn builtin_get_buffer_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("get-buffer-window", &args, 2)?;
    let target = match args.first() {
        None | Some(Value::Nil) => return Ok(Value::Nil),
        Some(Value::Str(name)) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => return Ok(Value::Nil),
        },
        Some(Value::Buffer(id)) => {
            if eval.buffers.get(*id).is_none() {
                return Ok(Value::Nil);
            }
            *id
        }
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    let fid = ensure_selected_frame_id(eval);
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    for wid in frame.window_list() {
        let matches = frame
            .find_window(wid)
            .and_then(|w| w.buffer_id())
            .is_some_and(|bid| bid == target);
        if matches {
            return Ok(Value::Int(wid.0 as i64));
        }
    }

    Ok(Value::Nil)
}

/// `(get-buffer-window-list &optional BUFFER-OR-NAME MINIBUF ALL-FRAMES)`
/// -> list of windows displaying BUFFER-OR-NAME.
///
/// Batch-compatible behavior: collects matching windows from the selected
/// frame and ignores MINIBUF/ALL-FRAMES.
pub(crate) fn builtin_get_buffer_window_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("get-buffer-window-list", &args, 3)?;
    let target = match args.first() {
        None | Some(Value::Nil) => return Ok(Value::Nil),
        Some(Value::Str(name)) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => {
                return Err(signal(
                    "error",
                    vec![Value::string(format!("No such live buffer {name}"))],
                ))
            }
        },
        Some(Value::Buffer(id)) => {
            if eval.buffers.get(*id).is_none() {
                return Err(signal(
                    "error",
                    vec![Value::string("No such live buffer #<killed buffer>")],
                ));
            }
            *id
        }
        Some(other) => {
            return Err(signal(
                "error",
                vec![Value::string(format!("No such buffer {}", other))],
            ))
        }
    };
    let fid = ensure_selected_frame_id(eval);
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    let mut windows = Vec::new();
    for wid in frame.window_list() {
        let matches = frame
            .find_window(wid)
            .and_then(|w| w.buffer_id())
            .is_some_and(|bid| bid == target);
        if matches {
            windows.push(Value::Int(wid.0 as i64));
        }
    }

    if windows.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::list(windows))
    }
}

/// `(fit-window-to-buffer &optional WINDOW MAX-HEIGHT MIN-HEIGHT MAX-WIDTH PRESERVE-SIZE)`
/// -> nil in current batch compatibility mode.
///
/// Batch-compatible no-op: validates WINDOW and returns nil.
/// GNU Emacs signals generic `error` for invalid/deleted designators here.
pub(crate) fn builtin_fit_window_to_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("fit-window-to-buffer", &args, 6)?;
    let wid = match args.first() {
        None | Some(Value::Nil) => {
            let (_, wid) = resolve_window_id(eval, None)?;
            wid
        }
        Some(Value::Int(n)) => {
            let wid = WindowId(*n as u64);
            if eval.frames.find_window_frame_id(wid).is_some() {
                wid
            } else {
                return Err(signal("error", vec![Value::string("Invalid window")]));
            }
        }
        Some(_) => return Err(signal("error", vec![Value::string("Invalid window")])),
    };
    let _ = wid;
    Ok(Value::Nil)
}

/// `(window-dedicated-p &optional WINDOW)` -> t or nil.
pub(crate) fn builtin_window_dedicated_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-dedicated-p", &args, 1)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf { dedicated, .. } => Ok(Value::bool(*dedicated)),
        _ => Ok(Value::Nil),
    }
}

/// `(set-window-dedicated-p WINDOW FLAG)` -> FLAG.
pub(crate) fn builtin_set_window_dedicated_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-dedicated-p", &args, 2)?;
    let flag = args[1].is_truthy();
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    if let Some(w) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(wid))
    {
        if let Window::Leaf { dedicated, .. } = w {
            *dedicated = flag;
        }
    }
    Ok(Value::bool(flag))
}

/// `(windowp OBJ)` -> t if OBJ is a window id (integer) that exists.
pub(crate) fn builtin_windowp(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("windowp", &args, 1)?;
    let id = match args[0].as_int() {
        Some(n) => n as u64,
        None => return Ok(Value::Nil),
    };
    let found = eval.frames.is_window_object_id(WindowId(id));
    Ok(Value::bool(found))
}

/// `(window-live-p OBJ)` -> t if OBJ is a live leaf window id.
pub(crate) fn builtin_window_live_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("window-live-p", &args, 1)?;
    let id = match args[0].as_int() {
        Some(n) => n as u64,
        None => return Ok(Value::Nil),
    };
    let live = eval.frames.is_live_window_id(WindowId(id));
    Ok(Value::bool(live))
}

// ===========================================================================
// Window manipulation
// ===========================================================================

/// `(split-window &optional WINDOW SIZE SIDE)` -> new window id.
///
/// SIDE: nil or `below` = vertical split, `right` = horizontal split.
pub(crate) fn builtin_split_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("split-window", &args, 4)?;
    let (fid, wid) = resolve_window_id_or_error(eval, args.first())?;

    // Determine split direction from SIDE argument.
    let direction = match args.get(2) {
        Some(Value::Symbol(s)) if s == "right" || s == "left" => SplitDirection::Horizontal,
        _ => SplitDirection::Vertical,
    };

    // Use the same buffer as the window being split.
    let buf_id = {
        let w = get_leaf(&eval.frames, fid, wid)?;
        w.buffer_id().unwrap_or(BufferId(0))
    };

    let new_wid = eval
        .frames
        .split_window(fid, wid, direction, buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Cannot split window")]))?;
    Ok(Value::Int(new_wid.0 as i64))
}

/// `(delete-window &optional WINDOW)` -> nil.
pub(crate) fn builtin_delete_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("delete-window", &args, 1)?;
    let (fid, wid) = resolve_window_id_or_error(eval, args.first())?;
    if !eval.frames.delete_window(fid, wid) {
        return Err(signal(
            "error",
            vec![Value::string("Cannot delete sole window")],
        ));
    }
    let selected_buffer = eval
        .frames
        .get(fid)
        .and_then(|frame| frame.find_window(frame.selected_window))
        .and_then(|w| w.buffer_id());
    if let Some(buffer_id) = selected_buffer {
        eval.buffers.set_current(buffer_id);
    }
    Ok(Value::Nil)
}

/// `(delete-other-windows &optional WINDOW)` -> nil.
///
/// Deletes all windows in the frame except WINDOW (or selected window).
pub(crate) fn builtin_delete_other_windows(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("delete-other-windows", &args, 2)?;
    let (fid, keep_wid) = resolve_window_id_or_error(eval, args.first())?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    let all_ids: Vec<WindowId> = frame.window_list();
    let to_delete: Vec<WindowId> = all_ids.into_iter().filter(|&w| w != keep_wid).collect();

    for wid in to_delete {
        let _ = eval.frames.delete_window(fid, wid);
    }
    // Select the kept window.
    let selected_buffer = if let Some(f) = eval.frames.get_mut(fid) {
        f.select_window(keep_wid);
        f.find_window(keep_wid).and_then(|w| w.buffer_id())
    } else {
        None
    };
    if let Some(buffer_id) = selected_buffer {
        eval.buffers.set_current(buffer_id);
    }
    Ok(Value::Nil)
}

/// `(select-window WINDOW &optional NORECORD)` -> WINDOW.
pub(crate) fn builtin_select_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("select-window", &args, 1)?;
    expect_max_args("select-window", &args, 2)?;
    let fid = ensure_selected_frame_id(eval);
    let wid = match args.first() {
        Some(Value::Int(n)) => WindowId(*n as u64),
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), other.clone()],
            ))
        }
        None => unreachable!("expect_min_args enforced"),
    };
    let selected_buffer = {
        let frame = eval
            .frames
            .get_mut(fid)
            .ok_or_else(|| signal("error", vec![Value::string("No selected frame")]))?;
        if !frame.select_window(wid) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), args[0].clone()],
            ));
        }
        frame.find_window(wid).and_then(|w| w.buffer_id())
    };
    if let Some(buffer_id) = selected_buffer {
        eval.buffers.set_current(buffer_id);
    }
    Ok(Value::Int(wid.0 as i64))
}

/// `(other-window COUNT &optional ALL-FRAMES)` -> nil.
///
/// Select another window in cyclic order.
pub(crate) fn builtin_other_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("other-window", &args, 3)?;
    let count = if args.is_empty() || args[0].is_nil() {
        1
    } else {
        expect_int(&args[0])?
    };
    let Some(fid) = eval.frames.selected_frame().map(|f| f.id) else {
        return Ok(Value::Nil);
    };
    let Some(frame) = eval.frames.get(fid) else {
        return Ok(Value::Nil);
    };
    let list = frame.window_list();
    if list.is_empty() {
        return Ok(Value::Nil);
    }
    let cur = frame.selected_window;
    let cur_idx = list.iter().position(|w| *w == cur).unwrap_or(0);
    let len = list.len() as i64;
    let new_idx = ((cur_idx as i64 + count) % len + len) % len;
    let new_wid = list[new_idx as usize];
    let selected_buffer = if let Some(frame) = eval.frames.get_mut(fid) {
        frame.select_window(new_wid);
        frame.find_window(new_wid).and_then(|w| w.buffer_id())
    } else {
        None
    };
    if let Some(buffer_id) = selected_buffer {
        eval.buffers.set_current(buffer_id);
    }
    Ok(Value::Nil)
}

/// `(next-window &optional WINDOW MINIBUF ALL-FRAMES)` -> window id.
pub(crate) fn builtin_next_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("next-window", &args, 3)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let list = frame.window_list();
    if list.is_empty() {
        return Ok(Value::Nil);
    }
    let idx = list.iter().position(|w| *w == wid).unwrap_or(0);
    let next = (idx + 1) % list.len();
    Ok(Value::Int(list[next].0 as i64))
}

/// `(previous-window &optional WINDOW MINIBUF ALL-FRAMES)` -> window id.
pub(crate) fn builtin_previous_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("previous-window", &args, 3)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let list = frame.window_list();
    if list.is_empty() {
        return Ok(Value::Nil);
    }
    let idx = list.iter().position(|w| *w == wid).unwrap_or(0);
    let prev = if idx == 0 { list.len() - 1 } else { idx - 1 };
    Ok(Value::Int(list[prev].0 as i64))
}

/// `(set-window-buffer WINDOW BUFFER-OR-NAME &optional KEEP-MARGINS)` -> nil.
pub(crate) fn builtin_set_window_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-buffer", &args, 2)?;
    expect_max_args("set-window-buffer", &args, 3)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let buf_id = match &args[1] {
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_none() {
                return Err(signal(
                    "error",
                    vec![Value::string("Attempt to display deleted buffer")],
                ));
            }
            *id
        }
        Value::Str(name) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("bufferp"), Value::Nil],
                ))
            }
        },
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };

    if let Some(w) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(wid))
    {
        w.set_buffer(buf_id);
    }
    Ok(Value::Nil)
}

/// `(switch-to-buffer BUFFER-OR-NAME &optional NORECORD FORCE-SAME-WINDOW)` -> buffer.
pub(crate) fn builtin_switch_to_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("switch-to-buffer", &args, 1)?;
    expect_max_args("switch-to-buffer", &args, 3)?;
    let buf_id = match &args[0] {
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_none() {
                return Err(signal(
                    "error",
                    vec![Value::string("Attempt to display deleted buffer")],
                ));
            }
            *id
        }
        Value::Str(name) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => eval.buffers.create_buffer(name),
        },
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };

    // Set the selected window's buffer.
    let fid = ensure_selected_frame_id(eval);
    let sel_wid = eval
        .frames
        .get(fid)
        .map(|f| f.selected_window)
        .ok_or_else(|| signal("error", vec![Value::string("No selected window")]))?;
    if let Some(w) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(sel_wid))
    {
        w.set_buffer(buf_id);
    }
    // Also switch the buffer manager's current buffer.
    eval.buffers.set_current(buf_id);
    Ok(Value::Buffer(buf_id))
}

/// `(display-buffer BUFFER-OR-NAME &optional ACTION FRAME)` -> window id or nil.
///
/// Simplified: displays the buffer in the selected window.
pub(crate) fn builtin_display_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("display-buffer", &args, 1)?;
    expect_max_args("display-buffer", &args, 3)?;
    let buf_id = match &args[0] {
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_none() {
                return Err(signal("error", vec![Value::string("Invalid buffer")]));
            }
            *id
        }
        Value::Str(name) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => return Err(signal("error", vec![Value::string("Invalid buffer")])),
        },
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };

    let fid = ensure_selected_frame_id(eval);
    let sel_wid = eval
        .frames
        .get(fid)
        .map(|f| f.selected_window)
        .ok_or_else(|| signal("error", vec![Value::string("No selected window")]))?;
    if let Some(w) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(sel_wid))
    {
        w.set_buffer(buf_id);
    }
    Ok(Value::Int(sel_wid.0 as i64))
}

/// `(pop-to-buffer BUFFER-OR-NAME &optional ACTION NORECORD)` -> buffer.
///
/// Batch compatibility follows Emacs' noninteractive behavior: switch current
/// buffer and return the buffer object.
pub(crate) fn builtin_pop_to_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("pop-to-buffer", &args, 1)?;
    expect_max_args("pop-to-buffer", &args, 3)?;
    let buf_id = match &args[0] {
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_none() {
                return Err(signal("error", vec![Value::string("Invalid buffer")]));
            }
            *id
        }
        Value::Str(name) => match eval.buffers.find_buffer_by_name(name) {
            Some(id) => id,
            None => eval.buffers.create_buffer(name),
        },
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };

    let fid = ensure_selected_frame_id(eval);
    let sel_wid = eval
        .frames
        .get(fid)
        .map(|f| f.selected_window)
        .ok_or_else(|| signal("error", vec![Value::string("No selected window")]))?;
    if let Some(w) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(sel_wid))
    {
        w.set_buffer(buf_id);
    }
    eval.buffers.set_current(buf_id);
    Ok(Value::Buffer(buf_id))
}

// ===========================================================================
// Frame operations
// ===========================================================================

/// `(selected-frame)` -> frame id (int).
pub(crate) fn builtin_selected_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("selected-frame", &args, 0)?;
    let fid = ensure_selected_frame_id(eval);
    Ok(Value::Int(fid.0 as i64))
}

/// `(select-frame FRAME &optional NORECORD)` -> frame.
pub(crate) fn builtin_select_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("select-frame", &args, 1)?;
    expect_max_args("select-frame", &args, 2)?;
    let fid = match &args[0] {
        Value::Int(n) => {
            let fid = FrameId(*n as u64);
            if eval.frames.get(fid).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), Value::Int(*n)],
                ));
            }
            fid
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), other.clone()],
            ))
        }
    };
    if !eval.frames.select_frame(fid) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), args[0].clone()],
        ));
    }
    if let Some(buf_id) = eval
        .frames
        .get(fid)
        .and_then(|f| f.find_window(f.selected_window))
        .and_then(|w| w.buffer_id())
    {
        eval.buffers.set_current(buf_id);
    }
    Ok(Value::Int(fid.0 as i64))
}

/// `(select-frame-set-input-focus FRAME &optional NORECORD)` -> nil.
pub(crate) fn builtin_select_frame_set_input_focus(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("select-frame-set-input-focus", &args, 1)?;
    expect_max_args("select-frame-set-input-focus", &args, 2)?;
    let fid = match &args[0] {
        Value::Int(n) => {
            let fid = FrameId(*n as u64);
            if eval.frames.get(fid).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), Value::Int(*n)],
                ));
            }
            fid
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), other.clone()],
            ))
        }
    };
    if !eval.frames.select_frame(fid) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), args[0].clone()],
        ));
    }
    if let Some(buf_id) = eval
        .frames
        .get(fid)
        .and_then(|f| f.find_window(f.selected_window))
        .and_then(|w| w.buffer_id())
    {
        eval.buffers.set_current(buf_id);
    }
    Ok(Value::Nil)
}

/// `(frame-list)` -> list of frame ids.
pub(crate) fn builtin_frame_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("frame-list", &args, 0)?;
    let _ = ensure_selected_frame_id(eval);
    let ids: Vec<Value> = eval
        .frames
        .frame_list()
        .into_iter()
        .map(|fid| Value::Int(fid.0 as i64))
        .collect();
    Ok(Value::list(ids))
}

/// `(make-frame &optional PARAMETERS)` -> frame id.
///
/// Creates a new frame.  PARAMETERS is an alist; we currently
/// only honour `width`, `height`, and `name`.
pub(crate) fn builtin_make_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("make-frame", &args, 1)?;
    let mut width: u32 = 800;
    let mut height: u32 = 600;
    let mut name = String::from("F");

    // Parse optional alist parameters.
    if let Some(params) = args.first() {
        if let Some(items) = super::value::list_to_vec(params) {
            for item in &items {
                if let Value::Cons(cell) = item {
                    let pair = cell.lock().expect("poisoned");
                    if let Value::Symbol(key) = &pair.car {
                        match key.as_str() {
                            "width" => {
                                if let Some(n) = pair.cdr.as_int() {
                                    width = n as u32;
                                }
                            }
                            "height" => {
                                if let Some(n) = pair.cdr.as_int() {
                                    height = n as u32;
                                }
                            }
                            "name" => {
                                if let Some(s) = pair.cdr.as_str() {
                                    name = s.to_string();
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    // Use the current buffer (or BufferId(0) as fallback) for the initial window.
    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .unwrap_or(BufferId(0));
    let fid = eval.frames.create_frame(&name, width, height, buf_id);
    Ok(Value::Int(fid.0 as i64))
}

/// `(delete-frame &optional FRAME FORCE)` -> nil.
pub(crate) fn builtin_delete_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("delete-frame", &args, 2)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    if !eval.frames.delete_frame(fid) {
        return Err(signal("error", vec![Value::string("Cannot delete frame")]));
    }
    Ok(Value::Nil)
}

/// `(frame-parameter FRAME PARAMETER)` -> value or nil.
pub(crate) fn builtin_frame_parameter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("frame-parameter", &args, 2)?;
    expect_max_args("frame-parameter", &args, 2)?;
    let fid = resolve_frame_id(eval, Some(&args[0]), "framep")?;
    let param_name = match &args[1] {
        Value::Symbol(s) => s.clone(),
        _ => return Ok(Value::Nil),
    };
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    // Check built-in properties first.
    match param_name.as_str() {
        "name" => return Ok(Value::string(frame.name.clone())),
        "title" => return Ok(Value::string(frame.title.clone())),
        // In Emacs, frame parameter width/height are text columns/lines.
        // For the bootstrap batch frame, explicit parameter overrides preserve
        // the 80x25 report shape.
        "width" => {
            return Ok(frame
                .parameters
                .get("width")
                .cloned()
                .unwrap_or(Value::Int(frame.columns() as i64)));
        }
        "height" => {
            return Ok(frame
                .parameters
                .get("height")
                .cloned()
                .unwrap_or(Value::Int(frame.lines() as i64)));
        }
        "visibility" => {
            return Ok(if frame.visible {
                Value::True
            } else {
                Value::Nil
            })
        }
        _ => {}
    }
    // User-set parameters.
    Ok(frame
        .parameters
        .get(&param_name)
        .cloned()
        .unwrap_or(Value::Nil))
}

/// `(frame-parameters &optional FRAME)` -> alist.
pub(crate) fn builtin_frame_parameters(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-parameters", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let mut pairs: Vec<Value> = Vec::new();
    // Built-in parameters.
    pairs.push(Value::cons(
        Value::symbol("name"),
        Value::string(frame.name.clone()),
    ));
    pairs.push(Value::cons(
        Value::symbol("title"),
        Value::string(frame.title.clone()),
    ));
    let width = frame
        .parameters
        .get("width")
        .cloned()
        .unwrap_or(Value::Int(frame.columns() as i64));
    let height = frame
        .parameters
        .get("height")
        .cloned()
        .unwrap_or(Value::Int(frame.lines() as i64));
    pairs.push(Value::cons(Value::symbol("width"), width));
    pairs.push(Value::cons(Value::symbol("height"), height));
    pairs.push(Value::cons(
        Value::symbol("visibility"),
        Value::bool(frame.visible),
    ));
    // User parameters.
    for (k, v) in &frame.parameters {
        pairs.push(Value::cons(Value::symbol(k.clone()), v.clone()));
    }
    Ok(Value::list(pairs))
}

/// `(modify-frame-parameters FRAME ALIST)` -> nil.
pub(crate) fn builtin_modify_frame_parameters(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("modify-frame-parameters", &args, 2)?;
    expect_max_args("modify-frame-parameters", &args, 2)?;
    let fid = resolve_frame_id(eval, Some(&args[0]), "frame-live-p")?;
    let items = super::value::list_to_vec(&args[1]).unwrap_or_default();

    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    for item in items {
        if let Value::Cons(cell) = &item {
            let pair = cell.lock().expect("poisoned");
            if let Value::Symbol(key) = &pair.car {
                match key.as_str() {
                    "name" => {
                        if let Some(s) = pair.cdr.as_str() {
                            frame.name = s.to_string();
                        }
                    }
                    "title" => {
                        if let Some(s) = pair.cdr.as_str() {
                            frame.title = s.to_string();
                        }
                    }
                    "width" => {
                        if let Some(n) = pair.cdr.as_int() {
                            frame.width = n as u32;
                        }
                    }
                    "height" => {
                        if let Some(n) = pair.cdr.as_int() {
                            frame.height = n as u32;
                        }
                    }
                    "visibility" => {
                        frame.visible = pair.cdr.is_truthy();
                    }
                    _ => {
                        frame.parameters.insert(key.clone(), pair.cdr.clone());
                    }
                }
            }
        }
    }
    Ok(Value::Nil)
}

/// `(frame-visible-p FRAME)` -> t or nil.
pub(crate) fn builtin_frame_visible_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("frame-visible-p", &args, 1)?;
    let fid = match args.first() {
        Some(Value::Int(n)) => FrameId(*n as u64),
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), other.clone()],
            ))
        }
        None => unreachable!("expect_args enforced"),
    };
    let frame = eval.frames.get(fid).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), args[0].clone()],
        )
    })?;
    Ok(Value::bool(frame.visible))
}

/// `(framep OBJ)` -> t if OBJ is a frame id that exists.
pub(crate) fn builtin_framep(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("framep", &args, 1)?;
    let id = match args[0].as_int() {
        Some(n) => n as u64,
        None => return Ok(Value::Nil),
    };
    Ok(Value::bool(eval.frames.get(FrameId(id)).is_some()))
}

/// `(frame-live-p OBJ)` -> t if OBJ is a live frame id.
pub(crate) fn builtin_frame_live_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("frame-live-p", &args, 1)?;
    let id = match args[0].as_int() {
        Some(n) => n as u64,
        None => return Ok(Value::Nil),
    };
    Ok(Value::bool(eval.frames.get(FrameId(id)).is_some()))
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use crate::elisp::{format_eval_result, parse_forms, Evaluator};

    /// Evaluate all forms with a fresh evaluator that has a frame+window set up.
    fn eval_with_frame(src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        // Create a buffer for the initial window.
        let buf = ev.buffers.create_buffer("*scratch*");
        // Create a frame so window/frame builtins have something to work with.
        ev.frames.create_frame("F1", 800, 600, buf);
        ev.eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect()
    }

    fn eval_one_with_frame(src: &str) -> String {
        eval_with_frame(src).into_iter().next().unwrap()
    }

    // -- Window queries --

    #[test]
    fn selected_window_returns_int() {
        let r = eval_one_with_frame("(selected-window)");
        assert!(r.starts_with("OK "), "expected OK, got: {r}");
        let val: i64 = r.strip_prefix("OK ").unwrap().trim().parse().unwrap();
        assert!(val > 0);
    }

    #[test]
    fn selected_window_bootstraps_initial_frame() {
        let forms = parse_forms("(window-live-p (selected-window))").expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
    }

    #[test]
    fn frame_selected_window_arity_and_designators() {
        let forms = parse_forms(
            "(windowp (frame-selected-window))
             (windowp (frame-selected-window nil))
             (windowp (frame-selected-window (selected-frame)))
             (condition-case err (frame-selected-window \"x\") (error err))
             (condition-case err (frame-selected-window 999999) (error err))
             (condition-case err (frame-selected-window nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
        assert_eq!(out[1], "OK t");
        assert_eq!(out[2], "OK t");
        assert_eq!(out[3], "OK (wrong-type-argument frame-live-p \"x\")");
        assert_eq!(out[4], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[5], "OK wrong-number-of-arguments");
    }

    #[test]
    fn window_frame_arity_and_designators() {
        let forms = parse_forms(
            "(framep (window-frame))
             (framep (window-frame nil))
             (framep (window-frame (selected-window)))
             (condition-case err (window-frame \"x\") (error err))
             (condition-case err (window-frame 999999) (error err))
             (condition-case err (window-frame nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
        assert_eq!(out[1], "OK t");
        assert_eq!(out[2], "OK t");
        assert_eq!(out[3], "OK (wrong-type-argument window-valid-p \"x\")");
        assert_eq!(out[4], "OK (wrong-type-argument window-valid-p 999999)");
        assert_eq!(out[5], "OK wrong-number-of-arguments");
    }

    #[test]
    fn window_designators_bootstrap_nil_and_validate_invalid_window_handles() {
        let forms = parse_forms(
            "(window-start nil)
             (window-point nil)
             (window-buffer nil)
             (condition-case err (window-start 999999) (error err))
             (condition-case err (window-buffer 999999) (error err))
             (condition-case err (set-window-start nil 1) (error err))
             (condition-case err (set-window-point nil 1) (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK 1");
        assert_eq!(out[1], "OK 1");
        assert!(
            out[2].starts_with("OK #<buffer "),
            "unexpected value: {}",
            out[2]
        );
        assert_eq!(out[3], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[4], "OK (wrong-type-argument windowp 999999)");
        assert_eq!(out[5], "OK 1");
        assert_eq!(out[6], "OK 1");
    }

    #[test]
    fn windowp_true() {
        let r = eval_with_frame("(windowp (selected-window))");
        assert_eq!(r[0], "OK t");
    }

    #[test]
    fn windowp_true_for_stale_deleted_window() {
        let r = eval_one_with_frame("(let ((w (split-window))) (delete-window w) (windowp w))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn windowp_false() {
        let r = eval_one_with_frame("(windowp 999999)");
        assert_eq!(r, "OK nil");
    }

    #[test]
    fn window_live_p_true() {
        let r = eval_with_frame("(window-live-p (selected-window))");
        assert_eq!(r[0], "OK t");
    }

    #[test]
    fn window_live_p_false_for_non_window() {
        let r = eval_one_with_frame("(window-live-p 999999)");
        assert_eq!(r, "OK nil");
    }

    #[test]
    fn window_buffer_returns_buffer() {
        let r = eval_one_with_frame("(bufferp (window-buffer))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn window_buffer_returns_nil_for_stale_deleted_window() {
        let r =
            eval_one_with_frame("(let ((w (split-window))) (delete-window w) (window-buffer w))");
        assert_eq!(r, "OK nil");
    }

    #[test]
    fn window_start_default() {
        let r = eval_one_with_frame("(window-start)");
        assert_eq!(r, "OK 0");
    }

    #[test]
    fn set_window_start_and_read() {
        let results = eval_with_frame(
            "(set-window-start (selected-window) 42)
             (window-start)",
        );
        assert_eq!(results[0], "OK 42");
        assert_eq!(results[1], "OK 42");
    }

    #[test]
    fn window_point_default() {
        let r = eval_one_with_frame("(window-point)");
        assert_eq!(r, "OK 0");
    }

    #[test]
    fn set_window_point_and_read() {
        let results = eval_with_frame(
            "(set-window-point (selected-window) 10)
             (window-point)",
        );
        assert_eq!(results[0], "OK 10");
        assert_eq!(results[1], "OK 10");
    }

    #[test]
    fn window_height_positive() {
        let r = eval_one_with_frame("(window-height)");
        assert!(r.starts_with("OK "));
        let val: i64 = r.strip_prefix("OK ").unwrap().trim().parse().unwrap();
        assert!(val > 0, "window-height should be positive, got {val}");
    }

    #[test]
    fn window_width_positive() {
        let r = eval_one_with_frame("(window-width)");
        assert!(r.starts_with("OK "));
        let val: i64 = r.strip_prefix("OK ").unwrap().trim().parse().unwrap();
        assert!(val > 0, "window-width should be positive, got {val}");
    }

    #[test]
    fn window_body_height_pixelwise() {
        let r = eval_one_with_frame("(window-body-height nil t)");
        assert!(r.starts_with("OK "));
        let val: i64 = r.strip_prefix("OK ").unwrap().trim().parse().unwrap();
        // Batch mode returns character rows.
        assert_eq!(val, 36);
    }

    #[test]
    fn window_body_width_pixelwise() {
        let r = eval_one_with_frame("(window-body-width nil t)");
        assert!(r.starts_with("OK "));
        let val: i64 = r.strip_prefix("OK ").unwrap().trim().parse().unwrap();
        // Batch mode returns character columns.
        assert_eq!(val, 100);
    }

    #[test]
    fn window_total_size_queries_work() {
        let results = eval_with_frame(
            "(list (integerp (window-total-height))
                   (integerp (window-total-width))
                   (integerp (window-total-height nil t))
                   (integerp (window-total-width nil t)))",
        );
        assert_eq!(results[0], "OK (t t t t)");
    }

    #[test]
    fn get_buffer_window_finds_selected_window_for_current_buffer() {
        let result = eval_one_with_frame(
            "(let ((w (selected-window)))
               (eq w (get-buffer-window (window-buffer w))))",
        );
        assert_eq!(result, "OK t");
    }

    #[test]
    fn get_buffer_window_list_returns_matching_windows() {
        let result = eval_one_with_frame("(length (get-buffer-window-list (window-buffer)))");
        assert_eq!(result, "OK 1");
    }

    #[test]
    fn get_buffer_window_and_list_match_optional_and_missing_buffer_semantics() {
        let forms = parse_forms(
            "(condition-case err (get-buffer-window) (error err))
             (condition-case err (get-buffer-window nil) (error err))
             (condition-case err (get-buffer-window \"missing\") (error err))
             (windowp (get-buffer-window \"*scratch*\"))
             (condition-case err (get-buffer-window-list) (error err))
             (condition-case err (get-buffer-window-list nil) (error err))
             (length (get-buffer-window-list \"*scratch*\"))
             (condition-case err (get-buffer-window-list \"missing\") (error err))
             (condition-case err (get-buffer-window-list 1) (error err))
             (let ((b (generate-new-buffer \"gbwl-live\")))
               (prog1 (condition-case err (get-buffer-window-list b) (error err))
                 (kill-buffer b)))
             (let ((b (generate-new-buffer \"gbwl-dead\")))
               (kill-buffer b)
               (condition-case err (get-buffer-window-list b) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let results = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(results[0], "OK nil");
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK nil");
        assert_eq!(results[3], "OK t");
        assert_eq!(results[4], "OK nil");
        assert_eq!(results[5], "OK nil");
        assert_eq!(results[6], "OK 1");
        assert_eq!(results[7], "OK (error \"No such live buffer missing\")");
        assert_eq!(results[8], "OK (error \"No such buffer 1\")");
        assert_eq!(results[9], "OK nil");
        assert_eq!(
            results[10],
            "OK (error \"No such live buffer #<killed buffer>\")"
        );
    }

    #[test]
    fn fit_window_to_buffer_returns_nil_in_batch_mode() {
        let result = eval_one_with_frame("(fit-window-to-buffer)");
        assert_eq!(result, "OK nil");
    }

    #[test]
    fn fit_window_to_buffer_invalid_window_designators_signal_error() {
        let results = eval_with_frame(
            "(condition-case err (fit-window-to-buffer 999999) (error (car err)))
             (condition-case err (fit-window-to-buffer 'foo) (error (car err)))
             (let ((w (split-window)))
               (delete-window w)
               (condition-case err (fit-window-to-buffer w) (error (car err))))",
        );
        assert_eq!(results[0], "OK error");
        assert_eq!(results[1], "OK error");
        assert_eq!(results[2], "OK error");
    }

    #[test]
    fn window_list_returns_list() {
        let r = eval_one_with_frame("(listp (window-list))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn window_list_has_one_entry() {
        let r = eval_one_with_frame("(length (window-list))");
        assert_eq!(r, "OK 1");
    }

    #[test]
    fn window_list_matches_frame_minibuffer_and_all_frames_batch_semantics() {
        let forms = parse_forms(
            "(condition-case err (length (window-list)) (error err))
             (condition-case err (length (window-list (selected-frame))) (error err))
             (condition-case err (window-list 999999) (error err))
             (condition-case err (window-list 'foo) (error err))
             (condition-case err (window-list (selected-window)) (error err))
             (condition-case err (window-list 999999 nil t) (error err))
             (condition-case err (window-list nil nil t) (error err))
             (condition-case err (window-list nil nil 0) (error err))
             (length (window-list nil t))
             (length (window-list (selected-frame) t))
             (length (window-list nil nil (selected-window)))
             (length (window-list nil t (selected-window)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK 1");
        assert_eq!(out[1], "OK 1");
        assert_eq!(out[2], "OK (error \"Window is on a different frame\")");
        assert_eq!(out[3], "OK (error \"Window is on a different frame\")");
        assert_eq!(out[4], "OK (error \"Window is on a different frame\")");
        assert_eq!(out[5], "OK (wrong-type-argument windowp t)");
        assert_eq!(out[6], "OK (wrong-type-argument windowp t)");
        assert_eq!(out[7], "OK (wrong-type-argument windowp 0)");
        assert_eq!(out[8], "OK 2");
        assert_eq!(out[9], "OK 2");
        assert_eq!(out[10], "OK 1");
        assert_eq!(out[11], "OK 2");
    }

    #[test]
    fn minibuffer_window_from_window_list_supports_basic_accessors() {
        let forms = parse_forms(
            "(let ((m (car (last (window-list nil t)))))
               (list (window-live-p m)
                     (windowp m)
                     (buffer-name (window-buffer m))
                     (window-start m)
                     (window-point m)))
             (let ((m (car (last (window-list nil t)))))
               (set-window-start m 7)
               (window-start m))
             (let ((m (car (last (window-list nil t)))))
               (set-window-point m 8)
               (window-point m))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (t t \" *Minibuf-0*\" 1 1)");
        assert_eq!(out[1], "OK 1");
        assert_eq!(out[2], "OK 1");
    }

    #[test]
    fn window_dedicated_p_default() {
        let r = eval_one_with_frame("(window-dedicated-p)");
        assert_eq!(r, "OK nil");
    }

    #[test]
    fn window_accessors_enforce_max_arity() {
        let forms = parse_forms(
            "(condition-case err (window-buffer nil nil) (error (car err)))
             (condition-case err (window-start nil nil) (error (car err)))
             (condition-case err (window-end nil nil nil) (error (car err)))
             (condition-case err (window-point nil nil) (error (car err)))
             (condition-case err (window-dedicated-p nil nil) (error (car err)))
             (condition-case err (set-window-start nil 1 nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK wrong-number-of-arguments");
        assert_eq!(out[2], "OK wrong-number-of-arguments");
        assert_eq!(out[3], "OK wrong-number-of-arguments");
        assert_eq!(out[4], "OK wrong-number-of-arguments");
        assert_eq!(out[5], "OK wrong-number-of-arguments");
    }

    #[test]
    fn set_window_dedicated_p() {
        let results = eval_with_frame(
            "(set-window-dedicated-p (selected-window) t)
             (window-dedicated-p)",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK t");
    }

    #[test]
    fn set_window_dedicated_p_bootstraps_nil_and_validates_designators() {
        let forms = parse_forms(
            "(condition-case err (set-window-dedicated-p nil t) (error err))
             (window-dedicated-p nil)
             (condition-case err (set-window-dedicated-p 'foo t) (error err))
             (condition-case err (set-window-dedicated-p 999999 t) (error err))
             (condition-case err (set-window-dedicated-p nil nil) (error err))
             (window-dedicated-p nil)",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
        assert_eq!(out[1], "OK t");
        assert_eq!(out[2], "OK (wrong-type-argument window-live-p foo)");
        assert_eq!(out[3], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[4], "OK nil");
        assert_eq!(out[5], "OK nil");
    }

    // -- Window manipulation --

    #[test]
    fn split_window_creates_new() {
        let results = eval_with_frame(
            "(split-window)
             (length (window-list))",
        );
        assert!(results[0].starts_with("OK "));
        assert_eq!(results[1], "OK 2");
    }

    #[test]
    fn split_window_enforces_max_arity() {
        let forms = parse_forms(
            "(condition-case err (split-window nil nil nil nil nil) (error (car err)))
             (let ((w (split-window nil nil nil nil)))
               (window-live-p w))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK t");
    }

    #[test]
    fn split_delete_window_invalid_designators_signal_error() {
        let results = eval_with_frame(
            "(condition-case err (split-window 999999) (error (car err)))
             (condition-case err (split-window 'foo) (error (car err)))
             (condition-case err (delete-window 999999) (error (car err)))
             (condition-case err (delete-window 'foo) (error (car err)))
             (condition-case err (delete-other-windows 999999) (error (car err)))
             (condition-case err (delete-other-windows 'foo) (error (car err)))",
        );
        assert_eq!(results[0], "OK error");
        assert_eq!(results[1], "OK error");
        assert_eq!(results[2], "OK error");
        assert_eq!(results[3], "OK error");
        assert_eq!(results[4], "OK error");
        assert_eq!(results[5], "OK error");
    }

    #[test]
    fn delete_window_after_split() {
        let results = eval_with_frame(
            "(let ((new-win (split-window)))
               (delete-window new-win)
               (length (window-list)))",
        );
        assert_eq!(results[0], "OK 1");
    }

    #[test]
    fn delete_window_updates_current_buffer_to_selected_window_buffer() {
        let result = eval_one_with_frame(
            "(save-current-buffer
               (let* ((b1 (get-buffer-create \"dw-curbuf-a\"))
                      (b2 (get-buffer-create \"dw-curbuf-b\")))
                 (set-window-buffer nil b1)
                 (let ((w2 (split-window)))
                   (set-window-buffer w2 b2)
                   (select-window w2)
                   (delete-window w2)
                   (buffer-name (current-buffer)))))",
        );
        assert_eq!(result, "OK \"dw-curbuf-a\"");
    }

    #[test]
    fn delete_sole_window_errors() {
        let r = eval_one_with_frame("(delete-window)");
        assert!(r.contains("ERR"), "deleting sole window should error: {r}");
    }

    #[test]
    fn delete_window_and_delete_other_windows_enforce_max_arity() {
        let forms = parse_forms(
            "(condition-case err (delete-window nil nil) (error (car err)))
             (condition-case err (delete-other-windows nil nil nil) (error (car err)))
             (condition-case err
                 (let ((w2 (split-window)))
                   (delete-other-windows w2 nil))
               (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK wrong-number-of-arguments");
        assert_eq!(out[2], "OK nil");
    }

    #[test]
    fn delete_other_windows_keeps_one() {
        let results = eval_with_frame(
            "(split-window)
             (split-window)
             (delete-other-windows)
             (length (window-list))",
        );
        assert_eq!(results[3], "OK 1");
    }

    #[test]
    fn delete_other_windows_updates_current_buffer_when_kept_window_differs() {
        let result = eval_one_with_frame(
            "(save-current-buffer
               (let* ((b1 (get-buffer-create \"dow-curbuf-a\"))
                      (b2 (get-buffer-create \"dow-curbuf-b\")))
                 (set-window-buffer nil b1)
                 (let ((w2 (split-window))
                       (w1 (selected-window)))
                   (set-window-buffer w2 b2)
                   (select-window w2)
                   (delete-other-windows w1)
                   (buffer-name (current-buffer)))))",
        );
        assert_eq!(result, "OK \"dow-curbuf-a\"");
    }

    #[test]
    fn select_window_works() {
        let results = eval_with_frame(
            "(let ((new-win (split-window)))
               (select-window new-win)
               (= (selected-window) new-win))",
        );
        assert_eq!(results[0], "OK t");
    }

    #[test]
    fn select_window_validates_designators_and_arity() {
        let forms = parse_forms(
            "(condition-case err (select-window nil) (error err))
             (condition-case err (select-window 'foo) (error err))
             (condition-case err (select-window 999999) (error err))
             (windowp (select-window (selected-window)))
             (condition-case err (select-window (selected-window) nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (wrong-type-argument window-live-p nil)");
        assert_eq!(out[1], "OK (wrong-type-argument window-live-p foo)");
        assert_eq!(out[2], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[3], "OK t");
        assert_eq!(out[4], "OK wrong-number-of-arguments");
    }

    #[test]
    fn select_window_updates_current_buffer_to_selected_window_buffer() {
        let result = eval_one_with_frame(
            "(save-current-buffer
               (let* ((b1 (get-buffer-create \"sw-curbuf-a\"))
                      (b2 (get-buffer-create \"sw-curbuf-b\")))
                 (set-window-buffer nil b1)
                 (let ((w2 (split-window)))
                   (set-window-buffer w2 b2)
                   (select-window w2)
                   (buffer-name (current-buffer)))))",
        );
        assert_eq!(result, "OK \"sw-curbuf-b\"");
    }

    #[test]
    fn other_window_cycles() {
        let results = eval_with_frame(
            "(let ((w1 (selected-window)))
               (split-window)
               (other-window 1)
               (/= (selected-window) w1))",
        );
        assert_eq!(results[0], "OK t");
    }

    #[test]
    fn other_window_updates_current_buffer_to_selected_window_buffer() {
        let result = eval_one_with_frame(
            "(save-current-buffer
               (let* ((b1 (get-buffer-create \"ow-curbuf-a\"))
                      (b2 (get-buffer-create \"ow-curbuf-b\")))
                 (set-window-buffer nil b1)
                 (let ((w2 (split-window)))
                   (set-window-buffer w2 b2)
                   (other-window 1)
                   (buffer-name (current-buffer)))))",
        );
        assert_eq!(result, "OK \"ow-curbuf-b\"");
    }

    #[test]
    fn other_window_defaults_count_to_one() {
        let results = eval_with_frame(
            "(let ((w1 (selected-window)))
               (split-window)
               (other-window)
               (/= (selected-window) w1))",
        );
        assert_eq!(results[0], "OK t");
    }

    #[test]
    fn other_window_enforces_max_arity() {
        let forms = parse_forms(
            "(condition-case err (other-window 1 nil nil nil) (error (car err)))
             (condition-case err
                 (let ((w1 (selected-window)))
                   (split-window)
                   (other-window 1 nil nil)
                   (not (eq (selected-window) w1)))
               (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK t");
    }

    #[test]
    fn other_window_without_selected_frame_returns_nil() {
        let forms = parse_forms("(other-window)").expect("parse");
        let mut ev = Evaluator::new();
        let results = ev.eval_forms(&forms);
        assert_eq!(format_eval_result(&results[0]), "OK nil");
    }

    #[test]
    fn selected_frame_bootstraps_initial_frame() {
        let forms =
            parse_forms("(list (framep (selected-frame)) (length (frame-list)))").expect("parse");
        let mut ev = Evaluator::new();
        let results = ev.eval_forms(&forms);
        assert_eq!(format_eval_result(&results[0]), "OK (t 1)");
    }

    #[test]
    fn window_size_queries_bootstrap_initial_frame() {
        let forms = parse_forms(
            "(list (integerp (window-height))
                   (integerp (window-width))
                   (integerp (window-body-height))
                   (integerp (window-body-width)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let results = ev.eval_forms(&forms);
        assert_eq!(format_eval_result(&results[0]), "OK (t t t t)");
    }

    #[test]
    fn window_size_queries_match_batch_defaults_and_invalid_window_predicates() {
        let forms = parse_forms(
            "(window-height nil)
             (window-width nil)
             (window-body-height nil)
             (window-body-width nil)
             (window-total-height nil)
             (window-total-width nil)
             (condition-case err (window-height 999999) (error err))
             (condition-case err (window-width 999999) (error err))
             (condition-case err (window-body-height 999999) (error err))
             (condition-case err (window-body-width 999999) (error err))
             (condition-case err (window-total-height 999999) (error err))
             (condition-case err (window-total-width 999999) (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK 24");
        assert_eq!(out[1], "OK 80");
        assert_eq!(out[2], "OK 23");
        assert_eq!(out[3], "OK 80");
        assert_eq!(out[4], "OK 24");
        assert_eq!(out[5], "OK 80");
        assert_eq!(out[6], "OK (wrong-type-argument window-valid-p 999999)");
        assert_eq!(out[7], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[8], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[9], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[10], "OK (wrong-type-argument window-valid-p 999999)");
        assert_eq!(out[11], "OK (wrong-type-argument window-valid-p 999999)");
    }

    #[test]
    fn next_window_cycles() {
        let results = eval_with_frame(
            "(let ((w1 (selected-window)))
               (split-window)
               (let ((w2 (next-window)))
                 (/= w1 w2)))",
        );
        assert_eq!(results[0], "OK t");
    }

    #[test]
    fn next_previous_window_enforce_max_arity() {
        let forms = parse_forms(
            "(condition-case err (next-window nil nil nil nil) (error (car err)))
             (condition-case err (previous-window nil nil nil nil) (error (car err)))
             (let ((w1 (selected-window)))
               (split-window)
               (windowp (next-window w1 nil nil)))
             (let ((w1 (selected-window)))
               (split-window)
               (windowp (previous-window w1 nil nil)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK wrong-number-of-arguments");
        assert_eq!(out[2], "OK t");
        assert_eq!(out[3], "OK t");
    }

    #[test]
    fn previous_window_wraps() {
        let results = eval_with_frame(
            "(split-window)
             (let ((w (previous-window)))
               (windowp w))",
        );
        assert_eq!(results[1], "OK t");
    }

    // -- Frame operations --

    #[test]
    fn frame_ops_enforce_max_arity() {
        let forms = parse_forms(
            "(condition-case err (make-frame nil nil) (error (car err)))
             (condition-case err (delete-frame nil nil nil) (error (car err)))
             (condition-case err (frame-parameter nil 'name nil) (error (car err)))
             (condition-case err (frame-parameters nil nil) (error (car err)))
             (condition-case err (modify-frame-parameters nil nil nil) (error (car err)))
             (condition-case err (frame-visible-p nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK wrong-number-of-arguments");
        assert_eq!(out[2], "OK wrong-number-of-arguments");
        assert_eq!(out[3], "OK wrong-number-of-arguments");
        assert_eq!(out[4], "OK wrong-number-of-arguments");
        assert_eq!(out[5], "OK wrong-number-of-arguments");
    }

    #[test]
    fn frame_visible_p_enforces_arity_and_designators() {
        let forms = parse_forms(
            "(condition-case err (frame-visible-p) (error (car err)))
             (condition-case err (frame-visible-p nil) (error err))
             (condition-case err (frame-visible-p 999999) (error err))
             (frame-visible-p (selected-frame))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK (wrong-type-argument frame-live-p nil)");
        assert_eq!(out[2], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[3], "OK t");
    }

    #[test]
    fn frame_designator_errors_use_emacs_predicates() {
        let forms = parse_forms(
            "(condition-case err (frame-parameter \"x\" 'name) (error err))
             (condition-case err (frame-parameter 999999 'name) (error err))
             (condition-case err (frame-parameters \"x\") (error err))
             (condition-case err (frame-parameters 999999) (error err))
             (condition-case err (modify-frame-parameters \"x\" nil) (error err))
             (condition-case err (modify-frame-parameters 999999 nil) (error err))
             (condition-case err (delete-frame \"x\") (error err))
             (condition-case err (delete-frame 999999) (error err))
             (frame-parameter nil 'name)
             (condition-case err (modify-frame-parameters nil nil) (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (wrong-type-argument framep \"x\")");
        assert_eq!(out[1], "OK (wrong-type-argument framep 999999)");
        assert_eq!(out[2], "OK (wrong-type-argument framep \"x\")");
        assert_eq!(out[3], "OK (wrong-type-argument framep 999999)");
        assert_eq!(out[4], "OK (wrong-type-argument frame-live-p \"x\")");
        assert_eq!(out[5], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[6], "OK (wrong-type-argument framep \"x\")");
        assert_eq!(out[7], "OK (wrong-type-argument framep 999999)");
        assert_eq!(out[8], "OK \"F1\"");
        assert_eq!(out[9], "OK nil");
    }

    #[test]
    fn select_frame_arity_designators_and_selection() {
        let forms = parse_forms(
            "(condition-case err (select-frame) (error (car err)))
             (condition-case err (select-frame nil) (error err))
             (condition-case err (select-frame \"x\") (error err))
             (condition-case err (select-frame 999999) (error err))
             (let ((f1 (selected-frame))
                   (f2 (make-frame)))
               (prog1
                   (list (framep (select-frame f2))
                         (eq (selected-frame) f2))
                 (select-frame f1)
                 (delete-frame f2)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK (wrong-type-argument frame-live-p nil)");
        assert_eq!(out[2], "OK (wrong-type-argument frame-live-p \"x\")");
        assert_eq!(out[3], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[4], "OK (t t)");
    }

    #[test]
    fn select_frame_set_input_focus_arity_designators_and_result() {
        let forms = parse_forms(
            "(condition-case err (select-frame-set-input-focus) (error (car err)))
             (condition-case err (select-frame-set-input-focus nil) (error err))
             (condition-case err (select-frame-set-input-focus \"x\") (error err))
             (condition-case err (select-frame-set-input-focus 999999) (error err))
             (let ((f (selected-frame)))
               (list (select-frame-set-input-focus f)
                     (eq (selected-frame) f)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK (wrong-type-argument frame-live-p nil)");
        assert_eq!(out[2], "OK (wrong-type-argument frame-live-p \"x\")");
        assert_eq!(out[3], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[4], "OK (nil t)");
    }

    #[test]
    fn selected_frame_returns_int() {
        let r = eval_one_with_frame("(selected-frame)");
        assert!(r.starts_with("OK "));
        let val: i64 = r.strip_prefix("OK ").unwrap().trim().parse().unwrap();
        assert!(val > 0);
    }

    #[test]
    fn frame_list_has_one() {
        let r = eval_one_with_frame("(length (frame-list))");
        assert_eq!(r, "OK 1");
    }

    #[test]
    fn make_frame_creates_new() {
        let results = eval_with_frame(
            "(make-frame)
             (length (frame-list))",
        );
        assert!(results[0].starts_with("OK "));
        assert_eq!(results[1], "OK 2");
    }

    #[test]
    fn delete_frame_works() {
        let results = eval_with_frame(
            "(let ((f2 (make-frame)))
               (delete-frame f2)
               (length (frame-list)))",
        );
        assert_eq!(results[0], "OK 1");
    }

    #[test]
    fn framep_true() {
        let r = eval_one_with_frame("(framep (selected-frame))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn framep_false() {
        let r = eval_one_with_frame("(framep 999999)");
        assert_eq!(r, "OK nil");
    }

    #[test]
    fn frame_live_p_true() {
        let r = eval_one_with_frame("(frame-live-p (selected-frame))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn frame_live_p_false() {
        let r = eval_one_with_frame("(frame-live-p 999999)");
        assert_eq!(r, "OK nil");
    }

    #[test]
    fn frame_visible_p_requires_one_arg() {
        let r = eval_one_with_frame("(frame-visible-p)");
        assert_eq!(r, "OK wrong-number-of-arguments");
    }

    #[test]
    fn frame_parameter_name() {
        let r = eval_one_with_frame("(frame-parameter (selected-frame) 'name)");
        assert_eq!(r, r#"OK "F1""#);
    }

    #[test]
    fn frame_parameter_width() {
        let r = eval_one_with_frame("(frame-parameter (selected-frame) 'width)");
        assert_eq!(r, "OK 100");
    }

    #[test]
    fn frame_parameter_height() {
        let r = eval_one_with_frame("(frame-parameter (selected-frame) 'height)");
        assert_eq!(r, "OK 37");
    }

    #[test]
    fn frame_parameters_returns_alist() {
        let r = eval_one_with_frame("(listp (frame-parameters))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn modify_frame_parameters_name() {
        let results = eval_with_frame(
            "(modify-frame-parameters (selected-frame) '((name . \"NewName\")))
             (frame-parameter (selected-frame) 'name)",
        );
        assert_eq!(results[0], "OK nil");
        assert_eq!(results[1], r#"OK "NewName""#);
    }

    #[test]
    fn switch_to_buffer_changes_window() {
        let results = eval_with_frame(
            "(get-buffer-create \"other-buf\")
             (switch-to-buffer \"other-buf\")
             (bufferp (window-buffer))",
        );
        assert_eq!(results[2], "OK t");
    }

    #[test]
    fn set_window_buffer_works() {
        let results = eval_with_frame(
            "(get-buffer-create \"buf2\")
             (set-window-buffer (selected-window) \"buf2\")
             (bufferp (window-buffer))",
        );
        assert_eq!(results[1], "OK nil"); // set-window-buffer returns nil
        assert_eq!(results[2], "OK t");
    }

    #[test]
    fn window_end_greater_than_start() {
        let r = eval_one_with_frame("(> (window-end) (window-start))");
        assert_eq!(r, "OK t");
    }

    #[test]
    fn display_buffer_returns_window() {
        let results = eval_with_frame(
            "(get-buffer-create \"disp-buf\")
             (windowp (display-buffer \"disp-buf\"))",
        );
        assert_eq!(results[1], "OK t");
    }

    #[test]
    fn pop_to_buffer_returns_buffer() {
        let results = eval_with_frame(
            "(get-buffer-create \"pop-buf\")
             (bufferp (pop-to-buffer \"pop-buf\"))",
        );
        assert_eq!(results[1], "OK t");
    }

    #[test]
    fn switch_display_pop_bootstrap_initial_frame() {
        let forms = parse_forms(
            "(save-current-buffer (bufferp (switch-to-buffer \"*scratch*\")))
             (save-current-buffer (windowp (display-buffer \"*scratch*\")))
             (save-current-buffer (bufferp (pop-to-buffer \"*scratch*\")))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
        assert_eq!(out[1], "OK t");
        assert_eq!(out[2], "OK t");
    }

    #[test]
    fn switch_display_pop_enforce_max_arity() {
        let results = eval_with_frame(
            "(condition-case err (switch-to-buffer \"*scratch*\" nil nil nil) (error (car err)))
             (condition-case err (display-buffer \"*scratch*\" nil nil nil) (error (car err)))
             (condition-case err (pop-to-buffer \"*scratch*\" nil nil nil) (error (car err)))
             (condition-case err (set-window-buffer (selected-window) \"*scratch*\" nil nil) (error (car err)))",
        );
        assert_eq!(results[0], "OK wrong-number-of-arguments");
        assert_eq!(results[1], "OK wrong-number-of-arguments");
        assert_eq!(results[2], "OK wrong-number-of-arguments");
        assert_eq!(results[3], "OK wrong-number-of-arguments");
    }

    #[test]
    fn switch_display_pop_reject_non_buffer_designators() {
        let results = eval_with_frame(
            "(condition-case err (switch-to-buffer 1) (error (list (car err) (cadr err) (caddr err))))
             (condition-case err (display-buffer 1) (error (list (car err) (cadr err) (caddr err))))
             (condition-case err (pop-to-buffer 1) (error (list (car err) (cadr err) (caddr err))))
             (condition-case err (set-window-buffer (selected-window) 1) (error (list (car err) (cadr err) (caddr err))))",
        );
        assert_eq!(results[0], "OK (wrong-type-argument stringp 1)");
        assert_eq!(results[1], "OK (wrong-type-argument stringp 1)");
        assert_eq!(results[2], "OK (wrong-type-argument stringp 1)");
        assert_eq!(results[3], "OK (wrong-type-argument stringp 1)");
    }

    #[test]
    fn switch_and_pop_create_missing_named_buffers() {
        let results = eval_with_frame(
            "(save-current-buffer (bufferp (switch-to-buffer \"sw-auto-create\")))
             (buffer-live-p (get-buffer \"sw-auto-create\"))
             (kill-buffer \"sw-auto-create\")
             (save-current-buffer (bufferp (pop-to-buffer \"pop-auto-create\")))
             (buffer-live-p (get-buffer \"pop-auto-create\"))
             (kill-buffer \"pop-auto-create\")",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK t");
        assert_eq!(results[3], "OK t");
        assert_eq!(results[4], "OK t");
        assert_eq!(results[5], "OK t");
    }

    #[test]
    fn display_buffer_missing_or_dead_signals_invalid_buffer() {
        let results = eval_with_frame(
            "(condition-case err (display-buffer \"db-missing\") (error err))
             (let ((b (generate-new-buffer \"db-dead\")))
               (kill-buffer b)
               (condition-case err (display-buffer b) (error err)))",
        );
        assert_eq!(results[0], "OK (error \"Invalid buffer\")");
        assert_eq!(results[1], "OK (error \"Invalid buffer\")");
    }

    #[test]
    fn set_window_buffer_matches_window_and_buffer_designator_errors() {
        let results = eval_with_frame(
            "(condition-case err (set-window-buffer nil \"*scratch*\") (error err))
             (condition-case err (set-window-buffer nil \"swb-missing\") (error err))
             (let ((b (generate-new-buffer \"swb-dead\")))
               (kill-buffer b)
               (condition-case err (set-window-buffer nil b) (error err)))
             (condition-case err (set-window-buffer 999999 \"*scratch*\") (error err))
             (condition-case err (set-window-buffer 'foo \"*scratch*\") (error err))",
        );
        assert_eq!(results[0], "OK nil");
        assert_eq!(results[1], "OK (wrong-type-argument bufferp nil)");
        assert_eq!(
            results[2],
            "OK (error \"Attempt to display deleted buffer\")"
        );
        assert_eq!(results[3], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(results[4], "OK (wrong-type-argument window-live-p foo)");
    }

    #[test]
    fn set_window_buffer_bootstraps_initial_frame_for_nil_window_designator() {
        let forms = parse_forms(
            "(condition-case err
                 (let ((b (get-buffer-create \"swb-bootstrap\")))
                   (set-buffer b)
                   (erase-buffer)
                   (insert \"abcdef\")
                   (goto-char 1)
                   (set-window-buffer nil b)
                   (list (buffer-name (window-buffer nil))
                         (window-start nil)
                         (window-end nil)))
               (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (\"swb-bootstrap\" 1 7)");
    }
}
