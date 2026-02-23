//! Window, frame, and display-related builtins for the Elisp VM.
//!
//! Bridges the `FrameManager` (in `crate::window`) to Elisp by exposing
//! builtins such as `selected-window`, `split-window`, `selected-frame`, etc.
//! Frames are represented as frame handles. Windows are represented as window
//! handles, while legacy integer designators are still accepted in resolver
//! paths for compatibility.

use super::error::{signal, EvalResult, Flow};
use super::value::{list_to_vec, Value};
use crate::buffer::BufferId;
use crate::window::{FrameId, FrameManager, SplitDirection, Window, WindowId};
use std::collections::HashSet;

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

/// Extract a numeric value from a Value.
fn expect_number(value: &Value) -> Result<f64, Flow> {
    match value {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64 as f64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

#[derive(Clone, Debug)]
enum IntegerOrMarkerArg {
    Int(i64),
    Marker { raw: Value, position: Option<i64> },
}

fn parse_integer_or_marker_arg(value: &Value) -> Result<IntegerOrMarkerArg, Flow> {
    match value {
        Value::Int(n) => Ok(IntegerOrMarkerArg::Int(*n)),
        Value::Char(c) => Ok(IntegerOrMarkerArg::Int(*c as i64)),
        v if super::marker::is_marker(v) => {
            let position = match v {
                Value::Vector(vec) => {
                    let elems = vec.lock().expect("poisoned");
                    match elems.get(2) {
                        Some(Value::Int(n)) => Some(*n),
                        Some(Value::Char(c)) => Some(*c as i64),
                        _ => None,
                    }
                }
                _ => None,
            };
            Ok(IntegerOrMarkerArg::Marker {
                raw: value.clone(),
                position,
            })
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

fn expect_number_or_marker_count(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        Value::Float(n) => Ok(n.floor() as i64),
        marker if super::marker::is_marker(marker) => match parse_integer_or_marker_arg(marker)? {
            IntegerOrMarkerArg::Marker {
                position: Some(pos),
                ..
            } => Ok(pos),
            IntegerOrMarkerArg::Marker { position: None, .. } => Err(signal(
                "error",
                vec![Value::string("Marker does not point anywhere")],
            )),
            IntegerOrMarkerArg::Int(pos) => Ok(pos),
        },
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

fn clamped_window_position(
    eval: &super::eval::Evaluator,
    fid: FrameId,
    wid: WindowId,
    pos: i64,
) -> Option<usize> {
    if pos <= 0 {
        return None;
    }
    let requested = pos as usize;
    let Some(Window::Leaf { buffer_id, .. }) = eval
        .frames
        .get(fid)
        .and_then(|frame| frame.find_window(wid))
    else {
        return Some(requested);
    };
    let buffer_end = eval
        .buffers
        .get(*buffer_id)
        .map(|buf| buf.text.char_count().saturating_add(1))
        .unwrap_or(requested);
    Some(requested.min(buffer_end.max(1)))
}

/// Extract a fixnum-like integer from a Value.
fn expect_fixnum(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), other.clone()],
        )),
    }
}

/// Extract a number-or-marker argument as f64.
fn expect_number_or_marker(value: &Value) -> Result<f64, Flow> {
    match value {
        Value::Int(n) => Ok(*n as f64),
        Value::Char(c) => Ok(*c as i64 as f64),
        Value::Float(f) => Ok(*f),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), other.clone()],
        )),
    }
}

/// Convert a numeric result into Lisp integer/float shape.
fn numeric_value(value: f64) -> Value {
    if value.abs() < f64::EPSILON {
        return Value::Int(0);
    }
    if value.fract().abs() < f64::EPSILON
        && value.is_finite()
        && value >= i64::MIN as f64
        && value <= i64::MAX as f64
    {
        Value::Int(value as i64)
    } else {
        Value::Float(value)
    }
}

/// Parse a window margin argument (`nil` or non-negative integer).
fn expect_margin_width(value: &Value) -> Result<usize, Flow> {
    const MAX_MARGIN: i64 = 2_147_483_647;
    match value {
        Value::Nil => Ok(0),
        Value::Int(n) => {
            if *n < 0 || *n > MAX_MARGIN {
                return Err(signal(
                    "args-out-of-range",
                    vec![Value::Int(*n), Value::Int(0), Value::Int(MAX_MARGIN)],
                ));
            }
            Ok(*n as usize)
        }
        Value::Char(c) => {
            let n = *c as i64;
            if n > MAX_MARGIN {
                return Err(signal(
                    "args-out-of-range",
                    vec![Value::Int(n), Value::Int(0), Value::Int(MAX_MARGIN)],
                ));
            }
            Ok(n as usize)
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn window_value(wid: WindowId) -> Value {
    Value::Window(wid.0)
}

fn window_id_from_designator(value: &Value) -> Option<WindowId> {
    match value {
        Value::Window(id) => Some(WindowId(*id)),
        Value::Int(n) if *n >= 0 => Some(WindowId(*n as u64)),
        _ => None,
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
            let Some(wid) = window_id_from_designator(val) else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(pred), val.clone()],
                ));
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

/// Resolve an optional window designator that may be stale (window object).
///
/// - nil/omitted => selected live window
/// - non-nil invalid designator => `(wrong-type-argument PRED VALUE)`
fn resolve_window_object_id_with_pred(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
    pred: &str,
) -> Result<WindowId, Flow> {
    match arg {
        None | Some(Value::Nil) => {
            let (_fid, wid) = resolve_window_id(eval, None)?;
            Ok(wid)
        }
        Some(val) => {
            let Some(wid) = window_id_from_designator(val) else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(pred), val.clone()],
                ));
            };
            if eval.frames.is_window_object_id(wid) {
                Ok(wid)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(pred), val.clone()],
                ))
            }
        }
    }
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
        Some(value) => {
            let Some(wid) = window_id_from_designator(value) else {
                return Err(signal("error", vec![Value::string("Invalid window")]));
            };
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                Ok((fid, wid))
            } else {
                Err(signal("error", vec![Value::string("Invalid window")]))
            }
        }
    }
}

fn format_window_designator_for_error(eval: &super::eval::Evaluator, value: &Value) -> String {
    if let Some(wid) = window_id_from_designator(value) {
        if eval.frames.is_window_object_id(wid) || matches!(value, Value::Window(_)) {
            return format!("#<window {}>", wid.0);
        }
    }
    super::print::print_value(value)
}

fn resolve_window_id_or_window_error(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
    live_only: bool,
) -> Result<(FrameId, WindowId), Flow> {
    match arg {
        None | Some(Value::Nil) => resolve_window_id(eval, arg),
        Some(val) => {
            let Some(wid) = window_id_from_designator(val) else {
                let window_kind = if live_only { "live" } else { "valid" };
                return Err(signal(
                    "error",
                    vec![Value::string(format!(
                        "{} is not a {} window",
                        format_window_designator_for_error(eval, val),
                        window_kind
                    ))],
                ));
            };
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                Ok((fid, wid))
            } else {
                let window_kind = if live_only { "live" } else { "valid" };
                Err(signal(
                    "error",
                    vec![Value::string(format!(
                        "{} is not a {} window",
                        format_window_designator_for_error(eval, val),
                        window_kind
                    ))],
                ))
            }
        }
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
        Some(Value::Frame(id)) => {
            let fid = FrameId(*id);
            if eval.frames.get(fid).is_some() {
                Ok(fid)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(predicate), Value::Frame(*id)],
                ))
            }
        }
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol(predicate), other.clone()],
        )),
    }
}

/// Resolve a frame designator that may also be a live window designator.
///
/// `frame-first-window` accepts either a frame or window object in GNU Emacs.
fn resolve_frame_or_window_frame_id(
    eval: &mut super::eval::Evaluator,
    arg: Option<&Value>,
    predicate: &str,
) -> Result<FrameId, Flow> {
    match arg {
        None | Some(Value::Nil) => Ok(ensure_selected_frame_id(eval)),
        Some(Value::Frame(id)) => {
            let fid = FrameId(*id);
            if eval.frames.get(fid).is_some() {
                Ok(fid)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol(predicate), Value::Frame(*id)],
                ))
            }
        }
        Some(Value::Int(n)) => {
            let fid = FrameId(*n as u64);
            if eval.frames.get(fid).is_some() {
                return Ok(fid);
            }
            let wid = WindowId(*n as u64);
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                return Ok(fid);
            }
            Err(signal(
                "wrong-type-argument",
                vec![Value::symbol(predicate), Value::Int(*n)],
            ))
        }
        Some(Value::Window(id)) => {
            let wid = WindowId(*id);
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                return Ok(fid);
            }
            Err(signal(
                "wrong-type-argument",
                vec![Value::symbol(predicate), Value::Window(*id)],
            ))
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

fn is_minibuffer_window(frames: &FrameManager, fid: FrameId, wid: WindowId) -> bool {
    frames
        .get(fid)
        .is_some_and(|frame| frame.minibuffer_window == Some(wid))
}

fn window_body_height_lines(frames: &FrameManager, fid: FrameId, wid: WindowId, w: &Window) -> i64 {
    let ch = frames.get(fid).map(|f| f.char_height).unwrap_or(16.0);
    let lines = window_height_lines(w, ch);
    if is_minibuffer_window(frames, fid, wid) {
        lines
    } else {
        lines.saturating_sub(1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ResizeAxis {
    Vertical,
    Horizontal,
}

fn resize_axis(horizontal: bool) -> ResizeAxis {
    if horizontal {
        ResizeAxis::Horizontal
    } else {
        ResizeAxis::Vertical
    }
}

fn axis_size_units(window: &Window, axis: ResizeAxis, char_width: f32, char_height: f32) -> i64 {
    match axis {
        ResizeAxis::Vertical => window_height_lines(window, char_height),
        ResizeAxis::Horizontal => window_width_cols(window, char_width),
    }
}

fn split_matches_axis(direction: SplitDirection, axis: ResizeAxis) -> bool {
    matches!(
        (direction, axis),
        (SplitDirection::Vertical, ResizeAxis::Vertical)
            | (SplitDirection::Horizontal, ResizeAxis::Horizontal)
    )
}

/// Find resize capacities for TARGET in NODE.
///
/// Returns `(target_size, sibling_expand_capacity)` where expand capacity is
/// derived from the nearest ancestor split matching AXIS.
fn find_resize_caps(
    node: &Window,
    target: WindowId,
    axis: ResizeAxis,
    min_size: i64,
    char_width: f32,
    char_height: f32,
) -> Option<(i64, Option<i64>)> {
    match node {
        Window::Leaf { id, .. } => {
            if *id == target {
                Some((axis_size_units(node, axis, char_width, char_height), None))
            } else {
                None
            }
        }
        Window::Internal {
            direction,
            children,
            ..
        } => {
            for (idx, child) in children.iter().enumerate() {
                if let Some((target_size, expand_capacity)) =
                    find_resize_caps(child, target, axis, min_size, char_width, char_height)
                {
                    if expand_capacity.is_some() {
                        return Some((target_size, expand_capacity));
                    }
                    if split_matches_axis(*direction, axis) {
                        let capacity = children
                            .iter()
                            .enumerate()
                            .filter(|(sibling_idx, _)| *sibling_idx != idx)
                            .map(|(_, sibling)| {
                                let sibling_size =
                                    axis_size_units(sibling, axis, char_width, char_height);
                                (sibling_size - min_size).max(0)
                            })
                            .sum::<i64>();
                        return Some((target_size, Some(capacity)));
                    }
                    return Some((target_size, None));
                }
            }
            None
        }
    }
}

fn window_preserved_size_key() -> Value {
    Value::symbol("window-preserved-size")
}

fn decode_preserved_size(raw: &Value) -> Result<(Value, Value), Flow> {
    if raw.is_nil() {
        return Ok((Value::Nil, Value::Nil));
    }
    let items = list_to_vec(raw).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), raw.clone()],
        )
    })?;
    let width = items.get(1).cloned().unwrap_or(Value::Nil);
    let height = items.get(2).cloned().unwrap_or(Value::Nil);
    Ok((width, height))
}

fn preserved_size_components(frames: &FrameManager, wid: WindowId) -> Result<(Value, Value), Flow> {
    let key = window_preserved_size_key();
    match frames.window_parameter(wid, &key) {
        Some(raw) => decode_preserved_size(&raw),
        None => Ok((Value::Nil, Value::Nil)),
    }
}

fn window_is_fixed_for_axis(
    frames: &FrameManager,
    wid: WindowId,
    horizontal: bool,
    ignore: bool,
) -> Result<bool, Flow> {
    if ignore {
        return Ok(false);
    }
    let (width, height) = preserved_size_components(frames, wid)?;
    Ok(if horizontal {
        !width.is_nil()
    } else {
        !height.is_nil()
    })
}

fn window_edges_cols_lines(w: &Window, char_width: f32, char_height: f32) -> (i64, i64, i64, i64) {
    let b = w.bounds();
    let left = if char_width > 0.0 {
        (b.x / char_width) as i64
    } else {
        0
    };
    let top = if char_height > 0.0 {
        (b.y / char_height) as i64
    } else {
        0
    };
    let right = if char_width > 0.0 {
        ((b.x + b.width) / char_width) as i64
    } else {
        0
    };
    let bottom = if char_height > 0.0 {
        ((b.y + b.height) / char_height) as i64
    } else {
        0
    };
    (left, top, right, bottom)
}

fn window_body_edges_cols_lines(
    frames: &FrameManager,
    fid: FrameId,
    wid: WindowId,
    w: &Window,
    char_width: f32,
    char_height: f32,
) -> (i64, i64, i64, i64) {
    let (left, top, right, bottom) = window_edges_cols_lines(w, char_width, char_height);
    let body_bottom = if is_minibuffer_window(frames, fid, wid) {
        bottom
    } else {
        bottom.saturating_sub(1)
    };
    (left, top, right, body_bottom)
}

// ===========================================================================
// Window queries
// ===========================================================================

/// `(selected-window)` -> window object.
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
    Ok(window_value(frame.selected_window))
}

/// `(old-selected-window)` -> previous selected window.
pub(crate) fn builtin_old_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("old-selected-window", &args, 0)?;
    let fid = ensure_selected_frame_id(eval);
    let selected_wid = eval
        .frames
        .get(fid)
        .map(|frame| frame.selected_window)
        .ok_or_else(|| signal("error", vec![Value::string("No selected frame")]))?;
    let old_wid = eval.frames.old_selected_window().unwrap_or(selected_wid);
    Ok(window_value(old_wid))
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
    Ok(window_value(frame.selected_window))
}

/// `(frame-old-selected-window &optional FRAME)` -> nil.
///
/// Batch GNU Emacs reports nil for this accessor throughout startup and
/// selection operations; keep frame designator validation aligned with
/// `frame-live-p` semantics.
pub(crate) fn builtin_frame_old_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-old-selected-window", &args, 1)?;
    let _ = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    Ok(Value::Nil)
}

/// `(set-frame-selected-window FRAME WINDOW &optional NORECORD)` -> WINDOW.
pub(crate) fn builtin_set_frame_selected_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-frame-selected-window", &args, 2)?;
    expect_max_args("set-frame-selected-window", &args, 3)?;
    let fid = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    let wid = match window_id_from_designator(&args[1]) {
        Some(wid) => {
            if eval.frames.find_window_frame_id(wid).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("window-live-p"), args[1].clone()],
                ));
            }
            wid
        }
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), args[1].clone()],
            ))
        }
    };
    let window_fid = eval
        .frames
        .find_window_frame_id(wid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    if window_fid != fid {
        return Err(signal(
            "error",
            vec![Value::string(
                "In `set-frame-selected-window', WINDOW is not on FRAME",
            )],
        ));
    }
    let selected_fid = ensure_selected_frame_id(eval);
    if fid == selected_fid {
        let mut select_args = vec![window_value(wid)];
        if let Some(norecord) = args.get(2) {
            select_args.push(norecord.clone());
        }
        return builtin_select_window(eval, select_args);
    }

    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    frame.selected_window = wid;
    Ok(window_value(wid))
}

/// `(frame-first-window &optional FRAME-OR-WINDOW)` -> first window on frame.
pub(crate) fn builtin_frame_first_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-first-window", &args, 1)?;
    let fid = resolve_frame_or_window_frame_id(eval, args.first(), "frame-live-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let first = frame
        .window_list()
        .first()
        .copied()
        .unwrap_or(frame.selected_window);
    Ok(window_value(first))
}

/// `(frame-root-window &optional FRAME-OR-WINDOW)` -> root window on frame.
///
/// NeoVM currently models leaf window IDs only; batch startup parity in our
/// corpus expects root and first windows to coincide.
pub(crate) fn builtin_frame_root_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-root-window", &args, 1)?;
    let fid = resolve_frame_or_window_frame_id(eval, args.first(), "frame-live-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let root = frame
        .window_list()
        .first()
        .copied()
        .unwrap_or(frame.selected_window);
    Ok(window_value(root))
}

/// `(frame-root-window-p WINDOW)` -> t if WINDOW is the root window of its frame.
pub(crate) fn builtin_frame_root_window_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("frame-root-window-p", &args, 1)?;
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-live-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let root = frame
        .window_list()
        .first()
        .copied()
        .unwrap_or(frame.selected_window);
    Ok(Value::bool(root == wid))
}

/// `(minibuffer-window &optional FRAME)` -> minibuffer window of FRAME.
pub(crate) fn builtin_minibuffer_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("minibuffer-window", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    match frame.minibuffer_window {
        Some(wid) => Ok(window_value(wid)),
        None => Ok(Value::Nil),
    }
}

/// `(window-minibuffer-p &optional WINDOW)` -> t when WINDOW is minibuffer.
pub(crate) fn builtin_window_minibuffer_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-minibuffer-p", &args, 1)?;
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let is_minibuffer = eval
        .frames
        .get(fid)
        .is_some_and(|frame| frame.minibuffer_window == Some(wid));
    Ok(Value::bool(is_minibuffer))
}

/// `(minibuffer-selected-window)` -> nil in batch (no active minibuffer).
pub(crate) fn builtin_minibuffer_selected_window(args: Vec<Value>) -> EvalResult {
    expect_args("minibuffer-selected-window", &args, 0)?;
    Ok(Value::Nil)
}

/// `(active-minibuffer-window)` -> nil in batch.
pub(crate) fn builtin_active_minibuffer_window(args: Vec<Value>) -> EvalResult {
    expect_args("active-minibuffer-window", &args, 0)?;
    Ok(Value::Nil)
}

/// `(minibuffer-window-active-p WINDOW)` -> nil in batch.
pub(crate) fn builtin_minibuffer_window_active_p(args: Vec<Value>) -> EvalResult {
    expect_args("minibuffer-window-active-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(window-frame &optional WINDOW)` -> frame of WINDOW.
pub(crate) fn builtin_window_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-frame", &args, 1)?;
    let (fid, _wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    Ok(Value::Frame(fid.0))
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
        Some(val) => {
            let Some(wid) = window_id_from_designator(val) else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("windowp"), val.clone()],
                ));
            };
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                return resolve_buffer(&eval.frames, fid, wid);
            }
            if eval.frames.is_window_object_id(wid) {
                return Ok(Value::Nil);
            }
            Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("windowp"), val.clone()],
            ))
        }
    }
}

/// `(window-display-table &optional WINDOW)` -> display table or nil.
pub(crate) fn builtin_window_display_table(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-display-table", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    Ok(eval.frames.window_display_table(wid))
}

/// `(set-window-display-table WINDOW TABLE)` -> TABLE.
pub(crate) fn builtin_set_window_display_table(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-display-table", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    let table = args[1].clone();
    eval.frames.set_window_display_table(wid, table.clone());
    Ok(table)
}

/// `(window-cursor-type &optional WINDOW)` -> cursor type object.
pub(crate) fn builtin_window_cursor_type(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-cursor-type", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    Ok(eval.frames.window_cursor_type(wid))
}

/// `(set-window-cursor-type WINDOW TYPE)` -> TYPE.
pub(crate) fn builtin_set_window_cursor_type(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-cursor-type", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    let cursor_type = args[1].clone();
    eval.frames.set_window_cursor_type(wid, cursor_type.clone());
    Ok(cursor_type)
}

/// `(window-size-fixed-p &optional WINDOW HORIZONTAL IGNORE)` -> t/nil.
pub(crate) fn builtin_window_size_fixed_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-size-fixed-p", &args, 3)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id_or_window_error(eval, args.first(), false)?;
    let horizontal = args.get(1).is_some_and(Value::is_truthy);
    let ignore = args.get(2).is_some_and(Value::is_truthy);
    Ok(Value::bool(window_is_fixed_for_axis(
        &eval.frames,
        wid,
        horizontal,
        ignore,
    )?))
}

/// `(window-preserve-size &optional WINDOW HORIZONTAL PRESERVE)` -> size tuple.
pub(crate) fn builtin_window_preserve_size(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-preserve-size", &args, 3)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_or_window_error(eval, args.first(), true)?;

    let horizontal = args.get(1).is_some_and(Value::is_truthy);
    let preserve = args.get(2).is_some_and(Value::is_truthy);
    let (mut width, mut height) = preserved_size_components(&eval.frames, wid)?;

    let window = get_leaf(&eval.frames, fid, wid)?;
    let buffer = Value::Buffer(window.buffer_id().unwrap_or(BufferId(0)));
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let current_width = Value::Int(window_width_cols(window, frame.char_width));
    let current_height = Value::Int(window_body_height_lines(&eval.frames, fid, wid, window));

    if horizontal {
        width = if preserve { current_width } else { Value::Nil };
    } else {
        height = if preserve { current_height } else { Value::Nil };
    }
    let preserved = Value::list(vec![buffer, width, height]);
    eval.frames
        .set_window_parameter(wid, window_preserved_size_key(), preserved.clone());
    Ok(preserved)
}

/// `(window-resizable WINDOW DELTA &optional HORIZONTAL IGNORE PIXELWISE)` -> number.
pub(crate) fn builtin_window_resizable(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("window-resizable", &args, 2)?;
    expect_max_args("window-resizable", &args, 5)?;
    let _ = ensure_selected_frame_id(eval);

    let (fid, wid) = resolve_window_id_or_window_error(eval, args.first(), false)?;
    let delta = expect_number_or_marker(&args[1])?;
    let horizontal = args.get(2).is_some_and(Value::is_truthy);
    let ignore = args.get(3).is_some_and(Value::is_truthy);
    let _pixelwise = args.get(4).is_some_and(Value::is_truthy);

    if window_is_fixed_for_axis(&eval.frames, wid, horizontal, ignore)? {
        return Ok(Value::Int(0));
    }

    let axis = resize_axis(horizontal);
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    if frame.minibuffer_window == Some(wid) {
        return Ok(Value::Int(0));
    }

    let min_size = match axis {
        ResizeAxis::Vertical => 4,
        ResizeAxis::Horizontal => 10,
    };
    let (target_size, expand_capacity) = find_resize_caps(
        &frame.root_window,
        wid,
        axis,
        min_size,
        frame.char_width,
        frame.char_height,
    )
    .unwrap_or((0, Some(0)));
    let max_expand = expand_capacity.unwrap_or(0).max(0) as f64;
    let max_shrink = (target_size - min_size).max(0) as f64;

    let result = if delta > 0.0 {
        delta.min(max_expand)
    } else if delta < 0.0 {
        delta.max(-max_shrink)
    } else {
        0.0
    };
    Ok(numeric_value(result))
}

/// `(window-parameter WINDOW PARAMETER)` -> window parameter or nil.
pub(crate) fn builtin_window_parameter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("window-parameter", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let wid = resolve_window_object_id_with_pred(eval, args.first(), "windowp")?;
    Ok(eval
        .frames
        .window_parameter(wid, &args[1])
        .unwrap_or(Value::Nil))
}

/// `(set-window-parameter WINDOW PARAMETER VALUE)` -> VALUE.
pub(crate) fn builtin_set_window_parameter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-parameter", &args, 3)?;
    let _ = ensure_selected_frame_id(eval);
    let wid = resolve_window_object_id_with_pred(eval, args.first(), "windowp")?;
    let value = args[2].clone();
    eval.frames
        .set_window_parameter(wid, args[1].clone(), value.clone());
    Ok(value)
}

/// `(window-parameters &optional WINDOW)` -> alist of parameters.
pub(crate) fn builtin_window_parameters(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-parameters", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    Ok(eval.frames.window_parameters_alist(wid))
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

/// `(window-group-start &optional WINDOW)` -> integer position.
///
/// Batch GNU Emacs exposes group-start as point-min (`1`) in startup flows.
pub(crate) fn builtin_window_group_start(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-group-start", &args, 1)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    if eval
        .frames
        .get(fid)
        .is_some_and(|frame| frame.minibuffer_window == Some(wid))
    {
        return Ok(Value::Int(1));
    }
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf { window_start, .. } => Ok(Value::Int(*window_start as i64)),
        _ => Ok(Value::Int(1)),
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
    let pos = parse_integer_or_marker_arg(&args[1])?;
    let is_minibuffer = eval
        .frames
        .get(fid)
        .is_some_and(|frame| frame.minibuffer_window == Some(wid));
    match pos {
        IntegerOrMarkerArg::Int(pos) => {
            if !is_minibuffer {
                if let Some(clamped) = clamped_window_position(eval, fid, wid, pos) {
                    if let Some(Window::Leaf { window_start, .. }) = eval
                        .frames
                        .get_mut(fid)
                        .and_then(|frame| frame.find_window_mut(wid))
                    {
                        *window_start = clamped;
                    }
                }
            }
            Ok(Value::Int(pos))
        }
        IntegerOrMarkerArg::Marker { raw, position } => {
            if !is_minibuffer {
                if let Some(pos) = position {
                    if let Some(clamped) = clamped_window_position(eval, fid, wid, pos) {
                        if let Some(Window::Leaf { window_start, .. }) = eval
                            .frames
                            .get_mut(fid)
                            .and_then(|frame| frame.find_window_mut(wid))
                        {
                            *window_start = clamped;
                        }
                    }
                }
            }
            Ok(raw)
        }
    }
}

/// `(set-window-group-start WINDOW POS &optional NOFORCE)` -> POS.
pub(crate) fn builtin_set_window_group_start(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-group-start", &args, 2)?;
    expect_max_args("set-window-group-start", &args, 3)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let pos = parse_integer_or_marker_arg(&args[1])?;
    let is_minibuffer = eval
        .frames
        .get(fid)
        .is_some_and(|frame| frame.minibuffer_window == Some(wid));
    match pos {
        IntegerOrMarkerArg::Int(pos) => {
            if !is_minibuffer {
                if let Some(clamped) = clamped_window_position(eval, fid, wid, pos) {
                    if let Some(Window::Leaf { window_start, .. }) = eval
                        .frames
                        .get_mut(fid)
                        .and_then(|frame| frame.find_window_mut(wid))
                    {
                        *window_start = clamped;
                    }
                }
            }
            Ok(Value::Int(pos))
        }
        IntegerOrMarkerArg::Marker { raw, position } => {
            if !is_minibuffer {
                if let Some(pos) = position {
                    if let Some(clamped) = clamped_window_position(eval, fid, wid, pos) {
                        if let Some(Window::Leaf {
                            window_start,
                            point,
                            ..
                        }) = eval
                            .frames
                            .get_mut(fid)
                            .and_then(|frame| frame.find_window_mut(wid))
                        {
                            *window_start = clamped;
                            *point = clamped;
                        }
                    }
                }
            }
            Ok(raw)
        }
    }
}

/// `(set-window-point WINDOW POS)` -> POS.
pub(crate) fn builtin_set_window_point(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-point", &args, 2)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let pos = parse_integer_or_marker_arg(&args[1])?;
    let is_minibuffer = eval
        .frames
        .get(fid)
        .is_some_and(|frame| frame.minibuffer_window == Some(wid));
    match pos {
        IntegerOrMarkerArg::Int(pos) => {
            if !is_minibuffer {
                if let Some(clamped) = clamped_window_position(eval, fid, wid, pos) {
                    if let Some(Window::Leaf { point, .. }) = eval
                        .frames
                        .get_mut(fid)
                        .and_then(|frame| frame.find_window_mut(wid))
                    {
                        *point = clamped;
                    }
                }
            }
            Ok(Value::Int(pos))
        }
        IntegerOrMarkerArg::Marker { raw, position } => {
            if is_minibuffer {
                return Ok(raw);
            }
            let pos = position.ok_or_else(|| {
                signal(
                    "error",
                    vec![Value::string("Marker does not point anywhere")],
                )
            })?;
            if let Some(clamped) = clamped_window_position(eval, fid, wid, pos) {
                if let Some(Window::Leaf { point, .. }) = eval
                    .frames
                    .get_mut(fid)
                    .and_then(|frame| frame.find_window_mut(wid))
                {
                    *point = clamped;
                }
                Ok(Value::Int(clamped as i64))
            } else {
                Ok(Value::Int(1))
            }
        }
    }
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

/// `(window-use-time &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_use_time(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-use-time", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    Ok(Value::Int(eval.frames.window_use_time(wid)))
}

/// `(window-bump-use-time &optional WINDOW)` -> integer or nil.
pub(crate) fn builtin_window_bump_use_time(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-bump-use-time", &args, 1)?;
    let selected_fid = ensure_selected_frame_id(eval);
    let selected_wid = eval
        .frames
        .get(selected_fid)
        .map(|frame| frame.selected_window)
        .ok_or_else(|| signal("error", vec![Value::string("No selected frame")]))?;
    let target_wid = match args.first() {
        None | Some(Value::Nil) => selected_wid,
        Some(Value::Window(id)) => {
            let wid = WindowId(*id);
            if eval.frames.find_window_frame_id(wid).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("window-live-p"), Value::Window(*id)],
                ));
            }
            wid
        }
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), other.clone()],
            ))
        }
    };
    Ok(
        match eval.frames.bump_window_use_time(selected_wid, target_wid) {
            Some(use_time) => Value::Int(use_time),
            None => Value::Nil,
        },
    )
}

/// `(window-old-point &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_old_point(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-old-point", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf { point, .. } => Ok(Value::Int((*point).max(1) as i64)),
        _ => Ok(Value::Int(1)),
    }
}

/// `(window-old-buffer &optional WINDOW)` -> nil in batch.
pub(crate) fn builtin_window_old_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-old-buffer", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    Ok(Value::Nil)
}

/// `(window-prev-buffers &optional WINDOW)` -> previous buffer list or nil.
pub(crate) fn builtin_window_prev_buffers(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-prev-buffers", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    Ok(eval.frames.window_prev_buffers(wid))
}

/// `(window-next-buffers &optional WINDOW)` -> next buffer list or nil.
pub(crate) fn builtin_window_next_buffers(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-next-buffers", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    Ok(eval.frames.window_next_buffers(wid))
}

/// `(set-window-prev-buffers WINDOW PREV-BUFFERS)` -> PREV-BUFFERS.
pub(crate) fn builtin_set_window_prev_buffers(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-prev-buffers", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    let value = args[1].clone();
    eval.frames.set_window_prev_buffers(wid, value.clone());
    Ok(value)
}

/// `(set-window-next-buffers WINDOW NEXT-BUFFERS)` -> NEXT-BUFFERS.
pub(crate) fn builtin_set_window_next_buffers(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-next-buffers", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, wid) = resolve_window_id(eval, args.first())?;
    let value = args[1].clone();
    eval.frames.set_window_next_buffers(wid, value.clone());
    Ok(value)
}

/// `(window-left-column &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_left_column(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-left-column", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let cw = eval.frames.get(fid).map(|f| f.char_width).unwrap_or(8.0);
    let left = if cw > 0.0 {
        (w.bounds().x / cw) as i64
    } else {
        0
    };
    Ok(Value::Int(left))
}

/// `(window-top-line &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_top_line(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-top-line", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let ch = eval.frames.get(fid).map(|f| f.char_height).unwrap_or(16.0);
    let top = if ch > 0.0 {
        (w.bounds().y / ch) as i64
    } else {
        0
    };
    Ok(Value::Int(top))
}

/// `(window-hscroll &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_hscroll(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-hscroll", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    match w {
        Window::Leaf { hscroll, .. } => Ok(Value::Int(*hscroll as i64)),
        _ => Ok(Value::Int(0)),
    }
}

/// `(set-window-hscroll WINDOW NCOLS)` -> integer.
pub(crate) fn builtin_set_window_hscroll(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-window-hscroll", &args, 2)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let cols = expect_fixnum(&args[1])?.max(0) as usize;
    if let Some(Window::Leaf { hscroll, .. }) = eval
        .frames
        .get_mut(fid)
        .and_then(|frame| frame.find_window_mut(wid))
    {
        *hscroll = cols;
    }
    Ok(Value::Int(cols as i64))
}

fn scroll_prefix_value(value: &Value) -> i64 {
    match value {
        Value::Int(n) => *n,
        Value::Float(f) => *f as i64,
        Value::Char(c) => *c as i64,
        Value::Symbol(s) if s == "-" => -1,
        Value::Cons(cell) => {
            let car = {
                let pair = cell.lock().expect("poisoned");
                pair.car.clone()
            };
            match car {
                Value::Int(n) => n,
                Value::Float(f) => f as i64,
                Value::Char(c) => c as i64,
                _ => 1,
            }
        }
        _ => 1,
    }
}

fn default_scroll_columns(eval: &super::eval::Evaluator, fid: FrameId, wid: WindowId) -> i64 {
    let char_width = eval.frames.get(fid).map(|f| f.char_width).unwrap_or(8.0);
    let window_cols = get_leaf(&eval.frames, fid, wid)
        .ok()
        .map(|leaf| {
            if char_width > 0.0 {
                (leaf.bounds().width / char_width).floor() as i64
            } else {
                80
            }
        })
        .unwrap_or(80);
    (window_cols - 2).max(1)
}

/// `(scroll-left &optional SET-MINIMUM ARG)` -> new horizontal scroll amount.
pub(crate) fn builtin_scroll_left(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("scroll-left", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, None)?;
    let base = eval
        .frames
        .get(fid)
        .and_then(|frame| frame.find_window(wid))
        .and_then(|window| match window {
            Window::Leaf { hscroll, .. } => Some(*hscroll as i64),
            _ => None,
        })
        .unwrap_or(0);
    let delta = match args.first() {
        None | Some(Value::Nil) => default_scroll_columns(eval, fid, wid),
        Some(value) => scroll_prefix_value(value),
    };
    let mut next = base as i128 + delta as i128;
    if next < 0 {
        next = 0;
    }
    let next = next.min(i64::MAX as i128) as i64;
    if let Some(Window::Leaf { hscroll, .. }) = eval
        .frames
        .get_mut(fid)
        .and_then(|frame| frame.find_window_mut(wid))
    {
        *hscroll = next as usize;
    }
    Ok(Value::Int(next))
}

/// `(scroll-right &optional SET-MINIMUM ARG)` -> new horizontal scroll amount.
pub(crate) fn builtin_scroll_right(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("scroll-right", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, None)?;
    let base = eval
        .frames
        .get(fid)
        .and_then(|frame| frame.find_window(wid))
        .and_then(|window| match window {
            Window::Leaf { hscroll, .. } => Some(*hscroll as i64),
            _ => None,
        })
        .unwrap_or(0);
    let delta = match args.first() {
        None | Some(Value::Nil) => default_scroll_columns(eval, fid, wid),
        Some(value) => scroll_prefix_value(value),
    };
    let mut next = base as i128 - delta as i128;
    if next < 0 {
        next = 0;
    }
    let next = next.min(i64::MAX as i128) as i64;
    if let Some(Window::Leaf { hscroll, .. }) = eval
        .frames
        .get_mut(fid)
        .and_then(|frame| frame.find_window_mut(wid))
    {
        *hscroll = next as usize;
    }
    Ok(Value::Int(next))
}

/// `(window-vscroll &optional WINDOW PIXELWISE)` -> integer.
///
/// Batch-mode GNU Emacs reports zero vertical scroll, including for minibuffer
/// windows; `PIXELWISE` is accepted but ignored.
pub(crate) fn builtin_window_vscroll(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-vscroll", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    Ok(Value::Int(0))
}

/// `(set-window-vscroll WINDOW VSCROLL &optional PIXELWISE PRESERVE)` -> integer.
///
/// We currently model batch semantics where visible vertical scrolling remains
/// zero; argument validation follows GNU Emacs (`WINDOW` live predicate and
/// `VSCROLL` as `numberp`).
pub(crate) fn builtin_set_window_vscroll(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-vscroll", &args, 2)?;
    expect_max_args("set-window-vscroll", &args, 4)?;
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    match &args[1] {
        Value::Int(_) | Value::Float(_) | Value::Char(_) => Ok(Value::Int(0)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

/// `(set-window-margins WINDOW LEFT-WIDTH &optional RIGHT-WIDTH)` -> changed-p.
pub(crate) fn builtin_set_window_margins(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-margins", &args, 2)?;
    expect_max_args("set-window-margins", &args, 3)?;
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let left = expect_margin_width(&args[1])?;
    let right = if let Some(arg) = args.get(2) {
        expect_margin_width(arg)?
    } else {
        0
    };

    if let Some(Window::Leaf { margins, .. }) = eval
        .frames
        .get_mut(fid)
        .and_then(|frame| frame.find_window_mut(wid))
    {
        let next = (left, right);
        if *margins != next {
            *margins = next;
            return Ok(Value::True);
        }
    }
    Ok(Value::Nil)
}

/// `(window-margins &optional WINDOW)` -> margins pair or nil.
pub(crate) fn builtin_window_margins(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-margins", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let (left, right) = match w {
        Window::Leaf { margins, .. } => *margins,
        _ => (0, 0),
    };
    let left_v = if left == 0 {
        Value::Nil
    } else {
        Value::Int(left as i64)
    };
    let right_v = if right == 0 {
        Value::Nil
    } else {
        Value::Int(right as i64)
    };
    Ok(Value::cons(left_v, right_v))
}

/// `(window-fringes &optional WINDOW)` -> fringe tuple.
pub(crate) fn builtin_window_fringes(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-fringes", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    // Batch GNU Emacs startup reports zero-width fringes.
    Ok(Value::list(vec![
        Value::Int(0),
        Value::Int(0),
        Value::Nil,
        Value::Nil,
    ]))
}

/// `(set-window-fringes WINDOW LEFT &optional RIGHT OUTSIDE-MARGINS PERSISTENT)` -> nil.
pub(crate) fn builtin_set_window_fringes(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-fringes", &args, 2)?;
    expect_max_args("set-window-fringes", &args, 5)?;
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    Ok(Value::Nil)
}

/// `(window-scroll-bars &optional WINDOW)` -> scroll-bar tuple.
pub(crate) fn builtin_window_scroll_bars(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-scroll-bars", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    // Batch GNU Emacs startup reports no scroll-bars with default sizing payload.
    Ok(Value::list(vec![
        Value::Nil,
        Value::Int(0),
        Value::True,
        Value::Nil,
        Value::Int(0),
        Value::True,
        Value::Nil,
    ]))
}

/// `(set-window-scroll-bars WINDOW &optional WIDTH VERTICAL-TYPE HEIGHT HORIZONTAL-TYPE)` -> nil.
pub(crate) fn builtin_set_window_scroll_bars(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-window-scroll-bars", &args, 1)?;
    expect_max_args("set-window-scroll-bars", &args, 6)?;
    let (_fid, _wid) = resolve_window_id(eval, args.first())?;
    Ok(Value::Nil)
}

/// `(window-mode-line-height &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_mode_line_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-mode-line-height", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let height = if is_minibuffer_window(&eval.frames, fid, wid) {
        0
    } else {
        1
    };
    Ok(Value::Int(height))
}

/// `(window-header-line-height &optional WINDOW)` -> integer.
pub(crate) fn builtin_window_header_line_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-header-line-height", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let _ = resolve_window_id(eval, args.first())?;
    Ok(Value::Int(0))
}

/// `(window-pixel-height &optional WINDOW)` -> integer.
///
/// Batch GNU Emacs reports character-line units for this query.
pub(crate) fn builtin_window_pixel_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-pixel-height", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let ch = eval.frames.get(fid).map(|f| f.char_height).unwrap_or(16.0);
    Ok(Value::Int(window_height_lines(w, ch)))
}

/// `(window-pixel-width &optional WINDOW)` -> integer.
///
/// Batch GNU Emacs reports character-column units for this query.
pub(crate) fn builtin_window_pixel_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-pixel-width", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
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
    // The body area excludes one mode-line row for regular windows, but
    // minibuffer windows report their full single-line height.
    let body_lines = window_body_height_lines(&eval.frames, fid, wid, w);
    Ok(Value::Int(body_lines))
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

/// `(window-text-height &optional WINDOW PIXELWISE)` -> integer.
pub(crate) fn builtin_window_text_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-text-height", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let _pixelwise = args.get(1);
    Ok(Value::Int(window_body_height_lines(
        &eval.frames,
        fid,
        wid,
        w,
    )))
}

/// `(window-text-width &optional WINDOW PIXELWISE)` -> integer.
pub(crate) fn builtin_window_text_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-text-width", &args, 2)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id(eval, args.first())?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let _pixelwise = args.get(1);
    let cw = eval.frames.get(fid).map(|f| f.char_width).unwrap_or(8.0);
    Ok(Value::Int(window_width_cols(w, cw)))
}

/// `(window-body-pixel-edges &optional WINDOW)` -> (LEFT TOP RIGHT BOTTOM).
pub(crate) fn builtin_window_body_pixel_edges(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-body-pixel-edges", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_or_window_error(eval, args.first(), true)?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let (left, top, right, bottom) = window_body_edges_cols_lines(
        &eval.frames,
        fid,
        wid,
        w,
        frame.char_width,
        frame.char_height,
    );
    Ok(Value::list(vec![
        Value::Int(left),
        Value::Int(top),
        Value::Int(right),
        Value::Int(bottom),
    ]))
}

/// `(window-body-edges &optional WINDOW)` -> (LEFT TOP RIGHT BOTTOM).
pub(crate) fn builtin_window_body_edges(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-body-edges", &args, 1)?;
    builtin_window_body_pixel_edges(eval, args)
}

/// `(window-pixel-edges &optional WINDOW)` -> (LEFT TOP RIGHT BOTTOM).
pub(crate) fn builtin_window_pixel_edges(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-pixel-edges", &args, 1)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, wid) = resolve_window_id_or_window_error(eval, args.first(), false)?;
    let w = get_leaf(&eval.frames, fid, wid)?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let (left, top, right, bottom) =
        window_edges_cols_lines(w, frame.char_width, frame.char_height);
    Ok(Value::list(vec![
        Value::Int(left),
        Value::Int(top),
        Value::Int(right),
        Value::Int(bottom),
    ]))
}

/// `(window-edges &optional WINDOW BODY ABSOLUTE)`.
///
/// GNU Emacs currently reports max arity 4; trailing args are accepted.
pub(crate) fn builtin_window_edges(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-edges", &args, 4)?;
    let _ = ensure_selected_frame_id(eval);
    let body = args.get(1).is_some_and(Value::is_truthy);
    if body {
        return builtin_window_body_edges(eval, vec![args.first().cloned().unwrap_or(Value::Nil)]);
    }
    builtin_window_pixel_edges(eval, vec![args.first().cloned().unwrap_or(Value::Nil)])
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

/// `(window-list &optional FRAME MINIBUF ALL-FRAMES)` -> list of window objects.
pub(crate) fn builtin_window_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-list", &args, 3)?;
    let selected_fid = ensure_selected_frame_id(eval);
    // GNU Emacs validates ALL-FRAMES before FRAME mismatch checks.
    let all_frames_fid = match args.get(2) {
        None | Some(Value::Nil) => None,
        Some(arg) => {
            let Some(wid) = window_id_from_designator(arg) else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("windowp"), arg.clone()],
                ));
            };
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                Some(fid)
            } else if eval.frames.is_window_object_id(wid) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("window-live-p"), arg.clone()],
                ));
            } else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("windowp"), arg.clone()],
                ));
            }
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
        Some(Value::Frame(id)) => {
            let fid = FrameId(*id);
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
    let mut ids: Vec<Value> = frame.window_list().into_iter().map(window_value).collect();
    if include_minibuffer {
        if let Some(minibuffer_wid) = frame.minibuffer_window {
            ids.push(window_value(minibuffer_wid));
        }
    }
    Ok(Value::list(ids))
}

/// `(window-list-1 &optional WINDOW MINIBUF ALL-FRAMES)` -> list of live windows.
pub(crate) fn builtin_window_list_1(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-list-1", &args, 3)?;
    let _ = ensure_selected_frame_id(eval);
    let (fid, start_wid) = match args.first() {
        None | Some(Value::Nil) => resolve_window_id_with_pred(eval, None, "window-live-p")?,
        Some(Value::Window(id)) => {
            let wid = WindowId(*id);
            if let Some(fid) = eval.frames.find_window_frame_id(wid) {
                (fid, wid)
            } else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("window-live-p"), args[0].clone()],
                ));
            }
        }
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), other.clone()],
            ))
        }
    };

    // ALL-FRAMES matches GNU Emacs: nil/default => WINDOW's frame; t => all
    // frames; 'visible and 0 => visible/iconified frames (we only model
    // visibility); a frame object => that frame; anything else => WINDOW's frame.
    let mut frame_ids: Vec<FrameId> = match args.get(2) {
        None | Some(Value::Nil) => vec![fid],
        Some(Value::True) => {
            let mut ids = eval.frames.frame_list();
            ids.sort_by_key(|f| f.0);
            ids
        }
        Some(Value::Symbol(sym)) if sym == "visible" => {
            let mut ids = eval.frames.frame_list();
            ids.sort_by_key(|f| f.0);
            ids.into_iter()
                .filter(|frame_id| {
                    eval.frames
                        .get(*frame_id)
                        .is_some_and(|frame| frame.visible)
                })
                .collect()
        }
        Some(Value::Int(0)) => {
            let mut ids = eval.frames.frame_list();
            ids.sort_by_key(|f| f.0);
            ids.into_iter()
                .filter(|frame_id| {
                    eval.frames
                        .get(*frame_id)
                        .is_some_and(|frame| frame.visible)
                })
                .collect()
        }
        Some(Value::Frame(frame_raw_id)) => {
            let frame_id = FrameId(*frame_raw_id);
            if eval.frames.get(frame_id).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), args[2].clone()],
                ));
            }
            vec![frame_id]
        }
        Some(_) => vec![fid],
    };
    if frame_ids.is_empty() {
        frame_ids.push(fid);
    }

    if let Some(start_pos) = frame_ids.iter().position(|frame_id| *frame_id == fid) {
        frame_ids.rotate_left(start_pos);
    }

    let include_minibuffer = matches!(args.get(1), Some(Value::True));
    let mut seen_window_ids: HashSet<u64> = HashSet::new();
    let mut windows: Vec<Value> = Vec::new();

    for frame_id in frame_ids {
        let Some(frame) = eval.frames.get(frame_id) else {
            continue;
        };

        // GNU Emacs starts traversal at WINDOW when it appears in the returned list.
        let mut window_ids = frame.window_list();
        if frame_id == fid {
            if let Some(start_index) = window_ids.iter().position(|wid| *wid == start_wid) {
                window_ids.rotate_left(start_index);
            }
        }

        for window_id in window_ids {
            if seen_window_ids.insert(window_id.0) {
                windows.push(window_value(window_id));
            }
        }

        if include_minibuffer {
            if let Some(minibuffer_wid) = frame.minibuffer_window {
                if seen_window_ids.insert(minibuffer_wid.0) {
                    windows.push(window_value(minibuffer_wid));
                }
            }
        }
    }

    Ok(Value::list(windows))
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
            return Ok(window_value(wid));
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
            windows.push(window_value(wid));
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
        Some(value) => {
            let Some(wid) = window_id_from_designator(value) else {
                return Err(signal("error", vec![Value::string("Invalid window")]));
            };
            if eval.frames.find_window_frame_id(wid).is_some() {
                wid
            } else {
                return Err(signal("error", vec![Value::string("Invalid window")]));
            }
        }
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

/// `(windowp OBJ)` -> t if OBJ is a window object/designator that exists.
pub(crate) fn builtin_windowp(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("windowp", &args, 1)?;
    let wid = match window_id_from_designator(&args[0]) {
        Some(wid) => wid,
        None => return Ok(Value::Nil),
    };
    let found = eval.frames.is_window_object_id(wid);
    Ok(Value::bool(found))
}

/// `(window-valid-p OBJ)` -> t if OBJ is a live window.
pub(crate) fn builtin_window_valid_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("window-valid-p", &args, 1)?;
    let wid = match window_id_from_designator(&args[0]) {
        Some(wid) => wid,
        None => return Ok(Value::Nil),
    };
    Ok(Value::bool(eval.frames.is_live_window_id(wid)))
}

/// `(window-live-p OBJ)` -> t if OBJ is a live leaf window.
pub(crate) fn builtin_window_live_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("window-live-p", &args, 1)?;
    let wid = match window_id_from_designator(&args[0]) {
        Some(wid) => wid,
        None => return Ok(Value::Nil),
    };
    let live = eval.frames.is_live_window_id(wid);
    Ok(Value::bool(live))
}

/// `(window-at X Y &optional FRAME)` -> window object or nil.
pub(crate) fn builtin_window_at(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("window-at", &args, 2)?;
    expect_max_args("window-at", &args, 3)?;
    let x = expect_number(&args[0])?;
    let y = expect_number(&args[1])?;
    let fid = resolve_frame_id(eval, args.get(2), "frame-live-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let total_cols = frame_total_cols(frame) as f64;
    let total_lines = frame_total_lines(frame) as f64;
    if x < 0.0 || y < 0.0 || x >= total_cols || y >= total_lines {
        return Ok(Value::Nil);
    }

    let px = (x * frame.char_width as f64) as f32;
    let py = (y * frame.char_height as f64) as f32;
    if let Some(wid) = frame.window_at(px, py) {
        return Ok(window_value(wid));
    }

    if let (Some(minibuffer_wid), Some(minibuffer_leaf)) =
        (frame.minibuffer_window, frame.minibuffer_leaf.as_ref())
    {
        if minibuffer_leaf.bounds().contains(px, py) {
            return Ok(window_value(minibuffer_wid));
        }
    }

    Ok(Value::Nil)
}

// ===========================================================================
// Window manipulation
// ===========================================================================

/// `(split-window &optional WINDOW SIZE SIDE)` -> new window object.
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
    Ok(window_value(new_wid))
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

/// `(delete-window-internal WINDOW)` -> nil.
///
/// GNU Emacs exposes this primitive for low-level window internals. For the
/// compatibility surface we mirror the observable error behavior used by the
/// vm-compat coverage corpus.
pub(crate) fn builtin_delete_window_internal(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-window-internal", &args, 1)?;

    let (fid, wid) = if args[0].is_nil() {
        resolve_window_id(eval, None)?
    } else {
        resolve_window_id_with_pred(eval, args.first(), "windowp")?
    };

    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    let is_minibuffer = frame.minibuffer_window == Some(wid);
    let is_sole_ordinary_window = frame.window_list().len() <= 1;

    if is_minibuffer || is_sole_ordinary_window {
        return Err(signal(
            "error",
            vec![Value::string(
                "Attempt to delete minibuffer or sole ordinary window",
            )],
        ));
    }

    Err(signal("error", vec![Value::string("Deletion failed")]))
}

/// `(delete-other-windows-internal &optional WINDOW ALL-FRAMES)` -> nil.
///
/// Deletes all ordinary windows in FRAME except WINDOW. ALL-FRAMES is accepted
/// for arity compatibility and currently ignored.
pub(crate) fn builtin_delete_other_windows_internal(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("delete-other-windows-internal", &args, 2)?;
    let (fid, keep_wid) = resolve_window_id_with_pred(eval, args.first(), "window-valid-p")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;

    let all_ids: Vec<WindowId> = frame.window_list();
    let to_delete: Vec<WindowId> = all_ids.into_iter().filter(|&w| w != keep_wid).collect();

    for wid in to_delete {
        let _ = eval.frames.delete_window(fid, wid);
    }
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
    let wid = match args.first().and_then(window_id_from_designator) {
        Some(wid) => wid,
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), args[0].clone()],
            ))
        }
    };
    let record_selection = args.get(1).is_none_or(Value::is_nil);
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
    if record_selection {
        let _ = eval.frames.note_window_selected(wid);
    }
    if let Some(buffer_id) = selected_buffer {
        eval.buffers.set_current(buffer_id);
    }
    Ok(window_value(wid))
}

/// `(other-window COUNT &optional ALL-FRAMES)` -> nil.
///
/// Select another window in cyclic order.
pub(crate) fn builtin_other_window(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("other-window", &args, 1)?;
    expect_max_args("other-window", &args, 3)?;
    let count = expect_number_or_marker_count(&args[0])?;
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
    let (selected_buffer, switched) = if let Some(frame) = eval.frames.get_mut(fid) {
        let switched = frame.select_window(new_wid);
        (
            frame.find_window(new_wid).and_then(|w| w.buffer_id()),
            switched,
        )
    } else {
        (None, false)
    };
    if switched {
        let _ = eval.frames.note_window_selected(new_wid);
    };
    if let Some(buffer_id) = selected_buffer {
        eval.buffers.set_current(buffer_id);
    }
    Ok(Value::Nil)
}

/// `(one-window-p &optional NOMINI ALL-FRAMES)` -> non-nil iff only one window.
///
/// Batch-focused compatibility implementation: we currently count ordinary
/// windows on the selected frame and ignore NOMINI/ALL-FRAMES semantics.
pub(crate) fn builtin_one_window_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("one-window-p", &args, 2)?;
    let Some(fid) = eval.frames.selected_frame().map(|frame| frame.id) else {
        return Ok(Value::Nil);
    };
    let Some(frame) = eval.frames.get(fid) else {
        return Ok(Value::Nil);
    };
    Ok(Value::bool(frame.window_list().len() <= 1))
}

/// `(other-window-for-scrolling)` -> window object used for scrolling.
pub(crate) fn builtin_other_window_for_scrolling(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("other-window-for-scrolling", &args, 0)?;
    let fid = ensure_selected_frame_id(eval);
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("No selected frame")]))?;
    let windows = frame.window_list();
    if windows.len() <= 1 {
        return Err(signal(
            "error",
            vec![Value::string("There is no other window")],
        ));
    }
    let selected = frame.selected_window;
    let other = windows
        .into_iter()
        .find(|wid| *wid != selected)
        .unwrap_or(selected);
    Ok(window_value(other))
}

/// `(next-window &optional WINDOW MINIBUF ALL-FRAMES)` -> window object.
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
    Ok(window_value(list[next]))
}

/// `(previous-window &optional WINDOW MINIBUF ALL-FRAMES)` -> window object.
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
    Ok(window_value(list[prev]))
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

    let keep_margins = args.get(2).is_some_and(|arg| !arg.is_nil());
    let target_point = eval
        .buffers
        .get(buf_id)
        .map(|buf| buf.point_char().saturating_add(1))
        .unwrap_or(1)
        .max(1);

    let mut old_state = None;
    if let Some(Window::Leaf {
        buffer_id,
        window_start,
        point,
        ..
    }) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(wid))
    {
        old_state = Some((*buffer_id, *window_start, *point));
    }
    if let Some((old_buffer_id, old_window_start, old_point)) = old_state {
        eval.frames
            .set_window_buffer_position(wid, old_buffer_id, old_window_start, old_point);
        if old_buffer_id != buf_id {
            let prev_raw = eval.frames.window_prev_buffers(wid);
            let prev_entries = list_to_vec(&prev_raw).ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), prev_raw.clone()],
                )
            })?;
            let old_buffer_value = Value::Buffer(old_buffer_id);
            let marker_buffer_name = eval.buffers.get(old_buffer_id).map(|buf| buf.name.clone());
            let old_window_start_pos = old_window_start.max(1) as i64;
            let old_point_pos = old_point.max(1) as i64;
            let history_entry = Value::list(vec![
                old_buffer_value.clone(),
                super::marker::make_marker_value(
                    marker_buffer_name.as_deref(),
                    Some(old_window_start_pos),
                    false,
                ),
                super::marker::make_marker_value(
                    marker_buffer_name.as_deref(),
                    Some(old_point_pos),
                    false,
                ),
            ]);
            let filtered_prev = prev_entries
                .into_iter()
                .filter(|entry| {
                    let Some(items) = list_to_vec(entry) else {
                        return true;
                    };
                    !matches!(items.first(), Some(first) if *first == old_buffer_value)
                })
                .collect::<Vec<_>>();
            let mut next_prev = Vec::with_capacity(filtered_prev.len() + 1);
            next_prev.push(history_entry);
            next_prev.extend(filtered_prev);
            eval.frames
                .set_window_prev_buffers(wid, Value::list(next_prev));
            eval.frames.set_window_next_buffers(wid, Value::Nil);
        }
    }

    let (next_window_start, next_point) = eval
        .frames
        .window_buffer_position(wid, buf_id)
        .unwrap_or((1, target_point));
    if let Some(Window::Leaf {
        buffer_id,
        window_start,
        point,
        margins,
        ..
    }) = eval
        .frames
        .get_mut(fid)
        .and_then(|f| f.find_window_mut(wid))
    {
        *buffer_id = buf_id;
        *window_start = next_window_start.max(1);
        *point = next_point.max(1);
        if !keep_margins {
            *margins = (0, 0);
        }
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

/// `(display-buffer BUFFER-OR-NAME &optional ACTION FRAME)` -> window object or nil.
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
    Ok(window_value(sel_wid))
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

const MIN_FRAME_COLS: i64 = 10;
const MIN_FRAME_TEXT_LINES: i64 = 5;
const FRAME_TEXT_LINES_PARAM: &str = "neovm--frame-text-lines";

fn frame_total_cols(frame: &crate::window::Frame) -> i64 {
    frame
        .parameters
        .get("width")
        .and_then(Value::as_int)
        .unwrap_or(frame.columns() as i64)
}

fn frame_total_lines(frame: &crate::window::Frame) -> i64 {
    frame
        .parameters
        .get("height")
        .and_then(Value::as_int)
        .unwrap_or(frame.lines() as i64)
}

fn frame_text_lines(frame: &crate::window::Frame) -> i64 {
    frame
        .parameters
        .get(FRAME_TEXT_LINES_PARAM)
        .and_then(Value::as_int)
        .unwrap_or_else(|| frame_total_lines(frame))
}

fn clamp_frame_dimension(value: i64, minimum: i64) -> i64 {
    value.max(minimum).min(u32::MAX as i64)
}

fn set_frame_text_size(frame: &mut crate::window::Frame, cols: i64, text_lines: i64) {
    let cols = clamp_frame_dimension(cols, MIN_FRAME_COLS);
    let text_lines = clamp_frame_dimension(text_lines, MIN_FRAME_TEXT_LINES);
    let total_lines = text_lines.saturating_add(1).min(u32::MAX as i64);

    frame.width = cols as u32;
    frame.height = total_lines as u32;
    frame
        .parameters
        .insert("width".to_string(), Value::Int(cols));
    frame
        .parameters
        .insert("height".to_string(), Value::Int(total_lines));
    frame
        .parameters
        .insert(FRAME_TEXT_LINES_PARAM.to_string(), Value::Int(text_lines));
}

// ===========================================================================
// Scroll / frame visibility command shims
// ===========================================================================

fn recenter_missing_display_error() -> Flow {
    signal(
        "error",
        vec![Value::string(
            "‘recenter’ing a window that does not display current-buffer.",
        )],
    )
}

fn scroll_up_batch_error() -> Flow {
    signal("end-of-buffer", vec![])
}

fn scroll_down_batch_error() -> Flow {
    signal("beginning-of-buffer", vec![])
}

/// `(scroll-up-command &optional ARG)` -> signal `end-of-buffer` in batch mode.
pub(crate) fn builtin_scroll_up_command(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("scroll-up-command", &args, 1)?;
    Err(scroll_up_batch_error())
}

/// `(scroll-down-command &optional ARG)` -> signal `beginning-of-buffer` in batch mode.
pub(crate) fn builtin_scroll_down_command(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("scroll-down-command", &args, 1)?;
    Err(scroll_down_batch_error())
}

/// `(scroll-up &optional ARG)` -> command alias behavior.
pub(crate) fn builtin_scroll_up(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("scroll-up", &args, 1)?;
    Err(scroll_up_batch_error())
}

/// `(scroll-down &optional ARG)` -> command alias behavior.
pub(crate) fn builtin_scroll_down(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("scroll-down", &args, 1)?;
    Err(scroll_down_batch_error())
}

/// `(recenter-top-bottom &optional ARG)` -> signal no-window error in batch mode.
pub(crate) fn builtin_recenter_top_bottom(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("recenter-top-bottom", &args, 1)?;
    Err(recenter_missing_display_error())
}

/// `(recenter &optional ARG REDISPLAY)` -> signal no-window error in batch mode.
pub(crate) fn builtin_recenter(_eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("recenter", &args, 2)?;
    Err(recenter_missing_display_error())
}

/// `(iconify-frame &optional FRAME)` -> nil.
pub(crate) fn builtin_iconify_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("iconify-frame", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    frame.visible = false;
    Ok(Value::Nil)
}

/// `(make-frame-visible &optional FRAME)` -> frame.
pub(crate) fn builtin_make_frame_visible(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("make-frame-visible", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    frame.visible = true;
    Ok(Value::Frame(frame.id.0))
}

// ===========================================================================
// Frame operations
// ===========================================================================

/// `(selected-frame)` -> frame object.
pub(crate) fn builtin_selected_frame(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("selected-frame", &args, 0)?;
    let fid = ensure_selected_frame_id(eval);
    Ok(Value::Frame(fid.0))
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
        Value::Frame(id) => {
            let fid = FrameId(*id);
            if eval.frames.get(fid).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), Value::Frame(*id)],
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
    if args.get(1).is_none_or(Value::is_nil) {
        if let Some(selected_wid) = eval.frames.get(fid).map(|f| f.selected_window) {
            let _ = eval.frames.note_window_selected(selected_wid);
        }
    }
    if let Some(buf_id) = eval
        .frames
        .get(fid)
        .and_then(|f| f.find_window(f.selected_window))
        .and_then(|w| w.buffer_id())
    {
        eval.buffers.set_current(buf_id);
    }
    Ok(Value::Frame(fid.0))
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
        Value::Frame(id) => {
            let fid = FrameId(*id);
            if eval.frames.get(fid).is_none() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), Value::Frame(*id)],
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
    if args.get(1).is_none_or(Value::is_nil) {
        if let Some(selected_wid) = eval.frames.get(fid).map(|f| f.selected_window) {
            let _ = eval.frames.note_window_selected(selected_wid);
        }
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

/// `(frame-list)` -> list of frame objects.
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
        .map(|fid| Value::Frame(fid.0))
        .collect();
    Ok(Value::list(ids))
}

/// `(visible-frame-list)` -> list of visible frame objects.
pub(crate) fn builtin_visible_frame_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("visible-frame-list", &args, 0)?;
    let _ = ensure_selected_frame_id(eval);
    let mut frame_ids = eval.frames.frame_list();
    frame_ids.sort_by_key(|fid| fid.0);
    let visible = frame_ids
        .into_iter()
        .filter(|fid| eval.frames.get(*fid).is_some_and(|frame| frame.visible))
        .map(|fid| Value::Frame(fid.0))
        .collect::<Vec<_>>();
    Ok(Value::list(visible))
}

/// `(frame-char-height &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_char_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-char-height", &args, 1)?;
    let _ = resolve_frame_id(eval, args.first(), "framep")?;
    Ok(Value::Int(1))
}

/// `(frame-char-width &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_char_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-char-width", &args, 1)?;
    let _ = resolve_frame_id(eval, args.first(), "framep")?;
    Ok(Value::Int(1))
}

/// `(frame-native-height &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_native_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-native-height", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame_total_lines(frame)))
}

/// `(frame-native-width &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_native_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-native-width", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame_total_cols(frame)))
}

/// `(frame-text-cols &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_text_cols(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-text-cols", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame_total_cols(frame)))
}

/// `(frame-text-lines &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_text_lines(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-text-lines", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame_text_lines(frame)))
}

/// `(frame-text-width &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_text_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    builtin_frame_text_cols(eval, args)
}

/// `(frame-text-height &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_text_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    builtin_frame_text_lines(eval, args)
}

/// `(frame-total-cols &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_total_cols(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-total-cols", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame_total_cols(frame)))
}

/// `(frame-total-lines &optional FRAME)` -> integer.
pub(crate) fn builtin_frame_total_lines(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-total-lines", &args, 1)?;
    let fid = resolve_frame_id(eval, args.first(), "framep")?;
    let frame = eval
        .frames
        .get(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    Ok(Value::Int(frame_total_lines(frame)))
}

/// `(frame-position &optional FRAME)` -> (X . Y).
pub(crate) fn builtin_frame_position(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-position", &args, 1)?;
    let _ = resolve_frame_id(eval, args.first(), "frame-live-p")?;
    Ok(Value::cons(Value::Int(0), Value::Int(0)))
}

/// `(set-frame-height FRAME HEIGHT &optional PRETEND PIXELWISE)` -> nil.
pub(crate) fn builtin_set_frame_height(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-frame-height", &args, 2)?;
    expect_max_args("set-frame-height", &args, 4)?;
    let fid = resolve_frame_id(eval, Some(&args[0]), "frame-live-p")?;
    let text_lines = expect_int(&args[1])?;

    let cols = {
        let frame = eval
            .frames
            .get(fid)
            .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
        frame_total_cols(frame)
    };
    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    set_frame_text_size(frame, cols, text_lines);
    Ok(Value::Nil)
}

/// `(set-frame-width FRAME WIDTH &optional PRETEND PIXELWISE)` -> nil.
pub(crate) fn builtin_set_frame_width(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-frame-width", &args, 2)?;
    expect_max_args("set-frame-width", &args, 4)?;
    let fid = resolve_frame_id(eval, Some(&args[0]), "frame-live-p")?;
    let cols = expect_int(&args[1])?;

    let text_lines = {
        let frame = eval
            .frames
            .get(fid)
            .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
        frame_text_lines(frame)
    };
    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    set_frame_text_size(frame, cols, text_lines);
    Ok(Value::Nil)
}

/// `(set-frame-size FRAME WIDTH HEIGHT &optional PIXELWISE)` -> nil.
pub(crate) fn builtin_set_frame_size(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-frame-size", &args, 3)?;
    expect_max_args("set-frame-size", &args, 4)?;
    let fid = resolve_frame_id(eval, Some(&args[0]), "frame-live-p")?;
    let cols = expect_int(&args[1])?;
    let text_lines = expect_int(&args[2])?;

    let frame = eval
        .frames
        .get_mut(fid)
        .ok_or_else(|| signal("error", vec![Value::string("Frame not found")]))?;
    set_frame_text_size(frame, cols, text_lines);
    Ok(Value::Nil)
}

/// `(set-frame-position FRAME X Y)` -> t.
pub(crate) fn builtin_set_frame_position(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-frame-position", &args, 3)?;
    let _ = resolve_frame_id(eval, Some(&args[0]), "frame-live-p")?;
    let _ = expect_int(&args[1])?;
    let _ = expect_int(&args[2])?;
    Ok(Value::True)
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
    Ok(Value::Frame(fid.0))
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
        Some(Value::Frame(id)) => FrameId(*id),
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

/// `(framep OBJ)` -> t if OBJ is a frame object or frame id that exists.
pub(crate) fn builtin_framep(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("framep", &args, 1)?;
    let id = match &args[0] {
        Value::Frame(id) => *id,
        Value::Int(n) => *n as u64,
        _ => return Ok(Value::Nil),
    };
    Ok(Value::bool(eval.frames.get(FrameId(id)).is_some()))
}

/// `(frame-live-p OBJ)` -> t if OBJ is a live frame object or frame id.
pub(crate) fn builtin_frame_live_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("frame-live-p", &args, 1)?;
    let id = match &args[0] {
        Value::Frame(id) => *id,
        Value::Int(n) => *n as u64,
        _ => return Ok(Value::Nil),
    };
    Ok(Value::bool(eval.frames.get(FrameId(id)).is_some()))
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use crate::elisp::{format_eval_result, parse_forms, Evaluator, Value};

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
    fn selected_window_returns_window_handle() {
        let r = eval_one_with_frame("(selected-window)");
        assert!(
            r.starts_with("OK #<window "),
            "expected window handle, got: {r}"
        );
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
    fn minibuffer_window_frame_first_window_and_window_minibuffer_p_semantics() {
        let forms = parse_forms(
            "(window-minibuffer-p)
             (windowp (minibuffer-window))
             (windowp (minibuffer-window (selected-frame)))
             (window-minibuffer-p (minibuffer-window))
             (eq (frame-first-window) (selected-window))
             (eq (frame-first-window (selected-window)) (selected-window))
             (eq (frame-first-window (minibuffer-window)) (selected-window))
             (eq (minibuffer-window) (car (last (window-list nil t))))
             (condition-case err (minibuffer-window 999999) (error err))
             (condition-case err (window-minibuffer-p 999999) (error err))
             (condition-case err (frame-first-window 999999) (error err))
             (condition-case err (minibuffer-window (selected-window)) (error (car err)))
             (condition-case err (window-minibuffer-p nil nil) (error (car err)))
             (condition-case err (frame-first-window nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK nil");
        assert_eq!(out[1], "OK t");
        assert_eq!(out[2], "OK t");
        assert_eq!(out[3], "OK t");
        assert_eq!(out[4], "OK t");
        assert_eq!(out[5], "OK t");
        assert_eq!(out[6], "OK t");
        assert_eq!(out[7], "OK t");
        assert_eq!(out[8], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[9], "OK (wrong-type-argument window-valid-p 999999)");
        assert_eq!(out[10], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[11], "OK wrong-type-argument");
        assert_eq!(out[12], "OK wrong-number-of-arguments");
        assert_eq!(out[13], "OK wrong-number-of-arguments");
    }

    #[test]
    fn frame_root_window_window_valid_and_minibuffer_activity_semantics() {
        let forms = parse_forms(
            "(window-valid-p (selected-window))
             (window-valid-p (minibuffer-window))
             (window-valid-p nil)
             (window-valid-p 999999)
             (window-valid-p 'foo)
             (eq (frame-root-window) (selected-window))
             (eq (frame-root-window (selected-frame)) (selected-window))
             (eq (frame-root-window (selected-window)) (selected-window))
             (eq (frame-root-window (minibuffer-window)) (selected-window))
             (minibuffer-selected-window)
             (active-minibuffer-window)
             (minibuffer-window-active-p (minibuffer-window))
             (minibuffer-window-active-p (selected-window))
             (minibuffer-window-active-p nil)
             (minibuffer-window-active-p 999999)
             (minibuffer-window-active-p 'foo)
             (let ((w (split-window)))
               (delete-window w)
               (window-valid-p w))
             (condition-case err (window-valid-p) (error err))
             (condition-case err (window-valid-p nil nil) (error err))
             (condition-case err (frame-root-window 999999) (error err))
             (condition-case err (frame-root-window 'foo) (error err))
             (condition-case err (frame-root-window nil nil) (error err))
             (condition-case err (minibuffer-selected-window nil) (error err))
             (condition-case err (active-minibuffer-window nil) (error err))
             (condition-case err (minibuffer-window-active-p) (error err))
             (condition-case err (minibuffer-window-active-p nil nil) (error err))",
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
        assert_eq!(out[2], "OK nil");
        assert_eq!(out[3], "OK nil");
        assert_eq!(out[4], "OK nil");
        assert_eq!(out[5], "OK t");
        assert_eq!(out[6], "OK t");
        assert_eq!(out[7], "OK t");
        assert_eq!(out[8], "OK t");
        assert_eq!(out[9], "OK nil");
        assert_eq!(out[10], "OK nil");
        assert_eq!(out[11], "OK nil");
        assert_eq!(out[12], "OK nil");
        assert_eq!(out[13], "OK nil");
        assert_eq!(out[14], "OK nil");
        assert_eq!(out[15], "OK nil");
        assert_eq!(out[16], "OK nil");
        assert_eq!(out[17], "OK (wrong-number-of-arguments window-valid-p 0)");
        assert_eq!(out[18], "OK (wrong-number-of-arguments window-valid-p 2)");
        assert_eq!(out[19], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[20], "OK (wrong-type-argument frame-live-p foo)");
        assert_eq!(
            out[21],
            "OK (wrong-number-of-arguments frame-root-window 2)"
        );
        assert_eq!(
            out[22],
            "OK (wrong-number-of-arguments minibuffer-selected-window 1)"
        );
        assert_eq!(
            out[23],
            "OK (wrong-number-of-arguments active-minibuffer-window 1)"
        );
        assert_eq!(
            out[24],
            "OK (wrong-number-of-arguments minibuffer-window-active-p 0)"
        );
        assert_eq!(
            out[25],
            "OK (wrong-number-of-arguments minibuffer-window-active-p 2)"
        );
    }

    #[test]
    fn frame_root_window_p_semantics_and_errors() {
        let forms = parse_forms(
            "(frame-root-window-p (selected-window))
             (frame-root-window-p (minibuffer-window))
             (condition-case err (frame-root-window-p 999999) (error err))
             (condition-case err (frame-root-window-p 'foo) (error err))
             (condition-case err (frame-root-window-p) (error (car err)))
             (condition-case err (frame-root-window-p nil nil) (error (car err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
        assert_eq!(out[1], "OK nil");
        assert_eq!(out[2], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[3], "OK (wrong-type-argument window-live-p foo)");
        assert_eq!(out[4], "OK wrong-number-of-arguments");
        assert_eq!(out[5], "OK wrong-number-of-arguments");
    }

    #[test]
    fn window_at_matches_batch_coordinate_and_error_semantics() {
        let forms = parse_forms(
            "(windowp (window-at 0 0))
             (windowp (window-at 79 0))
             (null (window-at 80 0))
             (windowp (window-at 0 23))
             (let ((w (window-at 0 24))) (and w (window-minibuffer-p w)))
             (null (window-at 0 25))
             (null (window-at -1 0))
             (null (window-at 0 -1))
             (windowp (window-at 79.9 0))
             (null (window-at 80.0 0))
             (windowp (window-at 0 24.1))
             (condition-case err (window-at 'foo 0) (error err))
             (condition-case err (window-at 0 'foo) (error err))
             (condition-case err (window-at 0 0 999999) (error err))
             (condition-case err (window-at 0) (error (car err)))
             (condition-case err (window-at 0 0 nil nil) (error (car err)))",
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
        assert_eq!(out[3], "OK t");
        assert_eq!(out[4], "OK t");
        assert_eq!(out[5], "OK t");
        assert_eq!(out[6], "OK t");
        assert_eq!(out[7], "OK t");
        assert_eq!(out[8], "OK t");
        assert_eq!(out[9], "OK t");
        assert_eq!(out[10], "OK t");
        assert_eq!(out[11], "OK (wrong-type-argument numberp foo)");
        assert_eq!(out[12], "OK (wrong-type-argument numberp foo)");
        assert_eq!(out[13], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[14], "OK wrong-number-of-arguments");
        assert_eq!(out[15], "OK wrong-number-of-arguments");
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
            "(let ((w (selected-window)))
                (with-current-buffer (window-buffer w)
                  (erase-buffer)
                  (insert (make-string 200 ?x)))
                (set-window-start w 42))
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
            "(let ((w (selected-window)))
                (with-current-buffer (window-buffer w)
                  (erase-buffer)
                  (insert (make-string 200 ?x)))
                (set-window-point w 10))
             (window-point)",
        );
        assert_eq!(results[0], "OK 10");
        assert_eq!(results[1], "OK 10");
    }

    #[test]
    fn set_window_start_point_and_group_start_accept_marker_positions() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (with-current-buffer (window-buffer w)
                         (erase-buffer)
                         (insert \"abcdef\")
                         (goto-char 3)
                         (point-marker))))
               (list (markerp (set-window-start w m))
                     (window-start w)
                     (set-window-point w m)
                     (window-point w)
                     (markerp (set-window-group-start w m))
                     (window-start w)
                     (window-point w)))
             (let* ((w (selected-window))
                    (_ (progn
                         (set-window-start w 7)
                         (set-window-point w 7)))
                    (m (with-current-buffer (get-buffer-create \" *neovm-marker-other*\")
                         (erase-buffer)
                         (insert \"xyz\")
                         (goto-char 2)
                         (point-marker))))
               (list (markerp (set-window-start w m))
                     (window-start w)
                     (set-window-point w m)
                     (window-point w)
                     (markerp (set-window-group-start w m))
                     (window-start w)
                     (window-point w)))
             (let* ((w (selected-window))
                    (_ (set-window-start w 1))
                    (_ (set-window-point w 1)))
               (list (= (set-window-start w 0) 0)
                     (= (window-start w) 1)
                     (= (set-window-point w 0) 0)
                     (= (window-point w) 1)
                     (= (set-window-group-start w 0) 0)
                     (= (window-group-start w) 1)
                     (= (window-point w) 1)
                     (= (set-window-start w -10) -10)
                     (= (window-start w) 1)
                     (= (set-window-point w -10) -10)
                     (= (window-point w) 1)
                     (= (set-window-group-start w -10) -10)
                     (= (window-group-start w) 1)
                     (= (window-point w) 1)))
             (let* ((w (selected-window))
                    (_ (set-window-start w 1))
                    (_ (set-window-point w 1))
                    (m0 (make-marker))
                    (_ (set-marker m0 0 (window-buffer w)))
                    (mneg (make-marker))
                    (_ (set-marker mneg -5 (window-buffer w))))
               (list (markerp (set-window-start w m0))
                     (= (window-start w) 1)
                     (= (set-window-point w m0) 1)
                     (= (window-point w) 1)
                     (markerp (set-window-group-start w m0))
                     (= (window-group-start w) 1)
                     (= (window-point w) 1)
                     (markerp (set-window-start w mneg))
                     (= (window-start w) 1)
                     (= (set-window-point w mneg) 1)
                     (= (window-point w) 1)
                     (markerp (set-window-group-start w mneg))
                     (= (window-group-start w) 1)
                     (= (window-point w) 1)))
             (let* ((w (selected-window))
                    (_ (with-current-buffer (window-buffer w)
                         (erase-buffer)
                         (insert \"abcdef\")
                         (goto-char 1)))
                    (_ (set-window-start w 1))
                    (_ (set-window-point w 1)))
               (list (= (set-window-start w 9999) 9999)
                     (= (window-start w) 7)
                     (= (set-window-point w 9999) 9999)
                     (= (window-point w) 7)
                     (= (set-window-group-start w 9999) 9999)
                     (= (window-group-start w) 7)
                     (= (window-point w) 7)))
             (let* ((w (selected-window))
                    (_ (with-current-buffer (window-buffer w)
                         (erase-buffer)
                         (insert \"abcdef\")
                         (goto-char 1)))
                    (m (make-marker))
                    (_ (set-marker m 9999 (window-buffer w))))
               (list (markerp (set-window-start w m))
                     (= (window-start w) 7)
                     (= (set-window-point w m) 7)
                     (= (window-point w) 7)
                     (markerp (set-window-group-start w m))
                     (= (window-group-start w) 7)
                     (= (window-point w) 7)))
             (let ((m (make-marker)))
               (list (condition-case err (set-window-start (selected-window) m) (error err))
                     (condition-case err (set-window-point (selected-window) m) (error err))
                     (condition-case err (set-window-group-start (selected-window) m) (error err))))
             (list (condition-case err (set-window-start nil 1.5) (error err))
                   (condition-case err (set-window-point nil 1.5) (error err))
                   (condition-case err (set-window-group-start nil 1.5) (error err))
                   (condition-case err (set-window-start nil 'foo) (error err))
                   (condition-case err (set-window-point nil 'foo) (error err))
                   (condition-case err (set-window-group-start nil 'foo) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (t 3 3 3 t 3 3)");
        assert_eq!(out[1], "OK (t 2 2 2 t 2 2)");
        assert_eq!(out[2], "OK (t t t t t t t t t t t t t t)");
        assert_eq!(out[3], "OK (t t t t t t t t t t t t t t)");
        assert_eq!(out[4], "OK (t t t t t t t)");
        assert_eq!(out[5], "OK (t t t t t t t)");
        assert_eq!(
            out[6],
            "OK ([:marker nil nil nil] (error \"Marker does not point anywhere\") [:marker nil nil nil])"
        );
        assert_eq!(
            out[7],
            "OK ((wrong-type-argument integer-or-marker-p 1.5) (wrong-type-argument integer-or-marker-p 1.5) (wrong-type-argument integer-or-marker-p 1.5) (wrong-type-argument integer-or-marker-p foo) (wrong-type-argument integer-or-marker-p foo) (wrong-type-argument integer-or-marker-p foo))"
        );
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
    fn window_list_1_callable_paths_return_live_windows() {
        let r = eval_one_with_frame(
            "(let* ((fn (indirect-function 'window-list-1))
                    (a (funcall #'window-list-1 nil nil))
                    (b (apply #'window-list-1 '(nil nil)))
                    (c (funcall fn nil nil)))
               (list (listp a)
                     (consp a)
                     (equal a b)
                     (equal a c)
                     (null (memq nil (mapcar #'windowp a)))))",
        );
        assert_eq!(r, "OK (t t t t t)");
    }

    #[test]
    fn window_list_1_stale_window_signals_wrong_type_argument() {
        let r = eval_one_with_frame(
            "(let ((w (split-window)))
               (delete-window w)
               (list (condition-case err (window-list-1 w nil) (error (car err)))
                     (condition-case err (funcall #'window-list-1 w nil) (error (car err)))
                     (condition-case err (apply #'window-list-1 (list w nil)) (error (car err)))))",
        );
        assert_eq!(
            r,
            "OK (wrong-type-argument wrong-type-argument wrong-type-argument)"
        );
    }

    #[test]
    fn window_list_1_all_frames_includes_other_frame_windows() {
        let r = eval_one_with_frame(
            "(let ((f1 (selected-frame))
                  (f2 (make-frame)))
               (let ((w1 (progn (select-frame f1) (selected-window)))
                     (w2 (progn (select-frame f2) (selected-window))))
                 (prog1
                     (list (null (memq w2 (window-list-1 w1 nil nil)))
                           (not (null (memq w2 (window-list-1 w1 nil t))))
                           (not (null (memq w2 (window-list-1 w1 nil 'visible))))
                           (not (null (memq w2 (window-list-1 w1 nil 0))))
                           (not (null (memq w2 (window-list-1 w1 nil f2))))
                           (null (memq w2 (window-list-1 w1 nil :bad))))
                   (select-frame f1)
                   (delete-frame f2))))",
        );
        assert_eq!(r, "OK (t t t t t t)");
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
                     (window-point m)
                     (window-body-height m)
                     (window-body-height m t)))
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
        assert_eq!(out[0], "OK (t t \" *Minibuf-0*\" 1 1 1 1)");
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
               (eq (selected-window) new-win))",
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
               (not (eq (selected-window) w1)))",
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
    fn other_window_requires_count_and_enforces_number_or_marker_p() {
        let forms = parse_forms(
            "(condition-case err (other-window) (error (car err)))
             (condition-case err (other-window nil) (error err))
             (condition-case err (other-window \"x\") (error err))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK (wrong-type-argument number-or-marker-p nil)");
        assert_eq!(out[2], "OK (wrong-type-argument number-or-marker-p \"x\")");
    }

    #[test]
    fn other_window_accepts_float_counts_with_floor_semantics() {
        let results = eval_with_frame(
            "(let* ((w1 (progn (delete-other-windows) (selected-window)))
                    (w2 (split-window)))
               (list
                 (progn (other-window 1.5) (eq (selected-window) w2))
                 (progn (select-window w1) (other-window 0.4) (eq (selected-window) w1))
                 (progn (select-window w1) (other-window -0.4) (eq (selected-window) w2))
                 (progn (select-window w1) (other-window -1.2) (eq (selected-window) w1))))",
        );
        assert_eq!(results[0], "OK (t t t t)");
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
        let forms = parse_forms("(other-window 1)").expect("parse");
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
    fn window_geometry_helper_queries_match_batch_defaults_and_error_predicates() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-left-column w)
                     (window-left-column m)
                     (window-top-line w)
                     (window-top-line m)
                     (window-hscroll w)
                     (window-hscroll m)
                     (window-margins w)
                     (window-margins m)
                     (window-fringes w)
                     (window-fringes m)
                     (window-scroll-bars w)
                     (window-scroll-bars m)))
             (list (condition-case err (window-left-column 999999) (error err))
                   (condition-case err (window-top-line 999999) (error err))
                   (condition-case err (window-hscroll 999999) (error err))
                   (condition-case err (window-margins 999999) (error err))
                   (condition-case err (window-fringes 999999) (error err))
                   (condition-case err (window-scroll-bars 999999) (error err))
                   (condition-case err (window-left-column nil nil) (error err))
                   (condition-case err (window-top-line nil nil) (error err))
                   (condition-case err (window-hscroll nil nil) (error err))
                   (condition-case err (window-margins nil nil) (error err))
                   (condition-case err (window-fringes nil nil) (error err))
                   (condition-case err (window-scroll-bars nil nil) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(
            out[0],
            "OK (0 0 0 24 0 0 (nil) (nil) (0 0 nil nil) (0 0 nil nil) (nil 0 t nil 0 t nil) (nil 0 t nil 0 t nil))"
        );
        assert_eq!(
            out[1],
            "OK ((wrong-type-argument window-valid-p 999999) (wrong-type-argument window-valid-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-number-of-arguments window-left-column 2) (wrong-number-of-arguments window-top-line 2) (wrong-number-of-arguments window-hscroll 2) (wrong-number-of-arguments window-margins 2) (wrong-number-of-arguments window-fringes 2) (wrong-number-of-arguments window-scroll-bars 2))"
        );
    }

    #[test]
    fn window_use_time_and_old_state_queries_match_batch_defaults_and_error_predicates() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-use-time w)
                     (window-use-time m)
                     (window-old-point w)
                     (window-old-point m)
                     (window-old-buffer w)
                     (window-old-buffer m)
                     (window-prev-buffers w)
                     (window-prev-buffers m)
                     (window-next-buffers w)
                     (window-next-buffers m)))
             (let* ((w1 (selected-window))
                    (w2 (split-window))
                    (m (car (last (window-list nil t)))))
               (list (window-use-time w1)
                     (window-use-time w2)
                     (window-use-time m)
                     (window-old-point w1)
                     (window-old-point w2)
                     (window-old-point m)
                     (window-old-buffer w1)
                     (window-old-buffer w2)
                     (window-prev-buffers w1)
                     (window-prev-buffers w2)
                     (window-next-buffers w1)
                     (window-next-buffers w2)))
             (list (condition-case err (window-use-time 999999) (error err))
                   (condition-case err (window-old-point 999999) (error err))
                   (condition-case err (window-old-buffer 999999) (error err))
                   (condition-case err (window-prev-buffers 999999) (error err))
                   (condition-case err (window-next-buffers 999999) (error err))
                   (condition-case err (window-use-time nil nil) (error err))
                   (condition-case err (window-old-point nil nil) (error err))
                   (condition-case err (window-old-buffer nil nil) (error err))
                   (condition-case err (window-prev-buffers nil nil) (error err))
                   (condition-case err (window-next-buffers nil nil) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (1 0 1 1 nil nil nil nil nil nil)");
        assert_eq!(out[1], "OK (1 0 0 1 1 1 nil nil nil nil nil nil)");
        assert_eq!(
            out[2],
            "OK ((wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-number-of-arguments window-use-time 2) (wrong-number-of-arguments window-old-point 2) (wrong-number-of-arguments window-old-buffer 2) (wrong-number-of-arguments window-prev-buffers 2) (wrong-number-of-arguments window-next-buffers 2))"
        );
    }

    #[test]
    fn window_bump_use_time_tracks_second_most_recent_window() {
        let forms = parse_forms(
            "(let* ((w1 (selected-window))
                    (w2 (split-window)))
               (list (window-use-time w1)
                     (window-use-time w2)
                     (window-bump-use-time w2)
                     (window-use-time w1)
                     (window-use-time w2)
                     (window-bump-use-time w1)))
             (list (condition-case err (window-bump-use-time 1) (error err))
                   (condition-case err (window-bump-use-time nil nil) (error err))
                   (let ((w (split-window)))
                     (delete-window w)
                     (condition-case err (window-bump-use-time w) (error (car err)))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (1 0 1 2 1 nil)");
        assert_eq!(
            out[1],
            "OK ((wrong-type-argument window-live-p 1) (wrong-number-of-arguments window-bump-use-time 2) wrong-type-argument)"
        );
    }

    #[test]
    fn window_group_start_and_buffer_history_setters_match_batch_semantics() {
        let forms = parse_forms(
            "(list (equal (subr-arity (symbol-function 'window-group-start)) '(0 . 1))
                   (equal (subr-arity (symbol-function 'set-window-group-start)) '(2 . 3))
                   (equal (subr-arity (symbol-function 'set-window-prev-buffers)) '(2 . 2))
                   (equal (subr-arity (symbol-function 'set-window-next-buffers)) '(2 . 2)))
             (let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (= (window-group-start w) 1)
                     (= (window-group-start m) 1)
                     (null (window-prev-buffers w))
                     (null (window-next-buffers w))
                     (null (window-prev-buffers m))
                     (null (window-next-buffers m))))
             (let* ((w (selected-window))
                    (p '((alpha 1 2)))
                    (n '((beta 3 4))))
               (list (equal (set-window-prev-buffers w p) p)
                     (equal (window-prev-buffers w) p)
                     (equal (set-window-next-buffers w n) n)
                     (equal (window-next-buffers w) n)
                     (null (set-window-prev-buffers w nil))
                     (null (window-prev-buffers w))
                     (null (set-window-next-buffers w nil))
                     (null (window-next-buffers w))))
             (let* ((w (selected-window))
                    (_ (with-current-buffer (window-buffer w)
                         (erase-buffer)
                         (insert (make-string 400 ?x))
                         (goto-char 1)))
                    (_ (set-window-start w 1))
                    (_ (set-window-point w 1)))
               (list (= (set-window-group-start w 120 nil) 120)
                     (= (window-group-start w) 120)
                     (= (window-point w) 1)
                     (= (set-window-group-start w ?A t) 65)
                     (= (window-group-start w) 65)
                     (= (window-point w) 1)))
             (let* ((m (car (last (window-list nil t))))
                    (p '(mini-prev))
                    (n '(mini-next)))
               (list (= (set-window-group-start m 42) 42)
                     (= (set-window-group-start m ?A t) 65)
                     (= (window-group-start m) 1)
                     (equal (set-window-prev-buffers m p) p)
                     (equal (window-prev-buffers m) p)
                     (equal (set-window-next-buffers m n) n)
                     (equal (window-next-buffers m) n)
                     (null (set-window-prev-buffers m nil))
                     (null (set-window-next-buffers m nil))
                     (null (window-prev-buffers m))
                     (null (window-next-buffers m))))
             (list (condition-case err (window-group-start 999999) (error (car err)))
                   (condition-case err (set-window-group-start 999999 1) (error (car err)))
                   (condition-case err (set-window-group-start nil 'foo) (error err))
                   (condition-case err (set-window-prev-buffers 999999 nil) (error (car err)))
                   (condition-case err (set-window-next-buffers 999999 nil) (error (car err)))
                   (condition-case err (set-window-prev-buffers 'foo nil) (error (car err)))
                   (condition-case err (set-window-next-buffers 'foo nil) (error (car err))))
             (list (condition-case err (window-group-start nil nil) (error err))
                   (condition-case err (set-window-group-start nil) (error err))
                   (condition-case err (set-window-group-start nil nil nil nil) (error err))
                   (condition-case err (set-window-prev-buffers nil) (error err))
                   (condition-case err (set-window-next-buffers nil nil nil) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (t t t t)");
        assert_eq!(out[1], "OK (t t t t t t)");
        assert_eq!(out[2], "OK (t t t t t t t t)");
        assert_eq!(out[3], "OK (t t t t t t)");
        assert_eq!(out[4], "OK (t t t t t t t t t t t)");
        assert_eq!(
            out[5],
            "OK (wrong-type-argument wrong-type-argument (wrong-type-argument integer-or-marker-p foo) wrong-type-argument wrong-type-argument wrong-type-argument wrong-type-argument)"
        );
        assert_eq!(
            out[6],
            "OK ((wrong-number-of-arguments window-group-start 2) (wrong-number-of-arguments set-window-group-start 1) (wrong-number-of-arguments set-window-group-start 4) (wrong-number-of-arguments set-window-prev-buffers 1) (wrong-number-of-arguments set-window-next-buffers 3))"
        );
    }

    #[test]
    fn window_vscroll_helpers_match_batch_defaults_and_error_predicates() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-vscroll w)
                     (window-vscroll m)
                     (window-vscroll w t)
                     (window-vscroll m t)
                     (set-window-vscroll w 1)
                     (set-window-vscroll w 2 t)
                     (set-window-vscroll w 3 t t)
                     (set-window-vscroll nil 1.5)
                     (window-vscroll w)
                     (window-vscroll w t)))
             (list (condition-case err (window-vscroll 999999) (error err))
                   (condition-case err (window-vscroll 'foo) (error err))
                   (condition-case err (set-window-vscroll 999999 1) (error err))
                   (condition-case err (set-window-vscroll 'foo 1) (error err))
                   (condition-case err (set-window-vscroll nil 'foo) (error err))
                   (condition-case err (window-vscroll nil nil nil) (error err))
                   (condition-case err (set-window-vscroll nil 1 nil nil nil) (error err)))
             (let ((w (split-window)))
               (delete-window w)
               (list (condition-case err (window-vscroll w) (error (car err)))
                     (condition-case err (set-window-vscroll w 1) (error (car err)))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (0 0 0 0 0 0 0 0 0 0)");
        assert_eq!(
            out[1],
            "OK ((wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-type-argument numberp foo) (wrong-number-of-arguments window-vscroll 3) (wrong-number-of-arguments set-window-vscroll 5))"
        );
        assert_eq!(out[2], "OK (wrong-type-argument wrong-type-argument)");
    }

    #[test]
    fn window_hscroll_and_margin_setters_match_batch_defaults_and_error_predicates() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-hscroll w)
                     (set-window-hscroll w 3)
                     (window-hscroll w)
                     (set-window-hscroll w -1)
                     (window-hscroll w)
                     (set-window-hscroll w ?a)
                     (window-hscroll w)
                     (window-margins w)
                     (set-window-margins w 1 2)
                     (window-margins w)
                     (set-window-margins w 1 2)
                     (set-window-margins w nil nil)
                     (window-margins w)
                     (set-window-margins w 3)
                     (window-margins w)
                     (set-window-margins w 3)
                     (window-hscroll m)
                     (set-window-hscroll m 4)
                     (window-hscroll m)
                     (window-margins m)
                     (set-window-margins m 4 5)
                     (window-margins m)))
             (list (condition-case err (set-window-hscroll nil 1.5) (error err))
                   (condition-case err (set-window-hscroll nil 'foo) (error err))
                   (condition-case err (set-window-hscroll 999999 1) (error err))
                   (condition-case err (set-window-hscroll 'foo 1) (error err))
                   (condition-case err (set-window-hscroll nil) (error err))
                   (condition-case err (set-window-hscroll nil 1 nil) (error err))
                   (condition-case err (set-window-margins nil -1 0) (error err))
                   (condition-case err (set-window-margins nil 1 -2) (error err))
                   (condition-case err (set-window-margins nil 1.5 0) (error err))
                   (condition-case err (set-window-margins nil 'foo 0) (error err))
                   (condition-case err (set-window-margins nil 1 'foo) (error err))
                   (condition-case err (set-window-margins 999999 1 2) (error err))
                   (condition-case err (set-window-margins 'foo 1 2) (error err))
                   (condition-case err (set-window-margins nil) (error err))
                   (condition-case err (set-window-margins nil 1 2 3) (error err)))
             (let ((w (split-window)))
               (delete-window w)
               (list (condition-case err (set-window-hscroll w 1) (error (car err)))
                     (condition-case err (set-window-margins w 1 2) (error (car err)))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(
            out[0],
            "OK (0 3 3 0 0 97 97 (nil) t (1 . 2) nil t (nil) t (3) nil 0 4 4 (nil) t (4 . 5))"
        );
        assert_eq!(
            out[1],
            "OK ((wrong-type-argument fixnump 1.5) (wrong-type-argument fixnump foo) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-number-of-arguments set-window-hscroll 1) (wrong-number-of-arguments set-window-hscroll 3) (args-out-of-range -1 0 2147483647) (args-out-of-range -2 0 2147483647) (wrong-type-argument integerp 1.5) (wrong-type-argument integerp foo) (wrong-type-argument integerp foo) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-number-of-arguments set-window-margins 1) (wrong-number-of-arguments set-window-margins 4))"
        );
        assert_eq!(out[2], "OK (wrong-type-argument wrong-type-argument)");
    }

    #[test]
    fn window_fringes_and_scroll_bar_setters_match_batch_defaults_and_error_predicates() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-fringes w)
                     (window-fringes m)
                     (set-window-fringes w 0 0)
                     (set-window-fringes w 1 2)
                     (set-window-fringes w nil nil)
                     (window-fringes w)
                     (window-fringes m)
                     (window-scroll-bars w)
                     (window-scroll-bars m)
                     (set-window-scroll-bars w nil nil nil nil)
                     (set-window-scroll-bars w 'left)
                     (window-scroll-bars w)
                     (window-scroll-bars m)
                     (set-window-fringes m 0 0)
                     (set-window-scroll-bars m nil)
                     (window-fringes m)
                     (window-scroll-bars m)))
             (list (condition-case err (set-window-fringes nil 1 2 nil nil nil) (error err))
                   (condition-case err (set-window-scroll-bars nil nil nil nil nil nil nil) (error err))
                   (condition-case err (set-window-fringes 999999 0 0) (error err))
                   (condition-case err (set-window-fringes 'foo 0 0) (error err))
                   (condition-case err (set-window-scroll-bars 999999 nil) (error err))
                   (condition-case err (set-window-scroll-bars 'foo nil) (error err))
                   (condition-case err (set-window-fringes nil) (error err))
                   (condition-case err (set-window-scroll-bars) (error err)))
             (let ((w (split-window)))
               (delete-window w)
               (list (condition-case err (set-window-fringes w 0 0) (error (car err)))
                     (condition-case err (set-window-scroll-bars w nil) (error (car err)))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(
            out[0],
            "OK ((0 0 nil nil) (0 0 nil nil) nil nil nil (0 0 nil nil) (0 0 nil nil) (nil 0 t nil 0 t nil) (nil 0 t nil 0 t nil) nil nil (nil 0 t nil 0 t nil) (nil 0 t nil 0 t nil) nil nil (0 0 nil nil) (nil 0 t nil 0 t nil))"
        );
        assert_eq!(
            out[1],
            "OK ((wrong-number-of-arguments set-window-fringes 6) (wrong-number-of-arguments set-window-scroll-bars 7) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-number-of-arguments set-window-fringes 1) (wrong-number-of-arguments set-window-scroll-bars 0))"
        );
        assert_eq!(out[2], "OK (wrong-type-argument wrong-type-argument)");
    }

    #[test]
    fn window_parameter_helpers_match_batch_defaults_and_key_semantics() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-parameters w)
                     (window-parameters m)
                     (window-parameter w 'foo)
                     (window-parameter m 'foo)
                     (set-window-parameter w 'foo 'bar)
                     (window-parameter w 'foo)
                     (window-parameters w)
                     (set-window-parameter m 'foo 42)
                     (window-parameter m 'foo)
                     (window-parameters m)
                     (set-window-parameter w 'foo nil)
                     (window-parameter w 'foo)
                     (window-parameters w)
                     (set-window-parameter w 1 2)
                     (window-parameter w 1)
                     (window-parameters w)))
             (list (condition-case err (window-parameter 999999 'foo) (error err))
                   (condition-case err (set-window-parameter 999999 'foo 'bar) (error err))
                   (condition-case err (window-parameters 999999) (error err))
                   (condition-case err (window-parameter nil) (error err))
                   (condition-case err (window-parameter nil nil nil) (error err))
                   (condition-case err (set-window-parameter nil nil) (error err))
                   (condition-case err (set-window-parameter nil nil nil nil) (error err))
                   (condition-case err (window-parameters nil nil) (error err))
                   (condition-case err (window-parameter 'foo 'bar) (error err))
                   (condition-case err (set-window-parameter 'foo 'bar 'baz) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(
            out[0],
            "OK (nil nil nil nil bar bar ((foo . bar)) 42 42 ((foo . 42)) nil nil ((foo)) 2 2 ((1 . 2) (foo)))"
        );
        assert_eq!(
            out[1],
            "OK ((wrong-type-argument windowp 999999) (wrong-type-argument windowp 999999) (wrong-type-argument window-valid-p 999999) (wrong-number-of-arguments window-parameter 1) (wrong-number-of-arguments window-parameter 3) (wrong-number-of-arguments set-window-parameter 2) (wrong-number-of-arguments set-window-parameter 4) (wrong-number-of-arguments window-parameters 2) (wrong-type-argument windowp foo) (wrong-type-argument windowp foo))"
        );
    }

    #[test]
    fn window_display_table_helpers_match_batch_defaults_and_set_get_semantics() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t))))
                    (dt '(1 2 3)))
               (list (null (window-display-table w))
                     (null (window-display-table m))
                     (let ((rv (set-window-display-table w dt))) (equal rv dt))
                     (equal (window-display-table w) dt)
                     (null (set-window-display-table w nil))
                     (null (window-display-table w))
                     (let ((rv (set-window-display-table m dt))) (equal rv dt))
                     (equal (window-display-table m) dt)
                     (eq (set-window-display-table m 'foo) 'foo)
                     (eq (window-display-table m) 'foo)
                     (null (set-window-display-table m nil))
                     (null (window-display-table m))))
             (list (condition-case err (window-display-table nil nil) (error err))
                   (condition-case err (set-window-display-table nil nil nil) (error err))
                   (condition-case err (window-display-table 999999) (error err))
                   (condition-case err (set-window-display-table 999999 nil) (error err))
                   (condition-case err (window-display-table 'foo) (error err))
                   (condition-case err (set-window-display-table 'foo nil) (error err)))
             (let ((w (split-window)))
               (delete-window w)
               (list (condition-case err (window-display-table w) (error (car err)))
                     (condition-case err (set-window-display-table w nil) (error (car err)))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (t t t t t t t t t t t t)");
        assert_eq!(
            out[1],
            "OK ((wrong-number-of-arguments window-display-table 2) (wrong-number-of-arguments set-window-display-table 3) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-type-argument window-live-p foo))"
        );
        assert_eq!(out[2], "OK (wrong-type-argument wrong-type-argument)");
    }

    #[test]
    fn window_cursor_type_helpers_match_batch_defaults_and_set_get_semantics() {
        let forms = parse_forms(
            "(let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-cursor-type w)
                     (window-cursor-type m)
                     (set-window-cursor-type w nil)
                     (window-cursor-type w)
                     (set-window-cursor-type w 'bar)
                     (window-cursor-type w)
                     (set-window-cursor-type w t)
                     (window-cursor-type w)
                     (set-window-cursor-type m 'hbar)
                     (window-cursor-type m)
                     (set-window-cursor-type m nil)
                     (window-cursor-type m)))
             (list (condition-case err (window-cursor-type nil nil) (error err))
                   (condition-case err (set-window-cursor-type nil) (error err))
                   (condition-case err (set-window-cursor-type nil nil nil) (error err))
                   (condition-case err (window-cursor-type 999999) (error err))
                   (condition-case err (set-window-cursor-type 999999 nil) (error err))
                   (condition-case err (window-cursor-type 'foo) (error err))
                   (condition-case err (set-window-cursor-type 'foo nil) (error err)))
             (let ((w (split-window)))
               (delete-window w)
               (list (condition-case err (window-cursor-type w) (error (car err)))
                     (condition-case err (set-window-cursor-type w nil) (error (car err)))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (t t nil nil bar bar t t hbar hbar nil nil)");
        assert_eq!(
            out[1],
            "OK ((wrong-number-of-arguments window-cursor-type 2) (wrong-number-of-arguments set-window-cursor-type 1) (wrong-number-of-arguments set-window-cursor-type 3) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p foo) (wrong-type-argument window-live-p foo))"
        );
        assert_eq!(out[2], "OK (wrong-type-argument wrong-type-argument)");
    }

    #[test]
    fn window_preserve_size_fixed_and_resizable_helpers_match_batch_semantics() {
        let forms = parse_forms(
            "(let ((w (selected-window)))
               (list (window-size-fixed-p w)
                     (window-size-fixed-p w t)
                     (let ((r (window-preserve-size w nil t)))
                       (list (bufferp (car r))
                             (nth 1 r)
                             (integerp (nth 2 r))))
                     (window-size-fixed-p w)
                     (window-size-fixed-p w t)
                     (let ((r (window-preserve-size w t t)))
                       (list (bufferp (car r))
                             (integerp (nth 1 r))
                             (integerp (nth 2 r))))
                     (window-size-fixed-p w)
                     (window-size-fixed-p w t)
                     (window-size-fixed-p w nil t)
                     (window-size-fixed-p w t t)
                     (progn
                       (window-preserve-size w nil nil)
                       (window-preserve-size w t nil)
                       (list (window-size-fixed-p w)
                             (window-size-fixed-p w t)))))
             (let ((w (split-window nil nil 'right)))
               (split-window w nil 'below)
               (window-preserve-size w t t)
               (let ((before (list (window-resizable w 100 t)
                                   (window-resizable w -100 t)
                                   (window-resizable w 100 nil)
                                   (window-resizable w -100 nil)
                                   (window-size-fixed-p w)
                                   (window-size-fixed-p w t)
                                   (window-resizable w 1 t)
                                   (window-resizable w 1 t 'preserved)
                                   (window-resizable w 1.5 t)
                                   (window-resizable w -1.5 t))))
                 (window-preserve-size w t nil)
                 (list before
                       (window-size-fixed-p w t)
                       (window-resizable w 1 t)
                       (window-resizable w 1.5 t)
                       (window-resizable w -1.5 t))))
             (list (condition-case err (window-size-fixed-p 999999) (error (car err)))
                   (condition-case err (window-preserve-size 999999 nil t) (error (car err)))
                   (condition-case err (window-resizable 999999 1) (error (car err)))
                   (condition-case err (window-resizable nil 'foo) (error (car err)))
                   (condition-case err (window-size-fixed-p nil nil nil nil) (error err))
                   (condition-case err (window-preserve-size nil nil nil nil) (error err))
                   (condition-case err (window-resizable nil 1 nil nil nil nil) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(
            out[0],
            "OK (nil nil (t nil t) t nil (t t t) t t nil nil (nil nil))"
        );
        assert_eq!(out[1], "OK ((0 0 8 -8 nil t 0 1 0 0) nil 1 1.5 -1.5)");
        assert_eq!(
            out[2],
            "OK (error error error wrong-type-argument (wrong-number-of-arguments window-size-fixed-p 4) (wrong-number-of-arguments window-preserve-size 4) (wrong-number-of-arguments window-resizable 6))"
        );
    }

    #[test]
    fn window_geometry_queries_match_batch_alias_and_edge_shapes() {
        let forms = parse_forms(
            "(list (symbol-function 'window-inside-pixel-edges)
                   (symbol-function 'window-inside-edges))
             (let* ((w (selected-window))
                    (m (car (last (window-list nil t)))))
               (list (window-mode-line-height w)
                     (window-mode-line-height m)
                     (window-header-line-height w)
                     (window-header-line-height m)
                     (window-pixel-height w)
                     (window-pixel-height m)
                     (window-pixel-width w)
                     (window-pixel-width m)
                     (window-text-height w)
                     (window-text-height m)
                     (window-text-height w t)
                     (window-text-height m t)
                     (window-text-width w)
                     (window-text-width m)
                     (window-text-width w t)
                     (window-text-width m t)
                     (window-body-pixel-edges w)
                     (window-body-pixel-edges m)
                     (window-pixel-edges w)
                     (window-pixel-edges m)
                     (window-body-edges w)
                     (window-body-edges m)
                     (window-edges w)
                     (window-edges m)
                     (window-edges w t)
                     (window-edges m t)))
             (list (condition-case err (window-mode-line-height 999999) (error err))
                   (condition-case err (window-header-line-height 999999) (error err))
                   (condition-case err (window-pixel-height 999999) (error err))
                   (condition-case err (window-pixel-width 999999) (error err))
                   (condition-case err (window-text-height 999999) (error err))
                   (condition-case err (window-text-width 999999) (error err))
                   (condition-case err (window-body-pixel-edges 999999) (error err))
                   (condition-case err (window-pixel-edges 999999) (error err))
                   (condition-case err (window-body-edges 999999) (error err))
                   (condition-case err (window-edges 999999) (error err))
                   (condition-case err (window-text-height nil nil nil) (error err))
                   (condition-case err (window-mode-line-height nil nil) (error err))
                   (condition-case err (window-inside-pixel-edges nil nil) (error err))
                   (condition-case err (window-edges nil nil nil nil) (error err))
                   (condition-case err (window-edges nil nil nil nil nil) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (window-body-pixel-edges window-body-edges)");
        assert_eq!(
            out[1],
            "OK (1 0 0 0 24 1 80 80 23 1 23 1 80 80 80 80 (0 0 80 23) (0 24 80 25) (0 0 80 24) (0 24 80 25) (0 0 80 23) (0 24 80 25) (0 0 80 24) (0 24 80 25) (0 0 80 23) (0 24 80 25))"
        );
        assert_eq!(
            out[2],
            "OK ((wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-valid-p 999999) (wrong-type-argument window-valid-p 999999) (wrong-type-argument window-live-p 999999) (wrong-type-argument window-live-p 999999) (error \"999999 is not a live window\") (error \"999999 is not a valid window\") (error \"999999 is not a live window\") (error \"999999 is not a valid window\") (wrong-number-of-arguments window-text-height 3) (wrong-number-of-arguments window-mode-line-height 2) (wrong-number-of-arguments window-inside-pixel-edges 2) (0 0 80 24) (wrong-number-of-arguments window-edges 5))"
        );
    }

    #[test]
    fn next_window_cycles() {
        let results = eval_with_frame(
            "(let ((w1 (selected-window)))
               (split-window)
               (let ((w2 (next-window)))
                 (not (eq w1 w2))))",
        );
        assert_eq!(results[0], "OK t");
    }

    #[test]
    fn one_window_p_tracks_current_window_count() {
        let results = eval_with_frame(
            "(list (one-window-p)
                   (progn (split-window) (one-window-p)))",
        );
        assert_eq!(results[0], "OK (t nil)");
    }

    #[test]
    fn one_window_p_enforces_max_arity() {
        let results =
            eval_with_frame("(condition-case err (one-window-p nil nil nil) (error (car err)))");
        assert_eq!(results[0], "OK wrong-number-of-arguments");
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
    fn set_frame_selected_window_matches_selection_and_error_semantics() {
        let forms = parse_forms(
            "(condition-case err (set-frame-selected-window) (error (car err)))
             (condition-case err (set-frame-selected-window nil nil) (error err))
             (condition-case err (set-frame-selected-window nil 999999) (error err))
             (let* ((w1 (selected-window))
                    (w2 (split-window)))
               (prog1
                   (list (eq (set-frame-selected-window nil w2) w2)
                         (eq (selected-window) w2))
                 (select-window w1)
                 (delete-window w2)))
             (let* ((w1 (selected-window))
                    (w2 (split-window))
                    (t1 (window-use-time w1))
                    (t2 (window-use-time w2)))
               (prog1
                   (list (eq (set-frame-selected-window nil w2 t) w2)
                         (= (window-use-time w1) t1)
                         (= (window-use-time w2) t2)
                         (eq (selected-window) w2))
                 (select-window w1)
                 (delete-window w2)))
             (let* ((f1 (selected-frame))
                    (f2 (make-frame))
                    (w2 (progn (select-frame f2) (split-window))))
               (select-frame f1)
               (prog1
                   (list (eq (set-frame-selected-window f2 w2) w2)
                         (eq (selected-frame) f1)
                         (eq (frame-selected-window f2) w2))
                 (select-frame f2)
                 (delete-window w2)
                 (select-frame f1)
                 (delete-frame f2)))
             (let* ((f2 (make-frame))
                    (w1 (selected-window)))
               (prog1
                   (condition-case err (set-frame-selected-window f2 w1) (error err))
                 (delete-frame f2)))
             (let* ((w1 (selected-window))
                    (w2 (split-window)))
               (prog1
                   (list (eq (funcall #'set-frame-selected-window nil w2) w2)
                         (eq (apply #'set-frame-selected-window (list nil w1)) w1))
                 (select-window w1)
                 (delete-window w2)))
             (list (condition-case err (funcall #'set-frame-selected-window nil (selected-window) nil nil) (error err))
                   (condition-case err (apply #'set-frame-selected-window '(nil)) (error err)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK wrong-number-of-arguments");
        assert_eq!(out[1], "OK (wrong-type-argument window-live-p nil)");
        assert_eq!(out[2], "OK (wrong-type-argument window-live-p 999999)");
        assert_eq!(out[3], "OK (t t)");
        assert_eq!(out[4], "OK (t t t t)");
        assert_eq!(out[5], "OK (t t t)");
        assert_eq!(
            out[6],
            "OK (error \"In `set-frame-selected-window', WINDOW is not on FRAME\")"
        );
        assert_eq!(out[7], "OK (t t)");
        assert_eq!(
            out[8],
            "OK ((wrong-number-of-arguments #<subr set-frame-selected-window> 4) (wrong-number-of-arguments #<subr set-frame-selected-window> 1))"
        );
    }

    #[test]
    fn old_selected_window_matches_stable_and_stale_window_semantics() {
        let forms = parse_forms(
            "(windowp (old-selected-window))
             (let* ((w1 (selected-window))
                    (w2 (split-window)))
               (prog1
                   (list (eq (old-selected-window) w1)
                         (progn (select-window w2) (eq (old-selected-window) w1))
                         (progn (select-window w1) (eq (old-selected-window) w1))
                         (progn (other-window 1) (eq (old-selected-window) w1))
                         (progn (other-window 1) (eq (old-selected-window) w1))
                         (progn (select-window w2 t) (eq (old-selected-window) w1))
                         (progn (select-window w1 t) (eq (old-selected-window) w1)))
                 (select-window w1)
                 (delete-window w2)))
             (let* ((w1 (selected-window))
                    (w2 (split-window)))
               (prog1
                   (list (progn (select-window w2) (eq (old-selected-window) w1))
                         (progn (delete-window w1) (windowp (old-selected-window)))
                         (window-live-p (old-selected-window))
                         (eq (old-selected-window) w2))
                 (delete-other-windows w2)))
             (list (condition-case err (old-selected-window nil) (error (car err)))
                   (eq (funcall #'old-selected-window) (old-selected-window))
                   (eq (apply #'old-selected-window nil) (old-selected-window))
                   (condition-case err (funcall #'old-selected-window nil) (error (car err)))
                   (condition-case err (apply #'old-selected-window '(nil)) (error (car err))))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK t");
        assert_eq!(out[1], "OK (t t t t t t t)");
        assert_eq!(out[2], "OK (t t nil nil)");
        assert_eq!(
            out[3],
            "OK (wrong-number-of-arguments t t wrong-number-of-arguments wrong-number-of-arguments)"
        );
    }

    #[test]
    fn frame_old_selected_window_matches_batch_and_arity_semantics() {
        let forms = parse_forms(
            "(condition-case err (frame-old-selected-window 999999) (error err))
             (condition-case err (frame-old-selected-window 'foo) (error err))
             (condition-case err (frame-old-selected-window nil nil) (error (car err)))
             (let ((f (selected-frame)))
               (list (frame-old-selected-window)
                     (frame-old-selected-window nil)
                     (frame-old-selected-window f)
                     (frame-old-selected-window (window-frame (selected-window)))))
             (let* ((w1 (selected-window))
                    (w2 (split-window)))
               (prog1
                   (list (eq (frame-old-selected-window) nil)
                         (progn (select-window w2) (eq (frame-old-selected-window) nil))
                         (progn (other-window 1) (eq (frame-old-selected-window) nil))
                         (progn (set-frame-selected-window nil w2) (eq (frame-old-selected-window) nil))
                         (progn (set-frame-selected-window nil w1) (eq (frame-old-selected-window) nil))
                         (progn (set-frame-selected-window nil w2 t) (eq (frame-old-selected-window) nil))
                         (progn (set-frame-selected-window nil w1 t) (eq (frame-old-selected-window) nil)))
                 (select-window w1)
                 (delete-window w2)))
             (list (condition-case err (funcall #'frame-old-selected-window nil nil) (error err))
                   (condition-case err (apply #'frame-old-selected-window '(nil nil)) (error err))
                   (eq (funcall #'frame-old-selected-window) (frame-old-selected-window))
                   (eq (apply #'frame-old-selected-window nil) (frame-old-selected-window)))",
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let out = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect::<Vec<_>>();
        assert_eq!(out[0], "OK (wrong-type-argument frame-live-p 999999)");
        assert_eq!(out[1], "OK (wrong-type-argument frame-live-p foo)");
        assert_eq!(out[2], "OK wrong-number-of-arguments");
        assert_eq!(out[3], "OK (nil nil nil nil)");
        assert_eq!(out[4], "OK (t t t t t t t)");
        assert_eq!(
            out[5],
            "OK ((wrong-number-of-arguments #<subr frame-old-selected-window> 2) (wrong-number-of-arguments #<subr frame-old-selected-window> 2) t t)"
        );
    }

    #[test]
    fn selected_frame_returns_frame_handle() {
        let r = eval_one_with_frame(
            "(let ((f (selected-frame)))
               (list (framep f)
                     (frame-live-p f)
                     (integerp f)
                     (eq f (window-frame))))",
        );
        assert_eq!(r, "OK (t t nil t)");
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
    fn frame_builtins_accept_frame_handle_values() {
        let mut ev = Evaluator::new();
        let fid = super::ensure_selected_frame_id(&mut ev);
        let frame = Value::Frame(fid.0);

        assert_eq!(
            super::builtin_framep(&mut ev, vec![frame.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            super::builtin_frame_live_p(&mut ev, vec![frame.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            super::builtin_frame_visible_p(&mut ev, vec![frame.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            super::builtin_select_frame(&mut ev, vec![frame.clone()]).unwrap(),
            Value::Frame(fid.0)
        );
        assert_eq!(
            super::builtin_select_frame_set_input_focus(&mut ev, vec![frame]).unwrap(),
            Value::Nil
        );
    }

    #[test]
    fn frame_visible_p_requires_one_arg() {
        let r = eval_one_with_frame("(condition-case err (frame-visible-p) (error (car err)))");
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
    fn set_window_buffer_restores_saved_window_point_and_keep_margins() {
        let results = eval_with_frame(
            "(setq swb-test-w (selected-window))
             (setq swb-test-b1 (get-buffer-create \"swb-state-a\"))
             (setq swb-test-b2 (get-buffer-create \"swb-state-b\"))
             (with-current-buffer swb-test-b1
               (erase-buffer)
               (insert (make-string 300 ?a))
               (goto-char 120))
             (with-current-buffer swb-test-b2
               (erase-buffer)
               (insert (make-string 300 ?b))
               (goto-char 150))
             (set-window-buffer swb-test-w swb-test-b1)
             (set-window-start swb-test-w 110)
             (set-window-point swb-test-w 120)
             (set-window-margins swb-test-w 3 4)
             (list (window-start swb-test-w)
                   (window-point swb-test-w)
                   (window-margins swb-test-w))
             (progn
               (set-window-buffer swb-test-w swb-test-b2)
               (list (window-start swb-test-w)
                     (window-point swb-test-w)
                     (window-margins swb-test-w)))
             (progn
               (set-window-margins swb-test-w 7 8)
               (set-window-buffer swb-test-w swb-test-b1 t)
               (list (window-start swb-test-w)
                     (window-point swb-test-w)
                     (window-margins swb-test-w)))
             (progn
               (set-window-margins swb-test-w 9 10)
               (set-window-buffer swb-test-w swb-test-b2 t)
               (list (window-start swb-test-w)
                     (window-point swb-test-w)
                     (window-margins swb-test-w)))
             (progn
               (set-window-margins swb-test-w 11 12)
               (set-window-buffer swb-test-w swb-test-b1 nil)
               (list (window-start swb-test-w)
                     (window-point swb-test-w)
                     (window-margins swb-test-w)))",
        );
        assert_eq!(results[9], "OK (110 120 (3 . 4))");
        assert_eq!(results[10], "OK (1 150 (nil))");
        assert_eq!(results[11], "OK (110 120 (7 . 8))");
        assert_eq!(results[12], "OK (1 150 (9 . 10))");
        assert_eq!(results[13], "OK (110 120 (nil))");
    }

    #[test]
    fn set_window_buffer_updates_history_lists_on_real_buffer_switches() {
        let results = eval_with_frame(
            "(let* ((w (selected-window))
                    (b1 (get-buffer-create \"swb-hist-a\"))
                    (b2 (get-buffer-create \"swb-hist-b\"))
                    (n '((foo 1 2))))
               (with-current-buffer b1
                 (erase-buffer)
                 (insert (make-string 300 ?a)))
               (with-current-buffer b2
                 (erase-buffer)
                 (insert (make-string 300 ?b)))
               (set-window-prev-buffers w nil)
               (set-window-next-buffers w nil)
               (set-window-buffer w b1)
               (set-window-start w 7)
               (set-window-point w 11)
               (set-window-next-buffers w n)
               (set-window-buffer w b2)
               (list (null (window-next-buffers w))
                     (mapcar (lambda (e) (buffer-name (car e))) (window-prev-buffers w))
                     (mapcar (lambda (e)
                               (list (markerp (nth 1 e))
                                     (marker-position (nth 1 e))
                                     (markerp (nth 2 e))
                                     (marker-position (nth 2 e))))
                             (window-prev-buffers w))))
             (let* ((w (selected-window))
                    (same (window-buffer w))
                    (n '((foo 1 2)))
                    (before (window-prev-buffers w)))
               (set-window-next-buffers w n)
               (set-window-buffer w same)
               (list (equal (window-prev-buffers w) before)
                     (equal (window-next-buffers w) n)))
             (let* ((w (selected-window))
                    (b1 (get-buffer-create \"swb-hist-d1\"))
                    (b2 (get-buffer-create \"swb-hist-d2\")))
               (set-window-prev-buffers w nil)
               (set-window-buffer w b1)
               (set-window-buffer w b2)
               (set-window-buffer w b1)
               (set-window-buffer w b2)
               (mapcar (lambda (e) (buffer-name (car e))) (window-prev-buffers w)))",
        );
        assert_eq!(
            results[0],
            "OK (t (\"swb-hist-a\" \"*scratch*\") ((t 7 t 11) (t 1 t 1)))"
        );
        assert_eq!(results[1], "OK (t t)");
        assert_eq!(
            results[2],
            "OK (\"swb-hist-d1\" \"swb-hist-d2\" \"swb-hist-b\")"
        );
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
