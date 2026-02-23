//! Frame/display property builtins.
//!
//! Provides stub implementations for display and terminal query functions.
//! Since Neomacs is always a GUI application, most display queries return
//! sensible defaults for a modern graphical display.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use crate::window::{FrameId, WindowId};
use std::cell::RefCell;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

thread_local! {
    static TERMINAL_PARAMS: RefCell<Vec<(Value, Value)>> = const { RefCell::new(Vec::new()) };
    static TERMINAL_HANDLE: Arc<Mutex<Vec<Value>>> =
        Arc::new(Mutex::new(vec![Value::symbol("--neovm-terminal--")]));
    static CURSOR_VISIBLE_WINDOWS: RefCell<Vec<(u64, bool)>> = const { RefCell::new(Vec::new()) };
}

const TERMINAL_NAME: &str = "initial_terminal";
const TERMINAL_ID: u64 = 0;
static CURSOR_VISIBLE: AtomicBool = AtomicBool::new(true);

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

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

fn expect_range_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_symbol_key(value: &Value) -> Result<Value, Flow> {
    match value {
        Value::Nil | Value::True | Value::Symbol(_) | Value::Keyword(_) => Ok(value.clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), other.clone()],
        )),
    }
}

fn dynamic_or_global_symbol_value(eval: &super::eval::Evaluator, name: &str) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(v) = frame.get(name) {
            return Some(v.clone());
        }
    }
    eval.obarray.symbol_value(name).cloned()
}

fn terminal_parameter_default_value(key: &Value) -> Option<Value> {
    match key.as_symbol_name() {
        Some("normal-erase-is-backspace") => Some(Value::Int(0)),
        Some("keyboard-coding-saved-meta-mode") => Some(Value::list(vec![Value::True])),
        _ => None,
    }
}

fn preserve_emacs_downcase_payload(code: i64) -> bool {
    matches!(
        code,
        304
            | 7305
            | 8490
            | 42955
            | 42956
            | 42958
            | 42962
            | 42964
            | 42970
            | 42972
            | 68944..=68965
            | 93856..=93880
    )
}

fn downcase_char_code(code: i64) -> i64 {
    if preserve_emacs_downcase_payload(code) {
        return code;
    }
    if let Some(c) = u32::try_from(code).ok().and_then(char::from_u32) {
        c.to_lowercase().next().unwrap_or(c) as i64
    } else {
        code
    }
}

fn terminal_parameter_default_entries() -> Vec<(Value, Value)> {
    vec![
        (Value::symbol("normal-erase-is-backspace"), Value::Int(0)),
        (
            Value::symbol("keyboard-coding-saved-meta-mode"),
            Value::list(vec![Value::True]),
        ),
    ]
}

fn lookup_terminal_parameter_value(params: &[(Value, Value)], key: &Value) -> Value {
    params
        .iter()
        .find_map(|(stored_key, stored_value)| {
            if eq_value(stored_key, key) {
                Some(stored_value.clone())
            } else {
                None
            }
        })
        .or_else(|| terminal_parameter_default_value(key))
        .unwrap_or(Value::Nil)
}

fn terminal_parameters_with_defaults(params: &[(Value, Value)]) -> Vec<(Value, Value)> {
    let mut merged = terminal_parameter_default_entries();
    for (key, value) in params {
        if let Some((_, existing_value)) = merged
            .iter_mut()
            .find(|(existing_key, _)| eq_value(existing_key, key))
        {
            *existing_value = value.clone();
        } else {
            merged.push((key.clone(), value.clone()));
        }
    }
    merged
}

fn invalid_get_device_terminal_error(value: &Value) -> Flow {
    signal(
        "error",
        vec![Value::string(format!(
            "Invalid argument {} in ‘get-device-terminal’",
            super::print::print_value(value)
        ))],
    )
}

fn display_does_not_exist_error(display: &str) -> Flow {
    signal(
        "error",
        vec![Value::string(format!("Display {display} does not exist"))],
    )
}

fn format_get_device_terminal_arg_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    let window_id = match value {
        Value::Window(id) => Some(WindowId(*id)),
        _ => None,
    };

    if let Some(window_id) = window_id {
        if let Some(frame_id) = eval.frames.find_window_frame_id(window_id) {
            if let Some(frame) = eval.frames.get(frame_id) {
                if let Some(window) = frame.find_window(window_id) {
                    if let Some(buffer_id) = window.buffer_id() {
                        if let Some(buffer) = eval.buffers.get(buffer_id) {
                            return format!("#<window {} on {}>", window_id.0, buffer.name);
                        }
                    }
                    return format!("#<window {} on {}>", window_id.0, frame.name);
                }
            }
        }
    }

    super::print::print_value(value)
}

fn invalid_get_device_terminal_error_eval(eval: &super::eval::Evaluator, value: &Value) -> Flow {
    signal(
        "error",
        vec![Value::string(format!(
            "Invalid argument {} in ‘get-device-terminal’",
            format_get_device_terminal_arg_eval(eval, value)
        ))],
    )
}

fn terminal_not_x_display_error(value: &Value) -> Option<Flow> {
    terminal_handle_id(value).map(|id| {
        signal(
            "error",
            vec![Value::string(format!("Terminal {id} is not an X display"))],
        )
    })
}

fn terminal_designator_p(value: &Value) -> bool {
    value.is_nil() || is_terminal_handle(value)
}

fn terminal_designator_eval_p(eval: &mut super::eval::Evaluator, value: &Value) -> bool {
    terminal_designator_p(value) || live_frame_designator_p(eval, value)
}

fn expect_terminal_designator_eval(
    eval: &mut super::eval::Evaluator,
    value: &Value,
) -> Result<(), Flow> {
    if terminal_designator_eval_p(eval, value) {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("terminal-live-p"), value.clone()],
        ))
    }
}

fn expect_terminal_designator(value: &Value) -> Result<(), Flow> {
    if terminal_designator_p(value) {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("terminal-live-p"), value.clone()],
        ))
    }
}

fn expect_frame_designator(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Int(id) if *id >= 0 => Ok(()),
        Value::Frame(_) => Ok(()),
        v if v.is_nil() => Ok(()),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), value.clone()],
        )),
    }
}

fn expect_window_designator(value: &Value) -> Result<(), Flow> {
    if value.is_nil() {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("windowp"), value.clone()],
        ))
    }
}

fn live_window_designator_p(eval: &mut super::eval::Evaluator, value: &Value) -> bool {
    match value {
        Value::Window(id) => eval.frames.find_window_frame_id(WindowId(*id)).is_some(),
        Value::Int(id) if *id >= 0 => eval
            .frames
            .find_window_frame_id(WindowId(*id as u64))
            .is_some(),
        _ => false,
    }
}

fn expect_window_designator_eval(
    eval: &mut super::eval::Evaluator,
    value: &Value,
) -> Result<(), Flow> {
    if value.is_nil() || live_window_designator_p(eval, value) {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("windowp"), value.clone()],
        ))
    }
}

fn window_id_from_window_designator(value: &Value) -> Option<WindowId> {
    match value {
        Value::Window(id) => Some(WindowId(*id)),
        Value::Int(id) if *id >= 0 => Some(WindowId(*id as u64)),
        _ => None,
    }
}

fn selected_window_id(eval: &mut super::eval::Evaluator) -> Option<WindowId> {
    let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(eval);
    eval.frames.get(frame_id).map(|frame| frame.selected_window)
}

fn resolve_internal_show_cursor_window_id(
    eval: &mut super::eval::Evaluator,
    value: &Value,
) -> Option<WindowId> {
    if value.is_nil() {
        selected_window_id(eval)
    } else {
        window_id_from_window_designator(value)
    }
}

fn set_window_cursor_visible(window_id: WindowId, visible: bool) {
    CURSOR_VISIBLE_WINDOWS.with(|slot| {
        let mut states = slot.borrow_mut();
        if let Some((_, existing)) = states
            .iter_mut()
            .find(|(stored_window_id, _)| *stored_window_id == window_id.0)
        {
            *existing = visible;
        } else {
            states.push((window_id.0, visible));
        }
    });
}

fn window_cursor_visible(window_id: WindowId) -> bool {
    CURSOR_VISIBLE_WINDOWS.with(|slot| {
        slot.borrow()
            .iter()
            .find_map(|(stored_window_id, visible)| {
                if *stored_window_id == window_id.0 {
                    Some(*visible)
                } else {
                    None
                }
            })
            .unwrap_or(true)
    })
}

fn expect_display_designator(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Nil => Ok(()),
        display if terminal_designator_p(display) => Ok(()),
        Value::Str(display) => Err(display_does_not_exist_error(display)),
        _ => Err(invalid_get_device_terminal_error(value)),
    }
}

fn live_frame_designator_p(eval: &mut super::eval::Evaluator, value: &Value) -> bool {
    match value {
        Value::Int(id) if *id >= 0 => eval.frames.get(FrameId(*id as u64)).is_some(),
        Value::Frame(id) => eval.frames.get(FrameId(*id)).is_some(),
        _ => false,
    }
}

fn expect_display_designator_eval(
    eval: &mut super::eval::Evaluator,
    value: &Value,
) -> Result<(), Flow> {
    if value.is_nil() || terminal_designator_p(value) || live_frame_designator_p(eval, value) {
        return Ok(());
    }
    if let Value::Str(display) = value {
        return Err(display_does_not_exist_error(display));
    }
    Err(invalid_get_device_terminal_error_eval(eval, value))
}

fn expect_optional_display_designator_eval(
    eval: &mut super::eval::Evaluator,
    name: &str,
    args: &[Value],
) -> Result<(), Flow> {
    expect_max_args(name, args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator_eval(eval, display)?;
    }
    Ok(())
}

fn terminal_handle_value() -> Value {
    TERMINAL_HANDLE.with(|handle| Value::Vector(handle.clone()))
}

fn is_terminal_handle(value: &Value) -> bool {
    match value {
        Value::Vector(v) => TERMINAL_HANDLE.with(|handle| Arc::ptr_eq(v, handle)),
        _ => false,
    }
}

pub(crate) fn terminal_handle_id(value: &Value) -> Option<u64> {
    if is_terminal_handle(value) {
        Some(TERMINAL_ID)
    } else {
        None
    }
}

pub(crate) fn print_terminal_handle(value: &Value) -> Option<String> {
    terminal_handle_id(value).map(|id| format!("#<terminal {id} on {TERMINAL_NAME}>"))
}

// ---------------------------------------------------------------------------
// Helper: build an alist (association list) from key-value pairs
// ---------------------------------------------------------------------------

fn make_alist(pairs: Vec<(Value, Value)>) -> Value {
    let entries: Vec<Value> = pairs.into_iter().map(|(k, v)| Value::cons(k, v)).collect();
    Value::list(entries)
}

fn frame_not_live_error(value: &Value) -> Flow {
    let printable = match value {
        Value::Str(s) => s.to_string(),
        _ => super::print::print_value(value),
    };
    signal(
        "error",
        vec![Value::string(format!("{printable} is not a live frame"))],
    )
}

fn frame_not_live_error_eval(_eval: &super::eval::Evaluator, value: &Value) -> Flow {
    let printable = match value {
        Value::Str(s) => s.to_string(),
        _ => format_get_device_terminal_arg_eval(_eval, value),
    };
    signal(
        "error",
        vec![Value::string(format!("{printable} is not a live frame"))],
    )
}

fn x_windows_not_initialized_error() -> Flow {
    signal(
        "error",
        vec![Value::string("X windows are not in use or not initialized")],
    )
}

fn x_window_system_frame_error() -> Flow {
    signal(
        "error",
        vec![Value::string("Window system frame should be used")],
    )
}

fn x_selection_unavailable_error() -> Flow {
    signal(
        "error",
        vec![Value::string("X selection unavailable for this frame")],
    )
}

fn x_display_open_error(display: &str) -> Flow {
    signal(
        "error",
        vec![Value::string(format!("Display {display} can’t be opened"))],
    )
}

fn x_display_query_first_arg_error(value: &Value) -> Flow {
    match value {
        Value::Nil => x_windows_not_initialized_error(),
        Value::Str(display) => x_display_open_error(display),
        Value::Frame(_) => x_window_system_frame_error(),
        other => {
            if let Some(err) = terminal_not_x_display_error(other) {
                err
            } else {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), other.clone()],
                )
            }
        }
    }
}

fn window_system_not_initialized_error() -> Flow {
    signal(
        "error",
        vec![Value::string(
            "Window system is not in use or not initialized",
        )],
    )
}

fn expect_optional_window_system_frame_arg(value: &Value) -> Result<(), Flow> {
    if value.is_nil() || matches!(value, Value::Frame(_)) {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), value.clone()],
        ))
    }
}

fn parse_geometry_unsigned(bytes: &[u8], index: &mut usize) -> Option<i64> {
    let start = *index;
    while *index < bytes.len() && bytes[*index].is_ascii_digit() {
        *index += 1;
    }
    if *index == start {
        return None;
    }
    std::str::from_utf8(&bytes[start..*index])
        .ok()?
        .parse::<i64>()
        .ok()
}

fn parse_geometry_signed_offset(bytes: &[u8], index: &mut usize) -> Option<i64> {
    if *index >= bytes.len() {
        return None;
    }
    let sign = match bytes[*index] {
        b'+' => 1,
        b'-' => -1,
        _ => return None,
    };
    *index += 1;
    Some(sign * parse_geometry_unsigned(bytes, index)?)
}

fn parse_x_geometry(spec: &str) -> Option<Value> {
    let spec = spec.trim();
    if spec.is_empty() {
        return None;
    }

    let bytes = spec.as_bytes();
    let mut index = 0usize;
    if bytes[index] == b'=' {
        index += 1;
        if index >= bytes.len() {
            return None;
        }
    }

    let mut width = None;
    let mut height = None;
    let mut left = None;
    let mut top = None;

    let geometry_start = index;
    if let Some(parsed_width) = parse_geometry_unsigned(bytes, &mut index) {
        if index < bytes.len() && bytes[index] == b'x' {
            index += 1;
            let parsed_height = parse_geometry_unsigned(bytes, &mut index)?;
            width = Some(parsed_width);
            height = Some(parsed_height);
        } else {
            index = geometry_start;
        }
    } else if index < bytes.len() && bytes[index] == b'x' {
        return None;
    }

    if index < bytes.len() {
        let parsed_left = parse_geometry_signed_offset(bytes, &mut index)?;
        left = Some(parsed_left);
        if index < bytes.len() {
            let parsed_top = parse_geometry_signed_offset(bytes, &mut index)?;
            top = Some(parsed_top);
        }
    }

    if index != bytes.len() {
        return None;
    }

    if width.is_none() && height.is_none() && left.is_none() && top.is_none() {
        return None;
    }

    let mut pairs = Vec::new();
    if let Some(h) = height {
        pairs.push(Value::cons(Value::symbol("height"), Value::Int(h)));
    }
    if let Some(w) = width {
        pairs.push(Value::cons(Value::symbol("width"), Value::Int(w)));
    }
    if let Some(y) = top {
        pairs.push(Value::cons(Value::symbol("top"), Value::Int(y)));
    }
    if let Some(x) = left {
        pairs.push(Value::cons(Value::symbol("left"), Value::Int(x)));
    }
    Some(Value::list(pairs))
}

fn display_optional_capability_p(name: &str, args: &[Value]) -> EvalResult {
    expect_max_args(name, args, 1)?;
    match args.first() {
        None | Some(Value::Nil) => Ok(Value::Nil),
        Some(display) if is_terminal_handle(display) => Ok(Value::Nil),
        Some(Value::Str(display)) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} does not exist"))],
        )),
        Some(other) => Err(invalid_get_device_terminal_error(other)),
    }
}

fn display_optional_capability_p_eval(
    eval: &mut super::eval::Evaluator,
    name: &str,
    args: &[Value],
) -> EvalResult {
    expect_max_args(name, args, 1)?;
    match args.first() {
        None | Some(Value::Nil) => Ok(Value::Nil),
        Some(display) if is_terminal_handle(display) => Ok(Value::Nil),
        Some(display) if live_frame_designator_p(eval, display) => Ok(Value::Nil),
        Some(Value::Str(display)) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} does not exist"))],
        )),
        Some(other) => Err(invalid_get_device_terminal_error_eval(eval, other)),
    }
}

fn x_optional_display_query_error(name: &str, args: &[Value]) -> EvalResult {
    expect_max_args(name, args, 1)?;
    match args.first() {
        None | Some(Value::Nil) => Err(x_windows_not_initialized_error()),
        Some(display) if is_terminal_handle(display) => {
            if let Some(err) = terminal_not_x_display_error(display) {
                Err(err)
            } else {
                Err(invalid_get_device_terminal_error(display))
            }
        }
        Some(Value::Str(display)) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} can’t be opened"))],
        )),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), other.clone()],
        )),
    }
}

fn x_optional_display_query_error_eval(
    eval: &mut super::eval::Evaluator,
    name: &str,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args(name, &args, 1)?;
    if let Some(display) = args.first() {
        if live_frame_designator_p(eval, display) {
            return Err(x_window_system_frame_error());
        }
    }
    x_optional_display_query_error(name, &args)
}

// ---------------------------------------------------------------------------
// Display query builtins
// ---------------------------------------------------------------------------

/// (redraw-frame &optional FRAME) -> nil
pub(crate) fn builtin_redraw_frame(args: Vec<Value>) -> EvalResult {
    expect_range_args("redraw-frame", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_designator(frame)?;
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `redraw-frame`.
///
/// Accepts live frame designators in addition to nil.
pub(crate) fn builtin_redraw_frame_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("redraw-frame", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !live_frame_designator_p(eval, frame) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// (redraw-display) -> nil
pub(crate) fn builtin_redraw_display(args: Vec<Value>) -> EvalResult {
    expect_args("redraw-display", &args, 0)?;
    Ok(Value::Nil)
}

/// (open-termscript FILE) -> error
///
/// NeoVM does not support terminal script logging.
pub(crate) fn builtin_open_termscript(args: Vec<Value>) -> EvalResult {
    expect_args("open-termscript", &args, 1)?;
    Err(signal(
        "error",
        vec![Value::string("Current frame is not on a tty device")],
    ))
}

/// (ding &optional ARG) -> nil
pub(crate) fn builtin_ding(args: Vec<Value>) -> EvalResult {
    expect_range_args("ding", &args, 0, 1)?;
    Ok(Value::Nil)
}

/// (send-string-to-terminal STRING &optional TERMINAL) -> nil
pub(crate) fn builtin_send_string_to_terminal(args: Vec<Value>) -> EvalResult {
    expect_range_args("send-string-to-terminal", &args, 1, 2)?;
    match &args[0] {
        Value::Str(_) => {
            if let Some(terminal) = args.get(1) {
                expect_terminal_designator(terminal)?;
            }
            Ok(Value::Nil)
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// Evaluator-aware variant of `send-string-to-terminal`.
///
/// Accepts live frame designators for the optional TERMINAL argument.
pub(crate) fn builtin_send_string_to_terminal_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("send-string-to-terminal", &args, 1, 2)?;
    match &args[0] {
        Value::Str(_) => {
            if let Some(terminal) = args.get(1) {
                expect_terminal_designator_eval(eval, terminal)?;
            }
            Ok(Value::Nil)
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// (internal-show-cursor WINDOW SHOW) -> nil
pub(crate) fn builtin_internal_show_cursor(args: Vec<Value>) -> EvalResult {
    expect_args("internal-show-cursor", &args, 2)?;
    expect_window_designator(&args[0])?;
    CURSOR_VISIBLE.store(!args[1].is_nil(), Ordering::Relaxed);
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `internal-show-cursor`.
///
/// Accepts live window designators in addition to nil.
pub(crate) fn builtin_internal_show_cursor_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("internal-show-cursor", &args, 2)?;
    expect_window_designator_eval(eval, &args[0])?;
    let visible = !args[1].is_nil();
    if let Some(window_id) = resolve_internal_show_cursor_window_id(eval, &args[0]) {
        set_window_cursor_visible(window_id, visible);
    } else {
        CURSOR_VISIBLE.store(visible, Ordering::Relaxed);
    }
    Ok(Value::Nil)
}

/// (internal-show-cursor-p &optional WINDOW) -> t/nil
pub(crate) fn builtin_internal_show_cursor_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("internal-show-cursor-p", &args, 0, 1)?;
    if let Some(window) = args.first() {
        expect_window_designator(window)?;
    }
    Ok(Value::bool(CURSOR_VISIBLE.load(Ordering::Relaxed)))
}

/// Evaluator-aware variant of `internal-show-cursor-p`.
///
/// Accepts live window designators in addition to nil.
pub(crate) fn builtin_internal_show_cursor_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("internal-show-cursor-p", &args, 0, 1)?;
    if let Some(window) = args.first() {
        expect_window_designator_eval(eval, window)?;
    }
    let query_window = args.first().unwrap_or(&Value::Nil);
    if let Some(window_id) = resolve_internal_show_cursor_window_id(eval, query_window) {
        return Ok(Value::bool(window_cursor_visible(window_id)));
    }
    Ok(Value::bool(CURSOR_VISIBLE.load(Ordering::Relaxed)))
}

/// (display-graphic-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_graphic_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-graphic-p", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Nil)
}

/// (display-color-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_color_p(args: Vec<Value>) -> EvalResult {
    display_optional_capability_p("display-color-p", &args)
}

/// (display-grayscale-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_grayscale_p(args: Vec<Value>) -> EvalResult {
    display_optional_capability_p("display-grayscale-p", &args)
}

/// (display-mouse-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_mouse_p(args: Vec<Value>) -> EvalResult {
    display_optional_capability_p("display-mouse-p", &args)
}

/// (display-popup-menus-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_popup_menus_p(args: Vec<Value>) -> EvalResult {
    display_optional_capability_p("display-popup-menus-p", &args)
}

/// (display-symbol-keys-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_symbol_keys_p(args: Vec<Value>) -> EvalResult {
    display_optional_capability_p("display-symbol-keys-p", &args)
}

/// (display-pixel-width &optional DISPLAY) -> 80 (terminal columns in batch).
pub(crate) fn builtin_display_pixel_width(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-pixel-width", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Int(80))
}

/// (display-pixel-height &optional DISPLAY) -> 25 (terminal rows in batch).
pub(crate) fn builtin_display_pixel_height(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-pixel-height", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Int(25))
}

/// (display-mm-width &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_mm_width(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-mm-width", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Nil)
}

/// (display-mm-height &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_mm_height(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-mm-height", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Nil)
}

/// (display-screens &optional DISPLAY) -> 1
pub(crate) fn builtin_display_screens(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-screens", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Int(1))
}

/// (display-color-cells &optional DISPLAY) -> 0 in batch-style vm context.
pub(crate) fn builtin_display_color_cells(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-color-cells", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Int(0))
}

/// (display-planes &optional DISPLAY) -> 3 in batch-style vm context.
pub(crate) fn builtin_display_planes(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-planes", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Int(3))
}

/// (display-visual-class &optional DISPLAY) -> 'static-gray in batch-style vm context.
pub(crate) fn builtin_display_visual_class(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-visual-class", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::symbol("static-gray"))
}

/// (display-backing-store &optional DISPLAY) -> 'not-useful in batch-style vm context.
pub(crate) fn builtin_display_backing_store(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-backing-store", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::symbol("not-useful"))
}

/// (display-save-under &optional DISPLAY) -> 'not-useful in batch-style vm context.
pub(crate) fn builtin_display_save_under(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-save-under", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::symbol("not-useful"))
}

/// (display-selections-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_selections_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-selections-p", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Nil)
}

/// (display-images-p &optional DISPLAY) -> nil in batch-style vm context.
pub(crate) fn builtin_display_images_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-images-p", &args, 1)?;
    if let Some(display) = args.first() {
        expect_display_designator(display)?;
    }
    Ok(Value::Nil)
}

/// (display-supports-face-attributes-p ATTRIBUTES &optional DISPLAY) -> nil
/// in batch-style vm context.
pub(crate) fn builtin_display_supports_face_attributes_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("display-supports-face-attributes-p", &args, 1, 2)?;
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `display-graphic-p`.
pub(crate) fn builtin_display_graphic_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-graphic-p", &args)?;
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `display-color-p`.
pub(crate) fn builtin_display_color_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    display_optional_capability_p_eval(eval, "display-color-p", &args)
}

/// Evaluator-aware variant of `display-grayscale-p`.
pub(crate) fn builtin_display_grayscale_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    display_optional_capability_p_eval(eval, "display-grayscale-p", &args)
}

/// Evaluator-aware variant of `display-mouse-p`.
pub(crate) fn builtin_display_mouse_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    display_optional_capability_p_eval(eval, "display-mouse-p", &args)
}

/// Evaluator-aware variant of `display-popup-menus-p`.
pub(crate) fn builtin_display_popup_menus_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    display_optional_capability_p_eval(eval, "display-popup-menus-p", &args)
}

/// Evaluator-aware variant of `display-symbol-keys-p`.
pub(crate) fn builtin_display_symbol_keys_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    display_optional_capability_p_eval(eval, "display-symbol-keys-p", &args)
}

/// Evaluator-aware variant of `display-pixel-width`.
pub(crate) fn builtin_display_pixel_width_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-pixel-width", &args)?;
    Ok(Value::Int(80))
}

/// Evaluator-aware variant of `display-pixel-height`.
pub(crate) fn builtin_display_pixel_height_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-pixel-height", &args)?;
    Ok(Value::Int(25))
}

/// Evaluator-aware variant of `display-mm-width`.
pub(crate) fn builtin_display_mm_width_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-mm-width", &args)?;
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `display-mm-height`.
pub(crate) fn builtin_display_mm_height_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-mm-height", &args)?;
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `display-screens`.
pub(crate) fn builtin_display_screens_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-screens", &args)?;
    Ok(Value::Int(1))
}

/// Evaluator-aware variant of `display-color-cells`.
pub(crate) fn builtin_display_color_cells_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-color-cells", &args)?;
    Ok(Value::Int(0))
}

/// Evaluator-aware variant of `display-planes`.
pub(crate) fn builtin_display_planes_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-planes", &args)?;
    Ok(Value::Int(3))
}

/// Evaluator-aware variant of `display-visual-class`.
pub(crate) fn builtin_display_visual_class_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-visual-class", &args)?;
    Ok(Value::symbol("static-gray"))
}

/// Evaluator-aware variant of `display-backing-store`.
pub(crate) fn builtin_display_backing_store_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-backing-store", &args)?;
    Ok(Value::symbol("not-useful"))
}

/// Evaluator-aware variant of `display-save-under`.
pub(crate) fn builtin_display_save_under_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-save-under", &args)?;
    Ok(Value::symbol("not-useful"))
}

/// Evaluator-aware variant of `display-selections-p`.
pub(crate) fn builtin_display_selections_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-selections-p", &args)?;
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `window-system`.
pub(crate) fn builtin_window_system_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("window-system", &args, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !live_frame_designator_p(eval, frame) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("framep"), frame.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// (frame-edges &optional FRAME TYPE) -> `(0 0 80 25)` in batch-style vm context.
pub(crate) fn builtin_frame_edges(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-edges", &args, 0, 2)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() {
            return Err(frame_not_live_error(frame));
        }
    }
    Ok(Value::list(vec![
        Value::Int(0),
        Value::Int(0),
        Value::Int(80),
        Value::Int(25),
    ]))
}

/// Evaluator-aware variant of `frame-edges`.
pub(crate) fn builtin_frame_edges_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("frame-edges", &args, 0, 2)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !live_frame_designator_p(eval, frame) {
            return Err(frame_not_live_error_eval(eval, frame));
        }
    }
    Ok(Value::list(vec![
        Value::Int(0),
        Value::Int(0),
        Value::Int(80),
        Value::Int(25),
    ]))
}

/// Evaluator-aware variant of `display-images-p`.
pub(crate) fn builtin_display_images_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-images-p", &args)?;
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `display-supports-face-attributes-p`.
///
/// Emacs accepts broad argument shapes here in batch mode and still returns
/// nil as long as arity is valid.
pub(crate) fn builtin_display_supports_face_attributes_p_eval(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("display-supports-face-attributes-p", &args, 1, 2)?;
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// X display builtins (compatibility stubs)
// ---------------------------------------------------------------------------

/// (x-display-list) -> nil in batch-style vm context.
pub(crate) fn builtin_x_display_list(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-display-list", &args, 0)?;
    Ok(Value::Nil)
}

/// (x-frame-edges &optional FRAME TYPE) -> nil in batch/no-X context.
pub(crate) fn builtin_x_frame_edges(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-frame-edges", &args, 2)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !matches!(frame, Value::Frame(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// (x-frame-geometry &optional FRAME) -> nil in batch/no-X context.
pub(crate) fn builtin_x_frame_geometry(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-frame-geometry", &args, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !matches!(frame, Value::Frame(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// (x-frame-list-z-order &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_frame_list_z_order(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-frame-list-z-order", &args, 1)?;
    match args.first() {
        None => Err(x_windows_not_initialized_error()),
        Some(display) => Err(x_display_query_first_arg_error(display)),
    }
}

/// (x-frame-restack FRAME1 FRAME2 &optional ABOVE) -> error in batch/no-X context.
///
/// Oracle batch behavior crashes on valid-arity runtime calls in this
/// environment, so we only expose arity/fboundp compatibility surface and a
/// conservative batch/no-X error result.
pub(crate) fn builtin_x_frame_restack(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-frame-restack", &args, 2, 3)?;
    Err(x_window_system_frame_error())
}

/// (x-mouse-absolute-pixel-position) -> nil in batch/no-X context.
pub(crate) fn builtin_x_mouse_absolute_pixel_position(args: Vec<Value>) -> EvalResult {
    expect_args("x-mouse-absolute-pixel-position", &args, 0)?;
    Ok(Value::Nil)
}

/// (x-set-mouse-absolute-pixel-position X Y) -> nil in batch/no-X context.
pub(crate) fn builtin_x_set_mouse_absolute_pixel_position(args: Vec<Value>) -> EvalResult {
    expect_args("x-set-mouse-absolute-pixel-position", &args, 2)?;
    Ok(Value::Nil)
}

/// (x-send-client-message DISPLAY PROP VALUE-0 VALUE-1 VALUE-2 VALUE-3) -> error in batch/no-X context.
pub(crate) fn builtin_x_send_client_message(args: Vec<Value>) -> EvalResult {
    expect_args("x-send-client-message", &args, 6)?;
    Err(x_display_query_first_arg_error(&args[0]))
}

/// (x-popup-dialog POSITION CONTENTS &optional HEADER) -> nil/error in batch context.
pub(crate) fn builtin_x_popup_dialog(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-popup-dialog", &args, 2, 3)?;

    if !matches!(args[0], Value::Frame(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("windowp"), Value::Nil],
        ));
    }

    let contents = &args[1];
    if contents.is_nil() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), Value::Nil],
        ));
    }

    let (title, rest) = match contents {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other.clone()],
            ))
        }
    };

    if !title.is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), title.clone()],
        ));
    }

    if !rest.is_cons() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), rest],
        ));
    }

    Ok(Value::Nil)
}

/// (x-popup-menu POSITION MENU) -> nil/error in batch context.
pub(crate) fn builtin_x_popup_menu(args: Vec<Value>) -> EvalResult {
    expect_args("x-popup-menu", &args, 2)?;
    let position = &args[0];
    let menu = &args[1];

    if position.is_nil() {
        return Ok(Value::Nil);
    }

    let (position_car, position_cdr) = match position {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other.clone()],
            ))
        }
    };

    if !position_car.is_list() {
        if matches!(position_car, Value::Int(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), position_car],
            ));
        }
        if menu.is_nil() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), Value::Nil],
            ));
        }
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), Value::True],
        ));
    }

    if !position_cdr.is_list() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), position_cdr],
        ));
    }

    if !position_car.is_nil() {
        let window_designator = match position_cdr {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                pair.car.clone()
            }
            _ => Value::Nil,
        };
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("windowp"), window_designator],
        ));
    }

    // This follows the menu descriptor shape expected by batch-mode oracle:
    // MENU = (TITLE . REST), REST either nil or (PANE . _)
    // PANE = (PANE-TITLE . PANE-ITEMS)
    if menu.is_nil() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), Value::Nil],
        ));
    }

    let (title, rest) = match menu {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other.clone()],
            ))
        }
    };

    if !title.is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), title],
        ));
    }

    if rest.is_nil() {
        return Ok(Value::Nil);
    }

    let pane = match rest {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            pair.car.clone()
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other],
            ))
        }
    };

    let (pane_title, pane_items) = match pane {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        }
        Value::Nil => (Value::Nil, Value::Nil),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other],
            ))
        }
    };

    if !pane_title.is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), pane_title],
        ));
    }

    if !pane_items.is_cons() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), pane_items],
        ));
    }

    Ok(Value::Nil)
}

/// (x-synchronize DISPLAY &optional NO-OP) -> error in batch/no-X context.
pub(crate) fn builtin_x_synchronize(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-synchronize", &args, 1, 2)?;
    Err(x_windows_not_initialized_error())
}

/// (x-translate-coordinates DISPLAY X Y &optional FRAME SOURCE-FRAME) -> error in batch/no-X context.
pub(crate) fn builtin_x_translate_coordinates(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-translate-coordinates", &args, 1, 6)?;
    Err(x_display_query_first_arg_error(&args[0]))
}

/// (x-register-dnd-atom ATOM &optional OLD-ATOM) -> error in batch/no-X context.
pub(crate) fn builtin_x_register_dnd_atom(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-register-dnd-atom", &args, 1, 2)?;
    Err(x_window_system_frame_error())
}

/// (x-export-frames &optional FRAME TYPE) -> error in batch/no-X context.
pub(crate) fn builtin_x_export_frames(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-export-frames", &args, 2)?;
    match args.first() {
        None => Err(x_window_system_frame_error()),
        Some(frame) if frame.is_nil() || matches!(frame, Value::Frame(_)) => {
            Err(x_window_system_frame_error())
        }
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), other.clone()],
        )),
    }
}

/// (x-focus-frame FRAME &optional NO-ACTIVATE) -> error in batch/no-X context.
pub(crate) fn builtin_x_focus_frame(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-focus-frame", &args, 1, 2)?;
    let frame = &args[0];
    if frame.is_nil() || matches!(frame, Value::Frame(_)) {
        Err(x_window_system_frame_error())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), frame.clone()],
        ))
    }
}

/// (x-get-clipboard) -> nil in batch/no-X context.
pub(crate) fn builtin_x_get_clipboard(args: Vec<Value>) -> EvalResult {
    expect_args("x-get-clipboard", &args, 0)?;
    Ok(Value::Nil)
}

/// (x-get-modifier-masks &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_get_modifier_masks(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-get-modifier-masks", &args, 1)?;
    match args.first() {
        None => Err(x_windows_not_initialized_error()),
        Some(display) if display.is_nil() => Err(x_windows_not_initialized_error()),
        Some(Value::Frame(_)) => Err(x_window_system_frame_error()),
        Some(display) => Err(x_display_query_first_arg_error(display)),
    }
}

/// (x-get-input-coding-system NAME) -> nil/error in batch/no-X context.
pub(crate) fn builtin_x_get_input_coding_system(args: Vec<Value>) -> EvalResult {
    expect_args("x-get-input-coding-system", &args, 1)?;
    match &args[0] {
        Value::Str(_) => Ok(Value::Nil),
        Value::Int(code) => {
            if *code < 0 {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), Value::Int(*code)],
                ))
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), Value::Int(downcase_char_code(*code))],
                ))
            }
        }
        Value::Char(code) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), Value::Int(downcase_char_code(*code as i64))],
        )),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), other.clone()],
        )),
    }
}

/// (x-hide-tip) -> nil in batch/no-X context.
pub(crate) fn builtin_x_hide_tip(args: Vec<Value>) -> EvalResult {
    expect_args("x-hide-tip", &args, 0)?;
    Ok(Value::Nil)
}

/// (x-show-tip STRING &optional FRAME PARMS TIMEOUT DX DY) -> error in batch/no-X context.
pub(crate) fn builtin_x_show_tip(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-show-tip", &args, 1, 6)?;
    if !args[0].is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0].clone()],
        ));
    }
    Err(x_window_system_frame_error())
}

/// (x-setup-function-keys TERMINAL) -> nil/error in batch/no-X context.
pub(crate) fn builtin_x_setup_function_keys(args: Vec<Value>) -> EvalResult {
    expect_args("x-setup-function-keys", &args, 1)?;
    match &args[0] {
        Value::Frame(_) => Ok(Value::Nil),
        Value::Int(_) | Value::Str(_) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("terminal-live-p"), args[0].clone()],
        )),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), other.clone()],
        )),
    }
}

/// (x-clear-preedit-text) -> 'tooltip-hide in batch/no-X context.
pub(crate) fn builtin_x_clear_preedit_text(args: Vec<Value>) -> EvalResult {
    expect_args("x-clear-preedit-text", &args, 0)?;
    Ok(Value::list(vec![Value::symbol("tooltip-hide")]))
}

/// (x-preedit-text ARG) -> nil/error in batch/no-X context.
pub(crate) fn builtin_x_preedit_text(args: Vec<Value>) -> EvalResult {
    expect_args("x-preedit-text", &args, 1)?;
    let arg = &args[0];
    if arg.is_nil() {
        return Ok(Value::Nil);
    }

    let rest = match arg {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            pair.cdr.clone()
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other.clone()],
            ))
        }
    };

    if !rest.is_list() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), rest],
        ));
    }

    if let Value::Cons(cell) = rest {
        let pair = cell.lock().expect("poisoned");
        let second = pair.car.clone();
        if !second.is_nil() && !second.is_string() && !second.is_list() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), second],
            ));
        }
    }

    Ok(Value::Nil)
}

/// (x-win-suspend-error) -> nil in batch/no-X context.
pub(crate) fn builtin_x_win_suspend_error(args: Vec<Value>) -> EvalResult {
    expect_args("x-win-suspend-error", &args, 0)?;
    Ok(Value::Nil)
}

/// (x-device-class DISPLAY) -> nil/error in batch/no-X context.
pub(crate) fn builtin_x_device_class(args: Vec<Value>) -> EvalResult {
    expect_args("x-device-class", &args, 1)?;
    match &args[0] {
        Value::Nil | Value::Str(_) => Ok(Value::Nil),
        Value::Int(_) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0].clone()],
        )),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), other.clone()],
        )),
    }
}

/// (x-internal-focus-input-context FRAME) -> nil in batch/no-X context.
pub(crate) fn builtin_x_internal_focus_input_context(args: Vec<Value>) -> EvalResult {
    expect_args("x-internal-focus-input-context", &args, 1)?;
    Ok(Value::Nil)
}

/// (x-wm-set-size-hint &optional FRAME) -> error in batch/no-X context.
pub(crate) fn builtin_x_wm_set_size_hint(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-wm-set-size-hint", &args, 1)?;
    match args.first() {
        None => Err(x_window_system_frame_error()),
        Some(frame) if frame.is_nil() => Err(x_window_system_frame_error()),
        Some(Value::Frame(_)) => Err(x_window_system_frame_error()),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), other.clone()],
        )),
    }
}

/// (x-backspace-delete-keys-p &optional FRAME) -> error in batch/no-X context.
pub(crate) fn builtin_x_backspace_delete_keys_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-backspace-delete-keys-p", &args, 1)?;
    if let Some(frame) = args.first() {
        expect_optional_window_system_frame_arg(frame)?;
    }
    Err(x_window_system_frame_error())
}

/// (x-family-fonts &optional FAMILY FRAME) -> nil in batch/no-X context.
pub(crate) fn builtin_x_family_fonts(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-family-fonts", &args, 2)?;
    if let Some(frame) = args.get(1) {
        expect_optional_window_system_frame_arg(frame)?;
    }
    if let Some(family) = args.first() {
        if !family.is_nil() && !family.is_string() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), family.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// (x-get-atom-name ATOM &optional FRAME) -> error in batch/no-X context.
pub(crate) fn builtin_x_get_atom_name(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-get-atom-name", &args, 1, 2)?;
    if let Some(frame) = args.get(1) {
        expect_optional_window_system_frame_arg(frame)?;
    }
    Err(x_window_system_frame_error())
}

/// (x-get-resource ATTRIBUTE CLASS &optional COMPONENT SUBCLASS) -> error in batch/no-X context.
pub(crate) fn builtin_x_get_resource(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-get-resource", &args, 2, 4)?;
    Err(window_system_not_initialized_error())
}

/// (x-apply-session-resources) -> error in batch/no-X context.
pub(crate) fn builtin_x_apply_session_resources(args: Vec<Value>) -> EvalResult {
    expect_args("x-apply-session-resources", &args, 0)?;
    Err(window_system_not_initialized_error())
}

/// (x-clipboard-yank) -> error in batch/no-X context.
pub(crate) fn builtin_x_clipboard_yank(args: Vec<Value>) -> EvalResult {
    expect_args("x-clipboard-yank", &args, 0)?;
    Err(signal("error", vec![Value::string("Kill ring is empty")]))
}

/// Evaluator-aware variant of `x-clipboard-yank`.
///
/// In batch/no-X context this mirrors Oracle behavior:
/// - empty kill-ring -> `(error "Kill ring is empty")`
/// - non-empty list with string/buffer head -> `nil`
/// - non-list kill-ring payload contracts -> `wrong-type-argument` shape
pub(crate) fn builtin_x_clipboard_yank_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("x-clipboard-yank", &args, 0)?;
    let kill_ring = dynamic_or_global_symbol_value(eval, "kill-ring").unwrap_or(Value::Nil);
    match kill_ring {
        Value::Nil => Err(signal("error", vec![Value::string("Kill ring is empty")])),
        Value::Cons(cell) => {
            let head = {
                let pair = cell.lock().expect("poisoned");
                pair.car.clone()
            };
            match head {
                Value::Str(_) | Value::Buffer(_) => Ok(Value::Nil),
                other => Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("buffer-or-string-p"), other],
                )),
            }
        }
        Value::Str(_) | Value::Vector(_) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), kill_ring],
        )),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other],
        )),
    }
}

/// (x-list-fonts PATTERN &optional FACE FRAME MAXIMUM WIDTH) -> error in batch/no-X context.
pub(crate) fn builtin_x_list_fonts(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-list-fonts", &args, 1, 5)?;
    Err(window_system_not_initialized_error())
}

/// (x-parse-geometry STRING) -> alist or nil.
pub(crate) fn builtin_x_parse_geometry(args: Vec<Value>) -> EvalResult {
    expect_args("x-parse-geometry", &args, 1)?;
    match &args[0] {
        Value::Str(spec) => Ok(parse_x_geometry(spec).unwrap_or(Value::Nil)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// (x-change-window-property PROPERTY VALUE &optional FRAME TYPE FORMAT OUTER-P DELETE-P)
/// -> error in batch/no-X context.
pub(crate) fn builtin_x_change_window_property(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-change-window-property", &args, 2, 7)?;
    if let Some(frame) = args.get(2) {
        expect_optional_window_system_frame_arg(frame)?;
    }
    Err(x_window_system_frame_error())
}

/// (x-delete-window-property PROPERTY &optional FRAME TYPE) -> error in batch/no-X context.
pub(crate) fn builtin_x_delete_window_property(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-delete-window-property", &args, 1, 3)?;
    if let Some(frame) = args.get(1) {
        expect_optional_window_system_frame_arg(frame)?;
    }
    Err(x_window_system_frame_error())
}

/// (x-disown-selection-internal SELECTION &optional TYPE FRAME) -> nil.
pub(crate) fn builtin_x_disown_selection_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-disown-selection-internal", &args, 1, 3)?;
    Ok(Value::Nil)
}

/// (x-get-local-selection &optional SELECTION TYPE) -> nil/error.
pub(crate) fn builtin_x_get_local_selection(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-get-local-selection", &args, 2)?;
    let selection = args.first().cloned().unwrap_or(Value::Nil);
    if !selection.is_cons() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("consp"), selection],
        ));
    }
    Ok(Value::Nil)
}

/// (x-get-selection-internal SELECTION TYPE &optional DATA-TYPE FRAME)
/// -> error in batch/no-X context.
pub(crate) fn builtin_x_get_selection_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-get-selection-internal", &args, 2, 4)?;
    Err(x_selection_unavailable_error())
}

/// (x-own-selection-internal SELECTION VALUE &optional FRAME)
/// -> error in batch/no-X context.
pub(crate) fn builtin_x_own_selection_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-own-selection-internal", &args, 2, 3)?;
    Err(x_selection_unavailable_error())
}

/// (gui-get-selection &optional TYPE DATA-TYPE) -> nil.
pub(crate) fn builtin_gui_get_selection(args: Vec<Value>) -> EvalResult {
    expect_max_args("gui-get-selection", &args, 2)?;
    Ok(Value::Nil)
}

/// (gui-get-primary-selection) -> error in batch/no-X context.
pub(crate) fn builtin_gui_get_primary_selection(args: Vec<Value>) -> EvalResult {
    expect_args("gui-get-primary-selection", &args, 0)?;
    Err(signal(
        "error",
        vec![Value::string("No selection is available")],
    ))
}

/// (gui-select-text TEXT) -> nil.
pub(crate) fn builtin_gui_select_text(args: Vec<Value>) -> EvalResult {
    expect_args("gui-select-text", &args, 1)?;
    Ok(Value::Nil)
}

/// (gui-selection-value) -> nil.
pub(crate) fn builtin_gui_selection_value(args: Vec<Value>) -> EvalResult {
    expect_args("gui-selection-value", &args, 0)?;
    Ok(Value::Nil)
}

/// (gui-set-selection TYPE VALUE) -> nil.
pub(crate) fn builtin_gui_set_selection(args: Vec<Value>) -> EvalResult {
    expect_args("gui-set-selection", &args, 2)?;
    Ok(Value::Nil)
}

/// (x-selection-exists-p &optional SELECTION TYPE) -> nil in batch/no-X context.
pub(crate) fn builtin_x_selection_exists_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-selection-exists-p", &args, 2)?;
    if let Some(selection) = args.first() {
        if !selection.is_nil() {
            expect_symbol_key(selection)?;
        }
    }
    Ok(Value::Nil)
}

/// (x-selection-owner-p &optional SELECTION TYPE) -> nil in batch/no-X context.
pub(crate) fn builtin_x_selection_owner_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-selection-owner-p", &args, 2)?;
    if let Some(selection) = args.first() {
        if !selection.is_nil() {
            expect_symbol_key(selection)?;
        }
    }
    Ok(Value::Nil)
}

/// (x-uses-old-gtk-dialog) -> nil
pub(crate) fn builtin_x_uses_old_gtk_dialog(args: Vec<Value>) -> EvalResult {
    expect_args("x-uses-old-gtk-dialog", &args, 0)?;
    Ok(Value::Nil)
}

/// (x-window-property PROPERTY &optional FRAME TYPE DELETE-P VECTOR-RET-P) -> error in batch/no-X context.
pub(crate) fn builtin_x_window_property(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-window-property", &args, 1, 6)?;
    if let Some(frame) = args.get(1) {
        expect_optional_window_system_frame_arg(frame)?;
    }
    Err(x_window_system_frame_error())
}

/// (x-window-property-attributes PROPERTY &optional FRAME TYPE) -> error in batch/no-X context.
pub(crate) fn builtin_x_window_property_attributes(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-window-property-attributes", &args, 1, 3)?;
    if let Some(frame) = args.get(1) {
        expect_optional_window_system_frame_arg(frame)?;
    }
    Err(x_window_system_frame_error())
}

/// (x-server-version &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_server_version(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-server-version", &args)
}

/// Evaluator-aware variant of `x-server-version`.
pub(crate) fn builtin_x_server_version_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-server-version", args)
}

/// (x-server-max-request-size &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_server_max_request_size(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-server-max-request-size", &args)
}

/// Evaluator-aware variant of `x-server-max-request-size`.
pub(crate) fn builtin_x_server_max_request_size_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-server-max-request-size", args)
}

/// (x-display-grayscale-p &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_grayscale_p(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-grayscale-p", &args)
}

/// Evaluator-aware variant of `x-display-grayscale-p`.
pub(crate) fn builtin_x_display_grayscale_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-grayscale-p", args)
}

/// (x-display-backing-store &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_backing_store(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-backing-store", &args)
}

/// Evaluator-aware variant of `x-display-backing-store`.
pub(crate) fn builtin_x_display_backing_store_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-backing-store", args)
}

/// (x-display-color-cells &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_color_cells(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-color-cells", &args)
}

/// Evaluator-aware variant of `x-display-color-cells`.
pub(crate) fn builtin_x_display_color_cells_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-color-cells", args)
}

/// (x-display-mm-height &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_mm_height(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-mm-height", &args)
}

/// Evaluator-aware variant of `x-display-mm-height`.
pub(crate) fn builtin_x_display_mm_height_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-mm-height", args)
}

/// (x-display-mm-width &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_mm_width(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-mm-width", &args)
}

/// Evaluator-aware variant of `x-display-mm-width`.
pub(crate) fn builtin_x_display_mm_width_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-mm-width", args)
}

/// (x-display-monitor-attributes-list &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_monitor_attributes_list(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-monitor-attributes-list", &args)
}

/// Evaluator-aware variant of `x-display-monitor-attributes-list`.
pub(crate) fn builtin_x_display_monitor_attributes_list_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-monitor-attributes-list", args)
}

/// (x-display-planes &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_planes(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-planes", &args)
}

/// Evaluator-aware variant of `x-display-planes`.
pub(crate) fn builtin_x_display_planes_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-planes", args)
}

/// (x-display-save-under &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_save_under(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-save-under", &args)
}

/// Evaluator-aware variant of `x-display-save-under`.
pub(crate) fn builtin_x_display_save_under_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-save-under", args)
}

/// (x-display-screens &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_screens(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-screens", &args)
}

/// Evaluator-aware variant of `x-display-screens`.
pub(crate) fn builtin_x_display_screens_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-screens", args)
}

/// (x-display-visual-class &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_visual_class(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-display-visual-class", &args)
}

/// Evaluator-aware variant of `x-display-visual-class`.
pub(crate) fn builtin_x_display_visual_class_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-display-visual-class", args)
}

/// (x-server-input-extension-version &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_server_input_extension_version(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-server-input-extension-version", &args)
}

/// Evaluator-aware variant of `x-server-input-extension-version`.
pub(crate) fn builtin_x_server_input_extension_version_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-server-input-extension-version", args)
}

/// (x-server-vendor &optional DISPLAY) -> error in batch/no-X context.
pub(crate) fn builtin_x_server_vendor(args: Vec<Value>) -> EvalResult {
    x_optional_display_query_error("x-server-vendor", &args)
}

/// Evaluator-aware variant of `x-server-vendor`.
pub(crate) fn builtin_x_server_vendor_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    x_optional_display_query_error_eval(eval, "x-server-vendor", args)
}

/// (x-display-set-last-user-time DISPLAY USER-TIME) -> error in batch/no-X context.
pub(crate) fn builtin_x_display_set_last_user_time(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-display-set-last-user-time", &args, 1, 2)?;
    if matches!(args.get(1), Some(Value::Frame(_))) {
        return Err(x_window_system_frame_error());
    }
    let query_args: Vec<Value> = args.get(1).cloned().into_iter().collect();
    x_optional_display_query_error("x-display-set-last-user-time", &query_args)
}

/// Evaluator-aware variant of `x-display-set-last-user-time`.
///
/// In batch/no-X context, payload class follows USER-TIME argument designator
/// semantics, including live-frame and terminal handle message mapping.
pub(crate) fn builtin_x_display_set_last_user_time_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("x-display-set-last-user-time", &args, 1, 2)?;
    let query_args: Vec<Value> = args.get(1).cloned().into_iter().collect();
    x_optional_display_query_error_eval(eval, "x-display-set-last-user-time", query_args)
}

/// (x-open-connection DISPLAY &optional XRM-STRING MUST-SUCCEED) -> nil
/// In batch/no-X context this reports a display-open failure.
pub(crate) fn builtin_x_open_connection(args: Vec<Value>) -> EvalResult {
    expect_range_args("x-open-connection", &args, 1, 3)?;
    match &args[0] {
        Value::Str(display) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} can’t be opened"))],
        )),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// (x-close-connection DISPLAY) -> nil
/// In batch/no-X context this signals display/X availability errors.
pub(crate) fn builtin_x_close_connection(args: Vec<Value>) -> EvalResult {
    expect_args("x-close-connection", &args, 1)?;
    match &args[0] {
        Value::Nil => Err(signal(
            "error",
            vec![Value::string("X windows are not in use or not initialized")],
        )),
        Value::Str(display) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} can’t be opened"))],
        )),
        other => {
            if let Some(err) = terminal_not_x_display_error(other) {
                Err(err)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("frame-live-p"), other.clone()],
                ))
            }
        }
    }
}

/// Evaluator-aware variant of `x-close-connection`.
///
/// Live frame designators map to batch-compatible frame-class errors.
pub(crate) fn builtin_x_close_connection_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("x-close-connection", &args, 1)?;
    if let Some(display) = args.first() {
        if live_frame_designator_p(eval, display) {
            return Err(signal(
                "error",
                vec![Value::string("Window system frame should be used")],
            ));
        }
    }
    builtin_x_close_connection(args)
}

/// (x-display-pixel-width &optional TERMINAL)
///
/// Batch/no-X semantics: signal X-not-in-use, invalid frame designator, or
/// display-open failure depending on argument shape.
pub(crate) fn builtin_x_display_pixel_width(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-display-pixel-width", &args, 1)?;
    match args.first() {
        None | Some(Value::Nil) => Err(signal(
            "error",
            vec![Value::string("X windows are not in use or not initialized")],
        )),
        Some(display) if is_terminal_handle(display) => {
            if let Some(err) = terminal_not_x_display_error(display) {
                Err(err)
            } else {
                Err(invalid_get_device_terminal_error(display))
            }
        }
        Some(Value::Str(display)) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} can’t be opened"))],
        )),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), other.clone()],
        )),
    }
}

/// Evaluator-aware variant of `x-display-pixel-width`.
///
/// Accepts live frame designators and maps them to the same batch/no-X error
/// class as nil/current-display queries.
pub(crate) fn builtin_x_display_pixel_width_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("x-display-pixel-width", &args, 1)?;
    if let Some(display) = args.first() {
        if live_frame_designator_p(eval, display) {
            return Err(signal(
                "error",
                vec![Value::string("Window system frame should be used")],
            ));
        }
    }
    builtin_x_display_pixel_width(args)
}

/// (x-display-pixel-height &optional TERMINAL)
///
/// Batch/no-X semantics: signal X-not-in-use, invalid frame designator, or
/// display-open failure depending on argument shape.
pub(crate) fn builtin_x_display_pixel_height(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-display-pixel-height", &args, 1)?;
    match args.first() {
        None | Some(Value::Nil) => Err(signal(
            "error",
            vec![Value::string("X windows are not in use or not initialized")],
        )),
        Some(display) if is_terminal_handle(display) => {
            if let Some(err) = terminal_not_x_display_error(display) {
                Err(err)
            } else {
                Err(invalid_get_device_terminal_error(display))
            }
        }
        Some(Value::Str(display)) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} can’t be opened"))],
        )),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), other.clone()],
        )),
    }
}

/// Evaluator-aware variant of `x-display-pixel-height`.
///
/// Accepts live frame designators and maps them to the same batch/no-X error
/// class as nil/current-display queries.
pub(crate) fn builtin_x_display_pixel_height_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("x-display-pixel-height", &args, 1)?;
    if let Some(display) = args.first() {
        if live_frame_designator_p(eval, display) {
            return Err(signal(
                "error",
                vec![Value::string("Window system frame should be used")],
            ));
        }
    }
    builtin_x_display_pixel_height(args)
}

/// (x-display-color-p &optional TERMINAL)
///
/// Batch/no-X semantics: nil for current display, otherwise argument-shape
/// specific errors.
pub(crate) fn builtin_x_display_color_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("x-display-color-p", &args, 1)?;
    match args.first() {
        None | Some(Value::Nil) => Ok(Value::Nil),
        Some(display) if is_terminal_handle(display) => Ok(Value::Nil),
        Some(Value::Str(display)) => Err(signal(
            "error",
            vec![Value::string(format!("Display {display} does not exist"))],
        )),
        Some(other) => Err(invalid_get_device_terminal_error(other)),
    }
}

/// Evaluator-aware variant of `x-display-color-p`.
///
/// Live frame designators are treated as current display queries in batch mode.
pub(crate) fn builtin_x_display_color_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("x-display-color-p", &args, 1)?;
    if let Some(display) = args.first() {
        if live_frame_designator_p(eval, display) {
            return Ok(Value::Nil);
        }
    }
    builtin_x_display_color_p(args)
}

// ---------------------------------------------------------------------------
// Terminal builtins
// ---------------------------------------------------------------------------

/// (terminal-name &optional TERMINAL) -> "initial_terminal"
pub(crate) fn builtin_terminal_name(args: Vec<Value>) -> EvalResult {
    expect_max_args("terminal-name", &args, 1)?;
    if let Some(term) = args.first() {
        if !term.is_nil() {
            expect_terminal_designator(term)?;
        }
    }
    Ok(Value::string(TERMINAL_NAME))
}

/// Evaluator-aware variant of `terminal-name`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_terminal_name_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("terminal-name", &args, 1)?;
    if let Some(term) = args.first() {
        if !term.is_nil() {
            expect_terminal_designator_eval(eval, term)?;
        }
    }
    Ok(Value::string(TERMINAL_NAME))
}

/// (terminal-list) -> list containing one opaque terminal handle.
pub(crate) fn builtin_terminal_list(args: Vec<Value>) -> EvalResult {
    expect_max_args("terminal-list", &args, 0)?;
    Ok(Value::list(vec![terminal_handle_value()]))
}

/// (selected-terminal) -> currently selected terminal handle.
#[cfg(test)]
pub(crate) fn builtin_selected_terminal(args: Vec<Value>) -> EvalResult {
    expect_args("selected-terminal", &args, 0)?;
    Ok(terminal_handle_value())
}

/// (frame-terminal &optional FRAME) -> opaque terminal handle.
pub(crate) fn builtin_frame_terminal(args: Vec<Value>) -> EvalResult {
    expect_max_args("frame-terminal", &args, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_designator(frame)?;
    }
    Ok(terminal_handle_value())
}

/// Evaluator-aware variant of `frame-terminal`.
///
/// Accepts live frame designators in addition to nil.
pub(crate) fn builtin_frame_terminal_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("frame-terminal", &args, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !live_frame_designator_p(eval, frame) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(terminal_handle_value())
}

/// (terminal-live-p TERMINAL) -> t
pub(crate) fn builtin_terminal_live_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("terminal-live-p", &args, 1, 1)?;
    Ok(Value::bool(terminal_designator_p(&args[0])))
}

/// Evaluator-aware variant of `terminal-live-p`.
///
/// Returns non-nil for terminal designators and live frame designators.
pub(crate) fn builtin_terminal_live_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_range_args("terminal-live-p", &args, 1, 1)?;
    Ok(Value::bool(terminal_designator_eval_p(eval, &args[0])))
}

/// (terminal-parameter TERMINAL PARAMETER) -> value
pub(crate) fn builtin_terminal_parameter(args: Vec<Value>) -> EvalResult {
    expect_args("terminal-parameter", &args, 2)?;
    expect_terminal_designator(&args[0])?;
    let key = expect_symbol_key(&args[1])?;
    TERMINAL_PARAMS.with(|slot| Ok(lookup_terminal_parameter_value(&slot.borrow(), &key)))
}

/// Evaluator-aware variant of `terminal-parameter`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_terminal_parameter_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("terminal-parameter", &args, 2)?;
    expect_terminal_designator_eval(eval, &args[0])?;
    let key = expect_symbol_key(&args[1])?;
    TERMINAL_PARAMS.with(|slot| Ok(lookup_terminal_parameter_value(&slot.borrow(), &key)))
}

/// (terminal-parameters &optional TERMINAL) -> alist of terminal parameters
pub(crate) fn builtin_terminal_parameters(args: Vec<Value>) -> EvalResult {
    expect_max_args("terminal-parameters", &args, 1)?;
    if let Some(term) = args.first() {
        if !term.is_nil() {
            expect_terminal_designator(term)?;
        }
    }
    TERMINAL_PARAMS.with(|slot| {
        let merged = terminal_parameters_with_defaults(&slot.borrow());
        Ok(make_alist(merged))
    })
}

/// Evaluator-aware variant of `terminal-parameters`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_terminal_parameters_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("terminal-parameters", &args, 1)?;
    if let Some(term) = args.first() {
        if !term.is_nil() {
            expect_terminal_designator_eval(eval, term)?;
        }
    }
    TERMINAL_PARAMS.with(|slot| {
        let merged = terminal_parameters_with_defaults(&slot.borrow());
        Ok(make_alist(merged))
    })
}

/// (set-terminal-parameter TERMINAL PARAMETER VALUE) -> previous value
pub(crate) fn builtin_set_terminal_parameter(args: Vec<Value>) -> EvalResult {
    expect_args("set-terminal-parameter", &args, 3)?;
    expect_terminal_designator(&args[0])?;
    if matches!(args[1], Value::Str(_)) {
        return Ok(Value::Nil);
    }
    let key = args[1].clone();
    TERMINAL_PARAMS.with(|slot| {
        let mut params = slot.borrow_mut();
        if let Some((_, stored_value)) = params
            .iter_mut()
            .find(|(stored_key, _)| eq_value(stored_key, &key))
        {
            let previous = stored_value.clone();
            *stored_value = args[2].clone();
            return Ok(previous);
        }

        let previous = terminal_parameter_default_value(&key).unwrap_or(Value::Nil);
        params.push((key, args[2].clone()));
        Ok(previous)
    })
}

/// Evaluator-aware variant of `set-terminal-parameter`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_set_terminal_parameter_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-terminal-parameter", &args, 3)?;
    expect_terminal_designator_eval(eval, &args[0])?;
    if matches!(args[1], Value::Str(_)) {
        return Ok(Value::Nil);
    }
    let key = args[1].clone();
    TERMINAL_PARAMS.with(|slot| {
        let mut params = slot.borrow_mut();
        if let Some((_, stored_value)) = params
            .iter_mut()
            .find(|(stored_key, _)| eq_value(stored_key, &key))
        {
            let previous = stored_value.clone();
            *stored_value = args[2].clone();
            return Ok(previous);
        }

        let previous = terminal_parameter_default_value(&key).unwrap_or(Value::Nil);
        params.push((key, args[2].clone()));
        Ok(previous)
    })
}

// ---------------------------------------------------------------------------
// TTY builtins (we are not a TTY, so these return nil)
// ---------------------------------------------------------------------------

/// (tty-type &optional TERMINAL) -> nil
pub(crate) fn builtin_tty_type(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-type", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `tty-type`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_tty_type_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("tty-type", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Ok(Value::Nil)
}

/// (tty-top-frame &optional TERMINAL) -> nil
pub(crate) fn builtin_tty_top_frame(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-top-frame", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `tty-top-frame`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_tty_top_frame_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("tty-top-frame", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Ok(Value::Nil)
}

/// (tty-display-color-p &optional TERMINAL) -> nil
pub(crate) fn builtin_tty_display_color_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-display-color-p", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `tty-display-color-p`.
pub(crate) fn builtin_tty_display_color_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("tty-display-color-p", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Ok(Value::Nil)
}

/// (tty-display-color-cells &optional TERMINAL) -> 0
pub(crate) fn builtin_tty_display_color_cells(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-display-color-cells", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Ok(Value::Int(0))
}

/// Evaluator-aware variant of `tty-display-color-cells`.
pub(crate) fn builtin_tty_display_color_cells_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("tty-display-color-cells", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Ok(Value::Int(0))
}

/// (tty-no-underline &optional TERMINAL) -> nil
pub(crate) fn builtin_tty_no_underline(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-no-underline", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `tty-no-underline`.
pub(crate) fn builtin_tty_no_underline_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("tty-no-underline", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Ok(Value::Nil)
}

/// (controlling-tty-p &optional TERMINAL) -> nil
pub(crate) fn builtin_controlling_tty_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("controlling-tty-p", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `controlling-tty-p`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_controlling_tty_p_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("controlling-tty-p", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Ok(Value::Nil)
}

/// (suspend-tty &optional TTY) -> error in GUI/non-text terminal context.
pub(crate) fn builtin_suspend_tty(args: Vec<Value>) -> EvalResult {
    expect_max_args("suspend-tty", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Err(signal(
        "error",
        vec![Value::string(
            "Attempt to suspend a non-text terminal device",
        )],
    ))
}

/// Evaluator-aware variant of `suspend-tty`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_suspend_tty_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("suspend-tty", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Err(signal(
        "error",
        vec![Value::string(
            "Attempt to suspend a non-text terminal device",
        )],
    ))
}

/// (resume-tty &optional TTY) -> error in GUI/non-text terminal context.
pub(crate) fn builtin_resume_tty(args: Vec<Value>) -> EvalResult {
    expect_max_args("resume-tty", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator(terminal)?;
    }
    Err(signal(
        "error",
        vec![Value::string(
            "Attempt to resume a non-text terminal device",
        )],
    ))
}

/// Evaluator-aware variant of `resume-tty`.
///
/// Accepts live frame designators in addition to terminal designators.
pub(crate) fn builtin_resume_tty_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("resume-tty", &args, 1)?;
    if let Some(terminal) = args.first() {
        expect_terminal_designator_eval(eval, terminal)?;
    }
    Err(signal(
        "error",
        vec![Value::string(
            "Attempt to resume a non-text terminal device",
        )],
    ))
}

// ---------------------------------------------------------------------------
// Monitor attribute builtins
// ---------------------------------------------------------------------------

/// (display-monitor-attributes-list &optional DISPLAY) -> list with one monitor alist
///
/// Returns a list containing a single alist describing the primary monitor.
/// Keys: geometry, workarea, mm-size, frames.
pub(crate) fn builtin_display_monitor_attributes_list(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-monitor-attributes-list", &args, 1)?;
    if let Some(display) = args.first() {
        if !display.is_nil() && !terminal_designator_p(display) {
            return Err(invalid_get_device_terminal_error(display));
        }
    }
    let monitor = make_monitor_alist(Value::Nil);
    Ok(Value::list(vec![monitor]))
}

/// Evaluator-aware variant of `display-monitor-attributes-list`.
///
/// This populates the `frames` slot from the live frame list.
pub(crate) fn builtin_display_monitor_attributes_list_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "display-monitor-attributes-list", &args)?;

    let _ = super::window_cmds::ensure_selected_frame_id(eval);
    let frames = eval
        .frames
        .frame_list()
        .into_iter()
        .map(|fid| Value::Frame(fid.0))
        .collect::<Vec<_>>();
    Ok(Value::list(vec![make_monitor_alist(Value::list(frames))]))
}

/// (frame-monitor-attributes &optional FRAME) -> alist with geometry info
pub(crate) fn builtin_frame_monitor_attributes(args: Vec<Value>) -> EvalResult {
    expect_max_args("frame-monitor-attributes", &args, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() && !terminal_designator_p(frame) {
            return Err(invalid_get_device_terminal_error(frame));
        }
    }
    Ok(make_monitor_alist(Value::Nil))
}

/// Evaluator-aware variant of `frame-monitor-attributes`.
///
/// This populates the `frames` slot from the live frame list.
pub(crate) fn builtin_frame_monitor_attributes_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_optional_display_designator_eval(eval, "frame-monitor-attributes", &args)?;

    let _ = super::window_cmds::ensure_selected_frame_id(eval);
    let frames = eval
        .frames
        .frame_list()
        .into_iter()
        .map(|fid| Value::Frame(fid.0))
        .collect::<Vec<_>>();
    Ok(make_monitor_alist(Value::list(frames)))
}

/// Build a single monitor alist with reasonable default values.
fn make_monitor_alist(frames: Value) -> Value {
    // geometry: (x y width height)
    let geometry = Value::list(vec![
        Value::Int(0),
        Value::Int(0),
        Value::Int(80),
        Value::Int(25),
    ]);

    // workarea: (x y width height)
    let workarea = Value::list(vec![
        Value::Int(0),
        Value::Int(0),
        Value::Int(80),
        Value::Int(25),
    ]);

    // mm-size: (width-mm height-mm)
    let mm_size = Value::list(vec![Value::Nil, Value::Nil]);

    make_alist(vec![
        (Value::symbol("geometry"), geometry),
        (Value::symbol("workarea"), workarea),
        (Value::symbol("mm-size"), mm_size),
        (Value::symbol("frames"), frames),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;

    fn clear_terminal_parameters() {
        TERMINAL_PARAMS.with(|slot| slot.borrow_mut().clear());
    }

    fn reset_cursor_visible() {
        CURSOR_VISIBLE.store(true, Ordering::Relaxed);
        CURSOR_VISIBLE_WINDOWS.with(|slot| slot.borrow_mut().clear());
    }

    #[test]
    fn terminal_parameter_exposes_oracle_defaults() {
        clear_terminal_parameters();
        let normal = builtin_terminal_parameter(vec![
            Value::Nil,
            Value::symbol("normal-erase-is-backspace"),
        ])
        .unwrap();
        assert_eq!(normal, Value::Int(0));

        let keyboard = builtin_terminal_parameter(vec![
            Value::Nil,
            Value::symbol("keyboard-coding-saved-meta-mode"),
        ])
        .unwrap();
        assert_eq!(keyboard, Value::list(vec![Value::True]));

        let missing =
            builtin_terminal_parameter(vec![Value::Nil, Value::symbol("neovm-param")]).unwrap();
        assert!(missing.is_nil());
    }

    #[test]
    fn terminal_parameter_round_trips() {
        clear_terminal_parameters();
        let set_result = builtin_set_terminal_parameter(vec![
            Value::Nil,
            Value::symbol("neovm-param"),
            Value::Int(42),
        ])
        .unwrap();
        assert!(set_result.is_nil());

        let get_result =
            builtin_terminal_parameter(vec![Value::Nil, Value::symbol("neovm-param")]).unwrap();
        assert_eq!(get_result, Value::Int(42));
    }

    #[test]
    fn set_terminal_parameter_returns_previous_default_values() {
        clear_terminal_parameters();
        let previous_normal = builtin_set_terminal_parameter(vec![
            Value::Nil,
            Value::symbol("normal-erase-is-backspace"),
            Value::Int(9),
        ])
        .unwrap();
        assert_eq!(previous_normal, Value::Int(0));

        let previous_keyboard = builtin_set_terminal_parameter(vec![
            Value::Nil,
            Value::symbol("keyboard-coding-saved-meta-mode"),
            Value::Nil,
        ])
        .unwrap();
        assert_eq!(previous_keyboard, Value::list(vec![Value::True]));
    }

    #[test]
    fn terminal_parameter_distinct_keys_do_not_alias() {
        clear_terminal_parameters();
        builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("k1"), Value::Int(1)])
            .unwrap();
        builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("k2"), Value::Int(2)])
            .unwrap();

        let first = builtin_terminal_parameter(vec![Value::Nil, Value::symbol("k1")]).unwrap();
        let second = builtin_terminal_parameter(vec![Value::Nil, Value::symbol("k2")]).unwrap();
        assert_eq!(first, Value::Int(1));
        assert_eq!(second, Value::Int(2));
    }

    #[test]
    fn terminal_parameter_rejects_non_symbol_key() {
        clear_terminal_parameters();
        let result = builtin_terminal_parameter(vec![Value::Nil, Value::string("k")]);
        assert!(result.is_err());
    }

    #[test]
    fn set_terminal_parameter_ignores_non_symbol_key() {
        clear_terminal_parameters();
        let set_result =
            builtin_set_terminal_parameter(vec![Value::Nil, Value::string("k"), Value::Int(9)])
                .unwrap();
        assert!(set_result.is_nil());

        let second_result =
            builtin_set_terminal_parameter(vec![Value::Nil, Value::string("k"), Value::Int(1)])
                .unwrap();
        assert!(second_result.is_nil());

        let get_result = builtin_terminal_parameter(vec![Value::Nil, Value::symbol("k")]).unwrap();
        assert!(get_result.is_nil());
    }

    #[test]
    fn set_terminal_parameter_returns_previous_for_repeat_non_symbol_key() {
        clear_terminal_parameters();
        let first =
            builtin_set_terminal_parameter(vec![Value::Nil, Value::Int(1), Value::Int(9)]).unwrap();
        assert!(first.is_nil());

        let second =
            builtin_set_terminal_parameter(vec![Value::Nil, Value::Int(1), Value::Int(1)]).unwrap();
        assert_eq!(second, Value::Int(9));
    }

    #[test]
    fn terminal_parameter_rejects_non_terminal_designator() {
        clear_terminal_parameters();
        let result = builtin_terminal_parameter(vec![Value::Int(1), Value::symbol("k")]);
        assert!(result.is_err());
    }

    #[test]
    fn terminal_parameters_lists_mutated_symbol_entries() {
        clear_terminal_parameters();
        let _ =
            builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("k1"), Value::Int(1)])
                .unwrap();
        let _ =
            builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("k2"), Value::Int(2)])
                .unwrap();

        let params = builtin_terminal_parameters(vec![Value::Nil]).unwrap();
        let entries = list_to_vec(&params).expect("parameter alist");
        assert!(entries.len() >= 4);
        assert!(entries
            .iter()
            .any(|entry| matches!(entry, Value::Cons(cell) if {
                let pair = cell.lock().expect("poisoned");
                pair.car == Value::symbol("normal-erase-is-backspace") && pair.cdr == Value::Int(0)
            })));
        assert!(entries
            .iter()
            .any(|entry| matches!(entry, Value::Cons(cell) if {
                let pair = cell.lock().expect("poisoned");
                pair.car == Value::symbol("keyboard-coding-saved-meta-mode")
                    && pair.cdr == Value::list(vec![Value::True])
            })));
        assert!(entries
            .iter()
            .any(|entry| matches!(entry, Value::Cons(cell) if {
                let pair = cell.lock().expect("poisoned");
                pair.car == Value::symbol("k1") && pair.cdr == Value::Int(1)
            })));
        assert!(entries
            .iter()
            .any(|entry| matches!(entry, Value::Cons(cell) if {
                let pair = cell.lock().expect("poisoned");
                pair.car == Value::symbol("k2") && pair.cdr == Value::Int(2)
            })));

        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let via_frame = builtin_terminal_parameters_eval(&mut eval, vec![Value::Int(frame_id)])
            .expect("eval terminal-parameters");
        let eval_entries = list_to_vec(&via_frame).expect("parameter alist");
        assert!(eval_entries.len() >= 4);
    }

    #[test]
    fn set_terminal_parameter_rejects_non_terminal_designator() {
        clear_terminal_parameters();
        let result =
            builtin_set_terminal_parameter(vec![Value::Int(1), Value::symbol("k"), Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_terminal_parameter_accepts_live_frame_designator() {
        clear_terminal_parameters();
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        builtin_set_terminal_parameter_eval(
            &mut eval,
            vec![
                Value::Int(frame_id),
                Value::symbol("neovm-frame-param"),
                Value::Int(7),
            ],
        )
        .unwrap();
        let value = builtin_terminal_parameter_eval(
            &mut eval,
            vec![Value::Int(frame_id), Value::symbol("neovm-frame-param")],
        )
        .unwrap();
        assert_eq!(value, Value::Int(7));
    }

    #[test]
    fn terminal_live_p_reflects_designator_shape() {
        let live_nil = builtin_terminal_live_p(vec![Value::Nil]).unwrap();
        let live_handle = builtin_terminal_live_p(vec![terminal_handle_value()]).unwrap();
        let live_string = builtin_terminal_live_p(vec![Value::string("initial_terminal")]).unwrap();
        let live_int = builtin_terminal_live_p(vec![Value::Int(1)]).unwrap();
        assert_eq!(live_nil, Value::True);
        assert_eq!(live_handle, Value::True);
        assert!(live_string.is_nil());
        assert!(live_int.is_nil());
    }

    #[test]
    fn eval_terminal_live_p_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let live = builtin_terminal_live_p_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();
        assert_eq!(live, Value::True);

        let stale = builtin_terminal_live_p_eval(&mut eval, vec![Value::Int(999_999)]).unwrap();
        assert!(stale.is_nil());
    }

    #[test]
    fn terminal_name_rejects_invalid_designator() {
        let result = builtin_terminal_name(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_terminal_name_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let result = builtin_terminal_name_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();
        assert_eq!(result, Value::string("initial_terminal"));
    }

    #[test]
    fn frame_terminal_rejects_non_frame_designator() {
        let result = builtin_frame_terminal(vec![Value::string("not-a-frame")]);
        assert!(result.is_err());
    }

    #[test]
    fn frame_terminal_accepts_frame_id() {
        let result = builtin_frame_terminal(vec![Value::Int(1)]);
        assert!(result.is_ok());
        let handle = result.unwrap();
        let live = builtin_terminal_live_p(vec![handle]).unwrap();
        assert_eq!(live, Value::True);
    }

    #[test]
    fn frame_terminal_returns_live_terminal_handle() {
        let handle = builtin_frame_terminal(vec![Value::Nil]).unwrap();
        let live = builtin_terminal_live_p(vec![handle]).unwrap();
        assert_eq!(live, Value::True);
    }

    #[test]
    fn selected_terminal_returns_live_terminal_handle() {
        let handle = builtin_selected_terminal(vec![]).unwrap();
        let live = builtin_terminal_live_p(vec![handle]).unwrap();
        assert_eq!(live, Value::True);
    }

    #[test]
    fn selected_terminal_arity() {
        assert!(builtin_selected_terminal(vec![Value::Nil]).is_err());
    }

    #[test]
    fn eval_frame_terminal_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let handle = builtin_frame_terminal_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();
        let live = builtin_terminal_live_p(vec![handle]).unwrap();
        assert_eq!(live, Value::True);
    }

    #[test]
    fn redraw_frame_rejects_non_frame_designator() {
        let result = builtin_redraw_frame(vec![Value::string("not-a-frame")]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_redraw_frame_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let result = builtin_redraw_frame_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn frame_edges_string_designator_uses_unquoted_live_frame_error_message() {
        let result = builtin_frame_edges(vec![Value::string("x")]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("x is not a live frame")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn eval_frame_edges_numeric_designator_reports_numeric_message() {
        let mut eval = crate::elisp::Evaluator::new();
        let result = builtin_frame_edges_eval(&mut eval, vec![Value::Int(999_999)]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("999999 is not a live frame")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn eval_frame_edges_live_window_designator_includes_buffer_context() {
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let window = crate::elisp::window_cmds::builtin_selected_window(&mut eval, vec![]).unwrap();
        let result = builtin_frame_edges_eval(&mut eval, vec![window]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                let message = match sig.data.as_slice() {
                    [Value::Str(msg)] => msg.as_str(),
                    other => panic!("expected single error message payload, got {other:?}"),
                };
                assert!(message.starts_with("#<window "));
                assert!(message.contains(" on "));
                assert!(message.ends_with(" is not a live frame"));
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn open_termscript_uses_batch_tty_error_payload() {
        let result = builtin_open_termscript(vec![Value::Nil]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Current frame is not on a tty device")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn send_string_to_terminal_rejects_invalid_terminal_designator() {
        let result = builtin_send_string_to_terminal(vec![Value::string(""), Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn send_string_to_terminal_accepts_live_terminal_handle() {
        let handle = terminal_handle_value();
        let result = builtin_send_string_to_terminal(vec![Value::string(""), handle]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn eval_send_string_to_terminal_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let result = builtin_send_string_to_terminal_eval(
            &mut eval,
            vec![Value::string(""), Value::Int(frame_id)],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_show_cursor_tracks_visibility_state() {
        reset_cursor_visible();
        let default_visible = builtin_internal_show_cursor_p(vec![]).unwrap();
        assert_eq!(default_visible, Value::True);

        builtin_internal_show_cursor(vec![Value::Nil, Value::Nil]).unwrap();
        let hidden = builtin_internal_show_cursor_p(vec![]).unwrap();
        assert!(hidden.is_nil());

        builtin_internal_show_cursor(vec![Value::Nil, Value::True]).unwrap();
        let visible = builtin_internal_show_cursor_p(vec![]).unwrap();
        assert_eq!(visible, Value::True);
    }

    #[test]
    fn internal_show_cursor_rejects_non_window_designator() {
        let result = builtin_internal_show_cursor(vec![Value::Int(1), Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_internal_show_cursor_accepts_live_window_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let window = crate::elisp::window_cmds::builtin_selected_window(&mut eval, vec![]).unwrap();
        let result =
            builtin_internal_show_cursor_eval(&mut eval, vec![window, Value::True]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn eval_internal_show_cursor_p_accepts_live_window_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let window = crate::elisp::window_cmds::builtin_selected_window(&mut eval, vec![]).unwrap();
        let result = builtin_internal_show_cursor_p_eval(&mut eval, vec![window]).unwrap();
        assert!(matches!(result, Value::True | Value::Nil));
    }

    #[test]
    fn eval_internal_show_cursor_tracks_per_window_state() {
        reset_cursor_visible();
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let selected =
            crate::elisp::window_cmds::builtin_selected_window(&mut eval, vec![]).unwrap();
        let other = crate::elisp::window_cmds::builtin_split_window(
            &mut eval,
            vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil],
        )
        .unwrap();

        assert_eq!(
            builtin_internal_show_cursor_p_eval(&mut eval, vec![selected.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            builtin_internal_show_cursor_p_eval(&mut eval, vec![other.clone()]).unwrap(),
            Value::True
        );

        builtin_internal_show_cursor_eval(&mut eval, vec![Value::Nil, Value::Nil]).unwrap();
        assert!(
            builtin_internal_show_cursor_p_eval(&mut eval, vec![selected.clone()])
                .unwrap()
                .is_nil()
        );
        assert_eq!(
            builtin_internal_show_cursor_p_eval(&mut eval, vec![other.clone()]).unwrap(),
            Value::True
        );
        assert!(builtin_internal_show_cursor_p_eval(&mut eval, vec![])
            .unwrap()
            .is_nil());

        builtin_internal_show_cursor_eval(&mut eval, vec![other.clone(), Value::True]).unwrap();
        assert!(
            builtin_internal_show_cursor_p_eval(&mut eval, vec![selected])
                .unwrap()
                .is_nil()
        );
        assert_eq!(
            builtin_internal_show_cursor_p_eval(&mut eval, vec![other]).unwrap(),
            Value::True
        );
        assert!(builtin_internal_show_cursor_p_eval(&mut eval, vec![])
            .unwrap()
            .is_nil());
    }

    #[test]
    fn tty_queries_reject_invalid_terminal_designator() {
        let tty_type = builtin_tty_type(vec![Value::Int(1)]);
        let tty_top_frame = builtin_tty_top_frame(vec![Value::Int(1)]);
        let controlling = builtin_controlling_tty_p(vec![Value::Int(1)]);
        assert!(tty_type.is_err());
        assert!(tty_top_frame.is_err());
        assert!(controlling.is_err());
    }

    #[test]
    fn eval_tty_queries_accept_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        assert!(builtin_tty_type_eval(&mut eval, vec![Value::Int(frame_id)])
            .unwrap()
            .is_nil());
        assert!(
            builtin_tty_top_frame_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_controlling_tty_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
    }

    #[test]
    fn suspend_tty_signals_non_text_terminal_error() {
        for args in [vec![], vec![Value::Nil], vec![terminal_handle_value()]] {
            let result = builtin_suspend_tty(args);
            match result {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string(
                            "Attempt to suspend a non-text terminal device"
                        )]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
    }

    #[test]
    fn eval_suspend_resume_accept_live_frame_and_signal_non_text_terminal_error() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let suspend = builtin_suspend_tty_eval(&mut eval, vec![Value::Int(frame_id)]);
        let resume = builtin_resume_tty_eval(&mut eval, vec![Value::Int(frame_id)]);
        assert!(suspend.is_err());
        assert!(resume.is_err());
    }

    #[test]
    fn resume_tty_signals_non_text_terminal_error() {
        for args in [vec![], vec![Value::Nil], vec![terminal_handle_value()]] {
            let result = builtin_resume_tty(args);
            match result {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string(
                            "Attempt to resume a non-text terminal device"
                        )]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
    }

    #[test]
    fn x_open_connection_requires_string_display_arg() {
        let bad = builtin_x_open_connection(vec![Value::Nil]);
        assert!(bad.is_err());
    }

    #[test]
    fn x_open_connection_arity_errors() {
        let x_open_none = builtin_x_open_connection(vec![]);
        let x_open_four = builtin_x_open_connection(vec![
            Value::string("foo"),
            Value::string("xrm"),
            Value::t(),
            Value::Nil,
        ]);
        assert!(x_open_none.is_err());
        assert!(x_open_four.is_err());
        match x_open_none {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
            }
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match x_open_four {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
            }
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_close_connection_argument_shape_errors() {
        let x_nil = builtin_x_close_connection(vec![Value::Nil]);
        let x_int = builtin_x_close_connection(vec![Value::Int(1)]);
        let x_str = builtin_x_close_connection(vec![Value::string("")]);
        let x_term = builtin_x_close_connection(vec![terminal_handle_value()]);
        let x_close_none = builtin_x_close_connection(vec![]);
        let x_close_two = builtin_x_close_connection(vec![Value::string("foo"), Value::Nil]);
        assert!(x_nil.is_err());
        assert!(x_int.is_err());
        assert!(x_str.is_err());
        assert!(x_close_none.is_err());
        assert!(x_close_two.is_err());
        match x_close_none {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
            }
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match x_close_two {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
            }
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match x_term {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn eval_x_close_connection_live_frame_uses_window_system_error() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;

        let result = builtin_x_close_connection_eval(&mut eval, vec![Value::Int(frame_id)]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn x_display_pixel_size_errors_match_batch_shapes() {
        let width_none = builtin_x_display_pixel_width(vec![]);
        let width_int = builtin_x_display_pixel_width(vec![Value::Int(1)]);
        let width_str = builtin_x_display_pixel_width(vec![Value::string("")]);
        let width_term = builtin_x_display_pixel_width(vec![terminal_handle_value()]);
        let height_none = builtin_x_display_pixel_height(vec![]);
        let height_int = builtin_x_display_pixel_height(vec![Value::Int(1)]);
        let height_str = builtin_x_display_pixel_height(vec![Value::string("")]);
        let height_term = builtin_x_display_pixel_height(vec![terminal_handle_value()]);
        assert!(width_none.is_err());
        assert!(width_int.is_err());
        assert!(width_str.is_err());
        assert!(height_none.is_err());
        assert!(height_int.is_err());
        assert!(height_str.is_err());
        match width_term {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match height_term {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn x_display_color_p_batch_and_arg_errors() {
        let none = builtin_x_display_color_p(vec![]).unwrap();
        let nil = builtin_x_display_color_p(vec![Value::Nil]).unwrap();
        let term = builtin_x_display_color_p(vec![terminal_handle_value()]).unwrap();
        let int_err = builtin_x_display_color_p(vec![Value::Int(1)]);
        let str_err = builtin_x_display_color_p(vec![Value::string("")]);
        assert!(none.is_nil());
        assert!(nil.is_nil());
        assert!(term.is_nil());
        match int_err {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Invalid argument 1 in ‘get-device-terminal’")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        assert!(str_err.is_err());
    }

    #[test]
    fn x_missing_optional_display_queries_match_batch_no_x_shapes() {
        let term = terminal_handle_value();
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;

        type PureXQuery = fn(Vec<Value>) -> EvalResult;
        type EvalXQuery = fn(&mut crate::elisp::eval::Evaluator, Vec<Value>) -> EvalResult;
        for (pure, eval_query) in [
            (
                builtin_x_display_backing_store as PureXQuery,
                builtin_x_display_backing_store_eval as EvalXQuery,
            ),
            (
                builtin_x_display_color_cells,
                builtin_x_display_color_cells_eval,
            ),
            (
                builtin_x_display_mm_height,
                builtin_x_display_mm_height_eval,
            ),
            (builtin_x_display_mm_width, builtin_x_display_mm_width_eval),
            (
                builtin_x_display_monitor_attributes_list,
                builtin_x_display_monitor_attributes_list_eval,
            ),
            (builtin_x_display_planes, builtin_x_display_planes_eval),
            (
                builtin_x_display_save_under,
                builtin_x_display_save_under_eval,
            ),
            (builtin_x_display_screens, builtin_x_display_screens_eval),
            (
                builtin_x_display_visual_class,
                builtin_x_display_visual_class_eval,
            ),
            (
                builtin_x_server_input_extension_version,
                builtin_x_server_input_extension_version_eval,
            ),
            (builtin_x_server_vendor, builtin_x_server_vendor_eval),
        ] {
            match pure(vec![]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("X windows are not in use or not initialized")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }

            match pure(vec![term.clone()]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Terminal 0 is not an X display")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }

            match pure(vec![Value::string("x")]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(sig.data, vec![Value::string("Display x can’t be opened")]);
                }
                other => panic!("expected error signal, got {other:?}"),
            }

            match pure(vec![Value::Int(1)]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
                }
                other => panic!("expected wrong-type-argument signal, got {other:?}"),
            }

            match eval_query(&mut eval, vec![Value::Int(frame_id)]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Window system frame should be used")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
    }

    #[test]
    fn x_display_set_last_user_time_batch_semantics() {
        match builtin_x_display_set_last_user_time(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("X windows are not in use or not initialized")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("X windows are not in use or not initialized")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![Value::string("x"), Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("X windows are not in use or not initialized")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![Value::Nil, Value::string("x")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Display x can’t be opened")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![Value::Nil, terminal_handle_value()]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![Value::Nil, Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        match builtin_x_display_set_last_user_time(vec![Value::Nil, Value::Int(1), Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_display_set_last_user_time_eval_uses_user_time_designator_payloads() {
        let term = terminal_handle_value();
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;

        for display in [
            Value::Nil,
            Value::string("display"),
            Value::Int(1),
            Value::symbol("foo"),
            Value::Int(frame_id),
            term.clone(),
        ] {
            match builtin_x_display_set_last_user_time_eval(
                &mut eval,
                vec![display.clone(), Value::string("x")],
            ) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(sig.data, vec![Value::string("Display x can’t be opened")]);
                }
                other => panic!("expected error signal, got {other:?}"),
            }

            match builtin_x_display_set_last_user_time_eval(
                &mut eval,
                vec![display.clone(), Value::Int(frame_id)],
            ) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Window system frame should be used")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }

            match builtin_x_display_set_last_user_time_eval(
                &mut eval,
                vec![display.clone(), term.clone()],
            ) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Terminal 0 is not an X display")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
    }

    #[test]
    fn x_selection_queries_and_old_gtk_dialog_batch_semantics() {
        assert!(builtin_x_selection_exists_p(vec![]).unwrap().is_nil());
        assert!(builtin_x_selection_owner_p(vec![]).unwrap().is_nil());
        assert!(builtin_x_selection_exists_p(vec![
            Value::symbol("PRIMARY"),
            Value::symbol("STRING")
        ])
        .unwrap()
        .is_nil());
        assert!(
            builtin_x_selection_owner_p(vec![Value::symbol("PRIMARY"), Value::Int(1)])
                .unwrap()
                .is_nil()
        );
        match builtin_x_selection_exists_p(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("symbolp"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_selection_owner_p(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("symbolp"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        assert!(builtin_x_uses_old_gtk_dialog(vec![]).unwrap().is_nil());
        match builtin_x_uses_old_gtk_dialog(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_geometry_fonts_and_resource_batch_semantics() {
        assert_eq!(
            builtin_x_parse_geometry(vec![Value::string("80x24+10+20")]).unwrap(),
            Value::list(vec![
                Value::cons(Value::symbol("height"), Value::Int(24)),
                Value::cons(Value::symbol("width"), Value::Int(80)),
                Value::cons(Value::symbol("top"), Value::Int(20)),
                Value::cons(Value::symbol("left"), Value::Int(10)),
            ])
        );
        assert_eq!(
            builtin_x_parse_geometry(vec![Value::string("80x24")]).unwrap(),
            Value::list(vec![
                Value::cons(Value::symbol("height"), Value::Int(24)),
                Value::cons(Value::symbol("width"), Value::Int(80)),
            ])
        );
        assert!(builtin_x_parse_geometry(vec![Value::string("x")])
            .unwrap()
            .is_nil());
        match builtin_x_parse_geometry(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        assert!(builtin_x_family_fonts(vec![]).unwrap().is_nil());
        assert!(
            builtin_x_family_fonts(vec![Value::string("abc"), Value::Nil])
                .unwrap()
                .is_nil()
        );
        match builtin_x_family_fonts(vec![Value::Int(1), Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_family_fonts(vec![Value::Int(1), Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        match builtin_x_list_fonts(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "Window system is not in use or not initialized"
                    )]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_x_get_resource(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "Window system is not in use or not initialized"
                    )]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_get_resource(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_property_and_frame_arg_batch_semantics() {
        for args in [vec![], vec![Value::Nil], vec![Value::Frame(1)]] {
            match builtin_x_backspace_delete_keys_p(args) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Window system frame should be used")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
        match builtin_x_backspace_delete_keys_p(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        match builtin_x_get_atom_name(vec![Value::symbol("WM_CLASS")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_get_atom_name(vec![Value::symbol("WM_CLASS"), Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        match builtin_x_window_property(vec![Value::string("WM_NAME")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_window_property(vec![Value::string("WM_NAME"), Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_window_property(vec![
            Value::string("WM_NAME"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        match builtin_x_window_property_attributes(vec![Value::string("WM_NAME")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_window_property_attributes(vec![Value::string("WM_NAME"), Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_window_property_attributes(vec![
            Value::string("WM_NAME"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_coordinate_sync_and_message_batch_semantics() {
        let term = terminal_handle_value();

        for args in [
            vec![Value::Nil],
            vec![Value::Nil, Value::Nil],
            vec![Value::Frame(1)],
            vec![Value::Int(1), Value::Nil],
            vec![Value::string("x"), Value::Nil],
            vec![term.clone(), Value::Nil],
        ] {
            match builtin_x_synchronize(args) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("X windows are not in use or not initialized")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
        match builtin_x_synchronize(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        match builtin_x_translate_coordinates(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("X windows are not in use or not initialized")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_translate_coordinates(vec![Value::Frame(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_translate_coordinates(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_translate_coordinates(vec![Value::string("x")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Display x can’t be opened")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_translate_coordinates(vec![term.clone()]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_translate_coordinates(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_translate_coordinates(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        match builtin_x_frame_list_z_order(vec![]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("X windows are not in use or not initialized")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_frame_list_z_order(vec![Value::Frame(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_frame_list_z_order(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_frame_list_z_order(vec![Value::string("x")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Display x can’t be opened")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_frame_list_z_order(vec![term.clone()]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_frame_list_z_order(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        match builtin_x_send_client_message(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("X windows are not in use or not initialized")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_send_client_message(vec![
            Value::Frame(1),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_send_client_message(vec![
            Value::Int(1),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_send_client_message(vec![
            Value::string("x"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Display x can’t be opened")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_send_client_message(vec![
            term,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Terminal 0 is not an X display")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_send_client_message(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_popup_dialog_and_menu_batch_semantics() {
        let term = terminal_handle_value();

        match builtin_x_popup_dialog(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("windowp"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_popup_dialog(vec![Value::Frame(1), Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_popup_dialog(vec![Value::Frame(1), Value::list(vec![Value::string("A")])]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("consp"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        assert!(builtin_x_popup_dialog(vec![
            Value::Frame(1),
            Value::list(vec![
                Value::string("Title"),
                Value::cons(Value::string("Yes"), Value::True),
            ]),
        ])
        .unwrap()
        .is_nil());
        assert!(builtin_x_popup_dialog(vec![
            Value::Frame(1),
            Value::list(vec![Value::string("A"), Value::Int(1)]),
        ])
        .unwrap()
        .is_nil());
        for arg in [Value::string("x"), Value::Int(1), term.clone()] {
            match builtin_x_popup_dialog(vec![arg, Value::Nil]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(sig.data, vec![Value::symbol("windowp"), Value::Nil]);
                }
                other => panic!("expected wrong-type-argument signal, got {other:?}"),
            }
        }
        match builtin_x_popup_dialog(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_popup_dialog(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_popup_dialog(vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        let assert_wta = |result: EvalResult, pred: &str, arg: Value| match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol(pred), arg]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        };
        let basic_menu = Value::list(vec![
            Value::string("A"),
            Value::cons(Value::string("Yes"), Value::True),
        ]);

        assert!(builtin_x_popup_menu(vec![Value::Nil, Value::Nil])
            .unwrap()
            .is_nil());
        assert!(builtin_x_popup_menu(vec![Value::Nil, basic_menu.clone()])
            .unwrap()
            .is_nil());
        for pos in [Value::Frame(1), Value::string("x"), Value::Int(1), term] {
            assert_wta(
                builtin_x_popup_menu(vec![pos.clone(), Value::Nil]),
                "listp",
                pos,
            );
        }

        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Int(0), Value::Int(0)]),
                Value::Nil,
            ]),
            "listp",
            Value::Int(0),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Int(0), Value::Int(0)]),
                basic_menu.clone(),
            ]),
            "listp",
            Value::Int(0),
        );
        assert_wta(
            builtin_x_popup_menu(vec![Value::list(vec![Value::Nil]), Value::Nil]),
            "stringp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![Value::list(vec![Value::Nil]), basic_menu.clone()]),
            "consp",
            Value::True,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::symbol("menu-bar")]),
                Value::Nil,
            ]),
            "stringp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::symbol("menu-bar")]),
                basic_menu.clone(),
            ]),
            "consp",
            Value::True,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::symbol("mouse-1")]),
                Value::Nil,
            ]),
            "stringp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::symbol("mouse-1")]),
                basic_menu.clone(),
            ]),
            "consp",
            Value::True,
        );

        assert_wta(
            builtin_x_popup_menu(vec![Value::list(vec![Value::Nil, Value::Nil]), Value::Nil]),
            "stringp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                basic_menu.clone(),
            ]),
            "consp",
            Value::True,
        );
        assert!(builtin_x_popup_menu(vec![
            Value::list(vec![Value::Nil, Value::Nil]),
            Value::list(vec![Value::string("A")]),
        ])
        .unwrap()
        .is_nil());
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![Value::string("A"), Value::Int(1)]),
            ]),
            "listp",
            Value::Int(1),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![
                    Value::Int(1),
                    Value::cons(Value::string("Yes"), Value::True),
                ]),
            ]),
            "stringp",
            Value::Int(1),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![Value::cons(Value::string("A"), Value::True)]),
            ]),
            "stringp",
            Value::cons(Value::string("A"), Value::True),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::Int(1),
            ]),
            "listp",
            Value::Int(1),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::string("x"),
            ]),
            "listp",
            Value::string("x"),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![Value::string("A"), Value::Nil]),
            ]),
            "stringp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![
                    Value::string("A"),
                    Value::list(vec![Value::string("Pane")]),
                ]),
            ]),
            "consp",
            Value::Nil,
        );
        assert!(builtin_x_popup_menu(vec![
            Value::list(vec![Value::Nil, Value::Nil]),
            Value::list(vec![
                Value::string("A"),
                Value::list(vec![Value::string("Pane"), Value::Nil]),
            ]),
        ])
        .unwrap()
        .is_nil());
        assert!(builtin_x_popup_menu(vec![
            Value::list(vec![Value::Nil, Value::Nil]),
            Value::list(vec![
                Value::string("A"),
                Value::list(vec![
                    Value::string("Pane"),
                    Value::cons(Value::string("Y"), Value::True),
                ]),
            ]),
        ])
        .unwrap()
        .is_nil());
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![
                    Value::string("A"),
                    Value::cons(Value::string("Pane"), Value::Int(1)),
                ]),
            ]),
            "consp",
            Value::Int(1),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::Nil, Value::Nil]),
                Value::list(vec![
                    Value::string("A"),
                    Value::cons(Value::Int(1), Value::Int(2)),
                ]),
            ]),
            "stringp",
            Value::Int(1),
        );

        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::list(vec![Value::Int(0), Value::Int(0)])]),
                Value::Nil,
            ]),
            "windowp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![Value::list(vec![Value::Int(0), Value::Int(0)])]),
                basic_menu.clone(),
            ]),
            "windowp",
            Value::Nil,
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![
                    Value::list(vec![Value::Int(0), Value::Int(0)]),
                    Value::Int(1),
                ]),
                Value::Nil,
            ]),
            "windowp",
            Value::Int(1),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::list(vec![
                    Value::list(vec![Value::Int(0), Value::Int(0)]),
                    Value::Int(1),
                ]),
                basic_menu,
            ]),
            "windowp",
            Value::Int(1),
        );
        assert_wta(
            builtin_x_popup_menu(vec![
                Value::cons(
                    Value::list(vec![Value::Int(0), Value::Int(0)]),
                    Value::Int(0),
                ),
                Value::Nil,
            ]),
            "listp",
            Value::Int(0),
        );
        match builtin_x_popup_menu(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_popup_menu(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_popup_menu(vec![Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_clipboard_input_context_batch_semantics() {
        let term = terminal_handle_value();
        let frame = Value::Frame(1);

        let assert_wrong_type = |result: EvalResult, pred: &str, arg: Value| match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol(pred), arg]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        };
        let assert_error = |result: EvalResult, msg: &str| match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string(msg)]);
            }
            other => panic!("expected error signal, got {other:?}"),
        };
        let assert_wrong_number = |result: EvalResult| match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        };

        assert!(builtin_x_get_clipboard(vec![]).unwrap().is_nil());
        assert_wrong_number(builtin_x_get_clipboard(vec![Value::Nil]));

        assert_error(
            builtin_x_get_modifier_masks(vec![]),
            "X windows are not in use or not initialized",
        );
        assert_error(
            builtin_x_get_modifier_masks(vec![Value::Nil]),
            "X windows are not in use or not initialized",
        );
        assert_error(
            builtin_x_get_modifier_masks(vec![term.clone()]),
            "Terminal 0 is not an X display",
        );
        assert_wrong_type(
            builtin_x_get_modifier_masks(vec![Value::Int(1)]),
            "frame-live-p",
            Value::Int(1),
        );
        assert_error(
            builtin_x_get_modifier_masks(vec![Value::string("x")]),
            "Display x can’t be opened",
        );
        assert_error(
            builtin_x_get_modifier_masks(vec![frame.clone()]),
            "Window system frame should be used",
        );
        assert_wrong_number(builtin_x_get_modifier_masks(vec![Value::Nil, Value::Nil]));

        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Nil]),
            "char-or-string-p",
            Value::Nil,
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![term.clone()]),
            "char-or-string-p",
            term.clone(),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(1)]),
            "stringp",
            Value::Int(1),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(-1)]),
            "char-or-string-p",
            Value::Int(-1),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(65)]),
            "stringp",
            Value::Int(97),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(90)]),
            "stringp",
            Value::Int(122),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(304)]),
            "stringp",
            Value::Int(304),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(7305)]),
            "stringp",
            Value::Int(7305),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(8490)]),
            "stringp",
            Value::Int(8490),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(42955)]),
            "stringp",
            Value::Int(42955),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(68944)]),
            "stringp",
            Value::Int(68944),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(93856)]),
            "stringp",
            Value::Int(93856),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Int(66560)]),
            "stringp",
            Value::Int(66600),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Char('A')]),
            "stringp",
            Value::Int(97),
        );
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![Value::Char('Z')]),
            "stringp",
            Value::Int(122),
        );
        assert!(builtin_x_get_input_coding_system(vec![Value::string("x")])
            .unwrap()
            .is_nil());
        assert_wrong_type(
            builtin_x_get_input_coding_system(vec![frame.clone()]),
            "char-or-string-p",
            frame.clone(),
        );
        assert_wrong_number(builtin_x_get_input_coding_system(vec![
            Value::Nil,
            Value::Nil,
        ]));

        assert!(builtin_x_hide_tip(vec![]).unwrap().is_nil());
        assert_wrong_number(builtin_x_hide_tip(vec![Value::Nil]));

        assert_wrong_type(
            builtin_x_setup_function_keys(vec![Value::Nil]),
            "frame-live-p",
            Value::Nil,
        );
        assert_wrong_type(
            builtin_x_setup_function_keys(vec![term.clone()]),
            "frame-live-p",
            term.clone(),
        );
        assert_wrong_type(
            builtin_x_setup_function_keys(vec![Value::Int(1)]),
            "terminal-live-p",
            Value::Int(1),
        );
        assert_wrong_type(
            builtin_x_setup_function_keys(vec![Value::string("x")]),
            "terminal-live-p",
            Value::string("x"),
        );
        assert!(builtin_x_setup_function_keys(vec![frame.clone()])
            .unwrap()
            .is_nil());
        assert_wrong_number(builtin_x_setup_function_keys(vec![]));
        assert_wrong_number(builtin_x_setup_function_keys(vec![Value::Nil, Value::Nil]));

        assert_eq!(
            builtin_x_clear_preedit_text(vec![]).unwrap(),
            Value::list(vec![Value::symbol("tooltip-hide")])
        );
        assert_wrong_number(builtin_x_clear_preedit_text(vec![Value::Nil]));

        assert!(builtin_x_preedit_text(vec![Value::Nil]).unwrap().is_nil());
        assert_wrong_type(
            builtin_x_preedit_text(vec![term.clone()]),
            "listp",
            term.clone(),
        );
        assert_wrong_type(
            builtin_x_preedit_text(vec![Value::Int(1)]),
            "listp",
            Value::Int(1),
        );
        assert_wrong_type(
            builtin_x_preedit_text(vec![Value::string("x")]),
            "listp",
            Value::string("x"),
        );
        assert_wrong_type(
            builtin_x_preedit_text(vec![frame.clone()]),
            "listp",
            frame.clone(),
        );
        assert_wrong_type(
            builtin_x_preedit_text(vec![Value::list(vec![Value::Int(1), Value::Int(2)])]),
            "stringp",
            Value::Int(2),
        );
        assert!(
            builtin_x_preedit_text(vec![Value::list(vec![Value::Nil, Value::Nil])])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_x_preedit_text(vec![Value::list(vec![Value::list(vec![
                Value::Int(1),
                Value::Int(2),
            ])])])
            .unwrap()
            .is_nil()
        );
        assert_wrong_type(
            builtin_x_preedit_text(vec![Value::cons(Value::Int(1), Value::Int(2))]),
            "listp",
            Value::Int(2),
        );
        assert_wrong_number(builtin_x_preedit_text(vec![]));
        assert_wrong_number(builtin_x_preedit_text(vec![Value::Nil, Value::Nil]));

        assert!(builtin_x_win_suspend_error(vec![]).unwrap().is_nil());
        assert_wrong_number(builtin_x_win_suspend_error(vec![Value::Nil]));

        assert!(builtin_x_device_class(vec![Value::Nil]).unwrap().is_nil());
        assert_wrong_type(
            builtin_x_device_class(vec![term.clone()]),
            "char-or-string-p",
            term.clone(),
        );
        assert_wrong_type(
            builtin_x_device_class(vec![Value::Int(1)]),
            "stringp",
            Value::Int(1),
        );
        assert!(builtin_x_device_class(vec![Value::string("x")])
            .unwrap()
            .is_nil());
        assert_wrong_type(
            builtin_x_device_class(vec![frame.clone()]),
            "char-or-string-p",
            frame.clone(),
        );
        assert_wrong_number(builtin_x_device_class(vec![]));
        assert_wrong_number(builtin_x_device_class(vec![Value::Nil, Value::Nil]));

        for arg in [
            Value::Nil,
            term.clone(),
            Value::Int(1),
            Value::string("x"),
            frame.clone(),
        ] {
            assert!(builtin_x_internal_focus_input_context(vec![arg])
                .unwrap()
                .is_nil());
        }
        assert_wrong_number(builtin_x_internal_focus_input_context(vec![]));
        assert_wrong_number(builtin_x_internal_focus_input_context(vec![
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(
            builtin_x_wm_set_size_hint(vec![]),
            "Window system frame should be used",
        );
        assert_error(
            builtin_x_wm_set_size_hint(vec![Value::Nil]),
            "Window system frame should be used",
        );
        assert_wrong_type(
            builtin_x_wm_set_size_hint(vec![term]),
            "frame-live-p",
            terminal_handle_value(),
        );
        assert_wrong_type(
            builtin_x_wm_set_size_hint(vec![Value::Int(1)]),
            "frame-live-p",
            Value::Int(1),
        );
        assert_wrong_type(
            builtin_x_wm_set_size_hint(vec![Value::string("x")]),
            "frame-live-p",
            Value::string("x"),
        );
        assert_error(
            builtin_x_wm_set_size_hint(vec![frame]),
            "Window system frame should be used",
        );
        assert_wrong_number(builtin_x_wm_set_size_hint(vec![Value::Nil, Value::Nil]));
    }

    #[test]
    fn x_selection_property_tip_batch_semantics() {
        let assert_wrong_type = |result: EvalResult, pred: &str, arg: Value| match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol(pred), arg]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        };
        let assert_error = |result: EvalResult, msg: &str| match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string(msg)]);
            }
            other => panic!("expected error signal, got {other:?}"),
        };
        let assert_wrong_number = |result: EvalResult| match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        };

        assert_error(
            builtin_x_apply_session_resources(vec![]),
            "Window system is not in use or not initialized",
        );
        assert_wrong_number(builtin_x_apply_session_resources(vec![Value::Nil]));

        assert_error(
            builtin_x_change_window_property(vec![Value::string("P"), Value::string("V")]),
            "Window system frame should be used",
        );
        assert_error(
            builtin_x_change_window_property(vec![
                Value::string("P"),
                Value::string("V"),
                Value::Nil,
            ]),
            "Window system frame should be used",
        );
        assert_error(
            builtin_x_change_window_property(vec![
                Value::string("P"),
                Value::string("V"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ]),
            "Window system frame should be used",
        );
        assert_wrong_number(builtin_x_change_window_property(vec![
            Value::string("P"),
            Value::string("V"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(
            builtin_x_delete_window_property(vec![Value::string("P")]),
            "Window system frame should be used",
        );
        assert_error(
            builtin_x_delete_window_property(vec![Value::string("P"), Value::Nil]),
            "Window system frame should be used",
        );
        assert_error(
            builtin_x_delete_window_property(vec![Value::string("P"), Value::Nil, Value::Nil]),
            "Window system frame should be used",
        );
        assert_wrong_number(builtin_x_delete_window_property(vec![
            Value::string("P"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(builtin_x_clipboard_yank(vec![]), "Kill ring is empty");
        assert_wrong_number(builtin_x_clipboard_yank(vec![Value::Nil]));

        assert!(builtin_x_disown_selection_internal(vec![Value::Nil])
            .unwrap()
            .is_nil());
        assert!(
            builtin_x_disown_selection_internal(vec![Value::Nil, Value::Nil])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_x_disown_selection_internal(vec![Value::Nil, Value::Nil, Value::Nil])
                .unwrap()
                .is_nil()
        );
        assert_wrong_number(builtin_x_disown_selection_internal(vec![]));
        assert_wrong_number(builtin_x_disown_selection_internal(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_wrong_type(builtin_x_get_local_selection(vec![]), "consp", Value::Nil);
        assert_wrong_type(
            builtin_x_get_local_selection(vec![Value::Nil]),
            "consp",
            Value::Nil,
        );
        assert_wrong_type(
            builtin_x_get_local_selection(vec![Value::Nil, Value::Nil]),
            "consp",
            Value::Nil,
        );
        assert_wrong_number(builtin_x_get_local_selection(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(
            builtin_x_get_selection_internal(vec![Value::Nil, Value::Nil]),
            "X selection unavailable for this frame",
        );
        assert_error(
            builtin_x_get_selection_internal(vec![Value::Nil, Value::Nil, Value::Nil]),
            "X selection unavailable for this frame",
        );
        assert_error(
            builtin_x_get_selection_internal(vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil]),
            "X selection unavailable for this frame",
        );
        assert_wrong_number(builtin_x_get_selection_internal(vec![]));
        assert_wrong_number(builtin_x_get_selection_internal(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(
            builtin_x_own_selection_internal(vec![Value::Nil, Value::Nil]),
            "X selection unavailable for this frame",
        );
        assert_error(
            builtin_x_own_selection_internal(vec![Value::Nil, Value::Nil, Value::Nil]),
            "X selection unavailable for this frame",
        );
        assert_wrong_number(builtin_x_own_selection_internal(vec![Value::Nil]));
        assert_wrong_number(builtin_x_own_selection_internal(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(
            builtin_x_show_tip(vec![Value::string("m")]),
            "Window system frame should be used",
        );
        assert_wrong_type(
            builtin_x_show_tip(vec![Value::Int(1)]),
            "stringp",
            Value::Int(1),
        );
        assert_error(
            builtin_x_show_tip(vec![
                Value::string("m"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ]),
            "Window system frame should be used",
        );
        assert_wrong_number(builtin_x_show_tip(vec![]));
        assert_wrong_number(builtin_x_show_tip(vec![
            Value::string("m"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));
    }

    #[test]
    fn gui_selection_batch_semantics() {
        let assert_error = |result: EvalResult, msg: &str| match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string(msg)]);
            }
            other => panic!("expected error signal, got {other:?}"),
        };
        let assert_wrong_number = |result: EvalResult| match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        };

        assert!(builtin_gui_get_selection(vec![]).unwrap().is_nil());
        assert!(builtin_gui_get_selection(vec![Value::Nil])
            .unwrap()
            .is_nil());
        assert!(builtin_gui_get_selection(vec![Value::Nil, Value::Nil])
            .unwrap()
            .is_nil());
        assert_wrong_number(builtin_gui_get_selection(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));

        assert_error(
            builtin_gui_get_primary_selection(vec![]),
            "No selection is available",
        );
        assert_wrong_number(builtin_gui_get_primary_selection(vec![Value::Nil]));

        assert!(builtin_gui_select_text(vec![Value::string("a")])
            .unwrap()
            .is_nil());
        assert!(builtin_gui_select_text(vec![Value::Int(1)])
            .unwrap()
            .is_nil());
        assert_wrong_number(builtin_gui_select_text(vec![
            Value::string("a"),
            Value::Nil,
        ]));

        assert!(builtin_gui_selection_value(vec![]).unwrap().is_nil());
        assert_wrong_number(builtin_gui_selection_value(vec![Value::Nil]));

        assert!(builtin_gui_set_selection(vec![Value::Nil, Value::Nil])
            .unwrap()
            .is_nil());
        assert_wrong_number(builtin_gui_set_selection(vec![
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]));
    }

    #[test]
    fn x_frame_restack_safe_arity_surface() {
        match builtin_x_frame_restack(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_frame_restack(vec![Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Window system frame should be used")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }
        match builtin_x_frame_restack(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_frame_restack(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_frame_restack(vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn x_frame_mouse_and_dnd_batch_semantics() {
        let term = terminal_handle_value();

        for args in [
            vec![],
            vec![Value::Nil],
            vec![Value::Frame(1)],
            vec![Value::Nil, Value::Nil],
        ] {
            match builtin_x_export_frames(args) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Window system frame should be used")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
        for arg in [Value::Int(1), Value::string("x"), term.clone()] {
            match builtin_x_export_frames(vec![arg.clone()]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), arg]);
                }
                other => panic!("expected wrong-type-argument signal, got {other:?}"),
            }
        }
        match builtin_x_export_frames(vec![Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        for args in [
            vec![Value::Nil],
            vec![Value::Frame(1)],
            vec![Value::Nil, Value::Nil],
        ] {
            match builtin_x_focus_frame(args) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Window system frame should be used")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
        for arg in [Value::Int(1), Value::string("x"), term] {
            match builtin_x_focus_frame(vec![arg.clone()]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "wrong-type-argument");
                    assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), arg]);
                }
                other => panic!("expected wrong-type-argument signal, got {other:?}"),
            }
        }
        match builtin_x_focus_frame(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        assert!(builtin_x_frame_edges(vec![]).unwrap().is_nil());
        assert!(builtin_x_frame_edges(vec![Value::Nil]).unwrap().is_nil());
        assert!(builtin_x_frame_edges(vec![Value::Frame(1)])
            .unwrap()
            .is_nil());
        assert!(builtin_x_frame_edges(vec![Value::Nil, Value::Nil])
            .unwrap()
            .is_nil());
        match builtin_x_frame_edges(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_frame_edges(vec![Value::string("x")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("frame-live-p"), Value::string("x")]
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_frame_edges(vec![Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        assert!(builtin_x_frame_geometry(vec![]).unwrap().is_nil());
        assert!(builtin_x_frame_geometry(vec![Value::Nil]).unwrap().is_nil());
        assert!(builtin_x_frame_geometry(vec![Value::Frame(1)])
            .unwrap()
            .is_nil());
        match builtin_x_frame_geometry(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_frame_geometry(vec![Value::string("x")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("frame-live-p"), Value::string("x")]
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
        match builtin_x_frame_geometry(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        assert!(builtin_x_mouse_absolute_pixel_position(vec![])
            .unwrap()
            .is_nil());
        match builtin_x_mouse_absolute_pixel_position(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        assert!(
            builtin_x_set_mouse_absolute_pixel_position(vec![Value::Nil, Value::Nil])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_x_set_mouse_absolute_pixel_position(vec![Value::Int(1), Value::Int(2)])
                .unwrap()
                .is_nil()
        );
        match builtin_x_set_mouse_absolute_pixel_position(vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_set_mouse_absolute_pixel_position(vec![Value::Nil, Value::Nil, Value::Nil])
        {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }

        for args in [
            vec![Value::Nil],
            vec![Value::Frame(1)],
            vec![Value::Int(1)],
            vec![terminal_handle_value()],
            vec![Value::Nil, Value::Nil],
        ] {
            match builtin_x_register_dnd_atom(args) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(
                        sig.data,
                        vec![Value::string("Window system frame should be used")]
                    );
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }
        match builtin_x_register_dnd_atom(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
        match builtin_x_register_dnd_atom(vec![Value::Nil, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn eval_x_display_queries_accept_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;

        let width = builtin_x_display_pixel_width_eval(&mut eval, vec![Value::Int(frame_id)]);
        let height = builtin_x_display_pixel_height_eval(&mut eval, vec![Value::Int(frame_id)]);
        let color = builtin_x_display_color_p_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();

        assert!(width.is_err());
        assert!(height.is_err());
        assert!(color.is_nil());
    }

    #[test]
    fn eval_x_clipboard_yank_respects_kill_ring_binding() {
        let mut eval = crate::elisp::Evaluator::new();

        match builtin_x_clipboard_yank_eval(&mut eval, vec![]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(sig.data, vec![Value::string("Kill ring is empty")]);
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        eval.assign("kill-ring", Value::list(vec![Value::string("abc")]));
        assert!(builtin_x_clipboard_yank_eval(&mut eval, vec![])
            .unwrap()
            .is_nil());

        eval.assign("kill-ring", Value::list(vec![Value::Int(1)]));
        match builtin_x_clipboard_yank_eval(&mut eval, vec![]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("buffer-or-string-p"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        eval.assign("kill-ring", Value::Int(1));
        match builtin_x_clipboard_yank_eval(&mut eval, vec![]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("sequencep"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        eval.assign("kill-ring", Value::string("abc"));
        match builtin_x_clipboard_yank_eval(&mut eval, vec![]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("listp"), Value::string("abc")]
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }

        match builtin_x_clipboard_yank_eval(&mut eval, vec![Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn eval_monitor_attributes_include_bootstrapped_frame() {
        let mut eval = crate::elisp::Evaluator::new();
        let list = builtin_display_monitor_attributes_list_eval(&mut eval, vec![]).unwrap();
        let monitors = list_to_vec(&list).expect("monitor list");
        let attrs = list_to_vec(monitors.first().expect("first monitor")).expect("monitor attrs");

        let mut frames_value = Value::Nil;
        for attr in attrs {
            if let Value::Cons(cell) = attr {
                let pair = cell.lock().expect("poisoned");
                if matches!(&pair.car, Value::Symbol(name) if name == "frames") {
                    frames_value = pair.cdr.clone();
                    break;
                }
            }
        }

        let frames = list_to_vec(&frames_value).expect("frames list");
        assert_eq!(frames.len(), 1);
        assert!(matches!(frames.first(), Some(Value::Frame(_))));
        assert!(!frames[0].is_integer());
        assert_eq!(
            crate::elisp::window_cmds::builtin_framep(&mut eval, vec![frames[0].clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            crate::elisp::window_cmds::builtin_frame_live_p(&mut eval, vec![frames[0].clone()])
                .unwrap(),
            Value::True
        );
    }

    #[test]
    fn eval_monitor_queries_accept_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;

        let list =
            builtin_display_monitor_attributes_list_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap();
        let monitors = list_to_vec(&list).expect("monitor list");
        assert_eq!(monitors.len(), 1);

        let attrs =
            builtin_frame_monitor_attributes_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();
        let attr_list = list_to_vec(&attrs).expect("monitor attrs");
        assert!(!attr_list.is_empty());
    }

    #[test]
    fn eval_monitor_queries_accept_frame_handle_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let list = builtin_display_monitor_attributes_list_eval(&mut eval, vec![]).unwrap();
        let monitors = list_to_vec(&list).expect("monitor list");
        let attrs = list_to_vec(monitors.first().expect("first monitor")).expect("monitor attrs");

        let mut frame = Value::Nil;
        for attr in attrs {
            if let Value::Cons(cell) = attr {
                let pair = cell.lock().expect("poisoned");
                if matches!(&pair.car, Value::Symbol(name) if name == "frames") {
                    let frames = list_to_vec(&pair.cdr).expect("frames list");
                    frame = frames.first().cloned().expect("first frame");
                    break;
                }
            }
        }
        assert!(matches!(frame, Value::Frame(_)));

        let by_display =
            builtin_display_monitor_attributes_list_eval(&mut eval, vec![frame.clone()]).unwrap();
        let display_list = list_to_vec(&by_display).expect("monitor list");
        assert_eq!(display_list.len(), 1);

        let by_frame = builtin_frame_monitor_attributes_eval(&mut eval, vec![frame]).unwrap();
        let frame_attrs = list_to_vec(&by_frame).expect("monitor attrs");
        assert!(!frame_attrs.is_empty());
    }

    #[test]
    fn eval_display_queries_accept_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;

        assert!(
            builtin_display_graphic_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_display_color_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert_eq!(
            builtin_display_pixel_width_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::Int(80)
        );
        assert_eq!(
            builtin_display_pixel_height_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::Int(25)
        );
        assert!(
            builtin_display_mm_width_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_display_mm_height_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert_eq!(
            builtin_display_screens_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::Int(1)
        );
        assert_eq!(
            builtin_display_color_cells_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::Int(0)
        );
        assert_eq!(
            builtin_display_planes_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            builtin_display_visual_class_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::symbol("static-gray")
        );
        assert_eq!(
            builtin_display_backing_store_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::symbol("not-useful")
        );
        assert_eq!(
            builtin_display_save_under_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap(),
            Value::symbol("not-useful")
        );
        assert!(
            builtin_display_selections_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_display_images_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(builtin_display_supports_face_attributes_p_eval(
            &mut eval,
            vec![Value::list(vec![
                Value::symbol(":weight"),
                Value::symbol("bold")
            ])]
        )
        .unwrap()
        .is_nil());
    }

    #[test]
    fn eval_display_queries_reject_invalid_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let result = builtin_display_pixel_width_eval(&mut eval, vec![Value::Int(999_999)]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_display_queries_string_designator_reports_missing_display() {
        fn assert_missing_display(result: EvalResult) {
            match result {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(sig.data, vec![Value::string("Display x does not exist")]);
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }

        let mut eval = crate::elisp::Evaluator::new();
        assert_missing_display(builtin_display_graphic_p_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_color_p_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_pixel_width_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_pixel_height_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_mm_width_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_mm_height_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_screens_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_color_cells_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_planes_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_visual_class_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_backing_store_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_save_under_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_selections_p_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
        assert_missing_display(builtin_display_images_p_eval(
            &mut eval,
            vec![Value::string("x")],
        ));
    }

    #[test]
    fn eval_display_monitor_errors_render_window_designators() {
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let window = crate::elisp::window_cmds::builtin_selected_window(&mut eval, vec![]).unwrap();

        let list_err =
            builtin_display_monitor_attributes_list_eval(&mut eval, vec![window.clone()])
                .expect_err("window designator should be rejected");
        let frame_err = builtin_frame_monitor_attributes_eval(&mut eval, vec![window])
            .expect_err("window designator should be rejected");

        for err in [list_err, frame_err] {
            match err {
                Flow::Signal(sig) => {
                    assert_eq!(sig.symbol, "error");
                    match sig.data.as_slice() {
                        [Value::Str(msg)] => {
                            assert!(msg.contains("get-device-terminal"));
                            assert!(msg.contains("#<window"));
                            assert!(msg.contains("*scratch*"));
                        }
                        other => panic!("unexpected signal payload: {other:?}"),
                    }
                }
                other => panic!("expected signal, got {other:?}"),
            }
        }
    }

    #[test]
    fn get_device_terminal_formatter_keeps_integer_literals() {
        let mut eval = crate::elisp::Evaluator::new();
        let _ = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval);
        let window = crate::elisp::window_cmds::builtin_selected_window(&mut eval, vec![]).unwrap();

        let rendered_window = format_get_device_terminal_arg_eval(&eval, &window);
        assert!(rendered_window.contains("#<window"));
        assert!(rendered_window.contains("*scratch*"));

        let rendered_integer = format_get_device_terminal_arg_eval(&eval, &Value::Int(1));
        assert_eq!(rendered_integer, "1");
    }

    #[test]
    fn display_images_p_shapes_and_errors() {
        assert!(builtin_display_images_p(vec![]).unwrap().is_nil());
        assert!(builtin_display_images_p(vec![Value::Nil]).unwrap().is_nil());

        match builtin_display_images_p(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Invalid argument 1 in ‘get-device-terminal’")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_display_images_p(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments, got {other:?}"),
        }
    }

    #[test]
    fn display_save_under_and_display_selections_p_shapes_and_errors() {
        assert_eq!(
            builtin_display_save_under(vec![]).unwrap(),
            Value::symbol("not-useful")
        );
        assert_eq!(
            builtin_display_save_under(vec![Value::Nil]).unwrap(),
            Value::symbol("not-useful")
        );
        assert!(builtin_display_selections_p(vec![]).unwrap().is_nil());
        assert!(builtin_display_selections_p(vec![Value::Nil])
            .unwrap()
            .is_nil());

        match builtin_display_save_under(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Invalid argument 1 in ‘get-device-terminal’")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_display_selections_p(vec![Value::Int(1)]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Invalid argument 1 in ‘get-device-terminal’")]
                );
            }
            other => panic!("expected error signal, got {other:?}"),
        }

        match builtin_display_save_under(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments, got {other:?}"),
        }

        match builtin_display_selections_p(vec![Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments, got {other:?}"),
        }
    }

    #[test]
    fn display_optional_capability_queries_match_color_shapes() {
        for query in [
            builtin_display_grayscale_p as fn(Vec<Value>) -> EvalResult,
            builtin_display_mouse_p,
            builtin_display_popup_menus_p,
            builtin_display_symbol_keys_p,
        ] {
            assert!(query(vec![]).unwrap().is_nil());
            assert!(query(vec![Value::Nil]).unwrap().is_nil());
            assert!(query(vec![terminal_handle_value()]).unwrap().is_nil());

            match query(vec![Value::Int(1)]) {
                Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
                other => panic!("expected error signal, got {other:?}"),
            }

            match query(vec![Value::string("x")]) {
                Err(Flow::Signal(sig)) => {
                    assert_eq!(sig.symbol, "error");
                    assert_eq!(sig.data, vec![Value::string("Display x does not exist")]);
                }
                other => panic!("expected error signal, got {other:?}"),
            }
        }

        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        assert!(
            builtin_display_grayscale_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_display_mouse_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_display_popup_menus_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_display_symbol_keys_p_eval(&mut eval, vec![Value::Int(frame_id)])
                .unwrap()
                .is_nil()
        );
    }

    #[test]
    fn display_supports_face_attributes_p_arity_and_nil_result() {
        let attrs = Value::list(vec![Value::symbol(":weight"), Value::symbol("bold")]);
        assert!(
            builtin_display_supports_face_attributes_p(vec![attrs.clone()])
                .unwrap()
                .is_nil()
        );
        assert!(builtin_display_supports_face_attributes_p(vec![
            attrs.clone(),
            Value::Int(999_999)
        ])
        .unwrap()
        .is_nil());
        assert!(
            builtin_display_supports_face_attributes_p(vec![Value::Int(1)])
                .unwrap()
                .is_nil()
        );

        match builtin_display_supports_face_attributes_p(vec![]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments, got {other:?}"),
        }
        match builtin_display_supports_face_attributes_p(vec![attrs, Value::Nil, Value::Nil]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments, got {other:?}"),
        }
    }
}
