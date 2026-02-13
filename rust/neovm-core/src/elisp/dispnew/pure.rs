use super::args::{expect_args, expect_args_range};
use super::{EvalResult, Value};

/// (redraw-frame &optional FRAME) -> nil
///
/// Redraw the given frame.  In a full implementation this would trigger
/// a complete redisplay of FRAME.  Stub: returns nil.
pub(crate) fn builtin_redraw_frame(args: Vec<Value>) -> EvalResult {
    expect_args_range("redraw-frame", &args, 0, 1)?;
    Ok(Value::Nil)
}

/// (redraw-display) -> nil
///
/// Redraw all visible frames.  Stub: returns nil.
pub(crate) fn builtin_redraw_display(args: Vec<Value>) -> EvalResult {
    expect_args("redraw-display", &args, 0)?;
    Ok(Value::Nil)
}

/// (open-termscript FILE) -> nil
///
/// Open a terminal script file for logging terminal output.
/// Stub: returns nil.
pub(crate) fn builtin_open_termscript(args: Vec<Value>) -> EvalResult {
    expect_args("open-termscript", &args, 1)?;
    Ok(Value::Nil)
}

/// (ding &optional ARG) -> nil
///
/// Ring the bell (beep).  If ARG is non-nil, don't beep, just flash the
/// screen (visible bell).  Stub: returns nil.
pub(crate) fn builtin_ding(args: Vec<Value>) -> EvalResult {
    expect_args_range("ding", &args, 0, 1)?;
    Ok(Value::Nil)
}

/// (send-string-to-terminal STRING &optional TERMINAL) -> nil
///
/// Send STRING directly to the terminal device.
/// Stub: returns nil.
pub(crate) fn builtin_send_string_to_terminal(args: Vec<Value>) -> EvalResult {
    expect_args_range("send-string-to-terminal", &args, 1, 2)?;
    Ok(Value::Nil)
}

/// (internal-show-cursor WINDOW SHOW) -> nil
///
/// Set cursor visibility in WINDOW.  If SHOW is non-nil, show the cursor;
/// otherwise hide it.  Stub: returns nil.
pub(crate) fn builtin_internal_show_cursor(args: Vec<Value>) -> EvalResult {
    expect_args("internal-show-cursor", &args, 2)?;
    Ok(Value::Nil)
}

/// (internal-show-cursor-p &optional WINDOW) -> t
///
/// Return t if the cursor is currently visible in WINDOW.
/// Always returns t since the cursor is always visible in this implementation.
pub(crate) fn builtin_internal_show_cursor_p(args: Vec<Value>) -> EvalResult {
    expect_args_range("internal-show-cursor-p", &args, 0, 1)?;
    Ok(Value::True)
}

/// (last-nonminibuffer-frame) -> nil
///
/// Return the last frame that is not a minibuffer-only frame.
/// Stub: returns nil.
pub(crate) fn builtin_last_nonminibuffer_frame(args: Vec<Value>) -> EvalResult {
    expect_args("last-nonminibuffer-frame", &args, 0)?;
    Ok(Value::Nil)
}
