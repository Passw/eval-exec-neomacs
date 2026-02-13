use super::args::{expect_args, expect_max_args};
use super::{EvalResult, Value};

/// `(terminal-list)` -> nil
///
/// Return a list of all live terminals.  Since neomacs has no terminal
/// objects, returns nil (the empty list).
pub(crate) fn builtin_terminal_list(args: Vec<Value>) -> EvalResult {
    expect_args("terminal-list", &args, 0)?;
    Ok(Value::Nil)
}

/// `(terminal-name &optional TERMINAL)` -> "neomacs"
///
/// Return the name of TERMINAL.  Since neomacs is a graphical
/// application without real terminal objects, always returns "neomacs".
pub(crate) fn builtin_terminal_name(args: Vec<Value>) -> EvalResult {
    expect_max_args("terminal-name", &args, 1)?;
    Ok(Value::string("neomacs"))
}

/// `(terminal-live-p TERMINAL)` -> nil
///
/// Return non-nil if TERMINAL is live.  Since neomacs has no terminal
/// objects, always returns nil.
pub(crate) fn builtin_terminal_live_p(args: Vec<Value>) -> EvalResult {
    expect_args("terminal-live-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(terminal-parameter TERMINAL PARAMETER)` -> nil
///
/// Return TERMINAL's value for parameter PARAMETER.
/// Stub: always returns nil.
pub(crate) fn builtin_terminal_parameter(args: Vec<Value>) -> EvalResult {
    expect_args("terminal-parameter", &args, 2)?;
    Ok(Value::Nil)
}

/// `(set-terminal-parameter TERMINAL PARAMETER VALUE)` -> VALUE
///
/// Set TERMINAL's value for parameter PARAMETER to VALUE.
/// Stub: returns VALUE without storing anything.
pub(crate) fn builtin_set_terminal_parameter(args: Vec<Value>) -> EvalResult {
    expect_args("set-terminal-parameter", &args, 3)?;
    Ok(args[2].clone())
}

/// `(terminal-parameters &optional TERMINAL)` -> nil
///
/// Return an alist of parameters for TERMINAL.
/// Stub: always returns nil (empty list).
pub(crate) fn builtin_terminal_parameters(args: Vec<Value>) -> EvalResult {
    expect_max_args("terminal-parameters", &args, 1)?;
    Ok(Value::Nil)
}

/// `(frame-terminal &optional FRAME)` -> nil
///
/// Return the terminal that FRAME is displayed on.
/// Stub: always returns nil.
pub(crate) fn builtin_frame_terminal(args: Vec<Value>) -> EvalResult {
    expect_max_args("frame-terminal", &args, 1)?;
    Ok(Value::Nil)
}

/// `(delete-terminal &optional TERMINAL FORCE)` -> nil
///
/// Delete TERMINAL by deleting all frames on it.
/// Stub: always returns nil.
pub(crate) fn builtin_delete_terminal(args: Vec<Value>) -> EvalResult {
    expect_max_args("delete-terminal", &args, 2)?;
    Ok(Value::Nil)
}
