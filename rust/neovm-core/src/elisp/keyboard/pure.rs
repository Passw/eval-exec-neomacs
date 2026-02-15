use super::args::{expect_args, expect_range_args};
use super::{EvalResult, Value};

/// `(recent-keys &optional INCLUDE-CMDS)` -> vector
///
/// Return a vector of recent input events.
pub(crate) fn builtin_recent_keys(args: Vec<Value>) -> EvalResult {
    expect_range_args("recent-keys", &args, 0, 1)?;
    // Optional INCLUDE-CMDS argument is accepted but ignored.
    Ok(Value::vector(vec![]))
}

/// `(input-pending-p &optional CHECK-TIMERS)` -> nil
///
/// Return t if command input is currently available with no wait, nil otherwise.
///
/// Stub implementation: always returns nil (no pending input).
pub(crate) fn builtin_input_pending_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("input-pending-p", &args, 0, 1)?;
    Ok(Value::Nil)
}

/// `(discard-input)` -> nil
///
/// Discard the contents of the terminal input buffer and cancel any pending
/// keyboard macro execution.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_discard_input(args: Vec<Value>) -> EvalResult {
    expect_args("discard-input", &args, 0)?;
    Ok(Value::Nil)
}

/// `(recursion-depth)` -> 0
///
/// Return the current depth in recursive edits.
///
/// Stub implementation: returns 0 (top level).
pub(crate) fn builtin_recursion_depth(args: Vec<Value>) -> EvalResult {
    expect_args("recursion-depth", &args, 0)?;
    Ok(Value::Int(0))
}

/// `(current-idle-time)` -> nil
///
/// Return the current length of Emacs idleness as a time value, or nil if
/// not idle.
///
/// Stub implementation: returns nil (not idle).
pub(crate) fn builtin_current_idle_time(args: Vec<Value>) -> EvalResult {
    expect_args("current-idle-time", &args, 0)?;
    Ok(Value::Nil)
}

/// `(event-convert-list LIST)` -> symbol or nil
///
/// Convert the event description LIST to an event type.
/// LIST should contain symbols representing modifier names and an event type.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_event_convert_list(args: Vec<Value>) -> EvalResult {
    expect_args("event-convert-list", &args, 1)?;
    Ok(Value::Nil)
}

/// `(internal-event-symbol-parse-modifiers SYMBOL)` -> list or nil
///
/// Parse SYMBOL as an event, returning a list of the base event and modifiers.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_internal_event_symbol_parse_modifiers(args: Vec<Value>) -> EvalResult {
    expect_args("internal-event-symbol-parse-modifiers", &args, 1)?;
    Ok(Value::Nil)
}

/// `(posn-at-x-y X Y &optional FRAME-OR-WINDOW WHOLE)` -> list or nil
///
/// Return buffer position information for pixel coordinates X and Y.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_posn_at_x_y(args: Vec<Value>) -> EvalResult {
    expect_range_args("posn-at-x-y", &args, 2, 4)?;
    Ok(Value::Nil)
}

/// `(posn-at-point &optional POS WINDOW)` -> list or nil
///
/// Return position information for buffer position POS in WINDOW.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_posn_at_point(args: Vec<Value>) -> EvalResult {
    expect_range_args("posn-at-point", &args, 0, 2)?;
    Ok(Value::Nil)
}

/// `(reset-this-command-lengths)` -> nil
///
/// Make the unread events replace the last command and echo.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_reset_this_command_lengths(args: Vec<Value>) -> EvalResult {
    expect_args("reset-this-command-lengths", &args, 0)?;
    Ok(Value::Nil)
}

/// `(clear-this-command-keys)` -> nil
///
/// Clear the record of keys typed so far in this command.
///
/// Stub implementation: returns nil.
pub(crate) fn builtin_clear_this_command_keys(args: Vec<Value>) -> EvalResult {
    expect_args("clear-this-command-keys", &args, 0)?;
    Ok(Value::Nil)
}

/// `(top-level)` -> nil
///
/// Exit all recursive editing levels.
pub(crate) fn builtin_top_level(args: Vec<Value>) -> EvalResult {
    super::super::minibuffer::builtin_top_level(args)
}

/// `(exit-recursive-edit)` -> non-local exit when active, otherwise user-error
///
/// Exit from the innermost recursive edit or minibuffer.
pub(crate) fn builtin_exit_recursive_edit(args: Vec<Value>) -> EvalResult {
    super::super::minibuffer::builtin_exit_recursive_edit(args)
}

/// `(abort-recursive-edit)` -> non-local abort when active, otherwise user-error
///
/// Abort the command that requested this recursive edit or minibuffer input.
pub(crate) fn builtin_abort_recursive_edit(args: Vec<Value>) -> EvalResult {
    super::super::minibuffer::builtin_abort_recursive_edit(args)
}
