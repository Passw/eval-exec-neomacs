use super::args::{expect_args, expect_max_args};
use super::{EvalResult, Value};

/// `(tty-display-color-p &optional TERMINAL)` -> t
///
/// Return non-nil if the terminal can display colors.
/// Neomacs is graphical, so always returns t.
pub(crate) fn builtin_tty_display_color_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-display-color-p", &args, 1)?;
    Ok(Value::True)
}

/// `(tty-display-color-cells &optional TERMINAL)` -> 16777216
///
/// Return the number of colors the terminal can display.
/// Returns 16777216 (2^24, 24-bit true color).
pub(crate) fn builtin_tty_display_color_cells(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-display-color-cells", &args, 1)?;
    Ok(Value::Int(16777216))
}

/// `(tty-type &optional TERMINAL)` -> nil
///
/// Return the type of the terminal (e.g. "xterm").
/// Since neomacs is not a TTY, returns nil.
pub(crate) fn builtin_tty_type(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-type", &args, 1)?;
    Ok(Value::Nil)
}

/// `(controlling-tty-p &optional TERMINAL)` -> nil
///
/// Return non-nil if TERMINAL is the controlling terminal of the process.
/// Since neomacs is graphical, always returns nil.
pub(crate) fn builtin_controlling_tty_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("controlling-tty-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(tty-no-underline &optional TERMINAL)` -> nil
///
/// Return non-nil if the terminal cannot display underlines.
/// Neomacs can display underlines, so returns nil.
pub(crate) fn builtin_tty_no_underline(args: Vec<Value>) -> EvalResult {
    expect_max_args("tty-no-underline", &args, 1)?;
    Ok(Value::Nil)
}

/// `(suspend-tty &optional TTY)` -> nil
///
/// Suspend the terminal device TTY.
/// Stub: always returns nil.
pub(crate) fn builtin_suspend_tty(args: Vec<Value>) -> EvalResult {
    expect_max_args("suspend-tty", &args, 1)?;
    Ok(Value::Nil)
}

/// `(resume-tty &optional TTY)` -> nil
///
/// Resume the previously suspended terminal device TTY.
/// Stub: always returns nil.
pub(crate) fn builtin_resume_tty(args: Vec<Value>) -> EvalResult {
    expect_max_args("resume-tty", &args, 1)?;
    Ok(Value::Nil)
}

/// `(serial-terminal-p &optional TERMINAL)` -> nil
///
/// Return non-nil if TERMINAL is a serial terminal.
/// Neomacs is graphical, so always returns nil.
pub(crate) fn builtin_serial_terminal_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("serial-terminal-p", &args, 1)?;
    Ok(Value::Nil)
}
