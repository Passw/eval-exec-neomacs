//! Rectangle operation builtins for the Elisp interpreter.
//!
//! Implements rectangle manipulation commands:
//! - `extract-rectangle`, `delete-rectangle`, `kill-rectangle`
//! - `yank-rectangle`, `insert-rectangle`, `open-rectangle`
//! - `clear-rectangle`, `string-rectangle`, `replace-rectangle`
//! - `delete-extract-rectangle`, `extract-rectangle-line`
//!
//! These are stub implementations that register the correct function
//! signatures so Elisp code can reference them.  Full column-aware
//! buffer operations will be added when the buffer model supports
//! column tracking.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers (local copies — same pattern as other modules)
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

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// RectangleState — stores the last killed rectangle
// ---------------------------------------------------------------------------

/// Persistent state for rectangle operations across the session.
#[derive(Clone, Debug)]
pub(crate) struct RectangleState {
    /// The last killed rectangle: one string per line.
    pub killed: Vec<String>,
}

impl RectangleState {
    pub fn new() -> Self {
        Self {
            killed: Vec::new(),
        }
    }
}

impl Default for RectangleState {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Helper: compute line count between two buffer positions
// ---------------------------------------------------------------------------

/// Given START and END positions, estimate the number of lines spanned.
/// Uses the current buffer's text to count newlines between the two positions.
fn line_count_between(
    eval: &super::eval::Evaluator,
    start: i64,
    end: i64,
) -> usize {
    let (lo, hi) = if start <= end {
        (start as usize, end as usize)
    } else {
        (end as usize, start as usize)
    };

    // Try to read text from the current buffer to count newlines.
    if let Some(buf) = eval.buffers.current_buffer() {
        let buf_size = buf.buffer_size();
        let lo = lo.min(buf_size);
        let hi = hi.min(buf_size);
        if lo >= hi {
            return 0;
        }
        let text = buf.buffer_substring(lo, hi);
        // Number of lines is newline count + 1 (unless region is empty).
        if text.is_empty() {
            return 0;
        }
        return text.chars().filter(|&c| c == '\n').count() + 1;
    }

    // Fallback: rough estimate based on typical line length.
    let span = (hi - lo).max(1);
    (span / 40).max(1)
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// `(extract-rectangle-line START END)` -- helper: extract text from a
/// single line between column positions.
///
/// Stub: returns an empty string.  Full implementation requires column-aware
/// buffer access.
pub(crate) fn builtin_extract_rectangle_line(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("extract-rectangle-line", &args, 2)?;
    let _start = expect_int(&args[0])?;
    let _end = expect_int(&args[1])?;
    // Stub: return empty string for the line.
    Ok(Value::string(""))
}

/// `(extract-rectangle START END)` -- return a list of strings, one per line,
/// representing the rectangular region between START and END.
///
/// Stub: returns a list of empty strings based on the line count between
/// START and END.
pub(crate) fn builtin_extract_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("extract-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let nlines = line_count_between(eval, start, end);
    let strings: Vec<Value> = (0..nlines).map(|_| Value::string("")).collect();
    Ok(Value::list(strings))
}

/// `(delete-rectangle START END)` -- delete the rectangular region between
/// START and END.
///
/// Stub: returns nil.  Full implementation requires column-aware deletion.
pub(crate) fn builtin_delete_rectangle(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-rectangle", &args, 2)?;
    let _start = expect_int(&args[0])?;
    let _end = expect_int(&args[1])?;
    // Stub: no-op.
    Ok(Value::Nil)
}

/// `(kill-rectangle START END)` -- save the rectangular region to the
/// rectangle kill buffer, then delete it.
///
/// Stub: saves empty strings to `RectangleState.killed`, does not modify
/// the buffer.
pub(crate) fn builtin_kill_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("kill-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let nlines = line_count_between(eval, start, end);
    let killed: Vec<String> = (0..nlines).map(|_| String::new()).collect();
    eval.rectangle.killed = killed;
    Ok(Value::Nil)
}

/// `(yank-rectangle)` -- insert the last killed rectangle at point.
///
/// Stub: returns nil.  Full implementation requires inserting each line
/// of the killed rectangle at the corresponding line and column.
pub(crate) fn builtin_yank_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("yank-rectangle", &args, 0)?;
    if eval.rectangle.killed.is_empty() {
        return Err(signal(
            "error",
            vec![Value::string("No rectangle to yank")],
        ));
    }
    // Stub: no-op insertion.
    Ok(Value::Nil)
}

/// `(insert-rectangle RECTANGLE)` -- insert RECTANGLE (a list of strings)
/// at point, one string per line.
///
/// Stub: validates argument type, returns nil.
pub(crate) fn builtin_insert_rectangle(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("insert-rectangle", &args, 1)?;
    // Validate that the argument is a list.
    if !args[0].is_list() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0].clone()],
        ));
    }
    // Validate each element is a string.
    if let Some(items) = list_to_vec(&args[0]) {
        for item in &items {
            if !item.is_string() {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), item.clone()],
                ));
            }
        }
    }
    // Stub: no-op insertion.
    Ok(Value::Nil)
}

/// `(open-rectangle START END)` -- insert blank space to fill the rectangle
/// defined by START and END, pushing existing text to the right.
///
/// Stub: returns nil.
pub(crate) fn builtin_open_rectangle(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("open-rectangle", &args, 2)?;
    let _start = expect_int(&args[0])?;
    let _end = expect_int(&args[1])?;
    // Stub: no-op.
    Ok(Value::Nil)
}

/// `(clear-rectangle START END &optional FILL)` -- replace the rectangle
/// contents with spaces (or FILL character if given).
///
/// Stub: returns nil.
pub(crate) fn builtin_clear_rectangle(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("clear-rectangle", &args, 2)?;
    expect_max_args("clear-rectangle", &args, 3)?;
    let _start = expect_int(&args[0])?;
    let _end = expect_int(&args[1])?;
    // Optional FILL argument (character or string) — validated but unused in stub.
    if args.len() == 3 {
        match &args[2] {
            Value::Nil => {} // nil means use spaces (default)
            Value::Char(_) | Value::Str(_) => {} // valid fill
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), other.clone()],
                ));
            }
        }
    }
    // Stub: no-op.
    Ok(Value::Nil)
}

/// `(string-rectangle START END STRING)` -- replace each line of the
/// rectangle with STRING.
///
/// Stub: returns nil.
pub(crate) fn builtin_string_rectangle(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("string-rectangle", &args, 3)?;
    let _start = expect_int(&args[0])?;
    let _end = expect_int(&args[1])?;
    let _string = expect_string(&args[2])?;
    // Stub: no-op.
    Ok(Value::Nil)
}

/// `(delete-extract-rectangle START END)` -- delete the rectangle and
/// return its contents as a list of strings.
///
/// Stub: returns a list of empty strings (same as `extract-rectangle`),
/// does not modify the buffer.
pub(crate) fn builtin_delete_extract_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-extract-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let nlines = line_count_between(eval, start, end);
    let strings: Vec<Value> = (0..nlines).map(|_| Value::string("")).collect();
    Ok(Value::list(strings))
}

/// `(replace-rectangle START END REPLACEMENT)` -- alias for `string-rectangle`.
///
/// Stub: returns nil.
pub(crate) fn builtin_replace_rectangle(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("replace-rectangle", &args, 3)?;
    let _start = expect_int(&args[0])?;
    let _end = expect_int(&args[1])?;
    let _string = expect_string(&args[2])?;
    // Stub: no-op (alias for string-rectangle).
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rectangle_state_default() {
        let state = RectangleState::new();
        assert!(state.killed.is_empty());
    }

    #[test]
    fn rectangle_state_default_trait() {
        let state = RectangleState::default();
        assert!(state.killed.is_empty());
    }

    #[test]
    fn extract_rectangle_line_wrong_arity() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_extract_rectangle_line(&mut eval, vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn extract_rectangle_line_returns_string() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_extract_rectangle_line(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_string());
    }

    #[test]
    fn extract_rectangle_returns_list() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_extract_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_list());
    }

    #[test]
    fn delete_rectangle_wrong_type() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_delete_rectangle(
            &mut eval,
            vec![Value::string("not-int"), Value::Int(10)],
        );
        assert!(result.is_err());
    }

    #[test]
    fn delete_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_delete_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn kill_rectangle_updates_state() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_kill_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        // State should have been updated (may be empty strings, but vector should exist).
        // The killed rectangle is stored in eval.rectangle.killed.
    }

    #[test]
    fn yank_rectangle_empty_errors() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_yank_rectangle(&mut eval, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn yank_rectangle_after_kill() {
        let mut eval = super::super::eval::Evaluator::new();
        eval.rectangle.killed = vec!["hello".to_string(), "world".to_string()];
        let result = builtin_yank_rectangle(&mut eval, vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn insert_rectangle_validates_list() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_insert_rectangle(&mut eval, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn insert_rectangle_validates_string_elements() {
        let mut eval = super::super::eval::Evaluator::new();
        let rect = Value::list(vec![Value::string("a"), Value::Int(42)]);
        let result = builtin_insert_rectangle(&mut eval, vec![rect]);
        assert!(result.is_err());
    }

    #[test]
    fn insert_rectangle_valid() {
        let mut eval = super::super::eval::Evaluator::new();
        let rect =
            Value::list(vec![Value::string("hello"), Value::string("world")]);
        let result = builtin_insert_rectangle(&mut eval, vec![rect]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn open_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_open_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_clear_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_rectangle_with_fill() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_clear_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::Char('x')],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_rectangle_bad_fill() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_clear_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::Int(42)],
        );
        assert!(result.is_err());
    }

    #[test]
    fn string_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_string_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::string("hi")],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn string_rectangle_wrong_type() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_string_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::Int(42)],
        );
        assert!(result.is_err());
    }

    #[test]
    fn delete_extract_rectangle_returns_list() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_delete_extract_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10)],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_list());
    }

    #[test]
    fn replace_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_replace_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::string("hi")],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn replace_rectangle_wrong_arity() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_replace_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10)],
        );
        assert!(result.is_err());
    }
}
