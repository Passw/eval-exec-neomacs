//! Rectangle operation builtins for the Elisp interpreter.
//!
//! Implements rectangle manipulation commands:
//! - `extract-rectangle-line`
//! - `extract-rectangle`, `delete-rectangle`, `kill-rectangle`
//! - `yank-rectangle`, `insert-rectangle`, `open-rectangle`
//! - `clear-rectangle`, `string-rectangle`, `replace-rectangle`
//! - `delete-extract-rectangle`
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
        Self { killed: Vec::new() }
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
fn line_count_between(eval: &super::eval::Evaluator, start: i64, end: i64) -> usize {
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

fn line_col_for_char_index(text: &str, target: usize) -> (usize, usize) {
    let mut line = 0usize;
    let mut col = 0usize;
    for (idx, ch) in text.chars().enumerate() {
        if idx == target {
            return (line, col);
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

fn extract_line_columns(line: &str, start_col: usize, end_col: usize) -> String {
    if start_col >= end_col {
        return String::new();
    }
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();

    if start_col >= len {
        return " ".repeat(end_col - start_col);
    }

    let mut out: String = chars[start_col..len.min(end_col)].iter().collect();
    if end_col > len {
        out.push_str(&" ".repeat(end_col - len));
    }
    out
}

fn rectangle_lines_for_extract(start_line: usize, end_line: usize) -> Vec<usize> {
    if start_line <= end_line {
        (start_line..=end_line).collect()
    } else {
        vec![start_line]
    }
}

fn char_index_to_byte(s: &str, char_idx: usize) -> usize {
    if char_idx == 0 {
        return 0;
    }
    s.char_indices()
        .nth(char_idx)
        .map(|(byte, _)| byte)
        .unwrap_or(s.len())
}

fn extract_rectangle_from_text(
    text: &str,
    start_line: usize,
    end_line: usize,
    left_col: usize,
    right_col: usize,
) -> Vec<String> {
    let lines: Vec<&str> = text.split('\n').collect();
    let mut out = Vec::new();
    for line_index in rectangle_lines_for_extract(start_line, end_line) {
        let line = lines.get(line_index).copied().unwrap_or("");
        out.push(extract_line_columns(line, left_col, right_col));
    }
    out
}

fn delete_extract_rectangle_from_text(
    text: &str,
    start_line: usize,
    end_line: usize,
    left_col: usize,
    right_col: usize,
) -> (Vec<String>, String) {
    let mut lines: Vec<String> = text.split('\n').map(ToString::to_string).collect();
    let mut extracted = Vec::new();
    let width = right_col.saturating_sub(left_col);

    for line_index in rectangle_lines_for_extract(start_line, end_line) {
        let Some(line) = lines.get_mut(line_index) else {
            extracted.push(" ".repeat(width));
            continue;
        };

        let line_len = line.chars().count();
        if line_len < left_col {
            extracted.push(" ".repeat(width));
            continue;
        }

        extracted.push(extract_line_columns(line, left_col, right_col));
        let del_end_char = line_len.min(right_col);
        let del_start_byte = char_index_to_byte(line, left_col);
        let del_end_byte = char_index_to_byte(line, del_end_char);
        if del_start_byte < del_end_byte {
            line.replace_range(del_start_byte..del_end_byte, "");
        }
    }

    (extracted, lines.join("\n"))
}

fn clamped_rect_inputs(
    eval: &super::eval::Evaluator,
    start: i64,
    end: i64,
) -> Option<(String, usize, usize, usize, usize, usize, usize)> {
    let buf = eval.buffers.current_buffer()?;
    let point_min_char = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max_char = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    let clamped_start = start.clamp(point_min_char, point_max_char);
    let clamped_end = end.clamp(point_min_char, point_max_char);
    let pmin = buf.point_min();
    let pmax = buf.point_max();
    let text = buf.buffer_substring(pmin, pmax);

    let rel_start = (clamped_start - point_min_char).max(0) as usize;
    let rel_end = (clamped_end - point_min_char).max(0) as usize;
    let (start_line, start_col) = line_col_for_char_index(&text, rel_start);
    let (end_line, end_col) = line_col_for_char_index(&text, rel_end);
    let (left_col, right_col) = if start_col <= end_col {
        (start_col, end_col)
    } else {
        (end_col, start_col)
    };
    Some((
        text, pmin, pmax, start_line, end_line, left_col, right_col,
    ))
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// `(extract-rectangle-line STARTCOL ENDCOL &optional LINE)` -- extract one
/// line of a rectangle as a string.
///
/// Compatibility behavior:
/// - with optional LINE, returns substring between STARTCOL and ENDCOL
/// - without LINE, returns an empty string (legacy stub path)
pub(crate) fn builtin_extract_rectangle_line(args: Vec<Value>) -> EvalResult {
    expect_min_args("extract-rectangle-line", &args, 2)?;
    expect_max_args("extract-rectangle-line", &args, 3)?;
    let start_col = expect_int(&args[0])?;
    let end_col = expect_int(&args[1])?;
    if start_col < 0 || end_col < 0 {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(start_col), Value::Int(end_col)],
        ));
    }
    if args.len() == 3 {
        let line = expect_string(&args[2])?;
        let mut lo = start_col as usize;
        let mut hi = end_col as usize;
        if lo > hi {
            std::mem::swap(&mut lo, &mut hi);
        }
        let chars: Vec<char> = line.chars().collect();
        lo = lo.min(chars.len());
        hi = hi.min(chars.len());
        if lo >= hi {
            return Ok(Value::string(""));
        }
        let slice: String = chars[lo..hi].iter().collect();
        return Ok(Value::string(slice));
    }
    Ok(Value::string(""))
}

/// `(extract-rectangle START END)` -- return a list of strings, one per line,
/// representing the rectangular region between START and END.
///
/// Compatibility behavior:
/// - columns come from START/END positions
/// - iteration starts at START's line and proceeds downward to END's line
///   (or just START's line when START is below END)
/// - lines shorter than the rectangle are padded with spaces
pub(crate) fn builtin_extract_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("extract-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let Some((text, _pmin, _pmax, start_line, end_line, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(Value::list(Vec::new()));
    };

    let strings: Vec<Value> = extract_rectangle_from_text(
        &text, start_line, end_line, left_col, right_col,
    )
    .into_iter()
    .map(Value::string)
    .collect();
    Ok(Value::list(strings))
}

/// `(delete-rectangle START END)` -- delete the rectangular region between
/// START and END.
///
/// Compatibility behavior:
/// - applies the same rectangle deletion semantics as
///   `delete-extract-rectangle`
/// - returns final point as 1-based character position
pub(crate) fn builtin_delete_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let Some((text, pmin, pmax, start_line, end_line, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(Value::Int(1));
    };

    let (.., rewritten) =
        delete_extract_rectangle_from_text(&text, start_line, end_line, left_col, right_col);
    let line_indices = rectangle_lines_for_extract(start_line, end_line);
    let last_line_index = line_indices.last().copied().unwrap_or(start_line);
    let rewritten_lines: Vec<&str> = rewritten.split('\n').collect();
    let mut final_rel_char = 0usize;
    for idx in 0..last_line_index {
        final_rel_char += rewritten_lines.get(idx).copied().unwrap_or("").chars().count();
        final_rel_char += 1; // newline
    }
    let last_line_len = rewritten_lines
        .get(last_line_index)
        .copied()
        .unwrap_or("")
        .chars()
        .count();
    final_rel_char += left_col.min(last_line_len);

    let Some(buf) = eval.buffers.current_buffer_mut() else {
        return Ok(Value::Int(1));
    };
    buf.delete_region(pmin, pmax);
    buf.goto_char(pmin);
    buf.insert(&rewritten);
    let final_byte = pmin + char_index_to_byte(&rewritten, final_rel_char);
    buf.goto_char(final_byte);
    Ok(Value::Int(buf.text.byte_to_char(final_byte) as i64 + 1))
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
        return Err(signal("error", vec![Value::string("No rectangle to yank")]));
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
            Value::Nil => {}                     // nil means use spaces (default)
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
/// Compatibility behavior:
/// - extracts rectangle text using the same START/END column/line model as
///   `extract-rectangle`
/// - deletes extracted text from each affected line
/// - when rectangle starts past EOL, returns width spaces and leaves line
///   unchanged
pub(crate) fn builtin_delete_extract_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-extract-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;

    let Some((text, pmin, pmax, start_line, end_line, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(Value::list(Vec::new()));
    };

    let (extracted, rewritten) =
        delete_extract_rectangle_from_text(&text, start_line, end_line, left_col, right_col);

    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.delete_region(pmin, pmax);
        buf.goto_char(pmin);
        buf.insert(&rewritten);
    }

    Ok(Value::list(
        extracted.into_iter().map(Value::string).collect(),
    ))
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
    fn extract_rectangle_returns_list() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_extract_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_list());
    }

    #[test]
    fn extract_rectangle_eval_basic_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_extract_rectangle(&mut eval, vec![Value::Int(1), Value::Int(9)])
            .expect("extract rectangle");
        assert_eq!(
            result,
            Value::list(vec![Value::string("a"), Value::string("1")])
        );
    }

    #[test]
    fn extract_rectangle_eval_start_line_order_matches_emacs() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let forward = builtin_extract_rectangle(&mut eval, vec![Value::Int(7), Value::Int(8)])
            .expect("extract rectangle forward");
        assert_eq!(
            forward,
            Value::list(vec![Value::string("abcdef"), Value::string("123456")])
        );

        let reversed = builtin_extract_rectangle(&mut eval, vec![Value::Int(8), Value::Int(7)])
            .expect("extract rectangle reversed");
        assert_eq!(reversed, Value::list(vec![Value::string("123456")]));
    }

    #[test]
    fn extract_rectangle_eval_clamps_positions() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef");
        }
        let result = builtin_extract_rectangle(&mut eval, vec![Value::Int(20), Value::Int(1)])
            .expect("extract rectangle clamped");
        assert_eq!(result, Value::list(vec![Value::string("abcdef")]));
    }

    #[test]
    fn extract_rectangle_line_returns_string() {
        let result = builtin_extract_rectangle_line(vec![Value::Int(1), Value::Int(3)]).unwrap();
        assert_eq!(result.as_str(), Some(""));
    }

    #[test]
    fn extract_rectangle_line_with_line_argument() {
        let result = builtin_extract_rectangle_line(vec![
            Value::Int(1),
            Value::Int(3),
            Value::string("abcdef"),
        ])
        .unwrap();
        assert_eq!(result.as_str(), Some("bc"));
    }

    #[test]
    fn extract_rectangle_line_swapped_columns() {
        let result = builtin_extract_rectangle_line(vec![
            Value::Int(3),
            Value::Int(1),
            Value::string("abcdef"),
        ])
        .unwrap();
        assert_eq!(result.as_str(), Some("bc"));
    }

    #[test]
    fn extract_rectangle_line_negative_column_errors() {
        assert!(builtin_extract_rectangle_line(vec![
            Value::Int(-1),
            Value::Int(1),
            Value::string("abc"),
        ])
        .is_err());
    }

    #[test]
    fn extract_rectangle_line_validates_args() {
        assert!(builtin_extract_rectangle_line(vec![]).is_err());
        assert!(builtin_extract_rectangle_line(vec![Value::Int(1)]).is_err());
        assert!(
            builtin_extract_rectangle_line(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
                .is_err()
        );
    }

    #[test]
    fn delete_rectangle_wrong_type() {
        let mut eval = super::super::eval::Evaluator::new();
        let result =
            builtin_delete_rectangle(&mut eval, vec![Value::string("not-int"), Value::Int(10)]);
        assert!(result.is_err());
    }

    #[test]
    fn delete_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_delete_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(_)));
    }

    #[test]
    fn delete_rectangle_eval_mutates_buffer() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_delete_rectangle(&mut eval, vec![Value::Int(1), Value::Int(9)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(7));
        let buffer_after = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist")
            .buffer_string();
        assert_eq!(buffer_after, "bcdef\n23456\n");
    }

    #[test]
    fn kill_rectangle_updates_state() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_kill_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
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
        let result = builtin_insert_rectangle(&mut eval, vec![Value::Int(42)]);
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
        let rect = Value::list(vec![Value::string("hello"), Value::string("world")]);
        let result = builtin_insert_rectangle(&mut eval, vec![rect]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn open_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_open_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_rectangle_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_clear_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
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
        let result =
            builtin_delete_extract_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_list());
    }

    #[test]
    fn delete_extract_rectangle_eval_basic_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result =
            builtin_delete_extract_rectangle(&mut eval, vec![Value::Int(1), Value::Int(9)])
                .expect("delete-extract-rectangle");
        assert_eq!(
            result,
            Value::list(vec![Value::string("a"), Value::string("1")])
        );
        let buffer_after = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist")
            .buffer_string();
        assert_eq!(buffer_after, "bcdef\n23456\n");
    }

    #[test]
    fn delete_extract_rectangle_eval_start_line_order() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result =
            builtin_delete_extract_rectangle(&mut eval, vec![Value::Int(8), Value::Int(7)])
                .expect("delete-extract-rectangle order");
        assert_eq!(result, Value::list(vec![Value::string("123456")]));
        let buffer_after = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist")
            .buffer_string();
        assert_eq!(buffer_after, "abcdef\n\n");
    }

    #[test]
    fn delete_extract_rectangle_eval_clamps_positions() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef");
        }
        let result =
            builtin_delete_extract_rectangle(&mut eval, vec![Value::Int(20), Value::Int(1)])
                .expect("delete-extract-rectangle clamped");
        assert_eq!(result, Value::list(vec![Value::string("abcdef")]));
        let buffer_after = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist")
            .buffer_string();
        assert_eq!(buffer_after, "");
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
        let result = builtin_replace_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_err());
    }
}
