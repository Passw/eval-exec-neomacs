//! Rectangle operation builtins for the Elisp interpreter.
//!
//! Implements rectangle manipulation commands:
//! - `extract-rectangle-line`
//! - `extract-rectangle`, `delete-rectangle`, `kill-rectangle`
//! - `yank-rectangle`, `insert-rectangle`, `open-rectangle`
//! - `clear-rectangle`, `string-rectangle`, `replace-rectangle`
//! - `delete-extract-rectangle`
//!
//! These implement compatibility-focused rectangle behavior used by
//! vm-compat batches. Remaining edge drift is tracked and locked by
//! oracle corpora.

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

fn expect_char_or_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        Value::Char(c) => Ok(c.to_string()),
        Value::Int(n) if *n >= 0 => match char::from_u32(*n as u32) {
            Some(ch) => Ok(ch.to_string()),
            None => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("char-or-string-p"), value.clone()],
            )),
        },
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), value.clone()],
        )),
    }
}

fn dynamic_or_global_symbol_value(eval: &super::eval::Evaluator, name: &str) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get(name) {
            return Some(value.clone());
        }
    }
    eval.obarray.symbol_value(name).cloned()
}

fn rectangle_strings_to_value(rectangle: &[String]) -> Value {
    Value::list(rectangle.iter().map(|s| Value::string(s.clone())).collect())
}

fn rectangle_strings_from_value(value: &Value) -> Result<Vec<String>, Flow> {
    let items = list_to_vec(value).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), value.clone()],
        )
    })?;
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        match item {
            Value::Str(s) => out.push((*s).clone()),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("buffer-or-string-p"), other],
                ));
            }
        }
    }
    Ok(out)
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

fn line_col_to_char_index(text: &str, line: usize, col: usize) -> usize {
    let lines: Vec<&str> = text.split('\n').collect();
    let mut pos = 0usize;
    for idx in 0..line {
        pos += lines.get(idx).copied().unwrap_or("").chars().count();
        pos += 1; // newline separator
    }
    let line_len = lines.get(line).copied().unwrap_or("").chars().count();
    pos + col.min(line_len)
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

fn insert_rectangle_into_text(
    text: &str,
    start_line: usize,
    start_col: usize,
    rectangle: &[String],
) -> (String, usize) {
    if rectangle.is_empty() {
        return (
            text.to_string(),
            line_col_to_char_index(text, start_line, start_col),
        );
    }

    let mut lines: Vec<String> = text.split('\n').map(ToString::to_string).collect();
    for (offset, segment) in rectangle.iter().enumerate() {
        let line_index = start_line + offset;
        while lines.len() <= line_index {
            lines.push(String::new());
        }
        let line = &mut lines[line_index];
        let line_len = line.chars().count();
        if line_len < start_col {
            line.push_str(&" ".repeat(start_col - line_len));
        }
        let insert_byte = char_index_to_byte(line, start_col);
        line.insert_str(insert_byte, segment);
    }

    let rewritten = lines.join("\n");
    let final_line = start_line + rectangle.len() - 1;
    let final_col = start_col + rectangle.last().map(|s| s.chars().count()).unwrap_or(0);
    let final_char = line_col_to_char_index(&rewritten, final_line, final_col);
    (rewritten, final_char)
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

fn string_rectangle_into_text(
    text: &str,
    start_line: usize,
    end_line: usize,
    left_col: usize,
    right_col: usize,
    replacement: &str,
) -> (String, usize) {
    let mut lines: Vec<String> = text.split('\n').map(ToString::to_string).collect();
    let line_indices = rectangle_lines_for_extract(start_line, end_line);

    for line_index in &line_indices {
        while lines.len() <= *line_index {
            lines.push(String::new());
        }
        let line = &mut lines[*line_index];
        let line_len = line.chars().count();
        if line_len < left_col {
            line.push_str(&" ".repeat(left_col - line_len));
        }
        let line_len = line.chars().count();
        let del_end_char = line_len.min(right_col);
        let del_start_byte = char_index_to_byte(line, left_col);
        let del_end_byte = char_index_to_byte(line, del_end_char);
        if del_start_byte < del_end_byte {
            line.replace_range(del_start_byte..del_end_byte, "");
        }
        let insert_at = char_index_to_byte(line, left_col);
        line.insert_str(insert_at, replacement);
    }

    let rewritten = lines.join("\n");
    let last_line = line_indices.last().copied().unwrap_or(start_line);
    let final_col = left_col + replacement.chars().count();
    let final_rel_char = line_col_to_char_index(&rewritten, last_line, final_col);
    (rewritten, final_rel_char)
}

fn clamped_rect_inputs(
    eval: &super::eval::Evaluator,
    start: i64,
    end: i64,
) -> Option<(
    String,
    usize,
    usize,
    usize,
    usize,
    usize,
    usize,
    usize,
    usize,
)> {
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
        text, pmin, pmax, start_line, start_col, end_line, end_col, left_col, right_col,
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
    let Some((text, _pmin, _pmax, start_line, _start_col, end_line, _end_col, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(Value::list(Vec::new()));
    };

    let strings: Vec<Value> =
        extract_rectangle_from_text(&text, start_line, end_line, left_col, right_col)
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
    let Some((text, pmin, pmax, start_line, _start_col, end_line, _end_col, left_col, right_col)) =
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
        final_rel_char += rewritten_lines
            .get(idx)
            .copied()
            .unwrap_or("")
            .chars()
            .count();
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
/// Compatibility behavior:
/// - performs the same extraction/deletion as `delete-extract-rectangle`
/// - updates `RectangleState.killed`
/// - returns the extracted rectangle list
pub(crate) fn builtin_kill_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("kill-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let extracted =
        builtin_delete_extract_rectangle(eval, vec![Value::Int(start), Value::Int(end)])?;
    let killed = list_to_vec(&extracted)
        .unwrap_or_default()
        .into_iter()
        .filter_map(|value| value.as_str().map(ToString::to_string))
        .collect();
    eval.rectangle.killed = killed;
    eval.obarray.set_symbol_value(
        "killed-rectangle",
        rectangle_strings_to_value(&eval.rectangle.killed),
    );
    Ok(extracted)
}

/// `(yank-rectangle)` -- insert the last killed rectangle at point.
///
/// Compatibility behavior:
/// - inserts `RectangleState.killed` at point using `insert-rectangle`
///   semantics
/// - returns nil when no rectangle has been killed yet
pub(crate) fn builtin_yank_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("yank-rectangle", &args, 0)?;
    let rectangle = if let Some(value) = dynamic_or_global_symbol_value(eval, "killed-rectangle") {
        if value.is_nil() {
            Vec::new()
        } else {
            rectangle_strings_from_value(&value)?
        }
    } else {
        eval.rectangle.killed.clone()
    };
    if rectangle.is_empty() {
        return Ok(Value::Nil);
    }
    eval.rectangle.killed = rectangle.clone();
    eval.obarray
        .set_symbol_value("killed-rectangle", rectangle_strings_to_value(&rectangle));
    builtin_insert_rectangle(eval, vec![rectangle_strings_to_value(&rectangle)])
}

/// `(insert-rectangle RECTANGLE)` -- insert RECTANGLE (a list of strings)
/// at point, one string per line.
///
/// Compatibility behavior:
/// - inserts each rectangle row on subsequent lines, starting at point's
///   current line/column
/// - pads with spaces when insertion column is past EOL
/// - creates missing lines as needed
/// - moves point to end of the final inserted row
pub(crate) fn builtin_insert_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("insert-rectangle", &args, 1)?;
    let items = list_to_vec(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0].clone()],
        )
    })?;
    let mut rectangle = Vec::with_capacity(items.len());
    for item in &items {
        match item {
            Value::Str(s) => rectangle.push((**s).clone()),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("buffer-or-string-p"), other.clone()],
                ));
            }
        }
    }

    if rectangle.is_empty() {
        return Ok(Value::Nil);
    }

    let (text, pmin, pmax, start_line, start_col) = {
        let buf = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        let pmin = buf.point_min();
        let pmax = buf.point_max();
        let point_min_char = buf.text.byte_to_char(pmin) as i64 + 1;
        let point_max_char = buf.text.byte_to_char(pmax) as i64 + 1;
        let point_char = buf.text.byte_to_char(buf.point()) as i64 + 1;
        let clamped_point = point_char.clamp(point_min_char, point_max_char);
        let text = buf.buffer_substring(pmin, pmax);
        let rel_point = (clamped_point - point_min_char).max(0) as usize;
        let (start_line, start_col) = line_col_for_char_index(&text, rel_point);
        (text, pmin, pmax, start_line, start_col)
    };

    let (rewritten, final_rel_char) =
        insert_rectangle_into_text(&text, start_line, start_col, &rectangle);

    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.delete_region(pmin, pmax);
        buf.goto_char(pmin);
        buf.insert(&rewritten);
        let final_byte = pmin + char_index_to_byte(&rewritten, final_rel_char);
        buf.goto_char(final_byte);
    }

    Ok(Value::Nil)
}

/// `(open-rectangle START END)` -- insert blank space to fill the rectangle
/// defined by START and END, pushing existing text to the right.
pub(crate) fn builtin_open_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("open-rectangle", &args, 2)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;

    let Some((text, pmin, pmax, start_line, _start_col, end_line, _end_col, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(args[0].clone());
    };

    let width = right_col.saturating_sub(left_col);
    if width > 0 {
        let mut lines: Vec<String> = text.split('\n').map(ToString::to_string).collect();
        let spaces = " ".repeat(width);
        for line_index in rectangle_lines_for_extract(start_line, end_line) {
            while lines.len() <= line_index {
                lines.push(String::new());
            }
            let line = &mut lines[line_index];
            let line_len = line.chars().count();
            if line_len < left_col {
                line.push_str(&" ".repeat(left_col - line_len));
            }
            let insert_at = char_index_to_byte(line, left_col);
            line.insert_str(insert_at, &spaces);
        }

        let rewritten = lines.join("\n");
        if let Some(buf) = eval.buffers.current_buffer_mut() {
            buf.delete_region(pmin, pmax);
            buf.goto_char(pmin);
            buf.insert(&rewritten);
        }
    }

    if let Some(buf) = eval.buffers.current_buffer_mut() {
        let target_char = if start > 0 { start as usize - 1 } else { 0 };
        let target_byte = buf
            .text
            .char_to_byte(target_char.min(buf.text.char_count()));
        buf.goto_char(target_byte);
    }

    Ok(args[0].clone())
}

/// `(clear-rectangle START END &optional FILL)` -- replace the rectangle
/// contents with spaces (or FILL character if given).
///
/// Compatibility behavior:
/// - fills rectangle width with spaces, then trims trailing spaces in affected
///   lines
/// - optional FILL is accepted but ignored
/// - preserves caller point position by restoring original point after rewrite
///   (clamped if buffer shrinks)
/// - returns rectangle end position after rewrite (1-based char)
pub(crate) fn builtin_clear_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("clear-rectangle", &args, 2)?;
    expect_max_args("clear-rectangle", &args, 3)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let old_point_char = eval
        .buffers
        .current_buffer()
        .map(|buf| buf.point_char() as i64 + 1)
        .unwrap_or(1);

    let Some((text, pmin, pmax, start_line, _start_col, end_line, _end_col, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(Value::Int(1));
    };

    let width = right_col.saturating_sub(left_col);
    let spaces = " ".repeat(width);
    let mut lines: Vec<String> = text.split('\n').map(ToString::to_string).collect();
    let line_indices = rectangle_lines_for_extract(start_line, end_line);
    for line_index in &line_indices {
        while lines.len() <= *line_index {
            lines.push(String::new());
        }
        let line = &mut lines[*line_index];
        let line_len = line.chars().count();
        if line_len < left_col {
            line.push_str(&" ".repeat(left_col - line_len));
        }
        let line_len = line.chars().count();
        let del_end_char = line_len.min(right_col);
        let del_start_byte = char_index_to_byte(line, left_col);
        let del_end_byte = char_index_to_byte(line, del_end_char);
        if del_start_byte < del_end_byte {
            line.replace_range(del_start_byte..del_end_byte, "");
        }
        let insert_at = char_index_to_byte(line, left_col);
        line.insert_str(insert_at, &spaces);
        while line.ends_with(' ') {
            line.pop();
        }
    }

    let rewritten = lines.join("\n");
    let last_line = line_indices.last().copied().unwrap_or(start_line);
    let return_rel_char = line_col_to_char_index(&rewritten, last_line, left_col + width);
    let return_char = return_rel_char as i64 + 1;

    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.delete_region(pmin, pmax);
        buf.goto_char(pmin);
        buf.insert(&rewritten);
        let restore_char = if old_point_char > 0 {
            old_point_char as usize - 1
        } else {
            0
        };
        let restore_byte = buf
            .text
            .char_to_byte(restore_char.min(buf.text.char_count()));
        buf.goto_char(restore_byte);
    }

    Ok(Value::Int(return_char))
}

/// `(string-rectangle START END STRING)` -- replace each line of the
/// rectangle with STRING.
///
/// Compatibility behavior:
/// - replaces each target rectangle slice with STRING
/// - pads short lines before replacement when rectangle starts past EOL
/// - updates point to end of replacement on the final processed line
/// - returns new point as 1-based char position
pub(crate) fn builtin_string_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("string-rectangle", &args, 3)?;
    let start = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let replacement = expect_char_or_string(&args[2])?;
    let Some((text, pmin, pmax, start_line, _start_col, end_line, _end_col, left_col, right_col)) =
        clamped_rect_inputs(eval, start, end)
    else {
        return Ok(Value::Int(1));
    };

    let (rewritten, final_rel_char) = string_rectangle_into_text(
        &text,
        start_line,
        end_line,
        left_col,
        right_col,
        &replacement,
    );

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

    let Some((text, pmin, pmax, start_line, _start_col, end_line, _end_col, left_col, right_col)) =
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
/// Compatibility behavior:
/// - delegates to `string-rectangle` after `replace-rectangle` arity check
pub(crate) fn builtin_replace_rectangle(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("replace-rectangle", &args, 3)?;
    builtin_string_rectangle(eval, args)
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
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_kill_rectangle(&mut eval, vec![Value::Int(1), Value::Int(9)])
            .expect("kill-rectangle");
        assert_eq!(
            result,
            Value::list(vec![Value::string("a"), Value::string("1")])
        );
        assert_eq!(
            eval.rectangle.killed,
            vec!["a".to_string(), "1".to_string()]
        );
        assert_eq!(
            eval.obarray
                .symbol_value("killed-rectangle")
                .cloned()
                .expect("killed-rectangle set"),
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
    fn yank_rectangle_empty_returns_nil() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_yank_rectangle(&mut eval, vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn yank_rectangle_after_kill() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abc\ndef\n");
            buf.goto_char(0);
        }
        eval.rectangle.killed = vec!["X".to_string(), "Y".to_string()];
        let result = builtin_yank_rectangle(&mut eval, vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "Xabc\nYdef\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 7);
    }

    #[test]
    fn yank_rectangle_uses_killed_rectangle_symbol() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abc\ndef\n");
            buf.goto_char(0);
        }
        eval.obarray.set_symbol_value(
            "killed-rectangle",
            Value::list(vec![Value::string("Q"), Value::string("W")]),
        );
        let result = builtin_yank_rectangle(&mut eval, vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "Qabc\nWdef\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 7);
    }

    #[test]
    fn yank_rectangle_non_list_symbol_errors() {
        let mut eval = super::super::eval::Evaluator::new();
        eval.obarray
            .set_symbol_value("killed-rectangle", Value::Int(1));
        let result = builtin_yank_rectangle(&mut eval, vec![]);
        assert!(result.is_err());
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
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abc\ndef\n");
            buf.goto_char(0);
        }
        let rect = Value::list(vec![Value::string("hello"), Value::string("world")]);
        let result = builtin_insert_rectangle(&mut eval, vec![rect]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "helloabc\nworlddef\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 15);
    }

    #[test]
    fn insert_rectangle_extends_and_pads_lines() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abc");
            buf.goto_char(1);
        }
        let rect = Value::list(vec![
            Value::string("X"),
            Value::string("Y"),
            Value::string("Z"),
        ]);
        let result = builtin_insert_rectangle(&mut eval, vec![rect]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "aXbc\n Y\n Z");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 11);
    }

    #[test]
    fn insert_rectangle_empty_keeps_point_and_text() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abc");
            buf.goto_char(1);
        }
        let result = builtin_insert_rectangle(&mut eval, vec![Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "abc");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 2);
    }

    #[test]
    fn open_rectangle_returns_start() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_open_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(1));
    }

    #[test]
    fn open_rectangle_eval_mutates_buffer_and_point() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_open_rectangle(&mut eval, vec![Value::Int(1), Value::Int(9)])
            .expect("open-rectangle");
        assert_eq!(result, Value::Int(1));
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), " abcdef\n 123456\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 1);
    }

    #[test]
    fn clear_rectangle_returns_point() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_clear_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(_)));
    }

    #[test]
    fn clear_rectangle_with_fill() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_clear_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::Char('x')],
        );
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(_)));
    }

    #[test]
    fn clear_rectangle_accepts_non_char_fill() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_clear_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::Float(1.5)],
        );
        assert!(result.is_ok());
    }

    #[test]
    fn clear_rectangle_eval_mutates_and_restores_point() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_clear_rectangle(&mut eval, vec![Value::Int(1), Value::Int(9)])
            .expect("clear-rectangle");
        assert_eq!(result, Value::Int(9));
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), " bcdef\n 23456\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 15);
    }

    #[test]
    fn string_rectangle_returns_point() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_string_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::string("hi")],
        );
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(_)));
    }

    #[test]
    fn string_rectangle_wrong_type() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_string_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::Float(1.5)],
        );
        assert!(result.is_err());
    }

    #[test]
    fn string_rectangle_eval_mutates_buffer_and_point() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_string_rectangle(
            &mut eval,
            vec![Value::Int(2), Value::Int(10), Value::string("XX")],
        )
        .expect("string-rectangle");
        assert_eq!(result, Value::Int(12));
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "aXXcdef\n1XX3456\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 12);
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
    fn replace_rectangle_aliases_string_rectangle() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abcdef\n123456\n");
        }
        let result = builtin_replace_rectangle(
            &mut eval,
            vec![Value::Int(1), Value::Int(10), Value::string("hi")],
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(10));
        let buf = eval
            .buffers
            .current_buffer()
            .expect("current buffer must exist");
        assert_eq!(buf.buffer_string(), "hicdef\nhi3456\n");
        assert_eq!(buf.text.byte_to_char(buf.point()) as i64 + 1, 10);
    }

    #[test]
    fn replace_rectangle_wrong_arity() {
        let mut eval = super::super::eval::Evaluator::new();
        let result = builtin_replace_rectangle(&mut eval, vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_err());
    }
}
