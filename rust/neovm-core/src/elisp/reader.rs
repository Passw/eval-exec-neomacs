//! Reader/printer builtins: read-from-string, read, prin1-to-string (enhanced),
//! format-spec, and various interactive-input stubs.

use super::error::{signal, EvalResult, Flow};
use super::expr::Expr;
use super::value::*;

// ---------------------------------------------------------------------------
// Helpers
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

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn expect_number(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Int(_) | Value::Float(_) | Value::Char(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), other.clone()],
        )),
    }
}

fn expect_initial_input_stringish(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Nil | Value::Str(_) => Ok(()),
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            if !matches!(pair.car, Value::Str(_)) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), pair.car.clone()],
                ));
            }
            Ok(())
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn expect_completing_read_initial_input(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Nil | Value::Str(_) => Ok(()),
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            if !matches!(pair.car, Value::Str(_)) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), pair.car.clone()],
                ));
            }
            if !matches!(pair.cdr, Value::Int(_) | Value::Char(_)) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("number-or-marker-p"), pair.cdr.clone()],
                ));
            }
            Ok(())
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// 1. read-from-string
// ---------------------------------------------------------------------------

/// `(read-from-string STRING &optional START END)`
///
/// Parse a single Lisp object from STRING starting at position START (default 0).
/// Returns `(OBJECT . END-POSITION)` where END-POSITION is the character index
/// after the parsed object.
pub(crate) fn builtin_read_from_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-from-string", &args, 1)?;
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("read-from-string"), Value::Int(args.len() as i64)],
        ));
    }

    let full_string = expect_string(&args[0])?;

    let start_arg = args.get(1).cloned().unwrap_or(Value::Nil);
    let end_arg = args.get(2).cloned().unwrap_or(Value::Nil);
    let to_index = |value: &Value| -> Result<usize, Flow> {
        match value {
            Value::Nil => Ok(0),
            Value::Int(n) => {
                let idx = if *n < 0 {
                    (full_string.len() as i64) + *n
                } else {
                    *n
                };
                if idx < 0 || idx > full_string.len() as i64 {
                    return Err(signal(
                        "args-out-of-range",
                        vec![args[0].clone(), start_arg.clone(), end_arg.clone()],
                    ));
                }
                Ok(idx as usize)
            }
            other => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("integerp"), other.clone()],
            )),
        }
    };
    let start = if args.len() > 1 {
        to_index(&start_arg)?
    } else {
        0
    };
    let end = if args.len() > 2 {
        to_index(&end_arg)?
    } else {
        full_string.len()
    };

    if start > end {
        return Err(signal(
            "args-out-of-range",
            vec![args[0].clone(), start_arg.clone(), end_arg.clone()],
        ));
    }

    let substring = &full_string[start..end];
    let end_pos = compute_read_end_position(substring);
    if end_pos == 0 {
        return Err(signal(
            "end-of-file",
            vec![Value::string("End of file during parsing")],
        ));
    }

    let consumed = &substring[..end_pos.min(substring.len())];
    let forms = super::parser::parse_forms(consumed).map_err(|e| {
        if e.message.contains("unterminated") || e.message.contains("end of input") {
            signal(
                "end-of-file",
                vec![Value::string("End of file during parsing")],
            )
        } else {
            signal("invalid-read-syntax", vec![Value::string(e.message.clone())])
        }
    })?;

    if forms.is_empty() {
        return Err(signal(
            "end-of-file",
            vec![Value::string("End of file during parsing")],
        ));
    }

    let value = if first_form_is_reader_hash_dollar(&forms[0], consumed) {
        eval.obarray()
            .symbol_value("load-file-name")
            .cloned()
            .unwrap_or(Value::Nil)
    } else if let Some(bytecode) = first_form_byte_code_literal_value(&forms[0]) {
        bytecode
    } else {
        super::eval::quote_to_value(&forms[0])
    };
    let absolute_end = start + end_pos;

    Ok(Value::cons(value, Value::Int(absolute_end as i64)))
}

fn first_form_is_reader_hash_dollar(expr: &Expr, consumed: &str) -> bool {
    matches!(expr, Expr::Symbol(s) if s == "load-file-name")
        && consumed_represents_hash_dollar(consumed)
}

fn first_form_byte_code_literal_value(expr: &Expr) -> Option<Value> {
    let Expr::List(items) = expr else {
        return None;
    };
    if items.len() != 2 {
        return None;
    }
    let Expr::Symbol(name) = &items[0] else {
        return None;
    };
    if name != "byte-code-literal" {
        return None;
    }
    let Expr::Vector(values) = &items[1] else {
        return None;
    };
    let values = values.iter().map(super::eval::quote_to_value).collect();
    Some(super::compiled_literal::maybe_coerce_compiled_literal_function(Value::vector(values)))
}

fn consumed_represents_hash_dollar(input: &str) -> bool {
    let bytes = input.as_bytes();
    let mut pos = skip_ws_comments(input, 0);
    loop {
        if pos + 1 >= bytes.len() {
            return false;
        }
        if bytes[pos] == b'#' && bytes[pos + 1] == b'@' {
            pos += 2;
            let digits_start = pos;
            while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            if pos == digits_start {
                return false;
            }
            let len = std::str::from_utf8(&bytes[digits_start..pos])
                .ok()
                .and_then(|s| s.parse::<usize>().ok());
            let Some(len) = len else {
                return false;
            };
            let Some(after_data) = pos.checked_add(len) else {
                return false;
            };
            if after_data > bytes.len() {
                return false;
            }
            pos = skip_ws_comments(input, after_data);
            continue;
        }
        return bytes[pos] == b'#' && bytes[pos + 1] == b'$';
    }
}

/// Estimate the end position of the first parsed form in the input string.
/// We re-parse character by character to find where the parser would stop
/// after reading one expression.
fn compute_read_end_position(input: &str) -> usize {
    // Use a simple approach: parse just one form and see how far we get.
    // We create a mini-parser that tracks position.
    let mut pos = 0;
    let bytes = input.as_bytes();

    // Skip leading whitespace and comments
    pos = skip_ws_comments(input, pos);

    if pos >= input.len() {
        return input.len();
    }

    // Now skip one sexp
    pos = skip_one_sexp(input, pos);

    // Skip any trailing whitespace up to the end of the consumed region
    // (Emacs `read-from-string` stops right after the sexp, no trailing ws skip)
    let _ = bytes;
    pos
}

fn skip_ws_comments(input: &str, mut pos: usize) -> usize {
    let bytes = input.as_bytes();
    loop {
        if pos >= bytes.len() {
            return pos;
        }
        let ch = bytes[pos];
        if ch.is_ascii_whitespace() {
            pos += 1;
            continue;
        }
        if ch == b';' {
            // line comment
            while pos < bytes.len() && bytes[pos] != b'\n' {
                pos += 1;
            }
            if pos < bytes.len() {
                pos += 1; // skip newline
            }
            continue;
        }
        if ch == b'#' && pos + 1 < bytes.len() && bytes[pos + 1] == b'|' {
            // block comment
            pos += 2;
            let mut depth = 1;
            while depth > 0 && pos < bytes.len() {
                if bytes[pos] == b'#' && pos + 1 < bytes.len() && bytes[pos + 1] == b'|' {
                    depth += 1;
                    pos += 2;
                } else if bytes[pos] == b'|' && pos + 1 < bytes.len() && bytes[pos + 1] == b'#' {
                    depth -= 1;
                    pos += 2;
                } else {
                    pos += 1;
                }
            }
            continue;
        }
        return pos;
    }
}

fn skip_one_sexp(input: &str, mut pos: usize) -> usize {
    let bytes = input.as_bytes();
    if pos >= bytes.len() {
        return pos;
    }

    let ch = bytes[pos];

    match ch {
        b'(' => {
            pos += 1;
            let mut depth = 1;
            while depth > 0 && pos < bytes.len() {
                match bytes[pos] {
                    b'(' => {
                        depth += 1;
                        pos += 1;
                    }
                    b')' => {
                        depth -= 1;
                        pos += 1;
                    }
                    b'"' => {
                        pos = skip_string(input, pos);
                    }
                    b';' => {
                        while pos < bytes.len() && bytes[pos] != b'\n' {
                            pos += 1;
                        }
                    }
                    b'\\' => {
                        pos += 1; // skip backslash
                        if pos < bytes.len() {
                            pos += 1; // skip escaped char
                        }
                    }
                    _ => {
                        pos += 1;
                    }
                }
            }
            pos
        }
        b'[' => {
            pos += 1;
            let mut depth = 1;
            while depth > 0 && pos < bytes.len() {
                match bytes[pos] {
                    b'[' => {
                        depth += 1;
                        pos += 1;
                    }
                    b']' => {
                        depth -= 1;
                        pos += 1;
                    }
                    b'"' => {
                        pos = skip_string(input, pos);
                    }
                    b'\\' => {
                        pos += 1;
                        if pos < bytes.len() {
                            pos += 1;
                        }
                    }
                    _ => {
                        pos += 1;
                    }
                }
            }
            pos
        }
        b'"' => skip_string(input, pos),
        b'\'' | b'`' => {
            // quote / backquote — skip prefix then one sexp
            pos += 1;
            pos = skip_ws_comments(input, pos);
            skip_one_sexp(input, pos)
        }
        b',' => {
            pos += 1;
            if pos < bytes.len() && bytes[pos] == b'@' {
                pos += 1;
            }
            pos = skip_ws_comments(input, pos);
            skip_one_sexp(input, pos)
        }
        b'#' => {
            pos += 1;
            if pos >= bytes.len() {
                return pos;
            }
            match bytes[pos] {
                b'\'' => {
                    // #'symbol
                    pos += 1;
                    pos = skip_ws_comments(input, pos);
                    skip_one_sexp(input, pos)
                }
                b'(' => {
                    // #(vector)
                    skip_one_sexp(input, pos)
                }
                b'[' => {
                    // #[vector] compiled-function literal
                    skip_one_sexp(input, pos)
                }
                b'@' => {
                    // #@N<bytes> ... next-object
                    pos += 1;
                    let digits_start = pos;
                    while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                        pos += 1;
                    }
                    if pos == digits_start {
                        return pos;
                    }
                    let len = std::str::from_utf8(&bytes[digits_start..pos])
                        .ok()
                        .and_then(|s| s.parse::<usize>().ok());
                    let Some(len) = len else {
                        return pos;
                    };
                    let Some(after_data) = pos.checked_add(len) else {
                        return bytes.len();
                    };
                    if after_data > bytes.len() {
                        return bytes.len();
                    }
                    pos = skip_ws_comments(input, after_data);
                    if pos >= bytes.len() {
                        pos
                    } else {
                        skip_one_sexp(input, pos)
                    }
                }
                b'$' => {
                    // #$ pseudo object
                    pos + 1
                }
                b's' => {
                    // #s(hash-table ...)
                    pos += 1;
                    if pos < bytes.len() && bytes[pos] == b'(' {
                        skip_one_sexp(input, pos)
                    } else {
                        pos
                    }
                }
                b'x' | b'X' | b'o' | b'O' | b'b' | b'B' => {
                    // radix number
                    pos += 1;
                    while pos < bytes.len()
                        && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_')
                    {
                        pos += 1;
                    }
                    pos
                }
                _ => pos,
            }
        }
        b'?' => {
            // char literal
            pos += 1;
            if pos < bytes.len() && bytes[pos] == b'\\' {
                pos += 1;
                if pos < bytes.len() {
                    let esc = bytes[pos];
                    pos += 1;
                    match esc {
                        b'x' => {
                            while pos < bytes.len() && bytes[pos].is_ascii_hexdigit() {
                                pos += 1;
                            }
                            // Optional terminating ';'
                            if pos < bytes.len() && bytes[pos] == b';' {
                                pos += 1;
                            }
                        }
                        b'u' => {
                            for _ in 0..4 {
                                if pos < bytes.len() && bytes[pos].is_ascii_hexdigit() {
                                    pos += 1;
                                }
                            }
                        }
                        b'U' => {
                            for _ in 0..8 {
                                if pos < bytes.len() && bytes[pos].is_ascii_hexdigit() {
                                    pos += 1;
                                }
                            }
                        }
                        b'0'..=b'7' => {
                            for _ in 0..2 {
                                if pos < bytes.len() && bytes[pos] >= b'0' && bytes[pos] <= b'7' {
                                    pos += 1;
                                }
                            }
                        }
                        b'C' | b'M' | b'S' => {
                            if pos < bytes.len() && bytes[pos] == b'-' {
                                pos += 1;
                                if pos < bytes.len() {
                                    pos += 1;
                                }
                            }
                        }
                        _ => {} // single escaped char already consumed
                    }
                }
            } else if pos < bytes.len() {
                // Regular character — consume one UTF-8 char
                let ch = input[pos..].chars().next();
                if let Some(c) = ch {
                    pos += c.len_utf8();
                }
            }
            pos
        }
        _ => {
            // Atom: symbol or number
            while pos < bytes.len() {
                let b = bytes[pos];
                if b.is_ascii_whitespace()
                    || b == b'('
                    || b == b')'
                    || b == b'['
                    || b == b']'
                    || b == b'\''
                    || b == b'`'
                    || b == b','
                    || b == b'"'
                    || b == b';'
                {
                    break;
                }
                pos += 1;
            }
            pos
        }
    }
}

fn skip_string(input: &str, mut pos: usize) -> usize {
    let bytes = input.as_bytes();
    if pos >= bytes.len() || bytes[pos] != b'"' {
        return pos;
    }
    pos += 1; // opening quote
    while pos < bytes.len() {
        match bytes[pos] {
            b'"' => {
                pos += 1;
                return pos;
            }
            b'\\' => {
                pos += 1;
                if pos < bytes.len() {
                    pos += 1;
                }
            }
            _ => {
                pos += 1;
            }
        }
    }
    pos
}

// ---------------------------------------------------------------------------
// 2. read
// ---------------------------------------------------------------------------

/// `(read &optional STREAM)`
///
/// Read one Lisp expression from STREAM.
/// - If STREAM is a string, read from that string (equivalent to car of read-from-string).
/// - If STREAM is nil, would read from stdin (returns nil in non-interactive mode).
/// - If STREAM is a buffer, read from buffer at point.
pub(crate) fn builtin_read(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("read", &args, 1)?;

    if args.is_empty() || args[0].is_nil() {
        // In batch/non-interactive runs, stdin-backed read signals EOF.
        return Err(signal(
            "end-of-file",
            vec![Value::string("End of file during parsing")],
        ));
    }

    match &args[0] {
        Value::Str(_) => {
            // Read from string
            let result = builtin_read_from_string(eval, args)?;
            // Return just the car (the parsed object)
            match &result {
                Value::Cons(cell) => {
                    let pair = cell.lock().expect("poisoned");
                    Ok(pair.car.clone())
                }
                _ => Ok(result),
            }
        }
        Value::Buffer(id) => {
            // Read from buffer at point
            let buf_id = *id;
            let (text, pt) = {
                let buf = eval
                    .buffers
                    .get(buf_id)
                    .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;
                (buf.buffer_string(), buf.pt)
            };
            // pt is 1-based, substring from (pt-1)
            let start = if pt > 0 { pt - 1 } else { 0 };
            if start >= text.len() {
                return Err(signal(
                    "end-of-file",
                    vec![Value::string("End of file during parsing")],
                ));
            }
            let substring = &text[start..];
            let forms = super::parser::parse_forms(substring).map_err(|e| {
                signal(
                    "invalid-read-syntax",
                    vec![Value::string(e.message.clone())],
                )
            })?;
            if forms.is_empty() {
                return Err(signal(
                    "end-of-file",
                    vec![Value::string("End of file during parsing")],
                ));
            }
            let value = super::eval::quote_to_value(&forms[0]);
            // Advance point past the read form
            let end_offset = compute_read_end_position(substring);
            let new_pt = pt + end_offset;
            if let Some(buf) = eval.buffers.get_mut(buf_id) {
                buf.pt = new_pt;
            }
            Ok(value)
        }
        Value::Symbol(name) => Err(signal("void-function", vec![Value::symbol(name.clone())])),
        Value::True => Err(signal(
            "end-of-file",
            vec![Value::string("End of file during parsing")],
        )),
        Value::Keyword(name) => Err(signal("void-function", vec![Value::symbol(name.clone())])),
        _ => {
            // Unsupported stream source type for read-char function protocol.
            Err(signal("invalid-function", vec![args[0].clone()]))
        }
    }
}

// ---------------------------------------------------------------------------
// 5. read-from-minibuffer
// ---------------------------------------------------------------------------

/// `(read-from-minibuffer PROMPT ...)`
///
/// In batch/non-interactive mode, consume a pending `unread-command-events`
/// character event when available; otherwise signal `end-of-file`.
pub(crate) fn builtin_read_from_minibuffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-from-minibuffer", &args, 1)?;
    expect_max_args("read-from-minibuffer", &args, 7)?;
    let _prompt = expect_string(&args[0])?;
    if let Some(initial) = args.get(1) {
        expect_initial_input_stringish(initial)?;
    }
    if let Some(event) = eval.peek_unread_command_event() {
        if let Some(ch) = event_to_char(&event) {
            let _ = eval.pop_unread_command_event();
            return Ok(Value::string(ch.to_string()));
        }
        return Err(non_character_input_event_error());
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

// ---------------------------------------------------------------------------
// 6. read-string
// ---------------------------------------------------------------------------

/// `(read-string PROMPT ...)`
///
/// In batch/non-interactive mode, consume a pending `unread-command-events`
/// character event when available; otherwise signal `end-of-file`.
pub(crate) fn builtin_read_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-string", &args, 1)?;
    expect_max_args("read-string", &args, 5)?;
    let _prompt = expect_string(&args[0])?;
    if let Some(initial) = args.get(1) {
        expect_initial_input_stringish(initial)?;
    }
    if let Some(event) = eval.peek_unread_command_event() {
        if let Some(ch) = event_to_char(&event) {
            let _ = eval.pop_unread_command_event();
            return Ok(Value::string(ch.to_string()));
        }
        return Err(non_character_input_event_error());
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

// ---------------------------------------------------------------------------
// 7. read-number
// ---------------------------------------------------------------------------

/// `(read-number PROMPT &optional DEFAULT)`
///
/// In batch mode, consume a pending numeric/readable event when available;
/// otherwise signal `end-of-file`.
pub(crate) fn builtin_read_number(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-number", &args, 1)?;
    expect_max_args("read-number", &args, 3)?;
    let _prompt = expect_string(&args[0])?;
    if let Some(default) = args.get(1) {
        if !default.is_nil() {
            let _ = expect_number(default)?;
        }
    }
    if let Some(event) = eval.peek_unread_command_event() {
        let _ = eval.pop_unread_command_event();
        if let Value::Int(n) = event {
            return Ok(Value::Int(n));
        }
        if let Value::Float(f) = event {
            return Ok(Value::Float(f));
        }
        if let Some(ch) = event_to_char(&event) {
            let token = ch.to_string();
            if let Ok(n) = token.parse::<i64>() {
                return Ok(Value::Int(n));
            }
            if let Ok(f) = token.parse::<f64>() {
                return Ok(Value::Float(f));
            }
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("numberp"), Value::string(token)],
            ));
        }
        return Err(non_character_input_event_error());
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

// ---------------------------------------------------------------------------
// 8. read-passwd
// ---------------------------------------------------------------------------

/// `(read-passwd PROMPT &optional CONFIRM DEFAULT)`
///
/// In batch/non-interactive mode, consume a pending `unread-command-events`
/// character event when available; otherwise signal `end-of-file`.
pub(crate) fn builtin_read_passwd(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-passwd", &args, 1)?;
    expect_max_args("read-passwd", &args, 3)?;
    let _prompt = expect_string(&args[0])?;
    if let Some(event) = eval.peek_unread_command_event() {
        if let Some(ch) = event_to_char(&event) {
            let _ = eval.pop_unread_command_event();
            return Ok(Value::string(ch.to_string()));
        }
        return Err(non_character_input_event_error());
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

// ---------------------------------------------------------------------------
// 9. completing-read
// ---------------------------------------------------------------------------

/// `(completing-read PROMPT COLLECTION ...)`
///
/// In batch/non-interactive mode, consume a pending `unread-command-events`
/// character event when available; otherwise signal `end-of-file`.
pub(crate) fn builtin_completing_read(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("completing-read", &args, 2)?;
    expect_max_args("completing-read", &args, 8)?;
    let _prompt = expect_string(&args[0])?;
    if let Some(initial) = args.get(4) {
        expect_completing_read_initial_input(initial)?;
    }
    if let Some(event) = eval.peek_unread_command_event() {
        if let Some(ch) = event_to_char(&event) {
            let _ = eval.pop_unread_command_event();
            return Ok(Value::string(ch.to_string()));
        }
        return Err(non_character_input_event_error());
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

fn event_to_int(event: &Value) -> Option<i64> {
    match event {
        Value::Int(n) => Some(*n),
        Value::Char(c) => Some(*c as i64),
        _ => None,
    }
}

fn event_to_char(event: &Value) -> Option<char> {
    match event {
        Value::Char(c) => Some(*c),
        Value::Int(n) if *n >= 0 => char::from_u32(*n as u32),
        _ => None,
    }
}

fn expect_optional_prompt_string(args: &[Value]) -> Result<(), Flow> {
    if args.is_empty() || args[0].is_nil() || matches!(args[0], Value::Str(_)) {
        return Ok(());
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("stringp"), args[0].clone()],
    ))
}

fn non_character_input_event_error() -> Flow {
    signal("error", vec![Value::string("Non-character input-event")])
}

// ---------------------------------------------------------------------------
// 10. input-pending-p
// ---------------------------------------------------------------------------

/// `(input-pending-p &optional CHECK-TIMERS)`
///
/// Return non-nil when `unread-command-events` has at least one pending event.
/// `CHECK-TIMERS` is currently accepted for arity compatibility and ignored.
pub(crate) fn builtin_input_pending_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("input-pending-p", &args, 1)?;
    Ok(Value::bool(eval.peek_unread_command_event().is_some()))
}

// ---------------------------------------------------------------------------
// 11. discard-input
// ---------------------------------------------------------------------------

/// `(discard-input)`
///
/// Discard pending unread command events for the current scope.
pub(crate) fn builtin_discard_input(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("discard-input", &args, 0)?;
    eval.assign("unread-command-events", Value::Nil);
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// 12. current-input-mode / set-input-mode
// ---------------------------------------------------------------------------

/// `(current-input-mode)` -> `(INTERRUPT FLOW META QUIT)`
pub(crate) fn builtin_current_input_mode(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("current-input-mode", &args, 0)?;
    let (interrupt, flow, meta, quit) = eval.current_input_mode_tuple();
    Ok(Value::list(vec![
        Value::bool(interrupt),
        Value::bool(flow),
        Value::bool(meta),
        Value::Int(quit),
    ]))
}

/// `(set-input-mode INTERRUPT FLOW META QUIT)`
///
/// Batch-compatible behavior currently tracks only INTERRUPT and ignores
/// FLOW/META/QUIT while preserving arity/return-value semantics.
pub(crate) fn builtin_set_input_mode(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-input-mode", &args, 3)?;
    expect_max_args("set-input-mode", &args, 4)?;
    eval.set_input_mode_interrupt(args[0].is_truthy());
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// 13. input mode helper setters
// ---------------------------------------------------------------------------

/// `(set-input-interrupt-mode INTERRUPT)`
pub(crate) fn builtin_set_input_interrupt_mode(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-input-interrupt-mode", &args, 1)?;
    eval.set_input_mode_interrupt(args[0].is_truthy());
    Ok(Value::Nil)
}

/// `(set-input-meta-mode META)`
///
/// Batch-compatible behavior: accepts one argument and returns nil.
pub(crate) fn builtin_set_input_meta_mode(args: Vec<Value>) -> EvalResult {
    expect_args("set-input-meta-mode", &args, 1)?;
    Ok(Value::Nil)
}

/// `(set-output-flow-control FLOW)`
///
/// Batch-compatible behavior: accepts one argument and returns nil.
pub(crate) fn builtin_set_output_flow_control(args: Vec<Value>) -> EvalResult {
    expect_min_args("set-output-flow-control", &args, 1)?;
    expect_max_args("set-output-flow-control", &args, 2)?;
    Ok(Value::Nil)
}

/// `(set-quit-char CHAR)`
///
/// Batch-compatible behavior: accepts one argument and returns nil.
pub(crate) fn builtin_set_quit_char(args: Vec<Value>) -> EvalResult {
    expect_args("set-quit-char", &args, 1)?;
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// 14. waiting-for-user-input-p
// ---------------------------------------------------------------------------

/// `(waiting-for-user-input-p)`
///
/// Batch-mode compatibility: always returns nil.
pub(crate) fn builtin_waiting_for_user_input_p(args: Vec<Value>) -> EvalResult {
    expect_args("waiting-for-user-input-p", &args, 0)?;
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// 15. y-or-n-p
// ---------------------------------------------------------------------------

/// `(y-or-n-p PROMPT)` currently returns EOF in batch mode.
pub(crate) fn builtin_y_or_n_p(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("y-or-n-p", &args, 1)?;
    match &args[0] {
        Value::Str(_) | Value::Vector(_) | Value::Nil => {}
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("sequencep"), other.clone()],
            ))
        }
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

// ---------------------------------------------------------------------------
// 16. yes-or-no-p
// ---------------------------------------------------------------------------

/// `(yes-or-no-p PROMPT)` currently returns EOF in batch mode.
pub(crate) fn builtin_yes_or_no_p(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("yes-or-no-p", &args, 1)?;
    if !matches!(args[0], Value::Str(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0].clone()],
        ));
    }
    Err(signal(
        "end-of-file",
        vec![Value::string("Error reading from stdin")],
    ))
}

// ---------------------------------------------------------------------------
// 17. read-char
// ---------------------------------------------------------------------------

/// `(read-char &optional PROMPT ...)`
///
/// Batch-mode: returns next unread character codepoint when available, otherwise nil.
pub(crate) fn builtin_read_char(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("read-char"), Value::Int(args.len() as i64)],
        ));
    }
    expect_optional_prompt_string(&args)?;
    if let Some(event) = eval.peek_unread_command_event() {
        if let Some(n) = event_to_int(&event) {
            let _ = eval.pop_unread_command_event();
            return Ok(Value::Int(n));
        }
        eval.assign("unread-command-events", Value::list(vec![event.clone()]));
        eval.record_input_event(event);
        return Err(non_character_input_event_error());
    }
    Ok(Value::Nil)
}

/// `(read-key &optional PROMPT)`
///
/// Batch-mode: return next `unread-command-events` event when present, else nil.
pub(crate) fn builtin_read_key(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("read-key"), Value::Int(args.len() as i64)],
        ));
    }
    expect_optional_prompt_string(&args)?;
    if let Some(event) = eval.pop_unread_command_event() {
        eval.set_read_command_keys(vec![event.clone()]);
        if let Some(n) = event_to_int(&event) {
            return Ok(Value::Int(n));
        }
        return Ok(event);
    }
    eval.clear_read_command_keys();
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// 18. read-key-sequence
// ---------------------------------------------------------------------------

/// `(read-key-sequence PROMPT)`
///
/// Batch-mode: consume one queued event and return it, or return empty string.
pub(crate) fn builtin_read_key_sequence(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-key-sequence", &args, 1)?;
    expect_max_args("read-key-sequence", &args, 6)?;
    expect_optional_prompt_string(&args)?;
    if let Some(event) = eval.pop_unread_command_event() {
        eval.set_read_command_keys(vec![event.clone()]);
        if let Some(c) = event_to_char(&event) {
            return Ok(Value::string(c.to_string()));
        }
        return Ok(Value::vector(vec![event]));
    }
    eval.clear_read_command_keys();
    Ok(Value::string(""))
}

/// `(read-key-sequence-vector PROMPT)`
///
/// Batch mode: returns next `unread-command-events` event as a single-element
/// vector when present, otherwise an empty vector.
pub(crate) fn builtin_read_key_sequence_vector(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-key-sequence-vector", &args, 1)?;
    expect_max_args("read-key-sequence-vector", &args, 6)?;
    expect_optional_prompt_string(&args)?;
    if let Some(event) = eval.pop_unread_command_event() {
        eval.set_read_command_keys(vec![event.clone()]);
        if let Some(n) = event_to_int(&event) {
            return Ok(Value::vector(vec![Value::Int(n)]));
        }
        return Ok(Value::vector(vec![event]));
    }
    eval.clear_read_command_keys();
    Ok(Value::vector(vec![]))
}

// ---------------------------------------------------------------------------
// 14. with-output-to-string (special form)
// ---------------------------------------------------------------------------

/// Special form: `(with-output-to-string BODY...)`
///
/// Evaluate BODY, capturing output from print functions into a temporary
/// buffer bound through `standard-output`.
pub(crate) fn sf_with_output_to_string(
    eval: &mut super::eval::Evaluator,
    tail: &[Expr],
) -> EvalResult {
    let temp_name = eval
        .buffers
        .generate_new_buffer_name(" *with-output-to-string*");
    let temp_id = eval.buffers.create_buffer(&temp_name);

    let mut frame = std::collections::HashMap::new();
    frame.insert("standard-output".to_string(), Value::Buffer(temp_id));
    eval.dynamic.push(frame);

    let body_result = eval.sf_progn(tail);
    let captured = eval
        .buffers
        .get(temp_id)
        .map(|buf| buf.buffer_string())
        .unwrap_or_default();

    let _ = eval.dynamic.pop();
    eval.buffers.kill_buffer(temp_id);

    body_result.map(|_| Value::string(captured))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::eval::Evaluator;
    use crate::elisp::parse_forms;

    // ===================================================================
    // read-from-string tests
    // ===================================================================

    #[test]
    fn read_from_string_integer() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("42")]).unwrap();
        // Should be (42 . 2)
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                assert!(matches!(&pair.cdr, Value::Int(2)));
            }
            _ => panic!("Expected cons, got {:?}", result),
        }
    }

    #[test]
    fn read_from_string_symbol() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("hello")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Symbol(s) if s == "hello"));
                assert!(matches!(&pair.cdr, Value::Int(5)));
            }
            _ => panic!("Expected cons, got {:?}", result),
        }
    }

    #[test]
    fn read_from_string_string_value() {
        let mut ev = Evaluator::new();
        let result =
            builtin_read_from_string(&mut ev, vec![Value::string(r#""hello world""#)]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert_eq!(pair.car.as_str(), Some("hello world"));
                assert!(matches!(&pair.cdr, Value::Int(13)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_list() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("(+ 1 2)")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                // car should be a list (+ 1 2)
                assert!(pair.car.is_cons());
                assert!(matches!(&pair.cdr, Value::Int(7)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_with_start() {
        let mut ev = Evaluator::new();
        // "  42 rest" — start at 2
        let result =
            builtin_read_from_string(&mut ev, vec![Value::string("  42 rest"), Value::Int(2)])
                .unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                assert!(matches!(&pair.cdr, Value::Int(4)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_float() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("3.14")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Float(f) if (*f - 3.14).abs() < 1e-10));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_char() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("?a")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Char('a')));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("nil")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(pair.car.is_nil());
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_t() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("t")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::True));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_vector() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("[1 2 3]")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(pair.car.is_vector());
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_quoted() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("'foo")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                // Should be (quote foo) as a list
                assert!(pair.car.is_cons());
                assert!(matches!(&pair.cdr, Value::Int(4)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_dotted_pair() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("(a . b)")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                // car should be a dotted pair (a . b)
                assert!(pair.car.is_cons());
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_keyword() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string(":test")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Keyword(s) if s == ":test"));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_empty_error() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("")]);
        assert!(result.is_err());
    }

    #[test]
    fn read_from_string_whitespace_only_error() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("   ")]);
        assert!(result.is_err());
    }

    #[test]
    fn read_from_string_multiple_forms_reads_first() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("42 99")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                // End position should be after "42" (position 2), not after "99"
                match &pair.cdr {
                    Value::Int(n) => assert!(*n <= 3, "end pos {} should be <= 3", n),
                    _ => panic!("Expected int end position"),
                }
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_with_start_and_end() {
        let mut ev = Evaluator::new();
        // "xxx42yyy" with start=3, end=5 -> substring "42"
        let result = builtin_read_from_string(
            &mut ev,
            vec![Value::string("xxx42yyy"), Value::Int(3), Value::Int(5)],
        )
        .unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                assert!(matches!(&pair.cdr, Value::Int(5)));
            }
            _ => panic!("Expected cons"),
        }
    }

    // ===================================================================
    // read tests
    // ===================================================================

    #[test]
    fn read_from_string_stream() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![Value::string("42")]).unwrap();
        assert!(matches!(result, Value::Int(42)));
    }

    #[test]
    fn read_nil_stream() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn read_no_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn read_rejects_extra_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![Value::string("a"), Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn read_non_stream_type_is_invalid_function() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![Value::Int(1)]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "invalid-function"),
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    // ===================================================================
    // Stub function tests
    // ===================================================================

    #[test]
    fn read_from_minibuffer_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_minibuffer(&mut ev, vec![Value::string("Prompt: ")]);
        assert!(result.is_err());
    }

    #[test]
    fn read_from_minibuffer_ignores_initial_and_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_minibuffer(
            &mut ev,
            vec![Value::string("Prompt: "), Value::string("initial")],
        );
        assert!(result.is_err());
    }

    #[test]
    fn read_from_minibuffer_rejects_non_stringish_initial_input() {
        let mut ev = Evaluator::new();
        let result =
            builtin_read_from_minibuffer(&mut ev, vec![Value::string("Prompt: "), Value::Int(1)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_from_minibuffer_rejects_cons_initial_with_non_string_car() {
        let mut ev = Evaluator::new();
        let cons_initial = Value::cons(Value::Int(1), Value::Int(1));
        let result =
            builtin_read_from_minibuffer(&mut ev, vec![Value::string("Prompt: "), cons_initial]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_from_minibuffer_rejects_more_than_seven_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_minibuffer(
            &mut ev,
            vec![
                Value::string("Prompt: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_string_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_string(&mut ev, vec![Value::string("Prompt: ")]);
        assert!(result.is_err());
    }

    #[test]
    fn read_string_ignores_initial_and_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_string(
            &mut ev,
            vec![Value::string("Prompt: "), Value::string("initial")],
        );
        assert!(result.is_err());
    }

    #[test]
    fn read_string_rejects_non_stringish_initial_input() {
        let mut ev = Evaluator::new();
        let result = builtin_read_string(&mut ev, vec![Value::string("Prompt: "), Value::Int(1)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_string_rejects_cons_initial_with_non_string_car() {
        let mut ev = Evaluator::new();
        let cons_initial = Value::cons(Value::Int(1), Value::Int(1));
        let result = builtin_read_string(&mut ev, vec![Value::string("Prompt: "), cons_initial]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_string_rejects_more_than_five_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_string(
            &mut ev,
            vec![
                Value::string("Prompt: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_number_signals_end_of_file_even_with_default() {
        let mut ev = Evaluator::new();
        let result = builtin_read_number(&mut ev, vec![Value::string("Number: "), Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn read_number_signals_end_of_file_without_default() {
        let mut ev = Evaluator::new();
        let result = builtin_read_number(&mut ev, vec![Value::string("Number: ")]);
        assert!(result.is_err());
    }

    #[test]
    fn read_number_rejects_non_numeric_default() {
        let mut ev = Evaluator::new();
        let result = builtin_read_number(
            &mut ev,
            vec![Value::string("Number: "), Value::string("x")],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_number_accepts_numeric_default_and_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_number(
            &mut ev,
            vec![Value::string("Number: "), Value::Float(1.5)],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
    }

    #[test]
    fn read_number_rejects_more_than_three_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_number(
            &mut ev,
            vec![Value::string("Number: "), Value::Int(42), Value::Nil, Value::Nil],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_number_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_read_number(&mut ev, vec![Value::Int(123)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_passwd_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_passwd(&mut ev, vec![Value::string("")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
    }

    #[test]
    fn read_passwd_accepts_optional_args_and_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_read_passwd(
            &mut ev,
            vec![Value::string(""), Value::True, Value::symbol("default")],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
    }

    #[test]
    fn read_passwd_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_read_passwd(&mut ev, vec![Value::Int(123)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_passwd_rejects_wrong_arity() {
        let mut ev = Evaluator::new();
        let too_few = builtin_read_passwd(&mut ev, vec![]);
        assert!(matches!(
            too_few,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let too_many = builtin_read_passwd(
            &mut ev,
            vec![Value::string(""), Value::Nil, Value::Nil, Value::Nil],
        );
        assert!(matches!(
            too_many,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn completing_read_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_completing_read(&mut ev, vec![Value::string("Choose: "), Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn completing_read_ignores_default_and_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_completing_read(
            &mut ev,
            vec![
                Value::string("Choose: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::string("fallback"),
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn completing_read_rejects_non_stringish_initial_input() {
        let mut ev = Evaluator::new();
        let result = builtin_completing_read(
            &mut ev,
            vec![
                Value::string("Choose: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Int(1),
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn completing_read_accepts_cons_initial_with_string_and_position() {
        let mut ev = Evaluator::new();
        let cons_initial = Value::cons(Value::string("x"), Value::Int(1));
        let result = builtin_completing_read(
            &mut ev,
            vec![
                Value::string("Choose: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                cons_initial,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
    }

    #[test]
    fn completing_read_rejects_cons_initial_with_non_string_car() {
        let mut ev = Evaluator::new();
        let cons_initial = Value::cons(Value::Int(1), Value::Int(1));
        let result = builtin_completing_read(
            &mut ev,
            vec![
                Value::string("Choose: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                cons_initial,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn completing_read_rejects_cons_initial_with_non_numeric_position() {
        let mut ev = Evaluator::new();
        let cons_initial = Value::cons(Value::string("x"), Value::Nil);
        let result = builtin_completing_read(
            &mut ev,
            vec![
                Value::string("Choose: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                cons_initial,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn completing_read_rejects_more_than_eight_args() {
        let mut ev = Evaluator::new();
        let result = builtin_completing_read(
            &mut ev,
            vec![
                Value::string("Choose: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn y_or_n_p_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_y_or_n_p(&mut ev, vec![Value::string("Continue? ")]);
        assert!(result.is_err());
    }

    #[test]
    fn y_or_n_p_rejects_non_sequence_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_y_or_n_p(&mut ev, vec![Value::Int(123)]);
        assert!(result.is_err());
    }

    #[test]
    fn y_or_n_p_rejects_extra_arg() {
        let mut ev = Evaluator::new();
        let result = builtin_y_or_n_p(&mut ev, vec![Value::string("Continue? "), Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn y_or_n_p_accepts_nil_and_vector_prompts() {
        let mut ev_nil = Evaluator::new();
        let nil_prompt = builtin_y_or_n_p(&mut ev_nil, vec![Value::Nil]);
        assert!(nil_prompt.is_err());

        let mut ev_vec = Evaluator::new();
        let vector_prompt = builtin_y_or_n_p(&mut ev_vec, vec![Value::vector(vec![
            Value::Int(121),
            Value::Int(47),
            Value::Int(110),
        ])]);
        assert!(vector_prompt.is_err());
    }

    #[test]
    fn y_or_n_p_rejects_list_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_y_or_n_p(&mut ev, vec![Value::list(vec![
            Value::Int(121),
            Value::Int(47),
            Value::Int(110),
        ])]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn y_or_n_p_ignores_unread_events_and_eofs() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(121)]));
        let result = builtin_y_or_n_p(&mut ev, vec![Value::string("Continue? ")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(121)]))
        );
    }

    #[test]
    fn y_or_n_p_unread_events_do_not_change() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(110)]));
        let result = builtin_y_or_n_p(&mut ev, vec![Value::string("Continue? ")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(110)]))
        );
    }

    #[test]
    fn y_or_n_p_rejects_invalid_character_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(48)]));
        let result = builtin_y_or_n_p(&mut ev, vec![Value::string("Continue? ")]);
        assert!(matches!(result, Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(48)]))
        );
    }

    #[test]
    fn yes_or_no_p_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::string("Confirm? ")]);
        assert!(result.is_err());
    }

    #[test]
    fn yes_or_no_p_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::Int(123)]);
        assert!(result.is_err());
    }

    #[test]
    fn yes_or_no_p_rejects_extra_arg() {
        let mut ev = Evaluator::new();
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::string("Confirm? "), Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn yes_or_no_p_ignores_unread_events_and_eofs() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(89)]));
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::string("Confirm? ")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(89)]))
        );
    }

    #[test]
    fn yes_or_no_p_unread_events_do_not_change() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(110)]));
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::string("Confirm? ")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"
        ));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(110)]))
        );
    }

    #[test]
    fn yes_or_no_p_rejects_invalid_character_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(48)]));
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::string("Confirm? ")]);
        assert!(matches!(result, Err(Flow::Signal(sig)) if sig.symbol == "end-of-file"));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(48)]))
        );
    }

    #[test]
    fn yes_or_no_p_rejects_nil_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_yes_or_no_p(&mut ev, vec![Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn input_pending_p_returns_nil_without_events() {
        let mut ev = Evaluator::new();
        let result = builtin_input_pending_p(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn input_pending_p_returns_t_with_unread_events() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_input_pending_p(&mut ev, vec![]).unwrap();
        assert_eq!(result, Value::True);
    }

    #[test]
    fn input_pending_p_uses_dynamic_unread_command_events_binding() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let forms = parse_forms("(let ((unread-command-events nil)) (input-pending-p))").unwrap();
        let result = ev.eval_expr(&forms[0]).unwrap();
        assert!(result.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(97)]))
        );
    }

    #[test]
    fn input_pending_p_returns_nil_for_non_list_unread_command_events() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::Int(7));
        let result = builtin_input_pending_p(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn input_pending_p_accepts_optional_check_timers_arg() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::symbol("foo")]));
        let result = builtin_input_pending_p(&mut ev, vec![Value::symbol("timers")]).unwrap();
        assert_eq!(result, Value::True);
    }

    #[test]
    fn input_pending_p_rejects_more_than_one_arg() {
        let mut ev = Evaluator::new();
        let result = builtin_input_pending_p(&mut ev, vec![Value::Nil, Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn discard_input_returns_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_discard_input(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn discard_input_clears_unread_command_events() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_discard_input(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
        assert_eq!(ev.obarray.symbol_value("unread-command-events"), Some(&Value::Nil));
    }

    #[test]
    fn discard_input_uses_dynamic_unread_command_events_binding() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let forms =
            parse_forms("(let ((unread-command-events (list 98))) (discard-input) unread-command-events)")
                .unwrap();
        let result = ev.eval_expr(&forms[0]).unwrap();
        assert!(result.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::Int(97)]))
        );
    }

    #[test]
    fn discard_input_rejects_args() {
        let mut ev = Evaluator::new();
        let result = builtin_discard_input(&mut ev, vec![Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn current_input_mode_returns_batch_tuple() {
        let mut ev = Evaluator::new();
        let result = builtin_current_input_mode(&mut ev, vec![]).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::True, Value::Nil, Value::True, Value::Int(7)])
        );
    }

    #[test]
    fn current_input_mode_rejects_args() {
        let mut ev = Evaluator::new();
        let result = builtin_current_input_mode(&mut ev, vec![Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn set_input_mode_toggles_interrupt_only() {
        let mut ev = Evaluator::new();
        let _ = builtin_set_input_mode(
            &mut ev,
            vec![Value::Nil, Value::True, Value::Nil, Value::Int(65)],
        )
        .unwrap();
        assert_eq!(
            builtin_current_input_mode(&mut ev, vec![]).unwrap(),
            Value::list(vec![Value::Nil, Value::Nil, Value::True, Value::Int(7)])
        );

        let _ = builtin_set_input_mode(
            &mut ev,
            vec![Value::symbol("x"), Value::Nil, Value::Nil, Value::Nil],
        )
        .unwrap();
        assert_eq!(
            builtin_current_input_mode(&mut ev, vec![]).unwrap(),
            Value::list(vec![Value::True, Value::Nil, Value::True, Value::Int(7)])
        );
    }

    #[test]
    fn set_input_mode_rejects_wrong_arity() {
        let mut ev = Evaluator::new();
        let too_few = builtin_set_input_mode(&mut ev, vec![Value::Nil, Value::Nil]);
        assert!(matches!(
            too_few,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));

        let too_many = builtin_set_input_mode(
            &mut ev,
            vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil, Value::Nil],
        );
        assert!(matches!(
            too_many,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn set_input_mode_accepts_three_args() {
        let mut ev = Evaluator::new();
        let result = builtin_set_input_mode(&mut ev, vec![Value::Nil, Value::True, Value::True])
            .expect("set-input-mode should accept 3 args");
        assert!(result.is_nil());
        assert_eq!(
            builtin_current_input_mode(&mut ev, vec![]).unwrap(),
            Value::list(vec![Value::Nil, Value::Nil, Value::True, Value::Int(7)])
        );
    }

    #[test]
    fn set_input_interrupt_mode_toggles_interrupt_state() {
        let mut ev = Evaluator::new();
        let _ = builtin_set_input_interrupt_mode(&mut ev, vec![Value::Nil]).unwrap();
        assert_eq!(
            builtin_current_input_mode(&mut ev, vec![]).unwrap(),
            Value::list(vec![Value::Nil, Value::Nil, Value::True, Value::Int(7)])
        );
        let _ = builtin_set_input_interrupt_mode(&mut ev, vec![Value::symbol("x")]).unwrap();
        assert_eq!(
            builtin_current_input_mode(&mut ev, vec![]).unwrap(),
            Value::list(vec![Value::True, Value::Nil, Value::True, Value::Int(7)])
        );
    }

    #[test]
    fn set_input_interrupt_mode_rejects_wrong_arity() {
        let mut ev = Evaluator::new();
        let result = builtin_set_input_interrupt_mode(&mut ev, vec![Value::Nil, Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn set_input_meta_mode_accepts_one_arg_and_returns_nil() {
        let result = builtin_set_input_meta_mode(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn set_input_meta_mode_rejects_wrong_arity() {
        let result = builtin_set_input_meta_mode(vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn set_output_flow_control_accepts_one_arg_and_returns_nil() {
        let result = builtin_set_output_flow_control(vec![Value::True]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn set_output_flow_control_accepts_two_args_and_returns_nil() {
        let result = builtin_set_output_flow_control(vec![Value::True, Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn set_output_flow_control_rejects_wrong_arity() {
        let result = builtin_set_output_flow_control(vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn set_quit_char_accepts_one_arg_and_returns_nil() {
        let result = builtin_set_quit_char(vec![Value::Int(65)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn set_quit_char_rejects_wrong_arity() {
        let result = builtin_set_quit_char(vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn waiting_for_user_input_p_returns_nil() {
        let result = builtin_waiting_for_user_input_p(vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn waiting_for_user_input_p_rejects_args() {
        let result = builtin_waiting_for_user_input_p(vec![Value::Nil]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_char_returns_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_read_char(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn read_char_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_read_char(&mut ev, vec![Value::Int(123)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_char_consumes_unread_command_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_char(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_int(), Some(97));
        assert_eq!(ev.recent_input_events(), &[Value::Int(97)]);
    }

    #[test]
    fn read_char_signals_error_on_non_character_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::symbol("foo")]));
        let result = builtin_read_char(&mut ev, vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                    && sig.data == vec![Value::string("Non-character input-event")]
        ));
        assert_eq!(ev.recent_input_events(), &[Value::symbol("foo")]);
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::symbol("foo")]))
        );
    }

    #[test]
    fn read_char_non_character_truncates_unread_tail_to_offending_event() {
        let mut ev = Evaluator::new();
        ev.obarray.set_symbol_value(
            "unread-command-events",
            Value::list(vec![Value::symbol("foo"), Value::Int(97)]),
        );
        let result = builtin_read_char(&mut ev, vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                    && sig.data == vec![Value::string("Non-character input-event")]
        ));
        assert_eq!(
            ev.obarray.symbol_value("unread-command-events"),
            Some(&Value::list(vec![Value::symbol("foo")]))
        );
        assert_eq!(ev.recent_input_events(), &[Value::symbol("foo")]);
    }

    #[test]
    fn read_char_rejects_more_than_three_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_char(
            &mut ev,
            vec![
                Value::string("key: "),
                Value::Nil,
                Value::Int(0),
                Value::Nil,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_key_consumes_unread_command_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_int(), Some(97));
        assert_eq!(ev.read_command_keys(), &[Value::Int(97)]);
    }

    #[test]
    fn read_key_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_read_key(&mut ev, vec![Value::Int(123)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_key_accepts_second_optional_arg() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key(&mut ev, vec![Value::string("key: "), Value::Int(1)]).unwrap();
        assert_eq!(result.as_int(), Some(97));
    }

    #[test]
    fn read_key_rejects_more_than_two_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_key(
            &mut ev,
            vec![Value::string("key: "), Value::Nil, Value::Int(123)],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_key_returns_non_integer_event() {
        let mut ev = Evaluator::new();
        let event = Value::symbol("f");
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![event.clone()]));
        let result = builtin_read_key(&mut ev, vec![Value::string("key: ")]).unwrap();
        assert_eq!(result, event);
        assert_eq!(ev.read_command_keys(), std::slice::from_ref(&event));
    }

    #[test]
    fn read_key_consumes_unread_character_and_keeps_tail() {
        let mut ev = Evaluator::new();
        let event = Value::symbol("foo");
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![event.clone(), Value::Int(97)]));
        let result = builtin_read_key(&mut ev, vec![Value::string("key: ")]).unwrap();
        assert_eq!(result, event);
        assert_eq!(ev.read_command_keys(), std::slice::from_ref(&event));
        assert_eq!(ev.obarray.symbol_value("unread-command-events"), Some(&Value::list(vec![Value::Int(97)])));
    }

    #[test]
    fn read_key_sequence_returns_empty_string() {
        let mut ev = Evaluator::new();
        let result = builtin_read_key_sequence(&mut ev, vec![Value::string("key: ")]).unwrap();
        assert!(matches!(result, Value::Str(s) if s.as_str().is_empty()));
    }

    #[test]
    fn read_key_sequence_consumes_unread_command_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key_sequence(&mut ev, vec![Value::string("key: ")]).unwrap();
        assert!(matches!(result, Value::Str(s) if s.as_str() == "a"));
        assert_eq!(ev.read_command_keys(), &[Value::Int(97)]);
    }

    #[test]
    fn read_key_sequence_consumes_non_character_event() {
        let mut ev = Evaluator::new();
        let event = Value::symbol("f");
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![event.clone()]));
        let result = builtin_read_key_sequence(&mut ev, vec![Value::string("key: ")]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0], event);
            }
            other => panic!("expected vector event payload, got {other:?}"),
        }
        assert_eq!(ev.read_command_keys(), std::slice::from_ref(&event));
    }

    #[test]
    fn read_key_sequence_consumes_non_character_event_and_preserves_tail() {
        let mut ev = Evaluator::new();
        let event = Value::symbol("foo");
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![event.clone(), Value::Int(97)]));
        let result = builtin_read_key_sequence(&mut ev, vec![Value::string("key: ")]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0], event);
            }
            other => panic!("expected vector event payload, got {other:?}"),
        }
        assert_eq!(ev.read_command_keys(), std::slice::from_ref(&event));
        assert_eq!(ev.obarray.symbol_value("unread-command-events"), Some(&Value::list(vec![Value::Int(97)])));
    }

    #[test]
    fn read_key_sequence_accepts_nil_prompt() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key_sequence(&mut ev, vec![Value::Nil]).unwrap();
        assert!(matches!(result, Value::Str(s) if s.as_str() == "a"));
    }

    #[test]
    fn read_key_sequence_rejects_more_than_six_args() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key_sequence(
            &mut ev,
            vec![
                Value::string("key: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn read_key_sequence_vector_returns_empty_vector() {
        let mut ev = Evaluator::new();
        let result =
            builtin_read_key_sequence_vector(&mut ev, vec![Value::string("key: ")]).unwrap();
        match result {
            Value::Vector(v) => assert!(v.lock().expect("poisoned").is_empty()),
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn read_key_sequence_vector_consumes_unread_command_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result =
            builtin_read_key_sequence_vector(&mut ev, vec![Value::string("key: ")]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0].as_int(), Some(97));
            }
            other => panic!("expected vector, got {other:?}"),
        }
        assert_eq!(ev.read_command_keys(), &[Value::Int(97)]);
    }

    #[test]
    fn read_key_sequence_vector_consumes_non_character_event() {
        let mut ev = Evaluator::new();
        let event = Value::symbol("x");
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![event.clone()]));
        let result =
            builtin_read_key_sequence_vector(&mut ev, vec![Value::string("key: ")]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0], event);
            }
            other => panic!("expected vector event payload, got {other:?}"),
        }
        assert_eq!(ev.read_command_keys(), std::slice::from_ref(&event));
    }

    #[test]
    fn read_key_sequence_vector_consumes_non_character_event_and_preserves_tail() {
        let mut ev = Evaluator::new();
        let event = Value::symbol("bar");
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![event.clone(), Value::Int(97)]));
        let result = builtin_read_key_sequence_vector(&mut ev, vec![Value::string("key: ")]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0], event);
            }
            other => panic!("expected vector, got {other:?}"),
        }
        assert_eq!(ev.read_command_keys(), std::slice::from_ref(&event));
        assert_eq!(ev.obarray.symbol_value("unread-command-events"), Some(&Value::list(vec![Value::Int(97)])));
    }

    #[test]
    fn read_key_sequence_vector_accepts_nil_prompt() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key_sequence_vector(&mut ev, vec![Value::Nil]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert_eq!(items[0].as_int(), Some(97));
            }
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn read_key_sequence_vector_rejects_more_than_six_args() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_key_sequence_vector(
            &mut ev,
            vec![
                Value::string("key: "),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    // ===================================================================
    // with-output-to-string tests
    // ===================================================================

    #[test]
    fn with_output_to_string_captures_print_output() {
        let mut ev = Evaluator::new();
        let forms = parse_forms(r#"(with-output-to-string (princ "a") (prin1 '(1 2)) (print "x"))"#)
            .unwrap();
        let result = ev.eval_expr(&forms[0]).unwrap();
        assert_eq!(result.as_str(), Some("a(1 2)\n\"x\"\n"));
    }

    #[test]
    fn with_output_to_string_keeps_explicit_destination_working() {
        let mut ev = Evaluator::new();
        let forms = parse_forms(
            r#"(with-temp-buffer
                 (let ((buf (current-buffer)))
                   (with-output-to-string
                     (princ "captured")
                     (princ " to-buf" buf))
                   (buffer-string)))"#,
        )
        .unwrap();
        let result = ev.eval_expr(&forms[0]).unwrap();
        assert_eq!(result.as_str(), Some(" to-buf"));
    }

    // ===================================================================
    // Edge case / integration tests
    // ===================================================================

    #[test]
    fn read_from_string_nested_list() {
        let mut ev = Evaluator::new();
        let result =
            builtin_read_from_string(&mut ev, vec![Value::string("((a b) (c d))")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(pair.car.is_cons());
                assert!(matches!(&pair.cdr, Value::Int(13)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_with_leading_whitespace() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("   42")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                // End position should be 5 (after "   42")
                assert!(matches!(&pair.cdr, Value::Int(5)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_negative_number() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("-7")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(-7)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_wrong_type() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn read_from_string_no_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn read_from_string_hash_syntax() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("#xff")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(255)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_hash_dollar_uses_load_file_name() {
        let mut ev = Evaluator::new();
        ev.set_variable("load-file-name", Value::string("/tmp/reader-probe.elc"));
        let result = builtin_read_from_string(&mut ev, vec![Value::string("#$")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert_eq!(pair.car.as_str(), Some("/tmp/reader-probe.elc"));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_hash_dollar_defaults_to_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("#$")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(pair.car.is_nil());
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_hash_skip_then_hash_dollar() {
        let mut ev = Evaluator::new();
        ev.set_variable("load-file-name", Value::string("/tmp/reader-skip.elc"));
        let result = builtin_read_from_string(&mut ev, vec![Value::string("#@4data#$")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert_eq!(pair.car.as_str(), Some("/tmp/reader-skip.elc"));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_hash_skip_bytes() {
        let mut ev = Evaluator::new();
        let input = "#@4data42 rest";
        let expected_end = input.find(" rest").unwrap() as i64;
        let result = builtin_read_from_string(&mut ev, vec![Value::string(input)]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                assert_eq!(pair.cdr, Value::Int(expected_end));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_string_hash_bracket_end_position() {
        let mut ev = Evaluator::new();
        let input = "#[(x) \"\\bT\\207\" [x] 1 (#$ . 83)] tail";
        let expected_end = input.find(" tail").unwrap() as i64;
        let result = builtin_read_from_string(&mut ev, vec![Value::string(input)]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert_eq!(pair.cdr, Value::Int(expected_end));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn read_from_string_hash_bracket_coerces_compiled_literal() {
        let mut ev = Evaluator::new();
        let input = "#[nil \"\\300\\207\" [0] 1]";
        let result = builtin_read_from_string(&mut ev, vec![Value::string(input)]).unwrap();
        match result {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert!(matches!(pair.car, Value::ByteCode(_)));
            }
            other => panic!("Expected cons from read-from-string, got {other:?}"),
        }
    }

    #[cfg(not(feature = "legacy-elc-literal"))]
    #[test]
    fn read_from_string_hash_bracket_preserves_vector_by_default() {
        let mut ev = Evaluator::new();
        let input = "#[nil \"\\300\\207\" [0] 1]";
        let result = builtin_read_from_string(&mut ev, vec![Value::string(input)]).unwrap();
        match result {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert!(matches!(pair.car, Value::Vector(_)));
            }
            other => panic!("Expected cons from read-from-string, got {other:?}"),
        }
    }

}
