//! Reader-internals builtins: read, read-from-string,
//! eval-buffer, eval-region, read-char, read-event, read-char-exclusive,
//! get-load-suffixes, locate-file, locate-file-internal, read-coding-system,
//! read-non-nil-coding-system.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::path::Path;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

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

fn expect_integer_or_marker(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        Value::Symbol(s) => Ok(s.clone()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// `(read &optional STREAM)`
///
/// Read one Lisp expression from STREAM.
/// - If STREAM is a string, parse the first form from it.
/// - If STREAM is nil or omitted, return nil (no terminal input in batch mode).
/// - If STREAM is a buffer, read from buffer at point.
pub(crate) fn builtin_read(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() || args[0].is_nil() {
        // No stream / nil -- non-interactive, return nil
        return Ok(Value::Nil);
    }

    match &args[0] {
        Value::Str(s) => {
            let input = (**s).clone();
            let forms = super::parser::parse_forms(&input).map_err(|e| {
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
            Ok(super::eval::quote_to_value(&forms[0]))
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
        _ => {
            // Unsupported stream type -- treat as nil
            Ok(Value::Nil)
        }
    }
}

/// `(read-from-string STRING &optional START END)`
///
/// Read one Lisp object from STRING starting at position START (default 0).
/// Returns `(OBJECT . END-POSITION)` where END-POSITION is the character index
/// after the parsed object.
pub(crate) fn builtin_read_from_string(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("read-from-string", &args, 1)?;

    let full_string = match &args[0] {
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };

    let start = if args.len() > 1 && args[1].is_truthy() {
        match &args[1] {
            Value::Int(n) => {
                if *n < 0 {
                    return Err(signal(
                        "args-out-of-range",
                        vec![args[0].clone(), args[1].clone()],
                    ));
                }
                *n as usize
            }
            _ => 0,
        }
    } else {
        0
    };

    let end = if args.len() > 2 && args[2].is_truthy() {
        match &args[2] {
            Value::Int(n) => {
                if *n < 0 || (*n as usize) > full_string.len() {
                    return Err(signal(
                        "args-out-of-range",
                        vec![args[0].clone(), args[2].clone()],
                    ));
                }
                *n as usize
            }
            _ => full_string.len(),
        }
    } else {
        full_string.len()
    };

    if start > end || start > full_string.len() {
        return Err(signal(
            "args-out-of-range",
            vec![args[0].clone(), Value::Int(start as i64)],
        ));
    }

    let substring = &full_string[start..end];

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

    // Compute end position after the first form
    let end_pos = compute_read_end_position(substring);
    let absolute_end = start + end_pos;

    Ok(Value::cons(value, Value::Int(absolute_end as i64)))
}

fn eval_forms_from_source(eval: &mut super::eval::Evaluator, source: &str) -> EvalResult {
    if source.is_empty() {
        return Ok(Value::Nil);
    }
    let forms = super::parser::parse_forms(source).map_err(|e| {
        signal(
            "invalid-read-syntax",
            vec![Value::string(e.message.clone())],
        )
    })?;
    for form in forms {
        eval.eval(&form)?;
    }
    Ok(Value::Nil)
}

fn eval_buffer_source_text(eval: &super::eval::Evaluator, arg: Option<&Value>) -> Result<String, Flow> {
    let buffer_id = match arg {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?,
        Some(Value::Buffer(id)) => *id,
        Some(Value::Str(name)) => eval
            .buffers
            .find_buffer_by_name(name)
            .ok_or_else(|| signal("error", vec![Value::string("No such buffer")]))?,
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    eval.buffers
        .get(buffer_id)
        .map(|buffer| buffer.buffer_string())
        .ok_or_else(|| signal("error", vec![Value::string("No such buffer")]))
}

/// `(eval-buffer &optional BUFFER PRINTFLAG FILENAME UNIBYTE DO-ALLOW-PRINT)`
///
/// Evaluate all forms from BUFFER (or current buffer) and return nil.
pub(crate) fn builtin_eval_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("eval-buffer", &args, 5)?;
    let source = eval_buffer_source_text(eval, args.first())?;
    eval_forms_from_source(eval, &source)
}

/// `(eval-region START END &optional PRINTFLAG READ-FUNCTION)`
///
/// Evaluate forms in the [START, END) region of the current buffer.
pub(crate) fn builtin_eval_region(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("eval-region", &args, 2)?;
    expect_max_args("eval-region", &args, 4)?;

    let (source, start_char_pos, end_char_pos) = {
        let buffer = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

        let point_char_pos = buffer.text.byte_to_char(buffer.point()) as i64 + 1;
        let max_char_pos = buffer.text.byte_to_char(buffer.point_max()) as i64 + 1;

        let raw_start = if args[0].is_nil() {
            point_char_pos
        } else {
            expect_integer_or_marker(&args[0])?
        };
        let raw_end = if args[1].is_nil() {
            point_char_pos
        } else {
            expect_integer_or_marker(&args[1])?
        };

        if raw_start < 1 || raw_start > max_char_pos || raw_end < 1 || raw_end > max_char_pos {
            return Err(signal(
                "args-out-of-range",
                vec![args[0].clone(), args[1].clone()],
            ));
        }

        if raw_start >= raw_end {
            return Ok(Value::Nil);
        }

        let start_byte = buffer.text.char_to_byte((raw_start - 1) as usize);
        let end_byte = buffer.text.char_to_byte((raw_end - 1) as usize);
        (buffer.buffer_substring(start_byte, end_byte), raw_start, raw_end)
    };

    if start_char_pos >= end_char_pos {
        return Ok(Value::Nil);
    }
    eval_forms_from_source(eval, &source)
}

fn pop_unread_command_event(eval: &mut super::eval::Evaluator) -> Option<Value> {
    let current = eval
        .obarray
        .symbol_value("unread-command-events")
        .cloned()
        .unwrap_or(Value::Nil);
    match current {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            let head = pair.car.clone();
            let tail = pair.cdr.clone();
            drop(pair);
            eval.obarray.set_symbol_value("unread-command-events", tail);
            Some(head)
        }
        _ => None,
    }
}

fn event_to_int(event: &Value) -> Option<i64> {
    match event {
        Value::Int(n) => Some(*n),
        Value::Char(c) => Some(*c as i64),
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

/// `(read-event &optional PROMPT INHERIT-INPUT-METHOD SECONDS)`
///
/// Stub: returns nil (no event input available in batch mode).
pub(crate) fn builtin_read_event(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("read-event"), Value::Int(args.len() as i64)],
        ));
    }
    expect_optional_prompt_string(&args)?;
    if let Some(event) = pop_unread_command_event(eval) {
        if let Some(n) = event_to_int(&event) {
            return Ok(Value::Int(n));
        }
        return Ok(event);
    }
    Ok(Value::Nil)
}

/// `(read-char-exclusive &optional PROMPT INHERIT-INPUT-METHOD SECONDS)`
///
/// Batch stub: returns nil (no terminal input available).
pub(crate) fn builtin_read_char_exclusive(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("read-char-exclusive"),
                Value::Int(args.len() as i64),
            ],
        ));
    }
    expect_optional_prompt_string(&args)?;
    while let Some(event) = pop_unread_command_event(eval) {
        if let Some(n) = event_to_int(&event) {
            return Ok(Value::Int(n));
        }
        // Skip non-character events.
    }
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(get-load-suffixes)`
///
/// Return a list of suffixes that `load` tries when searching for files.
pub(crate) fn builtin_get_load_suffixes(args: Vec<Value>) -> EvalResult {
    let _ = args;
    Ok(Value::list(vec![
        Value::string(".so"),
        Value::string(".so.gz"),
        Value::string(".el"),
        Value::string(".el.gz"),
    ]))
}

/// `(locate-file FILENAME PATH SUFFIXES &optional PREDICATE)`
///
/// Search PATH for FILENAME with each suffix in SUFFIXES.
pub(crate) fn builtin_locate_file(args: Vec<Value>) -> EvalResult {
    expect_min_args("locate-file", &args, 3)?;
    let filename = expect_string(&args[0])?;
    let path = parse_path_argument(&args[1])?;
    let suffixes = parse_suffixes_argument(&args[2])?;
    Ok(match locate_file_with_path_and_suffixes(&filename, &path, &suffixes, args.get(3))? {
        Some(found) => Value::string(found),
        None => Value::Nil,
    })
}

/// `(locate-file-internal FILENAME PATH SUFFIXES &optional PREDICATE)`
///
/// Internal variant of `locate-file`; currently uses the same lookup behavior.
pub(crate) fn builtin_locate_file_internal(args: Vec<Value>) -> EvalResult {
    expect_min_args("locate-file-internal", &args, 3)?;
    let filename = expect_string(&args[0])?;
    let path = parse_path_argument(&args[1])?;
    let suffixes = parse_suffixes_argument(&args[2])?;
    Ok(match locate_file_with_path_and_suffixes(&filename, &path, &suffixes, args.get(3))? {
        Some(found) => Value::string(found),
        None => Value::Nil,
    })
}

/// `(read-coding-system PROMPT &optional DEFAULT-CODING-SYSTEM)`
///
/// In batch mode, this prompts for input and signals end-of-file.
pub(crate) fn builtin_read_coding_system(args: Vec<Value>) -> EvalResult {
    expect_min_args("read-coding-system", &args, 1)?;
    expect_max_args("read-coding-system", &args, 2)?;
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

/// `(read-non-nil-coding-system PROMPT)`
///
/// In batch mode, this prompts for input and signals end-of-file.
pub(crate) fn builtin_read_non_nil_coding_system(args: Vec<Value>) -> EvalResult {
    if args.len() != 1 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("read-non-nil-coding-system"),
                Value::Int(args.len() as i64),
            ],
        ));
    }
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
// Internal helpers
// ---------------------------------------------------------------------------

fn expect_list(value: &Value) -> Result<Vec<Value>, Flow> {
    list_to_vec(value).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), value.clone()],
        )
    })
}

fn parse_path_argument(value: &Value) -> Result<Vec<String>, Flow> {
    let mut path = Vec::new();
    for entry in expect_list(value)? {
        match entry {
            Value::Nil => path.push(".".to_string()),
            Value::Str(s) => path.push((*s).clone()),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), other],
                ))
            }
        }
    }
    Ok(path)
}

fn parse_suffixes_argument(value: &Value) -> Result<Vec<String>, Flow> {
    let mut suffixes = Vec::new();
    for entry in expect_list(value)? {
        match entry {
            Value::Nil => suffixes.push(String::new()),
            Value::Str(s) => suffixes.push((*s).clone()),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), other],
                ))
            }
        }
    }
    Ok(suffixes)
}

fn locate_file_with_path_and_suffixes(
    filename: &str,
    path: &[String],
    suffixes: &[String],
    predicate: Option<&Value>,
) -> Result<Option<String>, Flow> {
    let effective_suffixes: Vec<&str> = if suffixes.is_empty() {
        vec![""]
    } else {
        suffixes.iter().map(|s| s.as_str()).collect()
    };

    let absolute = Path::new(filename).is_absolute();
    if absolute || path.is_empty() {
        for suffix in &effective_suffixes {
            let candidate = format!("{filename}{suffix}");
            if Path::new(&candidate).exists()
                && predicate_matches_candidate(predicate, &candidate)?
            {
                return Ok(Some(candidate));
            }
        }
        return Ok(None);
    }

    for dir in path {
        let base = Path::new(dir).join(filename);
        let base = base.to_string_lossy();
        for suffix in &effective_suffixes {
            let candidate = format!("{base}{suffix}");
            if Path::new(&candidate).exists()
                && predicate_matches_candidate(predicate, &candidate)?
            {
                return Ok(Some(candidate));
            }
        }
    }

    Ok(None)
}

fn predicate_matches_candidate(predicate: Option<&Value>, candidate: &str) -> Result<bool, Flow> {
    let Some(predicate) = predicate else {
        return Ok(true);
    };
    if predicate.is_nil() {
        return Ok(true);
    }

    let Some(symbol) = predicate.as_symbol_name() else {
        // We currently only support symbol predicates in pure dispatch;
        // unknown predicate object shapes default to accepting candidate.
        return Ok(true);
    };
    let Some(result) = super::builtins::dispatch_builtin_pure(symbol, vec![Value::string(candidate)])
    else {
        // Emacs locate-file tolerates non-callable predicate values in practice.
        // Keep search behavior instead of surfacing an execution error here.
        return Ok(true);
    };
    Ok(result?.is_truthy())
}

/// Compute the byte offset after the first s-expression in `input`.
/// Skips leading whitespace/comments, then skips one complete sexp.
fn compute_read_end_position(input: &str) -> usize {
    let pos = skip_ws_comments(input, 0);
    if pos >= input.len() {
        return input.len();
    }
    skip_one_sexp(input, pos)
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
            // block comment #| ... |#
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
            // quote / backquote -- skip prefix then one sexp
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
                // Regular character -- consume one UTF-8 char
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
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::eval::Evaluator;

    #[test]
    fn read_from_string_integer() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("42")]).unwrap();
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
                assert!(matches!(&pair.car, Value::Symbol(ref s) if s == "hello"));
                assert!(matches!(&pair.cdr, Value::Int(5)));
            }
            _ => panic!("Expected cons, got {:?}", result),
        }
    }

    #[test]
    fn read_from_string_with_start() {
        let mut ev = Evaluator::new();
        let result =
            builtin_read_from_string(&mut ev, vec![Value::string("  42 rest"), Value::Int(2)])
                .unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(matches!(&pair.car, Value::Int(42)));
                assert!(matches!(&pair.cdr, Value::Int(4)));
            }
            _ => panic!("Expected cons, got {:?}", result),
        }
    }

    #[test]
    fn read_from_string_empty_error() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("")]);
        assert!(result.is_err());
    }

    #[test]
    fn read_from_string_list() {
        let mut ev = Evaluator::new();
        let result = builtin_read_from_string(&mut ev, vec![Value::string("(+ 1 2)")]).unwrap();
        match &result {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert!(pair.car.is_cons());
                assert!(matches!(&pair.cdr, Value::Int(7)));
            }
            _ => panic!("Expected cons"),
        }
    }

    #[test]
    fn read_from_nil_stream() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn read_from_string_stream() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![Value::string("42")]).unwrap();
        assert!(matches!(result, Value::Int(42)));
    }

    #[test]
    fn read_no_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn eval_buffer_evaluates_current_buffer_forms() {
        let mut ev = Evaluator::new();
        {
            let buf = ev.buffers.current_buffer_mut().expect("current buffer");
            buf.insert("(setq lread-eb-a 11)\n(setq lread-eb-b (+ lread-eb-a 1))");
        }
        let result = builtin_eval_buffer(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("lread-eb-a").cloned(),
            Some(Value::Int(11))
        );
        assert_eq!(
            ev.obarray.symbol_value("lread-eb-b").cloned(),
            Some(Value::Int(12))
        );
    }

    #[test]
    fn eval_buffer_uses_source_text_without_switching_current() {
        let mut ev = Evaluator::new();
        let target = ev.buffers.create_buffer("*lread-eval-buffer-target*");
        {
            let target_buf = ev.buffers.get_mut(target).expect("target buffer");
            target_buf.insert("(setq lread-eb-current-name (buffer-name))");
        }
        let caller = ev.buffers.create_buffer("*lread-eval-buffer-caller*");
        ev.buffers.set_current(caller);

        let result = builtin_eval_buffer(&mut ev, vec![Value::Buffer(target)]).unwrap();
        assert!(result.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("lread-eb-current-name").cloned(),
            Some(Value::string("*lread-eval-buffer-caller*"))
        );
    }

    #[test]
    fn eval_buffer_reports_designator_and_arity_errors() {
        let mut ev = Evaluator::new();

        let missing = builtin_eval_buffer(&mut ev, vec![Value::string("*no-such-buffer*")]);
        assert!(matches!(
            missing,
            Err(Flow::Signal(sig))
                if sig.symbol == "error" && sig.data == vec![Value::string("No such buffer")]
        ));

        let bad_type = builtin_eval_buffer(&mut ev, vec![Value::Int(1)]);
        assert!(matches!(
            bad_type,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data == vec![Value::symbol("stringp"), Value::Int(1)]
        ));

        let arity = builtin_eval_buffer(
            &mut ev,
            vec![
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            arity,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-number-of-arguments"
                    && sig.data == vec![Value::symbol("eval-buffer"), Value::Int(6)]
        ));
    }

    #[test]
    fn eval_region_evaluates_forms_in_range() {
        let mut ev = Evaluator::new();
        {
            let buf = ev.buffers.current_buffer_mut().expect("current buffer");
            buf.insert("(setq lread-er-a 1)\n(setq lread-er-b (+ lread-er-a 2))");
        }
        let end = {
            let buf = ev.buffers.current_buffer().expect("current buffer");
            Value::Int(buf.text.char_count() as i64 + 1)
        };

        let result = builtin_eval_region(&mut ev, vec![Value::Int(1), end]).unwrap();
        assert!(result.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("lread-er-a").cloned(),
            Some(Value::Int(1))
        );
        assert_eq!(
            ev.obarray.symbol_value("lread-er-b").cloned(),
            Some(Value::Int(3))
        );
    }

    #[test]
    fn eval_region_nil_or_reversed_bounds_are_noop() {
        let mut ev = Evaluator::new();
        {
            let buf = ev.buffers.current_buffer_mut().expect("current buffer");
            buf.insert("(setq lread-er-noop 9)");
        }
        ev.obarray.set_symbol_value("lread-er-noop", Value::Int(0));

        let nil_bounds = builtin_eval_region(&mut ev, vec![Value::Nil, Value::Nil]).unwrap();
        assert!(nil_bounds.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("lread-er-noop").cloned(),
            Some(Value::Int(0))
        );

        let point_max = {
            let buf = ev.buffers.current_buffer().expect("current buffer");
            buf.text.char_count() as i64 + 1
        };
        let reversed = builtin_eval_region(&mut ev, vec![Value::Int(point_max), Value::Int(1)])
            .unwrap();
        assert!(reversed.is_nil());
        assert_eq!(
            ev.obarray.symbol_value("lread-er-noop").cloned(),
            Some(Value::Int(0))
        );
    }

    #[test]
    fn eval_region_reports_type_range_and_arity_errors() {
        let mut ev = Evaluator::new();
        {
            let buf = ev.buffers.current_buffer_mut().expect("current buffer");
            buf.insert("(+ 1 2)");
        }
        let point_max = {
            let buf = ev.buffers.current_buffer().expect("current buffer");
            buf.text.char_count() as i64 + 1
        };

        let bad_start =
            builtin_eval_region(&mut ev, vec![Value::string("1"), Value::Int(point_max)]);
        assert!(matches!(
            bad_start,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data
                        == vec![Value::symbol("integer-or-marker-p"), Value::string("1")]
        ));

        let bad_end = builtin_eval_region(&mut ev, vec![Value::Int(1), Value::string("2")]);
        assert!(matches!(
            bad_end,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data
                        == vec![Value::symbol("integer-or-marker-p"), Value::string("2")]
        ));

        let range = builtin_eval_region(&mut ev, vec![Value::Int(1), Value::Int(999)]);
        assert!(matches!(
            range,
            Err(Flow::Signal(sig))
                if sig.symbol == "args-out-of-range"
                    && sig.data == vec![Value::Int(1), Value::Int(999)]
        ));

        let arity_low = builtin_eval_region(&mut ev, vec![]);
        assert!(matches!(
            arity_low,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-number-of-arguments"
                    && sig.data == vec![Value::symbol("eval-region"), Value::Int(0)]
        ));

        let arity_high = builtin_eval_region(
            &mut ev,
            vec![
                Value::Int(1),
                Value::Int(point_max),
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(matches!(
            arity_high,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-number-of-arguments"
                    && sig.data == vec![Value::symbol("eval-region"), Value::Int(5)]
        ));
    }

    #[test]
    fn eval_region_keeps_point_stable_without_side_effects() {
        let mut ev = Evaluator::new();
        {
            let buf = ev.buffers.current_buffer_mut().expect("current buffer");
            buf.insert("(setq lread-er-point 1)");
            buf.goto_char(0);
        }
        let end = {
            let buf = ev.buffers.current_buffer().expect("current buffer");
            Value::Int(buf.text.char_count() as i64 + 1)
        };
        let result = builtin_eval_region(&mut ev, vec![Value::Int(1), end]).unwrap();
        assert!(result.is_nil());
        let point = ev.buffers.current_buffer().expect("current buffer").point_char() as i64 + 1;
        assert_eq!(point, 1);
    }

    #[test]
    fn read_event_returns_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_read_event(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn read_event_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_read_event(&mut ev, vec![Value::Int(123)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_event_consumes_unread_command_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_event(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_int(), Some(97));
    }

    #[test]
    fn read_event_rejects_more_than_three_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_event(
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
    fn read_char_exclusive_returns_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_read_char_exclusive(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn read_char_exclusive_rejects_non_string_prompt() {
        let mut ev = Evaluator::new();
        let result = builtin_read_char_exclusive(&mut ev, vec![Value::Int(123)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-type-argument"
        ));
    }

    #[test]
    fn read_char_exclusive_consumes_unread_command_event() {
        let mut ev = Evaluator::new();
        ev.obarray
            .set_symbol_value("unread-command-events", Value::list(vec![Value::Int(97)]));
        let result = builtin_read_char_exclusive(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_int(), Some(97));
    }

    #[test]
    fn read_char_exclusive_rejects_more_than_three_args() {
        let mut ev = Evaluator::new();
        let result = builtin_read_char_exclusive(
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
    fn read_char_exclusive_skips_non_character_events() {
        let mut ev = Evaluator::new();
        ev.obarray.set_symbol_value(
            "unread-command-events",
            Value::list(vec![Value::symbol("foo"), Value::Int(97)]),
        );
        let result = builtin_read_char_exclusive(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_int(), Some(97));
    }

    #[test]
    fn get_load_suffixes_returns_list() {
        let result = builtin_get_load_suffixes(vec![]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 4);
        assert_eq!(items[0].as_str(), Some(".so"));
        assert_eq!(items[1].as_str(), Some(".so.gz"));
        assert_eq!(items[2].as_str(), Some(".el"));
        assert_eq!(items[3].as_str(), Some(".el.gz"));
    }

    #[test]
    fn locate_file_finds_first_matching_suffix() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-locate-file-{unique}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        fs::write(dir.join("probe.el"), "(setq vm-locate 1)\n").expect("write .el");
        fs::write(dir.join("probe.elc"), "compiled").expect("write .elc");

        let result = builtin_locate_file(vec![
            Value::string("probe"),
            Value::list(vec![Value::string(dir.to_string_lossy())]),
            Value::list(vec![Value::string(".el"), Value::string(".elc")]),
        ])
        .expect("locate-file should succeed");
        let found = result.as_str().expect("locate-file should return path");
        assert!(
            found.ends_with("probe.el"),
            "expected first matching suffix (.el), got {found}",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn locate_file_respects_symbol_predicates() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-locate-file-predicate-{unique}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        fs::write(dir.join("probe.el"), "(setq vm-locate 1)\n").expect("write .el");

        let regular = builtin_locate_file(vec![
            Value::string("probe"),
            Value::list(vec![Value::string(dir.to_string_lossy())]),
            Value::list(vec![Value::string(".el")]),
            Value::symbol("file-regular-p"),
        ])
        .expect("locate-file with file-regular-p should evaluate");
        assert!(
            regular.as_str().is_some(),
            "regular-file predicate should accept candidate",
        );

        let directory = builtin_locate_file(vec![
            Value::string("probe"),
            Value::list(vec![Value::string(dir.to_string_lossy())]),
            Value::list(vec![Value::string(".el")]),
            Value::symbol("file-directory-p"),
        ])
        .expect("locate-file with file-directory-p should evaluate");
        assert!(directory.is_nil(), "directory predicate should reject file");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn locate_file_unknown_predicate_defaults_to_truthy_match() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-locate-file-bad-predicate-{unique}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        fs::write(dir.join("probe.el"), "(setq vm-locate 1)\n").expect("write .el");

        let result = builtin_locate_file(vec![
            Value::string("probe"),
            Value::list(vec![Value::string(dir.to_string_lossy())]),
            Value::list(vec![Value::string(".el")]),
            Value::symbol("definitely-not-a-real-predicate"),
        ])
        .expect("locate-file should evaluate");
        let found = result
            .as_str()
            .expect("unknown predicate should not prevent match");
        assert!(found.ends_with("probe.el"), "unexpected result: {found}");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn locate_file_internal_returns_nil_when_missing() {
        let result = builtin_locate_file_internal(vec![
            Value::string("definitely-missing-neovm-file"),
            Value::list(vec![Value::string(".")]),
            Value::list(vec![Value::string(".el")]),
        ])
        .expect("locate-file-internal should evaluate");
        assert!(result.is_nil());
    }

    #[test]
    fn locate_file_internal_finds_requested_suffix() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-locate-file-internal-{unique}"));
        fs::create_dir_all(&dir).expect("create temp dir");
        fs::write(dir.join("probe.elc"), "compiled").expect("write .elc");

        let result = builtin_locate_file_internal(vec![
            Value::string("probe"),
            Value::list(vec![Value::string(dir.to_string_lossy())]),
            Value::list(vec![Value::string(".elc")]),
        ])
        .expect("locate-file-internal should succeed");
        let found = result.as_str().expect("locate-file-internal should return path");
        assert!(
            found.ends_with("probe.elc"),
            "expected .elc resolution, got {found}",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn read_coding_system_signals_batch_eof() {
        let result = builtin_read_coding_system(vec![Value::string("")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "end-of-file"
                    && sig.data == vec![Value::string("Error reading from stdin")]
        ));
    }

    #[test]
    fn read_coding_system_validates_prompt_type_and_arity() {
        let bad_prompt = builtin_read_coding_system(vec![Value::Int(1)]);
        assert!(matches!(
            bad_prompt,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data == vec![Value::symbol("stringp"), Value::Int(1)]
        ));

        let arity = builtin_read_coding_system(vec![Value::string(""), Value::Nil, Value::Nil]);
        assert!(matches!(
            arity,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-number-of-arguments"
                    && sig.data == vec![Value::symbol("read-coding-system"), Value::Int(3)]
        ));
    }

    #[test]
    fn read_non_nil_coding_system_signals_batch_eof() {
        let result = builtin_read_non_nil_coding_system(vec![Value::string("")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "end-of-file"
                    && sig.data == vec![Value::string("Error reading from stdin")]
        ));
    }

    #[test]
    fn read_non_nil_coding_system_validates_prompt_type_and_arity() {
        let bad_prompt = builtin_read_non_nil_coding_system(vec![Value::Int(1)]);
        assert!(matches!(
            bad_prompt,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data == vec![Value::symbol("stringp"), Value::Int(1)]
        ));

        let arity = builtin_read_non_nil_coding_system(vec![Value::string(""), Value::Nil]);
        assert!(matches!(
            arity,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-number-of-arguments"
                    && sig.data
                        == vec![Value::symbol("read-non-nil-coding-system"), Value::Int(2)]
        ));
    }
}
