use super::*;

// ===========================================================================
// String operations
// ===========================================================================

pub(crate) fn builtin_string_equal(args: Vec<Value>) -> EvalResult {
    expect_args("string-equal", &args, 2)?;
    let a = expect_string_comparison_operand(&args[0])?;
    let b = expect_string_comparison_operand(&args[1])?;
    Ok(Value::bool(a == b))
}

pub(crate) fn builtin_string_lessp(args: Vec<Value>) -> EvalResult {
    expect_args("string-lessp", &args, 2)?;
    let a = expect_string_comparison_operand(&args[0])?;
    let b = expect_string_comparison_operand(&args[1])?;
    Ok(Value::bool(a < b))
}

pub(crate) fn builtin_string_greaterp(args: Vec<Value>) -> EvalResult {
    expect_args("string-greaterp", &args, 2)?;
    let a = expect_string_comparison_operand(&args[0])?;
    let b = expect_string_comparison_operand(&args[1])?;
    Ok(Value::bool(a > b))
}

fn substring_impl(name: &str, args: &[Value]) -> EvalResult {
    expect_min_args(name, args, 1)?;
    expect_max_args(name, args, 3)?;
    let s = expect_string(&args[0])?;
    let len = storage_char_len(&s) as i64;

    let normalize_index = |value: &Value, default: i64| -> Result<i64, Flow> {
        let raw = if value.is_nil() {
            default
        } else {
            expect_int(value)?
        };
        let idx = if raw < 0 { len + raw } else { raw };
        if idx < 0 || idx > len {
            return Err(signal(
                "args-out-of-range",
                vec![
                    args[0],
                    args[1],
                    args.get(2).cloned().unwrap_or(Value::Nil),
                ],
            ));
        }
        Ok(idx)
    };

    let from = if args.len() > 1 {
        normalize_index(&args[1], 0)?
    } else {
        0
    } as usize;

    let to = if args.len() > 2 {
        normalize_index(&args[2], len)?
    } else {
        len
    } as usize;

    if from > to {
        return Err(signal(
            "args-out-of-range",
            vec![
                args[0],
                args.get(1).cloned().unwrap_or(Value::Int(0)),
                args.get(2).cloned().unwrap_or(Value::Nil),
            ],
        ));
    }
    let result = storage_substring(&s, from, to).ok_or_else(|| {
        signal(
            "args-out-of-range",
            vec![
                args[0],
                args.get(1).cloned().unwrap_or(Value::Int(0)),
                args.get(2).cloned().unwrap_or(Value::Nil),
            ],
        )
    })?;
    Ok(Value::string(result))
}

pub(crate) fn builtin_substring(args: Vec<Value>) -> EvalResult {
    substring_impl("substring", &args)
}

pub(crate) fn builtin_substring_no_properties(args: Vec<Value>) -> EvalResult {
    substring_impl("substring-no-properties", &args)
}

pub(crate) fn builtin_concat(args: Vec<Value>) -> EvalResult {
    fn push_concat_int(result: &mut String, n: i64) -> Result<(), Flow> {
        if !(0..=0x3FFFFF).contains(&n) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), Value::Int(n)],
            ));
        }

        let cp = n as u32;
        if let Some(c) = char::from_u32(cp) {
            result.push(c);
            return Ok(());
        }

        // Emacs concat path for raw-byte non-Unicode chars uses byte->multibyte encoding.
        if (0x3FFF00..=0x3FFFFF).contains(&cp) {
            let b = (cp - 0x3FFF00) as u8;
            let bytes = if b < 0x80 {
                vec![b]
            } else {
                vec![0xC0 | ((b >> 6) & 0x01), 0x80 | (b & 0x3F)]
            };
            result.push_str(&bytes_to_storage_string(&bytes));
            return Ok(());
        }

        if let Some(encoded) = encode_nonunicode_char_for_storage(cp) {
            result.push_str(&encoded);
            return Ok(());
        }

        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), Value::Int(n)],
        ))
    }

    fn push_concat_element(result: &mut String, value: &Value) -> Result<(), Flow> {
        match value {
            Value::Char(c) => {
                result.push(*c);
                Ok(())
            }
            Value::Int(n) => push_concat_int(result, *n),
            other => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), *other],
            )),
        }
    }

    let mut result = String::new();
    for arg in &args {
        match arg {
            Value::Str(id) => result.push_str(&with_heap(|h| h.get_string(*id).clone())),
            Value::Nil => {}
            Value::Cons(_) => {
                let mut cursor = *arg;
                loop {
                    match cursor {
                        Value::Nil => break,
                        Value::Cons(cell) => {
                            let pair = read_cons(cell);
                            push_concat_element(&mut result, &pair.car)?;
                            cursor = pair.cdr;
                        }
                        tail => {
                            return Err(signal(
                                "wrong-type-argument",
                                vec![Value::symbol("listp"), tail],
                            ))
                        }
                    }
                }
            }
            Value::Vector(v) => {
                let items = with_heap(|h| h.get_vector(*v).clone());
                for item in items.iter() {
                    push_concat_element(&mut result, item)?;
                }
            }
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("sequencep"), *arg],
                ))
            }
        }
    }
    Ok(Value::string(result))
}

pub(crate) fn builtin_string_to_number(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-to-number", &args, 1)?;
    expect_max_args("string-to-number", &args, 2)?;
    let s = expect_string(&args[0])?;
    let base = if args.len() > 1 {
        expect_int(&args[1])?
    } else {
        10
    };

    if !(2..=16).contains(&base) {
        return Err(signal("args-out-of-range", vec![Value::Int(base)]));
    }

    let s = s.trim_start();
    if base == 10 {
        let number_prefix =
            Regex::new(r"^[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?")
                .expect("number prefix regexp should compile");
        if let Some(m) = number_prefix.find(s) {
            let token = m.as_str();
            let is_float = token.contains('.') || token.contains('e') || token.contains('E');
            if is_float {
                if let Ok(f) = token.parse::<f64>() {
                    return Ok(Value::Float(f));
                }
            } else if let Ok(n) = token.parse::<i64>() {
                return Ok(Value::Int(n));
            }
        }
    } else {
        let bytes = s.as_bytes();
        let mut pos = 0usize;
        let mut negative = false;
        if pos < bytes.len() {
            if bytes[pos] == b'+' {
                pos += 1;
            } else if bytes[pos] == b'-' {
                negative = true;
                pos += 1;
            }
        }
        let digit_start = pos;
        while pos < bytes.len() {
            let ch = bytes[pos] as char;
            let Some(d) = ch.to_digit(36) else { break };
            if (d as i64) < base {
                pos += 1;
            } else {
                break;
            }
        }
        if pos > digit_start {
            let token = &s[digit_start..pos];
            if let Ok(parsed) = i64::from_str_radix(token, base as u32) {
                return Ok(Value::Int(if negative { -parsed } else { parsed }));
            }
        }
    }
    Ok(Value::Int(0))
}

pub(crate) fn builtin_number_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("number-to-string", &args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::string(n.to_string())),
        Value::Float(f) => Ok(Value::string(format!("{}", f))),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("numberp"), *other],
        )),
    }
}

pub(crate) fn builtin_upcase(args: Vec<Value>) -> EvalResult {
    expect_args("upcase", &args, 1)?;
    match &args[0] {
        Value::Str(id) => Ok(Value::string(upcase_string_emacs_compat(&with_heap(|h| h.get_string(*id).clone())))),
        Value::Char(c) => {
            let mapped = upcase_char_code_emacs_compat(*c as i64);
            if let Some(ch) = u32::try_from(mapped).ok().and_then(char::from_u32) {
                Ok(Value::Char(ch))
            } else {
                Ok(Value::Char(*c))
            }
        }
        Value::Int(n) => {
            if *n < 0 {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), Value::Int(*n)],
                ))
            } else {
                Ok(Value::Int(upcase_char_code_emacs_compat(*n)))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), *other],
        )),
    }
}

fn preserve_emacs_upcase_payload(code: i64) -> bool {
    matches!(
        code,
        305
            | 329
            | 383
            | 411
            | 496
            | 612
            | 912
            | 944
            | 1415
            | 7306
            | 7830..=7834
            | 8016
            | 8018
            | 8020
            | 8022
            | 8072..=8079
            | 8088..=8095
            | 8104..=8111
            | 8114
            | 8116
            | 8118..=8119
            | 8124
            | 8130
            | 8132
            | 8134..=8135
            | 8140
            | 8146..=8147
            | 8150..=8151
            | 8162..=8164
            | 8166..=8167
            | 8178
            | 8180
            | 8182..=8183
            | 8188
            | 42957
            | 42959
            | 42963
            | 42965
            | 42971
            | 64256..=64262
            | 64275..=64279
            | 68976..=68997
            | 93883..=93907
    )
}

fn upcase_string_emacs_compat(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        let code = ch as i64;
        if ch == '\u{0131}' || preserve_emacs_upcase_string_payload(code) {
            out.push(ch);
            continue;
        }
        for up in ch.to_uppercase() {
            out.push(up);
        }
    }
    out
}

fn upcase_char_code_emacs_compat(code: i64) -> i64 {
    if preserve_emacs_upcase_payload(code) {
        return code;
    }
    match code {
        223 => 7838,
        8064..=8071 | 8080..=8087 | 8096..=8103 => code + 8,
        8115 | 8131 | 8179 => code + 9,
        _ => {
            if let Some(c) = u32::try_from(code).ok().and_then(char::from_u32) {
                c.to_uppercase().next().unwrap_or(c) as i64
            } else {
                code
            }
        }
    }
}

fn preserve_emacs_upcase_string_payload(code: i64) -> bool {
    matches!(
        code,
        411
            | 612
            | 7306
            | 42957
            | 42959
            | 42963
            | 42965
            | 42971
            | 68976..=68997
            | 93883..=93907
    )
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

pub(super) fn downcase_char_code_emacs_compat(code: i64) -> i64 {
    if preserve_emacs_downcase_payload(code) {
        return code;
    }
    if let Some(c) = u32::try_from(code).ok().and_then(char::from_u32) {
        c.to_lowercase().next().unwrap_or(c) as i64
    } else {
        code
    }
}

pub(crate) fn builtin_downcase(args: Vec<Value>) -> EvalResult {
    expect_args("downcase", &args, 1)?;
    match &args[0] {
        Value::Str(id) => Ok(Value::string(downcase_string_emacs_compat(&with_heap(|h| h.get_string(*id).clone())))),
        Value::Char(c) => {
            let mapped = downcase_char_code_emacs_compat(*c as i64);
            if let Some(ch) = u32::try_from(mapped).ok().and_then(char::from_u32) {
                Ok(Value::Char(ch))
            } else {
                Ok(Value::Char(*c))
            }
        }
        Value::Int(n) => {
            if *n < 0 {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), Value::Int(*n)],
                ))
            } else {
                Ok(Value::Int(downcase_char_code_emacs_compat(*n)))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), *other],
        )),
    }
}

fn downcase_string_emacs_compat(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        let code = ch as i64;
        if ch == '\u{212A}' || preserve_emacs_downcase_string_payload(code) {
            out.push(ch);
            continue;
        }
        for low in ch.to_lowercase() {
            out.push(low);
        }
    }
    out
}

fn preserve_emacs_downcase_string_payload(code: i64) -> bool {
    matches!(
        code,
        7305
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

pub(crate) fn builtin_format(args: Vec<Value>) -> EvalResult {
    builtin_format_wrapper_strict(args)
}

pub(crate) fn builtin_ngettext(args: Vec<Value>) -> EvalResult {
    expect_args("ngettext", &args, 3)?;
    let singular = expect_strict_string(&args[0])?;
    let plural = expect_strict_string(&args[1])?;
    let count = expect_int(&args[2])?;
    if count == 1 {
        Ok(Value::string(singular))
    } else {
        Ok(Value::string(plural))
    }
}

pub(crate) fn builtin_format_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    builtin_format_wrapper_strict_eval(eval, args)
}

fn format_percent_s_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    match value {
        Value::Str(id) => with_heap(|h| h.get_string(*id).clone()),
        Value::Buffer(id) => {
            if let Some(buf) = eval.buffers.get(*id) {
                return buf.name.clone();
            }
            if eval.buffers.dead_buffer_last_name(*id).is_some() {
                return "#<killed buffer>".to_string();
            }
            print_value_eval(eval, value)
        }
        _ => print_value_eval(eval, value),
    }
}

fn format_not_enough_args_error() -> Flow {
    signal(
        "error",
        vec![Value::string("Not enough arguments for format string")],
    )
}

fn format_spec_type_mismatch_error() -> Flow {
    signal(
        "error",
        vec![Value::string(
            "Format specifier doesn’t match argument type",
        )],
    )
}

fn format_char_argument(n: i64) -> Result<String, Flow> {
    if !(0..=KEY_CHAR_CODE_MASK).contains(&n) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), Value::Int(n)],
        ));
    }

    write_char_rendered_text(n).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), Value::Int(n)],
        )
    })
}

pub(super) fn builtin_format_wrapper_strict(args: Vec<Value>) -> EvalResult {
    expect_min_args("format", &args, 1)?;
    let fmt_str = expect_strict_string(&args[0])?;
    let mut result = String::new();
    let mut arg_idx = 1;
    let mut chars = fmt_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '%' {
            if let Some(&spec) = chars.peek() {
                chars.next();
                match spec {
                    's' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        match &args[arg_idx] {
                            Value::Str(id) => result.push_str(&with_heap(|h| h.get_string(*id).clone())),
                            other => result.push_str(&super::print::print_value(other)),
                        }
                        arg_idx += 1;
                    }
                    'S' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        result.push_str(&super::print::print_value(&args[arg_idx]));
                        arg_idx += 1;
                    }
                    'd' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        let n = expect_int(&args[arg_idx])
                            .map_err(|_| format_spec_type_mismatch_error())?;
                        result.push_str(&n.to_string());
                        arg_idx += 1;
                    }
                    'f' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        let f = expect_number(&args[arg_idx])
                            .map_err(|_| format_spec_type_mismatch_error())?;
                        result.push_str(&format!("{:.6}", f));
                        arg_idx += 1;
                    }
                    'c' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        let n = expect_int(&args[arg_idx])
                            .map_err(|_| format_spec_type_mismatch_error())?;
                        result.push_str(&format_char_argument(n)?);
                        arg_idx += 1;
                    }
                    '%' => result.push('%'),
                    _ => {
                        result.push('%');
                        result.push(spec);
                    }
                }
            } else {
                result.push('%');
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::string(result))
}

pub(super) fn builtin_format_wrapper_strict_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("format", &args, 1)?;
    let fmt_str = expect_strict_string(&args[0])?;
    let mut result = String::new();
    let mut arg_idx = 1;
    let mut chars = fmt_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '%' {
            if let Some(&spec) = chars.peek() {
                chars.next();
                match spec {
                    's' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        result.push_str(&format_percent_s_eval(eval, &args[arg_idx]));
                        arg_idx += 1;
                    }
                    'S' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        result.push_str(&print_value_eval(eval, &args[arg_idx]));
                        arg_idx += 1;
                    }
                    'd' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        let n = expect_int(&args[arg_idx])
                            .map_err(|_| format_spec_type_mismatch_error())?;
                        result.push_str(&n.to_string());
                        arg_idx += 1;
                    }
                    'f' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        let f = expect_number(&args[arg_idx])
                            .map_err(|_| format_spec_type_mismatch_error())?;
                        result.push_str(&format!("{:.6}", f));
                        arg_idx += 1;
                    }
                    'c' => {
                        if arg_idx >= args.len() {
                            return Err(format_not_enough_args_error());
                        }
                        let n = expect_int(&args[arg_idx])
                            .map_err(|_| format_spec_type_mismatch_error())?;
                        result.push_str(&format_char_argument(n)?);
                        arg_idx += 1;
                    }
                    '%' => result.push('%'),
                    _ => {
                        result.push('%');
                        result.push(spec);
                    }
                }
            } else {
                result.push('%');
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::string(result))
}

pub(crate) fn builtin_format_message_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("format-message", &args, 1)?;
    builtin_format_eval(eval, args)
}


// ===========================================================================
// Extended string operations
// ===========================================================================

pub(crate) fn builtin_string_prefix_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-prefix-p", &args, 2)?;
    expect_max_args("string-prefix-p", &args, 3)?;
    let mut prefix = expect_string(&args[0])?;
    let mut s = expect_string(&args[1])?;
    if args.get(2).is_some_and(|v| v.is_truthy()) {
        prefix = prefix.to_lowercase();
        s = s.to_lowercase();
    }
    Ok(Value::bool(s.starts_with(&prefix)))
}

pub(crate) fn builtin_string_suffix_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-suffix-p", &args, 2)?;
    expect_max_args("string-suffix-p", &args, 3)?;
    let mut suffix = expect_string(&args[0])?;
    let mut s = expect_string(&args[1])?;
    if args.get(2).is_some_and(|v| v.is_truthy()) {
        suffix = suffix.to_lowercase();
        s = s.to_lowercase();
    }
    Ok(Value::bool(s.ends_with(&suffix)))
}

pub(crate) fn builtin_string_join(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-join", &args, 1)?;
    expect_max_args("string-join", &args, 2)?;
    let strs = list_to_vec(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0]],
        )
    })?;
    let sep = match args.get(1) {
        None | Some(Value::Nil) => "".to_string(),
        Some(other) => expect_string(other)?,
    };
    let parts: Result<Vec<String>, Flow> = strs
        .iter()
        .map(|value| {
            let rendered = builtin_concat(vec![*value])?;
            let Value::Str(id) = rendered else {
                unreachable!("concat should always return a string");
            };
            Ok(with_heap(|h| h.get_string(id).clone()))
        })
        .collect();
    Ok(Value::string(parts?.join(&sep)))
}

pub(crate) fn builtin_split_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("split-string", &args, 1)?;
    expect_max_args("split-string", &args, 4)?;
    let s = expect_string(&args[0])?;

    let separator = match args.get(1) {
        None | Some(Value::Nil) => None,
        Some(other) => Some(expect_string(other)?),
    };
    let omit_nulls = args.get(2).is_some_and(|v| v.is_truthy());
    let trim_regex = match args.get(3) {
        None | Some(Value::Nil) => None,
        Some(other) => Some(expect_string(other)?),
    };

    let (splitter, default_omit_nulls) = match separator {
        Some(pattern) => {
            let compiled = Regex::new(&pattern).map_err(|e| {
                signal(
                    "invalid-regexp",
                    vec![Value::string(format!(
                        "Invalid regexp \"{}\": {}",
                        pattern, e
                    ))],
                )
            })?;
            (compiled, false)
        }
        None => (
            Regex::new(r"[ \f\t\n\r\v]+").expect("default split regexp should compile"),
            true,
        ),
    };

    let trimmer = match trim_regex {
        Some(pattern) => Some(Regex::new(&pattern).map_err(|e| {
            signal(
                "invalid-regexp",
                vec![Value::string(format!(
                    "Invalid regexp \"{}\": {}",
                    pattern, e
                ))],
            )
        })?),
        None => None,
    };

    let should_omit_nulls = default_omit_nulls || omit_nulls;
    let mut parts = Vec::new();
    for part in splitter.split(&s) {
        let mut segment = part.to_string();
        if let Some(trim_re) = trimmer.as_ref() {
            loop {
                let Some(m) = trim_re.find(&segment) else {
                    break;
                };
                if m.start() == 0 && m.end() > 0 {
                    segment = segment[m.end()..].to_string();
                } else {
                    break;
                }
            }
            loop {
                let tail = trim_re
                    .find_iter(&segment)
                    .filter(|m| m.end() == segment.len() && m.start() < m.end())
                    .last();
                let Some(m) = tail else { break };
                segment = segment[..m.start()].to_string();
            }
        }
        if should_omit_nulls && segment.is_empty() {
            continue;
        }
        parts.push(Value::string(segment));
    }

    Ok(Value::list(parts))
}

fn compile_trim_regex(name: &str, pattern: &str) -> Result<Regex, Flow> {
    Regex::new(pattern).map_err(|e| {
        signal(
            "invalid-regexp",
            vec![Value::string(format!(
                "Invalid regexp \"{}\" in {}: {}",
                pattern, name, e
            ))],
        )
    })
}

fn trim_leading_with_regex(input: &str, re: &Regex) -> String {
    let mut out = input.to_string();
    loop {
        let Some(m) = re.find(&out) else { break };
        if m.start() == 0 && m.end() > 0 {
            out = out[m.end()..].to_string();
        } else {
            break;
        }
    }
    out
}

fn trim_trailing_with_regex(input: &str, re: &Regex) -> String {
    let mut out = input.to_string();
    loop {
        let tail = re
            .find_iter(&out)
            .filter(|m| m.end() == out.len() && m.start() < m.end())
            .last();
        let Some(m) = tail else { break };
        out = out[..m.start()].to_string();
    }
    out
}

pub(crate) fn builtin_string_trim(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-trim", &args, 1)?;
    expect_max_args("string-trim", &args, 3)?;
    let s = expect_string(&args[0])?;
    let trim_left_pattern = match args.get(1) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_right_pattern = match args.get(2) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_left = compile_trim_regex("string-trim", &trim_left_pattern)?;
    let trim_right = compile_trim_regex("string-trim", &trim_right_pattern)?;
    let left_trimmed = trim_leading_with_regex(&s, &trim_left);
    Ok(Value::string(trim_trailing_with_regex(
        &left_trimmed,
        &trim_right,
    )))
}

pub(crate) fn builtin_string_trim_left(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-trim-left", &args, 1)?;
    expect_max_args("string-trim-left", &args, 2)?;
    let s = expect_string(&args[0])?;
    let trim_left_pattern = match args.get(1) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_left = compile_trim_regex("string-trim-left", &trim_left_pattern)?;
    Ok(Value::string(trim_leading_with_regex(&s, &trim_left)))
}

pub(crate) fn builtin_string_trim_right(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-trim-right", &args, 1)?;
    expect_max_args("string-trim-right", &args, 2)?;
    let s = expect_string(&args[0])?;
    let trim_right_pattern = match args.get(1) {
        None | Some(Value::Nil) => "[ \t\n\r]+".to_string(),
        Some(other) => expect_string(other)?,
    };
    let trim_right = compile_trim_regex("string-trim-right", &trim_right_pattern)?;
    Ok(Value::string(trim_trailing_with_regex(&s, &trim_right)))
}

pub(crate) fn builtin_make_string(args: Vec<Value>) -> EvalResult {
    expect_args("make-string", &args, 2)?;
    let count_raw = expect_int(&args[0])?;
    if count_raw < 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), args[0]],
        ));
    }
    let count = count_raw as usize;

    let ch = match &args[1] {
        Value::Int(c) => {
            if *c < 0 {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), args[1]],
                ));
            }
            match char::from_u32(*c as u32) {
                Some(ch) => ch,
                None => {
                    if let Some(encoded) = encode_nonunicode_char_for_storage(*c as u32) {
                        return Ok(Value::string(encoded.repeat(count)));
                    }
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("characterp"), args[1]],
                    ));
                }
            }
        }
        Value::Char(c) => *c,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), *other],
            ))
        }
    };
    Ok(Value::string(
        std::iter::repeat_n(ch, count).collect::<String>(),
    ))
}

pub(crate) fn builtin_string(args: Vec<Value>) -> EvalResult {
    let mut result = String::new();
    for arg in args {
        match arg {
            Value::Char(c) => result.push(c),
            Value::Int(code) => {
                if code < 0 {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("characterp"), Value::Int(code)],
                    ));
                }
                if let Some(ch) = char::from_u32(code as u32) {
                    result.push(ch);
                } else if let Some(encoded) = encode_nonunicode_char_for_storage(code as u32) {
                    result.push_str(&encoded);
                } else {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("characterp"), Value::Int(code)],
                    ));
                }
            }
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), other],
                ));
            }
        }
    }
    Ok(Value::string(result))
}

/// `(unibyte-string &rest BYTES)` -> unibyte storage string.
pub(crate) fn builtin_unibyte_string(args: Vec<Value>) -> EvalResult {
    let mut bytes = Vec::with_capacity(args.len());
    for arg in args {
        let n = match arg {
            Value::Int(v) => v,
            Value::Char(c) => c as i64,
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("integerp"), other],
                ))
            }
        };
        if !(0..=255).contains(&n) {
            return Err(signal(
                "args-out-of-range",
                vec![Value::Int(n), Value::Int(0), Value::Int(255)],
            ));
        }
        bytes.push(n as u8);
    }
    Ok(Value::string(bytes_to_unibyte_storage_string(&bytes)))
}

pub(crate) fn builtin_byte_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("byte-to-string", &args, 1)?;
    let byte = expect_fixnum(&args[0])?;
    if !(0..=255).contains(&byte) {
        return Err(signal("error", vec![Value::string("Invalid byte")]));
    }
    Ok(Value::string(bytes_to_unibyte_storage_string(
        &[byte as u8],
    )))
}

pub(crate) fn builtin_bitmap_spec_p(args: Vec<Value>) -> EvalResult {
    expect_args("bitmap-spec-p", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_clear_face_cache(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-face-cache", &args, 1)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_clear_buffer_auto_save_failure(args: Vec<Value>) -> EvalResult {
    expect_args("clear-buffer-auto-save-failure", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_string_to_list(args: Vec<Value>) -> EvalResult {
    expect_args("string-to-list", &args, 1)?;
    let s = expect_string(&args[0])?;
    let chars: Vec<Value> = decode_storage_char_codes(&s)
        .into_iter()
        .map(|cp| Value::Int(cp as i64))
        .collect();
    Ok(Value::list(chars))
}

pub(crate) fn builtin_string_width(args: Vec<Value>) -> EvalResult {
    expect_args("string-width", &args, 1)?;
    let s = expect_string(&args[0])?;
    Ok(Value::Int(storage_string_display_width(&s) as i64))
}
