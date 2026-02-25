use super::*;

// ===========================================================================
// Misc
// ===========================================================================

pub(crate) fn builtin_identity(args: Vec<Value>) -> EvalResult {
    expect_args("identity", &args, 1)?;
    Ok(args[0])
}

pub(crate) fn builtin_purecopy(args: Vec<Value>) -> EvalResult {
    expect_args("purecopy", &args, 1)?;
    Ok(args[0])
}

pub(crate) fn builtin_prefix_numeric_value(args: Vec<Value>) -> EvalResult {
    expect_args("prefix-numeric-value", &args, 1)?;
    let numeric = match &args[0] {
        Value::Nil => 1,
        Value::Symbol(id) if resolve_sym(*id) == "-" => -1,
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        Value::Cons(cell) => read_cons(*cell).car.as_int().unwrap_or(1),
        _ => 1,
    };
    Ok(Value::Int(numeric))
}

pub(crate) fn builtin_ignore(_args: Vec<Value>) -> EvalResult {
    Ok(Value::Nil)
}

pub(crate) fn builtin_always(_args: Vec<Value>) -> EvalResult {
    Ok(Value::True)
}

pub(crate) fn builtin_message(args: Vec<Value>) -> EvalResult {
    expect_min_args("message", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        expect_strict_string(&args[0])?
    } else {
        // Use format
        match builtin_format(args.clone())? {
            Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_box(args: Vec<Value>) -> EvalResult {
    expect_min_args("message-box", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        expect_strict_string(&args[0])?
    } else {
        match builtin_format_wrapper_strict(args.clone())? {
            Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_or_box(args: Vec<Value>) -> EvalResult {
    expect_min_args("message-or-box", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        expect_strict_string(&args[0])?
    } else {
        match builtin_format_wrapper_strict(args.clone())? {
            Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("message", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        expect_strict_string(&args[0])?
    } else {
        match builtin_format_eval(eval, args.clone())? {
            Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_box_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("message-box", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        expect_strict_string(&args[0])?
    } else {
        match builtin_format_wrapper_strict_eval(eval, args.clone())? {
            Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_message_or_box_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("message-or-box", &args, 1)?;
    if args.len() == 1 && args[0].is_nil() {
        return Ok(Value::Nil);
    }
    let msg = if args.len() == 1 {
        expect_strict_string(&args[0])?
    } else {
        match builtin_format_wrapper_strict_eval(eval, args.clone())? {
            Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
            _ => String::new(),
        }
    };
    eprintln!("{}", msg);
    Ok(Value::string(msg))
}

pub(crate) fn builtin_current_message(args: Vec<Value>) -> EvalResult {
    expect_args("current-message", &args, 0)?;
    // Batch mode keeps message display side effects out-of-band.
    Ok(Value::Nil)
}

pub(crate) fn builtin_daemonp(args: Vec<Value>) -> EvalResult {
    expect_args("daemonp", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_json_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("json-available-p", &args, 0)?;
    Ok(Value::True)
}

pub(crate) fn builtin_daemon_initialized(args: Vec<Value>) -> EvalResult {
    expect_args("daemon-initialized", &args, 0)?;
    Err(signal(
        "error",
        vec![Value::string(
            "This function can only be called if emacs is run as a daemon",
        )],
    ))
}

pub(crate) fn builtin_documentation_stringp(args: Vec<Value>) -> EvalResult {
    expect_args("documentation-stringp", &args, 1)?;
    Ok(Value::bool(matches!(
        args[0],
        Value::Str(_) | Value::Int(_)
    )))
}

pub(crate) fn builtin_flush_standard_output(args: Vec<Value>) -> EvalResult {
    expect_args("flush-standard-output", &args, 0)?;
    Ok(Value::Nil)
}

pub(crate) fn builtin_force_mode_line_update(args: Vec<Value>) -> EvalResult {
    expect_max_args("force-mode-line-update", &args, 1)?;
    Ok(args.first().cloned().unwrap_or(Value::Nil))
}


pub(crate) fn builtin_get_internal_run_time(args: Vec<Value>) -> EvalResult {
    expect_args("get-internal-run-time", &args, 0)?;
    use std::time::{SystemTime, UNIX_EPOCH};
    let dur = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let secs = dur.as_secs() as i64;
    let usecs = dur.subsec_micros() as i64;
    Ok(Value::list(vec![
        Value::Int(secs >> 16),
        Value::Int(secs & 0xFFFF),
        Value::Int(usecs),
        Value::Int(0),
    ]))
}

pub(crate) fn builtin_invocation_directory(args: Vec<Value>) -> EvalResult {
    expect_args("invocation-directory", &args, 0)?;
    let mut dir = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|parent| parent.to_path_buf()))
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|| "/".to_string());
    if !dir.ends_with('/') {
        dir.push('/');
    }
    Ok(Value::string(dir))
}

pub(crate) fn builtin_invocation_name(args: Vec<Value>) -> EvalResult {
    expect_args("invocation-name", &args, 0)?;
    let name = std::env::current_exe()
        .ok()
        .and_then(|p| {
            p.file_name()
                .map(|name| name.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| "emacs".to_string());
    Ok(Value::string(name))
}

pub(crate) fn builtin_error(args: Vec<Value>) -> EvalResult {
    expect_min_args("error", &args, 1)?;
    let msg = match builtin_format(args)? {
        Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
        _ => "error".to_string(),
    };
    Err(signal("error", vec![Value::string(msg)]))
}

pub(crate) fn builtin_error_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("error", &args, 1)?;
    let msg = match builtin_format_eval(eval, args)? {
        Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
        _ => "error".to_string(),
    };
    Err(signal("error", vec![Value::string(msg)]))
}

pub(crate) fn builtin_user_error(args: Vec<Value>) -> EvalResult {
    expect_min_args("user-error", &args, 1)?;
    let msg = match builtin_format(args)? {
        Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
        _ => "user-error".to_string(),
    };
    Err(signal("user-error", vec![Value::string(msg)]))
}

pub(crate) fn builtin_user_error_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("user-error", &args, 1)?;
    let msg = match builtin_format_eval(eval, args)? {
        Value::Str(id) => with_heap(|h| h.get_string(id).clone()),
        _ => "user-error".to_string(),
    };
    Err(signal("user-error", vec![Value::string(msg)]))
}

fn format_human_readable_number(value: f64, decimals: usize) -> String {
    let mut text = format!("{value:.decimals$}");
    if text.contains('.') {
        while text.ends_with('0') {
            text.pop();
        }
        if text.ends_with('.') {
            text.pop();
        }
    }
    text
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum HumanReadableSizeFlavor {
    Binary,
    Si,
    Iec,
}

fn parse_human_readable_size_flavor(value: Option<&Value>) -> HumanReadableSizeFlavor {
    match value {
        None | Some(Value::Nil) => HumanReadableSizeFlavor::Binary,
        Some(Value::Symbol(sym)) if resolve_sym(*sym) == "si" => HumanReadableSizeFlavor::Si,
        Some(Value::Symbol(sym)) if resolve_sym(*sym) == "iec" => HumanReadableSizeFlavor::Iec,
        Some(_) => HumanReadableSizeFlavor::Si,
    }
}

fn human_readable_size_base(flavor: HumanReadableSizeFlavor) -> f64 {
    match flavor {
        HumanReadableSizeFlavor::Si => 1000.0,
        HumanReadableSizeFlavor::Binary | HumanReadableSizeFlavor::Iec => 1024.0,
    }
}

fn human_readable_size_prefixes(flavor: HumanReadableSizeFlavor) -> &'static [&'static str] {
    const SI_PREFIXES: &[&str] = &["", "k", "M", "G", "T", "P", "E"];
    const IEC_PREFIXES: &[&str] = &["", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei"];

    match flavor {
        HumanReadableSizeFlavor::Binary | HumanReadableSizeFlavor::Si => SI_PREFIXES,
        HumanReadableSizeFlavor::Iec => IEC_PREFIXES,
    }
}

fn coerce_file_size_human_readable_space(value: Option<&Value>) -> String {
    match value {
        None | Some(Value::Nil) => String::new(),
        Some(Value::Str(id)) => with_heap(|h| h.get_string(*id).clone()),
        Some(other) => super::print::print_value(other),
    }
}

fn coerce_file_size_human_readable_unit(
    value: Option<&Value>,
    default_unit: &str,
) -> Result<String, Flow> {
    match value {
        None | Some(Value::Nil) => Ok(default_unit.to_string()),
        Some(other) => match builtin_concat(vec![*other])? {
            Value::Str(id) => Ok(with_heap(|h| h.get_string(id).clone())),
            _ => unreachable!("concat should produce a string"),
        },
    }
}

fn format_file_size_human_readable_with_style(
    size: f64,
    flavor: HumanReadableSizeFlavor,
    space: &str,
    unit: &str,
) -> String {
    let prefixes = human_readable_size_prefixes(flavor);
    let base = human_readable_size_base(flavor);

    let (rendered, prefix) = if size.is_sign_negative() {
        let decimals = if size.fract() == 0.0 { 0 } else { 1 };
        (format_human_readable_number(size, decimals), prefixes[0])
    } else {
        let mut scaled = size;
        let mut prefix_idx = 0usize;
        while scaled >= base && prefix_idx < prefixes.len() - 1 {
            scaled /= base;
            prefix_idx += 1;
        }

        let decimals = if scaled < 10.0 && scaled.fract() != 0.0 {
            1
        } else {
            0
        };
        (
            format_human_readable_number(scaled, decimals),
            prefixes[prefix_idx],
        )
    };

    let suffix = format!("{prefix}{unit}");
    if suffix.is_empty() {
        rendered
    } else if space.is_empty() {
        format!("{rendered}{suffix}")
    } else {
        format!("{rendered}{space}{suffix}")
    }
}

pub(crate) fn builtin_file_size_human_readable(args: Vec<Value>) -> EvalResult {
    expect_min_args("file-size-human-readable", &args, 1)?;
    expect_max_args("file-size-human-readable", &args, 4)?;
    let size = expect_number_or_marker_f64(&args[0])?;
    let flavor = parse_human_readable_size_flavor(args.get(1));
    let default_unit = if flavor == HumanReadableSizeFlavor::Iec {
        "B"
    } else {
        ""
    };
    let space = coerce_file_size_human_readable_space(args.get(2));
    let unit = coerce_file_size_human_readable_unit(args.get(3), default_unit)?;
    Ok(Value::string(format_file_size_human_readable_with_style(
        size, flavor, &space, &unit,
    )))
}

fn format_file_size_human_readable_iec(size: f64) -> String {
    format_file_size_human_readable_with_style(size, HumanReadableSizeFlavor::Iec, " ", "B")
}

pub(crate) fn builtin_file_size_human_readable_iec(args: Vec<Value>) -> EvalResult {
    expect_args("file-size-human-readable-iec", &args, 1)?;
    let size = expect_number_or_marker_f64(&args[0])?;
    Ok(Value::string(format_file_size_human_readable_iec(size)))
}

pub(crate) fn builtin_secure_hash_algorithms(args: Vec<Value>) -> EvalResult {
    expect_args("secure-hash-algorithms", &args, 0)?;
    Ok(Value::list(vec![
        Value::symbol("md5"),
        Value::symbol("sha1"),
        Value::symbol("sha224"),
        Value::symbol("sha256"),
        Value::symbol("sha384"),
        Value::symbol("sha512"),
    ]))
}

pub(crate) fn builtin_symbol_name(args: Vec<Value>) -> EvalResult {
    expect_args("symbol-name", &args, 1)?;
    match args[0].as_symbol_name() {
        Some(name) => Ok(Value::string(name)),
        None => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0]],
        )),
    }
}

pub(crate) fn builtin_make_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("make-symbol", &args, 1)?;
    let name = expect_string(&args[0])?;
    Ok(Value::Symbol(intern(&name)))
}
