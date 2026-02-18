//! Documentation and help support builtins.
//!
//! Provides:
//! - `documentation` — retrieve docstring from a function
//! - `describe-function` — return description string for a function
//! - `describe-variable` — return description string for a variable
//! - `documentation-property` — retrieve documentation property
//! - `Snarf-documentation` — internal DOC file loader compatibility shim
//! - `substitute-command-keys` — process special sequences in docstrings
//! - `help-function-arglist` — return argument list of a function

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers
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

fn expect_min_max_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// `(documentation FUNCTION &optional RAW)` -- return the docstring of FUNCTION.
///
/// Looks up FUNCTION in the obarray's function cell. If the function cell
/// holds a `Lambda` (or `Macro`) with a docstring, returns it as a string.
/// Otherwise returns nil.  The optional RAW argument is accepted but ignored
/// (in real Emacs it controls whether `substitute-command-keys` is applied).
pub(crate) fn builtin_documentation(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("documentation", &args, 1, 2)?;

    // For symbols, Emacs consults the `function-documentation` property first.
    // This can produce docs even when the function cell is non-callable.
    if let Some(name) = args[0].as_symbol_name() {
        let name = name.to_string();
        if let Some(prop) = eval
            .obarray
            .get_property(&name, "function-documentation")
            .cloned()
        {
            return eval_documentation_property_value(eval, prop);
        }

        let mut func_val =
            super::builtins::builtin_symbol_function(eval, vec![Value::symbol(name.clone())])?;
        if let Some(alias_name) = func_val.as_symbol_name() {
            let indirect =
                super::builtins::builtin_indirect_function(eval, vec![Value::symbol(alias_name)])?;
            if !indirect.is_nil() {
                func_val = indirect;
            }
        }
        if func_val.is_nil() {
            return Err(signal("void-function", vec![Value::symbol(name)]));
        }

        return function_doc_or_error(func_val);
    }

    function_doc_or_error(args[0].clone())
}

fn function_doc_or_error(func_val: Value) -> EvalResult {
    if let Some(result) = quoted_lambda_documentation(&func_val) {
        return result;
    }
    if let Some(result) = quoted_macro_invalid_designator(&func_val) {
        return result;
    }

    match func_val {
        Value::Lambda(data) | Value::Macro(data) => match &data.docstring {
            Some(doc) => Ok(Value::string(doc.clone())),
            None => Ok(Value::Nil),
        },
        Value::Subr(_) => Ok(Value::string("Built-in function.")),
        Value::Str(_) | Value::Vector(_) => Ok(Value::string("Keyboard macro.")),
        Value::ByteCode(bytecode) => Ok(bytecode
            .docstring
            .as_ref()
            .map_or(Value::Nil, |doc| Value::string(doc.clone()))),
        other => Err(signal("invalid-function", vec![other])),
    }
}

fn quoted_lambda_documentation(function: &Value) -> Option<EvalResult> {
    let Value::Cons(cell) = function else {
        return None;
    };

    let pair = cell.lock().ok()?;
    if pair.car.as_symbol_name() != Some("lambda") {
        return None;
    }

    let mut tail = pair.cdr.clone();
    drop(pair);

    let Value::Cons(param_cell) = tail else {
        return Some(Err(signal("invalid-function", vec![function.clone()])));
    };
    let params_and_body = param_cell.lock().ok()?;
    tail = params_and_body.cdr.clone();
    drop(params_and_body);

    match tail {
        Value::Nil => Some(Ok(Value::Nil)),
        Value::Cons(body_cell) => {
            let body = body_cell.lock().ok()?;
            if let Some(doc) = body.car.as_str() {
                Some(Ok(Value::string(doc)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        other => Some(Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), other],
        ))),
    }
}

fn quoted_macro_invalid_designator(function: &Value) -> Option<EvalResult> {
    let Value::Cons(cell) = function else {
        return None;
    };

    let pair = cell.lock().ok()?;
    if pair.car.as_symbol_name() != Some("macro") {
        return None;
    }

    let payload = pair.cdr.clone();
    if payload.is_nil() {
        return Some(Err(signal("void-function", vec![Value::Nil])));
    }

    Some(Err(signal("invalid-function", vec![payload])))
}

fn eval_documentation_property_value(
    eval: &mut super::eval::Evaluator,
    value: Value,
) -> EvalResult {
    if let Some(text) = value.as_str() {
        return Ok(Value::string(text));
    }

    // Integer doc offsets require DOC-file lookup; return nil when unresolved.
    if matches!(value, Value::Int(_)) {
        return Ok(Value::Nil);
    }

    let expr = super::eval::value_to_expr_pub(&value);
    eval.eval(&expr)
}

fn startup_variable_doc_offset_symbol(sym: &str, prop: &str, value: &Value) -> bool {
    prop == "variable-documentation"
        && matches!(value, Value::Int(_))
        && matches!(
            sym,
            "load-path"
                | "load-history"
                | "features"
                | "debug-on-error"
                | "default-directory"
                | "load-file-name"
                | "noninteractive"
                | "inhibit-quit"
                | "print-length"
                | "print-level"
                | "standard-output"
                | "buffer-read-only"
                | "kill-ring"
                | "kill-ring-yank-pointer"
                | "last-command"
                | "lexical-binding"
                | "load-prefer-newer"
        )
}

/// `(describe-function FUNCTION)` -- return a short description string.
///
/// This is a simplified stub that returns a type description of the function.
/// In real Emacs, `describe-function` opens a *Help* buffer with detailed
/// information.
pub(crate) fn builtin_describe_function(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("describe-function", &args, 1)?;

    let name = match args[0].as_symbol_name() {
        Some(n) => n.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            ));
        }
    };

    // Reuse symbol-function resolution so builtins and wrapper-backed callables
    // are visible here even when not explicitly interned in the function cell.
    let func_val = super::builtins::builtin_symbol_function(eval, vec![Value::symbol(name.clone())])?;
    if func_val.is_nil() {
        return Err(signal("void-function", vec![Value::symbol(&name)]));
    }

    if let Some(alias_name) = func_val.as_symbol_name() {
        if alias_name != name {
            let indirect =
                super::builtins::builtin_indirect_function(eval, vec![Value::symbol(alias_name)]);
            let text = match indirect {
                Ok(value) if !value.is_nil() => {
                    format!("{name} is an alias for `{alias_name}`.")
                }
                _ => format!(
                    "{name} is an alias for `{alias_name}`, which is not known to be defined."
                ),
            };
            return Ok(Value::string(text));
        }
    }

    let description = describe_function_kind(&func_val);

    Ok(Value::string(description))
}

fn describe_function_kind(function: &Value) -> &'static str {
    match function {
        Value::Lambda(_) => "Lisp function",
        Value::Macro(_) => "Lisp macro",
        Value::Subr(name) => {
            if super::subr_info::is_special_form(name) {
                "Special-form"
            } else {
                "Built-in function"
            }
        }
        Value::ByteCode(_) => "Compiled Lisp function",
        Value::Str(_) | Value::Vector(_) => "Keyboard macro",
        Value::Cons(cell) => {
            if super::autoload::is_autoload_value(function) {
                if let Some(items) = list_to_vec(function) {
                    if matches!(items.get(4).and_then(Value::as_symbol_name), Some("macro")) {
                        return "Lisp macro";
                    }
                }
                return "Lisp function";
            }

            let pair = match cell.lock() {
                Ok(pair) => pair,
                Err(_) => return "Lisp function",
            };
            match pair.car.as_symbol_name() {
                Some("macro") => "Lisp macro",
                Some("lambda") => "Lisp function",
                _ => "Lisp function",
            }
        }
        _ => "Lisp function",
    }
}

/// `(describe-variable VARIABLE)` -- return the documentation string for a
/// variable.
///
/// Looks up the `:variable-documentation` property on the symbol's plist.
/// Returns the documentation string if found, or a generic description if the
/// variable is bound but has no documentation, or signals `void-variable` if
/// the variable is not bound.
pub(crate) fn builtin_describe_variable(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("describe-variable", &args, 1, 3)?;

    let name = match args[0].as_symbol_name() {
        Some(n) => n.to_string(),
        None => {
            return Err(signal(
                "user-error",
                vec![Value::string("You didn't specify a variable")],
            ));
        }
    };

    // Check for :variable-documentation property.
    if let Some(doc_val) = eval
        .obarray
        .get_property(&name, "variable-documentation")
        .cloned()
    {
        match doc_val {
            Value::Str(_) => return Ok(doc_val),
            Value::Int(_) => {
                return Ok(Value::string(format!(
                    "{name} is a variable defined in `C source code`."
                )));
            }
            Value::Symbol(sym) => {
                if !eval.obarray.boundp(&sym) {
                    return Err(signal("void-variable", vec![Value::symbol(sym)]));
                }
                let sym_value = eval.obarray.symbol_value(&sym).cloned().unwrap_or(Value::Nil);
                return match sym_value {
                    Value::Str(_) | Value::Int(_) => Ok(describe_variable_value_or_void(eval, &name)),
                    other => Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("char-or-string-p"), other],
                    )),
                };
            }
            Value::True => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), Value::True],
                ));
            }
            _ => return Ok(describe_variable_value_or_void(eval, &name)),
        }
    }

    Ok(describe_variable_value_or_void(eval, &name))
}

fn describe_variable_value_or_void(eval: &super::eval::Evaluator, name: &str) -> Value {
    if eval.obarray.boundp(name) {
        let value = eval
            .obarray
            .symbol_value(name)
            .cloned()
            .unwrap_or(Value::symbol("void"));
        let rendered = super::print::print_value(&value);
        Value::string(format!("{name}\u{2019}s value is {rendered}\n"))
    } else {
        Value::string(format!("{name} is void as a variable.\n"))
    }
}

/// `(documentation-property SYMBOL PROP &optional RAW)` -- return the
/// documentation property PROP of SYMBOL.
///
/// Evaluator-aware implementation:
/// - validates SYMBOL as a symbol designator (`symbolp`)
/// - returns nil when PROP is not a symbol (matching Emacs `get`-like behavior)
/// - unresolved integer doc offsets return nil
/// - non-integer values are evaluated as Lisp and returned
/// - accepts RAW but currently ignores it
pub(crate) fn builtin_documentation_property_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("documentation-property", &args, 2, 3)?;

    let sym = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;

    let Some(prop) = args[1].as_symbol_name() else {
        return Ok(Value::Nil);
    };

    match eval.obarray.get_property(sym, prop).cloned() {
        Some(value) if startup_variable_doc_offset_symbol(sym, prop, &value) => Ok(
            Value::string(format!("{sym} is a variable defined in `C source code`.")),
        ),
        Some(value) => eval_documentation_property_value(eval, value),
        _ => Ok(Value::Nil),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(documentation-property SYMBOL PROP &optional RAW)` -- return the
/// documentation property PROP of SYMBOL.
///
/// Stub implementation: always returns nil.  In real Emacs this reads
/// properties like `variable-documentation` and `function-documentation`
/// from the symbol's plist, potentially loading from the DOC file.
pub(crate) fn builtin_documentation_property(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("documentation-property", &args, 2, 3)?;
    // Validate that the first argument is a symbol.
    if args[0].as_symbol_name().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// `(Snarf-documentation FILENAME)` -- load documentation strings from
/// the internal DOC file.
///
/// Compatibility implementation: accepts the canonical `"DOC"` token and
/// preserves observed GNU Emacs error classes for invalid and missing paths.
/// It does not load or parse an on-disk DOC table yet.
fn snarf_doc_path_invalid(filename: &str) -> bool {
    if filename.is_empty() {
        return true;
    }

    let mut segments = filename.split('/').filter(|segment| !segment.is_empty()).peekable();
    if segments.peek().is_none() {
        return true;
    }

    segments.all(|segment| segment == "." || segment == "..")
}

pub(crate) fn builtin_snarf_documentation(args: Vec<Value>) -> EvalResult {
    expect_args("Snarf-documentation", &args, 1)?;
    let filename = match args[0].as_str() {
        Some(name) => name,
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), args[0].clone()],
            ));
        }
    };

    // In batch compatibility mode, allow the canonical DOC token while
    // preserving observable error classes for invalid/missing names.
    if filename == "DOC" {
        return Ok(Value::Nil);
    }

    if filename.starts_with("DOC/") {
        return Err(signal(
            "file-error",
            vec![
                Value::string("Read error"),
                Value::string(format!("/usr/share/emacs/etc/{filename}")),
            ],
        ));
    }

    if snarf_doc_path_invalid(filename) {
        return Err(signal(
            "error",
            vec![Value::string("DOC file invalid at position 0")],
        ));
    }

    Err(signal(
        "file-missing",
        vec![
            Value::string("Opening doc string file"),
            Value::string("No such file or directory"),
            Value::string(format!("/usr/share/emacs/etc/{filename}")),
        ],
    ))
}

/// `(substitute-command-keys STRING)` -- process special documentation
/// sequences in STRING.
///
/// Recognized sequences:
/// - `\\[COMMAND]` — replaced with the key binding for COMMAND (stripped here)
/// - `\\{KEYMAP}` — replaced with a description of the keymap (stripped here)
/// - `\\<KEYMAP>` — sets the keymap for subsequent `\\[...]` (stripped here)
/// - `\\=` — quote the next character (prevents interpretation)
///
/// This implementation strips the special sequences, returning the plain
/// text content.  A full implementation would resolve key bindings and
/// format keymap descriptions.
pub(crate) fn builtin_substitute_command_keys(args: Vec<Value>) -> EvalResult {
    expect_args("substitute-command-keys", &args, 1)?;

    let input = match args[0].as_str() {
        Some(s) => s,
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), args[0].clone()],
            ));
        }
    };

    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.peek() {
                Some('[') => {
                    // \\[COMMAND] — skip until closing ']'
                    chars.next(); // consume '['
                    let mut command = String::new();
                    for c in chars.by_ref() {
                        if c == ']' {
                            break;
                        }
                        command.push(c);
                    }
                    // Replace with a placeholder showing the command name.
                    result.push_str(&format!("M-x {}", command));
                }
                Some('{') => {
                    // \\{KEYMAP} — skip until closing '}'
                    chars.next(); // consume '{'
                    for c in chars.by_ref() {
                        if c == '}' {
                            break;
                        }
                    }
                    // Omit keymap description entirely.
                }
                Some('<') => {
                    // \\<KEYMAP> — skip until closing '>'
                    chars.next(); // consume '<'
                    for c in chars.by_ref() {
                        if c == '>' {
                            break;
                        }
                    }
                    // Silently consumed (sets keymap context).
                }
                Some('=') => {
                    // \\= — quote next character literally.
                    chars.next(); // consume '='
                    if let Some(next) = chars.next() {
                        result.push(next);
                    }
                }
                Some('\\') => {
                    // Literal backslash (\\\\).
                    chars.next();
                    result.push('\\');
                }
                _ => {
                    // Not a recognized sequence; keep the backslash.
                    result.push(ch);
                }
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::string(result))
}

/// `(help-function-arglist FUNCTION &optional PRESERVE-NAMES)` -- return
/// the argument list of FUNCTION.
///
/// This returns oracle-compatible arglists for core thread/mutex/condition
/// primitives and for lambda-like objects. For unresolved shapes, Emacs
/// returns `t`, which we mirror here.
pub(crate) fn builtin_help_function_arglist(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("help-function-arglist", &args, 1, 2)?;
    let preserve_names = args.get(1).is_some_and(Value::is_truthy);

    if let Some(result) = help_arglist_from_quoted_designator(&args[0]) {
        return result;
    }

    if let Some(arglist) = help_arglist_from_value(&args[0], preserve_names) {
        return Ok(arglist);
    }

    Ok(Value::True)
}

pub(crate) fn builtin_help_function_arglist_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("help-function-arglist", &args, 1, 2)?;
    let preserve_names = args.get(1).is_some_and(Value::is_truthy);

    if let Some(name) = args[0].as_symbol_name() {
        let resolved =
            super::builtins::builtin_indirect_function(eval, vec![Value::symbol(name)])?;
        if let Some(result) = help_arglist_from_quoted_designator(&resolved) {
            return result;
        }
        if let Some(arglist) = help_arglist_from_value(&resolved, preserve_names) {
            return Ok(arglist);
        }
        if let Value::Subr(subr_name) = &resolved {
            if let Some(arglist) = help_arglist_from_subr_arity(subr_name) {
                return Ok(arglist);
            }
        }
        if super::autoload::is_autoload_value(&resolved) {
            return Ok(Value::string(
                "[Arg list not available until function definition is loaded.]",
            ));
        }
    }

    if let Some(result) = help_arglist_from_quoted_designator(&args[0]) {
        return result;
    }

    if let Some(arglist) = help_arglist_from_value(&args[0], preserve_names) {
        return Ok(arglist);
    }

    Ok(Value::True)
}

fn help_arglist_from_quoted_designator(function: &Value) -> Option<EvalResult> {
    let Value::Cons(cell) = function else {
        return None;
    };

    let pair = cell.lock().ok()?;
    let Some(head) = pair.car.as_symbol_name() else {
        return None;
    };

    match head {
        "macro" => Some(Ok(Value::True)),
        "lambda" => match pair.cdr.clone() {
            Value::Nil => Some(Ok(Value::Nil)),
            Value::Cons(arg_cell) => {
                let args = arg_cell.lock().ok()?;
                Some(Ok(args.car.clone()))
            }
            other => Some(Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other],
            ))),
        },
        _ => None,
    }
}

fn help_arglist(required: &[&str], optional: &[&str], rest: Option<&str>) -> Value {
    if required.is_empty() && optional.is_empty() && rest.is_none() {
        return Value::Nil;
    }

    let mut out = Vec::new();
    for name in required {
        out.push(Value::symbol(*name));
    }
    if !optional.is_empty() {
        out.push(Value::symbol("&optional"));
        for name in optional {
            out.push(Value::symbol(*name));
        }
    }
    if let Some(name) = rest {
        out.push(Value::symbol("&rest"));
        out.push(Value::symbol(name));
    }
    Value::list(out)
}

fn help_arglist_from_lambda_params(params: &LambdaParams) -> Value {
    let required: Vec<&str> = params.required.iter().map(String::as_str).collect();
    let optional: Vec<&str> = params.optional.iter().map(String::as_str).collect();
    let rest = params.rest.as_deref();
    help_arglist(&required, &optional, rest)
}

fn help_arglist_from_subr_name(name: &str, preserve_names: bool) -> Option<Value> {
    if name == "car" {
        return Some(if preserve_names {
            help_arglist(&["list"], &[], None)
        } else {
            help_arglist(&["arg1"], &[], None)
        });
    }

    if name == "cdr" {
        return Some(if preserve_names {
            help_arglist(&["list"], &[], None)
        } else {
            help_arglist(&["arg1"], &[], None)
        });
    }

    if name == "cons" {
        return Some(if preserve_names {
            help_arglist(&["car", "cdr"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if name == "list" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("objects"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "if" {
        return Some(help_arglist(&["arg1", "arg2"], &[], Some("rest")));
    }

    if name == "documentation" {
        return Some(if preserve_names {
            help_arglist(&["function"], &["raw"], None)
        } else {
            help_arglist(&["arg1"], &["arg2"], None)
        });
    }

    if name == "eq" {
        return Some(if preserve_names {
            help_arglist(&["obj1", "obj2"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if name == "min" || name == "max" {
        return Some(if preserve_names {
            help_arglist(&["number-or-marker"], &[], Some("numbers-or-markers"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if name == "message" {
        return Some(if preserve_names {
            help_arglist(&["format-string"], &[], Some("args"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if name == "concat" || name == "append" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("sequences"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "nconc" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("lists"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "vector" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("objects"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "format" {
        return Some(if preserve_names {
            help_arglist(&["string"], &[], Some("objects"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if name == "apply" {
        return Some(if preserve_names {
            help_arglist(&["function"], &[], Some("arguments"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if preserve_names && name == "assq" {
        return Some(help_arglist(&["key", "alist"], &[], None));
    }

    if preserve_names && name == "assoc" {
        return Some(help_arglist(&["key", "alist"], &["testfn"], None));
    }

    if preserve_names && (name == "memq" || name == "member") {
        return Some(help_arglist(&["elt", "list"], &[], None));
    }

    if name == "equal" {
        return Some(if preserve_names {
            help_arglist(&["o1", "o2"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if preserve_names && name == "string-match" {
        return Some(help_arglist(
            &["regexp", "string"],
            &["start", "inhibit-modify"],
            None,
        ));
    }

    if name == "substring" {
        return Some(if preserve_names {
            help_arglist(&["string"], &["from", "to"], None)
        } else {
            help_arglist(&["arg1"], &["arg2", "arg3"], None)
        });
    }

    if name == "aref" {
        return Some(if preserve_names {
            help_arglist(&["array", "idx"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if name == "aset" {
        return Some(if preserve_names {
            help_arglist(&["array", "idx", "newelt"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2", "arg3"], &[], None)
        });
    }

    if name == "make-string" {
        return Some(if preserve_names {
            help_arglist(&["length", "init"], &["multibyte"], None)
        } else {
            help_arglist(&["arg1", "arg2"], &["arg3"], None)
        });
    }

    if preserve_names && name == "read-from-string" {
        return Some(help_arglist(&["string"], &["start", "end"], None));
    }

    if name == "funcall" {
        return Some(if preserve_names {
            help_arglist(&["function"], &[], Some("arguments"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if preserve_names && name == "mapcar" {
        return Some(Value::list(vec![
            Value::symbol("function"),
            Value::symbol("sequence"),
        ]));
    }

    if name == "symbol-function" || name == "fboundp" {
        return Some(if preserve_names {
            help_arglist(&["symbol"], &[], None)
        } else {
            help_arglist(&["arg1"], &[], None)
        });
    }

    if name == "substitute-command-keys" {
        return Some(if preserve_names {
            help_arglist(&["string"], &["no-face", "include-menus"], None)
        } else {
            help_arglist(&["arg1"], &["arg2", "arg3"], None)
        });
    }

    if name == "where-is-internal" {
        return Some(if preserve_names {
            help_arglist(
                &["definition"],
                &["keymap", "firstonly", "noindirect", "no-remap"],
                None,
            )
        } else {
            help_arglist(&["arg1"], &["arg2", "arg3", "arg4", "arg5"], None)
        });
    }

    if name == "read" {
        return Some(if preserve_names {
            help_arglist(&[], &["stream"], None)
        } else {
            help_arglist(&[], &["arg1"], None)
        });
    }

    if name == "-" {
        return Some(if preserve_names {
            help_arglist(&[], &["number-or-marker"], Some("more-numbers-or-markers"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    let (required, optional) = match name {
        "thread-join" => (
            if preserve_names {
                vec!["thread"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "thread-yield" => (vec![], vec![]),
        "defining-kbd-macro" => (
            if preserve_names {
                vec!["append"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["no-exec"]
            } else {
                vec!["arg2"]
            },
        ),
        "help-key-description" => (
            if preserve_names {
                vec!["key", "untranslated"]
            } else {
                vec!["arg1", "arg2"]
            },
            vec![],
        ),
        "recent-keys" => (
            vec![],
            if preserve_names {
                vec!["include-cmds"]
            } else {
                vec!["arg1"]
            },
        ),
        "event-apply-modifier" => (
            if preserve_names {
                vec!["event", "symbol", "lshiftby", "prefix"]
            } else {
                vec!["arg1", "arg2", "arg3", "arg4"]
            },
            vec![],
        ),
        "thread-name" => (
            if preserve_names {
                vec!["thread"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "thread-live-p" => (
            if preserve_names {
                vec!["thread"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "thread-signal" => (
            if preserve_names {
                vec!["thread", "error-symbol", "data"]
            } else {
                vec!["arg1", "arg2", "arg3"]
            },
            vec![],
        ),
        "thread-last-error" => (
            vec![],
            if preserve_names {
                vec!["cleanup"]
            } else {
                vec!["arg1"]
            },
        ),
        "make-thread" => (
            if preserve_names {
                vec!["function"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["name"]
            } else {
                vec!["arg2"]
            },
        ),
        "make-mutex" => (
            vec![],
            if preserve_names {
                vec!["name"]
            } else {
                vec!["arg1"]
            },
        ),
        "mutexp" => (
            if preserve_names {
                vec!["object"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "mutex-name" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "mutex-lock" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "mutex-unlock" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "make-condition-variable" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["name"]
            } else {
                vec!["arg2"]
            },
        ),
        "condition-variable-p" => (
            if preserve_names {
                vec!["object"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "condition-wait" => (
            if preserve_names {
                vec!["cond"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "condition-notify" => (
            if preserve_names {
                vec!["cond"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["all"]
            } else {
                vec!["arg2"]
            },
        ),
        "current-thread" | "all-threads" => (vec![], vec![]),
        _ => return None,
    };

    Some(help_arglist(&required, &optional, None))
}

fn help_arglist_from_subr_arity(name: &str) -> Option<Value> {
    let arity = super::subr_info::builtin_subr_arity(vec![Value::Subr(name.to_string())]).ok()?;
    let Value::Cons(cell) = arity else {
        return None;
    };
    let pair = cell.lock().ok()?;
    let min = pair.car.as_int()?;
    if min < 0 {
        return None;
    }
    let min = min as usize;

    let required: Vec<String> = (1..=min).map(|idx| format!("arg{idx}")).collect();

    if pair.cdr.as_symbol_name() == Some("unevalled") || pair.cdr.as_symbol_name() == Some("many") {
        let required_refs: Vec<&str> = required.iter().map(String::as_str).collect();
        return Some(help_arglist(&required_refs, &[], Some("rest")));
    }

    let max = pair.cdr.as_int()?;
    if max < min as i64 {
        return None;
    }
    let max = max as usize;
    let optional: Vec<String> = ((min + 1)..=max).map(|idx| format!("arg{idx}")).collect();
    let required_refs: Vec<&str> = required.iter().map(String::as_str).collect();
    let optional_refs: Vec<&str> = optional.iter().map(String::as_str).collect();
    Some(help_arglist(&required_refs, &optional_refs, None))
}

fn help_arglist_from_value(function: &Value, preserve_names: bool) -> Option<Value> {
    match function {
        Value::Subr(name) => help_arglist_from_subr_name(name, preserve_names),
        Value::Symbol(name) => help_arglist_from_subr_name(name, preserve_names),
        Value::Lambda(lambda) | Value::Macro(lambda) => {
            Some(help_arglist_from_lambda_params(&lambda.params))
        }
        Value::ByteCode(bytecode) => Some(help_arglist_from_lambda_params(&bytecode.params)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // =======================================================================
    // substitute-command-keys
    // =======================================================================

    #[test]
    fn substitute_plain_string() {
        let result = builtin_substitute_command_keys(vec![Value::string("hello world")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello world"));
    }

    #[test]
    fn substitute_command_key_binding() {
        let result =
            builtin_substitute_command_keys(vec![Value::string("Press \\[save-buffer] to save.")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        assert!(text.contains("save-buffer"));
        assert!(!text.contains("\\["));
        assert!(!text.contains(']'));
    }

    #[test]
    fn substitute_keymap_description() {
        let result =
            builtin_substitute_command_keys(vec![Value::string("Bindings:\\{foo-mode-map}done")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        // The keymap description is stripped entirely.
        assert_eq!(text, "Bindings:done");
    }

    #[test]
    fn substitute_keymap_context() {
        let result =
            builtin_substitute_command_keys(vec![Value::string("\\<foo-map>Press \\[bar] now")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        assert!(!text.contains("\\<"));
        assert!(!text.contains('>'));
        assert!(text.contains("bar"));
    }

    #[test]
    fn substitute_quote_escape() {
        let result = builtin_substitute_command_keys(vec![Value::string(
            "Use \\=\\[not-a-command] literally",
        )]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        // \\= quotes the next char, so \\[ is literal.
        assert!(text.contains("\\[not-a-command]"));
    }

    #[test]
    fn substitute_literal_backslash() {
        let result = builtin_substitute_command_keys(vec![Value::string("path\\\\name")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert_eq!(s.as_str(), Some("path\\name"));
    }

    #[test]
    fn substitute_wrong_type() {
        let result = builtin_substitute_command_keys(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn substitute_wrong_arity() {
        let result = builtin_substitute_command_keys(vec![]);
        assert!(result.is_err());
    }

    // =======================================================================
    // documentation-property (stub)
    // =======================================================================

    #[test]
    fn documentation_property_returns_nil() {
        let result = builtin_documentation_property(vec![
            Value::symbol("foo"),
            Value::symbol("variable-documentation"),
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_property_with_raw() {
        let result = builtin_documentation_property(vec![
            Value::symbol("foo"),
            Value::symbol("variable-documentation"),
            Value::True,
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_property_wrong_type() {
        let result = builtin_documentation_property(vec![
            Value::Int(42),
            Value::symbol("variable-documentation"),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn documentation_property_wrong_arity() {
        let result = builtin_documentation_property(vec![Value::symbol("foo")]);
        assert!(result.is_err());
    }

    // =======================================================================
    // Snarf-documentation runtime/error semantics
    // =======================================================================

    #[test]
    fn snarf_documentation_returns_nil() {
        let result = builtin_snarf_documentation(vec![Value::string("DOC")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn snarf_documentation_wrong_type() {
        let result = builtin_snarf_documentation(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn snarf_documentation_empty_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_parent_dir_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("../")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_single_dot_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string(".")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_root_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("/")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_doc_dir_path_file_error() {
        let result = builtin_snarf_documentation(vec![Value::string("DOC/")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-error"),
            other => panic!("expected file-error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_doc_subpath_file_error() {
        let result = builtin_snarf_documentation(vec![Value::string("DOC/a")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-error"),
            other => panic!("expected file-error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_missing_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("NO_SUCH_DOC_FILE")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-missing"),
            other => panic!("expected file-missing signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_missing_dir_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("NO_SUCH_DOC_DIR/")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-missing"),
            other => panic!("expected file-missing signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_wrong_arity() {
        let result = builtin_snarf_documentation(vec![]);
        assert!(result.is_err());
    }

    // =======================================================================
    // help-function-arglist
    // =======================================================================

    fn arglist_names(v: &Value) -> Vec<String> {
        list_to_vec(v)
            .expect("arglist must be list")
            .into_iter()
            .map(|item| {
                item.as_symbol_name()
                    .expect("arglist item must be symbol")
                    .to_string()
            })
            .collect()
    }

    #[test]
    fn help_function_arglist_returns_t_for_unknown() {
        let result = builtin_help_function_arglist(vec![Value::symbol("foo")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn help_function_arglist_thread_join_matches_oracle_shape() {
        let result = builtin_help_function_arglist(vec![Value::symbol("thread-join")]).unwrap();
        assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
    }

    #[test]
    fn help_function_arglist_thread_join_preserve_names() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("thread-join"), Value::True]).unwrap();
        assert_eq!(arglist_names(&result), vec!["thread".to_string()]);
    }

    #[test]
    fn help_function_arglist_sub_non_preserve_names_matches_oracle_shape() {
        let result = builtin_help_function_arglist(vec![Value::symbol("-")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec!["&rest".to_string(), "rest".to_string()]
        );
    }

    #[test]
    fn help_function_arglist_sub_preserve_names_matches_oracle_shape() {
        let result = builtin_help_function_arglist(vec![Value::symbol("-"), Value::True]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "&optional".to_string(),
                "number-or-marker".to_string(),
                "&rest".to_string(),
                "more-numbers-or-markers".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_event_apply_modifier_matches_oracle_shape() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("event-apply-modifier")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "arg1".to_string(),
                "arg2".to_string(),
                "arg3".to_string(),
                "arg4".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_event_apply_modifier_preserve_names() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("event-apply-modifier"), Value::True])
                .unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "event".to_string(),
                "symbol".to_string(),
                "lshiftby".to_string(),
                "prefix".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_condition_notify_optional_arg() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("condition-notify")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "arg1".to_string(),
                "&optional".to_string(),
                "arg2".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_lambda_params() {
        let result =
            builtin_help_function_arglist(vec![Value::Lambda(std::sync::Arc::new(LambdaData {
                params: LambdaParams {
                    required: vec!["x".to_string()],
                    optional: vec!["y".to_string()],
                    rest: Some("rest".to_string()),
                },
                body: vec![],
                env: None,
                docstring: None,
            }))])
            .unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "x".to_string(),
                "&optional".to_string(),
                "y".to_string(),
                "&rest".to_string(),
                "rest".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_quoted_lambda_params() {
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::symbol("x"),
        ]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec!["x".to_string(), "y".to_string()]
        );
    }

    #[test]
    fn help_function_arglist_quoted_lambda_symbol_params() {
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::symbol("x"),
            Value::symbol("x"),
        ]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("x"));
    }

    #[test]
    fn help_function_arglist_quoted_macro_returns_t() {
        let quoted = Value::list(vec![
            Value::symbol("macro"),
            Value::list(vec![Value::symbol("x")]),
            Value::symbol("x"),
        ]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn help_function_arglist_quoted_lambda_improper_tail_errors() {
        let quoted = Value::cons(Value::symbol("lambda"), Value::Int(1));
        let result = builtin_help_function_arglist(vec![quoted]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn help_function_arglist_wrong_arity() {
        let result = builtin_help_function_arglist(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn help_function_arglist_eval_builtin_car() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("car")]).unwrap();
        assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
    }

    #[test]
    fn help_function_arglist_eval_builtin_car_preserve_names() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("car"), Value::True],
        )
        .unwrap();
        assert_eq!(arglist_names(&result), vec!["list".to_string()]);
    }

    #[test]
    fn help_function_arglist_eval_special_form_if() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("if")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "arg1".to_string(),
                "arg2".to_string(),
                "&rest".to_string(),
                "rest".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_eval_runtime_alias_builtin() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-hfa-alias-builtin", Value::symbol("car"));

        let result = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("vm-hfa-alias-builtin")],
        )
        .unwrap();
        assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
    }

    #[test]
    fn help_function_arglist_eval_autoload_placeholder() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("describe-function")],
        )
        .unwrap();
        assert_eq!(
            result.as_str(),
            Some("[Arg list not available until function definition is loaded.]")
        );
    }

    #[test]
    fn help_function_arglist_eval_subr_arity_fallback_shapes() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let cdr =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("cdr")]).unwrap();
        assert_eq!(arglist_names(&cdr), vec!["arg1".to_string()]);

        let list =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("list")]).unwrap();
        assert_eq!(
            arglist_names(&list),
            vec!["&rest".to_string(), "rest".to_string()]
        );

        let read =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("read")]).unwrap();
        assert_eq!(
            arglist_names(&read),
            vec!["&optional".to_string(), "arg1".to_string()]
        );

        let equal = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("equal")],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&equal),
            vec!["arg1".to_string(), "arg2".to_string()]
        );

        let substring = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("substring")],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&substring),
            vec![
                "arg1".to_string(),
                "&optional".to_string(),
                "arg2".to_string(),
                "arg3".to_string()
            ]
        );

        let funcall = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("funcall")],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&funcall),
            vec![
                "arg1".to_string(),
                "&rest".to_string(),
                "rest".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_eval_preserve_names_core_subrs() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let cdr = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("cdr"), Value::True],
        )
        .unwrap();
        assert_eq!(arglist_names(&cdr), vec!["list".to_string()]);

        let cons = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("cons"), Value::True],
        )
        .unwrap();
        assert_eq!(arglist_names(&cons), vec!["car".to_string(), "cdr".to_string()]);

        let list = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("list"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&list),
            vec!["&rest".to_string(), "objects".to_string()]
        );

        let min = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("min"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&min),
            vec![
                "number-or-marker".to_string(),
                "&rest".to_string(),
                "numbers-or-markers".to_string()
            ]
        );

        let max = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("max"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&max),
            vec![
                "number-or-marker".to_string(),
                "&rest".to_string(),
                "numbers-or-markers".to_string()
            ]
        );

        let message = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("message"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&message),
            vec![
                "format-string".to_string(),
                "&rest".to_string(),
                "args".to_string()
            ]
        );

        let concat = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("concat"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&concat),
            vec!["&rest".to_string(), "sequences".to_string()]
        );

        let nconc = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("nconc"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&nconc),
            vec!["&rest".to_string(), "lists".to_string()]
        );

        let format = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("format"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&format),
            vec![
                "string".to_string(),
                "&rest".to_string(),
                "objects".to_string()
            ]
        );

        let apply = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("apply"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&apply),
            vec![
                "function".to_string(),
                "&rest".to_string(),
                "arguments".to_string()
            ]
        );

        let assq = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("assq"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&assq),
            vec!["key".to_string(), "alist".to_string()]
        );

        let string_match = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("string-match"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&string_match),
            vec![
                "regexp".to_string(),
                "string".to_string(),
                "&optional".to_string(),
                "start".to_string(),
                "inhibit-modify".to_string()
            ]
        );

        let make_string = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("make-string"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&make_string),
            vec![
                "length".to_string(),
                "init".to_string(),
                "&optional".to_string(),
                "multibyte".to_string()
            ]
        );

        let read_from_string = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("read-from-string"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&read_from_string),
            vec![
                "string".to_string(),
                "&optional".to_string(),
                "start".to_string(),
                "end".to_string()
            ]
        );

        let funcall = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("funcall"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&funcall),
            vec![
                "function".to_string(),
                "&rest".to_string(),
                "arguments".to_string()
            ]
        );

        let mapcar = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("mapcar"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&mapcar),
            vec!["function".to_string(), "sequence".to_string()]
        );

        let read = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("read"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&read),
            vec!["&optional".to_string(), "stream".to_string()]
        );

        let documentation = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("documentation"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&documentation),
            vec![
                "function".to_string(),
                "&optional".to_string(),
                "raw".to_string()
            ]
        );
    }

    // =======================================================================
    // documentation (eval-dependent)
    // =======================================================================

    #[test]
    fn documentation_lambda_with_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();

        // Set up a lambda with a docstring in the function cell.
        let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["x".to_string()]),
            body: vec![],
            env: None,
            docstring: Some("Add one to X.".to_string()),
        }));
        evaluator.obarray.set_symbol_function("my-fn", lambda);

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("my-fn")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Add one to X."));
    }

    #[test]
    fn documentation_lambda_no_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: None,
        }));
        evaluator.obarray.set_symbol_function("no-doc", lambda);

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("no-doc")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_unbound_function() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_err());
    }

    #[test]
    fn documentation_subr() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("plus", Value::Subr("+".to_string()));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("plus")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_string());
    }

    #[test]
    fn documentation_symbol_alias_to_builtin_returns_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("alias-builtin", Value::symbol("car"));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("alias-builtin")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_string());
    }

    #[test]
    fn documentation_prefers_function_documentation_property() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::string("propdoc"),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert_eq!(result.unwrap().as_str(), Some("propdoc"));
    }

    #[test]
    fn documentation_integer_function_documentation_property_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator
            .obarray
            .put_property("doc-prop", "function-documentation", Value::Int(9));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_list_function_documentation_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert_eq!(result.unwrap().as_str(), Some("doc"));
    }

    #[test]
    fn documentation_symbol_function_documentation_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator
            .obarray
            .put_property("doc-prop", "function-documentation", Value::symbol("t"));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn documentation_vector_function_documentation_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::vector(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert!(result.unwrap().is_vector());
    }

    #[test]
    fn documentation_unbound_symbol_function_documentation_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::symbol("doc-prop-unbound"),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "void-variable"),
            other => panic!("expected void-variable signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_invalid_form_function_documentation_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::list(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "invalid-function"),
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_quoted_lambda_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x")]),
            Value::string("d"),
            Value::symbol("x"),
        ]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]).unwrap();
        assert_eq!(result.as_str(), Some("d"));
    }

    #[test]
    fn documentation_quoted_lambda_without_docstring_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x")]),
            Value::symbol("x"),
        ]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn documentation_vector_designator_returns_keyboard_macro_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result =
            builtin_documentation(&mut evaluator, vec![Value::vector(vec![Value::Int(1)])]).unwrap();
        assert_eq!(result.as_str(), Some("Keyboard macro."));
    }

    #[test]
    fn documentation_string_designator_returns_keyboard_macro_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(&mut evaluator, vec![Value::string("abc")]).unwrap();
        assert_eq!(result.as_str(), Some("Keyboard macro."));
    }

    #[test]
    fn documentation_quoted_macro_payload_matches_oracle_shape() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![
            Value::symbol("macro"),
            Value::list(vec![Value::symbol("x")]),
            Value::string("md"),
            Value::symbol("x"),
        ]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "invalid-function");
                assert_eq!(
                    sig.data.first(),
                    Some(&Value::list(vec![
                        Value::list(vec![Value::symbol("x")]),
                        Value::string("md"),
                        Value::symbol("x"),
                    ]))
                );
            }
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_empty_quoted_macro_errors_void_function_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![Value::symbol("macro")]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "void-function");
                assert!(sig.data.first().is_some_and(Value::is_nil));
            }
            other => panic!("expected void-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_non_symbol_non_function_errors_invalid_function() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(
            &mut evaluator,
            vec![Value::list(vec![Value::Int(1), Value::Int(2)])],
        );
        assert!(result.is_err());
    }

    #[test]
    fn documentation_wrong_arity() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(&mut evaluator, vec![]);
        assert!(result.is_err());
    }

    // =======================================================================
    // describe-function (eval-dependent)
    // =======================================================================

    #[test]
    fn describe_function_lambda() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: None,
        }));
        evaluator.obarray.set_symbol_function("my-fn", lambda);

        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("my-fn")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Lisp function"));
    }

    #[test]
    fn describe_function_subr() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("plus", Value::Subr("+".to_string()));

        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("plus")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Built-in function"));
    }

    #[test]
    fn describe_function_resolves_builtin_name() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("car")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Built-in function"));
    }

    #[test]
    fn describe_function_special_form_if() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("if")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Special-form"));
    }

    #[test]
    fn describe_function_keyboard_macro_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-kmacro-string", Value::string("abc"));

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-kmacro-string")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Keyboard macro"));
    }

    #[test]
    fn describe_function_keyboard_macro_vector() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_function(
            "vm-kmacro-vector",
            Value::vector(vec![Value::Int(97), Value::Int(98)]),
        );

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-kmacro-vector")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Keyboard macro"));
    }

    #[test]
    fn describe_function_macro_marker_list() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let macro_marker = Value::cons(
            Value::symbol("macro"),
            Value::list(vec![Value::symbol("lambda"), Value::Nil, Value::Int(1)]),
        );
        evaluator
            .obarray
            .set_symbol_function("vm-macro-marker", macro_marker);

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-macro-marker")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Lisp macro"));
    }

    #[test]
    fn describe_function_alias_reports_alias_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-alias-car", Value::symbol("car"));

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-alias-car")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("vm-alias-car is an alias for `car`."));
    }

    #[test]
    fn describe_function_alias_to_missing_reports_alias_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-alias-missing", Value::symbol("vm-no-such-fn"));

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-alias-missing")]);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().as_str(),
            Some("vm-alias-missing is an alias for `vm-no-such-fn`, which is not known to be defined.")
        );
    }

    #[test]
    fn describe_function_autoload_macro_form() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let autoload_macro = Value::list(vec![
            Value::symbol("autoload"),
            Value::string("files"),
            Value::string("doc"),
            Value::Nil,
            Value::symbol("macro"),
        ]);
        evaluator
            .obarray
            .set_symbol_function("vm-autoload-macro", autoload_macro);

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-macro")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Lisp macro"));
    }

    #[test]
    fn describe_function_void() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_err());
    }

    #[test]
    fn describe_function_non_symbol() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    // =======================================================================
    // describe-variable (eval-dependent)
    // =======================================================================

    #[test]
    fn describe_variable_with_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::string("The answer."),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("The answer."));
    }

    #[test]
    fn describe_variable_load_path_uses_c_source_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("load-path")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("defined in"))
        );
    }

    #[test]
    fn describe_variable_list_doc_property_falls_back_to_value_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("value is"))
        );
    }

    #[test]
    fn describe_variable_integer_doc_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator
            .obarray
            .put_property("my-var", "variable-documentation", Value::Int(9));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("defined in"))
        );
    }

    #[test]
    fn describe_variable_symbol_doc_property_unbound_errors_void_variable() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::symbol("vm-desc-unbound"),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "void-variable"),
            other => panic!("expected void-variable signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_symbol_doc_property_bound_true_errors_wrong_type_argument() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.set_symbol_value("my-doc", Value::True);
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::symbol("my-doc"),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data.first().and_then(Value::as_symbol_name),
                    Some("char-or-string-p")
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_symbol_doc_property_bound_list_errors_wrong_type_argument() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.set_symbol_value(
            "my-doc",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::symbol("my-doc"),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_true_doc_property_errors_wrong_type_argument() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator
            .obarray
            .put_property("my-var", "variable-documentation", Value::True);

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data.first().and_then(Value::as_symbol_name),
                    Some("char-or-string-p")
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_bound_no_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert!(s.as_str().unwrap().contains("x"));
    }

    #[test]
    fn describe_variable_bound_no_doc_matches_text_shape() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]).unwrap();
        let text = result.as_str().expect("describe-variable must return a string");
        assert!(text.ends_with('\n'));
        assert!(text.contains("x\u{2019}s value is"));
    }

    #[test]
    fn describe_variable_unbound() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("void as a variable"))
        );
    }

    #[test]
    fn describe_variable_unbound_has_trailing_newline() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.ends_with('\n'))
        );
    }

    #[test]
    fn describe_variable_non_symbol() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn describe_variable_accepts_optional_second_arg() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result =
            builtin_describe_variable(&mut evaluator, vec![Value::symbol("x"), Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().as_str().is_some());
    }

    #[test]
    fn describe_variable_accepts_optional_third_arg() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(
            &mut evaluator,
            vec![Value::symbol("x"), Value::Nil, Value::Nil],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_str().is_some());
    }

    #[test]
    fn describe_variable_rejects_fourth_arg() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(
            &mut evaluator,
            vec![Value::symbol("x"), Value::Nil, Value::Nil, Value::Nil],
        );
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_property_eval_returns_string_property() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::string("doc"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert_eq!(result.as_str(), Some("doc"));
    }

    #[test]
    fn documentation_property_eval_integer_property_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::Int(7));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn documentation_property_eval_load_path_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("load-path"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.as_str().is_some_and(|s| s.contains("defined in")));
    }

    #[test]
    fn documentation_property_eval_debug_on_error_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("debug-on-error"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.as_str().is_some_and(|s| s.contains("defined in")));
    }

    #[test]
    fn documentation_property_eval_list_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert_eq!(result.as_str(), Some("doc"));
    }

    #[test]
    fn documentation_property_eval_symbol_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::symbol("t"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn documentation_property_eval_vector_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::vector(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.is_vector());
    }

    #[test]
    fn documentation_property_eval_unbound_symbol_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::symbol("doc-sym-unbound"),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        );
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "void-variable"),
            other => panic!("expected void-variable signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_property_eval_invalid_form_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::list(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        );
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "invalid-function"),
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_property_eval_non_symbol_prop_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "x", Value::string("v"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![Value::symbol("doc-sym"), Value::Int(1)],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn documentation_property_eval_non_symbol_target_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![Value::Int(1), Value::symbol("variable-documentation")],
        );
        assert!(result.is_err());
    }
}
