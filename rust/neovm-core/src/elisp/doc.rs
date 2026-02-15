//! Documentation and help support builtins.
//!
//! Provides:
//! - `documentation` — retrieve docstring from a function
//! - `describe-function` — return description string for a function
//! - `describe-variable` — return description string for a variable
//! - `documentation-property` — retrieve documentation property
//! - `Snarf-documentation` — internal DOC file loader (stub)
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

    // Resolve the function value. FUNCTION may be a symbol, a lambda, or a
    // compiled function directly.
    let func_val = match &args[0] {
        Value::Symbol(name) => {
            // Look up the function cell via the obarray, following indirection.
            match eval.obarray.indirect_function(name) {
                Some(val) => val,
                None => {
                    // Also try the direct function cell without indirection.
                    match eval.obarray.symbol_function(name) {
                        Some(val) => val.clone(),
                        None => return Ok(Value::Nil),
                    }
                }
            }
        }
        Value::Nil => {
            // nil as a symbol — look up "nil" function cell.
            return Ok(Value::Nil);
        }
        other => other.clone(),
    };

    // Extract the docstring from the resolved function value.
    match &func_val {
        Value::Lambda(data) | Value::Macro(data) => match &data.docstring {
            Some(doc) => Ok(Value::string(doc.clone())),
            None => Ok(Value::Nil),
        },
        Value::Subr(_) | Value::ByteCode(_) => {
            // Built-in functions and bytecode don't carry docstrings in our
            // implementation.
            Ok(Value::Nil)
        }
        _ => Ok(Value::Nil),
    }
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

    // Look up the function cell.
    let func_val = eval.obarray.indirect_function(&name);

    let description = match func_val {
        Some(Value::Lambda(_)) => "Lisp function",
        Some(Value::Macro(_)) => "Lisp macro",
        Some(Value::Subr(_)) => "Built-in function",
        Some(Value::ByteCode(_)) => "Compiled Lisp function",
        Some(_) => "Lisp function",
        None => {
            if eval.obarray.fboundp(&name) {
                "Lisp function"
            } else {
                return Err(signal("void-function", vec![Value::symbol(&name)]));
            }
        }
    };

    Ok(Value::string(description))
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
    expect_args("describe-variable", &args, 1)?;

    let name = match args[0].as_symbol_name() {
        Some(n) => n.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            ));
        }
    };

    // Check for :variable-documentation property.
    if let Some(doc_val) = eval.obarray.get_property(&name, "variable-documentation") {
        if let Some(doc_str) = doc_val.as_str() {
            return Ok(Value::string(doc_str.to_string()));
        }
        // If the property exists but isn't a string, return it as-is.
        return Ok(doc_val.clone());
    }

    // No documentation property. Check if the variable is at least bound.
    if eval.obarray.boundp(&name) {
        Ok(Value::string(format!("{} is a variable.", name)))
    } else {
        Ok(Value::Nil)
    }
}

/// `(documentation-property SYMBOL PROP &optional RAW)` -- return the
/// documentation property PROP of SYMBOL.
///
/// Evaluator-aware implementation:
/// - validates SYMBOL as a symbol designator (`symbolp`)
/// - returns nil when PROP is not a symbol (matching Emacs `get`-like behavior)
/// - returns string-valued properties only; non-string properties map to nil
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

    match eval.obarray.get_property(sym, prop) {
        Some(value) if value.is_string() => Ok(value.clone()),
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
/// Stub implementation: always returns nil.  In real Emacs this is called
/// during startup to load the pre-built documentation file (DOC) that
/// contains docstrings for all built-in functions and variables.
pub(crate) fn builtin_snarf_documentation(args: Vec<Value>) -> EvalResult {
    expect_args("Snarf-documentation", &args, 1)?;
    // Validate that the argument is a string.
    if !args[0].is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
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

    if let Some(arglist) = help_arglist_from_value(&args[0], preserve_names) {
        return Ok(arglist);
    }

    Ok(Value::True)
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

fn help_arglist_from_quoted_lambda(function: &Value) -> Option<Value> {
    let items = list_to_vec(function)?;
    let head = items.first()?.as_symbol_name()?;
    if head != "lambda" && head != "macro" {
        return None;
    }
    let params = items.get(1)?;
    let param_items = list_to_vec(params)?;
    let mut required = Vec::new();
    let mut optional = Vec::new();
    let mut rest = None;
    enum Mode {
        Required,
        Optional,
        Rest,
    }
    let mut mode = Mode::Required;
    for item in &param_items {
        match item.as_symbol_name() {
            Some("&optional") => {
                mode = Mode::Optional;
                continue;
            }
            Some("&rest") => {
                mode = Mode::Rest;
                continue;
            }
            Some(name) => match mode {
                Mode::Required => required.push(name.to_string()),
                Mode::Optional => optional.push(name.to_string()),
                Mode::Rest => {
                    rest = Some(name.to_string());
                    break;
                }
            },
            None => return None,
        }
    }
    let params = LambdaParams {
        required,
        optional,
        rest,
    };
    Some(help_arglist_from_lambda_params(&params))
}

fn help_arglist_from_subr_name(name: &str, preserve_names: bool) -> Option<Value> {
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

fn help_arglist_from_value(function: &Value, preserve_names: bool) -> Option<Value> {
    match function {
        Value::Subr(name) => help_arglist_from_subr_name(name, preserve_names),
        Value::Symbol(name) => help_arglist_from_subr_name(name, preserve_names),
        Value::Lambda(lambda) | Value::Macro(lambda) => {
            Some(help_arglist_from_lambda_params(&lambda.params))
        }
        Value::ByteCode(bytecode) => Some(help_arglist_from_lambda_params(&bytecode.params)),
        Value::Cons(_) => help_arglist_from_quoted_lambda(function),
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
    // Snarf-documentation (stub)
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
    fn help_function_arglist_condition_notify_optional_arg() {
        let result = builtin_help_function_arglist(vec![Value::symbol("condition-notify")]).unwrap();
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
        let result = builtin_help_function_arglist(vec![Value::Lambda(std::sync::Arc::new(
            LambdaData {
                params: LambdaParams {
                    required: vec!["x".to_string()],
                    optional: vec!["y".to_string()],
                    rest: Some("rest".to_string()),
                },
                body: vec![],
                env: None,
                docstring: None,
            },
        ))])
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
    fn help_function_arglist_wrong_arity() {
        let result = builtin_help_function_arglist(vec![]);
        assert!(result.is_err());
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
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_subr() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("plus", Value::Subr("+".to_string()));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("plus")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
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
    fn describe_variable_bound_no_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert!(s.as_str().unwrap().contains("x"));
    }

    #[test]
    fn describe_variable_unbound() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn describe_variable_non_symbol() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn documentation_property_eval_returns_string_property() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::string("doc"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![Value::symbol("doc-sym"), Value::symbol("variable-documentation")],
        )
        .unwrap();
        assert_eq!(result.as_str(), Some("doc"));
    }

    #[test]
    fn documentation_property_eval_non_string_property_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::Int(7));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![Value::symbol("doc-sym"), Value::symbol("variable-documentation")],
        )
        .unwrap();
        assert!(result.is_nil());
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
