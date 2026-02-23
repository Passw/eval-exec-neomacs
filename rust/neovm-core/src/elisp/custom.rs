//! Customization and buffer-local variable system.
//!
//! Implements `defcustom`, `defgroup`, `defvar-local`, `setq-default`,
//! `custom-set-variables`, `custom-set-faces`, and related builtins.

use std::collections::HashMap;

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Custom variable and group data structures
// ---------------------------------------------------------------------------

/// Describes a variable declared via `defcustom`.
#[derive(Clone, Debug)]
pub struct CustomVariable {
    /// Variable name (symbol).
    pub name: String,
    /// The :type specification (stored as a Value for flexibility).
    pub custom_type: Value,
    /// The :group this variable belongs to.
    pub group: Option<String>,
    /// Documentation string.
    pub documentation: Option<String>,
    /// The standard (default) value as an unevaluated form.
    pub standard_value: Value,
    /// The :set function (called instead of `set` when the user sets the variable).
    pub set_function: Option<Value>,
    /// The :get function (called instead of `default-value` when reading).
    pub get_function: Option<Value>,
    /// The :initialize function.
    pub initialize: Option<Value>,
}

/// Describes a customization group declared via `defgroup`.
#[derive(Clone, Debug)]
pub struct CustomGroup {
    /// Group name (symbol).
    pub name: String,
    /// Member list: each member is (NAME WIDGET).
    pub members: Vec<(String, Value)>,
    /// Documentation string.
    pub documentation: Option<String>,
    /// Parent group name, if any.
    pub parent: Option<String>,
}

/// Central registry for custom variables and groups.
#[derive(Clone, Debug, Default)]
pub struct CustomManager {
    /// Custom variables keyed by name.
    pub variables: HashMap<String, CustomVariable>,
    /// Custom groups keyed by name.
    pub groups: HashMap<String, CustomGroup>,
    /// Set of variable names marked as automatically buffer-local.
    pub auto_buffer_local: std::collections::HashSet<String>,
}

impl CustomManager {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            groups: HashMap::new(),
            auto_buffer_local: std::collections::HashSet::new(),
        }
    }

    /// Register a custom variable.
    pub fn define_variable(&mut self, var: CustomVariable) {
        self.variables.insert(var.name.clone(), var);
    }

    /// Register a custom group.
    pub fn define_group(&mut self, group: CustomGroup) {
        self.groups.insert(group.name.clone(), group);
    }

    /// Check if a variable is registered as a custom variable.
    pub fn is_custom_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Check if a group is registered.
    pub fn is_custom_group(&self, name: &str) -> bool {
        self.groups.contains_key(name)
    }

    /// Get a custom variable definition.
    pub fn get_variable(&self, name: &str) -> Option<&CustomVariable> {
        self.variables.get(name)
    }

    /// Get a custom group definition.
    pub fn get_group(&self, name: &str) -> Option<&CustomGroup> {
        self.groups.get(name)
    }

    /// Mark a variable as automatically buffer-local.
    pub fn make_variable_buffer_local(&mut self, name: &str) {
        self.auto_buffer_local.insert(name.to_string());
    }

    /// Check if a variable is automatically buffer-local.
    pub fn is_auto_buffer_local(&self, name: &str) -> bool {
        self.auto_buffer_local.contains(name)
    }
}

// ---------------------------------------------------------------------------
// Pure builtins (no evaluator needed)
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

/// `(custom-variable-p SYMBOL)` -- returns t if SYMBOL is a custom variable.
pub(crate) fn builtin_custom_variable_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("custom-variable-p", &args, 1)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.as_str(),
        Value::Nil => "nil",
        Value::True => "t",
        _ => return Ok(Value::Nil),
    };
    Ok(Value::bool(eval.custom.is_custom_variable(name)))
}

// ---------------------------------------------------------------------------
// Evaluator-dependent builtins
// ---------------------------------------------------------------------------

/// `(custom-set-variables &rest ARGS)` -- batch-set custom variables.
///
/// Each ARG is (SYMBOL EXP [NOW [REQUEST [COMMENT]]]).
/// For now we just evaluate EXP and set the variable value.
pub(crate) fn builtin_custom_set_variables(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    for arg in &args {
        let items = list_to_vec(arg).ok_or_else(|| {
            signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), arg.clone()],
            )
        })?;
        if items.is_empty() {
            continue;
        }

        let name = match &items[0] {
            Value::Symbol(s) => s.clone(),
            Value::Nil => "nil".to_string(),
            Value::True => "t".to_string(),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), other.clone()],
                ))
            }
        };
        if items.len() < 2 {
            continue;
        }

        // Emacs ignores entries for unknown/unbound variables.
        let should_set =
            eval.obarray.symbol_value(&name).is_some() || eval.custom.is_custom_variable(&name);
        if !should_set {
            continue;
        }

        // The second element is the value (already evaluated by caller
        // since this is a regular function, not a special form).
        let value = items[1].clone();

        // If the custom variable has a :set function, call it.
        let set_fn = eval
            .custom
            .get_variable(&name)
            .and_then(|cv| cv.set_function.clone());
        if let Some(func) = set_fn {
            eval.apply(func, vec![Value::symbol(name.clone()), value.clone()])?;
        } else {
            eval.obarray.set_symbol_value(&name, value);
        }
    }
    Ok(Value::Nil)
}

/// `(custom-set-faces &rest ARGS)` -- validates custom theme spec shape.
pub(crate) fn builtin_custom_set_faces(args: Vec<Value>) -> EvalResult {
    for arg in &args {
        let items = list_to_vec(arg).ok_or_else(|| {
            signal(
                "error",
                vec![Value::string("Incompatible Custom theme spec")],
            )
        })?;
        if items.is_empty() {
            continue;
        }
        match &items[0] {
            Value::Symbol(_) | Value::Nil | Value::True => {}
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), other.clone()],
                ))
            }
        }
    }
    Ok(Value::Nil)
}

/// `(make-variable-buffer-local VARIABLE)` -- mark variable as automatically buffer-local.
pub(crate) fn builtin_make_variable_buffer_local(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("make-variable-buffer-local", &args, 1)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            ))
        }
    };
    eval.custom.make_variable_buffer_local(&name);
    Ok(args[0].clone())
}

/// `(make-local-variable VARIABLE)` -- make variable local in current buffer.
pub(crate) fn builtin_make_local_variable(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("make-local-variable", &args, 1)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            ))
        }
    };
    // Set the current value as buffer-local if a buffer is current.
    // Clone the value first to avoid borrow conflicts.
    let value = eval
        .obarray
        .symbol_value(&name)
        .cloned()
        .unwrap_or(Value::Nil);
    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.set_buffer_local(&name, value);
    }
    Ok(args[0].clone())
}

/// `(local-variable-p VARIABLE &optional BUFFER)` -- test if variable is local.
pub(crate) fn builtin_local_variable_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("local-variable-p", &args, 1)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        _ => return Ok(Value::Nil),
    };

    let buf = if args.len() > 1 {
        match &args[1] {
            Value::Buffer(id) => eval.buffers.get(*id),
            _ => eval.buffers.current_buffer(),
        }
    } else {
        eval.buffers.current_buffer()
    };

    match buf {
        Some(b) => Ok(Value::bool(b.get_buffer_local(&name).is_some())),
        None => Ok(Value::Nil),
    }
}

/// `(buffer-local-boundp SYMBOL BUFFER)` -- test if SYMBOL is bound in BUFFER.
pub(crate) fn builtin_buffer_local_bound_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("buffer-local-boundp", &args, 2)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            ))
        }
    };

    let buffer_id = match args[1] {
        Value::Buffer(id) => id,
        ref other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("bufferp"), other.clone()],
            ))
        }
    };

    let Some(buf) = eval.buffers.get(buffer_id) else {
        return Ok(Value::Nil);
    };

    if buf.get_buffer_local(&name).is_some() {
        return Ok(Value::True);
    }

    Ok(Value::bool(eval.obarray().boundp(&name)))
}

/// `(buffer-local-variables &optional BUFFER)` -- list all local variables.
pub(crate) fn builtin_buffer_local_variables(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("buffer-local-variables", &args, 1)?;

    let id = match args.first() {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?,
        Some(Value::Buffer(id)) => *id,
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("bufferp"), other.clone()],
            ))
        }
    };

    let buf = eval
        .buffers
        .get(id)
        .ok_or_else(|| signal("error", vec![Value::string("No such live buffer")]))?;

    let mut locals: Vec<(String, Value)> = buf
        .properties
        .iter()
        .map(|(name, value)| (name.clone(), value.clone()))
        .collect();
    locals.sort_by(|a, b| a.0.cmp(&b.0));

    let entries: Vec<Value> = locals
        .into_iter()
        .map(|(name, value)| Value::cons(Value::symbol(name), value))
        .collect();
    Ok(Value::list(entries))
}

/// `(kill-local-variable VARIABLE)` -- remove local binding in current buffer.
pub(crate) fn builtin_kill_local_variable(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("kill-local-variable", &args, 1)?;
    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            ))
        }
    };
    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.properties.remove(&name);
    }
    Ok(args[0].clone())
}

/// `(default-value SYMBOL)` -- get the default (global) value of a variable.
pub(crate) fn builtin_default_value(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("default-value", &args, 1)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = super::builtins::resolve_variable_alias_name(eval, name)?;
    match eval.obarray.symbol_value(&resolved) {
        Some(v) => Ok(v.clone()),
        None if resolved.starts_with(':') => Ok(Value::symbol(resolved)),
        None => Err(signal("void-variable", vec![Value::symbol(name)])),
    }
}

/// `(set-default SYMBOL VALUE)` -- set the default (global) value.
pub(crate) fn builtin_set_default(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-default", &args, 2)?;
    let name = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;
    let resolved = super::builtins::resolve_variable_alias_name(eval, name)?;
    if eval.obarray().is_constant(&resolved) {
        return Err(signal("setting-constant", vec![Value::symbol(name)]));
    }
    let value = args[1].clone();
    eval.obarray.set_symbol_value(&resolved, value.clone());
    eval.run_variable_watchers(&resolved, &value, &Value::Nil, "set")?;
    if resolved != name {
        eval.run_variable_watchers(&resolved, &value, &Value::Nil, "set")?;
    }
    Ok(value)
}

// ---------------------------------------------------------------------------
// Special form handlers (called from eval.rs try_special_form dispatch)
// ---------------------------------------------------------------------------

/// `(defcustom NAME VALUE DOCSTRING &rest KWARGS)`
///
/// Like `defvar`: only sets the variable if it is not already bound, marks it
/// as special (dynamically scoped).  Registers a [`CustomVariable`] with the
/// evaluator's [`CustomManager`].  Returns the symbol name.
pub(crate) fn sf_defcustom(
    eval: &mut super::eval::Evaluator,
    tail: &[super::expr::Expr],
) -> super::error::EvalResult {
    use super::eval::quote_to_value;
    use super::expr::Expr;

    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("defcustom"), Value::Int(tail.len() as i64)],
        ));
    }

    // 1. Extract the symbol name (unevaluated).
    let name = match &tail[0] {
        Expr::Symbol(s) => s.clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(other)],
            ));
        }
    };

    // 2. Evaluate the default value expression (second element).
    let default_value = if tail.len() > 1 {
        eval.eval(&tail[1])?
    } else {
        Value::Nil
    };

    // 3. Extract optional docstring (third element, if it is a string literal).
    let mut kwstart = 2;
    let documentation = if tail.len() > 2 {
        if let Expr::Str(s) = &tail[2] {
            kwstart = 3;
            Some(s.clone())
        } else {
            None
        }
    } else {
        None
    };

    // 4. Parse keyword arguments from the remaining tail.
    let mut custom_type = Value::Nil;
    let mut group: Option<String> = None;
    let mut set_function: Option<Value> = None;
    let mut get_function: Option<Value> = None;
    let mut initialize: Option<Value> = None;

    let mut i = kwstart;
    while i + 1 < tail.len() {
        match &tail[i] {
            Expr::Keyword(kw) => {
                let val = eval.eval(&tail[i + 1])?;
                match kw.as_str() {
                    ":type" => custom_type = val,
                    ":group" => {
                        group = match &val {
                            Value::Symbol(s) => Some(s.clone()),
                            _ => None,
                        };
                    }
                    ":set" => set_function = Some(val),
                    ":get" => get_function = Some(val),
                    ":initialize" => initialize = Some(val),
                    _ => { /* ignore unknown keywords gracefully */ }
                }
                i += 2;
            }
            _ => {
                // Not a keyword — skip (graceful handling of extra non-keyword forms).
                i += 1;
            }
        }
    }

    // 5. Like defvar: only set if not already bound.
    if !eval.obarray().boundp(&name) {
        eval.obarray_mut()
            .set_symbol_value(&name, default_value.clone());
    }

    // 6. Mark as special (dynamically scoped).
    eval.obarray_mut().make_special(&name);

    // 7. Register with the CustomManager.
    eval.custom.define_variable(CustomVariable {
        name: name.clone(),
        custom_type,
        group,
        documentation,
        standard_value: default_value,
        set_function,
        get_function,
        initialize,
    });

    Ok(Value::symbol(name))
}

/// `(defgroup NAME MEMBERS DOCSTRING &rest KWARGS)`
///
/// Registers a [`CustomGroup`] with the evaluator's [`CustomManager`].
/// Returns the symbol name.
pub(crate) fn sf_defgroup(
    eval: &mut super::eval::Evaluator,
    tail: &[super::expr::Expr],
) -> super::error::EvalResult {
    use super::eval::quote_to_value;
    use super::expr::Expr;

    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("defgroup"), Value::Int(tail.len() as i64)],
        ));
    }

    // 1. Extract the group name (unevaluated symbol).
    let name = match &tail[0] {
        Expr::Symbol(s) => s.clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(other)],
            ));
        }
    };

    // 2. Evaluate the members expression (usually nil).
    let members_val = if tail.len() > 1 {
        eval.eval(&tail[1])?
    } else {
        Value::Nil
    };

    // Convert members list to Vec<(String, Value)>.
    let members = if let Some(items) = list_to_vec(&members_val) {
        items
            .iter()
            .filter_map(|item| {
                if let Some(pair) = list_to_vec(item) {
                    if pair.len() >= 2 {
                        if let Value::Symbol(s) = &pair[0] {
                            return Some((s.clone(), pair[1].clone()));
                        }
                    }
                }
                None
            })
            .collect()
    } else {
        Vec::new()
    };

    // 3. Extract optional docstring (third element, if string literal).
    let mut kwstart = 2;
    let documentation = if tail.len() > 2 {
        if let Expr::Str(s) = &tail[2] {
            kwstart = 3;
            Some(s.clone())
        } else {
            None
        }
    } else {
        None
    };

    // 4. Parse keyword arguments.
    let mut parent: Option<String> = None;

    let mut i = kwstart;
    while i + 1 < tail.len() {
        match &tail[i] {
            Expr::Keyword(kw) => {
                let val = eval.eval(&tail[i + 1])?;
                match kw.as_str() {
                    ":group" => {
                        parent = match &val {
                            Value::Symbol(s) => Some(s.clone()),
                            _ => None,
                        };
                    }
                    ":prefix" | _ => { /* ignore unknown keywords */ }
                }
                i += 2;
            }
            _ => {
                i += 1;
            }
        }
    }

    // 5. Register the group.
    eval.custom.define_group(CustomGroup {
        name: name.clone(),
        members,
        documentation,
        parent,
    });

    Ok(Value::symbol(name))
}

/// `(setq-default SYM VAL [SYM VAL] ...)`
///
/// Like `setq` but sets the obarray (default/global) value directly, bypassing
/// any dynamic bindings.  Processes symbol/value pairs.  Returns the last
/// value.
pub(crate) fn sf_setq_default(
    eval: &mut super::eval::Evaluator,
    tail: &[super::expr::Expr],
) -> super::error::EvalResult {
    use super::eval::quote_to_value;
    use super::expr::Expr;

    if tail.is_empty() {
        return Ok(Value::Nil);
    }
    if tail.len() % 2 != 0 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("setq-default"), Value::Int(tail.len() as i64)],
        ));
    }

    let mut last = Value::Nil;
    let mut i = 0;
    while i < tail.len() {
        let name = match &tail[i] {
            Expr::Symbol(s) | Expr::Keyword(s) => s.clone(),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), quote_to_value(other)],
                ));
            }
        };
        let value = eval.eval(&tail[i + 1])?;
        last = builtin_set_default(eval, vec![Value::symbol(name), value])?;
        i += 2;
    }

    Ok(last)
}

/// `(defvar-local NAME VALUE &optional DOCSTRING)`
///
/// Like `defvar`: only sets the variable if it is not already bound, marks it
/// as special (dynamically scoped).  Additionally marks the variable as
/// automatically buffer-local via the [`CustomManager`].  Returns the symbol
/// name.
pub(crate) fn sf_defvar_local(
    eval: &mut super::eval::Evaluator,
    tail: &[super::expr::Expr],
) -> super::error::EvalResult {
    use super::eval::quote_to_value;
    use super::expr::Expr;

    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("defvar-local"), Value::Int(tail.len() as i64)],
        ));
    }

    // 1. Extract the symbol name (unevaluated).
    let name = match &tail[0] {
        Expr::Symbol(s) => s.clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(other)],
            ));
        }
    };

    // 2. Evaluate the default value expression.
    let default_value = if tail.len() > 1 {
        eval.eval(&tail[1])?
    } else {
        Value::Nil
    };

    // 3. Optional docstring (ignored for now, but consumed so we don't error).
    // (defvar-local NAME VALUE "docstring") — third element may be a string.

    // 4. Like defvar: only set if not already bound.
    if !eval.obarray().boundp(&name) {
        eval.obarray_mut().set_symbol_value(&name, default_value);
    }

    // 5. Mark as special (dynamically scoped).
    eval.obarray_mut().make_special(&name);

    // 6. Mark as automatically buffer-local.
    eval.custom.make_variable_buffer_local(&name);

    Ok(Value::symbol(name))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::{format_eval_result, parse_forms, Evaluator};

    fn eval_all(src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        ev.eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect()
    }

    // -- CustomManager unit tests ------------------------------------------

    #[test]
    fn custom_manager_new_is_empty() {
        let cm = CustomManager::new();
        assert!(cm.variables.is_empty());
        assert!(cm.groups.is_empty());
        assert!(cm.auto_buffer_local.is_empty());
    }

    #[test]
    fn custom_manager_define_variable() {
        let mut cm = CustomManager::new();
        cm.define_variable(CustomVariable {
            name: "my-var".into(),
            custom_type: Value::symbol("integer"),
            group: Some("my-group".into()),
            documentation: Some("A variable.".into()),
            standard_value: Value::Int(42),
            set_function: None,
            get_function: None,
            initialize: None,
        });
        assert!(cm.is_custom_variable("my-var"));
        assert!(!cm.is_custom_variable("other"));
        assert_eq!(cm.get_variable("my-var").unwrap().name, "my-var");
    }

    #[test]
    fn custom_manager_define_group() {
        let mut cm = CustomManager::new();
        cm.define_group(CustomGroup {
            name: "my-group".into(),
            members: vec![],
            documentation: Some("A group.".into()),
            parent: None,
        });
        assert!(cm.is_custom_group("my-group"));
        assert!(!cm.is_custom_group("other"));
    }

    #[test]
    fn custom_manager_buffer_local() {
        let mut cm = CustomManager::new();
        assert!(!cm.is_auto_buffer_local("tab-width"));
        cm.make_variable_buffer_local("tab-width");
        assert!(cm.is_auto_buffer_local("tab-width"));
    }

    // -- defcustom special form tests ----------------------------------------

    #[test]
    fn defcustom_basic() {
        let results = eval_all(r#"(defcustom my-var 42 "My variable.")"#);
        assert_eq!(results[0], "OK my-var");
    }

    #[test]
    fn defcustom_sets_value() {
        let results = eval_all(r#"(defcustom my-var 42 "My variable.") my-var"#);
        assert_eq!(results[1], "OK 42");
    }

    #[test]
    fn defcustom_with_type() {
        let results = eval_all(r#"(defcustom my-var 42 "Docs." :type 'integer) my-var"#);
        assert_eq!(results[1], "OK 42");
    }

    #[test]
    fn defcustom_with_group() {
        let results = eval_all(r#"(defcustom my-var 10 "Docs." :group 'my-group) my-var"#);
        assert_eq!(results[1], "OK 10");
    }

    #[test]
    fn defcustom_does_not_override_existing() {
        let results = eval_all(r#"(setq my-var 99) (defcustom my-var 42 "Docs.") my-var"#);
        // defcustom should not override an existing value, like defvar
        assert_eq!(results[2], "OK 99");
    }

    #[test]
    fn defcustom_marks_special() {
        let forms = parse_forms(r#"(defcustom my-var 42 "Docs.")"#).expect("parse");
        let mut ev = Evaluator::new();
        let _result = ev.eval_expr(&forms[0]);
        assert!(ev.obarray().is_special("my-var"));
    }

    #[test]
    fn defcustom_custom_variable_p() {
        let results = eval_all(
            r#"(defcustom my-var 42 "Docs.") (custom-variable-p 'my-var) (custom-variable-p 'other)"#,
        );
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK nil");
    }

    // -- defgroup special form tests -----------------------------------------

    #[test]
    fn defgroup_basic() {
        let results = eval_all(r#"(defgroup my-group nil "My group.")"#);
        assert_eq!(results[0], "OK my-group");
    }

    #[test]
    fn defgroup_registers_group() {
        let forms = parse_forms(r#"(defgroup my-group nil "Docs.")"#).expect("parse");
        let mut ev = Evaluator::new();
        let _result = ev.eval_expr(&forms[0]);
        assert!(ev.custom.is_custom_group("my-group"));
        assert!(!ev.custom.is_custom_group("other"));
    }

    #[test]
    fn custom_group_p_unavailable_without_custom_library() {
        let results = eval_all(
            r#"(defgroup my-group nil "Docs.")
               (fboundp 'custom-group-p)
               (custom-group-p 'my-group)
               (custom-group-p 'other)"#,
        );
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "ERR (void-function (custom-group-p))");
        assert_eq!(results[3], "ERR (void-function (custom-group-p))");
    }

    #[test]
    fn defgroup_with_parent_records_parent_group() {
        let forms = parse_forms(
            r#"(defgroup parent-group nil "Parent.")
               (defgroup child-group nil "Child." :group 'parent-group)"#,
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let _results: Vec<_> = ev.eval_forms(&forms);
        let child = ev
            .custom
            .get_group("child-group")
            .expect("child-group should be registered");
        assert_eq!(child.parent.as_deref(), Some("parent-group"));
    }

    // -- defvar-local special form tests ------------------------------------

    #[test]
    fn defvar_local_basic() {
        let results = eval_all(r#"(defvar-local my-local 42) my-local"#);
        assert_eq!(results[0], "OK my-local");
        assert_eq!(results[1], "OK 42");
    }

    #[test]
    fn defvar_local_marks_special() {
        let forms = parse_forms(r#"(defvar-local my-local 42)"#).expect("parse");
        let mut ev = Evaluator::new();
        let _result = ev.eval_expr(&forms[0]);
        assert!(ev.obarray().is_special("my-local"));
    }

    #[test]
    fn defvar_local_marks_buffer_local() {
        let forms = parse_forms(r#"(defvar-local my-local 42)"#).expect("parse");
        let mut ev = Evaluator::new();
        let _result = ev.eval_expr(&forms[0]);
        assert!(ev.custom.is_auto_buffer_local("my-local"));
    }

    #[test]
    fn defvar_local_does_not_override() {
        let results = eval_all(r#"(setq my-local 99) (defvar-local my-local 42) my-local"#);
        assert_eq!(results[2], "OK 99");
    }

    #[test]
    fn defvar_local_with_docstring() {
        let results = eval_all(r#"(defvar-local my-local 42 "Documentation.") my-local"#);
        assert_eq!(results[1], "OK 42");
    }

    // -- setq-default special form tests -----------------------------------

    #[test]
    fn setq_default_basic() {
        let results = eval_all(r#"(defvar x 10) (setq-default x 42) x"#);
        assert_eq!(results[2], "OK 42");
    }

    #[test]
    fn setq_default_multiple_pairs() {
        let results = eval_all(r#"(defvar a 1) (defvar b 2) (setq-default a 10 b 20) a"#);
        assert_eq!(results[3], "OK 10");
    }

    #[test]
    fn setq_default_returns_last_value() {
        let results = eval_all(r#"(setq-default x 42)"#);
        assert_eq!(results[0], "OK 42");
    }

    #[test]
    fn setq_default_follows_alias_resolution() {
        let results = eval_all(
            r#"(defvaralias 'vm-setq-default-alias 'vm-setq-default-base)
               (setq-default vm-setq-default-alias 3)
               (list (default-value 'vm-setq-default-base)
                     (default-value 'vm-setq-default-alias))"#,
        );
        assert_eq!(results[2], "OK (3 3)");
    }

    #[test]
    fn setq_default_rejects_constant_symbols() {
        let results = eval_all(
            r#"(list
                 (condition-case err (setq-default nil 1) (error err))
                 (condition-case err (setq-default :foo 1) (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK ((setting-constant nil) (setting-constant :foo))"
        );
    }

    #[test]
    fn setq_default_alias_triggers_variable_watchers_twice() {
        let results = eval_all(
            r#"(setq vm-setq-default-watch-events nil)
               (fset 'vm-setq-default-watch-rec
                     (lambda (symbol newval operation where)
                       (setq vm-setq-default-watch-events
                             (cons (list symbol newval operation where)
                                   vm-setq-default-watch-events))))
               (defvaralias 'vm-setq-default-watch 'vm-setq-default-watch-base)
               (add-variable-watcher 'vm-setq-default-watch-base 'vm-setq-default-watch-rec)
               (setq-default vm-setq-default-watch 7)
               (length vm-setq-default-watch-events)"#,
        );
        assert_eq!(results[5], "OK 2");
    }

    // -- default-value and set-default builtins ----------------------------

    #[test]
    fn default_value_returns_global() {
        let results = eval_all(r#"(defvar my-var 42) (default-value 'my-var)"#);
        assert_eq!(results[1], "OK 42");
    }

    #[test]
    fn default_value_void_signals_error() {
        let results = eval_all(r#"(default-value 'nonexistent-var)"#);
        assert!(results[0].starts_with("ERR"));
    }

    #[test]
    fn keyword_defaults_and_symbol_values_self_evaluate() {
        let results = eval_all(r#"(list (default-value :foo) (default-toplevel-value :foo) (symbol-value :foo))"#);
        assert_eq!(results[0], "OK (:foo :foo :foo)");
    }

    #[test]
    fn set_default_sets_global() {
        let results = eval_all(r#"(set-default 'my-var 99) (default-value 'my-var)"#);
        assert_eq!(results[1], "OK 99");
    }

    #[test]
    fn set_default_and_default_value_follow_alias_resolution() {
        let results = eval_all(
            r#"(defvaralias 'vm-set-default-alias 'vm-set-default-base)
               (set-default 'vm-set-default-alias 5)
               (list (default-value 'vm-set-default-base)
                     (default-value 'vm-set-default-alias))"#,
        );
        assert_eq!(results[2], "OK (5 5)");
    }

    #[test]
    fn default_value_alias_void_uses_original_symbol_in_error_payload() {
        let results = eval_all(
            r#"(defvaralias 'vm-default-alias-unbound 'vm-default-base-unbound)
               (condition-case err
                   (default-value 'vm-default-alias-unbound)
                 (error err))"#,
        );
        assert_eq!(results[1], "OK (void-variable vm-default-alias-unbound)");
    }

    #[test]
    fn set_default_rejects_constant_symbols() {
        let results = eval_all(
            r#"(list
                 (condition-case err (set-default nil 1) (error err))
                 (condition-case err (set-default t 1) (error err))
                 (condition-case err (set-default :foo 1) (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK ((setting-constant nil) (setting-constant t) (setting-constant :foo))"
        );
    }

    #[test]
    fn set_default_triggers_variable_watchers() {
        let results = eval_all(
            r#"(fset 'vm-set-default-watch-rec
                     (lambda (symbol newval operation where)
                       (setq vm-set-default-watch-last
                             (list symbol newval operation where))))
               (add-variable-watcher 'vm-set-default-watch-target 'vm-set-default-watch-rec)
               (set-default 'vm-set-default-watch-target 42)
               vm-set-default-watch-last"#,
        );
        assert_eq!(results[3], "OK (vm-set-default-watch-target 42 set nil)");
    }

    #[test]
    fn set_default_alias_triggers_variable_watchers_twice() {
        let results = eval_all(
            r#"(setq vm-set-default-alias-watch-events nil)
               (fset 'vm-set-default-alias-watch-rec
                     (lambda (symbol newval operation where)
                       (setq vm-set-default-alias-watch-events
                             (cons (list symbol newval operation where)
                                   vm-set-default-alias-watch-events))))
               (defvaralias 'vm-set-default-alias-watch 'vm-set-default-alias-base)
               (add-variable-watcher 'vm-set-default-alias-base 'vm-set-default-alias-watch-rec)
               (set-default 'vm-set-default-alias-watch 9)
               (length vm-set-default-alias-watch-events)"#,
        );
        assert_eq!(results[5], "OK 2");
    }

    #[test]
    fn set_default_toplevel_alias_triggers_variable_watchers_twice() {
        let results = eval_all(
            r#"(setq vm-set-default-top-watch-events nil)
               (fset 'vm-set-default-top-watch-rec
                     (lambda (symbol newval operation where)
                       (setq vm-set-default-top-watch-events
                             (cons (list symbol newval operation where)
                                   vm-set-default-top-watch-events))))
               (defvaralias 'vm-set-default-top-watch 'vm-set-default-top-base)
               (add-variable-watcher 'vm-set-default-top-base 'vm-set-default-top-watch-rec)
               (set-default-toplevel-value 'vm-set-default-top-watch 7)
               (length vm-set-default-top-watch-events)"#,
        );
        assert_eq!(results[5], "OK 2");
    }

    // -- make-variable-buffer-local builtin --------------------------------

    #[test]
    fn make_variable_buffer_local_works() {
        let results = eval_all(r#"(make-variable-buffer-local 'my-var)"#);
        assert_eq!(results[0], "OK my-var");
    }

    // -- make-local-variable builtin ---------------------------------------

    #[test]
    fn make_local_variable_in_buffer() {
        let results = eval_all(
            r#"(defvar my-var 42)
               (get-buffer-create "test-buf")
               (set-buffer "test-buf")
               (make-local-variable 'my-var)
               (local-variable-p 'my-var)"#,
        );
        assert_eq!(results[4], "OK t");
    }

    // -- local-variable-p builtin ------------------------------------------

    #[test]
    fn local_variable_p_returns_nil_when_not_local() {
        let results = eval_all(
            r#"(get-buffer-create "test-buf")
               (set-buffer "test-buf")
               (local-variable-p 'nonexistent)"#,
        );
        assert_eq!(results[2], "OK nil");
    }

    #[test]
    fn buffer_local_bound_p_matches_emacs_shape() {
        let results = eval_all(
            r#"(defvar neomacs-buffer-local-boundp-global 1)
               (let ((buf (get-buffer-create "test-buf")))
                 (buffer-local-boundp 'neomacs-buffer-local-boundp-global buf))
               (let ((buf (get-buffer-create "test-buf")))
                 (buffer-local-boundp 'neomacs-buffer-local-boundp-missing buf))
               (let ((buf (get-buffer-create "test-buf-local")))
                 (set-buffer buf)
                 (make-local-variable 'neomacs-buffer-local-boundp-local)
                 (setq neomacs-buffer-local-boundp-local 7)
                 (buffer-local-boundp 'neomacs-buffer-local-boundp-local buf))
               (let ((buf (generate-new-buffer "dead-buf")))
                 (kill-buffer buf)
                 (buffer-local-boundp 'neomacs-buffer-local-boundp-global buf))
               (condition-case err (buffer-local-boundp 1 (current-buffer)) (error (car err)))
               (condition-case err (buffer-local-boundp 'x nil) (error (car err)))
               (condition-case err (buffer-local-boundp 'x (current-buffer) nil)
                 (error (car err)))"#,
        );
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK nil");
        assert_eq!(results[3], "OK t");
        assert_eq!(results[4], "OK nil");
        assert_eq!(results[5], "OK wrong-type-argument");
        assert_eq!(results[6], "OK wrong-type-argument");
        assert_eq!(results[7], "OK wrong-number-of-arguments");
    }

    // -- buffer-local-variables builtin ------------------------------------

    #[test]
    fn buffer_local_variables_include_default_entries() {
        let results = eval_all(
            r#"(get-buffer-create "test-buf")
               (set-buffer "test-buf")
               (let ((locals (buffer-local-variables)))
                 (and (listp locals)
                      (assq 'buffer-read-only locals)))"#,
        );
        assert_eq!(results[2], "OK (buffer-read-only)");
    }

    #[test]
    fn buffer_local_variables_argument_validation() {
        let results = eval_all(
            r#"(condition-case err (buffer-local-variables 1) (error err))
               (condition-case err (buffer-local-variables "test-buf") (error err))
               (condition-case err (buffer-local-variables nil nil) (error err))"#,
        );
        assert_eq!(results[0], "OK (wrong-type-argument bufferp 1)");
        assert_eq!(results[1], "OK (wrong-type-argument bufferp \"test-buf\")");
        assert_eq!(
            results[2],
            "OK (wrong-number-of-arguments buffer-local-variables 2)"
        );
    }

    // -- kill-local-variable builtin ----------------------------------------

    #[test]
    fn kill_local_variable_removes_binding() {
        let results = eval_all(
            r#"(defvar my-var 42)
               (get-buffer-create "test-buf")
               (set-buffer "test-buf")
               (make-local-variable 'my-var)
               (local-variable-p 'my-var)
               (kill-local-variable 'my-var)
               (local-variable-p 'my-var)"#,
        );
        assert_eq!(results[4], "OK t");
        assert_eq!(results[6], "OK nil");
    }

    // -- custom-set-variables builtin --------------------------------------

    #[test]
    fn custom_set_variables_basic() {
        let results = eval_all(
            r#"(defvar my-var 1)
               (custom-set-variables '(my-var 42))
               (default-value 'my-var)"#,
        );
        assert_eq!(results[2], "OK 42");
    }

    #[test]
    fn custom_set_variables_ignores_unknown_variable() {
        let results = eval_all(
            r#"(custom-set-variables '(my-var 42))
               (condition-case err (default-value 'my-var) (error err))"#,
        );
        assert_eq!(results[1], "OK (void-variable my-var)");
    }

    // -- custom-set-faces --------------------------------------------------

    #[test]
    fn custom_set_faces_returns_nil() {
        let results = eval_all(r#"(custom-set-faces '(default ((t (:height 120)))))"#);
        assert_eq!(results[0], "OK nil");
    }

    #[test]
    fn custom_set_faces_non_list_spec_errors() {
        let results = eval_all(r#"(condition-case err (custom-set-faces 1) (error err))"#);
        assert_eq!(results[0], r#"OK (error "Incompatible Custom theme spec")"#);
    }

    #[test]
    fn custom_set_faces_requires_symbol_face_name() {
        let results = eval_all(r#"(condition-case err (custom-set-faces '(1 2)) (error err))"#);
        assert_eq!(results[0], "OK (wrong-type-argument symbolp 1)");
    }

    #[test]
    fn custom_set_variables_errors_for_non_list_spec() {
        let results = eval_all(r#"(condition-case err (custom-set-variables 1) (error err))"#);
        assert_eq!(results[0], "OK (wrong-type-argument listp 1)");
    }

    #[test]
    fn custom_set_variables_errors_for_non_symbol_variable_name() {
        let results = eval_all(r#"(condition-case err (custom-set-variables '(1 2)) (error err))"#);
        assert_eq!(results[0], "OK (wrong-type-argument symbolp 1)");
    }

    // -- Integration tests -------------------------------------------------

    #[test]
    fn defcustom_then_setq_default() {
        let results = eval_all(
            r#"(defcustom my-opt 10 "Opt." :type 'integer)
               (setq-default my-opt 20)
               my-opt"#,
        );
        assert_eq!(results[2], "OK 20");
    }

    #[test]
    fn defvar_local_then_buffer_local_check() {
        let forms = parse_forms(
            r#"(defvar-local my-local-var 99)
               (make-variable-buffer-local 'other-var)"#,
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let _results: Vec<_> = ev.eval_forms(&forms);
        assert!(ev.custom.is_auto_buffer_local("my-local-var"));
        assert!(ev.custom.is_auto_buffer_local("other-var"));
    }

    #[test]
    fn defcustom_keyword_args_ignored_gracefully() {
        // Extra keywords like :initialize should not cause errors
        let results = eval_all(
            r#"(defcustom my-var 5 "Docs." :type 'integer :group 'editing :initialize 'custom-initialize-default) my-var"#,
        );
        assert_eq!(results[1], "OK 5");
    }

    #[test]
    fn defgroup_multiple_groups() {
        let forms = parse_forms(
            r#"(defgroup g1 nil "Group 1.")
               (defgroup g2 nil "Group 2.")"#,
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let _results: Vec<_> = ev.eval_forms(&forms);
        assert!(ev.custom.is_custom_group("g1"));
        assert!(ev.custom.is_custom_group("g2"));
    }

    #[test]
    fn setq_default_works_on_new_variable() {
        let results = eval_all(r#"(setq-default new-var 100) new-var"#);
        assert_eq!(results[1], "OK 100");
    }
}
