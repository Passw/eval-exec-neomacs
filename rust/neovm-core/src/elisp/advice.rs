//! Advice system and variable watchers for the Elisp VM.
//!
//! Provides:
//! - **Advice system**: Before, After, Around, Override, FilterArgs, FilterReturn
//!   advice on functions (like Emacs `advice-add` / `advice-remove`).
//! - **Variable watchers**: Callbacks invoked when a watched variable changes
//!   (like Emacs `add-variable-watcher` / `remove-variable-watcher`).

use std::collections::HashMap;

use super::intern::resolve_sym;
use super::value::Value;
use crate::gc::GcTrace;

// ---------------------------------------------------------------------------
// Advice types
// ---------------------------------------------------------------------------

/// The kind of advice to apply around a target function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AdviceType {
    /// Called before the original function; receives same args.
    Before,
    /// Called after the original function; receives same args.
    After,
    /// Wraps the original function; receives the original as first arg.
    Around,
    /// Completely replaces the original function.
    Override,
    /// Filters the argument list before the original function sees it.
    FilterArgs,
    /// Filters the return value after the original function returns.
    FilterReturn,
}

impl AdviceType {
    /// Parse a keyword (e.g. `:before`) into an AdviceType.
    pub fn from_keyword(kw: &str) -> Option<Self> {
        match kw {
            ":before" => Some(Self::Before),
            ":after" => Some(Self::After),
            ":around" => Some(Self::Around),
            ":override" => Some(Self::Override),
            ":filter-args" => Some(Self::FilterArgs),
            ":filter-return" => Some(Self::FilterReturn),
            _ => None,
        }
    }

    /// Ordering key for sorting advice — lower values run first.
    fn sort_key(&self) -> u8 {
        match self {
            Self::FilterArgs => 0,
            Self::Before => 1,
            Self::Around => 2,
            Self::Override => 3,
            Self::After => 4,
            Self::FilterReturn => 5,
        }
    }
}

// ---------------------------------------------------------------------------
// Advice
// ---------------------------------------------------------------------------

/// A single piece of advice attached to a function.
#[derive(Clone, Debug)]
pub struct Advice {
    pub advice_type: AdviceType,
    /// The advice function — a lambda, symbol, or subr.
    pub function: Value,
    /// Optional name for identification / removal by name.
    pub name: Option<String>,
}

// ---------------------------------------------------------------------------
// AdviceManager
// ---------------------------------------------------------------------------

/// Central registry of all advice attached to named functions.
pub struct AdviceManager {
    /// Map from target function name → list of advice.
    advice_map: HashMap<String, Vec<Advice>>,
}

impl AdviceManager {
    pub fn new() -> Self {
        Self {
            advice_map: HashMap::new(),
        }
    }

    /// Add advice to a target function.
    pub fn add_advice(
        &mut self,
        target_fn: &str,
        advice_type: AdviceType,
        advice_fn: Value,
        name: Option<String>,
    ) {
        let entry = self.advice_map.entry(target_fn.to_string()).or_default();

        // If advice with the same name already exists, replace it
        if let Some(ref n) = name {
            if let Some(existing) = entry
                .iter_mut()
                .find(|a| a.name.as_deref() == Some(n.as_str()))
            {
                existing.advice_type = advice_type;
                existing.function = advice_fn;
                return;
            }
        }

        entry.push(Advice {
            advice_type,
            function: advice_fn,
            name,
        });
    }

    /// Remove advice from a target function by function name or advice name.
    pub fn remove_advice(&mut self, target_fn: &str, advice_fn_or_name: &str) {
        if let Some(list) = self.advice_map.get_mut(target_fn) {
            list.retain(|a| {
                // Keep if neither the name nor the function symbol matches
                let name_matches = a.name.as_deref() == Some(advice_fn_or_name);
                let fn_matches = matches!(&a.function, Value::Symbol(id) if resolve_sym(*id) == advice_fn_or_name);
                !name_matches && !fn_matches
            });
            if list.is_empty() {
                self.advice_map.remove(target_fn);
            }
        }
    }

    /// Get all advice for a function, sorted by type (filter-args first,
    /// filter-return last).
    pub fn get_advice(&self, target_fn: &str) -> Vec<&Advice> {
        match self.advice_map.get(target_fn) {
            Some(list) => {
                let mut sorted: Vec<&Advice> = list.iter().collect();
                sorted.sort_by_key(|a| a.advice_type.sort_key());
                sorted
            }
            None => Vec::new(),
        }
    }

    /// Check if a function has any advice attached.
    pub fn has_advice(&self, target_fn: &str) -> bool {
        self.advice_map
            .get(target_fn)
            .is_some_and(|list| !list.is_empty())
    }

    /// Check if a specific function or name is advising a target.
    pub fn advice_member_p(&self, target_fn: &str, advice_fn_or_name: &str) -> bool {
        match self.advice_map.get(target_fn) {
            Some(list) => list.iter().any(|a| {
                let name_matches = a.name.as_deref() == Some(advice_fn_or_name);
                let fn_matches = matches!(&a.function, Value::Symbol(id) if resolve_sym(*id) == advice_fn_or_name);
                name_matches || fn_matches
            }),
            None => false,
        }
    }
}

impl Default for AdviceManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Variable watcher system
// ---------------------------------------------------------------------------

/// A single variable watcher callback.
#[derive(Clone, Debug)]
pub struct VariableWatcher {
    /// The callback function to invoke on variable change.
    pub callback: Value,
}

/// Registry of variable watchers.
pub struct VariableWatcherList {
    /// Map from variable name → list of watcher callbacks.
    watchers: HashMap<String, Vec<VariableWatcher>>,
}

impl VariableWatcherList {
    pub fn new() -> Self {
        Self {
            watchers: HashMap::new(),
        }
    }

    /// Add a watcher callback for a variable.
    pub fn add_watcher(&mut self, var_name: &str, callback: Value) {
        let entry = self.watchers.entry(var_name.to_string()).or_default();
        // Don't add duplicate watchers.
        let already_exists = entry
            .iter()
            .any(|w| watcher_callback_matches(&w.callback, &callback));
        if !already_exists {
            entry.push(VariableWatcher { callback });
        }
    }

    /// Remove a watcher callback for a variable.
    pub fn remove_watcher(&mut self, var_name: &str, callback: &Value) {
        if let Some(list) = self.watchers.get_mut(var_name) {
            list.retain(|w| !watcher_callback_matches(&w.callback, callback));
            if list.is_empty() {
                self.watchers.remove(var_name);
            }
        }
    }

    /// Remove all watcher callbacks for a variable.
    pub fn clear_watchers(&mut self, var_name: &str) {
        self.watchers.remove(var_name);
    }

    /// Check if a variable has any watchers.
    pub fn has_watchers(&self, var_name: &str) -> bool {
        self.watchers
            .get(var_name)
            .is_some_and(|list| !list.is_empty())
    }

    /// Return registered watcher callbacks for a variable in insertion order.
    pub fn get_watchers(&self, var_name: &str) -> Vec<Value> {
        self.watchers
            .get(var_name)
            .map(|list| {
                list.iter()
                    .map(|watcher| watcher.callback)
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Build a list of (callback, args) pairs to invoke for a variable change.
    ///
    /// Returns a Vec of (callback_value, argument_list) that the evaluator
    /// should call. The caller is responsible for actually invoking them
    /// (to avoid borrow issues with the evaluator).
    ///
    /// Each callback receives: (SYMBOL NEWVAL OPERATION WHERE)
    /// - SYMBOL: the variable name
    /// - NEWVAL: the new value
    /// - OPERATION: one of "set", "let", "unlet", "makunbound", "defvaralias"
    /// - WHERE: location designator (`nil` for global, buffer for buffer-local)
    pub fn notify_watchers(
        &self,
        var_name: &str,
        new_val: &Value,
        _old_val: &Value,
        operation: &str,
        where_val: &Value,
    ) -> Vec<(Value, Vec<Value>)> {
        let mut calls = Vec::new();
        if let Some(list) = self.watchers.get(var_name) {
            for watcher in list {
                let args = vec![
                    Value::symbol(var_name),
                    *new_val,
                    Value::symbol(operation),
                    *where_val,
                ];
                calls.push((watcher.callback, args));
            }
        }
        calls
    }
}

fn watcher_callback_matches(registered: &Value, candidate: &Value) -> bool {
    if registered == candidate {
        return true;
    }
    match (registered, candidate) {
        (Value::Lambda(_), Value::Lambda(_)) | (Value::Macro(_), Value::Macro(_)) => {
            lambda_data_matches(registered, candidate)
        }
        _ => false,
    }
}

fn lambda_data_matches(left: &Value, right: &Value) -> bool {
    match (left.get_lambda_data(), right.get_lambda_data()) {
        (Some(l), Some(r)) => {
            l.params.required == r.params.required
                && l.params.optional == r.params.optional
                && l.params.rest == r.params.rest
                && l.body == r.body
                && l.env == r.env
                && l.docstring == r.docstring
        }
        _ => false,
    }
}

impl Default for VariableWatcherList {
    fn default() -> Self {
        Self::new()
    }
}

impl GcTrace for AdviceManager {
    fn trace_roots(&self, roots: &mut Vec<Value>) {
        for advice_list in self.advice_map.values() {
            for advice in advice_list {
                roots.push(advice.function);
            }
        }
    }
}

impl GcTrace for VariableWatcherList {
    fn trace_roots(&self, roots: &mut Vec<Value>) {
        for watcher_list in self.watchers.values() {
            for watcher in watcher_list {
                roots.push(watcher.callback);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Builtin functions (eval-dependent)
// ---------------------------------------------------------------------------

use super::error::{signal, EvalResult, Flow};

/// Expect at least N arguments.
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

/// Expect no more than N arguments.
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

/// Expect exactly N arguments.
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

/// Extract a symbol name from a Value.
fn expect_symbol_name(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Symbol(id) => Ok(resolve_sym(*id).to_owned()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), *other],
        )),
    }
}

/// `(advice-add SYMBOL WHERE FUNCTION &optional PROPS)`
///
/// WHERE is one of :before, :after, :around, :override, :filter-args, :filter-return.
/// PROPS is an optional plist; currently only :name is recognized.
pub(crate) fn builtin_advice_add(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("advice-add", &args, 3)?;
    expect_max_args("advice-add", &args, 4)?;

    let target = expect_symbol_name(&args[0])?;

    let where_kw = match &args[1] {
        Value::Keyword(id) => resolve_sym(*id).to_owned(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("keywordp"), *other],
            ));
        }
    };

    let advice_type = AdviceType::from_keyword(&where_kw).ok_or_else(|| {
        signal(
            "error",
            vec![Value::string(format!(
                "advice-add: unknown advice type {}",
                where_kw
            ))],
        )
    })?;

    let advice_fn = args[2];

    // Extract optional :name from PROPS plist
    let name = if args.len() > 3 {
        extract_plist_name(&args[3..])
    } else {
        // Use symbol name of advice function as default name
        match &advice_fn {
            Value::Symbol(id) => Some(resolve_sym(*id).to_owned()),
            _ => None,
        }
    };

    eval.advice
        .add_advice(&target, advice_type, advice_fn, name);
    Ok(Value::Nil)
}

/// Extract :name from a plist-style argument list.
fn extract_plist_name(props: &[Value]) -> Option<String> {
    let mut i = 0;
    while i + 1 < props.len() {
        if matches!(&props[i], Value::Keyword(id) if resolve_sym(*id) == ":name") {
            return match &props[i + 1] {
                Value::Symbol(id) => Some(resolve_sym(*id).to_owned()),
                Value::Str(s) => Some(super::value::with_heap(|h| h.get_string(*s).clone())),
                _ => None,
            };
        }
        i += 2;
    }
    None
}

/// `(advice-remove SYMBOL FUNCTION)`
///
/// Remove advice identified by FUNCTION (a symbol) or by name from SYMBOL.
pub(crate) fn builtin_advice_remove(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("advice-remove", &args, 2)?;

    let target = expect_symbol_name(&args[0])?;
    let fn_or_name = expect_symbol_name(&args[1])?;

    eval.advice.remove_advice(&target, &fn_or_name);
    Ok(Value::Nil)
}

/// `(advice-member-p FUNCTION SYMBOL)`
///
/// Return t if FUNCTION is advising SYMBOL.
pub(crate) fn builtin_advice_member_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("advice-member-p", &args, 2)?;

    let function = expect_symbol_name(&args[0])?;
    let target = expect_symbol_name(&args[1])?;

    Ok(Value::bool(eval.advice.advice_member_p(&target, &function)))
}

/// `(add-variable-watcher SYMBOL WATCH-FUNCTION)`
///
/// Arrange to call WATCH-FUNCTION when SYMBOL is set.
pub(crate) fn builtin_add_variable_watcher(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("add-variable-watcher", &args, 2)?;

    let var_name = expect_symbol_name(&args[0])?;
    let resolved = super::builtins::resolve_variable_alias_name(eval, &var_name)?;
    let callback = args[1];

    eval.watchers.add_watcher(&resolved, callback);
    Ok(Value::Nil)
}

/// `(remove-variable-watcher SYMBOL WATCH-FUNCTION)`
///
/// Remove WATCH-FUNCTION from the watchers of SYMBOL.
pub(crate) fn builtin_remove_variable_watcher(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("remove-variable-watcher", &args, 2)?;

    let var_name = expect_symbol_name(&args[0])?;
    let resolved = super::builtins::resolve_variable_alias_name(eval, &var_name)?;
    let callback = args[1];

    eval.watchers.remove_watcher(&resolved, &callback);
    Ok(Value::Nil)
}

/// `(get-variable-watchers SYMBOL)`
///
/// Return a list of watcher callbacks registered for SYMBOL.
pub(crate) fn builtin_get_variable_watchers(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("get-variable-watchers", &args, 1)?;

    let var_name = expect_symbol_name(&args[0])?;
    let resolved = super::builtins::resolve_variable_alias_name(eval, &var_name)?;
    Ok(Value::list(eval.watchers.get_watchers(&resolved)))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::expr::Expr;
    use super::super::intern::intern;
    use super::super::value::{LambdaData, LambdaParams};

    // -----------------------------------------------------------------------
    // AdviceType tests
    // -----------------------------------------------------------------------

    #[test]
    fn advice_type_from_keyword() {
        assert_eq!(
            AdviceType::from_keyword(":before"),
            Some(AdviceType::Before)
        );
        assert_eq!(AdviceType::from_keyword(":after"), Some(AdviceType::After));
        assert_eq!(
            AdviceType::from_keyword(":around"),
            Some(AdviceType::Around)
        );
        assert_eq!(
            AdviceType::from_keyword(":override"),
            Some(AdviceType::Override)
        );
        assert_eq!(
            AdviceType::from_keyword(":filter-args"),
            Some(AdviceType::FilterArgs)
        );
        assert_eq!(
            AdviceType::from_keyword(":filter-return"),
            Some(AdviceType::FilterReturn)
        );
        assert_eq!(AdviceType::from_keyword(":bogus"), None);
    }

    // -----------------------------------------------------------------------
    // AdviceManager tests
    // -----------------------------------------------------------------------

    #[test]
    fn add_and_get_advice() {
        let mut mgr = AdviceManager::new();
        assert!(!mgr.has_advice("my-fn"));

        mgr.add_advice(
            "my-fn",
            AdviceType::Before,
            Value::symbol("my-before-fn"),
            Some("my-before".to_string()),
        );

        assert!(mgr.has_advice("my-fn"));
        let advice_list = mgr.get_advice("my-fn");
        assert_eq!(advice_list.len(), 1);
        assert_eq!(advice_list[0].advice_type, AdviceType::Before);
        assert_eq!(advice_list[0].name.as_deref(), Some("my-before"));
    }

    #[test]
    fn add_multiple_advice_sorted_by_type() {
        let mut mgr = AdviceManager::new();

        mgr.add_advice("fn", AdviceType::After, Value::symbol("after-fn"), None);
        mgr.add_advice("fn", AdviceType::Before, Value::symbol("before-fn"), None);
        mgr.add_advice(
            "fn",
            AdviceType::FilterArgs,
            Value::symbol("filter-fn"),
            None,
        );
        mgr.add_advice("fn", AdviceType::Around, Value::symbol("around-fn"), None);

        let advice_list = mgr.get_advice("fn");
        assert_eq!(advice_list.len(), 4);
        assert_eq!(advice_list[0].advice_type, AdviceType::FilterArgs);
        assert_eq!(advice_list[1].advice_type, AdviceType::Before);
        assert_eq!(advice_list[2].advice_type, AdviceType::Around);
        assert_eq!(advice_list[3].advice_type, AdviceType::After);
    }

    #[test]
    fn remove_advice_by_name() {
        let mut mgr = AdviceManager::new();

        mgr.add_advice(
            "fn",
            AdviceType::Before,
            Value::symbol("adv1"),
            Some("first".to_string()),
        );
        mgr.add_advice(
            "fn",
            AdviceType::After,
            Value::symbol("adv2"),
            Some("second".to_string()),
        );

        assert_eq!(mgr.get_advice("fn").len(), 2);

        mgr.remove_advice("fn", "first");
        assert_eq!(mgr.get_advice("fn").len(), 1);
        assert_eq!(mgr.get_advice("fn")[0].name.as_deref(), Some("second"));
    }

    #[test]
    fn remove_advice_by_symbol_name() {
        let mut mgr = AdviceManager::new();

        mgr.add_advice("fn", AdviceType::Before, Value::symbol("adv-fn"), None);
        assert!(mgr.has_advice("fn"));

        mgr.remove_advice("fn", "adv-fn");
        assert!(!mgr.has_advice("fn"));
    }

    #[test]
    fn replace_advice_by_name() {
        let mut mgr = AdviceManager::new();

        mgr.add_advice(
            "fn",
            AdviceType::Before,
            Value::symbol("old-fn"),
            Some("my-advice".to_string()),
        );
        mgr.add_advice(
            "fn",
            AdviceType::After,
            Value::symbol("new-fn"),
            Some("my-advice".to_string()),
        );

        // Should have replaced, not added
        assert_eq!(mgr.get_advice("fn").len(), 1);
        assert_eq!(mgr.get_advice("fn")[0].advice_type, AdviceType::After);
    }

    #[test]
    fn advice_member_p_works() {
        let mut mgr = AdviceManager::new();

        mgr.add_advice(
            "fn",
            AdviceType::Before,
            Value::symbol("my-adv"),
            Some("named-adv".to_string()),
        );

        assert!(mgr.advice_member_p("fn", "my-adv"));
        assert!(mgr.advice_member_p("fn", "named-adv"));
        assert!(!mgr.advice_member_p("fn", "nonexistent"));
        assert!(!mgr.advice_member_p("other-fn", "my-adv"));
    }

    #[test]
    fn get_advice_empty() {
        let mgr = AdviceManager::new();
        assert!(mgr.get_advice("nonexistent").is_empty());
        assert!(!mgr.has_advice("nonexistent"));
    }

    // -----------------------------------------------------------------------
    // VariableWatcherList tests
    // -----------------------------------------------------------------------

    #[test]
    fn add_and_notify_watcher() {
        let mut wl = VariableWatcherList::new();
        assert!(!wl.has_watchers("my-var"));

        wl.add_watcher("my-var", Value::symbol("my-watcher"));
        assert!(wl.has_watchers("my-var"));

        let calls = wl.notify_watchers(
            "my-var",
            &Value::Int(42),
            &Value::Int(0),
            "set",
            &Value::Nil,
        );
        assert_eq!(calls.len(), 1);

        let (callback, args) = &calls[0];
        assert!(matches!(callback, Value::Symbol(id) if resolve_sym(*id) == "my-watcher"));
        assert_eq!(args.len(), 4);
        // arg 0: symbol name
        assert!(matches!(&args[0], Value::Symbol(id) if resolve_sym(*id) == "my-var"));
        // arg 1: new value
        assert!(matches!(&args[1], Value::Int(42)));
        // arg 2: operation
        assert!(matches!(&args[2], Value::Symbol(id) if resolve_sym(*id) == "set"));
        // arg 3: where (nil)
        assert!(matches!(&args[3], Value::Nil));
    }

    #[test]
    fn remove_watcher() {
        let mut wl = VariableWatcherList::new();
        wl.add_watcher("my-var", Value::symbol("watcher1"));
        wl.add_watcher("my-var", Value::symbol("watcher2"));
        assert!(wl.has_watchers("my-var"));

        wl.remove_watcher("my-var", &Value::symbol("watcher1"));
        let calls = wl.notify_watchers("my-var", &Value::Int(1), &Value::Int(0), "set", &Value::Nil);
        assert_eq!(calls.len(), 1);
        assert!(matches!(&calls[0].0, Value::Symbol(id) if resolve_sym(*id) == "watcher2"));
    }

    #[test]
    fn remove_all_watchers_cleans_up() {
        let mut wl = VariableWatcherList::new();
        wl.add_watcher("my-var", Value::symbol("w1"));

        wl.remove_watcher("my-var", &Value::symbol("w1"));
        assert!(!wl.has_watchers("my-var"));
    }

    #[test]
    fn no_duplicate_watchers() {
        let mut wl = VariableWatcherList::new();
        wl.add_watcher("my-var", Value::symbol("w"));
        wl.add_watcher("my-var", Value::symbol("w"));

        let calls = wl.notify_watchers("my-var", &Value::Int(1), &Value::Int(0), "set", &Value::Nil);
        assert_eq!(calls.len(), 1);
    }

    #[test]
    fn no_duplicate_equivalent_lambda_watchers() {
        let mut wl = VariableWatcherList::new();
        let callback_a = Value::make_lambda(LambdaData {
            params: LambdaParams {
                required: vec![
                    intern("symbol"),
                    intern("newval"),
                    intern("operation"),
                    intern("where"),
                ],
                optional: Vec::new(),
                rest: None,
            },
            body: vec![Expr::Int(0)],
            env: None,
            docstring: None,
        });
        let callback_b = Value::make_lambda(LambdaData {
            params: LambdaParams {
                required: vec![
                    intern("symbol"),
                    intern("newval"),
                    intern("operation"),
                    intern("where"),
                ],
                optional: Vec::new(),
                rest: None,
            },
            body: vec![Expr::Int(0)],
            env: None,
            docstring: None,
        });

        wl.add_watcher("my-var", callback_a);
        wl.add_watcher("my-var", callback_b);
        assert_eq!(wl.get_watchers("my-var"), vec![callback_a]);
    }

    #[test]
    fn notify_no_watchers_returns_empty() {
        let wl = VariableWatcherList::new();
        let calls = wl.notify_watchers("no-var", &Value::Int(1), &Value::Int(0), "set", &Value::Nil);
        assert!(calls.is_empty());
    }

    #[test]
    fn multiple_watchers_all_notified() {
        let mut wl = VariableWatcherList::new();
        wl.add_watcher("v", Value::symbol("w1"));
        wl.add_watcher("v", Value::symbol("w2"));
        wl.add_watcher("v", Value::symbol("w3"));

        let calls = wl.notify_watchers("v", &Value::Int(99), &Value::Int(0), "set", &Value::Nil);
        assert_eq!(calls.len(), 3);
    }

    #[test]
    fn get_watchers_returns_callbacks_in_registration_order() {
        let mut wl = VariableWatcherList::new();
        wl.add_watcher("v", Value::symbol("w1"));
        wl.add_watcher("v", Value::symbol("w2"));

        let watchers = wl.get_watchers("v");
        assert_eq!(watchers, vec![Value::symbol("w1"), Value::symbol("w2")]);
        assert!(wl.get_watchers("missing").is_empty());
    }

    #[test]
    fn builtin_get_variable_watchers_tracks_runtime_registry() {
        let mut eval = super::super::eval::Evaluator::new();
        builtin_add_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watched-var"), Value::symbol("watch-a")],
        )
        .unwrap();
        builtin_add_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watched-var"), Value::symbol("watch-b")],
        )
        .unwrap();

        let watchers =
            builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watched-var")])
                .unwrap();
        let watchers_vec = super::super::value::list_to_vec(&watchers).expect("watcher list");
        assert_eq!(
            watchers_vec,
            vec![Value::symbol("watch-a"), Value::symbol("watch-b")]
        );

        builtin_remove_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watched-var"), Value::symbol("watch-a")],
        )
        .unwrap();
        let remaining =
            builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watched-var")])
                .unwrap();
        assert_eq!(
            super::super::value::list_to_vec(&remaining).expect("watcher list"),
            vec![Value::symbol("watch-b")]
        );

        let wrong_type = builtin_get_variable_watchers(&mut eval, vec![Value::Int(1)]).unwrap_err();
        match wrong_type {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn variable_watcher_builtins_follow_runtime_alias_resolution() {
        let mut eval = super::super::eval::Evaluator::new();
        super::super::builtins::builtin_defvaralias_eval(
            &mut eval,
            vec![
                Value::symbol("vm-watch-alias"),
                Value::symbol("vm-watch-base"),
            ],
        )
        .expect("defvaralias should install alias edge");

        builtin_add_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watch-alias"), Value::symbol("watch-a")],
        )
        .expect("add-variable-watcher should resolve alias");

        let via_alias = builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watch-alias")])
            .expect("get-variable-watchers should resolve alias");
        assert_eq!(
            super::super::value::list_to_vec(&via_alias).expect("watcher list"),
            vec![Value::symbol("watch-a")]
        );

        let via_base = builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watch-base")])
            .expect("get-variable-watchers should resolve base");
        assert_eq!(
            super::super::value::list_to_vec(&via_base).expect("watcher list"),
            vec![Value::symbol("watch-a")]
        );

        builtin_remove_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watch-alias"), Value::symbol("watch-a")],
        )
        .expect("remove-variable-watcher should resolve alias");
        let remaining =
            builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watch-base")])
                .expect("get-variable-watchers should return empty after removal");
        assert!(remaining.is_nil());
    }

    #[test]
    fn remove_variable_watcher_accepts_non_symbol_callbacks() {
        let mut eval = super::super::eval::Evaluator::new();
        let callback = Value::make_lambda(LambdaData {
            params: LambdaParams {
                required: vec![
                    intern("symbol"),
                    intern("newval"),
                    intern("operation"),
                    intern("where"),
                ],
                optional: Vec::new(),
                rest: None,
            },
            body: vec![Expr::Symbol(intern("newval"))],
            env: None,
            docstring: None,
        });
        let equivalent_callback = Value::make_lambda(LambdaData {
            params: LambdaParams {
                required: vec![
                    intern("symbol"),
                    intern("newval"),
                    intern("operation"),
                    intern("where"),
                ],
                optional: Vec::new(),
                rest: None,
            },
            body: vec![Expr::Symbol(intern("newval"))],
            env: None,
            docstring: None,
        });

        builtin_add_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watch-nonsym"), callback],
        )
        .expect("add-variable-watcher should accept lambda callbacks");
        let before =
            builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watch-nonsym")])
                .expect("get-variable-watchers should return lambda callback");
        assert_eq!(
            super::super::value::list_to_vec(&before).expect("watcher list"),
            vec![callback]
        );

        builtin_remove_variable_watcher(
            &mut eval,
            vec![Value::symbol("vm-watch-nonsym"), equivalent_callback],
        )
        .expect("remove-variable-watcher should remove equivalent lambda callbacks");
        let after = builtin_get_variable_watchers(&mut eval, vec![Value::symbol("vm-watch-nonsym")])
            .expect("get-variable-watchers should be empty after removal");
        assert!(after.is_nil());
    }
}
