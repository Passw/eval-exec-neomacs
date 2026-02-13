//! Debugger and help system.
//!
//! Implements:
//! - Backtrace frames and stack introspection
//! - describe-function, describe-variable
//! - Debug-on-entry, debug-on-error
//! - Breakpoints and stepping
//! - Apropos searching
//! - Doc string storage and retrieval

use std::collections::{HashMap, HashSet};

use super::error::{signal, EvalResult, Flow};
use super::print::print_value;
use super::value::Value;

// ---------------------------------------------------------------------------
// Argument validation helpers (local to this module)
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

fn expect_symbol_name(value: &Value) -> Result<String, Flow> {
    match value.as_symbol_name() {
        Some(s) => Ok(s.to_string()),
        None => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), value.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Backtrace
// ---------------------------------------------------------------------------

/// A single stack frame in a backtrace.
#[derive(Clone, Debug)]
pub struct BacktraceFrame {
    /// Name of the function being called.
    pub function: String,
    /// Arguments passed to the function.
    pub args: Vec<Value>,
    /// Source file (if known).
    pub file: Option<String>,
    /// Source line (if known).
    pub line: Option<usize>,
    /// Whether this is a special form (e.g. `if`, `let`).
    pub is_special_form: bool,
}

/// A collection of backtrace frames representing the call stack.
#[derive(Clone, Debug)]
pub struct Backtrace {
    frames: Vec<BacktraceFrame>,
    max_depth: usize,
}

impl Default for Backtrace {
    fn default() -> Self {
        Self::new()
    }
}

impl Backtrace {
    /// Create a new empty backtrace with the default max depth.
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            max_depth: 100,
        }
    }

    /// Create a backtrace with a custom max depth.
    pub fn with_max_depth(max_depth: usize) -> Self {
        Self {
            frames: Vec::new(),
            max_depth,
        }
    }

    /// Push a frame onto the backtrace.  Silently drops frames beyond max depth.
    pub fn push(&mut self, frame: BacktraceFrame) {
        if self.frames.len() < self.max_depth {
            self.frames.push(frame);
        }
    }

    /// Pop the most recent frame.
    pub fn pop(&mut self) -> Option<BacktraceFrame> {
        self.frames.pop()
    }

    /// Current depth (number of frames).
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Access the frames slice.
    pub fn frames(&self) -> &[BacktraceFrame] {
        &self.frames
    }

    /// Format the backtrace as a human-readable string, most recent frame first.
    pub fn format(&self) -> String {
        if self.frames.is_empty() {
            return "  (no backtrace)\n".to_string();
        }
        let mut out = String::new();
        // Print newest frame first (like Emacs *Backtrace* buffer)
        for (i, frame) in self.frames.iter().rev().enumerate() {
            let kind = if frame.is_special_form {
                "  "
            } else {
                "  "
            };
            let args_str = frame
                .args
                .iter()
                .map(|v| print_value(v))
                .collect::<Vec<_>>()
                .join(" ");
            let loc = match (&frame.file, frame.line) {
                (Some(f), Some(l)) => format!(" [{}:{}]", f, l),
                (Some(f), None) => format!(" [{}]", f),
                _ => String::new(),
            };
            let marker = if frame.is_special_form { "*" } else { "" };
            out.push_str(&format!(
                "{}{}{}({}{}){}\n",
                kind,
                marker,
                if marker.is_empty() { "" } else { " " },
                frame.function,
                if args_str.is_empty() {
                    String::new()
                } else {
                    format!(" {}", args_str)
                },
                loc,
            ));
            // Guard against huge backtraces in display
            if i >= 99 {
                out.push_str("  ...(truncated)\n");
                break;
            }
        }
        out
    }

    /// Remove all frames.
    pub fn clear(&mut self) {
        self.frames.clear();
    }
}

// ---------------------------------------------------------------------------
// DebugAction
// ---------------------------------------------------------------------------

/// What the debugger should do when triggered.
#[derive(Clone, Debug)]
pub enum DebugAction {
    /// Continue execution normally.
    Continue,
    /// Step into the next form.
    Step,
    /// Step over the next form (evaluate it, then stop).
    Next,
    /// Finish the current function and stop at caller.
    Finish,
    /// Abort evaluation entirely.
    Quit,
    /// Evaluate an expression string in the current context.
    Eval(String),
}

// ---------------------------------------------------------------------------
// Breakpoint
// ---------------------------------------------------------------------------

/// A breakpoint set on a function.
#[derive(Clone, Debug)]
pub struct Breakpoint {
    /// Unique identifier.
    pub id: usize,
    /// The function this breakpoint is set on.
    pub function: String,
    /// Whether the breakpoint is currently enabled.
    pub enabled: bool,
    /// Optional condition expression (source string).
    pub condition: Option<String>,
    /// Number of times this breakpoint has been hit.
    pub hit_count: usize,
}

// ---------------------------------------------------------------------------
// DebugState
// ---------------------------------------------------------------------------

/// Central debug/introspection state for the evaluator.
pub struct DebugState {
    /// Whether the debugger is currently active (stopped at a breakpoint/error).
    pub active: bool,
    /// If true, enter debugger on any unhandled error signal.
    pub debug_on_error: bool,
    /// If true, enter debugger on quit (C-g).
    pub debug_on_quit: bool,
    /// Set of function names that should trigger the debugger on entry.
    pub debug_on_entry: HashSet<String>,
    /// Set of signal symbols that should trigger the debugger.
    pub debug_on_signal: HashSet<String>,
    /// Whether we are in single-step mode.
    pub stepping: bool,
    /// The current backtrace (populated during evaluation).
    pub current_backtrace: Backtrace,
    /// All breakpoints.
    pub breakpoints: Vec<Breakpoint>,
    /// Next breakpoint id.
    next_bp_id: usize,
}

impl Default for DebugState {
    fn default() -> Self {
        Self::new()
    }
}

impl DebugState {
    /// Create a new debug state with everything disabled.
    pub fn new() -> Self {
        Self {
            active: false,
            debug_on_error: false,
            debug_on_quit: false,
            debug_on_entry: HashSet::new(),
            debug_on_signal: HashSet::new(),
            stepping: false,
            current_backtrace: Backtrace::new(),
            breakpoints: Vec::new(),
            next_bp_id: 1,
        }
    }

    /// Check whether the debugger should be entered when `function` is called.
    pub fn should_debug_on_entry(&self, function: &str) -> bool {
        if self.debug_on_entry.contains(function) {
            return true;
        }
        // Also check breakpoints
        self.breakpoints
            .iter()
            .any(|bp| bp.enabled && bp.function == function)
    }

    /// Check whether the debugger should be entered for a given error signal.
    pub fn should_debug_on_error(&self, signal_name: &str) -> bool {
        if self.debug_on_error {
            return true;
        }
        self.debug_on_signal.contains(signal_name)
    }

    /// Mark a function for debug-on-entry.
    pub fn add_debug_on_entry(&mut self, function: &str) {
        self.debug_on_entry.insert(function.to_string());
    }

    /// Remove a function from debug-on-entry.
    pub fn remove_debug_on_entry(&mut self, function: &str) {
        self.debug_on_entry.remove(function);
    }

    /// Add a breakpoint on a function.  Returns the breakpoint id.
    pub fn add_breakpoint(&mut self, function: &str) -> usize {
        let id = self.next_bp_id;
        self.next_bp_id += 1;
        self.breakpoints.push(Breakpoint {
            id,
            function: function.to_string(),
            enabled: true,
            condition: None,
            hit_count: 0,
        });
        id
    }

    /// Add a breakpoint with a condition expression.  Returns the breakpoint id.
    pub fn add_conditional_breakpoint(&mut self, function: &str, condition: &str) -> usize {
        let id = self.next_bp_id;
        self.next_bp_id += 1;
        self.breakpoints.push(Breakpoint {
            id,
            function: function.to_string(),
            enabled: true,
            condition: Some(condition.to_string()),
            hit_count: 0,
        });
        id
    }

    /// Remove a breakpoint by id.  Returns true if found and removed.
    pub fn remove_breakpoint(&mut self, id: usize) -> bool {
        let before = self.breakpoints.len();
        self.breakpoints.retain(|bp| bp.id != id);
        self.breakpoints.len() < before
    }

    /// Toggle a breakpoint's enabled state.  Returns true if the breakpoint was found.
    pub fn toggle_breakpoint(&mut self, id: usize) -> bool {
        for bp in &mut self.breakpoints {
            if bp.id == id {
                bp.enabled = !bp.enabled;
                return true;
            }
        }
        false
    }

    /// Record a breakpoint hit (increment hit_count).
    pub fn record_breakpoint_hit(&mut self, function: &str) {
        for bp in &mut self.breakpoints {
            if bp.enabled && bp.function == function {
                bp.hit_count += 1;
            }
        }
    }

    /// List all breakpoints.
    pub fn list_breakpoints(&self) -> &[Breakpoint] {
        &self.breakpoints
    }
}

// ---------------------------------------------------------------------------
// DocStore
// ---------------------------------------------------------------------------

/// Storage for documentation strings (function and variable docs).
pub struct DocStore {
    function_docs: HashMap<String, String>,
    variable_docs: HashMap<String, String>,
}

impl Default for DocStore {
    fn default() -> Self {
        Self::new()
    }
}

impl DocStore {
    /// Create a new empty doc store.
    pub fn new() -> Self {
        Self {
            function_docs: HashMap::new(),
            variable_docs: HashMap::new(),
        }
    }

    /// Set the documentation string for a function.
    pub fn set_function_doc(&mut self, name: &str, doc: &str) {
        self.function_docs.insert(name.to_string(), doc.to_string());
    }

    /// Set the documentation string for a variable.
    pub fn set_variable_doc(&mut self, name: &str, doc: &str) {
        self.variable_docs.insert(name.to_string(), doc.to_string());
    }

    /// Get the documentation string for a function.
    pub fn get_function_doc(&self, name: &str) -> Option<&str> {
        self.function_docs.get(name).map(|s| s.as_str())
    }

    /// Get the documentation string for a variable.
    pub fn get_variable_doc(&self, name: &str) -> Option<&str> {
        self.variable_docs.get(name).map(|s| s.as_str())
    }

    /// Search for symbols whose names contain `pattern` (case-insensitive substring).
    /// Returns a vec of (name, has_function_doc, has_variable_doc).
    pub fn apropos(&self, pattern: &str) -> Vec<(String, bool, bool)> {
        let pattern_lower = pattern.to_lowercase();
        let mut seen: HashMap<String, (bool, bool)> = HashMap::new();

        for name in self.function_docs.keys() {
            if name.to_lowercase().contains(&pattern_lower) {
                let entry = seen.entry(name.clone()).or_insert((false, false));
                entry.0 = true;
            }
        }
        for name in self.variable_docs.keys() {
            if name.to_lowercase().contains(&pattern_lower) {
                let entry = seen.entry(name.clone()).or_insert((false, false));
                entry.1 = true;
            }
        }

        let mut results: Vec<(String, bool, bool)> = seen
            .into_iter()
            .map(|(name, (has_func, has_var))| (name, has_func, has_var))
            .collect();
        results.sort_by(|a, b| a.0.cmp(&b.0));
        results
    }

    /// Return all function names that have documentation, sorted.
    pub fn all_documented_functions(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.function_docs.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    /// Return all variable names that have documentation, sorted.
    pub fn all_documented_variables(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.variable_docs.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    /// Remove documentation for a function.
    pub fn remove_function_doc(&mut self, name: &str) -> bool {
        self.function_docs.remove(name).is_some()
    }

    /// Remove documentation for a variable.
    pub fn remove_variable_doc(&mut self, name: &str) -> bool {
        self.variable_docs.remove(name).is_some()
    }
}

// ---------------------------------------------------------------------------
// HelpFormatter
// ---------------------------------------------------------------------------

/// Formats help buffer content (describe-function, describe-variable, etc.).
pub struct HelpFormatter;

impl HelpFormatter {
    /// Format a `describe-function` help string.
    pub fn describe_function(name: &str, value: &Value, doc: Option<&str>) -> String {
        let mut out = String::new();

        let kind = match value {
            Value::Lambda(lam) => {
                if lam.env.is_some() {
                    "a Lisp closure"
                } else {
                    "a Lisp function"
                }
            }
            Value::Subr(_) => "a built-in function",
            Value::Macro(_) => "a Lisp macro",
            Value::ByteCode(_) => "a compiled Lisp function",
            _ => "a Lisp function",
        };

        out.push_str(&format!("{} is {}.\n\n", name, kind));

        // Signature
        match value {
            Value::Lambda(lam) | Value::Macro(lam) => {
                let params = format_param_list(&lam.params);
                out.push_str(&format!("({}{})\n", name, params));
            }
            Value::ByteCode(bc) => {
                let params = format_param_list(&bc.params);
                out.push_str(&format!("({}{})\n", name, params));
            }
            Value::Subr(subr_name) => {
                out.push_str(&format!("({} &rest ARGS)\n", subr_name));
            }
            _ => {
                out.push_str(&format!("({})\n", name));
            }
        }

        // Docstring from LambdaData
        let inline_doc = match value {
            Value::Lambda(lam) | Value::Macro(lam) => lam.docstring.as_deref(),
            _ => None,
        };

        // Prefer the docstore doc, fall back to inline
        let doc_text = doc.or(inline_doc);

        if let Some(d) = doc_text {
            out.push('\n');
            out.push_str(d);
            if !d.ends_with('\n') {
                out.push('\n');
            }
        } else {
            out.push_str("\nNot documented.\n");
        }

        out
    }

    /// Format a `describe-variable` help string.
    pub fn describe_variable(name: &str, value: &Value, doc: Option<&str>) -> String {
        let mut out = String::new();

        let printed = print_value(value);
        out.push_str(&format!("{}'s value is {}\n", name, printed));

        if let Some(d) = doc {
            out.push_str("\nDocumentation:\n");
            out.push_str(d);
            if !d.ends_with('\n') {
                out.push('\n');
            }
        } else {
            out.push_str("\nNot documented.\n");
        }

        out
    }

    /// Format a `describe-key` help string.
    pub fn describe_key(key: &str, binding: &str, doc: Option<&str>) -> String {
        let mut out = String::new();

        out.push_str(&format!("{} runs the command {}\n", key, binding));

        if let Some(d) = doc {
            out.push('\n');
            out.push_str(d);
            if !d.ends_with('\n') {
                out.push('\n');
            }
        }

        out
    }

    /// Format an apropos result listing.
    pub fn format_apropos(entries: &[(String, bool, bool)]) -> String {
        if entries.is_empty() {
            return "No matches.\n".to_string();
        }
        let mut out = String::new();
        for (name, has_func, has_var) in entries {
            let mut kinds = Vec::new();
            if *has_func {
                kinds.push("Function");
            }
            if *has_var {
                kinds.push("Variable");
            }
            out.push_str(&format!("{}\n  {}\n", name, kinds.join(", ")));
        }
        out
    }
}

/// Format a parameter list for display in help output.
fn format_param_list(params: &super::value::LambdaParams) -> String {
    let mut parts = Vec::new();
    for p in &params.required {
        parts.push(p.to_uppercase());
    }
    if !params.optional.is_empty() {
        parts.push("&optional".to_string());
        for p in &params.optional {
            parts.push(p.to_uppercase());
        }
    }
    if let Some(ref rest) = params.rest {
        parts.push("&rest".to_string());
        parts.push(rest.to_uppercase());
    }
    if parts.is_empty() {
        String::new()
    } else {
        format!(" {}", parts.join(" "))
    }
}

// ---------------------------------------------------------------------------
// Built-in functions
// ---------------------------------------------------------------------------

/// `(backtrace)` -- return a formatted backtrace string.
///
/// In a real integration this reads from the evaluator's debug state;
/// here it returns a placeholder since we don't have access to the evaluator.
pub(crate) fn builtin_backtrace(args: Vec<Value>) -> EvalResult {
    // Accept 0 args
    if !args.is_empty() {
        expect_args("backtrace", &args, 0)?;
    }
    // Without evaluator access, return a stub message.
    // In practice, the evaluator calls this with its own debug state.
    Ok(Value::string("Backtrace not available outside debugger context."))
}

/// `(debug &rest ARGS)` -- enter the debugger.
///
/// In a full implementation this would suspend evaluation and present
/// the debugger UI.  Here it signals `debug` so callers can catch it.
pub(crate) fn builtin_debug(args: Vec<Value>) -> EvalResult {
    let message = if args.is_empty() {
        "Entering debugger".to_string()
    } else {
        args.iter()
            .map(|v| print_value(v))
            .collect::<Vec<_>>()
            .join(" ")
    };
    Ok(Value::string(message))
}

/// `(debug-on-entry FUNCTION)` -- mark FUNCTION for debug-on-entry.
///
/// Returns the function name symbol.  The evaluator hooks this to its DebugState.
pub(crate) fn builtin_debug_on_entry(args: Vec<Value>) -> EvalResult {
    expect_args("debug-on-entry", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    // Return the name so the evaluator can wire it up
    Ok(Value::symbol(name))
}

/// `(cancel-debug-on-entry FUNCTION)` -- remove debug-on-entry for FUNCTION.
pub(crate) fn builtin_cancel_debug_on_entry(args: Vec<Value>) -> EvalResult {
    expect_args("cancel-debug-on-entry", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    Ok(Value::symbol(name))
}

/// `(debug-on-error-p)` -- return t if debug-on-error is enabled, nil otherwise.
///
/// The evaluator overrides this to consult its own DebugState.
pub(crate) fn builtin_debug_on_error_p(args: Vec<Value>) -> EvalResult {
    expect_args("debug-on-error-p", &args, 0)?;
    // Default: nil (disabled).  Evaluator replaces at dispatch time.
    Ok(Value::Nil)
}

/// `(describe-function SYMBOL)` -- return a description string for a function.
///
/// Without evaluator context, returns a stub.  The evaluator supplies the
/// actual function value and docstring.
pub(crate) fn builtin_describe_function(args: Vec<Value>) -> EvalResult {
    expect_args("describe-function", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    // Minimal stub -- the evaluator enriches this
    Ok(Value::string(format!("{} is a function.", name)))
}

/// `(describe-variable SYMBOL)` -- return a description string for a variable.
pub(crate) fn builtin_describe_variable(args: Vec<Value>) -> EvalResult {
    expect_args("describe-variable", &args, 1)?;
    let name = expect_symbol_name(&args[0])?;
    Ok(Value::string(format!("{} is a variable.", name)))
}

/// `(documentation FUNCTION &optional RAW)` -- return the docstring for FUNCTION.
pub(crate) fn builtin_documentation(args: Vec<Value>) -> EvalResult {
    expect_min_args("documentation", &args, 1)?;
    // Extract docstring from lambda/macro if the value is one
    match &args[0] {
        Value::Lambda(lam) | Value::Macro(lam) => match &lam.docstring {
            Some(doc) => Ok(Value::string(doc.as_str())),
            None => Ok(Value::Nil),
        },
        Value::Symbol(name) => {
            // When given a symbol, we can't look up the function cell without
            // the evaluator.  Return nil as a stub.
            let _ = name;
            Ok(Value::Nil)
        }
        _ => Ok(Value::Nil),
    }
}

/// `(documentation-property SYMBOL PROP &optional RAW)` -- return a plist doc.
///
/// In Emacs, this looks up PROP on SYMBOL's plist (usually `variable-documentation`
/// or `function-documentation`).  Stub: always returns nil.
pub(crate) fn builtin_documentation_property(args: Vec<Value>) -> EvalResult {
    expect_min_args("documentation-property", &args, 2)?;
    // Stub -- the evaluator can override via plist lookup
    Ok(Value::Nil)
}

/// `(apropos PATTERN)` -- search for symbols matching PATTERN.
///
/// Returns a list of matching symbol names.  Without evaluator context
/// this is a stub; the evaluator supplies the full obarray.
pub(crate) fn builtin_apropos(args: Vec<Value>) -> EvalResult {
    expect_min_args("apropos", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub -- evaluator fills in real results from obarray + docstore
    Ok(Value::Nil)
}

/// `(commandp OBJECT &optional FOR-CALL-INTERACTIVELY)` -- return t if OBJECT is a command.
///
/// A command is an interactive function.  In our simplified VM, we check if the
/// value is a function (lambda, subr, bytecode).  Full Emacs also checks for
/// `interactive` declaration.
pub(crate) fn builtin_commandp(args: Vec<Value>) -> EvalResult {
    expect_min_args("commandp", &args, 1)?;
    let val = &args[0];
    Ok(Value::bool(val.is_function()))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::value::{LambdaData, LambdaParams};
    use std::sync::Arc;

    // -- Backtrace tests --

    #[test]
    fn backtrace_push_pop() {
        let mut bt = Backtrace::new();
        assert_eq!(bt.depth(), 0);

        bt.push(BacktraceFrame {
            function: "foo".to_string(),
            args: vec![Value::Int(1)],
            file: None,
            line: None,
            is_special_form: false,
        });
        assert_eq!(bt.depth(), 1);

        bt.push(BacktraceFrame {
            function: "bar".to_string(),
            args: vec![Value::Int(2), Value::Int(3)],
            file: Some("test.el".to_string()),
            line: Some(42),
            is_special_form: false,
        });
        assert_eq!(bt.depth(), 2);

        let top = bt.pop().unwrap();
        assert_eq!(top.function, "bar");
        assert_eq!(bt.depth(), 1);

        let bottom = bt.pop().unwrap();
        assert_eq!(bottom.function, "foo");
        assert_eq!(bt.depth(), 0);

        assert!(bt.pop().is_none());
    }

    #[test]
    fn backtrace_max_depth() {
        let mut bt = Backtrace::with_max_depth(3);
        for i in 0..10 {
            bt.push(BacktraceFrame {
                function: format!("fn{}", i),
                args: vec![],
                file: None,
                line: None,
                is_special_form: false,
            });
        }
        assert_eq!(bt.depth(), 3);
    }

    #[test]
    fn backtrace_format_nonempty() {
        let mut bt = Backtrace::new();
        bt.push(BacktraceFrame {
            function: "+".to_string(),
            args: vec![Value::Int(1), Value::Int(2)],
            file: None,
            line: None,
            is_special_form: false,
        });
        bt.push(BacktraceFrame {
            function: "my-add".to_string(),
            args: vec![Value::Int(1), Value::Int(2)],
            file: Some("test.el".to_string()),
            line: Some(10),
            is_special_form: false,
        });
        let formatted = bt.format();
        assert!(formatted.contains("my-add"));
        assert!(formatted.contains("+"));
        assert!(formatted.contains("test.el"));
    }

    #[test]
    fn backtrace_format_empty() {
        let bt = Backtrace::new();
        let formatted = bt.format();
        assert!(formatted.contains("no backtrace"));
    }

    #[test]
    fn backtrace_format_special_form() {
        let mut bt = Backtrace::new();
        bt.push(BacktraceFrame {
            function: "if".to_string(),
            args: vec![Value::True],
            file: None,
            line: None,
            is_special_form: true,
        });
        let formatted = bt.format();
        assert!(formatted.contains("*"));
        assert!(formatted.contains("if"));
    }

    #[test]
    fn backtrace_clear() {
        let mut bt = Backtrace::new();
        bt.push(BacktraceFrame {
            function: "foo".to_string(),
            args: vec![],
            file: None,
            line: None,
            is_special_form: false,
        });
        assert_eq!(bt.depth(), 1);
        bt.clear();
        assert_eq!(bt.depth(), 0);
    }

    // -- DebugState tests --

    #[test]
    fn debug_on_entry_add_remove() {
        let mut ds = DebugState::new();
        assert!(!ds.should_debug_on_entry("foo"));

        ds.add_debug_on_entry("foo");
        assert!(ds.should_debug_on_entry("foo"));
        assert!(!ds.should_debug_on_entry("bar"));

        ds.remove_debug_on_entry("foo");
        assert!(!ds.should_debug_on_entry("foo"));
    }

    #[test]
    fn debug_on_error_check() {
        let mut ds = DebugState::new();
        assert!(!ds.should_debug_on_error("void-function"));

        ds.debug_on_error = true;
        assert!(ds.should_debug_on_error("void-function"));
        assert!(ds.should_debug_on_error("wrong-type-argument"));

        ds.debug_on_error = false;
        ds.debug_on_signal.insert("void-function".to_string());
        assert!(ds.should_debug_on_error("void-function"));
        assert!(!ds.should_debug_on_error("wrong-type-argument"));
    }

    #[test]
    fn debug_on_entry_via_breakpoint() {
        let mut ds = DebugState::new();
        let bp_id = ds.add_breakpoint("my-fn");
        assert!(ds.should_debug_on_entry("my-fn"));
        assert!(!ds.should_debug_on_entry("other-fn"));

        // Disable the breakpoint
        ds.toggle_breakpoint(bp_id);
        assert!(!ds.should_debug_on_entry("my-fn"));

        // Re-enable
        ds.toggle_breakpoint(bp_id);
        assert!(ds.should_debug_on_entry("my-fn"));
    }

    // -- Breakpoint tests --

    #[test]
    fn breakpoint_add_remove() {
        let mut ds = DebugState::new();
        let id1 = ds.add_breakpoint("foo");
        let id2 = ds.add_breakpoint("bar");
        assert_eq!(ds.list_breakpoints().len(), 2);

        assert!(ds.remove_breakpoint(id1));
        assert_eq!(ds.list_breakpoints().len(), 1);
        assert_eq!(ds.list_breakpoints()[0].function, "bar");

        // Removing non-existent returns false
        assert!(!ds.remove_breakpoint(999));

        assert!(ds.remove_breakpoint(id2));
        assert!(ds.list_breakpoints().is_empty());
    }

    #[test]
    fn breakpoint_toggle() {
        let mut ds = DebugState::new();
        let id = ds.add_breakpoint("test-fn");
        assert!(ds.list_breakpoints()[0].enabled);

        assert!(ds.toggle_breakpoint(id));
        assert!(!ds.list_breakpoints()[0].enabled);

        assert!(ds.toggle_breakpoint(id));
        assert!(ds.list_breakpoints()[0].enabled);

        // Toggle non-existent
        assert!(!ds.toggle_breakpoint(999));
    }

    #[test]
    fn breakpoint_hit_count() {
        let mut ds = DebugState::new();
        let _id = ds.add_breakpoint("count-me");
        assert_eq!(ds.list_breakpoints()[0].hit_count, 0);

        ds.record_breakpoint_hit("count-me");
        assert_eq!(ds.list_breakpoints()[0].hit_count, 1);

        ds.record_breakpoint_hit("count-me");
        ds.record_breakpoint_hit("count-me");
        assert_eq!(ds.list_breakpoints()[0].hit_count, 3);

        // Hitting a non-existent function is a no-op
        ds.record_breakpoint_hit("other");
        assert_eq!(ds.list_breakpoints()[0].hit_count, 3);
    }

    #[test]
    fn breakpoint_conditional() {
        let mut ds = DebugState::new();
        let id = ds.add_conditional_breakpoint("my-fn", "(> x 5)");
        let bp = &ds.list_breakpoints()[0];
        assert_eq!(bp.id, id);
        assert_eq!(bp.condition.as_deref(), Some("(> x 5)"));
    }

    // -- DocStore tests --

    #[test]
    fn docstore_set_get_function() {
        let mut store = DocStore::new();
        assert!(store.get_function_doc("car").is_none());

        store.set_function_doc("car", "Return the car of LIST.");
        assert_eq!(store.get_function_doc("car"), Some("Return the car of LIST."));

        // Overwrite
        store.set_function_doc("car", "Updated doc.");
        assert_eq!(store.get_function_doc("car"), Some("Updated doc."));
    }

    #[test]
    fn docstore_set_get_variable() {
        let mut store = DocStore::new();
        assert!(store.get_variable_doc("load-path").is_none());

        store.set_variable_doc("load-path", "List of directories to search.");
        assert_eq!(
            store.get_variable_doc("load-path"),
            Some("List of directories to search.")
        );
    }

    #[test]
    fn docstore_apropos_basic() {
        let mut store = DocStore::new();
        store.set_function_doc("car", "Return the car.");
        store.set_function_doc("cdr", "Return the cdr.");
        store.set_function_doc("cons", "Create a cons cell.");
        store.set_variable_doc("car-mode", "Mode for cars.");

        let results = store.apropos("car");
        // Should match "car" (func), "car-mode" (var)
        assert_eq!(results.len(), 2);
        // Results are sorted
        assert_eq!(results[0].0, "car");
        assert!(results[0].1); // has func
        assert!(!results[0].2); // no var

        assert_eq!(results[1].0, "car-mode");
        assert!(!results[1].1); // no func
        assert!(results[1].2); // has var
    }

    #[test]
    fn docstore_apropos_case_insensitive() {
        let mut store = DocStore::new();
        store.set_function_doc("Buffer-Name", "Return buffer name.");
        store.set_function_doc("buffer-size", "Return buffer size.");

        let results = store.apropos("buffer");
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn docstore_apropos_both() {
        let mut store = DocStore::new();
        store.set_function_doc("fill-column", "Get fill column.");
        store.set_variable_doc("fill-column", "Column for fill.");

        let results = store.apropos("fill");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, "fill-column");
        assert!(results[0].1); // has func
        assert!(results[0].2); // has var
    }

    #[test]
    fn docstore_apropos_no_match() {
        let mut store = DocStore::new();
        store.set_function_doc("car", "car doc");
        let results = store.apropos("zzz-nonexistent");
        assert!(results.is_empty());
    }

    #[test]
    fn docstore_all_documented() {
        let mut store = DocStore::new();
        store.set_function_doc("cdr", "cdr doc");
        store.set_function_doc("car", "car doc");
        store.set_variable_doc("x", "x doc");
        store.set_variable_doc("a", "a doc");

        let fns = store.all_documented_functions();
        assert_eq!(fns, vec!["car", "cdr"]);

        let vars = store.all_documented_variables();
        assert_eq!(vars, vec!["a", "x"]);
    }

    #[test]
    fn docstore_remove() {
        let mut store = DocStore::new();
        store.set_function_doc("foo", "doc");
        assert!(store.remove_function_doc("foo"));
        assert!(store.get_function_doc("foo").is_none());
        assert!(!store.remove_function_doc("foo")); // already gone

        store.set_variable_doc("bar", "doc");
        assert!(store.remove_variable_doc("bar"));
        assert!(store.get_variable_doc("bar").is_none());
    }

    // -- HelpFormatter tests --

    #[test]
    fn help_describe_function_lambda() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams {
                required: vec!["x".into(), "y".into()],
                optional: vec![],
                rest: None,
            },
            body: vec![],
            env: None,
            docstring: Some("Add X and Y.".to_string()),
        }));
        let output = HelpFormatter::describe_function("my-add", &lam, None);
        assert!(output.contains("my-add is a Lisp function."));
        assert!(output.contains("(my-add X Y)"));
        assert!(output.contains("Add X and Y."));
    }

    #[test]
    fn help_describe_function_with_docstore() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["x".into()]),
            body: vec![],
            env: None,
            docstring: Some("Inline doc.".to_string()),
        }));
        // Docstore doc overrides inline
        let output =
            HelpFormatter::describe_function("my-fn", &lam, Some("Docstore doc."));
        assert!(output.contains("Docstore doc."));
        assert!(!output.contains("Inline doc."));
    }

    #[test]
    fn help_describe_function_subr() {
        let subr = Value::Subr("car".to_string());
        let output =
            HelpFormatter::describe_function("car", &subr, Some("Return the car of LIST."));
        assert!(output.contains("car is a built-in function."));
        assert!(output.contains("Return the car of LIST."));
    }

    #[test]
    fn help_describe_function_no_doc() {
        let subr = Value::Subr("mystery".to_string());
        let output = HelpFormatter::describe_function("mystery", &subr, None);
        assert!(output.contains("Not documented."));
    }

    #[test]
    fn help_describe_function_closure() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["x".into()]),
            body: vec![],
            env: Some(vec![]),
            docstring: None,
        }));
        let output = HelpFormatter::describe_function("my-closure", &lam, None);
        assert!(output.contains("a Lisp closure"));
    }

    #[test]
    fn help_describe_variable() {
        let output =
            HelpFormatter::describe_variable("fill-column", &Value::Int(70), Some("Column beyond which automatic line-filling takes place."));
        assert!(output.contains("fill-column's value is 70"));
        assert!(output.contains("Column beyond which"));
    }

    #[test]
    fn help_describe_variable_no_doc() {
        let output = HelpFormatter::describe_variable("x", &Value::Nil, None);
        assert!(output.contains("x's value is nil"));
        assert!(output.contains("Not documented."));
    }

    #[test]
    fn help_describe_key() {
        let output =
            HelpFormatter::describe_key("C-x C-f", "find-file", Some("Visit a file."));
        assert!(output.contains("C-x C-f runs the command find-file"));
        assert!(output.contains("Visit a file."));
    }

    #[test]
    fn help_describe_key_no_doc() {
        let output = HelpFormatter::describe_key("C-c a", "my-cmd", None);
        assert!(output.contains("C-c a runs the command my-cmd"));
        assert!(!output.contains("documented"));
    }

    #[test]
    fn help_format_apropos_entries() {
        let entries = vec![
            ("car".to_string(), true, false),
            ("car-mode".to_string(), false, true),
            ("cons".to_string(), true, true),
        ];
        let output = HelpFormatter::format_apropos(&entries);
        assert!(output.contains("car\n  Function\n"));
        assert!(output.contains("car-mode\n  Variable\n"));
        assert!(output.contains("cons\n  Function, Variable\n"));
    }

    #[test]
    fn help_format_apropos_empty() {
        let output = HelpFormatter::format_apropos(&[]);
        assert!(output.contains("No matches."));
    }

    // -- Builtin function tests --

    #[test]
    fn builtin_backtrace_returns_string() {
        let result = builtin_backtrace(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_string());
    }

    #[test]
    fn builtin_backtrace_rejects_args() {
        let result = builtin_backtrace(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_debug_returns_string() {
        let result = builtin_debug(vec![]);
        assert!(result.is_ok());
        let val = result.unwrap();
        assert!(val.is_string());
        assert!(val.as_str().unwrap().contains("Entering debugger"));
    }

    #[test]
    fn builtin_debug_with_args() {
        let result = builtin_debug(vec![Value::string("error occurred"), Value::Int(42)]);
        assert!(result.is_ok());
        let val = result.unwrap();
        let s = val.as_str().unwrap();
        assert!(s.contains("error occurred"));
        assert!(s.contains("42"));
    }

    #[test]
    fn builtin_debug_on_entry_returns_symbol() {
        let result = builtin_debug_on_entry(vec![Value::symbol("my-fn")]);
        assert!(result.is_ok());
        let val = result.unwrap();
        assert_eq!(val.as_symbol_name(), Some("my-fn"));
    }

    #[test]
    fn builtin_debug_on_entry_wrong_type() {
        let result = builtin_debug_on_entry(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_cancel_debug_on_entry_returns_symbol() {
        let result = builtin_cancel_debug_on_entry(vec![Value::symbol("my-fn")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_symbol_name(), Some("my-fn"));
    }

    #[test]
    fn builtin_debug_on_error_p_default() {
        let result = builtin_debug_on_error_p(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_describe_function_stub() {
        let result = builtin_describe_function(vec![Value::symbol("car")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert!(s.as_str().unwrap().contains("car"));
    }

    #[test]
    fn builtin_describe_variable_stub() {
        let result = builtin_describe_variable(vec![Value::symbol("load-path")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert!(s.as_str().unwrap().contains("load-path"));
    }

    #[test]
    fn builtin_documentation_from_lambda() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: Some("My docstring.".to_string()),
        }));
        let result = builtin_documentation(vec![lam]);
        assert!(result.is_ok());
        let val = result.unwrap();
        assert_eq!(val.as_str(), Some("My docstring."));
    }

    #[test]
    fn builtin_documentation_nil_for_symbol() {
        let result = builtin_documentation(vec![Value::symbol("car")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_documentation_nil_for_no_doc() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: None,
        }));
        let result = builtin_documentation(vec![lam]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_documentation_property_stub() {
        let result = builtin_documentation_property(vec![
            Value::symbol("car"),
            Value::symbol("function-documentation"),
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_apropos_stub() {
        let result = builtin_apropos(vec![Value::string("car")]);
        assert!(result.is_ok());
        // Stub returns nil
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_apropos_wrong_args() {
        let result = builtin_apropos(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn builtin_commandp_function() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: None,
        }));
        let result = builtin_commandp(vec![lam]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn builtin_commandp_subr() {
        let result = builtin_commandp(vec![Value::Subr("car".into())]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn builtin_commandp_non_function() {
        let result = builtin_commandp(vec![Value::Int(42)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn builtin_commandp_nil() {
        let result = builtin_commandp(vec![Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    // -- DebugAction clone/debug --

    #[test]
    fn debug_action_variants() {
        let actions = vec![
            DebugAction::Continue,
            DebugAction::Step,
            DebugAction::Next,
            DebugAction::Finish,
            DebugAction::Quit,
            DebugAction::Eval("(+ 1 2)".to_string()),
        ];
        // Just verify they can be cloned and debug-printed
        for action in &actions {
            let _cloned = action.clone();
            let _debug = format!("{:?}", action);
        }
    }

    // -- Integration-style tests --

    #[test]
    fn debug_state_full_workflow() {
        let mut ds = DebugState::new();

        // Initially nothing triggers
        assert!(!ds.should_debug_on_entry("my-fn"));
        assert!(!ds.should_debug_on_error("error"));
        assert!(!ds.active);
        assert!(!ds.stepping);

        // Set up debug-on-entry
        ds.add_debug_on_entry("my-fn");
        assert!(ds.should_debug_on_entry("my-fn"));

        // Set up a breakpoint
        let bp_id = ds.add_breakpoint("other-fn");
        assert!(ds.should_debug_on_entry("other-fn"));

        // Enable debug-on-error
        ds.debug_on_error = true;
        assert!(ds.should_debug_on_error("void-function"));
        assert!(ds.should_debug_on_error("wrong-type-argument"));

        // Use the backtrace
        ds.current_backtrace.push(BacktraceFrame {
            function: "my-fn".to_string(),
            args: vec![Value::Int(1)],
            file: None,
            line: None,
            is_special_form: false,
        });
        assert_eq!(ds.current_backtrace.depth(), 1);

        // Record a hit
        ds.record_breakpoint_hit("other-fn");
        assert_eq!(ds.list_breakpoints()[0].hit_count, 1);

        // Clean up
        ds.remove_debug_on_entry("my-fn");
        assert!(!ds.should_debug_on_entry("my-fn"));
        ds.remove_breakpoint(bp_id);
        assert!(!ds.should_debug_on_entry("other-fn"));
    }

    #[test]
    fn docstore_full_workflow() {
        let mut store = DocStore::new();

        // Populate
        store.set_function_doc("car", "Return the car of LIST.\nThe car is the first element.");
        store.set_function_doc("cdr", "Return the cdr of LIST.");
        store.set_function_doc("cons", "Create a new cons cell from CAR and CDR.");
        store.set_variable_doc("load-path", "List of directories to search for files to load.");
        store.set_variable_doc("debug-on-error", "Non-nil means enter debugger on error.");

        // Lookup
        assert!(store.get_function_doc("car").unwrap().contains("first element"));
        assert!(store.get_variable_doc("load-path").unwrap().contains("directories"));

        // Apropos
        let results = store.apropos("c");
        // "car", "cdr", "cons" match
        assert_eq!(results.len(), 3);

        let results = store.apropos("on-error");
        // "debug-on-error" matches
        assert_eq!(results.len(), 1);
        assert!(!results[0].1); // no function doc for debug-on-error
        assert!(results[0].2); // has variable doc

        // Format apropos
        let formatted = HelpFormatter::format_apropos(&results);
        assert!(formatted.contains("debug-on-error"));
        assert!(formatted.contains("Variable"));

        // All documented
        let fns = store.all_documented_functions();
        assert_eq!(fns.len(), 3);
        let vars = store.all_documented_variables();
        assert_eq!(vars.len(), 2);
    }

    #[test]
    fn help_formatter_with_optional_and_rest() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams {
                required: vec!["x".into()],
                optional: vec!["y".into()],
                rest: Some("args".into()),
            },
            body: vec![],
            env: None,
            docstring: Some("A function with complex params.".to_string()),
        }));
        let output = HelpFormatter::describe_function("complex-fn", &lam, None);
        assert!(output.contains("(complex-fn X &optional Y &rest ARGS)"));
        assert!(output.contains("A function with complex params."));
    }

    #[test]
    fn help_formatter_macro() {
        let mac = Value::Macro(Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["body".into()]),
            body: vec![],
            env: None,
            docstring: Some("A test macro.".to_string()),
        }));
        let output = HelpFormatter::describe_function("my-macro", &mac, None);
        assert!(output.contains("a Lisp macro"));
        assert!(output.contains("(my-macro BODY)"));
    }
}
