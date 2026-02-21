//! Interactive command system and mode definition macros.
//!
//! Implements:
//! - `InteractiveSpec` and `InteractiveRegistry` for tracking which functions
//!   are interactive commands and their argument specifications.
//! - Built-in functions: `call-interactively`, `interactive-p`,
//!   `called-interactively-p`, `commandp`, `command-execute`,
//!   `execute-extended-command`, `key-binding`, `local-key-binding`,
//!   `global-key-binding`, `minor-mode-key-binding`, `where-is-internal`,
//!   `substitute-command-keys`, `describe-key-briefly`, `this-command-keys`,
//!   `this-command-keys-vector`, `thing-at-point`, `bounds-of-thing-at-point`,
//!   `symbol-at-point`.
//! - Special forms: `define-minor-mode`, `define-derived-mode`,
//!   `define-generic-mode`.

use std::collections::{HashMap, HashSet};

use super::error::{signal, EvalResult, Flow};
use super::eval::Evaluator;
use super::expr::Expr;
use super::keymap::{
    decode_keymap_handle, encode_keymap_handle, KeyBinding, KeyEvent, KeymapManager,
};
use super::mode::{MajorMode, MinorMode};
use super::value::*;

// ---------------------------------------------------------------------------
// InteractiveSpec — describes how a command reads its arguments
// ---------------------------------------------------------------------------

/// Interactive argument specification for a command.
#[derive(Clone, Debug)]
pub struct InteractiveSpec {
    /// Code letter(s) describing argument types, e.g. "r" for region,
    /// "p" for prefix arg, "sPrompt: " for string prompt, etc.
    pub code: String,
    /// Optional prompt string (extracted from the code).
    pub prompt: Option<String>,
}

impl InteractiveSpec {
    /// Create a new interactive spec from a code string.
    pub fn new(code: impl Into<String>) -> Self {
        let code = code.into();
        // Extract prompt from code if it contains a prompt (e.g. "sEnter name: ")
        let prompt = if code.len() > 1 && code.starts_with(|c: char| c.is_ascii_lowercase()) {
            Some(code[1..].to_string())
        } else {
            None
        };
        Self { code, prompt }
    }

    /// Create a spec with no arguments (plain interactive command).
    pub fn no_args() -> Self {
        Self {
            code: String::new(),
            prompt: None,
        }
    }
}

// ---------------------------------------------------------------------------
// InteractiveRegistry — tracks which functions are interactive commands
// ---------------------------------------------------------------------------

/// Registry for interactive command specifications.
///
/// Tracks which named functions are interactive (i.e., can be called via
/// `M-x` or key bindings) and their argument specs.
pub struct InteractiveRegistry {
    /// Map from function name to its interactive spec.
    specs: HashMap<String, InteractiveSpec>,
    /// Stack tracking whether the current function was called interactively.
    interactive_call_stack: Vec<bool>,
    /// The key sequence that invoked the current command (if any).
    this_command_keys: Vec<String>,
}

impl InteractiveRegistry {
    pub fn new() -> Self {
        Self {
            specs: HashMap::new(),
            interactive_call_stack: Vec::new(),
            this_command_keys: Vec::new(),
        }
    }

    /// Register a function as interactive with the given spec.
    pub fn register_interactive(&mut self, name: &str, spec: InteractiveSpec) {
        self.specs.insert(name.to_string(), spec);
    }

    /// Check if a function is registered as interactive.
    pub fn is_interactive(&self, name: &str) -> bool {
        self.specs.contains_key(name)
    }

    /// Get the interactive spec for a function, if registered.
    pub fn get_spec(&self, name: &str) -> Option<&InteractiveSpec> {
        self.specs.get(name)
    }

    /// Push an interactive call frame.
    pub fn push_interactive_call(&mut self, is_interactive: bool) {
        self.interactive_call_stack.push(is_interactive);
    }

    /// Pop an interactive call frame.
    pub fn pop_interactive_call(&mut self) {
        self.interactive_call_stack.pop();
    }

    /// Check if the current function was called interactively.
    pub fn is_called_interactively(&self) -> bool {
        self.interactive_call_stack.last().copied().unwrap_or(false)
    }

    /// Set the key sequence that invoked the current command.
    pub fn set_this_command_keys(&mut self, keys: Vec<String>) {
        self.this_command_keys = keys;
    }

    /// Get the key sequence that invoked the current command.
    pub fn this_command_keys(&self) -> &[String] {
        &self.this_command_keys
    }
}

impl Default for InteractiveRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Expect helpers (local to this module)
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

fn expect_optional_command_keys_vector(keys: Option<&Value>) -> Result<(), Flow> {
    if let Some(keys_value) = keys {
        if !keys_value.is_nil() && !keys_value.is_vector() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("vectorp"), keys_value.clone()],
            ));
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------
// Built-in functions (evaluator-dependent)
// ---------------------------------------------------------------------------

/// `(call-interactively FUNCTION &optional RECORD-FLAG KEYS)`
/// Call FUNCTION interactively, reading arguments according to its
/// interactive spec.
pub(crate) fn builtin_call_interactively(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("call-interactively", &args, 1)?;
    expect_max_args("call-interactively", &args, 3)?;
    expect_optional_command_keys_vector(args.get(2))?;

    let func_val = &args[0];
    if !command_designator_p(eval, func_val) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("commandp"), func_val.clone()],
        ));
    }
    let Some((resolved_name, func)) = resolve_command_target(eval, func_val) else {
        return Err(signal("void-function", vec![func_val.clone()]));
    };
    let func = normalize_command_callable(eval, func)?;
    let mut context = InteractiveInvocationContext::from_keys_arg(eval, args.get(2));
    let call_args = resolve_interactive_invocation_args(
        eval,
        &resolved_name,
        &func,
        CommandInvocationKind::CallInteractively,
        &mut context,
    )?;

    // Mark as interactive call
    eval.interactive.push_interactive_call(true);

    let result = eval.apply(func, call_args);

    eval.interactive.pop_interactive_call();
    result
}

/// `(interactive-p)` -> t if the calling function was called interactively.
pub(crate) fn builtin_interactive_p(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("interactive-p", &args, 0)?;
    let _ = eval;
    // Emacs 30 keeps `interactive-p` obsolete; it effectively returns nil.
    Ok(Value::Nil)
}

/// `(called-interactively-p &optional KIND)`
/// Return t if the calling function was called interactively.
/// KIND can be 'interactive or 'any.
pub(crate) fn builtin_called_interactively_p(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    // Accept 0 or 1 args
    if args.len() > 1 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("called-interactively-p"),
                Value::Int(args.len() as i64),
            ],
        ));
    }
    if !eval.interactive.is_called_interactively() {
        return Ok(Value::Nil);
    }

    // GNU Emacs semantics:
    // - KIND = 'interactive => nil
    // - KIND = nil / 'any / unknown => t (when called interactively)
    if args
        .first()
        .is_some_and(|v| matches!(v, Value::Symbol(s) if s == "interactive"))
    {
        Ok(Value::Nil)
    } else {
        Ok(Value::True)
    }
}

/// `(commandp FUNCTION &optional FOR-CALL-INTERACTIVELY)`
/// Return non-nil if FUNCTION is a command (i.e., can be called interactively).
pub(crate) fn builtin_commandp_interactive(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("commandp", &args, 1)?;
    expect_max_args("commandp", &args, 2)?;
    let is_command = command_designator_p(eval, &args[0]);
    Ok(Value::bool(is_command))
}

/// `(command-modes COMMAND)` -- return COMMAND's mode list.
///
/// Current compatibility behavior returns nil.
pub(crate) fn builtin_command_modes(args: Vec<Value>) -> EvalResult {
    expect_args("command-modes", &args, 1)?;
    Ok(Value::Nil)
}

/// `(command-remapping COMMAND &optional POSITION KEYMAP)` -- return remapped
/// command for COMMAND.
///
/// Respects local/global keymaps when KEYMAP is omitted or nil.
pub(crate) fn builtin_command_remapping(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("command-remapping", &args, 1)?;
    expect_max_args("command-remapping", &args, 3)?;
    if let Some(keymap) = args.get(2) {
        if !keymap.is_nil() && !command_remapping_keymap_arg_valid(eval, keymap) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("keymapp"), keymap.clone()],
            ));
        }
    }
    let Some(command_name) = command_remapping_command_name(&args[0]) else {
        return Ok(Value::Nil);
    };
    if let Some(keymap_arg) = args.get(2) {
        match keymap_arg {
            Value::Cons(keymap) => {
                let keymap_value = Value::Cons(keymap.clone());
                if let Some(target) =
                    command_remapping_lookup_in_lisp_keymap(&keymap_value, &command_name)
                {
                    return Ok(command_remapping_normalize_target(target));
                }
                return Ok(Value::Nil);
            }
            Value::Nil => {
                return Ok(
                    command_remapping_lookup_in_active_keymaps(eval, &command_name)
                        .unwrap_or(Value::Nil),
                );
            }
            _ => {
                let Some(map_id) = command_remapping_lookup_keymap_id(eval, Some(keymap_arg))
                else {
                    return Ok(Value::Nil);
                };
                return Ok(
                    command_remapping_lookup_in_keymap_id(eval, map_id, &command_name)
                        .unwrap_or(Value::Nil),
                );
            }
        }
    }
    Ok(command_remapping_lookup_in_active_keymaps(eval, &command_name).unwrap_or(Value::Nil))
}

fn builtin_command_name(name: &str) -> bool {
    matches!(
        name,
        "ignore"
            | "eval-expression"
            | "self-insert-command"
            | "newline"
            | "execute-extended-command"
            | "forward-char"
            | "backward-char"
            | "delete-char"
            | "insert-char"
            | "next-line"
            | "previous-line"
            | "kill-line"
            | "kill-word"
            | "backward-kill-word"
            | "kill-region"
            | "kill-ring-save"
            | "kill-whole-line"
            | "copy-region-as-kill"
            | "yank"
            | "yank-pop"
            | "transpose-chars"
            | "transpose-lines"
            | "transpose-paragraphs"
            | "transpose-sentences"
            | "transpose-sexps"
            | "transpose-words"
            | "open-line"
            | "delete-horizontal-space"
            | "just-one-space"
            | "delete-indentation"
            | "indent-for-tab-command"
            | "upcase-word"
            | "downcase-word"
            | "capitalize-word"
            | "upcase-region"
            | "downcase-region"
            | "capitalize-region"
            | "upcase-initials-region"
            | "switch-to-buffer"
            | "find-file"
            | "save-buffer"
            | "select-frame"
            | "set-mark-command"
            | "recenter-top-bottom"
            | "scroll-up-command"
            | "scroll-down-command"
            | "other-window"
            | "keyboard-quit"
            | "quoted-insert"
            | "universal-argument"
            | "beginning-of-line"
            | "end-of-line"
            | "move-beginning-of-line"
            | "move-end-of-line"
            | "abbrev-mode"
            | "abort-minibuffers"
            | "abort-recursive-edit"
            | "add-name-to-file"
            | "advice-remove"
            | "auto-composition-mode"
            | "back-to-indentation"
            | "backward-sexp"
            | "backward-word"
            | "base64-decode-region"
            | "base64-encode-region"
            | "base64url-encode-region"
            | "beginning-of-buffer"
            | "bookmark-delete"
            | "bookmark-jump"
            | "bookmark-load"
            | "bookmark-rename"
            | "bookmark-save"
            | "bookmark-set"
            | "buffer-disable-undo"
            | "buffer-enable-undo"
            | "call-last-kbd-macro"
            | "clear-rectangle"
            | "copy-file"
            | "copy-to-register"
            | "count-matches"
            | "defining-kbd-macro"
            | "delete-directory"
            | "delete-file"
            | "delete-frame"
            | "delete-other-windows"
            | "delete-other-windows-internal"
            | "delete-process"
            | "kill-process"
            | "signal-process"
            | "process-menu-delete-process"
            | "process-menu-mode"
            | "delete-rectangle"
            | "delete-region"
            | "delete-window"
            | "decode-coding-region"
            | "do-auto-save"
            | "handle-save-session"
            | "handle-switch-frame"
            | "describe-function"
            | "describe-key-briefly"
            | "describe-variable"
            | "display-buffer"
            | "emacs-version"
            | "end-kbd-macro"
            | "end-of-buffer"
            | "erase-buffer"
            | "eval-buffer"
            | "eval-region"
            | "encode-coding-region"
            | "exchange-point-and-mark"
            | "exit-minibuffer"
            | "exit-recursive-edit"
            | "expand-abbrev"
            | "fit-window-to-buffer"
            | "flush-lines"
            | "forward-line"
            | "forward-sexp"
            | "forward-word"
            | "garbage-collect"
            | "getenv"
            | "global-set-key"
            | "gui-set-selection"
            | "goto-char"
            | "goto-line"
            | "how-many"
            | "increment-register"
            | "indent-according-to-mode"
            | "indent-region"
            | "indent-rigidly"
            | "indent-to"
            | "insert-kbd-macro"
            | "insert-register"
            | "iconify-frame"
            | "isearch-backward"
            | "isearch-forward"
            | "kbd-macro-query"
            | "keep-lines"
            | "kill-buffer"
            | "kill-emacs"
            | "kill-local-variable"
            | "kill-rectangle"
            | "load-file"
            | "lower-frame"
            | "lossage-size"
            | "malloc-info"
            | "malloc-trim"
            | "list-processes"
            | "local-set-key"
            | "make-directory"
            | "make-frame"
            | "make-frame-invisible"
            | "make-frame-visible"
            | "make-indirect-buffer"
            | "make-local-variable"
            | "make-symbolic-link"
            | "make-variable-buffer-local"
            | "modify-syntax-entry"
            | "move-to-column"
            | "move-to-window-line"
            | "kmacro-name-last-macro"
            | "name-last-kbd-macro"
            | "narrow-to-region"
            | "newline-and-indent"
            | "number-to-register"
            | "open-dribble-file"
            | "open-rectangle"
            | "open-termscript"
            | "point-to-register"
            | "pop-to-buffer"
            | "posix-search-backward"
            | "posix-search-forward"
            | "query-replace"
            | "query-replace-regexp"
            | "raise-frame"
            | "recenter"
            | "redirect-debugging-output"
            | "re-search-backward"
            | "re-search-forward"
            | "search-backward-regexp"
            | "search-forward-regexp"
            | "recursive-edit"
            | "reindent-then-newline-and-indent"
            | "remove-hook"
            | "rename-buffer"
            | "rename-file"
            | "replace-buffer-contents"
            | "replace-regexp"
            | "replace-string"
            | "replace-rectangle"
            | "redraw-display"
            | "run-at-time"
            | "run-with-idle-timer"
            | "run-with-timer"
            | "scroll-down"
            | "scroll-left"
            | "scroll-right"
            | "search-backward"
            | "search-forward"
            | "scroll-up"
            | "set-file-modes"
            | "set-frame-height"
            | "set-frame-width"
            | "set-buffer-process-coding-system"
            | "set-keyboard-coding-system"
            | "set-terminal-coding-system"
            | "setenv"
            | "start-kbd-macro"
            | "string-rectangle"
            | "tab-to-tab-stop"
            | "suspend-emacs"
            | "top-level"
            | "transient-mark-mode"
            | "transpose-regions"
            | "undo"
            | "unix-sync"
            | "upcase-char"
            | "view-register"
            | "widen"
            | "word-search-backward"
            | "word-search-forward"
            | "write-region"
            | "x-clipboard-yank"
            | "x-menu-bar-open-internal"
            | "x-preedit-text"
            | "yank-rectangle"
    )
}

fn expr_is_interactive_form(expr: &Expr) -> bool {
    match expr {
        Expr::List(items) => items
            .first()
            .is_some_and(|head| matches!(head, Expr::Symbol(sym) if sym == "interactive")),
        _ => false,
    }
}

fn lambda_body_has_interactive_form(body: &[Expr]) -> bool {
    let mut body_index = 0;
    if matches!(body.first(), Some(Expr::Str(_))) {
        body_index = 1;
    }
    body.get(body_index).is_some_and(expr_is_interactive_form)
}

fn value_list_to_vec(list: &Value) -> Option<Vec<Value>> {
    let mut values = Vec::new();
    let mut cursor = list.clone();
    loop {
        match cursor {
            Value::Nil => return Some(values),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                values.push(pair.car.clone());
                cursor = pair.cdr.clone();
            }
            _ => return None,
        }
    }
}

fn value_is_interactive_form(value: &Value) -> bool {
    match value {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            pair.car.as_symbol_name() == Some("interactive")
        }
        _ => false,
    }
}

fn quoted_lambda_has_interactive_form(value: &Value) -> bool {
    let Some(items) = value_list_to_vec(value) else {
        return false;
    };
    if items.first().and_then(Value::as_symbol_name) != Some("lambda") {
        return false;
    }

    let mut body_index = 2;
    if matches!(items.get(body_index), Some(Value::Str(_))) {
        body_index += 1;
    }

    items.get(body_index).is_some_and(value_is_interactive_form)
}

fn resolve_function_designator_symbol(eval: &Evaluator, name: &str) -> Option<(String, Value)> {
    let mut current = name.to_string();
    let mut seen = HashSet::new();

    loop {
        if !seen.insert(current.clone()) {
            return None;
        }

        if eval.obarray.is_function_unbound(&current) {
            return None;
        }

        if let Some(function) = eval.obarray.symbol_function(&current) {
            if let Some(next) = function.as_symbol_name() {
                if next == "nil" {
                    return Some((current, Value::Nil));
                }
                current = next.to_string();
                continue;
            }
            return Some((current, function.clone()));
        }

        if let Some(function) = super::subr_info::fallback_macro_value(&current) {
            return Some((current, function));
        }

        if super::subr_info::is_special_form(&current)
            || super::subr_info::is_evaluator_callable_name(&current)
            || super::builtin_registry::is_dispatch_builtin_name(&current)
        {
            return Some((current.clone(), Value::Subr(current)));
        }

        return None;
    }
}

fn command_object_p(eval: &Evaluator, resolved_name: Option<&str>, value: &Value) -> bool {
    if let Some(name) = resolved_name {
        if eval.interactive.is_interactive(name) || builtin_command_name(name) {
            return true;
        }
    }

    match value {
        Value::Lambda(lambda) => lambda_body_has_interactive_form(&lambda.body),
        Value::Cons(_) => quoted_lambda_has_interactive_form(value),
        Value::Subr(name) => eval.interactive.is_interactive(name) || builtin_command_name(name),
        _ => false,
    }
}

fn command_designator_p(eval: &Evaluator, designator: &Value) -> bool {
    if let Some(name) = designator.as_symbol_name() {
        if eval.obarray.is_function_unbound(name) {
            return false;
        }
        if let Some((resolved_name, resolved_value)) =
            resolve_function_designator_symbol(eval, name)
        {
            return command_object_p(eval, Some(&resolved_name), &resolved_value);
        }
        return eval.interactive.is_interactive(name) || builtin_command_name(name);
    }
    command_object_p(eval, None, designator)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CommandInvocationKind {
    CallInteractively,
    CommandExecute,
}

#[derive(Clone, Debug)]
enum ParsedInteractiveSpec {
    NoArgs,
    StringCode(String),
    Form(Expr),
}

#[derive(Clone, Debug, Default)]
struct ParsedInteractiveStringCode {
    prefix_flags: Vec<char>,
    entries: Vec<(char, String)>,
}

#[derive(Clone, Debug, Default)]
struct InteractiveInvocationContext {
    command_keys: Vec<Value>,
    next_event_with_parameters_index: usize,
    has_command_keys_context: bool,
}

impl InteractiveInvocationContext {
    fn from_keys_arg(eval: &Evaluator, keys: Option<&Value>) -> Self {
        let mut context = Self::default();
        if let Some(Value::Vector(values)) = keys {
            let values = values.lock().expect("poisoned");
            if !values.is_empty() {
                context.command_keys = values.clone();
                context.has_command_keys_context = true;
                return context;
            }
        }
        if !eval.read_command_keys().is_empty() {
            context.command_keys = eval.read_command_keys().to_vec();
            context.has_command_keys_context = true;
        }
        context
    }
}

fn dynamic_or_global_symbol_value(eval: &Evaluator, name: &str) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(v) = frame.get(name) {
            return Some(v.clone());
        }
    }
    eval.obarray.symbol_value(name).cloned()
}

fn dynamic_buffer_or_global_symbol_value(
    eval: &Evaluator,
    buf: &crate::buffer::Buffer,
    name: &str,
) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(v) = frame.get(name) {
            return Some(v.clone());
        }
    }
    if let Some(v) = buf.get_buffer_local(name) {
        return Some(v.clone());
    }
    eval.obarray.symbol_value(name).cloned()
}

fn prefix_numeric_value(value: &Value) -> i64 {
    match value {
        Value::Nil => 1,
        Value::Int(n) => *n,
        Value::Float(f) => *f as i64,
        Value::Char(c) => *c as i64,
        Value::Symbol(s) if s == "-" => -1,
        Value::Cons(cell) => {
            let car = {
                let pair = cell.lock().expect("poisoned");
                pair.car.clone()
            };
            match car {
                Value::Int(n) => n,
                Value::Float(f) => f as i64,
                Value::Char(c) => c as i64,
                _ => 1,
            }
        }
        _ => 1,
    }
}

fn interactive_prefix_raw_arg(eval: &Evaluator, kind: CommandInvocationKind) -> Value {
    let symbol = match kind {
        CommandInvocationKind::CallInteractively => "current-prefix-arg",
        CommandInvocationKind::CommandExecute => "prefix-arg",
    };
    dynamic_or_global_symbol_value(eval, symbol).unwrap_or(Value::Nil)
}

fn interactive_prefix_numeric_arg(eval: &Evaluator, kind: CommandInvocationKind) -> Value {
    let raw = interactive_prefix_raw_arg(eval, kind);
    Value::Int(prefix_numeric_value(&raw))
}

fn interactive_region_args(
    eval: &Evaluator,
    missing_mark_signal: &str,
) -> Result<Vec<Value>, Flow> {
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let mark = buf.mark().ok_or_else(|| {
        signal(
            missing_mark_signal,
            vec![Value::string(
                "The mark is not set now, so there is no region",
            )],
        )
    })?;
    let pt = buf.point();
    let beg = pt.min(mark);
    let end = pt.max(mark);
    // Region-taking builtins use Emacs-style 1-based character positions.
    let beg_char = buf.text.byte_to_char(beg) as i64 + 1;
    let end_char = buf.text.byte_to_char(end) as i64 + 1;
    Ok(vec![Value::Int(beg_char), Value::Int(end_char)])
}

fn interactive_point_arg(eval: &Evaluator) -> Result<Value, Flow> {
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let point_char = buf.text.byte_to_char(buf.point()) as i64 + 1;
    Ok(Value::Int(point_char))
}

fn interactive_mark_arg(eval: &Evaluator) -> Result<Value, Flow> {
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let mark = buf
        .mark()
        .ok_or_else(|| signal("error", vec![Value::string("The mark is not set now")]))?;
    let mark_char = buf.text.byte_to_char(mark) as i64 + 1;
    Ok(Value::Int(mark_char))
}

fn interactive_read_expression_arg(eval: &mut Evaluator, prompt: String) -> Result<Value, Flow> {
    let input = super::reader::builtin_read_from_minibuffer(eval, vec![Value::string(prompt)])?;
    super::reader::builtin_read(eval, vec![input])
}

fn interactive_read_coding_system_optional_arg(prompt: String) -> Result<Value, Flow> {
    match super::lread::builtin_read_coding_system(vec![Value::string(prompt)]) {
        Ok(value) => Ok(value),
        Err(Flow::Signal(sig)) if sig.symbol == "end-of-file" => Ok(Value::Nil),
        Err(flow) => Err(flow),
    }
}

fn interactive_buffer_read_only_active(eval: &Evaluator, buf: &crate::buffer::Buffer) -> bool {
    if buf.read_only {
        return true;
    }
    dynamic_buffer_or_global_symbol_value(eval, buf, "buffer-read-only")
        .is_some_and(|v| v.is_truthy())
}

fn interactive_require_writable_current_buffer(eval: &Evaluator) -> Result<(), Flow> {
    let Some(buf) = eval.buffers.current_buffer() else {
        return Ok(());
    };
    if dynamic_buffer_or_global_symbol_value(eval, buf, "inhibit-read-only")
        .is_some_and(|v| v.is_truthy())
    {
        return Ok(());
    }
    if interactive_buffer_read_only_active(eval, buf) {
        return Err(signal("buffer-read-only", vec![Value::string(&buf.name)]));
    }
    Ok(())
}

fn interactive_apply_shift_selection_prefix(eval: &mut Evaluator) {
    let shifted = dynamic_or_global_symbol_value(eval, "this-command-keys-shift-translated")
        .is_some_and(|v| v.is_truthy());
    let shift_select_mode =
        dynamic_or_global_symbol_value(eval, "shift-select-mode").is_some_and(|v| v.is_truthy());
    if !shifted || !shift_select_mode {
        return;
    }

    let mut mark_activated = false;
    if let Some(buf) = eval.buffers.current_buffer_mut() {
        let point = buf.point();
        buf.set_mark(point);
        buf.properties
            .insert("mark-active".to_string(), Value::True);
        mark_activated = true;
    }
    if mark_activated {
        eval.assign("mark-active", Value::True);
    }
}

fn interactive_apply_prefix_flags(eval: &mut Evaluator, prefix_flags: &[char]) -> Result<(), Flow> {
    for prefix_flag in prefix_flags {
        match prefix_flag {
            '*' => interactive_require_writable_current_buffer(eval)?,
            '@' => {
                // Selecting the window from the first mouse event requires command-loop
                // event context; current batch paths have no such events yet.
            }
            '^' => interactive_apply_shift_selection_prefix(eval),
            _ => {}
        }
    }
    Ok(())
}

fn interactive_event_with_parameters_p(event: &Value) -> bool {
    matches!(event, Value::Cons(_))
}

fn interactive_next_event_with_parameters_from_keys(
    context: &mut InteractiveInvocationContext,
) -> Option<Value> {
    while context.next_event_with_parameters_index < context.command_keys.len() {
        let event = context.command_keys[context.next_event_with_parameters_index].clone();
        context.next_event_with_parameters_index += 1;
        if interactive_event_with_parameters_p(&event) {
            return Some(event);
        }
    }
    None
}

fn interactive_last_input_event_with_parameters(eval: &Evaluator) -> Option<Value> {
    let event = dynamic_or_global_symbol_value(eval, "last-input-event")?;
    interactive_event_with_parameters_p(&event).then_some(event)
}

fn interactive_next_event_with_parameters(
    eval: &Evaluator,
    context: &mut InteractiveInvocationContext,
) -> Option<Value> {
    if context.has_command_keys_context {
        return interactive_next_event_with_parameters_from_keys(context);
    }
    interactive_last_input_event_with_parameters(eval)
}

fn parse_interactive_spec(expr: &Expr) -> Option<ParsedInteractiveSpec> {
    let Expr::List(items) = expr else {
        return None;
    };
    if !items
        .first()
        .is_some_and(|head| matches!(head, Expr::Symbol(sym) if sym == "interactive"))
    {
        return None;
    }
    match items.get(1) {
        Some(Expr::Str(code)) => Some(ParsedInteractiveSpec::StringCode(code.clone())),
        Some(form) => Some(ParsedInteractiveSpec::Form(form.clone())),
        None => Some(ParsedInteractiveSpec::NoArgs),
    }
}

fn parsed_interactive_spec_from_lambda(lambda: &LambdaData) -> Option<ParsedInteractiveSpec> {
    lambda.body.first().and_then(parse_interactive_spec)
}

fn interactive_form_value_to_args(value: Value) -> Result<Vec<Value>, Flow> {
    if value.is_nil() {
        return Ok(Vec::new());
    }
    if let Some(values) = value_list_to_vec(&value) {
        return Ok(values);
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("listp"), value],
    ))
}

fn parse_interactive_prefix_flags(mut line: &str) -> (Vec<char>, &str) {
    let mut flags = Vec::new();
    while let Some(ch) = line.chars().next() {
        if matches!(ch, '*' | '@' | '^') {
            flags.push(ch);
            line = &line[ch.len_utf8()..];
        } else {
            break;
        }
    }
    (flags, line)
}

fn parse_interactive_code_entries(code: &str) -> ParsedInteractiveStringCode {
    let mut parsed = ParsedInteractiveStringCode::default();
    if code.is_empty() {
        return parsed;
    }

    for (index, raw_line) in code.split('\n').enumerate() {
        let line = if index == 0 {
            let (flags, stripped) = parse_interactive_prefix_flags(raw_line);
            parsed.prefix_flags = flags;
            stripped
        } else {
            raw_line
        };
        if line.is_empty() {
            continue;
        }
        let mut chars = line.chars();
        let Some(letter) = chars.next() else {
            continue;
        };
        parsed.entries.push((letter, chars.collect::<String>()));
    }
    parsed
}

fn invalid_interactive_control_letter_error(letter: char) -> Flow {
    let codepoint = letter as u32;
    signal(
        "error",
        vec![Value::string(format!(
            "Invalid control letter \u{2018}{letter}\u{2019} (#o{codepoint:o}, #x{codepoint:04x}) in interactive calling string"
        ))],
    )
}

fn interactive_args_from_string_code(
    eval: &mut Evaluator,
    code: &str,
    kind: CommandInvocationKind,
    context: &mut InteractiveInvocationContext,
) -> Result<Option<Vec<Value>>, Flow> {
    let parsed = parse_interactive_code_entries(code);
    interactive_apply_prefix_flags(eval, &parsed.prefix_flags)?;
    if parsed.entries.is_empty() {
        return Ok(Some(Vec::new()));
    }

    let mut args = Vec::new();
    for (letter, prompt) in parsed.entries {
        match letter {
            'a' => args.push(super::minibuffer::builtin_read_command(vec![
                Value::string(prompt),
            ])?),
            'b' => args.push(super::minibuffer::builtin_read_buffer(vec![
                Value::string(prompt),
                Value::Nil,
                Value::True,
            ])?),
            'B' => args.push(super::minibuffer::builtin_read_buffer(vec![
                Value::string(prompt),
                Value::Nil,
                Value::Nil,
            ])?),
            'c' => args.push(super::reader::builtin_read_char(
                eval,
                vec![Value::string(prompt)],
            )?),
            'C' => args.push(super::minibuffer::builtin_read_command(vec![
                Value::string(prompt),
            ])?),
            'd' => args.push(interactive_point_arg(eval)?),
            'D' => args.push(super::minibuffer::builtin_read_directory_name(vec![
                Value::string(prompt),
            ])?),
            'e' => {
                if let Some(event) = interactive_next_event_with_parameters(eval, context) {
                    args.push(event);
                } else {
                    return Err(signal(
                        "error",
                        vec![Value::string(
                            "command must be bound to an event with parameters",
                        )],
                    ));
                }
            }
            'f' => args.push(super::minibuffer::builtin_read_file_name(vec![
                Value::string(prompt),
                Value::Nil,
                Value::Nil,
                Value::True,
            ])?),
            'F' => args.push(super::minibuffer::builtin_read_file_name(vec![
                Value::string(prompt),
            ])?),
            'G' => args.push(super::minibuffer::builtin_read_file_name(vec![
                Value::string(prompt),
            ])?),
            'i' => args.push(Value::Nil),
            'k' => args.push(super::reader::builtin_read_key_sequence(
                eval,
                vec![Value::string(prompt)],
            )?),
            'K' => args.push(super::reader::builtin_read_key_sequence_vector(
                eval,
                vec![Value::string(prompt)],
            )?),
            'M' => args.push(super::reader::builtin_read_string(
                eval,
                vec![Value::string(prompt)],
            )?),
            'm' => args.push(interactive_mark_arg(eval)?),
            'N' => {
                let raw = interactive_prefix_raw_arg(eval, kind);
                if raw.is_nil() {
                    args.push(super::reader::builtin_read_number(
                        eval,
                        vec![Value::string(prompt)],
                    )?);
                } else {
                    args.push(Value::Int(prefix_numeric_value(&raw)));
                }
            }
            'p' => args.push(interactive_prefix_numeric_arg(eval, kind)),
            'P' => args.push(interactive_prefix_raw_arg(eval, kind)),
            'r' => args.extend(interactive_region_args(eval, "error")?),
            'S' => {
                let sym_name =
                    super::reader::builtin_read_string(eval, vec![Value::string(prompt)])?;
                if let Some(name) = sym_name.as_str() {
                    args.push(Value::symbol(name));
                } else {
                    return Ok(None);
                }
            }
            's' => args.push(super::reader::builtin_read_string(
                eval,
                vec![Value::string(prompt)],
            )?),
            'n' => args.push(super::reader::builtin_read_number(
                eval,
                vec![Value::string(prompt)],
            )?),
            'x' => args.push(interactive_read_expression_arg(eval, prompt)?),
            'X' => {
                let expr_value = interactive_read_expression_arg(eval, prompt)?;
                let expr = super::eval::value_to_expr_pub(&expr_value);
                args.push(eval.eval(&expr)?);
            }
            'U' => args.push(Value::Nil),
            'v' => args.push(super::minibuffer::builtin_read_variable(vec![
                Value::string(prompt),
            ])?),
            'z' => args.push(super::lread::builtin_read_coding_system(vec![
                Value::string(prompt),
            ])?),
            'Z' => args.push(interactive_read_coding_system_optional_arg(prompt)?),
            _ => return Err(invalid_interactive_control_letter_error(letter)),
        }
    }

    Ok(Some(args))
}

fn resolve_interactive_invocation_args(
    eval: &mut Evaluator,
    resolved_name: &str,
    func: &Value,
    kind: CommandInvocationKind,
    context: &mut InteractiveInvocationContext,
) -> Result<Vec<Value>, Flow> {
    if let Some(code) = eval
        .interactive
        .get_spec(resolved_name)
        .map(|spec| spec.code.clone())
    {
        if let Some(args) = interactive_args_from_string_code(eval, &code, kind, context)? {
            return Ok(args);
        }
    }

    if let Value::Lambda(lambda) = func {
        if let Some(spec) = parsed_interactive_spec_from_lambda(lambda) {
            let maybe_args = match spec {
                ParsedInteractiveSpec::NoArgs => Some(Vec::new()),
                ParsedInteractiveSpec::StringCode(code) => {
                    interactive_args_from_string_code(eval, &code, kind, context)?
                }
                ParsedInteractiveSpec::Form(form) => {
                    let value = eval.eval(&form)?;
                    Some(interactive_form_value_to_args(value)?)
                }
            };
            if let Some(args) = maybe_args {
                return Ok(args);
            }
        }
    }

    match kind {
        CommandInvocationKind::CallInteractively => {
            default_call_interactively_args(eval, resolved_name)
        }
        CommandInvocationKind::CommandExecute => default_command_execute_args(eval, resolved_name),
    }
}

fn value_is_lambda_form(value: &Value) -> bool {
    let Some(items) = value_list_to_vec(value) else {
        return false;
    };
    items.first().and_then(Value::as_symbol_name) == Some("lambda")
}

fn normalize_command_callable(eval: &mut Evaluator, value: Value) -> Result<Value, Flow> {
    if value_is_lambda_form(&value) {
        let expr = super::eval::value_to_expr_pub(&value);
        return eval.eval(&expr);
    }
    Ok(value)
}

fn default_command_execute_args(eval: &Evaluator, name: &str) -> Result<Vec<Value>, Flow> {
    match name {
        "self-insert-command"
        | "delete-char"
        | "kill-word"
        | "backward-kill-word"
        | "downcase-word"
        | "upcase-word"
        | "capitalize-word"
        | "transpose-lines"
        | "transpose-paragraphs"
        | "transpose-sentences"
        | "transpose-sexps"
        | "transpose-words" => Ok(vec![Value::Int(1)]),
        "kill-region" => interactive_region_args(eval, "user-error"),
        "kill-ring-save" => interactive_region_args(eval, "error"),
        "copy-region-as-kill" => interactive_region_args(eval, "error"),
        "set-mark-command" => Ok(vec![Value::Nil]),
        "capitalize-region" => interactive_region_args(eval, "error"),
        "upcase-initials-region" => interactive_region_args(eval, "error"),
        "upcase-region" | "downcase-region" => Err(signal(
            "args-out-of-range",
            vec![Value::string(""), Value::Int(0)],
        )),
        _ => Ok(Vec::new()),
    }
}

fn default_call_interactively_args(eval: &Evaluator, name: &str) -> Result<Vec<Value>, Flow> {
    match name {
        "self-insert-command"
        | "delete-char"
        | "kill-word"
        | "backward-kill-word"
        | "downcase-word"
        | "upcase-word"
        | "capitalize-word"
        | "transpose-lines"
        | "transpose-paragraphs"
        | "transpose-sentences"
        | "transpose-sexps"
        | "transpose-words"
        | "forward-char"
        | "backward-char"
        | "next-line"
        | "previous-line"
        | "beginning-of-line"
        | "end-of-line"
        | "move-beginning-of-line"
        | "move-end-of-line" => Ok(vec![interactive_prefix_numeric_arg(
            eval,
            CommandInvocationKind::CallInteractively,
        )]),
        "set-mark-command" => Ok(vec![dynamic_or_global_symbol_value(
            eval,
            "current-prefix-arg",
        )
        .unwrap_or(Value::Nil)]),
        "upcase-region" | "downcase-region" | "capitalize-region" => {
            interactive_region_args(eval, "error")
        }
        _ => default_command_execute_args(eval, name),
    }
}

fn resolve_command_target(eval: &Evaluator, designator: &Value) -> Option<(String, Value)> {
    if let Some(name) = designator.as_symbol_name() {
        if let Some((resolved_name, value)) = resolve_function_designator_symbol(eval, name) {
            return Some((resolved_name, value));
        }
        if builtin_command_name(name) {
            return Some((name.to_string(), Value::Subr(name.to_string())));
        }
        return None;
    }
    match designator {
        Value::Subr(name) => Some((name.clone(), designator.clone())),
        Value::True => Some(("t".to_string(), designator.clone())),
        Value::Keyword(name) => Some((name.clone(), designator.clone())),
        _ => Some(("<anonymous>".to_string(), designator.clone())),
    }
}

/// `(command-execute CMD &optional RECORD-FLAG KEYS SPECIAL)`
/// Execute CMD as an editor command.
pub(crate) fn builtin_command_execute(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("command-execute", &args, 1)?;
    expect_max_args("command-execute", &args, 4)?;
    expect_optional_command_keys_vector(args.get(2))?;

    let cmd = &args[0];
    if !command_designator_p(eval, cmd) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("commandp"), cmd.clone()],
        ));
    }
    let Some((resolved_name, func)) = resolve_command_target(eval, cmd) else {
        return Err(signal("void-function", vec![cmd.clone()]));
    };
    let func = normalize_command_callable(eval, func)?;
    let mut context = InteractiveInvocationContext::from_keys_arg(eval, args.get(2));
    let call_args = resolve_interactive_invocation_args(
        eval,
        &resolved_name,
        &func,
        CommandInvocationKind::CommandExecute,
        &mut context,
    )?;

    eval.interactive.push_interactive_call(true);
    let result = eval.apply(func, call_args);
    eval.interactive.pop_interactive_call();
    result
}

/// `(eval-expression EXPRESSION &optional INSERT-VALUE NO-TRUNCATE LEXICAL)` -- evaluate and
/// return EXPRESSION.
pub(crate) fn builtin_eval_expression(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() {
        if eval.interactive.is_called_interactively() {
            return Err(signal(
                "end-of-file",
                vec![Value::string("Error reading from stdin")],
            ));
        }
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("eval-expression"), Value::Int(0)],
        ));
    }
    if args.len() > 4 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("eval-expression"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    let expr = super::eval::value_to_expr_pub(&args[0]);
    eval.eval(&expr)
}

fn last_command_event_char(eval: &Evaluator) -> Option<char> {
    let event = dynamic_or_global_symbol_value(eval, "last-command-event")?;
    match event {
        Value::Char(c) => Some(c),
        Value::Int(n) if n >= 0 => char::from_u32(n as u32),
        _ => None,
    }
}

/// `(self-insert-command N &optional NOAUTOFILL)` -- insert the last typed
/// character N times.
pub(crate) fn builtin_self_insert_command(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() || args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("self-insert-command"),
                Value::Int(args.len() as i64),
            ],
        ));
    }
    let repeats = match args[0] {
        Value::Int(n) => n,
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("fixnump"), args[0].clone()],
            ))
        }
    };
    if repeats < 0 {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Negative repetition argument {}",
                repeats
            ))],
        ));
    }
    if repeats == 0 {
        return Ok(Value::Nil);
    }
    if args.get(1).is_some_and(|v| !v.is_nil()) {
        return Ok(Value::Nil);
    }

    let Some(ch) = last_command_event_char(eval) else {
        return Ok(Value::Nil);
    };
    let Some(repeat_count) = usize::try_from(repeats).ok() else {
        return Ok(Value::Nil);
    };
    let mut text = String::new();
    for _ in 0..repeat_count {
        text.push(ch);
    }
    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.insert(&text);
    }
    Ok(Value::Nil)
}

/// `(keyboard-quit)` -- cancel the current command sequence.
pub(crate) fn builtin_keyboard_quit(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("keyboard-quit", &args, 0)?;
    Err(signal("quit", vec![]))
}

/// `(find-file &optional FILENAME WILDCARDS)` -- visit FILENAME.
///
/// In batch mode interactive invocation without FILENAME signals EOF.
pub(crate) fn builtin_find_file_command(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() || args[0].is_nil() {
        return Err(signal(
            "end-of-file",
            vec![Value::string("Error reading from stdin")],
        ));
    }
    super::fileio::builtin_find_file_noselect(eval, vec![args[0].clone()])
}

/// `(save-buffer &optional ARG)` -- save current buffer.
///
/// In batch mode interactive invocation prompts for a file name and hits EOF.
pub(crate) fn builtin_save_buffer_command(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() || args[0].is_nil() {
        return Err(signal(
            "end-of-file",
            vec![Value::string("Error reading from stdin")],
        ));
    }
    Ok(Value::Nil)
}

/// `(set-mark-command ARG)` -- set mark and activate region.
pub(crate) fn builtin_set_mark_command(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set-mark-command", &args, 1)?;

    if args[0].is_nil() {
        // Nil argument sets mark at point and activates the region.
        return super::navigation::builtin_push_mark(
            eval,
            vec![Value::Nil, Value::Nil, Value::True],
        );
    }

    // Non-nil argument moves point to mark and preserves mark-active state.
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let mark = buf.mark().ok_or_else(|| {
        signal(
            "user-error",
            vec![Value::string("No mark set in this buffer")],
        )
    })?;
    buf.pt = mark;
    Ok(Value::Nil)
}

/// `(quoted-insert &optional ARG)` -- read a character and insert it.
///
/// In batch mode interactive invocation hits EOF while reading input.
pub(crate) fn builtin_quoted_insert_command(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() || args[0].is_nil() {
        return Err(signal(
            "end-of-file",
            vec![Value::string("Error reading from stdin")],
        ));
    }
    if !matches!(&args[0], Value::Int(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// `(universal-argument)` -- initialize a prefix argument command state.
pub(crate) fn builtin_universal_argument_command(
    _eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("universal-argument", &args, 0)?;
    Ok(Value::Lambda(std::sync::Arc::new(LambdaData {
        params: LambdaParams::simple(vec![]),
        body: vec![Expr::Symbol("nil".to_string())],
        env: None,
        docstring: None,
    })))
}

/// `(execute-extended-command PREFIXARG &optional COMMAND-NAME TYPED)`
/// Read a command name and execute it. This is the M-x equivalent.
/// In our stub implementation, COMMAND-NAME must be provided.
pub(crate) fn builtin_execute_extended_command(
    eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("execute-extended-command", &args, 1)?;
    expect_max_args("execute-extended-command", &args, 3)?;

    // Batch mode prompt path: M-x reads from stdin and hits EOF.
    if args.len() < 2 {
        return Err(signal(
            "end-of-file",
            vec![Value::string("Error reading from stdin")],
        ));
    }

    let command_name = if let Some(name) = args[1].as_str() {
        name.to_string()
    } else {
        let name = command_name_display(&args[1]);
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "\u{2018}{name}\u{2019} is not a valid command name"
            ))],
        ));
    };

    let command_designator = Value::symbol(command_name.clone());
    if !command_designator_p(eval, &command_designator) {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "\u{2018}{command_name}\u{2019} is not a valid command name"
            ))],
        ));
    }

    // Oracle M-x path invokes COMMAND interactively, with CURRENT-PREFIX-ARG
    // seeded from PREFIXARG and PREFIX-ARG reset for the command body.
    let mut frame = HashMap::new();
    frame.insert("current-prefix-arg".to_string(), args[0].clone());
    frame.insert("prefix-arg".to_string(), Value::Nil);
    eval.dynamic.push(frame);
    let result = builtin_call_interactively(eval, vec![command_designator]);
    eval.dynamic.pop();
    result?;
    Ok(Value::Nil)
}

fn command_name_display(value: &Value) -> String {
    if let Some(name) = value.as_symbol_name() {
        return name.to_string();
    }
    if let Some(text) = value.as_str() {
        return text.to_string();
    }
    if let Value::Int(n) = value {
        return n.to_string();
    }
    if let Value::Float(n) = value {
        return n.to_string();
    }
    value.type_name().to_string()
}

/// `(key-binding KEY &optional ACCEPT-DEFAULTS NO-REMAP POSITION)`
/// Return the binding for KEY in the current keymaps.
pub(crate) fn builtin_key_binding(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("key-binding", &args, 1)?;
    expect_max_args("key-binding", &args, 4)?;
    let string_designator = args[0].is_string();
    let no_remap = args.get(2).is_some_and(|v| v.is_truthy());

    let events = match super::kbd::key_events_from_designator(&args[0]) {
        Ok(events) => events,
        Err(super::kbd::KeyDesignatorError::WrongType(other)) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("arrayp"), other],
            ));
        }
        Err(super::kbd::KeyDesignatorError::Parse(_)) => {
            return Ok(Value::Nil);
        }
    };
    if events.is_empty() {
        if !string_designator {
            return Ok(Value::Nil);
        }
        let global_id = ensure_global_keymap(eval);
        let mut maps = Vec::new();
        if let Some(local_id) = eval.current_local_map {
            maps.push(Value::Int(encode_keymap_handle(local_id)));
        }
        maps.push(Value::Int(encode_keymap_handle(global_id)));
        return Ok(Value::list(maps));
    }

    if let Some(value) = key_binding_lookup_in_minor_mode_maps(eval, &events) {
        return Ok(key_binding_apply_remap(eval, value, no_remap));
    }

    // Try local map first, then global.
    if let Some(local_id) = eval.current_local_map {
        if let Some(value) = key_binding_lookup_in_keymap_id(eval, local_id, &events) {
            return Ok(key_binding_apply_remap(eval, value, no_remap));
        }
    }

    if let Some(global_id) = eval.keymaps.global_map() {
        if let Some(value) = key_binding_lookup_in_keymap_id(eval, global_id, &events) {
            return Ok(key_binding_apply_remap(eval, value, no_remap));
        }
    }
    if eval.keymaps.global_map().is_none()
        && events.len() == 1
        && is_plain_printable_char_event(&events[0])
    {
        return Ok(Value::symbol("self-insert-command"));
    }

    Ok(Value::Nil)
}

/// `(local-key-binding KEY &optional ACCEPT-DEFAULTS)`
pub(crate) fn builtin_local_key_binding(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("local-key-binding", &args, 1)?;
    expect_max_args("local-key-binding", &args, 2)?;

    let Some(local_id) = eval.current_local_map else {
        // Oracle batch behavior: when no local map is active, non-array KEY does
        // not error and local-key-binding simply returns nil.
        return Ok(Value::Nil);
    };

    let events = match super::kbd::key_events_from_designator(&args[0]) {
        Ok(events) => events,
        Err(super::kbd::KeyDesignatorError::WrongType(other)) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("arrayp"), other],
            ));
        }
        Err(super::kbd::KeyDesignatorError::Parse(_)) => {
            return Ok(Value::Nil);
        }
    };
    Ok(lookup_keymap_with_partial(eval, local_id, &events))
}

/// `(global-key-binding KEY &optional ACCEPT-DEFAULTS)`
pub(crate) fn builtin_global_key_binding(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("global-key-binding", &args, 1)?;
    expect_max_args("global-key-binding", &args, 2)?;

    let events = match super::kbd::key_events_from_designator(&args[0]) {
        Ok(events) => events,
        Err(super::kbd::KeyDesignatorError::WrongType(other)) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("arrayp"), other],
            ));
        }
        Err(super::kbd::KeyDesignatorError::Parse(_)) => {
            return Ok(Value::Nil);
        }
    };
    if events.is_empty() {
        return Ok(Value::Int(encode_keymap_handle(ensure_global_keymap(eval))));
    }
    if let Some(global_id) = eval.keymaps.global_map() {
        return Ok(lookup_keymap_with_partial(eval, global_id, &events));
    }
    if let Some(raw) = args[0].as_str() {
        if let Some(first) = raw.chars().next() {
            if !first.is_control() && first != '\u{7f}' {
                if raw.chars().nth(1).is_none() {
                    return Ok(Value::symbol("self-insert-command"));
                }
                return Ok(Value::Int(1));
            }
        }
    }
    if events.len() == 1 && is_plain_printable_char_event(&events[0]) {
        return Ok(Value::symbol("self-insert-command"));
    }

    Ok(Value::Nil)
}

fn minor_mode_map_entry(entry: &Value) -> Option<(String, Value)> {
    let Value::Cons(cell) = entry else {
        return None;
    };

    let (mode, cdr) = {
        let pair = cell.lock().expect("poisoned");
        (pair.car.clone(), pair.cdr.clone())
    };
    let mode_name = mode.as_symbol_name()?.to_string();
    let map_value = match cdr {
        Value::Cons(rest) => {
            let pair = rest.lock().expect("poisoned");
            pair.car.clone()
        }
        Value::Nil => return None,
        other => other,
    };
    Some((mode_name, map_value))
}

fn resolve_minor_mode_keymap_id(eval: &Evaluator, map_value: &Value) -> Result<Option<u64>, Flow> {
    match map_value {
        Value::Int(_) => expect_keymap_id(eval, map_value).map(Some),
        _ => Ok(None),
    }
}

fn key_binding_lookup_in_keymap_id(
    eval: &Evaluator,
    map_id: u64,
    events: &[KeyEvent],
) -> Option<Value> {
    if events.len() == 1 {
        return eval
            .keymaps
            .lookup_key(map_id, &events[0])
            .map(key_binding_to_value);
    }
    eval.keymaps
        .lookup_key_sequence(map_id, events)
        .map(key_binding_to_value)
}

fn key_binding_apply_remap(eval: &Evaluator, binding: Value, no_remap: bool) -> Value {
    if no_remap {
        return binding;
    }
    let Some(command_name) = binding.as_symbol_name().map(ToString::to_string) else {
        return binding;
    };
    match command_remapping_lookup_in_active_keymaps(eval, &command_name) {
        Some(remapped) if !remapped.is_nil() => remapped,
        _ => binding,
    }
}

fn key_binding_lookup_in_minor_mode_alist(
    eval: &Evaluator,
    events: &[KeyEvent],
    alist_value: &Value,
) -> Option<Value> {
    let entries = list_to_vec(alist_value)?;
    for entry in entries {
        let Some((mode_name, map_value)) = minor_mode_map_entry(&entry) else {
            continue;
        };
        if !dynamic_or_global_symbol_value(eval, &mode_name).is_some_and(|v| v.is_truthy()) {
            continue;
        }

        let map_id = match map_value {
            Value::Int(n) => decode_keymap_handle(n).filter(|id| eval.keymaps.is_keymap(*id)),
            _ => None,
        };
        let Some(map_id) = map_id else {
            continue;
        };

        if let Some(binding) = key_binding_lookup_in_keymap_id(eval, map_id, events) {
            return Some(binding);
        }
    }
    None
}

fn key_binding_lookup_in_minor_mode_maps(eval: &Evaluator, events: &[KeyEvent]) -> Option<Value> {
    if let Some(emulation_raw) = dynamic_or_global_symbol_value(eval, "emulation-mode-map-alists") {
        if let Some(emulation_entries) = list_to_vec(&emulation_raw) {
            for emulation_entry in emulation_entries {
                let alist_value = match emulation_entry.as_symbol_name() {
                    Some(name) => dynamic_or_global_symbol_value(eval, name).unwrap_or(Value::Nil),
                    None => emulation_entry,
                };
                if let Some(value) =
                    key_binding_lookup_in_minor_mode_alist(eval, events, &alist_value)
                {
                    return Some(value);
                }
            }
        }
    }

    for alist_name in ["minor-mode-overriding-map-alist", "minor-mode-map-alist"] {
        let Some(alist_value) = dynamic_or_global_symbol_value(eval, alist_name) else {
            continue;
        };
        if let Some(value) = key_binding_lookup_in_minor_mode_alist(eval, events, &alist_value) {
            return Some(value);
        }
    }
    None
}

fn lookup_minor_mode_binding_in_alist(
    eval: &Evaluator,
    events: &[KeyEvent],
    alist_value: &Value,
) -> Result<Option<(String, Value)>, Flow> {
    let Some(entries) = list_to_vec(alist_value) else {
        return Ok(None);
    };

    for entry in entries {
        let Some((mode_name, map_value)) = minor_mode_map_entry(&entry) else {
            continue;
        };
        if !dynamic_or_global_symbol_value(eval, &mode_name).is_some_and(|v| v.is_truthy()) {
            continue;
        }

        let Some(map_id) = resolve_minor_mode_keymap_id(eval, &map_value)? else {
            continue;
        };
        let binding = lookup_keymap_with_partial(eval, map_id, events);
        if binding.is_nil() {
            continue;
        }

        return Ok(Some((mode_name, binding)));
    }

    Ok(None)
}

/// `(minor-mode-key-binding KEY &optional ACCEPT-DEFAULTS)`
/// Look up KEY in active minor mode keymaps.
pub(crate) fn builtin_minor_mode_key_binding(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("minor-mode-key-binding", &args, 1)?;
    expect_max_args("minor-mode-key-binding", &args, 2)?;

    // Emacs returns nil (not a type error) for non-array key designators here.
    let events = match super::kbd::key_events_from_designator(&args[0]) {
        Ok(events) => events,
        Err(_) => return Ok(Value::Nil),
    };

    if let Some(emulation_raw) = dynamic_or_global_symbol_value(eval, "emulation-mode-map-alists") {
        if let Some(emulation_entries) = list_to_vec(&emulation_raw) {
            for emulation_entry in emulation_entries {
                let alist_value = match emulation_entry.as_symbol_name() {
                    Some(name) => dynamic_or_global_symbol_value(eval, name).unwrap_or(Value::Nil),
                    None => emulation_entry,
                };
                if let Some((mode_name, binding)) =
                    lookup_minor_mode_binding_in_alist(eval, &events, &alist_value)?
                {
                    return Ok(Value::list(vec![Value::cons(
                        Value::symbol(mode_name),
                        binding,
                    )]));
                }
            }
        }
    }

    for alist_name in ["minor-mode-overriding-map-alist", "minor-mode-map-alist"] {
        let Some(alist_value) = dynamic_or_global_symbol_value(eval, alist_name) else {
            continue;
        };
        if let Some((mode_name, binding)) =
            lookup_minor_mode_binding_in_alist(eval, &events, &alist_value)?
        {
            return Ok(Value::list(vec![Value::cons(
                Value::symbol(mode_name),
                binding,
            )]));
        }
    }

    Ok(Value::Nil)
}

/// `(where-is-internal DEFINITION &optional KEYMAP FIRSTONLY NOINDIRECT NO-REMAP)`
/// Return list of key sequences that invoke DEFINITION.
pub(crate) fn builtin_where_is_internal(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("where-is-internal", &args, 1)?;
    expect_max_args("where-is-internal", &args, 5)?;

    let definition = &args[0];
    let first_only = args.get(2).is_some_and(|v| !v.is_nil());

    let map_id = if let Some(keymap) = args.get(1) {
        if keymap.is_nil() {
            match eval.keymaps.global_map() {
                Some(id) => id,
                None => return Ok(Value::Nil),
            }
        } else {
            expect_keymap_id(eval, keymap)?
        }
    } else {
        match eval.keymaps.global_map() {
            Some(id) => id,
            None => return Ok(Value::Nil),
        }
    };

    let mut prefix = Vec::new();
    let mut visiting = Vec::new();
    let mut sequences = Vec::new();
    collect_where_is_sequences(
        eval,
        map_id,
        definition,
        &mut prefix,
        &mut visiting,
        &mut sequences,
        first_only,
    );

    if sequences.is_empty() {
        return Ok(Value::Nil);
    }

    if first_only {
        return Ok(key_sequence_to_value(&sequences[0]));
    }
    let out: Vec<Value> = sequences
        .iter()
        .map(|seq| key_sequence_to_value(seq))
        .collect();
    Ok(Value::list(out))
}

/// `(substitute-command-keys STRING)`
/// Replace \\[COMMAND], \\{KEYMAP}, and \\<KEYMAP> sequences in STRING.
pub(crate) fn builtin_substitute_command_keys(
    eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("substitute-command-keys", &args, 1)?;
    expect_max_args("substitute-command-keys", &args, 3)?;
    let s = match args[0].as_str() {
        Some(s) => s.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), args[0].clone()],
            ))
        }
    };

    // Simple substitution: replace \\[command] with "M-x command"
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&next) = chars.peek() {
                if next == '[' {
                    chars.next(); // consume '['
                    let mut cmd = String::new();
                    for c in chars.by_ref() {
                        if c == ']' {
                            break;
                        }
                        cmd.push(c);
                    }
                    // Try to find a key binding for the command
                    let key_desc = find_key_for_command(eval, &cmd);
                    result.push_str(&key_desc);
                    continue;
                } else if next == '\\' {
                    chars.next();
                    result.push('\\');
                    continue;
                }
            }
        }
        result.push(ch);
    }

    Ok(Value::string(result))
}

/// `(describe-key-briefly KEY &optional INSERT UNTRANSLATED)`
/// Print the command bound to KEY.
pub(crate) fn builtin_describe_key_briefly(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("describe-key-briefly", &args, 3)?;
    if args.is_empty() {
        return Ok(Value::string(""));
    }

    let events = match super::kbd::key_events_from_designator(&args[0]) {
        Ok(events) => events,
        Err(super::kbd::KeyDesignatorError::WrongType(other)) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("sequencep"), other],
            ));
        }
        Err(super::kbd::KeyDesignatorError::Parse(_)) => return Ok(Value::Nil),
    };
    if events.is_empty() {
        return Err(signal(
            "args-out-of-range",
            vec![args[0].clone(), Value::Int(-1)],
        ));
    }

    let key_desc = match args[0].as_str() {
        Some(s) => s.to_string(),
        None => KeymapManager::format_key_sequence(&events),
    };

    // Look up the binding
    let global_map_missing = eval.keymaps.global_map().is_none();
    let mut binding_val = builtin_key_binding(eval, vec![args[0].clone()])?;
    if binding_val.is_nil()
        && global_map_missing
        && events.len() == 1
        && is_plain_printable_char_event(&events[0])
    {
        binding_val = Value::symbol("self-insert-command");
    }
    let description = if binding_val.is_nil() {
        format!("{} is undefined", key_desc)
    } else if let Some(name) = binding_val.as_symbol_name() {
        format!("{} runs the command {}", key_desc, name)
    } else {
        format!("{} is bound to {}", key_desc, binding_val)
    };

    if args.get(1).is_some_and(|v| !v.is_nil()) {
        Ok(Value::Nil)
    } else {
        Ok(Value::string(description))
    }
}

/// `(this-command-keys)` -> string of keys that invoked current command.
pub(crate) fn builtin_this_command_keys(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("this-command-keys", &args, 0)?;
    let read_keys = eval.read_command_keys();
    if !read_keys.is_empty() {
        if let Some(rendered) = command_key_events_to_string(read_keys) {
            return Ok(Value::string(rendered));
        }
        return Ok(Value::vector(read_keys.to_vec()));
    }

    let keys = eval.interactive.this_command_keys();
    Ok(Value::string(keys.join(" ")))
}

/// `(this-command-keys-vector)` -> vector of keys that invoked current command.
pub(crate) fn builtin_this_command_keys_vector(
    eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("this-command-keys-vector", &args, 0)?;
    let read_keys = eval.read_command_keys();
    if !read_keys.is_empty() {
        return Ok(Value::vector(read_keys.to_vec()));
    }

    let keys = eval.interactive.this_command_keys();
    let vals: Vec<Value> = keys.iter().map(|k| Value::string(k.clone())).collect();
    Ok(Value::vector(vals))
}

/// `(clear-this-command-keys &optional KEEP-RECORD)` -> nil.
///
/// Clears current command-key context used by `this-command-keys*`.
/// When KEEP-RECORD is nil or omitted, also clears recent input history used
/// by `recent-keys`.
pub(crate) fn builtin_clear_this_command_keys(
    eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("clear-this-command-keys", &args, 1)?;
    let keep_record = args.first().is_some_and(|arg| arg.is_truthy());
    eval.clear_read_command_keys();
    eval.interactive.set_this_command_keys(Vec::new());
    if !keep_record {
        eval.clear_recent_input_events();
    }
    Ok(Value::Nil)
}

fn command_key_events_to_string(events: &[Value]) -> Option<String> {
    let mut out = String::new();
    for event in events {
        let ch = match event {
            Value::Char(c) => *c,
            Value::Int(n) if *n >= 0 => char::from_u32(*n as u32)?,
            _ => return None,
        };
        out.push(ch);
    }
    Some(out)
}

// ---------------------------------------------------------------------------
// Thing-at-point functions
// ---------------------------------------------------------------------------

fn maybe_materialize_word_at_point(eval: &mut Evaluator) {
    let should_materialize = {
        let obarray = &eval.obarray;
        if obarray.fboundp("word-at-point") {
            return;
        }
        // Respect explicit user-level `fmakunbound` after materialization.
        // Startup masking keeps the symbol uninterned and should still allow
        // first bootstrap.
        !(obarray.is_function_unbound("word-at-point")
            && obarray.intern_soft("word-at-point").is_some())
    };
    if should_materialize {
        eval.set_function("word-at-point", Value::Subr("word-at-point".to_string()));
    }
}

/// `(thing-at-point THING &optional NO-PROPERTIES)` -> the THING at point.
pub(crate) fn builtin_thing_at_point(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    maybe_materialize_word_at_point(eval);
    expect_min_args("thing-at-point", &args, 1)?;
    expect_max_args("thing-at-point", &args, 2)?;

    let thing = match args[0].as_symbol_name() {
        Some(s) => s.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            ))
        }
    };

    let buf = match eval.buffers.current_buffer() {
        Some(b) => b,
        None => return Ok(Value::Nil),
    };

    let text = buf.buffer_string();
    let byte_offset = buf.pt.saturating_sub(buf.begv);
    // pt is a 0-based byte position; convert to a 0-based char index
    let idx = text[..byte_offset.min(text.len())].chars().count();

    match thing.as_str() {
        "word" => Ok(extract_thing_word(&text, idx)),
        "symbol" => Ok(extract_thing_symbol(&text, idx)),
        "line" => Ok(extract_thing_line(&text, idx)),
        "sentence" => Ok(extract_thing_line(&text, idx)), // simplified
        "sexp" => Ok(extract_thing_symbol(&text, idx)),   // simplified
        "whitespace" => Ok(extract_thing_whitespace(&text, idx)),
        "number" => Ok(extract_thing_number(&text, idx)),
        "url" => Ok(extract_thing_url(&text, idx)),
        "email" => Ok(extract_thing_email(&text, idx)),
        "filename" => Ok(extract_thing_filename(&text, idx)),
        _ => Ok(Value::Nil),
    }
}

/// `(bounds-of-thing-at-point THING)` -> (START . END) or nil.
pub(crate) fn builtin_bounds_of_thing_at_point(
    eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    maybe_materialize_word_at_point(eval);
    expect_args("bounds-of-thing-at-point", &args, 1)?;

    let thing = match args[0].as_symbol_name() {
        Some(s) => s.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            ))
        }
    };

    let buf = match eval.buffers.current_buffer() {
        Some(b) => b,
        None => return Ok(Value::Nil),
    };

    let text = buf.buffer_string();
    let byte_offset = buf.pt.saturating_sub(buf.begv);
    // pt is a 0-based byte position; convert to a 0-based char index
    let idx = text[..byte_offset.min(text.len())].chars().count();

    let bounds = match thing.as_str() {
        "word" => bounds_word(&text, idx),
        "symbol" => bounds_symbol(&text, idx),
        "line" => bounds_line(&text, idx),
        "sentence" => bounds_line(&text, idx),
        "sexp" => bounds_symbol(&text, idx),
        "whitespace" => bounds_whitespace(&text, idx),
        "number" => bounds_number(&text, idx),
        "url" => bounds_url(&text, idx),
        "email" => bounds_email(&text, idx),
        "filename" => bounds_filename(&text, idx),
        _ => None,
    };

    match bounds {
        Some((start, end)) => {
            // Convert from 0-based to 1-based
            Ok(Value::cons(
                Value::Int((start + 1) as i64),
                Value::Int((end + 1) as i64),
            ))
        }
        None => Ok(Value::Nil),
    }
}

/// `(symbol-at-point)` -> symbol at point or nil.
pub(crate) fn builtin_symbol_at_point(eval: &mut Evaluator, _args: Vec<Value>) -> EvalResult {
    maybe_materialize_word_at_point(eval);

    let thing = builtin_thing_at_point(eval, vec![Value::symbol("symbol")])?;
    match thing {
        Value::Str(s) => Ok(Value::symbol((*s).clone())),
        _ => Ok(Value::Nil),
    }
}

/// `(word-at-point &optional NO-PROPERTIES)` -> word at point or nil.
pub(crate) fn builtin_word_at_point(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("word-at-point", &args, 1)?;
    let mut thing_args = vec![Value::symbol("word")];
    if let Some(no_properties) = args.first() {
        thing_args.push(no_properties.clone());
    }
    builtin_thing_at_point(eval, thing_args)
}

// ---------------------------------------------------------------------------
// Special forms for mode definition (called from eval.rs)
// ---------------------------------------------------------------------------

/// `(define-minor-mode MODE DOC &rest BODY)` with keyword args
/// :lighter :keymap :global
///
/// Expands to:
///   - defvar MODE (the toggle variable)
///   - defun MODE (the toggle function)
///   - registers the minor mode in ModeRegistry
pub(crate) fn sf_define_minor_mode(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.len() < 2 {
        return Err(signal("wrong-number-of-arguments", vec![]));
    }

    let Expr::Symbol(mode_name) = &tail[0] else {
        return Err(signal("wrong-type-argument", vec![]));
    };

    // Parse keyword arguments from the tail
    let mut lighter: Option<String> = None;
    let mut keymap_name: Option<String> = None;
    let mut global = false;
    let mut body_start = 2; // skip mode name and docstring

    // Skip docstring if present
    if tail.len() > 1 {
        if let Expr::Str(_) = &tail[1] {
            body_start = 2;
        } else {
            body_start = 1; // no docstring
        }
    }

    // Parse keyword args
    let mut i = body_start;
    while i + 1 < tail.len() {
        match &tail[i] {
            Expr::Keyword(k) if k == ":lighter" => {
                if let Expr::Str(s) = &tail[i + 1] {
                    lighter = Some(s.clone());
                }
                i += 2;
            }
            Expr::Keyword(k) if k == ":keymap" => {
                if let Expr::Symbol(s) = &tail[i + 1] {
                    keymap_name = Some(s.clone());
                }
                i += 2;
            }
            Expr::Keyword(k) if k == ":global" => {
                match &tail[i + 1] {
                    Expr::Bool(true) => {
                        global = true;
                    }
                    Expr::Symbol(s) if s == "t" => {
                        global = true;
                    }
                    _ => {}
                }
                i += 2;
            }
            _ => break,
        }
    }

    let body_forms = &tail[i..];

    // 1. Create the toggle variable (defvar MODE nil)
    eval.obarray.set_symbol_value(mode_name, Value::Nil);
    eval.obarray.make_special(mode_name);

    // 2. Register with ModeRegistry
    let mode = MinorMode {
        name: mode_name.clone(),
        lighter: lighter.clone(),
        keymap_name: keymap_name.clone(),
        global,
        body: None,
    };
    eval.modes.register_minor_mode(mode);

    // 3. Register as an interactive command
    eval.interactive
        .register_interactive(mode_name, InteractiveSpec::no_args());

    // 4. Create toggle function that:
    //    - Toggles the variable
    //    - Runs the body forms
    //    - Returns the new value
    let mode_name_owned = mode_name.clone();
    let _global_flag = global;

    // Build a lambda body for the toggle function
    // We synthesize: (progn (setq MODE (not MODE)) BODY...)
    let toggle_body_exprs: Vec<Expr> = {
        let mut exprs = Vec::new();
        // (setq MODE (not MODE))
        exprs.push(Expr::List(vec![
            Expr::Symbol("setq".to_string()),
            Expr::Symbol(mode_name_owned.clone()),
            Expr::List(vec![
                Expr::Symbol("not".to_string()),
                Expr::Symbol(mode_name_owned.clone()),
            ]),
        ]));
        // Append body forms
        for form in body_forms {
            exprs.push(form.clone());
        }
        // Return the mode variable value
        exprs.push(Expr::Symbol(mode_name_owned.clone()));
        exprs
    };

    let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
        params: LambdaParams::simple(vec![]),
        body: toggle_body_exprs,
        env: None,
        docstring: None,
    }));

    eval.obarray.set_symbol_function(mode_name, lambda);

    Ok(Value::symbol(mode_name.clone()))
}

/// `(define-derived-mode MODE PARENT NAME DOC &rest BODY)` with keyword args
/// :syntax-table :abbrev-table
///
/// Creates a major mode that derives from PARENT.
pub(crate) fn sf_define_derived_mode(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.len() < 3 {
        return Err(signal("wrong-number-of-arguments", vec![]));
    }

    let Expr::Symbol(mode_name) = &tail[0] else {
        return Err(signal("wrong-type-argument", vec![]));
    };

    // Parent can be nil or a symbol
    let parent = match &tail[1] {
        Expr::Symbol(s) if s == "nil" => None,
        Expr::Symbol(s) => Some(s.clone()),
        _ => None,
    };

    // Pretty name (3rd arg) - evaluate it
    let pretty_name = match &tail[2] {
        Expr::Str(s) => s.clone(),
        _ => {
            let val = eval.eval(&tail[2])?;
            match val.as_str() {
                Some(s) => s.to_string(),
                None => mode_name.clone(),
            }
        }
    };

    // Parse optional keyword args and body
    let mut syntax_table_name: Option<String> = None;
    let mut abbrev_table_name: Option<String> = None;
    let mut body_start = 3;

    // Skip docstring if present
    if tail.len() > 3 {
        if let Expr::Str(_) = &tail[3] {
            body_start = 4;
        }
    }

    // Parse keyword args
    let mut i = body_start;
    while i + 1 < tail.len() {
        match &tail[i] {
            Expr::Keyword(k) if k == ":syntax-table" => {
                if let Expr::Symbol(s) = &tail[i + 1] {
                    syntax_table_name = Some(s.clone());
                }
                i += 2;
            }
            Expr::Keyword(k) if k == ":abbrev-table" => {
                if let Expr::Symbol(s) = &tail[i + 1] {
                    abbrev_table_name = Some(s.clone());
                }
                i += 2;
            }
            _ => break,
        }
    }

    let body_forms = &tail[i..];

    // Derive hook name and keymap name
    let hook_name = format!("{}-hook", mode_name);
    let keymap_name = format!("{}-map", mode_name);

    // 1. Register the major mode
    let mode = MajorMode {
        name: mode_name.clone(),
        pretty_name: pretty_name.clone(),
        parent: parent.clone(),
        mode_hook: hook_name.clone(),
        keymap_name: Some(keymap_name.clone()),
        syntax_table_name: syntax_table_name.clone(),
        abbrev_table_name: abbrev_table_name.clone(),
        font_lock: None,
        body: None,
    };
    eval.modes.register_major_mode(mode);

    // 2. Create the hook variable
    eval.obarray.set_symbol_value(&hook_name, Value::Nil);
    eval.obarray.make_special(&hook_name);

    // 3. Register as interactive command
    eval.interactive
        .register_interactive(mode_name, InteractiveSpec::no_args());

    // 4. Create mode function that:
    //    - Calls parent mode first (if any)
    //    - Runs body
    //    - Sets major-mode variable
    //    - Runs mode hook
    let mut func_body: Vec<Expr> = Vec::new();

    // Call parent mode if it exists
    if let Some(ref par) = parent {
        func_body.push(Expr::List(vec![Expr::Symbol(par.clone())]));
    }

    // (setq major-mode 'MODE)
    func_body.push(Expr::List(vec![
        Expr::Symbol("setq".to_string()),
        Expr::Symbol("major-mode".to_string()),
        Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            Expr::Symbol(mode_name.clone()),
        ]),
    ]));

    // (setq mode-name PRETTY-NAME)
    func_body.push(Expr::List(vec![
        Expr::Symbol("setq".to_string()),
        Expr::Symbol("mode-name".to_string()),
        Expr::Str(pretty_name),
    ]));

    // Body forms
    for form in body_forms {
        func_body.push(form.clone());
    }

    // (run-hooks 'MODE-hook)
    func_body.push(Expr::List(vec![
        Expr::Symbol("run-hooks".to_string()),
        Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            Expr::Symbol(hook_name),
        ]),
    ]));

    let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
        params: LambdaParams::simple(vec![]),
        body: func_body,
        env: None,
        docstring: None,
    }));

    eval.obarray.set_symbol_function(mode_name, lambda);

    // Set up major-mode and mode-name as special variables
    if !eval.obarray.boundp("major-mode") {
        eval.obarray
            .set_symbol_value("major-mode", Value::symbol("fundamental-mode"));
    }
    eval.obarray.make_special("major-mode");
    if !eval.obarray.boundp("mode-name") {
        eval.obarray
            .set_symbol_value("mode-name", Value::string("Fundamental"));
    }
    eval.obarray.make_special("mode-name");

    Ok(Value::symbol(mode_name.clone()))
}

/// `(define-generic-mode MODE COMMENT-LIST KEYWORD-LIST FONT-LOCK-LIST
///    AUTO-MODE-LIST FUNCTION-LIST &optional DOCSTRING)`
/// Simplified generic mode definition.
pub(crate) fn sf_define_generic_mode(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.len() < 5 {
        return Err(signal("wrong-number-of-arguments", vec![]));
    }

    let Expr::Symbol(mode_name) = &tail[0] else {
        return Err(signal("wrong-type-argument", vec![]));
    };

    // Register as a basic major mode with no parent
    let mode = MajorMode {
        name: mode_name.clone(),
        pretty_name: mode_name.replace('-', " "),
        parent: None,
        mode_hook: format!("{}-hook", mode_name),
        keymap_name: None,
        syntax_table_name: None,
        abbrev_table_name: None,
        font_lock: None,
        body: None,
    };
    eval.modes.register_major_mode(mode);

    // Register as interactive
    eval.interactive
        .register_interactive(mode_name, InteractiveSpec::no_args());

    // Create a simple mode function
    let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
        params: LambdaParams::simple(vec![]),
        body: vec![Expr::List(vec![
            Expr::Symbol("setq".to_string()),
            Expr::Symbol("major-mode".to_string()),
            Expr::List(vec![
                Expr::Symbol("quote".to_string()),
                Expr::Symbol(mode_name.clone()),
            ]),
        ])],
        env: None,
        docstring: None,
    }));

    eval.obarray.set_symbol_function(mode_name, lambda);

    Ok(Value::symbol(mode_name.clone()))
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert a KeyBinding to a Value.
fn key_binding_to_value(binding: &KeyBinding) -> Value {
    match binding {
        KeyBinding::Command(name) => Value::symbol(name.clone()),
        KeyBinding::LispValue(v) => v.clone(),
        KeyBinding::Prefix(id) => Value::symbol(format!("keymap-{}", id)),
    }
}

fn is_plain_printable_char_event(event: &KeyEvent) -> bool {
    matches!(
        event,
        KeyEvent::Char {
            code,
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        } if !code.is_control() && *code != '\u{7f}'
    )
}

fn ensure_global_keymap(eval: &mut Evaluator) -> u64 {
    if let Some(id) = eval.keymaps.global_map() {
        id
    } else {
        let id = eval.keymaps.make_keymap();
        eval.keymaps.set_global_map(id);
        id
    }
}

fn command_remapping_keymap_arg_valid(eval: &Evaluator, value: &Value) -> bool {
    match value {
        Value::Int(n) => decode_keymap_handle(*n).is_some_and(|id| eval.keymaps.is_keymap(id)),
        // Oracle accepts cons/list keymap-like objects in this slot.
        Value::Cons(_) => true,
        _ => false,
    }
}

fn command_remapping_lookup_keymap_id(eval: &Evaluator, keymap: Option<&Value>) -> Option<u64> {
    match keymap {
        Some(Value::Int(n)) => decode_keymap_handle(*n).filter(|id| eval.keymaps.is_keymap(*id)),
        Some(_) => None,
        None => None,
    }
}

fn command_remapping_lookup_in_keymap_id(
    eval: &Evaluator,
    map_id: u64,
    command_name: &str,
) -> Option<Value> {
    let command_event = command_remapping_command_event(command_name);
    eval.keymaps
        .lookup_key_sequence(map_id, &[remap_event(), command_event])
        .map(command_remapping_binding_value)
}

fn command_remapping_lookup_in_minor_mode_alist(
    eval: &Evaluator,
    command_name: &str,
    alist_value: &Value,
) -> Option<Value> {
    let entries = list_to_vec(alist_value)?;
    for entry in entries {
        let Some((mode_name, map_value)) = minor_mode_map_entry(&entry) else {
            continue;
        };
        if !dynamic_or_global_symbol_value(eval, &mode_name).is_some_and(|v| v.is_truthy()) {
            continue;
        }

        let map_id = match map_value {
            Value::Int(n) => decode_keymap_handle(n).filter(|id| eval.keymaps.is_keymap(*id)),
            _ => None,
        };
        let Some(map_id) = map_id else {
            continue;
        };

        if let Some(value) = command_remapping_lookup_in_keymap_id(eval, map_id, command_name) {
            return Some(value);
        }
    }
    None
}

fn command_remapping_lookup_in_minor_mode_maps(
    eval: &Evaluator,
    command_name: &str,
) -> Option<Value> {
    if let Some(emulation_raw) = dynamic_or_global_symbol_value(eval, "emulation-mode-map-alists") {
        if let Some(emulation_entries) = list_to_vec(&emulation_raw) {
            for emulation_entry in emulation_entries {
                let alist_value = match emulation_entry.as_symbol_name() {
                    Some(name) => dynamic_or_global_symbol_value(eval, name).unwrap_or(Value::Nil),
                    None => emulation_entry,
                };
                if let Some(value) =
                    command_remapping_lookup_in_minor_mode_alist(eval, command_name, &alist_value)
                {
                    return Some(value);
                }
            }
        }
    }

    for alist_name in ["minor-mode-overriding-map-alist", "minor-mode-map-alist"] {
        let Some(alist_value) = dynamic_or_global_symbol_value(eval, alist_name) else {
            continue;
        };
        if let Some(value) =
            command_remapping_lookup_in_minor_mode_alist(eval, command_name, &alist_value)
        {
            return Some(value);
        }
    }

    None
}

fn command_remapping_lookup_in_active_keymaps(
    eval: &Evaluator,
    command_name: &str,
) -> Option<Value> {
    if let Some(value) = command_remapping_lookup_in_minor_mode_maps(eval, command_name) {
        return Some(value);
    }
    if let Some(local_id) = eval.current_local_map {
        if let Some(value) = command_remapping_lookup_in_keymap_id(eval, local_id, command_name) {
            return Some(value);
        }
    }
    let global_id = eval.keymaps.global_map()?;
    command_remapping_lookup_in_keymap_id(eval, global_id, command_name)
}

fn command_remapping_command_name(command: &Value) -> Option<String> {
    Some(match command {
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        Value::Symbol(s) => s.clone(),
        _ => return None,
    })
}

fn command_remapping_command_event(command_name: &str) -> KeyEvent {
    KeyEvent::Function {
        name: command_name.to_string(),
        ctrl: false,
        meta: false,
        shift: false,
        super_: false,
    }
}

fn remap_event() -> KeyEvent {
    KeyEvent::Function {
        name: "remap".to_string(),
        ctrl: false,
        meta: false,
        shift: false,
        super_: false,
    }
}

fn command_remapping_list_tail(value: &Value, n: usize) -> Option<Value> {
    let mut cursor = value.clone();
    for _ in 0..n {
        match cursor {
            Value::Cons(cell) => {
                cursor = {
                    let pair = cell.lock().expect("poisoned");
                    pair.cdr.clone()
                };
            }
            _ => return None,
        }
    }
    Some(cursor)
}

fn command_remapping_nth_list_element(value: &Value, index: usize) -> Option<Value> {
    let tail = command_remapping_list_tail(value, index)?;
    match tail {
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            Some(pair.car.clone())
        }
        _ => None,
    }
}

fn command_remapping_lookup_in_lisp_remap_entry(
    entry: &Value,
    command_name: &str,
) -> Option<Value> {
    if command_remapping_nth_list_element(entry, 0)?.as_symbol_name() != Some("remap") {
        return None;
    }
    if command_remapping_nth_list_element(entry, 1)?.as_symbol_name() != Some("keymap") {
        return None;
    }

    let mut bindings = command_remapping_list_tail(entry, 2)?;
    while let Value::Cons(cell) = bindings {
        let (binding_entry, rest) = {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        };
        if let Value::Cons(binding_pair) = binding_entry {
            let (binding_key, binding_target) = {
                let pair = binding_pair.lock().expect("poisoned");
                (pair.car.clone(), pair.cdr.clone())
            };
            if binding_key.as_symbol_name() == Some(command_name) {
                return Some(binding_target);
            }
        }
        bindings = rest;
    }
    None
}

fn command_remapping_lookup_in_lisp_keymap(keymap: &Value, command_name: &str) -> Option<Value> {
    let mut cursor = keymap.clone();
    let mut first = true;
    while let Value::Cons(cell) = cursor {
        let (car, cdr) = {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        };
        if first {
            if car.as_symbol_name() != Some("keymap") {
                return None;
            }
            first = false;
        } else if let Some(target) =
            command_remapping_lookup_in_lisp_remap_entry(&car, command_name)
        {
            return Some(target);
        }
        cursor = cdr;
    }
    None
}

fn command_remapping_menu_item_target(value: &Value) -> Option<Value> {
    let mut current = value.clone();
    let mut index = 0usize;
    let mut head_is_menu_item = false;
    while let Value::Cons(cell) = current {
        let (car, cdr) = {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        };
        if index == 0 {
            head_is_menu_item = car.as_symbol_name() == Some("menu-item");
        } else if index == 2 {
            return head_is_menu_item.then_some(car);
        }
        current = cdr;
        index += 1;
    }
    None
}

fn command_remapping_normalize_target(raw: Value) -> Value {
    if let Some(menu_target) = command_remapping_menu_item_target(&raw) {
        // Oracle unwraps well-formed menu-item bindings for command remapping.
        // Integer payloads in command slot still collapse to nil.
        return if menu_target.is_integer() {
            Value::Nil
        } else {
            menu_target
        };
    }

    // Oracle treats plain integer and `t` remap targets as no-remap.
    if matches!(raw, Value::Int(_) | Value::True) {
        Value::Nil
    } else {
        raw
    }
}

fn command_remapping_binding_value(binding: &KeyBinding) -> Value {
    let raw = match binding {
        KeyBinding::Command(name) => Value::symbol(name.clone()),
        KeyBinding::LispValue(value) => value.clone(),
        KeyBinding::Prefix(id) => Value::Int(encode_keymap_handle(*id)),
    };
    command_remapping_normalize_target(raw)
}

fn expect_keymap_id(eval: &Evaluator, value: &Value) -> Result<u64, Flow> {
    match value {
        Value::Int(n) => {
            let Some(id) = decode_keymap_handle(*n) else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("keymapp"), value.clone()],
                ));
            };
            if eval.keymaps.is_keymap(id) {
                Ok(id)
            } else {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("keymapp"), value.clone()],
                ))
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("keymapp"), other.clone()],
        )),
    }
}

fn key_event_to_value(event: &KeyEvent) -> Value {
    match event {
        KeyEvent::Char {
            code,
            ctrl,
            meta,
            shift,
            super_,
        } if !ctrl && !meta && !shift && !super_ => Value::Int(*code as i64),
        _ => Value::symbol(KeymapManager::format_key_event(event)),
    }
}

fn key_sequence_to_value(seq: &[KeyEvent]) -> Value {
    Value::vector(seq.iter().map(key_event_to_value).collect())
}

fn lookup_keymap_with_partial(eval: &Evaluator, map_id: u64, events: &[KeyEvent]) -> Value {
    if events.is_empty() {
        return Value::Int(encode_keymap_handle(map_id));
    }

    if events.len() == 1 {
        return match eval.keymaps.lookup_key(map_id, &events[0]) {
            Some(binding) => key_binding_to_value(binding),
            None => Value::Nil,
        };
    }

    let mut current_map = map_id;
    for (i, key) in events.iter().enumerate() {
        let Some(binding) = eval.keymaps.lookup_key(current_map, key) else {
            return if i == 0 { Value::Int(1) } else { Value::Nil };
        };

        if i == events.len() - 1 {
            return key_binding_to_value(binding);
        }

        match binding {
            KeyBinding::Prefix(next_map) => current_map = *next_map,
            _ => return Value::Int((i + 1) as i64),
        }
    }

    Value::Nil
}

fn binding_matches_definition(binding: &KeyBinding, definition: &Value) -> bool {
    match binding {
        KeyBinding::Command(name) => {
            definition.as_symbol_name().is_some_and(|sym| sym == name)
                || matches!(definition, Value::Subr(subr) if subr == name)
        }
        KeyBinding::LispValue(value) => value == definition,
        KeyBinding::Prefix(_) => false,
    }
}

fn collect_where_is_sequences(
    eval: &Evaluator,
    map_id: u64,
    definition: &Value,
    prefix: &mut Vec<KeyEvent>,
    visiting: &mut Vec<u64>,
    out: &mut Vec<Vec<KeyEvent>>,
    first_only: bool,
) -> bool {
    if visiting.contains(&map_id) {
        return false;
    }
    visiting.push(map_id);

    let (entries, parent) = match eval.keymaps.get(map_id) {
        Some(km) => {
            let mut items: Vec<(KeyEvent, KeyBinding)> = km
                .bindings
                .iter()
                .map(|(event, binding)| (event.clone(), binding.clone()))
                .collect();
            items.sort_by(|(a, _), (b, _)| {
                KeymapManager::format_key_event(a).cmp(&KeymapManager::format_key_event(b))
            });
            (items, km.parent)
        }
        None => {
            visiting.pop();
            return false;
        }
    };

    for (event, binding) in entries {
        prefix.push(event);
        match binding {
            KeyBinding::Prefix(next_map) => {
                if collect_where_is_sequences(
                    eval, next_map, definition, prefix, visiting, out, first_only,
                ) {
                    visiting.pop();
                    prefix.pop();
                    return true;
                }
            }
            other => {
                if binding_matches_definition(&other, definition) {
                    out.push(prefix.clone());
                    if first_only {
                        visiting.pop();
                        prefix.pop();
                        return true;
                    }
                }
            }
        }
        prefix.pop();
    }

    if let Some(parent_id) = parent {
        if collect_where_is_sequences(
            eval, parent_id, definition, prefix, visiting, out, first_only,
        ) {
            visiting.pop();
            return true;
        }
    }

    visiting.pop();
    false
}

/// Find the key binding description for a command name.
fn find_key_for_command(eval: &Evaluator, command: &str) -> String {
    // Search global map for the command
    if let Some(global_id) = eval.keymaps.global_map() {
        if let Some(km) = eval.keymaps.get(global_id) {
            for (event, binding) in &km.bindings {
                if let KeyBinding::Command(name) = binding {
                    if name == command {
                        return KeymapManager::format_key_event(event);
                    }
                }
            }
        }
    }

    // If local map is set, search it too
    if let Some(local_id) = eval.current_local_map {
        if let Some(km) = eval.keymaps.get(local_id) {
            for (event, binding) in &km.bindings {
                if let KeyBinding::Command(name) = binding {
                    if name == command {
                        return KeymapManager::format_key_event(event);
                    }
                }
            }
        }
    }

    // Fallback: "M-x command"
    format!("M-x {}", command)
}

// ---------------------------------------------------------------------------
// Thing-at-point extraction helpers
// ---------------------------------------------------------------------------

fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric()
        || matches!(
            c,
            '_' | '-' | '.' | '+' | '*' | '/' | '?' | '!' | '<' | '>' | '=' | ':'
        )
}

fn is_filename_char(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '-' | '.' | '/' | '~')
}

fn extract_thing_word(text: &str, idx: usize) -> Value {
    match bounds_word(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn extract_thing_symbol(text: &str, idx: usize) -> Value {
    match bounds_symbol(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn extract_thing_line(text: &str, idx: usize) -> Value {
    match bounds_line(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn extract_thing_whitespace(text: &str, idx: usize) -> Value {
    match bounds_whitespace(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn extract_thing_number(text: &str, idx: usize) -> Value {
    match bounds_number(text, idx) {
        Some((start, end)) => {
            let number = &text[start..end];
            if number.contains(['.', 'e', 'E']) {
                return number
                    .parse::<f64>()
                    .map(Value::Float)
                    .unwrap_or_else(|_| Value::string(number));
            }
            number
                .parse::<i64>()
                .map(Value::Int)
                .unwrap_or_else(|_| Value::string(number))
        }
        None => Value::Nil,
    }
}

fn extract_thing_url(text: &str, idx: usize) -> Value {
    match bounds_url(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn extract_thing_email(text: &str, idx: usize) -> Value {
    match bounds_email(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn extract_thing_filename(text: &str, idx: usize) -> Value {
    match bounds_filename(text, idx) {
        Some((start, end)) => Value::string(&text[start..end]),
        None => Value::Nil,
    }
}

fn bounds_word(text: &str, idx: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = text.chars().collect();
    if idx > chars.len() {
        return None;
    }

    let focus_idx = if idx < chars.len() && is_word_char(chars[idx]) {
        idx
    } else if idx > 0 && is_word_char(chars[idx - 1]) {
        idx - 1
    } else {
        return None;
    };

    let mut start = focus_idx;
    while start > 0 && is_word_char(chars[start - 1]) {
        start -= 1;
    }
    let mut end = focus_idx;
    while end < chars.len() && is_word_char(chars[end]) {
        end += 1;
    }

    // Convert char indices back to byte indices
    let byte_start: usize = chars[..start].iter().map(|c| c.len_utf8()).sum();
    let byte_end: usize = chars[..end].iter().map(|c| c.len_utf8()).sum();
    Some((byte_start, byte_end))
}

fn bounds_symbol(text: &str, idx: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = text.chars().collect();
    if idx > chars.len() {
        return None;
    }

    let focus_idx = if idx < chars.len() && is_symbol_char(chars[idx]) {
        idx
    } else if idx > 0 && is_symbol_char(chars[idx - 1]) {
        idx - 1
    } else {
        return None;
    };

    let mut start = focus_idx;
    while start > 0 && is_symbol_char(chars[start - 1]) {
        start -= 1;
    }
    let mut end = focus_idx;
    while end < chars.len() && is_symbol_char(chars[end]) {
        end += 1;
    }

    let byte_start: usize = chars[..start].iter().map(|c| c.len_utf8()).sum();
    let byte_end: usize = chars[..end].iter().map(|c| c.len_utf8()).sum();
    Some((byte_start, byte_end))
}

fn bounds_line(text: &str, idx: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = text.chars().collect();
    if idx >= chars.len() {
        return None;
    }

    let mut start = idx;
    while start > 0 && chars[start - 1] != '\n' {
        start -= 1;
    }
    let mut end = idx;
    while end < chars.len() && chars[end] != '\n' {
        end += 1;
    }
    // Include the newline if present
    if end < chars.len() {
        end += 1;
    }

    let byte_start: usize = chars[..start].iter().map(|c| c.len_utf8()).sum();
    let byte_end: usize = chars[..end].iter().map(|c| c.len_utf8()).sum();
    Some((byte_start, byte_end))
}

fn bounds_whitespace(text: &str, idx: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = text.chars().collect();
    if idx >= chars.len() || !chars[idx].is_whitespace() {
        return None;
    }

    let mut start = idx;
    while start > 0 && chars[start - 1].is_whitespace() {
        start -= 1;
    }
    let mut end = idx;
    while end < chars.len() && chars[end].is_whitespace() {
        end += 1;
    }

    let byte_start: usize = chars[..start].iter().map(|c| c.len_utf8()).sum();
    let byte_end: usize = chars[..end].iter().map(|c| c.len_utf8()).sum();
    Some((byte_start, byte_end))
}

fn bounds_number(text: &str, idx: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = text.chars().collect();
    if idx > chars.len() {
        return None;
    }

    let is_number_char = |c: char| c.is_ascii_digit() || c == '.';
    let has_adjacent_digit = |i: usize, chs: &[char]| {
        (i > 0 && chs[i - 1].is_ascii_digit()) || (i + 1 < chs.len() && chs[i + 1].is_ascii_digit())
    };

    let focus_idx = if idx < chars.len()
        && (chars[idx].is_ascii_digit() || (chars[idx] == '.' && has_adjacent_digit(idx, &chars)))
    {
        idx
    } else if idx > 0
        && (chars[idx - 1].is_ascii_digit()
            || (chars[idx - 1] == '.' && has_adjacent_digit(idx - 1, &chars)))
    {
        idx - 1
    } else {
        return None;
    };

    let mut start = focus_idx;
    while start > 0 && is_number_char(chars[start - 1]) {
        start -= 1;
    }
    let mut end = focus_idx;
    while end < chars.len() && is_number_char(chars[end]) {
        end += 1;
    }

    if !chars[start..end].iter().any(|c| c.is_ascii_digit()) {
        return None;
    }

    let byte_start: usize = chars[..start].iter().map(|c| c.len_utf8()).sum();
    let byte_end: usize = chars[..end].iter().map(|c| c.len_utf8()).sum();
    Some((byte_start, byte_end))
}

fn bounds_filename(text: &str, idx: usize) -> Option<(usize, usize)> {
    let chars: Vec<char> = text.chars().collect();
    if idx > chars.len() {
        return None;
    }

    let focus_idx = if idx < chars.len() && is_filename_char(chars[idx]) {
        idx
    } else if idx > 0 && is_filename_char(chars[idx - 1]) {
        idx - 1
    } else {
        return None;
    };

    let mut start = focus_idx;
    while start > 0 && is_filename_char(chars[start - 1]) {
        start -= 1;
    }
    let mut end = focus_idx;
    while end < chars.len() && is_filename_char(chars[end]) {
        end += 1;
    }

    let byte_start: usize = chars[..start].iter().map(|c| c.len_utf8()).sum();
    let byte_end: usize = chars[..end].iter().map(|c| c.len_utf8()).sum();
    Some((byte_start, byte_end))
}

fn char_index_to_byte_offset(text: &str, idx: usize) -> Option<usize> {
    if idx > text.chars().count() {
        return None;
    }
    if idx == text.chars().count() {
        return Some(text.len());
    }
    text.char_indices().nth(idx).map(|(offset, _)| offset)
}

fn trim_url_trailing_punctuation(text: &str, start: usize, mut end: usize) -> usize {
    while end > start {
        match text.as_bytes()[end - 1] {
            b',' | b'.' | b';' | b':' | b'!' | b'?' | b')' | b']' | b'}' | b'>' => end -= 1,
            _ => break,
        }
    }
    end
}

fn bounds_matching_regex(text: &str, idx: usize, pattern: &str) -> Option<(usize, usize)> {
    let byte_idx = char_index_to_byte_offset(text, idx)?;
    let re = regex::Regex::new(pattern).ok()?;

    for m in re.find_iter(text) {
        let start = m.start();
        let end = m.end();
        if start <= byte_idx && byte_idx <= end {
            return Some((start, end));
        }
    }

    None
}

fn bounds_url(text: &str, idx: usize) -> Option<(usize, usize)> {
    let byte_idx = char_index_to_byte_offset(text, idx)?;
    let re = regex::Regex::new(r#"(?i)\b(?:https?://|ftp://|www\.)[^\s<>"]+"#).ok()?;

    for m in re.find_iter(text) {
        let start = m.start();
        let raw_end = m.end();
        let canonical_end = trim_url_trailing_punctuation(text, start, raw_end);

        if start <= byte_idx && byte_idx <= canonical_end {
            return Some((start, canonical_end));
        }
        if canonical_end < byte_idx && byte_idx <= raw_end {
            return Some((start, raw_end));
        }
    }

    None
}

fn bounds_email(text: &str, idx: usize) -> Option<(usize, usize)> {
    bounds_matching_regex(text, idx, r#"(?i)\b[^\s<>"@]+@[^\s<>"]+"#)
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

    fn eval_one(src: &str) -> String {
        eval_all(src).into_iter().next().expect("at least one form")
    }

    fn eval_all_with(ev: &mut Evaluator, src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        ev.eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect()
    }

    // -------------------------------------------------------------------
    // InteractiveSpec
    // -------------------------------------------------------------------

    #[test]
    fn interactive_spec_no_args() {
        let spec = InteractiveSpec::no_args();
        assert!(spec.code.is_empty());
        assert!(spec.prompt.is_none());
    }

    #[test]
    fn interactive_spec_with_code() {
        let spec = InteractiveSpec::new("p");
        assert_eq!(spec.code, "p");
    }

    #[test]
    fn interactive_spec_with_prompt() {
        let spec = InteractiveSpec::new("sEnter name: ");
        assert_eq!(spec.code, "sEnter name: ");
        assert_eq!(spec.prompt.as_deref(), Some("Enter name: "));
    }

    // -------------------------------------------------------------------
    // InteractiveRegistry
    // -------------------------------------------------------------------

    #[test]
    fn registry_register_and_query() {
        let mut reg = InteractiveRegistry::new();
        reg.register_interactive("forward-char", InteractiveSpec::new("p"));
        assert!(reg.is_interactive("forward-char"));
        assert!(!reg.is_interactive("nonexistent"));
    }

    #[test]
    fn registry_get_spec() {
        let mut reg = InteractiveRegistry::new();
        reg.register_interactive("find-file", InteractiveSpec::new("FFind file: "));
        let spec = reg.get_spec("find-file").unwrap();
        assert_eq!(spec.code, "FFind file: ");
    }

    #[test]
    fn registry_interactive_call_stack() {
        let mut reg = InteractiveRegistry::new();
        assert!(!reg.is_called_interactively());

        reg.push_interactive_call(true);
        assert!(reg.is_called_interactively());

        reg.push_interactive_call(false);
        assert!(!reg.is_called_interactively());

        reg.pop_interactive_call();
        assert!(reg.is_called_interactively());

        reg.pop_interactive_call();
        assert!(!reg.is_called_interactively());
    }

    #[test]
    fn registry_this_command_keys() {
        let mut reg = InteractiveRegistry::new();
        assert!(reg.this_command_keys().is_empty());

        reg.set_this_command_keys(vec!["C-x".to_string(), "C-f".to_string()]);
        assert_eq!(reg.this_command_keys(), &["C-x", "C-f"]);
    }

    #[test]
    fn registry_default() {
        let reg = InteractiveRegistry::default();
        assert!(!reg.is_called_interactively());
    }

    // -------------------------------------------------------------------
    // define-minor-mode special form
    // -------------------------------------------------------------------

    #[test]
    fn define_minor_mode_creates_variable() {
        let results = eval_all(
            r#"(define-minor-mode test-mode "Test mode" :lighter " Test")
               test-mode"#,
        );
        assert_eq!(results[0], "OK test-mode");
        assert_eq!(results[1], "OK nil"); // initially off
    }

    #[test]
    fn define_minor_mode_creates_toggle_function() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(define-minor-mode my-mode "My mode" :lighter " My")
               (my-mode)
               my-mode"#,
        );
        assert_eq!(results[0], "OK my-mode");
        // After toggling, mode should be on (t)
        assert_eq!(results[2], "OK t");
    }

    #[test]
    fn define_minor_mode_toggle_off() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(define-minor-mode tog-mode "Toggle mode")
               (tog-mode)
               tog-mode
               (tog-mode)
               tog-mode"#,
        );
        // First toggle: on
        assert_eq!(results[2], "OK t");
        // Second toggle: off
        assert_eq!(results[4], "OK nil");
    }

    #[test]
    fn define_minor_mode_registers_interactive() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(define-minor-mode int-mode "Interactive mode")"#,
        );
        assert!(ev.interactive.is_interactive("int-mode"));
    }

    #[test]
    fn define_minor_mode_with_body() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(defvar body-ran nil)
               (define-minor-mode body-mode "Body mode"
                 (setq body-ran t))
               (body-mode)
               body-ran"#,
        );
        assert_eq!(results[3], "OK t");
    }

    #[test]
    fn define_minor_mode_global() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(define-minor-mode glob-mode "Global mode" :global t)"#,
        );
        assert_eq!(results[0], "OK glob-mode");
    }

    // -------------------------------------------------------------------
    // define-derived-mode special form
    // -------------------------------------------------------------------

    #[test]
    fn define_derived_mode_creates_mode() {
        let results = eval_all(
            r#"(define-derived-mode my-text-mode nil "MyText"
                 "A text mode.")"#,
        );
        assert_eq!(results[0], "OK my-text-mode");
    }

    #[test]
    fn define_derived_mode_with_parent() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(defvar parent-ran nil)
               (defun parent-mode ()
                 (setq parent-ran t))
               (define-derived-mode child-mode parent-mode "Child"
                 "A child mode.")"#,
        );
        assert_eq!(results[2], "OK child-mode");
    }

    #[test]
    fn define_derived_mode_sets_major_mode() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(define-derived-mode custom-mode nil "Custom")
               (custom-mode)
               major-mode"#,
        );
        assert_eq!(results[2], "OK custom-mode");
    }

    #[test]
    fn define_derived_mode_sets_mode_name() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(define-derived-mode fancy-mode nil "Fancy")
               (fancy-mode)
               mode-name"#,
        );
        assert_eq!(results[2], r#"OK "Fancy""#);
    }

    #[test]
    fn define_derived_mode_runs_body() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(defvar derived-body-ran nil)
               (define-derived-mode body-derived-mode nil "BodyDerived"
                 "Mode with body."
                 (setq derived-body-ran t))
               (body-derived-mode)
               derived-body-ran"#,
        );
        assert_eq!(results[3], "OK t");
    }

    #[test]
    fn define_derived_mode_registers_interactive() {
        let mut ev = Evaluator::new();
        eval_all_with(&mut ev, r#"(define-derived-mode ireg-mode nil "IReg")"#);
        assert!(ev.interactive.is_interactive("ireg-mode"));
    }

    // -------------------------------------------------------------------
    // define-generic-mode special form
    // -------------------------------------------------------------------

    #[test]
    fn define_generic_mode_creates_mode() {
        let results = eval_all(r#"(define-generic-mode my-generic-mode nil nil nil nil nil)"#);
        assert_eq!(results[0], "OK my-generic-mode");
    }

    // -------------------------------------------------------------------
    // commandp (interactive-aware version)
    // -------------------------------------------------------------------

    #[test]
    fn commandp_with_interactive_registration() {
        let mut ev = Evaluator::new();
        eval_all_with(&mut ev, r#"(define-minor-mode cmd-test-mode "Test")"#);
        // cmd-test-mode should now be a command
        let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol("cmd-test-mode")]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn commandp_non_interactive() {
        let mut ev = Evaluator::new();
        eval_all_with(&mut ev, r#"(defun my-plain-fn () 42)"#);
        let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol("my-plain-fn")]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn commandp_true_for_builtin_ignore() {
        let mut ev = Evaluator::new();
        let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol("ignore")]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn commandp_true_for_builtin_execute_extended_command() {
        let mut ev = Evaluator::new();
        let result =
            builtin_commandp_interactive(&mut ev, vec![Value::symbol("execute-extended-command")]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn commandp_true_for_builtin_forward_char() {
        let mut ev = Evaluator::new();
        let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol("forward-char")]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn commandp_true_for_builtin_editing_commands() {
        let mut ev = Evaluator::new();
        for name in [
            "backward-char",
            "delete-char",
            "insert-char",
            "yank",
            "yank-pop",
            "transpose-chars",
            "transpose-lines",
            "transpose-paragraphs",
            "transpose-sentences",
            "transpose-sexps",
            "upcase-word",
            "downcase-word",
            "capitalize-word",
            "upcase-region",
            "downcase-region",
            "capitalize-region",
            "upcase-initials-region",
            "kill-word",
            "backward-kill-word",
            "kill-region",
            "kill-ring-save",
            "kill-whole-line",
            "copy-region-as-kill",
            "open-line",
            "delete-horizontal-space",
            "just-one-space",
            "delete-indentation",
            "indent-for-tab-command",
            "transpose-words",
            "scroll-up",
            "scroll-down",
            "scroll-left",
            "scroll-right",
            "scroll-up-command",
            "scroll-down-command",
            "recenter",
            "recenter-top-bottom",
            "move-beginning-of-line",
            "move-end-of-line",
        ] {
            let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol(name)])
                .expect("commandp call");
            assert!(result.is_truthy(), "expected commandp true for {name}");
        }
    }

    #[test]
    fn commandp_true_for_additional_builtin_commands() {
        let mut ev = Evaluator::new();
        for name in [
            "abbrev-mode",
            "bookmark-jump",
            "base64-decode-region",
            "base64-encode-region",
            "base64url-encode-region",
            "decode-coding-region",
            "describe-function",
            "display-buffer",
            "encode-coding-region",
            "eval-buffer",
            "forward-sexp",
            "gui-set-selection",
            "goto-char",
            "indent-region",
            "isearch-forward",
            "iconify-frame",
            "kill-emacs",
            "list-processes",
            "lower-frame",
            "make-directory",
            "make-frame-invisible",
            "make-frame-visible",
            "make-indirect-buffer",
            "open-dribble-file",
            "query-replace",
            "raise-frame",
            "re-search-forward",
            "redirect-debugging-output",
            "rename-buffer",
            "replace-buffer-contents",
            "select-frame",
            "setenv",
            "set-buffer-process-coding-system",
            "transpose-regions",
            "kill-process",
            "signal-process",
            "process-menu-delete-process",
            "process-menu-mode",
            "suspend-emacs",
            "top-level",
            "unix-sync",
            "write-region",
            "x-clipboard-yank",
            "x-menu-bar-open-internal",
            "x-preedit-text",
            "yank-rectangle",
        ] {
            let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol(name)])
                .expect("commandp call");
            assert!(result.is_truthy(), "expected commandp true for {name}");
        }
    }

    #[test]
    fn commandp_false_for_noninteractive_builtin() {
        let mut ev = Evaluator::new();
        let result = builtin_commandp_interactive(&mut ev, vec![Value::symbol("car")]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn commandp_rejects_overflow_arity() {
        let mut ev = Evaluator::new();
        let result = builtin_commandp_interactive(
            &mut ev,
            vec![Value::symbol("ignore"), Value::Nil, Value::Nil],
        )
        .expect_err("commandp should reject more than two arguments");
        match result {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn commandp_resolves_aliases_and_symbol_designators() {
        let mut ev = Evaluator::new();
        ev.obarray.set_symbol_function("t", Value::symbol("ignore"));
        ev.obarray
            .set_symbol_function(":vm-command-alias-keyword", Value::symbol("ignore"));
        ev.obarray
            .set_symbol_function("vm-command-alias", Value::True);
        ev.obarray.set_symbol_function(
            "vm-command-alias-keyword",
            Value::keyword(":vm-command-alias-keyword"),
        );

        let t_result = builtin_commandp_interactive(&mut ev, vec![Value::True]);
        assert!(t_result.unwrap().is_truthy());
        let keyword_result = builtin_commandp_interactive(
            &mut ev,
            vec![Value::keyword(":vm-command-alias-keyword")],
        );
        assert!(keyword_result.unwrap().is_truthy());
        let alias_result =
            builtin_commandp_interactive(&mut ev, vec![Value::symbol("vm-command-alias")]);
        assert!(alias_result.unwrap().is_truthy());
        let keyword_alias_result =
            builtin_commandp_interactive(&mut ev, vec![Value::symbol("vm-command-alias-keyword")]);
        assert!(keyword_alias_result.unwrap().is_truthy());
    }

    #[test]
    fn commandp_true_for_lambda_with_interactive_form() {
        let mut ev = Evaluator::new();
        let lambda = eval_all_with(&mut ev, "(lambda () (interactive) 1)");
        let parsed = super::super::parser::parse_forms("(lambda () (interactive) 1)")
            .expect("lambda form should parse");
        let value = ev.eval(&parsed[0]).expect("lambda form should evaluate");
        assert_eq!(lambda[0], "OK (lambda nil (interactive) 1)");
        let result = builtin_commandp_interactive(&mut ev, vec![value]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn commandp_true_for_quoted_lambda_with_interactive_form() {
        let mut ev = Evaluator::new();
        let forms = super::super::parser::parse_forms("'(lambda () \"doc\" (interactive) 1)")
            .expect("quoted lambda form should parse");
        let quoted_lambda = ev.eval(&forms[0]).expect("quoted lambda should evaluate");
        let result = builtin_commandp_interactive(&mut ev, vec![quoted_lambda]);
        assert!(result.unwrap().is_truthy());
    }

    // -------------------------------------------------------------------
    // interactive-p / called-interactively-p
    // -------------------------------------------------------------------

    #[test]
    fn interactive_p_false_by_default() {
        let mut ev = Evaluator::new();
        let result = builtin_interactive_p(&mut ev, vec![]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn interactive_p_nil_when_interactive() {
        let mut ev = Evaluator::new();
        ev.interactive.push_interactive_call(true);
        let result = builtin_interactive_p(&mut ev, vec![]);
        ev.interactive.pop_interactive_call();
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn called_interactively_p_false_by_default() {
        let mut ev = Evaluator::new();
        let result = builtin_called_interactively_p(&mut ev, vec![]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn called_interactively_p_with_kind() {
        let mut ev = Evaluator::new();
        let result = builtin_called_interactively_p(&mut ev, vec![Value::symbol("any")]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn called_interactively_p_kind_interactive_is_nil_when_interactive() {
        let mut ev = Evaluator::new();
        ev.interactive.push_interactive_call(true);
        let result = builtin_called_interactively_p(&mut ev, vec![Value::symbol("interactive")]);
        ev.interactive.pop_interactive_call();
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn called_interactively_p_kind_any_is_t_when_interactive() {
        let mut ev = Evaluator::new();
        ev.interactive.push_interactive_call(true);
        let result = builtin_called_interactively_p(&mut ev, vec![Value::symbol("any")]);
        ev.interactive.pop_interactive_call();
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn called_interactively_p_unknown_kind_is_t_when_interactive() {
        let mut ev = Evaluator::new();
        ev.interactive.push_interactive_call(true);
        let result = builtin_called_interactively_p(&mut ev, vec![Value::symbol("foo")]);
        ev.interactive.pop_interactive_call();
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn called_interactively_p_too_many_args() {
        let mut ev = Evaluator::new();
        let result = builtin_called_interactively_p(
            &mut ev,
            vec![Value::symbol("any"), Value::symbol("extra")],
        );
        assert!(result.is_err());
    }

    // -------------------------------------------------------------------
    // this-command-keys / this-command-keys-vector
    // -------------------------------------------------------------------

    #[test]
    fn this_command_keys_empty() {
        let mut ev = Evaluator::new();
        let result = builtin_this_command_keys(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_str(), Some(""));
    }

    #[test]
    fn this_command_keys_after_set() {
        let mut ev = Evaluator::new();
        ev.interactive
            .set_this_command_keys(vec!["C-x".to_string(), "C-f".to_string()]);
        let result = builtin_this_command_keys(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_str(), Some("C-x C-f"));
    }

    #[test]
    fn this_command_keys_vector_empty() {
        let mut ev = Evaluator::new();
        let result = builtin_this_command_keys_vector(&mut ev, vec![]).unwrap();
        assert!(matches!(result, Value::Vector(_)));
    }

    #[test]
    fn this_command_keys_vector_after_set() {
        let mut ev = Evaluator::new();
        ev.interactive
            .set_this_command_keys(vec!["M-x".to_string()]);
        let result = builtin_this_command_keys_vector(&mut ev, vec![]).unwrap();
        if let Value::Vector(v) = result {
            let v = v.lock().unwrap();
            assert_eq!(v.len(), 1);
        } else {
            panic!("expected vector");
        }
    }

    #[test]
    fn this_command_keys_prefers_read_command_key_chars() {
        let mut ev = Evaluator::new();
        ev.interactive
            .set_this_command_keys(vec!["C-x".to_string(), "C-f".to_string()]);
        ev.set_read_command_keys(vec![Value::Int(97)]);

        let text = builtin_this_command_keys(&mut ev, vec![]).unwrap();
        assert_eq!(text.as_str(), Some("a"));

        let vec_result = builtin_this_command_keys_vector(&mut ev, vec![]).unwrap();
        match vec_result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.as_slice(), &[Value::Int(97)]);
            }
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn this_command_keys_returns_vector_for_non_char_read_command_keys() {
        let mut ev = Evaluator::new();
        ev.set_read_command_keys(vec![Value::list(vec![Value::symbol("mouse-1")])]);

        let result = builtin_this_command_keys(&mut ev, vec![]).unwrap();
        match result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert_eq!(items.len(), 1);
                assert!(matches!(items[0], Value::Cons(_)));
            }
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn clear_this_command_keys_clears_read_key_context() {
        let mut ev = Evaluator::new();
        ev.set_read_command_keys(vec![Value::Int(97)]);

        let result = builtin_clear_this_command_keys(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
        assert_eq!(ev.read_command_keys(), &[]);

        let vec_result = builtin_this_command_keys_vector(&mut ev, vec![]).unwrap();
        match vec_result {
            Value::Vector(v) => {
                let items = v.lock().expect("poisoned");
                assert!(items.is_empty());
            }
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn clear_this_command_keys_clears_interactive_fallback_context() {
        let mut ev = Evaluator::new();
        ev.interactive
            .set_this_command_keys(vec!["C-x".to_string(), "C-f".to_string()]);

        let result = builtin_clear_this_command_keys(&mut ev, vec![Value::Int(1)]).unwrap();
        assert!(result.is_nil());

        let keys = builtin_this_command_keys(&mut ev, vec![]).unwrap();
        assert_eq!(keys.as_str(), Some(""));
    }

    #[test]
    fn clear_this_command_keys_without_keep_record_clears_recent_input_history() {
        let mut ev = Evaluator::new();
        ev.record_input_event(Value::Int(97));
        assert_eq!(ev.recent_input_events(), &[Value::Int(97)]);

        let result = builtin_clear_this_command_keys(&mut ev, vec![]).unwrap();
        assert!(result.is_nil());
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn clear_this_command_keys_with_nil_keep_record_clears_recent_input_history() {
        let mut ev = Evaluator::new();
        ev.record_input_event(Value::Int(98));
        assert_eq!(ev.recent_input_events(), &[Value::Int(98)]);

        let result = builtin_clear_this_command_keys(&mut ev, vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn clear_this_command_keys_with_keep_record_preserves_recent_input_history() {
        let mut ev = Evaluator::new();
        ev.record_input_event(Value::Int(99));
        assert_eq!(ev.recent_input_events(), &[Value::Int(99)]);

        let result = builtin_clear_this_command_keys(&mut ev, vec![Value::symbol("t")]).unwrap();
        assert!(result.is_nil());
        assert_eq!(ev.recent_input_events(), &[Value::Int(99)]);
    }

    #[test]
    fn clear_this_command_keys_rejects_more_than_one_arg() {
        let mut ev = Evaluator::new();
        let result = builtin_clear_this_command_keys(&mut ev, vec![Value::Int(1), Value::Int(2)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-number-of-arguments"
                    && sig.data
                        == vec![Value::symbol("clear-this-command-keys"), Value::Int(2)]
        ));
    }

    // -------------------------------------------------------------------
    // key-binding / local-key-binding / global-key-binding
    // -------------------------------------------------------------------

    #[test]
    fn key_binding_global() {
        let mut ev = Evaluator::new();
        let map_id = ev.keymaps.make_keymap();
        ev.keymaps.set_global_map(map_id);
        let events = crate::elisp::kbd::key_events_from_designator(&Value::string("C-f"))
            .expect("key designator should decode");
        ev.keymaps.define_key(
            map_id,
            events[0].clone(),
            KeyBinding::Command("forward-char".to_string()),
        );

        let result = builtin_key_binding(&mut ev, vec![Value::string("C-f")]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("forward-char"));
    }

    #[test]
    fn key_binding_prefers_minor_and_emulation_mode_maps() {
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap))
                         (m (make-sparse-keymap))
                         (minor-mode-map-alist nil)
                         (demo-mode t))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key m (kbd "C-a") 'forward-char)
                     (define-key l (kbd "C-a") 'self-insert-command)
                     (setq minor-mode-map-alist (list (cons 'demo-mode m)))
                     (key-binding (kbd "C-a")))"#
            ),
            "OK forward-char"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (m-minor (make-sparse-keymap))
                         (m-emu (make-sparse-keymap))
                         (minor-mode-map-alist nil)
                         (emulation-mode-map-alists nil)
                         (minor-mode t)
                         (emu-mode t))
                     (use-global-map g)
                     (define-key m-minor (kbd "C-a") 'self-insert-command)
                     (define-key m-emu (kbd "C-a") 'forward-char)
                     (setq minor-mode-map-alist (list (cons 'minor-mode m-minor)))
                     (setq emulation-mode-map-alists (list (list (cons 'emu-mode m-emu))))
                     (key-binding (kbd "C-a")))"#
            ),
            "OK forward-char"
        );
    }

    #[test]
    fn key_binding_ignores_invalid_active_minor_emulation_entries() {
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (minor-mode-map-alist '((demo-mode . 999999)))
                         (demo-mode t))
                     (use-global-map g)
                     (define-key g (kbd "C-a") 'self-insert-command)
                     (key-binding (kbd "C-a")))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (emulation-mode-map-alists (list (list (cons 'demo-mode 999999))))
                         (demo-mode t))
                     (use-global-map g)
                     (define-key g (kbd "C-a") 'self-insert-command)
                     (key-binding (kbd "C-a")))"#
            ),
            "OK self-insert-command"
        );
    }

    #[test]
    fn key_binding_applies_command_remapping_unless_no_remap() {
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap)))
                     (use-global-map g)
                     (define-key g (kbd "a") 'self-insert-command)
                     (define-key g [remap self-insert-command] 'forward-char)
                     (list (key-binding (kbd "a"))
                           (key-binding (kbd "a") t nil)
                           (key-binding (kbd "a") t t)))"#
            ),
            "OK (forward-char forward-char self-insert-command)"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap)))
                     (use-global-map g)
                     (define-key g (kbd "a") 'self-insert-command)
                     (define-key g [remap self-insert-command] t)
                     (key-binding (kbd "a")))"#
            ),
            "OK self-insert-command"
        );
    }

    #[test]
    fn key_binding_unbound() {
        let mut ev = Evaluator::new();
        let result = builtin_key_binding(&mut ev, vec![Value::string("C-z")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn key_binding_empty_returns_keymap_list() {
        assert_eq!(
            eval_one(r#"(let ((m (key-binding ""))) (and (consp m) (keymapp (car m))))"#),
            "OK t"
        );
    }

    #[test]
    fn key_binding_empty_vector_is_nil() {
        assert_eq!(eval_one(r#"(key-binding [])"#), "OK nil");
    }

    #[test]
    fn key_binding_default_plain_char_self_insert() {
        let mut ev = Evaluator::new();
        let result = builtin_key_binding(&mut ev, vec![Value::string("a")]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("self-insert-command"));
    }

    #[test]
    fn key_binding_too_many_args_errors() {
        let mut ev = Evaluator::new();
        let result = builtin_key_binding(
            &mut ev,
            vec![
                Value::string("C-c"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn global_key_binding_returns_binding() {
        let mut ev = Evaluator::new();
        let map_id = ev.keymaps.make_keymap();
        ev.keymaps.set_global_map(map_id);
        let events = KeymapManager::parse_key_description("M-x").unwrap();
        ev.keymaps.define_key(
            map_id,
            events[0].clone(),
            KeyBinding::Command("execute-extended-command".to_string()),
        );

        let result = builtin_global_key_binding(&mut ev, vec![Value::string("M-x")]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("execute-extended-command"));
    }

    #[test]
    fn global_key_binding_default_plain_char_self_insert() {
        let mut ev = Evaluator::new();
        let result = builtin_global_key_binding(&mut ev, vec![Value::string("a")]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("self-insert-command"));
    }

    #[test]
    fn global_key_binding_default_multichar_string_returns_prefix_len() {
        let mut ev = Evaluator::new();
        let result = builtin_global_key_binding(&mut ev, vec![Value::string("ab")]).unwrap();
        assert_eq!(result, Value::Int(1));
    }

    #[test]
    fn global_key_binding_default_c_dash_z_string_returns_prefix_len() {
        let mut ev = Evaluator::new();
        let result = builtin_global_key_binding(&mut ev, vec![Value::string("C-z")]).unwrap();
        assert_eq!(result, Value::Int(1));
    }

    #[test]
    fn global_key_binding_empty_bootstraps_keymap() {
        assert_eq!(eval_one(r#"(keymapp (global-key-binding ""))"#), "OK t");
    }

    #[test]
    fn global_key_binding_too_many_args_errors() {
        let mut ev = Evaluator::new();
        let result =
            builtin_global_key_binding(&mut ev, vec![Value::string("C-c"), Value::Nil, Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn local_key_binding_nil_when_no_local_map() {
        let mut ev = Evaluator::new();
        let result = builtin_local_key_binding(&mut ev, vec![Value::string("C-c")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn local_key_binding_too_many_args_errors() {
        let mut ev = Evaluator::new();
        let result =
            builtin_local_key_binding(&mut ev, vec![Value::string("C-c"), Value::Nil, Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn minor_mode_key_binding_returns_nil_when_no_modes_are_active() {
        let mut ev = Evaluator::new();
        let result = builtin_minor_mode_key_binding(&mut ev, vec![Value::string("C-c")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn minor_mode_key_binding_returns_first_matching_mode_binding() {
        assert_eq!(
            eval_one(
                r#"(let* ((m1 (make-sparse-keymap))
                          (m2 (make-sparse-keymap)))
                     (define-key m1 (kbd "C-a") 'ignore)
                     (define-key m2 (kbd "C-a") 'forward-char)
                     (let ((minor-mode-map-alist (list (cons 'mode1 m1)
                                                       (cons 'mode2 m2)))
                           (mode1 t)
                           (mode2 t))
                       (minor-mode-key-binding (kbd "C-a"))))"#
            ),
            "OK ((mode1 . ignore))"
        );
    }

    #[test]
    fn minor_mode_key_binding_invalid_keymap_id_errors_for_active_mode() {
        assert_eq!(
            eval_one(
                r#"(let ((minor-mode-map-alist '((demo-mode . 999999)))
                         (demo-mode t))
                     (condition-case err
                         (minor-mode-key-binding (kbd "C-a"))
                       (error err)))"#
            ),
            "OK (wrong-type-argument keymapp 999999)"
        );
    }

    #[test]
    fn minor_mode_key_binding_prefers_emulation_mode_maps() {
        assert_eq!(
            eval_one(
                r#"(let* ((m-minor (make-sparse-keymap))
                          (m-emu (make-sparse-keymap)))
                     (define-key m-minor (kbd "C-a") 'ignore)
                     (define-key m-emu (kbd "C-a") 'forward-char)
                     (let ((emulation-mode-map-alists (list (list (cons 'emu-mode m-emu))))
                           (minor-mode-map-alist (list (cons 'minor-mode m-minor)))
                           (emu-mode t)
                           (minor-mode t))
                       (minor-mode-key-binding (kbd "C-a"))))"#
            ),
            "OK ((emu-mode . forward-char))"
        );
    }

    #[test]
    fn minor_mode_key_binding_prefers_overriding_mode_maps() {
        assert_eq!(
            eval_one(
                r#"(let* ((m-over (make-sparse-keymap))
                          (m-minor (make-sparse-keymap)))
                     (define-key m-over (kbd "C-a") 'forward-char)
                     (define-key m-minor (kbd "C-a") 'ignore)
                     (let ((minor-mode-overriding-map-alist (list (cons 'minor-mode m-over)))
                           (minor-mode-map-alist (list (cons 'minor-mode m-minor)))
                           (minor-mode t))
                       (minor-mode-key-binding (kbd "C-a"))))"#
            ),
            "OK ((minor-mode . forward-char))"
        );
    }

    #[test]
    fn minor_mode_key_binding_resolves_symbol_emulation_alists() {
        assert_eq!(
            eval_one(
                r#"(let* ((m (make-sparse-keymap)))
                     (define-key m (kbd "C-a") 'ignore)
                     (let ((emu-alist (list (cons 'emu-mode m)))
                           (emulation-mode-map-alists '(emu-alist))
                           (emu-mode t))
                       (minor-mode-key-binding (kbd "C-a"))))"#
            ),
            "OK ((emu-mode . ignore))"
        );
    }

    #[test]
    fn minor_mode_key_binding_invalid_emulation_keymap_id_errors() {
        assert_eq!(
            eval_one(
                r#"(let ((emulation-mode-map-alists (list (list (cons 'emu-mode 999999))))
                         (emu-mode t))
                     (condition-case err
                         (minor-mode-key-binding (kbd "C-a"))
                       (error err)))"#
            ),
            "OK (wrong-type-argument keymapp 999999)"
        );
    }

    #[test]
    fn minor_mode_key_binding_too_many_args_errors() {
        let mut ev = Evaluator::new();
        let result = builtin_minor_mode_key_binding(
            &mut ev,
            vec![Value::string("C-c"), Value::True, Value::symbol("extra")],
        );
        assert!(result.is_err());
    }

    // -------------------------------------------------------------------
    // substitute-command-keys
    // -------------------------------------------------------------------

    #[test]
    fn substitute_command_keys_plain() {
        let mut ev = Evaluator::new();
        let result =
            builtin_substitute_command_keys(&mut ev, vec![Value::string("Press C-x to save")])
                .unwrap();
        assert_eq!(result.as_str(), Some("Press C-x to save"));
    }

    #[test]
    fn substitute_command_keys_with_command() {
        let mut ev = Evaluator::new();
        let result = builtin_substitute_command_keys(
            &mut ev,
            vec![Value::string("Press \\[save-buffer] to save")],
        )
        .unwrap();
        // Should substitute with "M-x save-buffer" since no key is bound
        let s = result.as_str().unwrap();
        assert!(s.contains("save-buffer"));
    }

    #[test]
    fn substitute_command_keys_with_bound_key() {
        let mut ev = Evaluator::new();
        let map_id = ev.keymaps.make_keymap();
        ev.keymaps.set_global_map(map_id);
        let events = KeymapManager::parse_key_description("C-s").unwrap();
        ev.keymaps.define_key(
            map_id,
            events[0].clone(),
            KeyBinding::Command("save-buffer".to_string()),
        );

        let result = builtin_substitute_command_keys(
            &mut ev,
            vec![Value::string("Press \\[save-buffer] to save")],
        )
        .unwrap();
        let s = result.as_str().unwrap();
        assert!(s.contains("C-s"));
    }

    #[test]
    fn substitute_command_keys_not_string() {
        let mut ev = Evaluator::new();
        let result = builtin_substitute_command_keys(&mut ev, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn substitute_command_keys_accepts_optional_args() {
        let mut ev = Evaluator::new();
        let result = builtin_substitute_command_keys(
            &mut ev,
            vec![
                Value::string("x"),
                Value::symbol("extra"),
                Value::symbol("more"),
            ],
        )
        .unwrap();
        assert_eq!(result.as_str(), Some("x"));
    }

    // -------------------------------------------------------------------
    // describe-key-briefly
    // -------------------------------------------------------------------

    #[test]
    fn describe_key_briefly_unbound() {
        let mut ev = Evaluator::new();
        let result = builtin_describe_key_briefly(&mut ev, vec![Value::string("C-z")]).unwrap();
        let s = result.as_str().unwrap();
        assert!(s.contains("undefined"));
    }

    #[test]
    fn describe_key_briefly_bound() {
        let mut ev = Evaluator::new();
        let map_id = ev.keymaps.make_keymap();
        ev.keymaps.set_global_map(map_id);
        let events = crate::elisp::kbd::key_events_from_designator(&Value::string("C-f"))
            .expect("key designator should decode");
        ev.keymaps.define_key(
            map_id,
            events[0].clone(),
            KeyBinding::Command("forward-char".to_string()),
        );

        let result = builtin_describe_key_briefly(&mut ev, vec![Value::string("C-f")]).unwrap();
        let s = result.as_str().unwrap();
        assert!(s.contains("forward-char"));
    }

    #[test]
    fn describe_key_briefly_no_args_returns_empty_string() {
        let mut ev = Evaluator::new();
        let result = builtin_describe_key_briefly(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_str(), Some(""));
    }

    #[test]
    fn describe_key_briefly_insert_non_nil_returns_nil() {
        let mut ev = Evaluator::new();
        let result =
            builtin_describe_key_briefly(&mut ev, vec![Value::string("C-z"), Value::True]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn describe_key_briefly_non_sequence_errors() {
        let mut ev = Evaluator::new();
        let result = builtin_describe_key_briefly(&mut ev, vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn describe_key_briefly_empty_sequence_errors() {
        let mut ev = Evaluator::new();
        assert!(builtin_describe_key_briefly(&mut ev, vec![Value::string("")]).is_err());
        assert!(builtin_describe_key_briefly(&mut ev, vec![Value::vector(vec![])]).is_err());
    }

    #[test]
    fn describe_key_briefly_default_plain_char_reports_self_insert() {
        let mut ev = Evaluator::new();
        let result = builtin_describe_key_briefly(&mut ev, vec![Value::string("a")]).unwrap();
        let s = result.as_str().unwrap();
        assert!(s.contains("self-insert-command"));
    }

    // -------------------------------------------------------------------
    // thing-at-point
    // -------------------------------------------------------------------

    #[test]
    fn thing_at_point_word() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap")
               (set-buffer "tap")
               (insert "hello world")
               (goto-char 2)"#,
        );
        let result = builtin_thing_at_point(&mut ev, vec![Value::symbol("word")]).unwrap();
        assert_eq!(result.as_str(), Some("hello"));
    }

    #[test]
    fn thing_at_point_symbol() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap2")
               (set-buffer "tap2")
               (insert "foo-bar baz")
               (goto-char 3)"#,
        );
        let result = builtin_thing_at_point(&mut ev, vec![Value::symbol("symbol")]).unwrap();
        assert_eq!(result.as_str(), Some("foo-bar"));
    }

    #[test]
    fn thing_at_point_word_and_symbol_boundary() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap-bound")
               (set-buffer "tap-bound")
               (insert "alpha my-symbol")
               (goto-char (point-min))
               (search-forward "alpha")"#,
        );
        let word = builtin_thing_at_point(&mut ev, vec![Value::symbol("word")]).unwrap();
        assert_eq!(word.as_str(), Some("alpha"));

        eval_all_with(&mut ev, "(search-forward \"my-symbol\")");
        let symbol = builtin_thing_at_point(&mut ev, vec![Value::symbol("symbol")]).unwrap();
        assert_eq!(symbol.as_str(), Some("my-symbol"));
    }

    #[test]
    fn thing_at_point_line() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap3")
               (set-buffer "tap3")
               (insert "line1\nline2\n")
               (goto-char 1)"#,
        );
        let result = builtin_thing_at_point(&mut ev, vec![Value::symbol("line")]).unwrap();
        let s = result.as_str().unwrap();
        assert!(s.starts_with("line1"));
    }

    #[test]
    fn thing_at_point_url_and_email() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap4")
               (set-buffer "tap4")
               (insert "site https://example.com/path?a=1 and mail user@example.com")
               (goto-char 10)"#,
        );
        let url = builtin_thing_at_point(&mut ev, vec![Value::symbol("url")]).unwrap();
        assert_eq!(url.as_str(), Some("https://example.com/path?a=1"));

        eval_all_with(&mut ev, "(goto-char 46)");
        let email = builtin_thing_at_point(&mut ev, vec![Value::symbol("email")]).unwrap();
        assert_eq!(email.as_str(), Some("user@example.com"));
    }

    #[test]
    fn thing_at_point_number_returns_numeric_value() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap-num")
               (set-buffer "tap-num")
               (insert "v 12.34 56")
               (goto-char 5)"#,
        );
        let float_result = builtin_thing_at_point(&mut ev, vec![Value::symbol("number")]).unwrap();
        assert_eq!(float_result.as_float(), Some(12.34));

        eval_all_with(&mut ev, "(goto-char 10)");
        let int_result = builtin_thing_at_point(&mut ev, vec![Value::symbol("number")]).unwrap();
        assert_eq!(int_result.as_int(), Some(56));
    }

    #[test]
    fn thing_at_point_filename() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap5")
               (set-buffer "tap5")
               (insert "open ./dir/file.txt now")
               (goto-char 10)"#,
        );
        let result = builtin_thing_at_point(&mut ev, vec![Value::symbol("filename")]).unwrap();
        assert_eq!(result.as_str(), Some("./dir/file.txt"));
    }

    #[test]
    fn thing_at_point_no_buffer() {
        let mut ev = Evaluator::new();
        // No buffer set
        let result = builtin_thing_at_point(&mut ev, vec![Value::symbol("word")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn thing_at_point_not_symbol() {
        let mut ev = Evaluator::new();
        let result = builtin_thing_at_point(&mut ev, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    // -------------------------------------------------------------------
    // bounds-of-thing-at-point
    // -------------------------------------------------------------------

    #[test]
    fn bounds_of_thing_at_point_word() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "bnd")
               (set-buffer "bnd")
               (insert "hello world")
               (goto-char 3)"#,
        );
        let result =
            builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("word")]).unwrap();
        // Should be (1 . 6) for "hello"
        if let Value::Cons(cell) = &result {
            let cell = cell.lock().unwrap();
            assert_eq!(cell.car.as_int(), Some(1));
            assert_eq!(cell.cdr.as_int(), Some(6));
        } else {
            panic!("expected cons, got {:?}", result);
        }
    }

    #[test]
    fn bounds_of_thing_at_point_symbol_boundary() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "bnd-bound")
               (set-buffer "bnd-bound")
               (insert "my-symbol other")
               (goto-char (point-min))
               (search-forward "my-symbol")"#,
        );
        let result =
            builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("symbol")]).unwrap();
        if let Value::Cons(cell) = &result {
            let cell = cell.lock().expect("cons lock");
            assert_eq!(cell.car.as_int(), Some(1));
            assert_eq!(cell.cdr.as_int(), Some(10));
        } else {
            panic!("expected symbol bounds cons, got {result:?}");
        }
    }

    #[test]
    fn bounds_of_thing_at_point_sentence_and_sexp() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "bnd-sent-sexp")
               (set-buffer "bnd-sent-sexp")
               (insert "First sentence. Second sentence!")
               (goto-char (point-min))
               (search-forward "Second")"#,
        );
        let sentence =
            builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("sentence")]).unwrap();
        if let Value::Cons(cell) = &sentence {
            let cell = cell.lock().expect("cons lock");
            assert_eq!(cell.car.as_int(), Some(1));
            assert_eq!(cell.cdr.as_int(), Some(33));
        } else {
            panic!("expected sentence bounds cons, got {sentence:?}");
        }

        eval_all_with(
            &mut ev,
            r#"(erase-buffer)
               (insert "(foo bar) baz")
               (goto-char (point-min))
               (search-forward "foo")"#,
        );
        let sexp = builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("sexp")]).unwrap();
        if let Value::Cons(cell) = &sexp {
            let cell = cell.lock().expect("cons lock");
            assert_eq!(cell.car.as_int(), Some(2));
            assert_eq!(cell.cdr.as_int(), Some(5));
        } else {
            panic!("expected sexp bounds cons, got {sexp:?}");
        }
    }

    #[test]
    fn bounds_of_thing_at_point_nil() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "bnd2")
               (set-buffer "bnd2")
               (insert "   ")
               (goto-char 2)"#,
        );
        let result =
            builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("word")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn bounds_of_thing_at_point_url_and_email() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "bnd3")
               (set-buffer "bnd3")
               (insert "site https://example.com/path?a=1 and mail user@example.com.")
               (goto-char 10)"#,
        );

        let url = builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("url")]).unwrap();
        if let Value::Cons(cell) = &url {
            let cell = cell.lock().expect("cons lock");
            assert_eq!(cell.car.as_int(), Some(6));
            assert_eq!(cell.cdr.as_int(), Some(34));
        } else {
            panic!("expected url bounds cons, got {url:?}");
        }

        eval_all_with(&mut ev, "(goto-char 46)");
        let email =
            builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("email")]).unwrap();
        if let Value::Cons(cell) = &email {
            let cell = cell.lock().expect("cons lock");
            assert_eq!(cell.car.as_int(), Some(44));
            assert_eq!(cell.cdr.as_int(), Some(61));
        } else {
            panic!("expected email bounds cons, got {email:?}");
        }
    }

    #[test]
    fn bounds_of_thing_at_point_filename() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "bnd4")
               (set-buffer "bnd4")
               (insert "open ./dir/file.txt now")
               (goto-char 10)"#,
        );
        let result =
            builtin_bounds_of_thing_at_point(&mut ev, vec![Value::symbol("filename")]).unwrap();
        if let Value::Cons(cell) = &result {
            let cell = cell.lock().expect("cons lock");
            assert_eq!(cell.car.as_int(), Some(6));
            assert_eq!(cell.cdr.as_int(), Some(20));
        } else {
            panic!("expected filename bounds cons, got {result:?}");
        }
    }

    #[test]
    fn thing_at_point_filename_at_end_boundary() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap6")
               (set-buffer "tap6")
               (insert "open ./dir/file.txt now")
               (goto-char (point-max))"#,
        );
        let result = builtin_thing_at_point(&mut ev, vec![Value::symbol("filename")]).unwrap();
        assert_eq!(result.as_str(), Some("now"));
    }

    // -------------------------------------------------------------------
    // word-at-point / symbol-at-point
    // -------------------------------------------------------------------

    #[test]
    fn symbol_at_point_basic() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "sap")
               (set-buffer "sap")
               (insert "my-symbol other")
               (goto-char 3)"#,
        );
        let result = builtin_symbol_at_point(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("my-symbol"));
    }

    #[test]
    fn symbol_at_point_bootstraps_word_at_point_binding() {
        let mut ev = Evaluator::new();
        assert!(!ev.obarray.fboundp("word-at-point"));

        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "sap-bootstrap")
               (set-buffer "sap-bootstrap")
               (insert "my-symbol other")
               (goto-char 3)"#,
        );

        let _ = builtin_symbol_at_point(&mut ev, vec![]).unwrap();
        assert!(ev.obarray.fboundp("word-at-point"));
    }

    #[test]
    fn thing_at_point_bootstraps_word_at_point_binding() {
        let mut ev = Evaluator::new();
        assert!(!ev.obarray.fboundp("word-at-point"));

        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "tap-bootstrap")
               (set-buffer "tap-bootstrap")
               (insert "my-symbol other")
               (goto-char 3)"#,
        );

        let _ = builtin_thing_at_point(&mut ev, vec![Value::symbol("symbol")]).unwrap();
        assert!(ev.obarray.fboundp("word-at-point"));
    }

    #[test]
    fn symbol_at_point_respects_explicit_fmakunbound_word_at_point() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "sap-fmakunbound")
               (set-buffer "sap-fmakunbound")
               (insert "my-symbol other")
               (goto-char 3)"#,
        );

        let _ = builtin_symbol_at_point(&mut ev, vec![]).unwrap();
        assert!(ev.obarray.fboundp("word-at-point"));

        eval_all_with(&mut ev, "(fmakunbound 'word-at-point)");
        assert!(!ev.obarray.fboundp("word-at-point"));

        let result = builtin_symbol_at_point(&mut ev, vec![]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("my-symbol"));
        assert!(!ev.obarray.fboundp("word-at-point"));
    }

    #[test]
    fn word_at_point_basic() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(get-buffer-create "wap")
               (set-buffer "wap")
               (insert "alpha beta")
               (goto-char 3)"#,
        );
        let result = builtin_word_at_point(&mut ev, vec![]).unwrap();
        match result {
            Value::Str(s) => assert_eq!(&*s, "alpha"),
            other => panic!("expected string, got {other:?}"),
        }
    }

    #[test]
    fn word_at_point_arity() {
        let mut ev = Evaluator::new();
        assert!(builtin_word_at_point(&mut ev, vec![Value::Nil, Value::Nil]).is_err());
    }

    // -------------------------------------------------------------------
    // command-execute
    // -------------------------------------------------------------------

    #[test]
    fn command_execute_builtin_ignore() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("ignore")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn command_execute_rejects_non_vector_keys_argument() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(
            &mut ev,
            vec![Value::symbol("ignore"), Value::Nil, Value::string("a")],
        )
        .expect_err("command-execute should reject non-vector keys argument");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("vectorp"), Value::string("a")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_accepts_vector_keys_argument() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(
            &mut ev,
            vec![
                Value::symbol("ignore"),
                Value::Nil,
                Value::vector(vec![Value::Int(97)]),
            ],
        )
        .expect("command-execute should accept vector keys argument");
        assert!(result.is_nil());
    }

    #[test]
    fn command_execute_does_not_record_keys_argument_in_recent_history() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(
            &mut ev,
            vec![
                Value::symbol("ignore"),
                Value::Nil,
                Value::vector(vec![Value::Int(97), Value::symbol("mouse-1")]),
            ],
        )
        .expect("command-execute should accept vector keys argument");
        assert!(result.is_nil());
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn command_execute_keys_vector_keeps_this_command_keys_empty_in_batch() {
        let mut ev = Evaluator::new();
        let _ = eval_all_with(
            &mut ev,
            "(fset 'neo-rk-loop-probe
                    (lambda ()
                      (interactive)
                      (list (this-command-keys) (this-command-keys-vector))))",
        );

        let result = builtin_command_execute(
            &mut ev,
            vec![
                Value::symbol("neo-rk-loop-probe"),
                Value::Nil,
                Value::vector(vec![Value::Int(97), Value::Int(98)]),
            ],
        )
        .expect("command-execute should accept vector keys argument");
        let output = list_to_vec(&result).expect("probe result should be list");
        assert_eq!(output, vec![Value::string(""), Value::vector(vec![])]);
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn command_execute_rejects_list_keys_argument_without_recording_recent_history() {
        let mut ev = Evaluator::new();
        let keys = Value::list(vec![Value::Int(97), Value::Int(98)]);
        let result = builtin_command_execute(
            &mut ev,
            vec![Value::symbol("ignore"), Value::Nil, keys.clone()],
        )
        .expect_err("command-execute should reject list keys argument");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("vectorp"), keys]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn command_execute_rejects_too_many_arguments() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(
            &mut ev,
            vec![
                Value::symbol("ignore"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        )
        .expect_err("command-execute should reject too many arguments");
        match result {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_builtin_eval_expression_reads_stdin_in_batch() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("eval-expression")])
            .expect_err("command-execute eval-expression should signal end-of-file in batch");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "end-of-file");
                assert_eq!(sig.data, vec![Value::string("Error reading from stdin")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_builtin_self_insert_command_is_noop() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("self-insert-command")])
            .expect("self-insert-command should execute");
        assert!(result.is_nil());
    }

    #[test]
    fn command_execute_builtin_delete_char_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (command-execute 'delete-char)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"bc\"");
    }

    #[test]
    fn call_interactively_builtin_delete_char_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (call-interactively 'delete-char)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"bc\"");
    }

    #[test]
    fn call_interactively_rejects_non_vector_keys_argument() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(
            &mut ev,
            vec![Value::symbol("ignore"), Value::Nil, Value::string("b")],
        )
        .expect_err("call-interactively should reject non-vector keys argument");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("vectorp"), Value::string("b")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn call_interactively_accepts_vector_keys_argument() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(
            &mut ev,
            vec![
                Value::symbol("ignore"),
                Value::Nil,
                Value::vector(vec![Value::Int(98)]),
            ],
        )
        .expect("call-interactively should accept vector keys argument");
        assert!(result.is_nil());
    }

    #[test]
    fn call_interactively_does_not_record_keys_argument_in_recent_history() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(
            &mut ev,
            vec![
                Value::symbol("ignore"),
                Value::Nil,
                Value::vector(vec![Value::Int(98)]),
            ],
        )
        .expect("call-interactively should accept vector keys argument");
        assert!(result.is_nil());
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn call_interactively_keys_vector_keeps_this_command_keys_empty_in_batch() {
        let mut ev = Evaluator::new();
        let _ = eval_all_with(
            &mut ev,
            "(fset 'neo-rk-loop-probe
                    (lambda ()
                      (interactive)
                      (list (this-command-keys) (this-command-keys-vector))))",
        );

        let result = builtin_call_interactively(
            &mut ev,
            vec![
                Value::symbol("neo-rk-loop-probe"),
                Value::Nil,
                Value::vector(vec![Value::symbol("foo")]),
            ],
        )
        .expect("call-interactively should accept vector keys argument");
        let output = list_to_vec(&result).expect("probe result should be list");
        assert_eq!(output, vec![Value::string(""), Value::vector(vec![])]);
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn call_interactively_rejects_list_keys_argument_without_recording_recent_history() {
        let mut ev = Evaluator::new();
        let keys = Value::list(vec![Value::Int(97), Value::Int(98)]);
        let result = builtin_call_interactively(
            &mut ev,
            vec![Value::symbol("ignore"), Value::Nil, keys.clone()],
        )
        .expect_err("call-interactively should reject list keys argument");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("vectorp"), keys]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
        assert!(ev.recent_input_events().is_empty());
    }

    #[test]
    fn call_interactively_rejects_too_many_arguments() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(
            &mut ev,
            vec![Value::symbol("ignore"), Value::Nil, Value::Nil, Value::Nil],
        )
        .expect_err("call-interactively should reject too many arguments");
        match result {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_builtin_upcase_word_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc def")
                 (goto-char 1)
                 (command-execute 'upcase-word)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"ABC def\"");
    }

    #[test]
    fn call_interactively_builtin_capitalize_word_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc def")
                 (goto-char 1)
                 (call-interactively 'capitalize-word)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Abc def\"");
    }

    #[test]
    fn command_execute_builtin_transpose_words_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "aa bb")
                 (goto-char 1)
                 (command-execute 'transpose-words)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"bb aa\"");
    }

    #[test]
    fn call_interactively_builtin_transpose_words_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "aa bb")
                 (goto-char 1)
                 (call-interactively 'transpose-words)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"bb aa\"");
    }

    #[test]
    fn command_execute_builtin_transpose_sexps_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "(aa) (bb)")
                 (goto-char 5)
                 (command-execute 'transpose-sexps)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"(bb) (aa)\"");
    }

    #[test]
    fn call_interactively_builtin_transpose_sexps_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "(aa) (bb)")
                 (goto-char 5)
                 (call-interactively 'transpose-sexps)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"(bb) (aa)\"");
    }

    #[test]
    fn command_execute_builtin_transpose_sentences_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "One.  Two.")
                 (goto-char 1)
                 (command-execute 'transpose-sentences)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Two.  One.\"");
    }

    #[test]
    fn call_interactively_builtin_transpose_sentences_uses_default_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "One.  Two.")
                 (goto-char 1)
                 (call-interactively 'transpose-sentences)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Two.  One.\"");
    }

    #[test]
    fn command_execute_builtin_transpose_paragraphs_swaps_paragraphs() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "A\n\nB")
                 (goto-char 1)
                 (command-execute 'transpose-paragraphs)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"\\nBA\\n\"");
    }

    #[test]
    fn command_execute_builtin_kill_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (command-execute 'kill-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"c\"");
    }

    #[test]
    fn call_interactively_builtin_kill_ring_save_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (call-interactively 'kill-ring-save)
                 (current-kill 0 t))"#,
        );
        assert_eq!(results[0], "OK \"ab\"");
    }

    #[test]
    fn command_execute_builtin_copy_region_as_kill_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(let ((kill-ring nil))
                 (with-temp-buffer
                   (insert "abc")
                   (goto-char 1)
                   (set-mark 3)
                   (command-execute 'copy-region-as-kill)
                   (current-kill 0 t)))"#,
        );
        assert_eq!(results[0], "OK \"ab\"");
    }

    #[test]
    fn call_interactively_builtin_copy_region_as_kill_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(let ((kill-ring nil))
                 (with-temp-buffer
                   (insert "abc")
                   (goto-char 1)
                   (set-mark 3)
                   (call-interactively 'copy-region-as-kill)
                   (current-kill 0 t)))"#,
        );
        assert_eq!(results[0], "OK \"ab\"");
    }

    #[test]
    fn call_interactively_builtin_upcase_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (call-interactively 'upcase-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"ABc\"");
    }

    #[test]
    fn call_interactively_builtin_downcase_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "ABC")
                 (goto-char 1)
                 (set-mark 3)
                 (call-interactively 'downcase-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"abC\"");
    }

    #[test]
    fn call_interactively_builtin_capitalize_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (call-interactively 'capitalize-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Abc\"");
    }

    #[test]
    fn command_execute_builtin_upcase_region_signals_args_out_of_range() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (condition-case err
                     (command-execute 'upcase-region)
                   (error err)))"#,
        );
        assert_eq!(results[0], "OK (args-out-of-range \"\" 0)");
    }

    #[test]
    fn command_execute_builtin_downcase_region_signals_args_out_of_range() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "ABC")
                 (goto-char 1)
                 (set-mark 3)
                 (condition-case err
                     (command-execute 'downcase-region)
                   (error err)))"#,
        );
        assert_eq!(results[0], "OK (args-out-of-range \"\" 0)");
    }

    #[test]
    fn command_execute_builtin_capitalize_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (command-execute 'capitalize-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Abc\"");
    }

    #[test]
    fn command_execute_builtin_capitalize_region_without_mark_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (command-execute 'capitalize-region)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn call_interactively_builtin_upcase_initials_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (call-interactively 'upcase-initials-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Abc\"");
    }

    #[test]
    fn command_execute_builtin_upcase_initials_region_uses_marked_region() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (set-mark 3)
                 (command-execute 'upcase-initials-region)
                 (buffer-string))"#,
        );
        assert_eq!(results[0], "OK \"Abc\"");
    }

    #[test]
    fn command_execute_builtin_upcase_initials_region_without_mark_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (command-execute 'upcase-initials-region)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn command_execute_builtin_kill_region_without_mark_signals_user_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (command-execute 'kill-region)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (user-error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn command_execute_builtin_kill_ring_save_without_mark_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (command-execute 'kill-ring-save)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn call_interactively_builtin_kill_ring_save_without_mark_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (call-interactively 'kill-ring-save)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn command_execute_builtin_copy_region_as_kill_without_mark_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (command-execute 'copy-region-as-kill)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn call_interactively_builtin_copy_region_as_kill_without_mark_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (insert "abc")
                 (goto-char 1)
                 (condition-case err
                     (call-interactively 'copy-region-as-kill)
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK (error \"The mark is not set now, so there is no region\")"
        );
    }

    #[test]
    fn command_execute_builtin_find_file_reads_stdin_in_batch() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("find-file")])
            .expect_err("command-execute find-file should signal end-of-file in batch");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "end-of-file");
                assert_eq!(sig.data, vec![Value::string("Error reading from stdin")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_builtin_save_buffer_reads_stdin_in_batch() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("save-buffer")])
            .expect_err("command-execute save-buffer should signal end-of-file in batch");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "end-of-file");
                assert_eq!(sig.data, vec![Value::string("Error reading from stdin")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_builtin_set_mark_command_returns_nil() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("set-mark-command")])
            .expect("set-mark-command should execute");
        assert!(result.is_nil());
    }

    #[test]
    fn command_execute_builtin_quoted_insert_reads_stdin_in_batch() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("quoted-insert")])
            .expect_err("command-execute quoted-insert should signal end-of-file in batch");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "end-of-file");
                assert_eq!(sig.data, vec![Value::string("Error reading from stdin")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn command_execute_builtin_universal_argument_returns_noninteractive_function() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("universal-argument")])
            .expect("universal-argument should execute");
        assert!(matches!(result, Value::Lambda(_)));
        let as_command =
            builtin_commandp_interactive(&mut ev, vec![result]).expect("commandp call");
        assert!(as_command.is_nil());
    }

    #[test]
    fn command_execute_calls_function() {
        let mut ev = Evaluator::new();
        eval_all_with(
            &mut ev,
            r#"(defvar exec-ran nil)
               (defun test-cmd () (setq exec-ran t))"#,
        );
        ev.interactive
            .register_interactive("test-cmd", InteractiveSpec::no_args());

        let result = builtin_command_execute(&mut ev, vec![Value::symbol("test-cmd")]).unwrap();
        assert!(result.is_truthy());

        let ran = ev.obarray.symbol_value("exec-ran").unwrap().clone();
        assert!(ran.is_truthy());
    }

    #[test]
    fn command_execute_non_command_signals_commandp_error() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("car")])
            .expect_err("command-execute should reject non-command symbols");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("commandp"), Value::symbol("car")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn call_interactively_builtin_ignore() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(&mut ev, vec![Value::symbol("ignore")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn call_interactively_lambda_interactive_p_uses_current_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(let ((f (lambda (n) (interactive "p") n))
                     (current-prefix-arg '(4)))
                 (call-interactively f))"#,
        );
        assert_eq!(results[0], "OK 4");
    }

    #[test]
    fn command_execute_lambda_interactive_p_uses_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((f (lambda (n) (interactive "p") n))
                       (current-prefix-arg '(4)))
                   (command-execute f))
                 (let ((f (lambda (n) (interactive "p") n))
                       (current-prefix-arg '(4))
                       (prefix-arg '(5)))
                   (command-execute f)))"#,
        );
        assert_eq!(results[0], "OK (1 5)");
    }

    #[test]
    fn call_interactively_lambda_interactive_p_prefers_current_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(let ((f (lambda (n) (interactive "p") n))
                     (current-prefix-arg '(4))
                     (prefix-arg '(5)))
                 (call-interactively f))"#,
        );
        assert_eq!(results[0], "OK 4");
    }

    #[test]
    fn interactive_lambda_forms_support_p_p_and_expression_specs() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((f (lambda (arg) (interactive "P") arg))
                       (current-prefix-arg '(4)))
                   (call-interactively f))
                 (let ((f (lambda (arg) (interactive "P") arg))
                       (prefix-arg '(5)))
                   (command-execute f))
                 (call-interactively (lambda (x) (interactive (list 7)) x))
                 (command-execute (lambda (x) (interactive (list 8)) x))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive 7) x))
                   (error err)))"#,
        );
        assert_eq!(results[0], "OK ((4) (5) 7 8 (wrong-type-argument listp 7))");
    }

    #[test]
    fn call_interactively_accepts_quoted_lambda_commands() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(let ((current-prefix-arg 3))
                 (call-interactively '(lambda (n) (interactive "p") n)))"#,
        );
        assert_eq!(results[0], "OK 3");
    }

    #[test]
    fn interactive_lambda_r_spec_reads_region_for_call_and_command_execute() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (with-temp-buffer
                   (insert "abc")
                   (goto-char 2)
                   (set-mark 3)
                   (call-interactively (lambda (b e) (interactive "r") (list b e))))
                 (with-temp-buffer
                   (insert "abc")
                   (goto-char 2)
                   (set-mark 3)
                   (command-execute (lambda (b e) (interactive "r") (list b e))))
                 (with-temp-buffer
                   (insert "abc")
                   (goto-char 2)
                   (condition-case err
                       (call-interactively (lambda (b e) (interactive "r") (list b e)))
                     (error err))))"#,
        );
        assert_eq!(
            results[0],
            "OK ((2 3) (2 3) (error \"The mark is not set now, so there is no region\"))"
        );
    }

    #[test]
    fn interactive_lambda_s_spec_reads_prompt_and_signals_eof_in_batch() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (condition-case err
                     (call-interactively (lambda (s) (interactive "sPrompt: ") s))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (s) (interactive "sPrompt: ") s))
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK ((end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\"))"
        );
    }

    #[test]
    fn interactive_lambda_extended_string_codes_cover_point_mark_ignored_and_key_readers() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((unread-command-events (list 97 98 99)))
                   (with-temp-buffer
                     (insert "abcd")
                     (goto-char 3)
                     (set-mark 2)
                     (call-interactively
                      (lambda (pt mk ignored ch keys keyvec)
                        (interactive "d
m
i
c
k
K")
                        (list pt mk ignored ch keys keyvec)))))
                 (let ((unread-command-events (list 97 98 99)))
                   (with-temp-buffer
                     (insert "abcd")
                     (goto-char 3)
                     (set-mark 2)
                     (command-execute
                      (lambda (pt mk ignored ch keys keyvec)
                        (interactive "d
m
i
c
k
K")
                        (list pt mk ignored ch keys keyvec)))))
                 (with-temp-buffer
                   (insert "abc")
                   (goto-char 2)
                   (condition-case err
                       (call-interactively (lambda (mk) (interactive "m") mk))
                     (error err))))"#,
        );
        assert_eq!(
            results[0],
            "OK ((3 2 nil 97 \"b\" [99]) (3 2 nil 97 \"b\" [99]) (error \"The mark is not set now\"))"
        );
    }

    #[test]
    fn interactive_lambda_extended_reader_prompt_codes_signal_eof_in_batch() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "aFunction: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "bBuffer: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "BBuffer: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "CCommand: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "DDirectory: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "fFind file: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "FFind file: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "vVariable: ") x))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (x) (interactive "bBuffer: ") x))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (x) (interactive "fFind file: ") x))
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK ((end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\"))"
        );
    }

    #[test]
    fn interactive_lambda_n_and_optional_coding_specs_follow_prefix_and_batch_behavior() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((current-prefix-arg '(4))
                       (prefix-arg nil))
                   (call-interactively (lambda (n) (interactive "NNumber: ") n)))
                 (let ((current-prefix-arg nil)
                       (prefix-arg '(5)))
                   (command-execute (lambda (n) (interactive "NNumber: ") n)))
                 (let ((current-prefix-arg nil)
                       (prefix-arg nil))
                   (condition-case err
                       (call-interactively (lambda (n) (interactive "NNumber: ") n))
                     (error err)))
                 (let ((current-prefix-arg nil)
                       (prefix-arg nil))
                   (condition-case err
                       (command-execute (lambda (n) (interactive "NNumber: ") n))
                     (error err)))
                 (let ((unread-command-events (list 97)))
                   (list
                    (call-interactively (lambda (c) (interactive "ZCoding: ") c))
                    unread-command-events))
                 (let ((unread-command-events (list 97)))
                   (list
                    (command-execute (lambda (c) (interactive "ZCoding: ") c))
                    unread-command-events)))"#,
        );
        assert_eq!(
            results[0],
            "OK (4 5 (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (nil (97)) (nil (97)))"
        );
    }

    #[test]
    fn interactive_lambda_m_s_x_x_and_z_specs_signal_eof_in_batch() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (condition-case err
                     (call-interactively (lambda (s) (interactive "MString: ") s))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (s) (interactive "SSymbol: ") s))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "xExpr: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "XExpr: ") x))
                   (error err))
                 (condition-case err
                     (call-interactively (lambda (c) (interactive "zCoding: ") c))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (s) (interactive "MString: ") s))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (s) (interactive "SSymbol: ") s))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (x) (interactive "xExpr: ") x))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (x) (interactive "XExpr: ") x))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (c) (interactive "zCoding: ") c))
                   (error err)))"#,
        );
        assert_eq!(
            results[0],
            "OK ((end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\"))"
        );
    }

    #[test]
    fn interactive_lambda_g_e_and_u_specs_follow_batch_behavior() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "GFind file: ") x))
                   (error err))
                 (condition-case err
                     (command-execute (lambda (x) (interactive "GFind file: ") x))
                   (error err))
                 (let ((unread-command-events (list 97)))
                   (list
                    (call-interactively (lambda (x) (interactive "U") x))
                    unread-command-events))
                 (let ((unread-command-events (list 97)))
                   (list
                    (command-execute (lambda (x) (interactive "U") x))
                    unread-command-events))
                 (let ((unread-command-events (list 97)))
                   (condition-case err
                       (call-interactively (lambda (x) (interactive "e") x))
                     (error (list err unread-command-events))))
                 (let ((unread-command-events (list 97)))
                   (condition-case err
                       (command-execute (lambda (x) (interactive "e") x))
                     (error (list err unread-command-events)))))"#,
        );
        assert_eq!(
            results[0],
            "OK ((end-of-file \"Error reading from stdin\") (end-of-file \"Error reading from stdin\") (nil (97)) (nil (97)) ((error \"command must be bound to an event with parameters\") (97)) ((error \"command must be bound to an event with parameters\") (97)))"
        );
    }

    #[test]
    fn interactive_lambda_invalid_control_letter_signals_error() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((r (condition-case err
                              (call-interactively (lambda (x) (interactive "q") x))
                            (error err))))
                   (list (if (consp r) (car r) 'non-error)
                         (and (consp r)
                              (stringp (cadr r))
                              (string-prefix-p "Invalid control letter" (cadr r)))))
                 (let ((r (condition-case err
                              (command-execute (lambda (x) (interactive "q") x))
                            (error err))))
                   (list (if (consp r) (car r) 'non-error)
                         (and (consp r)
                              (stringp (cadr r))
                              (string-prefix-p "Invalid control letter" (cadr r)))))
                 (let ((called nil)
                       (r nil))
                   (setq r (condition-case err
                               (call-interactively (lambda () (interactive "q") (setq called t)))
                             (error err)))
                   (list (if (consp r) (car r) 'non-error)
                         (and (consp r)
                              (stringp (cadr r))
                              (string-prefix-p "Invalid control letter" (cadr r)))
                         called))
                 (let ((r (condition-case err
                              (call-interactively (lambda (x) (interactive "*q") x))
                            (error err))))
                   (list (if (consp r) (car r) 'non-error)
                         (and (consp r)
                              (stringp (cadr r))
                              (string-prefix-p "Invalid control letter" (cadr r))))))"#,
        );
        assert_eq!(
            results[0],
            "OK ((error t) (error t) (error t nil) (error t))"
        );
    }

    #[test]
    fn interactive_lambda_prefix_flags_star_hat_and_at_follow_batch_semantics() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (with-temp-buffer
                   (let ((buffer-read-only nil))
                     (call-interactively (lambda () (interactive "*") 'ok))))
                 (with-temp-buffer
                   (let ((buffer-read-only t))
                     (condition-case err
                         (call-interactively (lambda () (interactive "*") 'ok))
                       (error (car err)))))
                 (with-temp-buffer
                   (let ((buffer-read-only t)
                         (inhibit-read-only t))
                     (call-interactively (lambda () (interactive "*") 'ok))))
                 (with-temp-buffer
                   (insert "abcd")
                   (goto-char 3)
                   (setq mark-active nil)
                   (let ((this-command-keys-shift-translated t)
                         (shift-select-mode t))
                     (list
                      (call-interactively (lambda (pt) (interactive "^d") pt))
                      mark-active
                      (mark t))))
                 (with-temp-buffer
                   (insert "abcd")
                   (goto-char 3)
                   (setq mark-active nil)
                   (let ((this-command-keys-shift-translated t)
                         (shift-select-mode t))
                     (list
                      (command-execute (lambda (pt) (interactive "^d") pt))
                      mark-active
                      (mark t))))
                 (with-temp-buffer
                   (insert "abcd")
                   (goto-char 3)
                   (set-mark 1)
                   (setq mark-active nil)
                   (let ((this-command-keys-shift-translated nil)
                         (shift-select-mode t))
                     (list
                      (call-interactively (lambda (pt) (interactive "^d") pt))
                      mark-active
                      (mark t))))
                 (with-temp-buffer
                   (insert "ab")
                   (goto-char 2)
                   (call-interactively (lambda (pt) (interactive "@d") pt)))
                 (with-temp-buffer
                   (insert "ab")
                   (goto-char 2)
                   (command-execute (lambda (pt) (interactive "@d") pt)))
                 (with-temp-buffer
                   (insert "abcd")
                   (goto-char 3)
                   (setq buffer-read-only t)
                   (setq mark-active nil)
                   (let ((this-command-keys-shift-translated t)
                         (shift-select-mode t))
                     (condition-case err
                         (progn
                           (call-interactively (lambda (x) (interactive "*^d") x))
                           (list 'ok mark-active (mark t)))
                       (error (list (car err) mark-active (mark t))))))
                 (with-temp-buffer
                   (insert "abcd")
                   (goto-char 3)
                   (setq buffer-read-only t)
                   (setq mark-active nil)
                   (let ((this-command-keys-shift-translated t)
                         (shift-select-mode t))
                     (condition-case err
                         (progn
                           (call-interactively (lambda (x) (interactive "^*d") x))
                           (list 'ok mark-active (mark t)))
                       (error (list (car err) mark-active (mark t)))))))"#,
        );
        assert_eq!(
            results[0],
            "OK (ok buffer-read-only ok (3 t 3) (3 t 3) (3 nil 1) 2 2 (buffer-read-only nil nil) (buffer-read-only t 3))"
        );
    }

    #[test]
    fn interactive_lambda_e_spec_reads_parameterized_events_from_keys_vector() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((evt (list 'mouse-1 (list (list (selected-window) (point) '(0 . 0) 0))))
                       (r nil))
                   (setq r (call-interactively (lambda (x) (interactive "e") x) nil (vector evt)))
                   (and (consp r) (eq (car r) 'mouse-1)))
                 (let ((evt (list 'mouse-1 (list (list (selected-window) (point) '(0 . 0) 0))))
                       (r nil))
                   (setq r (command-execute (lambda (x) (interactive "e") x) nil (vector evt)))
                   (and (consp r) (eq (car r) 'mouse-1)))
                 (let ((evt (list 'mouse-1 (list (list (selected-window) (point) '(0 . 0) 0))))
                       (r nil))
                   (setq r (call-interactively (lambda (x) (interactive "e") x) nil (vector 97 evt)))
                   (and (consp r) (eq (car r) 'mouse-1)))
                 (equal
                  (call-interactively (lambda (x) (interactive "e") x) nil (vector '(mouse-1)))
                  '(mouse-1))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "e") x) nil [mouse-1])
                   (error (car err)))
                 (condition-case err
                     (command-execute (lambda (x) (interactive "e") x) nil [mouse-1])
                   (error (car err)))
                 (condition-case err
                     (call-interactively (lambda (x) (interactive "e") x) nil (vector [mouse-1]))
                   (error (car err))))"#,
        );
        assert_eq!(results[0], "OK (t t t t error error error)");
    }

    #[test]
    fn interactive_lambda_e_spec_uses_command_key_context_for_event_dispatch() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((unread-command-events (list '(mouse-1))))
                   (list
                    (read-event)
                    (call-interactively (lambda (x) (interactive "e") x))))
                 (let ((unread-command-events (list '(mouse-1))))
                   (list
                    (read-event)
                    (command-execute (lambda (x) (interactive "e") x))))
                 (let ((unread-command-events (list 97 '(mouse-1))))
                   (list
                    (read-event)
                    (call-interactively (lambda (x) (interactive "e") x))
                    unread-command-events))
                 (let ((unread-command-events (list 97 '(mouse-1))))
                   (list
                    (read-event)
                    (command-execute (lambda (x) (interactive "e") x))
                    unread-command-events)))"#,
        );
        assert_eq!(
            results[0],
            "OK (((mouse-1) (mouse-1)) ((mouse-1) (mouse-1)) (97 (mouse-1) ((mouse-1))) (97 (mouse-1) ((mouse-1))))"
        );
    }

    #[test]
    fn interactive_lambda_e_spec_does_not_use_unread_queue_without_command_key_context() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((start (length (recent-keys))))
                   (let ((unread-command-events (list '(mouse-1))))
                     (condition-case err
                         (call-interactively (lambda (x) (interactive "e") x))
                       (error (list (car err)
                                    unread-command-events
                                    (append (nthcdr start (append (recent-keys) nil)) nil))))))
                 (let ((start (length (recent-keys))))
                   (let ((unread-command-events (list '(mouse-1))))
                     (condition-case err
                         (command-execute (lambda (x) (interactive "e") x))
                       (error (list (car err)
                                    unread-command-events
                                    (append (nthcdr start (append (recent-keys) nil)) nil))))))
                 (let ((start (length (recent-keys))))
                   (let ((unread-command-events (list 97 '(mouse-1))))
                     (condition-case err
                         (call-interactively (lambda (x) (interactive "e") x))
                       (error (list (car err)
                                    unread-command-events
                                    (append (nthcdr start (append (recent-keys) nil)) nil))))))
                 (let ((start (length (recent-keys))))
                   (let ((unread-command-events (list 97 '(mouse-1))))
                     (condition-case err
                         (command-execute (lambda (x) (interactive "e") x))
                       (error (list (car err)
                                    unread-command-events
                                    (append (nthcdr start (append (recent-keys) nil)) nil)))))))"#,
        );
        assert_eq!(
            results[0],
            "OK ((error ((mouse-1)) nil) (error ((mouse-1)) nil) (error (97 (mouse-1)) nil) (error (97 (mouse-1)) nil))"
        );
    }

    #[test]
    fn interactive_lambda_e_spec_prefers_existing_command_keys_context() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(list
                 (let ((unread-command-events (list 97)))
                   (read-key-sequence ""))
                 (this-command-keys-vector)
                 (let ((unread-command-events (list '(mouse-1))))
                   (condition-case err
                       (call-interactively (lambda (x) (interactive "e") x))
                     (error (car err))))
                 (let ((unread-command-events (list '(mouse-1))))
                   (condition-case err
                       (call-interactively (lambda (x) (interactive "e") x) nil [])
                     (error (car err))))
                 (call-interactively
                  (lambda (x) (interactive "e") x)
                  nil
                  (vector '(mouse-1))))"#,
        );
        assert_eq!(results[0], "OK (\"a\" [97] error error (mouse-1))");
    }

    #[test]
    fn call_interactively_non_command_signals_commandp_error() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(&mut ev, vec![Value::symbol("car")])
            .expect_err("call-interactively should reject non-command symbols");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("commandp"), Value::symbol("car")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn call_interactively_eval_expression_reads_stdin_in_batch() {
        let mut ev = Evaluator::new();
        let result = builtin_call_interactively(&mut ev, vec![Value::symbol("eval-expression")])
            .expect_err("call-interactively eval-expression should signal end-of-file in batch");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "end-of-file");
                assert_eq!(sig.data, vec![Value::string("Error reading from stdin")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn eval_expression_rejects_too_many_args() {
        let mut ev = Evaluator::new();
        let result = builtin_eval_expression(
            &mut ev,
            vec![
                Value::Int(1),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        )
        .expect_err("eval-expression should reject more than four args");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("eval-expression"), Value::Int(5)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn self_insert_command_argument_validation() {
        let mut ev = Evaluator::new();

        let missing = builtin_self_insert_command(&mut ev, vec![])
            .expect_err("self-insert-command should require one arg");
        match missing {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("self-insert-command"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let too_many =
            builtin_self_insert_command(&mut ev, vec![Value::Int(1), Value::Nil, Value::Nil])
                .expect_err("self-insert-command should reject too many args");
        match too_many {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("self-insert-command"), Value::Int(3)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let wrong_type = builtin_self_insert_command(&mut ev, vec![Value::symbol("x")])
            .expect_err("self-insert-command should type check arg");
        match wrong_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("fixnump"), Value::symbol("x")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let negative = builtin_self_insert_command(&mut ev, vec![Value::Int(-1)])
            .expect_err("self-insert-command should reject negative repetition");
        match negative {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Negative repetition argument -1")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn self_insert_command_uses_last_command_event_character() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (let ((last-command-event 97))
                   (self-insert-command 2)
                   (buffer-string)))"#,
        );
        assert_eq!(results[0], "OK \"aa\"");
    }

    #[test]
    fn self_insert_command_non_nil_second_arg_is_noop() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (let ((last-command-event 97))
                   (self-insert-command 2 t)
                   (buffer-string)))"#,
        );
        assert_eq!(results[0], "OK \"\"");
    }

    #[test]
    fn command_execute_self_insert_uses_last_command_event_when_available() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(with-temp-buffer
                 (let ((last-command-event 98))
                   (command-execute 'self-insert-command nil [98])
                   (buffer-string)))"#,
        );
        assert_eq!(results[0], "OK \"b\"");
    }

    #[test]
    fn keyboard_quit_signals_quit() {
        let mut ev = Evaluator::new();
        let result = builtin_command_execute(&mut ev, vec![Value::symbol("keyboard-quit")])
            .expect_err("keyboard-quit should signal quit");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "quit");
                assert!(sig.data.is_empty());
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    // -------------------------------------------------------------------
    // execute-extended-command
    // -------------------------------------------------------------------

    #[test]
    fn execute_extended_command_with_command_name() {
        let mut ev = Evaluator::new();
        let result =
            builtin_execute_extended_command(&mut ev, vec![Value::Nil, Value::string("ignore")])
                .expect("execute-extended-command should run command names");
        assert!(result.is_nil());
    }

    #[test]
    fn execute_extended_command_returns_nil_and_seeds_current_prefix_arg() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(progn
                 (setq neo-eec-seen-vars :unset)
                 (defun neo-eec-vars ()
                   (interactive)
                   (setq neo-eec-seen-vars (list current-prefix-arg prefix-arg))
                   'neo-eec-vars-ret)
                 (list
                  (execute-extended-command nil "neo-eec-vars")
                  neo-eec-seen-vars
                  (let ((prefix-arg '(5))
                        (current-prefix-arg '(6)))
                    (execute-extended-command 7 "neo-eec-vars")
                    neo-eec-seen-vars)))"#,
        );
        assert_eq!(results[0], "OK (nil (nil nil) (7 nil))");
    }

    #[test]
    fn execute_extended_command_applies_prefix_arg_for_p_and_p_specs() {
        let mut ev = Evaluator::new();
        let results = eval_all_with(
            &mut ev,
            r#"(progn
                 (setq neo-eec-seen-p :unset)
                 (setq neo-eec-seen-P :unset)
                 (defun neo-eec-p (arg)
                   (interactive "p")
                   (setq neo-eec-seen-p arg)
                   'neo-eec-p-ret)
                 (defun neo-eec-P (arg)
                   (interactive "P")
                   (setq neo-eec-seen-P arg)
                   'neo-eec-P-ret)
                 (list
                  (list (execute-extended-command 7 "neo-eec-p") neo-eec-seen-p)
                  (list (execute-extended-command '(4) "neo-eec-p") neo-eec-seen-p)
                  (list (execute-extended-command '- "neo-eec-p") neo-eec-seen-p)
                  (list (execute-extended-command 7 "neo-eec-P") neo-eec-seen-P)
                  (list (execute-extended-command '(4) "neo-eec-P") neo-eec-seen-P)
                  (list (execute-extended-command '- "neo-eec-P") neo-eec-seen-P)))"#,
        );
        assert_eq!(
            results[0],
            "OK ((nil 7) (nil 4) (nil -1) (nil 7) (nil (4)) (nil -))"
        );
    }

    #[test]
    fn execute_extended_command_no_name_signals_end_of_file() {
        let mut ev = Evaluator::new();
        let result = builtin_execute_extended_command(&mut ev, vec![Value::Nil])
            .expect_err("execute-extended-command should signal end-of-file in batch");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "end-of-file");
                assert_eq!(sig.data, vec![Value::string("Error reading from stdin")]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn execute_extended_command_rejects_symbol_name_payload() {
        let mut ev = Evaluator::new();
        let result =
            builtin_execute_extended_command(&mut ev, vec![Value::Nil, Value::symbol("ignore")])
                .expect_err("symbol payload should not be accepted as a command name");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "\u{2018}ignore\u{2019} is not a valid command name"
                    )]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn execute_extended_command_rejects_non_command_name() {
        let mut ev = Evaluator::new();
        let result =
            builtin_execute_extended_command(&mut ev, vec![Value::Nil, Value::string("car")])
                .expect_err("non-command names should be rejected");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "\u{2018}car\u{2019} is not a valid command name"
                    )]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn execute_extended_command_rejects_non_string_name_payload() {
        let mut ev = Evaluator::new();
        let result = builtin_execute_extended_command(&mut ev, vec![Value::Nil, Value::Int(1)])
            .expect_err("non-string command names should be rejected");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "\u{2018}1\u{2019} is not a valid command name"
                    )]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn execute_extended_command_rejects_overflow_arity() {
        let mut ev = Evaluator::new();
        let result = builtin_execute_extended_command(
            &mut ev,
            vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil],
        )
        .expect_err("execute-extended-command should reject more than three arguments");
        match result {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn define_key_accepts_optional_remove_arg() {
        let result = eval_one(
            r#"(let ((m (make-sparse-keymap)))
                 (eq (define-key m "a" 'ignore t) 'ignore))"#,
        );
        assert_eq!(result, "OK t");
    }

    // -------------------------------------------------------------------
    // where-is-internal
    // -------------------------------------------------------------------

    #[test]
    fn where_is_internal_finds_binding_in_explicit_map() {
        let result = eval_one(
            r#"(let ((m (make-sparse-keymap)))
                 (define-key m "a" 'ignore)
                 (equal (car (where-is-internal 'ignore m)) [97]))"#,
        );
        assert_eq!(result, "OK t");
    }

    #[test]
    fn where_is_internal_first_only_returns_vector() {
        let result = eval_one(
            r#"(let ((m (make-sparse-keymap)))
                 (define-key m "a" 'ignore)
                 (equal (where-is-internal 'ignore m t) [97]))"#,
        );
        assert_eq!(result, "OK t");
    }

    #[test]
    fn where_is_internal_keymap_type_errors() {
        let result = eval_one(
            r#"(condition-case err
                   (where-is-internal 'ignore 'not-a-map)
                 (error err))"#,
        );
        assert_eq!(result, "OK (wrong-type-argument keymapp not-a-map)");
    }

    #[test]
    fn where_is_internal_non_definition_returns_nil() {
        let result = eval_one("(where-is-internal 1)");
        assert_eq!(result, "OK nil");
    }

    #[test]
    fn command_modes_returns_nil_with_arity_checks() {
        assert_eq!(eval_one("(command-modes 'ignore)"), "OK nil");
        assert_eq!(eval_one("(command-modes nil)"), "OK nil");
        assert_eq!(eval_one("(command-modes 0)"), "OK nil");
        assert_eq!(eval_one("(command-modes \"ignore\")"), "OK nil");
        assert_eq!(
            eval_one("(command-modes '(lambda () (interactive)))"),
            "OK nil"
        );
        assert_eq!(eval_one("(command-modes '(lambda (x) x))"), "OK nil");
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-modes)
                     (wrong-number-of-arguments (car err)))"#
            ),
            "OK wrong-number-of-arguments"
        );
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-modes 'ignore nil)
                     (wrong-number-of-arguments (car err)))"#
            ),
            "OK wrong-number-of-arguments"
        );
    }

    #[test]
    fn command_remapping_nil_and_keymap_type_checks() {
        assert_eq!(eval_one("(command-remapping 'ignore)"), "OK nil");
        assert_eq!(eval_one("(command-remapping 'ignore nil)"), "OK nil");
        assert_eq!(eval_one("(command-remapping 'ignore nil nil)"), "OK nil");
        assert_eq!(
            eval_one("(command-remapping 'ignore nil (list 'keymap))"),
            "OK nil"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(1 2 3))"),
            "OK nil"
        );
        assert_eq!(eval_one("(command-remapping 'ignore nil '(foo))"), "OK nil");
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(foo bar))"),
            "OK nil"
        );
        assert_eq!(eval_one("(command-remapping nil)"), "OK nil");
        assert_eq!(eval_one("(command-remapping 0)"), "OK nil");
        assert_eq!(eval_one("(command-remapping \"ignore\")"), "OK nil");
        assert_eq!(
            eval_one("(command-remapping '(lambda () (interactive)))"),
            "OK nil"
        );
        assert_eq!(eval_one("(command-remapping '(lambda (x) x))"), "OK nil");
        assert_eq!(eval_one("(command-remapping 'ignore '(x) nil)"), "OK nil");
        assert_eq!(
            eval_one("(command-remapping 'ignore '(x) (make-sparse-keymap))"),
            "OK nil"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore [x] (make-sparse-keymap))"),
            "OK nil"
        );

        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping 'ignore nil t)
                     (wrong-type-argument (list (car err) (cdr err))))"#
            ),
            "OK (wrong-type-argument (keymapp t))"
        );
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping 'ignore nil [1])
                     (wrong-type-argument (list (car err) (cdr err))))"#
            ),
            "OK (wrong-type-argument (keymapp [1]))"
        );
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping 'ignore nil "x")
                     (wrong-type-argument (list (car err) (cdr err))))"#
            ),
            "OK (wrong-type-argument (keymapp \"x\"))"
        );
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping 'ignore nil 1)
                     (wrong-type-argument (list (car err) (cdr err))))"#
            ),
            "OK (wrong-type-argument (keymapp 1))"
        );
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping 'ignore nil 'foo)
                     (wrong-type-argument (list (car err) (cdr err))))"#
            ),
            "OK (wrong-type-argument (keymapp foo))"
        );

        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping)
                     (wrong-number-of-arguments (car err)))"#
            ),
            "OK wrong-number-of-arguments"
        );
        assert_eq!(
            eval_one(
                r#"(condition-case err
                       (command-remapping 'ignore nil nil nil)
                     (wrong-number-of-arguments (car err)))"#
            ),
            "OK wrong-number-of-arguments"
        );
    }

    #[test]
    fn command_remapping_resolves_remap_bindings_on_keymap_handles() {
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] 'self-insert-command)
                     (command-remapping 'ignore nil m))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] [x])
                     (command-remapping 'ignore nil m))"#
            ),
            "OK [x]"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] 1)
                     (command-remapping 'ignore nil m))"#
            ),
            "OK nil"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] t)
                     (command-remapping 'ignore nil m))"#
            ),
            "OK nil"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] '(menu-item "x" ignore))
                     (command-remapping 'ignore nil m))"#
            ),
            "OK ignore"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] '(menu-item "x" 1))
                     (command-remapping 'ignore nil m))"#
            ),
            "OK nil"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] '(menu-item))
                     (command-remapping 'ignore nil m))"#
            ),
            "OK (menu-item)"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key m [remap ignore] 'self-insert-command)
                     (command-remapping 0 nil m))"#
            ),
            "OK nil"
        );
        assert_eq!(
            eval_one(
                r#"(let ((m (make-sparse-keymap)))
                     (define-key (current-global-map) [remap ignore] 'self-insert-command)
                     (command-remapping 'ignore))"#
            ),
            "OK self-insert-command"
        );
    }

    #[test]
    fn command_remapping_prefers_local_map_when_keymap_omitted_or_nil() {
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap)))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key l [remap ignore] 'self-insert-command)
                     (command-remapping 'ignore))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap)))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key l [remap ignore] 'self-insert-command)
                     (command-remapping 'ignore nil nil))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap)))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key g [remap ignore] 'forward-char)
                     (define-key l [remap ignore] 'self-insert-command)
                     (command-remapping 'ignore))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap)))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key g [remap ignore] 'self-insert-command)
                     (command-remapping 'ignore))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(with-temp-buffer
                     (let ((g (make-sparse-keymap))
                           (l (make-sparse-keymap)))
                       (use-global-map g)
                       (use-local-map l)
                       (define-key l [remap ignore] 'self-insert-command)
                       (command-remapping 'ignore (point-min))))"#
            ),
            "OK self-insert-command"
        );
    }

    #[test]
    fn command_remapping_checks_minor_mode_maps_before_local_and_global() {
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (m (make-sparse-keymap))
                         (minor-mode-map-alist nil)
                         (demo-mode t))
                     (use-global-map g)
                     (define-key m [remap ignore] 'self-insert-command)
                     (setq minor-mode-map-alist (list (cons 'demo-mode m)))
                     (command-remapping 'ignore))"#
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap))
                         (m (make-sparse-keymap))
                         (minor-mode-map-alist nil)
                         (demo-mode t))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key m [remap ignore] 'forward-char)
                     (define-key l [remap ignore] 'self-insert-command)
                     (setq minor-mode-map-alist (list (cons 'demo-mode m)))
                     (command-remapping 'ignore))"#
            ),
            "OK forward-char"
        );
        assert_eq!(
            eval_one(
                r#"(let ((g (make-sparse-keymap))
                         (l (make-sparse-keymap))
                         (m (make-sparse-keymap))
                         (minor-mode-overriding-map-alist nil)
                         (minor-mode-map-alist nil)
                         (demo-mode t))
                     (use-global-map g)
                     (use-local-map l)
                     (define-key m [remap ignore] 'forward-char)
                     (define-key l [remap ignore] 'self-insert-command)
                     (setq minor-mode-overriding-map-alist (list (cons 'demo-mode m)))
                     (setq minor-mode-map-alist (list (cons 'demo-mode l)))
                     (command-remapping 'ignore))"#
            ),
            "OK forward-char"
        );
        assert_eq!(
            eval_one(
                r#"(let ((minor-mode-map-alist '((demo-mode . 999999)))
                         (demo-mode t))
                     (command-remapping 'ignore))"#
            ),
            "OK nil"
        );
    }

    #[test]
    fn command_remapping_resolves_remap_bindings_on_lisp_keymaps() {
        assert_eq!(
            eval_one(
                "(command-remapping 'ignore nil '(keymap (remap keymap (ignore . self-insert-command))))"
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                "(command-remapping 'ignore nil '(keymap (remap keymap (ignore menu-item \"x\" ignore))))"
            ),
            "OK ignore"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(keymap (remap keymap (ignore . 1))))"),
            "OK nil"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(keymap (remap keymap (ignore . t))))"),
            "OK nil"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(keymap (remap keymap (ignore))))"),
            "OK nil"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(keymap (remap keymap (ignore . [x]))))"),
            "OK [x]"
        );
        assert_eq!(
            eval_one("(command-remapping 'ignore nil '(keymap (remap keymap (ignore . \"x\"))))"),
            "OK \"x\""
        );
        assert_eq!(
            eval_one(
                "(command-remapping 'not-bound nil '(keymap (remap keymap (not-bound . self-insert-command))))"
            ),
            "OK self-insert-command"
        );
        assert_eq!(
            eval_one(
                "(command-remapping 'ignore nil '(keymap (remap keymap (foo . self-insert-command))))"
            ),
            "OK nil"
        );
    }

    // -------------------------------------------------------------------
    // Extractors (unit tests for internal functions)
    // -------------------------------------------------------------------

    #[test]
    fn bounds_word_middle() {
        let text = "hello world";
        let bounds = bounds_word(text, 2).unwrap(); // 'l' in "hello"
        assert_eq!(&text[bounds.0..bounds.1], "hello");
    }

    #[test]
    fn bounds_word_at_start() {
        let text = "abc def";
        let bounds = bounds_word(text, 0).unwrap();
        assert_eq!(&text[bounds.0..bounds.1], "abc");
    }

    #[test]
    fn bounds_word_at_space() {
        let text = "abc def";
        let bounds = bounds_word(text, 3).unwrap(); // between words -> previous word
        assert_eq!(&text[bounds.0..bounds.1], "abc");
    }

    #[test]
    fn bounds_symbol_with_hyphen() {
        let text = "foo-bar baz";
        let bounds = bounds_symbol(text, 1).unwrap();
        assert_eq!(&text[bounds.0..bounds.1], "foo-bar");
    }

    #[test]
    fn bounds_line_first_line() {
        let text = "first\nsecond\n";
        let bounds = bounds_line(text, 2).unwrap();
        assert_eq!(&text[bounds.0..bounds.1], "first\n");
    }

    #[test]
    fn bounds_whitespace_basic() {
        let text = "hello   world";
        let bounds = bounds_whitespace(text, 6).unwrap(); // middle space
        assert_eq!(&text[bounds.0..bounds.1], "   ");
    }

    #[test]
    fn bounds_number_basic() {
        let text = "val=42.5ok";
        let bounds = bounds_number(text, 4).unwrap();
        assert_eq!(&text[bounds.0..bounds.1], "42.5");
    }

    #[test]
    fn bounds_number_no_number() {
        let text = "hello";
        assert!(bounds_number(text, 0).is_none());
    }

    // -------------------------------------------------------------------
    // Integration: via Elisp eval
    // -------------------------------------------------------------------

    #[test]
    fn eval_define_minor_mode() {
        let results =
            eval_all(r#"(define-minor-mode my-test-mode "A test minor mode" :lighter " T")"#);
        assert_eq!(results[0], "OK my-test-mode");
    }

    #[test]
    fn eval_define_derived_mode() {
        let results =
            eval_all(r#"(define-derived-mode my-custom-mode nil "MyCustom" "Custom mode.")"#);
        assert_eq!(results[0], "OK my-custom-mode");
    }

    #[test]
    fn eval_define_derived_mode_and_activate() {
        let results = eval_all(
            r#"(define-derived-mode act-mode nil "Activated")
               (act-mode)
               major-mode"#,
        );
        assert_eq!(results[2], "OK act-mode");
    }

    #[test]
    fn eval_minor_mode_full_cycle() {
        let results = eval_all(
            r#"(define-minor-mode cycle-mode "Cycle mode" :lighter " C")
               cycle-mode
               (cycle-mode)
               cycle-mode
               (cycle-mode)
               cycle-mode"#,
        );
        assert_eq!(results[1], "OK nil"); // initially off
        assert_eq!(results[3], "OK t"); // toggled on
        assert_eq!(results[5], "OK nil"); // toggled off
    }
}
