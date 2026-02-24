//! Keyboard macro support -- recording, playback, and macro management.
//!
//! Provides Emacs-compatible keyboard macro functionality:
//! - `start-kbd-macro` / `end-kbd-macro` -- record key sequences
//! - `call-last-kbd-macro` -- replay the last recorded macro
//! - `execute-kbd-macro` -- execute a macro N times
//! - `name-last-kbd-macro` -- bind a macro to a symbol
//! - `insert-kbd-macro` -- insert macro definition as Lisp text
//! - `kbd-macro-query` -- interactive query during playback
//! - `store-kbd-macro-event` -- add event to current recording
//! - `kmacro-set-counter` / `kmacro-add-counter` / `kmacro-set-format` -- counter ops
//! - `executing-kbd-macro-p` / `defining-kbd-macro-p` -- predicates
//! - `last-kbd-macro` -- retrieve last macro value
//! - `kmacro-p` -- predicate for macro values

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use crate::gc::GcTrace;

// ---------------------------------------------------------------------------
// Argument helpers (local copies, matching builtins.rs convention)
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

fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// KmacroManager
// ---------------------------------------------------------------------------

/// Central manager for keyboard macro state.
#[derive(Clone, Debug)]
pub struct KmacroManager {
    /// Whether we are currently recording a keyboard macro.
    pub recording: bool,
    /// Whether a keyboard macro is currently executing.
    pub executing: bool,
    /// Key events accumulated during the current recording session.
    pub current_macro: Vec<Value>,
    /// The last completed macro (after `end-kbd-macro`).
    pub last_macro: Option<Vec<Value>>,
    /// Ring of previously saved macros (most recent first).
    pub macro_ring: Vec<Vec<Value>>,
    /// Keyboard macro counter (for `kmacro-insert-counter`).
    pub counter: i64,
    /// Format string for the counter (printf-style, default "%d").
    pub counter_format: String,
}

impl Default for KmacroManager {
    fn default() -> Self {
        Self::new()
    }
}

impl GcTrace for KmacroManager {
    fn trace_roots(&self, roots: &mut Vec<Value>) {
        for value in &self.current_macro {
            roots.push(value.clone());
        }
        if let Some(ref last) = self.last_macro {
            for value in last {
                roots.push(value.clone());
            }
        }
        for macro_entry in &self.macro_ring {
            for value in macro_entry {
                roots.push(value.clone());
            }
        }
    }
}

impl KmacroManager {
    /// Create a new manager with default state.
    pub fn new() -> Self {
        Self {
            recording: false,
            executing: false,
            current_macro: Vec::new(),
            last_macro: None,
            macro_ring: Vec::new(),
            counter: 0,
            counter_format: "%d".to_string(),
        }
    }

    /// Start recording a new keyboard macro.
    /// If `append` is true, append to the last macro instead of starting fresh.
    pub fn start_recording(&mut self, append: bool) {
        self.recording = true;
        if append {
            // Begin with a copy of the last macro so new keys are appended.
            self.current_macro = self.last_macro.clone().unwrap_or_default();
        } else {
            self.current_macro.clear();
        }
    }

    /// Stop recording and push the result onto the ring.
    /// Returns the recorded macro, or None if nothing was recorded.
    pub fn stop_recording(&mut self) -> Option<Vec<Value>> {
        self.recording = false;
        if self.current_macro.is_empty() {
            return None;
        }
        let recorded = self.current_macro.clone();
        // Push old last_macro onto the ring before replacing.
        if let Some(prev) = self.last_macro.take() {
            self.macro_ring.push(prev);
        }
        self.last_macro = Some(recorded.clone());
        self.current_macro.clear();
        Some(recorded)
    }

    /// Record a single event into the current macro (no-op if not recording).
    pub fn store_event(&mut self, event: Value) {
        if self.recording {
            self.current_macro.push(event);
        }
    }

    /// Format the counter using the current format string.
    pub fn format_counter(&self) -> String {
        // Support basic %d / %o / %x / %X formats.
        // For anything more complex, fall back to decimal.
        let fmt = &self.counter_format;
        if fmt.contains("%d") {
            fmt.replace("%d", &self.counter.to_string())
        } else if fmt.contains("%o") {
            fmt.replace("%o", &format!("{:o}", self.counter))
        } else if fmt.contains("%x") {
            fmt.replace("%x", &format!("{:x}", self.counter))
        } else if fmt.contains("%X") {
            fmt.replace("%X", &format!("{:X}", self.counter))
        } else {
            // Fallback: just print the number.
            self.counter.to_string()
        }
    }
}

// ===========================================================================
// Builtins (evaluator-dependent)
// ===========================================================================

/// (defining-kbd-macro APPEND &optional NO-EXEC) -> nil
///
/// Compatibility subset:
/// - starts keyboard macro recording (like `start-kbd-macro`)
/// - when APPEND is non-nil with no prior macro, signal
///   `(wrong-type-argument arrayp nil)`
/// - when already recording, signal `(error "Already defining kbd macro")`
/// - NO-EXEC is accepted for arity compatibility and currently ignored
pub(crate) fn builtin_defining_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("defining-kbd-macro", &args, 1)?;
    expect_max_args("defining-kbd-macro", &args, 2)?;
    if eval.kmacro.recording {
        return Err(signal(
            "error",
            vec![Value::string("Already defining kbd macro")],
        ));
    }

    let append = args[0].is_truthy();
    if append && eval.kmacro.last_macro.is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), Value::Nil],
        ));
    }

    eval.kmacro.start_recording(append);
    Ok(Value::Nil)
}

/// (start-kbd-macro &optional APPEND NO-EXEC) -> nil
///
/// Start recording a keyboard macro.  With non-nil APPEND, append to
/// the last macro instead of starting a new one.  Signals an error if
/// already recording.  NO-EXEC is accepted for arity compatibility and
/// currently ignored.
pub(crate) fn builtin_start_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("start-kbd-macro", &args, 2)?;
    if eval.kmacro.recording {
        return Err(signal(
            "error",
            vec![Value::string("Already defining a keyboard macro")],
        ));
    }
    let append = args.first().map_or(false, |v| v.is_truthy());
    eval.kmacro.start_recording(append);
    Ok(Value::Nil)
}

/// (end-kbd-macro &optional REPEAT LOOPFUNC) -> nil
///
/// Stop recording a keyboard macro.  Signals an error if not currently
/// recording.  The optional REPEAT argument is accepted for compatibility
/// but ignored in this implementation.
pub(crate) fn builtin_end_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("end-kbd-macro", &args, 2)?;
    if !eval.kmacro.recording {
        return Err(signal(
            "error",
            vec![Value::string("Not defining a keyboard macro")],
        ));
    }
    eval.kmacro.stop_recording();
    Ok(Value::Nil)
}

/// (call-last-kbd-macro &optional REPEAT LOOPFUNC) -> nil
///
/// Execute the last keyboard macro.  Each recorded event is passed to
/// the evaluator via `funcall` on `execute-kbd-macro-event` if defined,
/// otherwise events are evaluated directly.
pub(crate) fn builtin_call_last_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("call-last-kbd-macro", &args, 2)?;
    let repeat = if args.is_empty() {
        1i64
    } else {
        expect_int(&args[0]).unwrap_or(1)
    };

    let macro_keys = match &eval.kmacro.last_macro {
        Some(keys) => keys.clone(),
        None => {
            return Err(signal(
                "error",
                vec![Value::string("No keyboard macro has been defined")],
            ));
        }
    };

    eval.kmacro.executing = true;
    for _ in 0..repeat {
        for event in &macro_keys {
            // Try to call the event as a command.  If it is a symbol,
            // funcall it; otherwise treat it as a self-insert character.
            match event {
                Value::Symbol(name) => {
                    let func = Value::symbol(name.clone());
                    if let Err(err) = eval.apply(func, vec![]) {
                        eval.kmacro.executing = false;
                        return Err(err);
                    }
                }
                _ => {
                    // For character events, attempt self-insert-command
                    // by looking it up; if unavailable, just ignore.
                    if let Some(func) = eval.obarray.symbol_function("self-insert-command").cloned()
                    {
                        if let Err(err) = eval.apply(func, vec![Value::Int(1)]) {
                            eval.kmacro.executing = false;
                            return Err(err);
                        }
                    }
                }
            }
        }
    }
    eval.kmacro.executing = false;
    Ok(Value::Nil)
}

/// (execute-kbd-macro MACRO &optional COUNT LOOPFUNC) -> nil
///
/// Execute MACRO (a vector, string, or symbol) COUNT times.
/// If MACRO is a symbol, its function definition is used.
pub(crate) fn builtin_execute_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("execute-kbd-macro", &args, 1)?;
    expect_max_args("execute-kbd-macro", &args, 3)?;

    let count = if args.len() >= 2 {
        expect_int(&args[1]).unwrap_or(1)
    } else {
        1
    };

    // Resolve the macro value.
    let macro_events = resolve_macro_events(&args[0])?;

    eval.kmacro.executing = true;
    for _ in 0..count {
        for event in &macro_events {
            match event {
                Value::Symbol(name) => {
                    let func = Value::symbol(name.clone());
                    if let Err(err) = eval.apply(func, vec![]) {
                        eval.kmacro.executing = false;
                        return Err(err);
                    }
                }
                _ => {
                    if let Some(func) = eval.obarray.symbol_function("self-insert-command").cloned()
                    {
                        if let Err(err) = eval.apply(func, vec![Value::Int(1)]) {
                            eval.kmacro.executing = false;
                            return Err(err);
                        }
                    }
                }
            }
        }
    }
    eval.kmacro.executing = false;
    Ok(Value::Nil)
}

/// (name-last-kbd-macro SYMBOL) -> nil
///
/// Bind the last keyboard macro to SYMBOL as its function definition.
/// Signals an error if no macro has been recorded.
fn name_last_kbd_macro_impl(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
    call_name: &str,
) -> EvalResult {
    expect_args(call_name, &args, 1)?;

    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Str(id) => with_heap(|h| h.get_string(*id).clone()),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            ));
        }
    };

    let macro_val = match &eval.kmacro.last_macro {
        Some(keys) => Value::vector(keys.clone()),
        None => {
            return Err(signal(
                "error",
                vec![Value::string("No keyboard macro has been defined")],
            ));
        }
    };

    eval.obarray.set_symbol_function(&name, macro_val);
    Ok(Value::Nil)
}

/// (name-last-kbd-macro SYMBOL) -> nil
///
/// Bind the last keyboard macro to SYMBOL as its function definition.
/// Signals an error if no macro has been recorded.
pub(crate) fn builtin_name_last_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    name_last_kbd_macro_impl(eval, args, "name-last-kbd-macro")
}

/// (kmacro-name-last-macro SYMBOL) -> nil
///
/// Alias entry point used in startup wrappers for arity payload parity.
pub(crate) fn builtin_kmacro_name_last_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    name_last_kbd_macro_impl(eval, args, "kmacro-name-last-macro")
}

/// (insert-kbd-macro MACRONAME &optional KEYS) -> nil
///
/// Insert the definition of keyboard macro MACRONAME as Lisp code.
/// Returns the text representation as a string (since we don't have
/// direct buffer insertion in the VM).
pub(crate) fn builtin_insert_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("insert-kbd-macro", &args, 1)?;
    expect_max_args("insert-kbd-macro", &args, 2)?;

    let name = match &args[0] {
        Value::Symbol(s) => s.clone(),
        Value::Str(id) => with_heap(|h| h.get_string(*id).clone()),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            ));
        }
    };

    // Look up the function definition.
    let func = eval.obarray.symbol_function(&name).cloned();
    let macro_events = match func {
        Some(Value::Vector(v)) => {
            let items = with_heap(|h| h.get_vector(v).clone());
            items.clone()
        }
        Some(other) => {
            // Try to resolve as a list.
            match list_to_vec(&other) {
                Some(v) => v,
                None => {
                    return Err(signal(
                        "error",
                        vec![Value::string(format!("{} is not a keyboard macro", name))],
                    ));
                }
            }
        }
        None => {
            // Fall back to last_macro if the symbol is not bound.
            match &eval.kmacro.last_macro {
                Some(keys) => keys.clone(),
                None => {
                    return Err(signal(
                        "error",
                        vec![Value::string(format!(
                            "{} is not defined as a keyboard macro",
                            name
                        ))],
                    ));
                }
            }
        }
    };

    // Build a textual representation.
    let mut parts = Vec::new();
    for ev in &macro_events {
        match ev {
            Value::Char(c) => parts.push(format!("?{}", c)),
            Value::Int(n) => parts.push(n.to_string()),
            Value::Symbol(s) => parts.push(s.clone()),
            Value::Str(id) => {
                let s = with_heap(|h| h.get_string(*id).clone());
                parts.push(format!("\"{}\"", s));
            }
            other => parts.push(format!("{:?}", other)),
        }
    }
    let definition = format!("(fset '{}  (vector {}))\n", name, parts.join(" "));
    Ok(Value::string(definition))
}

/// (kbd-macro-query FLAG) -> nil
///
/// Compatibility subset:
/// - arity is exactly 1
/// - when neither recording nor executing a keyboard macro, signal
///   `(user-error "Not defining or executing kbd macro")`
/// - otherwise return nil (interactive query flow is not implemented yet)
pub(crate) fn builtin_kbd_macro_query(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("kbd-macro-query", &args, 1)?;
    if !eval.kmacro.recording && !eval.kmacro.executing {
        return Err(signal(
            "user-error",
            vec![Value::string("Not defining or executing kbd macro")],
        ));
    }
    Ok(Value::Nil)
}

/// (defining-kbd-macro-p) -> non-nil when keyboard macro recording is active.
#[cfg(test)]
pub(crate) fn builtin_defining_kbd_macro_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("defining-kbd-macro-p", &args, 0)?;
    Ok(Value::bool(eval.kmacro.recording))
}

/// (executing-kbd-macro-p) -> non-nil when keyboard macro execution is active.
#[cfg(test)]
pub(crate) fn builtin_executing_kbd_macro_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("executing-kbd-macro-p", &args, 0)?;
    Ok(Value::bool(eval.kmacro.executing))
}

/// (last-kbd-macro) -> last recorded macro vector or nil.
#[cfg(test)]
pub(crate) fn builtin_last_kbd_macro(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("last-kbd-macro", &args, 0)?;
    match &eval.kmacro.last_macro {
        Some(keys) => Ok(Value::vector(keys.clone())),
        None => Ok(Value::Nil),
    }
}

/// (kmacro-p OBJECT) -> non-nil when OBJECT is a keyboard macro value.
///
/// Compatibility subset: accepts vector and string macro encodings.
#[cfg(test)]
pub(crate) fn builtin_kmacro_p(args: Vec<Value>) -> EvalResult {
    expect_args("kmacro-p", &args, 1)?;
    Ok(Value::bool(matches!(
        args[0],
        Value::Vector(_) | Value::Str(_)
    )))
}

/// (kmacro-set-counter COUNTER &optional FORMAT-START) -> nil
#[cfg(test)]
pub(crate) fn builtin_kmacro_set_counter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("kmacro-set-counter", &args, 1)?;
    expect_max_args("kmacro-set-counter", &args, 2)?;
    eval.kmacro.counter = expect_int(&args[0])?;
    Ok(Value::Nil)
}

/// (kmacro-add-counter DELTA) -> nil
#[cfg(test)]
pub(crate) fn builtin_kmacro_add_counter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("kmacro-add-counter", &args, 1)?;
    eval.kmacro.counter += expect_int(&args[0])?;
    Ok(Value::Nil)
}

/// (kmacro-set-format FORMAT) -> nil
#[cfg(test)]
pub(crate) fn builtin_kmacro_set_format(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("kmacro-set-format", &args, 1)?;
    let format = match &args[0] {
        Value::Str(id) => {
            let s = crate::elisp::value::with_heap(|h| h.get_string(*id).clone());
            if s.is_empty() { "%d".to_string() } else { s }
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    eval.kmacro.counter_format = format;
    Ok(Value::Nil)
}

/// (store-kbd-macro-event EVENT) -> nil
///
/// Add EVENT to the keyboard macro currently being recorded.
/// If not currently recording, this is a no-op.
pub(crate) fn builtin_store_kbd_macro_event(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("store-kbd-macro-event", &args, 1)?;
    eval.kmacro.store_event(args[0].clone());
    Ok(Value::Nil)
}

// ===========================================================================
// Internal helpers
// ===========================================================================

/// Resolve a macro value (vector, string, list, or symbol) into a Vec of events.
fn resolve_macro_events(value: &Value) -> Result<Vec<Value>, Flow> {
    match value {
        Value::Vector(v) => {
            let items = with_heap(|h| h.get_vector(*v).clone());
            Ok(items.clone())
        }
        Value::Str(id) => {
            // Each character in the string becomes a Char event.
            let s = with_heap(|h| h.get_string(*id).clone());
            Ok(s.chars().map(Value::Char).collect())
        }
        Value::Cons(_) | Value::Nil => {
            // Try to interpret as a proper list.
            match list_to_vec(value) {
                Some(v) => Ok(v),
                None => Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("arrayp"), value.clone()],
                )),
            }
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), value.clone()],
        )),
    }
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // KmacroManager unit tests
    // -----------------------------------------------------------------------

    #[test]
    fn new_manager_defaults() {
        let mgr = KmacroManager::new();
        assert!(!mgr.recording);
        assert!(!mgr.executing);
        assert!(mgr.current_macro.is_empty());
        assert!(mgr.last_macro.is_none());
        assert!(mgr.macro_ring.is_empty());
        assert_eq!(mgr.counter, 0);
        assert_eq!(mgr.counter_format, "%d");
    }

    #[test]
    fn start_stop_recording() {
        let mut mgr = KmacroManager::new();

        // Start recording
        mgr.start_recording(false);
        assert!(mgr.recording);

        // Store some events
        mgr.store_event(Value::Char('a'));
        mgr.store_event(Value::Char('b'));
        mgr.store_event(Value::Char('c'));
        assert_eq!(mgr.current_macro.len(), 3);

        // Stop recording
        let result = mgr.stop_recording();
        assert!(!mgr.recording);
        assert!(result.is_some());
        let recorded = result.unwrap();
        assert_eq!(recorded.len(), 3);

        // last_macro should be set
        assert!(mgr.last_macro.is_some());
        assert_eq!(mgr.last_macro.as_ref().unwrap().len(), 3);

        // current_macro should be cleared
        assert!(mgr.current_macro.is_empty());
    }

    #[test]
    fn stop_recording_empty() {
        let mut mgr = KmacroManager::new();
        mgr.start_recording(false);
        // Don't store any events
        let result = mgr.stop_recording();
        assert!(result.is_none());
        assert!(mgr.last_macro.is_none());
    }

    #[test]
    fn append_recording() {
        let mut mgr = KmacroManager::new();

        // Record first macro
        mgr.start_recording(false);
        mgr.store_event(Value::Char('x'));
        mgr.store_event(Value::Char('y'));
        mgr.stop_recording();

        // Record second macro with append
        mgr.start_recording(true);
        assert_eq!(mgr.current_macro.len(), 2); // starts with previous
        mgr.store_event(Value::Char('z'));
        assert_eq!(mgr.current_macro.len(), 3);
        mgr.stop_recording();

        assert_eq!(mgr.last_macro.as_ref().unwrap().len(), 3);
    }

    #[test]
    fn macro_ring_push() {
        let mut mgr = KmacroManager::new();

        // Record first macro
        mgr.start_recording(false);
        mgr.store_event(Value::Char('a'));
        mgr.stop_recording();
        assert!(mgr.macro_ring.is_empty()); // first macro, nothing to push

        // Record second macro (pushes first onto ring)
        mgr.start_recording(false);
        mgr.store_event(Value::Char('b'));
        mgr.stop_recording();
        assert_eq!(mgr.macro_ring.len(), 1);
        assert_eq!(mgr.macro_ring[0].len(), 1); // the 'a' macro

        // Record third macro (pushes second onto ring)
        mgr.start_recording(false);
        mgr.store_event(Value::Char('c'));
        mgr.stop_recording();
        assert_eq!(mgr.macro_ring.len(), 2);
    }

    #[test]
    fn store_event_not_recording() {
        let mut mgr = KmacroManager::new();
        // Not recording -- store_event should be a no-op
        mgr.store_event(Value::Char('a'));
        assert!(mgr.current_macro.is_empty());
    }

    #[test]
    fn format_counter_decimal() {
        let mgr = KmacroManager {
            counter: 42,
            counter_format: "%d".to_string(),
            ..KmacroManager::new()
        };
        assert_eq!(mgr.format_counter(), "42");
    }

    #[test]
    fn format_counter_hex() {
        let mgr = KmacroManager {
            counter: 255,
            counter_format: "%x".to_string(),
            ..KmacroManager::new()
        };
        assert_eq!(mgr.format_counter(), "ff");
    }

    #[test]
    fn format_counter_octal() {
        let mgr = KmacroManager {
            counter: 8,
            counter_format: "%o".to_string(),
            ..KmacroManager::new()
        };
        assert_eq!(mgr.format_counter(), "10");
    }

    #[test]
    fn format_counter_with_prefix() {
        let mgr = KmacroManager {
            counter: 7,
            counter_format: "item-%d".to_string(),
            ..KmacroManager::new()
        };
        assert_eq!(mgr.format_counter(), "item-7");
    }

    #[test]
    fn format_counter_unknown_format() {
        let mgr = KmacroManager {
            counter: 99,
            counter_format: "???".to_string(),
            ..KmacroManager::new()
        };
        // Fallback to plain decimal
        assert_eq!(mgr.format_counter(), "99");
    }

    // -----------------------------------------------------------------------
    // Builtin-level tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_start_and_end_macro() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Start recording
        let result = builtin_start_kbd_macro(&mut eval, vec![]);
        assert!(result.is_ok());
        assert!(eval.kmacro.recording);

        // Double-start should error
        let result = builtin_start_kbd_macro(&mut eval, vec![]);
        assert!(result.is_err());

        // Store some events
        let _ = builtin_store_kbd_macro_event(&mut eval, vec![Value::Char('h')]);
        let _ = builtin_store_kbd_macro_event(&mut eval, vec![Value::Char('i')]);

        // End recording
        let result = builtin_end_kbd_macro(&mut eval, vec![]);
        assert!(result.is_ok());
        assert!(!eval.kmacro.recording);
        assert!(eval.kmacro.last_macro.is_some());
        assert_eq!(eval.kmacro.last_macro.as_ref().unwrap().len(), 2);

        // Double-end should error
        let result = builtin_end_kbd_macro(&mut eval, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn test_defining_kbd_macro_builtin_contract() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Arity contract.
        assert!(builtin_defining_kbd_macro(&mut eval, vec![]).is_err());
        assert!(
            builtin_defining_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil, Value::Nil])
                .is_err()
        );

        // APPEND with no prior macro should signal wrong-type-argument.
        let append_without_last = builtin_defining_kbd_macro(&mut eval, vec![Value::True]);
        assert!(append_without_last.is_err());

        // Fresh recording with APPEND=nil should succeed.
        assert_eq!(
            builtin_defining_kbd_macro(&mut eval, vec![Value::Nil]).unwrap(),
            Value::Nil
        );
        assert!(eval.kmacro.recording);

        // Re-entry while recording should signal `error`.
        let already = builtin_defining_kbd_macro(&mut eval, vec![Value::Nil, Value::True]);
        assert!(already.is_err());

        // Finish recording and ensure append path works once a last macro exists.
        let _ = builtin_store_kbd_macro_event(&mut eval, vec![Value::Char('a')]);
        let _ = builtin_end_kbd_macro(&mut eval, vec![]);
        assert!(eval.kmacro.last_macro.is_some());
        assert_eq!(
            builtin_defining_kbd_macro(&mut eval, vec![Value::True, Value::True]).unwrap(),
            Value::Nil
        );
        let _ = builtin_end_kbd_macro(&mut eval, vec![]);
    }

    #[test]
    fn test_start_with_append() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Record a macro
        let _ = builtin_start_kbd_macro(&mut eval, vec![]);
        let _ = builtin_store_kbd_macro_event(&mut eval, vec![Value::Char('a')]);
        let _ = builtin_end_kbd_macro(&mut eval, vec![]);

        // Append to it
        let _ = builtin_start_kbd_macro(&mut eval, vec![Value::True]);
        assert_eq!(eval.kmacro.current_macro.len(), 1); // 'a' carried over
        let _ = builtin_store_kbd_macro_event(&mut eval, vec![Value::Char('b')]);
        let _ = builtin_end_kbd_macro(&mut eval, vec![]);

        assert_eq!(eval.kmacro.last_macro.as_ref().unwrap().len(), 2);
    }

    #[test]
    fn test_call_last_macro_no_macro() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // No macro defined -- should error
        let result = builtin_call_last_kbd_macro(&mut eval, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn test_store_event_wrong_args() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Wrong arg count
        let result = builtin_store_kbd_macro_event(&mut eval, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn test_defining_executing_kbd_macro_p_builtins() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        assert_eq!(
            builtin_defining_kbd_macro_p(&mut eval, vec![]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_executing_kbd_macro_p(&mut eval, vec![]).unwrap(),
            Value::Nil
        );

        eval.kmacro.recording = true;
        assert_eq!(
            builtin_defining_kbd_macro_p(&mut eval, vec![]).unwrap(),
            Value::True
        );
        eval.kmacro.recording = false;

        eval.kmacro.executing = true;
        assert_eq!(
            builtin_executing_kbd_macro_p(&mut eval, vec![]).unwrap(),
            Value::True
        );

        assert!(builtin_defining_kbd_macro_p(&mut eval, vec![Value::Nil]).is_err());
        assert!(builtin_executing_kbd_macro_p(&mut eval, vec![Value::Nil]).is_err());
    }

    #[test]
    fn test_last_kbd_macro_builtin() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        assert_eq!(
            builtin_last_kbd_macro(&mut eval, vec![]).unwrap(),
            Value::Nil
        );

        eval.kmacro.last_macro = Some(vec![Value::Char('x'), Value::Char('y')]);
        let value = builtin_last_kbd_macro(&mut eval, vec![]).unwrap();
        match value {
            Value::Vector(v) => {
                let items = with_heap(|h| h.get_vector(v).clone());
                assert_eq!(*items, vec![Value::Char('x'), Value::Char('y')]);
            }
            other => panic!("expected vector, got {other:?}"),
        }

        assert!(builtin_last_kbd_macro(&mut eval, vec![Value::Nil]).is_err());
    }

    #[test]
    fn test_kmacro_p_builtin_subset() {
        assert_eq!(builtin_kmacro_p(vec![Value::Nil]).unwrap(), Value::Nil);
        assert_eq!(
            builtin_kmacro_p(vec![Value::vector(vec![])]).unwrap(),
            Value::True
        );
        assert_eq!(
            builtin_kmacro_p(vec![Value::string("abc")]).unwrap(),
            Value::True
        );
        assert_eq!(builtin_kmacro_p(vec![Value::Int(1)]).unwrap(), Value::Nil);
        assert!(builtin_kmacro_p(vec![]).is_err());
        assert!(builtin_kmacro_p(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn test_kmacro_set_counter_builtin() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        assert_eq!(
            builtin_kmacro_set_counter(&mut eval, vec![Value::Int(42)]).unwrap(),
            Value::Nil
        );
        assert_eq!(eval.kmacro.counter, 42);

        assert_eq!(
            builtin_kmacro_set_counter(&mut eval, vec![Value::Int(-3), Value::Nil]).unwrap(),
            Value::Nil
        );
        assert_eq!(eval.kmacro.counter, -3);

        assert!(builtin_kmacro_set_counter(&mut eval, vec![]).is_err());
        assert!(builtin_kmacro_set_counter(&mut eval, vec![Value::Nil]).is_err());
        assert!(
            builtin_kmacro_set_counter(&mut eval, vec![Value::Int(1), Value::Nil, Value::Nil])
                .is_err()
        );
    }

    #[test]
    fn test_kmacro_add_counter_builtin() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        eval.kmacro.counter = 10;
        assert_eq!(
            builtin_kmacro_add_counter(&mut eval, vec![Value::Int(5)]).unwrap(),
            Value::Nil
        );
        assert_eq!(eval.kmacro.counter, 15);

        assert_eq!(
            builtin_kmacro_add_counter(&mut eval, vec![Value::Int(-2)]).unwrap(),
            Value::Nil
        );
        assert_eq!(eval.kmacro.counter, 13);

        assert!(builtin_kmacro_add_counter(&mut eval, vec![]).is_err());
        assert!(builtin_kmacro_add_counter(&mut eval, vec![Value::Nil]).is_err());
        assert!(builtin_kmacro_add_counter(&mut eval, vec![Value::Int(1), Value::Nil]).is_err());
    }

    #[test]
    fn test_kmacro_set_format_builtin() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        assert_eq!(eval.kmacro.counter_format, "%d");

        assert_eq!(
            builtin_kmacro_set_format(&mut eval, vec![Value::string("item-%d")]).unwrap(),
            Value::Nil
        );
        assert_eq!(eval.kmacro.counter_format, "item-%d");

        assert_eq!(
            builtin_kmacro_set_format(&mut eval, vec![Value::string("")]).unwrap(),
            Value::Nil
        );
        assert_eq!(eval.kmacro.counter_format, "%d");

        assert!(builtin_kmacro_set_format(&mut eval, vec![]).is_err());
        assert!(builtin_kmacro_set_format(&mut eval, vec![Value::Nil]).is_err());
        assert!(
            builtin_kmacro_set_format(&mut eval, vec![Value::string("%d"), Value::Nil]).is_err()
        );
    }

    #[test]
    fn test_kmacro_builtin_arity_contracts() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        assert_eq!(
            builtin_start_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil]).unwrap(),
            Value::Nil
        );
        assert!(builtin_start_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil]).is_err());
        assert_eq!(
            builtin_end_kbd_macro(&mut eval, vec![]).unwrap(),
            Value::Nil
        );
        assert!(
            builtin_start_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil, Value::Nil]).is_err()
        );
        assert!(
            builtin_end_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil, Value::Nil]).is_err()
        );
        assert!(
            builtin_call_last_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil, Value::Nil])
                .is_err()
        );
        assert!(builtin_execute_kbd_macro(&mut eval, vec![]).is_err());
        assert!(builtin_execute_kbd_macro(
            &mut eval,
            vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil]
        )
        .is_err());
        assert!(builtin_insert_kbd_macro(&mut eval, vec![]).is_err());
        assert!(
            builtin_insert_kbd_macro(&mut eval, vec![Value::Nil, Value::Nil, Value::Nil]).is_err()
        );
    }

    #[test]
    fn test_name_last_kbd_macro() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // No macro -- should error
        let result = builtin_name_last_kbd_macro(&mut eval, vec![Value::symbol("my-macro")]);
        assert!(result.is_err());

        // Record a macro
        eval.kmacro.start_recording(false);
        eval.kmacro
            .store_event(Value::Symbol("forward-char".to_string()));
        eval.kmacro.stop_recording();

        // Name it
        let result = builtin_name_last_kbd_macro(&mut eval, vec![Value::symbol("my-macro")]);
        assert!(result.is_ok());

        // Check that the symbol has a function binding
        let func = eval.obarray.symbol_function("my-macro");
        assert!(func.is_some());
        match func.unwrap() {
            Value::Vector(v) => {
                let items = with_heap(|h| h.get_vector(*v).clone());
                assert_eq!(items.len(), 1);
            }
            other => panic!("Expected Vector, got {:?}", other),
        }
    }

    #[test]
    fn test_name_last_kbd_macro_wrong_type() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        let result = builtin_name_last_kbd_macro(&mut eval, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn test_kbd_macro_query_subset() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        let wrong_arity = builtin_kbd_macro_query(&mut eval, vec![]).unwrap_err();
        match wrong_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("kbd-macro-query"), Value::Int(0)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let not_recording = builtin_kbd_macro_query(&mut eval, vec![Value::True]).unwrap_err();
        match not_recording {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "user-error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Not defining or executing kbd macro")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        eval.kmacro.start_recording(false);
        let ok = builtin_kbd_macro_query(&mut eval, vec![Value::Nil]).unwrap();
        assert!(ok.is_nil());
    }

    #[test]
    fn test_resolve_macro_events_vector() {
        let v = Value::vector(vec![Value::Char('a'), Value::Char('b')]);
        let events = resolve_macro_events(&v).unwrap();
        assert_eq!(events.len(), 2);
    }

    #[test]
    fn test_resolve_macro_events_string() {
        let s = Value::string("hello");
        let events = resolve_macro_events(&s).unwrap();
        assert_eq!(events.len(), 5);
        match &events[0] {
            Value::Char('h') => {}
            other => panic!("Expected Char('h'), got {:?}", other),
        }
    }

    #[test]
    fn test_resolve_macro_events_list() {
        let list = Value::list(vec![Value::Char('x'), Value::Char('y')]);
        let events = resolve_macro_events(&list).unwrap();
        assert_eq!(events.len(), 2);
    }

    #[test]
    fn test_resolve_macro_events_wrong_type() {
        let result = resolve_macro_events(&Value::Int(42));
        assert!(result.is_err());
    }

    #[test]
    fn test_insert_kbd_macro() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Record and name a macro
        eval.kmacro.start_recording(false);
        eval.kmacro.store_event(Value::Char('a'));
        eval.kmacro.store_event(Value::Char('b'));
        eval.kmacro.stop_recording();
        let _ = builtin_name_last_kbd_macro(&mut eval, vec![Value::symbol("test-macro")]);

        // Insert it
        let result = builtin_insert_kbd_macro(&mut eval, vec![Value::symbol("test-macro")]);
        assert!(result.is_ok());
        let text = result.unwrap();
        let s = text.as_str().unwrap();
        assert!(s.contains("fset"));
        assert!(s.contains("test-macro"));
    }
}
