//! Register system -- quick storage and retrieval of text, positions, etc.
//!
//! Provides Emacs-compatible register functionality:
//! - `copy-to-register` -- store text in a register
//! - `insert-register` -- insert text from a register
//! - `point-to-register` -- store current position in a register
//! - `jump-to-register` -- jump to a stored position
//! - `number-to-register` -- store a number in a register
//! - `increment-register` -- increment a number in a register
//! - `view-register` -- describe a register's contents
//! - `list-registers` -- list all non-empty registers

use std::collections::HashMap;

use super::error::{signal, EvalResult, Flow};
use super::intern::resolve_sym;
use super::value::Value;
use crate::gc::GcTrace;

// ---------------------------------------------------------------------------
// Register content types
// ---------------------------------------------------------------------------

/// The different kinds of data that can be stored in a register.
#[derive(Clone, Debug)]
pub enum RegisterContent {
    /// Plain text string.
    Text(String),
    /// An integer value.
    Number(i64),
    /// A saved buffer position: (buffer name, point offset).
    Position { buffer: String, point: usize },
    /// A rectangle (list of strings, one per line).
    Rectangle(Vec<String>),
    /// A saved window/frame configuration (opaque Lisp value).
    FrameConfig(Value),
    /// A file name (for `set-register` with file references).
    File(String),
    /// A keyboard macro (sequence of key events).
    KbdMacro(Vec<Value>),
}

impl RegisterContent {
    /// Return a short human-readable description of the content kind.
    fn description(&self) -> &str {
        match self {
            RegisterContent::Text(_) => "text",
            RegisterContent::Number(_) => "number",
            RegisterContent::Position { .. } => "position",
            RegisterContent::Rectangle(_) => "rectangle",
            RegisterContent::FrameConfig(_) => "frame-config",
            RegisterContent::File(_) => "file",
            RegisterContent::KbdMacro(_) => "kbd-macro",
        }
    }
}

// ---------------------------------------------------------------------------
// RegisterManager
// ---------------------------------------------------------------------------

/// Central registry for all registers.
#[derive(Clone, Debug)]
pub struct RegisterManager {
    registers: HashMap<char, RegisterContent>,
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self::new()
    }
}

impl RegisterManager {
    /// Create a new empty register manager.
    pub fn new() -> Self {
        Self {
            registers: HashMap::new(),
        }
    }

    /// Store content in a register, replacing any previous content.
    pub fn set(&mut self, register: char, content: RegisterContent) {
        self.registers.insert(register, content);
    }

    /// Retrieve the content of a register, if any.
    pub fn get(&self, register: char) -> Option<&RegisterContent> {
        self.registers.get(&register)
    }

    /// Clear a single register.
    pub fn clear(&mut self, register: char) {
        self.registers.remove(&register);
    }

    /// Clear all registers.
    pub fn clear_all(&mut self) {
        self.registers.clear();
    }

    /// Return a sorted list of (register-char, description) pairs for all
    /// non-empty registers.
    pub fn list(&self) -> Vec<(char, &str)> {
        let mut entries: Vec<(char, &str)> = self
            .registers
            .iter()
            .map(|(&ch, content)| (ch, content.description()))
            .collect();
        entries.sort_by_key(|(ch, _)| *ch);
        entries
    }

    /// Convenience: get the text stored in a register, if it holds text.
    pub fn get_text(&self, register: char) -> Option<&str> {
        match self.registers.get(&register) {
            Some(RegisterContent::Text(s)) => Some(s.as_str()),
            _ => None,
        }
    }

    /// Append (or prepend) text to a register that already holds text.
    /// If the register is empty or not text, it becomes a Text register
    /// containing just the new text.
    pub fn append_text(&mut self, register: char, text: &str, prepend: bool) {
        match self.registers.get_mut(&register) {
            Some(RegisterContent::Text(existing)) => {
                if prepend {
                    let mut new = String::with_capacity(text.len() + existing.len());
                    new.push_str(text);
                    new.push_str(existing);
                    *existing = new;
                } else {
                    existing.push_str(text);
                }
            }
            _ => {
                self.registers
                    .insert(register, RegisterContent::Text(text.to_string()));
            }
        }
    }
}

impl GcTrace for RegisterManager {
    fn trace_roots(&self, roots: &mut Vec<Value>) {
        for content in self.registers.values() {
            match content {
                RegisterContent::FrameConfig(v) => {
                    roots.push(*v);
                }
                RegisterContent::KbdMacro(keys) => {
                    for v in keys {
                        roots.push(*v);
                    }
                }
                _ => {}
            }
        }
    }
}

// ===========================================================================
// Builtin helpers
// ===========================================================================

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

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(_) => Ok(value.as_str().unwrap().to_string()),
        Value::Symbol(id) => Ok(resolve_sym(*id).to_owned()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), *other],
        )),
    }
}

fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), *other],
        )),
    }
}

/// Extract a register character from a first argument.
/// Accepts a Char directly, or an Int (treated as ASCII code), or a
/// single-character string.
fn expect_register(value: &Value) -> Result<char, Flow> {
    match value {
        Value::Char(c) => Ok(*c),
        Value::Int(n) => {
            if *n >= 0 && *n <= 0x10FFFF {
                if let Some(c) = char::from_u32(*n as u32) {
                    return Ok(c);
                }
            }
            Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), *value],
            ))
        }
        Value::Str(_) => {
            let st = value.as_str().unwrap();
            let mut chars = st.chars();
            match (chars.next(), chars.next()) {
                (Some(c), None) => Ok(c),
                _ => Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("characterp"), *value],
                )),
            }
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), *other],
        )),
    }
}

// ===========================================================================
// Builtins (evaluator-dependent)
// ===========================================================================

/// (copy-to-register REGISTER START END &optional DELETE-FLAG) -> nil
///
/// In the VM we don't have buffer positions, so START and END are
/// interpreted as a text string to store (the caller passes the
/// extracted region text as a string in arg index 1).
/// Simplified: (copy-to-register REGISTER TEXT) -> nil
pub(crate) fn builtin_copy_to_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("copy-to-register", &args, 2)?;
    expect_max_args("copy-to-register", &args, 5)?;
    let reg = expect_register(&args[0])?;
    let text = expect_string(&args[1])?;
    eval.registers.set(reg, RegisterContent::Text(text));
    Ok(Value::Nil)
}

/// (insert-register REGISTER &optional NOT-KILL) -> nil
///
/// Returns the text stored in the register as a string (for the caller
/// to insert into the buffer).  Signals an error if the register is empty
/// or does not hold text.
pub(crate) fn builtin_insert_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("insert-register", &args, 1)?;
    expect_max_args("insert-register", &args, 2)?;
    let reg = expect_register(&args[0])?;
    match eval.registers.get(reg) {
        Some(RegisterContent::Text(s)) => Ok(Value::string(s.clone())),
        Some(RegisterContent::Number(n)) => Ok(Value::string(n.to_string())),
        Some(RegisterContent::Rectangle(lines)) => Ok(Value::string(lines.join("\n"))),
        Some(_) => Err(signal(
            "error",
            vec![Value::string(format!(
                "Register does not contain text: {}",
                reg
            ))],
        )),
        None => Err(signal(
            "error",
            vec![Value::string(format!("Register '{}' is empty", reg))],
        )),
    }
}

/// (point-to-register REGISTER) -> nil
///
/// Store the current buffer name and point in the register.
pub(crate) fn builtin_point_to_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("point-to-register", &args, 1)?;
    let reg = expect_register(&args[0])?;
    let buffer_name = eval
        .buffers
        .current_buffer()
        .map(|b| b.name.clone())
        .unwrap_or_else(|| "*scratch*".to_string());
    let point = eval
        .buffers
        .current_buffer()
        .map(|b| b.point())
        .unwrap_or(1);
    eval.registers.set(
        reg,
        RegisterContent::Position {
            buffer: buffer_name,
            point,
        },
    );
    Ok(Value::Nil)
}

/// (number-to-register NUMBER REGISTER) -> nil
pub(crate) fn builtin_number_to_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("number-to-register", &args, 2)?;
    let num = expect_int(&args[0])?;
    let reg = expect_register(&args[1])?;
    eval.registers.set(reg, RegisterContent::Number(num));
    Ok(Value::Nil)
}

/// (increment-register NUMBER REGISTER) -> nil
///
/// If the register holds a number, add NUMBER to it.
/// If it holds text, append the printed number.
/// Otherwise signal an error.
pub(crate) fn builtin_increment_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("increment-register", &args, 2)?;
    let inc = expect_int(&args[0])?;
    let reg = expect_register(&args[1])?;
    match eval.registers.get(reg).cloned() {
        Some(RegisterContent::Number(n)) => {
            eval.registers.set(reg, RegisterContent::Number(n + inc));
            Ok(Value::Nil)
        }
        Some(RegisterContent::Text(mut s)) => {
            s.push_str(&inc.to_string());
            eval.registers.set(reg, RegisterContent::Text(s));
            Ok(Value::Nil)
        }
        Some(_) => Err(signal(
            "error",
            vec![Value::string(format!(
                "Register does not contain a number or text: {}",
                reg
            ))],
        )),
        None => {
            // Empty register: treat as number starting from 0
            eval.registers.set(reg, RegisterContent::Number(inc));
            Ok(Value::Nil)
        }
    }
}

/// (view-register REGISTER) -> string
///
/// Return a human-readable description of the register's contents.
pub(crate) fn builtin_view_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("view-register", &args, 1)?;
    let reg = expect_register(&args[0])?;
    match eval.registers.get(reg) {
        Some(RegisterContent::Text(s)) => {
            let desc = if s.len() > 60 {
                format!("Register {} contains text: {}...", reg, &s[..60])
            } else {
                format!("Register {} contains text: {}", reg, s)
            };
            Ok(Value::string(desc))
        }
        Some(RegisterContent::Number(n)) => Ok(Value::string(format!(
            "Register {} contains the number {}",
            reg, n
        ))),
        Some(RegisterContent::Position { buffer, point }) => Ok(Value::string(format!(
            "Register {} contains a position: buffer={} point={}",
            reg, buffer, point
        ))),
        Some(RegisterContent::Rectangle(lines)) => Ok(Value::string(format!(
            "Register {} contains a rectangle ({} lines)",
            reg,
            lines.len()
        ))),
        Some(RegisterContent::FrameConfig(_)) => Ok(Value::string(format!(
            "Register {} contains a frame configuration",
            reg
        ))),
        Some(RegisterContent::File(f)) => Ok(Value::string(format!(
            "Register {} contains file: {}",
            reg, f
        ))),
        Some(RegisterContent::KbdMacro(keys)) => Ok(Value::string(format!(
            "Register {} contains a keyboard macro ({} keys)",
            reg,
            keys.len()
        ))),
        None => Ok(Value::string(format!("Register {} is empty", reg))),
    }
}

/// (get-register REGISTER) -> value or nil
///
/// Return the content of a register as a Lisp value.
/// Text -> string, Number -> integer, Position -> list, otherwise nil.
pub(crate) fn builtin_get_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("get-register", &args, 1)?;
    let reg = expect_register(&args[0])?;
    match eval.registers.get(reg) {
        Some(RegisterContent::Text(s)) => Ok(Value::string(s.clone())),
        Some(RegisterContent::Number(n)) => Ok(Value::Int(*n)),
        Some(RegisterContent::Position { buffer, point }) => Ok(Value::cons(
            Value::string(buffer.clone()),
            Value::Int(*point as i64),
        )),
        Some(RegisterContent::Rectangle(lines)) => {
            let vals: Vec<Value> = lines.iter().map(|l| Value::string(l.clone())).collect();
            Ok(Value::list(vals))
        }
        Some(RegisterContent::File(f)) => Ok(Value::string(f.clone())),
        Some(RegisterContent::FrameConfig(v)) => Ok(*v),
        Some(RegisterContent::KbdMacro(keys)) => Ok(Value::list(keys.clone())),
        None => Ok(Value::Nil),
    }
}

/// (register-to-string REGISTER) -> string or nil
///
/// Return textual content from REGISTER when available.
#[cfg(test)]
pub(crate) fn builtin_register_to_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("register-to-string", &args, 1)?;
    let reg = expect_register(&args[0])?;
    match eval.registers.get(reg) {
        Some(RegisterContent::Text(s)) => Ok(Value::string(s.clone())),
        Some(RegisterContent::Rectangle(lines)) => Ok(Value::string(lines.join("\n"))),
        _ => Ok(Value::Nil),
    }
}

/// (set-register REGISTER VALUE) -> nil
///
/// Low-level: store an arbitrary Lisp value.  Strings become Text,
/// integers become Number, otherwise stored as FrameConfig (opaque).
pub(crate) fn builtin_set_register(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-register", &args, 2)?;
    let reg = expect_register(&args[0])?;
    let content = match &args[1] {
        Value::Str(_) => RegisterContent::Text(args[1].as_str().unwrap().to_string()),
        Value::Int(n) => RegisterContent::Number(*n),
        Value::Nil => {
            eval.registers.clear(reg);
            return Ok(Value::Nil);
        }
        other => RegisterContent::FrameConfig(*other),
    };
    eval.registers.set(reg, content);
    Ok(Value::Nil)
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // RegisterManager unit tests
    // -----------------------------------------------------------------------

    #[test]
    fn set_get_clear() {
        let mut mgr = RegisterManager::new();

        // Initially empty
        assert!(mgr.get('a').is_none());

        // Set text
        mgr.set('a', RegisterContent::Text("hello".to_string()));
        assert!(mgr.get('a').is_some());
        assert_eq!(mgr.get_text('a'), Some("hello"));

        // Overwrite
        mgr.set('a', RegisterContent::Number(42));
        assert!(mgr.get_text('a').is_none());
        match mgr.get('a') {
            Some(RegisterContent::Number(42)) => {}
            other => panic!("Expected Number(42), got {:?}", other),
        }

        // Clear
        mgr.clear('a');
        assert!(mgr.get('a').is_none());
    }

    #[test]
    fn clear_all() {
        let mut mgr = RegisterManager::new();
        mgr.set('a', RegisterContent::Text("one".to_string()));
        mgr.set('b', RegisterContent::Text("two".to_string()));
        mgr.set('c', RegisterContent::Number(3));

        assert_eq!(mgr.list().len(), 3);
        mgr.clear_all();
        assert_eq!(mgr.list().len(), 0);
    }

    #[test]
    fn text_append_and_prepend() {
        let mut mgr = RegisterManager::new();

        // Append to empty register creates text
        mgr.append_text('x', "hello", false);
        assert_eq!(mgr.get_text('x'), Some("hello"));

        // Append
        mgr.append_text('x', " world", false);
        assert_eq!(mgr.get_text('x'), Some("hello world"));

        // Prepend
        mgr.append_text('x', ">> ", true);
        assert_eq!(mgr.get_text('x'), Some(">> hello world"));
    }

    #[test]
    fn append_to_non_text_replaces() {
        let mut mgr = RegisterManager::new();
        mgr.set('n', RegisterContent::Number(99));
        mgr.append_text('n', "new text", false);
        assert_eq!(mgr.get_text('n'), Some("new text"));
    }

    #[test]
    fn position_storage() {
        let mut mgr = RegisterManager::new();
        mgr.set(
            'p',
            RegisterContent::Position {
                buffer: "*scratch*".to_string(),
                point: 42,
            },
        );
        match mgr.get('p') {
            Some(RegisterContent::Position { buffer, point }) => {
                assert_eq!(buffer, "*scratch*");
                assert_eq!(*point, 42);
            }
            other => panic!("Expected Position, got {:?}", other),
        }
    }

    #[test]
    fn list_registers_sorted() {
        let mut mgr = RegisterManager::new();
        mgr.set('z', RegisterContent::Text("z-text".to_string()));
        mgr.set('a', RegisterContent::Number(1));
        mgr.set('m', RegisterContent::File("/tmp/foo".to_string()));

        let list = mgr.list();
        assert_eq!(list.len(), 3);
        assert_eq!(list[0].0, 'a');
        assert_eq!(list[0].1, "number");
        assert_eq!(list[1].0, 'm');
        assert_eq!(list[1].1, "file");
        assert_eq!(list[2].0, 'z');
        assert_eq!(list[2].1, "text");
    }

    #[test]
    fn rectangle_and_kbd_macro() {
        let mut mgr = RegisterManager::new();

        let rect = vec![
            "line1".to_string(),
            "line2".to_string(),
            "line3".to_string(),
        ];
        mgr.set('r', RegisterContent::Rectangle(rect));
        match mgr.get('r') {
            Some(RegisterContent::Rectangle(lines)) => assert_eq!(lines.len(), 3),
            other => panic!("Expected Rectangle, got {:?}", other),
        }

        let macro_keys = vec![Value::Char('a'), Value::Char('b')];
        mgr.set('k', RegisterContent::KbdMacro(macro_keys));
        match mgr.get('k') {
            Some(RegisterContent::KbdMacro(keys)) => assert_eq!(keys.len(), 2),
            other => panic!("Expected KbdMacro, got {:?}", other),
        }
    }

    // -----------------------------------------------------------------------
    // Builtin-level tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_expect_register() {
        // Char
        assert_eq!(expect_register(&Value::Char('a')).unwrap(), 'a');

        // Int (ASCII code)
        assert_eq!(expect_register(&Value::Int(65)).unwrap(), 'A');

        // Single-char string
        assert_eq!(expect_register(&Value::string("z")).unwrap(), 'z');

        // Multi-char string is an error
        assert!(expect_register(&Value::string("ab")).is_err());

        // Float is an error
        assert!(expect_register(&Value::Float(1.0)).is_err());
    }

    #[test]
    fn test_builtin_copy_and_insert() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // copy-to-register
        let result = builtin_copy_to_register(
            &mut eval,
            vec![Value::Char('a'), Value::string("hello world")],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());

        // insert-register -> returns the text
        let result = builtin_insert_register(&mut eval, vec![Value::Char('a')]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello world"));

        // insert-register on empty register -> error
        let result = builtin_insert_register(&mut eval, vec![Value::Char('z')]);
        assert!(result.is_err());
    }

    #[test]
    fn test_builtin_number_and_increment() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // number-to-register
        let result = builtin_number_to_register(&mut eval, vec![Value::Int(10), Value::Char('n')]);
        assert!(result.is_ok());

        // get-register -> returns 10
        let result = builtin_get_register(&mut eval, vec![Value::Char('n')]);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(10)));

        // increment-register by 5
        let result = builtin_increment_register(&mut eval, vec![Value::Int(5), Value::Char('n')]);
        assert!(result.is_ok());

        // Now should be 15
        let result = builtin_get_register(&mut eval, vec![Value::Char('n')]);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(15)));
    }

    #[test]
    fn test_builtin_increment_empty_register() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Incrementing empty register starts from 0
        let result = builtin_increment_register(&mut eval, vec![Value::Int(7), Value::Char('e')]);
        assert!(result.is_ok());

        let result = builtin_get_register(&mut eval, vec![Value::Char('e')]);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap(), Value::Int(7)));
    }

    #[test]
    fn test_builtin_set_and_get_register() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Set string
        let result = builtin_set_register(
            &mut eval,
            vec![Value::Char('s'), Value::string("saved text")],
        );
        assert!(result.is_ok());

        let result = builtin_get_register(&mut eval, vec![Value::Char('s')]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("saved text"));

        // Set nil clears
        let result = builtin_set_register(&mut eval, vec![Value::Char('s'), Value::Nil]);
        assert!(result.is_ok());

        let result = builtin_get_register(&mut eval, vec![Value::Char('s')]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn test_builtin_view_register() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Empty register
        let result = builtin_view_register(&mut eval, vec![Value::Char('v')]);
        assert!(result.is_ok());
        let desc = result.unwrap();
        assert!(desc.as_str().unwrap().contains("empty"));

        // Text register
        eval.registers
            .set('v', RegisterContent::Text("some text".to_string()));
        let result = builtin_view_register(&mut eval, vec![Value::Char('v')]);
        assert!(result.is_ok());
        let desc = result.unwrap();
        assert!(desc.as_str().unwrap().contains("text"));
        assert!(desc.as_str().unwrap().contains("some text"));

        // Number register
        eval.registers.set('v', RegisterContent::Number(99));
        let result = builtin_view_register(&mut eval, vec![Value::Char('v')]);
        assert!(result.is_ok());
        let desc = result.unwrap();
        assert!(desc.as_str().unwrap().contains("99"));
    }

    #[test]
    fn test_builtin_register_to_string() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // Empty register => nil
        let empty = builtin_register_to_string(&mut eval, vec![Value::Char('r')]).unwrap();
        assert!(empty.is_nil());

        // Text register => string
        builtin_set_register(&mut eval, vec![Value::Char('r'), Value::string("abc")]).unwrap();
        let text = builtin_register_to_string(&mut eval, vec![Value::Char('r')]).unwrap();
        assert_eq!(text.as_str(), Some("abc"));
    }

    #[test]
    fn test_wrong_arg_count() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // copy-to-register needs at least 2 args
        let result = builtin_copy_to_register(&mut eval, vec![Value::Char('a')]);
        assert!(result.is_err());

        // point-to-register needs exactly 1 arg
        let result = builtin_point_to_register(&mut eval, vec![]);
        assert!(result.is_err());
    }
}
