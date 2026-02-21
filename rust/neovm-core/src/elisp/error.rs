//! Error and signal types for the evaluator.

use std::error::Error;
use std::fmt::{self, Display, Formatter};

use super::value::Value;

/// Public-facing evaluation error.
#[derive(Clone, Debug)]
pub enum EvalError {
    Signal { symbol: String, data: Vec<Value> },
    UncaughtThrow { tag: Value, value: Value },
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Signal { symbol, data } => {
                write!(
                    f,
                    "signal {} {}",
                    symbol,
                    super::print::print_value(&Value::list(data.clone()))
                )
            }
            Self::UncaughtThrow { tag, value } => write!(
                f,
                "uncaught throw tag={} value={}",
                super::print::print_value(tag),
                super::print::print_value(value),
            ),
        }
    }
}

impl Error for EvalError {}

/// Internal non-local control flow.
#[derive(Clone, Debug)]
pub(crate) enum Flow {
    Signal(SignalData),
    Throw { tag: Value, value: Value },
}

#[derive(Clone, Debug)]
pub(crate) struct SignalData {
    pub symbol: String,
    pub data: Vec<Value>,
    /// Original cdr payload when a signal uses non-list data.
    pub raw_data: Option<Value>,
}

pub(crate) type EvalResult = Result<Value, Flow>;

/// Create a signal flow.
pub(crate) fn signal(symbol: &str, data: Vec<Value>) -> Flow {
    Flow::Signal(SignalData {
        symbol: symbol.to_string(),
        data,
        raw_data: None,
    })
}

/// Create a signal where DATA is used as the raw cdr payload.
///
/// This preserves dotted signal data shapes such as `(foo . 1)`.
pub(crate) fn signal_with_data(symbol: &str, data: Value) -> Flow {
    let normalized = super::value::list_to_vec(&data).unwrap_or_else(|| vec![data.clone()]);
    Flow::Signal(SignalData {
        symbol: symbol.to_string(),
        data: normalized,
        raw_data: Some(data),
    })
}

/// Convert internal flow to public EvalError.
pub(crate) fn map_flow(flow: Flow) -> EvalError {
    match flow {
        Flow::Signal(sig) => EvalError::Signal {
            symbol: sig.symbol,
            data: sig.data,
        },
        Flow::Throw { tag, value } => EvalError::UncaughtThrow { tag, value },
    }
}

/// Check if a condition-case pattern matches a signal symbol.
pub(crate) fn signal_matches(pattern: &super::expr::Expr, symbol: &str) -> bool {
    use super::expr::Expr;
    match pattern {
        Expr::Symbol(name) => name == symbol || name == "error" || name == "t",
        Expr::List(items) => items.iter().any(|item| signal_matches(item, symbol)),
        _ => false,
    }
}

/// Build the binding value for condition-case variable: (symbol . data)
pub(crate) fn make_signal_binding_value(sig: &SignalData) -> Value {
    if let Some(raw) = &sig.raw_data {
        return Value::cons(Value::symbol(sig.symbol.clone()), raw.clone());
    }
    let mut values = Vec::with_capacity(sig.data.len() + 1);
    values.push(Value::symbol(sig.symbol.clone()));
    values.extend(sig.data.clone());
    Value::list(values)
}

/// Format an eval result for the compat test harness (TSV output).
pub fn format_eval_result(result: &Result<Value, EvalError>) -> String {
    match result {
        Ok(value) => format!("OK {}", super::print::print_value(value)),
        Err(EvalError::Signal { symbol, data }) => {
            let payload = if data.is_empty() {
                "nil".to_string()
            } else {
                super::print::print_value(&Value::list(data.clone()))
            };
            format!("ERR ({} {})", symbol, payload)
        }
        Err(EvalError::UncaughtThrow { tag, value }) => {
            format!(
                "ERR (no-catch ({} {}))",
                super::print::print_value(tag),
                super::print::print_value(value),
            )
        }
    }
}

/// Render a value with evaluator-context-aware opaque handle formatting.
pub fn print_value_with_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    format_value_with_eval(eval, value)
}

fn format_opaque_handle_with_eval(eval: &super::eval::Evaluator, value: &Value) -> Option<String> {
    if let Some(handle) = super::display::print_terminal_handle(value) {
        return Some(handle);
    }
    if let Some(id) = eval.threads.thread_id_from_handle(value) {
        return Some(format!("#<thread {id}>"));
    }
    if let Some(id) = eval.threads.mutex_id_from_handle(value) {
        return Some(format!("#<mutex {id}>"));
    }
    if let Some(id) = eval.threads.condition_variable_id_from_handle(value) {
        return Some(format!("#<condvar {id}>"));
    }
    if let Value::Buffer(id) = value {
        if let Some(buf) = eval.buffers.get(*id) {
            return Some(format!("#<buffer {}>", buf.name));
        }
        if eval.buffers.dead_buffer_last_name(*id).is_some() {
            return Some("#<killed buffer>".to_string());
        }
    }
    None
}

fn format_value_with_eval(eval: &super::eval::Evaluator, value: &Value) -> String {
    if let Some(handle) = format_opaque_handle_with_eval(eval, value) {
        return handle;
    }
    match value {
        super::value::Value::Cons(_) | super::value::Value::Vector(_) => {
            format_value_with_eval_slow(eval, value)
        }
        _ => super::print::print_value(value),
    }
}

fn format_value_with_eval_slow(eval: &super::eval::Evaluator, value: &Value) -> String {
    match value {
        Value::Cons(_) => {
            if let Some(shorthand) = format_list_shorthand_with_eval(eval, value) {
                return shorthand;
            }
            let mut out = String::from("(");
            format_cons_with_eval(eval, value, &mut out);
            out.push(')');
            out
        }
        Value::Vector(vec) => {
            let mut out = String::from("[");
            let items = vec.lock().expect("poisoned");
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    out.push(' ');
                }
                out.push_str(&format_value_with_eval(eval, item));
            }
            out.push(']');
            out
        }
        _ => super::print::print_value(value),
    }
}

fn format_list_shorthand_with_eval(eval: &super::eval::Evaluator, value: &Value) -> Option<String> {
    let items = super::value::list_to_vec(value)?;
    if items.len() != 2 {
        return None;
    }

    let head = match &items[0] {
        Value::Symbol(name) => name.as_str(),
        _ => return None,
    };

    if head == "make-hash-table-from-literal" {
        let payload = quote_payload(&items[1])?;
        return Some(format!("#s{}", format_value_with_eval(eval, &payload)));
    }

    let (prefix, quoted) = match head {
        "quote" => Some(("'", &items[1])),
        "function" => Some(("#'", &items[1])),
        "\\`" => Some(("`", &items[1])),
        "\\," => Some((",", &items[1])),
        "\\,@" => Some((",@", &items[1])),
        _ => None,
    }?;

    Some(format!("{prefix}{}", format_value_with_eval(eval, quoted)))
}

fn format_cons_with_eval(eval: &super::eval::Evaluator, value: &Value, out: &mut String) {
    let mut cursor = value.clone();
    let mut first = true;
    loop {
        match cursor {
            Value::Cons(cell) => {
                if !first {
                    out.push(' ');
                }
                let pair = cell.lock().expect("poisoned");
                out.push_str(&format_value_with_eval(eval, &pair.car));
                cursor = pair.cdr.clone();
                first = false;
            }
            Value::Nil => return,
            other => {
                if !first {
                    out.push_str(" . ");
                }
                out.push_str(&format_value_with_eval(eval, &other));
                return;
            }
        }
    }
}

/// Render a value as bytes with evaluator-context-aware opaque handle formatting.
pub fn print_value_bytes_with_eval(eval: &super::eval::Evaluator, value: &Value) -> Vec<u8> {
    if let Some(handle) = format_opaque_handle_with_eval(eval, value) {
        return handle.into_bytes();
    }
    format_value_bytes_with_eval(eval, value)
}

fn format_value_bytes_with_eval(eval: &super::eval::Evaluator, value: &Value) -> Vec<u8> {
    match value {
        Value::Cons(_) => format_cons_bytes_with_eval(eval, value),
        Value::Vector(_) => format_vector_bytes_with_eval(eval, value),
        _ => super::print::print_value_bytes(value),
    }
}

fn format_cons_bytes_with_eval(eval: &super::eval::Evaluator, value: &Value) -> Vec<u8> {
    if let Some(shorthand) = format_list_shorthand_bytes_with_eval(eval, value) {
        return shorthand;
    }
    let mut out = Vec::new();
    out.push(b'(');
    append_cons_bytes_with_eval(eval, value, &mut out);
    out.push(b')');
    out
}

fn format_vector_bytes_with_eval(eval: &super::eval::Evaluator, value: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    out.push(b'[');
    let Value::Vector(items) = value else {
        return out;
    };
    let values = items.lock().expect("poisoned");
    for (idx, item) in values.iter().enumerate() {
        if idx > 0 {
            out.push(b' ');
        }
        out.extend(print_value_bytes_with_eval(eval, item));
    }
    out.push(b']');
    out
}

fn format_list_shorthand_bytes_with_eval(
    eval: &super::eval::Evaluator,
    value: &Value,
) -> Option<Vec<u8>> {
    let items = super::value::list_to_vec(value)?;
    if items.len() != 2 {
        return None;
    }

    let head = match &items[0] {
        Value::Symbol(name) => name.as_str(),
        _ => return None,
    };

    if head == "make-hash-table-from-literal" {
        let payload = quote_payload(&items[1])?;
        let mut out = Vec::new();
        out.extend_from_slice(b"#s");
        out.extend(print_value_bytes_with_eval(eval, &payload));
        return Some(out);
    }

    let (prefix, quoted) = match head {
        "quote" => Some((b"'" as &[u8], &items[1])),
        "function" => Some((b"#'" as &[u8], &items[1])),
        "\\`" => Some((b"`" as &[u8], &items[1])),
        "\\," => Some((b"," as &[u8], &items[1])),
        "\\,@" => Some((b",@" as &[u8], &items[1])),
        _ => None,
    }?;

    let mut out = Vec::new();
    out.extend_from_slice(prefix);
    out.extend(print_value_bytes_with_eval(eval, quoted));
    Some(out)
}

fn quote_payload(value: &Value) -> Option<Value> {
    let items = super::value::list_to_vec(value)?;
    if items.len() != 2 {
        return None;
    }
    match &items[0] {
        Value::Symbol(name) if name == "quote" => Some(items[1].clone()),
        _ => None,
    }
}

fn append_cons_bytes_with_eval(eval: &super::eval::Evaluator, value: &Value, out: &mut Vec<u8>) {
    let mut cursor = value.clone();
    let mut first = true;
    loop {
        match cursor {
            Value::Cons(cell) => {
                if !first {
                    out.push(b' ');
                }
                let pair = cell.lock().expect("poisoned");
                out.extend(print_value_bytes_with_eval(eval, &pair.car));
                cursor = pair.cdr.clone();
                first = false;
            }
            Value::Nil => return,
            other => {
                if !first {
                    out.extend_from_slice(b" . ");
                }
                out.extend(print_value_bytes_with_eval(eval, &other));
                return;
            }
        }
    }
}

fn print_data_payload_with_eval(eval: &super::eval::Evaluator, data: &[Value]) -> String {
    if data.is_empty() {
        "nil".to_string()
    } else {
        let parts = data
            .iter()
            .map(|v| print_value_with_eval(eval, v))
            .collect::<Vec<_>>();
        format!("({})", parts.join(" "))
    }
}

fn append_print_value_bytes_with_eval(
    eval: &super::eval::Evaluator,
    value: &Value,
    out: &mut Vec<u8>,
) {
    out.extend_from_slice(&print_value_bytes_with_eval(eval, value));
}

/// Format an eval result for harnesses that have evaluator context and need
/// opaque handle rendering for thread/mutex/condvar/terminal values.
pub fn format_eval_result_with_eval(
    eval: &super::eval::Evaluator,
    result: &Result<Value, EvalError>,
) -> String {
    match result {
        Ok(value) => format!("OK {}", print_value_with_eval(eval, value)),
        Err(EvalError::Signal { symbol, data }) => {
            let payload = print_data_payload_with_eval(eval, data);
            format!("ERR ({} {})", symbol, payload)
        }
        Err(EvalError::UncaughtThrow { tag, value }) => {
            format!(
                "ERR (no-catch ({} {}))",
                print_value_with_eval(eval, tag),
                print_value_with_eval(eval, value),
            )
        }
    }
}

/// Byte-preserving variant of `format_eval_result_with_eval`.
///
/// This preserves non-UTF-8 byte payloads in printed string literals used by
/// vm-compat corpus checks while still applying evaluator-aware opaque-handle
/// rendering for thread/mutex/condvar/terminal values.
pub fn format_eval_result_bytes_with_eval(
    eval: &super::eval::Evaluator,
    result: &Result<Value, EvalError>,
) -> Vec<u8> {
    let mut out = Vec::new();
    match result {
        Ok(value) => {
            out.extend_from_slice(b"OK ");
            append_print_value_bytes_with_eval(eval, value, &mut out);
        }
        Err(EvalError::Signal { symbol, data }) => {
            out.extend_from_slice(b"ERR (");
            out.extend_from_slice(symbol.as_bytes());
            out.push(b' ');
            if data.is_empty() {
                out.extend_from_slice(b"nil");
            } else {
                out.push(b'(');
                for (idx, item) in data.iter().enumerate() {
                    if idx > 0 {
                        out.push(b' ');
                    }
                    append_print_value_bytes_with_eval(eval, item, &mut out);
                }
                out.push(b')');
            }
            out.push(b')');
        }
        Err(EvalError::UncaughtThrow { tag, value }) => {
            out.extend_from_slice(b"ERR (no-catch (");
            append_print_value_bytes_with_eval(eval, tag, &mut out);
            out.push(b' ');
            append_print_value_bytes_with_eval(eval, value, &mut out);
            out.extend_from_slice(b"))");
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::EvalError;
    use crate::elisp::{
        parse_forms, print_value_bytes_with_eval, print_value_with_eval, Evaluator, Value,
    };

    #[test]
    fn list_prints_buffers_with_names_in_eval_context() -> Result<(), EvalError> {
        let forms = parse_forms(
            "(let ((b (generate-new-buffer \"stale-win-buf\") )
               (w (selected-window)))
  (set-window-buffer nil b)
  (kill-buffer b)
  (list (window-buffer) (window-start) (window-point)))",
        )
        .map_err(|err| EvalError::Signal {
            symbol: "parse-error".to_string(),
            data: vec![Value::string(err.to_string())],
        })?;

        let mut eval = Evaluator::new();
        let mut value = Value::Nil;
        for form in &forms {
            value = eval.eval_expr(form).expect("evaluation should succeed");
        }

        assert_eq!(
            print_value_with_eval(&eval, &value),
            "(#<buffer *scratch*> 1 1)"
        );
        assert_eq!(
            String::from_utf8(print_value_bytes_with_eval(&eval, &value)).unwrap(),
            "(#<buffer *scratch*> 1 1)"
        );

        Ok(())
    }

    #[test]
    fn eval_context_printer_renders_killed_buffer_handles() -> Result<(), EvalError> {
        let forms = parse_forms(
            "(with-temp-buffer
               (condition-case err
                   (key-binding 1 nil nil 0)
                 (error err)))",
        )
        .map_err(|err| EvalError::Signal {
            symbol: "parse-error".to_string(),
            data: vec![Value::string(err.to_string())],
        })?;

        let mut eval = Evaluator::new();
        let value = eval.eval_expr(&forms[0])?;

        assert_eq!(
            print_value_with_eval(&eval, &value),
            "(args-out-of-range #<killed buffer> 0)"
        );
        assert_eq!(
            String::from_utf8(print_value_bytes_with_eval(&eval, &value)).unwrap(),
            "(args-out-of-range #<killed buffer> 0)"
        );

        Ok(())
    }

    #[test]
    fn eval_context_printer_renders_mutex_handles_consistently() -> Result<(), EvalError> {
        let forms = parse_forms("(make-mutex \"error-printer-mutex\")").map_err(|err| {
            EvalError::Signal {
                symbol: "parse-error".to_string(),
                data: vec![Value::string(err.to_string())],
            }
        })?;

        let mut eval = Evaluator::new();
        let value = eval.eval_expr(&forms[0])?;
        let printed = print_value_with_eval(&eval, &value);

        assert!(printed.starts_with("#<mutex "));
        assert_eq!(
            String::from_utf8(print_value_bytes_with_eval(&eval, &value)).unwrap(),
            printed
        );

        Ok(())
    }

    #[test]
    fn eval_context_printer_renders_hash_s_literal_shorthand() {
        let eval = Evaluator::new();
        let literal = Value::list(vec![
            Value::symbol("make-hash-table-from-literal"),
            Value::list(vec![
                Value::symbol("quote"),
                Value::list(vec![Value::symbol("x")]),
            ]),
        ]);
        assert_eq!(print_value_with_eval(&eval, &literal), "#s(x)");
        assert_eq!(
            String::from_utf8(print_value_bytes_with_eval(&eval, &literal)).unwrap(),
            "#s(x)"
        );
    }
}
