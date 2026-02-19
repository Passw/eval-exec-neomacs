//! Value printing (Lisp representation).

use super::expr::{self, Expr};
use super::string_escape::{format_lisp_string, format_lisp_string_bytes};
use super::value::{list_to_vec, Value};

fn print_special_handle(value: &Value) -> Option<String> {
    super::display::print_terminal_handle(value)
}

/// Print a `Value` as a Lisp string.
pub fn print_value(value: &Value) -> String {
    if let Some(handle) = print_special_handle(value) {
        return handle;
    }
    match value {
        Value::Nil => "nil".to_string(),
        Value::True => "t".to_string(),
        Value::Int(v) => v.to_string(),
        Value::Float(f) => format_float(*f),
        Value::Symbol(s) => format_symbol_name(s),
        Value::Keyword(s) => s.clone(),
        Value::Str(s) => format_lisp_string(s),
        // Emacs chars are integer values, so print as codepoint.
        Value::Char(c) => (*c as u32).to_string(),
        Value::Cons(_) => {
            if let Some(shorthand) = print_list_shorthand(value) {
                return shorthand;
            }
            let mut out = String::from("(");
            print_cons(value, &mut out);
            out.push(')');
            out
        }
        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            let parts: Vec<String> = items.iter().map(print_value).collect();
            format!("[{}]", parts.join(" "))
        }
        Value::HashTable(_) => "#<hash-table>".to_string(),
        Value::Lambda(lambda) => {
            let params = format_params(&lambda.params);
            let body = lambda
                .body
                .iter()
                .map(|e| expr::print_expr(e))
                .collect::<Vec<_>>()
                .join(" ");
            if lambda.env.is_some() {
                format!("(closure {} {})", params, body)
            } else {
                format!("(lambda {} {})", params, body)
            }
        }
        Value::Macro(m) => {
            let params = format_params(&m.params);
            let body = m
                .body
                .iter()
                .map(|e| expr::print_expr(e))
                .collect::<Vec<_>>()
                .join(" ");
            format!("(macro {} {})", params, body)
        }
        Value::Subr(name) => format!("#<subr {}>", name),
        Value::ByteCode(bc) => {
            let params = format_params(&bc.params);
            format!("#<bytecode {} ({} ops)>", params, bc.ops.len())
        }
        Value::Buffer(id) => format!("#<buffer {}>", id.0),
        Value::Window(id) => format!("#<window {}>", id),
        Value::Frame(id) => format!("#<frame {}>", id),
        Value::Timer(id) => format!("#<timer {}>", id),
    }
}

/// Print a `Value` as a Lisp byte sequence.
///
/// This preserves non-UTF-8 byte payloads encoded via NeoVM string sentinels.
pub fn print_value_bytes(value: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    append_print_value_bytes(value, &mut out);
    out
}

fn append_print_value_bytes(value: &Value, out: &mut Vec<u8>) {
    if let Some(handle) = print_special_handle(value) {
        out.extend_from_slice(handle.as_bytes());
        return;
    }
    match value {
        Value::Nil => out.extend_from_slice(b"nil"),
        Value::True => out.extend_from_slice(b"t"),
        Value::Int(v) => out.extend_from_slice(v.to_string().as_bytes()),
        Value::Float(f) => out.extend_from_slice(format_float(*f).as_bytes()),
        Value::Symbol(s) => out.extend_from_slice(format_symbol_name(s).as_bytes()),
        Value::Keyword(s) => out.extend_from_slice(s.as_bytes()),
        Value::Str(s) => out.extend_from_slice(&format_lisp_string_bytes(s)),
        Value::Char(c) => out.extend_from_slice((*c as u32).to_string().as_bytes()),
        Value::Cons(_) => {
            if let Some(shorthand) = print_list_shorthand_bytes(value) {
                out.extend_from_slice(&shorthand);
                return;
            }
            out.push(b'(');
            print_cons_bytes(value, out);
            out.push(b')');
        }
        Value::Vector(v) => {
            out.push(b'[');
            let items = v.lock().expect("poisoned");
            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    out.push(b' ');
                }
                append_print_value_bytes(item, out);
            }
            out.push(b']');
        }
        Value::HashTable(_) => out.extend_from_slice(b"#<hash-table>"),
        Value::Lambda(lambda) => {
            let params = format_params(&lambda.params);
            let body = lambda
                .body
                .iter()
                .map(|e| expr::print_expr(e))
                .collect::<Vec<_>>()
                .join(" ");
            let text = if lambda.env.is_some() {
                format!("(closure {} {})", params, body)
            } else {
                format!("(lambda {} {})", params, body)
            };
            out.extend_from_slice(text.as_bytes());
        }
        Value::Macro(m) => {
            let params = format_params(&m.params);
            let body = m
                .body
                .iter()
                .map(|e| expr::print_expr(e))
                .collect::<Vec<_>>()
                .join(" ");
            out.extend_from_slice(format!("(macro {} {})", params, body).as_bytes());
        }
        Value::Subr(name) => out.extend_from_slice(format!("#<subr {}>", name).as_bytes()),
        Value::ByteCode(bc) => {
            let params = format_params(&bc.params);
            out.extend_from_slice(
                format!("#<bytecode {} ({} ops)>", params, bc.ops.len()).as_bytes(),
            );
        }
        Value::Buffer(id) => out.extend_from_slice(format!("#<buffer {}>", id.0).as_bytes()),
        Value::Window(id) => out.extend_from_slice(format!("#<window {}>", id).as_bytes()),
        Value::Frame(id) => out.extend_from_slice(format!("#<frame {}>", id).as_bytes()),
        Value::Timer(id) => out.extend_from_slice(format!("#<timer {}>", id).as_bytes()),
    }
}

/// Re-export for compatibility.
pub fn print_expr(expr: &Expr) -> String {
    expr::print_expr(expr)
}

fn format_symbol_name(name: &str) -> String {
    if name.is_empty() {
        return "##".to_string();
    }
    let mut out = String::with_capacity(name.len());
    for (idx, ch) in name.chars().enumerate() {
        let needs_escape = matches!(
            ch,
            ' ' | '\t'
                | '\n'
                | '\r'
                | '\u{0c}'
                | '('
                | ')'
                | '['
                | ']'
                | '"'
                | '\\'
                | ';'
                | '#'
                | '\''
                | '`'
                | ','
        ) || (idx == 0 && matches!(ch, '.' | '?'));
        if needs_escape {
            out.push('\\');
        }
        out.push(ch);
    }
    out
}

fn format_float(f: f64) -> String {
    const NAN_QUIET_BIT: u64 = 1u64 << 51;
    const NAN_PAYLOAD_MASK: u64 = (1u64 << 51) - 1;

    if f.is_nan() {
        let bits = f.to_bits();
        let frac = bits & ((1u64 << 52) - 1);
        if (frac & NAN_QUIET_BIT) != 0 {
            let payload = frac & NAN_PAYLOAD_MASK;
            return if f.is_sign_negative() {
                format!("-{}.0e+NaN", payload)
            } else {
                format!("{}.0e+NaN", payload)
            };
        }
        return if f.is_sign_negative() {
            "-0.0e+NaN".to_string()
        } else {
            "0.0e+NaN".to_string()
        };
    }
    if f.is_infinite() {
        return if f > 0.0 {
            "1.0e+INF".to_string()
        } else {
            "-1.0e+INF".to_string()
        };
    }
    if f.fract() == 0.0 {
        format!("{:.1}", f)
    } else {
        format!("{}", f)
    }
}

fn format_params(params: &super::value::LambdaParams) -> String {
    let mut parts = Vec::new();
    for p in &params.required {
        parts.push(p.clone());
    }
    if !params.optional.is_empty() {
        parts.push("&optional".to_string());
        for p in &params.optional {
            parts.push(p.clone());
        }
    }
    if let Some(ref rest) = params.rest {
        parts.push("&rest".to_string());
        parts.push(rest.clone());
    }
    if parts.is_empty() {
        "nil".to_string()
    } else {
        format!("({})", parts.join(" "))
    }
}

fn print_list_shorthand(value: &Value) -> Option<String> {
    let items = list_to_vec(value)?;
    if items.len() != 2 {
        return None;
    }

    let head = match &items[0] {
        Value::Symbol(name) => name.as_str(),
        _ => return None,
    };

    if head == "make-hash-table-from-literal" {
        if let Some(payload) = quote_payload(&items[1]) {
            return Some(format!("#s{}", print_value(&payload)));
        }
        return None;
    }

    let prefix = match head {
        "quote" => "'",
        "function" => "#'",
        "\\`" => "`",
        "\\," => ",",
        "\\,@" => ",@",
        _ => return None,
    };

    Some(format!("{prefix}{}", print_value(&items[1])))
}

fn print_list_shorthand_bytes(value: &Value) -> Option<Vec<u8>> {
    let items = list_to_vec(value)?;
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
        append_print_value_bytes(&payload, &mut out);
        return Some(out);
    }

    let prefix: &[u8] = match head {
        "quote" => b"'",
        "function" => b"#'",
        "\\`" => b"`",
        "\\," => b",",
        "\\,@" => b",@",
        _ => return None,
    };

    let mut out = Vec::new();
    out.extend_from_slice(prefix);
    append_print_value_bytes(&items[1], &mut out);
    Some(out)
}

fn quote_payload(value: &Value) -> Option<Value> {
    let items = list_to_vec(value)?;
    if items.len() != 2 {
        return None;
    }
    match &items[0] {
        Value::Symbol(name) if name == "quote" => Some(items[1].clone()),
        _ => None,
    }
}

fn print_cons(value: &Value, out: &mut String) {
    let mut cursor = value.clone();
    let mut first = true;
    loop {
        match cursor {
            Value::Cons(cell) => {
                if !first {
                    out.push(' ');
                }
                let pair = cell.lock().expect("poisoned");
                out.push_str(&print_value(&pair.car));
                cursor = pair.cdr.clone();
                first = false;
            }
            Value::Nil => return,
            other => {
                if !first {
                    out.push_str(" . ");
                }
                out.push_str(&print_value(&other));
                return;
            }
        }
    }
}

fn print_cons_bytes(value: &Value, out: &mut Vec<u8>) {
    let mut cursor = value.clone();
    let mut first = true;
    loop {
        match cursor {
            Value::Cons(cell) => {
                if !first {
                    out.push(b' ');
                }
                let pair = cell.lock().expect("poisoned");
                append_print_value_bytes(&pair.car, out);
                cursor = pair.cdr.clone();
                first = false;
            }
            Value::Nil => return,
            other => {
                if !first {
                    out.extend_from_slice(b" . ");
                }
                append_print_value_bytes(&other, out);
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::value::{LambdaData, LambdaParams};
    use std::sync::Arc;

    #[test]
    fn print_basic_values() {
        assert_eq!(print_value(&Value::Nil), "nil");
        assert_eq!(print_value(&Value::True), "t");
        assert_eq!(print_value(&Value::Int(42)), "42");
        assert_eq!(print_value(&Value::Float(3.14)), "3.14");
        assert_eq!(print_value(&Value::Float(1.0)), "1.0");
        assert_eq!(print_value(&Value::symbol("foo")), "foo");
        assert_eq!(print_value(&Value::symbol(".foo")), "\\.foo");
        assert_eq!(print_value(&Value::symbol("")), "##");
        assert_eq!(print_value(&Value::keyword(":bar")), ":bar");
    }

    #[test]
    fn print_symbol_escapes_reader_sensitive_chars() {
        assert_eq!(print_value(&Value::symbol("a b")), "a\\ b");
        assert_eq!(print_value(&Value::symbol("a,b")), "a\\,b");
        assert_eq!(print_value(&Value::symbol("a,@b")), "a\\,@b");
        assert_eq!(print_value(&Value::symbol("a#b")), "a\\#b");
        assert_eq!(print_value(&Value::symbol("a'b")), "a\\'b");
        assert_eq!(print_value(&Value::symbol("a`b")), "a\\`b");
        assert_eq!(print_value(&Value::symbol("a\\b")), "a\\\\b");
        assert_eq!(print_value(&Value::symbol("a\"b")), "a\\\"b");
        assert_eq!(print_value(&Value::symbol("a(b")), "a\\(b");
        assert_eq!(print_value(&Value::symbol("a)b")), "a\\)b");
        assert_eq!(print_value(&Value::symbol("a[b")), "a\\[b");
        assert_eq!(print_value(&Value::symbol("a]b")), "a\\]b");
        assert_eq!(print_value(&Value::symbol("##")), "\\#\\#");
        assert_eq!(print_value(&Value::symbol("?a")), "\\?a");
        assert_eq!(print_value(&Value::symbol("a?b")), "a?b");
    }

    #[test]
    fn print_float_nan_preserves_sign() {
        assert_eq!(print_value(&Value::Float(f64::NAN)), "0.0e+NaN");
        let neg_nan = f64::from_bits(f64::NAN.to_bits() | (1_u64 << 63));
        assert_eq!(print_value(&Value::Float(neg_nan)), "-0.0e+NaN");
    }

    #[test]
    fn print_float_nan_payload_tag_round_trip_shape() {
        let tagged = f64::from_bits((0x7ffu64 << 52) | (1u64 << 51) | 1u64);
        assert_eq!(print_value(&Value::Float(tagged)), "1.0e+NaN");

        let neg_tagged = f64::from_bits((1u64 << 63) | (0x7ffu64 << 52) | (1u64 << 51) | 2u64);
        assert_eq!(print_value(&Value::Float(neg_tagged)), "-2.0e+NaN");
    }

    #[test]
    fn print_string() {
        assert_eq!(print_value(&Value::string("hello")), "\"hello\"");
    }

    #[test]
    fn print_string_keeps_non_bmp_visible() {
        assert_eq!(print_value(&Value::string("\u{10ffff}")), "\"\u{10ffff}\"");
    }

    #[test]
    fn print_string_bytes_preserve_non_utf8_payloads() {
        let raw = char::from_u32(0xE0FF).expect("raw-byte sentinel");
        assert_eq!(
            print_value_bytes(&Value::string(raw.to_string())),
            b"\"\\377\""
        );
    }

    #[test]
    fn print_list() {
        let lst = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(print_value(&lst), "(1 2 3)");
    }

    #[test]
    fn print_hash_s_literal_shorthand() {
        let literal = Value::list(vec![
            Value::symbol("make-hash-table-from-literal"),
            Value::list(vec![
                Value::symbol("quote"),
                Value::list(vec![Value::symbol("x")]),
            ]),
        ]);
        assert_eq!(print_value(&literal), "#s(x)");
        assert_eq!(print_value_bytes(&literal), b"#s(x)");
    }

    #[test]
    fn print_quote_shorthand_lists() {
        let quoted = Value::list(vec![Value::symbol("quote"), Value::symbol("foo")]);
        let function = Value::list(vec![Value::symbol("function"), Value::symbol("car")]);
        let quasiquoted = Value::list(vec![
            Value::symbol("\\`"),
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
        ]);
        let unquoted = Value::list(vec![Value::symbol("\\,"), Value::symbol("x")]);
        let unquote_splice = Value::list(vec![Value::symbol("\\,@"), Value::symbol("xs")]);

        assert_eq!(print_value(&quoted), "'foo");
        assert_eq!(print_value(&function), "#'car");
        assert_eq!(print_value(&quasiquoted), "`(a b)");
        assert_eq!(print_value(&unquoted), ",x");
        assert_eq!(print_value(&unquote_splice), ",@xs");
    }

    #[test]
    fn print_dotted_pair() {
        let pair = Value::cons(Value::Int(1), Value::Int(2));
        assert_eq!(print_value(&pair), "(1 . 2)");
    }

    #[test]
    fn print_vector() {
        let v = Value::vector(vec![Value::Int(1), Value::Int(2)]);
        assert_eq!(print_value(&v), "[1 2]");
    }

    #[test]
    fn print_lambda() {
        let lam = Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["x".into(), "y".into()]),
            body: vec![Expr::List(vec![
                Expr::Symbol("+".into()),
                Expr::Symbol("x".into()),
                Expr::Symbol("y".into()),
            ])],
            env: None,
            docstring: None,
        }));
        assert_eq!(print_value(&lam), "(lambda (x y) (+ x y))");
    }

    #[test]
    fn print_terminal_handle_special_form() {
        let list = super::super::display::builtin_terminal_list(vec![]).unwrap();
        let items = list_to_vec(&list).expect("terminal-list should return a list");
        let handle = items
            .first()
            .expect("terminal-list should contain one handle");

        let printed = print_value(handle);
        assert!(printed.starts_with("#<terminal "));
        assert!(printed.contains("on initial_terminal>"));
    }
}
