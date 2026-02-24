//! AST (expression) types produced by the parser.

use std::error::Error;
use std::fmt::{self, Display, Formatter};

use super::intern::{resolve_sym, SymId};
#[cfg(test)]
use super::intern::intern;
use super::string_escape::format_lisp_string;

/// Parsed Lisp expression (AST node).
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Symbol(SymId),
    Keyword(SymId),
    Str(String),
    Char(char),
    List(Vec<Expr>),
    Vector(Vec<Expr>),
    /// Dotted pair `(a b . c)` — last cdr is not nil.
    DottedList(Vec<Expr>, Box<Expr>),
    /// Boolean literal — Emacs uses nil/t symbols, but we also accept #t/#f.
    Bool(bool),
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub position: usize,
    pub message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at {}: {}", self.position, self.message)
    }
}

impl Error for ParseError {}

/// Print an expression as Lisp source.
pub fn print_expr(expr: &Expr) -> String {
    match expr {
        Expr::Int(v) => v.to_string(),
        Expr::Float(v) => format_float(*v),
        Expr::Symbol(id) => format_symbol_name(resolve_sym(*id)),
        Expr::Keyword(id) => resolve_sym(*id).to_owned(),
        Expr::Str(s) => format_lisp_string(s),
        // Emacs chars are integer values, so print as codepoint.
        Expr::Char(c) => (*c as u32).to_string(),
        Expr::Bool(true) => "t".to_string(),
        Expr::Bool(false) => "nil".to_string(),
        Expr::List(items) => {
            if items.is_empty() {
                return "nil".to_string();
            }
            if items.len() == 2 {
                if let Expr::Symbol(id) = &items[0] {
                    let s = resolve_sym(*id);
                    if s == "quote" {
                        return format!("'{}", print_expr(&items[1]));
                    }
                    if s == "function" {
                        return format!("#'{}", print_expr(&items[1]));
                    }
                    if s == "\\`" {
                        return format!("`{}", print_expr(&items[1]));
                    }
                    if s == "\\," {
                        return format!(",{}", print_expr(&items[1]));
                    }
                    if s == "\\,@" {
                        return format!(",@{}", print_expr(&items[1]));
                    }
                }
            }
            let parts: Vec<String> = items.iter().map(print_expr).collect();
            format!("({})", parts.join(" "))
        }
        Expr::DottedList(items, last) => {
            let mut parts: Vec<String> = items.iter().map(print_expr).collect();
            parts.push(format!(". {}", print_expr(last)));
            format!("({})", parts.join(" "))
        }
        Expr::Vector(items) => {
            let parts: Vec<String> = items.iter().map(print_expr).collect();
            format!("[{}]", parts.join(" "))
        }
    }
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
    if f.fract() == 0.0 && f.is_finite() {
        format!("{:.1}", f)
    } else {
        format!("{}", f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_basic_exprs() {
        assert_eq!(print_expr(&Expr::Int(42)), "42");
        assert_eq!(print_expr(&Expr::Float(3.14)), "3.14");
        assert_eq!(print_expr(&Expr::Symbol(intern("foo"))), "foo");
        assert_eq!(print_expr(&Expr::Symbol(intern(".foo"))), "\\.foo");
        assert_eq!(print_expr(&Expr::Symbol(intern(""))), "##");
        assert_eq!(print_expr(&Expr::Str("hello".into())), "\"hello\"");
    }

    #[test]
    fn print_symbol_escapes_reader_sensitive_chars() {
        assert_eq!(print_expr(&Expr::Symbol(intern("a b"))), "a\\ b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a,b"))), "a\\,b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a,@b"))), "a\\,@b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a#b"))), "a\\#b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a'b"))), "a\\'b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a`b"))), "a\\`b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a\\b"))), "a\\\\b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a\"b"))), "a\\\"b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a(b"))), "a\\(b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a)b"))), "a\\)b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a[b"))), "a\\[b");
        assert_eq!(print_expr(&Expr::Symbol(intern("a]b"))), "a\\]b");
        assert_eq!(print_expr(&Expr::Symbol(intern("##"))), "\\#\\#");
        assert_eq!(print_expr(&Expr::Symbol(intern("?a"))), "\\?a");
        assert_eq!(print_expr(&Expr::Symbol(intern("a?b"))), "a?b");
    }

    #[test]
    fn print_list() {
        let expr = Expr::List(vec![Expr::Symbol(intern("+")), Expr::Int(1), Expr::Int(2)]);
        assert_eq!(print_expr(&expr), "(+ 1 2)");
    }

    #[test]
    fn print_quote_shorthand() {
        let expr = Expr::List(vec![
            Expr::Symbol(intern("quote")),
            Expr::Symbol(intern("foo")),
        ]);
        assert_eq!(print_expr(&expr), "'foo");
    }

    #[test]
    fn print_vector() {
        let expr = Expr::Vector(vec![Expr::Int(1), Expr::Int(2)]);
        assert_eq!(print_expr(&expr), "[1 2]");
    }

    #[test]
    fn print_string_keeps_non_bmp_visible() {
        assert_eq!(
            print_expr(&Expr::Str("\u{10ffff}".into())),
            "\"\u{10ffff}\""
        );
    }

    #[test]
    fn print_special_float_spellings_match_oracle_shape() {
        assert_eq!(print_expr(&Expr::Float(f64::NAN)), "0.0e+NaN");
        let neg_nan = -f64::NAN;
        assert_eq!(print_expr(&Expr::Float(neg_nan)), "-0.0e+NaN");
        assert_eq!(print_expr(&Expr::Float(f64::INFINITY)), "1.0e+INF");
        assert_eq!(print_expr(&Expr::Float(f64::NEG_INFINITY)), "-1.0e+INF");

        let tagged = f64::from_bits((0x7ffu64 << 52) | (1u64 << 51) | 1u64);
        assert_eq!(print_expr(&Expr::Float(tagged)), "1.0e+NaN");
        let neg_tagged = f64::from_bits((1u64 << 63) | (0x7ffu64 << 52) | (1u64 << 51) | 2u64);
        assert_eq!(print_expr(&Expr::Float(neg_tagged)), "-2.0e+NaN");
    }
}
