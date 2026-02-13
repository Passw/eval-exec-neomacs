//! AST (expression) types produced by the parser.

use std::error::Error;
use std::fmt::{self, Display, Formatter};

/// Parsed Lisp expression (AST node).
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Symbol(String),
    Keyword(String),
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
        Expr::Symbol(s) => s.clone(),
        Expr::Keyword(s) => s.clone(),
        Expr::Str(s) => format!("{:?}", s),
        // Emacs chars are integer values, so print as codepoint.
        Expr::Char(c) => (*c as u32).to_string(),
        Expr::Bool(true) => "t".to_string(),
        Expr::Bool(false) => "nil".to_string(),
        Expr::List(items) => {
            if items.is_empty() {
                return "nil".to_string();
            }
            if items.len() == 2 {
                if let Expr::Symbol(s) = &items[0] {
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

fn format_float(f: f64) -> String {
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
        assert_eq!(print_expr(&Expr::Symbol("foo".into())), "foo");
        assert_eq!(print_expr(&Expr::Str("hello".into())), "\"hello\"");
    }

    #[test]
    fn print_list() {
        let expr = Expr::List(vec![Expr::Symbol("+".into()), Expr::Int(1), Expr::Int(2)]);
        assert_eq!(print_expr(&expr), "(+ 1 2)");
    }

    #[test]
    fn print_quote_shorthand() {
        let expr = Expr::List(vec![
            Expr::Symbol("quote".into()),
            Expr::Symbol("foo".into()),
        ]);
        assert_eq!(print_expr(&expr), "'foo");
    }

    #[test]
    fn print_vector() {
        let expr = Expr::Vector(vec![Expr::Int(1), Expr::Int(2)]);
        assert_eq!(print_expr(&expr), "[1 2]");
    }
}
