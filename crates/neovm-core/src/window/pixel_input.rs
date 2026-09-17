//! Owned dependencies of pixel expressions, not an evaluator or a Lisp clone.
//!
//! Follows the arithmetic/number forms consumed by display_pixel_calc and GNU
//! xdisp.c's calc_pixel_width_or_height. Image/resource operands remain opaque;
//! resource-content revisions belong to their own dependency contract.

use crate::emacs_core::value::Value;
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum PixelInput {
    Atom(usize),
    Expression(Arc<[Token]>),
}

/// Prefix encoding, with explicit ends for variadic arithmetic. Back references
/// make shared subexpressions and cycles finite without recursive Rust stacks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum Token {
    Atom(usize),
    Sum,
    Difference,
    AbsolutePixels(usize),
    Scale(usize),
    End,
    Reference(usize),
    Opaque(usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Role {
    Expression,
    Arguments,
}

impl PixelInput {
    pub(super) fn capture(value: Value) -> Self {
        if !value.is_cons() {
            return Self::Atom(value.bits());
        }
        let mut tokens = Vec::new();
        let mut pending = vec![(value, Role::Expression)];
        let mut seen = FxHashMap::default();
        while let Some((value, role)) = pending.pop() {
            if !value.is_cons() {
                tokens.push(match role {
                    Role::Expression => Token::Atom(value.bits()),
                    // Arithmetic ignores a non-cons tail, including dotted tails.
                    Role::Arguments => Token::End,
                });
                continue;
            }
            let next = tokens.len();
            if let Some(previous) = seen.insert((value.bits(), role), next) {
                tokens.push(Token::Reference(previous));
                continue;
            }
            let car = value.cons_car();
            let cdr = value.cons_cdr();
            match role {
                Role::Arguments => {
                    pending.push((cdr, Role::Arguments));
                    pending.push((car, Role::Expression));
                }
                Role::Expression if car.is_symbol_named("+") || car.is_symbol_named("-") => {
                    tokens.push(if car.is_symbol_named("+") {
                        Token::Sum
                    } else {
                        Token::Difference
                    });
                    pending.push((cdr, Role::Arguments));
                }
                Role::Expression if car.as_int().is_some() || car.as_float().is_some() => {
                    if cdr.is_nil() {
                        tokens.push(Token::AbsolutePixels(car.bits()));
                    } else {
                        tokens.push(Token::Scale(car.bits()));
                        pending.push((cdr, Role::Expression));
                    }
                }
                Role::Expression => tokens.push(Token::Opaque(value.bits())),
            }
        }
        Self::Expression(tokens.into())
    }
}
