//! Oracle-backed Elisp parity tests.

pub(crate) mod common;
mod coverage;
mod coverage_manifest;
mod r#and;
mod arithmetic;
mod assoc;
mod assq;
mod r#catch;
#[path = "car-safe.rs"]
mod car_safe;
mod comparison;
#[path = "condition-case.rs"]
mod condition_case;
mod equality;
mod format;
mod r#get;
#[path = "goto-char.rs"]
mod goto_char;
mod r#if;
mod insert;
mod r#let;
#[path = "let-star.rs"]
mod let_star;
mod list;
mod max;
mod memq;
mod member;
mod min;
mod nreverse;
mod r#not;
mod r#or;
mod point;
#[path = "point-min.rs"]
mod point_min;
#[path = "point-max.rs"]
mod point_max;
mod predicates;
mod progn;
mod put;
mod setcar;
mod setcdr;
mod setq;
mod string;
#[path = "string-match.rs"]
mod string_match;
#[path = "string-to-number.rs"]
mod string_to_number;
mod symbol;
mod r#throw;
mod unless;
#[path = "unwind-protect.rs"]
mod unwind_protect;
mod vector;
mod when;
