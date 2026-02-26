//! Oracle-backed Elisp parity tests.

pub(crate) mod common;
mod coverage;
mod coverage_manifest;
mod r#and;
mod arithmetic;
mod assoc;
mod assq;
mod r#catch;
mod comparison;
#[path = "condition-case.rs"]
mod condition_case;
mod equality;
mod r#get;
mod r#if;
mod r#let;
#[path = "let-star.rs"]
mod let_star;
mod list;
mod max;
mod memq;
mod min;
mod r#not;
mod r#or;
mod predicates;
mod progn;
mod put;
mod setcar;
mod setcdr;
mod setq;
mod string;
mod symbol;
mod r#throw;
mod unless;
#[path = "unwind-protect.rs"]
mod unwind_protect;
mod vector;
mod when;
