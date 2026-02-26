//! Oracle-backed Elisp parity tests.

pub(crate) mod common;
mod coverage;
mod coverage_manifest;
mod r#and;
mod arithmetic;
mod assoc;
mod assq;
#[path = "beginning-of-line.rs"]
mod beginning_of_line;
mod r#catch;
#[path = "car-safe.rs"]
mod car_safe;
#[path = "char-after.rs"]
mod char_after;
mod comparison;
#[path = "condition-case.rs"]
mod condition_case;
mod equality;
mod format;
#[path = "forward-line.rs"]
mod forward_line;
mod r#get;
#[path = "goto-char.rs"]
mod goto_char;
mod r#if;
mod insert;
mod r#let;
#[path = "let-star.rs"]
mod let_star;
mod list;
#[path = "match-beginning.rs"]
mod match_beginning;
#[path = "match-end.rs"]
mod match_end;
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
#[path = "re-search-forward.rs"]
mod re_search_forward;
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
