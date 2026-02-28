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
#[path = "buffer-name.rs"]
mod buffer_name;
#[path = "buffer-string.rs"]
mod buffer_string;
#[path = "buffer-substring.rs"]
mod buffer_substring;
#[path = "car-safe.rs"]
mod car_safe;
mod r#catch;
#[path = "char-after.rs"]
mod char_after;
#[path = "compare-strings.rs"]
mod compare_strings;
mod comparison;
mod cond;
#[path = "condition-case.rs"]
mod condition_case;
#[path = "copy-alist.rs"]
mod copy_alist;
#[path = "current-buffer.rs"]
mod current_buffer;
mod defvar;
#[path = "delete-region.rs"]
mod delete_region;
mod delq;
#[path = "end-of-line.rs"]
mod end_of_line;
mod eval;
mod equality;
mod format;
#[path = "forward-char.rs"]
mod forward_char;
#[path = "forward-line.rs"]
mod forward_line;
mod funcall;
mod r#get;
#[path = "goto-char.rs"]
mod goto_char;
#[path = "hash-table.rs"]
mod hash_table;
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
mod member;
mod memq;
mod min;
mod nconc;
mod nreverse;
mod r#not;
mod nthcdr;
mod r#or;
mod point;
#[path = "point-max.rs"]
mod point_max;
#[path = "point-min.rs"]
mod point_min;
mod predicates;
mod prog1;
mod progn;
mod put;
#[path = "re-search-forward.rs"]
mod re_search_forward;
#[path = "set-buffer.rs"]
mod set_buffer;
mod setcar;
mod setcdr;
mod setq;
mod sort;
mod string;
#[path = "string-distance.rs"]
mod string_distance;
#[path = "string-equal.rs"]
mod string_equal;
#[path = "string-lessp.rs"]
mod string_lessp;
#[path = "string-match.rs"]
mod string_match;
#[path = "string-to-number.rs"]
mod string_to_number;
#[path = "string-version-lessp.rs"]
mod string_version_lessp;
mod symbol;
mod take;
mod r#throw;
mod unless;
#[path = "unwind-protect.rs"]
mod unwind_protect;
mod vector;
mod when;
mod r#while;
