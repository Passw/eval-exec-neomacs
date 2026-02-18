//! Search and regex builtins for the Elisp interpreter.
//!
//! Pure builtins:
//! - `string-match`, `string-match-p`, `regexp-quote`
//! - `match-beginning`, `match-end`, `match-data`, `set-match-data`
//! - `looking-at`, `looking-at-p`, `replace-regexp-in-string`
//!
//! Eval-dependent builtins:
//! - `search-forward`, `search-backward`
//! - `re-search-forward`, `re-search-backward`
//! - `posix-search-forward`, `posix-search-backward`
//! - `replace-match`
//! - `word-search-forward`, `word-search-backward`

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::cell::RefCell;

thread_local! {
    static PURE_MATCH_DATA: RefCell<Option<super::regex::MatchData>> = const { RefCell::new(None) };
}

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

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

fn expect_range_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_int(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn expect_integer_or_marker(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

fn expect_string(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn normalize_string_start_arg(string: &str, start: Option<&Value>) -> Result<usize, Flow> {
    let Some(start_val) = start else {
        return Ok(0);
    };
    if start_val.is_nil() {
        return Ok(0);
    }

    let raw_start = expect_int(start_val)?;
    let len = string.chars().count() as i64;
    let normalized = if raw_start < 0 {
        len.checked_add(raw_start)
    } else {
        Some(raw_start)
    };

    let Some(start_idx) = normalized else {
        return Err(signal(
            "args-out-of-range",
            vec![Value::string(string), Value::Int(raw_start)],
        ));
    };

    if !(0..=len).contains(&start_idx) {
        return Err(signal(
            "args-out-of-range",
            vec![Value::string(string), Value::Int(raw_start)],
        ));
    }

    let start_char_idx = start_idx as usize;
    if start_char_idx == len as usize {
        return Ok(string.len());
    }

    Ok(string
        .char_indices()
        .nth(start_char_idx)
        .map(|(byte_idx, _)| byte_idx)
        .unwrap_or(string.len()))
}

fn string_byte_to_char_index(s: &str, byte_idx: usize) -> Option<usize> {
    s.get(..byte_idx).map(|prefix| prefix.chars().count())
}

fn preserve_case(replacement: &str, matched: &str) -> String {
    if matched.is_empty() || replacement.is_empty() {
        return replacement.to_string();
    }

    let all_upper = matched
        .chars()
        .all(|c| !c.is_alphabetic() || c.is_uppercase());
    let has_alpha = matched.chars().any(|c| c.is_alphabetic());
    if all_upper && has_alpha {
        return replacement.to_uppercase();
    }

    let mut chars = matched.chars();
    let first = chars.next().unwrap();
    let first_upper = first.is_uppercase();
    let rest_lower = chars.all(|c| !c.is_alphabetic() || c.is_lowercase());
    if first_upper && rest_lower {
        let mut out = String::with_capacity(replacement.len());
        let mut rep_chars = replacement.chars();
        if let Some(ch) = rep_chars.next() {
            for uc in ch.to_uppercase() {
                out.push(uc);
            }
        }
        out.extend(rep_chars);
        return out;
    }

    replacement.to_string()
}

fn expand_emacs_replacement(rep: &str, caps: &regex::Captures<'_>, literal: bool) -> String {
    if literal {
        return rep.to_string();
    }

    let mut out = String::with_capacity(rep.len());
    let mut chars = rep.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }

        let Some(next) = chars.next() else {
            out.push('\\');
            break;
        };

        match next {
            '&' => {
                if let Some(m) = caps.get(0) {
                    out.push_str(m.as_str());
                }
            }
            '1'..='9' => {
                let idx = next.to_digit(10).unwrap() as usize;
                if let Some(m) = caps.get(idx) {
                    out.push_str(m.as_str());
                }
            }
            '\\' => out.push('\\'),
            other => out.push(other),
        }
    }

    out
}

fn flatten_match_data(md: &super::regex::MatchData) -> Value {
    let mut trailing = md.groups.len();
    while trailing > 0 && md.groups[trailing - 1].is_none() {
        trailing -= 1;
    }

    let mut flat: Vec<Value> = Vec::with_capacity(trailing * 2);
    for grp in md.groups.iter().take(trailing) {
        match grp {
            Some((start, end)) => {
                if let Some(searched) = md.searched_string.as_ref() {
                    let start_char = string_byte_to_char_index(searched, *start);
                    let end_char = string_byte_to_char_index(searched, *end);
                    match (start_char, end_char) {
                        (Some(s), Some(e)) => {
                            flat.push(Value::Int(s as i64));
                            flat.push(Value::Int(e as i64));
                        }
                        _ => {
                            flat.push(Value::Nil);
                            flat.push(Value::Nil);
                        }
                    }
                } else {
                    flat.push(Value::Int(*start as i64));
                    flat.push(Value::Int(*end as i64));
                }
            }
            None => {
                flat.push(Value::Nil);
                flat.push(Value::Nil);
            }
        }
    }
    Value::list(flat)
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(string-match REGEXP STRING &optional START)` -- search for REGEXP in
/// STRING starting at START (default 0).  Returns the index of the match
/// or nil.  Updates match data.
pub(crate) fn builtin_string_match(args: Vec<Value>) -> EvalResult {
    expect_range_args("string-match", &args, 2, 4)?;
    let pattern = expect_string(&args[0])?;
    let s = expect_string(&args[1])?;
    let start = normalize_string_start_arg(&s, args.get(2))?;

    PURE_MATCH_DATA.with(|slot| {
        let mut md = slot.borrow_mut();
        match super::regex::string_match_full(&pattern, &s, start, &mut md) {
            Ok(Some(pos)) => Ok(Value::Int(s[..pos].chars().count() as i64)),
            Ok(None) => Ok(Value::Nil),
            Err(msg) => Err(signal("invalid-regexp", vec![Value::string(msg)])),
        }
    })
}

/// `(string-match-p REGEXP STRING &optional START)` -- like `string-match`
/// but does not modify match data.
pub(crate) fn builtin_string_match_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("string-match-p", &args, 2, 3)?;
    let pattern = expect_string(&args[0])?;
    let s = expect_string(&args[1])?;
    let start = normalize_string_start_arg(&s, args.get(2))?;

    // Emacs defaults `case-fold-search` to non-nil for string matching.
    let rust_pattern = format!("(?i:{})", super::regex::translate_emacs_regex(&pattern));
    let re = regex::Regex::new(&rust_pattern)
        .map_err(|e| signal("invalid-regexp", vec![Value::string(e.to_string())]))?;

    let search_region = &s[start..];
    match re.find(search_region) {
        Some(m) => {
            let match_start = m.start() + start;
            Ok(Value::Int(s[..match_start].chars().count() as i64))
        }
        None => Ok(Value::Nil),
    }
}

/// `(regexp-quote STRING)` -- return a regexp that matches STRING literally,
/// quoting all special regex characters.
pub(crate) fn builtin_regexp_quote(args: Vec<Value>) -> EvalResult {
    expect_args("regexp-quote", &args, 1)?;
    let s = expect_string(&args[0])?;
    // Quote Emacs regex special characters.
    // In Emacs regex, the special characters that need quoting when used
    // literally are: . * + ? [ ] ^ $ \
    // Note: In Emacs, ( ) { } | are literal by default (their escaped
    // forms \( \) \{ \} \| are the special ones), so they do NOT need
    // quoting.
    let mut result = String::with_capacity(s.len() + 8);
    for ch in s.chars() {
        match ch {
            '.' | '*' | '+' | '?' | '[' | ']' | '^' | '$' | '\\' => {
                result.push('\\');
                result.push(ch);
            }
            _ => result.push(ch),
        }
    }
    Ok(Value::string(result))
}

/// `(match-beginning SUBEXP)` -- return the start position of the SUBEXPth
/// match group, or nil if unavailable.
pub(crate) fn builtin_match_beginning(args: Vec<Value>) -> EvalResult {
    expect_args("match-beginning", &args, 1)?;
    let subexp = expect_int(&args[0])? as usize;
    PURE_MATCH_DATA.with(|slot| {
        let md = slot.borrow();
        let Some(md) = md.as_ref() else {
            return Ok(Value::Nil);
        };
        match md.groups.get(subexp) {
            Some(Some((start, _end))) => {
                if let Some(searched) = md.searched_string.as_ref() {
                    match string_byte_to_char_index(searched, *start) {
                        Some(pos) => Ok(Value::Int(pos as i64)),
                        None => Ok(Value::Nil),
                    }
                } else {
                    Ok(Value::Int(*start as i64))
                }
            }
            _ => Ok(Value::Nil),
        }
    })
}

/// `(match-end SUBEXP)` -- return the end position of the SUBEXPth
/// match group, or nil if unavailable.
pub(crate) fn builtin_match_end(args: Vec<Value>) -> EvalResult {
    expect_args("match-end", &args, 1)?;
    let subexp = expect_int(&args[0])? as usize;
    PURE_MATCH_DATA.with(|slot| {
        let md = slot.borrow();
        let Some(md) = md.as_ref() else {
            return Ok(Value::Nil);
        };
        match md.groups.get(subexp) {
            Some(Some((_start, end))) => {
                if let Some(searched) = md.searched_string.as_ref() {
                    match string_byte_to_char_index(searched, *end) {
                        Some(pos) => Ok(Value::Int(pos as i64)),
                        None => Ok(Value::Nil),
                    }
                } else {
                    Ok(Value::Int(*end as i64))
                }
            }
            _ => Ok(Value::Nil),
        }
    })
}

/// `(match-data &optional INTEGERS REUSE RESEAT)` -- return the match data
/// as a list.
pub(crate) fn builtin_match_data(args: Vec<Value>) -> EvalResult {
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("match-data"), Value::Int(args.len() as i64)],
        ));
    }
    PURE_MATCH_DATA.with(|slot| {
        let md = slot.borrow();
        let Some(md) = md.as_ref() else {
            return Ok(Value::Nil);
        };
        Ok(flatten_match_data(md))
    })
}

/// `(set-match-data LIST &optional RESEAT)` -- set match data from LIST.
pub(crate) fn builtin_set_match_data(args: Vec<Value>) -> EvalResult {
    expect_min_args("set-match-data", &args, 1)?;
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("set-match-data"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    if args[0].is_nil() {
        PURE_MATCH_DATA.with(|slot| {
            *slot.borrow_mut() = None;
        });
        return Ok(Value::Nil);
    }

    let items = list_to_vec(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0].clone()],
        )
    })?;

    let mut groups: Vec<Option<(usize, usize)>> = Vec::with_capacity(items.len() / 2);
    let mut i = 0usize;
    while i + 1 < items.len() {
        let start_v = &items[i];
        let end_v = &items[i + 1];
        if start_v.is_nil() && end_v.is_nil() {
            groups.push(None);
            i += 2;
            continue;
        }

        let start = expect_integer_or_marker(start_v)?;
        let end = expect_integer_or_marker(end_v)?;

        // Negative marker positions terminate match-data parsing.
        if start < 0 || end < 0 {
            break;
        }

        groups.push(Some((start as usize, end as usize)));
        i += 2;
    }

    PURE_MATCH_DATA.with(|slot| {
        if groups.is_empty() {
            *slot.borrow_mut() = None;
        } else {
            *slot.borrow_mut() = Some(super::regex::MatchData {
                groups,
                searched_string: None,
            });
        }
    });

    Ok(Value::Nil)
}

fn anchored_looking_at_matches(
    pattern: &str,
    text: &str,
) -> Result<Vec<Option<(usize, usize)>>, Flow> {
    let translated = super::regex::translate_emacs_regex(pattern);
    let anchored = if translated.starts_with("\\A") || translated.starts_with('^') {
        translated
    } else {
        format!("\\A(?:{translated})")
    };
    let re = regex::Regex::new(&format!("(?i:{anchored})"))
        .map_err(|e| signal("invalid-regexp", vec![Value::string(e.to_string())]))?;

    match re.captures(text) {
        Some(caps) => {
            let mut groups = Vec::with_capacity(caps.len());
            for i in 0..caps.len() {
                groups.push(caps.get(i).map(|m| (m.start(), m.end())));
            }
            Ok(groups)
        }
        None => Ok(Vec::new()),
    }
}

/// `(looking-at REGEXP)` -- test whether text after point matches REGEXP.
/// In batch mode we support an optional second argument as a sample string.
/// When absent, this returns nil after validating REGEXP.
pub(crate) fn builtin_looking_at(args: Vec<Value>) -> EvalResult {
    expect_range_args("looking-at", &args, 1, 2)?;
    let pattern = expect_string(&args[0])?;

    let text = args.get(1).and_then(|value| value.as_str());
    match text {
        Some(text) => match anchored_looking_at_matches(&pattern, text)? {
            groups if groups.is_empty() => {
                PURE_MATCH_DATA.with(|slot| *slot.borrow_mut() = None);
                Ok(Value::Nil)
            }
            groups => {
                PURE_MATCH_DATA.with(|slot| {
                    *slot.borrow_mut() = Some(super::regex::MatchData {
                        groups,
                        searched_string: Some(text.to_string()),
                    })
                });
                Ok(Value::True)
            }
        },
        None => {
            let _ = anchored_looking_at_matches(&pattern, "")?;
            PURE_MATCH_DATA.with(|slot| *slot.borrow_mut() = None);
            Ok(Value::Nil)
        }
    }
}

/// `(looking-at-p REGEXP)` -- same as `looking-at`, preserving match data.
pub(crate) fn builtin_looking_at_p(args: Vec<Value>) -> EvalResult {
    expect_args("looking-at-p", &args, 1)?;
    let pattern = expect_string(&args[0])?;
    PURE_MATCH_DATA.with(|slot| {
        let snapshot = slot.borrow().clone();
        let _ = anchored_looking_at_matches(&pattern, "")?;
        *slot.borrow_mut() = snapshot;
        Ok(Value::Nil)
    })
}

/// `(replace-regexp-in-string REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)`
/// -- replace all matches of REGEXP in STRING with REP.
pub(crate) fn builtin_replace_regexp_in_string(args: Vec<Value>) -> EvalResult {
    builtin_replace_regexp_in_string_with_case_fold(args, true)
}

fn builtin_replace_regexp_in_string_with_case_fold(
    args: Vec<Value>,
    case_fold: bool,
) -> EvalResult {
    expect_range_args("replace-regexp-in-string", &args, 3, 7)?;
    let pattern = expect_string(&args[0])?;
    let rep = expect_string(&args[1])?;
    let s = expect_string(&args[2])?;
    let fixedcase = args.get(3).is_some_and(|v| v.is_truthy());
    let literal = args.get(4).is_some_and(|v| v.is_truthy());

    let (subexp, start) = if args.len() == 7 {
        (Some(&args[5]), args.get(6))
    } else {
        (None, args.get(5))
    };

    let subexp = match subexp {
        Some(Value::Nil) | None => 0,
        Some(value) => expect_int(value)?,
    };
    if subexp < 0 {
        return Err(signal(
            "args-out-of-range",
            vec![
                Value::Int(subexp),
                Value::Int(0),
                Value::Int(s.len() as i64),
            ],
        ));
    }

    let start = normalize_string_start_arg(&s, start)?;

    let translated = super::regex::translate_emacs_regex(&pattern);
    let rust_pattern = if case_fold {
        format!("(?i:{translated})")
    } else {
        translated
    };
    let re = regex::Regex::new(&rust_pattern)
        .map_err(|e| signal("invalid-regexp", vec![Value::string(e.to_string())]))?;

    let max_subexp = re.captures_len().saturating_sub(1);
    if (subexp as usize) > max_subexp {
        return Err(signal(
            "error",
            vec![
                Value::string("replace-match subexpression does not exist"),
                Value::Int(subexp),
            ],
        ));
    }

    let search_region = &s[start..];
    let mut out = String::with_capacity(search_region.len());
    let mut cursor = 0usize;

    for caps in re.captures_iter(search_region) {
        let full_match = match caps.get(0) {
            Some(m) => m,
            None => continue,
        };

        let (replace_start, replace_end, case_source) = if subexp == 0 {
            let src = full_match.as_str();
            (full_match.start(), full_match.end(), src)
        } else if let Some(g) = caps.get(subexp as usize) {
            let src = g.as_str();
            (g.start(), g.end(), src)
        } else {
            return Err(signal(
                "error",
                vec![
                    Value::string("replace-match subexpression does not exist"),
                    Value::Int(subexp),
                ],
            ));
        };

        out.push_str(&search_region[cursor..replace_start]);
        let base = expand_emacs_replacement(&rep, &caps, literal);
        let replacement = if fixedcase {
            base
        } else {
            preserve_case(&base, case_source)
        };
        out.push_str(&replacement);
        cursor = replace_end;
    }

    out.push_str(&search_region[cursor..]);
    Ok(Value::string(out))
}

fn dynamic_or_global_symbol_value(eval: &super::eval::Evaluator, name: &str) -> Option<Value> {
    for frame in eval.dynamic.iter().rev() {
        if let Some(value) = frame.get(name) {
            return Some(value.clone());
        }
    }
    eval.obarray.symbol_value(name).cloned()
}

pub(crate) fn builtin_replace_regexp_in_string_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let case_fold = dynamic_or_global_symbol_value(eval, "case-fold-search")
        .map(|value| !value.is_nil())
        .unwrap_or(true);
    builtin_replace_regexp_in_string_with_case_fold(args, case_fold)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_int(val: Value, expected: i64) {
        match val {
            Value::Int(n) => assert_eq!(n, expected),
            other => panic!("Expected Int({}), got {:?}", expected, other),
        }
    }

    fn assert_nil(val: Value) {
        assert!(val.is_nil(), "Expected nil, got {:?}", val);
    }

    fn assert_true(val: Value) {
        assert!(val.is_truthy(), "Expected true, got {:?}", val);
    }

    fn assert_str(val: Value, expected: &str) {
        match val {
            Value::Str(s) => assert_eq!(&*s, expected),
            other => panic!("Expected string {:?}, got {:?}", expected, other),
        }
    }

    #[test]
    fn string_match_basic() {
        let result =
            builtin_string_match(vec![Value::string("he..o"), Value::string("hello world")]);
        assert_int(result.unwrap(), 0);
    }

    #[test]
    fn string_match_with_start() {
        let result = builtin_string_match(vec![
            Value::string("world"),
            Value::string("hello world"),
            Value::Int(6),
        ]);
        assert_int(result.unwrap(), 6);
    }

    #[test]
    fn string_match_no_match() {
        let result = builtin_string_match(vec![Value::string("xyz"), Value::string("hello world")]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn string_match_defaults_to_case_fold() {
        let result = builtin_string_match(vec![Value::string("a"), Value::string("A")]);
        assert_int(result.unwrap(), 0);
    }

    #[test]
    fn string_match_p_basic() {
        let result =
            builtin_string_match_p(vec![Value::string("[0-9]+"), Value::string("abc 123 def")]);
        assert_int(result.unwrap(), 4);
    }

    #[test]
    fn string_match_p_no_match() {
        let result = builtin_string_match_p(vec![
            Value::string("[0-9]+"),
            Value::string("no digits here"),
        ]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn string_match_p_defaults_to_case_fold() {
        let result = builtin_string_match_p(vec![Value::string("a"), Value::string("A")]);
        assert_int(result.unwrap(), 0);
    }

    #[test]
    fn regexp_quote_specials() {
        let result = builtin_regexp_quote(vec![Value::string("foo.bar*baz+qux")]);
        assert_str(result.unwrap(), "foo\\.bar\\*baz\\+qux");
    }

    #[test]
    fn regexp_quote_no_specials() {
        let result = builtin_regexp_quote(vec![Value::string("hello")]);
        assert_str(result.unwrap(), "hello");
    }

    #[test]
    fn regexp_quote_all_specials() {
        let result = builtin_regexp_quote(vec![Value::string(".*+?[]^$\\")]);
        assert_str(result.unwrap(), "\\.\\*\\+\\?\\[\\]\\^\\$\\\\");
    }

    #[test]
    fn match_beginning_nil_without_match_data() {
        builtin_set_match_data(vec![Value::Nil]).unwrap();
        let result = builtin_match_beginning(vec![Value::Int(0)]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn match_end_nil_without_match_data() {
        builtin_set_match_data(vec![Value::Nil]).unwrap();
        let result = builtin_match_end(vec![Value::Int(0)]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn match_data_nil_without_match_data() {
        builtin_set_match_data(vec![Value::Nil]).unwrap();
        let result = builtin_match_data(vec![]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn set_match_data_nil_clears_state() {
        builtin_set_match_data(vec![Value::list(vec![Value::Int(1), Value::Int(2)])]).unwrap();
        let result = builtin_set_match_data(vec![Value::Nil]);
        assert_nil(result.unwrap());
        let md = builtin_match_data(vec![]).unwrap();
        assert_nil(md);
    }

    #[test]
    fn set_match_data_round_trip() {
        builtin_set_match_data(vec![Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Nil,
            Value::Nil,
            Value::Int(5),
            Value::Int(7),
        ])])
        .unwrap();
        let md = builtin_match_data(vec![]).unwrap();
        assert_eq!(
            md,
            Value::list(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
                Value::Int(5),
                Value::Int(7)
            ])
        );
    }

    #[test]
    fn string_match_updates_match_data() {
        builtin_set_match_data(vec![Value::Nil]).unwrap();
        let result = builtin_string_match(vec![
            Value::string("\\(foo\\|bar\\)"),
            Value::string("test bar"),
        ]);
        assert_int(result.unwrap(), 5);

        let begin = builtin_match_beginning(vec![Value::Int(0)]).unwrap();
        let end = builtin_match_end(vec![Value::Int(0)]).unwrap();
        assert_int(begin, 5);
        assert_int(end, 8);
    }

    #[test]
    fn string_match_start_nil_and_negative() {
        let with_nil =
            builtin_string_match(vec![Value::string("a"), Value::string("ba"), Value::Nil])
                .unwrap();
        assert_int(with_nil, 1);

        let with_negative = builtin_string_match(vec![
            Value::string("a"),
            Value::string("ba"),
            Value::Int(-1),
        ])
        .unwrap();
        assert_int(with_negative, 1);

        let out_of_range =
            builtin_string_match(vec![Value::string("a"), Value::string("ba"), Value::Int(3)]);
        assert!(out_of_range.is_err());
    }

    #[test]
    fn looking_at_default_at_point() {
        let result = builtin_looking_at(vec![Value::string("foo")]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn looking_at_with_text() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::string("foobar")]);
        assert_true(result.unwrap());
        let begin = builtin_match_beginning(vec![Value::Int(0)]).unwrap();
        let end = builtin_match_end(vec![Value::Int(0)]).unwrap();
        assert_int(begin, 0);
        assert_int(end, 3);
    }

    #[test]
    fn looking_at_with_text_case_fold_default() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::string("FOO BAR")]);
        assert_true(result.unwrap());
    }

    #[test]
    fn looking_at_with_text_requires_start_position() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::string("bar foo")]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn looking_at_with_text_no_match() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::string("bar")]);
        assert_nil(result.unwrap());

        let begin = builtin_match_beginning(vec![Value::Int(0)]).unwrap();
        assert_nil(begin);
    }

    #[test]
    fn looking_at_invalid_regexp_signals() {
        let result = builtin_looking_at(vec![Value::string("[")]);
        assert!(result.is_err());
    }

    #[test]
    fn looking_at_wrong_number_of_arguments() {
        let result = builtin_looking_at(vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn looking_at_with_limit_any_value() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::True]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn looking_at_with_limit_limit_nil() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::Nil]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn looking_at_with_limit_marker_like_char() {
        let result = builtin_looking_at(vec![Value::string("foo"), Value::Char('a')]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn looking_at_p_preserves_match_data() {
        let _ = builtin_looking_at(vec![Value::string("foo"), Value::string("foobar")]);
        let before = builtin_match_data(vec![]).unwrap();
        let result = builtin_looking_at_p(vec![Value::string("foo")]);
        assert_nil(result.unwrap());
        let after = builtin_match_data(vec![]).unwrap();
        assert_eq!(before, after);
    }

    #[test]
    fn looking_at_p_does_not_signal_without_text() {
        let result = builtin_looking_at_p(vec![Value::string("foo")]);
        assert_nil(result.unwrap());
    }

    #[test]
    fn looking_at_p_invalid_regexp_signals() {
        let result = builtin_looking_at_p(vec![Value::string("[")]);
        assert!(result.is_err());
    }

    #[test]
    fn looking_at_p_wrong_number_of_arguments() {
        let result = builtin_looking_at_p(vec![]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn replace_regexp_basic() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("[0-9]+"),
            Value::string("NUM"),
            Value::string("abc 123 def 456"),
        ]);
        assert_str(result.unwrap(), "abc NUM def NUM");
    }

    #[test]
    fn replace_regexp_literal() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("[0-9]+"),
            Value::string("$0"),
            Value::string("abc 123 def"),
            Value::Nil,  // fixedcase
            Value::True, // literal
        ]);
        assert_str(result.unwrap(), "abc $0 def");
    }

    #[test]
    fn replace_regexp_with_backref() {
        // Use Emacs-style group: \(\w+\) and back-reference \1
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("\\(\\w+\\)"),
            Value::string("[\\1]"),
            Value::string("hello world"),
        ]);
        assert_str(result.unwrap(), "[hello] [world]");
    }

    #[test]
    fn replace_regexp_with_start() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("[0-9]+"),
            Value::string("X"),
            Value::string("111 222 333"),
            Value::Nil,    // fixedcase
            Value::Nil,    // literal
            Value::Nil,    // subexp
            Value::Int(4), // start
        ]);
        assert_str(result.unwrap(), "X X");
    }

    #[test]
    fn replace_regexp_with_start_no_subexp() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("[0-9]+"),
            Value::string("X"),
            Value::string("111 222 333"),
            Value::Nil,    // fixedcase
            Value::Nil,    // literal
            Value::Int(4), // start
        ]);
        assert_str(result.unwrap(), "X X");
    }

    #[test]
    fn replace_regexp_subexp() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("\\([a-z]+\\)-\\([0-9]+\\)"),
            Value::string("N"),
            Value::string("aaa-111 bbb-222"),
            Value::Nil, // fixedcase
            Value::Nil, // literal
            Value::Int(1),
            Value::Nil, // start
        ]);
        assert_str(result.unwrap(), "N-111 N-222");
    }

    #[test]
    fn replace_regexp_subexp_unmatched_errors() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("\\(a\\)?b"),
            Value::string("N"),
            Value::string("b"),
            Value::Nil,
            Value::Nil,
            Value::Int(1),
            Value::Nil,
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn replace_regexp_preserves_case_when_fixedcase_nil() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("a"),
            Value::string("x"),
            Value::string("A a"),
        ]);
        assert_str(result.unwrap(), "X x");
    }

    #[test]
    fn replace_regexp_fixedcase_disables_case_preserve() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("a"),
            Value::string("x"),
            Value::string("A a"),
            Value::True, // fixedcase
        ]);
        assert_str(result.unwrap(), "x x");
    }

    #[test]
    fn string_match_wrong_type() {
        let result = builtin_string_match(vec![Value::Int(42), Value::string("hello")]);
        assert!(result.is_err());
    }

    #[test]
    fn string_match_too_few_args() {
        let result = builtin_string_match(vec![Value::string("foo")]);
        assert!(result.is_err());
    }

    #[test]
    fn regexp_quote_parens_not_escaped() {
        // In Emacs regex, literal ( ) are NOT special, so regexp-quote
        // should NOT escape them.
        let result = builtin_regexp_quote(vec![Value::string("(foo)")]);
        assert_str(result.unwrap(), "(foo)");
    }

    #[test]
    fn string_match_emacs_groups() {
        // Emacs regex with groups: \(foo\|bar\) matching "test bar"
        let result = builtin_string_match(vec![
            Value::string("\\(foo\\|bar\\)"),
            Value::string("test bar"),
        ]);
        assert_int(result.unwrap(), 5);
    }
}
