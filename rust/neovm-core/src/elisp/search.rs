//! Search and regex builtins for the Elisp interpreter.
//!
//! Pure builtins:
//! - `string-match`, `string-match-p`, `regexp-quote`
//! - `match-beginning`, `match-end`, `match-data`, `set-match-data`
//! - `looking-at` (stub), `replace-regexp-in-string`
//!
//! Eval-dependent builtins:
//! - `search-forward`, `search-backward`
//! - `re-search-forward`, `re-search-backward`
//! - `posix-search-forward`, `posix-search-backward`
//! - `replace-match`
//! - `word-search-forward`, `word-search-backward`

use super::error::{signal, EvalResult, Flow};
use super::value::*;

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

fn expect_string(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(string-match REGEXP STRING &optional START)` -- search for REGEXP in
/// STRING starting at START (default 0).  Returns the index of the match
/// or nil.  Updates match data.
pub(crate) fn builtin_string_match(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-match", &args, 2)?;
    let pattern = expect_string(&args[0])?;
    let s = expect_string(&args[1])?;
    let start = if args.len() > 2 && !args[2].is_nil() {
        expect_int(&args[2])? as usize
    } else {
        0
    };

    let rust_pattern = super::regex::translate_emacs_regex(&pattern);
    let re = regex::Regex::new(&rust_pattern)
        .map_err(|e| signal("invalid-regexp", vec![Value::string(e.to_string())]))?;

    if start > s.len() {
        return Ok(Value::Nil);
    }

    let search_region = &s[start..];
    match re.captures(search_region) {
        Some(caps) => {
            let m = caps.get(0).unwrap();
            let match_start = m.start() + start;
            Ok(Value::Int(match_start as i64))
        }
        None => Ok(Value::Nil),
    }
}

/// `(string-match-p REGEXP STRING &optional START)` -- like `string-match`
/// but does not modify match data.
pub(crate) fn builtin_string_match_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-match-p", &args, 2)?;
    let pattern = expect_string(&args[0])?;
    let s = expect_string(&args[1])?;
    let start = if args.len() > 2 && !args[2].is_nil() {
        expect_int(&args[2])? as usize
    } else {
        0
    };

    let rust_pattern = super::regex::translate_emacs_regex(&pattern);
    let re = regex::Regex::new(&rust_pattern)
        .map_err(|e| signal("invalid-regexp", vec![Value::string(e.to_string())]))?;

    if start > s.len() {
        return Ok(Value::Nil);
    }

    let search_region = &s[start..];
    match re.find(search_region) {
        Some(m) => {
            let match_start = m.start() + start;
            Ok(Value::Int(match_start as i64))
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
/// match group.  Stub: returns 0.
pub(crate) fn builtin_match_beginning(args: Vec<Value>) -> EvalResult {
    expect_args("match-beginning", &args, 1)?;
    let _subexp = expect_int(&args[0])?;
    // Stub: proper implementation requires match data stored on the evaluator.
    Ok(Value::Int(0))
}

/// `(match-end SUBEXP)` -- return the end position of the SUBEXPth
/// match group.  Stub: returns 0.
pub(crate) fn builtin_match_end(args: Vec<Value>) -> EvalResult {
    expect_args("match-end", &args, 1)?;
    let _subexp = expect_int(&args[0])?;
    // Stub: proper implementation requires match data stored on the evaluator.
    Ok(Value::Int(0))
}

/// `(match-data &optional INTEGERS REUSE RESEAT)` -- return the match data
/// as a list.  Stub: returns nil.
pub(crate) fn builtin_match_data(args: Vec<Value>) -> EvalResult {
    let _ = args;
    // Stub: proper implementation requires match data stored on the evaluator.
    Ok(Value::Nil)
}

/// `(set-match-data LIST &optional RESEAT)` -- set match data from LIST.
/// Stub: returns nil.
pub(crate) fn builtin_set_match_data(args: Vec<Value>) -> EvalResult {
    expect_min_args("set-match-data", &args, 1)?;
    let _ = &args[0];
    // Stub: proper implementation requires match data stored on the evaluator.
    Ok(Value::Nil)
}

/// `(looking-at REGEXP)` -- test whether text after point matches REGEXP.
/// Stub: returns nil (needs buffer context).
pub(crate) fn builtin_looking_at(args: Vec<Value>) -> EvalResult {
    expect_args("looking-at", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

/// `(replace-regexp-in-string REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)`
/// -- replace all matches of REGEXP in STRING with REP.
pub(crate) fn builtin_replace_regexp_in_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("replace-regexp-in-string", &args, 3)?;
    let pattern = expect_string(&args[0])?;
    let rep = expect_string(&args[1])?;
    let s = expect_string(&args[2])?;
    let _fixedcase = args.get(3).is_some_and(|v| v.is_truthy());
    let literal = args.get(4).is_some_and(|v| v.is_truthy());
    let _subexp = args.get(5);
    let start = if args.len() > 6 {
        expect_int(&args[6])? as usize
    } else {
        0
    };

    let rust_pattern = super::regex::translate_emacs_regex(&pattern);
    let re = regex::Regex::new(&rust_pattern)
        .map_err(|e| signal("invalid-regexp", vec![Value::string(e.to_string())]))?;

    let search_region = if start > 0 && start < s.len() {
        &s[start..]
    } else {
        &s
    };

    let result = if literal {
        re.replace_all(search_region, regex::NoExpand(&rep)).into_owned()
    } else {
        // Translate Emacs-style back-references (\1, \&, etc.) to regex crate style ($1, ${0})
        let rust_rep = rep
            .replace("\\&", "${0}")
            .replace("\\1", "${1}")
            .replace("\\2", "${2}")
            .replace("\\3", "${3}")
            .replace("\\4", "${4}")
            .replace("\\5", "${5}")
            .replace("\\6", "${6}")
            .replace("\\7", "${7}")
            .replace("\\8", "${8}")
            .replace("\\9", "${9}");
        re.replace_all(search_region, rust_rep.as_str()).into_owned()
    };

    if start > 0 && start < s.len() {
        Ok(Value::string(format!("{}{}", &s[..start], result)))
    } else {
        Ok(Value::string(result))
    }
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// `(search-forward STRING &optional BOUND NOERROR COUNT)` -- search
/// forward in buffer for literal STRING.  Stub: returns nil.
pub(crate) fn builtin_search_forward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("search-forward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

/// `(search-backward STRING &optional BOUND NOERROR COUNT)` -- search
/// backward in buffer for literal STRING.  Stub: returns nil.
pub(crate) fn builtin_search_backward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("search-backward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

/// `(re-search-forward REGEXP &optional BOUND NOERROR COUNT)` -- regex
/// search forward in buffer.  Stub: returns nil.
pub(crate) fn builtin_re_search_forward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("re-search-forward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

/// `(re-search-backward REGEXP &optional BOUND NOERROR COUNT)` -- regex
/// search backward in buffer.  Stub: returns nil.
pub(crate) fn builtin_re_search_backward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("re-search-backward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

/// `(posix-search-forward REGEXP &optional BOUND NOERROR COUNT)` -- like
/// `re-search-forward` but uses POSIX matching semantics.  Stub: returns nil.
pub(crate) fn builtin_posix_search_forward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("posix-search-forward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context and POSIX regex engine.
    Ok(Value::Nil)
}

/// `(posix-search-backward REGEXP &optional BOUND NOERROR COUNT)` -- like
/// `re-search-backward` but uses POSIX matching semantics.  Stub: returns nil.
pub(crate) fn builtin_posix_search_backward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("posix-search-backward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context and POSIX regex engine.
    Ok(Value::Nil)
}

/// `(replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)` --
/// replace the text matched by the last search.  Stub: returns nil.
pub(crate) fn builtin_replace_match(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("replace-match", &args, 1)?;
    let _newtext = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context and match data.
    Ok(Value::Nil)
}

/// `(word-search-forward STRING &optional BOUND NOERROR COUNT)` -- search
/// forward for STRING treating it as a sequence of words.  Stub: returns nil.
pub(crate) fn builtin_word_search_forward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("word-search-forward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

/// `(word-search-backward STRING &optional BOUND NOERROR COUNT)` -- search
/// backward for STRING treating it as a sequence of words.  Stub: returns nil.
pub(crate) fn builtin_word_search_backward(
    _eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("word-search-backward", &args, 1)?;
    let _pattern = expect_string(&args[0])?;
    // Stub: proper implementation requires buffer context.
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_match_basic() {
        let result = builtin_string_match(vec![
            Value::string("he..o"),
            Value::string("hello world"),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn string_match_with_start() {
        let result = builtin_string_match(vec![
            Value::string("world"),
            Value::string("hello world"),
            Value::Int(6),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(6));
    }

    #[test]
    fn string_match_no_match() {
        let result = builtin_string_match(vec![
            Value::string("xyz"),
            Value::string("hello world"),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);
    }

    #[test]
    fn string_match_p_basic() {
        let result = builtin_string_match_p(vec![
            Value::string("[0-9]+"),
            Value::string("abc 123 def"),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(4));
    }

    #[test]
    fn string_match_p_no_match() {
        let result = builtin_string_match_p(vec![
            Value::string("[0-9]+"),
            Value::string("no digits here"),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);
    }

    #[test]
    fn regexp_quote_specials() {
        let result = builtin_regexp_quote(vec![Value::string("foo.bar*baz+qux")]);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "foo\\.bar\\*baz\\+qux"),
            other => panic!("Expected string, got {:?}", other),
        }
    }

    #[test]
    fn regexp_quote_no_specials() {
        let result = builtin_regexp_quote(vec![Value::string("hello")]);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "hello"),
            other => panic!("Expected string, got {:?}", other),
        }
    }

    #[test]
    fn regexp_quote_all_specials() {
        let result = builtin_regexp_quote(vec![Value::string(".*+?[]^$\\")]);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "\\.\\*\\+\\?\\[\\]\\^\\$\\\\"),
            other => panic!("Expected string, got {:?}", other),
        }
    }

    #[test]
    fn match_beginning_stub() {
        let result = builtin_match_beginning(vec![Value::Int(0)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn match_end_stub() {
        let result = builtin_match_end(vec![Value::Int(0)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn match_data_stub() {
        let result = builtin_match_data(vec![]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);
    }

    #[test]
    fn set_match_data_stub() {
        let result = builtin_set_match_data(vec![Value::Nil]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);
    }

    #[test]
    fn looking_at_stub() {
        let result = builtin_looking_at(vec![Value::string("foo")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Nil);
    }

    #[test]
    fn replace_regexp_basic() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("[0-9]+"),
            Value::string("NUM"),
            Value::string("abc 123 def 456"),
        ]);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "abc NUM def NUM"),
            other => panic!("Expected string, got {:?}", other),
        }
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
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "abc $0 def"),
            other => panic!("Expected string, got {:?}", other),
        }
    }

    #[test]
    fn replace_regexp_with_backref() {
        // Use Emacs-style group: \(\w+\) and back-reference \1
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("\\(\\w+\\)"),
            Value::string("[\\1]"),
            Value::string("hello world"),
        ]);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "[hello] [world]"),
            other => panic!("Expected string, got {:?}", other),
        }
    }

    #[test]
    fn replace_regexp_with_start() {
        let result = builtin_replace_regexp_in_string(vec![
            Value::string("[0-9]+"),
            Value::string("X"),
            Value::string("111 222 333"),
            Value::Nil, // fixedcase
            Value::Nil, // literal
            Value::Nil, // subexp
            Value::Int(4), // start
        ]);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "111 X X"),
            other => panic!("Expected string, got {:?}", other),
        }
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
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Str(s) => assert_eq!(&*s, "(foo)"),
            other => panic!("Expected string, got {:?}", other),
        }
    }

    #[test]
    fn string_match_emacs_groups() {
        // Emacs regex with groups: \(foo\|bar\) matching "test bar"
        let result = builtin_string_match(vec![
            Value::string("\\(foo\\|bar\\)"),
            Value::string("test bar"),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(5));
    }
}
