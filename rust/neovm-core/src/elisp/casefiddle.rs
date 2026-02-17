//! Case conversion and character builtins.
//!
//! Implements `capitalize`, `upcase-initials`, and `char-resolve-modifiers`.

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

// ---------------------------------------------------------------------------
// Character helpers
// ---------------------------------------------------------------------------

const CHAR_META: i64 = 0x8000000;
const CHAR_CTL: i64 = 0x4000000;
const CHAR_SHIFT: i64 = 0x2000000;
const CHAR_HYPER: i64 = 0x1000000;
const CHAR_SUPER: i64 = 0x0800000;
const CHAR_ALT: i64 = 0x0400000;
const CHAR_MODIFIER_MASK: i64 =
    CHAR_META | CHAR_CTL | CHAR_SHIFT | CHAR_HYPER | CHAR_SUPER | CHAR_ALT;

/// Convert a character code to a Rust char (if it's a valid Unicode scalar value).
fn code_to_char(code: i64) -> Option<char> {
    if code >= 0 && code <= 0x10FFFF {
        char::from_u32(code as u32)
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Case conversion helpers
// ---------------------------------------------------------------------------

/// Uppercase a single character code, returning the new code.
fn upcase_char(code: i64) -> i64 {
    match code_to_char(code) {
        Some(c) => {
            let mut upper = c.to_uppercase();
            // to_uppercase() may yield multiple chars (e.g. German eszett);
            // take only the first to stay consistent with Emacs behavior.
            upper.next().map(|u| u as i64).unwrap_or(code)
        }
        None => code,
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(capitalize OBJ)` -- if OBJ is a string, capitalize the first letter
/// (uppercase first, lowercase rest).  If OBJ is a character, uppercase it.
pub(crate) fn builtin_capitalize(args: Vec<Value>) -> EvalResult {
    expect_args("capitalize", &args, 1)?;
    match &args[0] {
        Value::Str(s) => {
            let capitalized = capitalize_string(s);
            Ok(Value::string(capitalized))
        }
        Value::Char(c) => {
            let code = *c as i64;
            Ok(Value::Int(upcase_char(code)))
        }
        Value::Int(n) => Ok(Value::Int(upcase_char(*n))),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), other.clone()],
        )),
    }
}

/// Capitalize a string: uppercase the first letter of each word,
/// lowercase the rest.  A "word" starts after any non-alphanumeric character.
fn capitalize_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut new_word = true;
    for c in s.chars() {
        if c.is_alphanumeric() {
            if new_word {
                for u in c.to_uppercase() {
                    result.push(u);
                }
                new_word = false;
            } else {
                for l in c.to_lowercase() {
                    result.push(l);
                }
            }
        } else {
            result.push(c);
            new_word = true;
        }
    }
    result
}

/// `(upcase-initials OBJ)` -- uppercase the first letter of each word in
/// a string, leaving the rest unchanged.  For a char, uppercase it.
pub(crate) fn builtin_upcase_initials(args: Vec<Value>) -> EvalResult {
    expect_args("upcase-initials", &args, 1)?;
    match &args[0] {
        Value::Str(s) => {
            let result = upcase_initials_string(s);
            Ok(Value::string(result))
        }
        Value::Char(c) => {
            let code = *c as i64;
            Ok(Value::Int(upcase_char(code)))
        }
        Value::Int(n) => Ok(Value::Int(upcase_char(*n))),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("char-or-string-p"), other.clone()],
        )),
    }
}

/// Uppercase the first letter of each word, leaving the rest unchanged.
fn upcase_initials_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut new_word = true;
    for c in s.chars() {
        if c.is_alphanumeric() {
            if new_word {
                for u in c.to_uppercase() {
                    result.push(u);
                }
                new_word = false;
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
            new_word = true;
        }
    }
    result
}

/// `(char-resolve-modifiers CHAR)` -- resolve modifier bits in character.
/// Resolve shift/control modifiers into the base character where possible.
pub(crate) fn builtin_char_resolve_modifiers(args: Vec<Value>) -> EvalResult {
    expect_args("char-resolve-modifiers", &args, 1)?;

    let code = match &args[0] {
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("fixnump"), other.clone()],
            ))
        }
    };

    let modifiers = code & CHAR_MODIFIER_MASK;
    let mut base = code & !CHAR_MODIFIER_MASK;
    let mut remaining_mods = modifiers;

    if remaining_mods & CHAR_SHIFT != 0 {
        if base >= 'a' as i64 && base <= 'z' as i64 {
            base = base - 'a' as i64 + 'A' as i64;
            remaining_mods &= !CHAR_SHIFT;
        } else if base >= 'A' as i64 && base <= 'Z' as i64 {
            remaining_mods &= !CHAR_SHIFT;
        }
    }

    if remaining_mods & CHAR_CTL != 0 {
        if base >= '@' as i64 && base <= '_' as i64 {
            base &= 0x1F;
            remaining_mods &= !CHAR_CTL;
        } else if base >= 'a' as i64 && base <= 'z' as i64 {
            base &= 0x1F;
            remaining_mods &= !CHAR_CTL;
        } else if base == '?' as i64 {
            base = 127;
            remaining_mods &= !CHAR_CTL;
        }
    }

    Ok(Value::Int(base | remaining_mods))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // =======================================================================
    // capitalize
    // =======================================================================

    #[test]
    fn capitalize_string_basic() {
        let result = builtin_capitalize(vec![Value::string("hello world")]).unwrap();
        assert_eq!(result.as_str(), Some("Hello World"));
    }

    #[test]
    fn capitalize_string_mixed() {
        let result = builtin_capitalize(vec![Value::string("hELLO wORLD")]).unwrap();
        assert_eq!(result.as_str(), Some("Hello World"));
    }

    #[test]
    fn capitalize_char() {
        let result = builtin_capitalize(vec![Value::Char('a')]).unwrap();
        assert_eq!(result.as_int(), Some('A' as i64));
    }

    #[test]
    fn capitalize_empty_string() {
        let result = builtin_capitalize(vec![Value::string("")]).unwrap();
        assert_eq!(result.as_str(), Some(""));
    }

    // =======================================================================
    // upcase-initials
    // =======================================================================

    #[test]
    fn upcase_initials_basic() {
        let result = builtin_upcase_initials(vec![Value::string("hello world")]).unwrap();
        assert_eq!(result.as_str(), Some("Hello World"));
    }

    #[test]
    fn upcase_initials_preserves_rest() {
        let result = builtin_upcase_initials(vec![Value::string("hELLO wORLD")]).unwrap();
        // Only first letter of each word is uppercased; rest is left alone.
        assert_eq!(result.as_str(), Some("HELLO WORLD"));
    }

    #[test]
    fn upcase_initials_char() {
        let result = builtin_upcase_initials(vec![Value::Char('a')]).unwrap();
        assert_eq!(result.as_int(), Some('A' as i64));
    }

    // =======================================================================
    // char-resolve-modifiers
    // =======================================================================

    #[test]
    fn char_resolve_modifiers_resolves_shift_lowercase() {
        let result =
            builtin_char_resolve_modifiers(vec![Value::Int(0x2000000 | ('a' as i64))]).unwrap();
        assert_eq!(result.as_int(), Some('A' as i64));
    }

    #[test]
    fn char_resolve_modifiers_clears_shift_on_uppercase() {
        let result =
            builtin_char_resolve_modifiers(vec![Value::Int(0x2000000 | ('A' as i64))]).unwrap();
        assert_eq!(result.as_int(), Some('A' as i64));
    }

    #[test]
    fn char_resolve_modifiers_wrong_type_predicate() {
        let result = builtin_char_resolve_modifiers(vec![Value::string("a")]).unwrap_err();
        match result {
            super::super::error::Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("fixnump"), Value::string("a")]);
            }
            other => panic!("expected signal flow, got {other:?}"),
        }
    }

    // =======================================================================
    // Edge cases
    // =======================================================================

    #[test]
    fn capitalize_with_punctuation() {
        let result = builtin_capitalize(vec![Value::string("it's a test")]).unwrap();
        assert_eq!(result.as_str(), Some("It'S A Test"));
    }
}
