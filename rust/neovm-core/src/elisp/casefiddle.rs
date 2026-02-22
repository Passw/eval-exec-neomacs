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
    if preserve_casefiddle_upcase_payload(code) {
        return code;
    }
    match code {
        223 => return 7838,
        452 | 497 => return code + 1,
        454 | 457 | 460 | 499 => return code - 1,
        455 | 458 => return code + 1,
        8064..=8071 | 8080..=8087 | 8096..=8103 => return code + 8,
        8115 | 8131 | 8179 => return code + 9,
        _ => {}
    }
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

fn preserve_casefiddle_upcase_payload(code: i64) -> bool {
    matches!(
        code,
        329
            | 411
            | 453
            | 456
            | 459
            | 496
            | 498
            | 612
            | 912
            | 944
            | 1415
            | 4304..=4346
            | 4349..=4351
            | 7306
            | 7830..=7834
            | 8016
            | 8018
            | 8020
            | 8022
            | 8072..=8079
            | 8088..=8095
            | 8104..=8111
            | 8114
            | 8116
            | 8118..=8119
            | 8124
            | 8130
            | 8132
            | 8134..=8135
            | 8140
            | 8146..=8147
            | 8150..=8151
            | 8162..=8164
            | 8166..=8167
            | 8178
            | 8180
            | 8182..=8183
            | 8188
            | 42957
            | 42959
            | 42963
            | 42965
            | 42971
            | 64256..=64262
            | 64275..=64279
            | 68976..=68997
            | 93883..=93907
    )
}

fn titlecase_from_uppercase_expansion(expansion: &[char]) -> String {
    let mut result = String::new();
    let mut seen_cased = false;

    for uc in expansion {
        let is_cased = uc.is_uppercase() || uc.is_lowercase();
        if !seen_cased {
            result.push(*uc);
            if is_cased {
                seen_cased = true;
            }
            continue;
        }

        if is_cased {
            for lc in uc.to_lowercase() {
                result.push(lc);
            }
        } else {
            result.push(*uc);
        }
    }

    result
}

fn titlecase_combining_iota_override(code: i64) -> Option<&'static str> {
    match code {
        8114 => Some("\u{1FBA}\u{0345}"),
        8116 => Some("\u{0386}\u{0345}"),
        8119 => Some("\u{0391}\u{0342}\u{0345}"),
        8130 => Some("\u{1FCA}\u{0345}"),
        8132 => Some("\u{0389}\u{0345}"),
        8135 => Some("\u{0397}\u{0342}\u{0345}"),
        8178 => Some("\u{1FFA}\u{0345}"),
        8180 => Some("\u{038F}\u{0345}"),
        8183 => Some("\u{03A9}\u{0342}\u{0345}"),
        _ => None,
    }
}

fn titlecase_uses_precomposed_upcase(code: i64) -> bool {
    matches!(
        code,
        8064..=8071
            | 8072..=8111
            | 8115
            | 8124
            | 8131
            | 8140
            | 8179
            | 8188
    )
}

fn titlecase_word_initial(c: char) -> String {
    let code = c as i64;
    if let Some(explicit) = titlecase_combining_iota_override(code) {
        return explicit.to_string();
    }

    let expansion: Vec<char> = c.to_uppercase().collect();
    if expansion.len() > 1 && !titlecase_uses_precomposed_upcase(code) {
        return titlecase_from_uppercase_expansion(&expansion);
    }

    if let Some(mapped) = code_to_char(upcase_char(code)) {
        mapped.to_string()
    } else {
        c.to_uppercase().collect()
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
                for u in titlecase_word_initial(c).chars() {
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
                for u in titlecase_word_initial(c).chars() {
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

    #[test]
    fn capitalize_unicode_edge_semantics() {
        let int_sharp_s = builtin_capitalize(vec![Value::Int(223)]).unwrap();
        assert_eq!(int_sharp_s.as_int(), Some(7838));

        let int_mod_i = builtin_capitalize(vec![Value::Int(7306)]).unwrap();
        assert_eq!(int_mod_i.as_int(), Some(7306));

        let int_dz_small = builtin_capitalize(vec![Value::Int(452)]).unwrap();
        assert_eq!(int_dz_small.as_int(), Some(453));

        let int_georgian_an = builtin_capitalize(vec![Value::Int(4304)]).unwrap();
        assert_eq!(int_georgian_an.as_int(), Some(4304));

        let string_sharp_s = builtin_capitalize(vec![Value::string("ß")]).unwrap();
        assert_eq!(string_sharp_s.as_str(), Some("Ss"));

        let string_n_preceded = builtin_capitalize(vec![Value::string("\u{0149}")]).unwrap();
        assert_eq!(string_n_preceded.as_str(), Some("\u{02BC}N"));

        let string_j_caron = builtin_capitalize(vec![Value::string("\u{01F0}")]).unwrap();
        assert_eq!(string_j_caron.as_str(), Some("J\u{030C}"));

        let string_greek_dialytika_tonos =
            builtin_capitalize(vec![Value::string("\u{0390}")]).unwrap();
        assert_eq!(string_greek_dialytika_tonos.as_str(), Some("\u{0399}\u{0308}\u{0301}"));

        let string_armenian_small_ligature = builtin_capitalize(vec![Value::string("\u{0587}")]).unwrap();
        assert_eq!(string_armenian_small_ligature.as_str(), Some("\u{0535}\u{0582}"));

        let string_latin_ligature_ff = builtin_capitalize(vec![Value::string("\u{FB00}")]).unwrap();
        assert_eq!(string_latin_ligature_ff.as_str(), Some("Ff"));

        let string_armenian_presentation_ligature =
            builtin_capitalize(vec![Value::string("\u{FB13}")]).unwrap();
        assert_eq!(
            string_armenian_presentation_ligature.as_str(),
            Some("\u{0544}\u{0576}")
        );

        let string_greek_precomposed_prosgegrammeni =
            builtin_capitalize(vec![Value::string("\u{1F88}")]).unwrap();
        assert_eq!(string_greek_precomposed_prosgegrammeni.as_str(), Some("\u{1F88}"));

        let string_greek_small_alpha_ypogegrammeni =
            builtin_capitalize(vec![Value::string("\u{1F80}")]).unwrap();
        assert_eq!(string_greek_small_alpha_ypogegrammeni.as_str(), Some("\u{1F88}"));

        let string_greek_combining_prosgegrammeni =
            builtin_capitalize(vec![Value::string("\u{1FB2}")]).unwrap();
        assert_eq!(
            string_greek_combining_prosgegrammeni.as_str(),
            Some("\u{1FBA}\u{0345}")
        );
    }

    #[test]
    fn upcase_initials_unicode_edge_semantics() {
        let int_sharp_s = builtin_upcase_initials(vec![Value::Int(223)]).unwrap();
        assert_eq!(int_sharp_s.as_int(), Some(7838));

        let int_mod_i = builtin_upcase_initials(vec![Value::Int(7306)]).unwrap();
        assert_eq!(int_mod_i.as_int(), Some(7306));

        let int_dz_small = builtin_upcase_initials(vec![Value::Int(454)]).unwrap();
        assert_eq!(int_dz_small.as_int(), Some(453));

        let int_georgian_an = builtin_upcase_initials(vec![Value::Int(4304)]).unwrap();
        assert_eq!(int_georgian_an.as_int(), Some(4304));

        let string_sharp_s = builtin_upcase_initials(vec![Value::string("ß")]).unwrap();
        assert_eq!(string_sharp_s.as_str(), Some("Ss"));

        let string_n_preceded = builtin_upcase_initials(vec![Value::string("\u{0149}")]).unwrap();
        assert_eq!(string_n_preceded.as_str(), Some("\u{02BC}N"));

        let string_j_caron = builtin_upcase_initials(vec![Value::string("\u{01F0}")]).unwrap();
        assert_eq!(string_j_caron.as_str(), Some("J\u{030C}"));

        let string_greek_dialytika_tonos =
            builtin_upcase_initials(vec![Value::string("\u{0390}")]).unwrap();
        assert_eq!(string_greek_dialytika_tonos.as_str(), Some("\u{0399}\u{0308}\u{0301}"));

        let string_armenian_small_ligature =
            builtin_upcase_initials(vec![Value::string("\u{0587}")]).unwrap();
        assert_eq!(string_armenian_small_ligature.as_str(), Some("\u{0535}\u{0582}"));

        let string_latin_ligature_ff =
            builtin_upcase_initials(vec![Value::string("\u{FB00}")]).unwrap();
        assert_eq!(string_latin_ligature_ff.as_str(), Some("Ff"));

        let string_armenian_presentation_ligature =
            builtin_upcase_initials(vec![Value::string("\u{FB13}")]).unwrap();
        assert_eq!(
            string_armenian_presentation_ligature.as_str(),
            Some("\u{0544}\u{0576}")
        );

        let string_greek_precomposed_prosgegrammeni =
            builtin_upcase_initials(vec![Value::string("\u{1F88}")]).unwrap();
        assert_eq!(string_greek_precomposed_prosgegrammeni.as_str(), Some("\u{1F88}"));

        let string_greek_small_alpha_ypogegrammeni =
            builtin_upcase_initials(vec![Value::string("\u{1F80}")]).unwrap();
        assert_eq!(string_greek_small_alpha_ypogegrammeni.as_str(), Some("\u{1F88}"));

        let string_greek_combining_prosgegrammeni =
            builtin_upcase_initials(vec![Value::string("\u{1FB2}")]).unwrap();
        assert_eq!(
            string_greek_combining_prosgegrammeni.as_str(),
            Some("\u{1FBA}\u{0345}")
        );
    }
}
