//! `kbd` string parser and event encoder (compatibility subset).
//!
//! This module implements the common `kbd` behaviors used by vm-compat:
//! - plain tokens expand to character sequences,
//! - modifier prefixes (`C-`, `M-`, `S-`, `s-`) on single characters,
//! - angle-bracket symbolic events (`<f1>`, `C-<return>`, ...),
//! - string return when all events are plain chars, otherwise vector.

use super::{keymap::KeyEvent, value::{Value, with_heap}};

const CHAR_META: i64 = 0x8000000;
const CHAR_CTL: i64 = 0x4000000;
const CHAR_SHIFT: i64 = 0x2000000;
const CHAR_SUPER: i64 = 0x0800000;
const CHAR_MODIFIER_MASK: i64 = CHAR_META | CHAR_CTL | CHAR_SHIFT | CHAR_SUPER;

#[derive(Clone, Debug)]
pub(crate) enum KeyDesignatorError {
    WrongType(Value),
    Parse(String),
}

#[derive(Clone, Copy, Default)]
struct Modifiers {
    ctrl: bool,
    meta: bool,
    shift: bool,
    super_: bool,
}

impl Modifiers {
    fn any(self) -> bool {
        self.ctrl || self.meta || self.shift || self.super_
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum EncodedEvent {
    Char(char),
    Int(i64),
    Symbol(String),
}

pub(crate) fn parse_kbd_string(desc: &str) -> Result<Value, String> {
    let trimmed = desc.trim();
    if trimmed.is_empty() {
        return Ok(Value::string(""));
    }

    let mut encoded = Vec::new();
    for token in trimmed.split_whitespace() {
        parse_token(token, &mut encoded)?;
    }

    if encoded.iter().all(|e| matches!(e, EncodedEvent::Char(_))) {
        let s: String = encoded
            .into_iter()
            .map(|event| match event {
                EncodedEvent::Char(c) => c,
                _ => unreachable!("guarded by all(Char)"),
            })
            .collect();
        return Ok(Value::string(s));
    }

    let values = encoded
        .into_iter()
        .map(|event| match event {
            EncodedEvent::Char(c) => Value::Int(c as i64),
            EncodedEvent::Int(n) => Value::Int(n),
            EncodedEvent::Symbol(name) => Value::symbol(name),
        })
        .collect();
    Ok(Value::vector(values))
}

pub(crate) fn key_events_from_designator(
    designator: &Value,
) -> Result<Vec<KeyEvent>, KeyDesignatorError> {
    match designator {
        Value::Str(s) => {
            let encoded = parse_kbd_string(s).map_err(KeyDesignatorError::Parse)?;
            decode_encoded_key_events(&encoded).map_err(KeyDesignatorError::Parse)
        }
        Value::Vector(_) => {
            decode_encoded_key_events(designator).map_err(KeyDesignatorError::Parse)
        }
        other => Err(KeyDesignatorError::WrongType(other.clone())),
    }
}

fn decode_encoded_key_events(encoded: &Value) -> Result<Vec<KeyEvent>, String> {
    match encoded {
        Value::Str(s) => Ok(s
            .chars()
            .map(|ch| KeyEvent::Char {
                code: ch,
                ctrl: false,
                meta: false,
                shift: false,
                super_: false,
            })
            .collect()),
        Value::Vector(v) => {
            let items = with_heap(|h| h.get_vector(*v).clone());
            items.iter().map(decode_vector_event).collect()
        }
        other => Err(format!(
            "expected kbd-encoded string or vector, got {}",
            other.type_name()
        )),
    }
}

fn decode_vector_event(item: &Value) -> Result<KeyEvent, String> {
    match item {
        Value::Int(n) => decode_int_event(*n),
        Value::Char(ch) => Ok(KeyEvent::Char {
            code: *ch,
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        }),
        Value::Symbol(name) => decode_symbol_event(name),
        Value::Nil => decode_symbol_event("nil"),
        Value::True => decode_symbol_event("t"),
        other => Err(format!(
            "invalid key vector element type: {}",
            other.type_name()
        )),
    }
}

fn decode_int_event(code: i64) -> Result<KeyEvent, String> {
    let mods = code & CHAR_MODIFIER_MASK;
    let base = code & !CHAR_MODIFIER_MASK;
    if !(0..=0x10FFFF).contains(&base) {
        return Err(format!("invalid key event code: {code}"));
    }
    let ch =
        char::from_u32(base as u32).ok_or_else(|| format!("invalid key event code: {code}"))?;
    Ok(KeyEvent::Char {
        code: ch,
        ctrl: (mods & CHAR_CTL) != 0,
        meta: (mods & CHAR_META) != 0,
        shift: (mods & CHAR_SHIFT) != 0,
        super_: (mods & CHAR_SUPER) != 0,
    })
}

fn decode_symbol_event(symbol: &str) -> Result<KeyEvent, String> {
    let (mods, _prefix, remainder) = parse_modifiers(symbol);
    if remainder.is_empty() {
        return Err("invalid empty key symbol".to_string());
    }
    Ok(KeyEvent::Function {
        name: remainder.to_string(),
        ctrl: mods.ctrl,
        meta: mods.meta,
        shift: mods.shift,
        super_: mods.super_,
    })
}

fn parse_token(token: &str, out: &mut Vec<EncodedEvent>) -> Result<(), String> {
    let (mods, prefix, remainder) = parse_modifiers(token);

    if let Some(name) = parse_angle_symbol(remainder) {
        out.push(EncodedEvent::Symbol(format!("{prefix}{name}")));
        return Ok(());
    }

    if let Some(ch) = named_char_token(remainder) {
        out.push(encode_char(ch, mods, false));
        return Ok(());
    }

    if let Some(ch) = single_char(remainder) {
        out.push(encode_char(ch, mods, true));
        return Ok(());
    }

    if mods.any() {
        return Err(format!(
            "{prefix} must prefix a single character, not {remainder}"
        ));
    }

    out.extend(remainder.chars().map(EncodedEvent::Char));
    Ok(())
}

fn parse_modifiers(mut token: &str) -> (Modifiers, String, &str) {
    let mut mods = Modifiers::default();
    let mut prefix = String::new();

    loop {
        if let Some(rest) = token.strip_prefix("C-") {
            if rest.is_empty() {
                break;
            }
            mods.ctrl = true;
            prefix.push_str("C-");
            token = rest;
            continue;
        }
        if let Some(rest) = token.strip_prefix("M-") {
            if rest.is_empty() {
                break;
            }
            mods.meta = true;
            prefix.push_str("M-");
            token = rest;
            continue;
        }
        if let Some(rest) = token.strip_prefix("S-") {
            if rest.is_empty() {
                break;
            }
            mods.shift = true;
            prefix.push_str("S-");
            token = rest;
            continue;
        }
        if let Some(rest) = token.strip_prefix("s-") {
            if rest.is_empty() {
                break;
            }
            mods.super_ = true;
            prefix.push_str("s-");
            token = rest;
            continue;
        }
        break;
    }

    (mods, prefix, token)
}

fn parse_angle_symbol(token: &str) -> Option<&str> {
    let inner = token.strip_prefix('<')?.strip_suffix('>')?;
    if inner.is_empty() {
        None
    } else {
        Some(inner)
    }
}

fn named_char_token(token: &str) -> Option<char> {
    match token {
        "RET" | "return" => Some('\r'),
        "TAB" | "tab" => Some('\t'),
        "SPC" | "space" => Some(' '),
        "ESC" | "escape" => Some('\u{1b}'),
        "DEL" | "delete" => Some('\u{7f}'),
        _ => None,
    }
}

fn single_char(token: &str) -> Option<char> {
    let mut chars = token.chars();
    let ch = chars.next()?;
    if chars.next().is_none() {
        Some(ch)
    } else {
        None
    }
}

fn encode_char(ch: char, mods: Modifiers, allow_ctrl_resolution: bool) -> EncodedEvent {
    if !mods.any() {
        return EncodedEvent::Char(ch);
    }

    let mut base = ch as i64;
    let mut ctrl = mods.ctrl;

    if ctrl && allow_ctrl_resolution {
        if let Some(resolved) = resolve_control_char(ch) {
            base = resolved;
            ctrl = false;
        }
    }

    if !mods.meta && !mods.shift && !mods.super_ && !ctrl {
        if let Some(resolved) = char::from_u32(base as u32) {
            return EncodedEvent::Char(resolved);
        }
        return EncodedEvent::Int(base);
    }

    let mut code = base;
    if mods.meta {
        code |= CHAR_META;
    }
    if ctrl {
        code |= CHAR_CTL;
    }
    if mods.shift {
        code |= CHAR_SHIFT;
    }
    if mods.super_ {
        code |= CHAR_SUPER;
    }
    EncodedEvent::Int(code)
}

fn resolve_control_char(ch: char) -> Option<i64> {
    if ch.is_ascii_alphabetic() {
        return Some(((ch.to_ascii_uppercase() as u8) & 0x1F) as i64);
    }
    if ('@'..='_').contains(&ch) && ch != '?' {
        return Some(((ch as u8) & 0x1F) as i64);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expect_vector_ints(value: Value) -> Vec<i64> {
        match value {
            Value::Vector(v) => {
                let guard = with_heap(|h| h.get_vector(v).clone());
                guard
                    .iter()
                    .map(|item| match item {
                        Value::Int(n) => *n,
                        other => panic!("expected int in vector, got {other:?}"),
                    })
                    .collect()
            }
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn empty_kbd_string_returns_empty_string() {
        let result = parse_kbd_string("   ").expect("parse should succeed");
        assert_eq!(result.as_str(), Some(""));
    }

    #[test]
    fn kbd_ctrl_char_returns_string() {
        let result = parse_kbd_string("C-a").expect("parse should succeed");
        assert_eq!(result.as_str(), Some("\u{1}"));
    }

    #[test]
    fn kbd_ctrl_sequence_returns_string() {
        let result = parse_kbd_string("C-x C-f").expect("parse should succeed");
        assert_eq!(result.as_str(), Some("\u{18}\u{6}"));
    }

    #[test]
    fn kbd_meta_char_returns_vector() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = parse_kbd_string("M-x").expect("parse should succeed");
        assert_eq!(expect_vector_ints(result), vec![134_217_848]);
    }

    #[test]
    fn kbd_ctrl_meta_char_returns_vector() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = parse_kbd_string("C-M-a").expect("parse should succeed");
        assert_eq!(expect_vector_ints(result), vec![134_217_729]);
    }

    #[test]
    fn kbd_named_keys_without_modifiers_return_chars() {
        assert_eq!(
            parse_kbd_string("RET").expect("RET parse").as_str(),
            Some("\r")
        );
        assert_eq!(
            parse_kbd_string("TAB").expect("TAB parse").as_str(),
            Some("\t")
        );
        assert_eq!(
            parse_kbd_string("ESC").expect("ESC parse").as_str(),
            Some("\u{1b}")
        );
        assert_eq!(
            parse_kbd_string("DEL").expect("DEL parse").as_str(),
            Some("\u{7f}")
        );
    }

    #[test]
    fn kbd_named_keys_with_modifiers_return_modifier_encoded_ints() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        assert_eq!(
            expect_vector_ints(parse_kbd_string("C-RET").expect("C-RET parse")),
            vec![67_108_877]
        );
        assert_eq!(
            expect_vector_ints(parse_kbd_string("C-SPC").expect("C-SPC parse")),
            vec![67_108_896]
        );
    }

    #[test]
    fn kbd_plain_multi_char_token_expands_into_plain_string() {
        let result = parse_kbd_string("f1").expect("parse should succeed");
        assert_eq!(result.as_str(), Some("f1"));
    }

    #[test]
    fn kbd_mixed_sequence_returns_vector_with_plain_char_codes() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = parse_kbd_string("a M-b").expect("parse should succeed");
        assert_eq!(expect_vector_ints(result), vec![97, 134_217_826]);
    }

    #[test]
    fn kbd_angle_events_return_symbols() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = parse_kbd_string("<f1>").expect("parse should succeed");
        match result {
            Value::Vector(v) => {
                let guard = with_heap(|h| h.get_vector(v).clone());
                assert_eq!(guard.len(), 1);
                assert_eq!(guard[0], Value::symbol("f1"));
            }
            other => panic!("expected vector, got {other:?}"),
        }

        let result = parse_kbd_string("C-<f1>").expect("parse should succeed");
        match result {
            Value::Vector(v) => {
                let guard = with_heap(|h| h.get_vector(v).clone());
                assert_eq!(guard.len(), 1);
                assert_eq!(guard[0], Value::symbol("C-f1"));
            }
            other => panic!("expected vector, got {other:?}"),
        }
    }

    #[test]
    fn kbd_modifier_plus_multi_char_token_signals_error() {
        let err = parse_kbd_string("C-f1").expect_err("C-f1 should fail");
        assert_eq!(err, "C- must prefix a single character, not f1");
    }

    #[test]
    fn kbd_modifier_chain_uses_consumed_prefix_in_error() {
        let err = parse_kbd_string("C-M-BS").expect_err("C-M-BS should fail");
        assert_eq!(err, "C-M- must prefix a single character, not BS");
    }

    #[test]
    fn key_events_from_designator_accepts_kbd_string_and_vector() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let from_string = key_events_from_designator(&Value::string("M-x")).expect("decode string");
        assert_eq!(
            from_string,
            vec![KeyEvent::Char {
                code: 'x',
                ctrl: false,
                meta: true,
                shift: false,
                super_: false,
            }]
        );

        let from_vector = key_events_from_designator(&Value::vector(vec![Value::Int(134_217_848)]))
            .expect("decode vector int");
        assert_eq!(from_vector, from_string);
    }

    #[test]
    fn key_events_from_designator_decodes_symbol_events() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let events = key_events_from_designator(&Value::vector(vec![Value::symbol("C-f1")]))
            .expect("decode symbol");
        assert_eq!(
            events,
            vec![KeyEvent::Function {
                name: "f1".to_string(),
                ctrl: true,
                meta: false,
                shift: false,
                super_: false,
            }]
        );
    }

    #[test]
    fn key_events_from_designator_rejects_non_array_types() {
        let err = key_events_from_designator(&Value::Int(1)).expect_err("int should fail");
        match err {
            KeyDesignatorError::WrongType(v) => assert_eq!(v, Value::Int(1)),
            other => panic!("expected WrongType error, got {other:?}"),
        }
    }
}
