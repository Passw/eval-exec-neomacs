//! Keymap system — key binding lookup and command dispatch.
//!
//! Provides an Emacs-compatible keymap system with:
//! - Sparse and full keymaps
//! - Parent (inheritance) chain lookup
//! - Key description parsing (`kbd` style: "C-x C-f", "M-x", "RET", etc.)
//! - Global and local (buffer) keymap support

use super::chartable::{builtin_char_table_range, builtin_make_char_table, builtin_set_char_table_range, is_char_table};
use super::intern::resolve_sym;
use super::keyboard::pure::{KEY_CHAR_CODE_MASK, KEY_CHAR_CTRL, KEY_CHAR_META, KEY_CHAR_SHIFT, KEY_CHAR_SUPER};
use super::value::{read_cons, Value};

// ---------------------------------------------------------------------------
// Key events
// ---------------------------------------------------------------------------

/// A key event — a single keystroke with optional modifiers.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KeyEvent {
    /// A regular character with modifiers.
    Char {
        code: char,
        ctrl: bool,
        meta: bool,
        shift: bool,
        super_: bool,
    },
    /// A named function/special key (e.g. "return", "backspace", "f1").
    Function {
        name: String,
        ctrl: bool,
        meta: bool,
        shift: bool,
        super_: bool,
    },
}



// ---------------------------------------------------------------------------
// Key description parsing  ("kbd" style)
// ---------------------------------------------------------------------------

/// Parse a key description string into a sequence of `KeyEvent`s.
///
/// Supported syntax:
/// - `"C-x"` — Ctrl+x
/// - `"M-x"` — Meta(Alt)+x
/// - `"S-x"` — Shift+x
/// - `"s-x"` — Super+x
/// - `"C-M-x"` — Ctrl+Meta+x
/// - `"C-x C-f"` — sequence of Ctrl+x then Ctrl+f
/// - `"RET"`, `"TAB"`, `"SPC"`, `"ESC"`, `"DEL"`, `"BS"` — named keys
/// - `"f1"` .. `"f12"` — function keys
/// - `"a"`, `"b"`, `"1"`, `"!"` — plain characters
pub fn parse_key_description(desc: &str) -> Result<Vec<KeyEvent>, String> {
    let desc = desc.trim();
    if desc.is_empty() {
        return Err("empty key description".to_string());
    }

    let mut result = Vec::new();
    for part in desc.split_whitespace() {
        result.push(parse_single_key(part)?);
    }
    Ok(result)
}

/// Parse a single key token (e.g. "C-x", "M-RET", "a", "f1").
pub fn parse_single_key(token: &str) -> Result<KeyEvent, String> {
    let mut ctrl = false;
    let mut meta = false;
    let mut shift = false;
    let mut super_ = false;

    let mut remainder = token;

    // Parse modifier prefixes: "C-", "M-", "S-", "s-"
    loop {
        if let Some(rest) = remainder.strip_prefix("C-") {
            ctrl = true;
            remainder = rest;
        } else if let Some(rest) = remainder.strip_prefix("M-") {
            meta = true;
            remainder = rest;
        } else if remainder.starts_with("S-") && remainder.len() > 2 {
            let rest = &remainder[2..];
            shift = true;
            remainder = rest;
        } else if remainder.starts_with("s-") && remainder.len() > 2 {
            let rest = &remainder[2..];
            super_ = true;
            remainder = rest;
        } else {
            break;
        }
    }

    if remainder.is_empty() {
        return Err(format!("incomplete key description: {}", token));
    }

    // Check for named special keys
    match remainder {
        "RET" | "return" => Ok(KeyEvent::Function {
            name: "return".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "TAB" | "tab" => Ok(KeyEvent::Function {
            name: "tab".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "SPC" | "space" => Ok(KeyEvent::Char {
            code: ' ',
            ctrl,
            meta,
            shift,
            super_,
        }),
        "ESC" | "escape" => Ok(KeyEvent::Function {
            name: "escape".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "DEL" | "delete" => Ok(KeyEvent::Function {
            name: "delete".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "BS" | "backspace" => Ok(KeyEvent::Function {
            name: "backspace".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "up" => Ok(KeyEvent::Function {
            name: "up".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "down" => Ok(KeyEvent::Function {
            name: "down".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "left" => Ok(KeyEvent::Function {
            name: "left".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "right" => Ok(KeyEvent::Function {
            name: "right".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "home" => Ok(KeyEvent::Function {
            name: "home".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "end" => Ok(KeyEvent::Function {
            name: "end".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "prior" | "page-up" => Ok(KeyEvent::Function {
            name: "prior".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "next" | "page-down" => Ok(KeyEvent::Function {
            name: "next".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        "insert" => Ok(KeyEvent::Function {
            name: "insert".to_string(),
            ctrl,
            meta,
            shift,
            super_,
        }),
        other => {
            // Check for function keys: f1 .. f20
            if let Some(stripped) = other.strip_prefix('f') {
                if let Ok(n) = stripped.parse::<u32>() {
                    if (1..=20).contains(&n) {
                        return Ok(KeyEvent::Function {
                            name: format!("f{}", n),
                            ctrl,
                            meta,
                            shift,
                            super_,
                        });
                    }
                }
            }

            // Single character
            let mut chars = other.chars();
            let ch = chars
                .next()
                .ok_or_else(|| format!("empty key after modifiers: {}", token))?;
            if chars.next().is_some() {
                return Err(format!("unknown key name: {}", other));
            }
            Ok(KeyEvent::Char {
                code: ch,
                ctrl,
                meta,
                shift,
                super_,
            })
        }
    }
}

/// Format a key event back to a human-readable description string.
pub fn format_key_event(event: &KeyEvent) -> String {
    let mut parts = String::new();
    let (ctrl, meta, shift, super_) = match event {
        KeyEvent::Char {
            ctrl,
            meta,
            shift,
            super_,
            ..
        } => (*ctrl, *meta, *shift, *super_),
        KeyEvent::Function {
            ctrl,
            meta,
            shift,
            super_,
            ..
        } => (*ctrl, *meta, *shift, *super_),
    };
    if ctrl {
        parts.push_str("C-");
    }
    if meta {
        parts.push_str("M-");
    }
    if shift {
        parts.push_str("S-");
    }
    if super_ {
        parts.push_str("s-");
    }
    match event {
        KeyEvent::Char { code: ' ', .. } => {
            parts.push_str("SPC");
        }
        KeyEvent::Char { code, .. } => {
            parts.push(*code);
        }
        KeyEvent::Function { name, .. } => {
            match name.as_str() {
                "return" => parts.push_str("RET"),
                "tab" => parts.push_str("TAB"),
                "escape" => parts.push_str("ESC"),
                "delete" => parts.push_str("DEL"),
                "backspace" => parts.push_str("BS"),
                other => parts.push_str(other),
            }
        }
    }
    parts
}

/// Format a full key sequence.
pub fn format_key_sequence(events: &[KeyEvent]) -> String {
    events
        .iter()
        .map(format_key_event)
        .collect::<Vec<_>>()
        .join(" ")
}

// ===========================================================================
// Emacs-compatible list keymaps
// ===========================================================================
//
// Official Emacs keymap format:
//   Full keymap:   (keymap CHAR-TABLE (EVENT . DEF) (EVENT . DEF) ...)
//   Sparse keymap: (keymap (EVENT . DEF) (EVENT . DEF) ...)
//   With parent:   (keymap (EVENT . DEF) ... . PARENT-KEYMAP)
//
// - `keymapp` checks `(consp x) && (car x) == 'keymap`
// - Char-table stores character bindings (0-MAX_CHAR)
// - Alist stores non-character bindings (function keys, mouse, remap, modified chars)
// - Events: integers (char code with modifier bits) or symbols (function keys)
// - Parent keymap: last CDR in the list, itself a `(keymap ...)` list

/// Create a full list keymap: `(keymap CHAR-TABLE)`
pub fn make_list_keymap() -> Value {
    let char_table = builtin_make_char_table(vec![Value::Nil])
        .expect("make-char-table should not fail");
    Value::list(vec![Value::symbol("keymap"), char_table])
}

/// Create a sparse list keymap: `(keymap)` — a single-element list.
pub fn make_sparse_list_keymap() -> Value {
    Value::list(vec![Value::symbol("keymap")])
}

/// Check if a value is a keymap: `(consp x) && (car x) == 'keymap`.
pub fn is_list_keymap(v: &Value) -> bool {
    match v {
        Value::Cons(cell) => {
            let pair = read_cons(*cell);
            pair.car.as_symbol_name() == Some("keymap")
        }
        _ => false,
    }
}

/// Look up a single event in a keymap, following the parent chain.
///
/// Returns the binding or `Value::Nil` if not found.
pub fn list_keymap_lookup_one(keymap: &Value, event: &Value) -> Value {
    let mut current = *keymap;

    loop {
        let Value::Cons(cell) = current else {
            return Value::Nil;
        };
        let pair = read_cons(cell);
        // First element must be 'keymap
        if pair.car.as_symbol_name() != Some("keymap") {
            return Value::Nil;
        }

        // Walk the CDR chain scanning for the binding
        let mut cursor = pair.cdr;
        while let Value::Cons(entry_cell) = cursor {
            let entry = read_cons(entry_cell);

            // Check if this element is a char-table
            if is_char_table(&entry.car) {
                // For integer events in range, look up in char-table
                if let Value::Int(code) = event {
                    let base = *code & KEY_CHAR_CODE_MASK;
                    if base >= 0 && (base <= 0x3FFFFF) {
                        let result = builtin_char_table_range(vec![entry.car, *event])
                            .unwrap_or(Value::Nil);
                        if !result.is_nil() {
                            return result;
                        }
                    }
                }
                cursor = entry.cdr;
                continue;
            }

            // Check if this element is an alist entry: (EVENT . DEF)
            if let Value::Cons(binding_cell) = entry.car {
                let binding = read_cons(binding_cell);
                if events_match(&binding.car, event) {
                    return binding.cdr;
                }
            }

            cursor = entry.cdr;
        }

        // If last CDR is itself a keymap, follow as parent
        if is_list_keymap(&cursor) {
            current = cursor;
        } else {
            return Value::Nil;
        }
    }
}

/// Check if two event values match for keymap lookup purposes.
fn events_match(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Char(x), Value::Char(y)) => x == y,
        (Value::Int(x), Value::Char(y)) => *x == *y as i64,
        (Value::Char(x), Value::Int(y)) => *x as i64 == *y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        _ => false,
    }
}

/// Define a binding in a keymap.
///
/// For integer events without modifier bits in full keymaps: stores in char-table.
/// Otherwise: prepends `(event . def)` to the alist portion.
pub fn list_keymap_define(keymap: Value, event: Value, def: Value) {
    let Value::Cons(root_cell) = keymap else {
        return;
    };
    let root = read_cons(root_cell);
    if root.car.as_symbol_name() != Some("keymap") {
        return;
    }

    // Check if full keymap (second element is char-table) and event is plain char
    let cdr = root.cdr;
    if let Value::Cons(second_cell) = cdr {
        let second = read_cons(second_cell);
        if is_char_table(&second.car) {
            // For plain character events (no modifier bits), use char-table
            if let Value::Int(code) = event {
                let base = code & KEY_CHAR_CODE_MASK;
                let mods = code & !KEY_CHAR_CODE_MASK;
                if mods == 0 && base >= 0 && base <= 0x3FFFFF {
                    let _ = builtin_set_char_table_range(vec![second.car, event, def]);
                    return;
                }
            }
            // For non-char events: prepend after char-table
            let binding = Value::cons(event, def);
            let new_cdr = Value::cons(binding, second.cdr);
            Value::Cons(second_cell).set_cdr(new_cdr);
            return;
        }
    }

    // Sparse keymap: prepend (event . def) right after 'keymap symbol
    let binding = Value::cons(event, def);
    let new_cdr = Value::cons(binding, cdr);
    Value::Cons(root_cell).set_cdr(new_cdr);
}

/// Get the parent keymap (last CDR that is itself a keymap).
pub fn list_keymap_parent(keymap: &Value) -> Value {
    let Value::Cons(cell) = keymap else {
        return Value::Nil;
    };
    let pair = read_cons(*cell);
    if pair.car.as_symbol_name() != Some("keymap") {
        return Value::Nil;
    }

    let mut cursor = pair.cdr;
    while let Value::Cons(entry_cell) = cursor {
        // Check if cursor itself is a parent keymap before treating as alist entry
        if is_list_keymap(&cursor) {
            return cursor;
        }
        let entry = read_cons(entry_cell);
        if entry.cdr.is_nil() {
            return Value::Nil;
        }
        cursor = entry.cdr;
    }
    Value::Nil
}

/// Set the parent keymap: walk to the last alist cons cell, set its CDR.
pub fn list_keymap_set_parent(keymap: Value, parent: Value) {
    let Value::Cons(root_cell) = keymap else {
        return;
    };
    let root = read_cons(root_cell);
    if root.car.as_symbol_name() != Some("keymap") {
        return;
    }

    // Find the last cons cell in the keymap list
    let mut prev_cell_value = Value::Cons(root_cell);
    let mut cursor = root.cdr;
    loop {
        match cursor {
            Value::Cons(cell) => {
                let entry = read_cons(cell);
                // If cdr is a keymap (existing parent) or nil, we replace it
                if is_list_keymap(&entry.cdr) || entry.cdr.is_nil() {
                    Value::Cons(cell).set_cdr(parent);
                    return;
                }
                prev_cell_value = Value::Cons(cell);
                cursor = entry.cdr;
            }
            _ => {
                // cursor is either nil or an existing parent keymap
                // Set previous cell's cdr to the new parent
                prev_cell_value.set_cdr(parent);
                return;
            }
        }
    }
}

/// Convert a `KeyEvent` to an Emacs event value (integer with modifier bits, or symbol).
pub fn key_event_to_emacs_event(event: &KeyEvent) -> Value {
    match event {
        KeyEvent::Char { code, ctrl, meta, shift, super_ } => {
            let mut bits = *code as i64;
            if *ctrl { bits |= KEY_CHAR_CTRL; }
            if *meta { bits |= KEY_CHAR_META; }
            if *shift { bits |= KEY_CHAR_SHIFT; }
            if *super_ { bits |= KEY_CHAR_SUPER; }
            Value::Int(bits)
        }
        KeyEvent::Function { name, ctrl, meta, shift, super_ } => {
            let mut prefix = String::new();
            if *ctrl { prefix.push_str("C-"); }
            if *meta { prefix.push_str("M-"); }
            if *shift { prefix.push_str("S-"); }
            if *super_ { prefix.push_str("s-"); }
            Value::symbol(format!("{}{}", prefix, name))
        }
    }
}

/// Convert an Emacs event value to a `KeyEvent`.
pub fn emacs_event_to_key_event(event: &Value) -> Option<KeyEvent> {
    match event {
        Value::Int(code) => {
            let base = *code & KEY_CHAR_CODE_MASK;
            let ch = char::from_u32(base as u32)?;
            Some(KeyEvent::Char {
                code: ch,
                ctrl: (*code & KEY_CHAR_CTRL) != 0,
                meta: (*code & KEY_CHAR_META) != 0,
                shift: (*code & KEY_CHAR_SHIFT) != 0,
                super_: (*code & KEY_CHAR_SUPER) != 0,
            })
        }
        Value::Char(c) => Some(KeyEvent::Char {
            code: *c,
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        }),
        Value::Symbol(id) => {
            let name = resolve_sym(*id);
            // Parse modifier prefixes
            let mut rest = name;
            let mut ctrl = false;
            let mut meta = false;
            let mut shift = false;
            let mut super_ = false;
            loop {
                if let Some(r) = rest.strip_prefix("C-") { ctrl = true; rest = r; continue; }
                if let Some(r) = rest.strip_prefix("M-") { meta = true; rest = r; continue; }
                if let Some(r) = rest.strip_prefix("S-") { shift = true; rest = r; continue; }
                if let Some(r) = rest.strip_prefix("s-") { super_ = true; rest = r; continue; }
                break;
            }
            // If single char, return Char event
            let mut chars = rest.chars();
            if let Some(ch) = chars.next() {
                if chars.next().is_none() {
                    return Some(KeyEvent::Char { code: ch, ctrl, meta, shift, super_ });
                }
            }
            // Otherwise it's a function key
            Some(KeyEvent::Function { name: rest.to_string(), ctrl, meta, shift, super_ })
        }
        _ => None,
    }
}

/// Look up a key sequence in a keymap, following prefix keymaps and parent chains.
/// Returns the binding Value, or the number of keys matched (as `Value::Int`)
/// when the sequence resolves through a non-keymap binding.
pub fn list_keymap_lookup_seq(keymap: &Value, events: &[Value]) -> Value {
    if events.is_empty() {
        return *keymap;
    }

    let mut current_map = *keymap;
    for (i, event) in events.iter().enumerate() {
        let binding = list_keymap_lookup_one(&current_map, event);
        if binding.is_nil() {
            return if i == 0 { Value::Int(1) } else { Value::Nil };
        }
        if i == events.len() - 1 {
            return binding;
        }
        // Must be a prefix keymap to continue
        if is_list_keymap(&binding) {
            current_map = binding;
        } else {
            // Check if it's a symbol whose function cell is a keymap
            if let Some(sym_name) = binding.as_symbol_name() {
                // We can't resolve symbol function cells from keymap.rs —
                // caller must handle this case. For now treat as non-prefix.
                let _ = sym_name;
            }
            return Value::Int((i + 1) as i64);
        }
    }
    Value::Nil
}

/// Define a key in a keymap, auto-creating prefix maps for multi-key sequences.
pub fn list_keymap_define_seq(keymap: Value, events: &[Value], def: Value) {
    if events.is_empty() {
        return;
    }
    if events.len() == 1 {
        list_keymap_define(keymap, events[0], def);
        return;
    }

    let mut current_map = keymap;
    for (i, event) in events.iter().enumerate() {
        if i == events.len() - 1 {
            list_keymap_define(current_map, *event, def);
            return;
        }
        let binding = list_keymap_lookup_one(&current_map, event);
        if is_list_keymap(&binding) {
            current_map = binding;
        } else {
            // Create a new prefix keymap
            let prefix_map = make_sparse_list_keymap();
            list_keymap_define(current_map, *event, prefix_map);
            current_map = prefix_map;
        }
    }
}

/// Deep-copy a keymap cons-list structure.
pub fn list_keymap_copy(keymap: &Value) -> Value {
    let Value::Cons(cell) = keymap else {
        return *keymap;
    };
    let pair = read_cons(*cell);
    if pair.car.as_symbol_name() != Some("keymap") {
        return *keymap;
    }

    let mut elements = vec![Value::symbol("keymap")];
    let mut cursor = pair.cdr;
    let mut tail_parent = Value::Nil;

    while let Value::Cons(entry_cell) = cursor {
        let entry = read_cons(entry_cell);

        if is_char_table(&entry.car) {
            // Copy char-table: for now we share (char-tables are mutable vectors)
            // A proper deep copy would require cloning the vector
            elements.push(entry.car);
        } else if is_list_keymap(&entry.car) {
            // Nested keymap in an alist entry — recursively copy
            elements.push(list_keymap_copy(&entry.car));
        } else if let Value::Cons(binding_cell) = entry.car {
            // Alist entry (EVENT . DEF) — copy the cons, recurse if DEF is a keymap
            let binding = read_cons(binding_cell);
            if is_list_keymap(&binding.cdr) {
                elements.push(Value::cons(binding.car, list_keymap_copy(&binding.cdr)));
            } else {
                elements.push(Value::cons(binding.car, binding.cdr));
            }
        } else {
            elements.push(entry.car);
        }

        // Check if cdr is a parent keymap
        if is_list_keymap(&entry.cdr) {
            // Keep parent shared (don't recursively copy parent chain)
            tail_parent = entry.cdr;
            break;
        }
        cursor = entry.cdr;
    }

    // Build the new list
    let mut result = tail_parent;
    for elem in elements.into_iter().rev() {
        result = Value::cons(elem, result);
    }
    result
}

/// Collect all accessible sub-keymaps with their key prefixes.
pub fn list_keymap_accessible(
    keymap: &Value,
    prefix: &mut Vec<Value>,
    out: &mut Vec<Value>,
    seen: &mut Vec<Value>,
) {
    // Detect cycles: check if we've seen this exact keymap object
    for s in seen.iter() {
        if keymap_value_eq(s, keymap) {
            return;
        }
    }
    seen.push(*keymap);

    // Add current keymap
    out.push(Value::cons(
        Value::vector(prefix.clone()),
        *keymap,
    ));

    let Value::Cons(cell) = keymap else { return; };
    let pair = read_cons(*cell);
    if pair.car.as_symbol_name() != Some("keymap") {
        return;
    }

    // Scan alist entries for prefix keymaps
    let mut cursor = pair.cdr;
    while let Value::Cons(entry_cell) = cursor {
        let entry = read_cons(entry_cell);

        if let Value::Cons(binding_cell) = entry.car {
            let binding = read_cons(binding_cell);
            if is_list_keymap(&binding.cdr) {
                prefix.push(binding.car);
                list_keymap_accessible(&binding.cdr, prefix, out, seen);
                prefix.pop();
            }
        }

        if is_list_keymap(&entry.cdr) {
            break; // parent keymap, don't descend
        }
        cursor = entry.cdr;
    }

    seen.pop();
}

/// Check if two keymap values are the same object (by cons cell identity).
fn keymap_value_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Cons(x), Value::Cons(y)) => x == y,
        _ => false,
    }
}

/// Iterate over all bindings in a keymap (not following parent).
/// Calls `f(event, def)` for each binding.
pub fn list_keymap_for_each_binding<F>(keymap: &Value, mut f: F)
where
    F: FnMut(Value, Value),
{
    let Value::Cons(cell) = keymap else { return; };
    let pair = read_cons(*cell);
    if pair.car.as_symbol_name() != Some("keymap") {
        return;
    }

    let mut cursor = pair.cdr;
    while let Value::Cons(entry_cell) = cursor {
        let entry = read_cons(entry_cell);

        // Skip char-tables (we'd need to enumerate them, which is complex)
        // For now, only iterate alist entries
        if let Value::Cons(binding_cell) = entry.car {
            let binding = read_cons(binding_cell);
            f(binding.car, binding.cdr);
        }

        if is_list_keymap(&entry.cdr) {
            break;
        }
        cursor = entry.cdr;
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Key description parsing tests --

    #[test]
    fn parse_plain_char() {
        let keys = parse_key_description("a").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Char {
                code: 'a',
                ctrl: false,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_ctrl_x() {
        let keys = parse_key_description("C-x").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Char {
                code: 'x',
                ctrl: true,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_meta_x() {
        let keys = parse_key_description("M-x").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Char {
                code: 'x',
                ctrl: false,
                meta: true,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_ctrl_x_ctrl_f_sequence() {
        let keys = parse_key_description("C-x C-f").unwrap();
        assert_eq!(keys.len(), 2);
        assert_eq!(
            keys[0],
            KeyEvent::Char {
                code: 'x',
                ctrl: true,
                meta: false,
                shift: false,
                super_: false,
            }
        );
        assert_eq!(
            keys[1],
            KeyEvent::Char {
                code: 'f',
                ctrl: true,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_ret() {
        let keys = parse_key_description("RET").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Function {
                name: "return".to_string(),
                ctrl: false,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_tab() {
        let keys = parse_key_description("TAB").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Function {
                name: "tab".to_string(),
                ctrl: false,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_spc() {
        let keys = parse_key_description("SPC").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Char {
                code: ' ',
                ctrl: false,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_combined_modifiers() {
        let keys = parse_key_description("C-M-s").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Char {
                code: 's',
                ctrl: true,
                meta: true,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_function_key() {
        let keys = parse_key_description("f1").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Function {
                name: "f1".to_string(),
                ctrl: false,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_ctrl_function_key() {
        let keys = parse_key_description("C-f12").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Function {
                name: "f12".to_string(),
                ctrl: true,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    #[test]
    fn parse_error_empty() {
        assert!(parse_key_description("").is_err());
    }

    #[test]
    fn parse_error_unknown_name() {
        assert!(parse_key_description("foobar").is_err());
    }

    #[test]
    fn format_key_event_roundtrip() {
        let cases = vec![
            "C-x", "M-x", "C-M-s", "a", "SPC", "RET", "TAB", "f1", "C-f12",
        ];
        for desc in cases {
            let keys = parse_key_description(desc).unwrap();
            assert_eq!(keys.len(), 1, "expected single key for {}", desc);
            let formatted = format_key_event(&keys[0]);
            let reparsed = parse_key_description(&formatted).unwrap();
            assert_eq!(
                keys[0], reparsed[0],
                "roundtrip mismatch for {}: formatted as {}, reparsed as {:?}",
                desc, formatted, reparsed[0]
            );
        }
    }

    #[test]
    fn format_key_sequence_roundtrip() {
        let desc = "C-x C-f";
        let keys = parse_key_description(desc).unwrap();
        let formatted = format_key_sequence(&keys);
        assert_eq!(formatted, "C-x C-f");
    }

    #[test]
    fn parse_arrow_keys() {
        for name in &["up", "down", "left", "right"] {
            let keys = parse_key_description(name).unwrap();
            assert_eq!(keys.len(), 1);
            match &keys[0] {
                KeyEvent::Function { name: n, .. } => assert_eq!(n.as_str(), *name),
                other => panic!("expected Function for {}, got {:?}", name, other),
            }
        }
    }

    #[test]
    fn parse_modifier_with_named_key() {
        let keys = parse_key_description("C-RET").unwrap();
        assert_eq!(keys.len(), 1);
        assert_eq!(
            keys[0],
            KeyEvent::Function {
                name: "return".to_string(),
                ctrl: true,
                meta: false,
                shift: false,
                super_: false,
            }
        );
    }

    // -- List keymap tests --

    #[test]
    fn list_keymap_create_and_check() {
        let km = make_list_keymap();
        assert!(is_list_keymap(&km));
        let sparse = make_sparse_list_keymap();
        assert!(is_list_keymap(&sparse));
        assert!(!is_list_keymap(&Value::Nil));
        assert!(!is_list_keymap(&Value::Int(42)));
    }

    #[test]
    fn list_keymap_define_and_lookup() {
        let km = make_sparse_list_keymap();
        let event = Value::symbol("return");
        list_keymap_define(km, event, Value::symbol("newline"));
        let result = list_keymap_lookup_one(&km, &event);
        assert_eq!(result.as_symbol_name(), Some("newline"));
    }

    #[test]
    fn list_keymap_parent_chain() {
        let parent = make_sparse_list_keymap();
        let child = make_sparse_list_keymap();
        list_keymap_set_parent(child, parent);
        assert!(is_list_keymap(&list_keymap_parent(&child)));

        // Binding in parent is found via child
        let event = Value::Int(97); // 'a'
        list_keymap_define(parent, event, Value::symbol("cmd-a"));
        let result = list_keymap_lookup_one(&child, &event);
        assert_eq!(result.as_symbol_name(), Some("cmd-a"));
    }

    #[test]
    fn list_keymap_child_overrides_parent() {
        let parent = make_sparse_list_keymap();
        let child = make_sparse_list_keymap();
        list_keymap_set_parent(child, parent);

        let event = Value::Int(120); // 'x'
        list_keymap_define(parent, event, Value::symbol("parent-cmd"));
        list_keymap_define(child, event, Value::symbol("child-cmd"));
        let result = list_keymap_lookup_one(&child, &event);
        assert_eq!(result.as_symbol_name(), Some("child-cmd"));
    }

    #[test]
    fn list_keymap_event_conversion_roundtrip() {
        let key = KeyEvent::Char {
            code: 'x',
            ctrl: true,
            meta: false,
            shift: false,
            super_: false,
        };
        let emacs_event = key_event_to_emacs_event(&key);
        let roundtrip = emacs_event_to_key_event(&emacs_event).unwrap();
        assert_eq!(key, roundtrip);
    }
}
