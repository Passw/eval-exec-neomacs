//! Keymap system — key binding lookup and command dispatch.
//!
//! Provides an Emacs-compatible keymap system with:
//! - Sparse and full keymaps
//! - Parent (inheritance) chain lookup
//! - Key description parsing (`kbd` style: "C-x C-f", "M-x", "RET", etc.)
//! - Global and local (buffer) keymap support

use std::collections::HashMap;

use super::value::Value;

// ---------------------------------------------------------------------------
// Keymap handle encoding
// ---------------------------------------------------------------------------

/// Public Lisp-visible keymap handles are tagged integers rather than raw ids.
/// This avoids collisions with ordinary small integers in predicates like
/// `keymapp`.
pub const KEYMAP_HANDLE_BASE: i64 = 1_i64 << 50;

pub fn encode_keymap_handle(id: u64) -> i64 {
    KEYMAP_HANDLE_BASE + id as i64
}

pub fn decode_keymap_handle(raw: i64) -> Option<u64> {
    if raw < KEYMAP_HANDLE_BASE {
        return None;
    }
    Some((raw - KEYMAP_HANDLE_BASE) as u64)
}

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
// Key bindings
// ---------------------------------------------------------------------------

/// What a key is bound to.
#[derive(Clone, Debug)]
pub enum KeyBinding {
    /// A command to execute, identified by symbol name.
    Command(String),
    /// A prefix key leading to another keymap.
    Prefix(u64),
    /// An arbitrary Lisp value (lambda, etc.).
    LispValue(Value),
}

// ---------------------------------------------------------------------------
// Keymap
// ---------------------------------------------------------------------------

/// A single keymap — either sparse or full.
#[derive(Clone, Debug)]
pub struct Keymap {
    /// Unique identifier for this keymap.
    pub id: u64,
    /// Optional parent keymap for inheritance.
    pub parent: Option<u64>,
    /// Per-key bindings.
    pub bindings: HashMap<KeyEvent, KeyBinding>,
    /// Default binding (used when no specific binding matches).
    pub default_binding: Option<Box<KeyBinding>>,
    /// Human-readable name (for sparse keymaps created with a name).
    pub name: Option<String>,
}

impl Keymap {
    fn new(id: u64) -> Self {
        Self {
            id,
            parent: None,
            bindings: HashMap::new(),
            default_binding: None,
            name: None,
        }
    }

    fn new_with_name(id: u64, name: Option<String>) -> Self {
        Self {
            id,
            parent: None,
            bindings: HashMap::new(),
            default_binding: None,
            name,
        }
    }

    /// Look up a key by its description string (e.g. "C-x", "M-f", "a").
    /// Returns the bound `Value` if found, or `None`.
    pub fn lookup(&self, key_desc: &str) -> Option<Value> {
        let events = KeymapManager::parse_key_description(key_desc).ok()?;
        if events.len() != 1 {
            return None;
        }
        match self.bindings.get(&events[0])? {
            KeyBinding::Command(name) => Some(Value::Symbol(name.clone())),
            KeyBinding::LispValue(v) => Some(v.clone()),
            KeyBinding::Prefix(id) => Some(Value::Symbol(format!("keymap-{}", id))),
        }
    }
}

// ---------------------------------------------------------------------------
// KeymapManager
// ---------------------------------------------------------------------------

/// Central registry for all keymaps.
pub struct KeymapManager {
    keymaps: HashMap<u64, Keymap>,
    next_id: u64,
    global_map: Option<u64>,
}

impl KeymapManager {
    pub fn new() -> Self {
        Self {
            keymaps: HashMap::new(),
            next_id: 1,
            global_map: None,
        }
    }

    /// Allocate the next keymap id and bump the counter.
    fn alloc_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// Create a new full keymap (like Emacs `make-keymap`).
    pub fn make_keymap(&mut self) -> u64 {
        let id = self.alloc_id();
        self.keymaps.insert(id, Keymap::new(id));
        id
    }

    /// Create a sparse keymap (like Emacs `make-sparse-keymap`).
    pub fn make_sparse_keymap(&mut self, name: Option<String>) -> u64 {
        let id = self.alloc_id();
        self.keymaps.insert(id, Keymap::new_with_name(id, name));
        id
    }

    /// Look up a keymap by id.
    pub fn get(&self, id: u64) -> Option<&Keymap> {
        self.keymaps.get(&id)
    }

    /// Look up a keymap mutably by id.
    pub fn get_mut(&mut self, id: u64) -> Option<&mut Keymap> {
        self.keymaps.get_mut(&id)
    }

    /// Return true if `id` refers to a valid keymap.
    pub fn is_keymap(&self, id: u64) -> bool {
        self.keymaps.contains_key(&id)
    }

    /// Define a key binding in a keymap.
    pub fn define_key(&mut self, keymap_id: u64, key: KeyEvent, binding: KeyBinding) {
        if let Some(km) = self.keymaps.get_mut(&keymap_id) {
            km.bindings.insert(key, binding);
        }
    }

    /// Look up a single key event in a keymap, following the parent chain.
    pub fn lookup_key(&self, keymap_id: u64, key: &KeyEvent) -> Option<&KeyBinding> {
        let mut current = Some(keymap_id);
        while let Some(id) = current {
            if let Some(km) = self.keymaps.get(&id) {
                if let Some(binding) = km.bindings.get(key) {
                    return Some(binding);
                }
                if let Some(ref default) = km.default_binding {
                    return Some(default.as_ref());
                }
                current = km.parent;
            } else {
                break;
            }
        }
        None
    }

    /// Look up a sequence of key events, following prefix keymaps.
    /// Returns the final binding, or None if the sequence is unbound.
    pub fn lookup_key_sequence(&self, keymap_id: u64, keys: &[KeyEvent]) -> Option<&KeyBinding> {
        if keys.is_empty() {
            return None;
        }
        let mut current_map = keymap_id;
        for (i, key) in keys.iter().enumerate() {
            match self.lookup_key(current_map, key) {
                Some(binding) => {
                    if i == keys.len() - 1 {
                        return Some(binding);
                    }
                    // Not the last key — binding must be a prefix keymap.
                    match binding {
                        KeyBinding::Prefix(next_map) => {
                            current_map = *next_map;
                        }
                        _ => return None, // non-prefix in middle of sequence
                    }
                }
                None => return None,
            }
        }
        None
    }

    /// Set the parent of a keymap.
    pub fn set_keymap_parent(&mut self, keymap_id: u64, parent_id: Option<u64>) {
        if let Some(km) = self.keymaps.get_mut(&keymap_id) {
            km.parent = parent_id;
        }
    }

    /// Get the parent of a keymap.
    pub fn keymap_parent(&self, keymap_id: u64) -> Option<u64> {
        self.keymaps.get(&keymap_id).and_then(|km| km.parent)
    }

    /// Get the global keymap id.
    pub fn global_map(&self) -> Option<u64> {
        self.global_map
    }

    /// Set the global keymap.
    pub fn set_global_map(&mut self, id: u64) {
        self.global_map = Some(id);
    }

    // -----------------------------------------------------------------------
    // Key description parsing  ("kbd" style)
    // -----------------------------------------------------------------------

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
            result.push(Self::parse_single_key(part)?);
        }
        Ok(result)
    }

    /// Parse a single key token (e.g. "C-x", "M-RET", "a", "f1").
    fn parse_single_key(token: &str) -> Result<KeyEvent, String> {
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
                // "S-" is shift only when followed by more than one char or a
                // named key; a bare "S" after modifiers is the character 'S'.
                let rest = &remainder[2..];
                // Check that this isn't just the character 'S' followed by '-'
                // being consumed as a modifier of nothing. If rest is non-empty
                // and the original token had an explicit S- prefix, treat as shift.
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
                // Use canonical upper-case names for well-known keys
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
            .map(Self::format_key_event)
            .collect::<Vec<_>>()
            .join(" ")
    }
}

impl Default for KeymapManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- Parsing tests --

    #[test]
    fn parse_plain_char() {
        let keys = KeymapManager::parse_key_description("a").unwrap();
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
        let keys = KeymapManager::parse_key_description("C-x").unwrap();
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
        let keys = KeymapManager::parse_key_description("M-x").unwrap();
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
        let keys = KeymapManager::parse_key_description("C-x C-f").unwrap();
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
        let keys = KeymapManager::parse_key_description("RET").unwrap();
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
        let keys = KeymapManager::parse_key_description("TAB").unwrap();
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
        let keys = KeymapManager::parse_key_description("SPC").unwrap();
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
        let keys = KeymapManager::parse_key_description("C-M-s").unwrap();
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
        let keys = KeymapManager::parse_key_description("f1").unwrap();
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
        let keys = KeymapManager::parse_key_description("C-f12").unwrap();
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
        assert!(KeymapManager::parse_key_description("").is_err());
    }

    #[test]
    fn parse_error_unknown_name() {
        assert!(KeymapManager::parse_key_description("foobar").is_err());
    }

    // -- Keymap operations tests --

    #[test]
    fn make_keymap_returns_unique_ids() {
        let mut mgr = KeymapManager::new();
        let a = mgr.make_keymap();
        let b = mgr.make_keymap();
        assert_ne!(a, b);
        assert!(mgr.is_keymap(a));
        assert!(mgr.is_keymap(b));
    }

    #[test]
    fn make_sparse_keymap_with_name() {
        let mut mgr = KeymapManager::new();
        let id = mgr.make_sparse_keymap(Some("my-map".to_string()));
        let km = mgr.get(id).unwrap();
        assert_eq!(km.name.as_deref(), Some("my-map"));
    }

    #[test]
    fn define_and_lookup_key() {
        let mut mgr = KeymapManager::new();
        let map = mgr.make_keymap();
        let key = KeyEvent::Char {
            code: 'a',
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        };
        mgr.define_key(
            map,
            key.clone(),
            KeyBinding::Command("self-insert-command".to_string()),
        );
        match mgr.lookup_key(map, &key) {
            Some(KeyBinding::Command(name)) => assert_eq!(name, "self-insert-command"),
            other => panic!("expected Command, got {:?}", other),
        }
    }

    #[test]
    fn lookup_follows_parent_chain() {
        let mut mgr = KeymapManager::new();
        let parent = mgr.make_keymap();
        let child = mgr.make_sparse_keymap(None);
        mgr.set_keymap_parent(child, Some(parent));

        let key_a = KeyEvent::Char {
            code: 'a',
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        };
        let key_b = KeyEvent::Char {
            code: 'b',
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        };

        // Bind 'a' in parent, 'b' in child
        mgr.define_key(
            parent,
            key_a.clone(),
            KeyBinding::Command("cmd-a".to_string()),
        );
        mgr.define_key(
            child,
            key_b.clone(),
            KeyBinding::Command("cmd-b".to_string()),
        );

        // Child should find 'b' directly
        match mgr.lookup_key(child, &key_b) {
            Some(KeyBinding::Command(name)) => assert_eq!(name, "cmd-b"),
            other => panic!("expected cmd-b, got {:?}", other),
        }

        // Child should find 'a' via parent
        match mgr.lookup_key(child, &key_a) {
            Some(KeyBinding::Command(name)) => assert_eq!(name, "cmd-a"),
            other => panic!("expected cmd-a, got {:?}", other),
        }

        // Unbound key should return None
        let key_c = KeyEvent::Char {
            code: 'c',
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        };
        assert!(mgr.lookup_key(child, &key_c).is_none());
    }

    #[test]
    fn child_overrides_parent_binding() {
        let mut mgr = KeymapManager::new();
        let parent = mgr.make_keymap();
        let child = mgr.make_sparse_keymap(None);
        mgr.set_keymap_parent(child, Some(parent));

        let key = KeyEvent::Char {
            code: 'x',
            ctrl: true,
            meta: false,
            shift: false,
            super_: false,
        };

        mgr.define_key(
            parent,
            key.clone(),
            KeyBinding::Command("parent-cmd".to_string()),
        );
        mgr.define_key(
            child,
            key.clone(),
            KeyBinding::Command("child-cmd".to_string()),
        );

        // Child's binding should shadow parent's
        match mgr.lookup_key(child, &key) {
            Some(KeyBinding::Command(name)) => assert_eq!(name, "child-cmd"),
            other => panic!("expected child-cmd, got {:?}", other),
        }
    }

    #[test]
    fn prefix_key_sequence_lookup() {
        let mut mgr = KeymapManager::new();
        let root = mgr.make_keymap();
        let prefix_map = mgr.make_sparse_keymap(None);

        let cx = KeyEvent::Char {
            code: 'x',
            ctrl: true,
            meta: false,
            shift: false,
            super_: false,
        };
        let cf = KeyEvent::Char {
            code: 'f',
            ctrl: true,
            meta: false,
            shift: false,
            super_: false,
        };

        // C-x is a prefix leading to prefix_map
        mgr.define_key(root, cx.clone(), KeyBinding::Prefix(prefix_map));
        // C-f in the prefix map leads to find-file
        mgr.define_key(
            prefix_map,
            cf.clone(),
            KeyBinding::Command("find-file".to_string()),
        );

        // Lookup "C-x C-f"
        let binding = mgr.lookup_key_sequence(root, &[cx, cf]);
        match binding {
            Some(KeyBinding::Command(name)) => assert_eq!(name, "find-file"),
            other => panic!("expected find-file, got {:?}", other),
        }
    }

    #[test]
    fn global_map_set_and_get() {
        let mut mgr = KeymapManager::new();
        assert!(mgr.global_map().is_none());
        let map = mgr.make_keymap();
        mgr.set_global_map(map);
        assert_eq!(mgr.global_map(), Some(map));
    }

    // -- Roundtrip formatting tests --

    #[test]
    fn format_key_event_roundtrip() {
        let cases = vec![
            "C-x", "M-x", "C-M-s", "a", "SPC", "RET", "TAB", "f1", "C-f12",
        ];
        for desc in cases {
            let keys = KeymapManager::parse_key_description(desc).unwrap();
            assert_eq!(keys.len(), 1, "expected single key for {}", desc);
            let formatted = KeymapManager::format_key_event(&keys[0]);
            // Re-parse formatted string and compare
            let reparsed = KeymapManager::parse_key_description(&formatted).unwrap();
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
        let keys = KeymapManager::parse_key_description(desc).unwrap();
        let formatted = KeymapManager::format_key_sequence(&keys);
        assert_eq!(formatted, "C-x C-f");
    }

    #[test]
    fn keymap_parent_accessor() {
        let mut mgr = KeymapManager::new();
        let parent = mgr.make_keymap();
        let child = mgr.make_sparse_keymap(None);

        assert_eq!(mgr.keymap_parent(child), None);
        mgr.set_keymap_parent(child, Some(parent));
        assert_eq!(mgr.keymap_parent(child), Some(parent));
        mgr.set_keymap_parent(child, None);
        assert_eq!(mgr.keymap_parent(child), None);
    }

    #[test]
    fn parse_arrow_keys() {
        for name in &["up", "down", "left", "right"] {
            let keys = KeymapManager::parse_key_description(name).unwrap();
            assert_eq!(keys.len(), 1);
            match &keys[0] {
                KeyEvent::Function { name: n, .. } => assert_eq!(n.as_str(), *name),
                other => panic!("expected Function for {}, got {:?}", name, other),
            }
        }
    }

    #[test]
    fn parse_modifier_with_named_key() {
        let keys = KeymapManager::parse_key_description("C-RET").unwrap();
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

    #[test]
    fn lisp_value_binding() {
        let mut mgr = KeymapManager::new();
        let map = mgr.make_keymap();
        let key = KeyEvent::Char {
            code: 'z',
            ctrl: false,
            meta: false,
            shift: false,
            super_: false,
        };
        mgr.define_key(map, key.clone(), KeyBinding::LispValue(Value::Int(42)));
        match mgr.lookup_key(map, &key) {
            Some(KeyBinding::LispValue(Value::Int(42))) => {}
            other => panic!("expected LispValue(42), got {:?}", other),
        }
    }
}
