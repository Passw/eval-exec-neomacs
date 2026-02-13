//! Hook system and buffer change notification functions.
//!
//! Implements:
//! - `before-change-functions` and `after-change-functions`
//! - `kill-buffer-hook`, `find-file-hook`, etc.
//! - Generic hook variables (lists of functions to call)
//! - Hook running with error protection

use crate::elisp::value::Value;
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Hook
// ---------------------------------------------------------------------------

/// A named hook variable containing a list of functions to call.
#[derive(Clone, Debug)]
pub struct Hook {
    /// Hook name (e.g. "after-change-functions").
    pub name: String,
    /// List of function values (symbols, lambdas, etc.).
    pub functions: Vec<Value>,
    /// If true, the hook is buffer-local.
    pub buffer_local: bool,
    /// If true, the hook is permanent (survives `remove-hook` with no fn).
    pub permanent: bool,
}

impl Hook {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            functions: Vec::new(),
            buffer_local: false,
            permanent: false,
        }
    }

    /// Add a function to the hook (at the end by default).
    /// If `append` is false, adds at the beginning.
    pub fn add(&mut self, func: Value, append: bool) {
        // Don't add duplicates (compared by eq for symbols).
        let already = self.functions.iter().any(|f| match (f, &func) {
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Subr(a), Value::Subr(b)) => a == b,
            _ => false,
        });

        if !already {
            if append {
                self.functions.push(func);
            } else {
                self.functions.insert(0, func);
            }
        }
    }

    /// Remove a function from the hook.
    pub fn remove(&mut self, func: &Value) {
        self.functions.retain(|f| !match (f, func) {
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Subr(a), Value::Subr(b)) => a == b,
            _ => false,
        });
    }

    /// Whether the hook is empty.
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    /// Number of functions in the hook.
    pub fn len(&self) -> usize {
        self.functions.len()
    }

    /// Get all functions as a slice.
    pub fn functions(&self) -> &[Value] {
        &self.functions
    }

    /// Convert to a Lisp list.
    pub fn to_value(&self) -> Value {
        Value::list(self.functions.clone())
    }
}

// ---------------------------------------------------------------------------
// HookManager
// ---------------------------------------------------------------------------

/// Global hook registry.
pub struct HookManager {
    hooks: HashMap<String, Hook>,
}

impl HookManager {
    pub fn new() -> Self {
        let mut mgr = Self {
            hooks: HashMap::new(),
        };
        mgr.register_standard_hooks();
        mgr
    }

    fn register_standard_hooks(&mut self) {
        let standard = [
            "before-change-functions",
            "after-change-functions",
            "first-change-hook",
            "kill-buffer-hook",
            "kill-buffer-query-functions",
            "buffer-list-update-hook",
            "find-file-hook",
            "find-file-not-found-functions",
            "after-save-hook",
            "before-save-hook",
            "write-file-functions",
            "write-contents-functions",
            "after-init-hook",
            "before-init-hook",
            "emacs-startup-hook",
            "kill-emacs-hook",
            "suspend-hook",
            "suspend-resume-hook",
            "focus-in-hook",
            "focus-out-hook",
            "delete-frame-functions",
            "after-make-frame-functions",
            "window-configuration-change-hook",
            "window-size-change-functions",
            "window-scroll-functions",
            "window-selection-change-functions",
            "post-command-hook",
            "pre-command-hook",
            "post-self-insert-hook",
            "minibuffer-setup-hook",
            "minibuffer-exit-hook",
            "activate-mark-hook",
            "deactivate-mark-hook",
            "temp-buffer-show-hook",
            "change-major-mode-hook",
            "after-change-major-mode-hook",
            "hack-local-variables-hook",
            "font-lock-mode-hook",
            "auto-save-hook",
            "compilation-finish-functions",
        ];

        for name in &standard {
            self.hooks.insert(name.to_string(), Hook::new(name));
        }
    }

    /// Get or create a hook.
    pub fn get_or_create(&mut self, name: &str) -> &mut Hook {
        self.hooks
            .entry(name.to_string())
            .or_insert_with(|| Hook::new(name))
    }

    /// Get a hook by name.
    pub fn get(&self, name: &str) -> Option<&Hook> {
        self.hooks.get(name)
    }

    /// Get a mutable hook by name.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut Hook> {
        self.hooks.get_mut(name)
    }

    /// Add a function to a hook.
    pub fn add_hook(&mut self, hook_name: &str, func: Value, append: bool) {
        self.get_or_create(hook_name).add(func, append);
    }

    /// Remove a function from a hook.
    pub fn remove_hook(&mut self, hook_name: &str, func: &Value) {
        if let Some(hook) = self.hooks.get_mut(hook_name) {
            hook.remove(func);
        }
    }

    /// Get the list of functions for a hook (empty vec if not found).
    pub fn hook_functions(&self, name: &str) -> Vec<Value> {
        self.hooks
            .get(name)
            .map(|h| h.functions.clone())
            .unwrap_or_default()
    }

    /// List all hook names.
    pub fn hook_list(&self) -> Vec<String> {
        self.hooks.keys().cloned().collect()
    }
}

impl Default for HookManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Change notification data
// ---------------------------------------------------------------------------

/// Information about a buffer change, passed to change hooks.
#[derive(Clone, Debug)]
pub struct ChangeInfo {
    /// Byte position where the change begins.
    pub beg: usize,
    /// Byte position where the change ends (after the change).
    pub end: usize,
    /// Length of the old text that was replaced (0 for pure insertion).
    pub old_len: usize,
}

impl ChangeInfo {
    pub fn insertion(pos: usize, len: usize) -> Self {
        Self {
            beg: pos,
            end: pos + len,
            old_len: 0,
        }
    }

    pub fn deletion(pos: usize, old_len: usize) -> Self {
        Self {
            beg: pos,
            end: pos,
            old_len,
        }
    }

    pub fn replacement(pos: usize, new_len: usize, old_len: usize) -> Self {
        Self {
            beg: pos,
            end: pos + new_len,
            old_len,
        }
    }

    /// Convert to Lisp arguments for `after-change-functions`:
    /// (BEG END OLD-LEN) — all 1-based positions.
    pub fn to_after_change_args(&self) -> Vec<Value> {
        vec![
            Value::Int((self.beg + 1) as i64),
            Value::Int((self.end + 1) as i64),
            Value::Int(self.old_len as i64),
        ]
    }

    /// Convert to Lisp arguments for `before-change-functions`:
    /// (BEG END) — 1-based positions of the region about to change.
    pub fn to_before_change_args(&self) -> Vec<Value> {
        vec![
            Value::Int((self.beg + 1) as i64),
            Value::Int((self.end + 1) as i64),
        ]
    }
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hook_add_remove() {
        let mut hook = Hook::new("test-hook");
        assert!(hook.is_empty());

        hook.add(Value::symbol("func-a"), true);
        hook.add(Value::symbol("func-b"), true);
        assert_eq!(hook.len(), 2);

        // No duplicates
        hook.add(Value::symbol("func-a"), true);
        assert_eq!(hook.len(), 2);

        hook.remove(&Value::symbol("func-a"));
        assert_eq!(hook.len(), 1);
        assert_eq!(hook.functions()[0].as_symbol_name(), Some("func-b"),);
    }

    #[test]
    fn hook_prepend() {
        let mut hook = Hook::new("test");
        hook.add(Value::symbol("first"), true);
        hook.add(Value::symbol("second"), false); // prepend
        assert_eq!(hook.functions()[0].as_symbol_name(), Some("second"));
        assert_eq!(hook.functions()[1].as_symbol_name(), Some("first"));
    }

    #[test]
    fn hook_manager_standard() {
        let mgr = HookManager::new();
        assert!(mgr.get("before-change-functions").is_some());
        assert!(mgr.get("after-change-functions").is_some());
        assert!(mgr.get("kill-buffer-hook").is_some());
        assert!(mgr.get("post-command-hook").is_some());
    }

    #[test]
    fn hook_manager_add_remove() {
        let mut mgr = HookManager::new();
        mgr.add_hook("my-hook", Value::symbol("my-func"), true);

        let funcs = mgr.hook_functions("my-hook");
        assert_eq!(funcs.len(), 1);

        mgr.remove_hook("my-hook", &Value::symbol("my-func"));
        let funcs = mgr.hook_functions("my-hook");
        assert!(funcs.is_empty());
    }

    #[test]
    fn change_info_insertion() {
        let ci = ChangeInfo::insertion(10, 5);
        assert_eq!(ci.beg, 10);
        assert_eq!(ci.end, 15);
        assert_eq!(ci.old_len, 0);

        let args = ci.to_after_change_args();
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn change_info_deletion() {
        let ci = ChangeInfo::deletion(10, 5);
        assert_eq!(ci.beg, 10);
        assert_eq!(ci.end, 10);
        assert_eq!(ci.old_len, 5);
    }

    #[test]
    fn hook_to_value() {
        let mut hook = Hook::new("test");
        hook.add(Value::symbol("a"), true);
        hook.add(Value::symbol("b"), true);

        let val = hook.to_value();
        assert!(val.is_list());
    }
}
