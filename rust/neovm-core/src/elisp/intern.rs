//! String interner for symbol, keyword, and subr names.
//!
//! Provides `SymId(u32)` — a compact, Copy handle into an append-only
//! `StringInterner`. This replaces the `String` in `Value::Symbol`,
//! `Value::Keyword`, and `Value::Subr`, making `Value` `Copy` and 16 bytes.
//!
//! Thread-local access follows the same pattern as `CURRENT_HEAP` in `value.rs`.

use std::cell::Cell;
use std::collections::HashMap;

/// A compact handle to an interned string. Copy, 4 bytes.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SymId(pub(crate) u32);

/// Append-only string interner. Guarantees: same string → same SymId.
pub struct StringInterner {
    strings: Vec<String>,
    map: HashMap<String, u32>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Intern a string, returning its unique id.
    /// If the string was already interned, returns the existing id.
    pub fn intern(&mut self, s: &str) -> SymId {
        if let Some(&idx) = self.map.get(s) {
            return SymId(idx);
        }
        let idx = self.strings.len() as u32;
        self.strings.push(s.to_owned());
        self.map.insert(s.to_owned(), idx);
        SymId(idx)
    }

    /// Resolve a SymId back to its string. Panics if id is invalid.
    #[inline]
    pub fn resolve(&self, id: SymId) -> &str {
        &self.strings[id.0 as usize]
    }
}

// ---------------------------------------------------------------------------
// Thread-local interner access
// ---------------------------------------------------------------------------

thread_local! {
    static CURRENT_INTERNER: Cell<*mut StringInterner> = const { Cell::new(std::ptr::null_mut()) };
    #[cfg(test)]
    static TEST_FALLBACK_INTERNER: std::cell::RefCell<Option<Box<StringInterner>>> = const { std::cell::RefCell::new(None) };
}

/// Set the current thread-local interner pointer.
/// Must be called before any `intern()` / `resolve_sym()` calls.
pub fn set_current_interner(interner: &mut StringInterner) {
    CURRENT_INTERNER.with(|h| h.set(interner as *mut StringInterner));
}

/// Clear the thread-local interner pointer.
pub fn clear_current_interner() {
    CURRENT_INTERNER.with(|h| h.set(std::ptr::null_mut()));
}

/// Save and restore the current interner pointer around a closure.
/// Used when a temporary Evaluator is created that would overwrite the thread-local.
pub(crate) fn with_saved_interner<R>(f: impl FnOnce() -> R) -> R {
    let saved = CURRENT_INTERNER.with(|h| h.get());
    let result = f();
    CURRENT_INTERNER.with(|h| h.set(saved));
    result
}

/// Get raw pointer to the current interner.
/// In test mode, auto-creates a fallback interner if none is set.
#[inline]
fn current_interner_ptr() -> *mut StringInterner {
    CURRENT_INTERNER.with(|h| {
        let ptr = h.get();
        if !ptr.is_null() {
            return ptr;
        }
        #[cfg(test)]
        {
            TEST_FALLBACK_INTERNER.with(|fb| {
                let mut borrow = fb.borrow_mut();
                if borrow.is_none() {
                    *borrow = Some(Box::new(StringInterner::new()));
                }
                let interner_ref: &mut StringInterner = borrow.as_mut().unwrap();
                let ptr = interner_ref as *mut StringInterner;
                h.set(ptr);
                ptr
            })
        }
        #[cfg(not(test))]
        {
            panic!("current interner not set — call set_current_interner() first");
        }
    })
}

/// Intern a string using the thread-local interner. Convenience wrapper.
#[inline]
pub fn intern(s: &str) -> SymId {
    let ptr = current_interner_ptr();
    unsafe { &mut *ptr }.intern(s)
}

/// Resolve a SymId to its string using the thread-local interner.
///
/// # Safety
/// The returned `&str` borrows from the interner's internal `Vec<String>`.
/// Each `String`'s heap buffer is stable (append-only interner never removes
/// entries, and `String` data lives on the heap not inline in the Vec).
/// The interner outlives all Values (owned by Evaluator).
/// Same unsafe-pointer pattern as `as_str()` / `get_lambda_data()` in value.rs.
#[inline]
pub fn resolve_sym(id: SymId) -> &'static str {
    let ptr = current_interner_ptr();
    let interner = unsafe { &*ptr };
    let s = interner.resolve(id);
    // Safety: The String's heap buffer is stable because:
    // 1. StringInterner is append-only (strings never removed)
    // 2. String data lives on the heap, not inline in the Vec
    // 3. The interner outlives all Values (owned by Evaluator)
    unsafe { &*(s as *const str) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_dedup() {
        let mut interner = StringInterner::new();
        let a = interner.intern("foo");
        let b = interner.intern("foo");
        let c = interner.intern("bar");
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_eq!(interner.resolve(a), "foo");
        assert_eq!(interner.resolve(c), "bar");
    }

    #[test]
    fn thread_local_intern() {
        // Uses fallback interner in test mode
        let a = intern("hello");
        let b = intern("hello");
        let c = intern("world");
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_eq!(resolve_sym(a), "hello");
        assert_eq!(resolve_sym(c), "world");
    }
}
