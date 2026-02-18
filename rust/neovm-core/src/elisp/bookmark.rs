//! Bookmark system -- persistent named positions.
//!
//! Provides Emacs-compatible bookmark functionality:
//! - `bookmark-set` -- create or update a bookmark at the current position
//! - `bookmark-jump` -- retrieve bookmark data (position, buffer, context)
//! - `bookmark-delete` -- remove a bookmark
//! - `bookmark-rename` -- rename a bookmark
//! - `bookmark-all-names` -- list all bookmark names
//! - `bookmark-get-filename` -- get the filename for a bookmark
//! - `bookmark-get-position` -- get the position for a bookmark
//! - `bookmark-get-annotation` -- get the annotation for a bookmark
//! - `bookmark-set-annotation` -- set the annotation for a bookmark
//! - `bookmark-save` -- serialize bookmarks to a string
//! - `bookmark-load` -- deserialize bookmarks from a string

use std::collections::HashMap;

use super::error::{signal, EvalResult, Flow};
use super::value::Value;

// ---------------------------------------------------------------------------
// Bookmark types
// ---------------------------------------------------------------------------

/// A single bookmark entry.
#[derive(Clone, Debug)]
pub struct Bookmark {
    /// The bookmark name (human-readable label).
    pub name: String,
    /// The filename of the file the bookmark points to (if any).
    pub filename: Option<String>,
    /// The character position in the buffer/file.
    pub position: usize,
    /// Text after the bookmark position, used for relocating if the file
    /// has changed.
    pub front_context: Option<String>,
    /// Text before the bookmark position, used for relocating.
    pub rear_context: Option<String>,
    /// An optional annotation (user note).
    pub annotation: Option<String>,
    /// A handler function name for jump (nil means default handler).
    pub handler: Option<String>,
}

// ---------------------------------------------------------------------------
// BookmarkManager
// ---------------------------------------------------------------------------

/// Central registry for all bookmarks.
#[derive(Clone, Debug)]
pub struct BookmarkManager {
    bookmarks: HashMap<String, Bookmark>,
    /// Most recently used bookmark names (most recent first).
    recent: Vec<String>,
    /// True if bookmarks have been modified since last save.
    modified: bool,
}

impl Default for BookmarkManager {
    fn default() -> Self {
        Self::new()
    }
}

impl BookmarkManager {
    /// Create a new empty bookmark manager.
    pub fn new() -> Self {
        Self {
            bookmarks: HashMap::new(),
            recent: Vec::new(),
            modified: false,
        }
    }

    /// Set (create or update) a bookmark.  Pushes the name to the front
    /// of the recently-used list.
    pub fn set(&mut self, name: &str, bookmark: Bookmark) {
        self.bookmarks.insert(name.to_string(), bookmark);
        self.touch_recent(name);
        self.modified = true;
    }

    /// Get a bookmark by name.
    pub fn get(&self, name: &str) -> Option<&Bookmark> {
        self.bookmarks.get(name)
    }

    /// Delete a bookmark. Returns true if it existed.
    pub fn delete(&mut self, name: &str) -> bool {
        let removed = self.bookmarks.remove(name).is_some();
        if removed {
            self.recent.retain(|n| n != name);
            self.modified = true;
        }
        removed
    }

    /// Rename a bookmark.  Returns true on success, false if the old name
    /// does not exist or the new name is already taken.
    pub fn rename(&mut self, old: &str, new_name: &str) -> bool {
        if !self.bookmarks.contains_key(old) {
            return false;
        }
        if old != new_name && self.bookmarks.contains_key(new_name) {
            return false;
        }
        if let Some(mut bm) = self.bookmarks.remove(old) {
            bm.name = new_name.to_string();
            self.bookmarks.insert(new_name.to_string(), bm);
            // Update recent list
            for entry in &mut self.recent {
                if entry == old {
                    *entry = new_name.to_string();
                }
            }
            self.modified = true;
            true
        } else {
            false
        }
    }

    /// Return a sorted list of all bookmark names.
    pub fn all_names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.bookmarks.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    /// Return the most recently used bookmark names (most recent first).
    pub fn recent_names(&self) -> &[String] {
        &self.recent
    }

    /// Whether the bookmark set has been modified since last save.
    pub fn is_modified(&self) -> bool {
        self.modified
    }

    /// Mark bookmarks as saved (clear modified flag).
    pub fn mark_saved(&mut self) {
        self.modified = false;
    }

    /// Serialize all bookmarks to a string.
    ///
    /// Format: one bookmark per block, separated by form-feeds.
    /// Each block:
    /// ```text
    /// NAME\nFILENAME\nPOSITION\nFRONT_CONTEXT\nREAR_CONTEXT\nANNOTATION\nHANDLER
    /// ```
    /// Empty optional fields are represented as the empty string.
    pub fn save_to_string(&self) -> String {
        let mut out = String::new();
        let mut names: Vec<&String> = self.bookmarks.keys().collect();
        names.sort();
        for (i, name) in names.iter().enumerate() {
            let bm = &self.bookmarks[*name];
            if i > 0 {
                out.push('\x0C'); // form feed separator
            }
            out.push_str(&bm.name);
            out.push('\n');
            out.push_str(bm.filename.as_deref().unwrap_or(""));
            out.push('\n');
            out.push_str(&bm.position.to_string());
            out.push('\n');
            out.push_str(bm.front_context.as_deref().unwrap_or(""));
            out.push('\n');
            out.push_str(bm.rear_context.as_deref().unwrap_or(""));
            out.push('\n');
            out.push_str(bm.annotation.as_deref().unwrap_or(""));
            out.push('\n');
            out.push_str(bm.handler.as_deref().unwrap_or(""));
        }
        out
    }

    /// Deserialize bookmarks from a string produced by `save_to_string`.
    /// Replaces all current bookmarks.
    pub fn load_from_string(&mut self, data: &str) {
        self.bookmarks.clear();
        self.recent.clear();
        self.modified = false;

        if data.is_empty() {
            return;
        }

        for block in data.split('\x0C') {
            let lines: Vec<&str> = block.split('\n').collect();
            if lines.len() < 7 {
                continue; // malformed block, skip
            }
            let name = lines[0].to_string();
            if name.is_empty() {
                continue;
            }
            let filename = if lines[1].is_empty() {
                None
            } else {
                Some(lines[1].to_string())
            };
            let position = lines[2].parse::<usize>().unwrap_or(1);
            let front_context = if lines[3].is_empty() {
                None
            } else {
                Some(lines[3].to_string())
            };
            let rear_context = if lines[4].is_empty() {
                None
            } else {
                Some(lines[4].to_string())
            };
            let annotation = if lines[5].is_empty() {
                None
            } else {
                Some(lines[5].to_string())
            };
            let handler = if lines[6].is_empty() {
                None
            } else {
                Some(lines[6].to_string())
            };
            let bm = Bookmark {
                name: name.clone(),
                filename,
                position,
                front_context,
                rear_context,
                annotation,
                handler,
            };
            self.bookmarks.insert(name, bm);
        }
    }

    /// Move `name` to the front of the recently-used list, removing
    /// duplicates.
    fn touch_recent(&mut self, name: &str) {
        self.recent.retain(|n| n != name);
        self.recent.insert(0, name.to_string());
    }
}

// ===========================================================================
// Builtin helpers
// ===========================================================================

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

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        Value::Symbol(s) => Ok(s.clone()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

#[allow(dead_code)]
fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

// ===========================================================================
// Builtins (evaluator-dependent)
// ===========================================================================

/// (bookmark-set NAME &optional NO-OVERWRITE) -> nil
///
/// In batch/non-file buffers GNU Emacs signals:
///   (error "Buffer not visiting a file or directory")
/// This implementation mirrors that behavior.
pub(crate) fn builtin_bookmark_set(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("bookmark-set", &args, 1)?;
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("bookmark-set"), Value::Int(args.len() as i64)],
        ));
    }
    let name = expect_string(&args[0])?;
    let _no_overwrite = args.get(1);

    let (position, filename) = match eval.buffers.current_buffer() {
        Some(buffer) => (buffer.point(), buffer.file_name.clone()),
        None => (1, None),
    };

    let filename = match filename {
        Some(path) => Some(path),
        None => {
            return Err(signal(
                "error",
                vec![Value::string("Buffer not visiting a file or directory")],
            ))
        }
    };

    let bm = Bookmark {
        name: name.clone(),
        filename,
        position,
        front_context: None,
        rear_context: None,
        annotation: None,
        handler: None,
    };
    eval.bookmarks.set(&name, bm);
    Ok(Value::Nil)
}

/// (bookmark-jump NAME) -> alist with bookmark data
///
/// Returns an alist: ((filename . F) (position . P) (annotation . A))
/// or signals an error if the bookmark does not exist.
pub(crate) fn builtin_bookmark_jump(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.is_empty() || args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("bookmark-jump"), Value::Int(args.len() as i64)],
        ));
    }

    let name = match &args[0] {
        Value::Nil => {
            return Err(signal(
                "error",
                vec![Value::string("No bookmark specified")],
            ))
        }
        Value::Str(name) => (**name).clone(),
        _ => return Ok(Value::Nil),
    };

    match eval.bookmarks.get(&name) {
        Some(bm) => {
            let filename_val = match &bm.filename {
                Some(f) => Value::string(f.clone()),
                None => Value::Nil,
            };
            let position_val = Value::Int(bm.position as i64);
            let annotation_val = match &bm.annotation {
                Some(a) => Value::string(a.clone()),
                None => Value::Nil,
            };
            let alist = Value::list(vec![
                Value::cons(Value::symbol("filename"), filename_val),
                Value::cons(Value::symbol("position"), position_val),
                Value::cons(Value::symbol("annotation"), annotation_val),
            ]);
            Ok(alist)
        }
        None => Err(signal(
            "error",
            vec![Value::string(format!("Invalid bookmark {name}"))],
        )),
    }
}

/// (bookmark-delete NAME &optional BATCH) -> nil
pub(crate) fn builtin_bookmark_delete(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.is_empty() || args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("bookmark-delete"), Value::Int(args.len() as i64)],
        ));
    }

    // GNU Emacs accepts non-string NAME payloads and simply returns nil.
    // Only string names are actionable for deletion.
    if let Value::Str(name) = &args[0] {
        let _ = eval.bookmarks.delete(name);
    }
    Ok(Value::Nil)
}

/// (bookmark-rename OLD NEW) -> nil
pub(crate) fn builtin_bookmark_rename(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.is_empty() || args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("bookmark-rename"), Value::Int(args.len() as i64)],
        ));
    }

    // Batch-mode prompt fallbacks in GNU Emacs become end-of-file.
    if args.len() == 1 || args.get(1).is_some_and(Value::is_nil) {
        return Err(signal(
            "end-of-file",
            vec![Value::string("Error reading from stdin")],
        ));
    }

    let old = &args[0];
    let new_name = &args[1];

    if let Value::Str(old_name) = old {
        if eval.bookmarks.get(old_name).is_none() {
            return Err(signal(
                "error",
                vec![Value::string(format!("Invalid bookmark {old_name}"))],
            ));
        }

        let target = match new_name {
            Value::Str(name) => (**name).clone(),
            _ => {
                return Err(signal(
                    "error",
                    vec![Value::string(format!("Invalid bookmark {old_name}"))],
                ))
            }
        };

        if eval.bookmarks.rename(old_name, &target) {
            return Ok(Value::Nil);
        }
        return Err(signal(
            "error",
            vec![Value::string(format!("Invalid bookmark {old_name}"))],
        ));
    }

    if old.is_cons() {
        if let Value::Str(name) = new_name {
            return Err(signal(
                "error",
                vec![Value::string(format!("Invalid bookmark {name}"))],
            ));
        }
        return Ok(Value::Nil);
    }

    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("consp"), Value::Nil],
    ))
}

/// (bookmark-all-names) -> list of bookmark names (sorted)
pub(crate) fn builtin_bookmark_all_names(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-all-names", &args, 0)?;
    let names: Vec<Value> = eval
        .bookmarks
        .all_names()
        .into_iter()
        .map(|name| Value::string(name.to_string()))
        .collect();
    Ok(Value::list(names))
}

/// (bookmark-get-filename BOOKMARK) -> filename string or nil
///
/// BOOKMARK may be a bookmark name or a bookmark record alist.
pub(crate) fn builtin_bookmark_get_filename(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-get-filename", &args, 1)?;

    if let Some(items) = super::value::list_to_vec(&args[0]) {
        for item in &items {
            if let Value::Cons(cell) = item {
                let pair = cell.lock().expect("poisoned");
                if let Value::Symbol(sym) = &pair.car {
                    if sym == "filename" {
                        return Ok(pair.cdr.clone());
                    }
                }
            }
        }
        return Ok(Value::Nil);
    }

    let name = expect_string(&args[0])?;
    let filename = eval
        .bookmarks
        .get(&name)
        .and_then(|bm| bm.filename.as_ref())
        .map(|s| Value::string(s.clone()))
        .unwrap_or(Value::Nil);
    Ok(filename)
}

/// (bookmark-get-position BOOKMARK) -> integer position or nil
///
/// BOOKMARK may be a bookmark name or a bookmark record alist.
pub(crate) fn builtin_bookmark_get_position(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-get-position", &args, 1)?;

    if let Some(items) = super::value::list_to_vec(&args[0]) {
        for item in &items {
            if let Value::Cons(cell) = item {
                let pair = cell.lock().expect("poisoned");
                if let Value::Symbol(sym) = &pair.car {
                    if sym == "position" {
                        return Ok(pair.cdr.clone());
                    }
                }
            }
        }
        return Ok(Value::Nil);
    }

    let name = expect_string(&args[0])?;
    let position = eval
        .bookmarks
        .get(&name)
        .map(|bm| Value::Int(bm.position as i64))
        .unwrap_or(Value::Nil);
    Ok(position)
}

/// (bookmark-get-annotation BOOKMARK) -> annotation string or nil
///
/// BOOKMARK may be a bookmark name or a bookmark record alist.
pub(crate) fn builtin_bookmark_get_annotation(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-get-annotation", &args, 1)?;

    if let Some(items) = super::value::list_to_vec(&args[0]) {
        for item in &items {
            if let Value::Cons(cell) = item {
                let pair = cell.lock().expect("poisoned");
                if let Value::Symbol(sym) = &pair.car {
                    if sym == "annotation" {
                        return Ok(pair.cdr.clone());
                    }
                }
            }
        }
        return Ok(Value::Nil);
    }

    let name = expect_string(&args[0])?;
    let annotation = eval
        .bookmarks
        .get(&name)
        .and_then(|bm| bm.annotation.as_ref())
        .map(|s| Value::string(s.clone()))
        .unwrap_or(Value::Nil);
    Ok(annotation)
}

/// (bookmark-set-annotation BOOKMARK ANNOTATION) -> annotation string or nil
///
/// BOOKMARK is a bookmark name.  If missing, returns nil.
pub(crate) fn builtin_bookmark_set_annotation(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-set-annotation", &args, 2)?;
    let name = expect_string(&args[0])?;
    let annotation = if args[1].is_nil() {
        None
    } else {
        Some(expect_string(&args[1])?)
    };

    if let Some(mut bm) = eval.bookmarks.get(&name).cloned() {
        bm.annotation = annotation.clone();
        eval.bookmarks.set(&name, bm);
        if let Some(value) = annotation {
            Ok(Value::string(value))
        } else {
            Ok(Value::Nil)
        }
    } else {
        Ok(Value::Nil)
    }
}

/// (bookmark-save) -> string
///
/// Serialize all bookmarks and return the string.  In a real Emacs this
/// would write to `bookmark-default-file`; here we just return the data.
pub(crate) fn builtin_bookmark_save(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-save", &args, 0)?;
    let data = eval.bookmarks.save_to_string();
    eval.bookmarks.mark_saved();
    Ok(Value::string(data))
}

/// (bookmark-load DATA) -> nil
///
/// Deserialize bookmarks from a string.
pub(crate) fn builtin_bookmark_load(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("bookmark-load", &args, 1)?;
    let data = expect_string(&args[0])?;
    eval.bookmarks.load_from_string(&data);
    Ok(Value::Nil)
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // BookmarkManager unit tests
    // -----------------------------------------------------------------------

    #[test]
    fn set_get_delete() {
        let mut mgr = BookmarkManager::new();

        let bm = Bookmark {
            name: "test".to_string(),
            filename: Some("/tmp/test.txt".to_string()),
            position: 42,
            front_context: Some("after".to_string()),
            rear_context: Some("before".to_string()),
            annotation: None,
            handler: None,
        };

        mgr.set("test", bm);
        assert!(mgr.get("test").is_some());
        assert_eq!(mgr.get("test").unwrap().position, 42);
        assert_eq!(
            mgr.get("test").unwrap().filename.as_deref(),
            Some("/tmp/test.txt")
        );

        assert!(mgr.delete("test"));
        assert!(mgr.get("test").is_none());
        assert!(!mgr.delete("test")); // already gone
    }

    #[test]
    fn rename() {
        let mut mgr = BookmarkManager::new();

        let bm = Bookmark {
            name: "old".to_string(),
            filename: None,
            position: 10,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: None,
        };
        mgr.set("old", bm);

        assert!(mgr.rename("old", "new"));
        assert!(mgr.get("old").is_none());
        assert!(mgr.get("new").is_some());
        assert_eq!(mgr.get("new").unwrap().name, "new");
        assert_eq!(mgr.get("new").unwrap().position, 10);
    }

    #[test]
    fn rename_nonexistent() {
        let mut mgr = BookmarkManager::new();
        assert!(!mgr.rename("nope", "whatever"));
    }

    #[test]
    fn rename_collision() {
        let mut mgr = BookmarkManager::new();
        let bm1 = Bookmark {
            name: "a".to_string(),
            filename: None,
            position: 1,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: None,
        };
        let bm2 = Bookmark {
            name: "b".to_string(),
            filename: None,
            position: 2,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: None,
        };
        mgr.set("a", bm1);
        mgr.set("b", bm2);

        // Cannot rename a -> b when b already exists
        assert!(!mgr.rename("a", "b"));

        // Renaming to self is fine
        assert!(mgr.rename("a", "a"));
    }

    #[test]
    fn all_names_sorted() {
        let mut mgr = BookmarkManager::new();

        for name in &["zebra", "alpha", "middle"] {
            let bm = Bookmark {
                name: name.to_string(),
                filename: None,
                position: 1,
                front_context: None,
                rear_context: None,
                annotation: None,
                handler: None,
            };
            mgr.set(name, bm);
        }

        let names = mgr.all_names();
        assert_eq!(names, vec!["alpha", "middle", "zebra"]);
    }

    #[test]
    fn most_recent_tracking() {
        let mut mgr = BookmarkManager::new();

        for name in &["first", "second", "third"] {
            let bm = Bookmark {
                name: name.to_string(),
                filename: None,
                position: 1,
                front_context: None,
                rear_context: None,
                annotation: None,
                handler: None,
            };
            mgr.set(name, bm);
        }

        // Most recent should be "third"
        assert_eq!(mgr.recent_names()[0], "third");
        assert_eq!(mgr.recent_names()[1], "second");
        assert_eq!(mgr.recent_names()[2], "first");

        // Re-set "first" -> moves to front
        let bm = Bookmark {
            name: "first".to_string(),
            filename: None,
            position: 99,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: None,
        };
        mgr.set("first", bm);
        assert_eq!(mgr.recent_names()[0], "first");
    }

    #[test]
    fn serialize_deserialize() {
        let mut mgr = BookmarkManager::new();

        let bm1 = Bookmark {
            name: "alpha".to_string(),
            filename: Some("/home/test/file.el".to_string()),
            position: 100,
            front_context: Some("(defun".to_string()),
            rear_context: Some(";;".to_string()),
            annotation: Some("Important function".to_string()),
            handler: None,
        };
        let bm2 = Bookmark {
            name: "beta".to_string(),
            filename: None,
            position: 1,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: Some("my-handler".to_string()),
        };
        mgr.set("alpha", bm1);
        mgr.set("beta", bm2);

        let data = mgr.save_to_string();
        assert!(!data.is_empty());

        // Load into a fresh manager
        let mut mgr2 = BookmarkManager::new();
        mgr2.load_from_string(&data);

        let names = mgr2.all_names();
        assert_eq!(names, vec!["alpha", "beta"]);

        let a = mgr2.get("alpha").unwrap();
        assert_eq!(a.position, 100);
        assert_eq!(a.filename.as_deref(), Some("/home/test/file.el"));
        assert_eq!(a.front_context.as_deref(), Some("(defun"));
        assert_eq!(a.rear_context.as_deref(), Some(";;"));
        assert_eq!(a.annotation.as_deref(), Some("Important function"));
        assert!(a.handler.is_none());

        let b = mgr2.get("beta").unwrap();
        assert_eq!(b.position, 1);
        assert!(b.filename.is_none());
        assert_eq!(b.handler.as_deref(), Some("my-handler"));
    }

    #[test]
    fn load_empty_string() {
        let mut mgr = BookmarkManager::new();
        let bm = Bookmark {
            name: "test".to_string(),
            filename: None,
            position: 1,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: None,
        };
        mgr.set("test", bm);

        mgr.load_from_string("");
        assert!(mgr.all_names().is_empty());
    }

    #[test]
    fn modified_flag() {
        let mut mgr = BookmarkManager::new();
        assert!(!mgr.is_modified());

        let bm = Bookmark {
            name: "test".to_string(),
            filename: None,
            position: 1,
            front_context: None,
            rear_context: None,
            annotation: None,
            handler: None,
        };
        mgr.set("test", bm);
        assert!(mgr.is_modified());

        mgr.mark_saved();
        assert!(!mgr.is_modified());

        mgr.delete("test");
        assert!(mgr.is_modified());
    }

    // -----------------------------------------------------------------------
    // Builtin-level tests
    // -----------------------------------------------------------------------

    fn set_current_buffer_file(eval: &mut super::super::eval::Evaluator, path: &str) {
        if let Some(buffer) = eval.buffers.current_buffer_mut() {
            buffer.file_name = Some(path.to_string());
        }
    }

    #[test]
    fn test_builtin_bookmark_set_and_jump() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/test.el");

        // bookmark-set
        let result = builtin_bookmark_set(&mut eval, vec![Value::string("my-bookmark")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());

        // bookmark-jump returns alist
        let result = builtin_bookmark_jump(&mut eval, vec![Value::string("my-bookmark")]);
        assert!(result.is_ok());
        let alist = result.unwrap();
        assert!(alist.is_list());

        // bookmark-jump on nonexistent -> error
        let result = builtin_bookmark_jump(&mut eval, vec![Value::string("nope")]);
        assert!(result.is_err());
    }

    #[test]
    fn test_builtin_bookmark_jump_permissive_designators() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // nil designator is a dedicated error in GNU Emacs.
        let nil_result = builtin_bookmark_jump(&mut eval, vec![Value::Nil]);
        assert!(nil_result.is_err());

        // Non-string designators are tolerated and return nil.
        let int_result = builtin_bookmark_jump(&mut eval, vec![Value::Int(1)]);
        assert!(int_result.unwrap().is_nil());

        let list_result =
            builtin_bookmark_jump(&mut eval, vec![Value::list(vec![Value::symbol("foo")])]);
        assert!(list_result.unwrap().is_nil());

        // Optional second argument is accepted.
        let missing_with_flag =
            builtin_bookmark_jump(&mut eval, vec![Value::string("missing"), Value::True]);
        assert!(missing_with_flag.is_err());
    }

    #[test]
    fn test_builtin_bookmark_delete() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/delete.el");

        // Set a bookmark
        builtin_bookmark_set(&mut eval, vec![Value::string("del-me")]).unwrap();

        // Delete it (returns nil) and verify side effect.
        let result = builtin_bookmark_delete(&mut eval, vec![Value::string("del-me")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
        assert!(eval.bookmarks.get("del-me").is_none());

        // Delete again -> nil (not found).
        let result = builtin_bookmark_delete(&mut eval, vec![Value::string("del-me")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());

        // Non-string payloads are accepted and return nil.
        let result = builtin_bookmark_delete(&mut eval, vec![Value::Int(1)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());

        // Optional second argument is accepted.
        let result = builtin_bookmark_delete(&mut eval, vec![Value::Int(1), Value::True]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn test_builtin_bookmark_rename() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/rename.el");

        builtin_bookmark_set(&mut eval, vec![Value::string("old-name")]).unwrap();

        // Rename
        let result = builtin_bookmark_rename(
            &mut eval,
            vec![Value::string("old-name"), Value::string("new-name")],
        );
        assert!(result.is_ok());

        // Old name gone, new name exists.
        assert!(eval.bookmarks.get("old-name").is_none());
        assert!(eval.bookmarks.get("new-name").is_some());
    }

    #[test]
    fn test_builtin_bookmark_rename_permissive_designators() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/rename-permissive.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("old-name")]).unwrap();

        // One-arg calls fall back to prompt behavior in batch mode and error.
        let one_arg = builtin_bookmark_rename(&mut eval, vec![Value::string("old-name")]);
        assert!(one_arg.is_err());

        // Non-cons old payloads signal wrong-type in this compatibility path.
        let ints = builtin_bookmark_rename(&mut eval, vec![Value::Int(1), Value::Int(2)]);
        assert!(ints.is_err());

        // Cons/list old payloads with non-string NEW are tolerated and return nil.
        let list_ok = builtin_bookmark_rename(
            &mut eval,
            vec![
                Value::list(vec![Value::symbol("a")]),
                Value::list(vec![Value::symbol("b")]),
            ],
        );
        assert!(list_ok.unwrap().is_nil());

        // Cons/list old payloads with string NEW error on invalid bookmark designator.
        let list_str = builtin_bookmark_rename(
            &mut eval,
            vec![Value::list(vec![Value::symbol("a")]), Value::string("new-name")],
        );
        assert!(list_str.is_err());

        // String path still renames when the source bookmark exists.
        let rename_ok = builtin_bookmark_rename(
            &mut eval,
            vec![Value::string("old-name"), Value::string("new-name")],
        );
        assert!(rename_ok.is_ok());
        assert!(eval.bookmarks.get("old-name").is_none());
        assert!(eval.bookmarks.get("new-name").is_some());
    }

    #[test]
    fn test_builtin_bookmark_all_names() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/all-names.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("z-bookmark")]).unwrap();
        builtin_bookmark_set(&mut eval, vec![Value::string("a-bookmark")]).unwrap();

        let result = builtin_bookmark_all_names(&mut eval, vec![]).unwrap();
        let names = super::super::value::list_to_vec(&result).unwrap();
        assert_eq!(names.len(), 2);
        assert_eq!(names[0].as_str(), Some("a-bookmark"));
        assert_eq!(names[1].as_str(), Some("z-bookmark"));
    }

    #[test]
    fn test_builtin_bookmark_get_filename() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/file.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("with-file")]).unwrap();

        let found =
            builtin_bookmark_get_filename(&mut eval, vec![Value::string("with-file")]).unwrap();
        assert_eq!(found.as_str(), Some("/tmp/file.el"));

        let missing =
            builtin_bookmark_get_filename(&mut eval, vec![Value::string("missing")]).unwrap();
        assert!(missing.is_nil());
    }

    #[test]
    fn test_builtin_bookmark_get_position() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/position.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("at-point")]).unwrap();

        let found =
            builtin_bookmark_get_position(&mut eval, vec![Value::string("at-point")]).unwrap();
        assert_eq!(found.as_int(), Some(0));

        let missing =
            builtin_bookmark_get_position(&mut eval, vec![Value::string("missing")]).unwrap();
        assert!(missing.is_nil());
    }

    #[test]
    fn test_builtin_bookmark_get_annotation() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/annotation.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("with-note")]).unwrap();
        builtin_bookmark_set_annotation(
            &mut eval,
            vec![Value::string("with-note"), Value::string("note")],
        )
        .unwrap();

        let found =
            builtin_bookmark_get_annotation(&mut eval, vec![Value::string("with-note")]).unwrap();
        assert_eq!(found.as_str(), Some("note"));

        let missing =
            builtin_bookmark_get_annotation(&mut eval, vec![Value::string("missing")]).unwrap();
        assert!(missing.is_nil());
    }

    #[test]
    fn test_builtin_bookmark_set_annotation() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();
        set_current_buffer_file(&mut eval, "/tmp/set-annotation.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("entry")]).unwrap();

        let set_result = builtin_bookmark_set_annotation(
            &mut eval,
            vec![Value::string("entry"), Value::string("note")],
        )
        .unwrap();
        assert_eq!(set_result.as_str(), Some("note"));

        let got = builtin_bookmark_get_annotation(&mut eval, vec![Value::string("entry")]).unwrap();
        assert_eq!(got.as_str(), Some("note"));

        let missing = builtin_bookmark_set_annotation(
            &mut eval,
            vec![Value::string("missing"), Value::string("note")],
        )
        .unwrap();
        assert!(missing.is_nil());
    }

    #[test]
    fn test_builtin_bookmark_save_load() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        set_current_buffer_file(&mut eval, "/file1.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("bm1")]).unwrap();
        set_current_buffer_file(&mut eval, "/file2.el");
        builtin_bookmark_set(&mut eval, vec![Value::string("bm2")]).unwrap();

        // Save
        let result = builtin_bookmark_save(&mut eval, vec![]);
        assert!(result.is_ok());
        let saved_data = result.unwrap();
        assert!(saved_data.is_string());

        // Clear and load
        eval.bookmarks = BookmarkManager::new();
        let result = builtin_bookmark_load(&mut eval, vec![saved_data]);
        assert!(result.is_ok());

        // Verify restored bookmark payloads.
        let bm1 = eval.bookmarks.get("bm1").expect("bm1 restored");
        assert_eq!(bm1.filename.as_deref(), Some("/file1.el"));

        let bm2 = eval.bookmarks.get("bm2").expect("bm2 restored");
        assert_eq!(bm2.filename.as_deref(), Some("/file2.el"));
    }

    #[test]
    fn test_wrong_arg_count() {
        use super::super::eval::Evaluator;

        let mut eval = Evaluator::new();

        // bookmark-set needs between 1 and 2 args.
        let result = builtin_bookmark_set(&mut eval, vec![]);
        assert!(result.is_err());
        let result = builtin_bookmark_set(
            &mut eval,
            vec![Value::string("name"), Value::Nil, Value::Nil],
        );
        assert!(result.is_err());

        // bookmark-jump needs exactly 1
        let result = builtin_bookmark_jump(&mut eval, vec![]);
        assert!(result.is_err());

        // bookmark-delete needs exactly 1
        let result = builtin_bookmark_delete(&mut eval, vec![]);
        assert!(result.is_err());

        // bookmark-rename needs exactly 2
        let result = builtin_bookmark_rename(&mut eval, vec![Value::string("x")]);
        assert!(result.is_err());
    }
}
