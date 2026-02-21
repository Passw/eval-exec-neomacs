//! File loading and module system (require/provide/load).

use super::error::EvalError;
use super::expr::print_expr;
use super::expr::Expr;
use super::value::Value;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::path::{Path, PathBuf};

fn has_load_suffix(name: &str) -> bool {
    name.ends_with(".el")
}

fn source_suffixed_path(base: &Path) -> PathBuf {
    let base_str = base.to_string_lossy();
    PathBuf::from(format!("{base_str}.el"))
}

fn unsupported_compiled_suffixed_paths(base: &Path) -> [PathBuf; 2] {
    let base_str = base.to_string_lossy();
    [
        PathBuf::from(format!("{base_str}.elc")),
        PathBuf::from(format!("{base_str}.elc.gz")),
    ]
}

fn pick_suffixed(base: &Path, _prefer_newer: bool) -> Option<PathBuf> {
    let el = source_suffixed_path(base);
    if el.exists() {
        return Some(el);
    }
    None
}

fn find_for_base(
    base: &Path,
    original_name: &str,
    no_suffix: bool,
    must_suffix: bool,
    prefer_newer: bool,
) -> Option<PathBuf> {
    if no_suffix || has_load_suffix(original_name) {
        if base.exists() {
            return Some(base.to_path_buf());
        }
        return None;
    }

    if let Some(suffixed) = pick_suffixed(base, prefer_newer) {
        return Some(suffixed);
    }

    if !must_suffix && base.exists() {
        return Some(base.to_path_buf());
    }

    // Surface unsupported compiled artifacts explicitly instead of reporting
    // generic file-missing when only `.elc` payloads are present.
    for compiled in unsupported_compiled_suffixed_paths(base) {
        if compiled.exists() {
            return Some(compiled);
        }
    }

    None
}

/// Search for a file in the load path.
pub fn find_file_in_load_path(name: &str, load_path: &[String]) -> Option<PathBuf> {
    find_file_in_load_path_with_flags(name, load_path, false, false, false)
}

/// Search for a file in load-path with `load` optional suffix flags.
///
/// Behavior follows Emacs:
/// - `no_suffix`: load only the exact filename.
/// - `must_suffix`: require a suffixed file when FILE has no suffix.
/// - `prefer_newer`: kept for API compatibility; no effect in source-only mode.
/// - default: search each load-path directory in order, preferring suffixed
///   files within each directory before bare names.
pub fn find_file_in_load_path_with_flags(
    name: &str,
    load_path: &[String],
    no_suffix: bool,
    must_suffix: bool,
    prefer_newer: bool,
) -> Option<PathBuf> {
    let path = Path::new(name);
    if path.is_absolute() {
        return find_for_base(path, name, no_suffix, must_suffix, prefer_newer);
    }

    // Emacs searches load-path directory-by-directory; suffix preference
    // is evaluated within each directory.
    for dir in load_path {
        let full = Path::new(dir).join(name);
        if let Some(found) = find_for_base(&full, name, no_suffix, must_suffix, prefer_newer) {
            return Some(found);
        }
    }

    None
}

/// Extract `load-path` from the evaluator's obarray as a Vec<String>.
pub fn get_load_path(obarray: &super::symbol::Obarray) -> Vec<String> {
    let default_directory = obarray
        .symbol_value("default-directory")
        .and_then(|v| v.as_str())
        .unwrap_or(".");

    let val = obarray
        .symbol_value("load-path")
        .cloned()
        .unwrap_or(Value::Nil);
    super::value::list_to_vec(&val)
        .unwrap_or_default()
        .into_iter()
        .filter_map(|v| match v {
            Value::Nil => Some(default_directory.to_string()),
            _ => v.as_str().map(|s| s.to_string()),
        })
        .collect()
}

const ELISP_CACHE_MAGIC: &str = "NEOVM-ELISP-CACHE-V1";
const ELISP_CACHE_SCHEMA: &str = "schema=1";
const ELISP_CACHE_VM_VERSION: &str = env!("CARGO_PKG_VERSION");
pub const ELISP_CACHE_EXTENSION: &str = "neoc";
const ELISP_CACHE_TEMP_EXTENSION: &str = "neoc.tmp";

fn cache_key(lexical_binding: bool) -> String {
    let lexical = if lexical_binding { "1" } else { "0" };
    format!("{ELISP_CACHE_SCHEMA};vm={ELISP_CACHE_VM_VERSION};lexical={lexical}")
}

fn source_hash(content: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

fn cache_sidecar_path(source_path: &Path) -> PathBuf {
    source_path.with_extension(ELISP_CACHE_EXTENSION)
}

fn cache_temp_path(source_path: &Path) -> PathBuf {
    cache_sidecar_path(source_path).with_extension(ELISP_CACHE_TEMP_EXTENSION)
}

const CACHE_WRITE_PHASE_BEFORE_WRITE: u8 = 1;
const CACHE_WRITE_PHASE_AFTER_WRITE: u8 = 2;

#[cfg(test)]
thread_local! {
    static CACHE_WRITE_FAIL_PHASE: std::cell::Cell<u8> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
fn set_cache_write_fail_phase_for_test(phase: u8) {
    CACHE_WRITE_FAIL_PHASE.with(|p| p.set(phase));
}

#[cfg(test)]
fn clear_cache_write_fail_phase_for_test() {
    CACHE_WRITE_FAIL_PHASE.with(|p| p.set(0));
}

fn maybe_inject_cache_write_failure(_phase: u8) -> std::io::Result<()> {
    #[cfg(test)]
    {
        let should_fail = CACHE_WRITE_FAIL_PHASE.with(|p| p.get() == _phase);
        if should_fail {
            return Err(std::io::Error::other(format!(
                "injected .{ELISP_CACHE_EXTENSION} write failure at phase {_phase}"
            )));
        }
    }

    Ok(())
}

fn best_effort_sync_parent_dir(path: &Path) {
    if let Some(parent) = path.parent() {
        if let Ok(dir) = fs::File::open(parent) {
            let _ = dir.sync_data();
        }
    }
}

fn maybe_load_cached_forms(
    source_path: &Path,
    source: &str,
    lexical_binding: bool,
) -> Option<Vec<Expr>> {
    let cache_path = cache_sidecar_path(source_path);
    let raw = fs::read_to_string(cache_path).ok()?;
    let mut parts = raw.splitn(5, '\n');
    let magic = parts.next()?;
    let key = parts.next()?;
    let hash = parts.next()?;
    let blank = parts.next()?;
    let payload = parts.next().unwrap_or("");

    if magic != ELISP_CACHE_MAGIC {
        return None;
    }
    if blank != "" {
        return None;
    }

    let expected_key = format!("key={}", cache_key(lexical_binding));
    if key != expected_key {
        return None;
    }
    let expected_hash = format!("source-hash={:016x}", source_hash(source));
    if hash != expected_hash {
        return None;
    }

    super::parser::parse_forms(payload).ok()
}

fn write_forms_cache(
    source_path: &Path,
    source: &str,
    lexical_binding: bool,
    forms: &[Expr],
) -> std::io::Result<()> {
    let cache_path = cache_sidecar_path(source_path);
    let payload = forms.iter().map(print_expr).collect::<Vec<_>>().join("\n");
    let raw = format!(
        "{ELISP_CACHE_MAGIC}\nkey={}\nsource-hash={:016x}\n\n{}\n",
        cache_key(lexical_binding),
        source_hash(source),
        payload
    );

    let tmp_path = cache_temp_path(source_path);
    let write_result = (|| -> std::io::Result<()> {
        maybe_inject_cache_write_failure(CACHE_WRITE_PHASE_BEFORE_WRITE)?;

        let mut file = fs::File::create(&tmp_path)?;
        file.write_all(raw.as_bytes())?;
        file.sync_data()?;

        maybe_inject_cache_write_failure(CACHE_WRITE_PHASE_AFTER_WRITE)?;

        fs::rename(&tmp_path, &cache_path)?;
        best_effort_sync_parent_dir(&cache_path);
        Ok(())
    })();

    if write_result.is_err() {
        let _ = fs::remove_file(&tmp_path);
    }

    write_result
}

fn parse_source_with_cache(
    source_path: &Path,
    source: &str,
    lexical_binding: bool,
) -> Result<Vec<Expr>, EvalError> {
    if let Some(forms) = maybe_load_cached_forms(source_path, source, lexical_binding) {
        return Ok(forms);
    }

    let forms = super::parser::parse_forms(source).map_err(|e| EvalError::Signal {
        symbol: "invalid-read-syntax".to_string(),
        data: vec![Value::string(format!(
            "Parse error in {}: {:?}",
            source_path.display(),
            e
        ))],
    })?;
    // Cache persistence failures must not affect `load` semantics.
    if load_cache_writes_enabled() {
        let _ = write_forms_cache(source_path, source, lexical_binding, &forms);
    }
    Ok(forms)
}

fn cache_write_disabled_env_value(value: &str) -> bool {
    let normalized = value.trim().to_ascii_lowercase();
    matches!(normalized.as_str(), "1" | "true" | "yes" | "on")
}

fn load_cache_writes_enabled() -> bool {
    match std::env::var("NEOVM_DISABLE_LOAD_CACHE_WRITE") {
        Ok(value) => !cache_write_disabled_env_value(&value),
        Err(_) => true,
    }
}

fn lexical_binding_enabled_for_source(source: &str) -> bool {
    source
        .lines()
        .next()
        .is_some_and(|line| line.contains("lexical-binding: t"))
}

fn is_unsupported_compiled_path(path: &Path) -> bool {
    let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
        return false;
    };
    name.ends_with(".elc") || name.ends_with(".elc.gz")
}

/// Parse and precompile a source `.el` file into a `.neoc` sidecar cache.
///
/// The emitted cache is an internal NeoVM artifact and not a compatibility
/// boundary. Failures to persist cache are reported to callers.
pub fn precompile_source_file(source_path: &Path) -> Result<PathBuf, EvalError> {
    if is_unsupported_compiled_path(source_path) {
        return Err(EvalError::Signal {
            symbol: "file-error".to_string(),
            data: vec![Value::string(format!(
                "Precompile input must be source (.el), not compiled artifacts (.elc/.elc.gz): {}",
                source_path.display()
            ))],
        });
    }

    let content = fs::read_to_string(source_path).map_err(|e| EvalError::Signal {
        symbol: "file-error".to_string(),
        data: vec![Value::string(format!(
            "Cannot read source file for precompile: {}: {}",
            source_path.display(),
            e
        ))],
    })?;

    let lexical_binding = lexical_binding_enabled_for_source(&content);
    let forms = super::parser::parse_forms(&content).map_err(|e| EvalError::Signal {
        symbol: "invalid-read-syntax".to_string(),
        data: vec![Value::string(format!(
            "Parse error in {}: {:?}",
            source_path.display(),
            e
        ))],
    })?;

    write_forms_cache(source_path, &content, lexical_binding, &forms).map_err(|e| {
        EvalError::Signal {
            symbol: "file-error".to_string(),
            data: vec![Value::string(format!(
                "Failed to persist precompile cache {}: {}",
                cache_sidecar_path(source_path).display(),
                e
            ))],
        }
    })?;

    Ok(cache_sidecar_path(source_path))
}

/// Load and evaluate a file. Returns the last result.
pub fn load_file(eval: &mut super::eval::Evaluator, path: &Path) -> Result<Value, EvalError> {
    if is_unsupported_compiled_path(path) {
        return Err(EvalError::Signal {
            symbol: "error".to_string(),
            data: vec![Value::string(format!(
                "Loading compiled Elisp artifacts (.elc/.elc.gz) is unsupported in neomacs. Rebuild from source and load the .el file: {}",
                path.display()
            ))],
        });
    }

    let content = std::fs::read_to_string(path).map_err(|e| EvalError::Signal {
        symbol: "file-error".to_string(),
        data: vec![Value::string(format!(
            "Cannot read file: {}: {}",
            path.display(),
            e
        ))],
    })?;

    // Save dynamic loader context and restore it even on parse/eval errors.
    let old_lexical = eval.lexical_binding();
    let old_load_file = eval.obarray().symbol_value("load-file-name").cloned();

    // Check for lexical-binding file variable in file-local line.
    if lexical_binding_enabled_for_source(&content) {
        eval.set_lexical_binding(true);
    }

    eval.set_variable(
        "load-file-name",
        Value::string(path.to_string_lossy().to_string()),
    );

    let result = (|| -> Result<Value, EvalError> {
        let forms = parse_source_with_cache(path, &content, eval.lexical_binding())?;

        for form in forms.iter() {
            eval.eval_expr(form)?;
        }

        record_load_history(eval, path);

        // Emacs `load` returns non-nil on success (typically `t`).
        Ok(Value::True)
    })();

    eval.set_lexical_binding(old_lexical);
    if let Some(old) = old_load_file {
        eval.set_variable("load-file-name", old);
    } else {
        eval.set_variable("load-file-name", Value::Nil);
    }

    result
}

fn record_load_history(eval: &mut super::eval::Evaluator, path: &Path) {
    let path_str = path.to_string_lossy().to_string();
    let entry = Value::cons(Value::string(path_str), Value::Nil);
    let history = eval
        .obarray()
        .symbol_value("load-history")
        .cloned()
        .unwrap_or(Value::Nil);
    eval.set_variable("load-history", Value::cons(entry, history));
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    struct CacheWriteFailGuard;

    impl CacheWriteFailGuard {
        fn set(phase: u8) -> Self {
            set_cache_write_fail_phase_for_test(phase);
            Self
        }
    }

    impl Drop for CacheWriteFailGuard {
        fn drop(&mut self) {
            clear_cache_write_fail_phase_for_test();
        }
    }

    #[test]
    fn cache_write_disable_env_value_matrix() {
        for value in ["1", "true", "TRUE", " yes ", "On", "\tyEs\n"] {
            assert!(
                cache_write_disabled_env_value(value),
                "expected '{value}' to disable load cache writes",
            );
        }

        for value in ["0", "false", "FALSE", "no", "off", "", "   ", "maybe"] {
            assert!(
                !cache_write_disabled_env_value(value),
                "expected '{value}' to leave load cache writes enabled",
            );
        }
    }

    #[test]
    fn find_file_nonexistent() {
        assert!(find_file_in_load_path("nonexistent", &[]).is_none());
    }

    #[test]
    fn load_path_extraction() {
        let mut ob = super::super::symbol::Obarray::new();
        ob.set_symbol_value("default-directory", Value::string("/tmp/project"));
        ob.set_symbol_value(
            "load-path",
            Value::list(vec![
                Value::string("/usr/share/emacs/lisp"),
                Value::Nil,
                Value::string("/home/user/.emacs.d"),
            ]),
        );
        let paths = get_load_path(&ob);
        assert_eq!(
            paths,
            vec![
                "/usr/share/emacs/lisp",
                "/tmp/project",
                "/home/user/.emacs.d"
            ]
        );
    }

    #[test]
    fn find_file_with_suffix_flags() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-flags-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");

        let plain = dir.join("choice");
        let el = dir.join("choice.el");
        fs::write(&plain, "plain").expect("write plain fixture");
        fs::write(&el, "el").expect("write el fixture");

        let load_path = vec![dir.to_string_lossy().to_string()];

        // Default mode prefers suffixed files.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, false, false, false),
            Some(el.clone())
        );
        // no-suffix mode only tries exact name.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, true, false, false),
            Some(plain.clone())
        );
        // must-suffix mode rejects plain file and requires suffixed one.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, false, true, false),
            Some(el)
        );
        // no-suffix takes precedence if both flags are set.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, true, true, false),
            Some(plain)
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_file_prefers_earlier_load_path_directory() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("neovm-load-path-order-{unique}"));
        let d1 = root.join("d1");
        let d2 = root.join("d2");
        fs::create_dir_all(&d1).expect("create d1");
        fs::create_dir_all(&d2).expect("create d2");

        let plain = d1.join("choice");
        let el = d2.join("choice.el");
        fs::write(&plain, "plain").expect("write plain fixture");
        fs::write(&el, "el").expect("write el fixture");

        let load_path = vec![
            d1.to_string_lossy().to_string(),
            d2.to_string_lossy().to_string(),
        ];
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, false, false, false),
            Some(plain)
        );

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn find_file_prefers_newer_source_when_enabled() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-prefer-newer-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");

        let elc = dir.join("choice.elc");
        let el = dir.join("choice.el");
        fs::write(&elc, "compiled").expect("write compiled fixture");
        std::thread::sleep(std::time::Duration::from_secs(1));
        fs::write(&el, "source").expect("write source fixture");

        let load_path = vec![dir.to_string_lossy().to_string()];
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, false, false, false),
            Some(el.clone())
        );
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, false, false, true),
            Some(el)
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_file_records_load_history() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-history-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let file = dir.join("probe.el");
        fs::write(&file, "(setq vm-load-history-probe t)\n").expect("write fixture");

        let mut eval = super::super::eval::Evaluator::new();
        let loaded = load_file(&mut eval, &file).expect("load file");
        assert_eq!(loaded, Value::True);

        let history = eval
            .obarray()
            .symbol_value("load-history")
            .cloned()
            .unwrap_or(Value::Nil);
        let entries = super::super::value::list_to_vec(&history).expect("load-history is a list");
        assert!(
            !entries.is_empty(),
            "load-history should have at least one entry"
        );
        let first = super::super::value::list_to_vec(&entries[0]).expect("entry is a list");
        let path_str = file.to_string_lossy().to_string();
        assert_eq!(
            first.first().and_then(Value::as_str),
            Some(path_str.as_str())
        );
        assert_eq!(
            eval.obarray().symbol_value("load-file-name").cloned(),
            Some(Value::Nil)
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn nested_load_restores_parent_load_file_name() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-file-name-nested-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let parent = dir.join("parent.el");
        let child = dir.join("child.el");

        fs::write(
            &parent,
            "(setq vm-parent-seen load-file-name)\n\
             (load (expand-file-name \"child\" (file-name-directory load-file-name)) nil 'nomessage)\n\
             (setq vm-parent-after-child load-file-name)\n",
        )
        .expect("write parent fixture");
        fs::write(&child, "(setq vm-child-seen load-file-name)\n").expect("write child fixture");

        let mut eval = super::super::eval::Evaluator::new();
        let loaded = load_file(&mut eval, &parent).expect("load parent fixture");
        assert_eq!(loaded, Value::True);

        let parent_str = parent.to_string_lossy().to_string();
        let child_str = child.to_string_lossy().to_string();
        assert_eq!(
            eval.obarray()
                .symbol_value("vm-parent-seen")
                .and_then(Value::as_str),
            Some(parent_str.as_str())
        );
        assert_eq!(
            eval.obarray()
                .symbol_value("vm-child-seen")
                .and_then(Value::as_str),
            Some(child_str.as_str())
        );
        assert_eq!(
            eval.obarray()
                .symbol_value("vm-parent-after-child")
                .and_then(Value::as_str),
            Some(parent_str.as_str())
        );
        assert_eq!(
            eval.obarray().symbol_value("load-file-name").cloned(),
            Some(Value::Nil),
            "load-file-name should be restored after top-level load",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_file_writes_and_invalidates_neoc_cache() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-neoc-cache-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let file = dir.join("probe.el");
        let source_v1 = "(setq vm-load-cache-probe 'v1)\n";
        fs::write(&file, source_v1).expect("write source fixture");

        let mut eval = super::super::eval::Evaluator::new();
        let loaded = load_file(&mut eval, &file).expect("load source file");
        assert_eq!(loaded, Value::True);
        assert_eq!(
            eval.obarray().symbol_value("vm-load-cache-probe").cloned(),
            Some(Value::symbol("v1"))
        );

        let cache = cache_sidecar_path(&file);
        assert!(
            cache.exists(),
            "source load should create .neoc sidecar cache"
        );
        let cache_v1 = fs::read_to_string(&cache).expect("read cache v1");
        assert!(
            cache_v1.contains(&format!("key={}", cache_key(false))),
            "cache key should include lexical-binding dimension",
        );
        assert!(
            cache_v1.contains(&format!("source-hash={:016x}", source_hash(source_v1))),
            "cache should carry source hash invalidation key",
        );

        let source_v2 = ";;; -*- lexical-binding: t; -*-\n(setq vm-load-cache-probe 'v2)\n";
        fs::write(&file, source_v2).expect("write source fixture v2");

        let loaded = load_file(&mut eval, &file).expect("reload source file");
        assert_eq!(loaded, Value::True);
        assert_eq!(
            eval.obarray().symbol_value("vm-load-cache-probe").cloned(),
            Some(Value::symbol("v2"))
        );
        let cache_v2 = fs::read_to_string(&cache).expect("read cache v2");
        assert_ne!(cache_v1, cache_v2, "cache must refresh when source changes");
        assert!(
            cache_v2.contains(&format!("key={}", cache_key(true))),
            "cache key should update when lexical-binding dimension changes",
        );
        assert!(
            cache_v2.contains(&format!("source-hash={:016x}", source_hash(source_v2))),
            "cache hash should update when source text changes",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_file_ignores_corrupt_neoc_cache_and_loads_source() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-neoc-corrupt-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let file = dir.join("probe.el");
        fs::write(&file, "(setq vm-load-corrupt-neoc 'ok)\n").expect("write source fixture");
        let cache = cache_sidecar_path(&file);
        fs::write(&cache, "corrupt-neoc-cache").expect("write corrupt cache");

        let mut eval = super::super::eval::Evaluator::new();
        let loaded = load_file(&mut eval, &file).expect("load should ignore corrupt cache");
        assert_eq!(loaded, Value::True);
        assert_eq!(
            eval.obarray().symbol_value("vm-load-corrupt-neoc").cloned(),
            Some(Value::symbol("ok"))
        );
        let rewritten = fs::read_to_string(&cache).expect("cache should be rewritten");
        assert!(
            rewritten.starts_with(ELISP_CACHE_MAGIC),
            "rewritten cache should have expected header",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_file_ignores_cache_write_failures_before_write() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-neoc-write-fail-pre-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let file = dir.join("probe.el");
        fs::write(&file, "(setq vm-load-neoc-write-fail-pre 'ok)\n").expect("write source fixture");

        let _guard = CacheWriteFailGuard::set(CACHE_WRITE_PHASE_BEFORE_WRITE);
        let mut eval = super::super::eval::Evaluator::new();
        let loaded =
            load_file(&mut eval, &file).expect("load should succeed despite cache write failure");
        assert_eq!(loaded, Value::True);
        assert_eq!(
            eval.obarray()
                .symbol_value("vm-load-neoc-write-fail-pre")
                .cloned(),
            Some(Value::symbol("ok"))
        );
        assert!(
            !cache_sidecar_path(&file).exists(),
            "cache should be absent when write fails before cache file creation",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_file_cleans_tmp_after_cache_write_failure_before_rename() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-neoc-write-fail-post-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let file = dir.join("probe.el");
        fs::write(&file, "(setq vm-load-neoc-write-fail-post 'ok)\n")
            .expect("write source fixture");

        let _guard = CacheWriteFailGuard::set(CACHE_WRITE_PHASE_AFTER_WRITE);
        let mut eval = super::super::eval::Evaluator::new();
        let loaded =
            load_file(&mut eval, &file).expect("load should succeed despite cache rename failure");
        assert_eq!(loaded, Value::True);
        assert_eq!(
            eval.obarray()
                .symbol_value("vm-load-neoc-write-fail-post")
                .cloned(),
            Some(Value::symbol("ok"))
        );
        assert!(
            !cache_sidecar_path(&file).exists(),
            "cache should be absent when failure happens before rename",
        );
        assert!(
            !cache_temp_path(&file).exists(),
            "temporary cache file should be cleaned after write failure",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_elc_is_explicitly_unsupported() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-elc-unsupported-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let compiled = dir.join("probe.elc");
        fs::write(&compiled, "compiled-data").expect("write compiled fixture");

        let mut eval = super::super::eval::Evaluator::new();
        let err = load_file(&mut eval, &compiled).expect_err("load should reject .elc");
        match err {
            EvalError::Signal { symbol, data } => {
                assert_eq!(symbol, "error");
                assert!(
                    data.iter().any(|v| v
                        .as_str()
                        .is_some_and(|s| s.contains("unsupported in neomacs"))),
                    "error should explain .elc policy",
                );
            }
            other => panic!("unexpected error: {other:?}"),
        }

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_elc_is_rejected_even_if_sibling_el_exists() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-elc-with-sibling-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let source = dir.join("probe.el");
        let compiled = dir.join("probe.elc");
        fs::write(&source, "(setq vm-load-elc-sibling 'source)\n").expect("write source fixture");
        fs::write(&compiled, "compiled-data").expect("write compiled fixture");

        let mut eval = super::super::eval::Evaluator::new();
        let err = load_file(&mut eval, &compiled).expect_err("load should reject .elc");
        match err {
            EvalError::Signal { symbol, .. } => assert_eq!(symbol, "error"),
            other => panic!("unexpected error: {other:?}"),
        }
        assert_eq!(
            eval.obarray().symbol_value("vm-load-elc-sibling").cloned(),
            None,
            "rejecting .elc should not implicitly load source sibling",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_file_surfaces_elc_only_artifact_as_explicit_unsupported_load_target() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-elc-only-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");

        let compiled = dir.join("module.elc");
        fs::write(&compiled, "compiled").expect("write compiled fixture");

        let load_path = vec![dir.to_string_lossy().to_string()];
        let found = find_file_in_load_path_with_flags("module", &load_path, false, false, false);
        assert_eq!(found, Some(compiled));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn load_elc_gz_is_explicitly_unsupported() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-load-elc-gz-unsupported-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let compiled = dir.join("probe.elc.gz");
        fs::write(&compiled, "compiled-data").expect("write compiled fixture");

        let mut eval = super::super::eval::Evaluator::new();
        let err = load_file(&mut eval, &compiled).expect_err("load should reject .elc.gz");
        match err {
            EvalError::Signal { symbol, .. } => assert_eq!(symbol, "error"),
            other => panic!("unexpected error: {other:?}"),
        }

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn precompile_source_file_writes_deterministic_cache() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-precompile-deterministic-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let source = dir.join("probe.el");
        fs::write(
            &source,
            ";;; -*- lexical-binding: t; -*-\n(setq vm-precompile-probe '(1 2 3))\n",
        )
        .expect("write source fixture");

        let cache_path_1 =
            precompile_source_file(&source).expect("first precompile should succeed");
        let cache_v1 = fs::read_to_string(&cache_path_1).expect("read cache v1");
        let cache_path_2 =
            precompile_source_file(&source).expect("second precompile should succeed");
        let cache_v2 = fs::read_to_string(&cache_path_2).expect("read cache v2");

        assert_eq!(cache_path_1, cache_path_2, "cache path should be stable");
        assert_eq!(
            cache_v1, cache_v2,
            "precompile output should be deterministic"
        );
        assert!(
            cache_v1.contains("lexical=1"),
            "lexical-binding should be reflected in cache key",
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn precompile_source_file_rejects_compiled_inputs() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-precompile-reject-elc-{unique}"));
        fs::create_dir_all(&dir).expect("create temp fixture dir");
        let compiled = dir.join("probe.elc");
        fs::write(&compiled, "compiled").expect("write compiled fixture");

        let err = precompile_source_file(&compiled).expect_err("elc input should be rejected");
        match err {
            EvalError::Signal { symbol, .. } => assert_eq!(symbol, "file-error"),
            other => panic!("unexpected error: {other:?}"),
        }

        let _ = fs::remove_dir_all(&dir);
    }
}
