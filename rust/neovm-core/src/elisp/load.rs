//! File loading and module system (require/provide/load).

use super::error::EvalError;
use super::expr::print_expr;
use super::expr::Expr;
use super::value::Value;
use std::hash::{Hash, Hasher};
use std::fs;
use std::path::{Path, PathBuf};

fn has_load_suffix(name: &str) -> bool {
    name.ends_with(".el")
}

fn source_suffixed_path(base: &Path) -> PathBuf {
    let base_str = base.to_string_lossy();
    PathBuf::from(format!("{base_str}.el"))
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

fn cache_key(lexical_binding: bool) -> String {
    let lexical = if lexical_binding { "1" } else { "0" };
    format!(
        "{ELISP_CACHE_SCHEMA};vm={ELISP_CACHE_VM_VERSION};lexical={lexical}"
    )
}

fn source_hash(content: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}

fn cache_sidecar_path(source_path: &Path) -> PathBuf {
    source_path.with_extension("neoc")
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

fn write_forms_cache(source_path: &Path, source: &str, lexical_binding: bool, forms: &[Expr]) {
    let cache_path = cache_sidecar_path(source_path);
    let payload = forms.iter().map(print_expr).collect::<Vec<_>>().join("\n");
    let raw = format!(
        "{ELISP_CACHE_MAGIC}\nkey={}\nsource-hash={:016x}\n\n{}\n",
        cache_key(lexical_binding),
        source_hash(source),
        payload
    );

    let tmp_path = cache_path.with_extension("neoc.tmp");
    let _ = fs::write(&tmp_path, raw);
    let _ = fs::rename(&tmp_path, &cache_path);
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
    write_forms_cache(source_path, source, lexical_binding, &forms);
    Ok(forms)
}

/// Load and evaluate a file. Returns the last result.
pub fn load_file(eval: &mut super::eval::Evaluator, path: &Path) -> Result<Value, EvalError> {
    if path.extension().and_then(|s| s.to_str()) == Some("elc") {
        return Err(EvalError::Signal {
            symbol: "file-error".to_string(),
            data: vec![Value::string(format!(
                "Loading .elc is unsupported in neomacs. Rebuild from source and load the .el file: {}",
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
    let first_line: &str = content.lines().next().unwrap_or("");
    if first_line.contains("lexical-binding: t") {
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
        assert!(!entries.is_empty(), "load-history should have at least one entry");
        let first = super::super::value::list_to_vec(&entries[0]).expect("entry is a list");
        let path_str = file.to_string_lossy().to_string();
        assert_eq!(first.first().and_then(Value::as_str), Some(path_str.as_str()));
        assert_eq!(
            eval.obarray().symbol_value("load-file-name").cloned(),
            Some(Value::Nil)
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
        assert!(cache.exists(), "source load should create .neoc sidecar cache");
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
                assert_eq!(symbol, "file-error");
                assert!(
                    data.iter()
                        .any(|v| v.as_str().is_some_and(|s| s.contains("unsupported in neomacs"))),
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
            EvalError::Signal { symbol, .. } => assert_eq!(symbol, "file-error"),
            other => panic!("unexpected error: {other:?}"),
        }
        assert_eq!(
            eval.obarray().symbol_value("vm-load-elc-sibling").cloned(),
            None,
            "rejecting .elc should not implicitly load source sibling",
        );

        let _ = fs::remove_dir_all(&dir);
    }
}
