//! File loading and module system (require/provide/load).

use super::error::EvalError;
use super::value::Value;
use std::path::{Path, PathBuf};

fn has_load_suffix(name: &str) -> bool {
    name.ends_with(".el") || name.ends_with(".elc")
}

fn load_candidates(name: &str, no_suffix: bool, must_suffix: bool) -> Vec<String> {
    if no_suffix {
        return vec![name.to_string()];
    }
    if has_load_suffix(name) {
        return vec![name.to_string()];
    }
    if must_suffix {
        return vec![format!("{}.el", name)];
    }
    vec![format!("{}.el", name), name.to_string()]
}

/// Search for a file in the load path.
pub fn find_file_in_load_path(name: &str, load_path: &[String]) -> Option<PathBuf> {
    find_file_in_load_path_with_flags(name, load_path, false, false)
}

/// Search for a file in load-path with `load` optional suffix flags.
///
/// Behavior follows Emacs:
/// - `no_suffix`: load only the exact filename.
/// - `must_suffix`: require a suffixed file when FILE has no suffix.
/// - default: try suffixed file first, then exact filename.
pub fn find_file_in_load_path_with_flags(
    name: &str,
    load_path: &[String],
    no_suffix: bool,
    must_suffix: bool,
) -> Option<PathBuf> {
    let candidates = load_candidates(name, no_suffix, must_suffix);

    for candidate in &candidates {
        let path = Path::new(candidate);
        if path.is_absolute() && path.exists() {
            return Some(path.to_path_buf());
        }
    }

    for candidate in &candidates {
        for dir in load_path {
            let full = Path::new(dir).join(candidate);
            if full.exists() {
                return Some(full);
            }
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

/// Load and evaluate a file. Returns the last result.
pub fn load_file(eval: &mut super::eval::Evaluator, path: &Path) -> Result<Value, EvalError> {
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
        let forms = super::parser::parse_forms(&content).map_err(|e| EvalError::Signal {
            symbol: "invalid-read-syntax".to_string(),
            data: vec![Value::string(format!(
                "Parse error in {}: {:?}",
                path.display(),
                e
            ))],
        })?;

        for form in &forms {
            let _ = eval.eval_expr(form)?;
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
            find_file_in_load_path_with_flags("choice", &load_path, false, false),
            Some(el.clone())
        );
        // no-suffix mode only tries exact name.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, true, false),
            Some(plain.clone())
        );
        // must-suffix mode rejects plain file and requires suffixed one.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, false, true),
            Some(el)
        );
        // no-suffix takes precedence if both flags are set.
        assert_eq!(
            find_file_in_load_path_with_flags("choice", &load_path, true, true),
            Some(plain)
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
}
