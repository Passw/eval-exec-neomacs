//! Shared oracle helpers for Elisp unit tests.
//!
//! These helpers are intentionally test-only and require GNU Emacs
//! available on PATH (or via `NEOVM_FORCE_ORACLE_PATH`).

use std::io::Write;
use std::process::Command;

use crate::elisp::{format_eval_result, parse_forms, Evaluator};

pub(crate) const ORACLE_PROP_CASES: u32 = 10;

pub(crate) fn oracle_prop_enabled() -> bool {
    std::env::var_os("NEOVM_ENABLE_ORACLE_PROPTEST").is_some()
}

fn oracle_emacs_path() -> String {
    std::env::var("NEOVM_FORCE_ORACLE_PATH").unwrap_or_else(|_| "emacs".to_string())
}

fn write_oracle_form_file(form: &str) -> Result<tempfile::TempPath, String> {
    let mut file = tempfile::Builder::new()
        .prefix("neovm-oracle-form-")
        .suffix(".el")
        .tempfile()
        .map_err(|e| format!("failed to create oracle form file: {e}"))?;
    file.write_all(form.as_bytes())
        .map_err(|e| format!("failed to write oracle form file: {e}"))?;
    file.flush()
        .map_err(|e| format!("failed to flush oracle form file: {e}"))?;
    Ok(file.into_temp_path())
}

pub(crate) fn run_oracle_eval(form: &str) -> Result<String, String> {
    let form_path = write_oracle_form_file(form)?;
    let program = r#"(condition-case err
    (let* ((form-file (getenv "NEOVM_ORACLE_FORM_FILE"))
           (form (with-temp-buffer
                   (insert-file-contents form-file)
                   (goto-char (point-min))
                   (read (current-buffer)))))
      (princ (concat "OK " (prin1-to-string (eval form)))))
  (error
   (princ (concat "ERR " (prin1-to-string (cons (car err) (cdr err)))))))"#;
    let oracle_bin = oracle_emacs_path();

    let output = Command::new(&oracle_bin)
        .env("NEOVM_ORACLE_FORM_FILE", form_path.as_os_str())
        .args(["--batch", "-Q", "--eval", program])
        .output()
        .map_err(|e| format!("failed to run oracle Emacs: {e}"))?;

    if !output.status.success() {
        return Err(format!(
            "oracle Emacs failed: status={}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

pub(crate) fn run_neovm_eval(form: &str) -> Result<String, String> {
    let mut eval = Evaluator::new();
    let forms = parse_forms(form).map_err(|e| format!("parse error: {e}"))?;
    let Some(first) = forms.first() else {
        return Err("no form parsed".to_string());
    };
    Ok(format_eval_result(&eval.eval_expr(first)))
}
