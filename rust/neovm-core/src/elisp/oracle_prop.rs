//! Oracle-backed property tests for evaluator parity.
//!
//! These tests are intentionally opt-in because they require GNU Emacs
//! available on PATH (or via `NEOVM_FORCE_ORACLE_PATH`).

#[cfg(test)]
mod tests {
    use std::io::Write;
    use std::process::Command;

    use proptest::prelude::*;
    use proptest::test_runner::TestCaseError;

    use crate::elisp::{format_eval_result, parse_forms, Evaluator};

    fn oracle_prop_enabled() -> bool {
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

    fn run_oracle_eval(form: &str) -> Result<String, String> {
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
            .args(["--batch", "-Q", "--eval", &program])
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

    fn run_neovm_eval(form: &str) -> Result<String, String> {
        let mut eval = Evaluator::new();
        let forms = parse_forms(form).map_err(|e| format!("parse error: {e}"))?;
        let Some(first) = forms.first() else {
            return Err("no form parsed".to_string());
        };
        Ok(format_eval_result(&eval.eval_expr(first)))
    }

    #[test]
    fn oracle_prop_plus_1_3() {
        if !oracle_prop_enabled() {
            eprintln!("skipping oracle_prop_plus_1_3: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
            return;
        }

        let mut runner = proptest::test_runner::TestRunner::new(proptest::test_runner::Config {
            cases: 1,
            ..proptest::test_runner::Config::default()
        });
        let strategy = Just("(+ 1 3)".to_string());

        runner
            .run(&strategy, |form| {
                let oracle = run_oracle_eval(&form).map_err(TestCaseError::fail)?;
                let neovm = run_neovm_eval(&form).map_err(TestCaseError::fail)?;
                prop_assert_eq!(oracle.as_str(), "OK 4");
                prop_assert_eq!(neovm.as_str(), "OK 4");
                prop_assert_eq!(neovm, oracle);
                Ok(())
            })
            .expect("property parity for (+ 1 3) should pass");
    }

    proptest! {
        #[test]
        fn oracle_prop_plus_operator(
            a in -100_000i64..100_000i64,
            b in -100_000i64..100_000i64,
        ) {
            if !oracle_prop_enabled() {
                return Ok(());
            }

            let form = format!("(+ {} {})", a, b);
            let expected = format!("OK {}", a + b);

            let oracle = run_oracle_eval(&form).expect("oracle eval should succeed");
            let neovm = run_neovm_eval(&form).expect("neovm eval should succeed");

            prop_assert_eq!(oracle.as_str(), expected.as_str());
            prop_assert_eq!(neovm.as_str(), expected.as_str());
            prop_assert_eq!(neovm.as_str(), oracle.as_str());
        }
    }
}
