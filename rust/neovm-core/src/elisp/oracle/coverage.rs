//! Coverage checks for oracle parity tests.

use std::collections::{BTreeSet, HashSet};
use std::process::Command;

use super::common::oracle_prop_enabled;
use super::coverage_manifest::ORACLE_TESTED_NAMES;

fn oracle_emacs_path() -> String {
    std::env::var("NEOVM_FORCE_ORACLE_PATH").unwrap_or_else(|_| "emacs".to_string())
}

fn run_oracle_primitive_name_dump() -> Result<BTreeSet<String>, String> {
    let program = r#"(let (out)
  (mapatoms
   (lambda (sym)
     (when (fboundp sym)
       (let ((fn (symbol-function sym)))
         (when (subr-primitive-p fn)
           (push (symbol-name sym) out))))))
  (dolist (name (sort out #'string<))
    (princ name)
    (terpri)))"#;
    let oracle_bin = oracle_emacs_path();

    let output = Command::new(&oracle_bin)
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

    let names = String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(ToOwned::to_owned)
        .collect::<BTreeSet<_>>();

    Ok(names)
}

#[test]
fn oracle_prop_coverage_manifest_sorted_unique() {
    let mut seen = HashSet::new();
    let mut prev = "";
    for &name in ORACLE_TESTED_NAMES {
        assert!(!name.is_empty(), "manifest contains an empty name");
        assert!(name >= prev, "manifest should be sorted: '{name}' after '{prev}'");
        assert!(seen.insert(name), "manifest contains duplicate name: {name}");
        prev = name;
    }
}

#[test]
fn oracle_prop_coverage_snapshot() {
    if !oracle_prop_enabled() {
        eprintln!("skipping oracle_prop_coverage_snapshot: set NEOVM_ENABLE_ORACLE_PROPTEST=1");
        return;
    }

    let oracle_primitives =
        run_oracle_primitive_name_dump().expect("oracle primitive name dump should succeed");
    let tested = ORACLE_TESTED_NAMES
        .iter()
        .map(|name| (*name).to_string())
        .collect::<BTreeSet<_>>();

    let covered = tested.intersection(&oracle_primitives).collect::<Vec<_>>();
    let non_primitive = tested.difference(&oracle_primitives).collect::<Vec<_>>();
    let missing_count = oracle_primitives.len().saturating_sub(covered.len());
    let coverage_pct = if oracle_primitives.is_empty() {
        0.0
    } else {
        (covered.len() as f64 * 100.0) / oracle_primitives.len() as f64
    };

    eprintln!(
        "oracle primitive coverage: covered={}/{} ({:.2}%), tested-names={}, non-primitive-tested={}, missing={}",
        covered.len(),
        oracle_primitives.len(),
        coverage_pct,
        tested.len(),
        non_primitive.len(),
        missing_count
    );

    if !non_primitive.is_empty() {
        let preview = non_primitive
            .iter()
            .take(20)
            .map(|name| name.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        eprintln!("non-primitive tested names (first 20): {preview}");
    }

    assert!(
        !covered.is_empty(),
        "coverage sanity check failed: no tested names matched oracle primitive names"
    );
}
