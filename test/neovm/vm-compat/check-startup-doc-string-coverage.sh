#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
doc_rs="$repo_root/rust/neovm-core/src/elisp/doc.rs"

source "$script_dir/oracle-emacs-path.sh"

if [[ ! -x "$oracle_emacs_path" ]]; then
  echo "oracle emacs binary is not executable: $oracle_emacs_path" >&2
  echo "default oracle path is hardcoded to: $hardcoded_oracle_emacs" >&2
  echo "override with NEOVM_FORCE_ORACLE_PATH for temporary local runs if needed" >&2
  exit 127
fi

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

oracle_symbols="$tmp_dir/oracle-string-doc-symbols.txt"
seeded_symbols="$tmp_dir/startup-string-doc-symbols.txt"
missing_symbols="$tmp_dir/missing-startup-string-docs.txt"
extra_symbols="$tmp_dir/extra-startup-string-docs.txt"

"$oracle_emacs_path" --batch -Q --eval "(let (out) (mapatoms (lambda (s) (let ((d (get s 'variable-documentation))) (when (stringp d) (push (symbol-name s) out))))) (setq out (sort out #'string<)) (princ (mapconcat #'identity out \"\n\")))" \
  | sort -u > "$oracle_symbols"

perl -ne '
if(/^pub\(crate\)\s+static\s+STARTUP_VARIABLE_DOC_STRING_PROPERTIES:\s*&\[.*=\s*&\[/){$in=1; next}
if($in && /^\s*\];/){$in=0}
next unless $in;
if(/^\s*\(\s*"([^"]+)",/){print "$1\n"; $expect=0; next}
if(/^\s*\(\s*$/){$expect=1; next}
if($expect && /^\s*"([^"]+)",\s*$/){print "$1\n"; $expect=0; next}
' "$doc_rs" \
  | sort -u > "$seeded_symbols"

comm -23 "$oracle_symbols" "$seeded_symbols" > "$missing_symbols"
comm -13 "$oracle_symbols" "$seeded_symbols" > "$extra_symbols"

oracle_count="$(wc -l < "$oracle_symbols" | tr -d '[:space:]')"
seeded_count="$(wc -l < "$seeded_symbols" | tr -d '[:space:]')"
missing_count="$(wc -l < "$missing_symbols" | tr -d '[:space:]')"
extra_count="$(wc -l < "$extra_symbols" | tr -d '[:space:]')"

echo "startup string-doc coverage:"
echo "  oracle string-doc symbols: $oracle_count"
echo "  startup string-doc symbols: $seeded_count"
echo "  missing startup string-docs: $missing_count"
echo "  extra startup string-docs: $extra_count"

if [[ "$missing_count" -ne 0 ]]; then
  echo "missing startup string-doc symbols (must be added to STARTUP_VARIABLE_DOC_STRING_PROPERTIES):" >&2
  cat "$missing_symbols" >&2
  exit 1
fi

if [[ "$extra_count" -ne 0 ]]; then
  echo "extra startup string-doc symbols (not currently required by oracle startup string docs):" >&2
  cat "$extra_symbols" >&2
  exit 1
fi

echo "startup string-doc coverage check passed"
