#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
doc_rs="$repo_root/rust/neovm-core/src/elisp/doc.rs"
allowlist_file="${1:-$script_dir/cases/startup-doc-stub-extra-allowlist.txt}"

source "$script_dir/oracle-emacs-path.sh"

if [[ ! -x "$oracle_emacs_path" ]]; then
  echo "oracle emacs binary is not executable: $oracle_emacs_path" >&2
  echo "default oracle path is hardcoded to: $hardcoded_oracle_emacs" >&2
  echo "override with NEOVM_FORCE_ORACLE_PATH for temporary local runs if needed" >&2
  exit 127
fi

if [[ ! -f "$allowlist_file" ]]; then
  echo "allowlist file not found: $allowlist_file" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

oracle_symbols="$tmp_dir/oracle-integer-doc-symbols.txt"
stub_symbols="$tmp_dir/startup-doc-stub-symbols.txt"
missing_symbols="$tmp_dir/missing-startup-doc-stubs.txt"
extra_symbols="$tmp_dir/extra-startup-doc-stubs.txt"
allowlist_symbols="$tmp_dir/extra-allowlist-symbols.txt"
unexpected_extra_symbols="$tmp_dir/unexpected-extra-startup-doc-stubs.txt"
stale_allowlist_symbols="$tmp_dir/stale-extra-allowlist-symbols.txt"

"$oracle_emacs_path" --batch -Q --eval "(let (out) (mapatoms (lambda (s) (let ((d (get s 'variable-documentation))) (when (integerp d) (push (symbol-name s) out))))) (setq out (sort out #'string<)) (princ (mapconcat #'identity out \"\\n\")))" \
  | sort -u > "$oracle_symbols"

perl -ne '
if(/^pub\(crate\)\s+static\s+STARTUP_VARIABLE_DOC_STUBS:\s*&\[.*=\s*&\[/){$in=1; next}
if($in && /^\s*\];/){$in=0}
next unless $in;
if(/^\s*\(\s*"([^"]+)",/){print "$1\n"; $expect=0; next}
if(/^\s*\(\s*$/){$expect=1; next}
if($expect && /^\s*"([^"]+)",\s*$/){print "$1\n"; $expect=0; next}
' "$doc_rs" \
  | sort -u > "$stub_symbols"

comm -23 "$oracle_symbols" "$stub_symbols" > "$missing_symbols"
comm -13 "$oracle_symbols" "$stub_symbols" > "$extra_symbols"
awk 'NF && $1 !~ /^#/ { print $1 }' "$allowlist_file" | sort -u > "$allowlist_symbols"
comm -23 "$extra_symbols" "$allowlist_symbols" > "$unexpected_extra_symbols"
comm -23 "$allowlist_symbols" "$extra_symbols" > "$stale_allowlist_symbols"

oracle_count="$(wc -l < "$oracle_symbols" | tr -d '[:space:]')"
stub_count="$(wc -l < "$stub_symbols" | tr -d '[:space:]')"
missing_count="$(wc -l < "$missing_symbols" | tr -d '[:space:]')"
extra_count="$(wc -l < "$extra_symbols" | tr -d '[:space:]')"
allowlist_count="$(wc -l < "$allowlist_symbols" | tr -d '[:space:]')"
unexpected_extra_count="$(wc -l < "$unexpected_extra_symbols" | tr -d '[:space:]')"
stale_allowlist_count="$(wc -l < "$stale_allowlist_symbols" | tr -d '[:space:]')"

echo "startup doc stub coverage:"
echo "  oracle integer-doc symbols: $oracle_count"
echo "  startup stub symbols: $stub_count"
echo "  missing startup stubs: $missing_count"
echo "  extra startup stubs: $extra_count"
echo "  extra allowlist entries: $allowlist_count"

if [[ "$missing_count" -ne 0 ]]; then
  echo "missing startup doc stubs (must be added to STARTUP_VARIABLE_DOC_STUBS):" >&2
  cat "$missing_symbols" >&2
  exit 1
fi

if [[ "${SHOW_EXTRA_STUBS:-0}" == "1" && "$extra_count" -ne 0 ]]; then
  echo "extra startup stubs not currently required by oracle integer docs:"
  cat "$extra_symbols"
fi

if [[ "$unexpected_extra_count" -ne 0 ]]; then
  echo "unexpected extra startup stubs not in allowlist:" >&2
  cat "$unexpected_extra_symbols" >&2
  exit 1
fi

if [[ "$stale_allowlist_count" -ne 0 ]]; then
  echo "stale startup-doc extra allowlist entries (not currently extra):" >&2
  cat "$stale_allowlist_symbols" >&2
  exit 1
fi

echo "startup doc stub coverage check passed"
