#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
registry_file="$repo_root/rust/neovm-core/src/elisp/builtin_registry.rs"
allowlist_file="${1:-$script_dir/cases/builtin-registry-func-arity-allowlist.txt}"
source "$script_dir/lib/builtin-registry.sh"

if [[ ! -f "$registry_file" ]]; then
  echo "builtin registry not found: $registry_file" >&2
  exit 2
fi

if [[ ! -f "$allowlist_file" ]]; then
  echo "allowlist file not found: $allowlist_file" >&2
  exit 2
fi

tmp_names="$(mktemp)"
tmp_core_names="$(mktemp)"
tmp_forms="$(mktemp)"
tmp_oracle="$(mktemp)"
tmp_neovm="$(mktemp)"
tmp_oracle_results="$(mktemp)"
tmp_neovm_results="$(mktemp)"
tmp_mismatches="$(mktemp)"
tmp_allow="$(mktemp)"
tmp_unexpected="$(mktemp)"
tmp_stale="$(mktemp)"

cleanup() {
  rm -f \
    "$tmp_names" \
    "$tmp_core_names" \
    "$tmp_forms" \
    "$tmp_oracle" \
    "$tmp_neovm" \
    "$tmp_oracle_results" \
    "$tmp_neovm_results" \
    "$tmp_mismatches" \
    "$tmp_allow" \
    "$tmp_unexpected" \
    "$tmp_stale"
}
trap cleanup EXIT

collect_dispatch_builtin_names "$registry_file" "$tmp_names"

if [[ ! -s "$tmp_names" ]]; then
  echo "failed to parse builtin names from registry" >&2
  exit 1
fi

# Policy-declared extension symbols are intentionally absent in GNU Emacs;
# exclude them from oracle parity comparison.
collect_core_dispatch_builtin_names "$tmp_names" "$tmp_core_names"

if [[ ! -s "$tmp_core_names" ]]; then
  echo "failed to derive core builtin names from registry" >&2
  exit 1
fi

awk '
  {
    gsub(/\\/, "\\\\", $0)
    gsub(/"/, "\\\"", $0)
    printf "(let* ((sym (intern \"%s\")) (ok (fboundp sym))) (list ok (if ok (condition-case err (func-arity sym) (error (cons :error (list (car err))))) :unbound)))\n", $0
  }
' "$tmp_core_names" > "$tmp_forms"

"$script_dir/run-oracle.sh" "$tmp_forms" > "$tmp_oracle"
"$script_dir/run-neovm.sh" "$tmp_forms" > "$tmp_neovm"

core_total="$(wc -l < "$tmp_core_names" | tr -d ' ')"
oracle_count="$(wc -l < "$tmp_oracle" | tr -d ' ')"
neovm_count="$(wc -l < "$tmp_neovm" | tr -d ' ')"

if [[ "$oracle_count" -ne "$core_total" ]]; then
  echo "oracle result line count mismatch: expected $core_total, got $oracle_count" >&2
  exit 1
fi

if [[ "$neovm_count" -ne "$core_total" ]]; then
  echo "neovm result line count mismatch: expected $core_total, got $neovm_count" >&2
  exit 1
fi

cut -f3 "$tmp_oracle" > "$tmp_oracle_results"
cut -f3 "$tmp_neovm" > "$tmp_neovm_results"

paste "$tmp_core_names" "$tmp_oracle_results" "$tmp_neovm_results" | \
  awk -F '\t' '$2 != $3 { printf "%s\t%s\t%s\n", $1, $2, $3 }' > "$tmp_mismatches"

awk '
  NF && $1 !~ /^#/ { print $1 }
' "$allowlist_file" | sort -u > "$tmp_allow"

cut -f1 "$tmp_mismatches" | sort -u | comm -23 - "$tmp_allow" > "$tmp_unexpected"
comm -23 "$tmp_allow" <(cut -f1 "$tmp_mismatches" | sort -u) > "$tmp_stale"

total="$(wc -l < "$tmp_names" | tr -d ' ')"
extension_total="$((total - core_total))"
drift="$(wc -l < "$tmp_mismatches" | tr -d ' ')"
unexpected="$(wc -l < "$tmp_unexpected" | tr -d ' ')"
stale="$(wc -l < "$tmp_stale" | tr -d ' ')"

echo "checked DISPATCH_BUILTIN_NAMES entries: $total"
echo "checked core func-arity parity entries: $core_total"
echo "skipped extension entries: $extension_total"
echo "oracle/neovm func-arity drifts: $drift"
echo "allowlist entries: $(wc -l < "$tmp_allow" | tr -d ' ')"

if [[ "$drift" -gt 0 ]]; then
  echo
  echo "drift details (name | oracle | neovm):"
  cat "$tmp_mismatches"
fi

if [[ "$unexpected" -gt 0 ]]; then
  echo
  echo "unexpected drifts not in allowlist:"
  cat "$tmp_unexpected"
  exit 1
fi

if [[ "$stale" -gt 0 ]]; then
  echo
  echo "stale allowlist entries with no current drift:"
  cat "$tmp_stale"
  exit 1
fi

echo "builtin registry func-arity parity check passed"
