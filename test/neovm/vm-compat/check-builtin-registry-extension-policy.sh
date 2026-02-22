#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
registry_file="$repo_root/rust/neovm-core/src/elisp/builtin_registry.rs"
policy_file="${1:-$script_dir/cases/builtin-registry-extension-policy.txt}"
source "$script_dir/lib/builtin-registry.sh"

if [[ ! -f "$registry_file" ]]; then
  echo "builtin registry not found: $registry_file" >&2
  exit 2
fi

if [[ ! -f "$policy_file" ]]; then
  echo "extension policy file not found: $policy_file" >&2
  exit 2
fi

tmp_names="$(mktemp)"
tmp_extensions="$(mktemp)"
tmp_policy="$(mktemp)"
tmp_forms="$(mktemp)"
tmp_oracle="$(mktemp)"
tmp_neovm="$(mktemp)"
tmp_oracle_results="$(mktemp)"
tmp_neovm_results="$(mktemp)"
tmp_unexpected="$(mktemp)"
tmp_missing="$(mktemp)"
tmp_bad_oracle="$(mktemp)"
tmp_bad_neovm="$(mktemp)"

cleanup() {
  rm -f \
    "$tmp_names" \
    "$tmp_extensions" \
    "$tmp_policy" \
    "$tmp_forms" \
    "$tmp_oracle" \
    "$tmp_neovm" \
    "$tmp_oracle_results" \
    "$tmp_neovm_results" \
    "$tmp_unexpected" \
    "$tmp_missing" \
    "$tmp_bad_oracle" \
    "$tmp_bad_neovm"
}
trap cleanup EXIT

collect_dispatch_builtin_names "$registry_file" "$tmp_names"

if [[ ! -s "$tmp_names" ]]; then
  echo "failed to parse builtin names from registry" >&2
  exit 1
fi

collect_extension_dispatch_builtin_names "$tmp_names" "$tmp_extensions" "$policy_file"
awk 'NF && $1 !~ /^#/ { print $1 }' "$policy_file" | sort -u > "$tmp_policy"

comm -23 "$tmp_extensions" "$tmp_policy" > "$tmp_unexpected"
comm -13 "$tmp_extensions" "$tmp_policy" > "$tmp_missing"

if [[ -s "$tmp_unexpected" ]]; then
  echo "unexpected extension builtins in registry (not in policy file):"
  cat "$tmp_unexpected"
  exit 1
fi

if [[ -s "$tmp_missing" ]]; then
  echo "policy file lists extension builtins missing from registry:"
  cat "$tmp_missing"
  exit 1
fi

extension_total="$(wc -l < "$tmp_extensions" | tr -d ' ')"
policy_total="$(wc -l < "$tmp_policy" | tr -d ' ')"
total="$(wc -l < "$tmp_names" | tr -d ' ')"

if [[ "$extension_total" -eq 0 ]]; then
  echo "checked DISPATCH_BUILTIN_NAMES entries: $total"
  echo "extension entries: 0"
  echo "policy entries: $policy_total"
  echo "builtin registry extension policy check passed"
  exit 0
fi

awk '
  {
    gsub(/\\/, "\\\\", $0)
    gsub(/"/, "\\\"", $0)
    printf "(fboundp (intern \"%s\"))\n", $0
  }
' "$tmp_extensions" > "$tmp_forms"

"$script_dir/run-oracle.sh" "$tmp_forms" > "$tmp_oracle"
"$script_dir/run-neovm.sh" "$tmp_forms" > "$tmp_neovm"

oracle_count="$(wc -l < "$tmp_oracle" | tr -d ' ')"
neovm_count="$(wc -l < "$tmp_neovm" | tr -d ' ')"

if [[ "$oracle_count" -ne "$extension_total" ]]; then
  echo "oracle extension probe line count mismatch: expected $extension_total, got $oracle_count" >&2
  exit 1
fi

if [[ "$neovm_count" -ne "$extension_total" ]]; then
  echo "neovm extension probe line count mismatch: expected $extension_total, got $neovm_count" >&2
  exit 1
fi

cut -f3 "$tmp_oracle" > "$tmp_oracle_results"
cut -f3 "$tmp_neovm" > "$tmp_neovm_results"

paste "$tmp_extensions" "$tmp_oracle_results" "$tmp_neovm_results" | \
  awk -F '\t' '$2 != "OK nil" { print $1 "\t" $2 }' > "$tmp_bad_oracle"

paste "$tmp_extensions" "$tmp_oracle_results" "$tmp_neovm_results" | \
  awk -F '\t' '$3 != "OK t" { print $1 "\t" $3 }' > "$tmp_bad_neovm"

echo "checked DISPATCH_BUILTIN_NAMES entries: $total"
echo "extension entries: $extension_total"
echo "policy entries: $policy_total"

if [[ -s "$tmp_bad_oracle" ]]; then
  echo
  echo "extension names unexpectedly present in oracle:"
  cat "$tmp_bad_oracle"
  exit 1
fi

if [[ -s "$tmp_bad_neovm" ]]; then
  echo
  echo "extension names unexpectedly missing in neovm:"
  cat "$tmp_bad_neovm"
  exit 1
fi

echo "builtin registry extension policy check passed"
