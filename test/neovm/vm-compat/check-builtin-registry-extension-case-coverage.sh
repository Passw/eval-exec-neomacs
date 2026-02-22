#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
policy_file="${1:-$script_dir/cases/builtin-registry-extension-policy.txt}"
neovm_only_list="${2:-$script_dir/cases/neovm-only.list}"

if [[ ! -f "$policy_file" ]]; then
  echo "extension policy file not found: $policy_file" >&2
  exit 2
fi

if [[ ! -f "$neovm_only_list" ]]; then
  echo "neovm-only case list not found: $neovm_only_list" >&2
  exit 2
fi

mapfile -t extension_symbols < <(awk 'NF && $1 !~ /^#/ { print $1 }' "$policy_file")
mapfile -t neovm_only_cases < <(awk 'NF && $1 !~ /^#/ { print $1 }' "$neovm_only_list")

if [[ "${#extension_symbols[@]}" -eq 0 ]]; then
  echo "extension policy entries: 0"
  echo "builtin registry extension case coverage check passed"
  exit 0
fi

if [[ "${#neovm_only_cases[@]}" -eq 0 ]]; then
  echo "neovm-only case list is empty while extension policy is non-empty" >&2
  exit 1
fi

forms_files=()
for case_path in "${neovm_only_cases[@]}"; do
  forms_path="$script_dir/$case_path.forms"
  if [[ ! -f "$forms_path" ]]; then
    echo "missing forms for neovm-only case: $forms_path" >&2
    exit 1
  fi
  forms_files+=("$forms_path")
done

missing_symbols=()
for symbol in "${extension_symbols[@]}"; do
  if ! rg -q --fixed-strings -- "$symbol" "${forms_files[@]}"; then
    missing_symbols+=("$symbol")
  fi
done

echo "extension policy entries: ${#extension_symbols[@]}"
echo "neovm-only forms scanned: ${#forms_files[@]}"

if [[ "${#missing_symbols[@]}" -gt 0 ]]; then
  echo
  echo "extension symbols missing from neovm-only forms:"
  printf '%s\n' "${missing_symbols[@]}"
  exit 1
fi

echo "builtin registry extension case coverage check passed"
