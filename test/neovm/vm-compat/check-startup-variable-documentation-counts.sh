#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
forms_file="${1:-$script_dir/cases/startup-variable-documentation-counts-semantics.forms}"
expected_file="${2:-$script_dir/cases/startup-variable-documentation-counts-semantics.expected.tsv}"

if [[ ! -f "$forms_file" ]]; then
  echo "forms file not found: $forms_file" >&2
  exit 2
fi

if [[ ! -f "$expected_file" ]]; then
  echo "expected file not found: $expected_file" >&2
  exit 2
fi

oracle_result="$("$script_dir/run-oracle.sh" "$forms_file" | awk -F'\t' 'NR==1 { print $3; exit }')"
neovm_result="$("$script_dir/run-neovm.sh" "$forms_file" | awk -F'\t' 'NR==1 { print $3; exit }')"
expected_result="$(awk -F'\t' 'NR==1 { print $3; exit }' "$expected_file")"

if [[ -z "$expected_result" || -z "$oracle_result" || -z "$neovm_result" ]]; then
  echo "failed to extract startup variable-documentation count tuple" >&2
  echo "expected result: ${expected_result:-<empty>}" >&2
  echo "oracle result: ${oracle_result:-<empty>}" >&2
  echo "neovm result: ${neovm_result:-<empty>}" >&2
  exit 1
fi

parse_counts() {
  local result="$1"
  if [[ "$result" =~ ^OK\ \(([0-9]+)\ ([0-9]+)\)$ ]]; then
    printf '%s/%s\n' "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
    return 0
  fi
  return 1
}

expected_counts="$(parse_counts "$expected_result" || true)"
oracle_counts="$(parse_counts "$oracle_result" || true)"
neovm_counts="$(parse_counts "$neovm_result" || true)"

if [[ -z "$expected_counts" || -z "$oracle_counts" || -z "$neovm_counts" ]]; then
  echo "unexpected startup variable-documentation tuple shape" >&2
  echo "expected result: $expected_result" >&2
  echo "oracle result: $oracle_result" >&2
  echo "neovm result: $neovm_result" >&2
  exit 1
fi

echo "startup variable-documentation count parity:"
echo "  expected integer/string: $expected_counts"
echo "  oracle integer/string: $oracle_counts"
echo "  neovm integer/string: $neovm_counts"

if [[ "$oracle_result" != "$expected_result" ]]; then
  echo "oracle startup variable-documentation counts drifted from recorded baseline" >&2
  echo "expected: $expected_result" >&2
  echo "oracle: $oracle_result" >&2
  exit 1
fi

if [[ "$neovm_result" != "$oracle_result" ]]; then
  echo "neovm startup variable-documentation counts drift from oracle" >&2
  echo "oracle: $oracle_result" >&2
  echo "neovm: $neovm_result" >&2
  exit 1
fi

echo "startup variable-documentation count check passed"
