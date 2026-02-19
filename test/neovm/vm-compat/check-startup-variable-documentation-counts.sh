#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
count_forms_file="${1:-$script_dir/cases/startup-variable-documentation-counts-semantics.forms}"
count_expected_file="${2:-$script_dir/cases/startup-variable-documentation-counts-semantics.expected.tsv}"
runtime_resolution_forms_file="${3:-$script_dir/cases/startup-variable-documentation-runtime-resolution-semantics.forms}"
runtime_resolution_expected_file="${4:-$script_dir/cases/startup-variable-documentation-runtime-resolution-semantics.expected.tsv}"

parse_counts() {
  local result="$1"
  if [[ "$result" =~ ^OK\ \(([0-9]+)\ ([0-9]+)\)$ ]]; then
    printf '%s/%s\n' "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
    return 0
  fi
  return 1
}

check_case() {
  local case_label="$1"
  local forms_file="$2"
  local expected_file="$3"

  if [[ ! -f "$forms_file" ]]; then
    echo "forms file not found for $case_label: $forms_file" >&2
    exit 2
  fi

  if [[ ! -f "$expected_file" ]]; then
    echo "expected file not found for $case_label: $expected_file" >&2
    exit 2
  fi

  local oracle_result
  local neovm_result
  local expected_result
  oracle_result="$("$script_dir/run-oracle.sh" "$forms_file" | awk -F'\t' 'NR==1 { print $3; exit }')"
  neovm_result="$("$script_dir/run-neovm.sh" "$forms_file" | awk -F'\t' 'NR==1 { print $3; exit }')"
  expected_result="$(awk -F'\t' 'NR==1 { print $3; exit }' "$expected_file")"

  if [[ -z "$expected_result" || -z "$oracle_result" || -z "$neovm_result" ]]; then
    echo "failed to extract startup variable-documentation tuple for $case_label" >&2
    echo "expected result: ${expected_result:-<empty>}" >&2
    echo "oracle result: ${oracle_result:-<empty>}" >&2
    echo "neovm result: ${neovm_result:-<empty>}" >&2
    exit 1
  fi

  local expected_counts
  local oracle_counts
  local neovm_counts
  expected_counts="$(parse_counts "$expected_result" || true)"
  oracle_counts="$(parse_counts "$oracle_result" || true)"
  neovm_counts="$(parse_counts "$neovm_result" || true)"

  if [[ -z "$expected_counts" || -z "$oracle_counts" || -z "$neovm_counts" ]]; then
    echo "unexpected startup variable-documentation tuple shape for $case_label" >&2
    echo "expected result: $expected_result" >&2
    echo "oracle result: $oracle_result" >&2
    echo "neovm result: $neovm_result" >&2
    exit 1
  fi

  echo "startup variable-documentation $case_label parity:"
  echo "  expected integer/string: $expected_counts"
  echo "  oracle integer/string: $oracle_counts"
  echo "  neovm integer/string: $neovm_counts"
  echo "startup variable-documentation $case_label summary: $expected_counts|$oracle_counts|$neovm_counts"

  if [[ "$oracle_result" != "$expected_result" ]]; then
    echo "oracle startup variable-documentation $case_label drifted from recorded baseline" >&2
    echo "expected: $expected_result" >&2
    echo "oracle: $oracle_result" >&2
    exit 1
  fi

  if [[ "$neovm_result" != "$oracle_result" ]]; then
    echo "neovm startup variable-documentation $case_label drift from oracle" >&2
    echo "oracle: $oracle_result" >&2
    echo "neovm: $neovm_result" >&2
    exit 1
  fi
}

check_case "property-count" "$count_forms_file" "$count_expected_file"
check_case "runtime-resolution" "$runtime_resolution_forms_file" "$runtime_resolution_expected_file"

echo "startup variable-documentation count check passed"
