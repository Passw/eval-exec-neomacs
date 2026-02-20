#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
coverage_script="$script_dir/report-oracle-builtin-coverage.sh"
mode="${ORACLE_BUILTIN_UNIVERSE:-primitive-any}"

if [[ ! -x "$coverage_script" ]]; then
  echo "coverage script not executable: $coverage_script" >&2
  exit 2
fi

tmp_report="$(mktemp)"
cleanup() {
  rm -f "$tmp_report"
}
trap cleanup EXIT

ORACLE_BUILTIN_UNIVERSE="$mode" "$coverage_script" > "$tmp_report"
cat "$tmp_report"

registry_missing="$(awk -F': ' '/^oracle builtin names missing from registry:/ { print $2 }' "$tmp_report" | head -n 1)"
runtime_missing="$(awk -F': ' '/^oracle builtin names missing in neovm runtime:/ { print $2 }' "$tmp_report" | head -n 1)"
runtime_outside_registry="$(awk -F': ' '/^neovm runtime covered oracle builtins outside registry:/ { print $2 }' "$tmp_report" | head -n 1)"

registry_missing="${registry_missing:-0}"
runtime_missing="${runtime_missing:-0}"
runtime_outside_registry="${runtime_outside_registry:-0}"

echo "primitive-any coverage gate mode: $mode"
echo "primitive-any registry missing: $registry_missing"
echo "primitive-any runtime missing: $runtime_missing"
echo "primitive-any runtime outside registry: $runtime_outside_registry"

if [[ "$registry_missing" -ne 0 || "$runtime_missing" -ne 0 || "$runtime_outside_registry" -ne 0 ]]; then
  echo "primitive-any builtin coverage gate failed" >&2
  exit 1
fi

echo "primitive-any builtin coverage gate passed"
