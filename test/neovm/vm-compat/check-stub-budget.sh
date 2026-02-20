#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

max_budget="${NEOVM_STUB_BUDGET:-0}"

"$script_dir/compat-stub-index.sh" --text > "$tmp"

stub_count="$(awk '/^explicitly annotated function stubs:/ { print $5; exit }' "$tmp")"
if [ -z "${stub_count}" ]; then
  echo "could not parse explicit stub count"
  exit 2
fi

if [ "${stub_count}" -gt "${max_budget}" ]; then
  echo "explicit stub budget exceeded: ${stub_count} > ${max_budget}"
  echo "offending call-sites:"
  awk '
    BEGIN { capture=0 }
    /^stub call-sites:$/ { capture=1; next }
    capture { print }
  ' "$tmp"
  exit 1
fi

echo "explicit stubs within budget: ${stub_count} / ${max_budget}"
