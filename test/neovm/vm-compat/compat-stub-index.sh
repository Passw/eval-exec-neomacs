#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
ELISP_DIR="${REPO_ROOT}/rust/neovm-core/src/elisp"

stubs_tmp="$(mktemp)"
trap 'rm -f "${stubs_tmp}"' EXIT

echo "NeoVM explicit function stub index"
echo "================================="
rg -n --no-heading -g '*.rs' -i 'stub' "${ELISP_DIR}" > "${stubs_tmp}" \
  || true

if [ ! -s "${stubs_tmp}" ]; then
  echo "explicitly annotated function stubs: 0"
  exit 0
fi

declare -a entries=()
while IFS=: read -r file line text; do
  name="$(printf "%s" "${text}" | perl -ne 'if (/`\(([^[:space:]()]+)[[:space:]\)]/) { print $1; exit }')"
  [ -z "${name}" ] && continue
  relative="${file#"${REPO_ROOT}/"}"
  entries+=("${name}\t${relative}:${line}")
done < "${stubs_tmp}"

if [ "${#entries[@]}" -eq 0 ]; then
  echo "explicitly annotated function stubs: 0"
  exit 0
fi

uniq_names="$(printf "%s\n" "${entries[@]}" | cut -f1 | sort -u)"
count="$(printf "%s\n" "${uniq_names}" | wc -l | tr -d ' ')"
file_count="$(printf "%s\n" "${entries[@]}" | cut -f2 | cut -d: -f1 | sort -u | wc -l | tr -d ' ')"

echo "explicitly annotated function stubs: ${count}"
printf "across %s files\n" "$file_count"
printf "%s\n" "${uniq_names}"
printf "\n"
printf "stub call-sites:"
printf "\n"
printf "%s\n" "${entries[@]}" | sort -u
