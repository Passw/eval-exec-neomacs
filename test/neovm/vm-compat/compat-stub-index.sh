#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
ELISP_DIR="${REPO_ROOT}/neovm-core/src/elisp"
FORMAT="${STUB_INDEX_FORMAT:-text}"

stubs_tmp="$(mktemp)"
trap 'rm -f "${stubs_tmp}"' EXIT

json_escape() {
  printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/\r/\\r/g' -e 's/\n/\\n/g'
}

emit_json() {
  local names_text="$1"
  local sites_text="$2"
  local count="$3"
  local file_count="$4"
  local site_count="$5"
  local -a names=()
  local -a sites=()
  local i=0

  if [ -n "$names_text" ]; then
    mapfile -t names <<< "$names_text"
  fi
  if [ -n "$sites_text" ]; then
    mapfile -t sites <<< "$sites_text"
  fi

  printf '{\n'
  printf '  "explicitly_annotated_function_stubs": %s,\n' "$count"
  printf '  "files_with_stub_comments": %s,\n' "$file_count"
  printf '  "stub_comment_call_sites": %s,\n' "$site_count"
  printf '  "functions": [\n'
  for i in "${!names[@]}"; do
    if [ "$i" -gt 0 ]; then
      printf ',\n'
    fi
    printf '    "%s"' "$(json_escape "${names[$i]}")"
  done
  printf '\n  ],\n'

  printf '  "call_sites": [\n'
  for i in "${!sites[@]}"; do
    if [ "$i" -gt 0 ]; then
      printf ',\n'
    fi
    printf '    "%s"' "$(json_escape "${sites[$i]}")"
  done
  printf '\n  ]\n'
  printf '}\n'
}

for arg in "$@"; do
  case "$arg" in
    --json)
      FORMAT="json"
      ;;
    --text)
      FORMAT="text"
      ;;
    --help|-h)
      echo "Usage: $0 [--json|--text]"
      exit 0
      ;;
    *)
      echo "unknown argument: $arg" >&2
      echo "Usage: $0 [--json|--text]" >&2
      exit 2
      ;;
  esac
done

if [ "${FORMAT}" = "text" ]; then
  echo "NeoVM explicit function stub index"
  echo "================================="
fi
rg -n --no-heading -g '*.rs' -i 'stub' "${ELISP_DIR}" > "${stubs_tmp}" \
  || true

if [ ! -s "${stubs_tmp}" ]; then
  if [ "${FORMAT}" = "json" ]; then
  emit_json "" "" "0" "0" "0"
  else
    echo "explicitly annotated function stubs: 0"
  fi
  exit 0
fi

declare -a entries=()
while IFS=: read -r file line text; do
  name="$(printf "%s" "${text}" | perl -ne 'if (/`\(([^[:space:]()]+)[[:space:]\)]/) { print $1; exit }')"
  [ -z "${name}" ] && continue
  relative="${file#"${REPO_ROOT}/"}"
  entries+=("${name}|${relative}:${line}")
done < "${stubs_tmp}"

if [ "${#entries[@]}" -eq 0 ]; then
  if [ "${FORMAT}" = "json" ]; then
    emit_json "" "" "0" "0" "0"
  else
    echo "explicitly annotated function stubs: 0"
  fi
  exit 0
fi

uniq_names="$(printf "%s\n" "${entries[@]}" | cut -d'|' -f1 | sort -u)"
count="$(printf "%s\n" "${uniq_names}" | wc -l | tr -d ' ')"
file_count="$(printf "%s\n" "${entries[@]}" | cut -d'|' -f2 | cut -d: -f1 | sort -u | wc -l | tr -d ' ')"
site_count="$(printf "%s\n" "${entries[@]}" | wc -l | tr -d ' ')"
sorted_sites="$(printf "%s\n" "${entries[@]}" | sort -u)"

if [ "${FORMAT}" = "json" ]; then
  emit_json "${uniq_names}" "${sorted_sites}" "${count}" "${file_count}" "${site_count}"
  exit 0
fi

echo "explicitly annotated function stubs: ${count}"
printf "across %s files, %s comment call-sites\n" "$file_count" "$site_count"
printf "%s\n" "${uniq_names}"
printf "\n"
printf "stub call-sites:"
printf "\n"
printf "%s\n" "${sorted_sites}"
