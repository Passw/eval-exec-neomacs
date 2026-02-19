#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tracked_lists_file="$script_dir/cases/tracked-lists.txt"

default_list_files=(
  "$script_dir/cases/default.list"
  "$script_dir/cases/neovm-only.list"
  "$script_dir/cases/legacy-elc-literal.list"
  "$script_dir/cases/introspection.list"
  "$script_dir/cases/thread.list"
  "$script_dir/cases/startup-doc.list"
)
list_files=()

if [[ -f "$tracked_lists_file" ]]; then
  while IFS= read -r rel_path; do
    rel_path="${rel_path%$'\r'}"
    [[ -n "$rel_path" ]] || continue
    [[ "$rel_path" =~ ^[[:space:]]*# ]] && continue
    if [[ "$rel_path" = /* ]]; then
      list_files+=("$rel_path")
    else
      list_files+=("$script_dir/$rel_path")
    fi
  done < "$tracked_lists_file"
fi

if [[ ${#list_files[@]} -eq 0 ]]; then
  list_files=("${default_list_files[@]}")
fi

tmp_list="$(mktemp)"
tmp_forms="$(mktemp)"
tmp_missing="$(mktemp)"
tmp_unreferenced="$(mktemp)"
trap 'rm -f "$tmp_list" "$tmp_forms" "$tmp_missing" "$tmp_unreferenced"' EXIT

for list_file in "${list_files[@]}"; do
  if [[ -f "$list_file" ]]; then
    awk 'NF && $1 !~ /^#/ { sub(/^cases\//, "", $1); print $1 }' "$list_file" >> "$tmp_list"
  else
    echo "warning: list file not found: $list_file" >&2
  fi
done

sort -u "$tmp_list" -o "$tmp_list"

find "$script_dir/cases" -maxdepth 1 -type f -name '*.forms' -printf '%f\n' \
  | sed 's/\.forms$//' | sort -u > "$tmp_forms"

comm -23 "$tmp_list" "$tmp_forms" > "$tmp_missing"
comm -23 "$tmp_forms" "$tmp_list" > "$tmp_unreferenced"

listed_count="$(wc -l < "$tmp_list" | tr -d ' ')"
form_count="$(wc -l < "$tmp_forms" | tr -d ' ')"
missing_count="$(wc -l < "$tmp_missing" | tr -d ' ')"
unreferenced_count="$(wc -l < "$tmp_unreferenced" | tr -d ' ')"

echo "case inventory summary"
echo "- forms files under cases/: $form_count"
echo "- entries in list files: $listed_count"
echo "- unreferenced forms: $unreferenced_count"
echo "- listed missing forms: $missing_count"

if [[ "$missing_count" -gt 0 ]]; then
  echo "listed entries missing .forms files:"
  cat "$tmp_missing"
  exit 1
fi

if [[ "$unreferenced_count" -gt 0 ]]; then
  echo "unreferenced forms (consider adding to a list):"
  cat "$tmp_unreferenced"
  if [[ "${NEOVM_ALLOW_UNREFERENCED_FORMS:-0}" != "1" ]]; then
    echo "set NEOVM_ALLOW_UNREFERENCED_FORMS=1 to report without failing" >&2
    exit 1
  fi
fi
