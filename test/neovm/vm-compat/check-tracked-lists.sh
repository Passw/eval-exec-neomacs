#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tracked_lists_file="$script_dir/cases/tracked-lists.txt"

if [[ ! -f "$tracked_lists_file" ]]; then
  echo "missing tracked lists file: $tracked_lists_file" >&2
  exit 1
fi

tmp_entries="$(mktemp)"
trap 'rm -f "$tmp_entries"' EXIT

awk 'NF && $1 !~ /^#/ { print $1 }' "$tracked_lists_file" > "$tmp_entries"

entry_count="$(wc -l < "$tmp_entries" | tr -d ' ')"
if [[ "$entry_count" -eq 0 ]]; then
  echo "tracked lists file has no list entries: $tracked_lists_file" >&2
  exit 1
fi

dups="$(sort "$tmp_entries" | uniq -d)"
if [[ -n "$dups" ]]; then
  echo "duplicate entries in tracked lists file: $tracked_lists_file" >&2
  printf '%s\n' "$dups" >&2
  exit 1
fi

while IFS= read -r rel_path; do
  if [[ "$rel_path" = /* ]]; then
    echo "absolute path not allowed in tracked lists file: $rel_path" >&2
    exit 1
  fi
  if [[ ! "$rel_path" =~ ^cases/.+\.list$ ]]; then
    echo "invalid tracked list path (expected cases/*.list): $rel_path" >&2
    exit 1
  fi
  if [[ ! -f "$script_dir/$rel_path" ]]; then
    echo "tracked list file not found: $rel_path" >&2
    exit 1
  fi
done < "$tmp_entries"

echo "tracked list manifest valid: $entry_count files"
