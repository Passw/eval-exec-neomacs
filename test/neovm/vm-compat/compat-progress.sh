#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
registry_file="$repo_root/rust/neovm-core/src/elisp/builtin_registry.rs"
allowlist_file="$script_dir/cases/builtin-registry-fboundp-allowlist.txt"
source "$script_dir/lib/builtin-registry.sh"

if [[ ! -f "$registry_file" ]]; then
  echo "missing registry file: $registry_file" >&2
  exit 2
fi

if [[ ! -f "$allowlist_file" ]]; then
  echo "missing allowlist file: $allowlist_file" >&2
  exit 2
fi

tmp_all="$(mktemp)"
tmp_core="$(mktemp)"
tmp_tracker="$(mktemp)"
cleanup() {
  rm -f "$tmp_all" "$tmp_core" "$tmp_tracker"
}
trap cleanup EXIT

count_lines() {
  local file="$1"
  if [[ ! -f "$file" ]]; then
    echo 0
    return
  fi
  awk 'NF && $1 !~ /^#/ { count++ } END { print count+0 }' "$file"
}

collect_dispatch_builtin_names "$registry_file" "$tmp_all"
collect_core_dispatch_builtin_names "$tmp_all" "$tmp_core"
{
  awk 'NF && $1 !~ /^#/ { print $1 }' "$script_dir/cases/default.list"
  awk 'NF && $1 !~ /^#/ { print $1 }' "$script_dir/cases/neovm-only.list"
  awk 'NF && $1 !~ /^#/ { print $1 }' "$script_dir/cases/legacy-elc-literal.list"
  awk 'NF && $1 !~ /^#/ { print $1 }' "$script_dir/cases/introspection.list"
  awk 'NF && $1 !~ /^#/ { print $1 }' "$script_dir/cases/thread.list"
} > "$tmp_tracker"

all_builtins="$(wc -l < "$tmp_all" | tr -d ' ')"
core_builtins="$(wc -l < "$tmp_core" | tr -d ' ')"
extension_builtins="$((all_builtins - core_builtins))"
allowlisted="$(awk 'NF && $1 !~ /^#/ { count++ } END { print count+0 }' "$allowlist_file")"
tracked_unique="$(sort -u "$tmp_tracker" | awk 'END { print NR+0 }')"

printf 'compat progress snapshot\n'
printf 'case lists (entries):\n'
printf '  default: %s\n' "$(count_lines "$script_dir/cases/default.list")"
printf '  neovm-only: %s\n' "$(count_lines "$script_dir/cases/neovm-only.list")"
printf '  legacy-elc: %s\n' "$(count_lines "$script_dir/cases/legacy-elc-literal.list")"
printf '  introspection: %s\n' "$(count_lines "$script_dir/cases/introspection.list")"
printf '  thread: %s\n' "$(count_lines "$script_dir/cases/thread.list")"
printf '  total unique tracked: %s\n' "$tracked_unique"

printf 'builtin registry:\n'
printf '  total dispatch entries: %s\n' "$all_builtins"
printf '  core-compat entries: %s\n' "$core_builtins"
printf '  neovm extension entries: %s\n' "$extension_builtins"
printf '  allowed fboundp drifts: %s\n' "$allowlisted"

echo "done"
