#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
registry_file="$repo_root/rust/neovm-core/src/elisp/builtin_registry.rs"
builtins_file="$repo_root/rust/neovm-core/src/elisp/builtins.rs"
allowlist_file="${1:-$script_dir/cases/builtin-registry-sync-allowlist.txt}"
source "$script_dir/lib/builtin-registry.sh"

if [[ ! -f "$registry_file" ]]; then
  echo "builtin registry not found: $registry_file" >&2
  exit 2
fi

if [[ ! -f "$builtins_file" ]]; then
  echo "builtin dispatch source not found: $builtins_file" >&2
  exit 2
fi

if [[ ! -f "$allowlist_file" ]]; then
  echo "sync allowlist file not found: $allowlist_file" >&2
  exit 2
fi

tmp_registry="$(mktemp)"
tmp_eval_dispatch="$(mktemp)"
tmp_missing="$(mktemp)"
tmp_allowlist="$(mktemp)"
tmp_unexpected_missing="$(mktemp)"
tmp_stale_allowlist="$(mktemp)"

cleanup() {
  rm -f \
    "$tmp_registry" \
    "$tmp_eval_dispatch" \
    "$tmp_missing" \
    "$tmp_allowlist" \
    "$tmp_unexpected_missing" \
    "$tmp_stale_allowlist"
}
trap cleanup EXIT

collect_dispatch_builtin_names "$registry_file" "$tmp_registry"
sort -u "$tmp_registry" -o "$tmp_registry"

if [[ ! -s "$tmp_registry" ]]; then
  echo "failed to parse builtin names from registry" >&2
  exit 1
fi

# Evaluator-backed names are direct dispatch_builtin string match arms.
awk '
  /pub\(crate\) fn dispatch_builtin\(/ { in_fn=1; next }
  in_fn && /match name[[:space:]]*\{/ { in_dispatch_match=1; next }
  in_dispatch_match && /^[[:space:]]*_ =>[[:space:]]*\{/ { in_dispatch_match=0; next }
  in_dispatch_match && /^[[:space:]]*"[-+*/%<>=a-z0-9]+"[[:space:]]*=>/ {
    line = $0
    sub(/^[[:space:]]*"/, "", line)
    sub(/".*$/, "", line)
    print line
  }
' "$builtins_file" | sort -u > "$tmp_eval_dispatch"

comm -23 "$tmp_eval_dispatch" "$tmp_registry" > "$tmp_missing"
awk 'NF && $1 !~ /^#/ { print $1 }' "$allowlist_file" | sort -u > "$tmp_allowlist"
comm -23 "$tmp_missing" "$tmp_allowlist" > "$tmp_unexpected_missing"
comm -13 "$tmp_missing" "$tmp_allowlist" > "$tmp_stale_allowlist"

dispatch_total="$(wc -l < "$tmp_eval_dispatch" | tr -d ' ')"
registry_total="$(wc -l < "$tmp_registry" | tr -d ' ')"
missing_total="$(wc -l < "$tmp_missing" | tr -d ' ')"
allowlist_total="$(wc -l < "$tmp_allowlist" | tr -d ' ')"

echo "evaluator dispatch names discovered from builtins.rs: $dispatch_total"
echo "registry names in builtin_registry.rs: $registry_total"
echo "missing evaluator dispatch names: $missing_total"
echo "sync allowlist entries: $allowlist_total"

if [[ -s "$tmp_unexpected_missing" ]]; then
  echo
  echo "unexpected evaluator dispatch names missing from registry:"
  cat "$tmp_unexpected_missing"
  exit 1
fi

if [[ -s "$tmp_stale_allowlist" ]]; then
  echo
  echo "stale sync allowlist entries (not missing anymore):"
  cat "$tmp_stale_allowlist"
  exit 1
fi

echo "builtin registry dispatch/registry sync check passed"
