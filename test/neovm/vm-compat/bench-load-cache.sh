#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "usage: $0 <source.el> [iterations]" >&2
  exit 2
fi

source_file="$1"
iterations="${2:-100}"

if [[ ! -f "$source_file" ]]; then
  echo "source file not found: $source_file" >&2
  exit 2
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

run_bench() {
  local bench_source="$1"
  RUSTFLAGS="${RUSTFLAGS:--Awarnings}" cargo run \
    --manifest-path "$repo_root/neovm-core/Cargo.toml" \
    --example load_cache_bench \
    -- "$bench_source" "$iterations"
}

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

bench_source="$tmp_dir/bench-source.el"
cp "$source_file" "$bench_source"

base_output="$(run_bench "$bench_source")"
printf '%s\n' "$base_output"

printf '\n; neovm-post-edit-bench-marker\n' >> "$bench_source"
post_edit_output="$(run_bench "$bench_source")"
post_edit_rebuild_ms="$(
  printf '%s\n' "$post_edit_output" | awk -F': ' '/^cold_load_ms:/{print $2; exit}'
)"

if [[ -z "$post_edit_rebuild_ms" ]]; then
  echo "failed to parse post-edit rebuild timing" >&2
  exit 1
fi

echo "post_edit_rebuild_ms: $post_edit_rebuild_ms"
