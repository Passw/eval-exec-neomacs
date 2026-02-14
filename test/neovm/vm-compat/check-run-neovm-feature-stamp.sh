#!/usr/bin/env bash
set -euo pipefail

forms_file="${1:-cases/bytecode-literal-reader-semantics.forms}"
if [[ ! -f "$forms_file" ]]; then
  echo "forms file not found: $forms_file" >&2
  exit 2
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
stamp_file="$repo_root/rust/neovm-worker/target/debug/examples/elisp_compat_runner.features"

run_and_capture() {
  local features="$1"
  local out_file="$2"
  if [[ -n "$features" ]]; then
    NEOVM_WORKER_CARGO_FEATURES="$features" ./run-neovm.sh "$forms_file" >"$out_file"
  else
    NEOVM_WORKER_CARGO_FEATURES= ./run-neovm.sh "$forms_file" >"$out_file"
  fi
}

tmp_default_1="$(mktemp)"
tmp_legacy="$(mktemp)"
tmp_default_2="$(mktemp)"
trap 'rm -f "$tmp_default_1" "$tmp_legacy" "$tmp_default_2"' EXIT

run_and_capture "" "$tmp_default_1"
first_default_1="$(awk -F '\t' 'NR==1{print $3}' "$tmp_default_1")"
if [[ "$first_default_1" != "OK nil" ]]; then
  echo "expected default first result 'OK nil', got '$first_default_1'" >&2
  exit 1
fi
default_stamp_1="$(cat "$stamp_file" 2>/dev/null || true)"
if [[ -n "$default_stamp_1" ]]; then
  echo "expected empty feature stamp for default run, got '$default_stamp_1'" >&2
  exit 1
fi

run_and_capture "legacy-elc-literal" "$tmp_legacy"
first_legacy="$(awk -F '\t' 'NR==1{print $3}' "$tmp_legacy")"
if [[ "$first_legacy" != "OK t" ]]; then
  echo "expected legacy first result 'OK t', got '$first_legacy'" >&2
  exit 1
fi
legacy_stamp="$(cat "$stamp_file" 2>/dev/null || true)"
if [[ "$legacy_stamp" != "legacy-elc-literal" ]]; then
  echo "expected legacy feature stamp, got '$legacy_stamp'" >&2
  exit 1
fi

run_and_capture "" "$tmp_default_2"
first_default_2="$(awk -F '\t' 'NR==1{print $3}' "$tmp_default_2")"
if [[ "$first_default_2" != "OK nil" ]]; then
  echo "expected default first result after flip 'OK nil', got '$first_default_2'" >&2
  exit 1
fi
default_stamp_2="$(cat "$stamp_file" 2>/dev/null || true)"
if [[ -n "$default_stamp_2" ]]; then
  echo "expected empty feature stamp after flip back, got '$default_stamp_2'" >&2
  exit 1
fi

echo "run-neovm feature stamp flip checks passed"
