#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <allowlist-file> [load-file ...]" >&2
  exit 2
fi

allowlist_file="$1"
shift
if [[ ! -f "$allowlist_file" ]]; then
  echo "allowlist file not found: $allowlist_file" >&2
  exit 2
fi

allowlist_dir="$(cd "$(dirname "$allowlist_file")" && pwd)"
allowlist_abs="$allowlist_dir/$(basename "$allowlist_file")"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
runner_el="$script_dir/ert_allowlist_eval.el"
emacs_bin="${NEOVM_ORACLE_EMACS:-${ORACLE_EMACS:-}}"

find_nix_oracle_emacs() {
  local candidate version_banner
  local candidates=()
  local idx

  shopt -s nullglob
  candidates=(/nix/store/*-emacs-*/bin/emacs)
  shopt -u nullglob

  for ((idx=${#candidates[@]} - 1; idx>=0; idx--)); do
    candidate="${candidates[$idx]}"
    [[ -x "$candidate" ]] || continue

    version_banner="$("$candidate" --version 2>/dev/null | head -n 1 || true)"
    if [[ "$version_banner" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]] || [[ "$candidate" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]]; then
      continue
    fi

    printf '%s\n' "$candidate"
    return 0
  done

  return 1
}

if [[ -z "$emacs_bin" ]]; then
  emacs_bin="$(find_nix_oracle_emacs || true)"
fi

if [[ -z "$emacs_bin" ]]; then
  if command -v emacs >/dev/null 2>&1; then
    emacs_bin="$(command -v emacs)"
  else
    echo "emacs binary not found in PATH or /nix/store (or set NEOVM_ORACLE_EMACS/ORACLE_EMACS)" >&2
    exit 127
  fi
fi

if [[ ! -x "$emacs_bin" ]]; then
  echo "oracle emacs binary is not executable: $emacs_bin" >&2
  exit 127
fi

version_banner="$("$emacs_bin" --version 2>/dev/null | head -n 1 || true)"
if [[ "$version_banner" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]] || [[ "$emacs_bin" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]]; then
  fallback_bin="$(find_nix_oracle_emacs || true)"
  if [[ -n "$fallback_bin" ]] && [[ "$fallback_bin" != "$emacs_bin" ]]; then
    emacs_bin="$fallback_bin"
    version_banner="$("$emacs_bin" --version 2>/dev/null | head -n 1 || true)"
  fi
fi

if [[ "$version_banner" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]] || [[ "$emacs_bin" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]]; then
  echo "oracle emacs binary appears to be Neomacs, not GNU Emacs: $emacs_bin" >&2
  echo "set NEOVM_ORACLE_EMACS/ORACLE_EMACS to an official GNU Emacs binary" >&2
  exit 2
fi

load_files_abs=()
for path in "$@"; do
  if [[ ! -f "$path" ]]; then
    echo "load file not found: $path" >&2
    exit 2
  fi
  dir="$(cd "$(dirname "$path")" && pwd)"
  load_files_abs+=("$dir/$(basename "$path")")
done

load_env=""
for path in "${load_files_abs[@]}"; do
  if [[ -z "$load_env" ]]; then
    load_env="$path"
  else
    load_env="$load_env:$path"
  fi
done

NEOVM_ERT_ALLOWLIST_FILE="$allowlist_abs" \
NEOVM_ERT_LOAD_FILES="$load_env" \
"$emacs_bin" --batch -Q -l "$runner_el" 2>/dev/null
