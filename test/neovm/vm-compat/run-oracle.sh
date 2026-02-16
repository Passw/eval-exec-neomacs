#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <forms-file>" >&2
  exit 2
fi

forms_file="$1"
if [[ ! -f "$forms_file" ]]; then
  echo "forms file not found: $forms_file" >&2
  exit 2
fi

forms_dir="$(cd "$(dirname "$forms_file")" && pwd)"
forms_file_abs="$forms_dir/$(basename "$forms_file")"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
oracle_el="$script_dir/oracle_eval.el"
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
  if command -v emacs >/dev/null 2>&1; then
    emacs_bin="$(command -v emacs)"
  else
    emacs_bin="$(find_nix_oracle_emacs || true)"
  fi
fi

if [[ -z "$emacs_bin" ]]; then
  echo "emacs binary not found in PATH or /nix/store (or set NEOVM_ORACLE_EMACS/ORACLE_EMACS)" >&2
  exit 127
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

NEOVM_FORMS_FILE="$forms_file_abs" "$emacs_bin" --batch -Q -l "$oracle_el" 2>/dev/null \
  | awk -f "$script_dir/filter-case-lines.awk"
