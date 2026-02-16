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
hardcoded_oracle_emacs="/nix/store/hql3zwz5b4ywd2qwx8jssp4dyb7nx4cb-emacs-30.2/bin/emacs"
emacs_bin="${NEOVM_ORACLE_EMACS:-${ORACLE_EMACS:-$hardcoded_oracle_emacs}}"

if [[ ! -x "$emacs_bin" ]]; then
  echo "oracle emacs binary is not executable: $emacs_bin" >&2
  echo "default oracle path is hardcoded to: $hardcoded_oracle_emacs" >&2
  echo "override with NEOVM_ORACLE_EMACS/ORACLE_EMACS if needed" >&2
  exit 127
fi

version_banner="$("$emacs_bin" --version 2>/dev/null | head -n 1 || true)"
if [[ "$version_banner" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]] || [[ "$emacs_bin" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]]; then
  echo "oracle emacs binary appears to be Neomacs, not GNU Emacs: $emacs_bin" >&2
  echo "set NEOVM_ORACLE_EMACS/ORACLE_EMACS to an official GNU Emacs binary" >&2
  exit 2
fi

NEOVM_FORMS_FILE="$forms_file_abs" "$emacs_bin" --batch -Q -l "$oracle_el" 2>/dev/null \
  | LC_ALL=C awk -f "$script_dir/filter-case-lines.awk"
