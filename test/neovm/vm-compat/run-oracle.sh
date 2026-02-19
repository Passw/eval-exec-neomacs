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
source "$script_dir/oracle-emacs-path.sh"
oracle_el="$script_dir/oracle_eval.el"
emacs_bin="$oracle_emacs_path"

if [[ ! -x "$emacs_bin" ]]; then
  echo "oracle emacs binary is not executable: $emacs_bin" >&2
  echo "default oracle path is hardcoded to: $hardcoded_oracle_emacs" >&2
  echo "override with NEOVM_FORCE_ORACLE_PATH for temporary local runs if needed" >&2
  exit 127
fi

version_banner="$("$emacs_bin" --version 2>/dev/null | head -n 1 || true)"
if [[ "$version_banner" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]] || [[ "$emacs_bin" =~ [Nn][Ee][Oo][Mm][Aa][Cc][Ss] ]]; then
  echo "oracle emacs binary appears to be Neomacs, not GNU Emacs: $emacs_bin" >&2
  echo "set NEOVM_FORCE_ORACLE_PATH to an official GNU Emacs binary" >&2
  exit 2
fi

tmp_stdout="$(mktemp)"
tmp_stderr="$(mktemp)"
trap 'rm -f "$tmp_stdout" "$tmp_stderr"' EXIT

set +e
NEOVM_FORMS_FILE="$forms_file_abs" "$emacs_bin" --batch -Q -l "$oracle_el" >"$tmp_stdout" 2>"$tmp_stderr"
emacs_rc=$?
set -e
if [[ "$emacs_rc" -ne 0 ]]; then
  echo "oracle emacs run failed (exit=$emacs_rc): $forms_file_abs" >&2
  if [[ -s "$tmp_stderr" ]]; then
    echo "oracle stderr:" >&2
    cat "$tmp_stderr" >&2
  fi
  exit "$emacs_rc"
fi

set +e
LC_ALL=C awk -f "$script_dir/filter-case-lines.awk" "$tmp_stdout"
awk_rc=$?
set -e
if [[ "$awk_rc" -ne 0 ]]; then
  if [[ -s "$tmp_stderr" ]]; then
    echo "oracle stderr:" >&2
    cat "$tmp_stderr" >&2
  fi
  if [[ -s "$tmp_stdout" ]]; then
    echo "oracle stdout (first 20 lines):" >&2
    head -n 20 "$tmp_stdout" >&2
  fi
  exit "$awk_rc"
fi
