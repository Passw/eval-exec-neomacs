#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
registry_file="$repo_root/rust/neovm-core/src/elisp/builtin_registry.rs"
source "$script_dir/oracle-emacs-path.sh"
source "$script_dir/lib/builtin-registry.sh"

if [[ ! -f "$registry_file" ]]; then
  echo "builtin registry not found: $registry_file" >&2
  exit 2
fi

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

tmp_names="$(mktemp)"
tmp_core_names="$(mktemp)"
tmp_oracle_builtins="$(mktemp)"
tmp_covered="$(mktemp)"
tmp_missing="$(mktemp)"
tmp_extra="$(mktemp)"

cleanup() {
  rm -f \
    "$tmp_names" \
    "$tmp_core_names" \
    "$tmp_oracle_builtins" \
    "$tmp_covered" \
    "$tmp_missing" \
    "$tmp_extra"
}
trap cleanup EXIT

collect_dispatch_builtin_names "$registry_file" "$tmp_names"
if [[ ! -s "$tmp_names" ]]; then
  echo "failed to parse builtin names from registry" >&2
  exit 1
fi

# NeoVM-prefixed symbols are extension builtins and intentionally absent in
# GNU Emacs; exclude them from coverage accounting.
collect_core_dispatch_builtin_names "$tmp_names" "$tmp_core_names"
sort -u "$tmp_core_names" -o "$tmp_core_names"
if [[ ! -s "$tmp_core_names" ]]; then
  echo "failed to derive core builtin names from registry" >&2
  exit 1
fi

"$emacs_bin" --batch -Q --eval '
(let (names)
  (mapatoms
   (lambda (sym)
     (when (fboundp sym)
       (let ((fn (symbol-function sym)))
         (when (or (subrp fn) (special-form-p sym))
           (push (symbol-name sym) names))))))
  (dolist (name (sort names (lambda (a b) (string< a b))))
    (princ name)
    (terpri)))
' > "$tmp_oracle_builtins"
sort -u "$tmp_oracle_builtins" -o "$tmp_oracle_builtins"

comm -12 "$tmp_oracle_builtins" "$tmp_core_names" > "$tmp_covered"
comm -23 "$tmp_oracle_builtins" "$tmp_core_names" > "$tmp_missing"
comm -13 "$tmp_oracle_builtins" "$tmp_core_names" > "$tmp_extra"

oracle_total="$(wc -l < "$tmp_oracle_builtins" | tr -d ' ')"
core_total="$(wc -l < "$tmp_core_names" | tr -d ' ')"
covered_total="$(wc -l < "$tmp_covered" | tr -d ' ')"
missing_total="$(wc -l < "$tmp_missing" | tr -d ' ')"
extra_total="$(wc -l < "$tmp_extra" | tr -d ' ')"

preview="${ORACLE_BUILTIN_COVERAGE_PREVIEW:-25}"
if [[ ! "$preview" =~ ^[0-9]+$ ]]; then
  preview=25
fi

echo "oracle builtin universe entries: $oracle_total"
echo "registry core entries: $core_total"
echo "oracle builtin names covered by registry: $covered_total"
echo "oracle builtin names missing from registry: $missing_total"
echo "registry names outside oracle builtin universe: $extra_total"

if [[ "$preview" -gt 0 ]] && [[ "$missing_total" -gt 0 ]]; then
  echo
  echo "missing oracle builtin names (first $preview):"
  head -n "$preview" "$tmp_missing"
fi

if [[ "$preview" -gt 0 ]] && [[ "$extra_total" -gt 0 ]]; then
  echo
  echo "registry names outside oracle builtin universe (first $preview):"
  head -n "$preview" "$tmp_extra"
fi
