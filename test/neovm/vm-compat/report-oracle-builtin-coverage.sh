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

universe_mode="${ORACLE_BUILTIN_UNIVERSE:-primitive-subr}"
case "$universe_mode" in
  primitive-subr|primitive-any|subr-or-special) ;;
  *)
    echo "unsupported ORACLE_BUILTIN_UNIVERSE mode: $universe_mode" >&2
    echo "supported modes: primitive-subr, primitive-any, subr-or-special" >&2
    exit 2
    ;;
esac

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
tmp_forms="$(mktemp)"
tmp_neovm="$(mktemp)"
tmp_runtime_covered="$(mktemp)"
tmp_runtime_missing="$(mktemp)"
tmp_runtime_outside_registry="$(mktemp)"

cleanup() {
  rm -f \
    "$tmp_names" \
    "$tmp_core_names" \
    "$tmp_oracle_builtins" \
    "$tmp_covered" \
    "$tmp_missing" \
    "$tmp_extra" \
    "$tmp_forms" \
    "$tmp_neovm" \
    "$tmp_runtime_covered" \
    "$tmp_runtime_missing" \
    "$tmp_runtime_outside_registry"
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

NEOVM_ORACLE_BUILTIN_UNIVERSE="$universe_mode" "$emacs_bin" --batch -Q --eval '
(let ((mode (or (getenv "NEOVM_ORACLE_BUILTIN_UNIVERSE") "primitive-subr"))
      names)
  (mapatoms
   (lambda (sym)
     (when (fboundp sym)
       (let ((fn (symbol-function sym))
             (special (special-form-p sym)))
         (when
             (cond
              ((string= mode "primitive-subr")
               (and (subr-primitive-p fn) (not special)))
              ((string= mode "primitive-any")
               (subr-primitive-p fn))
              ((string= mode "subr-or-special")
               (or (subrp fn) special))
              (t
               (error "Unknown builtin universe mode: %s" mode)))
           (push (symbol-name sym) names))))))
  (dolist (name (sort names (lambda (a b) (string< a b))))
    (princ name)
    (terpri)))
' > "$tmp_oracle_builtins"
sort -u "$tmp_oracle_builtins" -o "$tmp_oracle_builtins"

comm -12 "$tmp_oracle_builtins" "$tmp_core_names" > "$tmp_covered"
comm -23 "$tmp_oracle_builtins" "$tmp_core_names" > "$tmp_missing"
comm -13 "$tmp_oracle_builtins" "$tmp_core_names" > "$tmp_extra"

awk '
  {
    gsub(/\\/, "\\\\", $0)
    gsub(/"/, "\\\"", $0)
    printf "(fboundp (intern \"%s\"))\n", $0
  }
' "$tmp_oracle_builtins" > "$tmp_forms"

"$script_dir/run-neovm.sh" "$tmp_forms" > "$tmp_neovm"

awk -F '\t' -v covered="$tmp_runtime_covered" -v missing="$tmp_runtime_missing" '
  NR==FNR {
    names[FNR] = $0
    next
  }
  {
    if (!(FNR in names)) {
      next
    }
    if ($3 == "OK t") {
      print names[FNR] > covered
    } else {
      print names[FNR] > missing
    }
  }
' "$tmp_oracle_builtins" "$tmp_neovm"

sort -u "$tmp_runtime_covered" -o "$tmp_runtime_covered"
sort -u "$tmp_runtime_missing" -o "$tmp_runtime_missing"
comm -23 "$tmp_runtime_covered" "$tmp_core_names" > "$tmp_runtime_outside_registry"

oracle_total="$(wc -l < "$tmp_oracle_builtins" | tr -d ' ')"
core_total="$(wc -l < "$tmp_core_names" | tr -d ' ')"
covered_total="$(wc -l < "$tmp_covered" | tr -d ' ')"
missing_total="$(wc -l < "$tmp_missing" | tr -d ' ')"
extra_total="$(wc -l < "$tmp_extra" | tr -d ' ')"
runtime_covered_total="$(wc -l < "$tmp_runtime_covered" | tr -d ' ')"
runtime_missing_total="$(wc -l < "$tmp_runtime_missing" | tr -d ' ')"
runtime_outside_registry_total="$(wc -l < "$tmp_runtime_outside_registry" | tr -d ' ')"

preview="${ORACLE_BUILTIN_COVERAGE_PREVIEW:-25}"
if [[ ! "$preview" =~ ^[0-9]+$ ]]; then
  preview=25
fi

if [[ $((runtime_covered_total + runtime_missing_total)) -ne "$oracle_total" ]]; then
  echo "failed to compute runtime coverage across oracle builtin universe" >&2
  exit 1
fi

echo "oracle builtin universe mode: $universe_mode"
echo "oracle builtin universe entries: $oracle_total"
echo "registry core entries: $core_total"
echo "oracle builtin names covered by registry: $covered_total"
echo "oracle builtin names missing from registry: $missing_total"
echo "registry names outside oracle builtin universe: $extra_total"
echo "neovm runtime covered oracle builtin names: $runtime_covered_total"
echo "oracle builtin names missing in neovm runtime: $runtime_missing_total"
echo "neovm runtime covered oracle builtins outside registry: $runtime_outside_registry_total"

if [[ "$preview" -gt 0 ]] && [[ "$missing_total" -gt 0 ]]; then
  echo
  echo "missing oracle builtin names from registry (first $preview):"
  head -n "$preview" "$tmp_missing"
fi

if [[ "$preview" -gt 0 ]] && [[ "$extra_total" -gt 0 ]]; then
  echo
  echo "registry names outside oracle builtin universe (first $preview):"
  head -n "$preview" "$tmp_extra"
fi

if [[ "$preview" -gt 0 ]] && [[ "$runtime_missing_total" -gt 0 ]]; then
  echo
  echo "missing oracle builtin names in neovm runtime (first $preview):"
  head -n "$preview" "$tmp_runtime_missing"
fi

if [[ "$preview" -gt 0 ]] && [[ "$runtime_outside_registry_total" -gt 0 ]]; then
  echo
  echo "neovm runtime covered oracle builtins outside registry (first $preview):"
  head -n "$preview" "$tmp_runtime_outside_registry"
fi
