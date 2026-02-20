#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../../.." && pwd)"
registry_file="$repo_root/rust/neovm-core/src/elisp/builtin_registry.rs"
allowlist_file="$script_dir/cases/builtin-registry-fboundp-allowlist.txt"
function_kind_allowlist_file="$script_dir/cases/builtin-registry-function-kind-allowlist.txt"
function_kind_check_script="$script_dir/check-builtin-registry-function-kind.sh"
fboundp_check_script="$script_dir/check-builtin-registry-fboundp.sh"
startup_stub_coverage_script="$script_dir/check-startup-doc-stub-coverage.sh"
startup_string_coverage_script="$script_dir/check-startup-doc-string-coverage.sh"
startup_variable_doc_count_script="$script_dir/check-startup-variable-documentation-counts.sh"
oracle_builtin_coverage_script="$script_dir/report-oracle-builtin-coverage.sh"
tracked_lists_file="$script_dir/cases/tracked-lists.txt"
source "$script_dir/lib/builtin-registry.sh"
compat_stub_index_script="$script_dir/compat-stub-index.sh"

if [[ ! -f "$registry_file" ]]; then
  echo "missing registry file: $registry_file" >&2
  exit 2
fi

if [[ ! -f "$allowlist_file" ]]; then
  echo "missing allowlist file: $allowlist_file" >&2
  exit 2
fi

if [[ ! -f "$function_kind_allowlist_file" ]]; then
  echo "missing function-kind allowlist file: $function_kind_allowlist_file" >&2
  exit 2
fi

tmp_all="$(mktemp)"
tmp_core="$(mktemp)"
tmp_tracker="$(mktemp)"
tmp_tracker_forms="$(mktemp)"
tmp_tracker_expected="$(mktemp)"
tmp_forms_basenames="$(mktemp)"
tmp_expected_basenames="$(mktemp)"
tmp_expected_only="$(mktemp)"
tmp_forms_only="$(mktemp)"
tmp_function_kind_check="$(mktemp)"
tmp_fboundp_check="$(mktemp)"
tmp_stub_budget="$(mktemp)"
tmp_startup_stub_check="$(mktemp)"
tmp_startup_string_check="$(mktemp)"
tmp_startup_variable_doc_count_check="$(mktemp)"
tmp_oracle_builtin_coverage="$(mktemp)"
cleanup() {
  rm -f \
    "$tmp_all" \
    "$tmp_core" \
    "$tmp_tracker" \
    "$tmp_tracker_forms" \
    "$tmp_tracker_expected" \
    "$tmp_forms_basenames" \
    "$tmp_expected_basenames" \
    "$tmp_expected_only" \
    "$tmp_forms_only" \
    "$tmp_function_kind_check" \
    "$tmp_fboundp_check" \
    "$tmp_stub_budget" \
    "$tmp_startup_stub_check" \
    "$tmp_startup_string_check" \
    "$tmp_startup_variable_doc_count_check" \
    "$tmp_oracle_builtin_coverage"
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

list_label() {
  local list_file="$1"
  local label
  label="$(basename "$list_file" .list)"
  if [[ "$label" == "legacy-elc-literal" ]]; then
    echo "legacy-elc"
    return
  fi
  echo "$label"
}

read_list_files() {
  local default_list_files=(
    "$script_dir/cases/default.list"
    "$script_dir/cases/neovm-only.list"
    "$script_dir/cases/legacy-elc-literal.list"
    "$script_dir/cases/introspection.list"
    "$script_dir/cases/thread.list"
    "$script_dir/cases/startup-doc.list"
  )
  local collected=()
  if [[ -f "$tracked_lists_file" ]]; then
    while IFS= read -r rel_path; do
      rel_path="${rel_path%$'\r'}"
      [[ -n "$rel_path" ]] || continue
      [[ "$rel_path" =~ ^[[:space:]]*# ]] && continue
      if [[ "$rel_path" = /* ]]; then
        collected+=("$rel_path")
      else
        collected+=("$script_dir/$rel_path")
      fi
    done < "$tracked_lists_file"
  fi
  if [[ ${#collected[@]} -eq 0 ]]; then
    collected=("${default_list_files[@]}")
  fi
  printf '%s\n' "${collected[@]}"
}

mapfile -t list_files < <(read_list_files)

collect_dispatch_builtin_names "$registry_file" "$tmp_all"
collect_core_dispatch_builtin_names "$tmp_all" "$tmp_core"
{
  for list_file in "${list_files[@]}"; do
    awk 'NF && $1 !~ /^#/ { print $1 }' "$list_file"
  done
} > "$tmp_tracker"

while IFS= read -r case_path; do
  case_path="${case_path%$'\r'}"
  [[ -n "$case_path" ]] || continue
  printf '%s\n' "$script_dir/$case_path.forms"
done < "$tmp_tracker" > "$tmp_tracker_forms"
sort -u "$tmp_tracker_forms" -o "$tmp_tracker_forms"
while IFS= read -r case_path; do
  case_path="${case_path%$'\r'}"
  [[ -n "$case_path" ]] || continue
  printf '%s\n' "$script_dir/$case_path.expected.tsv"
done < "$tmp_tracker" > "$tmp_tracker_expected"
sort -u "$tmp_tracker_expected" -o "$tmp_tracker_expected"

all_builtins="$(wc -l < "$tmp_all" | tr -d ' ')"
core_builtins="$(wc -l < "$tmp_core" | tr -d ' ')"
extension_builtins="$((all_builtins - core_builtins))"
allowlisted="$(awk 'NF && $1 !~ /^#/ { count++ } END { print count+0 }' "$allowlist_file")"
function_kind_allowlisted="$(awk 'NF && $1 !~ /^#/ { count++ } END { print count+0 }' "$function_kind_allowlist_file")"
tracked_unique="$(sort -u "$tmp_tracker" | awk 'END { print NR+0 }')"

SHOW_ALLOWLISTED_DRIFTS=1 "$function_kind_check_script" > "$tmp_function_kind_check"
SHOW_ALLOWLISTED_DRIFTS=1 "$fboundp_check_script" > "$tmp_fboundp_check" || true
function_kind_drift="$(awk '/oracle\/neovm function-kind drifts:/ { print $4 }' "$tmp_function_kind_check" | head -n 1)"
fboundp_drift="$(awk '/oracle\/neovm fboundp drifts:/ { print $4 }' "$tmp_fboundp_check" | head -n 1)"
function_kind_stale="$(awk '/stale allowlist entries with no current drift:/ { stale=1; next } stale==1 && /^[^ ]/ { print $0 }' "$tmp_function_kind_check" | wc -l | tr -d ' ')"
fboundp_stale="$(awk '/stale allowlist entries with no current drift:/ { stale=1; next } stale==1 && /^[^ ]/ { print $0 }' "$tmp_fboundp_check" | wc -l | tr -d ' ')"

printf 'compat progress snapshot\n'
printf 'case lists (entries):\n'
for list_file in "${list_files[@]}"; do
  printf '  %s: %s\n' "$(list_label "$list_file")" "$(count_lines "$list_file")"
done
printf '  total unique tracked: %s\n' "$tracked_unique"
forms_count=0
while IFS= read -r path; do
  [[ -f "$path" ]] && forms_count=$((forms_count + 1))
done < "$tmp_tracker_forms"

expected_count=0
while IFS= read -r path; do
  [[ -f "$path" ]] && expected_count=$((expected_count + 1))
done < "$tmp_tracker_expected"
printf '  tracked .forms artifacts: %s\n' "$forms_count"
printf '  tracked expected artifacts: %s\n' "$expected_count"
stub_count="$("$compat_stub_index_script" 2>/dev/null | awk '/^explicitly annotated function stubs:/ { print $5 }')"
printf '  explicit function stubs: %s\n' "${stub_count:-0}"
if ! "$script_dir/check-stub-budget.sh" > "$tmp_stub_budget" 2>&1; then
  echo "  explicit stub budget: budget check failed"
  sed 's/^/    /' "$tmp_stub_budget"
  exit 1
fi
printf '  explicit stub budget: %s\n' "$(sed -n '1p' "$tmp_stub_budget")"
if ! "$startup_stub_coverage_script" > "$tmp_startup_stub_check" 2>&1; then
  echo "  startup integer-doc coverage: check failed"
  sed 's/^/    /' "$tmp_startup_stub_check"
  exit 1
fi
if ! "$startup_string_coverage_script" > "$tmp_startup_string_check" 2>&1; then
  echo "  startup string-doc coverage: check failed"
  sed 's/^/    /' "$tmp_startup_string_check"
  exit 1
fi
if ! "$startup_variable_doc_count_script" > "$tmp_startup_variable_doc_count_check" 2>&1; then
  echo "  startup variable-doc count parity: check failed"
  sed 's/^/    /' "$tmp_startup_variable_doc_count_check"
  exit 1
fi
if ! "$oracle_builtin_coverage_script" > "$tmp_oracle_builtin_coverage" 2>&1; then
  echo "  oracle builtin coverage: check failed"
  sed 's/^/    /' "$tmp_oracle_builtin_coverage"
  exit 1
fi
stub_oracle_count="$(awk -F': ' '/oracle integer-doc symbols:/ { print $2 }' "$tmp_startup_stub_check" | head -n 1)"
stub_startup_count="$(awk -F': ' '/startup stub symbols:/ { print $2 }' "$tmp_startup_stub_check" | head -n 1)"
stub_missing_count="$(awk -F': ' '/missing startup stubs:/ { print $2 }' "$tmp_startup_stub_check" | head -n 1)"
stub_extra_count="$(awk -F': ' '/extra startup stubs:/ { print $2 }' "$tmp_startup_stub_check" | head -n 1)"
string_oracle_count="$(awk -F': ' '/oracle string-doc symbols:/ { print $2 }' "$tmp_startup_string_check" | head -n 1)"
string_startup_count="$(awk -F': ' '/startup string-doc symbols:/ { print $2 }' "$tmp_startup_string_check" | head -n 1)"
string_missing_count="$(awk -F': ' '/missing startup string-docs:/ { print $2 }' "$tmp_startup_string_check" | head -n 1)"
string_extra_count="$(awk -F': ' '/extra startup string-docs:/ { print $2 }' "$tmp_startup_string_check" | head -n 1)"
startup_doc_property_summary="$(awk -F': ' '/startup variable-documentation property-count summary:/ { print $2 }' "$tmp_startup_variable_doc_count_check" | head -n 1)"
startup_doc_runtime_resolution_summary="$(awk -F': ' '/startup variable-documentation runtime-resolution summary:/ { print $2 }' "$tmp_startup_variable_doc_count_check" | head -n 1)"
oracle_builtin_total="$(awk -F': ' '/^oracle builtin universe entries:/ { print $2 }' "$tmp_oracle_builtin_coverage" | head -n 1)"
oracle_builtin_covered="$(awk -F': ' '/^oracle builtin names covered by registry:/ { print $2 }' "$tmp_oracle_builtin_coverage" | head -n 1)"
oracle_builtin_missing="$(awk -F': ' '/^oracle builtin names missing from registry:/ { print $2 }' "$tmp_oracle_builtin_coverage" | head -n 1)"
oracle_builtin_registry_extra="$(awk -F': ' '/^registry names outside oracle builtin universe:/ { print $2 }' "$tmp_oracle_builtin_coverage" | head -n 1)"
oracle_builtin_coverage_percent="$(awk -v covered="${oracle_builtin_covered:-0}" -v total="${oracle_builtin_total:-0}" 'BEGIN { if (total == 0) { printf "0.0" } else { printf "%.1f", (covered * 100.0) / total } }')"
printf '  startup integer docs (oracle/startup/missing/extra): %s/%s/%s/%s\n' \
  "${stub_oracle_count:-0}" "${stub_startup_count:-0}" "${stub_missing_count:-0}" "${stub_extra_count:-0}"
printf '  startup string docs (oracle/startup/missing/extra): %s/%s/%s/%s\n' \
  "${string_oracle_count:-0}" "${string_startup_count:-0}" "${string_missing_count:-0}" "${string_extra_count:-0}"
printf '  startup variable-doc property-counts (expected|oracle|neovm integer/string): %s\n' \
  "${startup_doc_property_summary:-0/0|0/0|0/0}"
printf '  startup variable-doc runtime-resolution counts (expected|oracle|neovm integer/string): %s\n' \
  "${startup_doc_runtime_resolution_summary:-0/0|0/0|0/0}"
if [[ "$expected_count" -ne "$forms_count" ]]; then
  printf '  corpus artifact delta (expected - forms): %+d\n' "$((expected_count - forms_count))"
  while IFS= read -r path; do
    printf '%s\n' "$(basename "$path" .forms)"
  done < <(while IFS= read -r p; do [[ -f "$p" ]] && echo "$p"; done < "$tmp_tracker_forms") \
    | sort -u > "$tmp_forms_basenames"
  while IFS= read -r path; do
    printf '%s\n' "$(basename "$path" .expected.tsv)"
  done < <(while IFS= read -r p; do [[ -f "$p" ]] && echo "$p"; done < "$tmp_tracker_expected") \
    | sort -u > "$tmp_expected_basenames"
  comm -23 "$tmp_expected_basenames" "$tmp_forms_basenames" > "$tmp_expected_only"
  comm -13 "$tmp_expected_basenames" "$tmp_forms_basenames" > "$tmp_forms_only"
  if [[ -s "$tmp_expected_only" ]]; then
    echo "  expected-only artifacts:"
    sed 's/^/    /' "$tmp_expected_only"
  fi
  if [[ -s "$tmp_forms_only" ]]; then
    echo "  forms-only artifacts:"
    sed 's/^/    /' "$tmp_forms_only"
  fi
fi

printf 'builtin registry:\n'
printf '  total dispatch entries: %s\n' "$all_builtins"
printf '  core-compat entries: %s\n' "$core_builtins"
printf '  neovm extension entries: %s\n' "$extension_builtins"
printf '  oracle builtin universe entries: %s\n' "${oracle_builtin_total:-0}"
printf '  oracle builtin coverage (covered/missing): %s/%s (%s%%)\n' \
  "${oracle_builtin_covered:-0}" "${oracle_builtin_missing:-0}" "$oracle_builtin_coverage_percent"
printf '  registry names outside oracle builtin universe: %s\n' "${oracle_builtin_registry_extra:-0}"
printf '  allowed fboundp drifts: %s\n' "$allowlisted"
printf '  fboundp current drifts: %s\n' "${fboundp_drift:-0}"
printf '  fboundp stale allowlist entries: %s\n' "${fboundp_stale:-0}"
printf '  function-kind allowlisted drifts: %s\n' "$function_kind_allowlisted"
printf '  function-kind current drifts: %s\n' "${function_kind_drift:-0}"
printf '  function-kind stale allowlist entries: %s\n' "${function_kind_stale:-0}"

echo "done"
