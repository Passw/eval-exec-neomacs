#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
filter_script="$script_dir/filter-case-lines.awk"

tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

mixed_in="$tmp_dir/mixed.in"
mixed_out="$tmp_dir/mixed.out"
mixed_expected="$tmp_dir/mixed.expected"

cat >"$mixed_in" <<'EOF'
password prompt: __NEOVM_CASE__	1	(fboundp 'read-passwd)	OK t
noise line that should be ignored
__NEOVM_CASE__	2	(condition-case err (read-passwd "pw: ") (error err))	OK (end-of-file "Error reading from stdin")
EOF

cat >"$mixed_expected" <<'EOF'
1	(fboundp 'read-passwd)	OK t
2	(condition-case err (read-passwd "pw: ") (error err))	OK (end-of-file "Error reading from stdin")
EOF

LC_ALL=C awk -f "$filter_script" "$mixed_in" >"$mixed_out"
diff -u "$mixed_expected" "$mixed_out"

empty_in="$tmp_dir/empty.in"
cat >"$empty_in" <<'EOF'
only noise
without prefixed cases
EOF

if LC_ALL=C awk -f "$filter_script" "$empty_in" >/dev/null 2>&1; then
  echo "expected filter-case-lines.awk to fail for empty case output" >&2
  exit 1
fi

NEOVM_ALLOW_EMPTY_CASE_OUTPUT=1 LC_ALL=C awk -f "$filter_script" "$empty_in" >/dev/null

echo "case-line filter checks passed"
