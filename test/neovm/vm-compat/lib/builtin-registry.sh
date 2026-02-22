#!/usr/bin/env bash

_builtin_registry_lib_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_builtin_registry_default_extension_policy_file="$_builtin_registry_lib_dir/../cases/builtin-registry-extension-policy.txt"

# Collect all entries from DISPATCH_BUILTIN_NAMES into OUT_FILE.
collect_dispatch_builtin_names() {
  local registry_file="$1"
  local out_file="$2"

  awk '
    /const DISPATCH_BUILTIN_NAMES:/ && /=[[:space:]]*&\[/ { in_table=1; next }
    in_table && /^[[:space:]]*\];/ { in_table=0; exit }
    in_table {
      if (match($0, /^[[:space:]]*"([^"]+)",[[:space:]]*$/, m)) {
        print m[1]
      }
    }
  ' "$registry_file" > "$out_file"
}

# Collect core entries (excluding extension names declared by policy and
# NeoVM-prefixed extension names).
collect_core_dispatch_builtin_names() {
  local all_names_file="$1"
  local out_file="$2"
  local policy_file="${3:-$_builtin_registry_default_extension_policy_file}"
  local policy_source="/dev/null"

  if [[ -f "$policy_file" ]]; then
    policy_source="$policy_file"
  fi

  awk '
    NR == FNR {
      if (NF && $1 !~ /^#/) {
        policy[$1] = 1
      }
      next
    }
    $0 !~ /^neovm-/ && !($0 in policy) {
      print $0
    }
  ' "$policy_source" "$all_names_file" > "$out_file"
}

# Collect extension entries (policy-declared and NeoVM-prefixed names),
# sorted unique.
collect_extension_dispatch_builtin_names() {
  local all_names_file="$1"
  local out_file="$2"
  local policy_file="${3:-$_builtin_registry_default_extension_policy_file}"
  local policy_source="/dev/null"

  if [[ -f "$policy_file" ]]; then
    policy_source="$policy_file"
  fi

  awk '
    NR == FNR {
      if (NF && $1 !~ /^#/) {
        policy[$1] = 1
      }
      next
    }
    $0 ~ /^neovm-/ || ($0 in policy) {
      print $0
    }
  ' "$policy_source" "$all_names_file" | sort -u > "$out_file" || true
}
