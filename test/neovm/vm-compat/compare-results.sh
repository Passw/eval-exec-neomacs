#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <oracle-tsv> <neovm-tsv>" >&2
  exit 2
fi

oracle_tsv="$1"
neovm_tsv="$2"

if [[ ! -f "$oracle_tsv" ]]; then
  echo "oracle file not found: $oracle_tsv" >&2
  exit 2
fi
if [[ ! -f "$neovm_tsv" ]]; then
  echo "neovm file not found: $neovm_tsv" >&2
  exit 2
fi

awk -F '\t' '
BEGIN {
  failed = 0;
  oracle_count = 0;
  neovm_count = 0;
}

FNR == NR {
  idx = $1;
  oracle_form[idx] = $2;
  oracle_result[idx] = $3;
  oracle_seen[idx] = 1;
  oracle_count++;
  next;
}

{
  idx = $1;
  form = $2;
  result = $3;

  neovm_seen[idx] = 1;
  neovm_count++;

  if (!(idx in oracle_seen)) {
    printf("extra case in neovm output: index=%s\n", idx) > "/dev/stderr";
    failed = 1;
    next;
  }

  if (oracle_form[idx] != form) {
    printf("form mismatch at index=%s\n", idx) > "/dev/stderr";
    printf("  oracle: %s\n", oracle_form[idx]) > "/dev/stderr";
    printf("  neovm:  %s\n", form) > "/dev/stderr";
    failed = 1;
  }

  if (oracle_result[idx] != result) {
    printf("result mismatch at index=%s\n", idx) > "/dev/stderr";
    printf("  oracle: %s\n", oracle_result[idx]) > "/dev/stderr";
    printf("  neovm:  %s\n", result) > "/dev/stderr";
    failed = 1;
  }
}

END {
  for (idx in oracle_seen) {
    if (!(idx in neovm_seen)) {
      printf("missing case in neovm output: index=%s\n", idx) > "/dev/stderr";
      failed = 1;
    }
  }

  if (failed) {
    exit 1;
  }

  printf("compat outputs match: %d/%d cases\n", neovm_count, oracle_count);
}
' "$oracle_tsv" "$neovm_tsv"
