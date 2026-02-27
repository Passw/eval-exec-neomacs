#!/usr/bin/env bash
set -euo pipefail

SSH_HOST="${SSH_HOST:-127.0.0.1}"
SSH_PORT="${SSH_PORT:-2222}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-2400}"
SLEEP_SECONDS="${SLEEP_SECONDS:-10}"

if ! command -v ssh-keyscan >/dev/null 2>&1; then
  echo "ssh-keyscan is required but not found in PATH." >&2
  exit 1
fi

deadline=$((SECONDS + TIMEOUT_SECONDS))

while ((SECONDS < deadline)); do
  if ssh-keyscan -T 5 -p "$SSH_PORT" "$SSH_HOST" >/dev/null 2>&1; then
    echo "SSH is reachable at ${SSH_HOST}:${SSH_PORT}."
    exit 0
  fi

  remaining=$((deadline - SECONDS))
  echo "Waiting for Windows SSH (${remaining}s remaining)..."
  sleep "$SLEEP_SECONDS"
done

echo "Timed out waiting for SSH at ${SSH_HOST}:${SSH_PORT}." >&2
exit 1
