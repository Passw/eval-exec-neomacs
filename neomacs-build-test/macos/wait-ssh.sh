#!/usr/bin/env bash
set -euo pipefail

SSH_HOST="${SSH_HOST:-127.0.0.1}"
SSH_PORT="${SSH_PORT:-50922}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-3600}"
SLEEP_SECONDS="${SLEEP_SECONDS:-10}"

if ! command -v nc >/dev/null 2>&1; then
  echo "nc is required but not found in PATH." >&2
  exit 1
fi

deadline=$((SECONDS + TIMEOUT_SECONDS))
while ((SECONDS < deadline)); do
  if nc -z "$SSH_HOST" "$SSH_PORT" >/dev/null 2>&1; then
    echo "SSH port is reachable at ${SSH_HOST}:${SSH_PORT}."
    exit 0
  fi

  remaining=$((deadline - SECONDS))
  echo "Waiting for SSH port ${SSH_HOST}:${SSH_PORT} (${remaining}s remaining)..."
  sleep "$SLEEP_SECONDS"
done

echo "Timed out waiting for SSH port ${SSH_HOST}:${SSH_PORT}." >&2
exit 1
