#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-full}"
case "$MODE" in
  rust-only|full) ;;
  *)
    echo "Usage: $0 [rust-only|full]" >&2
    exit 2
    ;;
esac

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

SSH_HOST="${SSH_HOST:-127.0.0.1}"
SSH_PORT="${SSH_PORT:-50922}"
SSH_USER="${SSH_USER:-user}"
SSH_PASSWORD="${SSH_PASSWORD:-alpine}"
REMOTE_WORKDIR="${REMOTE_WORKDIR:-neomacs-build-src}"
SKIP_SYNC="${SKIP_SYNC:-0}"
AUTO_INSTALL_DEPS="${AUTO_INSTALL_DEPS:-0}"
INSTALL_BREW="${INSTALL_BREW:-0}"

KNOWN_HOSTS_FILE="$SCRIPT_DIR/.known_hosts"
ARTIFACT_DIR="$SCRIPT_DIR/artifacts"
TIMESTAMP="$(date '+%Y%m%d-%H%M%S')"
LOCAL_LOG="$ARTIFACT_DIR/neomacs-build-${MODE}-${TIMESTAMP}.log"
REMOTE_LOG_PATH="$REMOTE_WORKDIR/neomacs-build.log"

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "$1 is required but not found in PATH." >&2
    exit 1
  fi
}

require_cmd tar
require_cmd ssh
require_cmd scp

SSH_WRAPPER=()
if [[ -n "${SSH_PASSWORD:-}" ]]; then
  require_cmd sshpass
  SSH_WRAPPER=(sshpass -p "$SSH_PASSWORD")
fi

SSH_OPTS=(
  -p "$SSH_PORT"
  -o "StrictHostKeyChecking=accept-new"
  -o "UserKnownHostsFile=${KNOWN_HOSTS_FILE}"
  -o "ConnectTimeout=20"
)
SCP_OPTS=(
  -P "$SSH_PORT"
  -o "StrictHostKeyChecking=accept-new"
  -o "UserKnownHostsFile=${KNOWN_HOSTS_FILE}"
  -o "ConnectTimeout=20"
)

REMOTE="${SSH_USER}@${SSH_HOST}"

run_ssh() {
  "${SSH_WRAPPER[@]}" ssh "${SSH_OPTS[@]}" "$REMOTE" "$@"
}

run_scp() {
  "${SSH_WRAPPER[@]}" scp "${SCP_OPTS[@]}" "$@"
}

quote_env() {
  printf '%q' "$1"
}

mkdir -p "$ARTIFACT_DIR"

if [[ "$SKIP_SYNC" != "1" ]]; then
  echo "Syncing local repo to ${REMOTE}:${REMOTE_WORKDIR} ..."
  tar -C "$REPO_ROOT" \
    --exclude='.git' \
    --exclude='neomacs-build-test/windows/storage' \
    --exclude='neomacs-build-test/macos/artifacts' \
    --exclude='neomacs-build-test/macos/.known_hosts' \
    -cf - . \
    | run_ssh "set -euo pipefail; rm -rf '$REMOTE_WORKDIR'; mkdir -p '$REMOTE_WORKDIR'; tar -xf - -C '$REMOTE_WORKDIR'"
else
  echo "SKIP_SYNC=1 set; reusing remote source directory ${REMOTE_WORKDIR}."
  run_ssh "set -euo pipefail; mkdir -p '$REMOTE_WORKDIR'"
fi

echo "Uploading guest build runner ..."
run_scp "$SCRIPT_DIR/guest-build.sh" "${REMOTE}:${REMOTE_WORKDIR}/guest-build.sh"
run_scp "$SCRIPT_DIR/install-deps.sh" "${REMOTE}:${REMOTE_WORKDIR}/install-deps.sh"

REMOTE_ENV=(
  "AUTO_INSTALL_DEPS=$(quote_env "$AUTO_INSTALL_DEPS")"
  "INSTALL_BREW=$(quote_env "$INSTALL_BREW")"
)

if [[ -n "${RUST_FEATURES:-}" ]]; then
  REMOTE_ENV+=("RUST_FEATURES=$(quote_env "$RUST_FEATURES")")
fi
if [[ -n "${RUST_NO_DEFAULT_FEATURES:-}" ]]; then
  REMOTE_ENV+=("RUST_NO_DEFAULT_FEATURES=$(quote_env "$RUST_NO_DEFAULT_FEATURES")")
fi
if [[ -n "${MAKE_JOBS:-}" ]]; then
  REMOTE_ENV+=("MAKE_JOBS=$(quote_env "$MAKE_JOBS")")
fi
if [[ -n "${NEOMACS_CONFIGURE_FLAGS:-}" ]]; then
  REMOTE_ENV+=("NEOMACS_CONFIGURE_FLAGS=$(quote_env "$NEOMACS_CONFIGURE_FLAGS")")
fi
if [[ -n "${BREW_PACKAGES:-}" ]]; then
  REMOTE_ENV+=("BREW_PACKAGES=$(quote_env "$BREW_PACKAGES")")
fi
if [[ -n "${BREW_UPDATE:-}" ]]; then
  REMOTE_ENV+=("BREW_UPDATE=$(quote_env "$BREW_UPDATE")")
fi

REMOTE_ENV_STRING="${REMOTE_ENV[*]}"

echo "Executing macOS build test (mode: $MODE) ..."
run_ssh "set -euo pipefail; chmod +x '$REMOTE_WORKDIR/guest-build.sh' '$REMOTE_WORKDIR/install-deps.sh'; $REMOTE_ENV_STRING '$REMOTE_WORKDIR/guest-build.sh' '$MODE' '$REMOTE_WORKDIR' '$REMOTE_LOG_PATH'"

echo "Fetching remote log ..."
run_scp "${REMOTE}:${REMOTE_LOG_PATH}" "$LOCAL_LOG"

echo "Build test finished. Log saved to: $LOCAL_LOG"
