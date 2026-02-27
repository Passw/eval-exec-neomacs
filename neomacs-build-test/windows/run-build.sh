#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-rust-only}"
case "$MODE" in
  rust-only|full) ;;
  -h|--help)
    cat <<'USAGE'
Usage: ./run-build.sh [rust-only|full]

Environment overrides:
  SKIP_UP=1                  Skip `docker compose up -d`
  TIMEOUT_SECONDS=2400       SSH wait timeout
  SLEEP_SECONDS=10           SSH poll interval
  SSH_HOST=127.0.0.1
  SSH_PORT=2222
  SSH_USER=Docker
  SSH_PASSWORD=admin
  SOURCE_PATH=\\host.lan\Data
  WINDOWS_VERSION=11         Passed to docker compose
USAGE
    exit 0
    ;;
  *)
    echo "Usage: $0 [rust-only|full]" >&2
    exit 1
    ;;
esac

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.yml"

SKIP_UP="${SKIP_UP:-0}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-2400}"
SLEEP_SECONDS="${SLEEP_SECONDS:-10}"

SSH_HOST="${SSH_HOST:-127.0.0.1}"
SSH_PORT="${SSH_PORT:-2222}"
SSH_USER="${SSH_USER:-${WINDOWS_USERNAME:-Docker}}"
SSH_PASSWORD="${SSH_PASSWORD:-${WINDOWS_PASSWORD:-admin}}"
SOURCE_PATH="${SOURCE_PATH:-\\\\host.lan\\Data}"
KNOWN_HOSTS_FILE="$SCRIPT_DIR/.known_hosts"

LOCAL_PS_PATH="$SCRIPT_DIR/remote-build.ps1"
REMOTE_PS_PATH="/C:/Users/${SSH_USER}/neomacs-build.ps1"

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "$1 is required but not found in PATH." >&2
    exit 1
  fi
}

wait_for_ssh() {
  local deadline remaining
  deadline=$((SECONDS + TIMEOUT_SECONDS))

  while ((SECONDS < deadline)); do
    if ssh-keyscan -T 5 -p "$SSH_PORT" "$SSH_HOST" >/dev/null 2>&1; then
      echo "SSH is reachable at ${SSH_HOST}:${SSH_PORT}."
      return 0
    fi

    remaining=$((deadline - SECONDS))
    echo "Waiting for Windows SSH (${remaining}s remaining)..."
    sleep "$SLEEP_SECONDS"
  done

  echo "Timed out waiting for SSH at ${SSH_HOST}:${SSH_PORT}." >&2
  return 1
}

require_cmd docker
require_cmd ssh
require_cmd scp
require_cmd ssh-keyscan
require_cmd setsid

if [[ ! -f "$LOCAL_PS_PATH" ]]; then
  echo "Missing $LOCAL_PS_PATH" >&2
  exit 1
fi

if [[ "$SKIP_UP" != "1" ]]; then
  echo "Starting Dockur Windows VM..."
  docker compose -f "$COMPOSE_FILE" up -d
fi

wait_for_ssh

ASKPASS_SCRIPT="$(mktemp "${SCRIPT_DIR}/.askpass.XXXXXX")"
cleanup() {
  rm -f "$ASKPASS_SCRIPT"
}
trap cleanup EXIT

cat >"$ASKPASS_SCRIPT" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "${SSH_PASSWORD:?SSH_PASSWORD is required}"
EOF
chmod 700 "$ASKPASS_SCRIPT"

export SSH_PASSWORD
export SSH_ASKPASS="$ASKPASS_SCRIPT"
export SSH_ASKPASS_REQUIRE=force
export DISPLAY="${DISPLAY:-:0}"

SSH_COMMON_OPTS=(
  -o "StrictHostKeyChecking=accept-new"
  -o "UserKnownHostsFile=${KNOWN_HOSTS_FILE}"
  -o "PreferredAuthentications=password"
  -o "PubkeyAuthentication=no"
  -o "NumberOfPasswordPrompts=1"
  -o "ConnectTimeout=20"
)

SSH_OPTS=(
  -p "$SSH_PORT"
  "${SSH_COMMON_OPTS[@]}"
)

SCP_OPTS=(
  -P "$SSH_PORT"
  "${SSH_COMMON_OPTS[@]}"
)

echo "Copying build runner to Windows guest..."
setsid -w scp "${SCP_OPTS[@]}" "$LOCAL_PS_PATH" "${SSH_USER}@${SSH_HOST}:${REMOTE_PS_PATH}" < /dev/null

echo "Running Windows build test in mode: $MODE"
setsid -w ssh "${SSH_OPTS[@]}" "${SSH_USER}@${SSH_HOST}" \
  "powershell -NoLogo -NoProfile -ExecutionPolicy Bypass -File C:\\Users\\${SSH_USER}\\neomacs-build.ps1 -SourcePath \"${SOURCE_PATH}\" -Mode \"${MODE}\"" \
  < /dev/null

echo "Remote build finished. Log file in guest: C:\\Users\\${SSH_USER}\\neomacs-build.log"
