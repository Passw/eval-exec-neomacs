#!/usr/bin/env bash
set -euo pipefail

# One-command wrapper:
# 1) launch/reuse Docker-OSX with high CPU allocation
# 2) wait for guest SSH login
# 3) run full Neomacs macOS build over SSH

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

CONTAINER_NAME="${CONTAINER_NAME:-neomacs-macos15-test}"
DOCKER_IMAGE="${DOCKER_IMAGE:-neomacs/docker-osx:naked-auto-local}"
MACOS_DISK_IMAGE="${MACOS_DISK_IMAGE:-/home/exec/virtual/macos15/mac_hdd_ng_sequoia.img}"

HOST_SSH_PORT="${HOST_SSH_PORT:-50922}"
MACOS_USERNAME="${MACOS_USERNAME:-user}"
MACOS_PASSWORD="${MACOS_PASSWORD:-alpine}"

DOCKER_SHORTNAME="${DOCKER_SHORTNAME:-sequoia}"
GENERATE_UNIQUE="${GENERATE_UNIQUE:-true}"
NOPICKER="${NOPICKER:-false}"
DOCKER_RAM_GB="${DOCKER_RAM_GB:-8}"
DOCKER_CORES="${DOCKER_CORES:-24}"
DOCKER_CPU="${DOCKER_CPU:-Haswell-noTSX}"
DOCKER_CPUID_FLAGS="${DOCKER_CPUID_FLAGS:-kvm=on,vendor=GenuineIntel,+invtsc,vmware-cpuid-freq=on}"
OSX_COMMANDS="${OSX_COMMANDS:-while true; do sleep 3600; done}"

RECREATE_CONTAINER="${RECREATE_CONTAINER:-0}"
CLEANUP_CONTAINER="${CLEANUP_CONTAINER:-1}"
SSH_LOGIN_TIMEOUT_SEC="${SSH_LOGIN_TIMEOUT_SEC:-900}"
SSH_POLL_INTERVAL_SEC="${SSH_POLL_INTERVAL_SEC:-5}"

MODE="${1:-full}"
case "$MODE" in
  full|rust-only) ;;
  *)
    echo "Usage: $0 [full|rust-only]" >&2
    exit 2
    ;;
esac

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "$1 is required but not found in PATH." >&2
    exit 1
  fi
}

require_cmd docker
require_cmd sshpass
require_cmd ssh
require_cmd rg

if [[ ! -f "$MACOS_DISK_IMAGE" ]]; then
  echo "macOS disk image not found: $MACOS_DISK_IMAGE" >&2
  exit 1
fi

container_exists() {
  docker ps -a --format '{{.Names}}' | rg -qx "$CONTAINER_NAME"
}

container_running() {
  docker ps --format '{{.Names}}' | rg -qx "$CONTAINER_NAME"
}

cleanup_container() {
  if [[ "$CLEANUP_CONTAINER" != "1" ]]; then
    return
  fi
  if container_exists; then
    echo "Cleaning up container: $CONTAINER_NAME"
    docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true
  fi
}

trap cleanup_container EXIT INT TERM

launch_container() {
  if container_exists; then
    if [[ "$RECREATE_CONTAINER" == "1" ]]; then
      echo "Removing existing container: $CONTAINER_NAME"
      docker rm -f "$CONTAINER_NAME" >/dev/null
    elif ! container_running; then
      echo "Starting existing container: $CONTAINER_NAME"
      docker start "$CONTAINER_NAME" >/dev/null
      return
    else
      echo "Container already running: $CONTAINER_NAME"
      return
    fi
  fi

  echo "Launching Docker-OSX container: $CONTAINER_NAME"
  docker run -d \
    --name "$CONTAINER_NAME" \
    --device /dev/kvm \
    -p "${HOST_SSH_PORT}:10022" \
    -v "${MACOS_DISK_IMAGE}:/image" \
    -e SHORTNAME="${DOCKER_SHORTNAME}" \
    -e GENERATE_UNIQUE="${GENERATE_UNIQUE}" \
    -e NOPICKER="${NOPICKER}" \
    -e TERMS_OF_USE=i_agree \
    -e USERNAME="${MACOS_USERNAME}" \
    -e PASSWORD="${MACOS_PASSWORD}" \
    -e RAM="${DOCKER_RAM_GB}" \
    -e SMP="${DOCKER_CORES}" \
    -e CORES="${DOCKER_CORES}" \
    -e CPU="${DOCKER_CPU}" \
    -e CPUID_FLAGS="${DOCKER_CPUID_FLAGS}" \
    -e AUDIO_DRIVER=none \
    -e OSX_COMMANDS="${OSX_COMMANDS}" \
    "$DOCKER_IMAGE" >/dev/null
}

verify_qemu_cpu_config() {
  local qemu_line
  qemu_line="$(docker exec "$CONTAINER_NAME" ps -ef | rg 'qemu-system-x86_64 .* -smp ' || true)"
  if [[ -z "$qemu_line" ]]; then
    echo "QEMU process not visible yet; continuing to SSH readiness wait."
    return
  fi
  echo "QEMU CPU config: $(echo "$qemu_line" | sed -E 's/.*(-smp [^ ]+).*/\1/')"
}

wait_for_ssh_login() {
  local start_ts now elapsed
  start_ts="$(date +%s)"

  while true; do
    if sshpass -p "$MACOS_PASSWORD" \
      ssh -p "$HOST_SSH_PORT" \
      -o StrictHostKeyChecking=no \
      -o UserKnownHostsFile=/dev/null \
      -o ConnectTimeout=5 \
      "$MACOS_USERNAME@127.0.0.1" \
      'echo ssh-ready' >/dev/null 2>&1; then
      echo "Guest SSH login is ready."
      return
    fi

    now="$(date +%s)"
    elapsed="$((now - start_ts))"
    if (( elapsed >= SSH_LOGIN_TIMEOUT_SEC )); then
      echo "Timed out waiting for guest SSH login (${SSH_LOGIN_TIMEOUT_SEC}s)." >&2
      echo "Tip: if the VM is in Recovery installer, SSH won't become usable until macOS install/user setup is complete." >&2
      echo "Recent container logs:" >&2
      docker logs --tail 80 "$CONTAINER_NAME" >&2 || true
      exit 1
    fi

    echo "Waiting for guest SSH login... (${elapsed}s/${SSH_LOGIN_TIMEOUT_SEC}s)"
    sleep "$SSH_POLL_INTERVAL_SEC"
  done
}

run_build() {
  local default_rust_features
  local default_configure_flags

  default_rust_features="neo-term,core-backend-emacs-c"
  default_configure_flags="--without-ns --with-file-notification=no --with-native-compilation=no --with-neomacs --with-neovm-core-backend=emacs-c"

  export SSH_HOST="127.0.0.1"
  export SSH_PORT="$HOST_SSH_PORT"
  export SSH_USER="$MACOS_USERNAME"
  export SSH_PASSWORD="$MACOS_PASSWORD"

  export AUTO_INSTALL_DEPS="${AUTO_INSTALL_DEPS:-1}"
  export INSTALL_BREW="${INSTALL_BREW:-0}"
  export MAKE_JOBS="${MAKE_JOBS:-$DOCKER_CORES}"
  export RUST_FEATURES="${RUST_FEATURES:-$default_rust_features}"
  export NEOMACS_CONFIGURE_FLAGS="${NEOMACS_CONFIGURE_FLAGS:-$default_configure_flags}"

  echo "Starting Neomacs build (mode=$MODE, MAKE_JOBS=$MAKE_JOBS, RUST_FEATURES=$RUST_FEATURES)"
  "$SCRIPT_DIR/build-over-ssh.sh" "$MODE"
}

launch_container
verify_qemu_cpu_config
wait_for_ssh_login
run_build
