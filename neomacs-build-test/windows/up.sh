#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

if ! command -v docker >/dev/null 2>&1; then
  echo "docker is required but not found in PATH." >&2
  exit 1
fi

if [ ! -e /dev/kvm ]; then
  echo "warning: /dev/kvm is missing. The VM may be very slow or fail to start." >&2
fi

if [ ! -e /dev/net/tun ]; then
  echo "warning: /dev/net/tun is missing. Networking may fail." >&2
fi

mkdir -p storage oem
docker compose -f docker-compose.yml up -d

echo "Windows VM started (or starting)."
echo "Next step: ./wait-ssh.sh"
