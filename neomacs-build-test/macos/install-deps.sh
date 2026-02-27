#!/usr/bin/env bash
set -euo pipefail

# Install missing macOS build dependencies via Homebrew.
#
# Defaults:
# - Only installs missing formulae.
# - Does not install Homebrew unless INSTALL_BREW=1.
# - Uses an internal default package list unless BREW_PACKAGES is provided.

INSTALL_BREW="${INSTALL_BREW:-0}"
BREW_UPDATE="${BREW_UPDATE:-0}"

DEFAULT_PACKAGES=(
  autoconf
  automake
  texinfo
  pkgconf
  glib
  cairo
  gstreamer
  gst-plugins-base
  gst-plugins-good
  jpeg-turbo
  libtiff
  giflib
  libpng
  librsvg
  webp
  gnutls
  libxml2
  sqlite
  jansson
  tree-sitter
  gmp
  mesa
)

log() {
  printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"
}

ensure_brew_env() {
  export PATH="/opt/homebrew/bin:/usr/local/bin:$PATH"
  if [[ -x /opt/homebrew/bin/brew ]]; then
    # shellcheck disable=SC2046
    eval "$(/opt/homebrew/bin/brew shellenv 2>/dev/null || true)"
  elif [[ -x /usr/local/bin/brew ]]; then
    # shellcheck disable=SC2046
    eval "$(/usr/local/bin/brew shellenv 2>/dev/null || true)"
  elif command -v brew >/dev/null 2>&1; then
    # shellcheck disable=SC2046
    eval "$(brew shellenv 2>/dev/null || true)"
  fi
}

install_brew_if_missing() {
  ensure_brew_env
  if command -v brew >/dev/null 2>&1; then
    return 0
  fi

  if [[ "$INSTALL_BREW" != "1" ]]; then
    cat >&2 <<'EOF'
Homebrew is not installed.
Install it manually, or rerun with INSTALL_BREW=1 to install automatically.
EOF
    return 1
  fi

  if ! command -v curl >/dev/null 2>&1; then
    echo "curl is required to install Homebrew." >&2
    return 1
  fi

  log "Homebrew not found; installing Homebrew"
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  ensure_brew_env

  if ! command -v brew >/dev/null 2>&1; then
    echo "Homebrew install finished but brew is still not in PATH." >&2
    return 1
  fi
}

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "This script is intended for macOS (Darwin)." >&2
  exit 1
fi

if ! xcode-select -p >/dev/null 2>&1; then
  cat >&2 <<'EOF'
Xcode Command Line Tools were not detected.
Please install them first (example):
  xcode-select --install
or via softwareupdate with sudo.
EOF
  exit 1
fi

install_brew_if_missing

if [[ "$BREW_UPDATE" == "1" ]]; then
  log "Running brew update"
  brew update
fi

PACKAGES=()
if [[ -n "${BREW_PACKAGES:-}" ]]; then
  read -r -a PACKAGES <<<"${BREW_PACKAGES}"
else
  PACKAGES=("${DEFAULT_PACKAGES[@]}")
fi

if [[ "${#PACKAGES[@]}" -eq 0 ]]; then
  log "No packages requested."
  exit 0
fi

MISSING=()
for pkg in "${PACKAGES[@]}"; do
  if brew list --versions "$pkg" >/dev/null 2>&1; then
    log "Present: $pkg"
    continue
  fi
  MISSING+=("$pkg")
done

if [[ "${#MISSING[@]}" -eq 0 ]]; then
  log "All requested Homebrew dependencies are already installed."
  exit 0
fi

log "Installing missing Homebrew packages: ${MISSING[*]}"
brew install "${MISSING[@]}"

log "Dependency installation completed."
