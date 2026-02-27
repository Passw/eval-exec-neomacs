#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-full}"
SOURCE_DIR="${2:-$HOME/neomacs-src}"
LOG_FILE="${3:-$HOME/neomacs-build.log}"
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$MODE" in
  rust-only|full) ;;
  *)
    echo "Usage: $0 [rust-only|full] [source-dir] [log-file]" >&2
    exit 2
    ;;
esac

RUST_FEATURES="${RUST_FEATURES:-video,neo-term,core-backend-emacs-c}"
RUST_NO_DEFAULT_FEATURES="${RUST_NO_DEFAULT_FEATURES:-1}"
MAKE_JOBS="${MAKE_JOBS:-$(sysctl -n hw.logicalcpu 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)}"
CONFIGURE_FLAGS_DEFAULT="--without-ns --with-file-notification=no --with-native-compilation=no --with-neomacs --with-neovm-core-backend=emacs-c"
CONFIGURE_FLAGS="${NEOMACS_CONFIGURE_FLAGS:-$CONFIGURE_FLAGS_DEFAULT}"
AUTO_INSTALL_DEPS="${AUTO_INSTALL_DEPS:-1}"
INSTALL_BREW="${INSTALL_BREW:-0}"

log() {
  printf '[%s] %s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$*"
}

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Missing required command: $1" >&2
    return 1
  fi
}

ensure_rust() {
  if command -v cargo >/dev/null 2>&1; then
    return 0
  fi

  log "cargo not found; installing Rust with rustup"
  require_cmd curl
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --profile minimal --default-toolchain stable
}

ensure_brew_env() {
  # Non-login SSH shells often miss Homebrew paths on macOS.
  export PATH="/usr/local/bin:/opt/homebrew/bin:$PATH"

  if command -v brew >/dev/null 2>&1; then
    # shellcheck disable=SC2046
    eval "$(/usr/local/bin/brew shellenv 2>/dev/null || brew shellenv 2>/dev/null || true)"
  fi
}

mkdir -p "$(dirname "$LOG_FILE")"
exec > >(tee "$LOG_FILE") 2>&1

log "Mode: $MODE"
log "Source: $SOURCE_DIR"
log "Log file: $LOG_FILE"

if [[ ! -d "$SOURCE_DIR" ]]; then
  echo "Source directory does not exist: $SOURCE_DIR" >&2
  exit 1
fi

SOURCE_DIR="$(cd "$SOURCE_DIR" && pwd)"

ensure_brew_env
ensure_rust
if [[ -f "$HOME/.cargo/env" ]]; then
  # shellcheck source=/dev/null
  . "$HOME/.cargo/env"
fi
require_cmd cargo

cd "$SOURCE_DIR"

log "Running Rust build for rust/neomacs-display"
cargo_args=(build --release --manifest-path rust/neomacs-display/Cargo.toml)
if [[ "$RUST_NO_DEFAULT_FEATURES" == "1" ]]; then
  cargo_args+=(--no-default-features)
fi
if [[ -n "$RUST_FEATURES" ]]; then
  cargo_args+=(--features "$RUST_FEATURES")
fi
cargo "${cargo_args[@]}"

if [[ "$MODE" == "full" ]]; then
  if [[ "$AUTO_INSTALL_DEPS" == "1" ]]; then
    if [[ -x "$SELF_DIR/install-deps.sh" ]]; then
      log "AUTO_INSTALL_DEPS=1; installing missing macOS dependencies"
      INSTALL_BREW="$INSTALL_BREW" "$SELF_DIR/install-deps.sh"
      ensure_brew_env
    else
      echo "AUTO_INSTALL_DEPS=1 was set, but install-deps.sh was not found at $SELF_DIR/install-deps.sh" >&2
      exit 1
    fi
  fi

  for cmd in autoconf automake aclocal pkg-config make; do
    require_cmd "$cmd"
  done

  log "Running full Neomacs build (autogen/configure/make)"
  ./autogen.sh

  read -r -a configure_argv <<< "$CONFIGURE_FLAGS"
  ./configure "${configure_argv[@]}"
  make -j"$MAKE_JOBS"
fi

log "Build test completed successfully."
