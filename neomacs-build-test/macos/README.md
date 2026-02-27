# Neomacs macOS Build Test

This folder provides a simple SSH-based build harness to test whether Neomacs can build on a macOS guest (for example Docker-OSX).

## One Command (Recommended)

Use a single script to launch Docker-OSX and run the full Neomacs build:

```bash
./docker-osx-full-build.sh
```

Defaults:

- Docker image: `neomacs/docker-osx:naked-auto-local`
- Docker container: `neomacs-macos15-test`
- CPU: `24` cores (`SMP=24`, `CORES=24`)
- SSH: `127.0.0.1:50922`, user `user`, password `alpine`
- Build mode: `full`
- Cleanup: container is stopped/removed automatically when script exits (`CLEANUP_CONTAINER=1`)
- Disk mode: persistent mounted disk (`USE_PERSISTENT_DISK=1`)

Fresh-from-base mode (recommended for no-manual, always-fresh runs):

```bash
FRESH_OVERLAY_DISK=1 \
BASE_DISK_IMAGE=/home/exec/virtual/macos15/mac_hdd_ng_sequoia.img \
./docker-osx-full-build.sh full
```

This creates a temporary qcow2 overlay on each run and deletes it on exit.

Note: `naked`/`naked-auto` without an installed disk usually boots Recovery.

Rust-only mode:

```bash
./docker-osx-full-build.sh rust-only
```

Common overrides:

```bash
DOCKER_CORES=24 \
MAKE_JOBS=24 \
MACOS_DISK_IMAGE=/home/exec/virtual/macos15/mac_hdd_ng_sequoia.img \
./docker-osx-full-build.sh full
```

## Host Prerequisites

- `ssh`, `scp`, `tar`
- `nc` (for `wait-ssh.sh`)
- `sshpass` if you are using password auth (default setup)

## Files

- `wait-ssh.sh`: wait until the guest SSH port is reachable.
- `build-over-ssh.sh`: sync this repo to macOS over SSH, run the build test, and download logs.
- `run-over-ssh.sh`: compatibility wrapper to `build-over-ssh.sh`.
- `guest-build.sh`: script executed inside macOS to run the build.
- `install-deps.sh`: install missing macOS build dependencies via Homebrew.

## Default SSH Target

Defaults match common Docker-OSX settings:

- Host: `127.0.0.1`
- Port: `50922`
- User: `user`
- Password: `alpine`

Override with env vars: `SSH_HOST`, `SSH_PORT`, `SSH_USER`, `SSH_PASSWORD`.

## Usage

From repo root:

```bash
cd neomacs-build-test/macos

# Optional: wait until guest SSH port is open
./wait-ssh.sh

# Rust-only smoke test
./build-over-ssh.sh rust-only

# Full build attempt (cargo + deps + autogen/configure/make)
./build-over-ssh.sh full

# No arg defaults to full
./build-over-ssh.sh
```

Logs are downloaded to `neomacs-build-test/macos/artifacts/`.

## Optional Overrides

- `SKIP_SYNC=1`: do not re-upload source tree; reuse remote workdir.
- `REMOTE_WORKDIR=...`: remote directory name (default `neomacs-build-src`).
- `RUST_FEATURES=...`: features used by `guest-build.sh` (default `video,neo-term,core-backend-emacs-c`).
- `RUST_NO_DEFAULT_FEATURES=0|1`: default `1`.
- `MAKE_JOBS=...`: parallelism for full mode.
- `NEOMACS_CONFIGURE_FLAGS=...`: full-mode configure flags.
  Default: `--without-ns --with-file-notification=no --with-native-compilation=no --with-neomacs --with-neovm-core-backend=emacs-c`.
- `AUTO_INSTALL_DEPS=0|1`: auto-run `install-deps.sh` in guest before full build (default `1`).
- `INSTALL_BREW=1`: allow auto-installing Homebrew if missing (used with `AUTO_INSTALL_DEPS=1`).

Example:

```bash
RUST_FEATURES='neo-term,core-backend-emacs-c' ./build-over-ssh.sh rust-only
```

## Guest Prerequisites (for `full` mode)

`full` mode expects autotools/build deps in macOS (for example via Homebrew), matching the project README macOS section.

You can install them with the helper script inside the macOS guest:

```bash
./install-deps.sh
```

Typical install command on macOS:

```bash
brew install autoconf automake texinfo pkgconf \
  glib cairo \
  gstreamer gst-plugins-base gst-plugins-good \
  jpeg-turbo libtiff giflib libpng librsvg webp \
  gnutls libxml2 sqlite jansson tree-sitter gmp mesa
```
