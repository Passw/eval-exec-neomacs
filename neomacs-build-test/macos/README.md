# Neomacs macOS Build Test

This folder provides a simple SSH-based build harness to test whether Neomacs can build on a macOS guest (for example Docker-OSX).

## Host Prerequisites

- `ssh`, `scp`, `tar`
- `nc` (for `wait-ssh.sh`)
- `sshpass` if you are using password auth (default setup)

## Files

- `wait-ssh.sh`: wait until the guest SSH port is reachable.
- `run-over-ssh.sh`: sync this repo to macOS over SSH, run the build test, and download logs.
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
./run-over-ssh.sh rust-only

# Install missing full-build dependencies first (on the guest)
AUTO_INSTALL_DEPS=1 ./run-over-ssh.sh full

# Full build attempt (cargo + autogen/configure/make)
./run-over-ssh.sh full
```

Logs are downloaded to `neomacs-build-test/macos/artifacts/`.

## Optional Overrides

- `SKIP_SYNC=1`: do not re-upload source tree; reuse remote workdir.
- `REMOTE_WORKDIR=...`: remote directory name (default `neomacs-build-src`).
- `RUST_FEATURES=...`: features used by `guest-build.sh` (default `video,neo-term`).
- `RUST_NO_DEFAULT_FEATURES=0|1`: default `1`.
- `MAKE_JOBS=...`: parallelism for full mode.
- `NEOMACS_CONFIGURE_FLAGS=...`: full-mode configure flags.
- `AUTO_INSTALL_DEPS=1`: auto-run `install-deps.sh` in guest before full build.
- `INSTALL_BREW=1`: allow auto-installing Homebrew if missing (used with `AUTO_INSTALL_DEPS=1`).

Example:

```bash
RUST_FEATURES=neo-term ./run-over-ssh.sh rust-only
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
