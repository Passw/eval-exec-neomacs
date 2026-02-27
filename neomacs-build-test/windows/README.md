# Neomacs Windows Build Test (Dockur)

This harness boots a Windows VM with `dockurr/windows`, enables OpenSSH during setup, and runs a Neomacs build smoke test over SSH.

## What This Tests

- Windows VM booted in Docker on Linux with KVM acceleration.
- SSH access into the Windows guest on `localhost:2222`.
- A Windows-side build attempt against this repo mounted as `\\host.lan\Data`.

Default mode is `rust-only` and runs:

```powershell
cargo +stable-x86_64-pc-windows-gnu build --release --target x86_64-pc-windows-gnu --manifest-path rust/neomacs-display/Cargo.toml --no-default-features --features neo-term
```

`full` mode also tries `./autogen.sh && ./configure && make` through `bash` (requires an MSYS2-style toolchain).

## Prerequisites

- Docker Engine + Docker Compose
- `/dev/kvm` and `/dev/net/tun` available on host
- `ssh`, `scp`, `ssh-keyscan` on host
- Internet access (first boot downloads Windows image)

## Usage

Single command (recommended):

```bash
cd neomacs-build-test/windows
./run-build.sh                 # rust-only mode
# or
./run-build.sh full            # full mode
```

`run-build.sh` handles:
- `docker compose up -d`
- waiting for guest SSH
- uploading the build runner
- executing the Windows build test

Optional:

```bash
SKIP_UP=1 ./run-build.sh
WINDOWS_VERSION=tiny11 ./run-build.sh
```

### Connection Details

- Web viewer: `http://127.0.0.1:8006`
- RDP: `127.0.0.1:3389`
- SSH: `ssh -p 2222 Docker@127.0.0.1`
- Default user/password: `Docker` / `admin`

## Notes

- First install can take 10-30+ minutes depending on network and storage speed.
- Current Neomacs `README.md` marks Windows as unsupported, so failures here are expected and useful for gap tracking.
- Build logs are written inside the guest to `C:\Users\Docker\neomacs-build.log`.
