# Releasing Neomacs for Linux

## Where each Linux artifact is built

| Artifact | Built on | Why there |
|---|---|---|
| tarball, `.deb`, AppImage | the `ubuntu-22.04` runner | Their linkage is Debian-family by design, and 22.04 is the oldest baseline the release still carries (glibc 2.35). |
| `.rpm` | an el9 container (`build-linux-rpm`) | Because the build host is part of an RPM's metadata — see below. |

## Why the RPM is built inside el9

`rpmbuild` derives each package's library `Requires` from the payload's ELF *and*
from the build host's rpm, so an RPM built on the Ubuntu runner records Ubuntu's
libraries as Fedora's requirements:

```
Requires: libtinfo.so.6(NCURSES6_TINFO_5.0.19991023)(64bit)
Requires: libm.so.6(GLIBC_2.35)(64bit)
```

The first line is Ubuntu's ncurses, which is built with versioned symbols.
Fedora's ncurses publishes no such version and no Fedora package provides it, so
`dnf` refused the package outright — "nothing provides
libtinfo.so.6(NCURSES6_TINFO_5.0.19991023)(64bit)" (issue #388). The second line
is Ubuntu 22.04's glibc, which rules out RHEL 9 and its rebuilds: their glibc is
2.34.

Building the binary **and** the package inside el9 fixes both by construction.
The requirements become el9's, and a binary linked against glibc 2.34 and an
unversioned ncurses runs on everything newer — RHEL 9 / Rocky / Alma and Fedora
43+. The GLIBC 2.35 ceiling that `scripts/test-linux-release-artifacts.sh`
enforces still passes, because 2.34 sorts below it.

`%{?dist}` is defined on el9, so the file is
`dist/neomacs-<version>-1.el9.<arch>.rpm`. Scripts that name or find that file
derive it from the directory rather than hardcoding the tag — the tag belongs to
the build host.

## Building the RPM by hand

The CI job is `build-linux-rpm` in `.github/workflows/release.yml`. The same
thing locally, from the repository root — build in a scratch clone rather than
the working tree, because the container runs as root and would otherwise leave
root-owned files (and a root-owned `target/`) behind:

```bash
git clone --local . tmp/el9build

docker run --rm --network host -v "$PWD/tmp/el9build:/build/src" \
  quay.io/almalinuxorg/almalinux:9 bash -c '
    set -e
    dnf install -y git tar gzip which make binutils cpio file gcc gcc-c++ \
      pkgconf-pkg-config gawk ncurses-devel fontconfig-devel freetype-devel \
      glib2-devel gstreamer1-devel gstreamer1-plugins-base-devel libdrm-devel \
      zlib-devel lcms2-devel rpm-build
    dnf install -y gstreamer1 gstreamer1-plugins-base glib2 fontconfig freetype \
      ncurses-libs libstdc++ zlib libdrm
    curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs \
      | sh -s -- -y --profile minimal --default-toolchain 1.96.1
    . "$HOME/.cargo/env"
    cd /build/src
    # The clone is owned by your uid while the container runs as root, so git
    # refuses it as dubiously owned -- silently, because package-rpm.sh discards
    # stderr.  Without this line `git describe` and `git rev-parse` both fail,
    # the version falls back to 0.0.0-dev, and rpmbuild rejects it at the very
    # end with "Illegal char '-' (0x2d) in: Version".
    git config --global --add safe.directory /build/src
    export NEOMACS_BUILD_PROFILE=release
    features=video,neomacs-layout-engine/freetype-bundled
    cargo build -p neomacs --features "$features" --profile release
    cargo xtask fresh-build --release --features "$features" --skip-build
    ./scripts/package-rpm.sh --target x86_64-unknown-linux-gnu --skip-build
  '
```

Details that cost time to discover:

- The el9 image already ships `curl-minimal`, which provides the `curl` binary.
  Asking for the full `curl` package is a hard conflict on el9 ("curl-minimal
  conflicts with curl provided by curl").
- Once the image is running, `git` must be installed before anything that needs
  it (`actions/checkout` in CI, `git clone` locally). The image has no git.
- `libdrm-devel` and `gawk` are mandatory and invisible to the payload's
  `DT_NEEDED`: `gstreamer-allocators-sys` links `drm`, and `neovm-core/build.rs`
  generates Lisp with GNU awk and panics without it.
- `gcc-c++` is required even though Neomacs is Rust: `simdutf` (via `rio-vt`)
  compiles C++ and links `libstdc++`.
- `gstreamer1-plugins-base-devel` must accompany `gstreamer1-devel`; the former
  supplies `gstapp`, `gstvideo`, `gstpbutils` and `gstallocators`.
- `make` is needed by `tikv-jemalloc-sys`, which runs `configure` and `make`.
- The second `dnf install` is not optional: the packaging scripts execute the
  binary they stage, so the runtime closure has to be present.

## Why there is no source RPM

`rpmbuild -ba` on this spec produces a hollow `.src.rpm`, and RPM is behaving
correctly: an SRPM carries the spec plus the files named in `SourceN:`, and this
spec names none -- it has no `Source:`, no `%prep` and no `%build`, because it
re-packages an already-built payload rather than compiling one.  Measured:

```
$ rpm -qpl neomacs-0.0.18-1.el9.src.rpm
neomacs.spec                       # 6.9 KB, nothing else
```

Shipping that would be worse than shipping nothing, since it looks like source.
Making it real means either embedding the 149 MB payload tarball as `Source0:`
(an SRPM that contains prebuilt binaries, which Fedora and COPR reject as a
source package), or writing a genuine source spec -- `Source0:` pointing at the
upstream tree, `%build` running cargo, `BuildRequires`, and vendored crates
because build hosts have no network.  The latter is the COPR/Packit path, and it
is where an SRPM belongs anyway: COPR builds and serves it from the repo, and
you stop hand-carrying the RPM at all.

The packager identity is passed to the build as a macro rather than written into
the spec, because Fedora's guidelines prohibit `Vendor:` and `Packager:` tags
there -- the build system is meant to own them.

## What CI verifies

1. **`build-linux-rpm`** installs its own output on el9
   (`dnf install -y ./dist/*.rpm`, no `--nodeps`) and runs
   `neomacs --batch --eval "(kill-emacs 0)"` — the package's declared
   dependencies resolved by a real transaction on the distro it was built for.
   It also fails outright if a versioned ncurses/tinfo requirement reappears.
2. **`verify-rpm-on-fedora`** downloads the x86_64 RPM and installs it on a real
   Fedora container, then runs the binary. This is the assertion issue #388
   needed and nothing performed before it shipped. `create-release` lists it in
   `needs:`, so a failing Fedora install blocks the release rather than
   annotating it.
3. **`scripts/test-linux-release-artifacts.sh --formats tar,rpm`** extracts the
   package, runs the binary from the extracted tree, and audits the GLIBC
   ceiling, the archlib layout and the desktop entry.

When changing `scripts/package-rpm.sh`, run the command above and check
`rpm -qp --requires dist/*.rpm`. What an el9 build records, and what to insist
on:

- **No ncurses/tinfo symbol version at all** — the requirement must read
  `libtinfo.so.6()(64bit)`, never
  `libtinfo.so.6(NCURSES6_TINFO_5.0.19991023)(64bit)`. That version stamp *is*
  issue #388. `build-linux-rpm` fails the job if it reappears.
- **No glibc version el9 does not itself provide.** The 2.35 ceiling in the
  artifact audit is the enforced bound; a build here lands at `GLIBC_2.34` for
  `libc.so.6` and `GLIBC_2.29` for `libm.so.6`, which is what the el9 install
  transaction proves.
