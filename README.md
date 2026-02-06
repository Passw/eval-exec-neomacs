<p align="center">
   <img src="assets/banner1.png" alt="NEOMACS - The Future of Emacs"/>
</p>

<p align="center">
  <a href="#features"><img src="https://img.shields.io/badge/status-alpha-blueviolet?style=for-the-badge" alt="Status: Alpha"/></a>
  <a href="#building"><img src="https://img.shields.io/badge/rust-1.70+-orange?style=for-the-badge&logo=rust" alt="Rust 1.70+"/></a>
  <a href="#license"><img src="https://img.shields.io/badge/license-GPL--3.0-blue?style=for-the-badge" alt="License: GPL-3.0"/></a>
</p>

---

## The Problem

Emacs's display engine (~50,000 lines of C in `xdisp.c`) was designed for text terminals in the 1980s. Despite decades of patches, it fundamentally struggles with:

- **Large images** — rendering slows down significantly
- **Video playback** — not natively supported
- **Modern animations** — no smooth cursor movement, buffer transitions, or visual effects
- **Web content** — limited browser integration
- **GPU utilization** — everything runs on CPU while your GPU sits idle

## The Solution

Throw it all away and start fresh.

**Neomacs** replaces Emacs's entire display subsystem with a modern **Rust + GPU** architecture:

- **~4,000 lines of Rust** replacing ~50,000 lines of legacy C
- **wgpu** for cross-platform GPU-accelerated rendering (Vulkan/Metal/DX12/OpenGL)
- **winit** for native window management
- **Zero-copy DMA-BUF** for efficient GPU texture sharing (Linux)
- **cosmic-text** for pure-Rust text shaping
- **Full Emacs compatibility** — your config and packages still work

---

## Features

### Working Now

| Feature | Description |
|---------|-------------|
| **GPU Text Rendering** | Hardware-accelerated text via wgpu (Vulkan/Metal/DX12/OpenGL) |
| **Video Playback** | GStreamer + VA-API hardware decode with DMA-BUF zero-copy |
| **Cursor Animations** | Neovide-style effects: railgun, torpedo, pixiedust, sonicboom, ripple |
| **Smooth Scrolling** | Animated scroll with configurable easing |
| **Buffer Transitions** | Fade/slide effects when switching buffers |
| **DMA-BUF Zero-Copy** | GPU-to-GPU texture sharing via Vulkan HAL (no CPU readback) |
| **Inline Images** | GPU-accelerated image rendering in buffers |

### The Ambitious Vision

Neomacs aims to transform Emacs from a text editor into a **modern graphical computing environment**:

**Rich Media First-Class Citizen**
- 4K video playback directly in buffers with hardware decoding
- PDF rendering with GPU acceleration
- Image manipulation and annotation

**GPU-Native Everything**
- Hardware-accelerated rendering for all content
- Shader effects (blur, shadows, glow)
- 120fps smooth animations
- Minimal CPU usage, maximum battery life

**Modern UI/UX**
- Neovide-style cursor animations
- Buffer transition effects
- Smooth scrolling everywhere
- Window animations and effects

**GPU-Powered Terminal Emulator**
- Blazing fast terminal emulation written in Rust
- GPU-accelerated rendering for smooth 120fps scrolling
- Replaces slow Emacs `term.el`/`ansi-term` and vterm (which suffer from Emacs redisplay bottlenecks)
- True color support, ligatures, and modern terminal features
- Zero-latency input handling

**Cross-Platform Excellence**
- Linux (Vulkan on Wayland & X11)
- macOS (Metal backend)
- Windows (Vulkan/DX12)

The goal: **Make Emacs the most powerful and beautiful computing environment on any platform.**

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Emacs Core (C/Lisp)                     │
└─────────────────────────┬───────────────────────────────────┘
                          │
┌─────────────────────────▼───────────────────────────────────┐
│                 Rust Display Engine                         │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │ Text Engine │  │ Animations  │  │ Video Pipeline      │  │
│  │ cosmic-text │  │ cursor/     │  │ GStreamer + VA-API  │  │
│  │ + atlas     │  │ transitions │  │ DMA-BUF zero-copy   │  │
│  └──────┬──────┘  └──────┬──────┘  └──────────┬──────────┘  │
│         └────────────────┼────────────────────┘             │
│                          ▼                                  │
│              ┌───────────────────────┐                      │
│              │     wgpu Renderer     │                      │
│              │  (GPU Render Pipeline)│                      │
│              └───────────┬───────────┘                      │
│                          │                                  │
│              ┌───────────▼───────────┐                      │
│              │   winit (Windowing)   │                      │
│              └───────────────────────┘                      │
└─────────────────────────────────────────────────────────────┘
                           │
              ┌────────────┼────────────┐
              ▼            ▼            ▼
        ┌─────────┐  ┌─────────┐  ┌─────────┐
        │ Vulkan  │  │  Metal  │  │DX12/GL  │
        │ (Linux) │  │ (macOS) │  │(Windows)│
        └─────────┘  └─────────┘  └─────────┘
```

### Why Rust?

- **Memory safety** without garbage collection
- **Zero-cost abstractions** for high-performance rendering
- **Excellent FFI** with C (Emacs core)
- **Modern tooling** (Cargo, async, traits)
- **Growing ecosystem** for graphics (wgpu, winit, cosmic-text)

### Why wgpu?

- **Cross-platform** — single API for Vulkan, Metal, DX12, and OpenGL
- **Safe Rust API** — no unsafe Vulkan/Metal code in application
- **WebGPU standard** — future-proof API design
- **Active development** — used by Firefox, Bevy, and many others

---

## Quick Demo

### Video Playback

```elisp
;; Insert video directly in buffer (VA-API hardware decode + DMA-BUF zero-copy)
(neomacs-video-insert "/path/to/video.mp4" 640 360)

;; Control playback
(neomacs-video-play video-id)
(neomacs-video-pause video-id)
(neomacs-video-stop video-id)
```

### Inline Images

```elisp
;; GPU-accelerated image display
(insert-image (create-image "/path/to/image.png"))
```

---

## Building

### Prerequisites

- **Emacs source** (this is a fork)
- **Rust** (stable, 1.70+)
- **GStreamer** (for video playback)
- **VA-API** (optional, for hardware video decode on Linux)

### Linux (Arch Linux)

```bash
# Install dependencies
sudo pacman -S --needed \
  base-devel autoconf automake texinfo clang git pkg-config \
  gtk4 glib2 cairo \
  gstreamer gst-plugins-base gst-plugins-good gst-plugins-bad \
  wpewebkit wpebackend-fdo \
  wayland wayland-protocols \
  mesa libva \
  libjpeg-turbo libtiff giflib libpng librsvg libwebp \
  ncurses gnutls libxml2 sqlite jansson tree-sitter \
  gmp acl libxpm \
  libgccjit

# Install Rust (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Build the Rust display engine
cargo build --release --manifest-path rust/neomacs-display/Cargo.toml

# Build Emacs
./autogen.sh
./configure --with-neomacs --with-native-compilation
make -j$(nproc)
```

> **Note:** WPE WebKit (`wpewebkit`) is required for browser embedding. It is available in
> Arch Linux repos and via NixOS. On distros without WPE WebKit packages, the build will
> skip webkit support and build the Rust crate with `--no-default-features --features "winit-backend,video"`.

### Docker (Build Test)

```bash
docker build -t neomacs-build-test .
```

Uses Arch Linux. See the [Dockerfile](Dockerfile) for the full build environment.

### Nix

```bash
# Enter development shell
nix-shell

# Build
./autogen.sh
./configure --with-neomacs --with-native-compilation
make -j$(nproc)
```

---

## Project Structure

```
neomacs/
├── rust/neomacs-display/     # Rust display engine
│   ├── src/
│   │   ├── core/             # Types, animations, scene graph
│   │   ├── backend/wgpu/     # wgpu GPU renderer
│   │   │   ├── mod.rs        # Main renderer
│   │   │   ├── video_cache.rs    # GStreamer video pipeline
│   │   │   ├── vulkan_dmabuf.rs  # DMA-BUF zero-copy import
│   │   │   └── shaders/      # WGSL shaders
│   │   ├── text/             # cosmic-text + glyph atlas
│   │   └── ffi.rs            # C FFI layer
│   └── Cargo.toml
├── src/                      # Emacs C source (with Rust hooks)
└── doc/display-engine/       # Design documentation
```

---

## Contributing

Contributions welcome! Areas where help is needed:

- **Graphics programmers** — shader effects, rendering optimizations
- **Rust developers** — architecture, performance, safety
- **Emacs hackers** — Lisp API design, integration testing
- **Documentation** — tutorials, API docs, examples

See [doc/display-engine/DESIGN.md](doc/display-engine/DESIGN.md) for architecture details.

---

## Acknowledgments

Built with:
- [wgpu](https://wgpu.rs/) — Cross-platform GPU rendering (Vulkan/Metal/DX12/GL)
- [winit](https://github.com/rust-windowing/winit) — Cross-platform window management
- [cosmic-text](https://github.com/pop-os/cosmic-text) — Pure Rust text shaping
- [GStreamer](https://gstreamer.freedesktop.org/) — Video playback with VA-API
- [ash](https://github.com/ash-rs/ash) — Vulkan bindings for DMA-BUF import
- Inspired by [Neovide](https://neovide.dev/) cursor animations

---

## License

GNU General Public License v3.0 (same as Emacs)
