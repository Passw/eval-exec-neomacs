# Neomacs - Modern Rust Display Engine

## Project Overview

**Neomacs** is a fork of GNU Emacs with a modernized GPU-accelerated display engine written in Rust.

### Project Decisions

| Decision | Choice |
|----------|--------|
| **Project Type** | Fork (maintained separately from GNU Emacs) |
| **Platform** | Linux only (initially) |
| **Display Server** | Wayland-first (via GTK4) |
| **GPU Rendering** | GTK4/GSK (uses Vulkan internally) |
| **Text Rendering** | Pango (via GTK4) |
| **Video Backend** | GStreamer |
| **Browser Embedding** | WPE WebKit (critical feature) |
| **Implementation** | Rust with C FFI |
| **Compatibility** | 90% (minor Lisp breakage acceptable) |
| **Backends** | TTY + GTK4 only (remove X11, W32, NS, etc.) |

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Emacs Core (C)                               │
│  Lisp interpreter, buffers, windows, faces, overlays            │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ C FFI
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│              libneomacs_display (Rust)                          │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    C API Layer (ffi.rs)                  │   │
│  └─────────────────────────────────┬───────────────────────┘   │
│                                    │                            │
│  ┌─────────────────────────────────▼───────────────────────┐   │
│  │              Display Engine Core (Rust)                  │   │
│  │  scene.rs, layout.rs, damage.rs, animation.rs           │   │
│  └─────────────────────────────────┬───────────────────────┘   │
│                                    │                            │
│  ┌──────────────┬──────────────────┼──────────────┬────────┐   │
│  │ TTY Backend  │  GTK4 Backend    │ Video        │ WebKit │   │
│  │ (tty.rs)     │  (gtk4-rs/GSK)   │ (gstreamer)  │ (wpe)  │   │
│  └──────────────┴──────────────────┴──────────────┴────────┘   │
│                            │                                    │
│                            ▼                                    │
│                   Vulkan / OpenGL (via GSK)                    │
└─────────────────────────────────────────────────────────────────┘
```

---

## Implementation Tasks

## Phase 1: Foundation & Project Setup ✅

### 1.1 Project Structure ✅
- [x] Create `rust/neomacs-display/` crate directory
- [x] Set up `Cargo.toml` with dependencies (gtk4, gsk4, pango, gstreamer)
- [x] Configure `cbindgen.toml` for C header generation
- [x] Create `build.rs` for build-time tasks
- [x] Create `shell.nix` for NixOS development
- [x] Create `Makefile` for build automation
- [ ] Set up CI/CD for Rust crate (cargo test, clippy, fmt)
- [ ] Add LICENSE (GPL3 to match Emacs)
- [ ] Create README.md for the Rust crate

### 1.2 Core Types ✅
- [x] Define `GlyphType` enum (Char, Image, Video, Wpe, etc.)
- [x] Define `Glyph` struct with `#[repr(C)]` for FFI
- [x] Define `Face` struct for text styling
- [x] Define `Color` type (RGBA)
- [x] Define `Rect`, `Point`, `Size` geometry types
- [x] Define `Transform` for 2D transformations
- [x] Write unit tests for core types

### 1.3 Scene Graph ✅
- [x] Define `NodeKind` enum (Container, TextRun, Image, Video, Wpe, ColorRect)
- [x] Define `Node` struct with bounds, opacity, clip, transform
- [x] Implement `Scene` struct as root container
- [x] Implement `WindowScene` struct for window representation
- [x] Implement scene graph builder methods
- [x] Write unit tests for scene graph

### 1.4 C FFI Layer ✅
- [x] Create `ffi.rs` module
- [x] Implement `neomacs_display_init()` / `neomacs_display_shutdown()`
- [x] Implement `neomacs_display_begin_frame()` / `neomacs_display_end_frame()`
- [x] Implement `neomacs_display_add_window()` / `neomacs_display_set_cursor()`
- [x] Implement animation FFI functions
- [ ] Generate C header with cbindgen
- [ ] Test FFI with simple C test program

### 1.5 Animation System ✅
- [x] Implement `Animation` struct with easing functions
- [x] Implement `AnimationManager` for cursor blink, smooth scroll
- [x] Write unit tests for animation

### 1.6 Backend Trait ✅
- [x] Define `DisplayBackend` trait
- [x] Create GTK4 backend skeleton
- [x] Create TTY backend skeleton

### 1.7 Study emacs-ng
- [ ] Clone and build emacs-ng
- [ ] Study `rust/emacs-sys/` - Emacs type bindings
- [ ] Study `rust/pgtk/` - GTK integration
- [ ] Study `rust/webrender/` - rendering approach
- [ ] Document lessons learned
- [ ] Identify code we can reuse or adapt

---

## Phase 2: TTY Backend

### 2.1 Terminal Output
- [ ] Create `backend/tty.rs` module
- [ ] Implement `DisplayBackend` trait for TTY
- [ ] Implement terminal capability detection (terminfo)
- [ ] Implement ANSI escape sequence generation
- [ ] Implement cursor positioning
- [ ] Implement color output (16, 256, truecolor)

### 2.2 Character Cell Rendering
- [ ] Convert scene graph to character cell grid
- [ ] Implement text rendering (UTF-8)
- [ ] Implement face/attribute rendering (bold, underline, etc.)
- [ ] Implement box-drawing characters for UI elements
- [ ] Handle wide characters (CJK)

### 2.3 TTY Optimizations
- [ ] Implement dirty region tracking
- [ ] Implement differential updates (only changed cells)
- [ ] Implement scroll optimization (terminal scroll regions)
- [ ] Benchmark TTY rendering performance

---

## Phase 3: GTK4 Backend - Basic Rendering ✅

### 3.1 GTK4 Application Setup ✅
- [x] Create `backend/gtk4/mod.rs` module
- [x] Initialize GTK4 with `gtk4::init()`
- [x] Create `GtkDrawingArea` as rendering canvas
- [x] Set up `set_draw_func` for frame rendering
- [x] Handle window resize events
- [ ] Create full `GtkApplication` integration
- [ ] Handle window close/destroy events
- [ ] Implement graceful shutdown

### 3.2 Cairo Drawing API ✅
- [x] Implement `DisplayBackend` trait for GTK4
- [x] Implement Cairo rectangle rendering (`rectangle()`, `fill()`)
- [x] Implement color rendering (`set_source_rgba()`)
- [x] Implement clipping (`clip()`)
- [x] Implement transforms (`save()`, `restore()`, `translate()`)
- [x] Create renderer module (`backend/gtk4/renderer.rs`)
- [ ] Port to GtkSnapshot API (for newer GTK4)

### 3.3 Text Rendering with Pango ✅
- [x] Get `PangoContext` from DrawingArea
- [x] Create `PangoLayout` for text runs
- [x] Implement `set_font_description()` for fonts
- [x] Implement `set_text()` for content
- [x] Implement text rendering with `pangocairo::show_layout()`
- [ ] Implement text measurement (`get_pixel_size()`)
- [ ] Handle foreground colors per-run
- [ ] Implement underline, strikethrough via Pango attributes
- [ ] Test CJK characters rendering
- [ ] Test emoji rendering

### 3.4 Glyph Atlas (GPU Text Caching)
- [ ] Create `core/atlas.rs` module
- [ ] Research if custom atlas needed (Pango/GSK may handle this)
- [ ] If needed: implement texture atlas allocation (bin packing)
- [ ] If needed: implement glyph rasterization to atlas
- [ ] If needed: implement glyph cache lookup (font + codepoint → coords)
- [ ] If needed: implement atlas texture upload to GPU
- [ ] If needed: implement atlas eviction/regeneration
- [ ] Benchmark text rendering performance vs current Emacs
- [ ] Profile GPU usage during text rendering

### 3.5 Input Handling (GTK4 Event Controllers)
- [ ] Create `backend/gtk4/input.rs` module
- [ ] Create `EventControllerKey` for keyboard input
- [ ] Implement `connect_key_pressed` handler
- [ ] Implement `connect_key_released` handler
- [ ] Convert `gdk4::Key` to Emacs key events
- [ ] Handle modifier keys (Ctrl, Alt, Shift, Super)
- [ ] Create `GestureClick` for mouse clicks
- [ ] Implement `connect_pressed` / `connect_released`
- [ ] Handle single, double, triple clicks
- [ ] Create `EventControllerMotion` for mouse movement
- [ ] Implement `connect_motion` for hover/tracking
- [ ] Create `EventControllerScroll` for scroll wheel
- [ ] Implement `connect_scroll` for wheel events
- [ ] Handle smooth scrolling (touchpad)
- [ ] Forward input events to Emacs event queue
- [ ] Test keyboard input with special keys (F1-F12, arrows, etc.)
- [ ] Test mouse input in different window regions

### 3.6 Frame Clock and Animation Loop
- [ ] Get `GdkFrameClock` from drawing area
- [ ] Implement `connect_update` callback
- [ ] Calculate delta time from `frame_time()`
- [ ] Call `begin_updating()` to start receiving updates
- [ ] Update animations in frame clock callback
- [ ] Call `queue_draw()` when content changes
- [ ] Implement frame rate limiting (if needed)
- [ ] Verify vsync synchronization
- [ ] Test smooth 60fps updates

### 3.7 Cursor Rendering
- [ ] Implement box cursor (filled rectangle)
- [ ] Implement bar cursor (thin line)
- [ ] Implement underline cursor
- [ ] Implement hollow cursor (unfocused)
- [ ] Implement cursor blinking via frame clock
- [ ] Handle cursor color from face
- [ ] Test cursor in different modes

---

## Phase 4: Image Support

### 4.1 Image Loading
- [ ] Create `backend/gtk4/image.rs` module
- [ ] Implement image loading (PNG, JPEG, GIF, SVG, WebP, AVIF)
- [ ] Implement image caching
- [ ] Implement image scaling/resampling
- [ ] Handle animated GIFs (see 4.4)

### 4.2 Image Rendering
- [ ] Convert images to GdkTexture
- [ ] Render images in scene graph
- [ ] Implement image clipping (partial display)
- [ ] Implement image transforms (rotation, flip)
- [ ] Support SVG rendering at any scale

### 4.3 Image FFI
- [ ] Implement `emacs_display_create_image()` FFI
- [ ] Implement `emacs_display_image_size()` FFI
- [ ] Implement `emacs_display_free_image()` FFI

### 4.4 Animated Image Support (GIF, APNG, WebP)
- [ ] Create `core/animated_image.rs` module
- [ ] Define `AnimatedImage` struct with frame cache
- [ ] Implement GIF decoder (pre-decode all frames to GPU textures)
- [ ] Implement APNG decoder
- [ ] Implement animated WebP decoder
- [ ] Implement animated AVIF decoder (optional)
- [ ] Create `AnimationManager` for all animated images
- [ ] Integrate with GTK4 frame clock (single animation loop)
- [ ] Implement per-frame delay handling
- [ ] Implement loop modes (forever, count, once)
- [ ] Implement play/pause/stop controls
- [ ] Implement speed control
- [ ] Benchmark: 10+ animated GIFs simultaneously
- [ ] Verify vsync-accurate frame timing

### 4.5 Animated Image FFI
- [ ] Implement `emacs_display_create_animated_image()` FFI
- [ ] Implement `emacs_display_animated_play()` FFI
- [ ] Implement `emacs_display_animated_pause()` FFI
- [ ] Implement `emacs_display_animated_set_speed()` FFI
- [ ] Implement `emacs_display_animated_get_frame_count()` FFI

### 4.6 Animated Image Lisp API
- [ ] Update `create-image` to detect animated images
- [ ] Implement `image-animate` using new backend
- [ ] Deprecate old timer-based animation
- [ ] Add `image-animation-frame-count` function
- [ ] Add `image-animation-current-frame` function

---

## Phase 5: Video Support (GStreamer)

### 5.1 GStreamer Setup
- [ ] Create `backend/gtk4/video.rs` module
- [ ] Initialize GStreamer
- [ ] Create video pipeline (uridecodebin → videoconvert → gtk4paintablesink)
- [ ] Handle pipeline state changes
- [ ] Handle end-of-stream
- [ ] Handle errors

### 5.2 Video Playback Control
- [ ] Implement play/pause
- [ ] Implement seek (absolute and relative)
- [ ] Implement volume control
- [ ] Implement playback speed control
- [ ] Implement loop mode
- [ ] Get current position and duration

### 5.3 Video Rendering
- [ ] Get GdkPaintable from GTK4 sink
- [ ] Convert paintable to texture
- [ ] Render video frame in scene graph
- [ ] Handle frame updates (redisplay on new frame)
- [ ] Implement aspect ratio preservation

### 5.4 Video FFI
- [ ] Implement `emacs_display_create_video()` FFI
- [ ] Implement `emacs_display_video_play()` FFI
- [ ] Implement `emacs_display_video_pause()` FFI
- [ ] Implement `emacs_display_video_seek()` FFI
- [ ] Implement `emacs_display_video_set_volume()` FFI
- [ ] Implement `emacs_display_video_get_state()` FFI

### 5.5 Video Lisp API
- [ ] Define `create-video` Lisp function
- [ ] Define `insert-video` Lisp function
- [ ] Define `video-play`, `video-pause`, `video-stop` functions
- [ ] Define `video-seek`, `video-set-volume` functions
- [ ] Define `video-playing-p`, `video-duration`, `video-current-time` functions

---

## Phase 6: WPE WebKit Support

### 6.1 WPE Backend Setup
- [ ] Create `backend/gtk4/wpe.rs` module
- [ ] Research WPE Rust bindings (or create minimal bindings)
- [ ] Initialize WPE WebKit context
- [ ] Create WPE view backend for offscreen rendering
- [ ] Set up buffer export (DMA-BUF or SHM)

### 6.2 WebKit View Management
- [ ] Create WebKit web view
- [ ] Load URI
- [ ] Handle navigation (back, forward, reload)
- [ ] Handle page load events
- [ ] Handle JavaScript execution
- [ ] Handle web notifications/alerts

### 6.3 WPE Rendering
- [ ] Receive rendered buffers from WPE
- [ ] Convert buffers to GdkTexture
- [ ] Render WPE view in scene graph
- [ ] Handle continuous updates

### 6.4 Input Forwarding
- [ ] Forward keyboard events to WPE
- [ ] Forward mouse events to WPE
- [ ] Forward scroll events to WPE
- [ ] Handle focus management

### 6.5 WPE FFI
- [ ] Implement `emacs_display_create_wpe()` FFI
- [ ] Implement `emacs_display_wpe_load_uri()` FFI
- [ ] Implement `emacs_display_wpe_go_back()` FFI
- [ ] Implement `emacs_display_wpe_go_forward()` FFI
- [ ] Implement `emacs_display_wpe_reload()` FFI
- [ ] Implement `emacs_display_wpe_execute_js()` FFI
- [ ] Implement `emacs_display_wpe_send_key()` FFI
- [ ] Implement `emacs_display_wpe_send_mouse()` FFI

### 6.6 WPE Lisp API
- [ ] Define `create-wpe-view` Lisp function
- [ ] Define `insert-wpe-view` Lisp function
- [ ] Define `wpe-load-uri`, `wpe-go-back`, `wpe-go-forward` functions
- [ ] Define `wpe-execute-js` with callback
- [ ] Define `wpe-uri`, `wpe-title` accessor functions

---

## Phase 7: Animation System

### 7.1 Animation Core
- [ ] Create `core/animation.rs` module
- [ ] Define animation types (scroll, fade, transform)
- [ ] Implement easing functions (linear, ease-in-out, cubic, etc.)
- [ ] Implement animation timeline/scheduler
- [ ] Handle animation completion callbacks

### 7.2 Smooth Scrolling
- [ ] Implement smooth scroll animation
- [ ] Integrate with GTK4 frame clock
- [ ] Handle scroll velocity/momentum
- [ ] Handle scroll interruption

### 7.3 Cursor Animation
- [ ] Implement cursor blink animation
- [ ] Implement cursor smooth movement (optional)

### 7.4 Transition Effects
- [ ] Implement fade in/out for windows
- [ ] Implement buffer switch transitions (optional)

---

## Phase 8: Emacs Integration

### 8.1 Build System Integration
- [ ] Add Rust build to Emacs `configure.ac`
- [ ] Add cargo build to `Makefile.in`
- [ ] Link `libemacs_display.a` with Emacs
- [ ] Add `--with-rust-display` configure option
- [ ] Handle cross-compilation

### 8.2 Modify dispnew.c
- [ ] Include generated C header
- [ ] Initialize Rust display engine on startup
- [ ] Replace `update_frame` to call Rust FFI
- [ ] Convert `glyph_matrix` to Rust `Glyph` array
- [ ] Handle backend selection (TTY vs GTK4)

### 8.3 Modify xdisp.c
- [ ] Add VIDEO_GLYPH and WPE_GLYPH handling in display spec
- [ ] Produce video/wpe glyphs from display properties
- [ ] Handle video/wpe glyph dimensions in layout

### 8.4 Modify keyboard.c
- [ ] Forward input to video/wpe when cursor on those glyphs
- [ ] Handle video playback shortcuts
- [ ] Handle WPE input mode

### 8.5 New Lisp Primitives
- [ ] Add video primitives to `src/video.c` (new file)
- [ ] Add WPE primitives to `src/wpe.c` (new file)
- [ ] Register primitives in `syms_of_video`, `syms_of_wpe`
- [ ] Add to `emacs.c` initialization

---

## Phase 9: Remove Legacy Backends

### 9.1 Remove X11 Backend
- [ ] Remove `xterm.c`, `xterm.h`
- [ ] Remove `xfns.c`
- [ ] Remove X11-specific code from `gtkutil.c`
- [ ] Update configure.ac
- [ ] Update Makefile.in

### 9.2 Remove Windows Backend
- [ ] Remove `w32term.c`, `w32term.h`
- [ ] Remove `w32fns.c`
- [ ] Remove `w32*.c` files
- [ ] Update configure.ac

### 9.3 Remove macOS Backend
- [ ] Remove `nsterm.m`, `nsterm.h`
- [ ] Remove `nsfns.m`
- [ ] Remove `ns*.m` files
- [ ] Update configure.ac

### 9.4 Remove Other Backends
- [ ] Remove Haiku backend (`haikuterm.c`, etc.)
- [ ] Remove Android backend (`androidterm.c`, etc.)
- [ ] Remove MS-DOS backend (`msdos.c`)
- [ ] Remove old X menu code (`oldXMenu/`)

### 9.5 Cleanup
- [ ] Remove `output_method` enum entries (keep initial, termcap, gtk4)
- [ ] Remove unused conditionals (`HAVE_X_WINDOWS`, `HAVE_NS`, etc.)
- [ ] Remove unused header includes
- [ ] Update documentation

---

## Phase 10: Testing & Documentation

### 10.1 Rust Unit Tests
- [ ] Test core types serialization
- [ ] Test scene graph construction
- [ ] Test glyph atlas packing
- [ ] Test animation interpolation
- [ ] Test TTY escape sequence generation

### 10.2 Integration Tests
- [ ] Test C FFI from test harness
- [ ] Test GTK4 rendering with screenshot comparison
- [ ] Test video playback
- [ ] Test WPE WebKit loading

### 10.3 Emacs Integration Tests
- [ ] Test basic text display
- [ ] Test face rendering (colors, styles)
- [ ] Test image display
- [ ] Test video insertion and playback
- [ ] Test WPE view insertion and navigation
- [ ] Test smooth scrolling

### 10.4 Performance Benchmarks
- [ ] Benchmark text rendering throughput
- [ ] Benchmark large buffer scrolling
- [ ] Benchmark image loading
- [ ] Benchmark video playback CPU/GPU usage
- [ ] Compare with old display engine

### 10.5 Documentation
- [ ] Write Rust API documentation (rustdoc)
- [ ] Write C FFI documentation
- [ ] Write Lisp API documentation (docstrings)
- [ ] Update Emacs manual for new features
- [ ] Write developer guide for extending display engine

---

## Phase 11: Polish & Optimization

### 11.1 Performance Optimization
- [ ] Profile and optimize hot paths
- [ ] Reduce memory allocations
- [ ] Optimize scene graph updates (incremental)
- [ ] Optimize glyph atlas usage
- [ ] Enable GPU shader optimizations

### 11.2 Error Handling
- [ ] Graceful fallback on GPU failure
- [ ] Handle video codec errors
- [ ] Handle WPE crash recovery
- [ ] Log errors to Emacs `*Messages*`

### 11.3 Accessibility
- [ ] Ensure screen reader compatibility
- [ ] Support high contrast themes
- [ ] Support reduced motion preferences
- [ ] Support system font scaling

### 11.4 Platform Testing
- [ ] Test on Linux (X11 via XWayland)
- [ ] Test on Linux (Wayland native)
- [ ] Test on macOS (via GTK4)
- [ ] Test on Windows (via GTK4/MSYS2)

---

## Milestones

| Milestone | Phases | Target |
|-----------|--------|--------|
| **M1: Basic Rendering** | 1-3 | TTY + GTK4 text rendering works |
| **M2: Feature Parity** | 4 | Images work like current Emacs |
| **M3: Video Support** | 5 | Video playback in buffers |
| **M4: WebKit Support** | 6 | WPE WebKit embedding works |
| **M5: Smooth UX** | 7 | Animations and smooth scrolling |
| **M6: Full Integration** | 8 | Emacs builds and runs with new engine |
| **M7: Cleanup** | 9 | Legacy backends removed |
| **M8: Release Ready** | 10-11 | Tested, documented, optimized |

---

## Dependencies

### Rust Crates
- `gtk4` (0.9+) - GTK4 bindings
- `gdk4` (0.9+) - GDK4 bindings  
- `gsk4` (0.9+) - GSK bindings
- `pango` (0.20+) - Text rendering
- `cairo-rs` (0.20+) - 2D graphics
- `gstreamer` (0.23+) - Video playback
- `gstreamer-video` (0.23+) - Video utilities
- `tokio` (1.0+) - Async runtime
- `log` (0.4+) - Logging
- `thiserror` (1.0+) - Error handling
- `cbindgen` (0.26+) - C header generation

### System Libraries
- GTK4 (4.10+)
- GStreamer (1.20+)
- WPE WebKit (2.38+) - when available
- Pango (1.50+)
- Cairo (1.16+)

---

## Open Questions

1. **WPE Rust bindings**: Do mature bindings exist, or need to create?
2. **GTK4 minimum version**: What's the minimum GTK4 version to support?
3. **Fallback rendering**: Should we support software rendering fallback?
4. **Thread model**: How to handle Rust async with Emacs event loop?
5. **Memory sharing**: How to efficiently share buffer text with Rust?

---

## References

- [gtk4-rs Documentation](https://gtk-rs.org/gtk4-rs/stable/latest/docs/gtk4/)
- [gstreamer-rs Documentation](https://gstreamer.freedesktop.org/documentation/rust/)
- [WPE WebKit](https://wpewebkit.org/)
- [cbindgen](https://github.com/mozilla/cbindgen)
- [Emacs Internals](https://www.gnu.org/software/emacs/manual/html_node/elisp/Display.html)
