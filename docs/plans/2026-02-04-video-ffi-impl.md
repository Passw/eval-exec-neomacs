# Video FFI Integration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Integrate video playback into the render thread, routing commands through the queue.

**Architecture:** Render thread owns VideoCache. FFI sends commands via channel. Decoded frames processed each frame.

**Tech Stack:** Rust, wgpu, GStreamer, VA-API, DMA-BUF

---

### Task 1: Add VideoCache Import and Field

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add import**

Near the top imports, add:

```rust
#[cfg(feature = "video")]
use crate::backend::wgpu::VideoCache;
```

**Step 2: Add field to RenderApp**

After the webkit fields, add:

```rust
#[cfg(feature = "video")]
video_cache: Option<VideoCache>,
```

**Step 3: Initialize in new()**

Add to the Self {} block:

```rust
#[cfg(feature = "video")]
video_cache: None,
```

**Step 4: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(video): add VideoCache field to RenderApp"
```

---

### Task 2: Initialize VideoCache in init_wgpu

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add initialization after webkit initialization**

In `init_wgpu()`, after the webkit initialization block, add:

```rust
// Initialize video cache
#[cfg(feature = "video")]
{
    let mut video_cache = VideoCache::new();
    video_cache.init_gpu(&device);
    self.video_cache = Some(video_cache);
    log::info!("Video cache initialized");
}
```

**Step 2: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(video): initialize VideoCache in render thread"
```

---

### Task 3: Implement Video Command Handlers

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Implement VideoCreate handler**

Replace the TODO stub:

```rust
RenderCommand::VideoCreate { id, path } => {
    log::info!("Loading video {}: {}", id, path);
    #[cfg(feature = "video")]
    if let Some(ref mut cache) = self.video_cache {
        match cache.load_file(&path) {
            Ok(video_id) => {
                log::info!("Video loaded with id {}", video_id);
            }
            Err(e) => log::error!("Failed to load video {}: {:?}", id, e),
        }
    }
}
```

**Step 2: Implement VideoPlay handler**

```rust
RenderCommand::VideoPlay { id } => {
    log::debug!("Playing video {}", id);
    #[cfg(feature = "video")]
    if let Some(ref mut cache) = self.video_cache {
        cache.play(id);
    }
}
```

**Step 3: Implement VideoPause handler**

```rust
RenderCommand::VideoPause { id } => {
    log::debug!("Pausing video {}", id);
    #[cfg(feature = "video")]
    if let Some(ref mut cache) = self.video_cache {
        cache.pause(id);
    }
}
```

**Step 4: Implement VideoDestroy handler**

```rust
RenderCommand::VideoDestroy { id } => {
    log::info!("Destroying video {}", id);
    #[cfg(feature = "video")]
    if let Some(ref mut cache) = self.video_cache {
        cache.remove(id);
    }
}
```

**Step 5: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 6: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(video): implement video command handlers"
```

---

### Task 4: Process Video Frames Each Render

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add process_video_frames method**

After `process_webkit_frames`, add:

```rust
/// Process pending video frames
#[cfg(feature = "video")]
fn process_video_frames(&mut self) {
    let device = match &self.device {
        Some(d) => d,
        None => return,
    };
    let queue = match &self.queue {
        Some(q) => q,
        None => return,
    };

    if let Some(ref mut cache) = self.video_cache {
        cache.process_pending_videos(device, queue);
    }
}

#[cfg(not(feature = "video"))]
fn process_video_frames(&mut self) {}
```

**Step 2: Call in render method**

In `render()`, after `process_webkit_frames()`, add:

```rust
// Process video frames
self.process_video_frames();
```

**Step 3: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(video): process video frames each render cycle"
```

---

### Task 5: Add Video ID Counter to FFI

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Add atomic counter**

Near the WEBKIT_VIEW_ID_COUNTER, add:

```rust
/// Atomic counter for generating video IDs in threaded mode
#[cfg(feature = "video")]
static VIDEO_ID_COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(1);
```

**Step 2: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "feat(video): add VIDEO_ID_COUNTER for threaded mode"
```

---

### Task 6: Update neomacs_display_load_video for Threaded Mode

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Find and update the function**

Search for `neomacs_display_load_video` and add threaded path at the start:

```rust
#[cfg(feature = "winit-backend")]
if let Some(ref state) = THREADED_STATE {
    let id = VIDEO_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    let path_str = CStr::from_ptr(path).to_string_lossy().into_owned();
    let cmd = RenderCommand::VideoCreate { id, path: path_str };
    let _ = state.emacs_comms.cmd_tx.try_send(cmd);
    return id;
}
```

**Step 2: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "feat(video): update load_video FFI for threaded mode"
```

---

### Task 7: Update Video Control FFI Functions

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Update neomacs_display_video_play**

Add threaded path:

```rust
#[cfg(feature = "winit-backend")]
if let Some(ref state) = THREADED_STATE {
    let cmd = RenderCommand::VideoPlay { id: video_id };
    let _ = state.emacs_comms.cmd_tx.try_send(cmd);
    return 0;
}
```

**Step 2: Update neomacs_display_video_pause**

Add threaded path:

```rust
#[cfg(feature = "winit-backend")]
if let Some(ref state) = THREADED_STATE {
    let cmd = RenderCommand::VideoPause { id: video_id };
    let _ = state.emacs_comms.cmd_tx.try_send(cmd);
    return 0;
}
```

**Step 3: Update neomacs_display_video_stop (maps to destroy)**

Add threaded path:

```rust
#[cfg(feature = "winit-backend")]
if let Some(ref state) = THREADED_STATE {
    let cmd = RenderCommand::VideoDestroy { id: video_id };
    let _ = state.emacs_comms.cmd_tx.try_send(cmd);
    return 0;
}
```

**Step 4: Verify build**

Run: `cargo build --features video -p neomacs-display`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "feat(video): update video control FFI for threaded mode"
```

---

### Task 8: Integration Test

**Step 1: Build full project**

```bash
cargo build --features video -p neomacs-display
```

**Step 2: Run tests**

```bash
cargo test --lib --features video -p neomacs-display
```

**Step 3: Verify no regressions**

Expected: Same test results as before (41 pass, 1 pre-existing failure)

**Step 4: Final commit if any fixes needed**

```bash
git add -A
git commit -m "fix(video): integration fixes"
```
