# GLib/WebKit Integration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Integrate GLib context pumping and WebKit view management into the render thread.

**Architecture:** Render thread owns WpeBackend and WebKit views. GLib context pumped every frame. State changes sent to Emacs via InputEvents.

**Tech Stack:** Rust, wgpu, WPE WebKit, GLib, crossbeam-channel

---

### Task 1: Implement pump_glib()

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs:282-287`

**Step 1: Add GLib imports**

At the top of the file, add conditional import:

```rust
#[cfg(feature = "wpe-webkit")]
use crate::backend::wpe::sys as plat;
```

**Step 2: Implement pump_glib**

Replace the stub:

```rust
#[cfg(feature = "wpe-webkit")]
fn pump_glib(&self) {
    unsafe {
        // WPEViewHeadless attaches to thread-default context
        let thread_ctx = plat::g_main_context_get_thread_default();
        let ctx = if thread_ctx.is_null() {
            plat::g_main_context_default()
        } else {
            thread_ctx
        };

        // Non-blocking iteration - process all pending events
        while plat::g_main_context_iteration(ctx, 0) != 0 {}

        // Also check default context if different
        let default_ctx = plat::g_main_context_default();
        if default_ctx != ctx {
            while plat::g_main_context_iteration(default_ctx, 0) != 0 {}
        }
    }
}
```

**Step 3: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(webkit): implement GLib context pumping in render thread"
```

---

### Task 2: Add WebKit State Events

**Files:**
- Modify: `rust/neomacs-display/src/thread_comm.rs`

**Step 1: Find InputEvent enum**

Search for `pub enum InputEvent` in thread_comm.rs.

**Step 2: Add WebKit event variants**

Add these variants to the enum:

```rust
#[cfg(feature = "wpe-webkit")]
WebKitTitleChanged { id: u32, title: String },

#[cfg(feature = "wpe-webkit")]
WebKitUrlChanged { id: u32, url: String },

#[cfg(feature = "wpe-webkit")]
WebKitProgressChanged { id: u32, progress: f64 },

#[cfg(feature = "wpe-webkit")]
WebKitLoadFinished { id: u32 },
```

**Step 3: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/thread_comm.rs
git commit -m "feat(webkit): add InputEvent variants for WebKit state changes"
```

---

### Task 3: Add WebKit Fields to RenderApp

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add imports**

```rust
#[cfg(feature = "wpe-webkit")]
use crate::backend::wpe::backend::WpeBackend;

#[cfg(feature = "wpe-webkit")]
use crate::backend::wpe::view::WpeWebView;

#[cfg(feature = "wpe-webkit")]
use crate::backend::wgpu::webkit_cache::WgpuWebKitCache;

#[cfg(feature = "wpe-webkit")]
use std::collections::HashMap as WebKitHashMap;
```

**Step 2: Add fields to RenderApp struct**

Add after `mouse_pos` field:

```rust
#[cfg(feature = "wpe-webkit")]
wpe_backend: Option<WpeBackend>,

#[cfg(feature = "wpe-webkit")]
webkit_views: WebKitHashMap<u32, WpeWebView>,

#[cfg(feature = "wpe-webkit")]
webkit_texture_cache: Option<WgpuWebKitCache>,
```

**Step 3: Initialize fields in RenderApp::new()**

Add to the Self {} block:

```rust
#[cfg(feature = "wpe-webkit")]
wpe_backend: None,

#[cfg(feature = "wpe-webkit")]
webkit_views: WebKitHashMap::new(),

#[cfg(feature = "wpe-webkit")]
webkit_texture_cache: None,
```

**Step 4: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(webkit): add WebKit fields to RenderApp"
```

---

### Task 4: Initialize WpeBackend

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add initialization after wgpu setup**

In `init_wgpu()`, after device/queue are created, add:

```rust
// Initialize WPE backend for WebKit
#[cfg(feature = "wpe-webkit")]
{
    // Get DRM render node from adapter info
    let adapter_info = adapter.get_info();
    let render_node = if adapter_info.backend == wgpu::Backend::Vulkan {
        // Extract render node path from adapter name/driver
        // Format varies by driver, try common patterns
        crate::backend::wgpu::drm_device::find_render_node_for_adapter(&adapter_info)
    } else {
        None
    };

    match WpeBackend::new_with_device(render_node.as_deref()) {
        Ok(backend) => {
            log::info!("WPE backend initialized");
            self.wpe_backend = Some(backend);
        }
        Err(e) => {
            log::warn!("Failed to initialize WPE backend: {:?}", e);
        }
    }
}
```

**Step 2: Initialize webkit texture cache**

After renderer creation:

```rust
#[cfg(feature = "wpe-webkit")]
{
    self.webkit_texture_cache = Some(WgpuWebKitCache::new(&device));
}
```

**Step 3: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(webkit): initialize WpeBackend in render thread"
```

---

### Task 5: Handle WebKit Commands

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Implement WebKitCreate handler**

Replace the TODO in process_commands():

```rust
RenderCommand::WebKitCreate { id, width, height } => {
    log::info!("Creating WebKit view: id={}, {}x{}", id, width, height);
    #[cfg(feature = "wpe-webkit")]
    if let Some(ref backend) = self.wpe_backend {
        if let Some(platform_display) = backend.platform_display() {
            match WpeWebView::new(id, platform_display, width, height) {
                Ok(view) => {
                    self.webkit_views.insert(id, view);
                    log::info!("WebKit view {} created successfully", id);
                }
                Err(e) => log::error!("Failed to create WebKit view {}: {:?}", id, e),
            }
        }
    }
}
```

**Step 2: Implement WebKitLoadUri handler**

```rust
RenderCommand::WebKitLoadUri { id, url } => {
    log::info!("Loading URL in WebKit view {}: {}", id, url);
    #[cfg(feature = "wpe-webkit")]
    if let Some(view) = self.webkit_views.get_mut(&id) {
        if let Err(e) = view.load_uri(&url) {
            log::error!("Failed to load URL: {:?}", e);
        }
    }
}
```

**Step 3: Implement WebKitResize handler**

```rust
RenderCommand::WebKitResize { id, width, height } => {
    log::debug!("Resizing WebKit view {}: {}x{}", id, width, height);
    #[cfg(feature = "wpe-webkit")]
    if let Some(view) = self.webkit_views.get_mut(&id) {
        view.resize(width, height);
    }
}
```

**Step 4: Implement WebKitDestroy handler**

```rust
RenderCommand::WebKitDestroy { id } => {
    log::info!("Destroying WebKit view {}", id);
    #[cfg(feature = "wpe-webkit")]
    {
        self.webkit_views.remove(&id);
        if let Some(ref mut cache) = self.webkit_texture_cache {
            cache.remove(id);
        }
    }
}
```

**Step 5: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 6: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(webkit): implement WebKit command handlers"
```

---

### Task 6: Update pump_glib to Update Views

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Change pump_glib signature to &mut self**

```rust
#[cfg(feature = "wpe-webkit")]
fn pump_glib(&mut self) {
```

**Step 2: Add view updates after GLib iteration**

At the end of pump_glib(), add:

```rust
    // Update all webkit views (fetches title, URL, etc.)
    for (id, view) in self.webkit_views.iter_mut() {
        let old_title = view.title.clone();
        let old_url = view.url.clone();
        let old_progress = view.progress;

        view.update();

        // Send state change events
        if view.title != old_title {
            if let Some(ref title) = view.title {
                let _ = self.comms.input_tx.send(InputEvent::WebKitTitleChanged {
                    id: *id,
                    title: title.clone(),
                });
            }
        }
        if view.url != old_url {
            let _ = self.comms.input_tx.send(InputEvent::WebKitUrlChanged {
                id: *id,
                url: view.url.clone(),
            });
        }
        if (view.progress - old_progress).abs() > 0.01 {
            let _ = self.comms.input_tx.send(InputEvent::WebKitProgressChanged {
                id: *id,
                progress: view.progress,
            });
        }
    }
```

**Step 3: Update non-webkit stub signature**

```rust
#[cfg(not(feature = "wpe-webkit"))]
fn pump_glib(&mut self) {}
```

**Step 4: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(webkit): update views and send state events in pump_glib"
```

---

### Task 7: Process WebKit Frames

**Files:**
- Modify: `rust/neomacs-display/src/render_thread.rs`

**Step 1: Add process_webkit_frames method**

```rust
#[cfg(feature = "wpe-webkit")]
fn process_webkit_frames(&mut self) {
    let device = match &self.device {
        Some(d) => d,
        None => return,
    };
    let queue = match &self.queue {
        Some(q) => q,
        None => return,
    };
    let cache = match &mut self.webkit_texture_cache {
        Some(c) => c,
        None => return,
    };

    for (view_id, view) in &self.webkit_views {
        // Try DMA-BUF first (zero-copy)
        if let Some(dmabuf) = view.take_latest_dmabuf() {
            use crate::backend::wgpu::external_buffer::DmaBufBuffer;

            let num_planes = dmabuf.fds.len().min(4) as u32;
            let mut fds = [-1i32; 4];
            let mut strides = [0u32; 4];
            let mut offsets = [0u32; 4];

            for i in 0..num_planes as usize {
                fds[i] = dmabuf.fds[i];
                strides[i] = dmabuf.strides[i];
                offsets[i] = dmabuf.offsets[i];
            }

            let buffer = DmaBufBuffer::new(
                fds,
                strides,
                offsets,
                num_planes,
                dmabuf.width,
                dmabuf.height,
                dmabuf.format,
                dmabuf.modifier,
            );

            if cache.update_view(*view_id, buffer, device, queue) {
                log::debug!("Imported DMA-BUF for webkit view {}", view_id);
            }
        }
        // Fallback to pixel upload
        else if let Some((width, height, pixels)) = view.take_latest_pixels() {
            if cache.update_view_from_pixels(*view_id, width, height, &pixels, device, queue) {
                log::debug!("Uploaded pixels for webkit view {}", view_id);
            }
        }
    }
}

#[cfg(not(feature = "wpe-webkit"))]
fn process_webkit_frames(&mut self) {}
```

**Step 2: Call in render method**

In the `render()` method, before drawing, add:

```rust
self.process_webkit_frames();
```

**Step 3: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/render_thread.rs
git commit -m "feat(webkit): process WebKit frames for texture import"
```

---

### Task 8: Update FFI to Use Commands

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Remove WEBKIT_CACHE thread-local**

Find and remove:
```rust
thread_local! {
    pub static WEBKIT_CACHE: RefCell<Option<WebKitViewCache>> = const { RefCell::new(None) };
    static WPE_BACKEND: RefCell<Option<WpeBackend>> = const { RefCell::new(None) };
}
```

**Step 2: Modify neomacs_display_webkit_create**

Replace implementation to send command:

```rust
#[no_mangle]
#[cfg(feature = "wpe-webkit")]
pub unsafe extern "C" fn neomacs_display_webkit_create(
    handle: *mut NeomacsDisplay,
    view_id: u32,
    width: u32,
    height: u32,
) -> i32 {
    if handle.is_null() {
        return -1;
    }
    let display = &*handle;
    display.send_command(RenderCommand::WebKitCreate {
        id: view_id,
        width,
        height,
    });
    0
}
```

**Step 3: Update other webkit FFI functions similarly**

- `neomacs_display_webkit_load_uri` → send `WebKitLoadUri` command
- `neomacs_display_webkit_resize` → send `WebKitResize` command
- `neomacs_display_webkit_destroy` → send `WebKitDestroy` command

**Step 4: Remove neomacs_display_webkit_init**

This is no longer needed - WpeBackend is initialized by render thread.

**Step 5: Verify build**

Run: `cargo build --features wpe-webkit -p neomacs-display`
Expected: Compiles without errors

**Step 6: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "feat(webkit): update FFI to send commands instead of direct manipulation"
```

---

### Task 9: Integration Test

**Step 1: Build full project**

```bash
cargo build --features wpe-webkit -p neomacs-display
```

**Step 2: Run tests**

```bash
cargo test --features wpe-webkit -p neomacs-display
```

**Step 3: Manual verification**

Start Emacs and verify:
- Window opens
- WebKit commands don't crash
- CPU idle when no activity

**Step 4: Final commit if any fixes needed**

```bash
git add -A
git commit -m "fix(webkit): integration fixes"
```
