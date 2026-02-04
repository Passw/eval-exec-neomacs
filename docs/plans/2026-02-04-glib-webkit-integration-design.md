# GLib/WebKit Integration Design

## Overview

Integrate GLib main context pumping and WebKit view management into the render thread architecture.

**Goals:**
- Pump GLib context on render thread for WebKit event dispatch
- Move WebKit view ownership from FFI layer to render thread
- Enable zero-copy DMA-BUF frame rendering
- Propagate WebKit state (title, URL, progress) to Emacs via events

## Architecture

### GLib Context Pumping

WebKit uses GLib's main loop for event dispatch. In headless mode, WPE attaches frame sources to the thread-default GLib context. The render thread pumps this context in `about_to_wait()` before each frame.

```rust
#[cfg(feature = "wpe-webkit")]
fn pump_glib(&mut self) {
    use crate::backend::wpe::sys as plat;

    unsafe {
        let thread_ctx = plat::g_main_context_get_thread_default();
        let ctx = if thread_ctx.is_null() {
            plat::g_main_context_default()
        } else {
            thread_ctx
        };

        while plat::g_main_context_iteration(ctx, 0) != 0 {}

        let default_ctx = plat::g_main_context_default();
        if default_ctx != ctx {
            while plat::g_main_context_iteration(default_ctx, 0) != 0 {}
        }
    }

    // Update all views
    for view in self.webkit_views.values_mut() {
        view.update();
    }
}
```

### WebKit Ownership

Render thread owns WebKit views:

```rust
struct RenderApp {
    // ... existing fields ...

    #[cfg(feature = "wpe-webkit")]
    wpe_backend: Option<WpeBackend>,

    #[cfg(feature = "wpe-webkit")]
    webkit_views: HashMap<u32, WpeWebView>,

    #[cfg(feature = "wpe-webkit")]
    webkit_texture_cache: WgpuWebKitCache,
}
```

### Command Flow

```
Emacs → neomacs_display_webkit_create()
      → NeomacsDisplay::send_command(WebKitCreate)
      → RenderCommand channel
      → RenderApp::process_commands()
      → WpeWebView::new()
```

### Frame Flow

```
WebKit renders → buffer-rendered signal
             → WpeWebView stores DMA-BUF data
             → pump_glib() calls view.update()
             → process_webkit_frames() imports to wgpu texture
             → render() draws texture at position
```

### State Updates (Option B)

Render thread sends events for state changes:

```rust
pub enum InputEvent {
    // ... existing variants ...
    WebKitTitleChanged { id: u32, title: String },
    WebKitUrlChanged { id: u32, url: String },
    WebKitProgressChanged { id: u32, progress: f64 },
    WebKitLoadFinished { id: u32 },
}
```

Emacs caches these values for query functions.

## Files Changed

| File | Change |
|------|--------|
| `render_thread.rs` | Add webkit fields, implement pump_glib, process frames |
| `ffi.rs` | Remove WEBKIT_CACHE, modify webkit FFI to send commands |
| `thread_comm.rs` | Add WebKit InputEvent variants |
| `src/neomacsterm.c` | Cache webkit state from events |

## Testing

1. Build with `cargo build --features wpe-webkit`
2. Create WebKit view, load URL
3. Verify frame renders
4. Verify state updates propagate
5. Verify CPU idle when no activity
