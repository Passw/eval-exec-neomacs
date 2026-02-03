# Winit Rendering Pipeline Connection Design

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Connect Emacs redisplay to render content in winit/wgpu windows.

**Architecture:** Share wgpu device/queue between renderer and surfaces. Track current render window to route drawing calls to correct scene.

**Tech Stack:** wgpu, winit, Rust FFI

---

## 1. WgpuRenderer Changes

Add a new constructor that accepts an existing device and queue:

```rust
impl WgpuRenderer {
    /// Create a renderer with an existing device and queue.
    /// This allows sharing GPU resources across multiple surfaces.
    pub fn with_device(
        device: Arc<wgpu::Device>,
        queue: Arc<wgpu::Queue>,
        width: u32,
        height: u32,
    ) -> Self {
        // Create shader module, pipelines, uniform buffer
        // using provided device (skip instance/adapter creation)
    }
}
```

**Changes:**
- Extract pipeline/buffer creation into helper method
- Add `with_device()` that uses the helper
- Refactor `new_async()` to also use the helper

---

## 2. init_wgpu_headless Changes

Create the shared renderer after device/queue creation:

```rust
pub fn init_wgpu_headless(&mut self) -> DisplayResult<()> {
    // ... existing instance, adapter, device, queue creation ...

    // Create the shared renderer with the device
    let renderer = WgpuRenderer::with_device(
        device.clone(),
        queue.clone(),
        self.width,
        self.height,
    );

    self.instance = Some(instance);
    self.device = Some(device);
    self.queue = Some(queue);
    self.renderer = Some(renderer);  // Store renderer
    self.wgpu_initialized = true;
    self.initialized = true;

    Ok(())
}
```

---

## 3. C Code Routing

Route winit windows through window-targeted rendering path.

**In neomacs_update_begin() (neomacsterm.c):**
```c
struct frame *f = SELECTED_FRAME();
if (FRAME_NEOMACS_P(f) && FRAME_OUTPUT_DATA(f)->window_id > 0)
  neomacs_display_begin_frame_window(dpyinfo->display_handle,
                                      FRAME_OUTPUT_DATA(f)->window_id);
else
  neomacs_display_begin_frame(dpyinfo->display_handle);
```

**In neomacs_update_end() (neomacsterm.c):**
```c
if (FRAME_NEOMACS_P(f) && FRAME_OUTPUT_DATA(f)->window_id > 0)
  neomacs_display_end_frame_window(dpyinfo->display_handle,
                                    FRAME_OUTPUT_DATA(f)->window_id);
else
  result = neomacs_display_end_frame(dpyinfo->display_handle);
```

---

## 4. Current Window Tracking

Track which window is being drawn to, routing drawing calls appropriately.

**In NeomacsDisplay struct (ffi.rs):**
```rust
pub struct NeomacsDisplay {
    // ... existing fields ...
    current_render_window_id: u32,  // 0 = legacy, >0 = winit window
}
```

**In begin_frame_window:**
```rust
pub fn neomacs_display_begin_frame_window(handle, window_id) {
    display.current_render_window_id = window_id;
    // clear window's scene
}
```

**In end_frame_window:**
```rust
pub fn neomacs_display_end_frame_window(handle, window_id) {
    // render and present
    display.current_render_window_id = 0;  // Reset
}
```

**Helper for drawing functions:**
```rust
fn get_target_scene(display: &mut NeomacsDisplay) -> &mut Scene {
    if display.current_render_window_id > 0 {
        display.winit_backend.as_mut()
            .and_then(|b| b.get_scene_mut(display.current_render_window_id))
            .unwrap_or(&mut display.scene)
    } else {
        &mut display.scene
    }
}
```

---

## 5. Implementation Summary

| Component | Change |
|-----------|--------|
| `WgpuRenderer` | Add `with_device()` constructor |
| `init_wgpu_headless()` | Create shared renderer with device |
| `NeomacsDisplay` | Add `current_render_window_id` field |
| `begin_frame_window` | Set current_render_window_id |
| `end_frame_window` | Reset current_render_window_id |
| Drawing functions | Route to window's scene via helper |
| `neomacsterm.c` | Call window-targeted begin/end_frame |

---

## 6. Testing

**Steps:**
1. Build and run `./src/emacs -Q`
2. Window should show Emacs content (scratch buffer, modeline)
3. Verify text is visible and readable
4. Test resize - content should re-render correctly

**Success criteria:**
- Emacs window displays text content
- No crashes or rendering errors
- Basic editing works (cursor visible, text appears)
