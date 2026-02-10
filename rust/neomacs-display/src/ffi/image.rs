//! Image Management FFI functions
//!
//! Video/image glyph stubs and image loading functions.

use super::*;

// ============================================================================
// Image Management (stubs - no GTK4 backend)
// ============================================================================

/// Add a video glyph to the current row
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_add_video_glyph(
    handle: *mut NeomacsDisplay,
    video_id: u32,
    pixel_width: c_int,
    pixel_height: c_int,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    let current_y = display.current_row_y;  // Frame-absolute Y
    let current_x = display.current_row_x;

    display.frame_glyphs.add_video(
        video_id,
        current_x as f32,
        current_y as f32,
        pixel_width as f32,
        pixel_height as f32,
    );
    display.current_row_x += pixel_width;
}

/// Load a video from file path (async - uses GStreamer)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_video(
    handle: *mut NeomacsDisplay,
    path: *const c_char,
) -> u32 {
    let display = match handle.as_mut() {
        Some(d) => d,
        None => return 0,
    };

    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    log::info!("load_video: path={}", path_str);

    // Threaded path: send command to render thread
    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref state) = THREADED_STATE {
        let id = VIDEO_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let cmd = RenderCommand::VideoCreate {
            id,
            path: path_str.to_string(),
        };
        let _ = state.emacs_comms.cmd_tx.try_send(cmd);
        log::info!("load_video: threaded path, id={}", id);
        return id;
    }

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            let id = renderer.load_video_file(path_str);
            log::info!("load_video: returned id={}", id);
            return id;
        }
    }

    0
}

/// Play a loaded video
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_video_play(
    handle: *mut NeomacsDisplay,
    video_id: u32,
) -> c_int {
    // Threaded path
    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref state) = THREADED_STATE {
        let cmd = RenderCommand::VideoPlay { id: video_id };
        let _ = state.emacs_comms.cmd_tx.try_send(cmd);
        return 0;
    }

    let display = match handle.as_mut() {
        Some(d) => d,
        None => return -1,
    };

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            renderer.video_play(video_id);
            return 0;
        }
    }

    -1
}

/// Pause a video
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_video_pause(
    handle: *mut NeomacsDisplay,
    video_id: u32,
) -> c_int {
    // Threaded path
    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref state) = THREADED_STATE {
        let cmd = RenderCommand::VideoPause { id: video_id };
        let _ = state.emacs_comms.cmd_tx.try_send(cmd);
        return 0;
    }

    let display = match handle.as_mut() {
        Some(d) => d,
        None => return -1,
    };

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            renderer.video_pause(video_id);
            return 0;
        }
    }

    -1
}

/// Stop a video
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_video_stop(
    handle: *mut NeomacsDisplay,
    video_id: u32,
) -> c_int {
    // Threaded path: stop maps to destroy
    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref state) = THREADED_STATE {
        let cmd = RenderCommand::VideoDestroy { id: video_id };
        let _ = state.emacs_comms.cmd_tx.try_send(cmd);
        return 0;
    }

    let display = match handle.as_mut() {
        Some(d) => d,
        None => return -1,
    };

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            renderer.video_stop(video_id);
            return 0;
        }
    }

    -1
}

/// Set video loop mode (-1 for infinite)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_video_set_loop(
    handle: *mut NeomacsDisplay,
    video_id: u32,
    loop_count: c_int,
) -> c_int {
    let display = match handle.as_mut() {
        Some(d) => d,
        None => return -1,
    };

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            renderer.video_set_loop(video_id, loop_count);
            return 0;
        }
    }

    -1
}

/// Process pending video frames (call each frame)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_video_update(
    handle: *mut NeomacsDisplay,
    _video_id: u32,
) -> c_int {
    let display = match handle.as_mut() {
        Some(d) => d,
        None => return -1,
    };

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            renderer.process_pending_videos();
            return 0;
        }
    }

    -1
}

/// Get video dimensions (works for pending and loaded videos)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_get_video_size(
    handle: *mut NeomacsDisplay,
    video_id: u32,
    width: *mut c_int,
    height: *mut c_int,
) -> c_int {
    if handle.is_null() || width.is_null() || height.is_null() {
        return -1;
    }
    let display = &mut *handle;

    #[cfg(all(feature = "winit-backend", feature = "video"))]
    if let Some(ref backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer() {
            if let Some((w, h)) = renderer.get_video_size(video_id) {
                *width = w as c_int;
                *height = h as c_int;
                return 0;
            }
        }
    }

    -1
}

// ============================================================================
// Image Functions (stubs - no GTK4 backend)
// ============================================================================

/// Load an image from a file path (delegates to load_image_file)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image(
    handle: *mut NeomacsDisplay,
    path: *const c_char,
) -> u32 {
    neomacs_display_load_image_file(handle, path)
}

/// Load an image from raw bytes (encoded image format)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_data(
    handle: *mut NeomacsDisplay,
    data: *const u8,
    len: usize,
) -> u32 {
    if handle.is_null() || data.is_null() || len == 0 {
        return 0;
    }
    let display = &mut *handle;

    let data_slice = std::slice::from_raw_parts(data, len);

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            return renderer.load_image_data(data_slice, 0, 0);
        }
    }
    0
}

/// Load an image from raw bytes with optional scaling
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_data_scaled(
    handle: *mut NeomacsDisplay,
    data: *const u8,
    len: usize,
    max_width: c_int,
    max_height: c_int,
) -> u32 {
    if handle.is_null() || data.is_null() || len == 0 {
        return 0;
    }
    let display = &mut *handle;

    let data_slice = std::slice::from_raw_parts(data, len);

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            return renderer.load_image_data(
                data_slice,
                max_width.max(0) as u32,
                max_height.max(0) as u32,
            );
        }
    }
    0
}

/// Load an image from raw ARGB32 pixel data
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_argb32(
    handle: *mut NeomacsDisplay,
    data: *const u8,
    width: c_int,
    height: c_int,
    stride: c_int,
) -> u32 {
    if handle.is_null() || data.is_null() || width <= 0 || height <= 0 || stride <= 0 {
        return 0;
    }
    let display = &mut *handle;

    // Use checked multiplication to prevent overflow
    let data_len = match (stride as usize).checked_mul(height as usize) {
        Some(len) => len,
        None => return 0, // Overflow would occur
    };
    let data_slice = std::slice::from_raw_parts(data, data_len);

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            return renderer.load_image_argb32(
                data_slice,
                width as u32,
                height as u32,
                stride as u32,
            );
        }
    }
    0
}

/// Load an image from raw RGB24 pixel data
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_rgb24(
    handle: *mut NeomacsDisplay,
    data: *const u8,
    width: c_int,
    height: c_int,
    stride: c_int,
) -> u32 {
    if handle.is_null() || data.is_null() || width <= 0 || height <= 0 || stride <= 0 {
        return 0;
    }
    let display = &mut *handle;

    // Use checked multiplication to prevent overflow
    let data_len = match (stride as usize).checked_mul(height as usize) {
        Some(len) => len,
        None => return 0, // Overflow would occur
    };
    let data_slice = std::slice::from_raw_parts(data, data_len);

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            return renderer.load_image_rgb24(
                data_slice,
                width as u32,
                height as u32,
                stride as u32,
            );
        }
    }
    0
}

/// Load an image from a file path (async - returns ID immediately)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_file(
    handle: *mut NeomacsDisplay,
    path: *const c_char,
) -> u32 {
    neomacs_display_load_image_file_scaled(handle, path, 0, 0)
}

/// Load an image from a file path with scaling (async)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_file_scaled(
    handle: *mut NeomacsDisplay,
    path: *const c_char,
    max_width: c_int,
    max_height: c_int,
) -> u32 {
    if handle.is_null() || path.is_null() {
        return 0;
    }
    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    log::info!("load_image_file_scaled: path={}, max={}x{}", path_str, max_width, max_height);

    // Threaded path: send command to render thread
    #[cfg(feature = "winit-backend")]
    if let Some(ref state) = THREADED_STATE {
        let id = IMAGE_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let cmd = RenderCommand::ImageLoadFile {
            id,
            path: path_str.to_string(),
            max_width: max_width.max(0) as u32,
            max_height: max_height.max(0) as u32,
        };
        let _ = state.emacs_comms.cmd_tx.try_send(cmd);
        log::info!("load_image_file_scaled: threaded path, id={}", id);
        return id;
    }

    // Non-threaded path: direct renderer access
    let display = &mut *handle;
    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            let id = renderer.load_image_file(
                path_str,
                max_width.max(0) as u32,
                max_height.max(0) as u32,
            );
            log::info!("load_image_file_scaled: returned id={}", id);
            return id;
        }
    }
    0
}

/// Load an image directly as texture (same as load_image_file)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_file_direct(
    handle: *mut NeomacsDisplay,
    path: *const c_char,
) -> u32 {
    neomacs_display_load_image_file(handle, path)
}

/// Load an image directly as texture with scaling
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_load_image_file_direct_scaled(
    handle: *mut NeomacsDisplay,
    path: *const c_char,
    max_width: c_int,
    max_height: c_int,
) -> u32 {
    neomacs_display_load_image_file_scaled(handle, path, max_width, max_height)
}

/// Get image dimensions (works for pending and loaded images)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_get_image_size(
    handle: *mut NeomacsDisplay,
    image_id: u32,
    width: *mut c_int,
    height: *mut c_int,
) -> c_int {
    if width.is_null() || height.is_null() {
        return -1;
    }

    // Threaded path: check shared map
    #[cfg(feature = "winit-backend")]
    if let Some(ref state) = THREADED_STATE {
        if let Ok(dims) = state.image_dimensions.lock() {
            if let Some(&(w, h)) = dims.get(&image_id) {
                *width = w as c_int;
                *height = h as c_int;
                return 0;
            }
        }
        // Not ready yet - return 0,0 so Emacs can retry on next redisplay
        *width = 0;
        *height = 0;
        return -1;
    }

    // Non-threaded path: direct renderer access
    if handle.is_null() {
        return -1;
    }
    let display = &mut *handle;

    #[cfg(feature = "winit-backend")]
    if let Some(ref backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer() {
            if let Some((w, h)) = renderer.get_image_size(image_id) {
                *width = w as c_int;
                *height = h as c_int;
                return 0;
            }
        }
    }
    -1
}

/// Query image file dimensions without loading (fast - reads header only)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_query_image_file_size(
    _handle: *mut NeomacsDisplay,
    path: *const c_char,
    width: *mut c_int,
    height: *mut c_int,
) -> c_int {
    if path.is_null() || width.is_null() || height.is_null() {
        return -1;
    }
    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    #[cfg(feature = "winit-backend")]
    {
        use crate::backend::wgpu::WgpuRenderer;
        if let Some((w, h)) = WgpuRenderer::query_image_file_size(path_str) {
            *width = w as c_int;
            *height = h as c_int;
            return 0;
        }
    }
    -1
}

/// Free an image from cache
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_free_image(
    handle: *mut NeomacsDisplay,
    image_id: u32,
) -> c_int {
    // Threaded path: send command to render thread
    #[cfg(feature = "winit-backend")]
    if let Some(ref state) = THREADED_STATE {
        let cmd = RenderCommand::ImageFree { id: image_id };
        let _ = state.emacs_comms.cmd_tx.try_send(cmd);
        return 0;
    }

    if handle.is_null() {
        return -1;
    }
    let display = &mut *handle;

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(renderer) = backend.renderer_mut() {
            renderer.free_image(image_id);
            return 0;
        }
    }
    -1
}

/// Set a floating video at a specific screen position
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_set_floating_video(
    handle: *mut NeomacsDisplay,
    video_id: u32,
    x: c_int,
    y: c_int,
    width: c_int,
    height: c_int,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;

    // Remove existing floating video for this ID
    let target_scene = display.get_target_scene();
    target_scene.remove_floating_video(video_id);

    // Add new floating video
    target_scene.add_floating_video(
        video_id,
        x as f32,
        y as f32,
        width as f32,
        height as f32,
    );
}

/// Remove a floating video
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_floating_video(
    handle: *mut NeomacsDisplay,
    video_id: u32,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.get_target_scene().remove_floating_video(video_id);
}

/// Set a floating image at a specific screen position
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_set_floating_image(
    handle: *mut NeomacsDisplay,
    image_id: u32,
    x: c_int,
    y: c_int,
    width: c_int,
    height: c_int,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;

    // Remove existing floating image for this ID
    let target_scene = display.get_target_scene();
    target_scene.remove_floating_image(image_id);

    // Add new floating image
    target_scene.add_floating_image(
        image_id,
        x as f32,
        y as f32,
        width as f32,
        height as f32,
    );
}

/// Remove a floating image
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_floating_image(
    handle: *mut NeomacsDisplay,
    image_id: u32,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.get_target_scene().remove_floating_image(image_id);
}

/// Clear a rectangular area of the display.
/// No-op with full-frame rebuild (buffer is rebuilt from scratch each frame).
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_area(
    _handle: *mut NeomacsDisplay,
    _x: c_int,
    _y: c_int,
    _width: c_int,
    _height: c_int,
) {
    // No-op: with full-frame rebuild, the buffer is cleared and rebuilt each frame.
}

/// Clear only media glyphs (Image, Video, WebKit) in a rectangular area.
/// No-op with full-frame rebuild (buffer is rebuilt from scratch each frame).
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_media_in_area(
    _handle: *mut NeomacsDisplay,
    _x: c_int,
    _y: c_int,
    _width: c_int,
    _height: c_int,
) {
    // No-op: with full-frame rebuild, the buffer is cleared and rebuilt each frame.
}

/// Clear all glyphs - used when frame layout changes
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_all_glyphs(handle: *mut NeomacsDisplay) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    log::info!("neomacs_display_clear_all_glyphs: clearing {} glyphs", display.frame_glyphs.glyphs.len());
    display.frame_glyphs.glyphs.clear();
    display.frame_glyphs.window_regions.clear();
    display.frame_glyphs.prev_window_regions.clear();
}

/// Clear all cursors
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_all_cursors(handle: *mut NeomacsDisplay) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.frame_glyphs.glyphs.retain(|g| !matches!(g, FrameGlyph::Cursor { .. }));
}

/// Clear all borders (window dividers)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_clear_all_borders(handle: *mut NeomacsDisplay) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.frame_glyphs.glyphs.retain(|g| !matches!(g, FrameGlyph::Border { .. }));
}

/// End frame and render
/// Returns 0 on success, 1 if layout changed, -1 on error
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_end_frame(handle: *mut NeomacsDisplay) -> c_int {
    if handle.is_null() {
        return -1;
    }

    let display = &mut *handle;
    let current_frame = display.frame_counter;

    // Reset frame flag
    display.in_frame = false;

    debug!("end_frame: frame={}, glyphs={}, regions={}",
           current_frame, display.frame_glyphs.len(), display.frame_glyphs.window_regions.len());

    // End frame - this handles layout change detection and stale glyph removal
    let mut layout_cleared = false;
    if display.use_hybrid {
        layout_cleared = display.frame_glyphs.end_frame();
        debug!("After end_frame: {} glyphs, cleared={}", display.frame_glyphs.len(), layout_cleared);
    }

    // Build scene if it has content (legacy scene graph path)
    if !display.scene.windows.is_empty() {
        display.scene.build();
    }

    // Update animations
    display.animations.tick();

    // Render - we need to match backend type explicitly to avoid borrow conflict
    let result = match display.backend_type {
        BackendType::Tty => {
            if let Some(backend) = display.tty_backend.as_mut() {
                backend.render(&display.scene)
                    .and_then(|_| backend.present())
            } else {
                Ok(())
            }
        }
        #[cfg(feature = "winit-backend")]
        BackendType::Wgpu => {
            if let Some(backend) = display.winit_backend.as_mut() {
                backend.render(&display.scene)
                    .and_then(|_| backend.present())
            } else {
                Ok(())
            }
        }
    };

    if let Err(e) = result {
        eprintln!("Render error: {}", e);
        return -1;
    }

    display.scene.clear_dirty();

    if layout_cleared { 1 } else { 0 }
}
