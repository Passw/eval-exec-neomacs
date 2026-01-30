//! C FFI layer for integration with Emacs.

use std::ffi::{c_char, c_int, c_void, CStr};
use std::ptr;

use crate::backend::{BackendType, DisplayBackend};
use crate::backend::gtk4::Gtk4Backend;
use crate::backend::tty::TtyBackend;
use crate::core::types::{Color, Rect};
use crate::core::scene::{Scene, WindowScene, CursorState, CursorStyle};
use crate::core::animation::AnimationManager;

/// Opaque handle to the display engine
pub struct NeomacsDisplay {
    backend_type: BackendType,
    gtk4_backend: Option<Gtk4Backend>,
    tty_backend: Option<TtyBackend>,
    scene: Scene,
    animations: AnimationManager,
}

impl NeomacsDisplay {
    fn get_backend(&mut self) -> Option<&mut dyn DisplayBackend> {
        match self.backend_type {
            BackendType::Gtk4 => self.gtk4_backend.as_mut().map(|b| b as &mut dyn DisplayBackend),
            BackendType::Tty => self.tty_backend.as_mut().map(|b| b as &mut dyn DisplayBackend),
        }
    }
}

// ============================================================================
// Initialization
// ============================================================================

/// Initialize the display engine
/// 
/// # Safety
/// Returns a pointer to NeomacsDisplay that must be freed with neomacs_display_shutdown.
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_init(backend: BackendType) -> *mut NeomacsDisplay {
    let mut display = Box::new(NeomacsDisplay {
        backend_type: backend,
        gtk4_backend: None,
        tty_backend: None,
        scene: Scene::new(800.0, 600.0),
        animations: AnimationManager::new(),
    });

    // Create the backend
    match backend {
        BackendType::Gtk4 => {
            let mut gtk4 = Gtk4Backend::new();
            if let Err(e) = gtk4.init() {
                eprintln!("Failed to initialize GTK4 backend: {}", e);
                return ptr::null_mut();
            }
            display.gtk4_backend = Some(gtk4);
        }
        BackendType::Tty => {
            let mut tty = TtyBackend::new();
            if let Err(e) = tty.init() {
                eprintln!("Failed to initialize TTY backend: {}", e);
                return ptr::null_mut();
            }
            display.tty_backend = Some(tty);
        }
    }

    Box::into_raw(display)
}

/// Shutdown the display engine
/// 
/// # Safety
/// The handle must have been returned by neomacs_display_init.
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_shutdown(handle: *mut NeomacsDisplay) {
    if handle.is_null() {
        return;
    }

    let mut display = Box::from_raw(handle);
    
    if let Some(backend) = display.get_backend() {
        backend.shutdown();
    }
    
    // display is dropped here
}

// ============================================================================
// Scene Management
// ============================================================================

/// Resize the display
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_resize(
    handle: *mut NeomacsDisplay,
    width: c_int,
    height: c_int,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.scene = Scene::new(width as f32, height as f32);
    
    if let Some(backend) = display.get_backend() {
        backend.resize(width as u32, height as u32);
    }
}

/// Begin building a new frame
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_begin_frame(handle: *mut NeomacsDisplay) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.scene.windows.clear();
    display.scene.mark_dirty();
}

/// Add a window to the current frame
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_add_window(
    handle: *mut NeomacsDisplay,
    window_id: c_int,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    bg_color: u32,
    selected: c_int,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    
    let window = WindowScene {
        window_id,
        bounds: Rect::new(x, y, width, height),
        background: Color::from_pixel(bg_color),
        rows: Vec::new(),
        cursor: None,
        scroll_offset: 0.0,
        selected: selected != 0,
        mode_line_height: 0,
        header_line_height: 0,
    };
    
    display.scene.windows.push(window);
}

/// Set cursor for the most recently added window
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_set_cursor(
    handle: *mut NeomacsDisplay,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    style: c_int,
    color: u32,
    visible: c_int,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    
    if let Some(window) = display.scene.windows.last_mut() {
        window.cursor = Some(CursorState {
            x,
            y,
            width,
            height,
            style: match style {
                0 => CursorStyle::Box,
                1 => CursorStyle::Bar,
                2 => CursorStyle::Underline,
                3 => CursorStyle::Hollow,
                _ => CursorStyle::Box,
            },
            color: Color::from_pixel(color),
            visible: visible != 0 && display.animations.cursor_visible(),
        });
    }
}

/// End frame and render
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_end_frame(handle: *mut NeomacsDisplay) -> c_int {
    if handle.is_null() {
        return -1;
    }

    let display = &mut *handle;
    
    // Update animations
    display.animations.tick();
    
    // Build scene graph
    display.scene.build();
    
    // Render - we need to match backend type explicitly to avoid borrow conflict
    let result = match display.backend_type {
        BackendType::Gtk4 => {
            if let Some(backend) = display.gtk4_backend.as_mut() {
                backend.render(&display.scene)
                    .and_then(|_| backend.present())
            } else {
                Ok(())
            }
        }
        BackendType::Tty => {
            if let Some(backend) = display.tty_backend.as_mut() {
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
    
    0
}

// ============================================================================
// Animation
// ============================================================================

/// Start smooth scroll animation
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_smooth_scroll(
    handle: *mut NeomacsDisplay,
    window_id: c_int,
    from_offset: f32,
    to_offset: f32,
) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.animations.animate_scroll(window_id, from_offset, to_offset);
}

/// Reset cursor blink (call when cursor moves)
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_reset_cursor_blink(handle: *mut NeomacsDisplay) {
    if handle.is_null() {
        return;
    }

    let display = &mut *handle;
    display.animations.reset_cursor_blink();
}

/// Check if animations are active
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_has_animations(handle: *mut NeomacsDisplay) -> c_int {
    if handle.is_null() {
        return 0;
    }

    let display = &mut *handle;
    display.animations.has_active_animations() as c_int
}

// ============================================================================
// Backend Info
// ============================================================================

/// Get backend name
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_backend_name(handle: *mut NeomacsDisplay) -> *const c_char {
    if handle.is_null() {
        return b"null\0".as_ptr() as *const c_char;
    }

    let display = &mut *handle;
    
    match display.get_backend() {
        Some(backend) => backend.name().as_ptr() as *const c_char,
        None => b"none\0".as_ptr() as *const c_char,
    }
}

/// Check if backend is initialized
#[no_mangle]
pub unsafe extern "C" fn neomacs_display_is_initialized(handle: *mut NeomacsDisplay) -> c_int {
    if handle.is_null() {
        return 0;
    }

    let display = &mut *handle;
    
    match display.get_backend() {
        Some(backend) => backend.is_initialized() as c_int,
        None => 0,
    }
}
