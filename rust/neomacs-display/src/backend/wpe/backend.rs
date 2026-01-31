//! WPE Backend initialization and management.
//!
//! Handles the initialization of WPE with EGL and manages
//! the exportable view backend for frame capture.

use std::ptr;
use std::sync::Once;

use crate::core::error::{DisplayError, DisplayResult};

use super::sys;
use super::sys::fdo;

static WPE_INIT: Once = Once::new();
static mut WPE_INITIALIZED: bool = false;

/// WPE Backend manager.
///
/// Handles WPE initialization with EGL display and provides
/// factory methods for creating exportable view backends.
pub struct WpeBackend {
    /// EGL display used for WPE
    egl_display: *mut libc::c_void,
}

impl WpeBackend {
    /// Initialize WPE backend with an EGL display.
    ///
    /// This must be called before creating any WPE views.
    /// The EGL display should come from GTK4's GL context.
    ///
    /// # Safety
    /// The EGL display must be valid for the lifetime of this backend.
    pub unsafe fn new(egl_display: *mut libc::c_void) -> DisplayResult<Self> {
        if egl_display.is_null() {
            return Err(DisplayError::WebKit("EGL display is null".into()));
        }

        // Initialize WPE-FDO with the EGL display (once)
        WPE_INIT.call_once(|| {
            let result = fdo::wpe_fdo_initialize_for_egl_display(egl_display);
            WPE_INITIALIZED = result;
            if !result {
                log::error!("Failed to initialize WPE-FDO for EGL display");
            } else {
                log::info!("WPE-FDO initialized successfully");
            }
        });

        if !WPE_INITIALIZED {
            return Err(DisplayError::WebKit(
                "Failed to initialize WPE-FDO".into(),
            ));
        }

        Ok(Self { egl_display })
    }

    /// Check if WPE is initialized
    pub fn is_initialized(&self) -> bool {
        unsafe { WPE_INITIALIZED }
    }

    /// Get the EGL display
    pub fn egl_display(&self) -> *mut libc::c_void {
        self.egl_display
    }
}

impl Drop for WpeBackend {
    fn drop(&mut self) {
        // WPE-FDO doesn't have a cleanup function - it's process-lifetime
        log::debug!("WpeBackend dropped");
    }
}

/// Exportable view backend for capturing WebKit frames.
///
/// This wraps wpe_view_backend_exportable_fdo which provides
/// callbacks when WebKit renders a new frame as an EGLImage.
pub struct ExportableBackend {
    /// The exportable backend handle
    exportable: *mut fdo::wpe_view_backend_exportable_fdo,
    /// Width of the view
    width: u32,
    /// Height of the view
    height: u32,
    /// Callback data (must live as long as exportable)
    _callback_data: Box<ExportCallbackData>,
}

/// Data passed to WPE export callbacks
struct ExportCallbackData {
    /// Callback for when a new frame is ready
    on_frame: Option<Box<dyn Fn(ExportedImage) + Send>>,
}

/// An exported EGL image from WebKit
pub struct ExportedImage {
    /// The raw exported image handle
    pub(crate) image: *mut fdo::wpe_fdo_egl_exported_image,
    /// The exportable backend (for releasing)
    pub(crate) exportable: *mut fdo::wpe_view_backend_exportable_fdo,
}

impl ExportedImage {
    /// Get the EGLImage handle
    ///
    /// # Safety
    /// The returned pointer is only valid until `release()` is called.
    pub unsafe fn egl_image(&self) -> *mut libc::c_void {
        fdo::wpe_fdo_egl_exported_image_get_egl_image(self.image)
    }

    /// Get the image width
    pub fn width(&self) -> u32 {
        unsafe { fdo::wpe_fdo_egl_exported_image_get_width(self.image) }
    }

    /// Get the image height
    pub fn height(&self) -> u32 {
        unsafe { fdo::wpe_fdo_egl_exported_image_get_height(self.image) }
    }

    /// Release the exported image back to WPE.
    ///
    /// Must be called when done with the image to allow WebKit
    /// to reuse the buffer.
    pub fn release(self) {
        unsafe {
            fdo::wpe_view_backend_exportable_fdo_egl_dispatch_release_exported_image(
                self.exportable,
                self.image,
            );
        }
        // Don't run Drop, we've already released
        std::mem::forget(self);
    }
}

impl Drop for ExportedImage {
    fn drop(&mut self) {
        // Auto-release if not explicitly released
        unsafe {
            fdo::wpe_view_backend_exportable_fdo_egl_dispatch_release_exported_image(
                self.exportable,
                self.image,
            );
        }
    }
}

impl ExportableBackend {
    /// Create a new exportable backend.
    ///
    /// # Arguments
    /// * `width` - Initial width
    /// * `height` - Initial height
    /// * `on_frame` - Callback when a new frame is ready
    pub fn new<F>(width: u32, height: u32, on_frame: F) -> DisplayResult<Self>
    where
        F: Fn(ExportedImage) + Send + 'static,
    {
        let callback_data = Box::new(ExportCallbackData {
            on_frame: Some(Box::new(on_frame)),
        });

        let data_ptr = Box::into_raw(callback_data);

        // Create the EGL client callbacks
        let client = fdo::wpe_view_backend_exportable_fdo_egl_client {
            export_egl_image: None, // Deprecated, use export_fdo_egl_image
            export_fdo_egl_image: Some(export_egl_image_callback),
            export_shm_buffer: None, // We use EGL, not SHM
            _wpe_reserved0: None,
            _wpe_reserved1: None,
        };

        let exportable = unsafe {
            fdo::wpe_view_backend_exportable_fdo_egl_create(
                &client,
                data_ptr as *mut libc::c_void,
                width,
                height,
            )
        };

        if exportable.is_null() {
            // Recover the callback data
            let _ = unsafe { Box::from_raw(data_ptr) };
            return Err(DisplayError::WebKit(
                "Failed to create exportable backend".into(),
            ));
        }

        Ok(Self {
            exportable,
            width,
            height,
            _callback_data: unsafe { Box::from_raw(data_ptr) },
        })
    }

    /// Get the underlying wpe_view_backend for use with WebKit.
    pub fn view_backend(&self) -> *mut sys::wpe_view_backend {
        unsafe { fdo::wpe_view_backend_exportable_fdo_get_view_backend(self.exportable) }
    }

    /// Dispatch frame complete to WPE.
    ///
    /// Call this after processing the exported frame.
    pub fn dispatch_frame_complete(&self) {
        unsafe {
            fdo::wpe_view_backend_exportable_fdo_dispatch_frame_complete(self.exportable);
        }
    }

    /// Resize the backend
    pub fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
        // Note: Actual resize is handled by WebKit - we just track dimensions
    }
}

impl Drop for ExportableBackend {
    fn drop(&mut self) {
        if !self.exportable.is_null() {
            unsafe {
                fdo::wpe_view_backend_exportable_fdo_destroy(self.exportable);
            }
        }
    }
}

/// C callback for exported EGL images
unsafe extern "C" fn export_egl_image_callback(
    data: *mut libc::c_void,
    image: *mut fdo::wpe_fdo_egl_exported_image,
) {
    if data.is_null() || image.is_null() {
        return;
    }

    let callback_data = &*(data as *const ExportCallbackData);

    if let Some(ref on_frame) = callback_data.on_frame {
        // We need to get the exportable from somewhere...
        // This is tricky because the callback doesn't give us the exportable
        // TODO: Need to store exportable in callback_data
        log::warn!("Frame exported but exportable not available in callback");
    }
}
