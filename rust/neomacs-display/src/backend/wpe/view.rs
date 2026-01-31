//! WPE WebKit view wrapper.
//!
//! Wraps a WebKitWebView with an exportable backend for
//! capturing rendered frames as GdkTextures.

use std::ffi::{CStr, CString};
use std::ptr;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};

use gdk4::prelude::*;
use gdk4::Texture;

use crate::core::error::{DisplayError, DisplayResult};

use super::sys;
use super::sys::fdo;
use super::sys::webkit as wk;
use super::backend::WpeBackend;
use super::dmabuf::DmaBufExporter;

/// State of a WPE WebKit view
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WpeViewState {
    /// View is being created
    Creating,
    /// View is loading content
    Loading,
    /// View is ready/idle
    Ready,
    /// View encountered an error
    Error,
}

/// Frame data received from WPE export callback
struct ExportedFrame {
    egl_image: *mut libc::c_void,
    width: u32,
    height: u32,
    image_handle: *mut fdo::wpe_fdo_egl_exported_image,
}

/// Callback data stored with the exportable backend
struct WpeCallbackData {
    /// The exportable backend pointer (needed for releasing images)
    exportable: *mut fdo::wpe_view_backend_exportable_fdo,
    /// Latest exported frame
    latest_frame: RefCell<Option<ExportedFrame>>,
    /// Flag indicating new frame available
    frame_available: AtomicBool,
}

/// A WPE WebKit browser view.
///
/// Unlike WebKitGTK, WPE renders to textures that can be composited
/// without needing a GTK widget hierarchy.
pub struct WpeWebView {
    /// Current URL
    pub url: String,

    /// View state
    pub state: WpeViewState,

    /// View dimensions
    pub width: u32,
    pub height: u32,

    /// Page title
    pub title: Option<String>,

    /// Loading progress (0.0 - 1.0)
    pub progress: f64,

    /// Latest rendered texture
    texture: Option<Texture>,

    /// The exportable backend
    exportable: *mut fdo::wpe_view_backend_exportable_fdo,
    
    /// The WebKit web view
    web_view: *mut wk::WebKitWebView,
    
    /// The WebKit view backend wrapper
    view_backend: *mut wk::WebKitWebViewBackend,
    
    /// Callback data (must be boxed and leaked to be stable)
    callback_data: *mut WpeCallbackData,

    /// DMA-BUF exporter for texture conversion
    dmabuf_exporter: DmaBufExporter,

    /// GDK display for texture creation
    gdk_display: Option<gdk4::Display>,

    /// Whether the view needs redraw
    needs_redraw: bool,
}

impl WpeWebView {
    /// Create a new WPE WebKit view.
    ///
    /// # Arguments
    /// * `wpe_backend` - The initialized WPE backend
    /// * `width` - Initial width
    /// * `height` - Initial height
    pub fn new(wpe_backend: &WpeBackend, width: u32, height: u32) -> DisplayResult<Self> {
        if !wpe_backend.is_initialized() {
            return Err(DisplayError::WebKit("WPE backend not initialized".into()));
        }

        // Create DMA-BUF exporter with the EGL display
        let dmabuf_exporter = DmaBufExporter::new(wpe_backend.egl_display());
        
        // Get the GDK display
        let gdk_display = gdk4::Display::default();

        unsafe {
            // Allocate callback data
            let callback_data = Box::into_raw(Box::new(WpeCallbackData {
                exportable: ptr::null_mut(),
                latest_frame: RefCell::new(None),
                frame_available: AtomicBool::new(false),
            }));

            // Create the EGL client callbacks
            let client = fdo::wpe_view_backend_exportable_fdo_egl_client {
                export_egl_image: None, // Deprecated
                export_fdo_egl_image: Some(export_egl_image_callback),
                export_shm_buffer: None,
                _wpe_reserved0: None,
                _wpe_reserved1: None,
            };

            // Create exportable backend
            let exportable = fdo::wpe_view_backend_exportable_fdo_egl_create(
                &client,
                callback_data as *mut libc::c_void,
                width,
                height,
            );

            if exportable.is_null() {
                let _ = Box::from_raw(callback_data);
                return Err(DisplayError::WebKit("Failed to create exportable backend".into()));
            }

            // Store exportable in callback data
            (*callback_data).exportable = exportable;

            // Get the raw wpe_view_backend
            let wpe_backend_ptr = fdo::wpe_view_backend_exportable_fdo_get_view_backend(exportable);
            
            if wpe_backend_ptr.is_null() {
                fdo::wpe_view_backend_exportable_fdo_destroy(exportable);
                let _ = Box::from_raw(callback_data);
                return Err(DisplayError::WebKit("Failed to get view backend".into()));
            }

            // Create WebKitWebViewBackend wrapper
            let view_backend = wk::webkit_web_view_backend_new(
                wpe_backend_ptr,
                None, // No destroy notify - we manage lifetime
                ptr::null_mut(),
            );

            if view_backend.is_null() {
                fdo::wpe_view_backend_exportable_fdo_destroy(exportable);
                let _ = Box::from_raw(callback_data);
                return Err(DisplayError::WebKit("Failed to create WebKitWebViewBackend".into()));
            }

            // Create WebKitWebView
            let web_view = wk::webkit_web_view_new(view_backend);

            if web_view.is_null() {
                // view_backend is owned by web_view on success, but we need to clean up on failure
                fdo::wpe_view_backend_exportable_fdo_destroy(exportable);
                let _ = Box::from_raw(callback_data);
                return Err(DisplayError::WebKit("Failed to create WebKitWebView".into()));
            }

            log::info!("WPE WebKitWebView created successfully ({}x{})", width, height);

            Ok(Self {
                url: String::new(),
                state: WpeViewState::Ready,
                width,
                height,
                title: None,
                progress: 0.0,
                texture: None,
                exportable,
                web_view,
                view_backend,
                callback_data,
                dmabuf_exporter,
                gdk_display,
                needs_redraw: false,
            })
        }
    }

    /// Load a URL
    pub fn load_uri(&mut self, uri: &str) -> DisplayResult<()> {
        self.url = uri.to_string();
        self.state = WpeViewState::Loading;
        self.progress = 0.0;

        let c_uri = CString::new(uri).map_err(|_| DisplayError::WebKit("Invalid URI".into()))?;
        
        unsafe {
            wk::webkit_web_view_load_uri(self.web_view, c_uri.as_ptr());
        }

        log::info!("WPE: Loading URI: {}", uri);
        Ok(())
    }

    /// Load HTML content directly
    pub fn load_html(&mut self, html: &str, base_uri: Option<&str>) -> DisplayResult<()> {
        self.state = WpeViewState::Loading;
        self.progress = 0.0;

        let c_html = CString::new(html).map_err(|_| DisplayError::WebKit("Invalid HTML".into()))?;
        let c_base_uri = base_uri
            .map(|u| CString::new(u).ok())
            .flatten();

        unsafe {
            wk::webkit_web_view_load_html(
                self.web_view,
                c_html.as_ptr(),
                c_base_uri.as_ref().map(|s| s.as_ptr()).unwrap_or(ptr::null()),
            );
        }

        log::info!("WPE: Loading HTML content");
        Ok(())
    }

    /// Navigate back
    pub fn go_back(&mut self) -> DisplayResult<()> {
        unsafe {
            if wk::webkit_web_view_can_go_back(self.web_view) != 0 {
                wk::webkit_web_view_go_back(self.web_view);
            }
        }
        Ok(())
    }

    /// Navigate forward
    pub fn go_forward(&mut self) -> DisplayResult<()> {
        unsafe {
            if wk::webkit_web_view_can_go_forward(self.web_view) != 0 {
                wk::webkit_web_view_go_forward(self.web_view);
            }
        }
        Ok(())
    }

    /// Reload the page
    pub fn reload(&mut self) -> DisplayResult<()> {
        self.state = WpeViewState::Loading;
        unsafe {
            wk::webkit_web_view_reload(self.web_view);
        }
        Ok(())
    }

    /// Stop loading
    pub fn stop(&mut self) -> DisplayResult<()> {
        unsafe {
            wk::webkit_web_view_stop_loading(self.web_view);
        }
        Ok(())
    }

    /// Execute JavaScript
    pub fn execute_javascript(&self, script: &str) -> DisplayResult<()> {
        let c_script = CString::new(script).map_err(|_| DisplayError::WebKit("Invalid script".into()))?;
        
        unsafe {
            wk::webkit_web_view_evaluate_javascript(
                self.web_view,
                c_script.as_ptr(),
                -1, // length, -1 for null-terminated
                ptr::null(), // world_name
                ptr::null(), // source_uri
                ptr::null_mut(), // cancellable
                None, // callback
                ptr::null_mut(), // user_data
            );
        }
        
        log::debug!("WPE: Executing JavaScript");
        Ok(())
    }

    /// Update view state from WebKit
    pub fn update(&mut self) {
        unsafe {
            // Update title
            let title_ptr = wk::webkit_web_view_get_title(self.web_view);
            if !title_ptr.is_null() {
                self.title = Some(CStr::from_ptr(title_ptr).to_string_lossy().into_owned());
            }

            // Update URL
            let uri_ptr = wk::webkit_web_view_get_uri(self.web_view);
            if !uri_ptr.is_null() {
                self.url = CStr::from_ptr(uri_ptr).to_string_lossy().into_owned();
            }

            // Update progress
            self.progress = wk::webkit_web_view_get_estimated_load_progress(self.web_view);

            // Update state
            if wk::webkit_web_view_is_loading(self.web_view) != 0 {
                self.state = WpeViewState::Loading;
            } else {
                self.state = WpeViewState::Ready;
            }

            // Check for new frame from callback and convert to texture
            if let Some(callback_data) = self.callback_data.as_ref() {
                if callback_data.frame_available.swap(false, Ordering::Acquire) {
                    self.needs_redraw = true;
                    
                    // Convert EGLImage to GdkTexture using DMA-BUF
                    if let Some(ref frame) = *callback_data.latest_frame.borrow() {
                        if let Some(ref display) = self.gdk_display {
                            match self.dmabuf_exporter.egl_image_to_texture(
                                frame.egl_image,
                                frame.width,
                                frame.height,
                                display,
                            ) {
                                Ok(texture) => {
                                    log::trace!("WPE: Converted frame to texture {}x{}", frame.width, frame.height);
                                    self.texture = Some(texture);
                                }
                                Err(e) => {
                                    log::warn!("WPE: Failed to convert frame to texture: {}", e);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Resize the view
    pub fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
        self.needs_redraw = true;
        // Note: WPE handles resize through the view backend
    }

    /// Get the current texture (latest rendered frame)
    pub fn texture(&self) -> Option<&Texture> {
        self.texture.as_ref()
    }

    /// Check if view needs redraw
    pub fn needs_redraw(&self) -> bool {
        self.needs_redraw
    }

    /// Clear redraw flag
    pub fn clear_redraw_flag(&mut self) {
        self.needs_redraw = false;
    }

    /// Dispatch frame complete to WPE
    pub fn dispatch_frame_complete(&self) {
        unsafe {
            fdo::wpe_view_backend_exportable_fdo_dispatch_frame_complete(self.exportable);
        }
    }

    /// Release the current exported image back to WPE
    fn release_current_frame(&self) {
        unsafe {
            if let Some(callback_data) = self.callback_data.as_ref() {
                if let Some(frame) = callback_data.latest_frame.borrow_mut().take() {
                    fdo::wpe_view_backend_exportable_fdo_egl_dispatch_release_exported_image(
                        self.exportable,
                        frame.image_handle,
                    );
                }
            }
        }
    }
}

impl Drop for WpeWebView {
    fn drop(&mut self) {
        unsafe {
            // Release any pending frame
            self.release_current_frame();

            // Destroy WebView (this also destroys the view_backend)
            if !self.web_view.is_null() {
                wk::g_object_unref(self.web_view as *mut _);
            }

            // Destroy exportable backend
            if !self.exportable.is_null() {
                fdo::wpe_view_backend_exportable_fdo_destroy(self.exportable);
            }

            // Free callback data
            if !self.callback_data.is_null() {
                let _ = Box::from_raw(self.callback_data);
            }
        }
        log::debug!("WpeWebView dropped");
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

    let callback_data = &*(data as *const WpeCallbackData);
    
    let width = fdo::wpe_fdo_egl_exported_image_get_width(image);
    let height = fdo::wpe_fdo_egl_exported_image_get_height(image);
    let egl_image = fdo::wpe_fdo_egl_exported_image_get_egl_image(image);

    log::trace!("WPE: Frame exported {}x{}", width, height);

    // Release previous frame if any
    if let Some(old_frame) = callback_data.latest_frame.borrow_mut().take() {
        fdo::wpe_view_backend_exportable_fdo_egl_dispatch_release_exported_image(
            callback_data.exportable,
            old_frame.image_handle,
        );
    }

    // Store new frame
    *callback_data.latest_frame.borrow_mut() = Some(ExportedFrame {
        egl_image,
        width,
        height,
        image_handle: image,
    });

    callback_data.frame_available.store(true, Ordering::Release);
}

// Stub for when wpe-webkit feature is disabled
#[cfg(not(feature = "wpe-webkit"))]
impl WpeWebView {
    pub fn new(_width: u32, _height: u32) -> DisplayResult<Self> {
        Err(DisplayError::WebKit(
            "WPE WebKit support not compiled".into(),
        ))
    }
}
