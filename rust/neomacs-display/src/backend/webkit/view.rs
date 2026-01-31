//! WebKit view wrapper for embedding in Neomacs.
//!
//! This module now wraps WPE WebKit instead of WebKitGTK.
//! WPE provides headless rendering with EGL/DMA-BUF export.

use crate::core::error::{DisplayError, DisplayResult};

#[cfg(feature = "wpe-webkit")]
use crate::backend::wpe::{WpeBackend, WpeWebView, WpeViewState};

/// State of a WebKit view
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WebKitState {
    /// View is loading
    Loading,
    /// View is ready/loaded
    Ready,
    /// View had an error
    Error,
}

#[cfg(feature = "wpe-webkit")]
impl From<WpeViewState> for WebKitState {
    fn from(state: WpeViewState) -> Self {
        match state {
            WpeViewState::Loading | WpeViewState::Creating => WebKitState::Loading,
            WpeViewState::Ready => WebKitState::Ready,
            WpeViewState::Error => WebKitState::Error,
        }
    }
}

/// A WebKit browser view instance.
///
/// This is a high-level wrapper that uses WPE WebKit internally.
#[cfg(feature = "wpe-webkit")]
pub struct WebKitView {
    /// The underlying WPE view
    wpe_view: WpeWebView,
}

#[cfg(feature = "wpe-webkit")]
impl WebKitView {
    /// Create a new WebKit view
    pub fn new(wpe_backend: &WpeBackend, width: i32, height: i32) -> DisplayResult<Self> {
        let wpe_view = WpeWebView::new(wpe_backend, width as u32, height as u32)?;
        Ok(Self { wpe_view })
    }

    /// Load a URL
    pub fn load_uri(&mut self, uri: &str) -> DisplayResult<()> {
        self.wpe_view.load_uri(uri)
    }

    /// Load HTML content
    pub fn load_html(&mut self, html: &str, base_uri: Option<&str>) -> DisplayResult<()> {
        self.wpe_view.load_html(html, base_uri)
    }

    /// Go back in history
    pub fn go_back(&mut self) -> DisplayResult<()> {
        self.wpe_view.go_back()
    }

    /// Go forward in history
    pub fn go_forward(&mut self) -> DisplayResult<()> {
        self.wpe_view.go_forward()
    }

    /// Reload the page
    pub fn reload(&mut self) -> DisplayResult<()> {
        self.wpe_view.reload()
    }

    /// Stop loading
    pub fn stop(&mut self) -> DisplayResult<()> {
        self.wpe_view.stop()
    }

    /// Execute JavaScript
    pub fn execute_javascript(&self, script: &str) -> DisplayResult<()> {
        self.wpe_view.execute_javascript(script)
    }

    /// Update view state (call periodically)
    pub fn update(&mut self) {
        self.wpe_view.update();
    }

    /// Resize the view
    pub fn resize(&mut self, width: i32, height: i32) {
        self.wpe_view.resize(width as u32, height as u32);
    }

    /// Get current URL
    pub fn url(&self) -> &str {
        &self.wpe_view.url
    }

    /// Get page title
    pub fn title(&self) -> Option<&str> {
        self.wpe_view.title.as_deref()
    }

    /// Get loading progress (0.0 - 1.0)
    pub fn progress(&self) -> f64 {
        self.wpe_view.progress
    }

    /// Get view state
    pub fn state(&self) -> WebKitState {
        self.wpe_view.state.into()
    }

    /// Get the texture for rendering
    pub fn texture(&self) -> Option<&gdk4::Texture> {
        self.wpe_view.texture()
    }

    /// Get dimensions
    pub fn dimensions(&self) -> (u32, u32) {
        (self.wpe_view.width, self.wpe_view.height)
    }

    /// Check if view needs redraw
    pub fn needs_redraw(&self) -> bool {
        self.wpe_view.needs_redraw()
    }

    /// Clear redraw flag
    pub fn clear_redraw_flag(&mut self) {
        self.wpe_view.clear_redraw_flag();
    }

    /// Dispatch frame complete
    pub fn dispatch_frame_complete(&self) {
        self.wpe_view.dispatch_frame_complete();
    }
}

// Stub implementation when wpe-webkit feature is disabled
#[cfg(not(feature = "wpe-webkit"))]
pub struct WebKitView {
    pub url: String,
    pub state: WebKitState,
    pub width: i32,
    pub height: i32,
    pub title: Option<String>,
    pub progress: f64,
}

#[cfg(not(feature = "wpe-webkit"))]
impl WebKitView {
    pub fn new(_width: i32, _height: i32) -> DisplayResult<Self> {
        Err(DisplayError::WebKit("WPE WebKit support not compiled".into()))
    }

    pub fn load_uri(&mut self, _uri: &str) -> DisplayResult<()> { Ok(()) }
    pub fn load_html(&mut self, _html: &str, _base_uri: Option<&str>) -> DisplayResult<()> { Ok(()) }
    pub fn go_back(&mut self) -> DisplayResult<()> { Ok(()) }
    pub fn go_forward(&mut self) -> DisplayResult<()> { Ok(()) }
    pub fn reload(&mut self) -> DisplayResult<()> { Ok(()) }
    pub fn stop(&mut self) -> DisplayResult<()> { Ok(()) }
    pub fn execute_javascript(&self, _script: &str) -> DisplayResult<()> { Ok(()) }
    pub fn update(&mut self) {}
    pub fn resize(&mut self, _width: i32, _height: i32) {}
    pub fn url(&self) -> &str { &self.url }
    pub fn title(&self) -> Option<&str> { self.title.as_deref() }
    pub fn progress(&self) -> f64 { self.progress }
    pub fn state(&self) -> WebKitState { self.state }
    pub fn dimensions(&self) -> (u32, u32) { (self.width as u32, self.height as u32) }
    pub fn needs_redraw(&self) -> bool { false }
    pub fn clear_redraw_flag(&mut self) {}
    pub fn dispatch_frame_complete(&self) {}
}
