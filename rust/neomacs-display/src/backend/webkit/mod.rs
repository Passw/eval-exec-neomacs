//! WebKit browser embedding for Neomacs.
//!
//! This module provides WebKit browser views that can be embedded
//! directly into Emacs buffers using the WPE WebKit backend.
//!
//! Note: This module now uses WPE WebKit (headless rendering with EGL export)
//! instead of WebKitGTK (which requires GTK widget tree).

mod view;
mod cache;

pub use view::WebKitView;
pub use cache::WebKitCache;
