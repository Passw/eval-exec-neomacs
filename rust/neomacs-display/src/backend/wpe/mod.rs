//! WPE WebKit backend for headless browser rendering.
//!
//! This module provides WPE WebKit integration for embedding web content
//! in Emacs buffers. Unlike WebKitGTK, WPE renders to EGL/DMA-BUF textures
//! that can be composited without a GTK widget tree.
//!
//! Architecture:
//! - libwpe: Core abstraction (view-backend, input events)
//! - wpebackend-fdo: FreeDesktop.org backend with EGL export
//! - wpe-webkit: WebKit engine (GObject API)

#[cfg(feature = "wpe-webkit")]
mod sys;

#[cfg(feature = "wpe-webkit")]
mod backend;

#[cfg(feature = "wpe-webkit")]
mod view;

#[cfg(feature = "wpe-webkit")]
mod dmabuf;

#[cfg(feature = "wpe-webkit")]
pub use backend::WpeBackend;

#[cfg(feature = "wpe-webkit")]
pub use view::{WpeWebView, WpeViewState};

#[cfg(feature = "wpe-webkit")]
pub use dmabuf::DmaBufExporter;
