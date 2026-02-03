//! Neomacs Display Engine
//!
//! A GPU-accelerated display engine for Neomacs using WPE WebKit and wgpu.
//!
//! # Architecture
//!
//! ```text
//! Emacs Core (C) ──► FFI ──► Scene Graph ──► wgpu ──► GPU
//! ```

#![allow(unused)] // TODO: Remove once implementation is complete

pub mod core;
pub mod backend;
pub mod text;
pub mod ffi;

pub use crate::core::*;
pub use crate::backend::DisplayBackend;
pub use crate::text::TextEngine;

/// Library version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize the display engine
pub fn init() -> Result<(), DisplayError> {
    env_logger::init();
    log::info!("Neomacs display engine v{} initializing (wgpu backend)", VERSION);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }
}
