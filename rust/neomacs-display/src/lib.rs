//! Neomacs Display Engine
//!
//! A GPU-accelerated display engine for Neomacs using GTK4/GSK.
//!
//! # Architecture
//!
//! ```text
//! Emacs Core (C) ──► FFI ──► Scene Graph ──► GTK4/GSK ──► GPU
//! ```

#![allow(unused)] // TODO: Remove once implementation is complete

pub mod core;
pub mod backend;
pub mod text;
pub mod ffi;

pub use crate::core::*;
pub use crate::backend::DisplayBackend;
pub use crate::text::{TextEngine, GlyphAtlas};

/// Library version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize the display engine
pub fn init() -> Result<(), DisplayError> {
    env_logger::init();
    log::info!("Neomacs display engine v{} initializing", VERSION);
    
    // Initialize GTK4
    gtk4::init().map_err(|e| DisplayError::InitFailed(e.to_string()))?;
    
    log::info!("GTK4 initialized successfully");
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
