//! Backend trait and module exports.

use crate::core::error::DisplayResult;
use crate::core::scene::Scene;

pub mod gtk4;
pub mod tty;

/// Display backend trait
/// 
/// Implementations provide platform-specific rendering.
/// Note: GTK4 backend is not Send+Sync because GTK is single-threaded.
pub trait DisplayBackend {
    /// Initialize the backend
    fn init(&mut self) -> DisplayResult<()>;

    /// Shutdown the backend
    fn shutdown(&mut self);

    /// Render a scene to the display
    fn render(&mut self, scene: &Scene) -> DisplayResult<()>;

    /// Present the rendered frame
    fn present(&mut self) -> DisplayResult<()>;

    /// Get the backend name
    fn name(&self) -> &'static str;

    /// Check if the backend is initialized
    fn is_initialized(&self) -> bool;

    /// Handle resize
    fn resize(&mut self, width: u32, height: u32);

    /// Set VSync enabled
    fn set_vsync(&mut self, enabled: bool);
}

/// Backend type selection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub enum BackendType {
    /// GTK4/GSK GPU-accelerated backend
    Gtk4 = 0,

    /// Terminal/TTY backend
    Tty = 1,
}

impl Default for BackendType {
    fn default() -> Self {
        Self::Gtk4
    }
}
