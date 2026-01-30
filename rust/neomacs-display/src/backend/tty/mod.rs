//! Terminal/TTY display backend.

use crate::core::error::{DisplayError, DisplayResult};
use crate::core::scene::Scene;
use crate::backend::DisplayBackend;

/// TTY backend state
pub struct TtyBackend {
    initialized: bool,
    width: u32,
    height: u32,
    // TODO: Add terminfo state
}

impl Default for TtyBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl TtyBackend {
    pub fn new() -> Self {
        Self {
            initialized: false,
            width: 80,
            height: 24,
        }
    }
}

impl DisplayBackend for TtyBackend {
    fn init(&mut self) -> DisplayResult<()> {
        // TODO: Initialize terminal
        // 1. Get terminal size
        // 2. Initialize terminfo/termcap
        // 3. Setup alternate screen buffer if available
        
        self.initialized = true;
        Ok(())
    }

    fn shutdown(&mut self) {
        // TODO: Cleanup terminal state
        self.initialized = false;
    }

    fn render(&mut self, _scene: &Scene) -> DisplayResult<()> {
        if !self.initialized {
            return Err(DisplayError::Backend("TTY backend not initialized".into()));
        }

        // TODO: Render scene to terminal
        // 1. Build character buffer
        // 2. Compute diff from previous frame
        // 3. Generate escape sequences
        
        Ok(())
    }

    fn present(&mut self) -> DisplayResult<()> {
        if !self.initialized {
            return Err(DisplayError::Backend("TTY backend not initialized".into()));
        }

        // TODO: Write escape sequences to stdout
        // Flush stdout
        
        Ok(())
    }

    fn name(&self) -> &'static str {
        "tty"
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
    }

    fn set_vsync(&mut self, _enabled: bool) {
        // No vsync on TTY
    }
}
