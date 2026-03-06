//! Window and chrome render commands.

use super::RenderApp;
use crate::thread_comm::RenderCommand;
use winit::dpi::{LogicalSize, PhysicalPosition};
use winit::window::{CursorIcon, Fullscreen, UserAttentionType};

impl RenderApp {
    pub(super) fn handle_window_command(
        &mut self,
        cmd: RenderCommand,
    ) -> Result<(), RenderCommand> {
        match cmd {
            RenderCommand::SetMouseCursor { cursor_type } => {
                if let Some(ref window) = self.window {
                    if cursor_type == 0 {
                        window.set_cursor_visible(false);
                    } else {
                        window.set_cursor_visible(true);
                        let icon = match cursor_type {
                            2 => CursorIcon::Text,
                            3 => CursorIcon::Pointer,
                            4 => CursorIcon::Crosshair,
                            5 => CursorIcon::EwResize,
                            6 => CursorIcon::NsResize,
                            7 => CursorIcon::Wait,
                            8 => CursorIcon::NwseResize,
                            9 => CursorIcon::NeswResize,
                            10 => CursorIcon::NeswResize,
                            11 => CursorIcon::NwseResize,
                            _ => CursorIcon::Default,
                        };
                        window.set_cursor(icon);
                    }
                }
                Ok(())
            }
            RenderCommand::WarpMouse { x, y } => {
                if let Some(ref window) = self.window {
                    let pos = PhysicalPosition::new(x as f64, y as f64);
                    let _ = window.set_cursor_position(pos);
                }
                Ok(())
            }
            RenderCommand::SetWindowTitle { title } => {
                self.chrome.title = title.clone();
                if let Some(ref window) = self.window {
                    window.set_title(&title);
                }
                if !self.chrome.decorations_enabled {
                    self.frame_dirty = true;
                }
                Ok(())
            }
            RenderCommand::SetWindowFullscreen { mode } => {
                if let Some(ref window) = self.window {
                    match mode {
                        3 => {
                            window.set_fullscreen(Some(Fullscreen::Borderless(None)));
                            self.chrome.is_fullscreen = true;
                        }
                        4 => {
                            window.set_maximized(true);
                            self.chrome.is_fullscreen = false;
                        }
                        _ => {
                            window.set_fullscreen(None);
                            window.set_maximized(false);
                            self.chrome.is_fullscreen = false;
                        }
                    }
                    self.frame_dirty = true;
                }
                Ok(())
            }
            RenderCommand::SetWindowMinimized { minimized } => {
                if let Some(ref window) = self.window {
                    window.set_minimized(minimized);
                }
                Ok(())
            }
            RenderCommand::SetWindowPosition { x, y } => {
                if let Some(ref window) = self.window {
                    window.set_outer_position(PhysicalPosition::new(x, y));
                }
                Ok(())
            }
            RenderCommand::SetWindowSize { width, height } => {
                if let Some(ref window) = self.window {
                    let size = LogicalSize::new(width, height);
                    let _ = window.request_inner_size(size);
                }
                Ok(())
            }
            RenderCommand::SetWindowDecorated { decorated } => {
                self.chrome.decorations_enabled = decorated;
                if let Some(ref window) = self.window {
                    window.set_decorations(decorated);
                }
                self.frame_dirty = true;
                Ok(())
            }
            RenderCommand::RequestAttention { urgent } => {
                if let Some(ref window) = self.window {
                    let attention = if urgent {
                        Some(UserAttentionType::Critical)
                    } else {
                        Some(UserAttentionType::Informational)
                    };
                    window.request_user_attention(attention);
                }
                Ok(())
            }
            RenderCommand::CreateWindow {
                emacs_frame_id,
                width,
                height,
                title,
            } => {
                tracing::info!(
                    "CreateWindow request: frame_id=0x{:x} {}x{} \"{}\"",
                    emacs_frame_id,
                    width,
                    height,
                    title
                );
                self.multi_windows
                    .request_create(emacs_frame_id, width, height, title);
                Ok(())
            }
            RenderCommand::DestroyWindow { emacs_frame_id } => {
                tracing::info!("DestroyWindow request: frame_id=0x{:x}", emacs_frame_id);
                self.multi_windows.request_destroy(emacs_frame_id);
                Ok(())
            }
            other => Err(other),
        }
    }
}
