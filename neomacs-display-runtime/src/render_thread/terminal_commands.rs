//! Terminal render commands.

use super::RenderApp;
use crate::thread_comm::RenderCommand;

impl RenderApp {
    pub(super) fn handle_terminal_command(
        &mut self,
        cmd: RenderCommand,
    ) -> Result<(), RenderCommand> {
        match cmd {
            #[cfg(feature = "neo-term")]
            RenderCommand::TerminalCreate {
                id,
                cols,
                rows,
                mode,
                shell,
            } => {
                let term_mode = match mode {
                    1 => crate::terminal::TerminalMode::Inline,
                    2 => crate::terminal::TerminalMode::Floating,
                    _ => crate::terminal::TerminalMode::Window,
                };
                match crate::terminal::TerminalView::new(
                    id,
                    cols,
                    rows,
                    term_mode,
                    shell.as_deref(),
                ) {
                    Ok(view) => {
                        if let Ok(mut shared) = self.shared_terminals.lock() {
                            shared.insert(id, view.term.clone());
                        }
                        self.terminal_manager.terminals.insert(id, view);
                        tracing::info!(
                            "Terminal {} created ({}x{}, {:?})",
                            id,
                            cols,
                            rows,
                            term_mode
                        );
                    }
                    Err(e) => {
                        tracing::error!("Failed to create terminal {}: {}", id, e);
                    }
                }
                Ok(())
            }
            #[cfg(feature = "neo-term")]
            RenderCommand::TerminalWrite { id, data } => {
                if let Some(view) = self.terminal_manager.get_mut(id) {
                    if let Err(e) = view.write(&data) {
                        tracing::warn!("Terminal {} write error: {}", id, e);
                    }
                }
                Ok(())
            }
            #[cfg(feature = "neo-term")]
            RenderCommand::TerminalResize { id, cols, rows } => {
                if let Some(view) = self.terminal_manager.get_mut(id) {
                    view.resize(cols, rows);
                }
                Ok(())
            }
            #[cfg(feature = "neo-term")]
            RenderCommand::TerminalDestroy { id } => {
                if let Ok(mut shared) = self.shared_terminals.lock() {
                    shared.remove(&id);
                }
                self.terminal_manager.destroy(id);
                tracing::info!("Terminal {} destroyed", id);
                Ok(())
            }
            #[cfg(feature = "neo-term")]
            RenderCommand::TerminalSetFloat { id, x, y, opacity } => {
                if let Some(view) = self.terminal_manager.get_mut(id) {
                    view.float_x = x;
                    view.float_y = y;
                    view.float_opacity = opacity;
                }
                Ok(())
            }
            other => Err(other),
        }
    }
}
