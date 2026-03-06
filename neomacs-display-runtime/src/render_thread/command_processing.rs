use super::RenderApp;
use crate::thread_comm::RenderCommand;

impl RenderApp {
    /// Process pending commands from Emacs.
    pub(super) fn process_commands(&mut self) -> bool {
        let mut should_exit = false;

        while let Ok(cmd) = self.comms.cmd_rx.try_recv() {
            match cmd {
                RenderCommand::Shutdown => {
                    tracing::info!("Render thread received shutdown command");
                    should_exit = true;
                    continue;
                }
                RenderCommand::ScrollBlit { .. } => {
                    tracing::debug!("ScrollBlit ignored (full-frame rendering mode)");
                    continue;
                }
                _ => {}
            }

            let cmd = match self.handle_asset_command(cmd) {
                Ok(()) => continue,
                Err(cmd) => cmd,
            };
            let cmd = match self.handle_window_command(cmd) {
                Ok(()) => continue,
                Err(cmd) => cmd,
            };
            let cmd = match self.handle_terminal_command(cmd) {
                Ok(()) => continue,
                Err(cmd) => cmd,
            };
            if self.handle_ui_command(cmd).is_ok() {
                continue;
            }
        }

        should_exit
    }
}
