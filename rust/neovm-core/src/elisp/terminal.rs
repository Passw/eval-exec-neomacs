//! Terminal and display management builtins.
//!
//! Implements Emacs-compatible terminal/display query functions from
//! terminal.c and term.c. Since neomacs is a graphical application,
//! most terminal-related queries return sensible defaults for a modern
//! GUI environment.
//!
//! Provided builtins:
//! - Terminal: `terminal-list`, `terminal-name`, `terminal-live-p`,
//!   `terminal-parameter`, `set-terminal-parameter`, `terminal-parameters`,
//!   `frame-terminal`, `delete-terminal`
//! - TTY: `tty-display-color-p`, `tty-display-color-cells`, `tty-type`,
//!   `controlling-tty-p`, `tty-no-underline`, `suspend-tty`, `resume-tty`,
//!   `serial-terminal-p`
//! - Display: `display-mouse-p`, `display-graphic-p`, `display-popup-menus-p`,
//!   `display-selections-p`, `display-screens`, `display-pixel-height`,
//!   `display-pixel-width`, `display-mm-height`, `display-mm-width`,
//!   `display-backing-store`, `display-save-under`, `display-planes`,
//!   `display-color-cells`, `display-visual-class`

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "terminal/args.rs"]
mod args;
#[path = "terminal/display_builtins.rs"]
mod display_builtins;
#[path = "terminal/terminal_builtins.rs"]
mod terminal_builtins;
#[path = "terminal/tty_builtins.rs"]
mod tty_builtins;

pub(crate) use display_builtins::*;
pub(crate) use terminal_builtins::*;
pub(crate) use tty_builtins::*;

#[cfg(test)]
#[path = "terminal/tests.rs"]
mod tests;
