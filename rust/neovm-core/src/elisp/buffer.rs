//! Buffer management builtins -- Emacs buffer.c equivalents.
//!
//! Provides Emacs-compatible buffer management:
//! - `current-buffer` -- return the current buffer
//! - `set-buffer` -- switch current buffer
//! - `get-buffer` -- find buffer by name
//! - `get-buffer-create` -- get or create buffer by name
//! - `generate-new-buffer-name` -- generate a unique buffer name
//! - `buffer-list` -- list all live buffers
//! - `buffer-name` -- return buffer name string
//! - `buffer-file-name` -- return associated file name or nil
//! - `buffer-modified-p` -- check modified flag
//! - `set-buffer-modified-p` -- set modified flag
//! - `buffer-live-p` -- check if buffer is live
//! - `rename-buffer` -- rename a buffer
//! - `other-buffer` -- return another buffer
//! - `kill-buffer` -- remove a buffer
//! - `erase-buffer` -- clear buffer content
//! - `buffer-enable-undo` -- enable undo recording (stub)
//! - `buffer-disable-undo` -- disable undo recording (stub)
//! - `buffer-size` -- return content length

use super::error::{signal, EvalResult, Flow};
use super::value::*;

use crate::buffer::BufferId;

#[path = "buffer/args.rs"]
mod args;
#[path = "buffer/stateful.rs"]
mod stateful;

pub(crate) use stateful::*;

#[cfg(test)]
#[path = "buffer/tests.rs"]
mod tests;
