//! Display update builtins from Emacs `dispnew.c`.
//!
//! Provides stub and functional implementations for display update functions:
//! - `redraw-frame` -- redraw a frame (stub)
//! - `redraw-display` -- redraw all frames (stub)
//! - `open-termscript` -- open terminal script file (stub)
//! - `ding` -- ring the bell (stub)
//! - `send-string-to-terminal` -- send raw string to terminal (stub)
//! - `internal-show-cursor` -- show/hide cursor in window (stub)
//! - `internal-show-cursor-p` -- query cursor visibility
//! - `last-nonminibuffer-frame` -- return last non-minibuffer frame
//! - `sleep-for` -- sleep for a duration
//! - `sit-for` -- sit (sleep) for a duration

use super::error::{signal, EvalResult, Flow};
use super::value::*;

#[path = "dispnew/args.rs"]
mod args;
#[path = "dispnew/pure.rs"]
mod pure;
#[path = "dispnew/timing.rs"]
mod timing;

pub(crate) use pure::*;
pub(crate) use timing::*;

#[cfg(test)]
#[path = "dispnew/tests.rs"]
mod tests;
