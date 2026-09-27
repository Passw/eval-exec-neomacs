//! Owned inputs for row production shared by synchronous and future worker paths.
//!
//! Only resolved text is admitted so far. This module does not yet define a
//! complete row job: geometry, realized fonts, validity and work budgets must
//! accompany these inputs before an off-screen worker can execute them.

mod input;
pub(crate) use input::ResolvedTextInput;
