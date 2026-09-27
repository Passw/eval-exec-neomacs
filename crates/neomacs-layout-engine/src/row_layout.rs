//! Experimental owned row inputs, exercised against the canonical writer in tests.
//!
//! Kept out of production until an off-screen consumer is integrated.
//! Only resolved natural and source-mapped text is admitted so far. This module does not yet define a
//! complete row job: geometry, realized fonts, validity and work budgets must
//! accompany these inputs before an off-screen worker can execute them.

mod input;
pub(crate) use input::ResolvedTextInput;

mod mapped_input;
pub(crate) use mapped_input::ResolvedMappedTextInput;
