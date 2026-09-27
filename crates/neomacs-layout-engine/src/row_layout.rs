//! Experimental owned row inputs, exercised against the canonical writer in tests.
//!
//! Kept out of production until an off-screen consumer is integrated.
//! Resolved natural text, source-mapped text and literal spacing are admitted.
//! This module does not yet define a complete row job: geometry, realized
//! fonts, validity and work budgets must
//! accompany these inputs before an off-screen worker can execute them.

mod input;
pub(crate) use input::ResolvedTextInput;

mod mapped_input;
pub(crate) use mapped_input::ResolvedMappedTextInput;

mod spacing_input;
pub(crate) use spacing_input::ResolvedSpacingInput;

pub(crate) mod program;

pub(crate) mod worker;
