//! Experimental owned row inputs, exercised against the canonical writer in tests.
//!
//! Kept out of production until an off-screen consumer is integrated.
//! Resolved natural text, source-mapped text and literal spacing are admitted.
//! Complete physical-line programs carry geometry, captured font measurements
//! and work budgets. A bounded worker executes them without evaluator state.
//! Window identity, admission and publication remain the engine's responsibility.

mod input;
pub(crate) use input::ResolvedTextInput;

mod mapped_input;
pub(crate) use mapped_input::ResolvedMappedTextInput;

mod spacing_input;
pub(crate) use spacing_input::ResolvedSpacingInput;

pub(crate) mod program;

pub(crate) mod worker;
