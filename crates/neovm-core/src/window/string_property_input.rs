//! Geometry-bearing space properties in prefix strings. Capturing interval
//! revisions alone cannot detect mutation inside a property's Lisp value.
use super::pixel_input::SpaceInput;
use crate::buffer::{CharPos0, text_props::TextPropertyTable};
use crate::emacs_core::{plist::plist_get, value::Value};
use std::sync::Arc;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct StringDisplaySpaces(Arc<[SpaceRun]>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct SpaceRun {
    start: CharPos0,
    end: CharPos0,
    space: SpaceInput,
}

impl StringDisplaySpaces {
    pub(super) fn capture(properties: &TextPropertyTable) -> Self {
        let mut runs = Vec::new();
        let display = Value::symbol("display");
        properties.for_each_interval_from_char_pos(CharPos0::new(0), |start, end, plist| {
            if let Some(value) = plist_get(plist, &display)
                && let Some(space) = SpaceInput::capture(value)
            {
                runs.push(SpaceRun { start, end, space });
            }
            true
        });
        Self(runs.into())
    }
}
