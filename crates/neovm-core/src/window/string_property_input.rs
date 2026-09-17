//! Geometry-bearing space properties in prefix strings. Capturing interval
//! revisions alone cannot detect mutation inside a property's Lisp value.
use super::pixel_input::SpaceInput;
use crate::buffer::{CharPos0, text_props::TextPropertyTable};
use crate::emacs_core::{display_spec::DisplayPropertySpecs, plist::plist_get, value::Value};
use std::{ops::ControlFlow, sync::Arc};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct StringDisplayInputs(Arc<[DisplayRun]>);

#[derive(Clone, Debug, PartialEq, Eq)]
struct DisplayRun {
    start: CharPos0,
    end: CharPos0,
    eval_enabled: bool,
    specs: Box<[SpecInput]>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum SpecInput {
    Space(SpaceInput),
    // Preserve order and identity of surrounding specs. Their nested payloads
    // (faces, replacement strings, resources, conditions) need separate capture.
    Other(usize),
}

impl StringDisplayInputs {
    pub(super) fn capture(properties: &TextPropertyTable) -> Self {
        let mut runs = Vec::new();
        let display = Value::symbol("display");
        properties.for_each_interval_from_char_pos(CharPos0::new(0), |start, end, plist| {
            if let Some(value) = plist_get(plist, &display) {
                let decoded = DisplayPropertySpecs::of(value);
                let mut specs = Vec::new();
                decoded.for_each(|spec| {
                    specs.push(match SpaceInput::capture(spec) {
                        Some(space) => SpecInput::Space(space),
                        None => SpecInput::Other(spec.bits()),
                    });
                    ControlFlow::Continue(())
                });
                runs.push(DisplayRun {
                    start,
                    end,
                    eval_enabled: decoded.eval_enabled,
                    specs: specs.into(),
                });
            }
            true
        });
        Self(runs.into())
    }
}
