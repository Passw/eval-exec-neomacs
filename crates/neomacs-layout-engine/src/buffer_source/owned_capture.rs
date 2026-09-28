//! Bounded acquisition through the canonical source producer. These values
//! remain on the evaluator thread until RowProgram removes all Lisp operands.

use super::consumption::BufferSourceConsumedItem;
use super::face_resolution::BufferSourceFaceResolutionContext;
use super::producer::BufferElementProducer;
use crate::display_item::{DisplayItem, DisplayItemKind};
use crate::display_source::DisplaySourceTextPosition;
use crate::display_source_resolver::PendingDisplaySourceFace;
use crate::frame_face_arena::FrameFaceAttempt;
use crate::neovm_bridge::{LayoutBufferView, LayoutCharPropertyLookup};
use crate::row_layout::program::RowProgramError;
use neovm_core::buffer::{BufferId, CharPos0, EmacsBytePos, EmacsByteRange};
use neovm_core::emacs_core::Value;

pub(crate) struct CapturedPhysicalLine {
    pub items: Vec<DisplayItem>,
    pub faces: Vec<PendingDisplaySourceFace>,
    pub end: CharPos0,
}

/// A conservative first source domain: complete physical lines, ordinary
/// resolved faces and literal item geometry. Window policy (prefixes, bidi,
/// display tables, trailing-whitespace and indicators) is checked by the
/// caller before entering this source-only operation.
pub(crate) fn capture_physical_line<B: LayoutBufferView>(
    buffer_id: BufferId,
    window: u64,
    start: CharPos0,
    max_chars: usize,
    max_items: usize,
    context: BufferSourceFaceResolutionContext<'_, B>,
    face_ids: &mut FrameFaceAttempt,
    cancelled: impl Fn() -> bool,
) -> Result<CapturedPhysicalLine, RowProgramError> {
    let buffer = context.buffer();
    let start_byte = buffer.layout_char_pos_to_emacs_byte_pos(start);
    if start_byte < buffer.layout_point_min_emacs_byte_pos() {
        return Err(RowProgramError::Unsupported);
    }
    if start_byte > buffer.layout_point_min_emacs_byte_pos()
        && buffer.layout_emacs_byte_at_pos(EmacsBytePos::new(start_byte.get() - 1)) != Some(b'\n')
    {
        return Err(RowProgramError::Unsupported);
    }
    let end = CharPos0::new(start.get().saturating_add(max_chars))
        .min(buffer.layout_point_max_char_pos());
    let end_byte = buffer.layout_char_pos_to_emacs_byte_pos(end);
    if start >= end || max_items == 0 {
        return Err(RowProgramError::Budget);
    }
    // Inspect only the physical line this job will consume. Scanning the
    // bounded byte chunks stops at the first newline without copying text;
    // properties on later lines must not veto this line's admission.
    let mut offset = start_byte.get();
    let scan = buffer.layout_try_for_each_emacs_byte_range_chunk(
        EmacsByteRange::new(start_byte, end_byte),
        |chunk| {
            if cancelled() {
                return Err(None);
            }
            if let Some(index) = chunk.iter().position(|byte| *byte == b'\n') {
                return Err(Some(EmacsBytePos::new(offset + index + 1)));
            }
            offset += chunk.len();
            Ok(())
        },
    );
    let end_byte = match scan {
        Err(Some(newline_end)) => newline_end,
        Err(None) => return Err(RowProgramError::Cancelled),
        Ok(()) => end_byte,
    };
    let end = buffer.layout_emacs_byte_pos_to_char_pos(end_byte);
    let display_lookup = LayoutCharPropertyLookup::new(buffer, Value::symbol("display"));
    let height_lookup = LayoutCharPropertyLookup::new(buffer, Value::symbol("line-height"));
    let lookups = [
        "invisible",
        "composition",
        "line-prefix",
        "wrap-prefix",
        "line-spacing",
    ]
    .map(|name| LayoutCharPropertyLookup::new(buffer, Value::symbol(name)));
    let string_lookups = ["before-string", "after-string"]
        .map(|name| LayoutCharPropertyLookup::new(buffer, Value::symbol(name)));
    // Ordinary overlay faces and pointer metadata use the canonical producer.
    // Bound intersecting overlays before it can collect them or inspect any
    // replacement strings. Category/alias properties use the same effective
    // lookup as visible layout, so indirect replacements cannot bypass this.
    let overlays = buffer.layout_overlays();
    for (index, overlay) in overlays
        .iter_overlays_in_accessible_emacs_byte_range(
            EmacsByteRange::new(start_byte, end_byte),
            buffer.layout_point_max_emacs_byte_pos(),
        )
        .enumerate()
    {
        if cancelled() {
            return Err(RowProgramError::Cancelled);
        }
        if index >= max_items {
            return Err(RowProgramError::Budget);
        }
        if overlays.overlay_applies_to_window(overlay, Some(window))
            && (height_lookup
                .effective_overlay_value(buffer, overlay)
                .is_some_and(|value| !literal_line_height(value))
                || lookups
                    .iter()
                    .chain(&string_lookups)
                    .any(|lookup| lookup.effective_overlay_value(buffer, overlay).is_some())
                || display_lookup
                    .effective_overlay_value(buffer, overlay)
                    .is_some_and(|value| !literal_raise_or_nil(value)))
        {
            return Err(RowProgramError::Unsupported);
        }
    }
    // Property boundaries are bounded too: a hostile line with a different
    // property on every character cannot hide unbounded capture work.
    let mut pos = start_byte;
    let mut boundaries = 0;
    while pos < end_byte {
        if cancelled() {
            return Err(RowProgramError::Cancelled);
        }
        boundaries += 1;
        if boundaries > max_items {
            return Err(RowProgramError::Budget);
        }
        if height_lookup
            .text_value_at(buffer, pos)
            .is_some_and(|value| !literal_line_height(value))
            || display_lookup
                .text_value_at(buffer, pos)
                .is_some_and(|value| !literal_raise_or_nil(value))
            || lookups.iter().any(|lookup| {
                lookup
                    .text_value_at(buffer, pos)
                    .is_some_and(|value| !value.is_nil())
            })
        {
            return Err(RowProgramError::Unsupported);
        }
        pos = buffer
            .layout_next_text_prop_change_after_emacs_byte_pos(pos)
            .filter(|next| *next > pos)
            .unwrap_or(end_byte)
            .min(end_byte);
    }
    let mut producer = BufferElementProducer::new_for_window_range(
        buffer_id,
        buffer,
        Some(window),
        start.get() as i64,
        end,
        start_byte.get(),
    );
    let mut position = DisplaySourceTextPosition::new(0, start.get() as i64);
    let mut items = Vec::new();
    let mut faces = Vec::new();
    for _ in 0..max_items {
        if cancelled() {
            return Err(RowProgramError::Cancelled);
        }
        let step = producer.produce_step(position, context, face_ids);
        if !step.pending_non_text_area.is_empty() {
            return Err(RowProgramError::Unsupported);
        }
        faces.extend(step.pending_faces);
        let Some(item) = step.source_item else {
            return Err(RowProgramError::Incomplete);
        };
        let BufferSourceConsumedItem::Renderable(item) = item else {
            return Err(RowProgramError::Unsupported);
        };
        let (_, end_char, end_byte, item) = item.into_render_parts();
        position = DisplaySourceTextPosition::new(
            end_byte.unwrap_or(step.source_position.byte_idx()),
            end_char.unwrap_or(step.source_position.charpos()),
        );
        if let DisplayItemKind::TextRun(run) = &item.kind
            && run.text.chars().any(|ch| {
                crate::display_source::nonascii_space_p(ch)
                    || crate::display_source::nonascii_hyphen_p(ch)
            })
        {
            // These require the buffer loop's nobreak face/substitution
            // policy, even though their source vocabulary is ordinary text.
            return Err(RowProgramError::Unsupported);
        }
        let complete = matches!(item.kind, DisplayItemKind::RowBreak(_));
        items.push(item);
        if complete {
            return Ok(CapturedPhysicalLine {
                items,
                faces,
                end: CharPos0::new(position.charpos() as usize),
            });
        }
    }
    Err(RowProgramError::Budget)
}

/// The canonical producer resolves this modifier into owned item geometry.
/// Admit only a finite literal operand: no conditions, expressions, compound
/// specs or replacement objects can cross this bounded preflight shortcut.
fn literal_raise_or_nil(value: Value) -> bool {
    if value.is_nil() {
        return true;
    }
    if !value.is_cons() || !value.cons_car().is_symbol_named("raise") {
        return false;
    }
    let tail = value.cons_cdr();
    tail.is_cons()
        && tail.cons_cdr().is_nil()
        && tail
            .cons_car()
            .as_number_f64()
            .is_some_and(|number| number.is_finite() && (number as f32).is_finite())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display_row::face_state::{DisplayRowMeasurementMode, DisplayRowMeasurementPolicy};
    use crate::display_row::metrics::DisplayRowFallbackMetrics;
    use crate::neovm_bridge::{FaceResolver, LayoutBufferSnapshot};
    use neomacs_display_protocol::types::FaceId;
    use neovm_core::emacs_core::Context;
    use neovm_core::face::FaceTable;

    fn capture(text: &str, chars: usize) -> Result<CapturedPhysicalLine, RowProgramError> {
        capture_with_properties(text, chars, None)
    }

    fn capture_with_properties(
        text: &str,
        chars: usize,
        property_start: Option<usize>,
    ) -> Result<CapturedPhysicalLine, RowProgramError> {
        let mut eval = Context::new();
        let buffer = eval.buffer_manager_mut().current_buffer_mut().unwrap();
        buffer.insert(text);
        if let Some(start) = property_start {
            buffer.text_props_put_property_in_emacs_byte_range(
                EmacsByteRange::new(EmacsBytePos::new(start), EmacsBytePos::new(text.len())),
                Value::symbol("display"),
                Value::string("replacement"),
            );
        }
        let id = buffer.id();
        let snapshot = LayoutBufferSnapshot::from_buffer(buffer);
        let resolver = FaceResolver::new(&FaceTable::new(), 0xffffff, 0, 14.0, None);
        let metrics = DisplayRowFallbackMetrics::from_default_face_extents(8.0, 16.0, 12.0);
        let context = BufferSourceFaceResolutionContext::new(
            &snapshot,
            &resolver,
            DisplayRowMeasurementPolicy::for_mode(DisplayRowMeasurementMode::ConcreteFont),
            resolver.default_face(),
            FaceId::new(1),
            metrics,
            metrics,
            Default::default(),
        );
        capture_physical_line(
            id,
            1,
            CharPos0::ZERO,
            chars,
            16,
            context,
            &mut FrameFaceAttempt::for_test_with_next_id(2),
            || false,
        )
    }

    #[test]
    fn later_line_replacement_does_not_reject_complete_plain_line() {
        let line = capture_with_properties("好a\nreplaced\n", 32, Some(5)).unwrap();
        assert_eq!(line.end, CharPos0::new(3));
        assert!(matches!(
            capture_with_properties("好a\nreplaced\n", 32, Some(3)),
            Err(RowProgramError::Unsupported)
        ));
    }

    #[test]
    fn acquisition_stops_before_copying_a_huge_physical_line() {
        assert!(matches!(
            capture(&"a".repeat(1_000_000), 32),
            Err(RowProgramError::Incomplete)
        ));
    }

    #[test]
    fn complete_line_capture_keeps_canonical_source_positions() {
        let line = capture("a好b\nnext\n", 16).unwrap();
        assert_eq!(line.end, CharPos0::new(4));
        assert!(matches!(
            line.items.last().unwrap().kind,
            DisplayItemKind::RowBreak(_)
        ));
        let first = &line.items[0];
        assert_eq!(
            first.span.start,
            crate::display_item::DisplaySourcePosition::buffer(
                match first.span.start {
                    crate::display_item::DisplaySourcePosition::Buffer { buffer_id, .. } =>
                        buffer_id,
                    _ => panic!("buffer provenance"),
                },
                CharPos0::ZERO,
                neovm_core::buffer::EmacsBytePos::ZERO
            )
        );
    }
    #[test]
    fn nobreak_text_requires_buffer_special_character_policy() {
        for ch in ['\u{00a0}', '\u{00ad}', '\u{2011}'] {
            let text = format!("before{ch}after\n");
            assert!(
                matches!(capture(&text, 32), Err(RowProgramError::Unsupported)),
                "{ch:?}"
            );
        }
    }
}

fn literal_line_height(value: Value) -> bool {
    value.is_nil()
        || value.is_t()
        || value.is_fixnum()
        || (value.is_float() && (value.xfloat() as f32).is_finite())
}
