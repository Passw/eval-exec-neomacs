//! Flatten bounded strings with the canonical Lisp-string cursor. Live source
//! operands return to the evaluator for rooting, never to the row worker.
use super::*;
use crate::display_origin::{DisplayOrigin, OverlayStringKind};
use crate::display_source::{
    DisplayItemSource, DisplaySourceContext, LispStringSourceCursor, LispStringSourceOrigin,
};
use crate::display_source_resolver::{
    DisplayDefaultFaceInstallPolicy, DisplaySourcePropertyResolver, DisplaySourceResolveState,
};

pub(super) fn bounded_string(value: Value, max_chars: usize) -> bool {
    if value.is_nil() {
        return true;
    }
    let Some(string) = value.as_lisp_string() else {
        return false;
    };
    if string.schars() > max_chars || string.as_bytes().len() > max_chars.saturating_mul(4) {
        return false;
    }
    let props = neovm_core::emacs_core::value::get_string_text_properties_table_for_value(value);
    let Some(props) = props else {
        return true;
    };
    let unsupported = [
        "invisible",
        "composition",
        "cursor",
        "line-prefix",
        "wrap-prefix",
        "display",
    ]
    .map(Value::symbol);
    !props.has_any_non_nil_property_in_char_range(
        neovm_core::buffer::CharRange::new(CharPos0::ZERO, CharPos0::new(string.schars())),
        &unsupported,
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn capture_insertions<B: LayoutBufferView>(
    strings: &super::super::text_source::BufferOverlayStringsItem,
    context: BufferSourceFaceResolutionContext<'_, B>,
    face_ids: &mut FrameFaceAttempt,
    items: &mut Vec<DisplayItem>,
    faces: &mut Vec<PendingDisplaySourceFace>,
    roots: &mut Vec<Value>,
    max_chars: usize,
    max_items: usize,
    cancelled: &impl Fn() -> bool,
) -> Result<(), RowProgramError> {
    let mut bytes = items
        .iter()
        .map(|item| match &item.kind {
            DisplayItemKind::TextRun(run) => run.text.len(),
            _ => 0,
        })
        .sum::<usize>();
    for (index, entry) in strings.strings().iter().enumerate() {
        if cancelled() {
            return Err(RowProgramError::Cancelled);
        }
        if !bounded_string(entry.string, max_chars) {
            return Err(RowProgramError::Unsupported);
        }
        bytes = bytes.saturating_add(
            entry
                .string
                .as_lisp_string()
                .ok_or(RowProgramError::Unsupported)?
                .as_bytes()
                .len(),
        );
        if bytes > max_chars.saturating_mul(4) {
            return Err(RowProgramError::Budget);
        }
        if roots.len() >= max_items.saturating_mul(2) {
            return Err(RowProgramError::Budget);
        }
        roots.extend([entry.string, entry.overlay_id]);
        let kind = if entry.after_string_p {
            OverlayStringKind::After
        } else {
            OverlayStringKind::Before
        };
        let origin = DisplayOrigin::OverlayString {
            overlay_id: entry.overlay_id,
            anchor_charpos: strings.anchor_charpos(),
            kind,
        };
        let base = context.resolve_display_string_base_face(
            origin,
            origin.default_base_face_policy(),
            None,
            DisplayDefaultFaceInstallPolicy::InstallDefaultFace,
            face_ids,
        );
        if let Some(face) = base.pending_face() {
            faces.push(face.clone());
        }
        let mut source = LispStringSourceCursor::new_with_box_boundaries(
            1,
            entry.string,
            crate::display_item::RenderFaceRef::FaceId(base.face_id()),
            LispStringSourceOrigin::OverlayString {
                overlay_id: entry.overlay_id,
                kind,
            },
            strings
                .box_boundaries()
                .sequence_member(index, strings.strings().len()),
        )
        .ok_or(RowProgramError::Unsupported)?;
        let mut state = DisplaySourceResolveState::default();
        state.remember_face(base.face_id(), base.face());
        let params = context.string_source_resolve_params(&base);
        let mut resolver = DisplaySourcePropertyResolver::buffer_local(
            context.buffer(),
            params,
            &mut state,
            face_ids,
            faces,
        );
        let mut non_text = Vec::new();
        let mut source_context = DisplaySourceContext::with_face_resolver_and_non_text_area_sink(
            &mut resolver,
            &mut non_text,
            context.buffer().layout_display_target(),
        )
        .with_automatic_composition(params.automatic_composition);
        loop {
            if cancelled() {
                return Err(RowProgramError::Cancelled);
            }
            let Some(item) = source.next_item(&mut source_context) else {
                break;
            };
            if items.len() >= max_items {
                return Err(RowProgramError::Budget);
            }
            if !matches!(&item.kind, DisplayItemKind::TextRun(run) if !matches!(run.composition, crate::display_item::DisplayTextComposition::Automatic(_)))
            {
                return Err(RowProgramError::Unsupported);
            }
            items.push(item);
        }
        if !non_text.is_empty() {
            return Err(RowProgramError::Unsupported);
        }
    }
    Ok(())
}
