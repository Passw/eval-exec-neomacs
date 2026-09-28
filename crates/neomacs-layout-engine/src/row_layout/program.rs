//! Complete, bounded row programs. Capture resolves source semantics and font
//! measurements; execution only calls the canonical glyph writer. In particular,
//! executing a program never opens a font database or consults evaluator TLS.

use super::{ResolvedMappedTextInput, ResolvedSpacingInput, ResolvedTextInput};
use crate::display_face_ref::render_face_ref_id;
use crate::display_item::*;
use crate::display_pixel_calc::PixelCalcContext;
use crate::display_row::builder::*;
use crate::display_row::face_state::{DisplayRowFace, DisplayRowMeasurementMode};
use crate::display_row::finalizer::DisplayRowLineEndFinalizer;
use crate::display_row::geometry::DisplayRowTextAreaOrigin;
use crate::display_row::metrics::DisplayRowFallbackMetrics;
use crate::display_text_run_measurement::DisplayTextRunMeasurement;
use neomacs_display_protocol::frame_glyphs::GlyphRowRole;
use neomacs_display_protocol::glyph_matrix::{GlyphArea, GlyphRow};
use neomacs_display_protocol::types::{Color, FaceId};
use rustc_hash::FxHashMap;

/// Limits apply to capture as well as execution. A partial row is never a result.
#[derive(Clone, Copy, Debug)]
pub(crate) struct RowProgramLimits {
    pub items: usize,
    pub text_bytes: usize,
    pub glyphs: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RowProgramError {
    Unsupported,
    Budget,
    Incomplete,
    Overflow,
    MissingMeasurement,
    Cancelled,
}

/// No pixel-calculation expressions, image catalogs, or Lisp values. Literal
/// spacing is the only supported spacing operation at this boundary.
#[derive(Clone, Debug)]
pub(crate) struct RowProgramGeometry {
    pub inherited_line_spacing: f32,
    pub width: f32,
    pub metrics: DisplayRowFallbackMetrics,
    pub tabs: DisplayTabPolicy,
    pub base_face: FaceId,
    pub background: Color,
}

impl RowProgramGeometry {
    fn layout(&self) -> DisplayRowLayout {
        DisplayRowLayout {
            role: GlyphRowRole::Text,
            y_px: 0.0,
            height_px: self.metrics.row_height(),
            ascent_px: self.metrics.ascent(),
            char_width_px: self.metrics.char_width(),
            tab_policy: self.tabs.clone(),
            line_number_width_px: 0.0,
            base_face: RenderFaceRef::FaceId(self.base_face),
            pixel_calc: PixelCalcContext::for_chrome_row(
                self.width,
                self.metrics.char_width(),
                self.metrics.row_height(),
                Default::default(),
            ),
            space_image_params: None,
        }
    }
}

#[derive(Clone, Debug)]
enum Operation {
    Text(ResolvedTextInput),
    Mapped(ResolvedMappedTextInput),
    Space(ResolvedSpacingInput),
    Break {
        span: SourceSpan,
        face: FaceId,
        line_height: DisplayLineHeightPolicy,
        line_spacing: f32,
        layout: DisplayItemLayout,
        pointer_appearance: Option<DisplayPointerAppearance>,
        edges: neomacs_display_protocol::face::BoxVerticalEdges,
        membership: neomacs_display_protocol::face::BoxRunMembership,
    },
}

impl Operation {
    fn capture(
        item: DisplayItem,
        base: FaceId,
        default_height: f32,
        inherited_line_spacing: f32,
    ) -> Result<Self, RowProgramError> {
        match &item.kind {
            DisplayItemKind::TextRun(run)
                if !matches!(run.composition, DisplayTextComposition::Automatic(_)) =>
            {
                ResolvedTextInput::capture(item, base)
                    .map(Self::Text)
                    .map_err(|_| RowProgramError::Unsupported)
            }
            DisplayItemKind::SourceMappedText(_) => ResolvedMappedTextInput::capture(item, base)
                .map(Self::Mapped)
                .map_err(|_| RowProgramError::Unsupported),
            DisplayItemKind::Stretch(_) => ResolvedSpacingInput::capture(item, base)
                .map(Self::Space)
                .map_err(|_| RowProgramError::Unsupported),
            DisplayItemKind::RowBreak(value)
                if !matches!(
                    value.line_spacing,
                    DisplayLineSpacingPolicy::Scale {
                        reference: DisplayLineSpacingReference::Named(_),
                        ..
                    }
                ) =>
            {
                Ok(Self::Break {
                    span: item.span,
                    face: render_face_ref_id(item.face, base),
                    line_height: value.line_height,
                    line_spacing: value
                        .line_spacing
                        .resolve(default_height, inherited_line_spacing),
                    layout: item.layout,
                    pointer_appearance: item.pointer_appearance,
                    edges: item.box_vertical_edges,
                    membership: item.box_run_membership,
                })
            }
            _ => Err(RowProgramError::Unsupported),
        }
    }

    fn into_item(self) -> DisplayItem {
        match self {
            Self::Text(input) => DisplayItem {
                span: input.span,
                face: RenderFaceRef::FaceId(input.face),
                kind: DisplayItemKind::TextRun(input.run),
                layout: input.layout,
                pointer_appearance: input.pointer_appearance,
                box_vertical_edges: input.box_vertical_edges,
                box_run_membership: input.box_run_membership,
            },
            Self::Mapped(input) => DisplayItem {
                span: input.span,
                face: RenderFaceRef::FaceId(input.face),
                kind: DisplayItemKind::SourceMappedText(DisplaySourceMappedText::face_segment(
                    input.text,
                    input.glyph_string_start,
                )),
                layout: input.layout,
                pointer_appearance: input.pointer_appearance,
                box_vertical_edges: input.box_vertical_edges,
                box_run_membership: input.box_run_membership,
            },
            Self::Space(input) => input.into_display_item(),
            Self::Break {
                span,
                face,
                line_height,
                line_spacing,
                layout,
                pointer_appearance,
                edges,
                membership,
            } => {
                let mut item = DisplayItem::new(
                    span,
                    RenderFaceRef::FaceId(face),
                    DisplayItemKind::RowBreak(DisplayRowBreak {
                        line_height,
                        line_spacing: DisplayLineSpacingPolicy::Pixels(line_spacing),
                    }),
                );
                item.box_vertical_edges = edges;
                item.box_run_membership = membership;
                item.layout = layout;
                item.pointer_appearance = pointer_appearance;
                item
            }
        }
    }
}

/// Measurements come from the capturing frame's actual selected fonts. A
/// missing query is an admission failure, never permission to guess metrics.
#[derive(Clone, Debug)]
struct Measurements {
    backend: DisplayGeometryBackend,
    char_width: f32,
    advances: FxHashMap<(char, FaceId, u8), Option<f32>>,
    vertical: FxHashMap<(char, FaceId), Option<DisplayRowVerticalMetrics>>,
    faces: FxHashMap<FaceId, (Option<f32>, Option<DisplayRowVerticalMetrics>)>,
    missing: bool,
}

impl Measurements {
    fn capture_item(
        &mut self,
        item: &DisplayItem,
        measurer: &mut dyn DisplayGlyphMeasurer,
        base: FaceId,
    ) {
        let face = render_face_ref_id(item.face, base);
        self.faces.entry(face).or_insert_with(|| {
            (
                measurer.face_space_width_px(face),
                measurer.face_vertical_metrics_px(face),
            )
        });
        let text = match &item.kind {
            DisplayItemKind::TextRun(run) => run.text.as_ref(),
            DisplayItemKind::SourceMappedText(run) => run.text.as_ref(),
            _ => "",
        };
        // Include the primary-space fallback used by tabs and literal spaces.
        for ch in text
            .chars()
            .chain(std::iter::once(' '))
            .chain(match &item.kind {
                DisplayItemKind::Stretch(DisplayStretch {
                    width: DisplayStretchWidth::RelativeToSource { source, .. },
                    ..
                }) => source.as_rust_char(),
                _ => None,
            })
        {
            self.vertical
                .entry((ch, face))
                .or_insert_with(|| measurer.glyph_vertical_metrics_px(ch, face));
            // Natural Unicode characters occupy zero, one or two columns.
            // Unsupported future width domains are rejected during execution.
            for columns in 0..=2 {
                let fallback = self.char_width.max(1.0) * f32::from(columns.max(1));
                self.advances
                    .entry((ch, face, columns))
                    .or_insert_with(|| measurer.glyph_advance_px(ch, face, columns, fallback));
            }
        }
    }
}

impl DisplayGlyphMeasurer for Measurements {
    fn glyph_advance_px(
        &mut self,
        ch: char,
        face: FaceId,
        columns: u8,
        fallback: f32,
    ) -> Option<f32> {
        if fallback != self.char_width.max(1.0) * f32::from(columns.max(1)) {
            self.missing = true;
        }
        self.advances
            .get(&(ch, face, columns))
            .copied()
            .unwrap_or_else(|| {
                self.missing = true;
                None
            })
    }
    fn glyph_vertical_metrics_px(
        &mut self,
        ch: char,
        face: FaceId,
    ) -> Option<DisplayRowVerticalMetrics> {
        self.vertical.get(&(ch, face)).copied().unwrap_or_else(|| {
            self.missing = true;
            None
        })
    }
    fn face_vertical_metrics_px(&mut self, face: FaceId) -> Option<DisplayRowVerticalMetrics> {
        self.faces
            .get(&face)
            .map(|entry| entry.1)
            .unwrap_or_else(|| {
                self.missing = true;
                None
            })
    }
    fn face_space_width_px(&mut self, face: FaceId) -> Option<f32> {
        self.faces
            .get(&face)
            .map(|entry| entry.0)
            .unwrap_or_else(|| {
                self.missing = true;
                None
            })
    }
    fn display_geometry_backend(&self) -> DisplayGeometryBackend {
        self.backend
    }
    fn text_run_advances_px(&mut self, _: &str, _: FaceId, _: f32) -> DisplayTextRunMeasurement {
        // Every admitted text operation carries its whole-run plan separately.
        self.missing = true;
        DisplayTextRunMeasurement::PerChar
    }
}

#[derive(Clone, Debug)]
pub(crate) struct RowProgram {
    geometry: RowProgramGeometry,
    operations: Vec<(Operation, DisplayTextRunMeasurement)>,
    measurements: Measurements,
    faces: Vec<DisplayRowFace>,
    limits: RowProgramLimits,
}

pub(crate) struct ComputedRow {
    pub row: GlyphRow,
    pub slots: Vec<DisplayRowGlyphSlot>,
    pub slot_heights: Vec<f32>,
    pub source: SourceSpan,
    pub end: DisplayRowPosition,
    pub terminator_width: f32,
    pub terminator_height: f32,
}

impl RowProgram {
    /// The iterator must be bounded at acquisition, before it allocates an
    /// item. These limits additionally bound everything retained in the job.
    pub(crate) fn capture(
        geometry: RowProgramGeometry,
        items: impl IntoIterator<Item = DisplayItem>,
        faces: Vec<DisplayRowFace>,
        measurer: &mut dyn DisplayGlyphMeasurer,
        limits: RowProgramLimits,
    ) -> Result<Self, RowProgramError> {
        if faces.len() > limits.items || !geometry.width.is_finite() || geometry.width <= 0.0 {
            return Err(RowProgramError::Budget);
        }
        let mut metadata_bytes = geometry
            .tabs
            .stop_cols
            .len()
            .saturating_mul(std::mem::size_of::<usize>());
        for face in &faces {
            if face.stipple.is_some() {
                return Err(RowProgramError::Unsupported);
            }
            metadata_bytes = metadata_bytes
                .saturating_add(face.font_family.len())
                .saturating_add(face.font_file_path.as_ref().map_or(0, String::len))
                .saturating_add(face.lisp_name.as_ref().map_or(0, String::len));
        }
        if metadata_bytes > limits.text_bytes {
            return Err(RowProgramError::Budget);
        }
        let mut measurements = Measurements {
            backend: measurer.display_geometry_backend(),
            char_width: geometry.metrics.char_width(),
            advances: Default::default(),
            vertical: Default::default(),
            faces: Default::default(),
            missing: false,
        };
        let mut operations = Vec::new();
        let mut bytes = metadata_bytes;
        let mut glyphs = 0usize;
        let mut complete = false;
        for item in items {
            if complete {
                return Err(RowProgramError::Unsupported);
            }
            if operations.len() == limits.items {
                return Err(RowProgramError::Budget);
            }
            if item.layout.break_after_row {
                return Err(RowProgramError::Unsupported);
            }
            let text = match &item.kind {
                DisplayItemKind::TextRun(run) => run.text.as_ref(),
                DisplayItemKind::SourceMappedText(run) => run.text.as_ref(),
                _ => "",
            };
            bytes = bytes.saturating_add(text.len());
            // Wide glyph padding and the newline fill can each add slots.
            glyphs = glyphs.saturating_add(text.chars().count().saturating_mul(2).max(1));
            if bytes > limits.text_bytes || glyphs > limits.glyphs {
                return Err(RowProgramError::Budget);
            }
            let face = render_face_ref_id(item.face, geometry.base_face);
            if !faces.iter().any(|candidate| candidate.face_id == face) {
                return Err(RowProgramError::Unsupported);
            }
            let plan = if text.is_empty() {
                DisplayTextRunMeasurement::PerChar
            } else {
                measurer.text_run_advances_px(text, face, geometry.metrics.char_width().max(1.0))
            };
            measurements.capture_item(&item, measurer, geometry.base_face);
            complete = matches!(item.kind, DisplayItemKind::RowBreak(_));
            operations.push((
                Operation::capture(
                    item,
                    geometry.base_face,
                    geometry.metrics.row_height(),
                    geometry.inherited_line_spacing,
                )?,
                plan,
            ));
        }
        if !complete {
            return Err(RowProgramError::Incomplete);
        }
        Ok(Self {
            geometry,
            operations,
            measurements,
            faces,
            limits,
        })
    }

    pub(crate) fn limits(&self) -> RowProgramLimits {
        self.limits
    }

    /// Cancellation is checked between bounded operations. Overflow rejects
    /// the complete row: wrapping must preserve producer rewind semantics and
    /// is intentionally not invented by this physical-line kernel.
    pub(crate) fn compute(
        mut self,
        cancelled: impl Fn() -> bool,
    ) -> Result<ComputedRow, RowProgramError> {
        let layout = self.geometry.layout();
        let mut row = new_display_row(&layout);
        let mut position = DisplayRowPosition::new(0.0, 0);
        let mut slots = Vec::new();
        let mut slot_heights = Vec::new();
        let mut source_start = None;
        let mut source_end = None;
        let mut terminator_width = self.geometry.metrics.char_width();
        let mut terminator_height = self.geometry.metrics.row_height();
        for (operation, plan) in self.operations {
            if cancelled() {
                return Err(RowProgramError::Cancelled);
            }
            let item = operation.into_item();
            source_start.get_or_insert_with(|| item.span.start.clone());
            source_end = Some(item.span.end.clone());
            let newline = if let DisplayItemKind::RowBreak(value) = item.kind {
                Some((
                    value,
                    render_face_ref_id(item.face, self.geometry.base_face),
                    item.box_vertical_edges,
                    item.box_run_membership,
                ))
            } else {
                None
            };
            // Source-slot height describes the active face at this item,
            // not the eventual maximum height of the complete row.
            let face = render_face_ref_id(item.face, self.geometry.base_face);
            let face_metrics = self
                .faces
                .iter()
                .find(|candidate| candidate.face_id == face)
                .map(|face| face.metrics)
                .ok_or(RowProgramError::Unsupported)?;
            let face_height = face_metrics.line_height_px();
            // Visible buffer layout installs the active face's minimum
            // extents before emitting an item. Preserve that descent even
            // when every glyph in the item is raised above the baseline.
            let minimum = self.measurements.face_vertical_metrics_px(face).unwrap_or(
                DisplayRowVerticalMetrics::new(face_height, face_metrics.ascent_px()),
            );
            let mut item_layout = layout.clone();
            item_layout.height_px = row.height_px.max(minimum.height_px());
            item_layout.ascent_px = row.ascent_px.max(minimum.ascent_px());
            let progress = DisplayRowProgressWriter::with_text_run_measurement_and_glyph_measurer_for_area_and_start_policy(
                &item_layout, &mut row, plan, &mut self.measurements, position, self.geometry.width,
                DisplayRowTextAreaOrigin::row_local(), GlyphArea::Text, DisplayRowAppendStartPolicy::ReconcileWithRowTail,
            ).push_item(item);
            if self.measurements.missing {
                return Err(RowProgramError::MissingMeasurement);
            }
            if progress.status() == DisplayRowAppendStatus::Clipped {
                return Err(RowProgramError::Overflow);
            }
            position = progress.end();
            slot_heights.extend(std::iter::repeat_n(face_height, progress.slots().len()));
            slots.extend(progress.slots().iter().cloned());
            if let Some((value, face, edges, membership)) = newline {
                if let Some(realized) = self
                    .faces
                    .iter()
                    .find(|candidate| candidate.face_id == face)
                {
                    terminator_width = realized
                        .metrics
                        .char_width_px(self.geometry.metrics.char_width());
                    terminator_height = realized.metrics.line_height_px();
                }
                let mode = match self.measurements.backend {
                    DisplayGeometryBackend::WindowSystemPixels => {
                        DisplayRowMeasurementMode::ConcreteFont
                    }
                    DisplayGeometryBackend::TerminalCells => {
                        DisplayRowMeasurementMode::LogicalCells
                    }
                };
                DisplayRowLineEndFinalizer::new(
                    value,
                    face,
                    self.geometry.width - position.x_px(),
                    self.geometry.metrics,
                    self.geometry.background,
                    mode,
                    edges,
                    membership,
                )
                .finalize(&mut row, &self.faces);
                if value.line_height != DisplayLineHeightPolicy::Default {
                    let resolved = crate::display_row::metrics::resolve_line_height(
                        value.line_height,
                        (row.height_px, row.ascent_px),
                        (face_metrics.line_height_px(), face_metrics.ascent_px()),
                        (
                            self.geometry.metrics.row_height(),
                            self.geometry.metrics.ascent(),
                        ),
                    );
                    terminator_height = resolved.newline_height;
                }
                // As in the canonical row transition, line spacing contributes
                // to logical descent after newline fills and hit cells are set.
                if mode.uses_concrete_font_geometry()
                    && value.line_height != DisplayLineHeightPolicy::ContentOnly
                {
                    row.line_spacing_px =
                        crate::display_row::spacing::ResolvedLineSpacing::from_pixels(
                            value
                                .line_spacing
                                .resolve(self.geometry.metrics.row_height(), 0.0),
                        )
                        .pixels();
                    row.height_px += row.line_spacing_px;
                }
            }
            if row.glyphs.iter().map(Vec::len).sum::<usize>() > self.limits.glyphs {
                return Err(RowProgramError::Budget);
            }
        }
        crate::glyph_row_writer::normalize_external_row(&mut row);
        Ok(ComputedRow {
            row,
            slots,
            slot_heights,
            source: SourceSpan::new(
                source_start.ok_or(RowProgramError::Incomplete)?,
                source_end.ok_or(RowProgramError::Incomplete)?,
            ),
            end: position,
            terminator_width,
            terminator_height,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display_row::face_state::DisplayRowGlyphMeasurer;
    use crate::font::metrics::FontMetricsService;
    use crate::glyph_advance::GlyphAdvanceQuantization;
    use crate::neovm_bridge::ResolvedFace;

    fn geometry(width: f32) -> RowProgramGeometry {
        RowProgramGeometry {
            inherited_line_spacing: 0.0,
            width,
            metrics: DisplayRowFallbackMetrics::from_default_face_extents(8.0, 16.0, 12.0),
            tabs: DisplayTabPolicy::every(4),
            base_face: FaceId::new(1),
            background: Color::BLACK,
        }
    }
    fn limits() -> RowProgramLimits {
        RowProgramLimits {
            items: 16,
            text_bytes: 4096,
            glyphs: 1024,
        }
    }
    fn items() -> Vec<DisplayItem> {
        let text = |id, text: &str| {
            DisplayItem::new(
                SourceSpan::synthetic(id, 0, text.chars().count()),
                RenderFaceRef::FaceId(FaceId::new(id as u32)),
                DisplayItemKind::TextRun(DisplayTextRun::independent(text)),
            )
        };
        let mut raised = text(2, "affinity 好 café á שלום سلام");
        raised.layout.raise = Some(0.25);
        let mut spaced = text(1, "a b\tc");
        spaced.layout.space_width = Some(1.7);
        vec![
            raised,
            spaced,
            DisplayItem::new(
                SourceSpan::synthetic(1, 40, 41),
                RenderFaceRef::FaceId(FaceId::new(1)),
                DisplayItemKind::RowBreak(DisplayRowBreak {
                    line_height: DisplayLineHeightPolicy::Default,
                    line_spacing: DisplayLineSpacingPolicy::Inherit,
                }),
            ),
        ]
    }
    fn faces() -> Vec<DisplayRowFace> {
        let base = ResolvedFace::default();
        let mut tall = base.clone();
        tall.font_size = 26.0;
        vec![
            DisplayRowFace::from_resolved(FaceId::new(1), &base),
            DisplayRowFace::from_resolved(FaceId::new(2), &tall),
        ]
    }

    #[test]
    fn complete_row_program_uses_exact_captured_fonts_on_worker() {
        fn require_send<T: Send + 'static>() {}
        require_send::<RowProgram>();
        require_send::<ComputedRow>();
        let faces = faces();
        let mut fonts = FontMetricsService::new();
        let mut measurer = DisplayRowGlyphMeasurer::with_mode(
            &faces,
            Some(&mut fonts),
            8.0,
            GlyphAdvanceQuantization::PreserveLogicalPixels,
            DisplayRowMeasurementMode::ConcreteFont,
        );
        let geometry = geometry(2000.0);
        let inputs = items();
        let job = RowProgram::capture(
            geometry.clone(),
            inputs.clone(),
            faces.clone(),
            &mut measurer,
            limits(),
        )
        .unwrap();
        // Independent synchronous writer call: no capture or replay adapter.
        let layout = geometry.layout();
        let mut expected = new_display_row(&layout);
        let mut position = DisplayRowPosition::new(0.0, 0);
        let mut slots = Vec::new();
        for input in inputs {
            // Match the buffer source's active-face installation before
            // handing the item to the independent synchronous glyph writer.
            let face = faces
                .iter()
                .find(|face| face.face_id == render_face_ref_id(input.face, geometry.base_face))
                .unwrap();
            let minimum = measurer.face_vertical_metrics_px(face.face_id).unwrap_or(
                DisplayRowVerticalMetrics::new(
                    face.metrics.line_height_px(),
                    face.metrics.ascent_px(),
                ),
            );
            let mut item_layout = layout.clone();
            item_layout.height_px = expected.height_px.max(minimum.height_px());
            item_layout.ascent_px = expected.ascent_px.max(minimum.ascent_px());
            let newline = match input.kind {
                DisplayItemKind::RowBreak(value) => Some(value),
                _ => None,
            };
            let progress = DisplayRowProgressWriter::with_glyph_measurer(
                &item_layout,
                &mut expected,
                &mut measurer,
                position,
                geometry.width,
            )
            .push_item(input);
            assert_ne!(progress.status(), DisplayRowAppendStatus::Clipped);
            position = progress.end();
            slots.extend(progress.slots().iter().cloned());
            if let Some(newline) = newline {
                DisplayRowLineEndFinalizer::new(
                    newline,
                    FaceId::new(1),
                    geometry.width - position.x_px(),
                    geometry.metrics,
                    geometry.background,
                    DisplayRowMeasurementMode::ConcreteFont,
                    Default::default(),
                    Default::default(),
                )
                .finalize(&mut expected, &faces);
            }
        }
        crate::glyph_row_writer::normalize_external_row(&mut expected);
        // Dropping the real font service before spawning prevents accidental
        // dependence on a live catalog or a fresh default-font worker.
        drop(measurer);
        drop(fonts);
        let actual = std::thread::spawn(move || job.compute(|| false))
            .join()
            .unwrap()
            .unwrap();
        assert_eq!(actual.row, expected);
        assert_eq!(actual.slots, slots);
    }

    #[test]
    fn incomplete_overbudget_cancelled_and_clipped_rows_are_never_admitted() {
        let faces = faces();
        let mut measurer = DisplayRowGlyphMeasurer::new(&faces, None, 8.0);
        assert!(matches!(
            RowProgram::capture(
                geometry(2000.0),
                items().into_iter().take(1),
                faces.clone(),
                &mut measurer,
                limits()
            ),
            Err(RowProgramError::Incomplete)
        ));
        for budget in [
            RowProgramLimits {
                items: 1,
                ..limits()
            },
            RowProgramLimits {
                text_bytes: 1,
                ..limits()
            },
            RowProgramLimits {
                glyphs: 1,
                ..limits()
            },
        ] {
            assert!(matches!(
                RowProgram::capture(
                    geometry(2000.0),
                    items(),
                    faces.clone(),
                    &mut measurer,
                    budget
                ),
                Err(RowProgramError::Budget)
            ));
        }
        let job = RowProgram::capture(
            geometry(2000.0),
            items(),
            faces.clone(),
            &mut measurer,
            limits(),
        )
        .unwrap();
        assert!(matches!(
            job.compute(|| true),
            Err(RowProgramError::Cancelled)
        ));
        let job = RowProgram::capture(
            geometry(8.0),
            items(),
            faces.clone(),
            &mut measurer,
            limits(),
        )
        .unwrap();
        assert!(matches!(
            job.compute(|| false),
            Err(RowProgramError::Overflow)
        ));
    }

    #[test]
    fn named_line_spacing_never_crosses_worker_boundary() {
        let _eval = neovm_core::emacs_core::Context::new();
        let faces = faces();
        let mut measurer = DisplayRowGlyphMeasurer::new(&faces, None, 8.0);
        let mut input = items();
        if let DisplayItemKind::RowBreak(value) = &mut input.last_mut().unwrap().kind {
            value.line_spacing = DisplayLineSpacingPolicy::Scale {
                factor: 1.5,
                reference: DisplayLineSpacingReference::Named(
                    neovm_core::emacs_core::Value::symbol("default"),
                ),
            };
        }
        assert!(matches!(
            RowProgram::capture(
                geometry(2000.0),
                input,
                faces.clone(),
                &mut measurer,
                limits()
            ),
            Err(RowProgramError::Unsupported)
        ));
    }
}
