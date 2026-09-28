//! Input-driven body projection over immutable, certified scroll coverage.

use neomacs_display_protocol::input_progress::InputReceipt;
use neomacs_display_protocol::scroll_coverage::ScrollSurface;
use neomacs_display_protocol::{
    FrameGlyphBuffer, PresentationFramePoint, PresentedHit, PresentedHitError,
};
use std::sync::Arc;

#[derive(Default)]
pub(in crate::render_thread) struct InputScroll {
    active: Option<PendingScroll>,
    /// Only submitted pixels may change pointer observations.
    submitted: Option<(Arc<ScrollSurface>, f32)>,
    staged: Option<Option<(Arc<ScrollSurface>, f32)>>,
    staged_tokens: Vec<neomacs_display_protocol::input_latency::InputToken>,
}

struct PendingScroll {
    surface: Arc<ScrollSurface>,
    offset: f32,
    receipts: Vec<InputReceipt>,
    tokens: Vec<neomacs_display_protocol::input_latency::InputToken>,
}

impl InputScroll {
    pub(in crate::render_thread) fn push(
        &mut self,
        frame: &FrameGlyphBuffer,
        x: f32,
        y: f32,
        delta: f32,
        receipt: InputReceipt,
        token: Option<neomacs_display_protocol::input_latency::InputToken>,
    ) -> bool {
        if !delta.is_finite() || delta == 0.0 || receipt.cancelled() {
            return false;
        }
        let Some(surface) = frame.scroll_surfaces.iter().find(|surface| {
            let coverage = surface.coverage();
            let viewport = coverage.viewport;
            coverage.predict_pixels
                && delta.abs() < viewport.height
                && x >= viewport.x
                && x < viewport.right()
                && y >= viewport.y
                && y < viewport.bottom()
        }) else {
            return false;
        };
        if self
            .active
            .as_ref()
            .is_none_or(|active| active.surface.coverage().epoch != surface.coverage().epoch)
        {
            self.active = Some(PendingScroll {
                surface: surface.clone(),
                offset: 0.0,
                receipts: Vec::new(),
                tokens: Vec::new(),
            });
        }
        let active = self.active.as_mut().unwrap();
        if active.receipts.len() >= 128 {
            self.active = None;
            return true;
        }
        let offset = surface.clamp_offset(active.offset + delta);
        if offset != active.offset
            && let Some(token) = token
        {
            active.tokens.push(token);
        }
        active.offset = offset;
        active.receipts.push(receipt);
        true
    }

    /// Carry the same visual target across intermediate command redisplays.
    /// Full completion always returns authority to the evaluator, including
    /// no-op commands. A changed revision/policy/geometry cancels immediately.
    pub(in crate::render_thread) fn reconcile(
        &mut self,
        frame: Option<&FrameGlyphBuffer>,
    ) -> Option<neomacs_display_protocol::DisplayWindowId> {
        let mut active = self.active.take()?;
        let window = active.surface.coverage().content.window_id;
        let Some(frame) = frame else {
            return Some(window);
        };
        active.receipts.retain(|receipt| {
            !receipt.acknowledged_by(&frame.input_checkpoint) && !receipt.cancelled()
        });
        if active.receipts.is_empty() {
            return Some(window);
        }
        let Some(next) = frame.scroll_surfaces.iter().find(|surface| {
            surface.coverage().epoch == active.surface.coverage().epoch
                && surface.coverage().predict_pixels
                && surface.coverage().viewport == active.surface.coverage().viewport
        }) else {
            return Some(window);
        };
        let old = &active.surface.coverage().content;
        let new = &next.coverage().content;
        let common = new
            .matrix
            .rows
            .iter()
            .filter(|row| row.enabled)
            .find_map(|row| {
                old.matrix
                    .rows
                    .iter()
                    .find(|previous| {
                        previous.enabled
                            && previous.start_charpos == row.start_charpos
                            && previous.end_charpos == row.end_charpos
                    })
                    .map(|previous| {
                        old.text_pixel_bounds.y + previous.pixel_y
                            - active.surface.coverage().origin
                            - new.text_pixel_bounds.y
                            - row.pixel_y
                            + next.coverage().origin
                    })
            });
        let Some(displacement) = common else {
            return Some(window);
        };
        active.offset = next.clamp_offset(active.offset - displacement);
        active.surface = next.clone();
        self.active = Some(active);
        Some(window)
    }

    pub(in crate::render_thread) fn paint(&mut self, frame: &mut FrameGlyphBuffer) {
        self.staged = Some(None);
        self.staged_tokens.clear();
        if self
            .active
            .as_ref()
            .is_some_and(|active| active.receipts.iter().any(InputReceipt::cancelled))
        {
            self.active = None;
        }
        let Some(active) = &self.active else {
            return;
        };
        if active.surface.coverage().hit_index.presentation() != frame.presentation_id {
            return;
        }
        active.surface.paint(frame, active.offset);
        if active.offset != 0.0 {
            self.staged_tokens.extend_from_slice(&active.tokens);
        }
        // Until point has been adjusted by the command, suppress its caret
        // rather than attach an authoritative slot to a different source row.
        let window = active.surface.coverage().content.window_id;
        frame
            .window_cursors
            .retain(|cursor| cursor.window_id != window);
        self.staged = Some(Some((active.surface.clone(), active.offset)));
    }

    pub(in crate::render_thread) fn active(&self) -> bool {
        self.active.is_some()
    }

    pub(in crate::render_thread) fn staged_tokens(
        &self,
    ) -> &[neomacs_display_protocol::input_latency::InputToken] {
        &self.staged_tokens
    }

    pub(in crate::render_thread) fn submit(&mut self) {
        self.staged_tokens.clear();
        if let Some(staged) = self.staged.take() {
            if let Some((surface, offset)) = &staged {
                tracing::debug!(target: "neomacs_display_runtime::input_scroll", window = surface.coverage().content.window_id.get(), offset, "submitted input-driven scroll");
            }
            self.submitted = staged;
        }
    }

    pub(in crate::render_thread) fn hit(
        &self,
        point: PresentationFramePoint,
    ) -> Option<Result<Option<PresentedHit>, PresentedHitError>> {
        let (surface, offset) = self.submitted.as_ref()?;
        if surface.coverage().hit_index.presentation() != point.presentation() {
            return None;
        }
        let viewport = surface.coverage().viewport;
        if point.x() < viewport.x
            || point.x() >= viewport.right()
            || point.y() < viewport.y
            || point.y() >= viewport.bottom()
        {
            return None;
        }
        Some(surface.hit(point, *offset))
    }
}

#[cfg(test)]
mod tests;
