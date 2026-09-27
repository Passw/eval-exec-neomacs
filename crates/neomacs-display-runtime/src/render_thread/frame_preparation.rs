//! Prepare immutable presentations without occupying the native event loop.
//!
//! The active scene stays available while a worker materializes the next one.
//! The result owns glyphs, damage, and continuity metadata from one revision.

use super::frame_compositor::{ReflowImprintsByWindow, ScrollAnchorsByWindow};
use crossbeam_channel::{Receiver, Sender, bounded, select};
use neomacs_display_protocol::{FrameGlyphBuffer, SealedFramePresentation};

pub(super) struct PreparedFrame {
    pub state: SealedFramePresentation,
    pub frame: FrameGlyphBuffer,
    pub damage: neomacs_renderer_wgpu::FrameRowDamage,
    pub scroll: ScrollAnchorsByWindow,
    pub reflow: ReflowImprintsByWindow,
    pub received: neomacs_display_protocol::frame_time::EventTime,
}

impl PreparedFrame {
    /// Damage is relative to the producer's preceding presentation. If that
    /// presentation was skipped, conservatively rebuild every row instead of
    /// treating a delta against an unseen scene as a delta against the active one.
    pub(super) fn invalidate_row_reuse(&mut self) {
        for window in self.damage.windows.values_mut() {
            for row in &mut window.rows {
                row.damage = neomacs_display_protocol::glyph_matrix::RowDamage::New;
            }
        }
    }

    pub(super) fn new(state: SealedFramePresentation) -> Self {
        let received = neomacs_display_protocol::frame_time::observe_platform_now();
        let scroll = super::frame_compositor::continuity::scroll::anchors_by_window(&state);
        let reflow = super::frame_compositor::continuity::reflow::imprints_by_window(&state);
        let damage = neomacs_renderer_wgpu::FrameRowDamage::from_display_state(&state);
        let frame = state.materialize();
        Self {
            state,
            frame,
            damage,
            scroll,
            reflow,
            received,
        }
    }
}

pub(super) struct FramePreparation {
    ready: Receiver<PreparedFrame>,
    stop: Sender<()>,
}

impl FramePreparation {
    /// Backpressure bounds materialized results to two queued frames and one
    /// in flight. The evaluator's existing presentation channel remains the
    /// input owner; this worker is its only receiver in asynchronous mode.
    pub(super) fn spawn(
        incoming: Receiver<SealedFramePresentation>,
        discarded: Sender<crate::thread_comm::InputEvent>,
        wake: impl Fn() + Send + 'static,
    ) -> std::io::Result<Self> {
        let (completed, ready) = bounded(2);
        let (stop, stopped) = bounded(1);
        std::thread::Builder::new().name("neomacs-frame-prepare".into()).spawn(move || {
            loop {
                let state = select! {
                    recv(stopped) -> _ => break,
                    recv(incoming) -> state => match state { Ok(state) => state, Err(_) => break },
                };
                // Collapse an already queued burst per logical frame before
                // doing expensive materialization. Bound each drain so a busy
                // producer cannot starve preparation indefinitely.
                let mut batch = vec![state];
                let mut coalesced = std::collections::HashSet::new();
                for state in incoming.try_iter().take(31) {
                    let id = state.frame_placement.frame();
                    if let Some(index) = batch.iter().position(|old| old.frame_placement.frame() == id) {
                        let old = batch.remove(index);
                        coalesced.insert(id);
                        let event = crate::thread_comm::InputEvent::PresentationDiscarded {
                            presentation: old.presentation().get(), emacs_frame_id: id.get(),
                        };
                        select! {
                            recv(stopped) -> _ => return,
                            send(discarded, event) -> sent => if sent.is_err() { return; },
                        }
                    }
                    batch.push(state);
                }
                for state in batch {
                    let skipped_predecessor = coalesced.contains(&state.frame_placement.frame());
                    let mut prepared = PreparedFrame::new(state);
                    if skipped_predecessor {
                        prepared.invalidate_row_reuse();
                    }
                    select! {
                        recv(stopped) -> _ => return,
                        send(completed, prepared) -> sent => if sent.is_err() { return; },
                    }
                    wake();
                }
            }
        })?;
        Ok(Self { ready, stop })
    }

    pub(super) fn ready(&self) -> impl Iterator<Item = PreparedFrame> + '_ {
        self.ready.try_iter()
    }
}

impl Drop for FramePreparation {
    fn drop(&mut self) {
        // Never join from the native event loop. The worker owns only immutable
        // data and exits after its current finite materialization, even if its
        // result queue is full or the evaluator still owns the input sender.
        let _ = self.stop.try_send(());
    }
}
