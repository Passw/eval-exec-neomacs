//! Exact, bounded geometry reuse for synchronous queries. These entries are
//! observations, never presentations or a source of retained window markers.
use neovm_core::{
    emacs_core::Context,
    window::{FrameId, WindowId, WindowLayoutQuery, WindowLayoutQueryScope},
};
use std::collections::VecDeque;

mod placement;

const MAX_ENTRIES: usize = 4;
const MAX_ROWS: usize = 256;
const MAX_POINTS: usize = 16_384;

struct Entry {
    frame: FrameId,
    window: WindowId,
    scope: WindowLayoutQueryScope,
    source_point: neovm_core::buffer::LispCharPos1,
    collections: neovm_core::tagged::mutate::LispCollectionRevision,
    query: WindowLayoutQuery,
}

#[derive(Default)]
pub(super) struct QueryCache {
    entries: VecDeque<Entry>,
}

impl QueryCache {
    pub(super) fn clear(&mut self) {
        self.entries.clear();
    }

    pub(super) fn get(
        &self,
        evaluator: &Context,
        frame: FrameId,
        window: WindowId,
        scope: WindowLayoutQueryScope,
    ) -> Option<WindowLayoutQuery> {
        let buffer = evaluator
            .frame_manager()
            .get(frame)?
            .find_window(window)?
            .buffer_id()?;
        let current = evaluator.window_display_snapshot_freshness(frame, window, buffer)?;
        let source_point = evaluator
            .buffer_manager()
            .get(buffer)?
            .point_lisp_char_pos();
        self.entries.iter().rev().find_map(|entry| {
            if !(entry.frame == frame
                && entry.window == window
                && entry.scope == scope
                && entry.source_point == source_point
                && entry.collections
                    == neovm_core::tagged::mutate::LispCollectionRevision::current())
            {
                return None;
            }
            if entry.query.geometry()?.layout_freshness.as_ref() == Some(&current) {
                return Some(entry.query.clone());
            }
            if scope != WindowLayoutQueryScope::Viewport {
                return None;
            }
            placement::reposition(&entry.query, &current, source_point)
        })
    }

    pub(super) fn remember(
        &mut self,
        evaluator: &Context,
        frame: FrameId,
        window: WindowId,
        scope: WindowLayoutQueryScope,
        query: &WindowLayoutQuery,
    ) {
        let Some(snapshot) = query.geometry() else {
            return;
        };
        // A frontend cache is not an evaluator GC root. Never retain Lisp
        // chrome objects here, and do not turn unbounded measurement requests
        // into persistent geometry allocations.
        if snapshot.layout_freshness.is_none()
            || !snapshot.chrome_strings.is_empty()
            || snapshot.rows.len() > MAX_ROWS
            || snapshot.body_rows.len() > MAX_ROWS
            || snapshot.points.len() > MAX_POINTS
        {
            return;
        }
        // Direct query clients need not have synchronized the selected
        // window's saved point yet. The producer also reads the source point.
        let Some(buffer) = evaluator
            .frame_manager()
            .get(frame)
            .and_then(|frame| frame.find_window(window))
            .and_then(|window| window.buffer_id())
            .and_then(|buffer| evaluator.buffer_manager().get(buffer))
        else {
            return;
        };
        let source_point = buffer.point_lisp_char_pos();
        self.entries
            .retain(|entry| entry.frame != frame || entry.window != window || entry.scope != scope);
        self.entries.push_back(Entry {
            frame,
            window,
            scope,
            source_point,
            collections: neovm_core::tagged::mutate::LispCollectionRevision::current(),
            query: query.clone(),
        });
        while self.entries.len() > MAX_ENTRIES {
            self.entries.pop_front();
        }
    }
}
