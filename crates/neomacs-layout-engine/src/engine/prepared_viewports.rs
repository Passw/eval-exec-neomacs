//! Bounded storage for accepted viewports outside the current window.
//!
//! Entries own their original face namespace and are admitted only after the
//! full layout key matches. This is content reuse, not a replay of old input or
//! old chrome. No live evaluator state is retained here.

use super::*;
use crate::incremental_layout::WindowDelta;
use std::collections::VecDeque;

const MAX_VIEWPORTS: usize = 8;
const MAX_ROWS: usize = 512;
const MAX_GLYPHS: usize = 65_536;

struct PreparedViewport {
    frame: neovm_core::window::FrameId,
    window: DisplayWindowId,
    retained: RetainedWindowMatrix,
    faces: FrameFaceArena,
    rows: usize,
    glyphs: usize,
}

#[derive(Default)]
pub(super) struct PreparedViewports {
    entries: VecDeque<PreparedViewport>,
}

impl PreparedViewports {
    pub(super) fn replay(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        key: &RetainedWindowKey,
    ) -> Option<(CursorOnlyReplay, FrameFaceArena)> {
        self.entries.iter().rev().find_map(|entry| {
            if entry.frame != frame
                || entry.window != window
                || entry.retained.key.window_start != key.window_start
            {
                return None;
            }
            let replay = entry.retained.cursor_only_replay(key).ok()?;
            // The cursor builder deliberately leaves chrome empty. Even when
            // revisiting a page, mode-line Lisp must run against today's state.
            Some((replay, entry.faces.clone()))
        })
    }

    pub(super) fn accept(
        &mut self,
        frame: neovm_core::window::FrameId,
        previous: rustc_hash::FxHashMap<DisplayWindowId, RetainedWindowMatrix>,
        next: &rustc_hash::FxHashMap<DisplayWindowId, RetainedWindowMatrix>,
        faces: Option<&FrameFaceArena>,
    ) {
        self.entries.retain(|entry| {
            if entry.frame != frame {
                return true;
            }
            next.get(&entry.window).is_some_and(|current| {
                let delta = WindowDelta::between(&entry.retained.key, &current.key);
                !delta.text_changed && !delta.properties_changed && !delta.overlays_changed
                    && !delta.other_changed
                    // The current viewport is already retained by the engine.
                    && delta.window_start_moved
            })
        });
        let Some(faces) = faces else {
            return;
        };
        for (window, retained) in previous {
            let Some(current) = next.get(&window) else {
                continue;
            };
            if retained.validity != MatrixValidity::Valid
                || !RetainedWindowKey::scroll_eligible(&retained.key, &current.key)
            {
                continue;
            }
            let rows = retained.matrix.rows.len();
            let glyphs = retained
                .matrix
                .rows
                .iter()
                .flat_map(|row| row.glyphs.iter())
                .map(Vec::len)
                .sum::<usize>();
            if rows > MAX_ROWS || glyphs > MAX_GLYPHS {
                continue;
            }
            self.entries.retain(|entry| {
                entry.frame != frame
                    || entry.window != window
                    || entry.retained.key.window_start != retained.key.window_start
            });
            self.entries.push_back(PreparedViewport {
                frame,
                window,
                retained,
                faces: faces.clone(),
                rows,
                glyphs,
            });
        }
        while self.entries.len() > MAX_VIEWPORTS
            || self.entries.iter().map(|entry| entry.rows).sum::<usize>() > MAX_ROWS
            || self.entries.iter().map(|entry| entry.glyphs).sum::<usize>() > MAX_GLYPHS
        {
            self.entries.pop_front();
        }
    }
}
