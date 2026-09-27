//! Bounded storage for accepted viewports outside the current window.
//!
//! Entries own their original face namespace and are admitted only after the
//! full layout key matches. This is content reuse, not a replay of old input or
//! old chrome. No live evaluator state is retained here.

use super::*;
use crate::frame_face_arena::PreparedFaceSnapshot;
use crate::incremental_layout::WindowDelta;
use std::collections::VecDeque;

const MAX_VIEWPORTS: usize = 8;
const MAX_ROWS: usize = 512;
const MAX_GLYPHS: usize = 65_536;

struct PreparedViewport {
    frame: neovm_core::window::FrameId,
    window: DisplayWindowId,
    retained: RetainedWindowMatrix,
    faces: PreparedFaceSnapshot,
    rows: usize,
    glyphs: usize,
}

#[derive(Default)]
pub(super) struct PreparedViewports {
    entries: VecDeque<PreparedViewport>,
}

impl PreparedViewports {
    #[cfg(test)]
    pub(super) fn insert_computed(
        &mut self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        retained: RetainedWindowMatrix,
        faces: PreparedFaceSnapshot,
    ) {
        let rows = retained.matrix.rows.len();
        let glyphs = retained
            .matrix
            .rows
            .iter()
            .flat_map(|row| &row.glyphs)
            .map(Vec::len)
            .sum();
        if rows > MAX_ROWS || glyphs > MAX_GLYPHS {
            return;
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
            faces,
            rows,
            glyphs,
        });
        self.trim();
    }

    fn trim(&mut self) {
        while self.entries.len() > MAX_VIEWPORTS
            || self.entries.iter().map(|entry| entry.rows).sum::<usize>() > MAX_ROWS
            || self.entries.iter().map(|entry| entry.glyphs).sum::<usize>() > MAX_GLYPHS
        {
            self.entries.pop_front();
        }
    }

    pub(super) fn replay(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        key: &RetainedWindowKey,
    ) -> Option<(CursorOnlyReplay, PreparedFaceSnapshot)> {
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

    /// Reuse coverage from an older viewport when it supplies more rows than
    /// the immediately preceding presentation. In particular, a backward
    /// scroll can be a forward replay within an older prepared page.
    pub(super) fn scroll_replay(
        &self,
        frame: neovm_core::window::FrameId,
        window: DisplayWindowId,
        key: &RetainedWindowKey,
        minimum_reused: usize,
    ) -> Option<(ScrollReplay, PreparedFaceSnapshot)> {
        self.entries.iter().rev().find_map(|entry| {
            if entry.frame != frame
                || entry.window != window
                || entry.retained.key.window_start >= key.window_start
            {
                return None;
            }
            // Avoid allocating shifted glyph rows unless this entry can beat
            // the current plan. The canonical replay builder still decides
            // eligibility, including row boundaries, fringes and wrapping.
            let potential = entry
                .retained
                .matrix
                .rows
                .iter()
                .filter(|row| {
                    row.enabled
                        && !RetainedWindowMatrix::is_chrome_role(row.role)
                        && row.start_charpos as i64 >= key.window_start
                })
                .count();
            if potential <= minimum_reused {
                return None;
            }
            let replay = entry.retained.scroll_replay(key)?;
            (replay.reused_rows.len() > minimum_reused).then(|| (replay, entry.faces.clone()))
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
                faces: faces.prepared_snapshot(),
                rows,
                glyphs,
            });
        }
        self.trim();
    }
}
