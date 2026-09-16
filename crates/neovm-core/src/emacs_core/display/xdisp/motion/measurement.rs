//! Offscreen motion through the canonical row producer. Source newlines only
//! choose a safe backtracking region; they never answer a display-row count.

use super::*;
use crate::buffer::{Buffer, EmacsByteLen, EmacsBytePos};
use crate::window::{WindowLayoutQueryOutcome, WindowLayoutQueryScope};
use std::num::NonZeroUsize;

fn source_line_start(buffer: &Buffer, pos: EmacsBytePos) -> EmacsBytePos {
    let region = buffer.accessible_emacs_byte_region();
    buffer
        .prev_newline_emacs_byte(region.clamp(pos), region.start())
        .map_or(region.start(), |newline| {
            newline.add_len(EmacsByteLen::new(1))
        })
}

pub(super) fn backtrack(buffer: &Buffer, start: LispCharPos1, count: usize) -> LispCharPos1 {
    let begv = buffer.accessible_emacs_byte_region().start();
    let mut position = source_line_start(buffer, buffer.lisp_pos_to_emacs_byte_pos(start));
    for _ in 0..count {
        if position <= begv {
            break;
        }
        position = source_line_start(buffer, position.saturating_sub_len(EmacsByteLen::new(1)));
    }
    buffer.emacs_byte_pos_to_lisp_char_pos(position)
}

fn failed(message: &str) -> Flow {
    signal(LispCondition::Error, vec![Value::string(message)])
}

/// A row query is speculative: it never installs a presentation or moves a
/// window marker. All motion consumers share its identity/freshness checks.
pub(super) fn query(
    eval: &mut Context,
    frame: FrameId,
    window: WindowId,
    buffer: BufferId,
    start: LispCharPos1,
    count: NonZeroUsize,
) -> Result<Option<WindowDisplaySnapshot>, Flow> {
    eval.maybe_quit()?;
    let snapshot = match eval.query_window_layout_scope(
        frame,
        window,
        WindowLayoutQueryScope::Rows { start, count },
    ) {
        WindowLayoutQueryOutcome::Ready(query) => query
            .into_geometry()
            .ok_or_else(|| failed("Display motion query produced no rows"))?,
        WindowLayoutQueryOutcome::Unavailable => return Ok(None),
        WindowLayoutQueryOutcome::LayoutBusy => {
            return Err(failed(
                "Window layout query reentered an active layout callback",
            ));
        }
        WindowLayoutQueryOutcome::Failed(error) => return Err(failed(error.message())),
    };
    if motion_window(eval, Some(window), buffer) != Some((frame, window)) {
        return Err(failed("Window changed during display motion"));
    }
    if snapshot.layout_freshness.is_some_and(|freshness| {
        Some(freshness) != eval.window_display_snapshot_freshness(frame, window, buffer)
    }) {
        return Err(failed("Display motion query returned stale rows"));
    }
    Ok(Some(snapshot))
}

pub(super) fn resolve(
    eval: &mut Context,
    frame: FrameId,
    window: WindowId,
    request: MotionRequest,
) -> Result<Option<MeasuredMotion>, Flow> {
    let Some(buffer) = eval.buffers.get(request.buffer) else {
        return Ok(None);
    };
    let mut backtrack_lines = if request.rows < 0 {
        request.rows.unsigned_abs().min(64) as usize
    } else if request.rows == 0 {
        1
    } else {
        0
    };
    let mut start = backtrack(buffer, request.origin, backtrack_lines);
    let mut count = NonZeroUsize::new(64).expect("nonzero initial row budget");
    loop {
        let Some(snapshot) = query(eval, frame, window, request.buffer, start, count)? else {
            return Ok(None);
        };
        let (accessible, wrap) = motion_parameters(eval, request)
            .ok_or_else(|| failed("Buffer changed during display motion"))?;
        if let Some(motion) =
            vertical_motion_on_rows(&snapshot, request, accessible, wrap).settled()
        {
            return Ok(Some(motion));
        }
        // GNU backs up through source lines and measures forward. Expand
        // geometrically only when the measured rows prove the region too short.
        let rows = snapshot_text_rows(&snapshot);
        if rows.is_empty() {
            return Err(failed("Display motion measurement has no source rows"));
        }
        if request.rows < 0
            && snapshot_row_index_for_pos(&rows, request.origin).is_some()
            && start > accessible.start_lisp()
        {
            backtrack_lines = backtrack_lines.saturating_mul(2).max(1);
            let buffer = eval
                .buffers
                .get(request.buffer)
                .expect("validated source buffer");
            start = backtrack(buffer, start, backtrack_lines);
        } else if rows.last().and_then(|row| row.end_buffer_pos) == Some(accessible.end_lisp()) {
            // More rows cannot expose an origin missing from complete source
            // coverage. Do not double the allocation forever at EOB.
            return Err(failed(
                "Display motion origin is outside measured source coverage",
            ));
        }
        let Some(next_count) = count.get().checked_mul(2).and_then(NonZeroUsize::new) else {
            return Err(failed(
                "Display motion measurement exceeds the row address space",
            ));
        };
        count = next_count;
    }
}
