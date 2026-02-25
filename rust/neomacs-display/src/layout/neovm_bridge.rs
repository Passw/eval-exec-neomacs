//! Bridge between neovm-core data types and the layout engine.
//!
//! Provides functions to build `WindowParams` and `FrameParams` from
//! the Rust Evaluator's state, replacing C FFI data sources.

use neovm_core::buffer::Buffer;
use neovm_core::elisp::{Evaluator, Value};
use neovm_core::window::{Frame, FrameId, Window};

use super::types::{FrameParams, WindowParams};
use crate::core::types::Rect;

/// Build `FrameParams` from a neovm-core `Frame`.
pub fn frame_params_from_neovm(frame: &Frame) -> FrameParams {
    FrameParams {
        width: frame.width as f32,
        height: frame.height as f32,
        char_width: frame.char_width,
        char_height: frame.char_height,
        font_pixel_size: frame.font_pixel_size,
        background: 0x00000000,         // TODO: read from frame face table
        vertical_border_fg: 0x00808080, // TODO: read from face table
        right_divider_width: 0,         // TODO: read from frame parameters
        bottom_divider_width: 0,
        divider_fg: 0x00444444,
        divider_first_fg: 0x00555555,
        divider_last_fg: 0x00333333,
    }
}

/// Helper: extract an integer buffer-local variable.
fn buffer_local_int(buffer: &Buffer, name: &str, default: i64) -> i64 {
    match buffer.properties.get(name) {
        Some(Value::Int(n)) => *n,
        _ => default,
    }
}

/// Helper: extract a boolean buffer-local variable (nil = false, anything else = true).
fn buffer_local_bool(buffer: &Buffer, name: &str) -> bool {
    match buffer.properties.get(name) {
        Some(Value::Nil) | None => false,
        Some(_) => true,
    }
}

/// Build `WindowParams` from neovm-core window + buffer + frame data.
///
/// `is_selected` indicates whether this window is the frame's selected window.
/// `is_minibuffer` indicates whether this is the minibuffer window.
///
/// Returns `None` for internal (non-leaf) windows.
pub fn window_params_from_neovm(
    window: &Window,
    buffer: &Buffer,
    frame: &Frame,
    is_selected: bool,
    is_minibuffer: bool,
) -> Option<WindowParams> {
    // Only leaf windows can be laid out.
    let (win_id, _buf_id, bounds, window_start, point, hscroll, margins, fringes) = match window {
        Window::Leaf {
            id,
            buffer_id,
            bounds,
            window_start,
            point,
            hscroll,
            margins,
            fringes,
            ..
        } => (*id, *buffer_id, bounds, *window_start, *point, *hscroll, *margins, *fringes),
        Window::Internal { .. } => return None,
    };

    let char_width = frame.char_width;
    let char_height = frame.char_height;

    // Convert neovm-core Rect to display Rect (same fields, different types).
    let display_bounds = Rect::new(bounds.x, bounds.y, bounds.width, bounds.height);

    // Compute text bounds (bounds minus fringes and margins).
    let left_fringe = fringes.0 as f32;
    let right_fringe = fringes.1 as f32;
    let left_margin = margins.0 as f32 * char_width;
    let right_margin = margins.1 as f32 * char_width;
    let text_x = bounds.x + left_fringe + left_margin;
    let text_width = (bounds.width - left_fringe - right_fringe - left_margin - right_margin).max(0.0);
    // FIXME: When header_line_height / tab_line_height are implemented,
    // text_bounds.y must be shifted down and text_bounds.height reduced
    // to account for header-line and tab-line at the top plus mode-line
    // at the bottom.
    let text_bounds = Rect::new(text_x, bounds.y, text_width, bounds.height);

    // Read buffer-local variables.
    let truncate_lines = buffer_local_bool(buffer, "truncate-lines");
    let word_wrap = buffer_local_bool(buffer, "word-wrap");
    let tab_width = buffer_local_int(buffer, "tab-width", 8) as i32;

    // Mode-line: non-minibuffer windows get one line of mode-line.
    let mode_line_height = if is_minibuffer { 0.0 } else { char_height };

    Some(WindowParams {
        window_id: win_id.0 as i64,
        buffer_id: buffer.id.0,
        bounds: display_bounds,
        text_bounds,
        selected: is_selected,
        is_minibuffer,
        window_start: window_start as i64,
        window_end: 0, // filled after layout
        point: point as i64,
        buffer_size: buffer.zv as i64,
        buffer_begv: buffer.begv as i64,
        hscroll: hscroll as i32,
        vscroll: 0,
        truncate_lines,
        word_wrap,
        tab_width,
        default_fg: 0x00FFFFFF,    // TODO: read from face table
        default_bg: 0x00000000,    // TODO: read from face table
        char_width,
        char_height,
        font_pixel_size: frame.font_pixel_size,
        // FIXME: 0.8 ratio is a rough heuristic.  Store actual font_ascent on
        // Frame (via FontMetricsService) when Phase 3 face/font integration lands.
        font_ascent: frame.font_pixel_size * 0.8,
        mode_line_height,
        header_line_height: 0.0, // TODO: read from buffer/window vars
        tab_line_height: 0.0,    // TODO: read from buffer/window vars
        cursor_type: 0,          // filled box default
        cursor_bar_width: 2,
        left_fringe_width: left_fringe,
        right_fringe_width: right_fringe,
        indicate_empty_lines: 0,
        show_trailing_whitespace: buffer_local_bool(buffer, "show-trailing-whitespace"),
        trailing_ws_bg: 0,
        fill_column_indicator: buffer_local_int(
            buffer,
            "display-fill-column-indicator-column",
            0,
        ) as i32,
        fill_column_indicator_char: '|',
        fill_column_indicator_fg: 0,
        extra_line_spacing: 0.0,
        cursor_in_non_selected: true,
        selective_display: 0,
        escape_glyph_fg: 0,
        nobreak_char_display: 0,
        nobreak_char_fg: 0,
        glyphless_char_fg: 0,
        wrap_prefix: Vec::new(),
        line_prefix: Vec::new(),
        left_margin_width: left_margin,
        right_margin_width: right_margin,
    })
}

/// Collect all leaf windows from a frame (including minibuffer) and build
/// `WindowParams` for each.
///
/// Returns `(FrameParams, Vec<WindowParams>)`, or `None` if the frame does
/// not exist.
pub fn collect_layout_params(
    evaluator: &Evaluator,
    frame_id: FrameId,
) -> Option<(FrameParams, Vec<WindowParams>)> {
    let frame = evaluator.frame_manager().get(frame_id)?;
    let frame_params = frame_params_from_neovm(frame);

    let mut window_params = Vec::new();

    // Collect leaf windows from the root window tree.
    let leaf_ids = frame.root_window.leaf_ids();
    for win_id in &leaf_ids {
        let Some(window) = frame.root_window.find(*win_id) else { continue };
        let Some(buf_id) = window.buffer_id() else { continue };
        let Some(buffer) = evaluator.buffer_manager().get(buf_id) else { continue };
        let is_selected = frame.selected_window == *win_id;
        if let Some(wp) = window_params_from_neovm(window, buffer, frame, is_selected, false) {
            window_params.push(wp);
        }
    }

    // Add minibuffer window if present.
    if let Some(mini_leaf) = &frame.minibuffer_leaf {
        let buf_id = mini_leaf.buffer_id();
        let buffer = buf_id.and_then(|id| evaluator.buffer_manager().get(id));
        if let Some(buffer) = buffer {
            let is_selected = frame.selected_window == mini_leaf.id();
            if let Some(wp) = window_params_from_neovm(mini_leaf, buffer, frame, is_selected, true) {
                window_params.push(wp);
            }
        }
    }

    Some((frame_params, window_params))
}

#[cfg(test)]
mod tests {
    use super::*;
    use neovm_core::buffer::{BufferId, BufferManager};
    use neovm_core::window::{FrameManager, Rect as NeoRect, WindowId};

    /// Create a minimal Evaluator-like test fixture (FrameManager + BufferManager)
    /// and verify `collect_layout_params` produces correct output.
    #[test]
    fn test_collect_layout_params_basic() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();

        // Create a buffer.
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*test*");

        // Create a frame with that buffer.
        let frame_id = evaluator
            .frame_manager_mut()
            .create_frame("test-frame", 800, 600, buf_id);

        // Set some frame font metrics.
        if let Some(frame) = evaluator.frame_manager_mut().get_mut(frame_id) {
            frame.font_pixel_size = 14.0;
            frame.char_width = 7.0;
            frame.char_height = 14.0;
        }

        let (fp, wps) = collect_layout_params(&evaluator, frame_id)
            .expect("collect_layout_params should succeed");

        // Check FrameParams.
        assert_eq!(fp.width, 800.0);
        assert_eq!(fp.height, 600.0);
        assert_eq!(fp.char_width, 7.0);
        assert_eq!(fp.char_height, 14.0);
        assert_eq!(fp.font_pixel_size, 14.0);

        // Should have 1 root leaf + 1 minibuffer = 2 windows.
        assert_eq!(wps.len(), 2, "expected root leaf + minibuffer");

        // First window: root leaf (not minibuffer).
        let root_wp = &wps[0];
        assert!(!root_wp.is_minibuffer);
        assert!(root_wp.selected); // first window is selected by default
        assert_eq!(root_wp.char_width, 7.0);
        assert_eq!(root_wp.char_height, 14.0);
        assert_eq!(root_wp.mode_line_height, 14.0); // non-minibuffer gets mode-line

        // Second window: minibuffer.
        let mini_wp = &wps[1];
        assert!(mini_wp.is_minibuffer);
        assert!(!mini_wp.selected);
        assert_eq!(mini_wp.mode_line_height, 0.0); // minibuffer has no mode-line
    }

    #[test]
    fn test_frame_params_from_neovm() {
        let mut buf_mgr = BufferManager::new();
        let buf_id = buf_mgr.create_buffer("*scratch*");
        let mut frame_mgr = FrameManager::new();
        let fid = frame_mgr.create_frame("test", 1024, 768, buf_id);
        let frame = frame_mgr.get(fid).unwrap();

        let fp = frame_params_from_neovm(frame);
        assert_eq!(fp.width, 1024.0);
        assert_eq!(fp.height, 768.0);
    }

    #[test]
    fn test_window_params_from_neovm_internal_returns_none() {
        use neovm_core::window::SplitDirection;

        let internal = Window::Internal {
            id: WindowId(99),
            direction: SplitDirection::Vertical,
            children: vec![],
            bounds: NeoRect::new(0.0, 0.0, 100.0, 100.0),
        };
        let buf = neovm_core::buffer::Buffer::new(BufferId(1), "*test*".to_string());
        let mut frame_mgr = FrameManager::new();
        let fid = frame_mgr.create_frame("test", 800, 600, BufferId(1));
        let frame = frame_mgr.get(fid).unwrap();

        let result = window_params_from_neovm(&internal, &buf, frame, false, false);
        assert!(result.is_none(), "Internal windows should return None");
    }

    #[test]
    fn test_window_params_buffer_locals() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*locals*");

        // Set buffer-local variables.
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.properties
                .insert("truncate-lines".to_string(), Value::True);
            buf.properties
                .insert("tab-width".to_string(), Value::Int(4));
            buf.properties
                .insert("word-wrap".to_string(), Value::Nil);
        }

        let frame_id = evaluator
            .frame_manager_mut()
            .create_frame("test", 800, 600, buf_id);

        let (_, wps) = collect_layout_params(&evaluator, frame_id).unwrap();

        // The root window should pick up the buffer-local vars.
        let wp = &wps[0];
        assert!(wp.truncate_lines);
        assert!(!wp.word_wrap);
        assert_eq!(wp.tab_width, 4);
    }

    #[test]
    fn test_window_params_fringes_and_margins() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*fringe*");
        let frame_id = evaluator
            .frame_manager_mut()
            .create_frame("test", 800, 600, buf_id);

        // Set fringes and margins on the root window.
        if let Some(frame) = evaluator.frame_manager_mut().get_mut(frame_id) {
            frame.char_width = 8.0;
            if let Some(win) = frame.selected_window_mut() {
                if let Window::Leaf {
                    fringes, margins, ..
                } = win
                {
                    *fringes = (10, 12);
                    *margins = (2, 3);
                }
            }
        }

        let (_, wps) = collect_layout_params(&evaluator, frame_id).unwrap();
        let wp = &wps[0];

        assert_eq!(wp.left_fringe_width, 10.0);
        assert_eq!(wp.right_fringe_width, 12.0);
        assert_eq!(wp.left_margin_width, 16.0);  // 2 * 8.0
        assert_eq!(wp.right_margin_width, 24.0); // 3 * 8.0

        // text_bounds should be narrower by fringes + margins.
        let expected_text_x = wp.bounds.x + 10.0 + 16.0;
        assert!((wp.text_bounds.x - expected_text_x).abs() < 0.01);
    }

    #[test]
    fn test_collect_nonexistent_frame() {
        let evaluator = neovm_core::elisp::Evaluator::new();
        let result = collect_layout_params(&evaluator, FrameId(999999));
        assert!(result.is_none());
    }
}
