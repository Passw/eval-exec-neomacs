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

/// Buffer accessor for the layout engine.
///
/// Wraps a reference to a neovm-core `Buffer` and provides the operations
/// that the layout engine needs: text byte copying, position conversion,
/// and line counting.
pub struct RustBufferAccess<'a> {
    buffer: &'a Buffer,
}

impl<'a> RustBufferAccess<'a> {
    /// Create a new buffer accessor.
    pub fn new(buffer: &'a Buffer) -> Self {
        Self { buffer }
    }

    /// Convert a character position to a byte position.
    ///
    /// Wraps `GapBuffer::char_to_byte()`.
    pub fn charpos_to_bytepos(&self, charpos: i64) -> i64 {
        if charpos <= 0 {
            return 0;
        }
        self.buffer.text.char_to_byte(charpos as usize) as i64
    }

    /// Copy buffer text bytes in the range `[byte_from, byte_to)` into `out`.
    ///
    /// Uses the efficient `copy_bytes_to` method on the gap buffer.
    pub fn copy_text(&self, byte_from: i64, byte_to: i64, out: &mut Vec<u8>) {
        let from = (byte_from as usize).min(self.buffer.text.len());
        let to = (byte_to as usize).min(self.buffer.text.len());
        if from >= to {
            out.clear();
            return;
        }
        self.buffer.text.copy_bytes_to(from, to, out);
    }

    /// Count the number of newlines in `[byte_from, byte_to)`.
    ///
    /// Used for line number display.
    pub fn count_lines(&self, byte_from: i64, byte_to: i64) -> i64 {
        let from = (byte_from as usize).min(self.buffer.text.len());
        let to = (byte_to as usize).min(self.buffer.text.len());
        if from >= to {
            return 0;
        }
        // Count newlines by iterating byte by byte
        let mut count: i64 = 0;
        for pos in from..to {
            if self.buffer.text.byte_at(pos) == b'\n' {
                count += 1;
            }
        }
        count
    }

    /// Get the buffer's narrowed beginning (begv) as byte position.
    pub fn begv(&self) -> i64 {
        self.buffer.begv as i64
    }

    /// Get the buffer's narrowed end (zv) as byte position.
    pub fn zv(&self) -> i64 {
        self.buffer.zv as i64
    }

    /// Get point (cursor) byte position.
    pub fn point(&self) -> i64 {
        self.buffer.pt as i64
    }

    /// Whether the buffer has been modified.
    pub fn modified(&self) -> bool {
        self.buffer.modified
    }

    /// Buffer name.
    pub fn name(&self) -> &str {
        &self.buffer.name
    }

    /// Buffer file name, if any.
    pub fn file_name(&self) -> Option<&str> {
        self.buffer.file_name.as_deref()
    }

    /// Get the underlying neovm-core Buffer reference (for text property
    /// and overlay access in later tasks).
    pub fn inner(&self) -> &'a Buffer {
        self.buffer
    }
}

/// Text property and overlay accessor for the layout engine.
///
/// Wraps a reference to a neovm-core `Buffer` and provides query methods
/// for invisible text, display properties, overlay strings, and other
/// text property-based features.
pub struct RustTextPropAccess<'a> {
    buffer: &'a Buffer,
}

impl<'a> RustTextPropAccess<'a> {
    /// Create a new text property accessor.
    pub fn new(buffer: &'a Buffer) -> Self {
        Self { buffer }
    }

    /// Check if text at `charpos` is invisible.
    ///
    /// Returns `(is_invisible, next_visible_pos)`.
    /// `next_visible_pos` is the next char position where visibility might change.
    /// If no change is found, returns `buffer.zv` as the next boundary.
    pub fn check_invisible(&self, charpos: i64) -> (bool, i64) {
        let pos = charpos as usize;
        let invis = self.buffer.text_props.get_property(pos, "invisible");

        let is_invisible = match invis {
            Some(Value::Nil) | None => false,
            Some(_) => true, // Any non-nil value means invisible
        };

        // Find the next position where the invisible property changes
        let next_change = self
            .buffer
            .text_props
            .next_property_change(pos)
            .unwrap_or(self.buffer.zv);

        (is_invisible, next_change as i64)
    }

    /// Check for a display text property at `charpos`.
    ///
    /// Returns the display property value if present, along with the
    /// next position where display properties change.
    pub fn check_display_prop(&self, charpos: i64) -> (Option<&Value>, i64) {
        let pos = charpos as usize;
        let display = self.buffer.text_props.get_property(pos, "display");

        let next_change = self
            .buffer
            .text_props
            .next_property_change(pos)
            .unwrap_or(self.buffer.zv);

        (display, next_change as i64)
    }

    /// Check for line-spacing text property at `charpos`.
    ///
    /// Returns extra line spacing in pixels (0.0 if no property).
    pub fn check_line_spacing(&self, charpos: i64, base_height: f32) -> f32 {
        let pos = charpos as usize;
        match self.buffer.text_props.get_property(pos, "line-spacing") {
            Some(Value::Int(n)) => *n as f32,
            Some(Value::Float(f)) => {
                if *f < 1.0 {
                    // Fraction of base height
                    base_height * (*f as f32)
                } else {
                    *f as f32
                }
            }
            _ => 0.0,
        }
    }

    /// Collect overlay before-string and after-string at `charpos`.
    ///
    /// Before-strings come from overlays starting at charpos.
    /// After-strings come from overlays ending at charpos.
    ///
    /// Returns `(before_strings, after_strings)` where each is a Vec of
    /// (string_bytes, overlay_id) pairs.
    pub fn overlay_strings_at(&self, charpos: i64) -> (Vec<(Vec<u8>, u64)>, Vec<(Vec<u8>, u64)>) {
        let pos = charpos as usize;
        let mut before = Vec::new();
        let mut after = Vec::new();

        // Get all overlays covering this position
        let overlay_ids = self.buffer.overlays.overlays_at(pos);
        for oid in &overlay_ids {
            let oid = *oid;
            // Before-string: from overlays that START at this position
            if let Some(start) = self.buffer.overlays.overlay_start(oid) {
                if start == pos {
                    if let Some(val) = self.buffer.overlays.overlay_get(oid, "before-string") {
                        if let Some(s) = value_as_string(val) {
                            before.push((s.as_bytes().to_vec(), oid));
                        }
                    }
                }
            }

            // After-string: from overlays that END at this position
            if let Some(end) = self.buffer.overlays.overlay_end(oid) {
                if end == pos {
                    if let Some(val) = self.buffer.overlays.overlay_get(oid, "after-string") {
                        if let Some(s) = value_as_string(val) {
                            after.push((s.as_bytes().to_vec(), oid));
                        }
                    }
                }
            }
        }

        // Also check overlays_in for overlays that end exactly at this position
        // (overlays_at only returns overlays that CONTAIN pos, not those ending at pos)
        // The range [pos, pos+1) covers overlays ending at pos
        // Actually, overlays_at covers [start, end) so overlays ending at pos won't be included.
        // We need a broader search for after-strings.
        if pos > 0 {
            let nearby_ids = self
                .buffer
                .overlays
                .overlays_in(pos.saturating_sub(1), pos + 1);
            for oid in &nearby_ids {
                let oid = *oid;
                if let Some(end) = self.buffer.overlays.overlay_end(oid) {
                    if end == pos {
                        // Check we haven't already processed this overlay
                        if !overlay_ids.contains(&oid) {
                            if let Some(val) =
                                self.buffer.overlays.overlay_get(oid, "after-string")
                            {
                                if let Some(s) = value_as_string(val) {
                                    after.push((s.as_bytes().to_vec(), oid));
                                }
                            }
                        }
                    }
                }
            }
        }

        (before, after)
    }

    /// Get the next position where any text property changes.
    ///
    /// This is useful for the layout engine's "next_check" optimization
    /// to avoid per-character property lookups.
    pub fn next_property_change(&self, charpos: i64) -> i64 {
        let pos = charpos as usize;
        self.buffer
            .text_props
            .next_property_change(pos)
            .unwrap_or(self.buffer.zv) as i64
    }

    /// Get a specific text property at a position.
    pub fn get_property(&self, charpos: i64, name: &str) -> Option<&Value> {
        let pos = charpos as usize;
        self.buffer.text_props.get_property(pos, name)
    }

    /// Get the underlying neovm-core Buffer reference.
    pub fn inner(&self) -> &'a Buffer {
        self.buffer
    }
}

/// Helper: extract a string from a Value.
///
/// For `Value::Str`, resolves through the heap to get the string content.
/// For other Value types, returns None.
fn value_as_string(val: &Value) -> Option<String> {
    // Value::Str uses ObjId -- need to resolve through the heap.
    // For now, use the display format as a fallback.
    // TODO: When the heap is accessible, use with_heap(|h| h.get_str(id))
    match val {
        Value::Nil => None,
        _ => {
            // Try to get the string representation.
            // This is a temporary approach -- proper string extraction
            // needs heap access which isn't available through a &Buffer reference.
            // For overlay/text prop strings, they're typically stored as
            // interned symbols or heap strings.
            None // TODO: implement proper string extraction with heap access
        }
    }
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

    // -----------------------------------------------------------------------
    // RustBufferAccess tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_rust_buffer_access_copy_text() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*test-copy*");
        // Insert some text
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "Hello, world!");
            buf.zv = buf.text.len();
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustBufferAccess::new(buf);

        let mut out = Vec::new();
        access.copy_text(0, 5, &mut out);
        assert_eq!(&out, b"Hello");

        access.copy_text(7, 13, &mut out);
        assert_eq!(&out, b"world!");
    }

    #[test]
    fn test_rust_buffer_access_charpos_to_bytepos() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*test-pos*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "abc");
            buf.zv = buf.text.len();
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustBufferAccess::new(buf);

        assert_eq!(access.charpos_to_bytepos(0), 0);
        assert_eq!(access.charpos_to_bytepos(1), 1); // ASCII: 1 byte per char
        assert_eq!(access.charpos_to_bytepos(3), 3);
    }

    #[test]
    fn test_rust_buffer_access_count_lines() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*test-lines*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "line1\nline2\nline3");
            buf.zv = buf.text.len();
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustBufferAccess::new(buf);

        assert_eq!(access.count_lines(0, 17), 2); // 2 newlines
        assert_eq!(access.count_lines(0, 6), 1);  // 1 newline in "line1\n"
        assert_eq!(access.count_lines(0, 5), 0);  // no newline in "line1"
    }

    #[test]
    fn test_rust_buffer_access_metadata() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*meta*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "content");
            buf.zv = buf.text.len();
            buf.modified = true;
            buf.file_name = Some("/tmp/test.el".to_string());
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustBufferAccess::new(buf);

        assert_eq!(access.name(), "*meta*");
        assert!(access.modified());
        assert_eq!(access.file_name(), Some("/tmp/test.el"));
        assert_eq!(access.zv(), 7);
    }

    // -----------------------------------------------------------------------
    // RustTextPropAccess tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_text_prop_check_invisible() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*invis*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "visible hidden visible");
            buf.zv = buf.text.len();
            // Mark "hidden" (positions 8..14) as invisible
            buf.text_props.put_property(8, 14, "invisible", Value::True);
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustTextPropAccess::new(buf);

        // Position 0: not invisible
        let (invis, _next) = access.check_invisible(0);
        assert!(!invis);

        // Position 8: invisible
        let (invis, _next) = access.check_invisible(8);
        assert!(invis);

        // Position 14: visible again
        let (invis, _next) = access.check_invisible(14);
        assert!(!invis);
    }

    #[test]
    fn test_text_prop_check_display() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*display*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "abcdef");
            buf.zv = buf.text.len();
            // Set a display property on positions 2..4
            buf.text_props.put_property(2, 4, "display", Value::Int(42));
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustTextPropAccess::new(buf);

        // Position 0: no display prop
        let (dp, _next) = access.check_display_prop(0);
        assert!(dp.is_none());

        // Position 2: has display prop
        let (dp, _next) = access.check_display_prop(2);
        assert!(dp.is_some());
        assert!(matches!(dp, Some(Value::Int(42))));
    }

    #[test]
    fn test_text_prop_line_spacing() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*spacing*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "line1\nline2");
            buf.zv = buf.text.len();
            // Set line-spacing on "line2" area
            buf.text_props.put_property(6, 11, "line-spacing", Value::Int(4));
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustTextPropAccess::new(buf);

        // Position 0: no line-spacing
        assert_eq!(access.check_line_spacing(0, 16.0), 0.0);

        // Position 6: line-spacing = 4
        assert_eq!(access.check_line_spacing(6, 16.0), 4.0);
    }

    #[test]
    fn test_text_prop_next_change() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*next*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "aabbcc");
            buf.zv = buf.text.len();
            buf.text_props.put_property(2, 4, "face", Value::True);
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustTextPropAccess::new(buf);

        // At position 0, next change should be at 2 (where face starts)
        let next = access.next_property_change(0);
        assert_eq!(next, 2);

        // At position 2, next change should be at 4 (where face ends)
        let next = access.next_property_change(2);
        assert_eq!(next, 4);
    }

    #[test]
    fn test_text_prop_get_property() {
        let mut evaluator = neovm_core::elisp::Evaluator::new();
        let buf_id = evaluator.buffer_manager_mut().create_buffer("*prop*");
        if let Some(buf) = evaluator.buffer_manager_mut().get_mut(buf_id) {
            buf.text.insert_str(0, "test");
            buf.zv = buf.text.len();
            buf.text_props.put_property(0, 4, "face", Value::Int(5));
        }

        let buf = evaluator.buffer_manager().get(buf_id).unwrap();
        let access = RustTextPropAccess::new(buf);

        let face = access.get_property(0, "face");
        assert!(matches!(face, Some(Value::Int(5))));

        let none = access.get_property(0, "nonexistent");
        assert!(none.is_none());
    }
}
