//! Frame glyph buffer for hybrid rendering.
//!
//! This module implements a simple buffer that collects glyphs during
//! Emacs redisplay. Unlike the scene graph approach, we don't track
//! window state - Emacs provides positions, we just render.

use crate::core::types::{Color, Rect};

/// A single glyph to render
#[derive(Debug, Clone)]
pub enum FrameGlyph {
    /// Character glyph with text
    Char {
        /// Character to render
        char: char,
        /// Frame-absolute X position
        x: f32,
        /// Frame-absolute Y position
        y: f32,
        /// Glyph width
        width: f32,
        /// Row height
        height: f32,
        /// Font ascent
        ascent: f32,
        /// Foreground color
        fg: Color,
        /// Background color (if not transparent)
        bg: Option<Color>,
        /// Face ID for font lookup
        face_id: u32,
        /// Bold flag
        bold: bool,
        /// Italic flag
        italic: bool,
        /// Underline style (0=none, 1=single, 2=double, 3=wave)
        underline: u8,
        /// Underline color
        underline_color: Option<Color>,
    },

    /// Stretch (whitespace) glyph
    Stretch {
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        bg: Color,
    },

    /// Image glyph
    Image {
        image_id: u32,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    },

    /// Video glyph (inline in buffer)
    Video {
        video_id: u32,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    },

    /// WebKit glyph (inline in buffer)
    WebKit {
        webkit_id: u32,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
    },

    /// Cursor
    Cursor {
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        /// 0=box, 1=hollow, 2=bar, 3=hbar
        style: u8,
        color: Color,
    },

    /// Window background
    Background {
        bounds: Rect,
        color: Color,
    },

    /// Window border (vertical/horizontal divider)
    Border {
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        color: Color,
    },
}

/// Buffer collecting glyphs for current frame
#[derive(Debug, Default)]
pub struct FrameGlyphBuffer {
    /// Frame dimensions
    pub width: f32,
    pub height: f32,

    /// Frame background color
    pub background: Color,

    /// All glyphs to render this frame
    pub glyphs: Vec<FrameGlyph>,

    /// Current face attributes (set before adding char glyphs)
    current_face_id: u32,
    current_fg: Color,
    current_bg: Option<Color>,
    current_bold: bool,
    current_italic: bool,
    current_underline: u8,
    current_underline_color: Option<Color>,
}

impl FrameGlyphBuffer {
    pub fn new() -> Self {
        Self {
            width: 0.0,
            height: 0.0,
            background: Color::WHITE,
            glyphs: Vec::with_capacity(10000), // Pre-allocate for typical frame
            current_face_id: 0,
            current_fg: Color::BLACK,
            current_bg: None,
            current_bold: false,
            current_italic: false,
            current_underline: 0,
            current_underline_color: None,
        }
    }

    /// Clear buffer for new frame
    pub fn begin_frame(&mut self, width: f32, height: f32, background: Color) {
        self.width = width;
        self.height = height;
        self.background = background;
        self.glyphs.clear();
    }

    /// Set current face attributes for subsequent char glyphs
    pub fn set_face(&mut self, face_id: u32, fg: Color, bg: Option<Color>,
                    bold: bool, italic: bool, underline: u8, underline_color: Option<Color>) {
        self.current_face_id = face_id;
        self.current_fg = fg;
        self.current_bg = bg;
        self.current_bold = bold;
        self.current_italic = italic;
        self.current_underline = underline;
        self.current_underline_color = underline_color;
    }

    /// Add a window background rectangle
    pub fn add_background(&mut self, x: f32, y: f32, width: f32, height: f32, color: Color) {
        self.glyphs.push(FrameGlyph::Background {
            bounds: Rect::new(x, y, width, height),
            color,
        });
    }

    /// Remove glyphs that overlap with the given rectangle
    fn remove_overlapping(&mut self, x: f32, y: f32, width: f32, height: f32) {
        let x_end = x + width;
        let y_end = y + height;
        self.glyphs.retain(|g| {
            let (gx, gy, gw, gh) = match g {
                FrameGlyph::Char { x, y, width, height, .. } => (*x, *y, *width, *height),
                FrameGlyph::Stretch { x, y, width, height, .. } => (*x, *y, *width, *height),
                FrameGlyph::Image { x, y, width, height, .. } => (*x, *y, *width, *height),
                FrameGlyph::Video { x, y, width, height, .. } => (*x, *y, *width, *height),
                FrameGlyph::WebKit { x, y, width, height, .. } => (*x, *y, *width, *height),
                // Don't remove backgrounds, cursors, borders - they're managed separately
                _ => return true,
            };
            let gx_end = gx + gw;
            let gy_end = gy + gh;
            // Keep if no overlap
            gx_end <= x || gx >= x_end || gy_end <= y || gy >= y_end
        });
    }

    /// Add a character glyph (removes overlapping glyphs first)
    pub fn add_char(&mut self, char: char, x: f32, y: f32, width: f32, height: f32, ascent: f32) {
        // Remove any existing glyphs at this position
        self.remove_overlapping(x, y, width, height);
        self.glyphs.push(FrameGlyph::Char {
            char,
            x,
            y,
            width,
            height,
            ascent,
            fg: self.current_fg,
            bg: self.current_bg,
            face_id: self.current_face_id,
            bold: self.current_bold,
            italic: self.current_italic,
            underline: self.current_underline,
            underline_color: self.current_underline_color,
        });
    }

    /// Add a stretch (whitespace) glyph (removes overlapping glyphs first)
    pub fn add_stretch(&mut self, x: f32, y: f32, width: f32, height: f32, bg: Color) {
        self.remove_overlapping(x, y, width, height);
        self.glyphs.push(FrameGlyph::Stretch { x, y, width, height, bg });
    }

    /// Add an image glyph
    pub fn add_image(&mut self, image_id: u32, x: f32, y: f32, width: f32, height: f32) {
        self.remove_overlapping(x, y, width, height);
        self.glyphs.push(FrameGlyph::Image { image_id, x, y, width, height });
    }

    /// Add a video glyph
    pub fn add_video(&mut self, video_id: u32, x: f32, y: f32, width: f32, height: f32) {
        self.glyphs.push(FrameGlyph::Video { video_id, x, y, width, height });
    }

    /// Add a webkit glyph
    pub fn add_webkit(&mut self, webkit_id: u32, x: f32, y: f32, width: f32, height: f32) {
        self.glyphs.push(FrameGlyph::WebKit { webkit_id, x, y, width, height });
    }

    /// Add cursor
    pub fn add_cursor(&mut self, x: f32, y: f32, width: f32, height: f32, style: u8, color: Color) {
        self.glyphs.push(FrameGlyph::Cursor { x, y, width, height, style, color });
    }

    /// Add border
    pub fn add_border(&mut self, x: f32, y: f32, width: f32, height: f32, color: Color) {
        self.glyphs.push(FrameGlyph::Border { x, y, width, height, color });
    }

    /// Get glyph count
    pub fn len(&self) -> usize {
        self.glyphs.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.glyphs.is_empty()
    }
}
