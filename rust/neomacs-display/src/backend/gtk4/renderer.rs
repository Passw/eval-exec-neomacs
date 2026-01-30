//! GTK4 scene renderer using Cairo (compatible with DrawingArea).
//!
//! Note: GTK4's DrawingArea still uses Cairo contexts. For full GPU acceleration,
//! we would need to use GtkSnapshot with custom widgets, but Cairo provides
//! good performance for text-heavy workloads like Emacs.

use gtk4::prelude::*;
use gtk4::{gdk, pango, cairo};

use crate::core::scene::{Scene, WindowScene, Node, NodeKind, CursorStyle};
use crate::core::types::{Color, Rect};

/// Renderer that converts our scene graph to Cairo drawing commands.
pub struct Gtk4Renderer {
    /// Pango context for text layout
    pango_context: Option<pango::Context>,
}

impl Default for Gtk4Renderer {
    fn default() -> Self {
        Self::new()
    }
}

impl Gtk4Renderer {
    pub fn new() -> Self {
        Self {
            pango_context: None,
        }
    }

    /// Initialize renderer with a Pango context from a widget
    pub fn init_with_context(&mut self, context: pango::Context) {
        self.pango_context = Some(context);
    }

    /// Render a scene to a Cairo context
    pub fn render(&self, cr: &cairo::Context, scene: &Scene) {
        // Background
        self.render_color_rect(cr, 0.0, 0.0, scene.width, scene.height, &scene.background);

        // Render each window
        for window in &scene.windows {
            self.render_window(cr, window);
        }
    }

    /// Render a window scene
    fn render_window(&self, cr: &cairo::Context, window: &WindowScene) {
        // Save state
        cr.save().ok();

        // Translate to window position
        cr.translate(window.bounds.x as f64, window.bounds.y as f64);

        // Clip to window bounds
        cr.rectangle(0.0, 0.0, window.bounds.width as f64, window.bounds.height as f64);
        cr.clip();

        // Apply scroll offset
        if window.scroll_offset != 0.0 {
            cr.translate(0.0, -window.scroll_offset as f64);
        }

        // Window background
        self.render_color_rect(cr, 0.0, 0.0, window.bounds.width, window.bounds.height, &window.background);

        // TODO: Render glyph rows
        // for row in &window.rows {
        //     self.render_glyph_row(cr, row);
        // }

        // Render cursor if visible
        if let Some(cursor) = &window.cursor {
            if cursor.visible {
                self.render_cursor(
                    cr,
                    cursor.x,
                    cursor.y,
                    cursor.width,
                    cursor.height,
                    cursor.style,
                    &cursor.color,
                );
            }
        }

        // Draw a sample text to show it works
        if let Some(context) = &self.pango_context {
            self.render_sample_text(cr, context, window);
        }

        cr.restore().ok();
    }

    /// Render sample text to demonstrate text rendering
    fn render_sample_text(&self, cr: &cairo::Context, context: &pango::Context, window: &WindowScene) {
        let layout = pango::Layout::new(context);
        
        // Set font
        let font_desc = pango::FontDescription::from_string("Monospace 14");
        layout.set_font_description(Some(&font_desc));
        
        // Set text
        let text = if window.selected {
            "Welcome to Neomacs Display Engine\n\nThis is a GPU-accelerated display engine\nwritten in Rust using GTK4.\n\n;; Press C-x C-c to quit"
        } else {
            "[Minibuffer]"
        };
        layout.set_text(text);

        // Set color (light text)
        cr.set_source_rgb(0.9, 0.9, 0.85);
        
        // Position and render
        cr.move_to(10.0, 10.0);
        pangocairo::functions::show_layout(cr, &layout);
    }

    /// Render a solid color rectangle
    fn render_color_rect(&self, cr: &cairo::Context, x: f32, y: f32, width: f32, height: f32, color: &Color) {
        cr.set_source_rgba(color.r as f64, color.g as f64, color.b as f64, color.a as f64);
        cr.rectangle(x as f64, y as f64, width as f64, height as f64);
        cr.fill().ok();
    }

    /// Render text using Pango
    fn render_text(&self, cr: &cairo::Context, text: &str, x: f32, y: f32) {
        let Some(context) = &self.pango_context else {
            return;
        };

        let layout = pango::Layout::new(context);
        layout.set_text(text);

        cr.set_source_rgb(1.0, 1.0, 1.0);
        cr.move_to(x as f64, y as f64);
        pangocairo::functions::show_layout(cr, &layout);
    }

    /// Render cursor
    fn render_cursor(
        &self,
        cr: &cairo::Context,
        x: f32,
        y: f32,
        width: f32,
        height: f32,
        style: CursorStyle,
        color: &Color,
    ) {
        cr.set_source_rgba(color.r as f64, color.g as f64, color.b as f64, color.a as f64);

        match style {
            CursorStyle::Box => {
                cr.rectangle(x as f64, y as f64, width as f64, height as f64);
                cr.fill().ok();
            }
            CursorStyle::Bar => {
                cr.rectangle(x as f64, y as f64, 2.0, height as f64);
                cr.fill().ok();
            }
            CursorStyle::Underline => {
                cr.rectangle(x as f64, (y + height - 2.0) as f64, width as f64, 2.0);
                cr.fill().ok();
            }
            CursorStyle::Hollow => {
                cr.set_line_width(1.0);
                cr.rectangle(x as f64 + 0.5, y as f64 + 0.5, (width - 1.0) as f64, (height - 1.0) as f64);
                cr.stroke().ok();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_renderer_creation() {
        let renderer = Gtk4Renderer::new();
        assert!(renderer.pango_context.is_none());
    }
}
