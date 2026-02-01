//! Hybrid GSK renderer - renders directly from FrameGlyphBuffer.
//!
//! This bypasses the scene graph and builds GSK nodes directly from
//! the glyph buffer, matching Emacs's immediate-mode redisplay model.

use gtk4::prelude::*;
use gtk4::{gdk, gsk, pango, graphene};

use crate::core::frame_glyphs::{FrameGlyph, FrameGlyphBuffer};
use crate::core::types::Color;
use crate::core::face::FaceCache;
use super::video::VideoCache;
use super::image::ImageCache;

/// Hybrid renderer that builds GSK nodes directly from FrameGlyphBuffer.
pub struct HybridRenderer {
    /// Pango context for text layout
    pango_context: Option<pango::Context>,
    /// Face cache for styling
    face_cache: FaceCache,
    /// Font description cache
    font_desc: Option<pango::FontDescription>,
}

impl Default for HybridRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl HybridRenderer {
    pub fn new() -> Self {
        Self {
            pango_context: None,
            face_cache: FaceCache::new(),
            font_desc: None,
        }
    }

    /// Initialize with Pango context
    pub fn init_with_context(&mut self, context: pango::Context) {
        // Create default font description
        let mut font_desc = pango::FontDescription::new();
        font_desc.set_family("Monospace");
        font_desc.set_size(13 * pango::SCALE);
        self.font_desc = Some(font_desc);
        self.pango_context = Some(context);
    }

    /// Get mutable face cache
    pub fn face_cache_mut(&mut self) -> &mut FaceCache {
        &mut self.face_cache
    }

    /// Build GSK render nodes from FrameGlyphBuffer
    pub fn build_render_node(
        &mut self,
        buffer: &FrameGlyphBuffer,
        _video_cache: Option<&VideoCache>,
        _image_cache: Option<&mut ImageCache>,
    ) -> Option<gsk::RenderNode> {
        let mut nodes: Vec<gsk::RenderNode> = Vec::with_capacity(buffer.len() + 10);

        // Frame background
        let bg_rect = graphene::Rect::new(0.0, 0.0, buffer.width, buffer.height);
        let bg_color = color_to_gdk(&buffer.background);
        nodes.push(gsk::ColorNode::new(&bg_color, &bg_rect).upcast());

        // Get pango context and font
        let pango_ctx = match &self.pango_context {
            Some(ctx) => ctx,
            None => return Some(gsk::ContainerNode::new(&nodes).upcast()),
        };
        let font_desc = match &self.font_desc {
            Some(fd) => fd,
            None => return Some(gsk::ContainerNode::new(&nodes).upcast()),
        };

        // Process each glyph
        for glyph in &buffer.glyphs {
            match glyph {
                FrameGlyph::Background { bounds, color } => {
                    let rect = graphene::Rect::new(bounds.x, bounds.y, bounds.width, bounds.height);
                    let gdk_color = color_to_gdk(color);
                    nodes.push(gsk::ColorNode::new(&gdk_color, &rect).upcast());
                }

                FrameGlyph::Char { char, x, y, width, height, ascent, fg, bg, .. } => {
                    // Draw background if present
                    if let Some(bg_color) = bg {
                        let rect = graphene::Rect::new(*x, *y, *width, *height);
                        nodes.push(gsk::ColorNode::new(&color_to_gdk(bg_color), &rect).upcast());
                    }

                    // Draw character using Pango
                    let text = char.to_string();
                    let layout = pango::Layout::new(pango_ctx);
                    layout.set_font_description(Some(font_desc));
                    layout.set_text(&text);

                    // Position text at baseline
                    let text_y = *y + *ascent;
                    let point = graphene::Point::new(*x, text_y);
                    let fg_color = color_to_gdk(fg);

                    // Create text node
                    if let Some(node) = create_text_node(&layout, &point, &fg_color) {
                        nodes.push(node);
                    }
                }

                FrameGlyph::Stretch { x, y, width, height, bg } => {
                    let rect = graphene::Rect::new(*x, *y, *width, *height);
                    nodes.push(gsk::ColorNode::new(&color_to_gdk(bg), &rect).upcast());
                }

                FrameGlyph::Cursor { x, y, width, height, style, color } => {
                    let cursor_color = color_to_gdk(color);
                    match style {
                        0 => {
                            // Box (filled)
                            let rect = graphene::Rect::new(*x, *y, *width, *height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &rect).upcast());
                        }
                        1 => {
                            // Bar (vertical line)
                            let rect = graphene::Rect::new(*x, *y, 2.0, *height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &rect).upcast());
                        }
                        2 => {
                            // Underline
                            let rect = graphene::Rect::new(*x, *y + *height - 2.0, *width, 2.0);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &rect).upcast());
                        }
                        3 => {
                            // Hollow box (outline)
                            let thickness = 1.0;
                            // Top
                            let top = graphene::Rect::new(*x, *y, *width, thickness);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &top).upcast());
                            // Bottom
                            let bottom = graphene::Rect::new(*x, *y + *height - thickness, *width, thickness);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &bottom).upcast());
                            // Left
                            let left = graphene::Rect::new(*x, *y, thickness, *height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &left).upcast());
                            // Right
                            let right = graphene::Rect::new(*x + *width - thickness, *y, thickness, *height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &right).upcast());
                        }
                        _ => {}
                    }
                }

                FrameGlyph::Border { x, y, width, height, color } => {
                    let rect = graphene::Rect::new(*x, *y, *width, *height);
                    nodes.push(gsk::ColorNode::new(&color_to_gdk(color), &rect).upcast());
                }

                FrameGlyph::Image { image_id, x, y, width, height } => {
                    // TODO: Look up image from cache and render
                    let rect = graphene::Rect::new(*x, *y, *width, *height);
                    let placeholder = gdk::RGBA::new(0.3, 0.3, 0.4, 1.0);
                    nodes.push(gsk::ColorNode::new(&placeholder, &rect).upcast());
                }

                FrameGlyph::Video { video_id, x, y, width, height } => {
                    // TODO: Look up video from cache and render
                    let rect = graphene::Rect::new(*x, *y, *width, *height);
                    let placeholder = gdk::RGBA::new(0.2, 0.2, 0.3, 1.0);
                    nodes.push(gsk::ColorNode::new(&placeholder, &rect).upcast());
                }

                FrameGlyph::WebKit { webkit_id, x, y, width, height } => {
                    // TODO: Look up webkit from cache and render
                    let rect = graphene::Rect::new(*x, *y, *width, *height);
                    let placeholder = gdk::RGBA::new(0.1, 0.1, 0.2, 1.0);
                    nodes.push(gsk::ColorNode::new(&placeholder, &rect).upcast());
                }
            }
        }

        if nodes.is_empty() {
            None
        } else {
            Some(gsk::ContainerNode::new(&nodes).upcast())
        }
    }
}

/// Convert our Color to GDK RGBA
fn color_to_gdk(color: &Color) -> gdk::RGBA {
    gdk::RGBA::new(
        color.r as f32 / 255.0,
        color.g as f32 / 255.0,
        color.b as f32 / 255.0,
        color.a as f32 / 255.0,
    )
}

/// Create a text node from a Pango layout
fn create_text_node(layout: &pango::Layout, point: &graphene::Point, color: &gdk::RGBA) -> Option<gsk::RenderNode> {
    let snapshot = gtk4::Snapshot::new();
    snapshot.save();
    snapshot.translate(point);

    // Get layout baseline offset
    let baseline = layout.baseline() as f32 / pango::SCALE as f32;
    snapshot.translate(&graphene::Point::new(0.0, -baseline));

    snapshot.append_layout(layout, color);
    snapshot.restore();
    snapshot.to_node()
}
