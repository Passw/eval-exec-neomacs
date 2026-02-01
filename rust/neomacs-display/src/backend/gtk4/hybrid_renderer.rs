//! Hybrid GSK renderer - renders directly from FrameGlyphBuffer.
//!
//! This bypasses the scene graph and builds GSK nodes directly from
//! the glyph buffer, matching Emacs's immediate-mode redisplay model.
//!
//! Uses cosmic-text for text rendering (pure Rust, no Pango).
//!
//! Enable logging with: RUST_LOG=neomacs_display::backend::gtk4::hybrid_renderer=debug

use gtk4::prelude::*;
use gtk4::{gdk, gsk, graphene};
use log::{debug, trace, warn};

use crate::core::frame_glyphs::{FrameGlyph, FrameGlyphBuffer};
use crate::core::types::Color;
use crate::core::face::{Face, FaceCache};
use crate::text::{TextEngine, GlyphAtlas, GlyphKey, CachedGlyph};
use super::video::VideoCache;
use super::image::ImageCache;

/// Hybrid renderer that builds GSK nodes directly from FrameGlyphBuffer.
/// Uses cosmic-text for text rendering instead of Pango.
pub struct HybridRenderer {
    /// cosmic-text engine for text shaping and rasterization
    text_engine: TextEngine,
    /// Glyph texture atlas for caching
    glyph_atlas: GlyphAtlas,
    /// Face cache for styling
    face_cache: FaceCache,
}

impl Default for HybridRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl HybridRenderer {
    pub fn new() -> Self {
        Self {
            text_engine: TextEngine::new(),
            glyph_atlas: GlyphAtlas::new(),
            face_cache: FaceCache::new(),
        }
    }

    /// Get mutable face cache
    pub fn face_cache_mut(&mut self) -> &mut FaceCache {
        &mut self.face_cache
    }

    /// Get or rasterize a glyph, returning a cached texture
    fn get_or_rasterize_glyph(
        &mut self,
        c: char,
        face_id: u32,
        fg: &Color,
    ) -> Option<&CachedGlyph> {
        let key = GlyphKey {
            charcode: c as u32,
            face_id,
        };

        // Check cache first
        if self.glyph_atlas.contains(&key) {
            return self.glyph_atlas.get(&key);
        }

        debug!("Rasterizing '{}' (face_id={}, fg={:?})", c, face_id, fg);

        // Need to rasterize - create a temporary face with the foreground color
        let face = Face {
            id: face_id,
            foreground: *fg,
            background: Color::TRANSPARENT,
            underline_color: None,
            overline_color: None,
            strike_through_color: None,
            box_color: None,
            font_family: "monospace".to_string(),
            font_size: 13.0,
            font_weight: 400,
            attributes: crate::core::face::FaceAttributes::empty(),
            underline_style: crate::core::face::UnderlineStyle::None,
            box_type: crate::core::face::BoxType::None,
            box_line_width: 0,
        };

        // Rasterize the character
        if let Some((width, height, pixels, bearing_x, bearing_y)) =
            self.text_engine.rasterize_char(c, Some(&face))
        {
            warn!("Rasterized '{}': {}x{} bearing=({},{}) pixels_len={}", c, width, height, bearing_x, bearing_y, pixels.len());
            // Sample some pixel data to verify - find max alpha
            let max_alpha = pixels.chunks(4).map(|c| c[3]).max().unwrap_or(0);
            let non_zero_count = pixels.chunks(4).filter(|c| c[3] > 0).count();
            warn!("  max_alpha={} non_zero_alpha_pixels={}", max_alpha, non_zero_count);
            // Create GPU texture
            if let Some(texture) = TextEngine::create_texture(width, height, &pixels) {
                warn!("Created texture for '{}' size={}x{}", c, texture.width(), texture.height());
                self.glyph_atlas.insert_texture(
                    key.clone(),
                    texture,
                    width,
                    height,
                    bearing_x,
                    bearing_y,
                );
                return self.glyph_atlas.get(&key);
            } else {
                warn!("Failed to create texture for '{}'", c);
            }
        } else {
            warn!("Failed to rasterize '{}'", c);
        }

        None
    }

    /// Build GSK render nodes from FrameGlyphBuffer
    pub fn build_render_node(
        &mut self,
        buffer: &FrameGlyphBuffer,
        _video_cache: Option<&VideoCache>,
        _image_cache: Option<&mut ImageCache>,
    ) -> Option<gsk::RenderNode> {
        debug!(
            "build_render_node: {} glyphs, frame={}x{}, bg={:?}",
            buffer.len(),
            buffer.width,
            buffer.height,
            buffer.background
        );
        let mut nodes: Vec<gsk::RenderNode> = Vec::with_capacity(buffer.len() + 10);

        // Frame background
        let bg_rect = graphene::Rect::new(0.0, 0.0, buffer.width, buffer.height);
        let bg_color = color_to_gdk(&buffer.background);
        nodes.push(gsk::ColorNode::new(&bg_color, &bg_rect).upcast());
        debug!("Added frame background node");

        // Collect glyph data first to avoid borrow conflicts
        let glyphs: Vec<_> = buffer.glyphs.iter().cloned().collect();
        debug!("Collected {} glyphs to process", glyphs.len());

        // **FIX**: Process backgrounds FIRST so they don't cover text
        // Backgrounds need to be rendered before text glyphs
        let mut bg_count = 0;
        for glyph in &glyphs {
            if let FrameGlyph::Background { bounds, color } = glyph {
                bg_count += 1;
                let rect = graphene::Rect::new(bounds.x, bounds.y, bounds.width, bounds.height);
                let gdk_color = color_to_gdk(color);
                nodes.push(gsk::ColorNode::new(&gdk_color, &rect).upcast());
            }
        }
        debug!("Added {} background(s) FIRST", bg_count);

        // Process remaining glyphs (excluding backgrounds)
        let mut char_count = 0;
        let mut node_order_log: Vec<String> = Vec::new();
        for glyph in glyphs {
            match glyph {
                FrameGlyph::Background { .. } => {
                    // Already processed above
                }

                FrameGlyph::Char {
                    char,
                    x,
                    y,
                    width,
                    height,
                    ascent,
                    fg,
                    bg,
                    face_id,
                    ..
                } => {
                    char_count += 1;
                    // Draw char background if specified
                    if let Some(bg_color) = bg {
                        let rect = graphene::Rect::new(x, y, width, height);
                        nodes.push(gsk::ColorNode::new(&color_to_gdk(&bg_color), &rect).upcast());
                    }

                    // Skip whitespace - no need to render
                    if char == ' ' || char == '\t' || char == '\n' {
                        continue;
                    }

                    // Log all chars on first line (y=0) to debug duplication
                    if y < 1.0 {
                        warn!(
                            "LINE1 Char '{}' at x={:.1}",
                            char, x
                        );
                    }

                    // Get or rasterize glyph
                    if let Some(cached) = self.get_or_rasterize_glyph(char, face_id, &fg) {
                        // Position glyph using bearing
                        // x is the cell position, bearing_x is the glyph offset
                        // y is cell top, ascent is baseline offset, bearing_y is glyph offset from baseline
                        let glyph_x = x + cached.bearing_x;
                        let glyph_y = y + ascent - cached.bearing_y;

                        // Log texture info for first few glyphs
                        if x < 50.0 {
                            warn!(
                                "TextureNode '{}' at ({:.1},{:.1}) size={}x{} texture_size={}x{}",
                                char, glyph_x, glyph_y, cached.width, cached.height,
                                cached.texture.width(), cached.texture.height()
                            );
                        }

                        let rect = graphene::Rect::new(
                            glyph_x,
                            glyph_y,
                            cached.width as f32,
                            cached.height as f32,
                        );

                        // Create texture node
                        let texture_node = gsk::TextureNode::new(&cached.texture, &rect);
                        nodes.push(texture_node.upcast());
                        node_order_log.push(format!("'{}' @({:.0},{:.0})", char, glyph_x, glyph_y));
                    } else {
                        // Fallback: draw a small rectangle for missing glyphs
                        if char != ' ' {
                            debug!("Missing glyph for '{}' at ({:.1},{:.1})", char, x, y);
                        }
                    }
                }

                FrameGlyph::Stretch {
                    x,
                    y,
                    width,
                    height,
                    bg,
                } => {
                    node_order_log.push(format!("STRETCH@({:.0},{:.0})", x, y));
                    let rect = graphene::Rect::new(x, y, width, height);
                    nodes.push(gsk::ColorNode::new(&color_to_gdk(&bg), &rect).upcast());
                }

                FrameGlyph::Cursor {
                    window_id: _,  // Not used in rendering, just for tracking
                    x,
                    y,
                    width,
                    height,
                    style,
                    color,
                } => {
                    warn!("CURSOR GLYPH: x={}, y={}, w={}, h={}, style={}, color=({},{},{},{})", 
                          x, y, width, height, style, color.r, color.g, color.b, color.a);
                    let cursor_color = color_to_gdk(&color);
                    match style {
                        0 => {
                            // Box (filled)
                            let rect = graphene::Rect::new(x, y, width, height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &rect).upcast());
                        }
                        1 => {
                            // Bar (vertical line)
                            let rect = graphene::Rect::new(x, y, 2.0, height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &rect).upcast());
                        }
                        2 => {
                            // Underline
                            let rect = graphene::Rect::new(x, y + height - 2.0, width, 2.0);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &rect).upcast());
                        }
                        3 => {
                            // Hollow box (outline)
                            let thickness = 1.0;
                            // Top
                            let top = graphene::Rect::new(x, y, width, thickness);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &top).upcast());
                            // Bottom
                            let bottom = graphene::Rect::new(x, y + height - thickness, width, thickness);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &bottom).upcast());
                            // Left
                            let left = graphene::Rect::new(x, y, thickness, height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &left).upcast());
                            // Right
                            let right = graphene::Rect::new(x + width - thickness, y, thickness, height);
                            nodes.push(gsk::ColorNode::new(&cursor_color, &right).upcast());
                        }
                        _ => {}
                    }
                }

                FrameGlyph::Border {
                    x,
                    y,
                    width,
                    height,
                    color,
                } => {
                    let rect = graphene::Rect::new(x, y, width, height);
                    nodes.push(gsk::ColorNode::new(&color_to_gdk(&color), &rect).upcast());
                }

                FrameGlyph::Image {
                    image_id,
                    x,
                    y,
                    width,
                    height,
                } => {
                    // TODO: Look up image from cache and render
                    let rect = graphene::Rect::new(x, y, width, height);
                    let placeholder = gdk::RGBA::new(0.3, 0.3, 0.4, 1.0);
                    nodes.push(gsk::ColorNode::new(&placeholder, &rect).upcast());
                }

                FrameGlyph::Video {
                    video_id,
                    x,
                    y,
                    width,
                    height,
                } => {
                    // TODO: Look up video from cache and render
                    let rect = graphene::Rect::new(x, y, width, height);
                    let placeholder = gdk::RGBA::new(0.2, 0.2, 0.3, 1.0);
                    nodes.push(gsk::ColorNode::new(&placeholder, &rect).upcast());
                }

                FrameGlyph::WebKit {
                    webkit_id,
                    x,
                    y,
                    width,
                    height,
                } => {
                    // TODO: Look up webkit from cache and render
                    let rect = graphene::Rect::new(x, y, width, height);
                    let placeholder = gdk::RGBA::new(0.1, 0.1, 0.2, 1.0);
                    nodes.push(gsk::ColorNode::new(&placeholder, &rect).upcast());
                }
            }
        }

        // Log ALL nodes in order (just first and last few)
        if node_order_log.len() > 0 {
            let first_part: Vec<_> = node_order_log.iter().take(50).cloned().collect();
            let last_part: Vec<_> = node_order_log.iter().rev().take(30).rev().cloned().collect();
            warn!("NODE ORDER first 50: {}", first_part.join(", "));
            warn!("NODE ORDER last 30: {}", last_part.join(", "));
        }

        warn!("Processed {} chars, {} backgrounds, total {} nodes", char_count, bg_count, nodes.len());
        
        // Log first few node types
        for (i, node) in nodes.iter().take(5).enumerate() {
            warn!("  node[{}]: type={:?}", i, node.node_type());
        }

        if nodes.is_empty() {
            warn!("build_render_node: returning None (empty nodes)");
            None
        } else {
            warn!("build_render_node: returning ContainerNode with {} nodes", nodes.len());
            Some(gsk::ContainerNode::new(&nodes).upcast())
        }
    }
}

/// Convert our Color to GDK RGBA
fn color_to_gdk(color: &Color) -> gdk::RGBA {
    // Color fields are already in 0.0-1.0 range
    gdk::RGBA::new(color.r, color.g, color.b, color.a)
}
