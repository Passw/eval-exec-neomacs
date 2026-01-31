//! Pure Rust text rendering using cosmic-text
//! 
//! This module provides text shaping and rasterization using:
//! - cosmic-text for text layout and glyph caching
//! - GdkTexture for GPU upload
//! - GskTextureNode for rendering

mod engine;
mod atlas;

pub use engine::TextEngine;
pub use atlas::{GlyphAtlas, GlyphKey, CachedGlyph};
