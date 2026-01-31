//! Scene graph for display rendering.

use std::collections::HashMap;
use crate::core::types::{Color, Rect, Transform, Point};
use crate::core::glyph::{GlyphRow, GlyphString};
use crate::core::face::Face;

/// Scene graph node types
#[derive(Debug, Clone)]
pub enum NodeKind {
    /// Container with children
    Container {
        children: Vec<Node>,
    },

    /// Text run with shaped glyphs
    TextRun {
        text: String,
        face_id: u32,
        x: f32,
        y: f32,
    },

    /// Solid color rectangle
    ColorRect {
        color: Color,
    },

    /// Image texture
    Image {
        image_id: u32,
    },

    /// Video frame
    Video {
        video_id: u32,
    },

    /// WPE WebKit view
    Wpe {
        view_id: u32,
    },

    /// Cursor
    Cursor {
        style: CursorStyle,
        color: Color,
        blink_on: bool,
    },
}

/// Cursor style
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CursorStyle {
    Box,
    Bar,
    Underline,
    Hollow,
}

impl Default for CursorStyle {
    fn default() -> Self {
        Self::Box
    }
}

/// A node in the scene graph
#[derive(Debug, Clone)]
pub struct Node {
    /// Kind of node
    pub kind: NodeKind,

    /// Bounding rectangle
    pub bounds: Rect,

    /// Opacity (0.0 - 1.0)
    pub opacity: f32,

    /// Optional transform
    pub transform: Option<Transform>,

    /// Optional clip rectangle
    pub clip: Option<Rect>,
}

impl Node {
    /// Create a container node
    pub fn container(children: Vec<Node>) -> Self {
        Self {
            kind: NodeKind::Container { children },
            bounds: Rect::ZERO,
            opacity: 1.0,
            transform: None,
            clip: None,
        }
    }

    /// Create a container with transform
    pub fn container_with_transform(children: Vec<Node>, transform: Transform) -> Self {
        Self {
            kind: NodeKind::Container { children },
            bounds: Rect::ZERO,
            opacity: 1.0,
            transform: Some(transform),
            clip: None,
        }
    }

    /// Create a color rectangle node
    pub fn color_rect(bounds: Rect, color: Color) -> Self {
        Self {
            kind: NodeKind::ColorRect { color },
            bounds,
            opacity: 1.0,
            transform: None,
            clip: None,
        }
    }

    /// Create a text run node
    pub fn text_run(text: String, face_id: u32, x: f32, y: f32, bounds: Rect) -> Self {
        Self {
            kind: NodeKind::TextRun { text, face_id, x, y },
            bounds,
            opacity: 1.0,
            transform: None,
            clip: None,
        }
    }

    /// Create an image node
    pub fn image(image_id: u32, bounds: Rect) -> Self {
        Self {
            kind: NodeKind::Image { image_id },
            bounds,
            opacity: 1.0,
            transform: None,
            clip: None,
        }
    }

    /// Create a video node
    pub fn video(video_id: u32, bounds: Rect) -> Self {
        Self {
            kind: NodeKind::Video { video_id },
            bounds,
            opacity: 1.0,
            transform: None,
            clip: None,
        }
    }

    /// Create a cursor node
    pub fn cursor(style: CursorStyle, color: Color, bounds: Rect) -> Self {
        Self {
            kind: NodeKind::Cursor {
                style,
                color,
                blink_on: true,
            },
            bounds,
            opacity: 1.0,
            transform: None,
            clip: None,
        }
    }

    /// Set opacity
    pub fn with_opacity(mut self, opacity: f32) -> Self {
        self.opacity = opacity;
        self
    }

    /// Set transform
    pub fn with_transform(mut self, transform: Transform) -> Self {
        self.transform = Some(transform);
        self
    }

    /// Set clip
    pub fn with_clip(mut self, clip: Rect) -> Self {
        self.clip = Some(clip);
        self
    }
}

/// Represents a window in the scene
#[derive(Debug, Clone)]
pub struct WindowScene {
    /// Window ID
    pub window_id: i32,

    /// Position and size
    pub bounds: Rect,

    /// Background color
    pub background: Color,

    /// Glyph rows
    pub rows: Vec<GlyphRow>,

    /// Cursor position and style
    pub cursor: Option<CursorState>,

    /// Scroll offset (for smooth scrolling)
    pub scroll_offset: f32,

    /// Is this the selected (focused) window?
    pub selected: bool,

    /// Mode line height
    pub mode_line_height: i32,

    /// Header line height
    pub header_line_height: i32,
}

/// Cursor state in a window
#[derive(Debug, Clone)]
pub struct CursorState {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub style: CursorStyle,
    pub color: Color,
    pub visible: bool,
}

/// Complete scene for a frame
#[derive(Debug, Clone)]
pub struct Scene {
    /// Frame dimensions
    pub width: f32,
    pub height: f32,

    /// Background color
    pub background: Color,

    /// Windows in this frame
    pub windows: Vec<WindowScene>,

    /// Root node of the scene graph
    pub root: Option<Node>,

    /// Dirty region (needs redraw)
    pub dirty: Option<Rect>,
    
    /// Faces used in this scene (face_id -> Face)
    pub faces: HashMap<u32, Face>,
    
    /// Floating videos at screen positions
    pub floating_videos: Vec<FloatingVideo>,
    
    /// Floating images at screen positions
    pub floating_images: Vec<FloatingImage>,
    
    /// Floating WebKit views at screen positions
    pub floating_webkits: Vec<FloatingWebKit>,
}

/// Floating video layer for rendering video at a specific screen position
#[derive(Debug, Clone)]
pub struct FloatingVideo {
    pub video_id: u32,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

/// Floating image layer for rendering image at a specific screen position
#[derive(Debug, Clone)]
pub struct FloatingImage {
    pub image_id: u32,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

/// Floating WebKit view for rendering web content at a specific screen position
#[derive(Debug, Clone)]
pub struct FloatingWebKit {
    pub webkit_id: u32,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl Scene {
    /// Create a new empty scene
    pub fn new(width: f32, height: f32) -> Self {
        Self {
            width,
            height,
            background: Color::BLACK,
            windows: Vec::new(),
            root: None,
            dirty: None,
            faces: HashMap::new(),
            floating_videos: Vec::new(),
            floating_images: Vec::new(),
            floating_webkits: Vec::new(),
        }
    }

    /// Mark the entire scene as dirty
    pub fn mark_dirty(&mut self) {
        self.dirty = Some(Rect::new(0.0, 0.0, self.width, self.height));
    }

    /// Mark a region as dirty
    pub fn mark_region_dirty(&mut self, region: Rect) {
        self.dirty = Some(match self.dirty {
            Some(existing) => {
                // Union of existing and new dirty region
                let x = existing.x.min(region.x);
                let y = existing.y.min(region.y);
                let right = existing.right().max(region.right());
                let bottom = existing.bottom().max(region.bottom());
                Rect::new(x, y, right - x, bottom - y)
            }
            None => region,
        });
    }

    /// Clear dirty region
    pub fn clear_dirty(&mut self) {
        self.dirty = None;
    }

    /// Clear all windows and content from the scene (preserves faces)
    pub fn clear(&mut self) {
        self.windows.clear();
        self.root = None;
        self.mark_dirty();
    }
    
    /// Add or update a face in the scene
    pub fn set_face(&mut self, face: Face) {
        self.faces.insert(face.id, face);
    }
    
    /// Get a face by ID
    pub fn get_face(&self, id: u32) -> Option<&Face> {
        self.faces.get(&id)
    }
    
    /// Add a floating video at screen position
    pub fn add_floating_video(&mut self, video_id: u32, x: f32, y: f32, width: f32, height: f32) {
        self.floating_videos.push(FloatingVideo { video_id, x, y, width, height });
        self.mark_dirty();
    }
    
    /// Remove floating video by video ID
    pub fn remove_floating_video(&mut self, video_id: u32) {
        self.floating_videos.retain(|v| v.video_id != video_id);
        self.mark_dirty();
    }
    
    /// Clear all floating videos
    pub fn clear_floating_videos(&mut self) {
        self.floating_videos.clear();
        self.mark_dirty();
    }
    
    /// Add a floating image at screen position
    pub fn add_floating_image(&mut self, image_id: u32, x: f32, y: f32, width: f32, height: f32) {
        self.floating_images.push(FloatingImage { image_id, x, y, width, height });
        self.mark_dirty();
    }
    
    /// Remove floating image by image ID
    pub fn remove_floating_image(&mut self, image_id: u32) {
        self.floating_images.retain(|i| i.image_id != image_id);
        self.mark_dirty();
    }
    
    /// Clear all floating images
    pub fn clear_floating_images(&mut self) {
        self.floating_images.clear();
        self.mark_dirty();
    }
    
    /// Add a floating WebKit view at screen position
    pub fn add_floating_webkit(&mut self, webkit_id: u32, x: f32, y: f32, width: f32, height: f32) {
        self.floating_webkits.push(FloatingWebKit { webkit_id, x, y, width, height });
        self.mark_dirty();
    }
    
    /// Remove floating WebKit view by ID
    pub fn remove_floating_webkit(&mut self, webkit_id: u32) {
        self.floating_webkits.retain(|w| w.webkit_id != webkit_id);
        self.mark_dirty();
    }
    
    /// Clear all floating WebKit views
    pub fn clear_floating_webkits(&mut self) {
        self.floating_webkits.clear();
        self.mark_dirty();
    }

    /// Build the scene graph from windows
    pub fn build(&mut self) {
        let mut children = Vec::new();

        // Background
        children.push(Node::color_rect(
            Rect::new(0.0, 0.0, self.width, self.height),
            self.background,
        ));

        // Each window
        for window in &self.windows {
            children.push(self.build_window_node(window));
        }

        self.root = Some(Node::container(children));
    }

    fn build_window_node(&self, window: &WindowScene) -> Node {
        let mut children = Vec::new();

        // Window background
        children.push(Node::color_rect(
            Rect::new(0.0, 0.0, window.bounds.width, window.bounds.height),
            window.background,
        ));

        // TODO: Build text nodes from glyph rows

        // Cursor
        if let Some(cursor) = &window.cursor {
            if cursor.visible {
                children.push(Node::cursor(
                    cursor.style,
                    cursor.color,
                    Rect::new(cursor.x, cursor.y, cursor.width, cursor.height),
                ));
            }
        }

        // Apply window position and scroll offset
        let transform = Transform::translate(
            window.bounds.x,
            window.bounds.y - window.scroll_offset,
        );

        Node::container_with_transform(children, transform)
            .with_clip(window.bounds)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scene_creation() {
        let scene = Scene::new(800.0, 600.0);
        assert_eq!(scene.width, 800.0);
        assert_eq!(scene.height, 600.0);
    }

    #[test]
    fn test_dirty_region() {
        let mut scene = Scene::new(800.0, 600.0);
        scene.mark_region_dirty(Rect::new(10.0, 10.0, 100.0, 100.0));
        scene.mark_region_dirty(Rect::new(50.0, 50.0, 100.0, 100.0));
        
        let dirty = scene.dirty.unwrap();
        assert_eq!(dirty.x, 10.0);
        assert_eq!(dirty.y, 10.0);
        assert_eq!(dirty.right(), 150.0);
        assert_eq!(dirty.bottom(), 150.0);
    }
}
