//! Custom GTK4 widget for Neomacs GPU-accelerated rendering.
//!
//! This widget uses GtkSnapshot for true GPU rendering via GSK render nodes,
//! bypassing the Cairo software rasterization path.

use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use gtk4::prelude::*;
use gtk4::subclass::prelude::*;
use gtk4::{glib, graphene, gsk, pango, Snapshot};

use crate::core::scene::Scene;
use crate::core::frame_glyphs::FrameGlyphBuffer;
use super::gsk_renderer::GskRenderer;
use super::hybrid_renderer::HybridRenderer;
use super::video::VideoCache;
use super::image::ImageCache;

// Thread-local caches for widget rendering
// These are set by the FFI layer before triggering a widget redraw
thread_local! {
    pub static WIDGET_VIDEO_CACHE: RefCell<Option<*const VideoCache>> = const { RefCell::new(None) };
    pub static WIDGET_IMAGE_CACHE: RefCell<Option<*mut ImageCache>> = const { RefCell::new(None) };
    pub static WIDGET_FRAME_GLYPHS: RefCell<Option<*const FrameGlyphBuffer>> = const { RefCell::new(None) };
    pub static WIDGET_USE_HYBRID: RefCell<bool> = const { RefCell::new(true) };
}

/// Set the video cache for widget rendering (called from FFI before queue_draw)
pub fn set_widget_video_cache(cache: *const VideoCache) {
    WIDGET_VIDEO_CACHE.with(|c| {
        *c.borrow_mut() = if cache.is_null() { None } else { Some(cache) };
    });
}

/// Set the image cache for widget rendering (called from FFI before queue_draw)
pub fn set_widget_image_cache(cache: *mut ImageCache) {
    WIDGET_IMAGE_CACHE.with(|c| {
        *c.borrow_mut() = if cache.is_null() { None } else { Some(cache) };
    });
}

/// Set the frame glyph buffer for hybrid rendering (called from FFI before queue_draw)
pub fn set_widget_frame_glyphs(buffer: *const FrameGlyphBuffer) {
    WIDGET_FRAME_GLYPHS.with(|c| {
        *c.borrow_mut() = if buffer.is_null() { None } else { Some(buffer) };
    });
}

/// Set whether to use hybrid rendering mode
pub fn set_widget_use_hybrid(use_hybrid: bool) {
    WIDGET_USE_HYBRID.with(|c| {
        *c.borrow_mut() = use_hybrid;
    });
}

/// Inner state for NeomacsWidget
#[derive(Default)]
pub struct NeomacsWidgetInner {
    /// The scene to render (legacy mode)
    scene: RefCell<Option<Scene>>,
    /// GSK renderer (legacy mode)
    renderer: RefCell<GskRenderer>,
    /// Hybrid renderer (hybrid mode)
    hybrid_renderer: RefCell<HybridRenderer>,
    /// Whether Pango context has been initialized
    pango_initialized: RefCell<bool>,
}

/// GObject subclass implementation
#[glib::object_subclass]
impl ObjectSubclass for NeomacsWidgetInner {
    const NAME: &'static str = "NeomacsWidget";
    type Type = NeomacsWidget;
    type ParentType = gtk4::Widget;
}

impl ObjectImpl for NeomacsWidgetInner {
    fn constructed(&self) {
        self.parent_constructed();

        // Request keyboard focus
        self.obj().set_focusable(true);
        self.obj().set_can_focus(true);
    }
}

impl WidgetImpl for NeomacsWidgetInner {
    fn snapshot(&self, snapshot: &Snapshot) {
        let widget = self.obj();
        let width = widget.width() as f32;
        let height = widget.height() as f32;

        // Initialize Pango context if needed (for both renderers)
        if !*self.pango_initialized.borrow() {
            let context = widget.pango_context();
            self.renderer.borrow_mut().init_with_context(context.clone());
            self.hybrid_renderer.borrow_mut().init_with_context(context);
            *self.pango_initialized.borrow_mut() = true;
        }

        // Check if using hybrid mode
        let use_hybrid = WIDGET_USE_HYBRID.with(|c| *c.borrow());

        if use_hybrid {
            // Hybrid path: render from FrameGlyphBuffer
            let frame_glyphs = WIDGET_FRAME_GLYPHS.with(|c| {
                c.borrow().map(|ptr| unsafe { &*ptr })
            });

            if let Some(buffer) = frame_glyphs {
                // Get video cache from thread-local
                let video_cache = WIDGET_VIDEO_CACHE.with(|c| {
                    c.borrow().map(|ptr| unsafe { &*ptr })
                });

                // Get image cache from thread-local
                let mut image_cache_ptr = WIDGET_IMAGE_CACHE.with(|c| *c.borrow());
                let image_cache = image_cache_ptr.as_mut().map(|ptr| unsafe { &mut **ptr });

                // Build render node with hybrid renderer
                let mut renderer = self.hybrid_renderer.borrow_mut();
                if let Some(node) = renderer.build_render_node(buffer, video_cache, image_cache) {
                    snapshot.append_node(&node);
                }
            } else {
                // No frame glyphs - draw background
                let rect = graphene::Rect::new(0.0, 0.0, width, height);
                let color = gtk4::gdk::RGBA::new(0.1, 0.1, 0.12, 1.0);
                snapshot.append_color(&color, &rect);
            }
        } else {
            // Legacy path: render from Scene
            let scene_opt = self.scene.borrow();

            if let Some(scene) = scene_opt.as_ref() {
                // Get video cache from thread-local (set by FFI before queue_draw)
                let video_cache = WIDGET_VIDEO_CACHE.with(|c| {
                    c.borrow().map(|ptr| unsafe { &*ptr })
                });

                // Get image cache from thread-local
                let mut image_cache_ptr = WIDGET_IMAGE_CACHE.with(|c| *c.borrow());
                let image_cache = image_cache_ptr.as_mut().map(|ptr| unsafe { &mut **ptr });

                // Build render node with caches
                let mut renderer = self.renderer.borrow_mut();
                if let Some(node) = renderer.build_render_node(scene, video_cache, image_cache, None) {
                    snapshot.append_node(&node);
                }
            } else {
                // No scene - draw background
                let rect = graphene::Rect::new(0.0, 0.0, width, height);
                let color = gtk4::gdk::RGBA::new(0.1, 0.1, 0.12, 1.0);
                snapshot.append_color(&color, &rect);
            }
        }
    }

    fn measure(&self, orientation: gtk4::Orientation, _for_size: i32) -> (i32, i32, i32, i32) {
        // Return reasonable defaults
        match orientation {
            gtk4::Orientation::Horizontal => (400, 800, -1, -1),
            gtk4::Orientation::Vertical => (300, 600, -1, -1),
            _ => (100, 400, -1, -1),
        }
    }
}

glib::wrapper! {
    /// Custom widget for GPU-accelerated Emacs rendering.
    pub struct NeomacsWidget(ObjectSubclass<NeomacsWidgetInner>)
        @extends gtk4::Widget,
        @implements gtk4::Accessible;
}

impl Default for NeomacsWidget {
    fn default() -> Self {
        Self::new()
    }
}

impl NeomacsWidget {
    /// Create a new NeomacsWidget
    pub fn new() -> Self {
        glib::Object::builder().build()
    }

    /// Update the scene to render
    pub fn set_scene(&self, scene: Scene) {
        let inner = self.imp();
        *inner.scene.borrow_mut() = Some(scene);
        self.queue_draw();
    }

    /// Clear the scene
    pub fn clear_scene(&self) {
        let inner = self.imp();
        *inner.scene.borrow_mut() = None;
        self.queue_draw();
    }

    /// Get access to the renderer (for face registration, etc.)
    pub fn with_renderer<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut GskRenderer) -> R,
    {
        let inner = self.imp();
        f(&mut inner.renderer.borrow_mut())
    }
}
