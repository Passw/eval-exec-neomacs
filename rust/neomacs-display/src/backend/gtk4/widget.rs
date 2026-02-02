//! Custom GTK4 widget for Neomacs GPU-accelerated rendering.
//!
//! This widget uses GtkSnapshot for true GPU rendering via GSK render nodes,
//! bypassing the Cairo software rasterization path.
//!
//! Glyphs accumulate in FrameGlyphBuffer - Emacs incremental redisplay only
//! sends changed content. Window backgrounds cover old content in changed areas.
//!
//! Enable logging with: RUST_LOG=neomacs_display::backend::gtk4::widget=debug

use std::cell::RefCell;
use std::sync::{Arc, Mutex};

use gtk4::prelude::*;
use gtk4::subclass::prelude::*;
use gtk4::{glib, graphene, gsk, pango, Snapshot};
use log::{debug, trace, warn, info};

use crate::core::scene::Scene;
use crate::core::scene::FloatingImage;
use crate::core::scene::FloatingWebKit;
use crate::core::frame_glyphs::FrameGlyphBuffer;
use super::gsk_renderer::GskRenderer;
use super::hybrid_renderer::HybridRenderer;
use super::video::VideoCache;
use super::image::ImageCache;
#[cfg(feature = "wpe-webkit")]
use crate::backend::webkit::WebKitCache;

// Thread-local caches for widget rendering
// These are set by the FFI layer before triggering a widget redraw
thread_local! {
    pub static WIDGET_VIDEO_CACHE: RefCell<Option<*const VideoCache>> = const { RefCell::new(None) };
    pub static WIDGET_IMAGE_CACHE: RefCell<Option<*mut ImageCache>> = const { RefCell::new(None) };
    // Store CLONED glyphs, not a pointer - pointer becomes invalid when Emacs clears buffer
    pub static WIDGET_FRAME_GLYPHS: RefCell<Option<FrameGlyphBuffer>> = const { RefCell::new(None) };
    pub static WIDGET_USE_HYBRID: RefCell<bool> = const { RefCell::new(true) };
    // Resize callback - called when widget size changes
    pub static WIDGET_RESIZE_CALLBACK: RefCell<Option<Box<dyn Fn(i32, i32) + Send + 'static>>> = const { RefCell::new(None) };
    // Floating images for overlay rendering in hybrid mode
    pub static WIDGET_FLOATING_IMAGES: RefCell<Vec<FloatingImage>> = const { RefCell::new(Vec::new()) };
    // Floating webkit views for overlay rendering
    pub static WIDGET_FLOATING_WEBKITS: RefCell<Vec<FloatingWebKit>> = const { RefCell::new(Vec::new()) };
    // WebKit cache pointer for rendering webkit textures
    #[cfg(feature = "wpe-webkit")]
    pub static WIDGET_WEBKIT_CACHE: RefCell<Option<*const WebKitCache>> = const { RefCell::new(None) };
    // Mouse button callback - called on button press/release
    // Args: (x, y, button, pressed, modifiers, time)
    pub static WIDGET_MOUSE_BUTTON_CALLBACK: RefCell<Option<Box<dyn Fn(f64, f64, u32, bool, u32, u32) + Send + 'static>>> = const { RefCell::new(None) };
    // Mouse motion callback - called on mouse movement
    // Args: (x, y, modifiers, time)
    pub static WIDGET_MOUSE_MOTION_CALLBACK: RefCell<Option<Box<dyn Fn(f64, f64, u32, u32) + Send + 'static>>> = const { RefCell::new(None) };
    // Mouse scroll callback - called on scroll wheel
    // Args: (x, y, delta_x, delta_y, modifiers, time)
    pub static WIDGET_MOUSE_SCROLL_CALLBACK: RefCell<Option<Box<dyn Fn(f64, f64, f64, f64, u32, u32) + Send + 'static>>> = const { RefCell::new(None) };
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
/// Now CLONES the buffer so it survives Emacs clearing it for the next frame.
/// Only updates if buffer has content - preserves last valid frame to prevent flickering.
pub fn set_widget_frame_glyphs(buffer: *const FrameGlyphBuffer) {
    WIDGET_FRAME_GLYPHS.with(|c| {
        if buffer.is_null() {
            return;
        }
        let new_buffer = unsafe { (*buffer).clone() };
        let glyph_count = new_buffer.glyphs.len();
        // Only update if we have content - prevents black flash from empty buffers
        if !new_buffer.glyphs.is_empty() {
            *c.borrow_mut() = Some(new_buffer);
        } else {
        }
    });
}

/// Set whether to use hybrid rendering mode
pub fn set_widget_use_hybrid(use_hybrid: bool) {
    WIDGET_USE_HYBRID.with(|c| {
        *c.borrow_mut() = use_hybrid;
    });
}

/// Set the resize callback - called when widget size changes
pub fn set_widget_resize_callback<F>(callback: F)
where
    F: Fn(i32, i32) + Send + 'static,
{
    WIDGET_RESIZE_CALLBACK.with(|c| {
        *c.borrow_mut() = Some(Box::new(callback));
    });
}

/// Set floating images for overlay rendering (called from FFI before queue_draw)
pub fn set_widget_floating_images(images: Vec<FloatingImage>) {
    WIDGET_FLOATING_IMAGES.with(|c| {
        *c.borrow_mut() = images;
    });
}

/// Set floating webkit views for overlay rendering in hybrid mode
pub fn set_widget_floating_webkits(webkits: Vec<FloatingWebKit>) {
    WIDGET_FLOATING_WEBKITS.with(|c| {
        *c.borrow_mut() = webkits;
    });
}

/// Set the webkit cache for widget rendering (called from FFI before queue_draw)
#[cfg(feature = "wpe-webkit")]
pub fn set_widget_webkit_cache(cache: *const WebKitCache) {
    WIDGET_WEBKIT_CACHE.with(|c| {
        *c.borrow_mut() = if cache.is_null() { None } else { Some(cache) };
    });
}

/// Set the mouse button callback - called on button press/release
/// Args: (x, y, button, pressed, modifiers, time)
pub fn set_widget_mouse_button_callback<F>(callback: F)
where
    F: Fn(f64, f64, u32, bool, u32, u32) + Send + 'static,
{
    info!("Mouse button callback set");
    WIDGET_MOUSE_BUTTON_CALLBACK.with(|c| {
        *c.borrow_mut() = Some(Box::new(callback));
    });
}

/// Set the mouse motion callback - called on mouse movement
/// Args: (x, y, modifiers, time)
pub fn set_widget_mouse_motion_callback<F>(callback: F)
where
    F: Fn(f64, f64, u32, u32) + Send + 'static,
{
    info!("Mouse motion callback set");
    WIDGET_MOUSE_MOTION_CALLBACK.with(|c| {
        *c.borrow_mut() = Some(Box::new(callback));
    });
}

/// Set the mouse scroll callback - called on scroll wheel
/// Args: (x, y, delta_x, delta_y, modifiers, time)
pub fn set_widget_mouse_scroll_callback<F>(callback: F)
where
    F: Fn(f64, f64, f64, f64, u32, u32) + Send + 'static,
{
    info!("Mouse scroll callback set");
    WIDGET_MOUSE_SCROLL_CALLBACK.with(|c| {
        *c.borrow_mut() = Some(Box::new(callback));
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
    /// Last known size (to detect changes)
    last_size: RefCell<(i32, i32)>,
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

        let widget = self.obj();

        // Request keyboard focus and enable input targeting
        widget.set_focusable(true);
        widget.set_can_focus(true);
        widget.set_can_target(true);  // Enable receiving pointer events
        widget.set_sensitive(true);   // Enable input events

        // Note: Mouse event handlers are added by C code in neomacsfns.c
        // (neomacs_click_pressed_cb, neomacs_motion_cb, neomacs_scroll_cb)
        // The Rust-side handlers via FFI callbacks are for cases where
        // C code wants to delegate to Rust, but currently all mouse handling
        // is done in C for proper Emacs integration.

        info!("NeomacsWidget constructed");
    }
}

impl NeomacsWidgetInner {
    fn snapshot_impl(&self, snapshot: &Snapshot) {
        let widget = self.obj();
        let width = widget.width() as f32;
        let height = widget.height() as f32;
        debug!("snapshot: size={}x{}", width, height);

        // Initialize Pango context for legacy renderer (GSK renderer still uses Pango)
        if !*self.pango_initialized.borrow() {
            let context = widget.pango_context();
            self.renderer.borrow_mut().init_with_context(context.clone());
            // Note: hybrid_renderer now uses cosmic-text, no Pango init needed
            *self.pango_initialized.borrow_mut() = true;
        }

        // Check if using hybrid mode
        let use_hybrid = WIDGET_USE_HYBRID.with(|c| *c.borrow());

        if use_hybrid {
            // Hybrid path: render from CLONED FrameGlyphBuffer (uses cosmic-text)
            // We clone the buffer when setting it, so it survives Emacs clearing
            // the original for the next frame.
            let frame_glyphs: Option<FrameGlyphBuffer> = WIDGET_FRAME_GLYPHS.with(|c| {
                c.borrow().clone()  // Clone the Option<FrameGlyphBuffer>
            });

            // Get floating images from thread-local
            let floating_images: Vec<FloatingImage> = WIDGET_FLOATING_IMAGES.with(|c| {
                c.borrow().clone()
            });

            // Get floating webkits from thread-local
            let floating_webkits: Vec<FloatingWebKit> = WIDGET_FLOATING_WEBKITS.with(|c| {
                c.borrow().clone()
            });

            if let Some(ref buffer) = frame_glyphs {

                // Get video cache from thread-local (mutable for update())
                let mut video_cache = WIDGET_VIDEO_CACHE.with(|c| {
                    c.borrow().map(|ptr| unsafe { &mut *(ptr as *const VideoCache as *mut VideoCache) })
                });

                // Get image cache from thread-local
                let mut image_cache_ptr = WIDGET_IMAGE_CACHE.with(|c| *c.borrow());
                let image_cache = image_cache_ptr.as_mut().map(|ptr| unsafe { &mut **ptr });

                // Get webkit cache from thread-local
                #[cfg(feature = "wpe-webkit")]
                let webkit_cache = WIDGET_WEBKIT_CACHE.with(|c| {
                    c.borrow().map(|ptr| unsafe { &*ptr })
                });
                #[cfg(not(feature = "wpe-webkit"))]
                let webkit_cache: Option<()> = None;

                // Build render node with hybrid renderer
                let mut renderer = self.hybrid_renderer.borrow_mut();

                if let Some(node) = renderer.build_render_node(buffer, video_cache, image_cache, &floating_images, &floating_webkits, webkit_cache) {
                    snapshot.append_node(&node);
                } else {
                    let rect = graphene::Rect::new(0.0, 0.0, width, height);
                    let color = gtk4::gdk::RGBA::new(0.5, 0.1, 0.1, 1.0);
                    snapshot.append_color(&color, &rect);
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
}

impl WidgetImpl for NeomacsWidgetInner {
    fn snapshot(&self, snapshot: &Snapshot) {
        // Wrap everything in catch_unwind to prevent panics from crashing Emacs
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.snapshot_impl(snapshot);
        }));
        
        if let Err(e) = result {
            eprintln!("PANIC in snapshot: {:?}", e);
            warn!("PANIC in snapshot: {:?}", e);
            // Draw error background
            let widget = self.obj();
            let width = widget.width() as f32;
            let height = widget.height() as f32;
            let rect = graphene::Rect::new(0.0, 0.0, width, height);
            let color = gtk4::gdk::RGBA::new(1.0, 0.0, 0.0, 1.0);
            snapshot.append_color(&color, &rect);
        }
    }

    fn size_allocate(&self, width: i32, height: i32, baseline: i32) {
        // Call parent implementation
        self.parent_size_allocate(width, height, baseline);
        
        // Check if size changed
        let last_size = *self.last_size.borrow();
        if last_size != (width, height) {
            *self.last_size.borrow_mut() = (width, height);
            debug!("NeomacsWidget size_allocate: {}x{}", width, height);
            
            // Call resize callback if set
            WIDGET_RESIZE_CALLBACK.with(|c| {
                if let Some(ref callback) = *c.borrow() {
                    callback(width, height);
                }
            });
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
