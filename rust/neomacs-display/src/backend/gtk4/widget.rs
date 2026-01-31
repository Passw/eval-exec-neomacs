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
use super::gsk_renderer::GskRenderer;

/// Inner state for NeomacsWidget
#[derive(Default)]
pub struct NeomacsWidgetInner {
    /// The scene to render
    scene: RefCell<Option<Scene>>,
    /// GSK renderer
    renderer: RefCell<GskRenderer>,
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

        // Initialize Pango context if needed
        if !*self.pango_initialized.borrow() {
            let context = widget.pango_context();
            self.renderer.borrow_mut().init_with_context(context);
            *self.pango_initialized.borrow_mut() = true;
        }

        // Get the scene to render
        let scene_opt = self.scene.borrow();
        
        if let Some(scene) = scene_opt.as_ref() {
            // Use GSK renderer for GPU-accelerated rendering
            self.renderer.borrow_mut().render_to_snapshot(snapshot, scene);
        } else {
            // No scene - draw background
            let rect = graphene::Rect::new(0.0, 0.0, width, height);
            let color = gtk4::gdk::RGBA::new(0.1, 0.1, 0.12, 1.0);
            snapshot.append_color(&color, &rect);
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
