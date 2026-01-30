# Modern Emacs Display Engine Design

## Goals

1. **GPU-accelerated rendering** via GTK4/GSK (Vulkan, Metal, OpenGL)
2. **Only 2 backends**: TTY and GTK4
3. **Enable rich content**: 4K video, WebKit browser, smooth animations
4. **Maintain Lisp API compatibility** (faces, overlays, text properties)
5. **Cross-platform**: Linux, macOS, Windows via GTK4
6. **Written in Rust** for memory safety and modern tooling

---

## Rust Architecture

### Why Rust?

| Benefit | Description |
|---------|-------------|
| **Memory Safety** | No use-after-free, buffer overflows, data races |
| **Modern Tooling** | Cargo, rustfmt, clippy, excellent IDE support |
| **GTK4 Bindings** | `gtk4-rs` is mature and well-maintained |
| **GStreamer Bindings** | `gstreamer-rs` is official and complete |
| **FFI** | Easy to expose C API for Emacs core |
| **Performance** | Zero-cost abstractions, no GC |
| **Concurrency** | Safe async/threading for video decode, network |

### Architecture: Rust Display Library + C FFI

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Emacs Core (C)                                   │
│  Lisp interpreter, buffers, windows, faces, overlays                │
└─────────────────────────────────┬───────────────────────────────────┘
                                  │ C FFI
                                  ▼
┌─────────────────────────────────────────────────────────────────────┐
│              libemacs_display (Rust)                                │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    C API Layer (ffi.rs)                      │   │
│  │  emacs_display_init(), emacs_display_render_frame(), etc.   │   │
│  └─────────────────────────────────┬───────────────────────────┘   │
│                                    │                                │
│  ┌─────────────────────────────────▼───────────────────────────┐   │
│  │                  Display Engine Core                         │   │
│  │  scene.rs, layout.rs, damage.rs, animation.rs               │   │
│  └─────────────────────────────────┬───────────────────────────┘   │
│                                    │                                │
│  ┌──────────────┬──────────────────┼──────────────┬────────────┐   │
│  │ TTY Backend  │  GTK4 Backend    │ Video        │ WPE WebKit │   │
│  │ (tty.rs)     │  (gtk4.rs)       │ (video.rs)   │ (wpe.rs)   │   │
│  │              │  gtk4-rs         │ gstreamer-rs │ wpe-rs     │   │
│  └──────────────┴──────────────────┴──────────────┴────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

### Crate Structure

```
emacs-display/
├── Cargo.toml
├── src/
│   ├── lib.rs              # Library root
│   ├── ffi.rs              # C FFI interface (extern "C" functions)
│   │
│   ├── core/
│   │   ├── mod.rs
│   │   ├── scene.rs        # Scene graph
│   │   ├── layout.rs       # Text layout
│   │   ├── glyph.rs        # Glyph types
│   │   ├── face.rs         # Face/style handling
│   │   ├── damage.rs       # Dirty region tracking
│   │   ├── animation.rs    # Smooth scrolling, transitions
│   │   └── atlas.rs        # Glyph texture atlas
│   │
│   ├── backend/
│   │   ├── mod.rs          # Backend trait
│   │   ├── tty.rs          # TTY/terminal backend
│   │   └── gtk4/
│   │       ├── mod.rs      # GTK4 backend
│   │       ├── text.rs     # Text rendering (Pango)
│   │       ├── image.rs    # Image rendering
│   │       ├── video.rs    # Video (GStreamer)
│   │       └── wpe.rs      # WPE WebKit
│   │
│   └── types/
│       ├── mod.rs
│       ├── color.rs        # Color types
│       ├── rect.rs         # Rectangle/geometry
│       └── buffer.rs       # Buffer/text types
│
├── cbindgen.toml           # Generate C headers
└── build.rs                # Build script
```

### Cargo.toml

```toml
[package]
name = "emacs-display"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["staticlib", "cdylib"]  # For linking with Emacs

[dependencies]
# GTK4 bindings
gtk4 = "0.9"
gdk4 = "0.9"
gsk4 = "0.9"
graphene = "0.20"
pango = "0.20"
pangocairo = "0.20"
cairo-rs = "0.20"

# GStreamer for video
gstreamer = "0.23"
gstreamer-video = "0.23"
gstreamer-app = "0.23"

# Async runtime (for video decode, network)
tokio = { version = "1", features = ["rt-multi-thread", "sync"] }

# Utilities
log = "0.4"
thiserror = "1.0"
bitflags = "2.0"

# Optional: WPE WebKit (when bindings mature)
# wpe-webkit = "0.1"

[build-dependencies]
cbindgen = "0.26"  # Generate C headers

[features]
default = ["gtk4-backend"]
gtk4-backend = []
tty-backend = []
video = ["gstreamer", "gstreamer-video"]
webkit = []  # WPE WebKit
```

### Core Types (Rust)

```rust
// src/core/glyph.rs

/// Glyph types matching Emacs display model
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlyphType {
    Char,
    Composite,
    Glyphless,
    Image,
    Stretch,
    XWidget,
    Video,    // NEW
    Wpe,      // NEW
}

/// A single glyph in the display
#[repr(C)]
#[derive(Debug, Clone)]
pub struct Glyph {
    pub glyph_type: GlyphType,
    pub charcode: u32,
    pub face_id: u32,
    pub pixel_width: i32,
    pub ascent: i32,
    pub descent: i32,
    pub charpos: i64,      // Position in buffer
    
    // Type-specific data
    pub data: GlyphData,
}

#[derive(Debug, Clone)]
pub enum GlyphData {
    Char { code: char },
    Image { image_id: u32 },
    Video { video_id: u32 },
    Wpe { view_id: u32 },
    Stretch { width: i32 },
    None,
}
```

```rust
// src/core/scene.rs

use crate::types::{Color, Rect};

/// Scene graph node types
#[derive(Debug)]
pub enum NodeKind {
    /// Container with children and optional transform
    Container {
        children: Vec<Node>,
        transform: Option<Transform>,
    },
    
    /// Text run with same face
    TextRun {
        text: String,
        face_id: u32,
        glyphs: Vec<ShapedGlyph>,
    },
    
    /// Image
    Image {
        texture_id: u32,
        source_rect: Option<Rect>,
    },
    
    /// Video frame
    Video {
        video_id: u32,
    },
    
    /// WPE WebKit view
    Wpe {
        view_id: u32,
    },
    
    /// Solid color rectangle
    ColorRect {
        color: Color,
    },
}

/// A node in the scene graph
#[derive(Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub bounds: Rect,
    pub opacity: f32,
    pub clip: Option<Rect>,
}

/// Complete scene for a frame
pub struct Scene {
    pub root: Node,
    pub glyph_atlas: GlyphAtlas,
    pub dirty_region: Option<Rect>,
}

impl Scene {
    pub fn new() -> Self {
        Self {
            root: Node::container(Vec::new()),
            glyph_atlas: GlyphAtlas::new(2048, 2048),
            dirty_region: None,
        }
    }
    
    /// Build scene from Emacs window/glyph data
    pub fn build_from_frame(&mut self, frame: &FrameData) {
        let mut children = Vec::new();
        
        for window in &frame.windows {
            children.push(self.build_window_node(window));
        }
        
        self.root = Node::container(children);
    }
    
    fn build_window_node(&mut self, window: &WindowData) -> Node {
        let mut layers = Vec::new();
        
        // Background
        layers.push(Node::color_rect(window.bounds, window.background_color));
        
        // Text content
        for row in &window.glyph_rows {
            layers.push(self.build_row_node(row));
        }
        
        // Cursor
        if let Some(cursor) = &window.cursor {
            layers.push(self.build_cursor_node(cursor));
        }
        
        Node::container_with_transform(
            layers,
            Transform::translate(window.x as f32, window.y as f32 - window.scroll_offset),
        )
    }
}
```

### Backend Trait

```rust
// src/backend/mod.rs

use crate::core::Scene;

/// Backend trait - implemented by TTY and GTK4
pub trait DisplayBackend: Send {
    /// Initialize the backend
    fn init(&mut self) -> Result<(), BackendError>;
    
    /// Render a scene
    fn render(&mut self, scene: &Scene) -> Result<(), BackendError>;
    
    /// Handle resize
    fn resize(&mut self, width: i32, height: i32);
    
    /// Get backend capabilities
    fn capabilities(&self) -> BackendCapabilities;
    
    /// Shutdown
    fn shutdown(&mut self);
}

#[derive(Debug, Clone)]
pub struct BackendCapabilities {
    pub supports_images: bool,
    pub supports_video: bool,
    pub supports_webkit: bool,
    pub supports_animations: bool,
    pub gpu_accelerated: bool,
}

impl BackendCapabilities {
    pub fn tty() -> Self {
        Self {
            supports_images: false,  // Sixel could be added
            supports_video: false,
            supports_webkit: false,
            supports_animations: false,
            gpu_accelerated: false,
        }
    }
    
    pub fn gtk4() -> Self {
        Self {
            supports_images: true,
            supports_video: true,
            supports_webkit: true,
            supports_animations: true,
            gpu_accelerated: true,
        }
    }
}
```

### GTK4 Backend

```rust
// src/backend/gtk4/mod.rs

use gtk4::prelude::*;
use gtk4::{Application, ApplicationWindow, DrawingArea};
use gsk4::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Gtk4Backend {
    app: gtk4::Application,
    window: Option<ApplicationWindow>,
    drawing_area: Option<DrawingArea>,
    scene: Rc<RefCell<Option<Scene>>>,
    glyph_atlas: GlyphAtlas,
    video_manager: VideoManager,
    wpe_manager: WpeManager,
}

impl Gtk4Backend {
    pub fn new() -> Self {
        let app = Application::builder()
            .application_id("org.gnu.emacs")
            .build();
        
        Self {
            app,
            window: None,
            drawing_area: None,
            scene: Rc::new(RefCell::new(None)),
            glyph_atlas: GlyphAtlas::new(2048, 2048),
            video_manager: VideoManager::new(),
            wpe_manager: WpeManager::new(),
        }
    }
    
    fn setup_drawing_area(&mut self) {
        let drawing_area = DrawingArea::new();
        let scene = self.scene.clone();
        let atlas = self.glyph_atlas.clone();
        let video_mgr = self.video_manager.clone();
        
        drawing_area.set_draw_func(move |_, snapshot, width, height| {
            if let Some(ref scene) = *scene.borrow() {
                render_scene_to_snapshot(snapshot, scene, &atlas, &video_mgr);
            }
        });
        
        self.drawing_area = Some(drawing_area);
    }
}

impl DisplayBackend for Gtk4Backend {
    fn render(&mut self, scene: &Scene) -> Result<(), BackendError> {
        *self.scene.borrow_mut() = Some(scene.clone());
        
        if let Some(ref area) = self.drawing_area {
            area.queue_draw();
        }
        
        Ok(())
    }
    
    fn capabilities(&self) -> BackendCapabilities {
        BackendCapabilities::gtk4()
    }
}

/// Render scene graph to GTK4 snapshot
fn render_scene_to_snapshot(
    snapshot: &gtk4::Snapshot,
    scene: &Scene,
    atlas: &GlyphAtlas,
    video_mgr: &VideoManager,
) {
    render_node(snapshot, &scene.root, atlas, video_mgr);
}

fn render_node(
    snapshot: &gtk4::Snapshot,
    node: &Node,
    atlas: &GlyphAtlas,
    video_mgr: &VideoManager,
) {
    // Apply transform if any
    if let Some(transform) = node.transform() {
        snapshot.save();
        snapshot.transform(Some(&transform.to_gsk()));
    }
    
    // Apply clip if any
    if let Some(clip) = &node.clip {
        snapshot.push_clip(&clip.to_graphene());
    }
    
    // Apply opacity if not 1.0
    if node.opacity < 1.0 {
        snapshot.push_opacity(node.opacity as f64);
    }
    
    // Render based on node kind
    match &node.kind {
        NodeKind::Container { children, .. } => {
            for child in children {
                render_node(snapshot, child, atlas, video_mgr);
            }
        }
        
        NodeKind::TextRun { text, face_id, glyphs } => {
            render_text_run(snapshot, text, *face_id, glyphs, atlas);
        }
        
        NodeKind::Image { texture_id, source_rect } => {
            if let Some(texture) = atlas.get_texture(*texture_id) {
                snapshot.append_texture(texture, &node.bounds.to_graphene());
            }
        }
        
        NodeKind::Video { video_id } => {
            if let Some(texture) = video_mgr.get_current_frame(*video_id) {
                snapshot.append_texture(&texture, &node.bounds.to_graphene());
            }
        }
        
        NodeKind::Wpe { view_id } => {
            // WPE rendering handled similarly
        }
        
        NodeKind::ColorRect { color } => {
            snapshot.append_color(
                &color.to_gdk(),
                &node.bounds.to_graphene(),
            );
        }
    }
    
    // Pop transforms/clips
    if node.opacity < 1.0 {
        snapshot.pop();
    }
    if node.clip.is_some() {
        snapshot.pop();
    }
    if node.transform().is_some() {
        snapshot.restore();
    }
}

/// Render text using Pango
fn render_text_run(
    snapshot: &gtk4::Snapshot,
    text: &str,
    face_id: u32,
    glyphs: &[ShapedGlyph],
    atlas: &GlyphAtlas,
) {
    let pango_context = snapshot.pango_context();
    let layout = pango::Layout::new(&pango_context);
    
    // Set font from face_id
    let font_desc = get_font_description(face_id);
    layout.set_font_description(Some(&font_desc));
    layout.set_text(text);
    
    // Get face colors
    let (fg_color, bg_color) = get_face_colors(face_id);
    
    // Render using GSK
    snapshot.append_layout(&layout, &fg_color.to_gdk());
}
```

### Video Manager (GStreamer)

```rust
// src/backend/gtk4/video.rs

use gstreamer as gst;
use gstreamer::prelude::*;
use gtk4::gdk;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

pub struct VideoManager {
    videos: HashMap<u32, Video>,
    next_id: u32,
}

struct Video {
    pipeline: gst::Pipeline,
    paintable: gtk4::gdk::Paintable,
    state: VideoState,
    width: i32,
    height: i32,
}

#[derive(Debug, Clone, Copy)]
pub enum VideoState {
    Stopped,
    Playing,
    Paused,
}

impl VideoManager {
    pub fn new() -> Self {
        gst::init().expect("Failed to initialize GStreamer");
        
        Self {
            videos: HashMap::new(),
            next_id: 0,
        }
    }
    
    /// Create a new video from URI
    pub fn create_video(&mut self, uri: &str, width: i32, height: i32) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        
        // Create GStreamer pipeline with GTK4 sink
        let pipeline = gst::Pipeline::new();
        
        let source = gst::ElementFactory::make("uridecodebin")
            .property("uri", uri)
            .build()
            .unwrap();
        
        let convert = gst::ElementFactory::make("videoconvert")
            .build()
            .unwrap();
        
        let scale = gst::ElementFactory::make("videoscale")
            .build()
            .unwrap();
        
        // GTK4 paintable sink - renders to GdkPaintable
        let sink = gst::ElementFactory::make("gtk4paintablesink")
            .build()
            .unwrap();
        
        pipeline.add_many([&source, &convert, &scale, &sink]).unwrap();
        gst::Element::link_many([&convert, &scale, &sink]).unwrap();
        
        // Connect dynamic pads from uridecodebin
        source.connect_pad_added(move |_, src_pad| {
            let sink_pad = convert.static_pad("sink").unwrap();
            src_pad.link(&sink_pad).unwrap();
        });
        
        // Get the paintable for rendering
        let paintable: gdk::Paintable = sink.property("paintable");
        
        let video = Video {
            pipeline,
            paintable,
            state: VideoState::Stopped,
            width,
            height,
        };
        
        self.videos.insert(id, video);
        id
    }
    
    /// Get current frame as texture
    pub fn get_current_frame(&self, video_id: u32) -> Option<gdk::Texture> {
        self.videos.get(&video_id).map(|v| {
            v.paintable.current_image()
        })
    }
    
    /// Play video
    pub fn play(&mut self, video_id: u32) {
        if let Some(video) = self.videos.get_mut(&video_id) {
            video.pipeline.set_state(gst::State::Playing).unwrap();
            video.state = VideoState::Playing;
        }
    }
    
    /// Pause video
    pub fn pause(&mut self, video_id: u32) {
        if let Some(video) = self.videos.get_mut(&video_id) {
            video.pipeline.set_state(gst::State::Paused).unwrap();
            video.state = VideoState::Paused;
        }
    }
    
    /// Seek to position (seconds)
    pub fn seek(&mut self, video_id: u32, position: f64) {
        if let Some(video) = self.videos.get(&video_id) {
            video.pipeline.seek_simple(
                gst::SeekFlags::FLUSH | gst::SeekFlags::KEY_UNIT,
                gst::ClockTime::from_seconds_f64(position),
            ).unwrap();
        }
    }
}
```

### C FFI Layer

```rust
// src/ffi.rs

use std::ffi::{c_char, c_int, c_void, CStr};
use std::ptr;

use crate::core::{Scene, Glyph, GlyphType};
use crate::backend::{DisplayBackend, Gtk4Backend, TtyBackend};

/// Opaque handle to display engine
pub struct DisplayEngine {
    backend: Box<dyn DisplayBackend>,
    scene: Scene,
}

// ============ Initialization ============

/// Initialize the display engine
/// Returns opaque handle, or NULL on failure
#[no_mangle]
pub extern "C" fn emacs_display_init(backend_type: c_int) -> *mut DisplayEngine {
    let backend: Box<dyn DisplayBackend> = match backend_type {
        0 => Box::new(TtyBackend::new()),
        1 => Box::new(Gtk4Backend::new()),
        _ => return ptr::null_mut(),
    };
    
    let engine = Box::new(DisplayEngine {
        backend,
        scene: Scene::new(),
    });
    
    Box::into_raw(engine)
}

/// Shutdown and free the display engine
#[no_mangle]
pub extern "C" fn emacs_display_shutdown(engine: *mut DisplayEngine) {
    if !engine.is_null() {
        unsafe {
            let mut engine = Box::from_raw(engine);
            engine.backend.shutdown();
        }
    }
}

// ============ Scene Building ============

/// Begin building a new frame
#[no_mangle]
pub extern "C" fn emacs_display_begin_frame(engine: *mut DisplayEngine) {
    let engine = unsafe { &mut *engine };
    engine.scene = Scene::new();
}

/// Add a glyph row to the scene
#[no_mangle]
pub extern "C" fn emacs_display_add_glyph_row(
    engine: *mut DisplayEngine,
    window_id: c_int,
    row_y: c_int,
    glyphs: *const Glyph,
    glyph_count: c_int,
) {
    let engine = unsafe { &mut *engine };
    let glyphs = unsafe { 
        std::slice::from_raw_parts(glyphs, glyph_count as usize) 
    };
    
    engine.scene.add_glyph_row(window_id, row_y, glyphs);
}

/// End frame and render
#[no_mangle]
pub extern "C" fn emacs_display_end_frame(engine: *mut DisplayEngine) -> c_int {
    let engine = unsafe { &mut *engine };
    
    match engine.backend.render(&engine.scene) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

// ============ Video ============

/// Create a video
#[no_mangle]
pub extern "C" fn emacs_display_create_video(
    engine: *mut DisplayEngine,
    uri: *const c_char,
    width: c_int,
    height: c_int,
) -> u32 {
    let engine = unsafe { &mut *engine };
    let uri = unsafe { CStr::from_ptr(uri).to_str().unwrap_or("") };
    
    if let Some(gtk4) = engine.backend.as_gtk4_mut() {
        gtk4.video_manager.create_video(uri, width, height)
    } else {
        u32::MAX  // Error: video not supported
    }
}

/// Play video
#[no_mangle]
pub extern "C" fn emacs_display_video_play(engine: *mut DisplayEngine, video_id: u32) {
    let engine = unsafe { &mut *engine };
    if let Some(gtk4) = engine.backend.as_gtk4_mut() {
        gtk4.video_manager.play(video_id);
    }
}

/// Pause video
#[no_mangle]
pub extern "C" fn emacs_display_video_pause(engine: *mut DisplayEngine, video_id: u32) {
    let engine = unsafe { &mut *engine };
    if let Some(gtk4) = engine.backend.as_gtk4_mut() {
        gtk4.video_manager.pause(video_id);
    }
}

/// Seek video
#[no_mangle]
pub extern "C" fn emacs_display_video_seek(
    engine: *mut DisplayEngine,
    video_id: u32,
    position: f64,
) {
    let engine = unsafe { &mut *engine };
    if let Some(gtk4) = engine.backend.as_gtk4_mut() {
        gtk4.video_manager.seek(video_id, position);
    }
}

// ============ WPE WebKit ============

/// Create a WPE WebKit view
#[no_mangle]
pub extern "C" fn emacs_display_create_wpe(
    engine: *mut DisplayEngine,
    uri: *const c_char,
    width: c_int,
    height: c_int,
) -> u32 {
    let engine = unsafe { &mut *engine };
    let uri = unsafe { CStr::from_ptr(uri).to_str().unwrap_or("") };
    
    if let Some(gtk4) = engine.backend.as_gtk4_mut() {
        gtk4.wpe_manager.create_view(uri, width, height)
    } else {
        u32::MAX
    }
}

/// Load URI in WPE view
#[no_mangle]
pub extern "C" fn emacs_display_wpe_load_uri(
    engine: *mut DisplayEngine,
    view_id: u32,
    uri: *const c_char,
) {
    let engine = unsafe { &mut *engine };
    let uri = unsafe { CStr::from_ptr(uri).to_str().unwrap_or("") };
    
    if let Some(gtk4) = engine.backend.as_gtk4_mut() {
        gtk4.wpe_manager.load_uri(view_id, uri);
    }
}
```

### Generated C Header (via cbindgen)

```c
// emacs_display.h (auto-generated)

#ifndef EMACS_DISPLAY_H
#define EMACS_DISPLAY_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque handle
typedef struct DisplayEngine DisplayEngine;

// Backend types
#define EMACS_BACKEND_TTY  0
#define EMACS_BACKEND_GTK4 1

// Glyph types
typedef enum {
    GLYPH_CHAR = 0,
    GLYPH_COMPOSITE,
    GLYPH_GLYPHLESS,
    GLYPH_IMAGE,
    GLYPH_STRETCH,
    GLYPH_XWIDGET,
    GLYPH_VIDEO,
    GLYPH_WPE,
} GlyphType;

// Glyph structure (matches Rust repr(C))
typedef struct {
    GlyphType glyph_type;
    uint32_t charcode;
    uint32_t face_id;
    int32_t pixel_width;
    int32_t ascent;
    int32_t descent;
    int64_t charpos;
    // ... additional fields
} Glyph;

// Initialization
DisplayEngine* emacs_display_init(int backend_type);
void emacs_display_shutdown(DisplayEngine* engine);

// Frame rendering
void emacs_display_begin_frame(DisplayEngine* engine);
void emacs_display_add_glyph_row(DisplayEngine* engine, int window_id, 
                                  int row_y, const Glyph* glyphs, int count);
int emacs_display_end_frame(DisplayEngine* engine);

// Video
uint32_t emacs_display_create_video(DisplayEngine* engine, const char* uri,
                                     int width, int height);
void emacs_display_video_play(DisplayEngine* engine, uint32_t video_id);
void emacs_display_video_pause(DisplayEngine* engine, uint32_t video_id);
void emacs_display_video_seek(DisplayEngine* engine, uint32_t video_id, double pos);

// WPE WebKit
uint32_t emacs_display_create_wpe(DisplayEngine* engine, const char* uri,
                                   int width, int height);
void emacs_display_wpe_load_uri(DisplayEngine* engine, uint32_t view_id, 
                                 const char* uri);

#ifdef __cplusplus
}
#endif

#endif // EMACS_DISPLAY_H
```

### Integration with Emacs C Code

```c
// In Emacs src/dispnew.c (modified)

#include "emacs_display.h"

static DisplayEngine *display_engine = NULL;

void
init_display_engine (void)
{
#ifdef HAVE_PGTK
  display_engine = emacs_display_init(EMACS_BACKEND_GTK4);
#else
  display_engine = emacs_display_init(EMACS_BACKEND_TTY);
#endif
  
  if (!display_engine)
    fatal("Failed to initialize display engine");
}

void
update_frame_1 (struct frame *f, bool force_p)
{
  emacs_display_begin_frame(display_engine);
  
  // Convert Emacs windows/glyphs to Rust display format
  for (struct window *w = f->root_window; w; w = next_window(w))
    {
      for (int y = 0; y < w->matrix->nrows; y++)
        {
          struct glyph_row *row = w->matrix->rows + y;
          if (row->enabled_p)
            {
              // Convert glyph_row to Glyph array
              Glyph *glyphs = convert_glyph_row(row);
              emacs_display_add_glyph_row(display_engine, 
                                          w->window_id, 
                                          y * row->height,
                                          glyphs,
                                          row->used[TEXT_AREA]);
              free(glyphs);
            }
        }
    }
  
  emacs_display_end_frame(display_engine);
}
```

### Build Integration

```makefile
# In Emacs Makefile.in (additions)

RUST_DISPLAY_LIB = libemacs_display.a
RUST_DISPLAY_DIR = $(srcdir)/../emacs-display

$(RUST_DISPLAY_LIB):
	cd $(RUST_DISPLAY_DIR) && cargo build --release
	cp $(RUST_DISPLAY_DIR)/target/release/libemacs_display.a .

emacs$(EXEEXT): $(RUST_DISPLAY_LIB) ...
	$(CC) ... -L. -lemacs_display $(GTK4_LIBS) $(GSTREAMER_LIBS) ...
```

---

## Benefits of Rust Implementation

| Aspect | C (current) | Rust (new) |
|--------|-------------|------------|
| Memory safety | Manual, error-prone | Guaranteed at compile time |
| Concurrency | Dangerous | Safe async, no data races |
| Error handling | errno, NULL checks | Result<T, E>, Option<T> |
| Dependencies | Manual, pkg-config | Cargo, automatic |
| Testing | Separate framework | Built-in `cargo test` |
| Documentation | Manual | `cargo doc`, inline |
| Tooling | Varies | rustfmt, clippy, IDE support |

---

## File Structure (Final)

```
emacs/
├── src/                    # Emacs C core (unchanged mostly)
│   ├── dispnew.c          # Modified: calls Rust FFI
│   ├── xdisp.c            # Modified: builds data for Rust
│   └── ...
│
└── emacs-display/          # NEW: Rust display engine
    ├── Cargo.toml
    ├── cbindgen.toml
    └── src/
        ├── lib.rs
        ├── ffi.rs          # C FFI
        ├── core/           # Scene graph, layout
        └── backend/        # TTY, GTK4, video, WPE
```

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Lisp Layer                                  │
│  (faces, overlays, text-properties, display tables, images)         │
└─────────────────────────────────┬───────────────────────────────────┘
                                  │
┌─────────────────────────────────▼───────────────────────────────────┐
│                    Display Model (NEW)                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                 │
│  │ TextLayer   │  │ ImageLayer  │  │ WidgetLayer │                 │
│  │ (glyphs)    │  │ (bitmaps)   │  │ (webkit,etc)│                 │
│  └─────────────┘  └─────────────┘  └─────────────┘                 │
│                    Scene Graph / Render Tree                        │
└─────────────────────────────────┬───────────────────────────────────┘
                                  │
┌─────────────────────────────────▼───────────────────────────────────┐
│                    Rendering Interface                              │
│            (abstract API for backends to implement)                 │
└──────────────┬──────────────────────────────────┬───────────────────┘
               │                                  │
┌──────────────▼──────────────┐    ┌──────────────▼──────────────────┐
│      TTY Backend            │    │         GTK4 Backend            │
│  (ANSI escape sequences)    │    │  (GSK Scene Graph → GPU)        │
│  - Character cells only     │    │  - Hardware accelerated         │
│  - No images/video          │    │  - Video, WebKit, animations    │
└─────────────────────────────┘    └─────────────────────────────────┘
```

---

## Core Design Principles

### 1. Scene Graph Based Rendering

**Current**: Immediate mode - compute glyphs → draw immediately
**New**: Retained mode - build scene graph → GPU renders

```c
// New: Scene graph node types
enum display_node_type {
    NODE_TEXT_RUN,      // Contiguous text with same face
    NODE_IMAGE,         // Bitmap/vector image
    NODE_VIDEO,         // Video surface (via GStreamer)
    NODE_WIDGET,        // Embedded widget (WebKit, etc.)
    NODE_RECTANGLE,     // Solid color rectangle (backgrounds)
    NODE_CONTAINER,     // Group node with transform
};

struct display_node {
    enum display_node_type type;
    float x, y, width, height;      // In pixels (float for subpixel)
    float opacity;                   // For animations
    struct display_node *children;   // For containers
    struct display_node *next;       // Sibling list
    
    union {
        struct text_run_data text;
        struct image_data image;
        struct video_data video;
        struct widget_data widget;
        struct rect_data rect;
    } data;
};
```

### 2. Text Rendering Pipeline

```
Buffer Text + Properties
        │
        ▼
┌───────────────────┐
│  Layout Engine    │  (reuse xdisp.c logic, but output to scene graph)
│  - Bidi reorder   │
│  - Line breaking  │
│  - Face resolve   │
└───────┬───────────┘
        │
        ▼
┌───────────────────┐
│  Text Shaping     │  (HarfBuzz - already used)
│  - Ligatures      │
│  - Complex scripts│
└───────┬───────────┘
        │
        ▼
┌───────────────────┐
│  Glyph Cache      │  (texture atlas on GPU)
│  - Pre-rendered   │
│  - SDF optional   │  (Signed Distance Field for scaling)
└───────┬───────────┘
        │
        ▼
┌───────────────────┐
│  Scene Graph      │  (NODE_TEXT_RUN nodes)
│  - Position       │
│  - Texture refs   │
└───────────────────┘
```

### 3. Layer System for Windows

Each Emacs window becomes a composited layer:

```c
struct window_layer {
    int window_id;
    
    // Layers composited back-to-front
    struct display_node *background;    // Window background color
    struct display_node *text_content;  // Main text
    struct display_node *overlays;      // Overlay strings, images
    struct display_node *cursor;        // Cursor (can animate)
    struct display_node *widgets;       // Embedded widgets (xwidgets)
    
    // Scroll state (for smooth scrolling)
    float scroll_offset_y;
    float scroll_velocity_y;
    
    // Dirty tracking
    bool needs_relayout;
    bool needs_repaint;
    struct region dirty_region;
};
```

### 4. Incremental Updates

Only rebuild changed parts:

```c
struct damage_tracker {
    // Track what changed since last frame
    struct region text_changed;     // Buffer modifications
    struct region face_changed;     // Face/property changes  
    struct region scroll_changed;   // Scroll position
    bool cursor_moved;
    bool window_resized;
};

// Redisplay becomes:
void redisplay_window(struct window *w) {
    struct damage_tracker *damage = w->damage;
    
    if (damage->text_changed) {
        // Only re-layout affected lines
        relayout_region(w, damage->text_changed);
    }
    
    if (damage->scroll_changed) {
        // Just update transform, no re-layout
        w->layer->scroll_offset_y = w->scroll_position;
    }
    
    // Mark scene graph dirty, actual render happens in
    // next frame (compositor handles timing)
    mark_layer_dirty(w->layer);
}
```

---

## GTK4 Backend Implementation

### Using GSK (GTK Scene Kit)

```c
// GTK4 backend render function
static void
gtk4_render_frame(GtkWidget *widget, GtkSnapshot *snapshot) 
{
    struct frame *f = get_frame_for_widget(widget);
    
    for (struct window *w = f->windows; w; w = w->next) {
        // Push window transform (for scrolling)
        gtk_snapshot_save(snapshot);
        gtk_snapshot_translate(snapshot, 
            &GRAPHENE_POINT_INIT(w->x, w->y - w->scroll_offset));
        
        // Render each layer
        render_layer_to_snapshot(snapshot, w->layer->background);
        render_layer_to_snapshot(snapshot, w->layer->text_content);
        render_layer_to_snapshot(snapshot, w->layer->overlays);
        render_layer_to_snapshot(snapshot, w->layer->cursor);
        render_layer_to_snapshot(snapshot, w->layer->widgets);
        
        gtk_snapshot_restore(snapshot);
    }
}

// Text rendering using Pango + GSK
static void
render_text_run(GtkSnapshot *snapshot, struct text_run_data *run)
{
    PangoLayout *layout = pango_layout_new(pango_context);
    pango_layout_set_text(layout, run->text, run->length);
    pango_layout_set_font_description(layout, run->font);
    
    // GSK handles GPU upload of glyphs automatically
    gtk_snapshot_append_layout(snapshot, layout, &run->color);
    
    g_object_unref(layout);
}

// Image rendering
static void
render_image(GtkSnapshot *snapshot, struct image_data *img)
{
    GdkTexture *texture = get_or_create_texture(img);
    gtk_snapshot_append_texture(snapshot, texture, &img->bounds);
}

// Video rendering via GStreamer + GTK4
static void
render_video(GtkSnapshot *snapshot, struct video_data *video)
{
    GdkPaintable *paintable = video->gst_paintable;
    gdk_paintable_snapshot(paintable, snapshot, 
                           video->width, video->height);
}
```

### WebKit Integration

```c
// Embed WebKitGTK as a widget layer
struct widget_data {
    GtkWidget *webkit_view;
    GdkTexture *offscreen_texture;  // For compositing
    bool needs_input_forwarding;
};

static void
create_webkit_widget(struct window *w, const char *url)
{
    WebKitWebView *webview = WEBKIT_WEB_VIEW(webkit_web_view_new());
    webkit_web_view_load_uri(webview, url);
    
    // Embed in Emacs window's widget layer
    struct display_node *node = make_widget_node(GTK_WIDGET(webview));
    add_to_layer(w->layer->widgets, node);
}
```

---

## New Data Structures

### Replace glyph_matrix with scene graph

```c
// OLD: glyph_matrix (2D array of glyphs)
struct glyph_matrix {
    struct glyph_row *rows;
    int nrows;
};

// NEW: scene graph (tree of render nodes)
struct render_tree {
    struct display_node *root;
    
    // Fast lookup for incremental updates
    struct hash_table *line_to_node;  // buffer line → text node
    struct hash_table *overlay_nodes; // overlay → node
    
    // GPU resources
    struct glyph_atlas *atlas;        // Shared glyph texture
    struct texture_cache *images;     // Image texture cache
};
```

### Glyph Atlas for GPU Text

```c
struct glyph_atlas {
    GdkTexture *texture;          // GPU texture (e.g., 2048x2048)
    int width, height;
    
    // Glyph location lookup
    struct hash_table *glyph_cache;  // (font, charcode) → atlas_entry
    
    // Packing state
    int current_x, current_y;
    int row_height;
};

struct atlas_entry {
    int x, y;                // Position in atlas
    int width, height;       // Glyph dimensions
    int bearing_x, bearing_y;
    int advance;
};
```

---

## Smooth Scrolling & Animations

```c
// Animation system for smooth scrolling, cursor blink, etc.
struct animation {
    enum animation_type type;
    double start_time;
    double duration;
    double from_value;
    double to_value;
    double *target;           // Pointer to value being animated
    easing_func easing;       // ease-in-out, linear, etc.
};

// Called every frame by GTK4's frame clock
static void
on_frame_tick(GdkFrameClock *clock, gpointer user_data)
{
    struct frame *f = user_data;
    double now = gdk_frame_clock_get_frame_time(clock) / 1000000.0;
    
    // Update all active animations
    update_animations(f, now);
    
    // Request redraw if anything changed
    if (f->needs_redraw) {
        gtk_widget_queue_draw(f->gtk_widget);
    }
}

// Smooth scroll implementation
void scroll_window_smoothly(struct window *w, int lines)
{
    double target_y = w->scroll_offset_y + lines * w->line_height;
    
    animate(&w->scroll_offset_y, target_y, 
            SCROLL_DURATION, EASE_OUT_CUBIC);
}
```

---

## File Structure (New)

```
src/
├── display/
│   ├── scene.c          # Scene graph construction
│   ├── scene.h
│   ├── layout.c         # Text layout (refactored from xdisp.c)
│   ├── layout.h
│   ├── damage.c         # Dirty region tracking
│   ├── damage.h
│   ├── animation.c      # Animation system
│   ├── animation.h
│   ├── atlas.c          # Glyph atlas management
│   └── atlas.h
├── backend/
│   ├── tty.c            # TTY backend (refactored from term.c)
│   ├── tty.h
│   ├── gtk4.c           # GTK4 backend (replaces pgtkterm.c)
│   ├── gtk4.h
│   ├── gtk4-text.c      # Text rendering for GTK4
│   ├── gtk4-image.c     # Image rendering for GTK4
│   ├── gtk4-video.c     # Video playback (GStreamer)
│   └── gtk4-webkit.c    # WebKit integration
├── xdisp.c              # Keep: Lisp display logic, bidi, etc.
├── dispnew.c            # Heavily modified: frame update coordination
└── dispextern.h         # Updated: new data structures
```

---

## Migration Strategy

### Phase 1: Parallel Implementation
- Keep existing display engine working
- Build new scene graph system alongside
- Add `--with-new-display` configure flag

### Phase 2: GTK4 Backend
- Port PGTK to GTK4 with scene graph
- Verify GPU rendering works
- Benchmark against old system

### Phase 3: Feature Parity
- All faces, overlays, images working
- TTY backend uses scene graph (renders to cells)
- Smooth scrolling, animations

### Phase 4: Rich Content
- Video playback via GStreamer
- WebKit embedding
- Custom GPU shaders (optional)

### Phase 5: Cleanup
- Remove old backends (X11, W32, NS, etc.)
- Remove old glyph_matrix code
- Simplify build system

---

## API Compatibility

### Lisp APIs to Preserve
```elisp
;; These must keep working:
(set-face-attribute 'default nil :height 120)
(overlay-put ov 'face 'highlight)
(insert-image image-descriptor)
(put-text-property start end 'display '(image ...))

;; New APIs for modern features:
(insert-video "file.mp4" :width 640 :height 480)
(insert-webkit-view "https://..." :width 800 :height 600)
(set-smooth-scroll-mode t)
```

### Internal C API Changes
```c
// OLD: Immediate drawing
void draw_glyphs(struct glyph_string *s);

// NEW: Build scene graph
struct display_node *build_text_node(struct glyph_string *s);
void add_node_to_scene(struct render_tree *tree, struct display_node *node);

// Rendering is separate
void render_scene(struct render_tree *tree, Backend *backend);
```

---

## Performance Considerations

1. **Glyph caching**: Pre-render glyphs to GPU texture atlas
2. **Incremental updates**: Only rebuild changed parts of scene graph
3. **GPU compositing**: Let GPU handle layer blending
4. **Frame pacing**: Sync to display refresh rate (GTK4 handles this)
5. **Async text shaping**: Shape text in background thread

## Estimated Effort

| Phase | Effort | Description |
|-------|--------|-------------|
| 1 | 3-6 months | Scene graph + basic GTK4 rendering |
| 2 | 2-3 months | Full text rendering, faces, overlays |
| 3 | 2-3 months | Images, smooth scrolling, animations |
| 4 | 2-3 months | Video, WebKit integration |
| 5 | 1-2 months | Cleanup, optimization, testing |

**Total: ~12-18 months** for a single experienced developer

---

---

## Embedding Video and WPE WebKit in Buffers

### Current Glyph Types in Emacs

```c
enum glyph_type {
  CHAR_GLYPH,       // Regular character
  COMPOSITE_GLYPH,  // Composed characters
  GLYPHLESS_GLYPH,  // Characters without glyphs
  IMAGE_GLYPH,      // Images (PNG, JPEG, SVG, etc.)
  STRETCH_GLYPH,    // Whitespace/padding
  XWIDGET_GLYPH     // Embedded widgets (WebKit currently)
};
```

### Current Image Display Model

Images in Emacs are displayed via the `display` text property:

```elisp
;; Insert image at point
(insert-image (create-image "file.png"))

;; Or as display property
(put-text-property start end 'display 
  (create-image "file.png" nil nil :width 200))
```

Internally:
1. `create-image` → creates image descriptor (Lisp object)
2. Display engine sees `display` property → creates `IMAGE_GLYPH`
3. Backend renders the image at glyph position

### New Glyph Types for Video and WPE

```c
enum glyph_type {
  CHAR_GLYPH,
  COMPOSITE_GLYPH,
  GLYPHLESS_GLYPH,
  IMAGE_GLYPH,
  STRETCH_GLYPH,
  XWIDGET_GLYPH,
  VIDEO_GLYPH,      // NEW: Video playback
  WPE_GLYPH         // NEW: WPE WebKit (lightweight, offscreen)
};
```

---

### VIDEO_GLYPH: Video in Buffer

#### Lisp API (similar to images)

```elisp
;; Create video descriptor
(setq my-video (create-video "movie.mp4" 
                :width 640 
                :height 480
                :autoplay t
                :loop nil
                :muted nil))

;; Insert video at point (like insert-image)
(insert-video my-video)

;; Or as display property (inline in text)
(put-text-property start end 'display my-video)

;; Control playback
(video-play my-video)
(video-pause my-video)
(video-seek my-video 30.5)  ; seconds
(video-set-volume my-video 0.8)

;; Get state
(video-playing-p my-video)
(video-duration my-video)
(video-current-time my-video)
```

#### C Implementation

```c
// New structure for video
struct video {
  struct vectorlike_header header;
  
  // GStreamer pipeline
  GstElement *pipeline;
  GstElement *video_sink;
  
  // Current frame as texture
  GdkTexture *current_frame;
  
  // Properties
  int width, height;
  double duration;
  double current_time;
  bool playing;
  bool loop;
  double volume;
  
  // Buffer/window association
  Lisp_Object buffer;
  ptrdiff_t charpos;
};

// Display property handling in xdisp.c
static void
handle_display_prop (struct it *it, Lisp_Object prop)
{
  if (VIDEOP (prop))
    {
      // Create VIDEO_GLYPH similar to IMAGE_GLYPH
      struct video *v = XVIDEO (prop);
      produce_video_glyph (it, v);
    }
  // ... existing image handling
}

// Video glyph production
static void
produce_video_glyph (struct it *it, struct video *video)
{
  struct glyph *glyph = it->glyph_row->glyphs[TEXT_AREA] + it->glyph_row->used[TEXT_AREA];
  
  glyph->type = VIDEO_GLYPH;
  glyph->u.video.video_id = video->id;
  glyph->pixel_width = video->width;
  glyph->ascent = video->height;
  // ...
}
```

#### GTK4 Backend Rendering

```c
// In gtk4-video.c
static void
render_video_glyph (GtkSnapshot *snapshot, struct glyph_string *s)
{
  struct video *video = get_video_by_id (s->first_glyph->u.video.video_id);
  
  if (video->current_frame)
    {
      // Render current video frame as texture
      graphene_rect_t bounds = GRAPHENE_RECT_INIT (
        s->x, s->y, video->width, video->height);
      
      gtk_snapshot_append_texture (snapshot, video->current_frame, &bounds);
    }
}

// GStreamer integration for video decoding
static void
setup_gstreamer_pipeline (struct video *video, const char *uri)
{
  video->pipeline = gst_pipeline_new ("video-pipeline");
  
  GstElement *source = gst_element_factory_make ("uridecodebin", "source");
  GstElement *convert = gst_element_factory_make ("videoconvert", "convert");
  
  // Use gtksink for GTK4 integration
  video->video_sink = gst_element_factory_make ("gtk4paintablesink", "sink");
  
  g_object_set (source, "uri", uri, NULL);
  
  gst_bin_add_many (GST_BIN (video->pipeline), source, convert, video->video_sink, NULL);
  gst_element_link_many (convert, video->video_sink, NULL);
  
  // Connect to frame updates
  GdkPaintable *paintable;
  g_object_get (video->video_sink, "paintable", &paintable, NULL);
  g_signal_connect (paintable, "invalidate-contents", 
                    G_CALLBACK (on_video_frame_ready), video);
}

// Called when new video frame is ready
static void
on_video_frame_ready (GdkPaintable *paintable, struct video *video)
{
  // Get frame as texture
  video->current_frame = gdk_paintable_get_current_image (paintable);
  
  // Mark window for redisplay
  if (video->window)
    redisplay_window (video->window);
}
```

---

### WPE_GLYPH: WPE WebKit in Buffer

**Why WPE instead of WebKitGTK?**
- WPE = WebKit Platform for Embedded
- Designed for **offscreen rendering** (perfect for embedding)
- Lighter weight than full WebKitGTK
- Renders to buffer/texture, not to a window
- Better for compositing

#### Lisp API

```elisp
;; Create WPE WebKit view
(setq my-web (create-wpe-view "https://example.com"
               :width 800
               :height 600
               :javascript t
               :user-agent "Emacs-WPE/1.0"))

;; Insert in buffer (like image)
(insert-wpe-view my-web)

;; Or as display property
(put-text-property start end 'display my-web)

;; Navigation
(wpe-load-uri my-web "https://other-site.com")
(wpe-go-back my-web)
(wpe-go-forward my-web)
(wpe-reload my-web)

;; JavaScript interaction
(wpe-execute-js my-web "document.title" 
  (lambda (result) (message "Title: %s" result)))

;; Input forwarding (when point is on wpe-view)
(wpe-send-key my-web ?a)
(wpe-send-mouse-click my-web x y button)
(wpe-scroll my-web dx dy)

;; Get state
(wpe-uri my-web)
(wpe-title my-web)
(wpe-loading-p my-web)
```

#### C Implementation

```c
// WPE WebKit structure
struct wpe_view {
  struct vectorlike_header header;
  
  // WPE WebKit backend
  WebKitWebContext *context;
  WebKitWebView *webview;
  struct wpe_view_backend *backend;
  
  // Offscreen rendering target
  GdkTexture *texture;
  cairo_surface_t *surface;  // Fallback for software rendering
  
  // Properties
  int width, height;
  char *uri;
  char *title;
  bool loading;
  
  // Buffer association
  Lisp_Object buffer;
  ptrdiff_t charpos;
};

// WPE backend for offscreen rendering
static struct wpe_view_backend_exportable_fdo_client wpe_client = {
  .export_buffer_resource = on_wpe_buffer_export,
  .export_dmabuf_resource = on_wpe_dmabuf_export,
};

static void
create_wpe_backend (struct wpe_view *view)
{
  // Create WPE exportable backend (renders to DMA-BUF or SHM)
  view->backend = wpe_view_backend_exportable_fdo_create (
    &wpe_client, view, view->width, view->height);
  
  // Create WebKit view with this backend
  WebKitWebViewBackend *wk_backend = 
    webkit_web_view_backend_new (
      wpe_view_backend_exportable_fdo_get_view_backend (view->backend),
      NULL, NULL);
  
  view->webview = webkit_web_view_new (wk_backend);
}

// Called when WPE renders a new frame
static void
on_wpe_buffer_export (void *data, struct wl_resource *buffer)
{
  struct wpe_view *view = data;
  
  // Convert buffer to GdkTexture for GTK4 rendering
  // (DMA-BUF → GdkDmabufTexture, or SHM → GdkMemoryTexture)
  view->texture = create_texture_from_buffer (buffer);
  
  // Trigger redisplay
  redisplay_window_for_wpe_view (view);
  
  // Tell WPE we're done with the buffer
  wpe_view_backend_exportable_fdo_dispatch_frame_complete (view->backend);
}
```

#### GTK4 Backend Rendering

```c
// In gtk4-wpe.c
static void
render_wpe_glyph (GtkSnapshot *snapshot, struct glyph_string *s)
{
  struct wpe_view *view = get_wpe_view_by_id (s->first_glyph->u.wpe.view_id);
  
  if (view->texture)
    {
      graphene_rect_t bounds = GRAPHENE_RECT_INIT (
        s->x, s->y, view->width, view->height);
      
      gtk_snapshot_append_texture (snapshot, view->texture, &bounds);
    }
}
```

---

### Input Handling for Video and WPE

When cursor is on VIDEO_GLYPH or WPE_GLYPH:

```c
// In keyboard.c or new input.c
static void
handle_input_for_embedded_content (struct input_event *event)
{
  struct glyph *glyph = glyph_at_point ();
  
  if (glyph->type == VIDEO_GLYPH)
    {
      // Video controls (space = play/pause, arrow keys = seek)
      struct video *video = get_video (glyph);
      switch (event->code)
        {
        case ' ': video_toggle_play (video); break;
        case KEY_LEFT: video_seek_relative (video, -5.0); break;
        case KEY_RIGHT: video_seek_relative (video, 5.0); break;
        }
    }
  else if (glyph->type == WPE_GLYPH)
    {
      // Forward all input to WPE WebKit
      struct wpe_view *view = get_wpe_view (glyph);
      wpe_forward_keyboard_event (view, event);
    }
}

// Mouse input forwarding
static void
handle_mouse_for_embedded_content (int x, int y, int button, int state)
{
  struct glyph *glyph = glyph_at_position (x, y);
  
  if (glyph->type == WPE_GLYPH)
    {
      struct wpe_view *view = get_wpe_view (glyph);
      
      // Convert to local coordinates within the web view
      int local_x = x - glyph->x;
      int local_y = y - glyph->y;
      
      wpe_forward_mouse_event (view, local_x, local_y, button, state);
    }
}
```

---

### Comparison: Image vs Video vs WPE

| Feature | IMAGE_GLYPH | VIDEO_GLYPH | WPE_GLYPH |
|---------|-------------|-------------|-----------|
| Static | ✓ | ✗ | ✗ |
| Animated | GIF only | Full video | Web animations |
| Audio | ✗ | ✓ | ✓ |
| Interactive | ✗ | Play/pause/seek | Full web interaction |
| Renderer | Cairo/Pixbuf | GStreamer | WPE WebKit |
| GPU accel | Optional | ✓ (video decode) | ✓ (WebKit) |
| Use case | Screenshots, diagrams | Media playback | Web content, apps |

---

### Example Usage

```elisp
;; Rich document with mixed content
(defun insert-rich-content ()
  (interactive)
  (insert "# My Document\n\n")
  (insert "Here's an image: ")
  (insert-image (create-image "diagram.png"))
  (insert "\n\nWatch this video:\n")
  (insert-video (create-video "demo.mp4" :width 640 :height 360))
  (insert "\n\nLive web content:\n")
  (insert-wpe-view (create-wpe-view "https://docs.example.com" 
                     :width 800 :height 600))
  (insert "\n\nBack to text...\n"))

;; Org-mode integration
(org-link-set-parameters "video"
  :follow #'org-video-open
  :export #'org-video-export)

;; EWW replacement using WPE
(defun eww-wpe (url)
  "Browse URL using embedded WPE WebKit."
  (interactive "sURL: ")
  (switch-to-buffer (generate-new-buffer "*eww-wpe*"))
  (insert-wpe-view (create-wpe-view url :width (window-pixel-width)
                                        :height (window-pixel-height))))
```

---

## References

- GTK4 Documentation: https://docs.gtk.org/gtk4/
- GSK Render Nodes: https://docs.gtk.org/gsk4/
- Pango Text Rendering: https://docs.gtk.org/Pango/
- WebKitGTK: https://webkitgtk.org/
- WPE WebKit: https://wpewebkit.org/
- GStreamer: https://gstreamer.freedesktop.org/
- GStreamer GTK4 sink: https://gitlab.freedesktop.org/gstreamer/gst-plugins-good/-/tree/main/ext/gtk

---

## Animated Image (GIF) Performance Improvements

### Current Emacs GIF Animation - Problems

```
┌─────────────────────────────────────────────────────────────────┐
│                    Current Architecture                         │
│                                                                 │
│  ┌─────────┐    run-with-timer    ┌──────────────────┐         │
│  │ Frame 1 │ ──────────────────▶  │ image-animate-   │         │
│  └─────────┘      (Lisp timer)    │ timeout          │         │
│       │                           │ (Elisp function) │         │
│       │                           └────────┬─────────┘         │
│       │                                    │                    │
│       ▼                                    ▼                    │
│  ┌─────────┐                      ┌──────────────────┐         │
│  │ Decode  │ ◀──────────────────  │ image-show-frame │         │
│  │ Frame N │    (full redecode)   │ (change :index)  │         │
│  └─────────┘                      └──────────────────┘         │
│       │                                    │                    │
│       ▼                                    │                    │
│  ┌─────────┐                               │                    │
│  │ Redraw  │ ◀─────────────────────────────┘                   │
│  │ Window  │    (full redisplay)                               │
│  └─────────┘                                                    │
└─────────────────────────────────────────────────────────────────┘
```

### Current Problems

| Issue | Description |
|-------|-------------|
| **Lisp Timer Overhead** | `run-with-timer` has ~1-10ms overhead per callback |
| **Full Redecode** | Each frame switch re-decodes the GIF from disk/memory |
| **Full Redisplay** | Changes `:index` → marks display dirty → full window redraw |
| **No GPU** | All decoding/compositing on CPU |
| **Blocks Main Thread** | Animation runs in Emacs event loop |
| **Tardiness Tracking** | Has to detect "too slow" and stop animation |

### New Architecture (Rust + GTK4)

```
┌─────────────────────────────────────────────────────────────────┐
│                    New Architecture (Rust + GTK4)               │
│                                                                 │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                  Rust Animation Manager                  │   │
│  │                                                          │   │
│  │  ┌──────────┐   Pre-decoded    ┌─────────────────────┐  │   │
│  │  │ GIF File │ ───────────────▶ │ Frame Cache (GPU)   │  │   │
│  │  └──────────┘   at load time   │ [Tex0][Tex1][Tex2]..│  │   │
│  │                                └──────────┬──────────┘  │   │
│  │                                           │              │   │
│  │  ┌──────────────────┐                     │              │   │
│  │  │ GTK4 Frame Clock │ ◀───────────────────┘              │   │
│  │  │ (60fps/vsync)    │    (just swap texture index)       │   │
│  │  └────────┬─────────┘                                    │   │
│  │           │                                              │   │
│  │           ▼                                              │   │
│  │  ┌──────────────────┐                                    │   │
│  │  │ GSK Compositor   │ ◀── GPU compositing                │   │
│  │  │ (GPU accelerated)│                                    │   │
│  │  └──────────────────┘                                    │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

### Improvements

| Aspect | Current (Elisp Timer) | New (Rust + GTK4) |
|--------|----------------------|-------------------|
| **Frame Switch** | Re-decode + Lisp call | Just change texture index |
| **Timing** | Lisp timer (~1-10ms jitter) | GTK4 frame clock (vsync accurate) |
| **Decoding** | Per-frame, on CPU | Once at load, can use GPU |
| **Memory** | Re-allocate each frame | Pre-allocated texture atlas |
| **Compositing** | CPU (Cairo) | GPU (GSK/Vulkan/Metal) |
| **Thread** | Main Emacs thread | Separate render thread |
| **Multiple GIFs** | Each needs timer | Single animation loop |

### Rust Implementation

```rust
// src/core/animated_image.rs

use std::time::{Duration, Instant};
use gtk4::gdk::Texture;

pub struct AnimatedImage {
    /// Pre-decoded frames as GPU textures
    frames: Vec<Texture>,
    
    /// Delay for each frame (can vary per frame)
    delays: Vec<Duration>,
    
    /// Current frame index
    current_frame: usize,
    
    /// Time of last frame change
    last_frame_time: Instant,
    
    /// Animation state
    playing: bool,
    loop_count: LoopMode,
}

pub enum LoopMode {
    Forever,
    Count(u32),
    Once,
}

impl AnimatedImage {
    /// Load and pre-decode all frames to GPU textures
    pub fn from_gif(data: &[u8]) -> Result<Self, ImageError> {
        let decoder = gif::DecodeOptions::new().read_info(data)?;
        
        let mut frames = Vec::new();
        let mut delays = Vec::new();
        
        // Decode ALL frames upfront to GPU textures
        for frame in decoder {
            let frame = frame?;
            
            // Convert frame to RGBA
            let rgba = frame_to_rgba(&frame);
            
            // Upload to GPU texture immediately
            let texture = Texture::for_pixbuf(&rgba_to_pixbuf(&rgba));
            frames.push(texture);
            
            // Store frame delay
            delays.push(Duration::from_millis(frame.delay as u64 * 10));
        }
        
        Ok(Self {
            frames,
            delays,
            current_frame: 0,
            last_frame_time: Instant::now(),
            playing: true,
            loop_count: LoopMode::Forever,
        })
    }
    
    /// Called every frame by GTK4 frame clock - O(1) operation!
    pub fn tick(&mut self) -> bool {
        if !self.playing || self.frames.is_empty() {
            return false;
        }
        
        let now = Instant::now();
        let elapsed = now - self.last_frame_time;
        
        if elapsed >= self.delays[self.current_frame] {
            self.current_frame = (self.current_frame + 1) % self.frames.len();
            self.last_frame_time = now;
            return true; // Need redraw
        }
        
        false // No change
    }
    
    /// Get current frame texture - just a pointer, no decode!
    pub fn current_texture(&self) -> &Texture {
        &self.frames[self.current_frame]
    }
}

// Animation manager handles all animated images
pub struct AnimationManager {
    images: HashMap<u32, AnimatedImage>,
}

impl AnimationManager {
    /// Called by GTK4 frame clock (~60fps)
    pub fn tick_all(&mut self) -> Vec<u32> {
        let mut needs_redraw = Vec::new();
        
        for (id, image) in &mut self.images {
            if image.tick() {
                needs_redraw.push(*id);
            }
        }
        
        needs_redraw
    }
}
```

### GTK4 Integration

```rust
// In GTK4 backend - single animation loop for ALL images
fn setup_animation_loop(animation_mgr: Rc<RefCell<AnimationManager>>) {
    let frame_clock = widget.frame_clock().unwrap();
    
    frame_clock.connect_update(move |clock| {
        let needs_redraw = animation_mgr.borrow_mut().tick_all();
        
        if !needs_redraw.is_empty() {
            // Only redraw regions with animated images
            for image_id in needs_redraw {
                queue_redraw_for_image(image_id);
            }
        }
    });
    
    frame_clock.begin_updating();
}
```

### Performance Comparison

| Metric | Current Emacs | New Rust Engine |
|--------|---------------|-----------------|
| **10 GIFs on screen** | 10 timers, 10x decode/frame | 1 loop, 0 decodes |
| **Frame latency** | 5-20ms | <1ms (vsync) |
| **CPU usage** | High (decode + redraw) | Low (just texture swap) |
| **GPU usage** | None | Efficient compositing |
| **Memory** | Re-allocate per frame | Fixed (texture atlas) |
| **Smoothness** | Choppy under load | Always smooth |

### Additional Animated Format Support

With the new engine, we can easily support more animated formats:

| Format | Current Emacs | New Engine |
|--------|---------------|------------|
| **GIF** | ✅ (slow) | ✅ Fast, GPU |
| **APNG** | ❌ | ✅ |
| **WebP animated** | ❌ | ✅ |
| **AVIF animated** | ❌ | ✅ |
| **Lottie (JSON)** | ❌ | ✅ (vector animations) |

All animated formats use the same `AnimatedImage` infrastructure with format-specific decoders.

---

## GPU Text Rendering

### Yes! Pure Text Uses GPU Too

In the modern display engine, **everything** goes through the GPU, including plain text. Here's how:

### How GPU Text Rendering Works

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        GPU Text Rendering Pipeline                       │
│                                                                          │
│  1. FONT LOADING                                                         │
│  ┌──────────────┐                                                        │
│  │ Font File    │  FreeType/FontConfig                                   │
│  │ (.ttf/.otf)  │ ─────────────────────┐                                │
│  └──────────────┘                      │                                 │
│                                        ▼                                 │
│  2. TEXT SHAPING                  ┌──────────────┐                       │
│  ┌──────────────┐                 │   HarfBuzz   │                       │
│  │ "Hello 世界" │ ──────────────▶ │ Text Shaper  │                       │
│  └──────────────┘                 │ (ligatures,  │                       │
│                                   │  kerning,    │                       │
│                                   │  bidi, etc.) │                       │
│                                   └──────┬───────┘                       │
│                                          │                               │
│                                          ▼                               │
│  3. GLYPH RASTERIZATION          ┌──────────────────┐                   │
│                                  │ Glyph IDs:       │                   │
│                                  │ [H][e][l][l][o]  │                   │
│                                  │ [世][界]         │                   │
│                                  └──────┬───────────┘                   │
│                                         │                                │
│                     ┌───────────────────┴───────────────────┐           │
│                     ▼                                       ▼           │
│            ┌─────────────────┐                    ┌─────────────────┐   │
│            │ Cache Hit?      │───── YES ────────▶│ Use Existing    │   │
│            │ (glyph in atlas)│                    │ Texture Coords  │   │
│            └────────┬────────┘                    └────────┬────────┘   │
│                     │ NO                                   │            │
│                     ▼                                      │            │
│            ┌─────────────────┐                             │            │
│            │ Rasterize Glyph │                             │            │
│            │ (FreeType → RGB)│                             │            │
│            └────────┬────────┘                             │            │
│                     │                                      │            │
│                     ▼                                      │            │
│            ┌─────────────────┐                             │            │
│            │ Upload to Atlas │                             │            │
│            │ (GPU Texture)   │                             │            │
│            └────────┬────────┘                             │            │
│                     │                                      │            │
│                     └──────────────────┬───────────────────┘            │
│                                        │                                │
│  4. GPU RENDERING                      ▼                                │
│                              ┌───────────────────┐                      │
│                              │   Glyph Atlas     │                      │
│                              │   (GPU Texture)   │                      │
│                              │ ┌───┬───┬───┬───┐ │                      │
│                              │ │ A │ B │ C │ D │ │                      │
│                              │ ├───┼───┼───┼───┤ │                      │
│                              │ │ E │ F │ G │ H │ │  2048x2048 texture   │
│                              │ ├───┼───┼───┼───┤ │                      │
│                              │ │世 │界 │你 │好 │ │                      │
│                              │ └───┴───┴───┴───┘ │                      │
│                              └─────────┬─────────┘                      │
│                                        │                                │
│                                        ▼                                │
│                              ┌───────────────────┐                      │
│                              │   GPU Shader      │                      │
│                              │                   │                      │
│                              │ For each glyph:   │                      │
│                              │ - Position (x,y)  │                      │
│                              │ - Texture coords  │                      │
│                              │ - Color           │                      │
│                              │ → Draw textured   │                      │
│                              │   quad            │                      │
│                              └─────────┬─────────┘                      │
│                                        │                                │
│                                        ▼                                │
│                              ┌───────────────────┐                      │
│                              │   Screen Output   │                      │
│                              │   (60+ FPS)       │                      │
│                              └───────────────────┘                      │
└─────────────────────────────────────────────────────────────────────────┘
```

### Glyph Atlas Explained

Instead of drawing each character individually, we:

1. **Pre-rasterize** glyphs to a large texture (atlas)
2. **Store coordinates** of each glyph in the atlas
3. **Draw quads** with texture coordinates pointing to the atlas

```
┌─────────────────────────────────────────────────────────────────┐
│                      Glyph Atlas (2048x2048)                    │
│  ┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┐ │
│  │ !  │ "  │ #  │ $  │ %  │ &  │ '  │ (  │ )  │ *  │ +  │ ,  │ │
│  ├────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┤ │
│  │ A  │ B  │ C  │ D  │ E  │ F  │ G  │ H  │ I  │ J  │ K  │ L  │ │
│  ├────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┤ │
│  │ a  │ b  │ c  │ d  │ e  │ f  │ g  │ h  │ i  │ j  │ k  │ l  │ │
│  ├────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┤ │
│  │ 你 │ 好 │ 世 │ 界 │ 日 │ 本 │ 語 │ 한 │ 글 │ 🎉 │ 🚀 │ .. │ │
│  └────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┘ │
│                                                                 │
│  Cache lookup: O(1) hash table                                  │
│  Miss: rasterize + upload (~1ms per glyph, amortized)          │
└─────────────────────────────────────────────────────────────────┘
```

### GTK4/GSK Text Rendering

GTK4 handles this automatically via Pango + GSK:

```rust
// In our Rust backend - text rendering is simple!
fn render_text_run(
    snapshot: &gtk4::Snapshot,
    text: &str,
    x: f32, 
    y: f32,
    face: &Face,
) {
    // Create Pango layout
    let layout = pango::Layout::new(&pango_context);
    layout.set_text(text);
    layout.set_font_description(Some(&face.font_desc));
    
    // Set position
    snapshot.save();
    snapshot.translate(&graphene::Point::new(x, y));
    
    // GSK handles: glyph atlas, GPU upload, shader dispatch
    // This single call does ALL the GPU magic!
    snapshot.append_layout(&layout, &face.foreground.to_gdk());
    
    snapshot.restore();
}
```

Under the hood, `snapshot.append_layout()`:
1. Shapes text with HarfBuzz
2. Looks up/rasterizes glyphs to atlas
3. Creates GPU draw calls
4. Batches with other text for efficiency

### CPU vs GPU Text Rendering

| Aspect | CPU (Current Emacs) | GPU (New Engine) |
|--------|---------------------|------------------|
| **Rasterization** | Every redraw | Once, cached in atlas |
| **Compositing** | Cairo software blend | GPU parallel blend |
| **Memory bandwidth** | High (copy pixels) | Low (texture coords) |
| **Batch efficiency** | Draw char by char | Draw thousands in 1 call |
| **Resolution scaling** | Re-rasterize | Instant (just scale) |
| **Subpixel positioning** | Expensive | Free (GPU interpolation) |

### Performance Numbers

Typical text rendering performance:

| Scenario | CPU (Cairo) | GPU (GSK) |
|----------|-------------|-----------|
| **1000 chars, first draw** | ~15ms | ~5ms |
| **1000 chars, cached** | ~10ms | ~0.5ms |
| **Scroll 1000 lines** | ~50ms | ~2ms |
| **4K display full redraw** | ~100ms | ~8ms |

### Subpixel Text Rendering

GPU enables high-quality subpixel rendering:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Subpixel Text Rendering                      │
│                                                                 │
│  Standard rendering (1 pixel = 1 sample):                       │
│  ┌─┬─┬─┬─┬─┐                                                   │
│  │█│█│ │█│█│  "A" at low resolution - jagged edges             │
│  └─┴─┴─┴─┴─┘                                                   │
│                                                                 │
│  Subpixel rendering (1 pixel = 3 samples R/G/B):               │
│  ┌───────────────┐                                             │
│  │▓▓▓███░░░███▓▓▓│  "A" with 3x horizontal resolution          │
│  └───────────────┘  (uses LCD subpixels for smoothing)         │
│                                                                 │
│  GPU makes this trivial - fragment shader samples subpixels    │
└─────────────────────────────────────────────────────────────────┘
```

### What Gets GPU Accelerated?

| Element | GPU Accelerated? | How |
|---------|------------------|-----|
| **Text** | ✅ | Glyph atlas + textured quads |
| **Cursor** | ✅ | Colored rectangle |
| **Selection** | ✅ | Colored rectangle behind text |
| **Images** | ✅ | Texture upload + quad |
| **Animated GIF** | ✅ | Texture array, swap index |
| **Video** | ✅ | Hardware decode → texture |
| **WebKit** | ✅ | Offscreen render → texture |
| **Scroll** | ✅ | Just change transform matrix |
| **Window borders** | ✅ | Rectangles/lines |
| **Fringes** | ✅ | Bitmap textures |
| **Mode line** | ✅ | Text (same as buffer) |

### The Key Insight

**GPU rendering isn't just for graphics-heavy apps.**

Even for a "text editor", GPU provides:
- **Massive parallelism**: Thousands of glyphs rendered simultaneously
- **Hardware acceleration**: Dedicated silicon for blending/compositing
- **Memory efficiency**: Textures stay on GPU, no CPU↔GPU copies
- **Vsync**: Tear-free, smooth display at monitor refresh rate
- **High DPI**: Resolution-independent, crisp at any scale

### Code Example: Full Frame Render

```rust
impl Gtk4Backend {
    fn render_frame(&self, snapshot: &gtk4::Snapshot, frame: &Frame) {
        // 1. Clear background (GPU: 1 draw call)
        snapshot.append_color(
            &frame.background_color,
            &graphene::Rect::new(0.0, 0.0, frame.width, frame.height),
        );
        
        // 2. Render each window
        for window in &frame.windows {
            snapshot.save();
            snapshot.translate(&graphene::Point::new(
                window.x, 
                window.y - window.scroll_offset  // Smooth scroll!
            ));
            
            // 3. Window background (GPU: 1 draw call)
            snapshot.append_color(&window.background, &window.bounds);
            
            // 4. Render all text lines (GPU: batched into few draw calls)
            for line in &window.lines {
                self.render_text_line(snapshot, line);
            }
            
            // 5. Render cursor (GPU: 1 draw call)
            if let Some(cursor) = &window.cursor {
                snapshot.append_color(&cursor.color, &cursor.rect);
            }
            
            // 6. Render images/videos/widgets
            for embed in &window.embedded_content {
                self.render_embedded(snapshot, embed);
            }
            
            snapshot.restore();
        }
        
        // Total: ~10-20 GPU draw calls for entire frame
        // vs CPU: thousands of individual pixel operations
    }
}
```

### Summary

**Q: Does pure text use GPU?**  
**A: Yes! Every pixel on screen is rendered by the GPU.**

The GPU isn't just for "fancy graphics" - it's fundamentally more efficient for ALL 2D rendering, including text. Modern terminals (Alacritty, Kitty, WezTerm) and editors (VS Code, Zed) all use GPU text rendering for this reason.

---

## GTK4 Architecture and Usage

### GTK4 vs GTK3 - Key Differences

| Aspect | GTK3 (Current Emacs) | GTK4 (New Engine) |
|--------|----------------------|-------------------|
| **Rendering** | Cairo (CPU) | GSK (GPU) |
| **Drawing API** | `cairo_t` immediate mode | `GtkSnapshot` scene graph |
| **Event handling** | Signal-based | Event controllers |
| **Input** | `GdkEventKey`, etc. | `GtkEventController*` |
| **Windows** | `GdkWindow` | `GtkNative`, surfaces |
| **Widgets** | Many deprecated | Modernized, fewer |

### GTK4 Rendering Model

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         GTK4 Rendering Pipeline                         │
│                                                                         │
│  ┌─────────────────┐                                                    │
│  │ Application     │                                                    │
│  │ (Our Rust Code) │                                                    │
│  └────────┬────────┘                                                    │
│           │                                                             │
│           │  build snapshot                                             │
│           ▼                                                             │
│  ┌─────────────────┐                                                    │
│  │  GtkSnapshot    │  ← We build render nodes here                      │
│  │  (Scene Builder)│                                                    │
│  └────────┬────────┘                                                    │
│           │                                                             │
│           │  creates                                                    │
│           ▼                                                             │
│  ┌─────────────────┐                                                    │
│  │  GskRenderNode  │  ← Immutable render tree                          │
│  │  (Render Tree)  │     - ColorNode                                   │
│  │                 │     - TextNode                                    │
│  │                 │     - TextureNode                                 │
│  │                 │     - ContainerNode                               │
│  │                 │     - TransformNode                               │
│  │                 │     - ClipNode                                    │
│  └────────┬────────┘                                                    │
│           │                                                             │
│           │  rendered by                                                │
│           ▼                                                             │
│  ┌─────────────────┐                                                    │
│  │  GskRenderer    │  ← Automatic backend selection                    │
│  │  (GPU Backend)  │                                                    │
│  │                 │     ┌──────────────────────────────┐              │
│  │                 │     │ • Vulkan (Linux, Windows)    │              │
│  │                 │     │ • Metal (macOS - planned)    │              │
│  │                 │     │ • OpenGL (fallback)          │              │
│  │                 │     │ • Cairo (software fallback)  │              │
│  │                 │     └──────────────────────────────┘              │
│  └────────┬────────┘                                                    │
│           │                                                             │
│           │  presents to                                                │
│           ▼                                                             │
│  ┌─────────────────┐                                                    │
│  │  Display/Screen │                                                    │
│  └─────────────────┘                                                    │
└─────────────────────────────────────────────────────────────────────────┘
```

### Core GTK4 Components We Use

#### 1. GtkApplication - Entry Point

```rust
use gtk4::prelude::*;
use gtk4::{Application, ApplicationWindow};

pub fn create_emacs_app() -> Application {
    let app = Application::builder()
        .application_id("org.gnu.emacs")
        .build();
    
    app.connect_activate(|app| {
        // Create main window when app activates
        let window = create_main_window(app);
        window.present();
    });
    
    app
}
```

#### 2. GtkApplicationWindow - Main Frame

```rust
fn create_main_window(app: &Application) -> ApplicationWindow {
    let window = ApplicationWindow::builder()
        .application(app)
        .title("Emacs")
        .default_width(1024)
        .default_height(768)
        .build();
    
    // Add our custom drawing area
    let drawing_area = create_emacs_drawing_area();
    window.set_child(Some(&drawing_area));
    
    // Set up input handling
    setup_input_controllers(&window);
    
    window
}
```

#### 3. GtkDrawingArea - Our Canvas

```rust
use gtk4::{DrawingArea, glib};
use std::cell::RefCell;
use std::rc::Rc;

fn create_emacs_drawing_area() -> DrawingArea {
    let area = DrawingArea::new();
    
    // Store our display state
    let state = Rc::new(RefCell::new(DisplayState::new()));
    
    // Set the draw function - called every frame
    let state_clone = state.clone();
    area.set_draw_func(move |area, snapshot, width, height| {
        let state = state_clone.borrow();
        render_emacs_frame(snapshot, &state, width, height);
    });
    
    // Request redraws when content changes
    // area.queue_draw();
    
    area
}
```

#### 4. GtkSnapshot - Building the Scene

```rust
fn render_emacs_frame(
    snapshot: &gtk4::Snapshot,
    state: &DisplayState,
    width: i32,
    height: i32,
) {
    // 1. Background
    snapshot.append_color(
        &gdk4::RGBA::new(0.1, 0.1, 0.1, 1.0),  // Dark background
        &graphene::Rect::new(0.0, 0.0, width as f32, height as f32),
    );
    
    // 2. Render each Emacs window
    for window in &state.windows {
        render_window(snapshot, window);
    }
    
    // 3. Mode line
    render_mode_line(snapshot, state);
    
    // 4. Minibuffer
    render_minibuffer(snapshot, state);
}

fn render_window(snapshot: &gtk4::Snapshot, window: &EmacsWindow) {
    // Save transform state
    snapshot.save();
    
    // Position window (with smooth scroll offset)
    snapshot.translate(&graphene::Point::new(
        window.x as f32,
        window.y as f32 - window.smooth_scroll_offset,
    ));
    
    // Clip to window bounds
    snapshot.push_clip(&graphene::Rect::new(
        0.0, 0.0,
        window.width as f32,
        window.height as f32,
    ));
    
    // Window background
    snapshot.append_color(&window.background_color, &window.bounds);
    
    // Render text content
    for line in &window.lines {
        render_text_line(snapshot, line);
    }
    
    // Render cursor
    if window.has_focus {
        render_cursor(snapshot, &window.cursor);
    }
    
    // Pop clip and restore transform
    snapshot.pop();  // clip
    snapshot.restore();  // transform
}
```

#### 5. Text Rendering with Pango

```rust
use pango::prelude::*;
use pangocairo::functions as pango_cairo;

fn render_text_line(snapshot: &gtk4::Snapshot, line: &TextLine) {
    let pango_context = snapshot.pango_context();
    
    for run in &line.runs {
        // Create layout for this text run
        let layout = pango::Layout::new(&pango_context);
        
        // Set font
        let font_desc = pango::FontDescription::from_string(&run.font_spec);
        layout.set_font_description(Some(&font_desc));
        
        // Set text
        layout.set_text(&run.text);
        
        // Position
        snapshot.save();
        snapshot.translate(&graphene::Point::new(run.x, run.y));
        
        // Draw with foreground color
        snapshot.append_layout(&layout, &run.fg_color);
        
        snapshot.restore();
    }
}

fn render_cursor(snapshot: &gtk4::Snapshot, cursor: &Cursor) {
    match cursor.style {
        CursorStyle::Box => {
            // Box cursor - filled rectangle
            snapshot.append_color(
                &cursor.color,
                &graphene::Rect::new(
                    cursor.x, cursor.y,
                    cursor.width, cursor.height,
                ),
            );
        }
        CursorStyle::Bar => {
            // Bar cursor - thin line
            snapshot.append_color(
                &cursor.color,
                &graphene::Rect::new(
                    cursor.x, cursor.y,
                    2.0, cursor.height,  // 2 pixel wide bar
                ),
            );
        }
        CursorStyle::Underline => {
            // Underline cursor
            snapshot.append_color(
                &cursor.color,
                &graphene::Rect::new(
                    cursor.x, cursor.y + cursor.height - 2.0,
                    cursor.width, 2.0,
                ),
            );
        }
    }
}
```

#### 6. Input Handling with Event Controllers

```rust
use gtk4::{EventControllerKey, EventControllerScroll, GestureClick};

fn setup_input_controllers(window: &ApplicationWindow) {
    // Keyboard input
    let key_controller = EventControllerKey::new();
    key_controller.connect_key_pressed(|controller, keyval, keycode, modifier| {
        handle_key_press(keyval, keycode, modifier)
    });
    key_controller.connect_key_released(|controller, keyval, keycode, modifier| {
        handle_key_release(keyval, keycode, modifier);
    });
    window.add_controller(key_controller);
    
    // Mouse clicks
    let click_gesture = GestureClick::new();
    click_gesture.connect_pressed(|gesture, n_press, x, y| {
        handle_mouse_press(n_press, x, y);
    });
    click_gesture.connect_released(|gesture, n_press, x, y| {
        handle_mouse_release(n_press, x, y);
    });
    window.add_controller(click_gesture);
    
    // Scroll (mouse wheel, touchpad)
    let scroll_controller = EventControllerScroll::new(
        gtk4::EventControllerScrollFlags::BOTH_AXES
    );
    scroll_controller.connect_scroll(|controller, dx, dy| {
        handle_scroll(dx, dy)
    });
    window.add_controller(scroll_controller);
    
    // Mouse motion
    let motion_controller = gtk4::EventControllerMotion::new();
    motion_controller.connect_motion(|controller, x, y| {
        handle_mouse_motion(x, y);
    });
    window.add_controller(motion_controller);
}

fn handle_key_press(
    keyval: gdk4::Key,
    keycode: u32,
    modifier: gdk4::ModifierType,
) -> glib::Propagation {
    // Convert to Emacs key event
    let emacs_key = convert_to_emacs_key(keyval, modifier);
    
    // Send to Emacs event queue
    send_key_event_to_emacs(emacs_key);
    
    glib::Propagation::Stop  // We handled it
}
```

#### 7. Frame Clock for Animations

```rust
fn setup_animation_loop(drawing_area: &DrawingArea, state: Rc<RefCell<DisplayState>>) {
    // Get the frame clock (synced to monitor refresh)
    drawing_area.connect_realize(move |area| {
        if let Some(frame_clock) = area.frame_clock() {
            // Called before each frame
            frame_clock.connect_update(clone!(@weak area, @strong state => move |clock| {
                let mut state = state.borrow_mut();
                
                // Update animations
                let now = clock.frame_time() as f64 / 1_000_000.0;  // microseconds to seconds
                state.update_animations(now);
                
                // Request redraw if anything changed
                if state.needs_redraw() {
                    area.queue_draw();
                }
            }));
            
            // Start receiving frame clock updates
            frame_clock.begin_updating();
        }
    });
}

impl DisplayState {
    fn update_animations(&mut self, time: f64) {
        // Update smooth scrolling
        for window in &mut self.windows {
            window.update_scroll_animation(time);
        }
        
        // Update cursor blink
        self.cursor_visible = ((time * 2.0) as i32 % 2) == 0;
        
        // Update animated images
        self.animation_manager.tick(time);
    }
}
```

#### 8. Image and Texture Handling

```rust
use gtk4::gdk::{Texture, MemoryTexture, MemoryFormat};
use gtk4::gdk_pixbuf::Pixbuf;

fn load_image_to_texture(path: &str) -> Result<Texture, Error> {
    // Load via gdk-pixbuf (supports PNG, JPEG, GIF, SVG, WebP, etc.)
    let pixbuf = Pixbuf::from_file(path)?;
    
    // Convert to GPU texture
    let texture = Texture::for_pixbuf(&pixbuf);
    
    Ok(texture)
}

fn render_image(snapshot: &gtk4::Snapshot, texture: &Texture, bounds: &graphene::Rect) {
    snapshot.append_texture(texture, bounds);
}

// For raw pixel data (e.g., from video frame)
fn create_texture_from_rgba(
    data: &[u8],
    width: i32,
    height: i32,
) -> MemoryTexture {
    let bytes = glib::Bytes::from(data);
    
    MemoryTexture::new(
        width,
        height,
        MemoryFormat::R8g8b8a8,
        &bytes,
        width as usize * 4,  // stride
    )
}
```

#### 9. Video with GStreamer GTK4 Sink

```rust
use gstreamer as gst;
use gstreamer::prelude::*;

fn create_video_pipeline(uri: &str) -> (gst::Pipeline, gtk4::gdk::Paintable) {
    let pipeline = gst::Pipeline::new();
    
    // Source
    let source = gst::ElementFactory::make("uridecodebin")
        .property("uri", uri)
        .build()
        .unwrap();
    
    // Video convert
    let convert = gst::ElementFactory::make("videoconvert").build().unwrap();
    
    // GTK4 paintable sink - this is the magic!
    let sink = gst::ElementFactory::make("gtk4paintablesink").build().unwrap();
    
    pipeline.add_many([&source, &convert, &sink]).unwrap();
    gst::Element::link_many([&convert, &sink]).unwrap();
    
    // Handle dynamic pads from uridecodebin
    let convert_weak = convert.downgrade();
    source.connect_pad_added(move |_, src_pad| {
        if let Some(convert) = convert_weak.upgrade() {
            let sink_pad = convert.static_pad("sink").unwrap();
            if !sink_pad.is_linked() {
                src_pad.link(&sink_pad).unwrap();
            }
        }
    });
    
    // Get the paintable for rendering
    let paintable: gtk4::gdk::Paintable = sink.property("paintable");
    
    (pipeline, paintable)
}

fn render_video(
    snapshot: &gtk4::Snapshot,
    paintable: &gtk4::gdk::Paintable,
    bounds: &graphene::Rect,
) {
    // GdkPaintable integrates directly with GtkSnapshot
    paintable.snapshot(
        snapshot.upcast_ref(),
        bounds.width() as f64,
        bounds.height() as f64,
    );
}
```

### Complete GTK4 Backend Structure

```rust
// src/backend/gtk4/mod.rs

pub struct Gtk4Backend {
    app: gtk4::Application,
    window: Option<gtk4::ApplicationWindow>,
    drawing_area: Option<gtk4::DrawingArea>,
    
    // Display state
    state: Rc<RefCell<DisplayState>>,
    
    // Managers
    animation_manager: Rc<RefCell<AnimationManager>>,
    video_manager: Rc<RefCell<VideoManager>>,
    wpe_manager: Rc<RefCell<WpeManager>>,
    
    // Input state
    modifier_state: gdk4::ModifierType,
}

impl Gtk4Backend {
    pub fn new() -> Self {
        // Initialize GTK4
        gtk4::init().expect("Failed to initialize GTK4");
        
        let app = gtk4::Application::builder()
            .application_id("org.gnu.emacs")
            .build();
        
        Self {
            app,
            window: None,
            drawing_area: None,
            state: Rc::new(RefCell::new(DisplayState::new())),
            animation_manager: Rc::new(RefCell::new(AnimationManager::new())),
            video_manager: Rc::new(RefCell::new(VideoManager::new())),
            wpe_manager: Rc::new(RefCell::new(WpeManager::new())),
            modifier_state: gdk4::ModifierType::empty(),
        }
    }
    
    pub fn run(&self) {
        let state = self.state.clone();
        let anim_mgr = self.animation_manager.clone();
        let video_mgr = self.video_manager.clone();
        
        self.app.connect_activate(move |app| {
            // Create window
            let window = gtk4::ApplicationWindow::builder()
                .application(app)
                .title("Emacs")
                .default_width(1024)
                .default_height(768)
                .build();
            
            // Create drawing area
            let area = gtk4::DrawingArea::new();
            
            // Set up drawing
            let state_draw = state.clone();
            let anim_draw = anim_mgr.clone();
            let video_draw = video_mgr.clone();
            
            area.set_draw_func(move |_, snapshot, width, height| {
                let state = state_draw.borrow();
                let videos = video_draw.borrow();
                render_frame(snapshot, &state, &videos, width, height);
            });
            
            // Set up animation loop
            setup_frame_clock(&area, state.clone(), anim_mgr.clone());
            
            // Set up input
            setup_input_controllers(&window);
            
            window.set_child(Some(&area));
            window.present();
        });
        
        self.app.run();
    }
}

impl DisplayBackend for Gtk4Backend {
    fn render(&mut self, scene: &Scene) -> Result<(), BackendError> {
        // Update state from scene
        self.state.borrow_mut().update_from_scene(scene);
        
        // Request redraw
        if let Some(area) = &self.drawing_area {
            area.queue_draw();
        }
        
        Ok(())
    }
    
    fn capabilities(&self) -> BackendCapabilities {
        BackendCapabilities {
            supports_images: true,
            supports_video: true,
            supports_webkit: true,
            supports_animations: true,
            gpu_accelerated: true,
        }
    }
}
```

### Summary: GTK4 Usage

| GTK4 Component | Purpose | Our Usage |
|----------------|---------|-----------|
| `GtkApplication` | App lifecycle | Entry point, quit handling |
| `GtkApplicationWindow` | Main window | Frame/window management |
| `GtkDrawingArea` | Custom drawing | Our entire Emacs display |
| `GtkSnapshot` | Build render nodes | Scene graph → GPU commands |
| `GdkTexture` | GPU textures | Images, video frames |
| `PangoLayout` | Text shaping/render | All text rendering |
| `GdkFrameClock` | Vsync timing | Animations, smooth scroll |
| `EventController*` | Input handling | Keyboard, mouse, scroll |
| `gtk4paintablesink` | Video frames | GStreamer integration |

GTK4 provides the complete infrastructure - we just need to:
1. Build `GtkSnapshot` each frame
2. Handle input via event controllers
3. Use frame clock for animations
