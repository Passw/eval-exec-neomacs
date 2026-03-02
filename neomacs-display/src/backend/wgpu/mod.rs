//! Winit + wgpu GPU-accelerated display backend.

mod animation;
mod backend;
mod events;
pub(crate) mod external_buffer;
mod glyph_atlas;
mod image_cache;
mod renderer;
pub mod toolbar_icons;
mod transition;
mod vertex;
mod window_state;
mod xbm;
mod xpm;

#[cfg(all(feature = "video", target_os = "linux"))]
mod vulkan_dmabuf;

#[cfg(all(feature = "video", target_os = "linux"))]
mod va_dmabuf_export;

#[cfg(feature = "video")]
mod video_cache;

pub mod media_budget;

#[cfg(feature = "video")]
pub use video_cache::{CachedVideo, DecodedFrame, VideoCache, VideoState};

pub use backend::{Callbacks, NeomacsApp, UserEvent, WinitBackend, run_event_loop};
pub use glyph_atlas::{CachedGlyph, GlyphKey, WgpuGlyphAtlas};
pub use image_cache::{CachedImage, ImageCache, ImageDimensions, ImageState};
pub use renderer::WgpuRenderer;
pub use vertex::GlyphVertex;

#[cfg(target_os = "linux")]
pub use external_buffer::DmaBufBuffer;
pub use external_buffer::{BufferFormat, ExternalBuffer, PlatformBuffer, SharedMemoryBuffer};

pub use animation::{AnimatedProperty, Animation, AnimationEngine, AnimationTarget, Easing};
pub use events::{
    EventKind, NEOMACS_CTRL_MASK, NEOMACS_EVENT_BUTTON_PRESS, NEOMACS_EVENT_BUTTON_RELEASE,
    NEOMACS_EVENT_CLOSE, NEOMACS_EVENT_FILE_DROP, NEOMACS_EVENT_FOCUS_IN, NEOMACS_EVENT_FOCUS_OUT,
    NEOMACS_EVENT_IMAGE_DIMENSIONS_READY, NEOMACS_EVENT_KEY_PRESS, NEOMACS_EVENT_KEY_RELEASE,
    NEOMACS_EVENT_MENU_BAR_CLICK, NEOMACS_EVENT_MENU_SELECTION, NEOMACS_EVENT_MOUSE_MOVE,
    NEOMACS_EVENT_RESIZE, NEOMACS_EVENT_SCROLL, NEOMACS_EVENT_TERMINAL_EXITED,
    NEOMACS_EVENT_TERMINAL_TITLE_CHANGED, NEOMACS_EVENT_TOOL_BAR_CLICK, NEOMACS_META_MASK,
    NEOMACS_SHIFT_MASK, NEOMACS_SUPER_MASK, NeomacsInputEvent,
};
pub use transition::{BufferTransition, TransitionManager, TransitionType};
pub use window_state::WindowState;

#[cfg(all(feature = "wpe-webkit", target_os = "linux"))]
mod webkit_cache;

#[cfg(all(feature = "wpe-webkit", target_os = "linux"))]
pub use webkit_cache::{CachedWebKitView, WgpuWebKitCache};

// DRM device discovery for GPU device path mapping
#[cfg(target_os = "linux")]
mod drm_device;

#[cfg(target_os = "linux")]
pub use drm_device::{
    DrmDeviceInfo, find_drm_render_nodes, find_render_node_for_adapter,
    get_render_node_from_adapter_info,
};
