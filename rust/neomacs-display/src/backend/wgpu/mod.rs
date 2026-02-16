//! Winit + wgpu GPU-accelerated display backend.

mod vertex;
mod renderer;
mod backend;
mod glyph_atlas;
pub(crate) mod external_buffer;
mod animation;
mod transition;
mod window_state;
mod events;
mod image_cache;
mod xpm;

#[cfg(all(feature = "video", target_os = "linux"))]
mod vulkan_dmabuf;

#[cfg(all(feature = "video", target_os = "linux"))]
mod va_dmabuf_export;

#[cfg(feature = "video")]
mod video_cache;

pub mod media_budget;

#[cfg(feature = "video")]
pub use video_cache::{VideoCache, CachedVideo, VideoState, DecodedFrame};

pub use renderer::WgpuRenderer;
pub use backend::{WinitBackend, UserEvent, Callbacks, NeomacsApp, run_event_loop};
pub use glyph_atlas::{WgpuGlyphAtlas, GlyphKey, CachedGlyph};
pub use image_cache::{ImageCache, CachedImage, ImageDimensions, ImageState};
pub use vertex::GlyphVertex;

pub use external_buffer::{ExternalBuffer, SharedMemoryBuffer, BufferFormat, PlatformBuffer};
#[cfg(target_os = "linux")]
pub use external_buffer::DmaBufBuffer;

pub use animation::{AnimationTarget, AnimatedProperty, Easing, Animation, AnimationEngine};
pub use transition::{TransitionType, BufferTransition, TransitionManager};
pub use window_state::WindowState;
pub use events::{
    EventKind, NeomacsInputEvent,
    NEOMACS_SHIFT_MASK, NEOMACS_CTRL_MASK, NEOMACS_META_MASK, NEOMACS_SUPER_MASK,
    NEOMACS_EVENT_KEY_PRESS, NEOMACS_EVENT_KEY_RELEASE,
    NEOMACS_EVENT_BUTTON_PRESS, NEOMACS_EVENT_BUTTON_RELEASE,
    NEOMACS_EVENT_MOUSE_MOVE, NEOMACS_EVENT_SCROLL,
    NEOMACS_EVENT_RESIZE, NEOMACS_EVENT_CLOSE,
    NEOMACS_EVENT_FOCUS_IN, NEOMACS_EVENT_FOCUS_OUT,
    NEOMACS_EVENT_IMAGE_DIMENSIONS_READY,
    NEOMACS_EVENT_TERMINAL_EXITED,
    NEOMACS_EVENT_MENU_SELECTION,
    NEOMACS_EVENT_FILE_DROP,
    NEOMACS_EVENT_TERMINAL_TITLE_CHANGED,
};

#[cfg(all(feature = "wpe-webkit", target_os = "linux"))]
mod webkit_cache;

#[cfg(all(feature = "wpe-webkit", target_os = "linux"))]
pub use webkit_cache::{WgpuWebKitCache, CachedWebKitView};

// DRM device discovery for GPU device path mapping
#[cfg(target_os = "linux")]
mod drm_device;

#[cfg(target_os = "linux")]
pub use drm_device::{DrmDeviceInfo, find_drm_render_nodes, find_render_node_for_adapter, get_render_node_from_adapter_info};
