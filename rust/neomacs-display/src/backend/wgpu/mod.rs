//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod vertex;
#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;
#[cfg(feature = "winit-backend")]
mod glyph_atlas;
#[cfg(any(feature = "winit-backend", feature = "wpe-webkit"))]
pub(crate) mod external_buffer;
#[cfg(feature = "winit-backend")]
mod animation;
#[cfg(feature = "winit-backend")]
mod transition;
#[cfg(feature = "winit-backend")]
mod window_state;
#[cfg(feature = "winit-backend")]
mod events;
#[cfg(feature = "winit-backend")]
mod image_cache;

#[cfg(all(feature = "video", target_os = "linux"))]
mod vulkan_dmabuf;

#[cfg(all(feature = "video", target_os = "linux"))]
mod va_dmabuf_export;

#[cfg(feature = "video")]
mod video_cache;

pub mod media_budget;

#[cfg(feature = "video")]
pub use video_cache::{VideoCache, CachedVideo, VideoState, DecodedFrame};

#[cfg(feature = "winit-backend")]
pub use renderer::WgpuRenderer;
#[cfg(feature = "winit-backend")]
pub use backend::{WinitBackend, UserEvent, Callbacks, NeomacsApp, run_event_loop};
#[cfg(feature = "winit-backend")]
pub use glyph_atlas::{WgpuGlyphAtlas, GlyphKey, CachedGlyph};
#[cfg(feature = "winit-backend")]
pub use image_cache::{ImageCache, CachedImage, ImageDimensions, ImageState};
#[cfg(feature = "winit-backend")]
pub use vertex::GlyphVertex;

#[cfg(feature = "winit-backend")]
pub use external_buffer::{ExternalBuffer, SharedMemoryBuffer, BufferFormat, PlatformBuffer};
#[cfg(all(feature = "winit-backend", target_os = "linux"))]
pub use external_buffer::DmaBufBuffer;

#[cfg(feature = "winit-backend")]
pub use animation::{AnimationTarget, AnimatedProperty, Easing, Animation, AnimationEngine};
#[cfg(feature = "winit-backend")]
pub use transition::{TransitionType, BufferTransition, TransitionManager};
#[cfg(feature = "winit-backend")]
pub use window_state::WindowState;
#[cfg(feature = "winit-backend")]
pub use events::{
    EventKind, NeomacsInputEvent,
    NEOMACS_SHIFT_MASK, NEOMACS_CTRL_MASK, NEOMACS_META_MASK, NEOMACS_SUPER_MASK,
    NEOMACS_EVENT_KEY_PRESS, NEOMACS_EVENT_KEY_RELEASE,
    NEOMACS_EVENT_BUTTON_PRESS, NEOMACS_EVENT_BUTTON_RELEASE,
    NEOMACS_EVENT_MOUSE_MOVE, NEOMACS_EVENT_SCROLL,
    NEOMACS_EVENT_RESIZE, NEOMACS_EVENT_CLOSE,
    NEOMACS_EVENT_FOCUS_IN, NEOMACS_EVENT_FOCUS_OUT,
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
