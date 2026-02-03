//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod vertex;
#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;
#[cfg(feature = "winit-backend")]
mod glyph_atlas;
#[cfg(feature = "winit-backend")]
mod external_buffer;
#[cfg(feature = "winit-backend")]
mod animation;
#[cfg(feature = "winit-backend")]
mod transition;

#[cfg(feature = "winit-backend")]
pub use renderer::WgpuRenderer;
#[cfg(feature = "winit-backend")]
pub use backend::{WinitBackend, UserEvent, Callbacks, NeomacsApp, run_event_loop};
#[cfg(feature = "winit-backend")]
pub use glyph_atlas::{WgpuGlyphAtlas, GlyphKey, CachedGlyph};
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
