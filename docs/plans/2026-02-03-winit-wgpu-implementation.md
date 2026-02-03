# Winit + wgpu Migration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace GTK4/GSK with winit + wgpu for Rust-native GPU rendering.

**Architecture:** Create new `backend/wgpu/` module with WgpuRenderer and WinitBackend. Keep Scene struct unchanged. Use ExternalBuffer trait for zero-copy DMA-BUF import on Linux, shared memory fallback elsewhere.

**Tech Stack:** winit (window/input), wgpu (GPU rendering), arboard (clipboard), raw-window-handle

---

## Task 1: Add Dependencies

**Files:**
- Modify: `rust/neomacs-display/Cargo.toml`

**Step 1: Add winit and wgpu dependencies**

Add to `[dependencies]` section:

```toml
# Winit + wgpu backend
winit = { version = "0.30", optional = true }
wgpu = { version = "23", optional = true }
raw-window-handle = { version = "0.6", optional = true }
arboard = { version = "3.4", optional = true }
bytemuck = { version = "1.18", features = ["derive"], optional = true }
pollster = { version = "0.4", optional = true }
```

**Step 2: Add feature flag**

Update `[features]` section:

```toml
[features]
default = ["winit-backend", "video", "wpe-webkit"]
gtk4-backend = []
winit-backend = ["winit", "wgpu", "raw-window-handle", "arboard", "bytemuck", "pollster"]
tty-backend = []
video = ["gstreamer", "gstreamer-video"]
wpe-webkit = []
```

**Step 3: Verify compilation**

Run: `cargo check --features winit-backend --no-default-features`
Expected: Compiles (warnings OK)

**Step 4: Commit**

```bash
git add rust/neomacs-display/Cargo.toml
git commit -m "feat: add winit + wgpu dependencies"
```

---

## Task 2: Create Backend Module Structure

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/mod.rs`
- Create: `rust/neomacs-display/src/backend/wgpu/renderer.rs`
- Create: `rust/neomacs-display/src/backend/wgpu/backend.rs`
- Create: `rust/neomacs-display/src/backend/wgpu/shaders/rect.wgsl`
- Modify: `rust/neomacs-display/src/backend/mod.rs`

**Step 1: Create mod.rs**

```rust
//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;

#[cfg(feature = "winit-backend")]
pub use renderer::WgpuRenderer;
#[cfg(feature = "winit-backend")]
pub use backend::WinitBackend;
```

**Step 2: Create renderer.rs stub**

```rust
//! wgpu GPU-accelerated scene renderer.

use crate::core::scene::Scene;

/// GPU-accelerated renderer using wgpu.
pub struct WgpuRenderer {
    // Will be populated in later tasks
}

impl WgpuRenderer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn render(&mut self, _scene: &Scene) {
        // Stub - will be implemented
    }
}

impl Default for WgpuRenderer {
    fn default() -> Self {
        Self::new()
    }
}
```

**Step 3: Create backend.rs stub**

```rust
//! Winit window and event handling backend.

use crate::core::error::DisplayResult;
use crate::core::scene::Scene;
use crate::backend::DisplayBackend;

/// Winit-based window and input backend.
pub struct WinitBackend {
    initialized: bool,
    width: u32,
    height: u32,
}

impl WinitBackend {
    pub fn new() -> Self {
        Self {
            initialized: false,
            width: 800,
            height: 600,
        }
    }
}

impl Default for WinitBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl DisplayBackend for WinitBackend {
    fn init(&mut self) -> DisplayResult<()> {
        self.initialized = true;
        Ok(())
    }

    fn shutdown(&mut self) {
        self.initialized = false;
    }

    fn render(&mut self, _scene: &Scene) -> DisplayResult<()> {
        Ok(())
    }

    fn present(&mut self) -> DisplayResult<()> {
        Ok(())
    }

    fn name(&self) -> &'static str {
        "winit-wgpu"
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn resize(&mut self, width: u32, height: u32) {
        self.width = width;
        self.height = height;
    }

    fn set_vsync(&mut self, _enabled: bool) {
        // Will be implemented with wgpu surface
    }
}
```

**Step 4: Create rect.wgsl shader**

```wgsl
// Vertex shader for solid color rectangles

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
}

struct Uniforms {
    screen_size: vec2<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    // Convert pixel coordinates to clip space (-1 to 1)
    let x = (in.position.x / uniforms.screen_size.x) * 2.0 - 1.0;
    let y = 1.0 - (in.position.y / uniforms.screen_size.y) * 2.0;
    out.clip_position = vec4<f32>(x, y, 0.0, 1.0);
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
```

**Step 5: Update backend/mod.rs**

Add after `pub mod tty;`:

```rust
#[cfg(feature = "winit-backend")]
pub mod wgpu;
```

Add to BackendType enum:

```rust
/// Winit/wgpu GPU-accelerated backend
Wgpu = 2,
```

**Step 6: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 7: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/
git add rust/neomacs-display/src/backend/mod.rs
git commit -m "feat: add wgpu backend module structure"
```

---

## Task 3: Implement WgpuRenderer Core

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/renderer.rs`
- Create: `rust/neomacs-display/src/backend/wgpu/vertex.rs`

**Step 1: Create vertex.rs**

```rust
//! Vertex types for wgpu rendering.

use bytemuck::{Pod, Zeroable};

/// Vertex for solid color rectangles.
#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct RectVertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
}

impl RectVertex {
    pub fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<RectVertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 0,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 2]>() as wgpu::BufferAddress,
                    shader_location: 1,
                    format: wgpu::VertexFormat::Float32x4,
                },
            ],
        }
    }
}

/// Vertex for textured quads (images, video, webkit).
#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct TextureVertex {
    pub position: [f32; 2],
    pub tex_coords: [f32; 2],
}

impl TextureVertex {
    pub fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<TextureVertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 0,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 2]>() as wgpu::BufferAddress,
                    shader_location: 1,
                    format: wgpu::VertexFormat::Float32x2,
                },
            ],
        }
    }
}

/// Uniforms passed to shaders.
#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct Uniforms {
    pub screen_size: [f32; 2],
    pub _padding: [f32; 2],
}
```

**Step 2: Update renderer.rs with wgpu initialization**

```rust
//! wgpu GPU-accelerated scene renderer.

use std::sync::Arc;
use wgpu::util::DeviceExt;

use crate::core::scene::Scene;
use crate::core::types::Color;

mod vertex;
use vertex::{RectVertex, Uniforms};

/// GPU-accelerated renderer using wgpu.
pub struct WgpuRenderer {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    surface: Option<wgpu::Surface<'static>>,
    surface_config: Option<wgpu::SurfaceConfiguration>,

    // Pipelines
    rect_pipeline: wgpu::RenderPipeline,

    // Buffers
    uniform_buffer: wgpu::Buffer,
    uniform_bind_group: wgpu::BindGroup,

    // State
    width: u32,
    height: u32,
}

impl WgpuRenderer {
    pub async fn new_async() -> Self {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: None,
                force_fallback_adapter: false,
            })
            .await
            .expect("Failed to find adapter");

        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: Some("neomacs"),
                    required_features: wgpu::Features::empty(),
                    required_limits: wgpu::Limits::default(),
                    memory_hints: Default::default(),
                },
                None,
            )
            .await
            .expect("Failed to create device");

        let device = Arc::new(device);
        let queue = Arc::new(queue);

        // Create uniform buffer
        let uniforms = Uniforms {
            screen_size: [800.0, 600.0],
            _padding: [0.0, 0.0],
        };
        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Uniform Buffer"),
            contents: bytemuck::cast_slice(&[uniforms]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        // Create bind group layout
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Uniform Bind Group Layout"),
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            }],
        });

        let uniform_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Uniform Bind Group"),
            layout: &bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: uniform_buffer.as_entire_binding(),
            }],
        });

        // Create rect pipeline
        let rect_shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("Rect Shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("shaders/rect.wgsl").into()),
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("Rect Pipeline Layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
        });

        let rect_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Rect Pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &rect_shader,
                entry_point: Some("vs_main"),
                buffers: &[RectVertex::desc()],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &rect_shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: wgpu::TextureFormat::Bgra8UnormSrgb,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: None,
                polygon_mode: wgpu::PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        Self {
            device,
            queue,
            surface: None,
            surface_config: None,
            rect_pipeline,
            uniform_buffer,
            uniform_bind_group,
            width: 800,
            height: 600,
        }
    }

    pub fn new() -> Self {
        pollster::block_on(Self::new_async())
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        if width == 0 || height == 0 {
            return;
        }
        self.width = width;
        self.height = height;

        // Update uniforms
        let uniforms = Uniforms {
            screen_size: [width as f32, height as f32],
            _padding: [0.0, 0.0],
        };
        self.queue.write_buffer(&self.uniform_buffer, 0, bytemuck::cast_slice(&[uniforms]));

        // Reconfigure surface if we have one
        if let (Some(surface), Some(config)) = (&self.surface, &mut self.surface_config) {
            config.width = width;
            config.height = height;
            surface.configure(&self.device, config);
        }
    }

    pub fn device(&self) -> &Arc<wgpu::Device> {
        &self.device
    }

    pub fn queue(&self) -> &Arc<wgpu::Queue> {
        &self.queue
    }

    /// Render scene to a texture (for offscreen rendering)
    pub fn render_to_texture(&mut self, scene: &Scene) -> wgpu::Texture {
        let texture = self.device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Render Target"),
            size: wgpu::Extent3d {
                width: self.width,
                height: self.height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Bgra8UnormSrgb,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        self.render_to_view(&view, scene);
        texture
    }

    /// Render scene to a texture view
    pub fn render_to_view(&mut self, view: &wgpu::TextureView, scene: &Scene) {
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Render Encoder"),
        });

        // Build vertex buffer for rectangles
        let mut vertices: Vec<RectVertex> = Vec::new();

        // Background
        self.add_rect(&mut vertices, 0.0, 0.0, scene.width, scene.height, &scene.background);

        // Windows
        for window in &scene.windows {
            let x = window.bounds.x;
            let y = window.bounds.y;
            let w = window.bounds.width;
            let h = window.bounds.height;

            // Window background
            self.add_rect(&mut vertices, x, y, w, h, &window.background);

            // Cursor
            if let Some(cursor) = &window.cursor {
                if cursor.visible {
                    self.add_rect(
                        &mut vertices,
                        x + cursor.x,
                        y + cursor.y,
                        cursor.width,
                        cursor.height,
                        &cursor.color,
                    );
                }
            }
        }

        // Borders
        for border in &scene.borders {
            self.add_rect(&mut vertices, border.x, border.y, border.width, border.height, &border.color);
        }

        // Create vertex buffer
        let vertex_buffer = self.device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Rect Vertex Buffer"),
            contents: bytemuck::cast_slice(&vertices),
            usage: wgpu::BufferUsages::VERTEX,
        });

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Render Pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: scene.background.r as f64,
                            g: scene.background.g as f64,
                            b: scene.background.b as f64,
                            a: scene.background.a as f64,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                occlusion_query_set: None,
                timestamp_writes: None,
            });

            render_pass.set_pipeline(&self.rect_pipeline);
            render_pass.set_bind_group(0, &self.uniform_bind_group, &[]);
            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
            render_pass.draw(0..vertices.len() as u32, 0..1);
        }

        self.queue.submit(std::iter::once(encoder.finish()));
    }

    fn add_rect(&self, vertices: &mut Vec<RectVertex>, x: f32, y: f32, w: f32, h: f32, color: &Color) {
        let c = [color.r, color.g, color.b, color.a];

        // Two triangles for a quad
        vertices.push(RectVertex { position: [x, y], color: c });
        vertices.push(RectVertex { position: [x + w, y], color: c });
        vertices.push(RectVertex { position: [x, y + h], color: c });

        vertices.push(RectVertex { position: [x + w, y], color: c });
        vertices.push(RectVertex { position: [x + w, y + h], color: c });
        vertices.push(RectVertex { position: [x, y + h], color: c });
    }
}

impl Default for WgpuRenderer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_renderer_creation() {
        // Note: This test requires a GPU, may fail in CI
        // let renderer = WgpuRenderer::new();
        // assert_eq!(renderer.width, 800);
    }
}
```

**Step 3: Update mod.rs to include vertex module**

Add to `rust/neomacs-display/src/backend/wgpu/mod.rs`:

```rust
//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;
#[cfg(feature = "winit-backend")]
mod vertex;

#[cfg(feature = "winit-backend")]
pub use renderer::WgpuRenderer;
#[cfg(feature = "winit-backend")]
pub use backend::WinitBackend;
```

**Step 4: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/
git commit -m "feat: implement WgpuRenderer core with rect pipeline"
```

---

## Task 4: Implement Glyph Rendering

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/shaders/glyph.wgsl`
- Create: `rust/neomacs-display/src/backend/wgpu/glyph_atlas.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/renderer.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/mod.rs`

**Step 1: Create glyph.wgsl shader**

```wgsl
// Glyph rendering shader - alpha-masked text

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
    @location(2) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
    @location(1) color: vec4<f32>,
}

struct Uniforms {
    screen_size: vec2<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

@group(1) @binding(0)
var glyph_texture: texture_2d<f32>;
@group(1) @binding(1)
var glyph_sampler: sampler;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    let x = (in.position.x / uniforms.screen_size.x) * 2.0 - 1.0;
    let y = 1.0 - (in.position.y / uniforms.screen_size.y) * 2.0;
    out.clip_position = vec4<f32>(x, y, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let alpha = textureSample(glyph_texture, glyph_sampler, in.tex_coords).r;
    return vec4<f32>(in.color.rgb, in.color.a * alpha);
}
```

**Step 2: Create glyph_atlas.rs**

```rust
//! Glyph atlas for caching rasterized glyphs as GPU textures.

use std::collections::HashMap;
use std::sync::Arc;

use crate::core::face::Face;
use crate::text::TextEngine;

/// Key for looking up cached glyphs.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct GlyphKey {
    pub charcode: u32,
    pub face_id: u32,
}

/// Cached glyph information.
pub struct CachedGlyph {
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    pub bind_group: wgpu::BindGroup,
    pub width: u32,
    pub height: u32,
    pub bearing_x: f32,
    pub bearing_y: f32,
}

/// Atlas for caching rasterized glyphs.
pub struct WgpuGlyphAtlas {
    cache: HashMap<GlyphKey, CachedGlyph>,
    text_engine: TextEngine,
    bind_group_layout: wgpu::BindGroupLayout,
    sampler: wgpu::Sampler,
}

impl WgpuGlyphAtlas {
    pub fn new(device: &wgpu::Device) -> Self {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Glyph Bind Group Layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Glyph Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        Self {
            cache: HashMap::new(),
            text_engine: TextEngine::new(),
            bind_group_layout,
            sampler,
        }
    }

    pub fn bind_group_layout(&self) -> &wgpu::BindGroupLayout {
        &self.bind_group_layout
    }

    /// Get or create a cached glyph.
    pub fn get_or_create(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        key: GlyphKey,
        face: Option<&Face>,
    ) -> Option<&CachedGlyph> {
        if self.cache.contains_key(&key) {
            return self.cache.get(&key);
        }

        // Rasterize the glyph
        let c = char::from_u32(key.charcode)?;
        let (width, height, pixels, bearing_x, bearing_y) =
            self.text_engine.rasterize_char(c, face)?;

        if width == 0 || height == 0 {
            return None;
        }

        // Create texture
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Glyph Texture"),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::R8Unorm,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });

        // Upload pixel data
        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &pixels,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(width),
                rows_per_image: Some(height),
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Glyph Bind Group"),
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
            ],
        });

        let cached = CachedGlyph {
            texture,
            view,
            bind_group,
            width,
            height,
            bearing_x,
            bearing_y,
        };

        self.cache.insert(key, cached);
        self.cache.get(&key)
    }

    /// Clear the cache.
    pub fn clear(&mut self) {
        self.cache.clear();
    }
}
```

**Step 3: Update vertex.rs with glyph vertex**

Add to `vertex.rs`:

```rust
/// Vertex for glyph rendering (textured with color).
#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct GlyphVertex {
    pub position: [f32; 2],
    pub tex_coords: [f32; 2],
    pub color: [f32; 4],
}

impl GlyphVertex {
    pub fn desc() -> wgpu::VertexBufferLayout<'static> {
        wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<GlyphVertex>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &[
                wgpu::VertexAttribute {
                    offset: 0,
                    shader_location: 0,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 2]>() as wgpu::BufferAddress,
                    shader_location: 1,
                    format: wgpu::VertexFormat::Float32x2,
                },
                wgpu::VertexAttribute {
                    offset: std::mem::size_of::<[f32; 4]>() as wgpu::BufferAddress,
                    shader_location: 2,
                    format: wgpu::VertexFormat::Float32x4,
                },
            ],
        }
    }
}
```

**Step 4: Update mod.rs**

```rust
//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;
#[cfg(feature = "winit-backend")]
mod vertex;
#[cfg(feature = "winit-backend")]
mod glyph_atlas;

#[cfg(feature = "winit-backend")]
pub use renderer::WgpuRenderer;
#[cfg(feature = "winit-backend")]
pub use backend::WinitBackend;
```

**Step 5: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 6: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/
git commit -m "feat: add glyph atlas and glyph rendering shader"
```

---

## Task 5: Implement Texture Pipeline for Images/Video/WebKit

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/shaders/texture.wgsl`
- Create: `rust/neomacs-display/src/backend/wgpu/external_buffer.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/mod.rs`

**Step 1: Create texture.wgsl shader**

```wgsl
// Texture rendering shader for images, video, webkit

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
}

struct Uniforms {
    screen_size: vec2<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

@group(1) @binding(0)
var t_texture: texture_2d<f32>;
@group(1) @binding(1)
var t_sampler: sampler;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    let x = (in.position.x / uniforms.screen_size.x) * 2.0 - 1.0;
    let y = 1.0 - (in.position.y / uniforms.screen_size.y) * 2.0;
    out.clip_position = vec4<f32>(x, y, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(t_texture, t_sampler, in.tex_coords);
}
```

**Step 2: Create external_buffer.rs**

```rust
//! External buffer abstraction for zero-copy texture import.
//!
//! On Linux: DMA-BUF import (zero-copy)
//! On other platforms: Shared memory (copy)

use std::sync::Arc;

/// Trait for external buffers that can be imported as wgpu textures.
pub trait ExternalBuffer: Send + Sync {
    /// Import the buffer as a wgpu texture.
    fn to_wgpu_texture(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Option<wgpu::Texture>;

    /// Get buffer dimensions.
    fn dimensions(&self) -> (u32, u32);
}

/// Shared memory buffer (cross-platform fallback).
pub struct SharedMemoryBuffer {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub stride: u32,
    pub format: BufferFormat,
}

#[derive(Clone, Copy, Debug)]
pub enum BufferFormat {
    Bgra8,
    Rgba8,
    Argb8,
}

impl ExternalBuffer for SharedMemoryBuffer {
    fn to_wgpu_texture(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Option<wgpu::Texture> {
        if self.width == 0 || self.height == 0 {
            return None;
        }

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("External Texture"),
            size: wgpu::Extent3d {
                width: self.width,
                height: self.height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Bgra8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });

        // Convert format if needed and upload
        let pixels = match self.format {
            BufferFormat::Bgra8 => self.data.clone(),
            BufferFormat::Rgba8 => {
                // RGBA -> BGRA
                let mut bgra = self.data.clone();
                for chunk in bgra.chunks_exact_mut(4) {
                    chunk.swap(0, 2);
                }
                bgra
            }
            BufferFormat::Argb8 => {
                // ARGB -> BGRA
                let mut bgra = Vec::with_capacity(self.data.len());
                for chunk in self.data.chunks_exact(4) {
                    bgra.push(chunk[3]); // B
                    bgra.push(chunk[2]); // G
                    bgra.push(chunk[1]); // R
                    bgra.push(chunk[0]); // A
                }
                bgra
            }
        };

        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &pixels,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(self.stride),
                rows_per_image: Some(self.height),
            },
            wgpu::Extent3d {
                width: self.width,
                height: self.height,
                depth_or_array_layers: 1,
            },
        );

        Some(texture)
    }

    fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }
}

/// DMA-BUF buffer for Linux (zero-copy).
#[cfg(target_os = "linux")]
pub struct DmaBufBuffer {
    pub fd: std::os::unix::io::RawFd,
    pub width: u32,
    pub height: u32,
    pub stride: u32,
    pub format: u32,      // DRM format
    pub modifier: u64,    // DRM modifier
}

#[cfg(target_os = "linux")]
impl ExternalBuffer for DmaBufBuffer {
    fn to_wgpu_texture(
        &self,
        device: &wgpu::Device,
        _queue: &wgpu::Queue,
    ) -> Option<wgpu::Texture> {
        // TODO: Implement DMA-BUF import via wgpu::hal
        // This requires using Vulkan's VK_EXT_external_memory_dma_buf
        // For now, fall back to None (caller should use SharedMemoryBuffer)
        log::warn!("DMA-BUF import not yet implemented, using fallback");
        None
    }

    fn dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }
}

/// Platform-specific buffer type alias.
#[cfg(target_os = "linux")]
pub type PlatformBuffer = DmaBufBuffer;

#[cfg(not(target_os = "linux"))]
pub type PlatformBuffer = SharedMemoryBuffer;
```

**Step 3: Update mod.rs**

```rust
//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;
#[cfg(feature = "winit-backend")]
mod vertex;
#[cfg(feature = "winit-backend")]
mod glyph_atlas;
#[cfg(feature = "winit-backend")]
mod external_buffer;

#[cfg(feature = "winit-backend")]
pub use renderer::WgpuRenderer;
#[cfg(feature = "winit-backend")]
pub use backend::WinitBackend;
#[cfg(feature = "winit-backend")]
pub use external_buffer::{ExternalBuffer, SharedMemoryBuffer, BufferFormat};
#[cfg(all(feature = "winit-backend", target_os = "linux"))]
pub use external_buffer::DmaBufBuffer;
```

**Step 4: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/
git commit -m "feat: add texture pipeline and external buffer abstraction"
```

---

## Task 6: Implement WinitBackend Window Creation

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/backend.rs`

**Step 1: Implement full WinitBackend**

```rust
//! Winit window and event handling backend.

use std::sync::Arc;

use raw_window_handle::{HasDisplayHandle, HasWindowHandle};
use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, PhysicalSize};
use winit::event::{ElementState, KeyEvent, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop, EventLoopProxy};
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::core::error::{DisplayError, DisplayResult};
use crate::core::scene::Scene;
use crate::backend::DisplayBackend;

use super::WgpuRenderer;

/// Custom events for the event loop.
#[derive(Debug, Clone)]
pub enum UserEvent {
    /// Request a redraw.
    Redraw,
    /// WebKit view has a new frame.
    WebKitFrame(u32),
    /// Video has a new frame.
    VideoFrame(u32),
    /// Scene updated.
    SceneUpdated,
}

/// Callbacks from Emacs.
pub struct Callbacks {
    pub on_key: Option<Box<dyn Fn(KeyEvent) + Send>>,
    pub on_mouse_button: Option<Box<dyn Fn(MouseButton, ElementState, f64, f64) + Send>>,
    pub on_mouse_move: Option<Box<dyn Fn(f64, f64) + Send>>,
    pub on_resize: Option<Box<dyn Fn(u32, u32) + Send>>,
    pub on_close: Option<Box<dyn Fn() + Send>>,
}

impl Default for Callbacks {
    fn default() -> Self {
        Self {
            on_key: None,
            on_mouse_button: None,
            on_mouse_move: None,
            on_resize: None,
            on_close: None,
        }
    }
}

/// Winit-based window and input backend.
pub struct WinitBackend {
    initialized: bool,
    width: u32,
    height: u32,
    window: Option<Arc<Window>>,
    renderer: Option<WgpuRenderer>,
    surface: Option<wgpu::Surface<'static>>,
    surface_config: Option<wgpu::SurfaceConfiguration>,
    event_loop_proxy: Option<EventLoopProxy<UserEvent>>,
    scene: Scene,
    callbacks: Callbacks,
}

impl WinitBackend {
    pub fn new() -> Self {
        Self {
            initialized: false,
            width: 800,
            height: 600,
            window: None,
            renderer: None,
            surface: None,
            surface_config: None,
            event_loop_proxy: None,
            scene: Scene::new(800.0, 600.0),
            callbacks: Callbacks::default(),
        }
    }

    /// Create window and initialize wgpu surface.
    pub fn create_window(&mut self, event_loop: &ActiveEventLoop) -> DisplayResult<()> {
        let window_attrs = Window::default_attributes()
            .with_title("Neomacs")
            .with_inner_size(LogicalSize::new(self.width, self.height));

        let window = event_loop
            .create_window(window_attrs)
            .map_err(|e| DisplayError::Backend(format!("Failed to create window: {}", e)))?;

        let window = Arc::new(window);

        // Initialize renderer
        let mut renderer = WgpuRenderer::new();

        // Create surface
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor::default());
        let surface = instance
            .create_surface(window.clone())
            .map_err(|e| DisplayError::Backend(format!("Failed to create surface: {}", e)))?;

        // Configure surface
        let size = window.inner_size();
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: wgpu::TextureFormat::Bgra8UnormSrgb,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::AutoVsync,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(renderer.device(), &config);

        renderer.resize(size.width, size.height);

        self.window = Some(window);
        self.renderer = Some(renderer);
        self.surface = Some(surface);
        self.surface_config = Some(config);
        self.width = size.width;
        self.height = size.height;
        self.initialized = true;

        Ok(())
    }

    /// Request a redraw.
    pub fn request_redraw(&self) {
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }

    /// Get event loop proxy for sending custom events.
    pub fn event_loop_proxy(&self) -> Option<&EventLoopProxy<UserEvent>> {
        self.event_loop_proxy.as_ref()
    }

    /// Set the event loop proxy.
    pub fn set_event_loop_proxy(&mut self, proxy: EventLoopProxy<UserEvent>) {
        self.event_loop_proxy = Some(proxy);
    }

    /// Update the scene.
    pub fn update_scene(&mut self, scene: Scene) {
        self.scene = scene;
    }

    /// Render current scene to window.
    pub fn do_render(&mut self) -> DisplayResult<()> {
        let surface = self.surface.as_ref()
            .ok_or_else(|| DisplayError::Backend("No surface".into()))?;
        let renderer = self.renderer.as_mut()
            .ok_or_else(|| DisplayError::Backend("No renderer".into()))?;

        let frame = surface
            .get_current_texture()
            .map_err(|e| DisplayError::Backend(format!("Failed to get frame: {}", e)))?;

        let view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());

        renderer.render_to_view(&view, &self.scene);

        frame.present();

        Ok(())
    }

    /// Handle window resize.
    fn handle_resize(&mut self, size: PhysicalSize<u32>) {
        if size.width == 0 || size.height == 0 {
            return;
        }

        self.width = size.width;
        self.height = size.height;

        if let (Some(surface), Some(config), Some(renderer)) =
            (&self.surface, &mut self.surface_config, &mut self.renderer)
        {
            config.width = size.width;
            config.height = size.height;
            surface.configure(renderer.device(), config);
            renderer.resize(size.width, size.height);
        }

        // Notify Emacs
        if let Some(cb) = &self.callbacks.on_resize {
            cb(size.width, size.height);
        }
    }

    /// Set callbacks.
    pub fn set_callbacks(&mut self, callbacks: Callbacks) {
        self.callbacks = callbacks;
    }
}

impl Default for WinitBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl DisplayBackend for WinitBackend {
    fn init(&mut self) -> DisplayResult<()> {
        // Actual initialization happens in create_window
        Ok(())
    }

    fn shutdown(&mut self) {
        self.surface = None;
        self.renderer = None;
        self.window = None;
        self.initialized = false;
    }

    fn render(&mut self, scene: &Scene) -> DisplayResult<()> {
        self.scene = scene.clone();
        Ok(())
    }

    fn present(&mut self) -> DisplayResult<()> {
        self.do_render()
    }

    fn name(&self) -> &'static str {
        "winit-wgpu"
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn resize(&mut self, width: u32, height: u32) {
        self.handle_resize(PhysicalSize::new(width, height));
    }

    fn set_vsync(&mut self, enabled: bool) {
        if let Some(config) = &mut self.surface_config {
            config.present_mode = if enabled {
                wgpu::PresentMode::AutoVsync
            } else {
                wgpu::PresentMode::AutoNoVsync
            };

            if let (Some(surface), Some(renderer)) = (&self.surface, &self.renderer) {
                surface.configure(renderer.device(), config);
            }
        }
    }
}

/// Application handler for winit event loop.
pub struct NeomacsApp {
    backend: WinitBackend,
}

impl NeomacsApp {
    pub fn new(backend: WinitBackend) -> Self {
        Self { backend }
    }
}

impl ApplicationHandler<UserEvent> for NeomacsApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.backend.window.is_none() {
            if let Err(e) = self.backend.create_window(event_loop) {
                log::error!("Failed to create window: {}", e);
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                if let Some(cb) = &self.backend.callbacks.on_close {
                    cb();
                }
                event_loop.exit();
            }
            WindowEvent::Resized(size) => {
                self.backend.handle_resize(size);
            }
            WindowEvent::RedrawRequested => {
                if let Err(e) = self.backend.do_render() {
                    log::error!("Render error: {}", e);
                }
            }
            WindowEvent::KeyboardInput { event, .. } => {
                if let Some(cb) = &self.backend.callbacks.on_key {
                    cb(event);
                }
            }
            WindowEvent::MouseInput { state, button, .. } => {
                if let Some(cb) = &self.backend.callbacks.on_mouse_button {
                    // TODO: Get actual cursor position
                    cb(button, state, 0.0, 0.0);
                }
            }
            WindowEvent::CursorMoved { position, .. } => {
                if let Some(cb) = &self.backend.callbacks.on_mouse_move {
                    cb(position.x, position.y);
                }
            }
            _ => {}
        }
    }

    fn user_event(&mut self, _event_loop: &ActiveEventLoop, event: UserEvent) {
        match event {
            UserEvent::Redraw | UserEvent::SceneUpdated => {
                self.backend.request_redraw();
            }
            UserEvent::WebKitFrame(_id) | UserEvent::VideoFrame(_id) => {
                self.backend.request_redraw();
            }
        }
    }
}
```

**Step 2: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/backend.rs
git commit -m "feat: implement WinitBackend with window creation and event handling"
```

---

## Task 7: Add Animation System

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/animation.rs`
- Create: `rust/neomacs-display/src/backend/wgpu/transition.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/mod.rs`

**Step 1: Create animation.rs**

```rust
//! Animation engine for wgpu backend.

use std::time::{Duration, Instant};

/// Animation target.
#[derive(Clone, Debug)]
pub enum AnimationTarget {
    Window(u32),
    Cursor,
    Global,
}

/// Animated property.
#[derive(Clone, Copy, Debug)]
pub enum AnimatedProperty {
    X,
    Y,
    Width,
    Height,
    Opacity,
    Scale,
    RotationY,
    RotationX,
    TranslateZ,
}

/// Easing function.
#[derive(Clone, Copy, Debug)]
pub enum Easing {
    Linear,
    EaseIn,
    EaseOut,
    EaseInOut,
    EaseOutBounce,
}

impl Easing {
    pub fn apply(&self, t: f32) -> f32 {
        match self {
            Easing::Linear => t,
            Easing::EaseIn => t * t,
            Easing::EaseOut => 1.0 - (1.0 - t) * (1.0 - t),
            Easing::EaseInOut => {
                if t < 0.5 {
                    2.0 * t * t
                } else {
                    1.0 - (-2.0 * t + 2.0).powi(2) / 2.0
                }
            }
            Easing::EaseOutBounce => {
                let n1 = 7.5625;
                let d1 = 2.75;
                if t < 1.0 / d1 {
                    n1 * t * t
                } else if t < 2.0 / d1 {
                    let t = t - 1.5 / d1;
                    n1 * t * t + 0.75
                } else if t < 2.5 / d1 {
                    let t = t - 2.25 / d1;
                    n1 * t * t + 0.9375
                } else {
                    let t = t - 2.625 / d1;
                    n1 * t * t + 0.984375
                }
            }
        }
    }
}

/// Single animation.
pub struct Animation {
    pub id: u64,
    pub target: AnimationTarget,
    pub property: AnimatedProperty,
    pub from: f32,
    pub to: f32,
    pub duration: Duration,
    pub easing: Easing,
    pub started: Instant,
}

impl Animation {
    /// Get current value based on elapsed time.
    pub fn current_value(&self) -> f32 {
        let elapsed = self.started.elapsed();
        let t = (elapsed.as_secs_f32() / self.duration.as_secs_f32()).min(1.0);
        let eased = self.easing.apply(t);
        self.from + (self.to - self.from) * eased
    }

    /// Check if animation is complete.
    pub fn is_complete(&self) -> bool {
        self.started.elapsed() >= self.duration
    }
}

/// Animation engine managing all active animations.
pub struct AnimationEngine {
    animations: Vec<Animation>,
    next_id: u64,
}

impl AnimationEngine {
    pub fn new() -> Self {
        Self {
            animations: Vec::new(),
            next_id: 1,
        }
    }

    /// Start a new animation.
    pub fn animate(
        &mut self,
        target: AnimationTarget,
        property: AnimatedProperty,
        from: f32,
        to: f32,
        duration: Duration,
        easing: Easing,
    ) -> u64 {
        let id = self.next_id;
        self.next_id += 1;

        self.animations.push(Animation {
            id,
            target,
            property,
            from,
            to,
            duration,
            easing,
            started: Instant::now(),
        });

        id
    }

    /// Cancel an animation.
    pub fn cancel(&mut self, id: u64) {
        self.animations.retain(|a| a.id != id);
    }

    /// Tick all animations, remove completed ones.
    /// Returns true if any animations are still active.
    pub fn tick(&mut self) -> bool {
        self.animations.retain(|a| !a.is_complete());
        !self.animations.is_empty()
    }

    /// Get current value for a target/property.
    pub fn get_value(&self, target: &AnimationTarget, property: AnimatedProperty) -> Option<f32> {
        for anim in &self.animations {
            if matches!(&anim.target, t if std::mem::discriminant(t) == std::mem::discriminant(target))
                && std::mem::discriminant(&anim.property) == std::mem::discriminant(&property)
            {
                return Some(anim.current_value());
            }
        }
        None
    }

    /// Check if any animations are active.
    pub fn has_animations(&self) -> bool {
        !self.animations.is_empty()
    }
}

impl Default for AnimationEngine {
    fn default() -> Self {
        Self::new()
    }
}
```

**Step 2: Create transition.rs**

```rust
//! Buffer transition effects (page flip, fade, etc.)

use std::time::{Duration, Instant};

/// Type of transition effect.
#[derive(Clone, Copy, Debug)]
pub enum TransitionType {
    PageFlipLeft,
    PageFlipRight,
    Fade,
    SlideLeft,
    SlideRight,
}

/// Buffer transition state.
pub struct BufferTransition {
    pub from_texture: wgpu::Texture,
    pub to_texture: wgpu::Texture,
    pub transition_type: TransitionType,
    pub duration: Duration,
    pub started: Instant,
}

impl BufferTransition {
    pub fn new(
        from_texture: wgpu::Texture,
        to_texture: wgpu::Texture,
        transition_type: TransitionType,
        duration: Duration,
    ) -> Self {
        Self {
            from_texture,
            to_texture,
            transition_type,
            duration,
            started: Instant::now(),
        }
    }

    /// Get progress (0.0 to 1.0).
    pub fn progress(&self) -> f32 {
        let elapsed = self.started.elapsed();
        (elapsed.as_secs_f32() / self.duration.as_secs_f32()).min(1.0)
    }

    /// Check if transition is complete.
    pub fn is_complete(&self) -> bool {
        self.progress() >= 1.0
    }

    /// Get rotation angle for page flip effect.
    /// Returns (old_page_angle, new_page_angle) in degrees.
    pub fn page_flip_angles(&self) -> (f32, f32) {
        let progress = self.progress();

        match self.transition_type {
            TransitionType::PageFlipLeft => {
                // Old page: 0° -> -90°, visible first half
                // New page: 90° -> 0°, visible second half
                let old_angle = progress * -90.0;
                let new_angle = (1.0 - progress) * 90.0;
                (old_angle, new_angle)
            }
            TransitionType::PageFlipRight => {
                // Opposite direction
                let old_angle = progress * 90.0;
                let new_angle = (1.0 - progress) * -90.0;
                (old_angle, new_angle)
            }
            _ => (0.0, 0.0),
        }
    }

    /// Get opacity for fade effect.
    /// Returns (old_opacity, new_opacity).
    pub fn fade_opacity(&self) -> (f32, f32) {
        let progress = self.progress();
        (1.0 - progress, progress)
    }

    /// Get offset for slide effect.
    /// Returns (old_offset, new_offset) as fraction of width.
    pub fn slide_offset(&self) -> (f32, f32) {
        let progress = self.progress();

        match self.transition_type {
            TransitionType::SlideLeft => {
                (-progress, 1.0 - progress)
            }
            TransitionType::SlideRight => {
                (progress, -1.0 + progress)
            }
            _ => (0.0, 0.0),
        }
    }
}

/// Transition manager.
pub struct TransitionManager {
    active: Option<BufferTransition>,
}

impl TransitionManager {
    pub fn new() -> Self {
        Self { active: None }
    }

    /// Start a new transition.
    pub fn start(
        &mut self,
        from_texture: wgpu::Texture,
        to_texture: wgpu::Texture,
        transition_type: TransitionType,
        duration: Duration,
    ) {
        self.active = Some(BufferTransition::new(
            from_texture,
            to_texture,
            transition_type,
            duration,
        ));
    }

    /// Get active transition.
    pub fn active(&self) -> Option<&BufferTransition> {
        self.active.as_ref()
    }

    /// Tick and clean up completed transitions.
    pub fn tick(&mut self) {
        if let Some(transition) = &self.active {
            if transition.is_complete() {
                self.active = None;
            }
        }
    }

    /// Check if a transition is active.
    pub fn has_transition(&self) -> bool {
        self.active.is_some()
    }
}

impl Default for TransitionManager {
    fn default() -> Self {
        Self::new()
    }
}
```

**Step 3: Update mod.rs**

```rust
//! Winit + wgpu GPU-accelerated display backend.

#[cfg(feature = "winit-backend")]
mod renderer;
#[cfg(feature = "winit-backend")]
mod backend;
#[cfg(feature = "winit-backend")]
mod vertex;
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
pub use backend::{WinitBackend, NeomacsApp, UserEvent, Callbacks};
#[cfg(feature = "winit-backend")]
pub use external_buffer::{ExternalBuffer, SharedMemoryBuffer, BufferFormat};
#[cfg(all(feature = "winit-backend", target_os = "linux"))]
pub use external_buffer::DmaBufBuffer;
#[cfg(feature = "winit-backend")]
pub use animation::{AnimationEngine, AnimationTarget, AnimatedProperty, Easing};
#[cfg(feature = "winit-backend")]
pub use transition::{TransitionManager, TransitionType, BufferTransition};
```

**Step 4: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/
git commit -m "feat: add animation engine and buffer transition effects"
```

---

## Task 8: Update FFI Layer

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`
- Modify: `rust/neomacs-display/src/backend/mod.rs`

**Step 1: Add Wgpu to BackendType**

In `src/backend/mod.rs`, ensure BackendType has:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub enum BackendType {
    Gtk4 = 0,
    Tty = 1,
    #[cfg(feature = "winit-backend")]
    Wgpu = 2,
}
```

**Step 2: Update ffi.rs imports**

Add at top of file:

```rust
#[cfg(feature = "winit-backend")]
use crate::backend::wgpu::{WinitBackend, WgpuRenderer};
```

**Step 3: Update NeomacsDisplay struct**

Add fields:

```rust
pub struct NeomacsDisplay {
    // ... existing fields ...

    #[cfg(feature = "winit-backend")]
    wgpu_backend: Option<WinitBackend>,
    #[cfg(feature = "winit-backend")]
    wgpu_renderer: Option<WgpuRenderer>,
}
```

**Step 4: Update neomacs_display_init**

Add handling for Wgpu backend:

```rust
#[cfg(feature = "winit-backend")]
BackendType::Wgpu => {
    let wgpu_backend = WinitBackend::new();
    let wgpu_renderer = WgpuRenderer::new();
    display.wgpu_backend = Some(wgpu_backend);
    display.wgpu_renderer = Some(wgpu_renderer);
    info!("Using wgpu backend");
}
```

**Step 5: Update get_backend method**

```rust
fn get_backend(&mut self) -> Option<&mut dyn DisplayBackend> {
    match self.backend_type {
        BackendType::Gtk4 => self.gtk4_backend.as_mut().map(|b| b as &mut dyn DisplayBackend),
        BackendType::Tty => self.tty_backend.as_mut().map(|b| b as &mut dyn DisplayBackend),
        #[cfg(feature = "winit-backend")]
        BackendType::Wgpu => self.wgpu_backend.as_mut().map(|b| b as &mut dyn DisplayBackend),
    }
}
```

**Step 6: Verify compilation**

Run: `cargo check --features winit-backend`
Expected: Compiles

**Step 7: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git add rust/neomacs-display/src/backend/mod.rs
git commit -m "feat: add wgpu backend to FFI layer"
```

---

## Task 9: Remove GTK4 Dependencies (Final Cleanup)

**Note:** This task should only be done after the wgpu backend is fully working and tested.

**Files:**
- Modify: `rust/neomacs-display/Cargo.toml`
- Delete: `rust/neomacs-display/src/backend/gtk4/` (entire directory)
- Modify: `rust/neomacs-display/src/backend/mod.rs`
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Update Cargo.toml**

Remove gtk4-backend from default features:

```toml
[features]
default = ["winit-backend", "video", "wpe-webkit"]
```

Remove or make optional all GTK4 dependencies.

**Step 2: Remove gtk4 backend module**

```bash
rm -rf rust/neomacs-display/src/backend/gtk4/
```

**Step 3: Update backend/mod.rs**

Remove gtk4 module and BackendType::Gtk4.

**Step 4: Update ffi.rs**

Remove all gtk4 backend code paths.

**Step 5: Verify compilation**

Run: `cargo build --features winit-backend`
Expected: Compiles and links

**Step 6: Commit**

```bash
git add -A
git commit -m "refactor: remove GTK4 backend, winit-wgpu is now the default"
```

---

## Summary

| Task | Description | Dependencies |
|------|-------------|--------------|
| 1 | Add dependencies | None |
| 2 | Module structure | Task 1 |
| 3 | WgpuRenderer core | Task 2 |
| 4 | Glyph rendering | Task 3 |
| 5 | Texture pipeline | Task 3 |
| 6 | WinitBackend | Task 3 |
| 7 | Animation system | Task 3 |
| 8 | FFI layer | Tasks 3-7 |
| 9 | Remove GTK4 | Task 8 (after testing) |

Estimated: 9 tasks, each with multiple small steps.
