//! WebKit view texture cache for wgpu rendering.

use std::collections::HashMap;
use std::time::Instant;

use super::external_buffer::DmaBufBuffer;

/// Cached WebKit view texture.
pub struct CachedWebKitView {
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    pub bind_group: wgpu::BindGroup,
    pub width: u32,
    pub height: u32,
    pub last_updated: Instant,
}

/// Cache of WebKit view textures for wgpu rendering.
pub struct WgpuWebKitCache {
    views: HashMap<u32, CachedWebKitView>,
    bind_group_layout: wgpu::BindGroupLayout,
    sampler: wgpu::Sampler,
}

impl WgpuWebKitCache {
    /// Create a new WebKit cache.
    pub fn new(device: &wgpu::Device) -> Self {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("WebKit Bind Group Layout"),
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
            label: Some("WebKit Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        Self {
            views: HashMap::new(),
            bind_group_layout,
            sampler,
        }
    }

    /// Get the bind group layout for texture rendering.
    pub fn bind_group_layout(&self) -> &wgpu::BindGroupLayout {
        &self.bind_group_layout
    }

    /// Update or create a cached view from DmaBufBuffer.
    pub fn update_view(
        &mut self,
        view_id: u32,
        buffer: DmaBufBuffer,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> bool {
        let texture = match buffer.to_wgpu_texture(device, queue) {
            Some(t) => t,
            None => {
                log::warn!("Failed to import DMA-BUF for view {}", view_id);
                return false;
            }
        };

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("WebKit Bind Group"),
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

        let (width, height) = buffer.dimensions();

        self.views.insert(view_id, CachedWebKitView {
            texture,
            view,
            bind_group,
            width,
            height,
            last_updated: Instant::now(),
        });

        true
    }

    /// Get a cached view.
    pub fn get(&self, view_id: u32) -> Option<&CachedWebKitView> {
        self.views.get(&view_id)
    }

    /// Get bind group for a view.
    pub fn get_bind_group(&self, view_id: u32) -> Option<&wgpu::BindGroup> {
        self.views.get(&view_id).map(|v| &v.bind_group)
    }

    /// Remove a view.
    pub fn remove(&mut self, view_id: u32) {
        self.views.remove(&view_id);
    }

    /// Clear all cached views.
    pub fn clear(&mut self) {
        self.views.clear();
    }
}
