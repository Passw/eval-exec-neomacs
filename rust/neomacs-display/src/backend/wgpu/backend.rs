//! Winit window and event handling backend.

use std::sync::Arc;

use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, PhysicalSize};
use winit::event::{ElementState, KeyEvent, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop, EventLoopProxy};
use winit::window::{Window, WindowId};

use crate::backend::DisplayBackend;
use crate::core::error::{DisplayError, DisplayResult};
use crate::core::scene::Scene;

use super::WgpuRenderer;

/// Custom user events for the event loop.
#[derive(Debug, Clone)]
pub enum UserEvent {
    /// Request a redraw of the window.
    Redraw,
    /// A WebKit frame is ready for the given view ID.
    WebKitFrame(u32),
    /// A video frame is ready for the given video ID.
    VideoFrame(u32),
    /// The scene has been updated and needs re-rendering.
    SceneUpdated,
}

/// Callbacks for handling window events.
pub struct Callbacks {
    /// Called when a keyboard event occurs.
    pub on_key: Option<Box<dyn Fn(KeyEvent) + Send>>,
    /// Called when a mouse button event occurs.
    pub on_mouse_button: Option<Box<dyn Fn(MouseButton, ElementState, f64, f64) + Send>>,
    /// Called when the mouse cursor moves.
    pub on_mouse_move: Option<Box<dyn Fn(f64, f64) + Send>>,
    /// Called when the window is resized.
    pub on_resize: Option<Box<dyn Fn(u32, u32) + Send>>,
    /// Called when the window close is requested.
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
    /// Whether the backend has been initialized.
    initialized: bool,
    /// Current window width.
    width: u32,
    /// Current window height.
    height: u32,
    /// The winit window.
    window: Option<Arc<Window>>,
    /// The wgpu renderer.
    renderer: Option<WgpuRenderer>,
    /// The wgpu surface for rendering.
    surface: Option<wgpu::Surface<'static>>,
    /// Surface configuration.
    surface_config: Option<wgpu::SurfaceConfiguration>,
    /// Event loop proxy for sending custom events.
    event_loop_proxy: Option<EventLoopProxy<UserEvent>>,
    /// The current scene to render.
    scene: Scene,
    /// Callbacks for handling events.
    callbacks: Callbacks,
    /// Current cursor position.
    cursor_position: (f64, f64),
}

impl WinitBackend {
    /// Create a new WinitBackend.
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
            cursor_position: (0.0, 0.0),
        }
    }

    /// Create the window and initialize the wgpu surface.
    ///
    /// This should be called from the event loop's `resumed` event.
    pub fn create_window(&mut self, event_loop: &ActiveEventLoop) -> DisplayResult<()> {
        let window_attributes = Window::default_attributes()
            .with_title("Neomacs")
            .with_inner_size(LogicalSize::new(self.width, self.height));

        let window = event_loop
            .create_window(window_attributes)
            .map_err(|e| DisplayError::InitFailed(format!("Failed to create window: {}", e)))?;

        let window = Arc::new(window);

        // Get the actual size
        let size = window.inner_size();
        self.width = size.width;
        self.height = size.height;

        // Create wgpu instance
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        // Create surface - we need to use unsafe to create a surface from the window
        // SAFETY: The window is valid and will outlive the surface because we store
        // an Arc<Window> in self.window.
        let surface = instance
            .create_surface(window.clone())
            .map_err(|e| DisplayError::InitFailed(format!("Failed to create surface: {}", e)))?;

        // Request adapter
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .ok_or_else(|| DisplayError::InitFailed("Failed to find a suitable GPU adapter".to_string()))?;

        // Request device and queue
        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("Neomacs Device"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: Default::default(),
            },
            None,
        ))
        .map_err(|e| DisplayError::InitFailed(format!("Failed to create device: {}", e)))?;

        let device = Arc::new(device);
        let queue = Arc::new(queue);

        // Configure surface
        let caps = surface.get_capabilities(&adapter);
        let format = caps
            .formats
            .iter()
            .copied()
            .find(|f| f.is_srgb())
            .unwrap_or(caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: self.width,
            height: self.height,
            present_mode: wgpu::PresentMode::Fifo, // VSync
            alpha_mode: caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        // Create renderer
        let renderer = WgpuRenderer::new(None, self.width, self.height);

        self.window = Some(window);
        self.surface = Some(surface);
        self.surface_config = Some(config);
        self.renderer = Some(renderer);
        self.initialized = true;

        // Update scene dimensions
        self.scene = Scene::new(self.width as f32, self.height as f32);

        log::info!(
            "WinitBackend initialized: {}x{}, format: {:?}",
            self.width,
            self.height,
            format
        );

        Ok(())
    }

    /// Request a redraw of the window.
    pub fn request_redraw(&self) {
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }

    /// Get the event loop proxy.
    pub fn event_loop_proxy(&self) -> Option<&EventLoopProxy<UserEvent>> {
        self.event_loop_proxy.as_ref()
    }

    /// Set the event loop proxy.
    pub fn set_event_loop_proxy(&mut self, proxy: EventLoopProxy<UserEvent>) {
        self.event_loop_proxy = Some(proxy);
    }

    /// Update the scene to be rendered.
    pub fn update_scene(&mut self, scene: Scene) {
        self.scene = scene;
    }

    /// Perform the actual rendering.
    pub fn do_render(&mut self) -> DisplayResult<()> {
        let surface = match &self.surface {
            Some(s) => s,
            None => return Ok(()),
        };

        let renderer = match &self.renderer {
            Some(r) => r,
            None => return Ok(()),
        };

        // Get the current texture from the surface
        let output = match surface.get_current_texture() {
            Ok(output) => output,
            Err(wgpu::SurfaceError::Lost) => {
                // Reconfigure the surface
                self.handle_resize(PhysicalSize::new(self.width, self.height));
                return Ok(());
            }
            Err(wgpu::SurfaceError::OutOfMemory) => {
                return Err(DisplayError::Render("Out of GPU memory".to_string()));
            }
            Err(e) => {
                log::warn!("Surface error: {:?}", e);
                return Ok(());
            }
        };

        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        // Render the scene to the view
        renderer.render_to_view(&view, &self.scene);

        // Present the frame
        output.present();

        Ok(())
    }

    /// Handle window resize.
    pub fn handle_resize(&mut self, size: PhysicalSize<u32>) {
        if size.width == 0 || size.height == 0 {
            return;
        }

        self.width = size.width;
        self.height = size.height;

        // Reconfigure the surface
        if let (Some(surface), Some(config)) = (&self.surface, &mut self.surface_config) {
            config.width = size.width;
            config.height = size.height;

            // We need the device to reconfigure
            if let Some(renderer) = &self.renderer {
                surface.configure(renderer.device(), config);
            }
        }

        // Resize the renderer
        if let Some(renderer) = &mut self.renderer {
            renderer.resize(size.width, size.height);
        }

        // Update scene dimensions
        self.scene = Scene::new(size.width as f32, size.height as f32);

        // Call the resize callback
        if let Some(ref callback) = self.callbacks.on_resize {
            callback(size.width, size.height);
        }
    }

    /// Set the callbacks for handling events.
    pub fn set_callbacks(&mut self, callbacks: Callbacks) {
        self.callbacks = callbacks;
    }

    /// Get the window.
    pub fn window(&self) -> Option<&Arc<Window>> {
        self.window.as_ref()
    }
}

impl Default for WinitBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl DisplayBackend for WinitBackend {
    fn init(&mut self) -> DisplayResult<()> {
        // Actual initialization happens in create_window when we have access to ActiveEventLoop
        Ok(())
    }

    fn shutdown(&mut self) {
        self.surface = None;
        self.surface_config = None;
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
                wgpu::PresentMode::Fifo
            } else {
                wgpu::PresentMode::Immediate
            };

            // Reconfigure surface with new present mode
            if let (Some(surface), Some(renderer)) = (&self.surface, &self.renderer) {
                surface.configure(renderer.device(), config);
            }
        }
    }
}

/// Neomacs application handler for winit.
pub struct NeomacsApp {
    /// The backend instance.
    pub backend: WinitBackend,
}

impl NeomacsApp {
    /// Create a new NeomacsApp with the given backend.
    pub fn new(backend: WinitBackend) -> Self {
        Self { backend }
    }
}

impl ApplicationHandler<UserEvent> for NeomacsApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        // Create the window if it doesn't exist
        if self.backend.window.is_none() {
            if let Err(e) = self.backend.create_window(event_loop) {
                log::error!("Failed to create window: {}", e);
                event_loop.exit();
                return;
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
                if let Some(ref callback) = self.backend.callbacks.on_close {
                    callback();
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

            WindowEvent::KeyboardInput {
                event,
                is_synthetic: false,
                ..
            } => {
                if let Some(ref callback) = self.backend.callbacks.on_key {
                    callback(event);
                }
            }

            WindowEvent::MouseInput { state, button, .. } => {
                if let Some(ref callback) = self.backend.callbacks.on_mouse_button {
                    let (x, y) = self.backend.cursor_position;
                    callback(button, state, x, y);
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                self.backend.cursor_position = (position.x, position.y);
                if let Some(ref callback) = self.backend.callbacks.on_mouse_move {
                    callback(position.x, position.y);
                }
            }

            _ => {}
        }
    }

    fn user_event(&mut self, _event_loop: &ActiveEventLoop, event: UserEvent) {
        match event {
            UserEvent::Redraw => {
                self.backend.request_redraw();
            }

            UserEvent::SceneUpdated => {
                self.backend.request_redraw();
            }

            UserEvent::WebKitFrame(view_id) => {
                log::debug!("WebKit frame ready for view {}", view_id);
                self.backend.request_redraw();
            }

            UserEvent::VideoFrame(video_id) => {
                log::debug!("Video frame ready for video {}", video_id);
                self.backend.request_redraw();
            }
        }
    }
}

/// Create and run the event loop with the given backend.
pub fn run_event_loop(mut backend: WinitBackend) -> DisplayResult<()> {
    let event_loop = EventLoop::<UserEvent>::with_user_event()
        .build()
        .map_err(|e| DisplayError::InitFailed(format!("Failed to create event loop: {}", e)))?;

    event_loop.set_control_flow(ControlFlow::Wait);

    // Store the event loop proxy in the backend
    backend.set_event_loop_proxy(event_loop.create_proxy());

    let mut app = NeomacsApp::new(backend);

    event_loop
        .run_app(&mut app)
        .map_err(|e| DisplayError::Backend(format!("Event loop error: {}", e)))?;

    Ok(())
}
