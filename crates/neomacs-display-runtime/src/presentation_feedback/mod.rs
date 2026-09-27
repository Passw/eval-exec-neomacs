//! Opt-in native presentation receipts for GUI diagnostics.
//!
//! Submission, frame callbacks, and compositor-confirmed presentation are
//! different observations. This observer does not drive rendering or pacing.

cfg_select! {
    target_os = "linux" => {
        mod wayland;
        pub(crate) use wayland::PresentationObserver;
    }
    _ => {
        pub(crate) struct PresentationObserver;
        impl PresentationObserver {
            pub(crate) fn new() -> Self { Self }
            pub(crate) fn before_present(&mut self, _: &dyn winit::window::Window, _: u64, _: neomacs_display_protocol::PresentationId, _: (u32, u32), _: f64) {}
            pub(crate) fn dispatch_deadline(&mut self, _: neomacs_display_protocol::frame_time::EventTime) -> Option<neomacs_display_protocol::frame_time::EventTime> { None }
            pub(crate) fn dispatch_pending(&mut self) {}
            pub(crate) fn shutdown(&mut self) {}
        }
    }
}
