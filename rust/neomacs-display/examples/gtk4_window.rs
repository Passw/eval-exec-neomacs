//! Simple GTK4 window example demonstrating the display engine.
//!
//! Run with: cargo run --example gtk4_window

use gtk4::prelude::*;
use gtk4::{glib, Application, ApplicationWindow};

use neomacs_display::backend::gtk4::Gtk4Backend;
use neomacs_display::core::scene::{Scene, WindowScene, CursorState, CursorStyle};
use neomacs_display::core::types::{Color, Rect};
use neomacs_display::backend::DisplayBackend;

const APP_ID: &str = "org.neomacs.display.example";

fn main() -> glib::ExitCode {
    // Initialize GTK4
    let app = Application::builder()
        .application_id(APP_ID)
        .build();

    app.connect_activate(build_ui);
    app.run()
}

fn build_ui(app: &Application) {
    // Create backend and drawing area
    let mut backend = Gtk4Backend::new();
    backend.init().expect("Failed to initialize GTK4 backend");
    
    let drawing_area = backend.create_drawing_area();

    // Create a test scene
    let mut scene = create_test_scene();
    
    // Update the backend with our scene
    backend.update_scene(scene);

    // Create the window
    let window = ApplicationWindow::builder()
        .application(app)
        .title("Neomacs Display Engine Test")
        .default_width(800)
        .default_height(600)
        .child(&drawing_area)
        .build();

    // Set up animation timer for cursor blink
    let da_clone = drawing_area.clone();
    glib::timeout_add_local(std::time::Duration::from_millis(500), move || {
        da_clone.queue_draw();
        glib::ControlFlow::Continue
    });

    window.present();
}

fn create_test_scene() -> Scene {
    let mut scene = Scene::new(800.0, 600.0);
    scene.background = Color::rgb(0.1, 0.1, 0.15); // Dark background

    // Create a test window
    let window = WindowScene {
        window_id: 1,
        bounds: Rect::new(50.0, 50.0, 700.0, 500.0),
        background: Color::rgb(0.15, 0.15, 0.2),
        rows: Vec::new(),
        cursor: Some(CursorState {
            x: 60.0,
            y: 60.0,
            width: 10.0,
            height: 20.0,
            style: CursorStyle::Box,
            color: Color::rgb(0.8, 0.8, 0.2),
            visible: true,
        }),
        scroll_offset: 0.0,
        selected: true,
        mode_line_height: 20,
        header_line_height: 0,
    };

    scene.windows.push(window);

    // Add a second window (like a minibuffer)
    let minibuffer = WindowScene {
        window_id: 2,
        bounds: Rect::new(50.0, 560.0, 700.0, 30.0),
        background: Color::rgb(0.2, 0.2, 0.25),
        rows: Vec::new(),
        cursor: None,
        scroll_offset: 0.0,
        selected: false,
        mode_line_height: 0,
        header_line_height: 0,
    };

    scene.windows.push(minibuffer);

    scene
}
