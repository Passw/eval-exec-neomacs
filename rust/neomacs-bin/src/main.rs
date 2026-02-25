//! Neomacs — standalone Rust binary
//!
//! This is the pure-Rust entry point for Neomacs, bypassing the C Emacs
//! binary entirely.  It initializes the neovm-core Elisp evaluator,
//! creates the display/render thread, and runs a simple command loop:
//!
//!   key press → evaluator → buffer change → layout → render
//!
//! No C code is involved.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crossbeam_channel::TryRecvError;

use neomacs_display::thread_comm::ThreadComms;
use neomacs_display::render_thread::{RenderThread, SharedImageDimensions, SharedMonitorInfo};
use neomacs_display::thread_comm::InputEvent;
use neomacs_display::FrameGlyphBuffer;

use neovm_core::elisp::Evaluator;
use neovm_core::elisp::Value;
use neovm_core::buffer::BufferId;
use neovm_core::window::Window;

// Modifier bitmask constants (must match neomacs_display.h / thread_comm.rs)
const SHIFT_MASK: u32 = 1 << 0;
const CTRL_MASK: u32 = 1 << 1;
const META_MASK: u32 = 1 << 2;
#[allow(dead_code)]
const SUPER_MASK: u32 = 1 << 3;

// X11 keysym constants for special keys
const XK_RETURN: u32 = 0xFF0D;
const XK_TAB: u32 = 0xFF09;
const XK_BACKSPACE: u32 = 0xFF08;
const XK_DELETE: u32 = 0xFFFF;
const XK_ESCAPE: u32 = 0xFF1B;
const XK_LEFT: u32 = 0xFF51;
const XK_UP: u32 = 0xFF52;
const XK_RIGHT: u32 = 0xFF53;
const XK_DOWN: u32 = 0xFF54;
const XK_HOME: u32 = 0xFF50;
const XK_END: u32 = 0xFF57;

fn main() {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .init();

    log::info!("Neomacs {} starting (pure Rust, backend={})",
        neomacs_display::VERSION,
        neomacs_display::CORE_BACKEND);

    // 1. Initialize the evaluator
    let mut evaluator = Evaluator::new();
    evaluator.setup_thread_locals();
    log::info!("Evaluator initialized");

    // 2. Parse command-line arguments
    let args = parse_args();

    // 3. Bootstrap: create *scratch*, *Messages*, *Minibuf-0* buffers
    let width: u32 = 960;
    let height: u32 = 640;
    let scratch_id = bootstrap_buffers(&mut evaluator, width, height);
    log::info!("Bootstrap complete: *scratch* buffer={:?}", scratch_id);

    // 4. Load Elisp files specified on the command line
    for load_item in &args.load {
        match load_item {
            LoadItem::File(path) => {
                log::info!("Loading Elisp file: {}", path.display());
                evaluator.setup_thread_locals();
                match neovm_core::elisp::load::load_file(&mut evaluator, path) {
                    Ok(_) => log::info!("  Loaded: {}", path.display()),
                    Err(e) => log::error!("  Error loading {}: {:?}", path.display(), e),
                }
            }
            LoadItem::Eval(expr) => {
                log::info!("Evaluating: {}", expr);
                evaluator.setup_thread_locals();
                match neovm_core::elisp::parse_forms(expr) {
                    Ok(forms) => {
                        for form in &forms {
                            match evaluator.eval_expr(form) {
                                Ok(val) => log::info!("  => {:?}", val),
                                Err(e) => log::error!("  Error: {:?}", e),
                            }
                        }
                    }
                    Err(e) => log::error!("  Parse error: {}", e),
                }
            }
        }
    }

    // Open files specified on the command line
    for file_path in &args.files {
        open_file(&mut evaluator, file_path, scratch_id);
    }

    // 5. Create communication channels
    let comms = ThreadComms::new().expect("Failed to create thread comms");
    let (emacs_comms, render_comms) = comms.split();

    // 4. Create shared state
    let image_dimensions: SharedImageDimensions = Arc::new(Mutex::new(HashMap::new()));
    let shared_monitors: SharedMonitorInfo = Arc::new((
        Mutex::new(Vec::new()),
        std::sync::Condvar::new(),
    ));

    // 5. Spawn render thread
    let render_thread = RenderThread::spawn(
        render_comms,
        width,
        height,
        "Neomacs".to_string(),
        Arc::clone(&image_dimensions),
        Arc::clone(&shared_monitors),
        #[cfg(feature = "neo-term")]
        Arc::new(Mutex::new(HashMap::new())),
    );
    log::info!("Render thread spawned ({}x{})", width, height);

    // 6. Run initial layout and send first frame
    let frame_id = evaluator.frame_manager().selected_frame()
        .expect("No selected frame after bootstrap").id;

    let mut frame_glyphs = FrameGlyphBuffer::with_size(width as f32, height as f32);
    run_layout(&mut evaluator, frame_id, &mut frame_glyphs);
    let _ = emacs_comms.frame_tx.try_send(frame_glyphs.clone());
    log::info!("Initial frame sent ({} glyphs)", frame_glyphs.glyphs.len());

    // 7. Main event loop
    let wakeup_fd = emacs_comms.wakeup_read_fd;
    let mut running = true;

    while running {
        // Wait for events using poll() on the wakeup fd
        wait_for_wakeup(wakeup_fd);

        // Clear wakeup pipe
        emacs_comms.wakeup_clear.clear();

        // Drain all pending input events
        let mut need_redisplay = false;
        loop {
            match emacs_comms.input_rx.try_recv() {
                Ok(event) => {
                    match event {
                        InputEvent::Key { keysym, modifiers, pressed } => {
                            if pressed {
                                let handled = handle_key(&mut evaluator, keysym, modifiers);
                                if handled {
                                    need_redisplay = true;
                                }
                            }
                        }
                        InputEvent::WindowClose { .. } => {
                            log::info!("Window close requested");
                            running = false;
                        }
                        InputEvent::WindowResize { width: w, height: h, .. } => {
                            log::info!("Window resized to {}x{}", w, h);
                            // Update frame size in evaluator
                            if let Some(frame) = evaluator.frame_manager_mut().selected_frame_mut() {
                                frame.width = w;
                                frame.height = h;
                            }
                            frame_glyphs = FrameGlyphBuffer::with_size(w as f32, h as f32);
                            need_redisplay = true;
                        }
                        _ => {
                            // Ignore other events for now (mouse, focus, etc.)
                        }
                    }
                }
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    log::error!("Render thread disconnected");
                    running = false;
                    break;
                }
            }
        }

        // Redisplay if anything changed
        if need_redisplay {
            evaluator.setup_thread_locals();
            run_layout(&mut evaluator, frame_id, &mut frame_glyphs);
            let _ = emacs_comms.frame_tx.try_send(frame_glyphs.clone());
        }
    }

    // Shutdown
    log::info!("Shutting down...");
    let _ = emacs_comms.cmd_tx.try_send(
        neomacs_display::thread_comm::RenderCommand::Shutdown,
    );
    render_thread.join();
    log::info!("Neomacs exited cleanly");
}

/// Create initial buffers and frame in the evaluator.
fn bootstrap_buffers(eval: &mut Evaluator, width: u32, height: u32) -> BufferId {
    // Create *scratch* buffer with initial content
    let scratch_id = eval.buffer_manager_mut().create_buffer("*scratch*");
    if let Some(buf) = eval.buffer_manager_mut().get_mut(scratch_id) {
        let content = ";; This buffer is for text that is not saved, and for Lisp evaluation.\n\
                       ;; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n";
        buf.text.insert_str(0, content);
        let cc = buf.text.char_count();
        buf.begv = 0;
        buf.zv = cc;
        buf.pt = cc;
    }

    // Set *scratch* as the current buffer so self-insert-command works
    eval.buffer_manager_mut().set_current(scratch_id);

    // Create *Messages* buffer
    let msg_id = eval.buffer_manager_mut().create_buffer("*Messages*");
    if let Some(buf) = eval.buffer_manager_mut().get_mut(msg_id) {
        buf.begv = 0;
        buf.zv = 0;
        buf.pt = 0;
    }

    // Create *Minibuf-0*
    let mini_id = eval.buffer_manager_mut().create_buffer(" *Minibuf-0*");
    if let Some(buf) = eval.buffer_manager_mut().get_mut(mini_id) {
        buf.begv = 0;
        buf.zv = 0;
        buf.pt = 0;
    }

    // Create frame with *scratch* as the displayed buffer
    let frame_id = eval.frame_manager_mut().create_frame("F1", width, height, scratch_id);
    log::info!("Created frame {:?} ({}x{}) with *scratch*={:?}", frame_id, width, height, scratch_id);

    // Set window positions (0-based for neovm-core)
    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
        if let Window::Leaf { window_start, point, .. } = &mut frame.root_window {
            *window_start = 0;
            *point = 0;
        }
    }

    scratch_id
}

/// Run the layout engine on the current frame state.
fn run_layout(
    evaluator: &mut Evaluator,
    frame_id: neovm_core::window::FrameId,
    frame_glyphs: &mut FrameGlyphBuffer,
) {
    use neomacs_display::layout::LayoutEngine;

    // Use a thread-local layout engine
    thread_local! {
        static ENGINE: std::cell::RefCell<LayoutEngine> = std::cell::RefCell::new(LayoutEngine::new());
    }

    ENGINE.with(|engine| {
        engine.borrow_mut().layout_frame_rust(evaluator, frame_id, frame_glyphs);
    });
}

/// Handle a key event, returning true if the buffer changed (needs redisplay).
fn handle_key(eval: &mut Evaluator, keysym: u32, modifiers: u32) -> bool {
    eval.setup_thread_locals();

    // Determine the command to execute
    let command = match (keysym, modifiers) {
        // Printable ASCII without modifiers (or shift-only for uppercase)
        (32..=126, mods) if (mods & !SHIFT_MASK) == 0 => {
            eval.set_variable(
                "last-command-event",
                Value::Int(keysym as i64),
            );
            "(self-insert-command 1)"
        }

        // Return → newline
        (XK_RETURN, 0) => "(newline)",

        // Tab
        (XK_TAB, 0) => {
            eval.set_variable("last-command-event", Value::Int(9));
            "(self-insert-command 1)"
        }

        // Backspace → delete-backward-char
        (XK_BACKSPACE, 0) => "(delete-backward-char 1)",

        // Delete → delete-char
        (XK_DELETE, 0) => "(delete-char 1)",

        // Arrow keys
        (XK_LEFT, 0) => "(backward-char 1)",
        (XK_RIGHT, 0) => "(forward-char 1)",
        (XK_UP, 0) => "(previous-line 1)",
        (XK_DOWN, 0) => "(next-line 1)",

        // Home/End
        (XK_HOME, 0) => "(beginning-of-line)",
        (XK_END, 0) => "(end-of-line)",

        // C-a through C-z
        (key, mods) if (mods & CTRL_MASK) != 0
            && (mods & !CTRL_MASK & !SHIFT_MASK) == 0
            && (0x61..=0x7A).contains(&key) =>
        {
            match (key as u8) as char {
                'a' => "(beginning-of-line)",
                'b' => "(backward-char 1)",
                'd' => "(delete-char 1)",
                'e' => "(end-of-line)",
                'f' => "(forward-char 1)",
                'k' => "(kill-line)",
                'n' => "(next-line 1)",
                'p' => "(previous-line 1)",
                'y' => "(yank)",
                '/' => "(undo)",
                _ => {
                    log::debug!("Unhandled C-{}", (key as u8) as char);
                    return false;
                }
            }
        }

        // M-key (meta + letter)
        (key, mods) if (mods & META_MASK) != 0
            && (mods & !META_MASK) == 0
            && (0x61..=0x7A).contains(&key) =>
        {
            match (key as u8) as char {
                'f' => "(forward-word 1)",
                'b' => "(backward-word 1)",
                'd' => "(kill-word 1)",
                '<' => "(beginning-of-buffer)",
                '>' => "(end-of-buffer)",
                _ => {
                    log::debug!("Unhandled M-{}", (key as u8) as char);
                    return false;
                }
            }
        }

        // Unicode characters (non-ASCII, no modifiers) → self-insert
        (key, 0) if key >= 0x100 && key < 0xFF00 => {
            eval.set_variable("last-command-event", Value::Int(keysym as i64));
            "(self-insert-command 1)"
        }

        // Escape — ignore
        (XK_ESCAPE, 0) => return false,

        _ => {
            log::debug!("Unhandled keysym=0x{:X} mods=0x{:X}", keysym, modifiers);
            return false;
        }
    };

    // Execute the command via the evaluator
    match neovm_core::elisp::parse_forms(command) {
        Ok(forms) => {
            for form in &forms {
                if let Err(e) = eval.eval_expr(form) {
                    log::debug!("Command '{}' error: {:?}", command, e);
                    return false;
                }
            }
            true
        }
        Err(e) => {
            log::error!("Parse error for '{}': {}", command, e);
            false
        }
    }
}

/// Wait for the wakeup fd to become readable (blocking poll).
fn wait_for_wakeup(fd: std::os::unix::io::RawFd) {
    let mut pollfd = libc::pollfd {
        fd,
        events: libc::POLLIN,
        revents: 0,
    };

    // Poll with 16ms timeout (60fps) to allow periodic work
    unsafe {
        libc::poll(&mut pollfd as *mut _, 1, 16);
    }
}

// ===== CLI argument parsing =====

/// Items to load at startup.
enum LoadItem {
    /// Load an Elisp file.
    File(PathBuf),
    /// Evaluate an Elisp expression.
    Eval(String),
}

/// Parsed command-line arguments.
struct Args {
    /// Elisp files/expressions to load (in order).
    load: Vec<LoadItem>,
    /// Files to open in buffers.
    files: Vec<PathBuf>,
}

/// Parse command-line arguments.
///
/// Supported flags:
///   --load FILE / -l FILE   Load an Elisp file
///   --eval EXPR / -e EXPR   Evaluate an Elisp expression
///   FILE                    Open a file in a buffer
fn parse_args() -> Args {
    let mut args = Args {
        load: Vec::new(),
        files: Vec::new(),
    };

    let mut iter = std::env::args().skip(1); // skip program name
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--load" | "-l" => {
                if let Some(path) = iter.next() {
                    args.load.push(LoadItem::File(PathBuf::from(path)));
                } else {
                    log::error!("{} requires a file path argument", arg);
                }
            }
            "--eval" | "-e" => {
                if let Some(expr) = iter.next() {
                    args.load.push(LoadItem::Eval(expr));
                } else {
                    log::error!("{} requires an expression argument", arg);
                }
            }
            "--" => {
                // Everything after -- is a file to open
                for remaining in iter.by_ref() {
                    args.files.push(PathBuf::from(remaining));
                }
                break;
            }
            _ if arg.starts_with('-') => {
                log::warn!("Unknown option: {}", arg);
            }
            _ => {
                // Positional argument: file to open
                args.files.push(PathBuf::from(arg));
            }
        }
    }

    args
}

/// Open a file into a new buffer and switch to it.
fn open_file(eval: &mut Evaluator, path: &PathBuf, _scratch_id: BufferId) {
    let content = match std::fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            log::error!("Cannot open {}: {}", path.display(), e);
            return;
        }
    };

    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| path.display().to_string());

    let buf_id = eval.buffer_manager_mut().create_buffer(&name);
    if let Some(buf) = eval.buffer_manager_mut().get_mut(buf_id) {
        buf.text.insert_str(0, &content);
        let cc = buf.text.char_count();
        buf.begv = 0;
        buf.zv = cc;
        buf.pt = 0; // start at beginning
        buf.file_name = Some(path.to_string_lossy().to_string());
    }

    eval.buffer_manager_mut().set_current(buf_id);

    // Update the selected frame's root window to show this buffer
    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
        if let Window::Leaf { buffer_id, window_start, point, .. } = &mut frame.root_window {
            *buffer_id = buf_id;
            *window_start = 0;
            *point = 0;
        }
    }

    log::info!("Opened file: {} ({} chars)", path.display(), content.len());
}
