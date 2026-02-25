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
use neovm_core::window::{Window, WindowId};

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
    let bootstrap = bootstrap_buffers(&mut evaluator, width, height);
    let scratch_id = bootstrap.scratch_id;

    // Set a useful mode-line-format with %-constructs
    evaluator.set_variable("mode-line-format",
        Value::string(" %*%+ %b   %f "));

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

    // Add undo boundary after bootstrap so initial content isn't undoable
    if let Some(buf) = evaluator.buffer_manager_mut().current_buffer_mut() {
        buf.undo_list.boundary();
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
    let mut prefix_state = PrefixState::None;
    let mut minibuf = MinibufferState {
        active: false,
        prompt: String::new(),
        input: String::new(),
        action: MinibufferAction::FindFile,
        prev_selected: WindowId(0),
        minibuf_id: bootstrap.minibuf_id,
        search_origin: 0,
    };

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
                                if minibuf.active {
                                    // Keys go to the minibuffer handler
                                    match handle_minibuffer_key(
                                        &mut evaluator, keysym, modifiers,
                                        &mut minibuf, scratch_id,
                                    ) {
                                        KeyResult::Handled => need_redisplay = true,
                                        KeyResult::Quit => {
                                            // C-g cancels the minibuffer
                                            cancel_minibuffer(&mut evaluator, &mut minibuf);
                                            need_redisplay = true;
                                        }
                                        KeyResult::Ignored => {}
                                        KeyResult::Save => {}
                                    }
                                } else {
                                    match handle_key(
                                        &mut evaluator, keysym, modifiers,
                                        &mut prefix_state, &mut minibuf,
                                    ) {
                                        KeyResult::Handled => need_redisplay = true,
                                        KeyResult::Quit => {
                                            log::info!("C-x C-c: quit requested");
                                            running = false;
                                        }
                                        KeyResult::Save => {
                                            save_current_buffer(&evaluator);
                                            need_redisplay = true;
                                        }
                                        KeyResult::Ignored => {}
                                    }
                                }
                            }
                        }
                        InputEvent::WindowClose { .. } => {
                            log::info!("Window close requested");
                            running = false;
                        }
                        InputEvent::WindowResize { width: w, height: h, .. } => {
                            log::info!("Window resized to {}x{}", w, h);
                            let mini_h = 32.0_f32;
                            let mini_y = h as f32 - mini_h;
                            if let Some(frame) = evaluator.frame_manager_mut().selected_frame_mut() {
                                frame.width = w;
                                frame.height = h;
                                // Resize root window
                                if let Window::Leaf { ref mut bounds, .. } = &mut frame.root_window {
                                    bounds.width = w as f32;
                                    bounds.height = mini_y;
                                }
                                // Reposition minibuffer
                                if let Some(mini_leaf) = &mut frame.minibuffer_leaf {
                                    if let Window::Leaf { ref mut bounds, .. } = mini_leaf {
                                        bounds.y = mini_y;
                                        bounds.width = w as f32;
                                        bounds.height = mini_h;
                                    }
                                }
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
            // Add undo boundary after each command so undo can pop one group at a time
            if let Some(buf) = evaluator.buffer_manager_mut().current_buffer_mut() {
                buf.undo_list.boundary();
            }
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

/// Bootstrap result containing key buffer IDs.
struct BootstrapResult {
    scratch_id: BufferId,
    minibuf_id: BufferId,
}

/// Create initial buffers and frame in the evaluator.
fn bootstrap_buffers(eval: &mut Evaluator, width: u32, height: u32) -> BootstrapResult {
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

    // Fix window geometry: root window takes frame height minus minibuffer.
    // Give the minibuffer 2 lines (32px) so text is clearly visible.
    let mini_h = 32.0_f32;
    let mini_y = height as f32 - mini_h;
    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
        // Shrink root window to leave room for minibuffer
        if let Window::Leaf { ref mut bounds, .. } = &mut frame.root_window {
            bounds.height = mini_y;
        }
        // Point the minibuffer leaf to *Minibuf-0* and set correct position
        if let Some(mini_leaf) = &mut frame.minibuffer_leaf {
            if let Window::Leaf { buffer_id, window_start, point, ref mut bounds, .. } = mini_leaf {
                *buffer_id = mini_id;
                *window_start = 0;
                *point = 0;
                bounds.y = mini_y;
                bounds.height = mini_h;
                bounds.width = width as f32;
            }
        }
    }

    BootstrapResult { scratch_id, minibuf_id: mini_id }
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

/// Result of key handling.
enum KeyResult {
    /// Key was handled, buffer may have changed.
    Handled,
    /// Key was not handled / no change.
    Ignored,
    /// C-x C-c: quit the editor.
    Quit,
    /// C-x C-s: save the current buffer.
    Save,
}

/// Prefix key state machine.
enum PrefixState {
    None,
    CtrlX,
}

/// What action the minibuffer is being used for.
enum MinibufferAction {
    /// C-x C-f: find-file — open a file
    FindFile,
    /// C-x b: switch-to-buffer
    SwitchBuffer,
    /// C-s: incremental search forward
    SearchForward,
    /// M-x: execute command by name
    ExecuteCommand,
}

/// Minibuffer interaction state.
struct MinibufferState {
    /// Whether the minibuffer is active.
    active: bool,
    /// The prompt displayed to the user.
    prompt: String,
    /// The user's input so far.
    input: String,
    /// What action to take when Enter is pressed.
    action: MinibufferAction,
    /// The previously selected window, to restore on cancel.
    prev_selected: WindowId,
    /// The minibuffer buffer id.
    minibuf_id: BufferId,
    /// Saved point for incremental search (to reset on each keystroke).
    search_origin: usize,
}

/// Handle a key event, returning a `KeyResult`.
fn handle_key(
    eval: &mut Evaluator,
    keysym: u32,
    modifiers: u32,
    prefix: &mut PrefixState,
    minibuf: &mut MinibufferState,
) -> KeyResult {
    eval.setup_thread_locals();

    // Handle prefix key sequences first
    if let PrefixState::CtrlX = prefix {
        *prefix = PrefixState::None;
        return handle_cx_key(eval, keysym, modifiers, minibuf);
    }

    // Check for C-x prefix
    if keysym == 0x78 && (modifiers & CTRL_MASK) != 0
        && (modifiers & !CTRL_MASK & !SHIFT_MASK) == 0
    {
        *prefix = PrefixState::CtrlX;
        return KeyResult::Ignored;
    }

    // Determine the command to execute
    let command = match (keysym, modifiers) {
        // C-/ (slash = 0x2f): undo
        (0x2f, mods) if (mods & CTRL_MASK) != 0 => {
            undo(eval);
            return KeyResult::Handled;
        }

        // C-_ (underscore = 0x5f): also undo (Emacs convention)
        (0x5f, mods) if (mods & CTRL_MASK) != 0 => {
            undo(eval);
            return KeyResult::Handled;
        }

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

        // C-SPC: set mark
        (0x20, mods) if (mods & CTRL_MASK) != 0 => {
            set_mark_command(eval);
            return KeyResult::Handled;
        }

        // C-a through C-z (except C-x which was handled above)
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
                'g' => {
                    // C-g: deactivate mark
                    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
                        buf.mark = None;
                    }
                    return KeyResult::Ignored;
                }
                's' => {
                    activate_minibuffer(eval, minibuf, "I-search: ", MinibufferAction::SearchForward);
                    return KeyResult::Handled;
                }
                'k' => {
                    kill_line(eval);
                    return KeyResult::Handled;
                }
                'l' => "(recenter)",
                'n' => "(next-line 1)",
                'o' => "(open-line 1)",
                'p' => "(previous-line 1)",
                'v' => {
                    scroll_up(eval);
                    return KeyResult::Handled;
                }
                'w' => {
                    kill_region(eval);
                    return KeyResult::Handled;
                }
                'y' => {
                    yank(eval);
                    return KeyResult::Handled;
                }
                _ => {
                    log::debug!("Unhandled C-{}", (key as u8) as char);
                    return KeyResult::Ignored;
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
                'v' => {
                    scroll_down(eval);
                    return KeyResult::Handled;
                }
                'w' => {
                    copy_region_as_kill(eval);
                    return KeyResult::Handled;
                }
                'x' => {
                    activate_minibuffer(eval, minibuf, "M-x ", MinibufferAction::ExecuteCommand);
                    return KeyResult::Handled;
                }
                '<' => "(beginning-of-buffer)",
                '>' => "(end-of-buffer)",
                _ => {
                    log::debug!("Unhandled M-{}", (key as u8) as char);
                    return KeyResult::Ignored;
                }
            }
        }

        // Unicode characters (non-ASCII, no modifiers) → self-insert
        (key, 0) if key >= 0x100 && key < 0xFF00 => {
            eval.set_variable("last-command-event", Value::Int(keysym as i64));
            "(self-insert-command 1)"
        }

        // Escape — ignore
        (XK_ESCAPE, 0) => return KeyResult::Ignored,

        _ => {
            log::debug!("Unhandled keysym=0x{:X} mods=0x{:X}", keysym, modifiers);
            return KeyResult::Ignored;
        }
    };

    exec_command(eval, command)
}

/// Handle keys after C-x prefix.
fn handle_cx_key(
    eval: &mut Evaluator,
    keysym: u32,
    modifiers: u32,
    minibuf: &mut MinibufferState,
) -> KeyResult {
    let is_ctrl = (modifiers & CTRL_MASK) != 0;
    let key_char = if (0x61..=0x7A).contains(&keysym) {
        Some((keysym as u8) as char)
    } else {
        None
    };

    match (key_char, is_ctrl) {
        // C-x C-c → quit
        (Some('c'), true) => KeyResult::Quit,
        // C-x C-s → save
        (Some('s'), true) => KeyResult::Save,
        // C-x C-f → find-file (minibuffer prompt)
        (Some('f'), true) => {
            activate_minibuffer(eval, minibuf, "Find file: ", MinibufferAction::FindFile);
            KeyResult::Handled
        }
        // C-x C-w → write-file (save-as, treat same as save for now)
        (Some('w'), true) => KeyResult::Save,
        // C-x b → switch-to-buffer (minibuffer prompt)
        (Some('b'), false) => {
            activate_minibuffer(eval, minibuf, "Switch to buffer: ", MinibufferAction::SwitchBuffer);
            KeyResult::Handled
        }
        // C-x h → mark-whole-buffer
        (Some('h'), false) => exec_command(eval, "(mark-whole-buffer)"),
        // C-x u → undo
        (Some('u'), false) => exec_command(eval, "(undo)"),
        // C-x k → kill-buffer (switch to previous buffer or *scratch*)
        (Some('k'), false) => {
            kill_current_buffer(eval);
            KeyResult::Handled
        }
        // C-x o → other-window
        (Some('o'), false) => exec_command(eval, "(other-window 1)"),
        // C-x 1 → delete-other-windows
        (Some(c), false) if c.is_ascii_digit() => {
            match c {
                '0' => exec_command(eval, "(delete-window)"),
                '1' => exec_command(eval, "(delete-other-windows)"),
                '2' => exec_command(eval, "(split-window-below)"),
                '3' => exec_command(eval, "(split-window-right)"),
                _ => KeyResult::Ignored,
            }
        }
        _ => {
            log::debug!("Unhandled C-x {:?} ctrl={}", key_char, is_ctrl);
            KeyResult::Ignored
        }
    }
}

/// Execute an Elisp command string, returning Handled on success.
fn exec_command(eval: &mut Evaluator, command: &str) -> KeyResult {
    match neovm_core::elisp::parse_forms(command) {
        Ok(forms) => {
            for form in &forms {
                if let Err(e) = eval.eval_expr(form) {
                    log::debug!("Command '{}' error: {:?}", command, e);
                    return KeyResult::Ignored;
                }
            }
            KeyResult::Handled
        }
        Err(e) => {
            log::error!("Parse error for '{}': {}", command, e);
            KeyResult::Ignored
        }
    }
}

/// Save the current buffer to its associated file.
fn save_current_buffer(eval: &Evaluator) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => {
            log::warn!("No current buffer to save");
            return;
        }
    };

    let path = match &buf.file_name {
        Some(p) => p.clone(),
        None => {
            log::warn!("Buffer '{}' has no file name", buf.name);
            return;
        }
    };

    // Extract buffer text
    let text = buf.text.to_string();
    match std::fs::write(&path, &text) {
        Ok(()) => log::info!("Saved {} ({} bytes)", path, text.len()),
        Err(e) => log::error!("Error saving {}: {}", path, e),
    }
}

/// Activate the minibuffer with a prompt.
fn activate_minibuffer(
    eval: &mut Evaluator,
    minibuf: &mut MinibufferState,
    prompt: &str,
    action: MinibufferAction,
) {
    // Save the current selected window
    if let Some(frame) = eval.frame_manager().selected_frame() {
        minibuf.prev_selected = frame.selected_window;
    }

    minibuf.active = true;
    minibuf.prompt = prompt.to_string();
    minibuf.input.clear();
    minibuf.action = action;

    // Save point as search origin for incremental search
    minibuf.search_origin = eval.buffer_manager().current_buffer()
        .map(|b| b.pt).unwrap_or(0);

    // Update the minibuffer buffer to show the prompt
    update_minibuffer_display(eval, minibuf);

    log::info!("Minibuffer activated: {}", prompt);
}

/// Cancel the minibuffer (C-g).
fn cancel_minibuffer(eval: &mut Evaluator, minibuf: &mut MinibufferState) {
    minibuf.active = false;
    minibuf.input.clear();
    minibuf.prompt.clear();

    // Clear the minibuffer buffer
    if let Some(buf) = eval.buffer_manager_mut().get_mut(minibuf.minibuf_id) {
        let len = buf.text.len();
        if len > 0 { buf.text.delete_range(0, len); }
        buf.pt = 0;
        buf.begv = 0;
        buf.zv = 0;
    }

    log::info!("Minibuffer cancelled");
}

/// Update the minibuffer buffer to show prompt + input.
fn update_minibuffer_display(eval: &mut Evaluator, minibuf: &MinibufferState) {
    if let Some(buf) = eval.buffer_manager_mut().get_mut(minibuf.minibuf_id) {
        let len = buf.text.len();
        if len > 0 { buf.text.delete_range(0, len); }
        let display = format!("{}{}", minibuf.prompt, minibuf.input);
        buf.text.insert_str(0, &display);
        let cc = buf.text.char_count();
        buf.begv = 0;
        buf.zv = cc;
        buf.pt = cc; // cursor at end
    }
}

/// Handle a key press while the minibuffer is active.
fn handle_minibuffer_key(
    eval: &mut Evaluator,
    keysym: u32,
    modifiers: u32,
    minibuf: &mut MinibufferState,
    scratch_id: BufferId,
) -> KeyResult {
    // C-g: cancel
    if keysym == 0x67 && (modifiers & CTRL_MASK) != 0 {
        return KeyResult::Quit; // will trigger cancel_minibuffer
    }

    // Enter: submit
    if keysym == XK_RETURN && modifiers == 0 {
        let input = minibuf.input.clone();
        let action = std::mem::replace(&mut minibuf.action, MinibufferAction::FindFile);
        minibuf.active = false;
        minibuf.input.clear();
        minibuf.prompt.clear();

        // Clear the minibuffer display
        if let Some(buf) = eval.buffer_manager_mut().get_mut(minibuf.minibuf_id) {
            let len = buf.text.len();
        if len > 0 { buf.text.delete_range(0, len); }
            buf.pt = 0;
            buf.begv = 0;
            buf.zv = 0;
        }

        // Execute the action
        match action {
            MinibufferAction::FindFile => {
                let path = PathBuf::from(&input);
                let path = if path.is_absolute() {
                    path
                } else {
                    std::env::current_dir().unwrap_or_default().join(path)
                };
                if path.exists() {
                    open_file(eval, &path, scratch_id);
                    log::info!("find-file: opened {}", path.display());
                } else {
                    // Create new buffer for non-existent file
                    open_file_new(eval, &path);
                    log::info!("find-file: new file {}", path.display());
                }
            }
            MinibufferAction::SwitchBuffer => {
                // Find buffer by name
                if let Some(buf_id) = eval.buffer_manager().find_buffer_by_name(&input) {
                    eval.buffer_manager_mut().set_current(buf_id);
                    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
                        if let Window::Leaf { buffer_id, window_start, point, .. } = &mut frame.root_window {
                            *buffer_id = buf_id;
                            *window_start = 0;
                            *point = 0;
                        }
                    }
                    log::info!("switch-to-buffer: {}", input);
                } else {
                    log::warn!("No buffer named '{}'", input);
                }
            }
            MinibufferAction::SearchForward => {
                // On Enter, search from current point to find next occurrence
                let pt = eval.buffer_manager().current_buffer()
                    .map(|b| b.pt).unwrap_or(0);
                search_forward_from(eval, &input, pt);
            }
            MinibufferAction::ExecuteCommand => {
                // Try to evaluate (command-name) as Elisp
                let cmd = format!("({})", input);
                exec_command(eval, &cmd);
                log::info!("M-x {}", input);
            }
        }
        return KeyResult::Handled;
    }

    // Backspace: delete last char from input
    if keysym == XK_BACKSPACE && modifiers == 0 {
        if !minibuf.input.is_empty() {
            minibuf.input.pop();
            update_minibuffer_display(eval, minibuf);
        }
        return KeyResult::Handled;
    }

    // Printable ASCII: append to input
    if (32..=126).contains(&keysym) && (modifiers & !SHIFT_MASK) == 0 {
        let ch = keysym as u8 as char;
        minibuf.input.push(ch);
        update_minibuffer_display(eval, minibuf);
        // Incremental search: always search from origin point
        if matches!(minibuf.action, MinibufferAction::SearchForward) {
            search_forward_from(eval, &minibuf.input.clone(), minibuf.search_origin);
        }
        return KeyResult::Handled;
    }

    // C-a: go to beginning (move cursor but we keep input)
    // C-u: clear input
    if keysym == 0x75 && (modifiers & CTRL_MASK) != 0 {
        minibuf.input.clear();
        update_minibuffer_display(eval, minibuf);
        return KeyResult::Handled;
    }

    KeyResult::Ignored
}

/// Kill the current buffer and switch to the next available buffer.
fn kill_current_buffer(eval: &mut Evaluator) {
    let cur_id = match eval.buffer_manager().current_buffer().map(|b| b.id) {
        Some(id) => id,
        None => return,
    };

    let cur_name = eval.buffer_manager()
        .get(cur_id)
        .map(|b| b.name.clone())
        .unwrap_or_default();

    // Find another buffer to switch to (skip hidden buffers like *Minibuf-0*)
    let next_id = eval.buffer_manager().buffer_list().into_iter()
        .filter(|&id| id != cur_id)
        .filter(|&id| {
            eval.buffer_manager().get(id)
                .map(|b| !b.name.starts_with(' ')) // skip hidden buffers
                .unwrap_or(false)
        })
        .next();

    if let Some(next_id) = next_id {
        eval.buffer_manager_mut().set_current(next_id);
        // Update frame's root window
        if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
            if let Window::Leaf { buffer_id, window_start, point, .. } = &mut frame.root_window {
                *buffer_id = next_id;
                *window_start = 0;
                *point = 0;
            }
        }
        // Kill the old buffer
        eval.buffer_manager_mut().kill_buffer(cur_id);
        let next_name = eval.buffer_manager().get(next_id)
            .map(|b| b.name.as_str()).unwrap_or("?");
        log::info!("Killed buffer '{}', switched to '{}'", cur_name, next_name);
    } else {
        log::info!("Cannot kill '{}': no other buffer to switch to", cur_name);
    }
}

/// Search forward in the current buffer from a given starting position.
fn search_forward_from(eval: &mut Evaluator, query: &str, from: usize) {
    if query.is_empty() {
        return;
    }

    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };

    let text = buf.text.to_string();
    let start = from.min(text.len());

    // Search from start position forward
    if let Some(pos) = text[start..].find(query) {
        let new_pt = start + pos + query.len();
        if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
            buf.pt = new_pt;
        }
    } else if let Some(pos) = text[..start].find(query) {
        // Wrap around: search from beginning
        let new_pt = pos + query.len();
        if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
            buf.pt = new_pt;
        }
    }
}

/// Set mark at point (C-SPC).
fn set_mark_command(eval: &mut Evaluator) {
    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        let pt = buf.pt;
        buf.mark = Some(pt);
        log::info!("Mark set at {}", pt);
    }
}

/// Kill from point to end of line (C-k).
fn kill_line(eval: &mut Evaluator) {
    let (pt, line_end, text_at_pt) = {
        let buf = match eval.buffer_manager().current_buffer() {
            Some(b) => b,
            None => return,
        };
        let pt = buf.pt;
        let text = buf.text.to_string();
        // Find end of current line
        let remaining = &text[pt..];
        let newline_pos = remaining.find('\n');
        let line_end = match newline_pos {
            Some(0) => pt + 1, // At newline: kill just the newline
            Some(n) => pt + n, // Kill to end of line (not including newline)
            None => text.len(), // Last line: kill to end of buffer
        };
        let killed = text[pt..line_end].to_string();
        (pt, line_end, killed)
    };

    if pt < line_end {
        // Push killed text to kill ring
        eval.kill_ring_mut().push(text_at_pt);
        // Delete the region
        if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
            buf.delete_region(pt, line_end);
        }
    }
}

/// Kill the region between mark and point (C-w).
fn kill_region(eval: &mut Evaluator) {
    let (start, end, killed_text) = {
        let buf = match eval.buffer_manager().current_buffer() {
            Some(b) => b,
            None => return,
        };
        let pt = buf.pt;
        let mark = match buf.mark {
            Some(m) => m,
            None => {
                log::info!("kill-region: no mark set");
                return;
            }
        };
        let start = pt.min(mark);
        let end = pt.max(mark);
        let text = buf.text.to_string();
        let killed = text[start..end].to_string();
        (start, end, killed)
    };

    if start < end {
        eval.kill_ring_mut().push(killed_text);
        if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
            buf.delete_region(start, end);
            buf.mark = None;
        }
    }
}

/// Copy the region to the kill ring without deleting (M-w).
fn copy_region_as_kill(eval: &mut Evaluator) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let pt = buf.pt;
    let mark = match buf.mark {
        Some(m) => m,
        None => {
            log::info!("copy-region-as-kill: no mark set");
            return;
        }
    };
    let start = pt.min(mark);
    let end = pt.max(mark);
    let text = buf.text.to_string();
    let copied = text[start..end].to_string();

    if !copied.is_empty() {
        eval.kill_ring_mut().push(copied);
        log::info!("Copied {} chars to kill ring", end - start);
    }
}

/// Yank the most recent kill ring entry at point (C-y).
fn yank(eval: &mut Evaluator) {
    let text = match eval.kill_ring().current() {
        Some(t) => t.to_string(),
        None => {
            log::info!("yank: kill ring empty");
            return;
        }
    };

    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        let pt = buf.pt;
        buf.insert(&text);
        // Set mark at the beginning of yanked text
        buf.mark = Some(pt);
    }
}

/// Undo the last change (C-/).
fn undo(eval: &mut Evaluator) {
    let records = {
        let buf = match eval.buffer_manager_mut().current_buffer_mut() {
            Some(b) => b,
            None => return,
        };
        buf.undo_list.pop_undo_group()
    };

    if records.is_empty() {
        log::info!("undo: no more undo information");
        return;
    }

    // Apply undo records (they are returned in reverse order — most recent first)
    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        buf.undo_list.undoing = true;
        for record in &records {
            use neovm_core::buffer::undo::UndoRecord;
            match record {
                UndoRecord::Insert { pos, len } => {
                    // Undo an insert: delete [pos, pos+len)
                    let end = (*pos + *len).min(buf.text.len());
                    if *pos < end {
                        buf.text.delete_range(*pos, end);
                        let cc = buf.text.char_count();
                        buf.zv = cc;
                        buf.pt = (*pos).min(buf.text.len());
                    }
                }
                UndoRecord::Delete { pos, text } => {
                    // Undo a delete: re-insert text at pos
                    let insert_pos = (*pos).min(buf.text.len());
                    buf.text.insert_str(insert_pos, text);
                    let cc = buf.text.char_count();
                    buf.zv = cc;
                    buf.pt = insert_pos + text.len();
                }
                UndoRecord::CursorMove { pos } => {
                    buf.pt = (*pos).min(buf.text.len());
                }
                _ => {}
            }
        }
        buf.undo_list.undoing = false;
        log::info!("Undo: applied {} records, buffer now {} bytes", records.len(), buf.text.len());
    }
}

/// Scroll up (C-v): move point down by ~half the window height in lines.
fn scroll_up(eval: &mut Evaluator) {
    // Move forward roughly 20 lines (half-window heuristic)
    for _ in 0..20 {
        exec_command(eval, "(next-line 1)");
    }
}

/// Scroll down (M-v): move point up by ~half the window height in lines.
fn scroll_down(eval: &mut Evaluator) {
    for _ in 0..20 {
        exec_command(eval, "(previous-line 1)");
    }
}

/// Open a new (non-existent) file: create buffer with the name and file association.
fn open_file_new(eval: &mut Evaluator, path: &PathBuf) {
    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| path.display().to_string());

    let buf_id = eval.buffer_manager_mut().create_buffer(&name);
    if let Some(buf) = eval.buffer_manager_mut().get_mut(buf_id) {
        buf.begv = 0;
        buf.zv = 0;
        buf.pt = 0;
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
