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
use neovm_core::window::{SplitDirection, Window, WindowId};

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
const XK_PAGE_UP: u32 = 0xFF55;
const XK_PAGE_DOWN: u32 = 0xFF56;

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
    // %* = modified indicator, %b = buffer name, %l = line, %c = column
    evaluator.set_variable("mode-line-format",
        Value::string(" %*%+ %b   L%l C%c   %f "));

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
                        InputEvent::MouseButton { button, x, y, pressed, .. } => {
                            if pressed && button == 1 {
                                // Left click: set point in the clicked window
                                handle_mouse_click(&mut evaluator, x, y);
                                need_redisplay = true;
                            }
                        }
                        InputEvent::MouseScroll { delta_y, x, y, .. } => {
                            handle_mouse_scroll(&mut evaluator, delta_y, x, y);
                            need_redisplay = true;
                        }
                        _ => {
                            // Ignore other events (focus, mouse move, etc.)
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
    MetaG,
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
    /// M-g g: goto-line
    GotoLine,
    /// M-%: replace string (first prompt — search string)
    ReplaceFrom,
    /// M-%: replace string (second prompt — replacement string)
    ReplaceTo { from: String },
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
    if let PrefixState::MetaG = prefix {
        *prefix = PrefixState::None;
        return handle_mg_key(eval, keysym, modifiers, minibuf);
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

        // PageUp/PageDown
        (XK_PAGE_UP, 0) => {
            scroll_down(eval);
            return KeyResult::Handled;
        }
        (XK_PAGE_DOWN, 0) => {
            scroll_up(eval);
            return KeyResult::Handled;
        }

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
                'h' => {
                    show_help(eval);
                    return KeyResult::Handled;
                }
                'l' => "(recenter)",
                't' => {
                    transpose_chars(eval);
                    return KeyResult::Handled;
                }
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

        // M-< (keysym 0x3c '<'): beginning-of-buffer
        (0x3c, mods) if (mods & META_MASK) != 0 => {
            return exec_command(eval, "(beginning-of-buffer)");
        }

        // M-> (keysym 0x3e '>'): end-of-buffer
        (0x3e, mods) if (mods & META_MASK) != 0 => {
            return exec_command(eval, "(end-of-buffer)");
        }

        // M-% (keysym 0x25 '%'): replace-string
        (0x25, mods) if (mods & META_MASK) != 0 => {
            activate_minibuffer(eval, minibuf, "Replace: ", MinibufferAction::ReplaceFrom);
            return KeyResult::Handled;
        }

        // M-key (meta + letter)
        (key, mods) if (mods & META_MASK) != 0
            && (mods & !META_MASK & !SHIFT_MASK) == 0
            && (0x61..=0x7A).contains(&key) =>
        {
            match (key as u8) as char {
                'f' => "(forward-word 1)",
                'b' => "(backward-word 1)",
                'd' => "(kill-word 1)",
                'c' => {
                    capitalize_word(eval);
                    return KeyResult::Handled;
                }
                'g' => {
                    *prefix = PrefixState::MetaG;
                    return KeyResult::Ignored;
                }
                'l' => {
                    case_word(eval, false);
                    return KeyResult::Handled;
                }
                'u' => {
                    case_word(eval, true);
                    return KeyResult::Handled;
                }
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
    let key_char = if (0x61..=0x7A).contains(&keysym) || (0x30..=0x39).contains(&keysym) {
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
        // C-x C-b → list-buffers
        (Some('b'), true) => {
            list_buffers(eval);
            KeyResult::Handled
        }
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
        // C-x C-e → eval-last-sexp
        (Some('e'), true) => {
            eval_last_sexp(eval);
            KeyResult::Handled
        }
        // C-x o → other-window (cycle between windows)
        (Some('o'), false) => {
            cycle_window(eval);
            KeyResult::Handled
        }
        // C-x 0/1/2/3 → window commands
        (Some(c), false) if c.is_ascii_digit() => {
            match c {
                '0' => {
                    delete_window(eval);
                    KeyResult::Handled
                }
                '1' => {
                    delete_other_windows(eval);
                    KeyResult::Handled
                }
                '2' => {
                    split_window_below(eval);
                    KeyResult::Handled
                }
                '3' => {
                    split_window_right(eval);
                    KeyResult::Handled
                }
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
                    // Update the selected window (not root — handles split windows)
                    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
                        let wid = frame.selected_window;
                        if let Some(w) = frame.find_window_mut(wid) {
                            if let Window::Leaf { buffer_id, window_start, point, .. } = w {
                                *buffer_id = buf_id;
                                *window_start = 0;
                                *point = 0;
                            }
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
            MinibufferAction::GotoLine => {
                if let Ok(line_num) = input.parse::<usize>() {
                    goto_line(eval, line_num);
                    log::info!("goto-line: {}", line_num);
                } else {
                    log::warn!("goto-line: invalid number '{}'", input);
                }
            }
            MinibufferAction::ReplaceFrom => {
                // First prompt done, now ask for replacement string
                let prompt = format!("Replace {} with: ", input);
                activate_minibuffer(
                    eval, minibuf, &prompt,
                    MinibufferAction::ReplaceTo { from: input },
                );
            }
            MinibufferAction::ReplaceTo { from } => {
                replace_string(eval, &from, &input);
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

    // Tab: completion
    if keysym == XK_TAB && modifiers == 0 {
        if let Some(completed) = try_complete(eval, minibuf) {
            minibuf.input = completed;
            update_minibuffer_display(eval, minibuf);
        }
        return KeyResult::Handled;
    }

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

/// Handle keys after M-g prefix.
fn handle_mg_key(
    eval: &mut Evaluator,
    keysym: u32,
    _modifiers: u32,
    minibuf: &mut MinibufferState,
) -> KeyResult {
    match keysym {
        // g or M-g: goto-line
        0x67 => {
            activate_minibuffer(eval, minibuf, "Goto line: ", MinibufferAction::GotoLine);
            KeyResult::Handled
        }
        _ => {
            log::debug!("Unhandled M-g 0x{:X}", keysym);
            KeyResult::Ignored
        }
    }
}

/// Go to a specific line number (1-based).
fn goto_line(eval: &mut Evaluator, line: usize) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let target = if line == 0 { 1 } else { line };
    let mut current_line = 1;
    let mut pos = 0;
    for (i, ch) in text.char_indices() {
        if current_line == target {
            pos = i;
            break;
        }
        if ch == '\n' {
            current_line += 1;
            if current_line == target {
                pos = i + 1;
                break;
            }
        }
    }
    // If target line is beyond end, go to end of buffer
    if current_line < target {
        pos = text.len();
    }
    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        buf.pt = pos;
    }
}

/// Replace all occurrences of `from` with `to` in the current buffer.
fn replace_string(eval: &mut Evaluator, from: &str, to: &str) {
    if from.is_empty() {
        return;
    }
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let count = text.matches(from).count();
    if count == 0 {
        log::info!("replace-string: no occurrences of '{}'", from);
        return;
    }
    let new_text = text.replace(from, to);
    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        let len = buf.text.len();
        buf.text.delete_range(0, len);
        buf.text.insert_str(0, &new_text);
        let cc = buf.text.char_count();
        buf.zv = cc;
        buf.pt = buf.pt.min(buf.text.len());
    }
    log::info!("replace-string: replaced {} occurrences of '{}' with '{}'", count, from, to);
}

/// Show a list of all buffers in a *Buffer List* buffer (C-x C-b).
fn list_buffers(eval: &mut Evaluator) {
    let buf_list = eval.buffer_manager().buffer_list();
    let cur_id = eval.buffer_manager().current_buffer().map(|b| b.id);
    let mut content = String::new();
    content.push_str("  MR Buffer             Size    File\n");
    content.push_str("  -- ------             ----    ----\n");
    for id in &buf_list {
        if let Some(buf) = eval.buffer_manager().get(*id) {
            if buf.name.starts_with(' ') {
                continue; // skip hidden buffers
            }
            let marker = if cur_id == Some(*id) { '.' } else { ' ' };
            let mod_marker = if buf.modified { '*' } else { ' ' };
            let file = buf.file_name.as_deref().unwrap_or("");
            content.push_str(&format!(
                "  {}{} {:<18} {:>6}    {}\n",
                marker, mod_marker, buf.name, buf.text.len(), file
            ));
        }
    }

    let list_id = eval
        .buffer_manager()
        .find_buffer_by_name("*Buffer List*")
        .unwrap_or_else(|| eval.buffer_manager_mut().create_buffer("*Buffer List*"));
    if let Some(buf) = eval.buffer_manager_mut().get_mut(list_id) {
        let len = buf.text.len();
        if len > 0 {
            buf.text.delete_range(0, len);
        }
        buf.text.insert_str(0, &content);
        let cc = buf.text.char_count();
        buf.begv = 0;
        buf.zv = cc;
        buf.pt = 0;
    }
    eval.buffer_manager_mut().set_current(list_id);
    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
        if let Window::Leaf {
            buffer_id,
            window_start,
            point,
            ..
        } = &mut frame.root_window
        {
            *buffer_id = list_id;
            *window_start = 0;
            *point = 0;
        }
    }
}

/// Transpose the two characters before point (C-t).
fn transpose_chars(eval: &mut Evaluator) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let pt = buf.pt;
    // Need at least 2 chars and point > 0
    if text.len() < 2 || pt == 0 {
        return;
    }
    // Get the two characters around point
    let (a_start, a_char, b_start, b_char) = if pt >= text.len() {
        // At end of buffer: swap last two chars
        let mut chars = text.char_indices().rev();
        let (bi, bc) = chars.next().unwrap();
        let (ai, ac) = chars.next().unwrap();
        (ai, ac, bi, bc)
    } else {
        // Swap char before point and char at point
        let before = text[..pt].char_indices().last();
        let at = text[pt..].chars().next();
        match (before, at) {
            (Some((ai, ac)), Some(bc)) => (ai, ac, pt, bc),
            _ => return,
        }
    };
    // Reconstruct with swapped chars
    let mut new_text = String::with_capacity(text.len());
    new_text.push_str(&text[..a_start]);
    new_text.push(b_char);
    let mid_start = a_start + a_char.len_utf8();
    let mid_end = b_start;
    if mid_end > mid_start {
        new_text.push_str(&text[mid_start..mid_end]);
    }
    new_text.push(a_char);
    let after = b_start + b_char.len_utf8();
    new_text.push_str(&text[after..]);

    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        let len = buf.text.len();
        buf.text.delete_range(0, len);
        buf.text.insert_str(0, &new_text);
        let cc = buf.text.char_count();
        buf.zv = cc;
        // Move point past the transposed pair
        buf.pt = (b_start + b_char.len_utf8()).min(buf.text.len());
    }
}

/// Change the case of the next word (M-u = uppercase, M-l = lowercase).
fn case_word(eval: &mut Evaluator, uppercase: bool) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let pt = buf.pt;
    // Skip non-alphanumeric chars to find word start
    let word_start = text[pt..].find(|c: char| c.is_alphanumeric())
        .map(|i| pt + i)
        .unwrap_or(text.len());
    // Find word end
    let word_end = text[word_start..].find(|c: char| !c.is_alphanumeric())
        .map(|i| word_start + i)
        .unwrap_or(text.len());
    if word_start >= word_end {
        return;
    }
    let word = &text[word_start..word_end];
    let new_word = if uppercase {
        word.to_uppercase()
    } else {
        word.to_lowercase()
    };
    let mut new_text = String::with_capacity(text.len());
    new_text.push_str(&text[..word_start]);
    new_text.push_str(&new_word);
    new_text.push_str(&text[word_end..]);

    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        let len = buf.text.len();
        buf.text.delete_range(0, len);
        buf.text.insert_str(0, &new_text);
        let cc = buf.text.char_count();
        buf.zv = cc;
        buf.pt = word_end;
    }
}

/// Capitalize the next word (M-c).
fn capitalize_word(eval: &mut Evaluator) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let pt = buf.pt;
    let word_start = text[pt..].find(|c: char| c.is_alphanumeric())
        .map(|i| pt + i)
        .unwrap_or(text.len());
    let word_end = text[word_start..].find(|c: char| !c.is_alphanumeric())
        .map(|i| word_start + i)
        .unwrap_or(text.len());
    if word_start >= word_end {
        return;
    }
    let word = &text[word_start..word_end];
    let mut new_word = String::with_capacity(word.len());
    let mut first = true;
    for c in word.chars() {
        if first {
            for uc in c.to_uppercase() {
                new_word.push(uc);
            }
            first = false;
        } else {
            for lc in c.to_lowercase() {
                new_word.push(lc);
            }
        }
    }
    let mut new_text = String::with_capacity(text.len());
    new_text.push_str(&text[..word_start]);
    new_text.push_str(&new_word);
    new_text.push_str(&text[word_end..]);

    if let Some(buf) = eval.buffer_manager_mut().current_buffer_mut() {
        let len = buf.text.len();
        buf.text.delete_range(0, len);
        buf.text.insert_str(0, &new_text);
        let cc = buf.text.char_count();
        buf.zv = cc;
        buf.pt = word_end;
    }
}

/// Evaluate the last Lisp expression before point (C-x C-e).
fn eval_last_sexp(eval: &mut Evaluator) {
    let buf = match eval.buffer_manager().current_buffer() {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let pt = buf.pt;
    // Find matching opening paren backward from point
    let before = &text[..pt];
    let sexp_start = find_sexp_start(before);
    if sexp_start >= pt {
        log::info!("eval-last-sexp: no sexp found before point");
        return;
    }
    let sexp = &text[sexp_start..pt];
    log::info!("eval-last-sexp: evaluating '{}'", sexp);
    eval.setup_thread_locals();
    match neovm_core::elisp::parse_forms(sexp) {
        Ok(forms) => {
            for form in &forms {
                match eval.eval_expr(form) {
                    Ok(val) => log::info!("  => {:?}", val),
                    Err(e) => log::error!("  Error: {:?}", e),
                }
            }
        }
        Err(e) => log::error!("eval-last-sexp parse error: {}", e),
    }
}

/// Find the start of a sexp ending at `end` by scanning backward for matching parens.
fn find_sexp_start(text: &str) -> usize {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return 0;
    }
    let end = bytes.len();
    // If the text ends with ')', find matching '('
    if bytes[end - 1] == b')' {
        let mut depth = 0i32;
        for i in (0..end).rev() {
            match bytes[i] {
                b')' => depth += 1,
                b'(' => {
                    depth -= 1;
                    if depth == 0 {
                        return i;
                    }
                }
                _ => {}
            }
        }
        return 0;
    }
    // Otherwise, find start of current atom (word/number/symbol)
    let mut i = end;
    while i > 0 {
        let c = bytes[i - 1];
        if c == b' ' || c == b'\n' || c == b'\t' || c == b'(' || c == b')' {
            break;
        }
        i -= 1;
    }
    i
}

/// Try tab-completion for the current minibuffer action.
fn try_complete(eval: &Evaluator, minibuf: &MinibufferState) -> Option<String> {
    match &minibuf.action {
        MinibufferAction::SwitchBuffer => {
            // Complete buffer names
            let prefix = &minibuf.input;
            let matches: Vec<String> = eval.buffer_manager().buffer_list().into_iter()
                .filter_map(|id| eval.buffer_manager().get(id))
                .filter(|b| !b.name.starts_with(' '))
                .filter(|b| b.name.starts_with(prefix))
                .map(|b| b.name.clone())
                .collect();
            if matches.len() == 1 {
                Some(matches[0].clone())
            } else if matches.len() > 1 {
                // Find common prefix
                let common = common_prefix(&matches);
                if common.len() > prefix.len() {
                    Some(common)
                } else {
                    log::info!("Completions: {}", matches.join(", "));
                    None
                }
            } else {
                None
            }
        }
        MinibufferAction::FindFile => {
            // Complete file paths
            let input = &minibuf.input;
            let path = if input.is_empty() {
                std::env::current_dir().unwrap_or_default()
            } else {
                let p = PathBuf::from(input);
                if p.is_absolute() { p } else {
                    std::env::current_dir().unwrap_or_default().join(p)
                }
            };
            // If input ends with '/', list directory contents
            // Otherwise, complete the last component
            let (dir, prefix) = if input.ends_with('/') || input.is_empty() {
                (path.clone(), String::new())
            } else {
                let parent = path.parent().unwrap_or(&path);
                let stem = path.file_name()
                    .map(|n| n.to_string_lossy().to_string())
                    .unwrap_or_default();
                (parent.to_path_buf(), stem)
            };
            let entries: Vec<String> = match std::fs::read_dir(&dir) {
                Ok(rd) => rd.filter_map(|e| e.ok())
                    .map(|e| {
                        let name = e.file_name().to_string_lossy().to_string();
                        if e.path().is_dir() {
                            format!("{}/", name)
                        } else {
                            name
                        }
                    })
                    .filter(|n| n.starts_with(&prefix))
                    .collect(),
                Err(_) => Vec::new(),
            };
            if entries.len() == 1 {
                // Replace just the last component
                let dir_str = dir.to_string_lossy();
                let completed = if dir_str.ends_with('/') {
                    format!("{}{}", dir_str, entries[0])
                } else {
                    format!("{}/{}", dir_str, entries[0])
                };
                Some(completed)
            } else if entries.len() > 1 {
                let common = common_prefix(&entries);
                if common.len() > prefix.len() {
                    let dir_str = dir.to_string_lossy();
                    let completed = if dir_str.ends_with('/') {
                        format!("{}{}", dir_str, common)
                    } else {
                        format!("{}/{}", dir_str, common)
                    };
                    Some(completed)
                } else {
                    log::info!("Completions: {}", entries.join(", "));
                    None
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Find the longest common prefix of a list of strings.
fn common_prefix(strings: &[String]) -> String {
    if strings.is_empty() {
        return String::new();
    }
    let first = &strings[0];
    let mut len = first.len();
    for s in &strings[1..] {
        len = len.min(s.len());
        for (i, (a, b)) in first.chars().zip(s.chars()).enumerate() {
            if a != b {
                len = len.min(i);
                break;
            }
        }
    }
    first[..len].to_string()
}

/// Split the selected window vertically (C-x 2).
fn split_window_below(eval: &mut Evaluator) {
    let frame_id = match eval.frame_manager().selected_frame() {
        Some(f) => f.id,
        None => return,
    };
    let selected = eval.frame_manager().selected_frame()
        .map(|f| f.selected_window)
        .unwrap_or(WindowId(0));
    let buf_id = eval.frame_manager().selected_frame()
        .and_then(|f| f.find_window(selected))
        .and_then(|w| w.buffer_id())
        .unwrap_or(neovm_core::buffer::BufferId(0));

    match eval.frame_manager_mut().split_window(
        frame_id, selected, SplitDirection::Vertical, buf_id,
    ) {
        Some(new_wid) => {
            log::info!("split-window-below: new window {:?}", new_wid);
        }
        None => {
            log::warn!("split-window-below: failed");
        }
    }
}

/// Split the selected window horizontally (C-x 3).
fn split_window_right(eval: &mut Evaluator) {
    let frame_id = match eval.frame_manager().selected_frame() {
        Some(f) => f.id,
        None => return,
    };
    let selected = eval.frame_manager().selected_frame()
        .map(|f| f.selected_window)
        .unwrap_or(WindowId(0));
    let buf_id = eval.frame_manager().selected_frame()
        .and_then(|f| f.find_window(selected))
        .and_then(|w| w.buffer_id())
        .unwrap_or(neovm_core::buffer::BufferId(0));

    match eval.frame_manager_mut().split_window(
        frame_id, selected, SplitDirection::Horizontal, buf_id,
    ) {
        Some(new_wid) => {
            log::info!("split-window-right: new window {:?}", new_wid);
        }
        None => {
            log::warn!("split-window-right: failed");
        }
    }
}

/// Delete the selected window (C-x 0).
fn delete_window(eval: &mut Evaluator) {
    let frame_id = match eval.frame_manager().selected_frame() {
        Some(f) => f.id,
        None => return,
    };
    let selected = eval.frame_manager().selected_frame()
        .map(|f| f.selected_window)
        .unwrap_or(WindowId(0));
    if eval.frame_manager_mut().delete_window(frame_id, selected) {
        // Update current buffer to match newly selected window
        if let Some(frame) = eval.frame_manager().selected_frame() {
            if let Some(w) = frame.find_window(frame.selected_window) {
                if let Some(bid) = w.buffer_id() {
                    eval.buffer_manager_mut().set_current(bid);
                }
            }
        }
        log::info!("delete-window: deleted {:?}", selected);
    } else {
        log::info!("delete-window: cannot delete sole window");
    }
}

/// Delete all other windows (C-x 1).
fn delete_other_windows(eval: &mut Evaluator) {
    let frame_id = match eval.frame_manager().selected_frame() {
        Some(f) => f.id,
        None => return,
    };
    let selected = eval.frame_manager().selected_frame()
        .map(|f| f.selected_window)
        .unwrap_or(WindowId(0));
    // Delete all windows except the selected one
    loop {
        let leaves = eval.frame_manager().selected_frame()
            .map(|f| f.root_window.leaf_ids())
            .unwrap_or_default();
        let to_delete: Vec<_> = leaves.into_iter()
            .filter(|&id| id != selected)
            .collect();
        if to_delete.is_empty() {
            break;
        }
        for wid in to_delete {
            eval.frame_manager_mut().delete_window(frame_id, wid);
        }
    }
    log::info!("delete-other-windows: keeping {:?}", selected);
}

/// Cycle to the next window (C-x o).
fn cycle_window(eval: &mut Evaluator) {
    let frame = match eval.frame_manager().selected_frame() {
        Some(f) => f,
        None => return,
    };
    let leaves = frame.root_window.leaf_ids();
    if leaves.len() <= 1 {
        return;
    }
    let current = frame.selected_window;
    let idx = leaves.iter().position(|&id| id == current).unwrap_or(0);
    let next_idx = (idx + 1) % leaves.len();
    let next_wid = leaves[next_idx];

    let frame_id = frame.id;
    // Update selected window
    if let Some(frame) = eval.frame_manager_mut().get_mut(frame_id) {
        frame.selected_window = next_wid;
    }
    // Update current buffer to match
    if let Some(frame) = eval.frame_manager().selected_frame() {
        if let Some(w) = frame.find_window(next_wid) {
            if let Some(bid) = w.buffer_id() {
                eval.buffer_manager_mut().set_current(bid);
            }
        }
    }
    log::info!("other-window: switched to {:?}", next_wid);
}

/// Show a help buffer with keybinding summary (C-h).
fn show_help(eval: &mut Evaluator) {
    let content = "\
Neomacs Keybindings
===================

Movement
--------
  C-f / Right      Forward char         C-b / Left       Backward char
  C-n / Down       Next line            C-p / Up         Previous line
  C-a / Home       Beginning of line    C-e / End        End of line
  M-f              Forward word         M-b              Backward word
  C-v / PgDn       Scroll down          M-v / PgUp       Scroll up
  M-<              Beginning of buffer  M->              End of buffer
  M-g g            Goto line

Editing
-------
  C-d / Delete     Delete char          Backspace        Delete backward
  C-k              Kill line            M-d              Kill word
  C-w              Kill region          M-w              Copy region
  C-y              Yank (paste)         C-/              Undo
  C-t              Transpose chars
  M-u              Uppercase word       M-l              Lowercase word
  M-c              Capitalize word

Search & Replace
----------------
  C-s              Incremental search   M-%              Replace string

Mark & Region
-------------
  C-SPC            Set mark             C-x h            Mark whole buffer

Files & Buffers
---------------
  C-x C-f          Find file            C-x C-s          Save buffer
  C-x b            Switch buffer        C-x C-b          List buffers
  C-x k            Kill buffer

Windows
-------
  C-x 2            Split below          C-x 3            Split right
  C-x 0            Delete window        C-x 1            Delete others
  C-x o            Cycle window
  Mouse click      Select window & position

Other
-----
  C-x C-e          Eval last sexp       M-x              Execute command
  C-g              Cancel / Keyboard quit
  C-h              This help            C-x C-c          Quit
";

    let help_id = eval
        .buffer_manager()
        .find_buffer_by_name("*Help*")
        .unwrap_or_else(|| eval.buffer_manager_mut().create_buffer("*Help*"));
    if let Some(buf) = eval.buffer_manager_mut().get_mut(help_id) {
        let len = buf.text.len();
        if len > 0 {
            buf.text.delete_range(0, len);
        }
        buf.text.insert_str(0, content);
        let cc = buf.text.char_count();
        buf.begv = 0;
        buf.zv = cc;
        buf.pt = 0;
    }
    eval.buffer_manager_mut().set_current(help_id);

    // Show in selected window
    if let Some(frame) = eval.frame_manager_mut().selected_frame_mut() {
        let wid = frame.selected_window;
        if let Some(w) = frame.find_window_mut(wid) {
            if let Window::Leaf { buffer_id, window_start, point, .. } = w {
                *buffer_id = help_id;
                *window_start = 0;
                *point = 0;
            }
        }
    }
    log::info!("Showing help buffer");
}

/// Handle a left mouse click at pixel coordinates (x, y).
/// Finds the clicked window, selects it, and sets point to the approximate position.
fn handle_mouse_click(eval: &mut Evaluator, x: f32, y: f32) {
    let frame = match eval.frame_manager().selected_frame() {
        Some(f) => f,
        None => return,
    };
    let char_w = frame.char_width;
    let char_h = frame.char_height;
    let frame_id = frame.id;

    // Check if click is in the minibuffer
    if let Some(mini_leaf) = &frame.minibuffer_leaf {
        if let Window::Leaf { bounds, .. } = mini_leaf {
            if x >= bounds.x && x < bounds.x + bounds.width
                && y >= bounds.y && y < bounds.y + bounds.height
            {
                // Click in minibuffer — ignore (minibuffer is keyboard-driven)
                return;
            }
        }
    }

    // Find which leaf window was clicked
    let leaves = frame.root_window.leaf_ids();
    let mut clicked_window = None;
    for wid in &leaves {
        if let Some(w) = frame.find_window(*wid) {
            if let Window::Leaf { bounds, .. } = w {
                if x >= bounds.x && x < bounds.x + bounds.width
                    && y >= bounds.y && y < bounds.y + bounds.height
                {
                    clicked_window = Some(*wid);
                    break;
                }
            }
        }
    }

    let clicked_wid = match clicked_window {
        Some(wid) => wid,
        None => return,
    };

    // Select the clicked window
    if let Some(frame) = eval.frame_manager_mut().get_mut(frame_id) {
        frame.selected_window = clicked_wid;
    }

    // Get window bounds and buffer info
    let (bounds, buf_id, window_start) = {
        let frame = match eval.frame_manager().selected_frame() {
            Some(f) => f,
            None => return,
        };
        match frame.find_window(clicked_wid) {
            Some(Window::Leaf { bounds, buffer_id, window_start, .. }) => {
                (*bounds, *buffer_id, *window_start)
            }
            _ => return,
        }
    };

    // Update current buffer to match selected window
    eval.buffer_manager_mut().set_current(buf_id);

    // Compute row/col from click position
    // Account for mode-line at the bottom (1 line of char_h)
    let text_y = bounds.y;
    let row = ((y - text_y) / char_h).floor() as usize;
    let col = ((x - bounds.x) / char_w).floor() as usize;

    // Walk through buffer text from window_start to find the target position
    let buf = match eval.buffer_manager().get(buf_id) {
        Some(b) => b,
        None => return,
    };
    let text = buf.text.to_string();
    let start = window_start.min(text.len());
    let mut current_row = 0;
    let mut current_col = 0;
    let mut target_pos = start;

    for (i, ch) in text[start..].char_indices() {
        if current_row == row && current_col == col {
            target_pos = start + i;
            break;
        }
        if current_row > row {
            break;
        }
        if ch == '\n' {
            if current_row == row {
                // Click past end of line — set point at end of this line
                target_pos = start + i;
                break;
            }
            current_row += 1;
            current_col = 0;
        } else {
            if current_row == row {
                current_col += 1;
            } else {
                current_col += 1;
            }
        }
        // If we reach end of text
        if start + i + ch.len_utf8() >= text.len() {
            target_pos = text.len();
        }
    }

    // Clamp to row — if target row not found, point stays at start
    if current_row < row {
        target_pos = text.len();
    }

    if let Some(buf) = eval.buffer_manager_mut().get_mut(buf_id) {
        buf.pt = target_pos.min(buf.text.len());
    }
    log::info!("Mouse click at ({:.0},{:.0}) → row={}, col={}, pos={}", x, y, row, col, target_pos);
}

/// Handle mouse scroll wheel event.
fn handle_mouse_scroll(eval: &mut Evaluator, delta_y: f32, _x: f32, _y: f32) {
    let lines = if delta_y.abs() > 1.0 {
        // Discrete scroll (standard wheel): delta_y is typically 3.0 or -3.0
        delta_y.abs() as usize
    } else {
        // Fine/pixel-precise scroll
        3
    };

    if delta_y < 0.0 {
        // Scroll up (show earlier content)
        for _ in 0..lines {
            exec_command(eval, "(previous-line 1)");
        }
    } else {
        // Scroll down (show later content)
        for _ in 0..lines {
            exec_command(eval, "(next-line 1)");
        }
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
