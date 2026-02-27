# neovm-core Live Editor Integration — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace the C Emacs evaluator with neovm-core's Rust Evaluator as the sole Lisp execution engine in the live neomacs editor.

**Architecture:** The Rust `Evaluator` runs on the main thread, owns all Lisp state (buffers, windows, symbols, faces). The C command loop shell reads key events and delegates to Rust. The display engine reads buffer/face state directly from the Evaluator via Rust function calls. See `docs/plans/2026-02-25-neovm-core-integration-design.md` for full design.

**Tech Stack:** Rust (neovm-core, neomacs-display), C (keyboard.c, emacs.c, sysdep.c), cosmic-text (font metrics), crossbeam (input channel)

---

## Phase 1: Eval Bridge

**Goal:** Create the Rust Evaluator instance inside the live neomacs binary, load foundation `.el` files, and verify basic Lisp evaluation works.

### Task 1: Add eval_bridge module to neomacs-display FFI

**Files:**
- Create: `rust/neomacs-display/src/ffi/eval_bridge.rs`
- Modify: `rust/neomacs-display/src/ffi/mod.rs`
- Modify: `rust/neomacs-display/Cargo.toml` (add neovm-core dependency)

**Step 1: Add neovm-core dependency to neomacs-display**

In `rust/neomacs-display/Cargo.toml`, add:
```toml
[dependencies]
neovm-core = { path = "../neovm-core", features = ["core-backend-rust"] }
```

**Step 2: Create eval_bridge.rs with Evaluator singleton**

Create `rust/neomacs-display/src/ffi/eval_bridge.rs`:
```rust
//! FFI bridge: exposes neovm-core's Evaluator to C.
//!
//! The Evaluator is created once at startup and stored in a static.
//! All access is single-threaded (main thread only).

use std::ffi::{c_char, c_int, CStr, CString};

/// The global Evaluator instance. Only accessed from the main thread.
static mut RUST_EVALUATOR: Option<neovm_core::elisp::Evaluator> = None;

/// Initialize the Rust Evaluator. Called once from emacs.c main().
/// Returns 0 on success, -1 on failure.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_init() -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let eval = neovm_core::elisp::Evaluator::new();
        *std::ptr::addr_of_mut!(RUST_EVALUATOR) = Some(eval);
    }));
    match result {
        Ok(()) => 0,
        Err(e) => {
            log::error!("neomacs_rust_eval_init PANIC: {:?}", e);
            -1
        }
    }
}

/// Evaluate a Lisp expression given as a string.
/// Returns a newly allocated C string with the printed result,
/// or NULL on error. Caller must free the returned string with
/// neomacs_rust_free_string().
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_string(
    input: *const c_char,
) -> *mut c_char {
    if input.is_null() {
        return std::ptr::null_mut();
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let c_str = CStr::from_ptr(input);
        let source = c_str.to_str().ok()?;
        let eval = (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut()?;
        eval.setup_thread_locals();
        let forms = neovm_core::elisp::parse_forms(source).ok()?;
        let mut last = neovm_core::elisp::Value::nil();
        for form in &forms {
            match eval.eval_expr(form) {
                Ok(val) => last = val,
                Err(e) => {
                    log::error!("eval error: {:?}", e);
                    return None;
                }
            }
        }
        let printed = neovm_core::elisp::print_value_bytes_with_eval(eval, &last);
        CString::new(printed).ok()
    }));
    match result {
        Ok(Some(cstring)) => cstring.into_raw(),
        _ => std::ptr::null_mut(),
    }
}

/// Free a string returned by neomacs_rust_eval_string.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_free_string(s: *mut c_char) {
    if !s.is_null() {
        drop(CString::from_raw(s));
    }
}

/// Check if the Rust Evaluator is initialized.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_ready() -> c_int {
    if (*std::ptr::addr_of!(RUST_EVALUATOR)).is_some() { 1 } else { 0 }
}
```

**Step 3: Register the module in ffi/mod.rs**

In `rust/neomacs-display/src/ffi/mod.rs`, add:
```rust
pub mod eval_bridge;
```

**Step 4: Verify Rust compiles**

Run: `cargo check --manifest-path rust/neomacs-display/Cargo.toml`
Expected: compiles clean (or with known warnings)

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/ffi/eval_bridge.rs rust/neomacs-display/src/ffi/mod.rs rust/neomacs-display/Cargo.toml
git commit -m "feat: add eval_bridge FFI module with Evaluator singleton"
```

---

### Task 2: Wire C startup to initialize the Rust Evaluator

**Files:**
- Modify: `src/emacs.c` (add init call)
- Modify: `src/neomacsterm.c` (add extern declaration)

**Step 1: Add extern declaration**

In `src/neomacsterm.c` (near the other `extern` declarations for Rust FFI functions, around line 64), add:
```c
extern int neomacs_rust_eval_init (void);
extern char *neomacs_rust_eval_string (const char *input);
extern void neomacs_rust_free_string (char *s);
extern int neomacs_rust_eval_ready (void);
```

**Step 2: Call init from emacs.c**

In `src/emacs.c`, after the render thread is started (after `neomacs_display_create` or similar), add under `#if NEOVM_CORE_BACKEND_RUST`:
```c
#if NEOVM_CORE_BACKEND_RUST
  if (neomacs_rust_eval_init () < 0)
    {
      fprintf (stderr, "Failed to initialize Rust evaluator\n");
      exit (1);
    }
#endif
```

Find the exact insertion point after `syms_of_neomacsterm()` / display init and before `Frecursive_edit()`.

**Step 3: Build with --with-neovm-core-backend=rust**

Run:
```bash
./configure --with-neovm-core-backend=rust [other flags]
make
```

Expected: binary links successfully, Evaluator is initialized at startup.

**Step 4: Add a smoke test DEFUN**

In `src/neomacsterm.c`, add a temporary DEFUN that calls `neomacs_rust_eval_string`:
```c
DEFUN ("neomacs-rust-eval", Fneomacs_rust_eval, Sneomacs_rust_eval,
       1, 1, 0,
       doc: /* Evaluate FORM-STRING through the Rust evaluator.  */)
  (Lisp_Object form_string)
{
  CHECK_STRING (form_string);
  const char *input = SSDATA (form_string);
  char *result = neomacs_rust_eval_string (input);
  if (result == NULL)
    return Qnil;
  Lisp_Object ret = build_string (result);
  neomacs_rust_free_string (result);
  return ret;
}
```

Register it in `syms_of_neomacsterm()`:
```c
defsubr (&Sneomacs_rust_eval);
```

**Step 5: Test from the running editor**

Launch neomacs, evaluate in the minibuffer:
```
M-: (neomacs-rust-eval "(+ 1 2)")
```
Expected: returns `"3"`.

```
M-: (neomacs-rust-eval "(car '(a b c))")
```
Expected: returns `"a"`.

**Step 6: Commit**

```bash
git add src/emacs.c src/neomacsterm.c
git commit -m "feat: wire Rust Evaluator init into startup, add smoke test DEFUN"
```

---

### Task 3: Load foundation .el files in Rust Evaluator

**Files:**
- Modify: `rust/neomacs-display/src/ffi/eval_bridge.rs`

**Step 1: Add load-path setup and .el loading to init**

Extend `neomacs_rust_eval_init()` to set `load-path` and load foundation files:
```rust
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_init() -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut eval = neovm_core::elisp::Evaluator::new();

        // Set load-path to Emacs lisp directories
        // Determine lisp directory relative to binary or from env
        if let Ok(lisp_dir) = std::env::var("EMACSLOADPATH") {
            let paths: Vec<neovm_core::elisp::Value> = lisp_dir
                .split(':')
                .filter(|p| !p.is_empty())
                .map(|p| neovm_core::elisp::Value::string(&eval, p))
                .collect();
            let load_path = eval.list_from_vec(paths);
            eval.set_variable("load-path", load_path);
        }

        *std::ptr::addr_of_mut!(RUST_EVALUATOR) = Some(eval);
    }));
    match result {
        Ok(()) => 0,
        Err(e) => {
            log::error!("neomacs_rust_eval_init PANIC: {:?}", e);
            -1
        }
    }
}
```

**Step 2: Add a load-file FFI function**

```rust
/// Load an .el file through the Rust Evaluator.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_load_file(
    path: *const c_char,
) -> c_int {
    if path.is_null() { return -1; }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let c_str = CStr::from_ptr(path);
        let path_str = c_str.to_str().ok()?;
        let eval = (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut()?;
        eval.setup_thread_locals();
        // Use neovm-core's load machinery
        let load_expr = neovm_core::elisp::parse_forms(
            &format!("(load {:?})", path_str)
        ).ok()?;
        for form in &load_expr {
            eval.eval_expr(form).ok()?;
        }
        Some(())
    }));
    match result {
        Ok(Some(())) => 0,
        _ => -1,
    }
}
```

**Step 3: Test loading subr.el from the running editor**

```
M-: (neomacs-rust-eval "(load \"subr\")")
M-: (neomacs-rust-eval "(when t 42)")  ;; 'when' is defined in subr.el
```
Expected: returns `"42"`.

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/ffi/eval_bridge.rs
git commit -m "feat: add .el file loading support to Rust eval bridge"
```

---

### Task 4: Expose Evaluator reference for display engine access

**Files:**
- Modify: `rust/neomacs-display/src/ffi/eval_bridge.rs`

This is the critical step that enables later phases. The display engine (same crate) needs to borrow the Evaluator's buffer/face/window state during redisplay.

**Step 1: Add accessor functions for the Evaluator**

```rust
/// Get a reference to the Rust Evaluator.
/// SAFETY: Only call from the main thread. The returned reference
/// is valid until the next mutable access (eval call).
pub(crate) unsafe fn get_evaluator() -> Option<&'static neovm_core::elisp::Evaluator> {
    (*std::ptr::addr_of!(RUST_EVALUATOR)).as_ref()
}

/// Get a mutable reference to the Rust Evaluator.
/// SAFETY: Only call from the main thread.
pub(crate) unsafe fn get_evaluator_mut() -> Option<&'static mut neovm_core::elisp::Evaluator> {
    (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut()
}
```

**Step 2: Verify the display crate can import these**

In any display-side code (e.g. `layout/engine.rs`), verify this import compiles:
```rust
use crate::ffi::eval_bridge::{get_evaluator, get_evaluator_mut};
```

Run: `cargo check --manifest-path rust/neomacs-display/Cargo.toml`

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/ffi/eval_bridge.rs
git commit -m "feat: expose Evaluator accessors for display engine"
```

---

### Task 5: Implement neomacs_rust_handle_key FFI

**Files:**
- Modify: `rust/neomacs-display/src/ffi/eval_bridge.rs`

This is the first step toward Rust handling actual keyboard input.

**Step 1: Add key handling FFI function**

```rust
/// Handle a key event from C's read_char().
/// `key` is the Emacs internal key representation (integer).
/// `modifiers` is the modifier bitmask.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_handle_key(
    key: c_int,
    modifiers: c_int,
) -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let eval = (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut()?;
        eval.setup_thread_locals();

        // Build the key event as a Lisp expression and evaluate it
        // For now, just handle basic character insertion
        if modifiers == 0 && (32..=126).contains(&key) {
            let ch = key as u8 as char;
            let expr_str = format!("(self-insert-command 1)");
            // Set last-command-event
            eval.set_variable("last-command-event", neovm_core::elisp::Value::int(key as i64));
            let forms = neovm_core::elisp::parse_forms(&expr_str).ok()?;
            for form in &forms {
                eval.eval_expr(form).ok()?;
            }
        }
        Some(())
    }));
    match result {
        Ok(Some(())) => 0,
        _ => -1,
    }
}
```

**Step 2: Add extern declaration in C**

In `src/neomacsterm.c`:
```c
extern int neomacs_rust_handle_key (int key, int modifiers);
```

**Step 3: Verify it compiles**

Run: `cargo check --manifest-path rust/neomacs-display/Cargo.toml`
Run: `make`

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/ffi/eval_bridge.rs src/neomacsterm.c
git commit -m "feat: add neomacs_rust_handle_key FFI for keyboard input"
```

---

### Task 6: Verify neovm-core tests still pass

**Step 1: Run full test suite**

Run: `cargo nextest run --manifest-path rust/neovm-core/Cargo.toml`
Expected: 4571 tests pass

**Step 2: Run full build**

Run: `make`
Expected: binary links successfully

**Step 3: Manual smoke test**

Launch neomacs with `--with-neovm-core-backend=rust`:
```
M-: (neomacs-rust-eval "(+ 1 2)")       ;; => "3"
M-: (neomacs-rust-eval "(list 1 2 3)")  ;; => "(1 2 3)"
M-: (neomacs-rust-eval "(concat \"hello\" \" \" \"world\")")  ;; => "\"hello world\""
```

---

## Phase 2: Buffer State Integration (outline)

**Goal:** Make the display engine read buffer data from neovm-core's `BufferManager` instead of C structs.

### Task 7: Create LayoutContext that borrows Evaluator state
- New `LayoutContext` struct holding `&BufferManager`, `&WindowManager`, `&FaceTable`
- Layout engine's `layout_frame()` takes `LayoutContext` instead of raw C pointers
- Initially: populate LayoutContext from C data (no behavior change)
- Gate: layout renders identically with the new abstraction

### Task 8: Replace buffer text FFI with Rust reads
- `neomacs_layout_buffer_text()` → `BufferManager::current_buffer().text()`
- `neomacs_buf_charpos_to_bytepos()` → `GapBuffer::char_to_byte()`
- Gate: text renders from Rust buffer data

### Task 9: Replace text property FFI with Rust reads
- `check_invisible()` → `TextPropertyTable::get_property(pos, "invisible")`
- `check_display_prop()` → `TextPropertyTable::get_property(pos, "display")`
- `check_line_spacing()` → `TextPropertyTable::get_property(pos, "line-spacing")`
- Gate: invisible text, display properties work from Rust

### Task 10: Replace overlay FFI with Rust reads
- `overlay_strings_at()` → `OverlayList::at_pos()`
- `margin_strings_at()` → `OverlayList` + `TextPropertyTable`
- Gate: overlay strings render correctly

### Task 11: Replace window geometry FFI with Rust reads
- `get_window_params()` → `WindowManager`
- `set_window_end()` / `set_cursor()` → write to Rust `WindowManager`
- `adjust_window_start()` → computed in Rust
- Gate: window layout geometry from Rust

### Task 12: Replace line counting FFI
- `count_line_number()` → `GapBuffer` line iteration
- `line_number_config()` → `Evaluator` variable lookup
- Gate: line numbers render correctly

---

## Phase 3: Face/Font in Rust (outline)

**Goal:** Replace C's `face_at_buffer_position()` + `fill_face_data()` with a pure Rust `FaceResolver`.

### Task 13: Implement FaceResolver struct
- Reads `FaceTable`, `TextPropertyTable`, `OverlayList`
- Merges face attributes (default → text prop → overlay)
- Uses existing `Face::merge()` from `neovm-core/src/face.rs`
- Gate: unit tests for face merging at positions

### Task 14: Wire FaceResolver into layout engine
- Replace `neomacs_layout_face_at_pos()` FFI call with `FaceResolver::face_at_pos()`
- Replace `neomacs_layout_default_face()` with `FaceTable::get("default")`
- Gate: faces render correctly from Rust

### Task 15: Implement font metric extraction from cosmic-text
- Replace `fill_face_data()` font metrics (ascent, char_width, space_width) with `FontMetricsService`
- Implement overstrike detection via fontdb weight query
- Replace `line_number_face()` with `FaceTable` lookup
- Gate: font metrics match, bold simulation works

### Task 16: Implement face remapping and height inheritance
- `face-remapping-alist` lookup in buffer-local environment
- `FaceHeight::Relative` resolution against default face absolute height
- Gate: face remapping and relative heights work

---

## Phase 4: Display Completion (outline)

**Goal:** Eliminate all remaining C FFI calls from the display engine.

### Task 17: Implement Rust format-mode-line
- Interpret mode-line format specs (`%b`, `%l`, `(:eval ...)`, `(:propertize ...)`)
- Replace `neomacs_layout_mode_line_text()`, `header_line_text()`, `tab_line_text()` FFI
- Gate: mode-line renders correctly from Rust

### Task 18: Implement fontification triggering
- Layout engine calls Rust Evaluator to run `fontification-functions`
- Replace `neomacs_layout_ensure_fontified()` FFI
- Gate: syntax highlighting works via Rust eval

### Task 19: Replace remaining display FFI
- `get_fringe_bitmap()` → Rust static bitmap table
- `get_stipple_bitmap()` → Rust static bitmap table
- `check_glyphless()` → Rust Unicode category lookup
- Gate: all 24 FFI functions eliminated, zero `extern "C"` in `emacs_ffi.rs`

---

## Phase 5: Real Subprocess I/O (outline)

**Goal:** `start-process` spawns real OS processes with async I/O.

### Task 20: Implement LiveProcess with real Child handles
- Replace stub `start-process` with `std::process::Command::spawn()`
- Store `Child`, `ChildStdout`, `ChildStderr`, `ChildStdin`
- Gate: `(start-process "ls" nil "ls" "-la")` spawns real process

### Task 21: Wire process fds with xg_select
- `neomacs_register_process_fd()` FFI to register stdout/stderr fds
- C's `xg_select()` monitors them alongside keyboard input
- `neomacs_rust_process_ready()` callback reads data, runs filter
- Gate: `M-x shell` works, process output appears in buffer

### Task 22: Implement process sentinels and network processes
- Process exit detection via `child.try_wait()`
- Sentinel function dispatch via Evaluator
- `make-network-process` client mode with `TcpStream`
- Gate: network connections work, `url-retrieve` can fetch HTTP

---

## Phase 6: Startup and Package Ecosystem (outline)

**Goal:** Neomacs boots entirely through Rust eval, can install and load ELPA/MELPA packages.

### Task 23: Create neomacs-loadup.el
- Minimal `.el` file set for a working editor (~30-50 files)
- Skip C-specific files (term.el, mule-conf.el)
- Gate: editor boots to scratch buffer with all standard keybindings

### Task 24: Wire startup sequence
- `emacs.c:main()` → `neomacs_rust_eval_init()` → load `.el` files → `command-line` processing
- User init file loading
- Gate: `~/.emacs.d/init.el` is evaluated at startup

### Task 25: Implement #:uninterned-symbol in parser
- Add `#:foo` syntax to `neovm-core/src/elisp/parser.rs`
- Gate: macro-generated code with gensyms parses correctly

### Task 26: Test ELPA/MELPA package installation
- `package-install` from `.el` source
- Verify popular packages: `magit`, `which-key`, `company`
- Gate: packages install and load from `.el` source

---

## Phase 7: C Code Removal (outline)

**Goal:** Remove unused C source files from the build.

### Task 27: Remove C eval/buffer/alloc files from Makefile
- Remove `eval.o`, `buffer.o`, `alloc.o`, `fns.o`, `data.o`, etc. from `base_obj`
- Remove `xfaces.o`, `font.o`, `ftcrfont.o`
- Gate: binary links without removed C files

### Task 28: Strip unused syms_of_* calls
- Remove `syms_of_eval()`, `syms_of_buffer()`, `syms_of_alloc()`, etc. from `emacs.c`
- Keep only `syms_of_neomacsterm()` and OS-init calls
- Gate: clean startup with minimal C init

### Task 29: Final verification
- Full test suite: `cargo nextest run`
- Binary size comparison (before/after C removal)
- Manual testing: boot, edit, install packages, subprocess I/O
- Gate: all functionality works with minimal C footprint

---

## Verification Commands

After each task:
```bash
cargo check --manifest-path rust/neomacs-display/Cargo.toml  # Rust compiles
cargo nextest run --manifest-path rust/neovm-core/Cargo.toml  # neovm-core tests pass
make                                                           # binary links
# Manual smoke test for behavior gates
```

## Critical Files Reference

| File | Role |
|---|---|
| `rust/neomacs-display/src/ffi/eval_bridge.rs` | NEW: Evaluator singleton + FFI functions |
| `rust/neomacs-display/src/ffi/mod.rs` | Register eval_bridge module |
| `rust/neomacs-display/src/layout/engine.rs` | Layout engine — switch from C FFI to Rust reads |
| `rust/neomacs-display/src/layout/emacs_ffi.rs` | Current C FFI declarations — to be eliminated |
| `rust/neomacs-display/src/layout/font_metrics.rs` | FontMetricsService — already provides char widths |
| `rust/neovm-core/src/elisp/eval.rs` | Evaluator struct and eval_expr() |
| `rust/neovm-core/src/face.rs` | Face, FaceTable, Face::merge() |
| `rust/neovm-core/src/buffer/` | BufferManager, GapBuffer, TextPropertyTable, OverlayList |
| `src/emacs.c` | C main() — add Rust init call |
| `src/keyboard.c` | C command loop — route eval to Rust |
| `src/neomacsterm.c` | C FFI bridge — extern declarations for Rust functions |
| `src/eval.c` | C eval — to be bypassed/removed |
| `src/xfaces.c` | C face resolution — to be replaced by FaceResolver |
