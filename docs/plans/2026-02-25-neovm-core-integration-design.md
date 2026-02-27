# Design: Integrate neovm-core into the Live Editor

**Date**: 2026-02-25
**Status**: Approved

## Goal

Replace the C Emacs evaluator with neovm-core's Rust `Evaluator` as the sole Lisp execution engine in the live neomacs editor. Eliminate all C eval, buffer, and face/font code. The editor runs Lisp through Rust end-to-end, loads ELPA/MELPA packages from `.el` source files, and supports real subprocess I/O.

## Key Decisions

| Decision | Choice | Rationale |
|---|---|---|
| State authority | Rust-authoritative | neovm-core owns all Lisp state (buffers, windows, symbols, faces). No dual-state sync. |
| Transition strategy | Big-bang swap | 97/98 C files already have Rust dispatch counterparts. 4,571 tests pass. |
| Face/font resolution | Pure Rust | `FaceResolver` + cosmic-text `FontMetricsService`. No C xfaces.c dependency. |
| Display engine data source | Direct Rust reads | All 24 display FFI functions eliminated. Layout engine reads from neovm-core directly. |
| `.elc` loading | Not supported | `.el` source only. ELPA/MELPA packages always ship `.el` alongside `.elc`. |
| Milestone | Package ecosystem working | Boot, edit, install/load ELPA/MELPA packages from `.el` source. |

## Architecture

```
+-----------------------------------------------------+
|  C Shell (minimal, ~3 files)                        |
|  emacs.c:main() -> init OS, start Rust              |
|  keyboard.c:command_loop_1() -> read keys, call Rust |
|  xg_select() -> event loop (keyboard + process fds) |
+---------------+-------------------------------------+
                | FFI (integers, fd numbers only)
                v
+-----------------------------------------------------+
|  neovm-core Evaluator (single instance, main thread) |
|                                                      |
|  Obarray    FaceTable    BufferManager               |
|  LispHeap   KeymapMgr   ProcessManager (real I/O)   |
|  Interner   WindowMgr   TimerManager                |
+---------------+-------------------------------------+
                | Direct Rust calls (borrow, no FFI)
                v
+-----------------------------------------------------+
|  neomacs-display (layout + rendering)                |
|                                                      |
|  FaceResolver          FontMetricsService            |
|  (reads FaceTable,     (cosmic-text)                 |
|   TextProps, Overlays)                               |
|                                                      |
|  layout/engine.rs      -> FrameGlyphBuffer           |
|  (reads BufferMgr,        |                          |
|   WindowMgr)               | crossbeam channel       |
|                             v                         |
|                        Render Thread (wgpu, winit)    |
+-----------------------------------------------------+
```

## Component Designs

### 1. Eval Takeover

A new FFI function `neomacs_rust_eval()` in the neomacs-display crate routes C's `eval_sub()` and `Ffuncall()` into neovm-core's `Evaluator::eval_expr()`.

```
Current:  keyboard.c -> Ffuncall() -> eval_sub() -> C builtins
New:      keyboard.c -> Ffuncall() -> neomacs_rust_eval() -> Evaluator::eval_expr()
```

When `--with-neovm-core-backend=rust` is active, C's `eval_sub()` delegates to Rust instead of interpreting Lisp in C. The Evaluator is a single instance on the main thread, stored in a thread-local (same pattern as `THREADED_STATE` for the render channel).

### 2. Command Loop and Keyboard Input

The C command loop shell stays. It is thin: read keys via `read_key_sequence()` (blocks on `xg_select`), then call eval via Rust. The render thread sends key events via crossbeam channel -> `wakeup_fd` pipe -> `xg_select()` -> `kbd_buffer` as today.

```
C keyboard.c:command_loop_1():
  read_key_sequence(keybuf)              // stays in C
  cmd = lookup_key(keybuf)               // -> neomacs_rust_lookup_key()
  calln(Qcommand_execute, cmd)           // -> neomacs_rust_eval(form)
  redisplay()                            // -> Rust layout engine
```

**`recursive-edit`**: The Rust Evaluator's `recursive-edit` builtin calls back into C's `command_loop_1()` via FFI, creating a Rust->C->Rust call chain. This is safe because it is single-threaded and stack-based, exactly how C's recursive-edit works today.

```
Rust eval: (read-from-minibuffer "File: ")
  -> Rust builtin calls C FFI: neomacs_c_recursive_edit()
    -> C: command_loop_1() [recursive]
      -> read keys, eval via Rust...
    -> C returns when (exit-recursive-edit) is called
  -> Rust gets the minibuffer result
```

**`read-event` / `read-char`**: Rust builtins call into C's `read_char()` which blocks on `xg_select()`. Same Rust->C->Rust pattern.

### 3. Buffer State Bridge

The display engine reads buffer data directly from neovm-core's `BufferManager` via Rust function calls, not C FFI.

```
Current:  layout/engine.rs -> extern "C" neomacs_layout_buffer_text() -> C gap buffer
New:      layout/engine.rs -> BufferManager::current_buffer().text() -> Rust gap buffer
```

During redisplay (between commands on the main thread), the layout engine borrows the Evaluator's buffer state:

```rust
let bufmgr: &BufferManager = evaluator.buffer_manager();
let face_resolver = FaceResolver::new(&evaluator.face_table(), &mut font_metrics);
let layout_ctx = LayoutContext::new(bufmgr, face_resolver);
layout_ctx.layout_frame(&mut frame_glyphs);
```

### 4. Pure Rust Face/Font Resolution

Replace C's `face_at_buffer_position()` + `fill_face_data()` with a Rust `FaceResolver` that reads from neovm-core's `FaceTable`, `TextPropertyTable`, and `OverlayList`, then gets font metrics from `FontMetricsService` (cosmic-text).

```rust
pub struct FaceResolver<'a> {
    face_table: &'a FaceTable,
    font_metrics: &'a mut FontMetricsService,
    default_face: ResolvedFace,
}

impl<'a> FaceResolver<'a> {
    pub fn face_at_pos(
        &mut self,
        buffer: &Buffer,
        pos: usize,
        next_check: &mut usize,
    ) -> FaceData {
        // 1. Start with default face attributes
        let mut attrs = self.default_face.attrs.clone();

        // 2. Merge 'face' text property
        let (face_prop, prop_end) = buffer.text_props().get_property(pos, "face");
        if let Some(val) = face_prop { self.merge_face_spec(&mut attrs, val); }

        // 3. Merge 'font-lock-face' text property
        let (fl_prop, fl_end) = buffer.text_props().get_property(pos, "font-lock-face");
        if let Some(val) = fl_prop { self.merge_face_spec(&mut attrs, val); }

        // 4. Merge overlay faces sorted by priority
        for overlay in buffer.overlays().at_pos(pos) {
            if let Some(face) = overlay.get_property("face") {
                self.merge_face_spec(&mut attrs, face);
            }
        }

        // 5. Realize face with font metrics from cosmic-text
        *next_check = min(prop_end, fl_end, next_overlay_change);
        self.realize_face(attrs)
    }

    fn realize_face(&mut self, attrs: FaceAttrs) -> FaceData {
        let family = attrs.family.unwrap_or(self.default_face.family);
        let size = attrs.resolve_height(self.default_face.pixel_size);
        let weight = attrs.weight.unwrap_or(FontWeight::NORMAL);
        let slant = attrs.slant.unwrap_or(FontSlant::Roman);
        let metrics = self.font_metrics.get_metrics(family, weight, slant, size);

        FaceData {
            fg: attrs.foreground.unwrap_or(self.default_face.fg),
            bg: attrs.background.unwrap_or(self.default_face.bg),
            font_size: size,
            font_weight: weight.0,
            italic: slant == FontSlant::Italic,
            font_ascent: metrics.ascent,
            font_char_width: metrics.char_width,
            font_space_width: metrics.space_width,
            overstrike: metrics.overstrike,
            // ... underline, box, etc. from attrs
        }
    }
}
```

**Overstrike detection**: Query fontdb for the requested (family, bold weight). If the resolved font's actual weight differs from requested, set `overstrike = true`.

**Fontset fallback**: Use cosmic-text's built-in Unicode fallback initially. Covers CJK, emoji, and symbol blocks. Refine with explicit fontset logic later.

**Fontification**: The Rust Evaluator runs `fontification-functions` directly when the layout engine detects unfontified regions. Font-lock sets `font-lock-face` text properties on the Rust buffer. The `FaceResolver` reads these properties. All in Rust.

### 5. Display FFI Elimination

All 24 `extern "C"` FFI functions in `emacs_ffi.rs` are replaced with direct Rust calls:

| FFI Function | Rust Replacement |
|---|---|
| `buffer_text` | `BufferManager` direct read |
| `charpos_to_bytepos` | `GapBuffer::char_to_byte()` |
| `face_at_pos` | `FaceResolver::face_at_pos()` |
| `default_face` | `FaceTable::get("default")` |
| `char_width` | `FontMetricsService` (already default) |
| `fill_ascii_widths` | `FontMetricsService` (already default) |
| `check_invisible` | `TextPropertyTable` read |
| `overlay_strings_at` | `OverlayList` read |
| `check_display_prop` | `TextPropertyTable` + Rust eval for display specs |
| `check_line_spacing` | `TextPropertyTable` read |
| `margin_strings_at` | `TextPropertyTable` + `OverlayList` |
| `count_line_number` | `GapBuffer` line counting |
| `line_number_config` | `Evaluator` variable lookup |
| `mode_line_text` | Rust `format-mode-line` implementation |
| `header_line_text` | Same |
| `tab_line_text` | Same |
| `ensure_fontified` | Rust Evaluator runs `fontification-functions` |
| `line_number_face` | `FaceTable` lookup |
| `get_window_params` | `WindowManager` |
| `adjust_window_start` | `WindowManager` |
| `set_window_end` | `WindowManager` |
| `set_cursor` | `WindowManager` |
| `get_fringe_bitmap` | Rust static bitmap table |
| `get_stipple_bitmap` | Rust static bitmap table |
| `check_glyphless` | Rust Unicode category lookup |

### 6. Async Subprocess I/O

Real subprocess management using `std::process::Command`:

```rust
pub struct LiveProcess {
    child: std::process::Child,
    stdout: Option<BufReader<ChildStdout>>,
    stderr: Option<BufReader<ChildStderr>>,
    stdin: Option<ChildStdin>,
    filter: Option<Value>,
    sentinel: Option<Value>,
    buffer: Option<BufferId>,
    status: ProcessStatus,
}
```

Process fd polling integrates with `xg_select()`:

```
Rust ProcessManager registers child stdout/stderr fds with C's xg_select():
  neomacs_register_process_fd(fd, process_id)

When xg_select() returns with a readable process fd:
  -> C calls neomacs_rust_process_ready(process_id)
    -> Rust reads available data
    -> Runs process filter function via Evaluator
    -> Output inserted into process buffer
```

Network processes use the same pattern with `TcpStream` fds. TLS via `rustls` added later.

### 7. Startup Sequence

No pdumper. Load `.el` files from source.

```
main() [C]
  -> init OS resources (signal handlers, terminal)
  -> neomacs_rust_init()  [FFI into Rust]
    -> Evaluator::new()
    -> Load foundation .el files:
        subr.el, cl-lib.el, simple.el, files.el,
        startup.el, keymap.el, minibuffer.el, ...
    -> Run (command-line) -> processes user init file
  -> command_loop_1()  [normal editing begins]
```

A `neomacs-loadup.el` replaces `loadup.el`, loading only the ~30-50 `.el` files needed for a working editor (skipping C-specific files like `term.el`, `mule-conf.el`).

Startup speed mitigations:
- neovm-core's `.neoc` parse cache skips re-parsing unchanged files
- The Rust evaluator is fast (4,571 tests in 2.7s)
- Future: Rust-native snapshot format for instant startup (not needed for first milestone)

## What Remains in C

After integration, only 3 C files serve functional roles:

| File | Role | Lines (relevant) |
|---|---|---|
| `emacs.c` | `main()`, OS init, signal handlers | ~200 |
| `keyboard.c` | `command_loop_1()`, `read_key_sequence()`, `read_char()` | ~2000 |
| `sysdep.c` | `xg_select()`, process fd monitoring | ~500 |

Everything else is replaced:
- `eval.c` -> neovm-core `Evaluator`
- `buffer.c`, `alloc.c`, `fns.c`, `data.c`, etc. -> neovm-core builtins
- `xfaces.c`, `font.c`, `ftcrfont.c` -> `FaceResolver` + `FontMetricsService`
- `xdisp.c` -> already replaced by Rust layout engine
- `neomacsterm.c` FFI bridge -> eliminated (direct Rust calls)

Long-term, the remaining C shell (keyboard input, xg_select) can move to Rust too, but it is not blocking.

## Implementation Phases

### Phase 1: Eval Bridge
- Wire `neomacs_rust_eval()` FFI function
- Activate `--with-neovm-core-backend=rust` feature flag to route eval to Rust
- Implement `recursive-edit` Rust->C->Rust callback
- Implement `read-event`/`read-char` Rust->C callbacks
- **Gate**: Basic Lisp evaluation works through Rust, can call `(+ 1 2)` from the command loop

### Phase 2: Buffer State Integration
- Make layout engine read from neovm-core `BufferManager` instead of C structs
- Replace buffer text, text property, overlay, and line counting FFI calls
- Wire `WindowManager` for window geometry and cursor/window-end write-back
- **Gate**: `*scratch*` buffer renders with text from Rust buffer

### Phase 3: Face/Font in Rust
- Implement `FaceResolver` reading from `FaceTable`, `TextPropertyTable`, `OverlayList`
- Wire `FontMetricsService` for font metric extraction (ascent, char_width, space_width)
- Implement overstrike detection via fontdb
- Replace `face_at_pos`, `default_face`, `line_number_face` FFI calls
- **Gate**: Syntax-highlighted buffer renders with correct faces

### Phase 4: Display Completion
- Implement Rust `format-mode-line` (interpret mode-line format specs)
- Implement fontification triggering via Rust Evaluator
- Replace remaining FFI: fringe bitmaps, glyphless chars, stipple, margins
- **Gate**: Full frame renders (text + mode-line + fringes) with no C FFI calls

### Phase 5: Real Subprocess I/O
- Implement `LiveProcess` with real `Child` handles
- Wire process fd registration with `xg_select()`
- Implement process filter/sentinel dispatch
- Wire `make-network-process` client mode with `TcpStream`
- **Gate**: `(shell-command "ls")` works, `M-x shell` works

### Phase 6: Startup and Package Ecosystem
- Create `neomacs-loadup.el` with minimal `.el` file set
- Wire startup sequence: `Evaluator::new()` -> load `.el` files -> `command-line`
- Implement `#:uninterned-symbol` in parser
- Test ELPA/MELPA package installation and loading from `.el` source
- **Gate**: Can install and load packages, user init file works

### Phase 7: C Code Removal
- Remove C eval, buffer, face/font source files from build
- Strip unused `syms_of_*()` calls from `emacs.c`
- Clean up Makefile to skip removed C files
- **Gate**: Binary links and runs with minimal C footprint

## Verification

Each phase gate requires:
```bash
cargo check                    # Rust compiles clean
cargo nextest run              # All neovm-core tests pass
make                           # Binary links
# Manual: editor launches, specific gate behavior verified
```

## Risks

| Risk | Mitigation |
|---|---|
| `.el` files that depend on C-specific builtins | neovm-core has 775 builtins covering 97/98 C files. Gap-fill stubs return oracle-verified values. |
| font-lock / fontification complexity | Font-lock is pure Lisp. It sets text properties. FaceResolver reads them. No C dependency. |
| Startup speed without pdumper | `.neoc` parse cache + fast Rust evaluator. Snapshot format as future optimization. |
| cosmic-text fontset fallback differs from Emacs | Acceptable for first milestone. Refine with explicit fontset logic later. |
| Subprocess I/O edge cases | `call-process` already works. `start-process` is well-defined POSIX API. |
| `format-mode-line` complexity | Self-contained ~800 line algorithm. Mode-line format specs are well-documented. |
