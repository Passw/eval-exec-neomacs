# Native focus must select the input frame (#395)

## Reproduction and root cause

The headless X11 test creates a second frame with real `C-x 5 2` input,
assigns it a distinct buffer, focuses its native window, verifies X11's input
focus, and types into it. Before the fix, all three repeated runs inserted the
new text in the old frame. The identical fixture passed in GNU Emacs 31.1.

A temporary pre-command trace distinguished event transport from selection:

```text
switch-frame target=SECONDARY selected=PRIMARY
handle-switch-frame@PRIMARY
self-insert-command@PRIMARY
```

The frontend supplied the correct target and the reader delivered the switch
before the key. The registered `handle-switch-frame` primitive was a stateless
stub in `builtins/stubs.rs`: it checked its argument, then returned nil without
selecting anything. Existing tests asserted that placeholder return value and
never exercised editing in a newly focused frame.

A follow-up regression covered a programmatic cross-frame `select-window`
without any native focus notification. It exposed two further ordering gaps:
that path did not clear the cached input frame, and a frame-tagged key could
queue a switch but still be returned before it. Selection-cache invalidation
now belongs to the shared window-selection transaction. The keyboard reader
leaves a key (or decoded TTY character) queued while delivering its generated
switch first, before translation, input-method processing or macro recording.

Repeated native runs then exposed a separate frontend error: occasional extra
characters, even though the destination buffer was now correct. Winit's X11
focus handler queries currently held keys and replays them as
`KeyboardInput { is_synthetic: true, .. }`. The desktop event dispatcher had
discarded that provenance, interpreting the replay as a fresh press. A
deterministic regression holds `c` in one frame while moving focus to the
other: the old dispatcher inserts an extra `c`; GNU Emacs does not (three
consecutive control runs passed).

The desktop winit boundary now rejects synthetic **presses** before menus,
tooltips, webviews or editor commands process them. It preserves synthetic
**releases**, so consumers can clear held-key state, and leaves modifier and
focus notifications intact. This uses the existing typed `WindowEvent` and
`ElementState` variants; it does not add a parallel input-state abstraction.

## GNU reference

Studied GNU `src/keyboard.c`, `src/frame.c`, and `lisp/frame.el` first:

- Native focus and logical selected frame are separate concepts.
- `internal-handle-focus-in` can synthesize a `switch-frame` event.
- Ordinary keyboard input from another frame also yields `switch-frame`
  before the original key is consumed.
- `Fhandle_switch_frame` preserves the current prefix argument and runs
  `mouse-leave-buffer-hook` before calling `do_switch_frame` with tracking off.
- `Fselect_frame` validates a live frame and uses the same selection machinery
  with tracking on and the caller's `NORECORD` value.
- A queued event targeting a deleted frame is harmless; an explicit
  `select-frame` call on that frame signals `frame-live-p`.
- Selecting the already-selected frame returns that frame without recording
  another selection or changing the current buffer.
- On a genuine GUI frame change, the cached last input frame is cleared unless
  the destination is an ancestor. GNU avoids that reset on TTYs to prevent
  post-command selection loops.

For the replay failure, GNU `src/xterm.c` handles focus notifications separately
from key presses. Winit's `WindowEvent::KeyboardInput` contract documents held
key replay on X11 and Windows; its X11 `xinput2_focused` and
`handle_pressed_keys` implementations explain the observed duplicate.

## Ownership and design

```text
native focus / keyboard event, tagged with its source frame
  → keyboard reader: GNU-compatible switch-frame event
  → frame/selection.rs: handle-switch-frame or select-frame
  → window_cmds::select_window: frame + window + buffer + history transaction
  → Lisp hooks, with consistent selection already published
```

`emacs_core/display/frame/selection.rs` owns the frame-selection primitives and
their shared selection operation. It uses the existing window-selection
transaction for point restoration, buffer history, terminal keyboard ownership,
and redisplay invalidation. `Context` remains confined to the VM thread.

`FrameId` and `WindowId` remain distinct Rust types. The closed
`FrameFocusTracking::{FollowSelection, Preserve}` enum expresses the explicit
versus input-driven redirection policy; its implementation uses an exhaustive
match. Existing explicit-selection tracking remains unchanged, while native
input-driven switches preserve redirections.

The handler is registered as a context-dependent fixed-one-argument primitive.
Its Rust function signature determines the maximum arity, and stateless
dispatch must defer to an actual evaluator. Registration remains in the same
startup position.

This is the shared VM correction, not an X11-specific selection implementation.
The native regression exercises Linux X11; it is not evidence of a native
Windows or Wayland test run.

## Regression coverage

- Real X11 focus and keyboard events through `neomacs-gui-tests`, including
  switches back and forth between two distinct frame buffers and a held-key
  transfer that must not manufacture a new press in the destination.
- Public Lisp selection, return value, prefix preservation and old-frame hook.
- Focus-redirection preservation, both windows' points, and buffer history.
- Deleted targets, malformed events, and deletion from the leave hook.
- `NORECORD` and consistent frame/window/buffer observations in update hooks.
- Stateless dispatch rejection and bytecode dispatch of the real handler.
- Cross-frame programmatic window selection followed by a physical key from
  the still-focused frame, without a new focus notification.

The five new public Lisp contract forms were also run in GNU Emacs 31.1 under
Xvfb. Their results matched the regression expectations exactly. The GNU probe
used GUI `make-frame` instead of the core fixture's usable-terminal constructor.

## Running the native regression

Build the runnable editor and its matching runtime with `cargo xtask fresh-build
--release`, then run:

```sh
NEOMACS_GUI_TEST_BACKEND=x11 \
NEOMACS_GUI_TEST_BINARY="$PWD/target/release/neomacs" \
cargo nextest run -p neomacs-gui-tests --test native_frame_focus --stress-count 10
```

The test owns an isolated Xvfb display and requires `Xvfb`, `xauth`, and
`xdotool`. No desktop window manager is required: it sets and verifies actual
X11 keyboard focus. It runs automatically with the normal X11 GUI suite, and
does not run for other backends. To use GNU Emacs as a control, set
`NEOMACS_GUI_TEST_BINARY` to its executable instead. If fresh-build uses a
separate runtime root, also export the matching `NEOMACS_RUNTIME_ROOT`.

The final default-feature `neovm-core` library run passed all 10,023 tests
(53 skipped); the focused input/frame/window run passed all 149 tests. The
enabled X11 fixture passed five consecutive runs against GNU Emacs 31.1.

After the desktop replay fix, `cargo xtask fresh-build --release --low-memory`
completed successfully with an isolated runtime root. The expanded native
regression, including the held-key transfer, passed **20/20** stress runs
against that executable (and **3/3** GNU control runs).

The full GUI run passed 27 tests, failed two, and skipped 54. The two failures
were also present in the pre-fix baseline:

- `oversized_xwidget_keeps_its_intrinsic_page_visible_behind_the_window_clip`
- `static_xwidget_page_survives_renderer_device_replacement`

Thus the focus regression is green; the full GUI suite is not wholly green.
The native runtime validation here is Linux X11, not native Windows or Wayland.
