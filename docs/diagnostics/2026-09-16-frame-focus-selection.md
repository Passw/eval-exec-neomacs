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
  switches back and forth between two distinct frame buffers.
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
