# Driving and Observing the NeoMacs GUI (for AI agents)

How to see and drive the NeoMacs GUI as plain text. Design:
`docs/plans/2026-07-02-gui-observability-agent-driving-design.md`.

## The one-line summary

`(neomacs--frame-snapshot FRAME FORMAT)` returns **what is on screen** —
text, resolved colors, geometry, every visible frame — as a string. Combine
it with `emacsclient --eval` against a live GUI instance and you have an
interactive observe/drive loop with no screenshots involved.

## Observing: the frame snapshot

```elisp
(neomacs--frame-snapshot)                 ; selected frame, text grid
(neomacs--frame-snapshot t 'text-faces)   ; ALL visible frames + face runs
(neomacs--frame-snapshot nil 'json)       ; full-fidelity JSON
(neomacs--write-frame-snapshot "/tmp/snap.json" t 'json)
```

- FRAME: `nil` = selected frame, `t` = every visible frame bottom-to-top
  (includes child frames: posframe/corfu popups, tooltips), or a frame
  object.
- FORMAT: `text` (default), `text-faces`, `json`.
- The subr forces a full redisplay first, then lays the target frames out
  on demand — the snapshot is always current, and synchronous.
- Batch mode has no display; the subr signals an error there.

Text format (greppable; one line per glyph row):

```text
=== frame 1: 80x24 cols 640x384 px ===
-- window 3 "*scratch*" bounds=(0,0 640x368)px text=(8,0 632x352)px start=1 end=145 selected --
   0|;; This buffer is for text that is not saved...
   1|
[mode-line]|-UUU:----F1  *scratch*   All L1    (Lisp Interaction)
[cursor] window=3 row=0 col=0 charpos=1 style=FilledBox
```

`text-faces` adds one line per face run under each row — "what color is
this element" is answerable by grep:

```text
   0|;; hello
    : run 0-8 font-lock-comment-face fg=#5B6268 bg=#282C34
```

JSON is serde of the real `FrameDisplayState` structs (`{"frames":[...]}`):
every glyph with charpos and pixel metrics, the `faces` table with resolved
colors and `lisp_name`s, window boxes (`window_infos`, incl. `buffer_name`),
cursors, images/videos/scroll bars, menu/tool/tab bars. If the renderer can
draw it, it is in this JSON.

Pixel truth (did the GPU actually paint?) remains the PNG readback's job:
run with `NEOMACS_DEBUG_SURFACE_READBACK=1` and
`NEOMACS_DEBUG_SURFACE_READBACK_PNG=/path.png` — the snapshot is the
semantic screen, the PNG proves it was painted.

## Driving: the live agent loop

Start a headless display and a GUI NeoMacs with the elisp server:

```sh
Xvfb :99 -screen 0 1280x800x24 -nolisten tcp &
DISPLAY=:99 target/release/neomacs -Q \
  --eval '(progn (setq server-name "neomacs-agent") (server-start))' &
```

(Wayland alternative: `weston --backend=headless --socket=neomacs-agent` and
`WAYLAND_DISPLAY=neomacs-agent WINIT_UNIX_BACKEND=wayland`.)

Then iterate — send input, observe, repeat:

```sh
emacsclient -s neomacs-agent --eval \
  '(progn (execute-kbd-macro (kbd "C-x b demo RET"))
          (neomacs--frame-snapshot))'
```

Input recipes (all drive the real command loop):

- `(execute-kbd-macro (kbd "..."))` — keystrokes, including prefix maps.
- `(setq unread-command-events (listify-key-sequence (kbd "...")))` —
  queue events for the main loop to consume (asynchronous variant).
- Mouse: synthesize event lists via `posn-at-x-y` /
  `(posn-at-point POS WINDOW)`.
- Escape hatch for the true native path (raw modifiers, IME, focus):
  `xdotool key --window <id> ctrl+x` under Xvfb / `wtype` under Wayland.
  Reach for it only when the elisp channel can't represent the input.

## Elisp introspection cookbook

The eval channel answers most "why" questions without the snapshot:

- `(window-tree)` / `(window-list)` — window layout structure.
- `(frame-parameters FRAME)` — chrome, geometry, z-order parameters.
- `(text-properties-at POS)` / `(overlays-in BEG END)` — why text displays
  the way it does.
- `(face-attribute FACE :foreground)` — face definitions (the snapshot
  shows *realized* colors).
- `(posn-at-x-y X Y)` — what is at a pixel; `(pos-visible-in-window-p POS
  WINDOW t)` — where a buffer position lands on screen.
- `(with-current-buffer "*Messages*" (buffer-string))` — recent messages.
- `(this-command-keys)` / `last-command` — what input actually arrived.

## In the GUI test harness

`neomacs-gui-tests` writes the snapshot beside its other artifacts:

```text
target/neomacs-gui-tests/<backend>/<scenario>.frame-snapshot.json
target/neomacs-gui-tests/<backend>/<scenario>.frame-snapshot.txt
```

The harness exports `NEOMACS_GUI_FRAME_SNAPSHOT_JSON` / `..._TXT` to the
fixture, which calls `neomacs--write-frame-snapshot` (see
`fixtures/startup-smoke.el`). Assert on those files, not on Lisp-side state:
the snapshot reports what redisplay produced, `gui-state.json` only reports
what the fixture intended. Run the real smoke with:

```sh
NEOMACS_GUI_TEST_BACKEND=x11 cargo nextest run -p neomacs-gui-tests \
  -E 'test(real_gui_smoke)' > tmp/gui-smoke.log 2>&1
```

(release binary and matching runtime required: `cargo xtask fresh-build --release`.)

### Org image scrolling regressions

The `real_gui_smoke::scrolling` tests exercise eight `C-v` commands followed
by eight `M-v` commands in an Org buffer with variable-height text and a
display-overlay image. One image fits in the window; another is taller than
the window and must exercise pixel scrolling. Both must return to
`window-start = 1`, with zero pixel offset, and stay there on repeated `M-v`.

```sh
NEOMACS_GUI_TEST_BACKEND=x11 cargo nextest run -p neomacs-gui-tests \
  --test real_gui_smoke -E 'test(scrolling::)'
```

Set `NEOMACS_GUI_TEST_BINARY` when the fresh-built executable is outside
`target/release/neomacs`. These tests run in the existing X11 GUI CI job;
they are not ignored. Their fixture needs no third-party Lisp packages or
network access.

The test driver acknowledges three presentations: the initial image, the
image scrolled out of view, and the image after scrolling back. It checks
both the frame's image glyph and the actual PNG pixels before allowing the
fixture to advance. This prevents a stale initial screenshot from passing
the return-to-top assertion. Polling has a deadline; it does not assume the
renderer finishes within a fixed sleep.

Each run preserves `initial`, `away`, and `returned` JSON/PNG artifacts and
a per-command point/start/pixel-offset trace under
`target/neomacs-gui-tests/{org-banner-scroll,tall-org-image-scroll}-<pid>/`.
The trace also detects scrolling away again after reaching the top.
