# Issue 391: pointer observations block native resize

## Status

[Issue 391](https://github.com/eval-exec/neomacs/issues/391) reports that on
macOS the native window grows but its editor content keeps the old extent
until a mouse click. The reported executable is
`35d75bef0821d1d6843a1e47fe288590fb5e4b83`.

The same resize-until-click symptom is now **reproduced on Linux/X11**,
first with a real Openbox border drag and then without a window manager:
move the pointer over the buffer, then resize. A click repairs it. No Doom
configuration, second frame, split window, or HiDPI display is required.

The Linux root cause is head-of-line blocking by a pointer-hit observation
in the shared VM frontend-event queue. No production fix has been made.
The macOS path has not been run locally; the shared cause is consistent
with the reporter's trace, not yet a native macOS verification.

## Confirmed Linux reproduction and cause

The important missing condition in the initial passing tests was **pointer
motion before resize**. A real native-window drag introduced that condition.
It was then reduced to a bare Xvfb test with just one empty editor window.

`input_bridge.rs::convert_positioned_pointer_input` emits a
`PresentedRegion` observation before the mouse-movement action. In
`frontend_events.rs::semantics`, that observation is classified as
`LispSpecial`, with `PendingPolicy::Never` and `wait_special = false`.
This combination strands it during an idle input wait:

1. `sync_pending_resize_events_in_keyboard_runtime` stops at this queue
   head; later resize events cannot pass it.
2. `take_next_wait_request_special_input_event` cannot service it because
   it is neither internal nor wait-special.
3. Pending-input detection also returns false: the observation and resize
   are not command input, and movement is not command input with the normal
   `track-mouse = nil` setting.
4. A click finally counts as pending command input. The ordinary read-char
   path drains the observation and the events behind it, allowing resize
   and redisplay to catch up.

```text
Pointer moves over a presented buffer
  └─ queue: PresentedRegion → MouseMove → Resize → Resize …
             └─ neither serviced during wait nor considered pending input
                 └─ evaluator waits; native surface grows, content stays old
Click arrives
  └─ command-input wait completes → queue drains → content resizes
```

A debugger capture of the frozen process confirms that the evaluator is
blocked in `Poller::wait` through `wait_reading_process_output`, not busy
laying out the resized frame. No additional frame is published during the
freeze. This supports the queue-blocking explanation rather than a stale
GPU presentation of an already-current layout.

| Experiment | Result |
| --- | --- |
| Neomacs, Openbox border drag, split window | 3/3 freeze; a click repairs all three. |
| Neomacs, same drag, single window | 3/3 freeze; a click repairs all three. |
| GNU Emacs 31.1, same single-window drag | 3/3 pass without a click. |
| Neomacs, pointer motion then resize on bare Xvfb | 3/3 freeze; a click repairs all three. |
| Neomacs, same bare-Xvfb sequence with `track-mouse = t` | 3/3 pass: movement now counts as command input and unblocks the queue. This is a diagnostic control, not a proposed fix. |
| GNU Emacs 31.1, same bare-Xvfb sequence | 3/3 pass without a click. |

In the minimal failing case, the native surface reaches `1100x760` while
content remains at `664x646`. The bottom mode-line stays at rows 613–627;
after a click it moves to rows 727–741, spanning the new width.

The test `pointer_motion_does_not_block_idle_native_resize` preserves the
minimal sequence and fails on the unmodified release executable (3/3;
`tmp/issue-391-minimal-regression.log`). Its final GNU control run passed
twice and had one X-server startup failure before a window opened
(`tmp/issue-391-minimal-gnu-control.log`), separate from the earlier 3/3
passing GNU diagnostic run.
It is explicitly ignored while the bug is unfixed, so diagnosis-only work
does not silently turn the normal CI suite red. Run it deliberately:

```sh
NEOMACS_GUI_TEST_BACKEND=x11 \
NEOMACS_GUI_TEST_BINARY="$PWD/target/release/neomacs" \
cargo nextest run -p neomacs-gui-tests --test resize_presentation \
  -E 'test(pointer_motion_does_not_block_idle_native_resize)' \
  --run-ignored all --test-threads 1
```

Set the matching `NEOMACS_RUNTIME_ROOT` if using an isolated fresh build.
The retained test sends no click, so it cannot rescue its own failure.
Diagnostic click-recovery images, logs, and the temporary native-drag probe
are under `tmp/issue-391-*` and `target/neomacs-gui-tests/resize-*/`.

The ordinary three-test resize suite subsequently passed, with the known
failing regression skipped (`tmp/issue-391-normal-suite-after-repro-all.log`).
An earlier ordinary-suite attempt stalled at the penultimate size, without
logging the final native `SurfaceResized` notification. Keep that separate
from the deterministic hover reproduction, in which all resize events
reach the bridge. Both attempts are retained; this is not a stability claim
for the whole GUI suite.

## Evidence from the reporter

The attached debug log contains ANSI escapes and NUL bytes. Text searches
must strip those or explicitly permit binary input; otherwise they miss the
resize sequence.

| Time (UTC, September 15) | Observation |
| --- | --- |
| 16:06:07.519–07.726 | AppKit reports native resizes, Metal surfaces reconfigure, and the input bridge translates eight resize events for frame `4294967296`, scale `2.0`. |
| 16:06:07.726399 | The last translated size is `730x636` logical pixels. |
| 16:06:07.830385 | The command loop logs `redisplay skipped: visible state unchanged`. |
| 16:06:09.187747 | Positioned pointer input arrives. |
| 16:06:09.188607 | Redisplay runs; subsequent layout records use the new width and height. |

This rules out the simplest explanation that AppKit never delivers the
resize to the bridge. It does **not** prove where VM geometry, redisplay
eligibility, and layout publication diverge: the bridge log is not an
acknowledgement that the VM applied the event. Surface reconfiguration is
also not evidence of a new editor display snapshot.

The recording contains an empty scratch buffer, a second warnings window,
and a Doom after-init error about `doom-modules`. These distinguish the
reported environment from `-Q`; none is established as the cause.

## GNU Emacs reference

Studied `src/nsterm.m`, `resizeWithOldSuperviewSize:` in the local GNU Emacs
source before changing the test. GNU updates the view bounds, calls
`change_frame_size`, marks the frame garbaged, cancels mouse-face state, and
calls `ns_send_appdefined(-1)` to wake its event loop.

Also checked `src/xterm.c`, the non-toolkit `ConfigureNotify` handler:
it changes frame size, marks the frame garbaged, and cancels mouse-face
state while handling native events. The comment there identifies the
toolkit widget resize handler as the corresponding owner in toolkit builds.

The behavioral contract is that native resize independently advances editor
geometry and redisplay. A later editing command or mouse click must not be
necessary. Neomacs has separate VM/layout and native presentation owners;
the test must observe rendered content, not only native surface size.

## Initial GUI coverage and why it missed the bug

`crates/neomacs-gui-tests/tests/resize_presentation.rs` now covers:

- The existing startup-text resize/old-presentation ghosting case.
- An idle empty buffer receiving a burst of native resizes without input.
- The same idle resize with two vertically split editor windows.

The idle cases disable cursor blinking and announce readiness from a
one-shot idle callback. After that callback returns, the external X11
driver resizes the native window. There is no repeating Lisp observer,
keypress, mouse click, or explicit Lisp redisplay call to rescue a missed
update. A screenshot must show the bottom mode line spanning the new width
at the same inset from the new bottom. Each test process retains separate
before/after screenshots and editor logs.

The original no-red-at-old-rows assertion remains appropriate for the
single-window ghosting test, but is not used for split windows: a correctly
moved mode line can overlap another mode line's old position.

Run against a matching fresh-build release executable and runtime:

```sh
NEOMACS_GUI_TEST_BACKEND=x11 \
NEOMACS_GUI_TEST_BINARY="$PWD/target/release/neomacs" \
cargo nextest run -p neomacs-gui-tests --test resize_presentation \
  --test-threads 1
```

If the fresh build used an isolated runtime root, set
`NEOMACS_RUNTIME_ROOT` to that same directory. For a 2x-scale control, set
`WINIT_X11_SCALE_FACTOR=2` and filter with `-E 'test(idle_)'`. For a GNU
Emacs control, set `NEOMACS_GUI_TEST_BINARY` to the GNU GUI executable and
use the same idle-test filter. The fixture requests pixelwise resizing so
GNU's character-grid snapping does not invalidate a pixel-size assertion.

These initial cases contain no pointer-motion event before the resize and
therefore miss the blocking observation. The new pointer-motion regression
adds that condition. Tests require X11 and skip other native backends;
they do not certify the macOS native backend. During investigation, some runs could not
connect to the X server before opening a window; those startup failures are
not resize reproductions.

Validation against the fresh-build release executable at `c17acb4d9c`:

| Run | Result |
| --- | --- |
| New idle tests, X11 at 1x, five repeats each | 10/10 passed. |
| New idle tests, X11 at 2x, three repeats each | 6/6 passed. |
| Existing startup ghosting test, five repeats | 3 passed; 2 failed before a window opened, with an X-server connection error. |
| GNU Emacs 31.1, both idle tests, three repeats each | 5 passed; 1 failed before a window opened. An earlier split-only control passed 3/3. |

Final run logs are `tmp/issue-391-final-{x11,hidpi,gnu}.log`. These results
do not establish a clean full-suite pass or a macOS reproduction.

## Fix direction and remaining verification

Service pointer observations in the same ordered VM-owned event queue
during waits, without treating the observations as Lisp commands. Keep
presentation identity validation and the observation-before-action order.
The semantic classification should make it difficult to add another event
that neither completes a wait nor can be serviced within it. Do not work
around this by polling redisplay, adding a second queue, or enabling mouse
tracking globally.

This fix has not been implemented. After it is implemented, enable the
regression by default, rerun the drag and GNU controls, and verify native
macOS resizing. Reuse the visual contract; do not replace native resizing
with `set-frame-size` in a Lisp polling timer, which can conceal the symptom.

The existing manually dispatched native-display workflow provides a
possible macOS runner, but its Lisp-driven resize checks do not yet drive
this external native-resize scenario. It has not been dispatched for this
investigation.

No PR or issue comments were posted.
