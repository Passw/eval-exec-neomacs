# Issue 391: pointer observations and filtered focus events block native resize

## Status

[Issue 391](https://github.com/eval-exec/neomacs/issues/391) reports that on
macOS the native window grows but its editor content keeps the old extent
until a mouse click. The reported executable is
`35d75bef0821d1d6843a1e47fe288590fb5e4b83`.

The same resize-until-click symptom is now **reproduced on Linux/X11**,
first with a real Openbox border drag and then without a window manager:
move the pointer over the buffer, then resize. A click repairs it. No Doom
configuration, second frame, split window, or HiDPI display is required.

Linux investigation found two head-of-line blockers in the shared VM
frontend-event queue: pointer-hit observations that were not serviced during
waits, and focus events incorrectly hidden from the command reader by its
pending-input filter. Both shared paths have fixes; validation is recorded below.
A rarer Linux/X11 stall before native resize delivery remains under
investigation. The issue is therefore not claimed fully resolved on every path.
The macOS path has not been run locally; the shared cause is consistent
with the reporter's trace, not yet a native macOS verification.

## Confirmed Linux reproduction and cause

The important missing condition in the initial passing tests was **pointer
motion before resize**. A real native-window drag introduced that condition.
It was then reduced to a bare Xvfb test with just one empty editor window.

`input_bridge.rs::convert_positioned_pointer_input` emits a
`PresentedRegion` observation before the mouse-movement action. In
the original `frontend_events.rs::semantics`, that observation was classified as
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
It was initially ignored during diagnosis-only work. The fix enables it in
the normal GUI suite. Run it directly:

```sh
NEOMACS_GUI_TEST_BACKEND=x11 \
NEOMACS_GUI_TEST_BINARY="$PWD/target/release/neomacs" \
cargo nextest run -p neomacs-gui-tests --test resize_presentation \
  -E 'test(pointer_motion_does_not_block_idle_native_resize)' \
  --test-threads 1
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

## Second blocker: focus filtering is not reader readiness

The pointer-observation fix alone passed the minimal pointer regression,
but **did not fix the original native border drag**. The retained failing
trace showed this ordered sequence:

```text
WindowFocus(false) → Resize → PresentationActivated → PresentationRetired
                  → Resize → Resize … → WindowFocus(true)
```

The legacy resize synchronizer could defer the leading focus event long
enough to apply the first resize, then stopped at the presentation
acknowledgement. The focus event remained at the queue head. The command
wait incorrectly used the same configured filter as `input-pending-p`, so
the default `while-no-input-ignore-events` prevented focus from waking
`read_char`. The resulting window froze at the first intermediate size.

The second minimal regression, `focus_change_does_not_block_idle_native_resize`,
uses bare Xvfb: give the native window focus, then resize it while the editor
is idle. It needs neither pointer motion nor a window manager.

| Experiment | Result |
| --- | --- |
| Pointer fix only, focus then resize | 3/3 fail (`tmp/issue-391-focus-red.log`). |
| Same executable, diagnostic `while-no-input-ignore-events = nil` | 3/3 pass (`tmp/issue-391-focus-unfiltered-control.log`). |
| GNU Emacs 31.1, same native focus/resize sequence | 3/3 pass (`tmp/issue-391-focus-gnu-control.log`). |

Clearing the ignore list is only a control, not the fix. GNU
`process.c::wait_reading_process_output` uses
`keyboard.c::detect_input_pending_run_timers`, which calls
`get_input_pending(READABLE_EVENTS_DO_TIMERS_NOW)` **without**
`READABLE_EVENTS_FILTER_EVENTS`. The comments at `readable_events` distinguish
this ordinary reader-readiness query from a filtered `input-pending-p` query.

Neomacs now encodes the distinction as `FrontendInputQuery`:
`Readable` for command reads, `Pending(filter)` for pending-input queries.
`KeyboardWaitPolicy` selects that operation explicitly. This preserves
`input-pending-p` and `while-no-input` filtering instead of changing focus
hooks, making focus an internal event, or bypassing FIFO ordering. The
Android/Wasm branch's portable command reader uses the same `Readable` query.

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
- Pointer motion followed by idle resize, without a rescuing click.
- Native focus followed by idle resize, with the default input ignore list.

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

## Fix and remaining verification

Pointer observations now use `InternalFrontendEvent` in the same ordered
VM-owned event queue during waits. They are not Lisp commands. The dispatcher
retains presentation identity validation and observation-before-action order;
the observation itself neither resets idle time nor requests redisplay.
There is no redisplay polling, second queue, or global mouse-tracking change.

The policy is now an enum rather than independent class/pending/wait flags.
An unconditionally non-command event must carry its internal service action
or select wait servicing. Mouse motion explicitly alternates between readable
input and wait servicing according to `track-mouse`. Filterable Lisp special
events retain GNU's existing behavior. Internal classification constructs the
action directly, eliminating the separate wildcard-based conversion; the VM
dispatcher exhaustively handles each action on the Lisp thread.

Additional GNU study covered `keyboard.c::some_mouse_moved`, `readable_events`,
and `process_special_events`: ordinary motion is not readable input unless
tracking is enabled, and non-user-visible work is serviced separately from
command reads. GNU has no Neomacs presentation-observation transport; this
is an adaptation of that separation, not a claim of identical event storage.

Before production edits, the committed GUI regression failed again 3/3
(`tmp/issue-391-fix-red.log`), with exactly the old mode-line position. The
first internal-event fix passed all 76 focused keyboard/event tests. Existing
pointer tests now feed observations through the ordered read interface instead
of bypassing it with the raw Lisp-event handler.

The first fix passed the full VM suite: 10,023 passed, 53 skipped
(`tmp/issue-391-core-full.log`). Its fresh release passed all seven pointer
regression attempts that opened a window; three further attempts failed
before startup with `Failed to open connection to X server`
(`tmp/issue-391-fix-green.log`). At 2x scale, two passed and one failed at
the same startup step (`tmp/issue-391-fix-hidpi.log`). These are not clean
stress-run passes. The original border-drag failures motivated the second
fix above rather than treating the minimal regression as sufficient.

The second fix passes 148 focused input/wait tests, including focus,
`while-no-input`, input filtering and presentation ordering
(`tmp/issue-391-focus-core-focused.log`). The full VM suite also passes:
10,023 passed, 53 skipped (`tmp/issue-391-core-final.log`).

The rebased Android/Wasm branch passes 164 focused core/session tests
(`tmp/issue-391-branch-focused-runtime.log`) and
`cargo check -p neovm-core --no-default-features --target wasm32-unknown-unknown`
(`tmp/issue-391-branch-wasm-check.log`). The branch's first test attempt
lacked generated runtime assets in its new worktree; the passing run sets
`NEOMACS_RUNTIME_ROOT` to the existing complete main-worktree resources.
Verification also exposed pre-existing references to removed stack-growth
constants in the branch's bytecode fast path; a separate branch commit
uses its existing shared `stack_growth::should_probe` policy. These checks
do not constitute an Android device or browser GUI run.

`cargo xtask fresh-build --release --low-memory --runtime-root
"$PWD/tmp/issue-391-fixed-runtime"` completed successfully, including runtime
generation and byte-compilation (`tmp/issue-391-fresh-build-final.log`).
The original Openbox native border-drag probe then passed 6/6: three
single-window and three split-window runs, with no rescuing click
(`tmp/issue-391-final-native-drag{,-2,-3}.log`).

Initial final-release repetitions had no resize failures: 17/18 normal GUI
runs and 5/6 2x-scale runs passed; the other two failed before any window
opened with `Failed to open connection to X server`
(`tmp/issue-391-final-gui{,-hidpi}.log`).

The separate harness investigation found that Xvfb reset whenever its last
client disconnected. Short-lived `xdotool` probes during editor startup
could therefore reset the server while the editor connected. The existing
real X11 session contract now asserts that a root-window property survives
between disconnected clients. It failed before the change, reporting
`NEOMACS_GUI_SESSION: no such atom on any window`
(`tmp/issue-391-x11-lifetime-red.log`). The owned server now uses `-noreset`:
the Rust `DisplaySession` still terminates/reaps it and removes its exact
owned resources on drop, while individual probe lifetimes no longer reset
the session. That contract passes 20/20 repeats
(`tmp/issue-391-x11-lifetime-green.log`). No editor production behavior or
resize assertion was changed for this harness fix.

With resets disabled, the normal GUI tests passed 29/30 and the new
pointer/focus cases at 2x scale passed 10/10
(`tmp/issue-391-final-gui{,-hidpi}-stable.log`). The remaining ordinary idle
case reached `1100x760` but retained content at the penultimate `1045x745`
size; its log lacks the final `SurfaceResized` callback. An earlier
pre-fix ordinary-suite run showed the same distinct symptom. This is not
evidence of either confirmed VM queue blocker. GNU passed that bare-Xvfb
idle-burst scenario 10/10 with resets disabled
(`tmp/issue-391-final-gnu-idle-stable.log`).

Further diagnostic repetitions found a second harness startup defect: a
TCP-ready port could belong to another live session. An added contract
starts two sessions in the same process (therefore the same initial
display-number candidate); it failed because both reported the same display
(`tmp/issue-391-x11-owner-red.log`). Readiness now requires `xdpyinfo` to
complete an authenticated connection with this session's new cookie, rather
than accepting any TCP listener. Each probe is bounded and reaped, and a
failed candidate cleans up before another display is tried. All 16 harness
tests pass across 20 repetitions (`tmp/issue-391-x11-owner-green.log`).

The final authenticated-harness GUI run passes 30/30: five repetitions of
all five resize cases and native focus/typing routing
(`tmp/issue-391-final-gui-auth.log`). However, a separate diagnostic idle-burst
loop subsequently reproduced the penultimate-size stall on attempt 8
(`tmp/issue-391-idle-native-event-auth-8.log`). Both the native event loop
(`calloop`/`polling::Poller::wait`) and evaluator are waiting; the input bridge
is waiting on its channel. The native thread is not stuck inside GPU rendering.
The log has resize callbacks through `1045x745`, but none for `1100x760`,
while the external capture is already `1100x760`. The retained debugger dump
is `target/neomacs-gui-tests/resize-2443843/linux-x11/idle-resize-presentation.gdb.txt`.
This remains a separate unresolved native event-delivery observation, not a
clean full-path stability result. No callback polling workaround has been added.

Native
macOS resizing still requires verification on macOS. Reuse the visual
contract; do not replace native resizing with `set-frame-size` in a Lisp
polling timer, which can conceal the symptom.

The existing manually dispatched native-display workflow provides a
possible macOS runner, but its Lisp-driven resize checks do not yet drive
this external native-resize scenario. It has not been dispatched for this
investigation.

No PR or issue comments were posted.
