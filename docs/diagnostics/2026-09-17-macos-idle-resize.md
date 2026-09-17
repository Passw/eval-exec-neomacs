# Issue 391: native resize without follow-up input

## Status

[Issue 391](https://github.com/eval-exec/neomacs/issues/391) reports that on
macOS the native window grows but its editor content keeps the old extent
until a mouse click. The reported executable is
`35d75bef0821d1d6843a1e47fe288590fb5e4b83`.

The recording confirms that symptom. It has **not been reproduced locally**
on Linux/X11, and no production fix has been made. The exact root cause is
not established. Passing X11 coverage does not clear a macOS live-resize bug.

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

The behavioral contract is that native resize independently advances editor
geometry and redisplay. A later editing command or mouse click must not be
necessary. Neomacs has separate VM/layout and native presentation owners;
the test must observe rendered content, not only native surface size.

## GUI regression coverage

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

The tests currently require X11 and skip other native backends. They are
coverage of the required behavior, **not a demonstrated failing regression
for the reported macOS bug**. During investigation, some runs could not
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

## Remaining diagnosis

A macOS machine/session is required to reproduce the native live-resize
path. First compare `-Q` with the reporter's configuration, then distinguish
four boundaries: bridge receipt, VM geometry update, redisplay decision,
and publication/presentation of the new layout. Reuse the visual contract;
do not replace native resizing with `set-frame-size` in a Lisp polling
timer, which takes a different path and can conceal the symptom.

The existing manually dispatched native-display workflow provides a
possible macOS runner, but its Lisp-driven resize checks do not yet drive
this external native-resize scenario. It has not been dispatched for this
investigation.

No PR or issue comments were posted.
