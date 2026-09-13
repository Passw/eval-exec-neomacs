# Initial native content geometry

The initial resource-font controls exposed two different native geometry
boundaries. Neither needs a hardcoded titlebar correction in Lisp.

## macOS overlay safe area

Native run 34755829943 opened Menlo 13 with a `664x574` surface, then reported
`664x542` editor content. Exact resource, family, realized font size, and
80-column checks passed. The ordinary native GUI test passed. Earlier Menlo 16
requested 723 pixels of height on a small hosted desktop; using a smaller font
removed screen-size ambiguity but preserved the 32-pixel difference.

Neomacs prepares an AppKit full-size content view with an overlay titlebar.
`WindowChromeController::insets` obtains the native safe area and content
observations subtract it. Active resize requests already added this native
space to editor dimensions. Initial and later window creation did not.
Creation now requests the measured full surface before GPU configuration,
using the same inverse `ContentInsets::surface_size` conversion. A deferred
native configure remains an observation when it arrives; no presentation
acknowledgment is inferred.

Pending GPU cancellation retains the last usable editor size, seeded at
creation and updated by native resize events. Zero or fully covered surfaces
cannot erase that size. Recreation therefore adds the next window's insets
once and converts scale once. Cancellation itself performs no native size
query: the first implementation reproduced an X11 `GetGeometry` error after
window destruction and aborted. The existing public close regression caught
this before publication (`insets-destroyed-window-exact-red.log`, artifacts
`startup-pending-gpu-close-window-2121338`).

All 1,724 protocol/runtime checks passed before the cancellation follow-up,
with five existing skips. Its focused last-usable-size control passed afterward.
The integrated revision, including the cancellation correction and updated
winit pin, passed 2,149 protocol/runtime, frame-resize, and bytecode checks
(`native-content-final-core.log`). Rebuilt verification is recorded below.

## Wayland inner size constraints

The Linux CLI control requested DejaVu Sans Mono 16 and a `1069x952` window.
Font metrics remained 13x25, but a later native configure reported height 937.
No Neomacs resize request intervened. Its native hints were minimum height 102
and increment 25, correctly aligned for 952.

Pinned winit stored minimum/maximum dimensions after adding CSD borders, but
used that stored minimum to snap inner sizes. Adwaita's 35-pixel titlebar made
the calculation `137 + floor((952 - 137) / 25) * 25 = 937`. Reloading the stored
constraints could also add the borders again. The fix retains inner dimensions
in winit's state and adds decorations only at the compositor request boundary.

The dependency fix is
[`f24b3339`](https://github.com/eval-exec/winit/commit/f24b33399b870c3632788b885a51b8c21f0f1b5d),
on `neomacs-inner-size-hints`, based directly on the previous pin `dc7af19`.
Its crate test and formatting passed. The strengthened Neomacs GUI regression
failed on the old pin with actual 937 versus expected 952
(`winit-inner-hints-red.log`). The rebuilt control passes with the new pin.

Relevant pinned source:
[Wayland size constraints and configure snapping](https://github.com/eval-exec/winit/blob/dc7af19d15de375f980687f650174989f4b668a8/winit-wayland/src/window/state.rs),
[AppKit content sizing and safe area](https://github.com/eval-exec/winit/blob/dc7af19d15de375f980687f650174989f4b668a8/winit-appkit/src/window_delegate.rs),
[X11 size query](https://github.com/eval-exec/winit/blob/dc7af19d15de375f980687f650174989f4b668a8/winit-x11/src/window.rs).

Logs are under `target/diagnostics/issue-360/startup-fonts/`; native artifacts
are retained in the corresponding `native-small-font-macos`,
`native-font-info-macos`, and `native-font-info-windows` directories.

## Integrated Linux verification

At pushed revision `202aa91e5`, `cargo xtask fresh-build --release` completed
including byte compilation, with executable/runtime fingerprint
`383AB771ABDFD8BC5FA7D2C6A0C9345C5028A498D01E0BD372B228404EDF79F8`.
All 63 selected GUI tests passed with zero skips in 83.866 seconds
(`native-content-final-gui.log`). The strict CLI font control now retains
`1069x952`; pending-GPU close, display loss, and hidden-child resize/completion
also pass. This verifies the correction of the teardown regression described
above. The 24 window-configuration/resize checks passed after integrating the
incoming accessor refactor (`native-content-rebase-window-core.log`).

## Native macOS verification

Run [34758601668](https://github.com/eval-exec/neomacs/actions/runs/34758601668)
passed at the same pushed revision: native adapter, 14 public Lisp contracts,
fresh release build, and both GUI tests (9.810 seconds, zero skips).
The first native request was `664x574` editor content. Measured safe-area
insets were `(0, 32, 0, 0)`, so creation reserved a `664x606` surface. Lisp
reported exactly `664x574`, Menlo 13, and 80 columns. Both initial dimension
assertions and the ordinary startup/font/resize control passed.

Artifacts and the complete job log are retained under `native-insets-macos`
and `native-insets-macos-job.log`. The original red artifacts remain available
for comparison; no expected dimensions or assertions were weakened.
