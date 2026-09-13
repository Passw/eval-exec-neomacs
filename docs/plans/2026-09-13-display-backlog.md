# Remaining display work

The user authorized all four remaining handoff tasks and the additional known
gaps on 2026-09-13, after startup commit `084aaec74` was pushed. This record
tracks the full scope across implementation slices; an unchecked item is not
a claim of completion.

## Work and evidence

- [x] Submission terminology: popup state, video surface evidence, internal
  frame counters; retain existing exported diagnostic/log fields.
- [ ] Platform validation: portable compile/policy checks and actual macOS and
  Windows execution using repository CI runners; strengthen native GUI controls.
- [ ] GNU platform resources: Windows registry precedence and Cocoa user
  defaults, including initial font policy and public resource lookup.
  Public queries, later-frame font precedence, and first-native-window resource
  font discovery are implemented with winreg and objc2-foundation. Native CI
  validation of the first allocation remains pending.
- [x] Weston cached subsurface scale/detach diagnostic: reproduce on stock
  Weston, explain against official protocol/source, correct Neomacs only if
  the evidence identifies a client contract violation. Investigate independent
  unexplained peer resets separately.
- [x] Asynchronous initial GPU adapter/device acquisition: native dispatch must
  remain responsive while acquisition is pending; preserve main-thread window
  ownership and failure propagation.
- [x] macOS/Windows live-font behavior: establish GNU/platform capabilities.
  A user question is pending because GNU has no Linux-style system-monospace
  subscription on these backends; a Neomacs-specific preference would be a
  separate interface decision, not inferred from nonexistent OS settings.
  Research established no corresponding GNU subscription. Preserve that behavior
  unless the user defines a Neomacs-specific preference source.
- [x] Grown-minibuffer frame-height accounting: GNU differential regression and
  correction through public Lisp/native-host geometry observations.
- [x] Child-frame initial width with chrome: reproduce without suppressing
  chrome, compare GNU, correct initial geometry ownership.
- [x] GNU split-window minimum-size overrides: pin the actual font-change
  contract and retain native presented geometry evidence.
- [x] Overlapping resize observations: exercise multiple outstanding
  requests and stale native observations without losing newest intent.

## Working constraints

Use the supplied GNU checkout read-only. Tests use the already approved public
GUI/process, Lisp, and native-host interfaces; Rust tests stay in tracked test
files. Use cargo nextest, not cargo test. All Neomacs rebuilds use
`cargo xtask fresh-build`, including matching generated runtime artifacts.
Desktop-setting changes are confined to test-local settings or ephemeral CI.
Use stock compositors, without production delays, patched Weston, or invented
native presentation confirmations. Preserve exported log compatibility.
Commit independently reviewable slices and perform final integration checks.

## Verification checkpoints

The following entries preserve earlier checkpoints and failure evidence.
The last entry records the latest result.

The research skill runs independent primary-source investigations for native
resources/settings and the stock Weston diagnostic. Existing macOS fresh-build
workflow has been dispatched for a baseline. Current CI also has Windows
runners; native GUI validation will use an explicit display contract rather
than treating an installer or a cross-compile as display verification.

The resource slice passed 20 public evaluator/frame-creation tests and Linux
application/runtime/GUI-harness checks. Review found and corrected first-entry
alist precedence and normalization through the current runtime binding.
Native adapter tests and the manual `native-display-contract.yml` workflow
cover platform databases plus actual GUI font/resize behavior. Native execution
is pending; adapters are not yet claimed verified on their target systems.
The first macOS run compiled the adapter, then exposed a fixture error: a
custom volatile domain was not searched. The fixture now uses NSArgumentDomain
and restores it afterward; numeric values and custom resource classes are also
covered. A native rerun is required.

The geometry slice corrects grown-minibuffer frame-height accounting, child
initial fringe/border allocation, and per-axis font-change minimum overrides.
GNU oracles passed the child-chrome, frame-height, and split-minimum scenarios.
After review, Lisp minimum-policy nonlocal exits propagate rather than being
swallowed; its regression failed before the fix. The 24 selected frame creation
and resize tests passed (`geometry-reviewed-green.log`). LiveFontCase uses
strum::AsRefStr with kebab-case names. Neomacs GUI validation awaits a fresh build;
deliberately grown native minibuffer and child scrollbar/different-font controls
still need coverage. No native-presentation claim follows from core tests.

The fresh geometry build at `e54acbbd2` completed, including byte compilation
and matching runtime images (fingerprint
`73205D67B43468F5E5175587174D3C6DAC60FFDAB3775E5D2EDBC6E725296F1C`).
The expanded core run passed 107 tests. The serial GUI run passed 55/57:
GNU and Neomacs both passed deliberate three-row minibuffer, initial child
fringe/border, and horizontal/vertical split-minimum cases. A child-scrollbar
control exposed the still-zero `frame-scroll-bar-width` stub; the getter now
uses the same scrollbar area calculation as allocation, and its core control
passes. That fix awaits the next fresh binary. The other failure was a native
smoke compositor peer reset; the unchanged test passed alone. Preserve the
original failure evidence rather than describing the entire run as passing.

Native CI run 34750377073 passed adapters on macOS and Windows. macOS also
passed public Lisp contracts and entered its full build; Windows was compiling
those contracts at this checkpoint. Native GUI completion is still pending.

Asynchronous GPU acquisition is implemented but not yet native-verified.
The public Vulkan-driver FIFO regression first reproduced blocked native
dispatch after private-compositor loss. Review corrected pending zero-size,
native geometry reconciliation, close status, and explicit surface teardown.
Runtime nextest passed 966 tests (5 skipped); runtime/harness checks passed.

The next fresh build, fingerprint
`81CEBF4E7C3E6EC38B279ED8C8CA06A9B66F1930322C495ED816DB2E11BDA933`,
passed six focused native checks (`initial-resource-gpu-native.log`): explicit
command-line font at first allocation, both GNU and Neomacs child scrollbar
geometry, blocked-GPU close, display loss, and resize/completion. The GPU exit
fix was required because normal process finalizers re-entered Vulkan's blocked
loader mutex after native cleanup. A persistent cancellation witness now also
covers a successful retry after cancellation; final rebuild is still required.

First-window font preparation now observes GUI options in the existing argv
pass, shares option classification with GNU-style sorting, and forwards all
original Lisp arguments. Explicit font wins over native resource font, then
the ordinary platform candidates apply. The same native font opener supplies
the first allocation and installed font. No Lisp startup/early-init ordering
is moved: early-init frame alists still run after this architecture's initial
native window. The 44 startup-argument tests passed. New ephemeral native CI
checks verify the exact resource, realized size against an explicitly opened
control, and both dimensions of the first allocation.

macOS run 34750377073 completed successfully. Windows run 34749688492 also
completed successfully, including native GUI startup/font/resize. The newer
Windows job and updated resource-font CI validation remain pending.

The overlapping-resize regression reproduced loss of 101-column intent after
an earlier 91-column allocation. Separating requested intent from observations
passed 81 selected core checks and 62 resize integration checks. GNU's native
stress scenario passed. Review added queued input/focus/key/second-frame and
position-only controls; Neomacs native validation awaits the final build.

The final fresh build completed with matching runtime image and byte compilation
(fingerprint `B4260E90CA0EDF2B29B42B8D0CBA7D15A3C8F07500993E72E02CF80A0F0CF34F`).
The complete selected GUI integration run passed **63/63, zero skipped** in
71.544 seconds (`final-display-gui-integration.log`), including GNU and Neomacs
overlap, scrollbar, grown-minibuffer and minimum cases, startup fonts, native
smoke, and pending-GPU close/display-loss/resize controls. Final runtime nextest
passed 966 tests with five existing skips. The reviewed input-queue/position
fix passed 62 resize checks, and all 16 frame-parameter/position controls passed.

Implementation is pushed through `eab4a24fe` in three reviewable commits. Updated
native CI run 34753154974 verifies that source; its resource-font checks remain
pending. No Linux success is being substituted for those native platform results.
Both jobs in run 34753154974 passed native adapters and public Lisp contracts;
their fresh release builds are running before the resource-font GUI checks.

A strengthened pending-GPU regression found that selecting a hidden child
before the initial primary resize caused frame ID zero to target that child.
The input bridge now resolves zero to the primary Lisp frame before queueing.
The exact red control and source review are complete. Fresh fingerprint
`74C099BA5F5155879D462B99C0540833F0760B0C0558E48C4EDBB3991460ED41`
passed the corrected control and all 63 selected GUI tests with zero skips
(`primary-reviewed-gui-integration.log`, 80.112 seconds). This executable
predates the three incoming VM/JIT commits through `8195d4b69`.
See [GPU startup design and evidence](../diagnostics/2026-09-13-async-gpu-startup.md).

The primary-routing fix is pushed as `b0a3da856` after a conflict-free rebase.
All 425 selected resize/bytecode tests passed. A new full fresh build at that
commit produced fingerprint
`7CED3E6481FA9383CA3895CE7BCD63F34C22174C65A31B9BE6A0B9138690E8E0`;
all 63 GUI tests passed with zero skips in 81.796 seconds
(`primary-rebase-gui-integration.log`).

Native run 34753154974 completed its macOS build but exposed an invalid fixture
call: `open-font` requires a font entity, not a font specification. The fixture
now compares realized pixel sizes through `font-info`, whose named-font path
uses the existing opener. Standalone `open-font` remains a separate stub. The
same public control passed in the Linux command-line font fixture
(`native-resource-font-info-control.log`). Original macOS logs and artifacts
are retained under `native-resource-macos`; a corrected native run is required.
