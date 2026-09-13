# Asynchronous initial GPU acquisition

Work in progress on 2026-09-13. Device-loss recovery retains its existing
synchronous implementation; this slice changes initial acquisition only.

The native thread owns every window and surface. A worker constructs the
wgpu instance and enumerates adapters. The main thread creates the surface
and selects a compatible adapter; a second worker requests its device.
Completion wakes the event loop after publishing the result or disconnecting.
No worker holds a window, surface, or surface mutex. Cancellation drops its
receiver without joining a potentially blocked native driver operation.

The pinned dependencies are wgpu 30.0.1, wgpu-core 30.0.0, wgpu-hal 30.0.1,
and winit 0.31.0-beta.3 at dc7af19. Desktop Vulkan, Metal, DX12, EGL and WGL
ignore their enumeration surface hint. Enumeration followed by the same
surface-capability test therefore preserves their candidate set. This is a
property verified in the pinned source, not a WebGL contract. The small stable
ranking function follows wgpu-core: integrated before discrete for low power;
discrete before integrated for high performance; then Other, VirtualGpu, Cpu.
The None preference keeps enumeration order. There is no public wgpu selector
over an already enumerated vector; its utility helper would re-enumerate.
Sources: [wgpu-core selection](https://docs.rs/crate/wgpu-core/30.0.0/source/src/instance.rs),
[Vulkan HAL](https://docs.rs/crate/wgpu-hal/30.0.1/source/src/vulkan/instance.rs),
[Metal HAL](https://docs.rs/crate/wgpu-hal/30.0.1/source/src/metal/mod.rs),
[DX12 HAL](https://docs.rs/crate/wgpu-hal/30.0.1/source/src/dx12/instance.rs),
[EGL HAL](https://docs.rs/crate/wgpu-hal/30.0.1/source/src/gles/egl.rs),
[WGL HAL](https://docs.rs/crate/wgpu-hal/30.0.1/source/src/gles/wgl.rs).

wgpu-core retains the descriptor's owned display handle until after backend
instance destruction. The pinned winit OwnedDisplayHandle owns an independent
Arc and permits indefinite retention. This allows a cancelled worker to release
GPU objects safely after the event loop returns. Existing installed-GPU teardown
and its adapter-retention workaround remain unchanged. Pending native surfaces
are explicitly dropped before display teardown. Sources:
[wgpu instance ownership](https://docs.rs/crate/wgpu-core/30.0.0/source/src/instance.rs),
[winit owned display](https://github.com/eval-exec/winit/blob/dc7af19d15de375f980687f650174989f4b668a8/winit-core/src/event_loop/mod.rs).

Pending commands remain ordered until GPU installation; shutdown remains
observable. The evaluator preparation lifetime remains monitored through GPU
readiness. A zero-size native allocation retains the prepared GPU until a
nonzero size arrives. Installation reads and publishes current native geometry,
including events received while acquisition was pending. Surface loss cancels
the attempt synchronously and a later permission starts a fresh one.

Surface creation, capability checks, configuration and renderer construction
still run on the native thread. This change does not claim that every graphics
operation is nonblocking. Active post-installation suspension retains the
previous platform limitation; no new native presentation confirmation is added.

## Public regression and current evidence

The Linux process fixture points child-only VK_DRIVER_FILES at an owned `.json`
FIFO and selects Vulkan. The loader opens that manifest before checking size.
The test observes `wait_for_partner` in the child's procfs thread wait channels,
without opening a writer, then terminates only its private compositor. A writer
probe would release the blocked open and invalidate the test. Sources:
[loader driver overrides](https://github.com/KhronosGroup/Vulkan-Loader/blob/main/docs/LoaderDriverInterface.md#overriding-the-default-driver-discovery),
[manifest reader](https://github.com/KhronosGroup/Vulkan-Loader/blob/main/loader/loader_json.c),
[Linux FIFO open](https://github.com/torvalds/linux/blob/master/fs/pipe.c).

The regression failed on the synchronous implementation: native dispatch stayed
blocked for the five-second post-display-loss deadline. Evidence:
`target/diagnostics/issue-360/startup-fonts/gpu-discovery-display-loss-red.log`
and `target/neomacs-gui-tests/startup-pending-gpu-1342811/`.

The fixture also adds X11 close and resize/completion controls. Completion
releases the intentionally invalid first manifest and continues to a real ICD.
The first asynchronous build passed resize/completion but still hung on close
and display loss. The captured `gpu-exit-backtrace.log` proves native dispatch
had returned: libc exit entered `_dl_fini` and Vulkan `loader_release`, waiting
for the same mutex held by the worker blocked in manifest `fopen`.

Cancellation now records a process-lifetime witness, including an abandoned
attempt followed by a successful retry. After main-thread native cleanup,
Unix `_exit` or Windows `TerminateProcess` bypass foreign finalizers. An already
completed evaluator's status remains authoritative, including panic status 101.
Unexpected native startup interruption exits unsuccessfully without joining a
possibly blocked evaluator. A user close retains ordinary evaluator shutdown
semantics; this does not promise to interrupt arbitrary blocked Lisp code.
The helper avoids acquiring stdio locks during forced interruption. Sources:
[Linux libc _exit](https://man7.org/linux/man-pages/man2/_exit.2.html),
[Apple _exit](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/_exit.2.html),
[Windows DLL-detach deadlock](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-exitprocess),
[TerminateProcess](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-terminateprocess).

The next fresh build (fingerprint
`81CEBF4E7C3E6EC38B279ED8C8CA06A9B66F1930322C495ED816DB2E11BDA933`)
passed close (0.467s), display loss (0.243s), and resize/completion (1.012s).
See `initial-resource-gpu-native.log`. The persistent retry witness was added
after that build and awaits final integration. Zero-size, device-stage
cancellation, and suspension/retry were source-reviewed, not native-tested.
Runtime nextest previously passed 966 tests with 5 skipped.

Final integration uses fresh fingerprint
`B4260E90CA0EDF2B29B42B8D0CBA7D15A3C8F07500993E72E02CF80A0F0CF34F`,
including the persistent cancellation witness. All 63 selected GUI tests passed
with zero skips (`final-display-gui-integration.log`); pending-GPU close took
0.395s, display loss 0.193s and resize/completion 0.859s. Final runtime nextest
again passed 966 tests with five existing skips. The process tests retain emitted
Lisp output: the public printer flushes stdout at its write boundary.
