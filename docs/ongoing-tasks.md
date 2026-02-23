# Ongoing NeoVM Tasks

This is an auto-updating log for incremental NeoVM rewrite slices that should keep landing in regular verified batches.

## Current focus areas
1. **Compatibility slices** – keep expanding `test/neovm/vm-compat` cases (window/frame semantics, timers, inputs, display primitives). Every addition must gate on `check-all-neovm` and matching Oracle TSVs.
2. **Rust core backend** – keep `core-backend-rust` as the default path, use `core-backend-emacs-c` only as a short-lived rollback switch, and remove fallback paths as tracker rows reach `rust-default`/`c-removed`.
3. **Concurrency model** – reinforce the isolate-first scheduler (`neovm-worker`), message passing, and Specpdl isolation across isolates while keeping the host/VM boundary clean.
4. **Platform guidance** – expand README platform notes and link to issue #22 for macOS, plus consider creating future follow-ups for Windows/other hosts once a stable build path exists.

## Auto-exploration queue
These are the next candidate slices to explore automatically (an initial 20 tasks is already in `docs/neomacs-direct-c-to-rust-plan.md`; keep advancing through them in order, re-checking Oracle behavior each time). Track progress with verified batches and push after each slice.

## Next actionable move
- Identify the next VM builtin still stubbed or drifting (the plan references process/file wrappers, display/window, and stub enforcement). Implement a targeted lock-in slice with new corpus cases and `check-neovm` regression checks.
- Recent completed slice: `start-process` now matches oracle buffer/program/name/arg contracts (including buffer-object designators and strict arg typing), locked by `cases/start-process-buffer-and-type-contract-semantics`.
- Recent completed slice: `call-process`/`call-process-region`/`start-file-process` now enforce oracle string contracts (with `PROGRAM=nil` preserved where required), locked by `cases/call-process-start-file-process-string-contract-semantics`.
- Recent completed slice: `accept-process-output` now matches oracle millisecond/fixnum contract and error payload order, locked by `cases/accept-process-output-millis-semantics`.
- Recent completed slice: stale process mutators now match oracle for control-surface setters (`set-process-*`, `process-put`, `process-get`), locked by `cases/process-stale-mutator-semantics`.
- Recent completed slice: stale process control operations now match oracle inactive-process signaling behavior for `continue/interrupt/kill/stop/quit-process`, locked by `cases/process-stale-control-semantics`.
- Recent completed slice: `set-binary-mode` now matches oracle unsupported-stream errors (`(error "unsupported stream" <sym>)`) via `cases/file-runtime-wrapper-semantics`.
- Recent completed slice: `frame-edges` now preserves oracle-style live-window buffer context in `is not a live frame` messages, with `cases/frame-edges-window-message-semantics` added and gated.
- Recent completed slice: added dead-window payload lock-ins for `frame-terminal`/`terminal-name`/`tty-type`/`controlling-tty-p`/`suspend-tty`/`resume-tty` via `cases/terminal-dead-window-error-payload-semantics`.
- Recent completed slice: aligned vm-compat runner case serialization with oracle (`elisp_compat_runner` now escapes only newline/carriage-return/tab, not backslashes), unblocking strict-form parity for `cases/documentation-property-semantics`.
- Recent completed slice: aligned startup doc seeds for `use-system-tooltips` and `ctl-x-4-map` with oracle-facing `documentation-property` behavior, and refreshed `cases/documentation-property-semantics.expected.tsv` to lock the corrected tuples.
- Recent completed slice: hardened vm-compat runner emission to preserve raw result bytes and oracle-equivalent control-escape forms (including `\\t`/`\\n` strict-form parity), plus strengthened the `use-system-tooltips` prefix probe back to an alignment assertion.
- Keep documenting the auto-progress in the plan (update `docs/neomacs-direct-c-to-rust-plan.md` `## Doing` and `## Next`) each time a slice is completed.
- Track recent slices such as the `recent-keys` capture for `call-interactively`/`command-execute` so their documentation stays visible for observers and CI log correlation.
- Refer to `docs/neovm-subsystem-porting.md` when choosing the next subsystem to port; it lists the untracked Rust modules and the gated steps to bring each online.
