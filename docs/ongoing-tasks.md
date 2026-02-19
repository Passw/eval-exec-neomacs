# Ongoing NeoVM Tasks

This is an auto-updating log for incremental NeoVM rewrite slices that should keep landing in regular verified batches.

## Current focus areas
1. **Compatibility slices** – keep expanding `test/neovm/vm-compat` cases (window/frame semantics, timers, inputs, display primitives). Every addition must gate on `check-all-neovm` and matching Oracle TSVs.
2. **Rust core backend** – keep the Rust core behind the compile-time switch (`--with-neovm-core-backend=rust`), ensure `core-backend-emacs-c` remains the default, and document the split in `docs/elisp-vm-design.md`/`README.md`.
3. **Concurrency model** – reinforce the isolate-first scheduler (`neovm-worker`), message passing, and Specpdl isolation across isolates while keeping the host/VM boundary clean.
4. **Platform guidance** – expand README platform notes and link to issue #22 for macOS, plus consider creating future follow-ups for Windows/other hosts once a stable build path exists.

## Auto-exploration queue
These are the next candidate slices to explore automatically (an initial 20 tasks is already in `docs/plan.md`; keep advancing through them in order, re-checking Oracle behavior each time). Track progress with verified batches and push after each slice.

## Next actionable move
- Identify the next VM builtin still stubbed or drifting (the plan references `frame-terminal`, display/window, and stub enforcement). Implement a targeted lock-in slice with new corpus cases and `check-neovm` regression checks.
- Keep documenting the auto-progress in the plan (update `docs/plan.md` `## Doing` and `## Next`) each time a slice is completed.
- Track recent slices such as the `recent-keys` capture for `call-interactively`/`command-execute` so their documentation stays visible for observers and CI log correlation.
