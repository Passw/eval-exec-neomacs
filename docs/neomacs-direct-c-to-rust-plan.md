# Neomacs Direct C-to-Rust Rewrite Plan

Last updated: 2026-02-23

## Goal

Replace Emacs C core behavior used by Neomacs with Rust implementations directly, then run Neomacs on Rust core paths by default.

## Scope

- In scope: `src/*.c` core behavior replacement in Rust.
- Out of scope: rewriting official `lisp/*.el` libraries into Rust.
- Out of scope: architecture detours that delay direct replacement.

## Rewrite Policy (Direct, Not Phased)

1. No long phase model.
2. Work directly file/subsystem to Rust replacement.
3. Keep only temporary compatibility glue while replacing behavior.
4. Remove C fallback as soon as parity is proven stable.

## Direct Execution Model

For each target C area, run this loop:

1. Identify exact behavior contract from C + oracle behavior.
2. Implement equivalent behavior in Rust.
3. Add/extend tests (unit + vm-compat).
4. Switch runtime to Rust path for that behavior.
5. Run full required gates.
6. Delete/retire the replaced C path.

No “later migration phase” bucket. Every landed replacement moves production path forward immediately.

## Priority Order (Direct Cutover Order)

1. Runtime internals and mutation core
- `alloc`, `insdel`, `intervals`, related data/marker/text flow internals.

2. Command/eval loop boundaries
- `callint`, `callproc`, minibuffer-related call paths, bytecode/eval coupling edges.

3. Editor state and redisplay core
- `dispnew`, `frame`, `scroll`, `xfaces`, `fringe`, `region-cache`.

4. Host/system integrations
- `filelock`, `dbusbind`, `dynlib`, `inotify`, `sqlite`, related service hooks.

5. Platform/bootstrap tails
- remaining mandatory glue only (`sysdep`-class and platform-specific startup hooks).

## Mandatory Gates (Every PR)

1. `cargo test --manifest-path rust/neovm-core/Cargo.toml`
2. `make -C test/neovm/vm-compat check-neovm`
3. Targeted subsystem checks for touched behavior.
4. No new unresolved stubs/placeholders on rewritten path.

## Cutover Rule

- Default to Rust immediately after parity is demonstrated.
- Keep rollback switch only briefly for safety.
- Remove C fallback quickly after stable run window.

## Tracking and Accountability

Maintain `docs/c-rewrite-tracker.md` with one row per `src/*.c` file:

- `status`: `none`, `in-progress`, `rust-default`, `c-removed`, `glue-only`
- `rust owner module`
- `tests`
- `cutover commit`
- `c-removal commit`

A file is not complete until it is `c-removed` or explicitly `glue-only`.

## Stub/Placeholder Rule

Any stub/placeholder in rewritten runtime path blocks “done” status for that behavior.

Required action for each stub:

1. Replace with real behavior or explicit host ABI call.
2. Add vm-compat lock-in.
3. Cut over to Rust default.
4. Remove old C path.

## Definition of “Fully Rust Neomacs Core”

All must be true:

1. Core editor/runtime behavior formerly implemented by target C code is implemented in Rust.
2. Rust is the default path in production for those behaviors.
3. C core fallbacks for replaced behavior are removed.
4. vm-compat and integration gates pass on Rust-default path.
5. Remaining C is only documented bootstrap/platform glue.

## Immediate Actions (Now)

1. Create `docs/c-rewrite-tracker.md` seeded with all `src/*.c` files.
2. Start direct replacement with highest-leverage unresolved core behavior (`insdel`, `callint`, `alloc`-adjacent internals).
3. Burn down top runtime stubs/placeholders that block direct cutover.
4. Land Rust-default cutovers continuously, then delete C fallback paths.

## Program Completion

Program is complete when tracker confirms:

1. All targeted C core files are `c-removed` or justified as `glue-only`.
2. Rust-default core behavior is running in production builds.
3. Compatibility gates remain green without C fallback dependencies.
