# C Rewrite Tracker

Last updated: 2026-02-23

Scope: track direct replacement of Emacs C core files in `src/*.c` with Rust-default implementations.

Status values: `none`, `in-progress`, `rust-default`, `c-removed`, `glue-only`, `partial`.

| C File | Status | Rust Owner Module | Tests | Cutover Commit | C-Removal Commit | Notes |
|---|---|---|---|---|---|---|
| `src/alloc.c` | `none` | `-` | - | - | - | - |
| `src/atimer.c` | `none` | `-` | - | - | - | - |
| `src/bidi.c` | `none` | `-` | - | - | - | - |
| `src/bignum.c` | `none` | `-` | - | - | - | - |
| `src/buffer.c` | `partial` | `rust/neovm-core/src/buffer/buffer.rs` | - | - | - | - |
| `src/bytecode.c` | `none` | `-` | - | - | - | - |
| `src/callint.c` | `rust-default` | `rust/neovm-core/src/elisp/interactive.rs` | `cases/execute-extended-command-prefix-return-semantics`, `cases/execute-extended-command-batch-eof-semantics` | `6d25db1e` | - | rust backend default for NeoVM runtime; legacy editor C path still present |
| `src/callproc.c` | `none` | `-` | - | - | - | - |
| `src/casefiddle.c` | `partial` | `rust/neovm-core/src/elisp/casefiddle.rs` | - | - | - | - |
| `src/casetab.c` | `partial` | `rust/neovm-core/src/elisp/casetab.rs` | - | - | - | - |
| `src/category.c` | `partial` | `rust/neovm-core/src/elisp/category.rs` | - | - | - | - |
| `src/ccl.c` | `partial` | `rust/neovm-core/src/elisp/ccl.rs` | - | - | - | - |
| `src/character.c` | `none` | `-` | - | - | - | - |
| `src/charset.c` | `partial` | `rust/neovm-core/src/elisp/charset.rs` | - | - | - | - |
| `src/chartab.c` | `partial` | `rust/neovm-core/src/elisp/chartable.rs` | - | - | - | - |
| `src/cm.c` | `none` | `-` | - | - | - | - |
| `src/cmds.c` | `none` | `-` | - | - | - | - |
| `src/coding.c` | `partial` | `rust/neovm-core/src/elisp/coding.rs` | - | - | - | - |
| `src/comp.c` | `partial` | `rust/neovm-core/src/elisp/comp.rs` | - | - | - | - |
| `src/composite.c` | `partial` | `rust/neovm-core/src/elisp/composite.rs` | - | - | - | - |
| `src/data.c` | `none` | `-` | - | - | - | - |
| `src/dbusbind.c` | `none` | `-` | - | - | - | - |
| `src/decompress.c` | `none` | `-` | - | - | - | - |
| `src/dired.c` | `partial` | `rust/neovm-core/src/elisp/dired.rs` | - | - | - | - |
| `src/dispnew.c` | `none` | `-` | - | - | - | - |
| `src/doc.c` | `partial` | `rust/neovm-core/src/elisp/doc.rs` | - | - | - | - |
| `src/doprnt.c` | `none` | `-` | - | - | - | - |
| `src/dynlib.c` | `none` | `-` | - | - | - | - |
| `src/editfns.c` | `partial` | `rust/neovm-core/src/elisp/editfns.rs` | - | - | - | - |
| `src/emacs-module.c` | `none` | `-` | - | - | - | - |
| `src/emacs.c` | `none` | `-` | - | - | - | - |
| `src/eval.c` | `partial` | `rust/neovm-core/src/elisp/eval.rs` | - | - | - | - |
| `src/fileio.c` | `partial` | `rust/neovm-core/src/elisp/fileio.rs` | - | - | - | - |
| `src/filelock.c` | `none` | `-` | - | - | - | - |
| `src/floatfns.c` | `partial` | `rust/neovm-core/src/elisp/floatfns.rs` | - | - | - | - |
| `src/fns.c` | `partial` | `rust/neovm-core/src/elisp/fns.rs` | - | - | - | - |
| `src/font.c` | `partial` | `rust/neovm-core/src/elisp/font.rs` | - | - | - | - |
| `src/fontset.c` | `none` | `-` | - | - | - | - |
| `src/frame.c` | `none` | `-` | - | - | - | - |
| `src/fringe.c` | `none` | `-` | - | - | - | - |
| `src/ftcrfont.c` | `none` | `-` | - | - | - | - |
| `src/ftfont.c` | `none` | `-` | - | - | - | - |
| `src/gnutls.c` | `none` | `-` | - | - | - | - |
| `src/image.c` | `partial` | `rust/neovm-core/src/elisp/image.rs` | - | - | - | - |
| `src/indent.c` | `partial` | `rust/neovm-core/src/elisp/indent.rs` | - | - | - | - |
| `src/inotify.c` | `none` | `-` | - | - | - | - |
| `src/insdel.c` | `rust-default` | `rust/neovm-core/src/buffer/buffer.rs` | `cases/command-dispatch-default-arg-semantics`, `cases/call-interactively-prefix-numeric-arg-semantics` | `6d25db1e` | - | rust backend default for NeoVM runtime; legacy editor C path still present |
| `src/intervals.c` | `none` | `-` | - | - | - | - |
| `src/itree.c` | `partial` | `rust/neomacs-display/src/core/itree.rs` | - | - | - | - |
| `src/json.c` | `partial` | `rust/neovm-core/src/elisp/json.rs` | - | - | - | - |
| `src/keyboard.c` | `partial` | `rust/neovm-core/src/keyboard.rs` | - | - | - | - |
| `src/keymap.c` | `partial` | `rust/neovm-core/src/elisp/keymap.rs` | - | - | - | - |
| `src/lcms.c` | `none` | `-` | - | - | - | - |
| `src/lread.c` | `partial` | `rust/neovm-core/src/elisp/lread.rs` | - | - | - | - |
| `src/macros.c` | `none` | `-` | - | - | - | - |
| `src/marker.c` | `partial` | `rust/neovm-core/src/buffer/marker.rs` | - | - | - | - |
| `src/menu.c` | `none` | `-` | - | - | - | - |
| `src/minibuf.c` | `partial` | `rust/neovm-core/src/elisp/minibuffer.rs` | - | - | - | - |
| `src/neomacs_log.c` | `none` | `-` | - | - | - | - |
| `src/neomacsfns.c` | `none` | `-` | - | - | - | - |
| `src/neomacsterm.c` | `none` | `-` | - | - | - | - |
| `src/pdumper.c` | `none` | `-` | - | - | - | - |
| `src/print.c` | `partial` | `rust/neovm-core/src/elisp/print.rs` | - | - | - | - |
| `src/process.c` | `in-progress` | `rust/neovm-core/src/elisp/process.rs` | `cases/process-runtime-introspection-semantics`, `cases/file-runtime-wrapper-semantics` | - | - | `set-binary-mode` now matches oracle unsupported-stream payload semantics |
| `src/profiler.c` | `partial` | `rust/neomacs-display/src/core/profiler.rs` | - | - | - | - |
| `src/regex-emacs.c` | `partial` | `rust/neovm-core/src/elisp/regex.rs` | - | - | - | - |
| `src/region-cache.c` | `none` | `-` | - | - | - | - |
| `src/scroll.c` | `none` | `-` | - | - | - | - |
| `src/search.c` | `partial` | `rust/neovm-core/src/elisp/search.rs` | - | - | - | - |
| `src/sort.c` | `none` | `-` | - | - | - | - |
| `src/sound.c` | `none` | `-` | - | - | - | - |
| `src/sqlite.c` | `none` | `-` | - | - | - | - |
| `src/syntax.c` | `partial` | `rust/neovm-core/src/elisp/syntax.rs` | - | - | - | - |
| `src/sysdep.c` | `none` | `-` | - | - | - | - |
| `src/systhread.c` | `none` | `-` | - | - | - | - |
| `src/term.c` | `none` | `-` | - | - | - | - |
| `src/terminal.c` | `none` | `-` | - | - | - | - |
| `src/terminfo.c` | `none` | `-` | - | - | - | - |
| `src/textprop.c` | `partial` | `rust/neovm-core/src/elisp/textprop.rs` | - | - | - | - |
| `src/thread.c` | `partial` | `rust/neovm-core/src/elisp/threads.rs` | - | - | - | - |
| `src/timefns.c` | `partial` | `rust/neovm-core/src/elisp/timefns.rs` | - | - | - | - |
| `src/treesit.c` | `none` | `-` | - | - | - | - |
| `src/undo.c` | `partial` | `rust/neovm-core/src/buffer/undo.rs` | - | - | - | - |
| `src/window.c` | `partial` | `rust/neovm-core/src/window.rs` | - | - | - | - |
| `src/xdisp.c` | `partial` | `rust/neovm-core/src/elisp/xdisp.rs` | - | - | - | - |
| `src/xfaces.c` | `none` | `-` | - | - | - | - |
| `src/xgselect.c` | `none` | `-` | - | - | - | - |
| `src/xml.c` | `partial` | `rust/neovm-core/src/elisp/xml.rs` | - | - | - | - |
