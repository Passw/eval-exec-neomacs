# C Rewrite Tracker

Last updated: 2026-02-25

Scope: track direct replacement of Emacs C core files in `src/*.c` with Rust-default implementations.

Status values: `none`, `in-progress`, `rust-default`, `c-removed`, `glue-only`, `partial`.

| C File | Status | Rust Owner Module | Tests | Cutover Commit | C-Removal Commit | Notes |
|---|---|---|---|---|---|---|
| `src/alloc.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs` | `cases/alloc-constructors` | - | - | All 23 DEFUNs dispatched; 12 oracle lock-in forms |
| `src/atimer.c` | `none` | `-` | - | - | - | - |
| `src/bidi.c` | `none` | `-` | - | - | - | - |
| `src/bignum.c` | `none` | `-` | - | - | - | - |
| `src/buffer.c` | `partial` | `rust/neovm-core/src/buffer/buffer.rs` | - | - | - | - |
| `src/bytecode.c` | `none` | `-` | - | - | - | - |
| `src/callint.c` | `rust-default` | `rust/neovm-core/src/elisp/interactive.rs` | `cases/execute-extended-command-prefix-return-semantics`, `cases/execute-extended-command-batch-eof-semantics` | `6d25db1e` | - | rust backend default for NeoVM runtime; legacy editor C path still present |
| `src/callproc.c` | `in-progress` | `rust/neovm-core/src/elisp/process.rs` | `cases/process-basics`, `cases/call-process-start-file-process-string-contract-semantics` | - | - | direct call-process/call-process-region/start-file-process contract parity tightening in progress |
| `src/casefiddle.c` | `partial` | `rust/neovm-core/src/elisp/casefiddle.rs` | - | - | - | - |
| `src/casetab.c` | `rust-default` | `rust/neovm-core/src/elisp/casetab.rs` | `cases/casetab-core-semantics`, `cases/case-table-object-semantics` | - | - | All 5 DEFUNs dispatched; oracle lock-in |
| `src/category.c` | `partial` | `rust/neovm-core/src/elisp/category.rs` | - | - | - | - |
| `src/ccl.c` | `rust-default` | `rust/neovm-core/src/elisp/ccl.rs` | `cases/ccl-semantics`, `cases/ccl-arity-semantics`, `cases/ccl-runtime-semantics`, `cases/ccl-registration-semantics`, `cases/ccl-register-program-type-semantics`, `cases/ccl-register-program-plist-semantics`, `cases/ccl-registration-plist-error-no-side-effects-semantics`, `cases/ccl-program-p-designator-semantics`, `cases/ccl-header-second-slot-bounds-semantics`, `cases/ccl-code-conversion-map-type-semantics`, `cases/ccl-code-conversion-map-plist-semantics`, `cases/ccl-code-conversion-map-existing-symbol-plist-edge-semantics`, `cases/ccl-symbol-designator-plist-gate-semantics`, `cases/ccl-symbol-registration-runtime-semantics`, `cases/category-ccl-subr-arity-semantics` | - | - | All 5 DEFUNs dispatched; 15 oracle lock-in case files |
| `src/character.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs` | `cases/character-operations` | - | - | All 10 DEFUNs dispatched; 9 oracle lock-in forms |
| `src/charset.c` | `partial` | `rust/neovm-core/src/elisp/charset.rs` | - | - | - | - |
| `src/chartab.c` | `rust-default` | `rust/neovm-core/src/elisp/chartable.rs` | `cases/chartab-core-semantics` | - | - | All 13 DEFUNs dispatched; oracle lock-in |
| `src/cm.c` | `none` | `-` | - | - | - | - |
| `src/cmds.c` | `none` | `-` | - | - | - | - |
| `src/coding.c` | `partial` | `rust/neovm-core/src/elisp/coding.rs` | - | - | - | - |
| `src/comp.c` | `partial` | `rust/neovm-core/src/elisp/comp.rs` | - | - | - | - |
| `src/composite.c` | `rust-default` | `rust/neovm-core/src/elisp/composite.rs` | `cases/composite-core-semantics` | - | - | All 6 DEFUNs dispatched; oracle lock-in |
| `src/data.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs` | `cases/data-type-predicates-and-accessors` | - | - | All 120 DEFUNs dispatched; 62 oracle lock-in forms |
| `src/dbusbind.c` | `none` | `-` | - | - | - | - |
| `src/decompress.c` | `none` | `-` | - | - | - | - |
| `src/dired.c` | `partial` | `rust/neovm-core/src/elisp/dired.rs` | - | - | - | - |
| `src/dispnew.c` | `none` | `-` | - | - | - | - |
| `src/doc.c` | `rust-default` | `rust/neovm-core/src/elisp/doc.rs` | `cases/documentation-semantics`, `cases/documentation-runtime-builtin-semantics`, `cases/documentation-property-semantics`, `cases/documentation-flush-force-invocation-time-semantics`, `cases/doc-helper-arity-semantics`, `cases/doc-helper-subr-arity-semantics`, `cases/snarf-documentation-runtime-semantics`, `cases/autoload-startup-docstring-semantics` | - | - | All 6 DEFUNs dispatched; 8 oracle lock-in case files |
| `src/doprnt.c` | `none` | `-` | - | - | - | - |
| `src/dynlib.c` | `none` | `-` | - | - | - | - |
| `src/editfns.c` | `rust-default` | `rust/neovm-core/src/elisp/editfns.rs` | `cases/editfns-string-ops-semantics`, `cases/editfns-buffer-ops-semantics` | - | - | All 80 DEFUNs dispatched; 69 oracle lock-in forms |
| `src/emacs-module.c` | `none` | `-` | - | - | - | - |
| `src/emacs.c` | `none` | `-` | - | - | - | - |
| `src/eval.c` | `rust-default` | `rust/neovm-core/src/elisp/eval.rs` | `cases/eval-core-semantics` | - | - | All 54 DEFUNs dispatched; 45 oracle lock-in forms |
| `src/fileio.c` | `partial` | `rust/neovm-core/src/elisp/fileio.rs` | - | - | - | - |
| `src/filelock.c` | `none` | `-` | - | - | - | - |
| `src/floatfns.c` | `rust-default` | `rust/neovm-core/src/elisp/floatfns.rs` | `cases/floatfns-math-semantics` | - | - | All 25 DEFUNs dispatched; 86 oracle lock-in forms; fixed floor/ceiling/round/truncate 2-arg divisor |
| `src/fns.c` | `rust-default` | `rust/neovm-core/src/elisp/fns.rs` | `cases/fns-sequence-core-semantics`, `cases/fns-string-comparison-semantics`, `cases/fns-hash-table-semantics`, `cases/fns-equality-semantics`, `cases/fns-base64-crypto-semantics`, `cases/fns-misc-semantics` | - | - | All 104 DEFUNs dispatched; 250 oracle lock-in forms across 6 case files |
| `src/font.c` | `partial` | `rust/neovm-core/src/elisp/font.rs` | - | - | - | - |
| `src/fontset.c` | `none` | `-` | - | - | - | - |
| `src/frame.c` | `none` | `-` | - | - | - | - |
| `src/fringe.c` | `none` | `-` | - | - | - | - |
| `src/ftcrfont.c` | `none` | `-` | - | - | - | - |
| `src/ftfont.c` | `none` | `-` | - | - | - | - |
| `src/gnutls.c` | `none` | `-` | - | - | - | - |
| `src/image.c` | `rust-default` | `rust/neovm-core/src/elisp/image.rs` | `cases/image-type-semantics`, `cases/image-area-semantics`, `cases/image-batch-query-semantics`, `cases/image-clear-cache-semantics`, `cases/image-clear-cache-optional-semantics`, `cases/image-flush-semantics`, `cases/image-insert-put-semantics`, `cases/image-remove-semantics`, `cases/image-transforms-semantics`, `cases/create-image-type-resolution`, `cases/default-image-insert-marker-semantics`, `cases/display-images-face-support-semantics`, `cases/image-font-subr-arity-semantics` | - | - | All 9 DEFUNs dispatched; 13 oracle lock-in case files |
| `src/indent.c` | `rust-default` | `rust/neovm-core/src/elisp/indent.rs` | `cases/indent-column-semantics`, `cases/indent-for-tab-command-whitespace-semantics`, `cases/indent-line-to-semantics`, `cases/indent-line-to-return-column-semantics`, `cases/indent-mode-semantics`, `cases/indent-read-only-variable-semantics`, `cases/indent-region-semantics`, `cases/indent-rigidly-arg-contract-semantics`, `cases/indent-rigidly-read-only-variable-semantics`, `cases/indent-subr-arity-semantics`, `cases/indent-to-minimum-fixnump-semantics`, `cases/indent-to-return-column-semantics` | - | - | All 7 DEFUNs dispatched; 12 oracle lock-in case files |
| `src/inotify.c` | `none` | `-` | - | - | - | - |
| `src/insdel.c` | `rust-default` | `rust/neovm-core/src/buffer/buffer.rs` | `cases/command-dispatch-default-arg-semantics`, `cases/call-interactively-prefix-numeric-arg-semantics` | `6d25db1e` | - | rust backend default for NeoVM runtime; legacy editor C path still present |
| `src/intervals.c` | `none` | `-` | - | - | - | - |
| `src/itree.c` | `partial` | `rust/neomacs-display/src/core/itree.rs` | - | - | - | - |
| `src/json.c` | `rust-default` | `rust/neovm-core/src/elisp/json.rs` | `cases/json-semantics`, `cases/json-keyword-errors`, `cases/json-buffer-semantics`, `cases/charset-json-libxml-display-subr-arity-semantics` | - | - | All 4 DEFUNs dispatched; 4 oracle lock-in case files |
| `src/keyboard.c` | `partial` | `rust/neovm-core/src/keyboard.rs` | - | - | - | - |
| `src/keymap.c` | `partial` | `rust/neovm-core/src/elisp/keymap.rs` | - | - | - | - |
| `src/lcms.c` | `none` | `-` | - | - | - | - |
| `src/lread.c` | `rust-default` | `rust/neovm-core/src/elisp/lread.rs` | `cases/lread-core-semantics` | - | - | All 17 DEFUNs dispatched; 15 oracle lock-in forms |
| `src/macros.c` | `none` | `-` | - | - | - | - |
| `src/marker.c` | `rust-default` | `rust/neovm-core/src/buffer/marker.rs` | `cases/marker-core-semantics` | - | - | All 7 DEFUNs dispatched; 10 oracle lock-in forms |
| `src/menu.c` | `none` | `-` | - | - | - | - |
| `src/minibuf.c` | `partial` | `rust/neovm-core/src/elisp/minibuffer.rs` | - | - | - | - |
| `src/neomacs_log.c` | `none` | `-` | - | - | - | - |
| `src/neomacsfns.c` | `none` | `-` | - | - | - | - |
| `src/neomacsterm.c` | `none` | `-` | - | - | - | - |
| `src/pdumper.c` | `none` | `-` | - | - | - | - |
| `src/print.c` | `rust-default` | `rust/neovm-core/src/elisp/print.rs` | `cases/print-core-semantics` | - | - | All 11 DEFUNs dispatched; 16 oracle lock-in forms |
| `src/process.c` | `in-progress` | `rust/neovm-core/src/elisp/process.rs` | `cases/process-runtime-introspection-semantics`, `cases/start-process-buffer-and-type-contract-semantics`, `cases/file-runtime-wrapper-semantics`, `cases/accept-process-output-millis-semantics`, `cases/process-stale-mutator-semantics`, `cases/process-stale-control-semantics`, `cases/process-attributes-runtime-semantics`, `cases/process-serial-port-contract-semantics`, `cases/process-tty-stream-kind-semantics` | - | - | `set-binary-mode`, `accept-process-output`, `start-process` contracts, stale-handle mutator/control semantics, `process-attributes` runtime identity shape, `make-serial-process` `:port` type contracts, and `process-tty-name` stream/kind tty semantics now align with oracle lock-ins |
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
| `src/textprop.c` | `rust-default` | `rust/neovm-core/src/elisp/textprop.rs` | `cases/textprop-core-semantics` | - | - | All 20 DEFUNs dispatched; 10 oracle lock-in forms |
| `src/thread.c` | `rust-default` | `rust/neovm-core/src/elisp/threads.rs` | `cases/thread-all-threads-semantics`, `cases/thread-condition-wait-arity`, `cases/thread-handle-identity`, `cases/thread-handle-printing`, `cases/thread-help-function-arglist`, `cases/thread-introspection`, `cases/thread-join-error-paths`, `cases/thread-join-semantics`, `cases/thread-last-error-clear-flag`, `cases/thread-last-error-publication`, `cases/thread-last-error-semantics`, `cases/thread-live-p-semantics`, `cases/thread-make-thread-name-validation`, `cases/thread-make-thread-noncallable`, `cases/thread-mutex-error-payloads`, `cases/thread-name-semantics`, `cases/thread-signal-last-error`, `cases/thread-signal-semantics`, `cases/thread-subr-arity`, `cases/thread-sync-error-payloads`, `cases/thread-sync-semantics`, `cases/thread-yield-semantics`, `cases/process-mark-type-thread-send-semantics` | - | - | All 21 DEFUNs dispatched; 23 oracle lock-in case files |
| `src/timefns.c` | `rust-default` | `rust/neovm-core/src/elisp/timefns.rs` | `cases/timefns-arithmetic-semantics` | - | - | All 14 DEFUNs dispatched; oracle lock-in |
| `src/treesit.c` | `none` | `-` | - | - | - | - |
| `src/undo.c` | `rust-default` | `rust/neovm-core/src/buffer/undo.rs` | `cases/undo-basics`, `cases/undo-arity-semantics`, `cases/undo-buffer-arg`, `cases/undo-result-semantics`, `cases/buffer-undo-designator-semantics` | - | - | All 1 DEFUN dispatched; 5 oracle lock-in case files |
| `src/window.c` | `partial` | `rust/neovm-core/src/window.rs` | - | - | - | - |
| `src/xdisp.c` | `partial` | `rust/neovm-core/src/elisp/xdisp.rs` | - | - | - | - |
| `src/xfaces.c` | `none` | `-` | - | - | - | - |
| `src/xgselect.c` | `none` | `-` | - | - | - | - |
| `src/xml.c` | `partial` | `rust/neovm-core/src/elisp/xml.rs` | - | - | - | - |
