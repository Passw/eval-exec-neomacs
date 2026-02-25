# C Rewrite Tracker

Last updated: 2026-02-25 (full sweep complete)

Scope: track direct replacement of Emacs C core files in `src/*.c` with Rust-default implementations.

Status values: `none`, `in-progress`, `rust-default`, `c-removed`, `glue-only`, `partial`.

| C File | Status | Rust Owner Module | Tests | Cutover Commit | C-Removal Commit | Notes |
|---|---|---|---|---|---|---|
| `src/alloc.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs` | `cases/alloc-constructors` | - | - | All 23 DEFUNs dispatched; 12 oracle lock-in forms |
| `src/atimer.c` | `none` | `-` | - | - | - | - |
| `src/bidi.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/bignum.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/buffer.c` | `rust-default` | `rust/neovm-core/src/buffer/buffer.rs` | `cases/buffer-core-semantics` | - | - | All 53 DEFUNs dispatched; 21 oracle lock-in forms |
| `src/bytecode.c` | `none` | `-` | - | - | - | - |
| `src/callint.c` | `rust-default` | `rust/neovm-core/src/elisp/interactive.rs` | `cases/execute-extended-command-prefix-return-semantics`, `cases/execute-extended-command-batch-eof-semantics` | `6d25db1e` | - | rust backend default for NeoVM runtime; legacy editor C path still present |
| `src/callproc.c` | `rust-default` | `rust/neovm-core/src/elisp/process.rs` | `cases/process-basics`, `cases/call-process-start-file-process-string-contract-semantics` | - | - | All 3 DEFUNs dispatched; oracle lock-in via process case files |
| `src/casefiddle.c` | `rust-default` | `rust/neovm-core/src/elisp/casefiddle.rs` | `cases/casefiddle-core-semantics` | - | - | All 11 DEFUNs dispatched; 19 oracle lock-in forms |
| `src/casetab.c` | `rust-default` | `rust/neovm-core/src/elisp/casetab.rs` | `cases/casetab-core-semantics`, `cases/case-table-object-semantics` | - | - | All 5 DEFUNs dispatched; oracle lock-in |
| `src/category.c` | `rust-default` | `rust/neovm-core/src/elisp/category.rs` | `cases/category-core-semantics` | - | - | All 13 DEFUNs dispatched; 10 oracle lock-in forms |
| `src/ccl.c` | `rust-default` | `rust/neovm-core/src/elisp/ccl.rs` | `cases/ccl-semantics`, `cases/ccl-arity-semantics`, `cases/ccl-runtime-semantics`, `cases/ccl-registration-semantics`, `cases/ccl-register-program-type-semantics`, `cases/ccl-register-program-plist-semantics`, `cases/ccl-registration-plist-error-no-side-effects-semantics`, `cases/ccl-program-p-designator-semantics`, `cases/ccl-header-second-slot-bounds-semantics`, `cases/ccl-code-conversion-map-type-semantics`, `cases/ccl-code-conversion-map-plist-semantics`, `cases/ccl-code-conversion-map-existing-symbol-plist-edge-semantics`, `cases/ccl-symbol-designator-plist-gate-semantics`, `cases/ccl-symbol-registration-runtime-semantics`, `cases/category-ccl-subr-arity-semantics` | - | - | All 5 DEFUNs dispatched; 15 oracle lock-in case files |
| `src/character.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs` | `cases/character-operations` | - | - | All 10 DEFUNs dispatched; 9 oracle lock-in forms |
| `src/charset.c` | `rust-default` | `rust/neovm-core/src/elisp/charset.rs` | `cases/charsetp-semantics`, `cases/charset-api-availability`, `cases/charset-plist-error-semantics`, `cases/charset-priority-semantics`, `cases/charset-id-internal-semantics`, `cases/charset-internal-arity-semantics`, `cases/charset-alias-declare-equiv-semantics`, `cases/charset-encode-decode-semantics`, `cases/find-charset-string-semantics`, `cases/charset-region-after-semantics`, `cases/charset-big5-sjis-iso-final-semantics`, `cases/set-charset-plist-semantics` | - | - | All 23 DEFUNs dispatched; 14 oracle lock-in case files |
| `src/chartab.c` | `rust-default` | `rust/neovm-core/src/elisp/chartable.rs` | `cases/chartab-core-semantics` | - | - | All 13 DEFUNs dispatched; oracle lock-in |
| `src/cm.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/cmds.c` | `none` | `-` | - | - | - | - |
| `src/coding.c` | `rust-default` | `rust/neovm-core/src/elisp/coding.rs` | `cases/coding-core-semantics` | - | - | All 34 DEFUNs dispatched; 14 oracle lock-in forms |
| `src/comp.c` | `rust-default` | `rust/neovm-core/src/elisp/comp.rs` | `cases/native-comp-available-semantics`, `cases/native-comp-unit-contract-semantics`, `cases/comp-dbus-internal-semantics` | - | - | All 15 DEFUNs dispatched; 3+ oracle lock-in case files |
| `src/composite.c` | `rust-default` | `rust/neovm-core/src/elisp/composite.rs` | `cases/composite-core-semantics` | - | - | All 6 DEFUNs dispatched; oracle lock-in |
| `src/data.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs` | `cases/data-type-predicates-and-accessors` | - | - | All 120 DEFUNs dispatched; 62 oracle lock-in forms |
| `src/dbusbind.c` | `none` | `-` | - | - | - | - |
| `src/decompress.c` | `none` | `-` | - | - | - | - |
| `src/dired.c` | `rust-default` | `rust/neovm-core/src/elisp/dired.rs` | `cases/dired-core-semantics` | - | - | All 8 DEFUNs dispatched; 8 oracle lock-in forms |
| `src/dispnew.c` | `rust-default` | `rust/neovm-core/src/elisp/dispnew/pure.rs` | - | - | - | All 12 DEFUNs dispatched via dispnew/pure.rs module |
| `src/doc.c` | `rust-default` | `rust/neovm-core/src/elisp/doc.rs` | `cases/documentation-semantics`, `cases/documentation-runtime-builtin-semantics`, `cases/documentation-property-semantics`, `cases/documentation-flush-force-invocation-time-semantics`, `cases/doc-helper-arity-semantics`, `cases/doc-helper-subr-arity-semantics`, `cases/snarf-documentation-runtime-semantics`, `cases/autoload-startup-docstring-semantics` | - | - | All 6 DEFUNs dispatched; 8 oracle lock-in case files |
| `src/doprnt.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/dynlib.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/editfns.c` | `rust-default` | `rust/neovm-core/src/elisp/editfns.rs` | `cases/editfns-string-ops-semantics`, `cases/editfns-buffer-ops-semantics` | - | - | All 80 DEFUNs dispatched; 69 oracle lock-in forms |
| `src/emacs-module.c` | `none` | `-` | - | - | - | - |
| `src/emacs.c` | `none` | `-` | - | - | - | - |
| `src/eval.c` | `rust-default` | `rust/neovm-core/src/elisp/eval.rs` | `cases/eval-core-semantics` | - | - | All 54 DEFUNs dispatched; 45 oracle lock-in forms |
| `src/fileio.c` | `rust-default` | `rust/neovm-core/src/elisp/fileio.rs` | `cases/fileio-core-semantics` | - | - | All 54 DEFUNs dispatched; 30 oracle lock-in forms |
| `src/filelock.c` | `none` | `-` | - | - | - | - |
| `src/floatfns.c` | `rust-default` | `rust/neovm-core/src/elisp/floatfns.rs` | `cases/floatfns-math-semantics` | - | - | All 25 DEFUNs dispatched; 86 oracle lock-in forms; fixed floor/ceiling/round/truncate 2-arg divisor |
| `src/fns.c` | `rust-default` | `rust/neovm-core/src/elisp/fns.rs` | `cases/fns-sequence-core-semantics`, `cases/fns-string-comparison-semantics`, `cases/fns-hash-table-semantics`, `cases/fns-equality-semantics`, `cases/fns-base64-crypto-semantics`, `cases/fns-misc-semantics` | - | - | All 104 DEFUNs dispatched; 250 oracle lock-in forms across 6 case files |
| `src/font.c` | `rust-default` | `rust/neovm-core/src/elisp/font.rs` | `cases/font-batch-semantics`, `cases/font-xlfd-semantics`, `cases/font-color-semantics`, `cases/font-object-semantics`, `cases/font-face-helper-semantics`, `cases/font-face-batch-semantics`, `cases/face-font-semantics` | - | - | All 25 DEFUNs dispatched; 7+ oracle lock-in case files |
| `src/fontset.c` | `none` | `-` | - | - | - | - |
| `src/frame.c` | `rust-default` | `rust/neovm-core/src/elisp/builtins.rs`, `rust/neovm-core/src/elisp/window_cmds.rs` | - | - | - | All 67 DEFUNs dispatched; 62 via window_cmds.rs, 5 new stubs in builtins.rs |
| `src/fringe.c` | `none` | `-` | - | - | - | - |
| `src/ftcrfont.c` | `rust-default` | `rust/neomacs-display/` | - | - | - | 0 DEFUNs; font rendering handled by Rust neomacs-display |
| `src/ftfont.c` | `rust-default` | `rust/neomacs-display/` | - | - | - | 0 DEFUNs; font rendering handled by Rust neomacs-display |
| `src/gnutls.c` | `none` | `-` | - | - | - | - |
| `src/image.c` | `rust-default` | `rust/neovm-core/src/elisp/image.rs` | `cases/image-type-semantics`, `cases/image-area-semantics`, `cases/image-batch-query-semantics`, `cases/image-clear-cache-semantics`, `cases/image-clear-cache-optional-semantics`, `cases/image-flush-semantics`, `cases/image-insert-put-semantics`, `cases/image-remove-semantics`, `cases/image-transforms-semantics`, `cases/create-image-type-resolution`, `cases/default-image-insert-marker-semantics`, `cases/display-images-face-support-semantics`, `cases/image-font-subr-arity-semantics` | - | - | All 9 DEFUNs dispatched; 13 oracle lock-in case files |
| `src/indent.c` | `rust-default` | `rust/neovm-core/src/elisp/indent.rs` | `cases/indent-column-semantics`, `cases/indent-for-tab-command-whitespace-semantics`, `cases/indent-line-to-semantics`, `cases/indent-line-to-return-column-semantics`, `cases/indent-mode-semantics`, `cases/indent-read-only-variable-semantics`, `cases/indent-region-semantics`, `cases/indent-rigidly-arg-contract-semantics`, `cases/indent-rigidly-read-only-variable-semantics`, `cases/indent-subr-arity-semantics`, `cases/indent-to-minimum-fixnump-semantics`, `cases/indent-to-return-column-semantics` | - | - | All 7 DEFUNs dispatched; 12 oracle lock-in case files |
| `src/inotify.c` | `none` | `-` | - | - | - | - |
| `src/insdel.c` | `rust-default` | `rust/neovm-core/src/buffer/buffer.rs` | `cases/command-dispatch-default-arg-semantics`, `cases/call-interactively-prefix-numeric-arg-semantics` | `6d25db1e` | - | rust backend default for NeoVM runtime; legacy editor C path still present |
| `src/intervals.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/itree.c` | `rust-default` | `rust/neomacs-display/src/core/itree.rs` | - | - | - | 0 DEFUNs; internal only |
| `src/json.c` | `rust-default` | `rust/neovm-core/src/elisp/json.rs` | `cases/json-semantics`, `cases/json-keyword-errors`, `cases/json-buffer-semantics`, `cases/charset-json-libxml-display-subr-arity-semantics` | - | - | All 4 DEFUNs dispatched; 4 oracle lock-in case files |
| `src/keyboard.c` | `rust-default` | `rust/neovm-core/src/keyboard.rs` | `cases/keyboard-core-semantics` | - | - | All 37 DEFUNs dispatched; 15 oracle lock-in forms |
| `src/keymap.c` | `rust-default` | `rust/neovm-core/src/elisp/keymap.rs` | `cases/keymap-core-semantics` | - | - | All 29 DEFUNs dispatched; 15 oracle lock-in forms |
| `src/lcms.c` | `none` | `-` | - | - | - | - |
| `src/lread.c` | `rust-default` | `rust/neovm-core/src/elisp/lread.rs` | `cases/lread-core-semantics` | - | - | All 17 DEFUNs dispatched; 15 oracle lock-in forms |
| `src/macros.c` | `none` | `-` | - | - | - | - |
| `src/marker.c` | `rust-default` | `rust/neovm-core/src/buffer/marker.rs` | `cases/marker-core-semantics` | - | - | All 7 DEFUNs dispatched; 10 oracle lock-in forms |
| `src/menu.c` | `none` | `-` | - | - | - | - |
| `src/minibuf.c` | `rust-default` | `rust/neovm-core/src/elisp/minibuffer.rs` | `cases/minibuf-core-semantics` | - | - | All 23 DEFUNs dispatched; 14 oracle lock-in forms |
| `src/neomacs_log.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; neomacs-specific logging infrastructure |
| `src/neomacsfns.c` | `none` | `-` | - | - | - | - |
| `src/neomacsterm.c` | `none` | `-` | - | - | - | - |
| `src/pdumper.c` | `none` | `-` | - | - | - | - |
| `src/print.c` | `rust-default` | `rust/neovm-core/src/elisp/print.rs` | `cases/print-core-semantics` | - | - | All 11 DEFUNs dispatched; 16 oracle lock-in forms |
| `src/process.c` | `rust-default` | `rust/neovm-core/src/elisp/process.rs` | 39 case files (`cases/process-*`, `cases/start-process-*`, `cases/accept-process-output-*`, `cases/call-process-*`, `cases/file-runtime-wrapper-semantics`) | - | - | All 64 DEFUNs dispatched; 39 oracle lock-in case files covering runtime introspection, stale-handle mutators/control, process-attributes, serial-port contracts, tty-stream semantics, network/interface signals, coding/query, shell wrappers, and more |
| `src/profiler.c` | `partial` | `rust/neomacs-display/src/core/profiler.rs` | - | - | - | timing-dependent; skip oracle |
| `src/regex-emacs.c` | `rust-default` | `rust/neovm-core/src/elisp/regex.rs` | - | - | - | 0 DEFUNs; internal only |
| `src/region-cache.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/scroll.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/search.c` | `rust-default` | `rust/neovm-core/src/elisp/search.rs` | `cases/search-core-semantics` | - | - | All 19 DEFUNs dispatched; 16 oracle lock-in forms |
| `src/sort.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/sound.c` | `none` | `-` | - | - | - | - |
| `src/sqlite.c` | `none` | `-` | - | - | - | - |
| `src/syntax.c` | `rust-default` | `rust/neovm-core/src/elisp/syntax.rs` | `cases/syntax-core-semantics` | - | - | All 21 DEFUNs dispatched; 13 oracle lock-in forms |
| `src/sysdep.c` | `none` | `-` | - | - | - | - |
| `src/systhread.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/term.c` | `none` | `-` | - | - | - | - |
| `src/terminal.c` | `none` | `-` | - | - | - | - |
| `src/terminfo.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/textprop.c` | `rust-default` | `rust/neovm-core/src/elisp/textprop.rs` | `cases/textprop-core-semantics` | - | - | All 20 DEFUNs dispatched; 10 oracle lock-in forms |
| `src/thread.c` | `rust-default` | `rust/neovm-core/src/elisp/threads.rs` | `cases/thread-all-threads-semantics`, `cases/thread-condition-wait-arity`, `cases/thread-handle-identity`, `cases/thread-handle-printing`, `cases/thread-help-function-arglist`, `cases/thread-introspection`, `cases/thread-join-error-paths`, `cases/thread-join-semantics`, `cases/thread-last-error-clear-flag`, `cases/thread-last-error-publication`, `cases/thread-last-error-semantics`, `cases/thread-live-p-semantics`, `cases/thread-make-thread-name-validation`, `cases/thread-make-thread-noncallable`, `cases/thread-mutex-error-payloads`, `cases/thread-name-semantics`, `cases/thread-signal-last-error`, `cases/thread-signal-semantics`, `cases/thread-subr-arity`, `cases/thread-sync-error-payloads`, `cases/thread-sync-semantics`, `cases/thread-yield-semantics`, `cases/process-mark-type-thread-send-semantics` | - | - | All 21 DEFUNs dispatched; 23 oracle lock-in case files |
| `src/timefns.c` | `rust-default` | `rust/neovm-core/src/elisp/timefns.rs` | `cases/timefns-arithmetic-semantics` | - | - | All 14 DEFUNs dispatched; oracle lock-in |
| `src/treesit.c` | `none` | `-` | - | - | - | - |
| `src/undo.c` | `rust-default` | `rust/neovm-core/src/buffer/undo.rs` | `cases/undo-basics`, `cases/undo-arity-semantics`, `cases/undo-buffer-arg`, `cases/undo-result-semantics`, `cases/buffer-undo-designator-semantics` | - | - | All 1 DEFUN dispatched; 5 oracle lock-in case files |
| `src/window.c` | `rust-default` | `rust/neovm-core/src/window.rs` | (57+ existing window case files) | - | - | All 121 DEFUNs dispatched; 57+ oracle lock-in case files |
| `src/xdisp.c` | `rust-default` | `rust/neovm-core/src/elisp/xdisp.rs` | `cases/xdisp-core-semantics`, `cases/xdisp-image-map-semantics` | - | - | All 17 DEFUNs dispatched; oracle lock-in |
| `src/xfaces.c` | `rust-default` | `rust/neovm-core/src/elisp/font.rs` | - | - | - | All 29 DEFUNs dispatched; 28 via font.rs face builtins, x-load-color-file added |
| `src/xgselect.c` | `rust-default` | `-` | - | - | - | 0 DEFUNs; internal C infrastructure |
| `src/xml.c` | `rust-default` | `rust/neovm-core/src/elisp/xml.rs` | `cases/xml-core-semantics` | - | - | All 3 DEFUNs dispatched; 3 oracle lock-in forms |
