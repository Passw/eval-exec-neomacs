//! Subr/primitive introspection builtins.
//!
//! Provides type predicates and introspection for callable objects:
//! - `subrp`, `subr-name`, `subr-arity`
//! - `commandp`, `functionp`, `byte-code-function-p`, `closurep`
//! - `interpreted-function-p`, `special-form-p`, `macrop`
//! - `func-arity`, `indirect-function`

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::sync::Arc;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Evaluator/public callable classification
// ---------------------------------------------------------------------------

/// Returns true if `name` is recognized by the evaluator's special-form
/// dispatch path.
///
/// This list mirrors `Evaluator::try_special_form()` in `eval.rs`.
fn is_evaluator_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "quote"
            | "function"
            | "let"
            | "let*"
            | "setq"
            | "setq-local"
            | "if"
            | "and"
            | "or"
            | "cond"
            | "while"
            | "progn"
            | "prog1"
            | "lambda"
            | "defun"
            | "defvar"
            | "defconst"
            | "defmacro"
            | "funcall"
            | "catch"
            | "throw"
            | "unwind-protect"
            | "condition-case"
            | "interactive"
            | "declare"
            | "when"
            | "unless"
            | "bound-and-true-p"
            | "defalias"
            | "provide"
            | "require"
            | "save-excursion"
            | "save-window-excursion"
            | "save-selected-window"
            | "save-mark-and-excursion"
            | "save-restriction"
            | "save-match-data"
            | "with-local-quit"
            | "with-temp-message"
            | "with-demoted-errors"
            | "with-current-buffer"
            | "ignore-errors"
            | "dotimes"
            | "dolist"
            // Custom / defcustom
            | "defcustom"
            | "defgroup"
            | "setq-default"
            | "defvar-local"
            // Autoload
            | "autoload"
            | "eval-when-compile"
            | "eval-and-compile"
            | "declare-function"
            | "define-obsolete-function-alias"
            | "define-obsolete-variable-alias"
            | "make-obsolete"
            | "make-obsolete-variable"
            | "with-eval-after-load"
            // Error hierarchy
            | "define-error"
            // Pattern matching
            | "pcase"
            | "pcase-let"
            | "pcase-let*"
            | "pcase-dolist"
            // Generalized variables
            | "setf"
            | "push"
            | "pop"
            | "cl-incf"
            | "cl-decf"
            | "gv-define-simple-setter"
            | "gv-define-setter"
            // CL extended
            | "cl-defstruct"
            | "cl-loop"
            | "cl-destructuring-bind"
            | "cl-push"
            | "cl-pop"
            | "cl-pushnew"
            | "cl-assert"
            | "cl-check-type"
            | "cl-case"
            | "cl-ecase"
            | "cl-typecase"
            | "cl-etypecase"
            | "cl-block"
            | "cl-return-from"
            | "cl-dotimes"
            | "cl-dolist"
            | "cl-flet"
            | "cl-labels"
            | "cl-progv"
            // Reader/printer
            | "with-output-to-string"
            // Threading
            | "with-mutex"
            // Misc
            | "prog2"
            | "with-temp-buffer"
            | "save-current-buffer"
            | "track-mouse"
            | "with-syntax-table"
            // Mode definition
            | "define-minor-mode"
            | "define-derived-mode"
            | "define-generic-mode"
    )
}

/// Returns true for special forms exposed by `special-form-p`.
///
/// Emacs distinguishes evaluator internals from public special forms:
/// many evaluator-recognized constructs are macros/functions in user-visible
/// introspection.
fn is_public_special_form_name(name: &str) -> bool {
    matches!(
        name,
        "quote"
            | "function"
            | "let"
            | "let*"
            | "setq"
            | "if"
            | "and"
            | "or"
            | "cond"
            | "while"
            | "progn"
            | "prog1"
            | "defvar"
            | "defconst"
            | "catch"
            | "unwind-protect"
            | "condition-case"
            | "interactive"
            | "inline"
            | "save-excursion"
            | "save-restriction"
            | "save-current-buffer"
    )
}

pub(crate) fn is_special_form(name: &str) -> bool {
    is_public_special_form_name(name)
}

pub(crate) fn is_evaluator_macro_name(name: &str) -> bool {
    let is_macro = has_fallback_macro(name) || name == "declare";
    debug_assert!(!is_macro || is_evaluator_special_form_name(name));
    is_macro
}

pub(crate) fn is_evaluator_callable_name(name: &str) -> bool {
    // These are evaluator-dispatched entries that still behave as normal
    // callable symbols in introspection (`fboundp`/`functionp`/`symbol-function`).
    matches!(name, "throw")
}

#[derive(Clone, Copy)]
struct FallbackMacroSpec {
    min: usize,
    max: Option<usize>,
}

fn fallback_macro_spec(name: &str) -> Option<FallbackMacroSpec> {
    match name {
        "when"
        | "unless"
        | "dotimes"
        | "dolist"
        | "pcase-let"
        | "pcase-let*"
        | "pcase-dolist"
        | "with-mutex" => {
            Some(FallbackMacroSpec { min: 1, max: None })
        }
        "with-current-buffer" | "with-syntax-table" | "with-eval-after-load" => {
            Some(FallbackMacroSpec { min: 1, max: None })
        }
        "ignore-errors"
        | "setq-local"
        | "with-temp-buffer"
        | "with-output-to-string"
        | "track-mouse"
        | "save-window-excursion"
        | "save-selected-window"
        | "save-mark-and-excursion"
        | "save-match-data"
        | "with-local-quit"
        | "declare"
        | "eval-when-compile"
        | "eval-and-compile" => Some(FallbackMacroSpec { min: 0, max: None }),
        "with-temp-message" => Some(FallbackMacroSpec { min: 1, max: None }),
        "with-demoted-errors" => Some(FallbackMacroSpec { min: 1, max: None }),
        "bound-and-true-p" => Some(FallbackMacroSpec {
            min: 1,
            max: Some(1),
        }),
        "defvar-local" => Some(FallbackMacroSpec {
            min: 2,
            max: Some(3),
        }),
        "prog2" => Some(FallbackMacroSpec { min: 2, max: None }),
        _ => None,
    }
}

pub(crate) fn has_fallback_macro(name: &str) -> bool {
    fallback_macro_spec(name).is_some()
}

fn fallback_macro_params(spec: FallbackMacroSpec) -> LambdaParams {
    let required: Vec<String> = (0..spec.min).map(|idx| format!("arg{idx}")).collect();
    let (optional, rest) = match spec.max {
        None => (Vec::new(), Some("rest".to_string())),
        Some(max) => {
            debug_assert!(max >= spec.min);
            let optional_count = max.saturating_sub(spec.min);
            let optional = (0..optional_count)
                .map(|idx| format!("arg{}", spec.min + idx))
                .collect();
            (optional, None)
        }
    };

    LambdaParams {
        required,
        optional,
        rest,
    }
}

/// Return a placeholder macro object for evaluator-integrated macro names.
///
/// This keeps `fboundp`/`symbol-function`/`indirect-function`/`macrop`
/// introspection aligned with Emacs for core macros even when they are not
/// materialized via Elisp bootstrap code in the function cell.
pub(crate) fn fallback_macro_value(name: &str) -> Option<Value> {
    let spec = fallback_macro_spec(name)?;
    Some(Value::Macro(Arc::new(LambdaData {
        params: fallback_macro_params(spec),
        body: vec![],
        env: None,
        docstring: None,
    })))
}

// ---------------------------------------------------------------------------
// Arity helpers
// ---------------------------------------------------------------------------

/// Build a cons cell `(MIN . MAX)` representing arity.
/// `max` of `None` means "many" (unbounded &rest), represented by the
/// symbol `many`.
fn arity_cons(min: usize, max: Option<usize>) -> Value {
    let min_val = Value::Int(min as i64);
    let max_val = match max {
        Some(n) => Value::Int(n as i64),
        None => Value::symbol("many"),
    };
    Value::cons(min_val, max_val)
}

fn arity_unevalled(min: usize) -> Value {
    Value::cons(Value::Int(min as i64), Value::symbol("unevalled"))
}

fn is_cxr_subr_name(name: &str) -> bool {
    let Some(inner) = name.strip_prefix('c').and_then(|s| s.strip_suffix('r')) else {
        return false;
    };
    !inner.is_empty() && inner.chars().all(|ch| ch == 'a' || ch == 'd')
}

fn subr_arity_value(name: &str) -> Value {
    match name {
        // Oracle-compatible overrides for core subrs used in vm-compat.
        n if is_cxr_subr_name(n) => arity_cons(1, Some(1)),
        "message" | "message-box" | "message-or-box" | "format" | "format-message"
        | "user-error" => {
            arity_cons(1, None)
        }
        "always" => arity_cons(0, None),
        "/" | "<" | "<=" | "=" | ">" | ">=" | "apply" | "funcall" | "funcall-interactively" => {
            arity_cons(1, None)
        }
        "funcall-with-delayed-message" => arity_cons(3, Some(3)),
        "cons" => arity_cons(2, Some(2)),
        "1+" | "1-" | "abs" => arity_cons(1, Some(1)),
        "%" | "/=" | "ash" | "length<" | "length=" | "length>" => arity_cons(2, Some(2)),
        "copy-alist" | "copy-hash-table" | "copy-keymap" | "copy-sequence" | "purecopy" => {
            arity_cons(1, Some(1))
        }
        "copy-marker" => arity_cons(0, Some(2)),
        "copy-category-table" => arity_cons(0, Some(1)),
        "copy-syntax-table" => arity_cons(0, Some(1)),
        "copy-file" => arity_cons(2, Some(6)),
        "copy-region-as-kill" => arity_cons(2, Some(3)),
        "copy-to-register" => arity_cons(3, Some(5)),
        "make-directory" => arity_cons(1, Some(2)),
        "make-directory-internal" => arity_cons(1, Some(1)),
        "make-temp-file" => arity_cons(1, Some(4)),
        "make-temp-name" => arity_cons(1, Some(1)),
        "make-nearby-temp-file" => arity_cons(1, Some(3)),
        "make-symbolic-link" | "rename-file" => arity_cons(2, Some(3)),
        "auto-save-file-name-p" => arity_cons(1, Some(1)),
        "abbreviate-file-name" => arity_cons(1, Some(1)),
        "convert-standard-filename" => arity_cons(1, Some(1)),
        "file-name-absolute-p"
        | "backup-file-name-p"
        | "file-name-as-directory"
        | "file-name-directory"
        | "file-name-nondirectory"
        | "file-name-parent-directory"
        | "file-name-split"
        | "file-name-sans-extension"
        | "file-name-case-insensitive-p" => arity_cons(1, Some(1)),
        "file-name-all-completions" | "file-name-with-extension" => arity_cons(2, Some(2)),
        "file-name-completion" => arity_cons(2, Some(3)),
        "file-name-extension" | "file-name-sans-versions" => arity_cons(1, Some(2)),
        "file-local-name" => arity_cons(1, Some(1)),
        "file-nlinks" => arity_cons(1, Some(1)),
        "file-name-concat" => arity_cons(1, None),
        "file-remote-p" => arity_cons(1, Some(3)),
        "file-truename" => arity_cons(1, Some(3)),
        "get-truename-buffer" | "unhandled-file-name-directory" => arity_cons(1, Some(1)),
        "find-buffer" => arity_cons(2, Some(2)),
        "find-file" => arity_cons(1, Some(2)),
        "find-file-name-handler" => arity_cons(2, Some(2)),
        "find-file-noselect" => arity_cons(1, Some(4)),
        "insert-file-contents" => arity_cons(1, Some(5)),
        "load" => arity_cons(1, Some(5)),
        "provide" => arity_cons(1, Some(2)),
        "require" => arity_cons(1, Some(3)),
        "load-file" => arity_cons(1, Some(1)),
        "locate-file" | "locate-file-internal" => arity_cons(2, Some(4)),
        "file-attributes" | "file-modes" => arity_cons(1, Some(2)),
        "file-accessible-directory-p"
        | "file-acl"
        | "file-directory-p"
        | "file-executable-p"
        | "file-exists-p"
        | "file-locked-p"
        | "file-readable-p"
        | "file-regular-p"
        | "file-selinux-context"
        | "file-system-info"
        | "file-symlink-p"
        | "file-writable-p" => arity_cons(1, Some(1)),
        "file-newer-than-file-p" | "file-equal-p" | "file-in-directory-p" => arity_cons(2, Some(2)),
        "goto-char" => arity_cons(1, Some(1)),
        "exchange-point-and-mark" => arity_cons(0, Some(1)),
        "beginning-of-line"
        | "end-of-line"
        | "beginning-of-buffer"
        | "end-of-buffer"
        | "forward-char"
        | "backward-char"
        | "forward-word"
        | "backward-word"
        | "forward-line" => arity_cons(0, Some(1)),
        "current-buffer" | "buffer-string" | "point" | "point-max" | "point-min" | "bobp"
        | "eobp" | "bolp" | "eolp" | "erase-buffer" | "widen" => arity_cons(0, Some(0)),
        "barf-if-buffer-read-only" => arity_cons(0, Some(1)),
        "mark-marker" | "point-marker" | "point-max-marker" | "point-min-marker" | "pop-mark" => {
            arity_cons(0, Some(0))
        }
        "mark" => arity_cons(0, Some(1)),
        "push-mark" => arity_cons(0, Some(3)),
        "file-group-gid" | "group-gid" | "group-real-gid" | "interactive-p"
        | "last-nonminibuffer-frame" => {
            arity_cons(0, Some(0))
        }
        "group-name" => arity_cons(1, Some(1)),
        "following-char" | "garbage-collect" | "get-load-suffixes" | "byteorder" => {
            arity_cons(0, Some(0))
        }
        "buffer-file-name"
        | "buffer-base-buffer"
        | "buffer-last-name"
        | "buffer-name"
        | "file-name-base"
        | "buffer-size"
        | "buffer-chars-modified-tick"
        | "buffer-modified-p"
        | "buffer-modified-tick"
        | "buffer-list"
        | "buffer-disable-undo"
        | "buffer-enable-undo"
        | "buffer-hash"
        | "buffer-local-variables"
        | "buffer-line-statistics" => arity_cons(0, Some(1)),
        "other-buffer" => arity_cons(0, Some(3)),
        "bury-buffer-internal" => arity_cons(1, Some(1)),
        "marker-buffer" | "marker-insertion-type" | "marker-position" | "markerp" => {
            arity_cons(1, Some(1))
        }
        "get-byte" => arity_cons(0, Some(2)),
        "get-register" => arity_cons(1, Some(1)),
        "get-buffer" | "get-file-buffer" => arity_cons(1, Some(1)),
        "get-buffer-create" | "generate-new-buffer-name" | "generate-new-buffer" => {
            arity_cons(1, Some(2))
        }
        "buffer-live-p" | "buffer-swap-text" => arity_cons(1, Some(1)),
        "buffer-local-value" | "buffer-substring" | "buffer-substring-no-properties" => {
            arity_cons(2, Some(2))
        }
        "char-after"
        | "char-before"
        | "charset-after"
        | "charset-id-internal"
        | "charset-priority-list" => arity_cons(0, Some(1)),
        "char-category-set"
        | "char-displayable-p"
        | "char-or-string-p"
        | "char-resolve-modifiers"
        | "char-syntax"
        | "char-width"
        | "char-table-p"
        | "char-table-parent"
        | "char-table-subtype"
        | "charset-plist"
        | "charsetp"
        | "closurep"
        | "compiled-function-p"
        | "custom-variable-p"
        | "default-boundp"
        | "default-value"
        | "default-toplevel-value"
        | "decode-big5-char"
        | "decode-sjis-char"
        | "encode-big5-char"
        | "encode-sjis-char"
        | "native-comp-function-p" => arity_cons(1, Some(1)),
        "char-charset" => arity_cons(1, Some(2)),
        "char-table-extra-slot"
        | "char-table-range"
        | "define-charset-alias"
        | "get-unused-iso-final-char" => arity_cons(2, Some(2)),
        "declare-equiv-charset" => arity_cons(4, Some(4)),
        "decode-char" => arity_cons(2, Some(2)),
        "fceiling" | "ffloor" | "frexp" | "fround" | "framep" | "ftruncate" | "fixnump" => {
            arity_cons(1, Some(1))
        }
        "ldexp" => arity_cons(2, Some(2)),
        "sqrt" | "sin" | "cos" | "tan" | "asin" | "acos" | "exp" | "isnan" => {
            arity_cons(1, Some(1))
        }
        "atan" | "log" => arity_cons(1, Some(2)),
        "expt" => arity_cons(2, Some(2)),
        "random" => arity_cons(0, Some(1)),
        "logb" | "lognot" => arity_cons(1, Some(1)),
        "bignump"
        | "boundp"
        | "byte-code-function-p"
        | "car-safe"
        | "cdr-safe"
        | "integer-or-marker-p"
        | "bare-symbol"
        | "bare-symbol-p" => arity_cons(1, Some(1)),
        "identity"
        | "length"
        | "interpreted-function-p"
        | "invisible-p"
        | "macrop"
        | "functionp"
        | "prefix-numeric-value" => arity_cons(1, Some(1)),
        "gensym" => arity_cons(0, Some(1)),
        "fboundp" | "func-arity" | "symbol-function" | "symbol-value" | "fmakunbound"
        | "makunbound" => arity_cons(1, Some(1)),
        "make-symbol" | "symbol-name" | "symbol-plist" => arity_cons(1, Some(1)),
        "intern" | "intern-soft" | "indirect-function" | "unintern" => arity_cons(1, Some(2)),
        "symbol-file" => arity_cons(1, Some(3)),
        "fset" | "set" | "get" | "set-marker-insertion-type" => arity_cons(2, Some(2)),
        "defalias" => arity_cons(2, Some(3)),
        "put" => arity_cons(3, Some(3)),
        "set-mark" | "set-mark-command" => arity_cons(1, Some(1)),
        "set-marker" => arity_cons(2, Some(3)),
        "increment-register"
        | "number-to-register"
        | "set-register"
        | "register-ccl-program"
        | "register-code-conversion-map" => arity_cons(2, Some(2)),
        "insert-register" | "point-to-register" => arity_cons(1, Some(2)),
        "insert-byte" => arity_cons(2, Some(3)),
        "insert-char" => arity_cons(1, Some(3)),
        "hash-table-p"
        | "clrhash"
        | "hash-table-count"
        | "internal--hash-table-buckets"
        | "internal--hash-table-histogram"
        | "internal--hash-table-index-size"
        | "sxhash-eq"
        | "sxhash-eql"
        | "sxhash-equal"
        | "sxhash-equal-including-properties" => arity_cons(1, Some(1)),
        "gethash" => arity_cons(2, Some(3)),
        "puthash" => arity_cons(3, Some(3)),
        "remhash" | "maphash" => arity_cons(2, Some(2)),
        "hash-table-test"
        | "hash-table-size"
        | "hash-table-rehash-size"
        | "hash-table-rehash-threshold"
        | "hash-table-weakness" => arity_cons(1, Some(1)),
        "max" | "min" => arity_cons(1, None),
        "assq" | "assq-delete-all" | "car-less-than-car" | "member" | "memq" | "memql"
        | "rassoc" | "rassq" => {
            arity_cons(2, Some(2))
        }
        "mod" | "make-list" | "mapc" | "mapcan" | "mapcar" | "nth" | "nthcdr" | "remq" => {
            arity_cons(2, Some(2))
        }
        "delete" | "delq" | "elt" => arity_cons(2, Some(2)),
        "mapconcat" => arity_cons(2, Some(3)),
        "assoc" | "assoc-delete-all" | "assoc-string" => arity_cons(2, Some(3)),
        "assoc-default" => arity_cons(2, Some(4)),
        "alist-get" => arity_cons(2, Some(5)),
        "last" | "butlast" => arity_cons(1, Some(2)),
        "nconc" => arity_cons(0, None),
        "number-sequence" => arity_cons(1, Some(3)),
        "nreverse" | "proper-list-p" | "reverse" | "safe-length" => arity_cons(1, Some(1)),
        "back-to-indentation" | "backward-prefix-chars" => arity_cons(0, Some(0)),
        "backward-sexp" => arity_cons(0, Some(2)),
        "backward-kill-word"
        | "move-beginning-of-line"
        | "move-end-of-line"
        | "capitalize"
        | "capitalize-word"
        | "downcase-word"
        | "kill-local-variable"
        | "kill-word" => arity_cons(1, Some(1)),
        "newline" | "next-line" | "terpri" => arity_cons(0, Some(2)),
        "reindent-then-newline-and-indent" => arity_cons(0, Some(0)),
        "previous-line" => arity_cons(0, Some(2)),
        "newline-and-indent" => arity_cons(0, Some(1)),
        "open-line" => arity_cons(1, Some(1)),
        "other-window" | "goto-line" => arity_cons(1, Some(3)),
        "flush-lines" | "keep-lines" | "how-many" => arity_cons(1, Some(4)),
        "local-variable-p" => arity_cons(1, Some(2)),
        "buffer-local-boundp" => arity_cons(2, Some(2)),
        "locale-info" => arity_cons(1, Some(1)),
        "max-char" => arity_cons(0, Some(1)),
        "memory-use-counts" | "make-marker" => arity_cons(0, Some(0)),
        "make-local-variable" | "make-variable-buffer-local" => arity_cons(1, Some(1)),
        "make-syntax-table" => arity_cons(0, Some(1)),
        "mapatoms" => arity_cons(1, Some(2)),
        "map-char-table" => arity_cons(2, Some(2)),
        "capitalize-region" => arity_cons(2, Some(3)),
        "downcase-region" | "kill-region" | "kill-ring-save" => arity_cons(2, Some(3)),
        "kill-append" => arity_cons(2, Some(2)),
        "kill-new" => arity_cons(1, Some(2)),
        "kill-buffer" | "kill-line" | "kill-whole-line" | "just-one-space" => {
            arity_cons(0, Some(1))
        }
        "add-hook" => arity_cons(2, Some(4)),
        "add-name-to-file" => arity_cons(2, Some(3)),
        "add-face-text-property" => arity_cons(3, Some(5)),
        "add-text-properties" | "set-text-properties" => arity_cons(3, Some(4)),
        "put-text-property" | "text-property-any" | "text-property-not-all" => {
            arity_cons(4, Some(5))
        }
        "remove-text-properties" | "remove-list-of-text-properties" | "move-overlay" => {
            arity_cons(3, Some(4))
        }
        "get-text-property" | "get-char-property" | "get-pos-property" => arity_cons(2, Some(3)),
        "get-char-property-and-overlay" => arity_cons(2, Some(3)),
        "get-display-property" => arity_cons(2, Some(4)),
        "text-properties-at" | "overlays-at" => arity_cons(1, Some(2)),
        "next-single-property-change"
        | "next-single-char-property-change"
        | "previous-single-property-change"
        | "previous-single-char-property-change" => arity_cons(2, Some(4)),
        "next-property-change" | "previous-property-change" => arity_cons(1, Some(3)),
        "next-char-property-change" => arity_cons(1, Some(2)),
        "previous-char-property-change" => arity_cons(1, Some(2)),
        "make-overlay" => arity_cons(2, Some(5)),
        "overlay-put" => arity_cons(3, Some(3)),
        "overlay-get" | "overlays-in" => arity_cons(2, Some(2)),
        "overlay-start" | "overlay-end" | "overlay-buffer" | "overlay-properties" | "overlayp" => {
            arity_cons(1, Some(1))
        }
        "next-overlay-change" | "previous-overlay-change" => arity_cons(1, Some(1)),
        "remove-overlays" => arity_cons(0, Some(4)),
        "primitive-undo" => arity_cons(2, Some(2)),
        "prin1" | "prin1-to-string" => arity_cons(1, Some(3)),
        "princ" | "print" => arity_cons(1, Some(2)),
        "propertize" => arity_cons(1, None),
        "face-attribute-relative-p" | "font-get" | "internal-merge-in-global-face" => {
            arity_cons(2, Some(2))
        }
        "face-font" | "font-xlfd-name" => arity_cons(1, Some(3)),
        "close-font" => arity_cons(1, Some(2)),
        "face-id"
        | "fontp"
        | "internal-make-lisp-face"
        | "internal-lisp-face-empty-p"
        | "internal-lisp-face-p" => arity_cons(1, Some(2)),
        "face-list" => arity_cons(0, Some(0)),
        "font-put" => arity_cons(3, Some(3)),
        "internal-copy-lisp-face" => arity_cons(4, Some(4)),
        "internal-face-x-get-resource"
        | "internal-get-lisp-face-attribute"
        | "internal-lisp-face-equal-p" => arity_cons(2, Some(3)),
        "internal-lisp-face-attribute-values"
        | "internal-set-alternative-font-family-alist"
        | "internal-set-alternative-font-registry-alist"
        | "internal-set-font-selection-order" => arity_cons(1, Some(1)),
        "internal-set-lisp-face-attribute" => arity_cons(3, Some(4)),
        "internal--after-save-selected-window" => arity_cons(1, Some(1)),
        "internal--before-save-selected-window" => arity_cons(0, Some(0)),
        "internal--define-uninitialized-variable" => arity_cons(1, Some(2)),
        "internal--labeled-narrow-to-region" => arity_cons(3, Some(3)),
        "internal--labeled-widen" => arity_cons(1, Some(1)),
        "internal--obarray-buckets" => arity_cons(1, Some(1)),
        "internal--set-buffer-modified-tick" => arity_cons(1, Some(2)),
        "internal--track-mouse" => arity_cons(1, Some(1)),
        "internal-char-font" => arity_cons(1, Some(2)),
        "internal-complete-buffer" => arity_cons(3, Some(3)),
        "internal-describe-syntax-value" => arity_cons(1, Some(1)),
        "internal-event-symbol-parse-modifiers" => arity_cons(1, Some(1)),
        "internal-handle-focus-in" => arity_cons(1, Some(1)),
        "internal-make-var-non-special" => arity_cons(1, Some(1)),
        "internal-set-lisp-face-attribute-from-resource" => arity_cons(3, Some(4)),
        "internal-stack-stats" => arity_cons(0, Some(0)),
        "internal-subr-documentation" => arity_cons(1, Some(1)),
        "dump-emacs-portable" => arity_cons(1, Some(2)),
        "dump-emacs-portable--sort-predicate" => arity_cons(2, Some(2)),
        "dump-emacs-portable--sort-predicate-copied" => arity_cons(2, Some(2)),
        "malloc-info" => arity_cons(0, Some(0)),
        "malloc-trim" => arity_cons(0, Some(1)),
        "marker-last-position" => arity_cons(1, Some(1)),
        "match-data--translate" => arity_cons(1, Some(1)),
        "memory-info" => arity_cons(0, Some(0)),
        "make-frame-invisible" => arity_cons(0, Some(2)),
        "make-terminal-frame" => arity_cons(1, Some(1)),
        "menu-bar-menu-at-x-y" => arity_cons(2, Some(3)),
        "menu-or-popup-active-p" => arity_cons(0, Some(0)),
        "module-load" => arity_cons(1, Some(1)),
        "mouse-pixel-position" => arity_cons(0, Some(0)),
        "mouse-position" => arity_cons(0, Some(0)),
        "newline-cache-check" => arity_cons(0, Some(1)),
        "native-comp-available-p" => arity_cons(0, Some(0)),
        "native-comp-unit-file" => arity_cons(1, Some(1)),
        "native-comp-unit-set-file" => arity_cons(2, Some(2)),
        "native-elisp-load" => arity_cons(1, Some(2)),
        "new-fontset" => arity_cons(2, Some(2)),
        "object-intervals" => arity_cons(1, Some(1)),
        "old-selected-frame" => arity_cons(0, Some(0)),
        "old-selected-window" => arity_cons(0, Some(0)),
        "open-dribble-file" => arity_cons(1, Some(1)),
        "open-font" => arity_cons(1, Some(3)),
        "optimize-char-table" => arity_cons(1, Some(2)),
        "overlay-lists" => arity_cons(0, Some(0)),
        "overlay-recenter" => arity_cons(1, Some(1)),
        "pdumper-stats" => arity_cons(0, Some(0)),
        "play-sound-internal" => arity_cons(1, Some(1)),
        "position-symbol" => arity_cons(2, Some(2)),
        "posn-at-point" => arity_cons(0, Some(2)),
        "posn-at-x-y" => arity_cons(2, Some(4)),
        "profiler-cpu-log" => arity_cons(0, Some(0)),
        "profiler-cpu-running-p" => arity_cons(0, Some(0)),
        "profiler-cpu-start" => arity_cons(1, Some(1)),
        "profiler-cpu-stop" => arity_cons(0, Some(0)),
        "profiler-memory-log" => arity_cons(0, Some(0)),
        "profiler-memory-running-p" => arity_cons(0, Some(0)),
        "profiler-memory-start" => arity_cons(0, Some(0)),
        "profiler-memory-stop" => arity_cons(0, Some(0)),
        "query-font" => arity_cons(1, Some(1)),
        "query-fontset" => arity_cons(1, Some(2)),
        "read-positioning-symbols" => arity_cons(0, Some(1)),
        "recent-auto-save-p" => arity_cons(0, Some(0)),
        "record" => arity_cons(1, None),
        "recordp" => arity_cons(1, Some(1)),
        "reconsider-frame-fonts" => arity_cons(1, Some(1)),
        "redirect-debugging-output" => arity_cons(1, Some(2)),
        "redirect-frame-focus" => arity_cons(1, Some(2)),
        "remove-pos-from-symbol" => arity_cons(1, Some(1)),
        "resize-mini-window-internal" => arity_cons(1, Some(1)),
        "restore-buffer-modified-p" => arity_cons(1, Some(1)),
        "set--this-command-keys" => arity_cons(1, Some(1)),
        "set-buffer-auto-saved" => arity_cons(0, Some(0)),
        "set-buffer-redisplay" => arity_cons(4, Some(4)),
        "set-charset-plist" => arity_cons(2, Some(2)),
        "set-fontset-font" => arity_cons(3, Some(5)),
        "set-frame-selected-window" => arity_cons(2, Some(3)),
        "set-frame-window-state-change" => arity_cons(0, Some(2)),
        "set-fringe-bitmap-face" => arity_cons(1, Some(2)),
        "set-minibuffer-window" => arity_cons(1, Some(1)),
        "set-mouse-pixel-position" => arity_cons(3, Some(3)),
        "set-mouse-position" => arity_cons(3, Some(3)),
        "set-window-combination-limit" => arity_cons(2, Some(2)),
        "set-window-new-normal" => arity_cons(1, Some(2)),
        "set-window-new-pixel" => arity_cons(2, Some(3)),
        "set-window-new-total" => arity_cons(2, Some(3)),
        "sort-charsets" => arity_cons(1, Some(1)),
        "split-char" => arity_cons(1, Some(1)),
        "string-distance" => arity_cons(2, Some(3)),
        "subst-char-in-region" => arity_cons(4, Some(5)),
        "subr-native-comp-unit" => arity_cons(1, Some(1)),
        "subr-native-lambda-list" => arity_cons(1, Some(1)),
        "subr-type" => arity_cons(1, Some(1)),
        "this-single-command-keys" => arity_cons(0, Some(0)),
        "this-single-command-raw-keys" => arity_cons(0, Some(0)),
        "thread--blocker" => arity_cons(1, Some(1)),
        "tool-bar-get-system-style" => arity_cons(0, Some(0)),
        "tool-bar-pixel-width" => arity_cons(0, Some(1)),
        "translate-region-internal" => arity_cons(3, Some(3)),
        "transpose-regions" => arity_cons(4, Some(5)),
        "tty--output-buffer-size" => arity_cons(0, Some(1)),
        "tty--set-output-buffer-size" => arity_cons(1, Some(2)),
        "tty-suppress-bold-inverse-default-colors" => arity_cons(1, Some(1)),
        "unencodable-char-position" => arity_cons(3, Some(5)),
        "unicode-property-table-internal" => arity_cons(1, Some(1)),
        "unify-charset" => arity_cons(1, Some(3)),
        "unix-sync" => arity_cons(0, Some(0)),
        "value<" => arity_cons(2, Some(2)),
        "variable-binding-locus" => arity_cons(1, Some(1)),
        "byte-code" => arity_cons(3, Some(3)),
        "decode-coding-region" => arity_cons(3, Some(4)),
        "defconst-1" => arity_cons(2, Some(3)),
        "define-coding-system-internal" => arity_cons(13, None),
        "defvar-1" => arity_cons(2, Some(3)),
        "defvaralias" => arity_cons(2, Some(3)),
        "encode-coding-region" => arity_cons(3, Some(4)),
        "find-operation-coding-system" => arity_cons(1, None),
        "handler-bind-1" => arity_cons(1, None),
        "indirect-variable" => arity_cons(1, Some(1)),
        "insert-and-inherit" => arity_cons(0, None),
        "insert-before-markers-and-inherit" => arity_cons(0, None),
        "insert-buffer-substring" => arity_cons(1, Some(3)),
        "iso-charset" => arity_cons(3, Some(3)),
        "keymap--get-keyelt" => arity_cons(2, Some(2)),
        "keymap-prompt" => arity_cons(1, Some(1)),
        "kill-all-local-variables" => arity_cons(0, Some(1)),
        "kill-emacs" => arity_cons(0, Some(2)),
        "lower-frame" => arity_cons(0, Some(1)),
        "lread--substitute-object-in-subtree" => arity_cons(3, Some(3)),
        "macroexpand" => arity_cons(1, Some(2)),
        "make-byte-code" => arity_cons(4, None),
        "make-char" => arity_cons(1, Some(5)),
        "make-closure" => arity_cons(1, None),
        "make-finalizer" => arity_cons(1, Some(1)),
        "make-indirect-buffer" => arity_cons(2, Some(4)),
        "make-interpreted-closure" => arity_cons(3, Some(5)),
        "make-record" => arity_cons(3, Some(3)),
        "make-temp-file-internal" => arity_cons(4, Some(4)),
        "map-charset-chars" => arity_cons(2, Some(5)),
        "map-keymap" => arity_cons(2, Some(3)),
        "map-keymap-internal" => arity_cons(2, Some(2)),
        "mapbacktrace" => arity_cons(1, Some(2)),
        "minibuffer-innermost-command-loop-p" => arity_cons(0, Some(1)),
        "minibuffer-prompt-end" => arity_cons(0, Some(0)),
        "next-frame" => arity_cons(0, Some(2)),
        "ntake" => arity_cons(2, Some(2)),
        "obarray-clear" => arity_cons(1, Some(1)),
        "obarray-make" => arity_cons(0, Some(1)),
        "previous-frame" => arity_cons(0, Some(2)),
        "put-unicode-property-internal" => arity_cons(3, Some(3)),
        "raise-frame" => arity_cons(0, Some(1)),
        "re--describe-compiled" => arity_cons(1, Some(2)),
        "redisplay" => arity_cons(0, Some(1)),
        "rename-buffer" => arity_cons(1, Some(2)),
        "replace-buffer-contents" => arity_cons(1, Some(3)),
        "set-buffer-major-mode" => arity_cons(1, Some(1)),
        "set-buffer-multibyte" => arity_cons(1, Some(1)),
        "setplist" => arity_cons(2, Some(2)),
        "split-window-internal" => arity_cons(4, Some(4)),
        "suspend-emacs" => arity_cons(0, Some(1)),
        "vertical-motion" => arity_cons(1, Some(3)),
        "x-begin-drag" => arity_cons(1, Some(6)),
        "x-create-frame" => arity_cons(1, Some(1)),
        "x-double-buffered-p" => arity_cons(0, Some(1)),
        "x-menu-bar-open-internal" => arity_cons(0, Some(1)),
        "xw-color-defined-p" => arity_cons(1, Some(2)),
        "xw-color-values" => arity_cons(1, Some(2)),
        "xw-display-color-p" => arity_cons(0, Some(1)),
        "merge-face-attribute" => arity_cons(3, Some(3)),
        "put-image" => arity_cons(2, Some(4)),
        "insert-image" => arity_cons(1, Some(5)),
        "lookup-image-map" => arity_cons(3, Some(3)),
        "query-replace" | "query-replace-regexp" => arity_cons(2, Some(7)),
        "quoted-insert" => arity_cons(1, Some(1)),
        "looking-at" | "posix-looking-at" => arity_cons(1, Some(2)),
        "match-beginning" | "match-end" => arity_cons(1, Some(1)),
        "match-data" => arity_cons(0, Some(3)),
        "match-string" => arity_cons(1, Some(2)),
        "replace-match" => arity_cons(1, Some(5)),
        "replace-regexp-in-string" => arity_cons(3, Some(7)),
        "string-match" | "posix-string-match" => arity_cons(2, Some(4)),
        "string-as-multibyte"
        | "string-as-unibyte"
        | "string-bytes"
        | "string-make-multibyte"
        | "string-make-unibyte"
        | "string-to-multibyte"
        | "string-to-list"
        | "string-to-syntax"
        | "string-to-unibyte"
        | "substitute-in-file-name"
        | "syntax-after"
        | "syntax-class-to-char"
        | "syntax-table-p" => arity_cons(1, Some(1)),
        "string-collate-equalp" | "string-collate-lessp" => arity_cons(2, Some(4)),
        "make-string" => arity_cons(2, Some(3)),
        "string-join" => arity_cons(1, Some(2)),
        "string-lines" => arity_cons(1, Some(3)),
        "string-replace" => arity_cons(3, Some(3)),
        "split-string" => arity_cons(1, Some(4)),
        "string-search" => arity_cons(2, Some(3)),
        "string-prefix-p" | "string-suffix-p" => arity_cons(2, Some(3)),
        "string-trim" => arity_cons(1, Some(3)),
        "string-trim-left" | "string-trim-right" => arity_cons(1, Some(2)),
        "string-version-lessp" => arity_cons(2, Some(2)),
        "string-width" => arity_cons(1, Some(3)),
        // Startup helper wrappers / autoload-backed dispatch helpers.
        "autoloadp"
        | "bounds-of-thing-at-point"
        | "describe-function"
        | "insert-rectangle"
        | "kbd-macro-query"
        | "looking-at-p"
        | "safe-date-to-time"
        | "string-blank-p"
        | "string-chop-newline"
        | "string-clean-whitespace"
        | "string-empty-p"
        | "string-glyph-split"
        | "string-pixel-width"
        | "string-to-vector"
        | "upcase-char" => arity_cons(1, Some(1)),
        "symbol-at-point" | "yank-rectangle" => arity_cons(0, Some(0)),
        "word-at-point" => arity_cons(0, Some(1)),
        "bookmark-delete" | "bookmark-jump" | "bookmark-rename" | "insert-kbd-macro" => {
            arity_cons(1, Some(2))
        }
        "bookmark-load" => arity_cons(1, Some(4)),
        "bookmark-save" => arity_cons(0, Some(3)),
        "bookmark-set" => arity_cons(0, Some(2)),
        "clear-rectangle"
        | "delete-extract-rectangle"
        | "delete-rectangle"
        | "kill-rectangle"
        | "open-rectangle" => arity_cons(2, Some(3)),
        "extract-rectangle" => arity_cons(2, Some(2)),
        "describe-variable" => arity_cons(1, Some(3)),
        "format-seconds" => arity_cons(2, Some(2)),
        "format-spec" => arity_cons(2, Some(4)),
        "replace-rectangle" | "string-rectangle" => arity_cons(3, Some(3)),
        "thing-at-point" => arity_cons(1, Some(2)),
        "seq-concatenate" | "seq-drop" | "seq-empty-p" | "seq-reverse" | "seq-take"
        | "seq-uniq" => arity_cons(1, None),
        "seq-count"
        | "seq-do"
        | "seq-every-p"
        | "seq-into"
        | "seq-some"
        | "string-fill"
        | "string-equal-ignore-case" => arity_cons(2, Some(2)),
        "seq-contains-p" | "seq-position" | "seq-subseq" | "string-match-p" => {
            arity_cons(2, Some(3))
        }
        "seq-length" | "seq-max" | "seq-min" => arity_cons(1, Some(1)),
        "seq-mapn" | "seq-sort" => arity_cons(2, None),
        "seq-reduce" => arity_cons(3, Some(3)),
        "string-limit" | "string-pad" => arity_cons(2, Some(4)),
        "sort" | "syntax-ppss-flush-cache" => arity_cons(1, None),
        "subst-char-in-string" => arity_cons(3, Some(4)),
        "self-insert-command"
        | "single-key-description"
        | "skip-chars-backward"
        | "skip-chars-forward"
        | "skip-syntax-backward"
        | "skip-syntax-forward"
        | "transpose-sexps" => arity_cons(1, Some(2)),
        "shell-command-to-string"
        | "special-form-p"
        | "store-kbd-macro-event"
        | "unibyte-char-to-multibyte"
        | "multibyte-char-to-unibyte"
        | "multibyte-string-p"
        | "upcase-initials"
        | "upcase-word"
        | "use-global-map"
        | "use-local-map"
        | "view-register" => arity_cons(1, Some(1)),
        "subr-arity" | "subr-name" | "subrp" => arity_cons(1, Some(1)),
        "signal" | "take" => arity_cons(2, Some(2)),
        "secure-hash" => arity_cons(2, Some(5)),
        "secure-hash-algorithms" => arity_cons(0, Some(0)),
        "split-window" => arity_cons(0, Some(4)),
        "switch-to-buffer" => arity_cons(1, Some(3)),
        "combine-after-change-execute"
        | "this-command-keys"
        | "this-command-keys-vector"
        | "region-active-p"
        | "undo-boundary"
        | "universal-argument"
        | "use-region-p" => arity_cons(0, Some(0)),
        "clear-this-command-keys" | "transient-mark-mode" | "undo" | "yank" | "yank-pop" => {
            arity_cons(0, Some(1))
        }
        "transpose-chars"
        | "transpose-lines"
        | "transpose-paragraphs"
        | "transpose-sentences"
        | "transpose-words" => arity_cons(1, Some(1)),
        "treesit-available-p" => arity_cons(0, Some(0)),
        "treesit-compiled-query-p" => arity_cons(1, Some(1)),
        "treesit-induce-sparse-tree" => arity_cons(2, Some(4)),
        "treesit-language-abi-version" => arity_cons(0, Some(1)),
        "treesit-language-available-p" => arity_cons(1, Some(2)),
        "treesit-library-abi-version" => arity_cons(0, Some(1)),
        "treesit-node-check" => arity_cons(2, Some(2)),
        "treesit-node-child" => arity_cons(2, Some(3)),
        "treesit-node-child-by-field-name" => arity_cons(2, Some(2)),
        "treesit-node-child-count" => arity_cons(1, Some(2)),
        "treesit-node-descendant-for-range" => arity_cons(3, Some(4)),
        "treesit-node-end" => arity_cons(1, Some(1)),
        "treesit-node-eq" => arity_cons(2, Some(2)),
        "treesit-node-field-name-for-child" => arity_cons(2, Some(2)),
        "treesit-node-first-child-for-pos" => arity_cons(2, Some(3)),
        "treesit-node-match-p" => arity_cons(2, Some(3)),
        "treesit-node-next-sibling" => arity_cons(1, Some(2)),
        "treesit-node-p" => arity_cons(1, Some(1)),
        "treesit-node-parent" => arity_cons(1, Some(1)),
        "treesit-node-parser" => arity_cons(1, Some(1)),
        "treesit-node-prev-sibling" => arity_cons(1, Some(2)),
        "treesit-node-start" => arity_cons(1, Some(1)),
        "treesit-node-string" => arity_cons(1, Some(1)),
        "treesit-node-type" => arity_cons(1, Some(1)),
        "treesit-parser-add-notifier" => arity_cons(2, Some(2)),
        "treesit-parser-buffer" => arity_cons(1, Some(1)),
        "treesit-parser-create" => arity_cons(1, Some(4)),
        "treesit-parser-delete" => arity_cons(1, Some(1)),
        "treesit-parser-included-ranges" => arity_cons(1, Some(1)),
        "treesit-parser-language" => arity_cons(1, Some(1)),
        "treesit-parser-list" => arity_cons(0, Some(3)),
        "treesit-parser-notifiers" => arity_cons(1, Some(1)),
        "treesit-parser-p" => arity_cons(1, Some(1)),
        "treesit-parser-remove-notifier" => arity_cons(2, Some(2)),
        "treesit-parser-root-node" => arity_cons(1, Some(1)),
        "treesit-parser-set-included-ranges" => arity_cons(2, Some(2)),
        "treesit-parser-tag" => arity_cons(1, Some(1)),
        "treesit-pattern-expand" => arity_cons(1, Some(1)),
        "treesit-query-capture" => arity_cons(2, Some(5)),
        "treesit-query-compile" => arity_cons(2, Some(3)),
        "treesit-query-expand" => arity_cons(1, Some(1)),
        "treesit-query-language" => arity_cons(1, Some(1)),
        "treesit-query-p" => arity_cons(1, Some(1)),
        "treesit-search-forward" => arity_cons(2, Some(4)),
        "treesit-search-subtree" => arity_cons(2, Some(5)),
        "treesit-subtree-stat" => arity_cons(1, Some(1)),
        "upcase-initials-region" | "upcase-region" => arity_cons(2, Some(3)),
        "widget-get" => arity_cons(2, Some(2)),
        "widget-put" => arity_cons(3, Some(3)),
        "widget-apply" => arity_cons(2, None),
        "where-is-internal" => arity_cons(1, Some(5)),
        "text-char-description" | "threadp" | "y-or-n-p" | "yes-or-no-p" | "zerop" | "logcount" => {
            arity_cons(1, Some(1))
        }
        "syntax-ppss" => arity_cons(0, Some(1)),
        "syntax-table"
        | "standard-case-table"
        | "standard-category-table"
        | "standard-syntax-table" => arity_cons(0, Some(0)),
        "libxml-available-p" | "zlib-available-p" => arity_cons(0, Some(0)),
        "libxml-parse-html-region" | "libxml-parse-xml-region" => arity_cons(0, Some(4)),
        "zlib-decompress-region" => arity_cons(2, Some(3)),
        "search-forward"
        | "search-backward"
        | "search-forward-regexp"
        | "search-backward-regexp"
        | "re-search-forward"
        | "re-search-backward"
        | "posix-search-forward"
        | "posix-search-backward"
        | "word-search-forward"
        | "word-search-backward" => arity_cons(1, Some(4)),
        "add-timeout" => arity_cons(3, Some(4)),
        "add-variable-watcher" => arity_cons(2, Some(2)),
        "remove-hook" => arity_cons(2, Some(3)),
        "remove" | "remove-variable-watcher" | "narrow-to-region" => arity_cons(2, Some(2)),
        "remove-images" => arity_cons(2, Some(3)),
        "replace-regexp" | "replace-string" => arity_cons(2, Some(7)),
        "save-buffer"
        | "scroll-down"
        | "scroll-down-command"
        | "scroll-up"
        | "scroll-up-command" => arity_cons(0, Some(1)),
        "load-average" => arity_cons(0, Some(1)),
        "select-window" | "minor-mode-key-binding" => arity_cons(1, Some(2)),
        "select-frame" => arity_cons(1, Some(2)),
        "select-frame-set-input-focus" => arity_cons(1, Some(2)),
        "selected-frame" => arity_cons(0, Some(0)),
        "set-charset-priority" => arity_cons(1, None),
        "write-char" => arity_cons(1, Some(2)),
        "write-region" => arity_cons(3, Some(7)),
        "isearch-backward" | "isearch-forward" => arity_cons(0, Some(2)),
        "advice-add" => arity_cons(3, Some(4)),
        "advice-remove" | "advice-member-p" => arity_cons(2, Some(2)),
        "autoload" => arity_cons(2, Some(5)),
        "autoload-do-load" => arity_cons(1, Some(3)),
        "Snarf-documentation" => arity_cons(1, Some(1)),
        "substitute-command-keys" => arity_cons(1, Some(3)),
        "documentation" => arity_cons(1, Some(2)),
        "documentation-stringp" => arity_cons(1, Some(1)),
        "documentation-property" => arity_cons(2, Some(3)),
        "help-function-arglist" => arity_cons(1, Some(2)),
        "decode-coding-string" | "encode-coding-string" => arity_cons(2, Some(4)),
        "decode-time" => arity_cons(0, Some(3)),
        "detect-coding-region" => arity_cons(2, Some(3)),
        "detect-coding-string" => arity_cons(1, Some(2)),
        "encode-char" => arity_cons(2, Some(2)),
        "encode-time" => arity_cons(1, None),
        "format-time-string" => arity_cons(1, Some(3)),
        "format-mode-line" => arity_cons(1, Some(4)),
        "indent-according-to-mode" | "indent-for-tab-command" => arity_cons(0, Some(1)),
        "indent-line-to" => arity_cons(1, Some(1)),
        "indent-region" => arity_cons(2, Some(3)),
        "indent-rigidly" => arity_cons(3, Some(4)),
        "indent-to" | "move-to-column" => arity_cons(1, Some(2)),
        "tab-to-tab-stop" => arity_cons(0, Some(0)),
        "backtrace--frames-from-thread" => arity_cons(1, Some(1)),
        "backtrace--locals" => arity_cons(1, Some(2)),
        "backtrace-debug" | "backtrace-eval" => arity_cons(2, Some(3)),
        "backtrace-frame--internal" => arity_cons(3, Some(3)),
        "backtrace-frame" => arity_cons(1, Some(2)),
        "run-hook-with-args"
        | "run-hook-with-args-until-failure"
        | "run-hook-with-args-until-success" => arity_cons(1, None),
        "run-hook-query-error-with-timeout" => arity_cons(1, Some(1)),
        "run-hook-wrapped" => arity_cons(2, None),
        "run-mode-hooks" => arity_cons(0, None),
        "run-window-configuration-change-hook" | "run-window-scroll-functions" => {
            arity_cons(0, Some(1))
        }
        "base64-decode-region" => arity_cons(2, Some(4)),
        "base64-encode-region" | "base64url-encode-region" => arity_cons(2, Some(3)),
        "base64-decode-string" => arity_cons(1, Some(3)),
        "base64-encode-string" | "base64url-encode-string" => arity_cons(1, Some(2)),
        "md5" => arity_cons(1, Some(5)),
        "bool-vector" => arity_cons(0, None),
        "bool-vector-p" | "bool-vector-count-population" => arity_cons(1, Some(1)),
        "bool-vector-count-consecutive" => arity_cons(3, Some(3)),
        "bool-vector-not" => arity_cons(1, Some(2)),
        "bool-vector-subsetp" => arity_cons(2, Some(2)),
        "make-bool-vector" => arity_cons(2, Some(2)),
        "bitmap-spec-p" => arity_cons(1, Some(1)),
        "bool-vector-exclusive-or"
        | "bool-vector-intersection"
        | "bool-vector-set-difference"
        | "bool-vector-union" => arity_cons(2, Some(3)),
        "arrayp" | "atom" | "booleanp" | "bufferp" | "char-to-string" | "char-uppercase-p"
        | "consp" | "downcase" | "float" | "floatp" | "integer-or-null-p" | "integerp"
        | "keywordp" | "listp" | "list-of-strings-p" | "nlistp" | "null"
        | "number-to-string" | "numberp" | "sequencep" | "string-or-null-p"
        | "string-to-char" | "stringp" | "symbolp" | "type-of" | "cl-type-of" | "upcase"
        | "vectorp" => {
            arity_cons(1, Some(1))
        }
        "ceiling" | "characterp" | "floor" | "round" | "string-to-number" | "truncate" => {
            arity_cons(1, Some(2))
        }
        "byte-to-position" | "byte-to-string" | "position-bytes" => arity_cons(1, Some(1)),
        "substring" | "substring-no-properties" => arity_cons(1, Some(3)),
        "aref" | "char-equal" | "eq" | "eql" | "equal" | "function-equal" | "make-vector"
        | "string-equal" | "string-lessp" | "string-greaterp" | "string>" | "throw" => {
            arity_cons(2, Some(2))
        }
        "aset" => arity_cons(3, Some(3)),
        "activate-mark" | "auto-composition-mode" | "deactivate-mark" => arity_cons(0, Some(1)),
        "clear-composition-cache" => arity_cons(0, Some(0)),
        "composition-sort-rules" => arity_cons(1, Some(1)),
        "compute-motion" => arity_cons(7, Some(7)),
        "compose-region-internal" => arity_cons(2, Some(4)),
        "compose-string-internal" => arity_cons(3, Some(5)),
        "composition-get-gstring" => arity_cons(4, Some(4)),
        "find-composition-internal" => arity_cons(4, Some(4)),
        "call-interactively" => arity_cons(1, Some(3)),
        "command-execute" => arity_cons(1, Some(4)),
        "command-error-default-function" | "ngettext" => arity_cons(3, Some(3)),
        "compare-strings" => arity_cons(6, Some(7)),
        "compare-buffer-substrings" => arity_cons(6, Some(6)),
        "comp--init-ctxt"
        | "comp--release-ctxt"
        | "comp-libgccjit-version"
        | "comp-native-compiler-options-effective-p"
        | "comp-native-driver-options-effective-p" => arity_cons(0, Some(0)),
        "comp--compile-ctxt-to-file0"
        | "comp--subr-signature"
        | "comp-el-to-eln-rel-filename"
        | "dbus-get-unique-name" => arity_cons(1, Some(1)),
        "comp-el-to-eln-filename" | "dbus--init-bus" => arity_cons(1, Some(2)),
        "comp--install-trampoline" => arity_cons(2, Some(2)),
        "comp--late-register-subr" | "comp--register-lambda" | "comp--register-subr" => {
            arity_cons(7, Some(7))
        }
        "dbus-message-internal" => arity_cons(4, None),
        "define-fringe-bitmap" => arity_cons(2, Some(5)),
        "destroy-fringe-bitmap" | "external-debugging-output" => arity_cons(1, Some(1)),
        "display--line-is-continued-p" => arity_cons(0, Some(0)),
        "display--update-for-mouse-movement" | "fillarray" => arity_cons(2, Some(2)),
        "do-auto-save" | "delete-terminal" => arity_cons(0, Some(2)),
        "describe-buffer-bindings" => arity_cons(1, Some(3)),
        "describe-vector" => arity_cons(1, Some(2)),
        "face-attributes-as-vector" => arity_cons(1, Some(1)),
        "font-at" => arity_cons(1, Some(3)),
        "font-face-attributes" | "font-info" | "fontset-info" => arity_cons(1, Some(2)),
        "font-get-glyphs" => arity_cons(3, Some(4)),
        "font-get-system-font" | "font-get-system-normal-font" | "fontset-list" => {
            arity_cons(0, Some(0))
        }
        "font-has-char-p" | "fontset-font" => arity_cons(2, Some(3)),
        "font-match-p" | "font-shape-gstring" | "font-variation-glyphs" => arity_cons(2, Some(2)),
        "frame--set-was-invisible" | "frame-after-make-frame" | "frame-ancestor-p" => {
            arity_cons(2, Some(2))
        }
        "frame--face-hash-table"
        | "frame-bottom-divider-width"
        | "frame-child-frame-border-width"
        | "frame-focus"
        | "frame-font-cache"
        | "frame-fringe-width"
        | "frame-internal-border-width"
        | "frame-old-selected-window"
        | "frame-or-buffer-changed-p"
        | "frame-parent"
        | "frame-pointer-visible-p"
        | "frame-scale-factor"
        | "frame-scroll-bar-height"
        | "frame-scroll-bar-width"
        | "frame-window-state-change"
        | "frame-right-divider-width" => arity_cons(0, Some(1)),
        "fringe-bitmaps-at-pos" => arity_cons(0, Some(2)),
        "gap-position" | "gap-size" => arity_cons(0, Some(0)),
        "gnutls-available-p" | "gnutls-ciphers" | "gnutls-digests" | "gnutls-macs"
        | "gpm-mouse-start" | "gpm-mouse-stop" | "sqlite-available-p" | "sqlite-version" => {
            arity_cons(0, Some(0))
        }
        "gnutls-asynchronous-parameters" | "gnutls-bye" | "gnutls-hash-digest" => {
            arity_cons(2, Some(2))
        }
        "gnutls-boot" | "gnutls-hash-mac" | "inotify-add-watch" => arity_cons(3, Some(3)),
        "garbage-collect-maybe" | "get-variable-watchers" => arity_cons(1, Some(1)),
        "gnutls-deinit" | "gnutls-format-certificate" | "gnutls-get-initstage" => {
            arity_cons(1, Some(1))
        }
        "gnutls-error-fatalp"
        | "gnutls-error-string"
        | "gnutls-errorp"
        | "gnutls-peer-status"
        | "gnutls-peer-status-warning-describe"
        | "inotify-rm-watch"
        | "inotify-valid-p" => arity_cons(1, Some(1)),
        "gnutls-symmetric-decrypt" | "gnutls-symmetric-encrypt" => arity_cons(4, Some(5)),
        "handle-save-session"
        | "handle-switch-frame"
        | "init-image-library"
        | "interactive-form"
        | "lock-file"
        | "sqlite-close"
        | "sqlite-columns"
        | "sqlite-commit"
        | "sqlite-finalize"
        | "sqlite-more-p"
        | "sqlite-next"
        | "sqlite-rollback"
        | "sqlite-transaction"
        | "sqlitep"
        | "unlock-file" => arity_cons(1, Some(1)),
        "help--describe-vector" => arity_cons(7, Some(7)),
        "innermost-minibuffer-p" | "lock-buffer" | "lossage-size" | "sqlite-open" => {
            arity_cons(0, Some(1))
        }
        "sqlite-execute" => arity_cons(2, Some(3)),
        "sqlite-execute-batch" | "sqlite-load-extension" | "sqlite-pragma" => {
            arity_cons(2, Some(2))
        }
        "sqlite-select" => arity_cons(2, Some(4)),
        "window-at" => arity_cons(2, Some(3)),
        "window-combination-limit" => arity_cons(1, Some(1)),
        "window-line-height"
        | "window-normal-size"
        | "window-resize-apply"
        | "window-resize-apply-total" => arity_cons(0, Some(2)),
        "window-lines-pixel-dimensions" => arity_cons(0, Some(6)),
        "window-list-1" => arity_cons(0, Some(3)),
        "window-bottom-divider-width"
        | "window-bump-use-time"
        | "window-left-child"
        | "window-new-normal"
        | "window-new-pixel"
        | "window-new-total"
        | "window-next-sibling"
        | "window-old-body-pixel-height"
        | "window-old-body-pixel-width"
        | "window-old-pixel-height"
        | "window-old-pixel-width"
        | "window-parent"
        | "window-pixel-left"
        | "window-pixel-top"
        | "window-prev-sibling"
        | "window-right-divider-width"
        | "window-scroll-bar-height"
        | "window-scroll-bar-width"
        | "window-tab-line-height"
        | "window-top-child" => arity_cons(0, Some(1)),
        "local-variable-if-set-p" => arity_cons(1, Some(2)),
        "unlock-buffer" => arity_cons(0, Some(0)),
        "get-unicode-property-internal" => arity_cons(2, Some(2)),
        "define-hash-table-test" => arity_cons(3, Some(3)),
        "find-coding-systems-region-internal" => arity_cons(2, Some(3)),
        "completing-read" => arity_cons(2, Some(8)),
        "try-completion" | "test-completion" => arity_cons(2, Some(3)),
        "all-completions" => arity_cons(2, Some(4)),
        "read" => arity_cons(0, Some(1)),
        "read-char" | "read-char-exclusive" | "read-event" => arity_cons(0, Some(3)),
        "read-key" => arity_cons(0, Some(2)),
        "read-number" => arity_cons(1, Some(3)),
        "read-string" => arity_cons(1, Some(5)),
        "read-variable" | "read-command" => arity_cons(1, Some(2)),
        "read-from-string" => arity_cons(1, Some(3)),
        "read-buffer" => arity_cons(1, Some(4)),
        "read-coding-system" => arity_cons(1, Some(2)),
        "read-directory-name" => arity_cons(1, Some(5)),
        "read-file-name" => arity_cons(1, Some(6)),
        "read-from-minibuffer" => arity_cons(1, Some(7)),
        "read-key-sequence" | "read-key-sequence-vector" => arity_cons(1, Some(6)),
        "read-non-nil-coding-system" => arity_cons(1, Some(1)),
        "json-insert" | "json-parse-string" | "json-serialize" => arity_cons(1, None),
        "json-available-p" => arity_cons(0, Some(0)),
        "kbd" | "key-valid-p" | "keymap-parent" | "keymapp" | "listify-key-sequence" => {
            arity_cons(1, Some(1))
        }
        "accessible-keymaps" => arity_cons(1, Some(2)),
        "global-key-binding" | "local-key-binding" | "key-description" => arity_cons(1, Some(2)),
        "lookup-key" => arity_cons(2, Some(3)),
        "global-set-key" | "local-set-key" => arity_cons(2, Some(2)),
        "set-keymap-parent" => arity_cons(2, Some(2)),
        "key-binding" => arity_cons(1, Some(4)),
        "keyboard-coding-system" => arity_cons(0, Some(1)),
        "keyboard-quit" => arity_cons(0, Some(0)),
        "make-keymap" | "make-sparse-keymap" => arity_cons(0, Some(1)),
        "current-active-maps" => arity_cons(0, Some(2)),
        "current-bidi-paragraph-direction" | "bidi-resolved-levels" => arity_cons(0, Some(1)),
        "bidi-find-overridden-directionality" => arity_cons(3, Some(4)),
        "current-case-table"
        | "current-column"
        | "current-global-map"
        | "current-indentation"
        | "current-local-map"
        | "current-message"
        | "current-minor-mode-maps" => arity_cons(0, Some(0)),
        "set-buffer"
        | "set-buffer-modified-p"
        | "set-case-table"
        | "set-category-table"
        | "set-default-file-modes"
        | "set-standard-case-table"
        | "set-syntax-table"
        | "set-time-zone-rule" => arity_cons(1, Some(1)),
        "set-default"
        | "set-default-toplevel-value"
        | "set-window-dedicated-p"
        | "setcar"
        | "setcdr" => arity_cons(2, Some(2)),
        "set-char-table-parent" => arity_cons(2, Some(2)),
        "set-char-table-extra-slot" | "set-char-table-range" => arity_cons(3, Some(3)),
        "set-file-modes" => arity_cons(2, Some(3)),
        "set-file-acl" | "set-file-selinux-context" => arity_cons(2, Some(2)),
        "set-file-times" => arity_cons(1, Some(3)),
        "set-keyboard-coding-system"
        | "set-keyboard-coding-system-internal"
        | "set-match-data"
        | "set-text-conversion-style" => arity_cons(1, Some(2)),
        "set-terminal-coding-system" => arity_cons(1, Some(3)),
        "set-terminal-coding-system-internal" => arity_cons(1, Some(2)),
        "set-safe-terminal-coding-system-internal" => arity_cons(1, Some(1)),
        "set-visited-file-modtime" | "verify-visited-file-modtime" => arity_cons(0, Some(1)),
        "visited-file-modtime" => arity_cons(0, Some(0)),
        "scan-lists" => arity_cons(3, Some(3)),
        "scan-sexps" => arity_cons(2, Some(2)),
        "current-kill" => arity_cons(1, Some(2)),
        "current-time-string" | "current-time-zone" => arity_cons(0, Some(2)),
        "time-add" | "time-equal-p" | "time-less-p" | "time-subtract" => arity_cons(2, Some(2)),
        "time-convert" => arity_cons(1, Some(2)),
        "system-name" => arity_cons(0, Some(0)),
        "system-groups" | "system-users" => arity_cons(0, Some(0)),
        "tab-bar-height" | "tool-bar-height" => arity_cons(0, Some(2)),
        "file-user-uid" | "user-real-login-name" | "user-real-uid" | "user-uid" => {
            arity_cons(0, Some(0))
        }
        "user-full-name" | "user-login-name" => arity_cons(0, Some(1)),
        "emacs-version" => arity_cons(0, Some(1)),
        "line-beginning-position"
        | "line-end-position"
        | "line-number-display-width"
        | "pos-bol"
        | "pos-eol" => arity_cons(0, Some(1)),
        "line-number-at-pos" => arity_cons(0, Some(2)),
        "line-pixel-height" | "long-line-optimizations-p" => arity_cons(0, Some(0)),
        "scroll-left" | "scroll-right" => arity_cons(0, Some(2)),
        "recenter-top-bottom" => arity_cons(0, Some(1)),
        "recenter" => arity_cons(0, Some(2)),
        "recursion-depth" | "region-beginning" | "region-end" => arity_cons(0, Some(0)),
        "delete-frame" | "delete-other-windows" | "delete-other-windows-internal" => {
            arity_cons(0, Some(2))
        }
        "one-window-p" => arity_cons(0, Some(2)),
        "next-window" | "previous-window" | "pos-visible-in-window-p" => arity_cons(0, Some(3)),
        "other-window-for-scrolling" => arity_cons(0, Some(0)),
        "coordinates-in-window-p" => arity_cons(2, Some(2)),
        "pop-to-buffer" => arity_cons(1, Some(3)),
        "move-to-window-line" | "move-point-visually" => arity_cons(1, Some(1)),
        "modify-frame-parameters" => arity_cons(2, Some(2)),
        "make-frame" | "make-frame-visible" | "iconify-frame" => arity_cons(0, Some(1)),
        "get-unused-category" => arity_cons(0, Some(1)),
        "make-category-set"
        | "category-set-mnemonics"
        | "flatten-tree"
        | "forward-comment"
        | "natnump" => arity_cons(1, Some(1)),
        "make-category-table" | "preceding-char" => arity_cons(0, Some(0)),
        "make-char-table" => arity_cons(1, Some(2)),
        "modify-category-entry" => arity_cons(2, Some(4)),
        "modify-syntax-entry" | "plist-get" | "plist-member" => arity_cons(2, Some(3)),
        "plist-put" => arity_cons(3, Some(4)),
        "constrain-to-field" => arity_cons(2, Some(5)),
        "field-beginning" | "field-end" => arity_cons(0, Some(3)),
        "field-string" | "field-string-no-properties" => arity_cons(0, Some(1)),
        "parse-partial-sexp" => arity_cons(2, Some(6)),
        "matching-paren" => arity_cons(1, Some(1)),
        "forward-sexp" => arity_cons(0, Some(2)),
        "file-attributes-lessp" => arity_cons(2, Some(2)),
        "called-interactively-p" | "float-time" => arity_cons(0, Some(1)),
        "force-mode-line-update" | "force-window-update" => arity_cons(0, Some(1)),
        "featurep" => arity_cons(1, Some(2)),
        "commandp" => arity_cons(1, Some(2)),
        "command-modes" => arity_cons(1, Some(1)),
        "command-remapping" => arity_cons(1, Some(3)),
        "cancel-timer" | "timerp" | "timeout-event-p" => arity_cons(1, Some(1)),
        "run-at-time" | "run-with-timer" | "run-with-idle-timer" => arity_cons(3, None),
        "timer-activate" => arity_cons(1, Some(3)),
        "sleep-for" | "sit-for" => arity_cons(1, Some(2)),
        "current-cpu-time"
        | "current-idle-time"
        | "current-time"
        | "flush-standard-output"
        | "get-internal-run-time"
        | "invocation-directory"
        | "invocation-name" => arity_cons(0, Some(0)),
        "text-quoting-style" => arity_cons(0, Some(0)),
        "daemon-initialized" | "daemonp" => arity_cons(0, Some(0)),
        "category-table" | "clear-buffer-auto-save-failure" | "clear-charset-maps" => {
            arity_cons(0, Some(0))
        }
        "find-charset-region" => arity_cons(2, Some(3)),
        "find-charset-string" => arity_cons(1, Some(2)),
        "case-table-p"
        | "category-table-p"
        | "ccl-program-p"
        | "check-coding-system"
        | "clear-abbrev-table"
        | "clear-string"
        | "module-function-p"
        | "number-or-marker-p"
        | "obarrayp"
        | "special-variable-p"
        | "symbol-with-pos-p"
        | "symbol-with-pos-pos"
        | "user-ptrp"
        | "vector-or-char-table-p" => arity_cons(1, Some(1)),
        "coding-system-aliases"
        | "coding-system-base"
        | "coding-system-eol-type"
        | "coding-system-p"
        | "coding-system-type" => arity_cons(1, Some(1)),
        "coding-system-change-eol-conversion"
        | "coding-system-change-text-conversion"
        | "coding-system-get" => arity_cons(2, Some(2)),
        "check-coding-systems-region" => arity_cons(3, Some(3)),
        "coding-system-plist" => arity_cons(1, Some(1)),
        "coding-system-put" => arity_cons(3, Some(3)),
        "coding-system-list" | "coding-system-priority-list" => arity_cons(0, Some(1)),
        "defined-colors" => arity_cons(0, Some(1)),
        "color-distance" => arity_cons(2, Some(4)),
        "color-defined-p" | "color-values" => arity_cons(1, Some(2)),
        "color-gray-p" => arity_cons(1, Some(2)),
        "color-supported-p" => arity_cons(1, Some(3)),
        "color-values-from-color-spec" => arity_cons(1, Some(1)),
        "category-docstring" => arity_cons(1, Some(2)),
        "ccl-execute" => arity_cons(2, Some(2)),
        "ccl-execute-on-string" => arity_cons(3, Some(5)),
        "abbrev-mode" => arity_cons(0, Some(1)),
        "abbrev-expansion" => arity_cons(1, Some(2)),
        "abbrev-table-p" => arity_cons(1, Some(1)),
        "define-abbrev" => arity_cons(3, None),
        "define-abbrev-table" => arity_cons(2, None),
        "expand-abbrev" => arity_cons(0, Some(0)),
        "insert-abbrev-table-description" => arity_cons(1, Some(2)),
        "and"
        | "cond"
        | "inline"
        | "interactive"
        | "or"
        | "progn"
        | "save-current-buffer"
        | "save-excursion"
        | "save-restriction"
        | "setq" => arity_unevalled(0),
        "catch" | "defvar" | "function" | "let" | "let*" | "prog1" | "quote" | "unwind-protect"
        | "while" => arity_unevalled(1),
        "condition-case" | "defconst" | "if" => arity_unevalled(2),
        "defining-kbd-macro" => arity_cons(1, Some(2)),
        "start-kbd-macro" => arity_cons(1, Some(2)),
        "cancel-kbd-macro-events" => arity_cons(0, Some(0)),
        "name-last-kbd-macro" | "kmacro-name-last-macro" => arity_cons(1, Some(1)),
        "end-kbd-macro" | "call-last-kbd-macro" => arity_cons(0, Some(2)),
        "execute-kbd-macro" | "execute-extended-command" => arity_cons(1, Some(3)),
        "describe-key-briefly" => arity_cons(0, Some(3)),
        "delete-char" => arity_cons(1, Some(2)),
        "delete-all-overlays" => arity_cons(0, Some(1)),
        "delete-and-extract-region" => arity_cons(2, Some(2)),
        "delete-field" => arity_cons(0, Some(1)),
        "delete-region" => arity_cons(2, Some(2)),
        "delete-horizontal-space" => arity_cons(0, Some(1)),
        "delete-indentation" => arity_cons(0, Some(3)),
        "delete-overlay" => arity_cons(1, Some(1)),
        "delete-window" => arity_cons(0, Some(1)),
        "delete-window-internal" => arity_cons(1, Some(1)),
        "delete-directory" => arity_cons(1, Some(3)),
        "delete-directory-internal" => arity_cons(1, Some(1)),
        "delete-file" => arity_cons(1, Some(2)),
        "delete-file-internal" => arity_cons(1, Some(1)),
        "access-file" => arity_cons(2, Some(2)),
        "default-file-modes" => arity_cons(0, Some(0)),
        "directory-file-name" | "directory-empty-p" | "directory-name-p" => {
            arity_cons(1, Some(1))
        }
        "directory-files" => arity_cons(1, Some(5)),
        "directory-files-and-attributes" => arity_cons(1, Some(6)),
        "define-category" => arity_cons(2, Some(3)),
        "define-charset-internal" => arity_cons(17, None),
        "define-coding-system-alias" => arity_cons(2, Some(2)),
        "define-key" => arity_cons(3, Some(4)),
        "expand-file-name" => arity_cons(1, Some(2)),
        "event-basic-type"
        | "event-convert-list"
        | "event-modifiers"
        | "eventp"
        | "error-message-string" => arity_cons(1, Some(1)),
        "copysign" | "equal-including-properties" => arity_cons(2, Some(2)),
        "count-lines" => arity_cons(2, Some(3)),
        "emacs-pid" => arity_cons(0, Some(0)),
        "eval" => arity_cons(1, Some(2)),
        "eval-buffer" => arity_cons(0, Some(5)),
        "eval-expression" => arity_cons(1, Some(4)),
        "eval-region" => arity_cons(2, Some(4)),
        "help-key-description" => arity_cons(2, Some(2)),
        "recent-keys" => arity_cons(0, Some(1)),
        "input-pending-p" => arity_cons(0, Some(1)),
        "discard-input" => arity_cons(0, Some(0)),
        "current-input-mode" => arity_cons(0, Some(0)),
        "set-input-mode" => arity_cons(3, Some(4)),
        "set-input-meta-mode" => arity_cons(1, Some(2)),
        "set-input-interrupt-mode" | "set-quit-char" => arity_cons(1, Some(1)),
        "set-output-flow-control" => arity_cons(1, Some(2)),
        "waiting-for-user-input-p" => arity_cons(0, Some(0)),
        "minibufferp" => arity_cons(0, Some(2)),
        "next-read-file-uses-dialog-p" => arity_cons(0, Some(0)),
        "recursive-edit"
        | "top-level"
        | "exit-recursive-edit"
        | "abort-minibuffers"
        | "abort-recursive-edit"
        | "exit-minibuffer"
        | "minibuffer-depth"
        | "minibuffer-prompt"
        | "minibuffer-contents"
        | "minibuffer-contents-no-properties" => arity_cons(0, Some(0)),
        "read-passwd" => arity_cons(1, Some(3)),
        "event-apply-modifier" => arity_cons(4, Some(4)),
        "regexp-quote" => arity_cons(1, Some(1)),
        "gui-get-primary-selection" | "gui-selection-value" => arity_cons(0, Some(0)),
        "gui-get-selection" => arity_cons(0, Some(2)),
        "gui-select-text" => arity_cons(1, Some(1)),
        "gui-set-selection" => arity_cons(2, Some(2)),
        "x-apply-session-resources" | "x-clipboard-yank" => arity_cons(0, Some(0)),
        "open-termscript"
        | "x-close-connection"
        | "x-device-class"
        | "x-get-input-coding-system"
        | "x-internal-focus-input-context"
        | "x-parse-geometry"
        | "x-preedit-text"
        | "x-setup-function-keys" => arity_cons(1, Some(1)),
        "x-get-local-selection" => arity_cons(0, Some(2)),
        "send-string-to-terminal"
        | "display-supports-face-attributes-p"
        | "x-focus-frame"
        | "x-get-atom-name"
        | "x-register-dnd-atom"
        | "x-synchronize"
        | "x-display-set-last-user-time" => arity_cons(1, Some(2)),
        "x-own-selection-internal" => arity_cons(2, Some(3)),
        "x-get-selection-internal" => arity_cons(2, Some(4)),
        "x-change-window-property" => arity_cons(2, Some(7)),
        "x-send-client-message" => arity_cons(6, Some(6)),
        "x-set-mouse-absolute-pixel-position" => arity_cons(2, Some(2)),
        "x-popup-menu" => arity_cons(2, Some(2)),
        "x-delete-window-property"
        | "x-disown-selection-internal"
        | "x-window-property-attributes" => arity_cons(1, Some(3)),
        "x-open-connection" => arity_cons(1, Some(3)),
        "x-popup-dialog" | "x-frame-restack" => arity_cons(2, Some(3)),
        "x-get-resource" => arity_cons(2, Some(4)),
        "x-list-fonts" => arity_cons(1, Some(5)),
        "x-show-tip" | "x-window-property" | "x-translate-coordinates" => arity_cons(1, Some(6)),
        "internal-show-cursor" => arity_cons(2, Some(2)),
        "display-buffer" => arity_cons(1, Some(3)),
        "clear-face-cache" => arity_cons(0, Some(1)),
        "clear-font-cache" => arity_cons(0, Some(0)),
        "clear-image-cache" => arity_cons(0, Some(2)),
        "image-cache-size" => arity_cons(0, Some(0)),
        "create-image" => arity_cons(1, None),
        "find-font" => arity_cons(1, Some(2)),
        "font-family-list" => arity_cons(0, Some(1)),
        "image-flush" | "image-mask-p" | "image-metadata" => arity_cons(1, Some(2)),
        "imagep" => arity_cons(1, Some(1)),
        "image-size" | "image-type" => arity_cons(1, Some(3)),
        "image-transforms-p" => arity_cons(0, Some(1)),
        "image-type-available-p" => arity_cons(1, Some(1)),
        "list-fonts" => arity_cons(1, Some(4)),
        "frame-parameter"
        | "set-window-point"
        | "set-window-hscroll"
        | "set-window-display-table"
        | "set-window-cursor-type"
        | "set-window-prev-buffers"
        | "set-window-next-buffers" => arity_cons(2, Some(2)),
        "set-window-buffer"
        | "set-window-start"
        | "set-window-group-start"
        | "set-window-margins" => arity_cons(2, Some(3)),
        "set-window-configuration" => arity_cons(1, Some(3)),
        "set-window-vscroll" => arity_cons(2, Some(4)),
        "set-window-fringes" => arity_cons(2, Some(5)),
        "set-window-scroll-bars" => arity_cons(1, Some(6)),
        "fit-window-to-buffer" => arity_cons(0, Some(6)),
        "buffer-text-pixel-size" => arity_cons(0, Some(4)),
        "window-text-pixel-size" => arity_cons(0, Some(7)),
        // Process primitives
        "accept-process-output" => arity_cons(0, Some(4)),
        "backquote-delay-process" => arity_cons(2, Some(2)),
        "backquote-process" => arity_cons(1, Some(2)),
        "call-process" => arity_cons(1, None),
        "call-process-shell-command" => arity_cons(1, None),
        "call-process-region" => arity_cons(3, None),
        "clone-process" => arity_cons(1, Some(2)),
        "continue-process" | "interrupt-process" | "kill-process" | "quit-process"
        | "stop-process" => arity_cons(0, Some(2)),
        "delete-process" => arity_cons(0, Some(1)),
        "format-network-address" => arity_cons(1, Some(2)),
        "get-buffer-process" => arity_cons(1, Some(1)),
        "get-process" => arity_cons(1, Some(1)),
        "internal-default-interrupt-process" => arity_cons(0, Some(2)),
        "internal-default-process-filter" | "internal-default-process-sentinel" => {
            arity_cons(2, Some(2))
        }
        "internal-default-signal-process" => arity_cons(2, Some(3)),
        "isearch-process-search-char" => arity_cons(1, Some(2)),
        "isearch-process-search-string" => arity_cons(2, Some(2)),
        "list-processes" => arity_cons(0, Some(2)),
        "list-processes--refresh" | "list-system-processes" => arity_cons(0, Some(0)),
        "make-process" => arity_cons(0, None),
        "make-network-process" | "make-pipe-process" | "make-serial-process" => arity_cons(0, None),
        "minibuffer--sort-preprocess-history" => arity_cons(1, Some(1)),
        "network-interface-info" => arity_cons(1, Some(1)),
        "network-interface-list" => arity_cons(0, Some(2)),
        "network-lookup-address-info" => arity_cons(1, Some(3)),
        "num-processors" => arity_cons(0, Some(1)),
        "print--preprocess" => arity_cons(1, Some(1)),
        "process-buffer"
        | "process-attributes"
        | "process-coding-system"
        | "process-command"
        | "process-datagram-address"
        | "process-exit-status"
        | "process-filter"
        | "process-menu-visit-buffer"
        | "process-id"
        | "process-inherit-coding-system-flag"
        | "process-live-p"
        | "process-mark"
        | "process-name"
        | "process-plist"
        | "process-query-on-exit-flag"
        | "process-sentinel"
        | "process-status"
        | "process-thread"
        | "process-type"
        | "processp" => arity_cons(1, Some(1)),
        "process-kill-buffer-query-function"
        | "process-menu-delete-process"
        | "process-menu-mode" => arity_cons(0, Some(0)),
        "process-file"
        | "process-file-shell-command"
        | "process-lines"
        | "process-lines-ignore-status" => arity_cons(1, None),
        "process-lines-handling-status" => arity_cons(2, None),
        "process-running-child-p" | "process-send-eof" => arity_cons(0, Some(1)),
        "signal-process" => arity_cons(2, Some(3)),
        "signal-names" => arity_cons(0, Some(0)),
        "process-contact" => arity_cons(1, Some(3)),
        "process-list" => arity_cons(0, Some(0)),
        "process-get" => arity_cons(2, Some(2)),
        "process-put" => arity_cons(3, Some(3)),
        "process-send-region" => arity_cons(3, Some(3)),
        "process-send-string" => arity_cons(2, Some(2)),
        "process-tty-name" => arity_cons(1, Some(2)),
        "set-process-coding-system" => arity_cons(1, Some(3)),
        "set-binary-mode" => arity_cons(2, Some(2)),
        "set-process-filter"
        | "set-process-buffer"
        | "set-process-datagram-address"
        | "set-process-inherit-coding-system-flag"
        | "set-process-plist"
        | "set-process-query-on-exit-flag"
        | "set-process-thread"
        | "set-process-sentinel" => arity_cons(2, Some(2)),
        "set-process-window-size" => arity_cons(3, Some(3)),
        "set-buffer-process-coding-system" => arity_cons(2, Some(2)),
        "set-network-process-option" => arity_cons(3, Some(4)),
        "serial-process-configure" => arity_cons(0, None),
        "setenv" => arity_cons(1, Some(3)),
        "start-process" | "start-file-process" => arity_cons(3, None),
        "start-process-shell-command" | "start-file-process-shell-command" => {
            arity_cons(3, Some(3))
        }
        "syntax-propertize--in-process-p" => arity_cons(0, Some(0)),
        "tooltip-process-prompt-regexp" => arity_cons(1, Some(1)),
        "window--adjust-process-windows" | "window--process-window-list" => arity_cons(0, Some(0)),
        "window-adjust-process-window-size"
        | "window-adjust-process-window-size-largest"
        | "window-adjust-process-window-size-smallest" => arity_cons(2, Some(2)),
        "getenv" | "getenv-internal" => arity_cons(1, Some(2)),
        // Display/terminal query primitives
        "display-images-p"
        | "display-graphic-p"
        | "display-color-p"
        | "display-grayscale-p"
        | "display-mouse-p"
        | "display-popup-menus-p"
        | "display-symbol-keys-p"
        | "display-pixel-width"
        | "display-pixel-height"
        | "display-mm-width"
        | "display-mm-height"
        | "display-screens"
        | "display-color-cells"
        | "display-planes"
        | "display-visual-class"
        | "display-backing-store"
        | "display-save-under"
        | "display-selections-p"
        | "display-monitor-attributes-list"
        | "frame-monitor-attributes"
        | "terminal-name"
        | "terminal-parameters"
        | "frame-terminal"
        | "tty-type"
        | "tty-top-frame"
        | "tty-display-color-p"
        | "tty-display-color-cells"
        | "tty-no-underline"
        | "window-system"
        | "x-display-pixel-width"
        | "x-display-pixel-height"
        | "x-display-backing-store"
        | "x-display-color-cells"
        | "x-display-mm-height"
        | "x-display-mm-width"
        | "x-display-monitor-attributes-list"
        | "x-display-planes"
        | "x-display-save-under"
        | "x-display-screens"
        | "x-display-visual-class"
        | "x-get-modifier-masks"
        | "x-wm-set-size-hint"
        | "x-server-version"
        | "x-server-input-extension-version"
        | "x-server-max-request-size"
        | "x-server-vendor"
        | "x-display-grayscale-p"
        | "x-backspace-delete-keys-p"
        | "x-frame-geometry"
        | "x-frame-list-z-order"
        | "redraw-frame"
        | "ding"
        | "internal-show-cursor-p"
        | "controlling-tty-p"
        | "suspend-tty"
        | "resume-tty"
        | "terminal-coding-system"
        | "frame-char-height"
        | "frame-char-width"
        | "frame-first-window"
        | "frame-native-height"
        | "frame-native-width"
        | "frame-position"
        | "frame-root-window"
        | "frame-parameters"
        | "frame-selected-window"
        | "frame-text-cols"
        | "frame-text-height"
        | "frame-text-lines"
        | "frame-text-width"
        | "frame-total-cols"
        | "frame-total-lines"
        | "current-window-configuration"
        | "minibuffer-window"
        | "window-buffer"
        | "window-cursor-type"
        | "window-display-table"
        | "window-body-edges"
        | "window-body-pixel-edges"
        | "window-frame"
        | "window-fringes"
        | "window-group-start"
        | "window-header-line-height"
        | "window-dedicated-p"
        | "window-hscroll"
        | "window-left-column"
        | "window-margins"
        | "window-minibuffer-p"
        | "window-mode-line-height"
        | "window-pixel-edges"
        | "window-pixel-height"
        | "window-pixel-width"
        | "window-point"
        | "window-next-buffers"
        | "window-old-buffer"
        | "window-old-point"
        | "window-parameters"
        | "window-prev-buffers"
        | "window-scroll-bars"
        | "window-start"
        | "window-use-time"
        | "window-top-line" => arity_cons(0, Some(1)),
        "terminal-list"
        | "x-clear-preedit-text"
        | "x-display-list"
        | "x-get-clipboard"
        | "x-hide-tip"
        | "x-mouse-absolute-pixel-position"
        | "redraw-display"
        | "frame-list"
        | "visible-frame-list"
        | "x-uses-old-gtk-dialog"
        | "x-win-suspend-error"
        | "selected-window"
        | "active-minibuffer-window"
        | "minibuffer-selected-window" => arity_cons(0, Some(0)),
        "frame-edges"
        | "window-body-height"
        | "window-body-width"
        | "window-end"
        | "window-text-height"
        | "window-text-width"
        | "window-total-height"
        | "window-total-width"
        | "window-vscroll"
        | "x-export-frames"
        | "x-frame-edges"
        | "x-family-fonts"
        | "x-selection-exists-p"
        | "x-selection-owner-p"
        | "get-buffer-window" => arity_cons(0, Some(2)),
        "window-preserve-size" | "window-size-fixed-p" => arity_cons(0, Some(3)),
        "set-frame-height" | "set-frame-width" => arity_cons(2, Some(4)),
        "set-frame-size" => arity_cons(3, Some(4)),
        "set-frame-position" => arity_cons(3, Some(3)),
        "window-resizable" => arity_cons(2, Some(5)),
        "window-edges" => arity_cons(0, Some(4)),
        "window-list" | "get-buffer-window-list" => arity_cons(0, Some(3)),
        "terminal-live-p"
        | "frame-live-p"
        | "frame-visible-p"
        | "frame-root-window-p"
        | "window-live-p"
        | "window-configuration-p"
        | "window-valid-p"
        | "windowp"
        | "minibuffer-window-active-p" => arity_cons(1, Some(1)),
        "window-configuration-equal-p" => arity_cons(2, Some(2)),
        "window-configuration-frame" => arity_cons(1, Some(1)),
        "window-parameter" => arity_cons(2, Some(2)),
        "set-window-parameter" => arity_cons(3, Some(3)),
        "terminal-parameter" => arity_cons(2, Some(2)),
        "set-terminal-parameter" => arity_cons(3, Some(3)),
        // Threading primitives
        "thread-join"
        | "thread-name"
        | "thread-live-p"
        | "mutexp"
        | "mutex-name"
        | "mutex-lock"
        | "mutex-unlock"
        | "condition-variable-p"
        | "condition-name"
        | "condition-mutex"
        | "condition-wait" => arity_cons(1, Some(1)),
        "thread-yield" | "current-thread" | "all-threads" => arity_cons(0, Some(0)),
        "thread-signal" => arity_cons(3, Some(3)),
        "thread-last-error" | "make-mutex" => arity_cons(0, Some(1)),
        "make-thread" | "make-condition-variable" | "condition-notify" => arity_cons(1, Some(2)),
        _ => arity_cons(0, None),
    }
}

pub(crate) fn dispatch_subr_arity_value(name: &str) -> Value {
    subr_arity_value(name)
}

fn is_macro_object(value: &Value) -> bool {
    match value {
        Value::Macro(_) => true,
        Value::Cons(cell) => cell.lock().expect("poisoned").car.as_symbol_name() == Some("macro"),
        _ => false,
    }
}

fn autoload_macro_marker(value: &Value) -> Option<Value> {
    if !super::autoload::is_autoload_value(value) {
        return None;
    }

    let items = list_to_vec(value)?;
    let autoload_type = items.get(4)?;
    if autoload_type.as_symbol_name() == Some("macro") {
        Some(Value::list(vec![Value::symbol("macro"), Value::True]))
    } else if matches!(autoload_type, Value::True) {
        // GNU Emacs uses `t` as a legacy macro marker for some startup
        // autoloads (notably `pcase-dolist`), and `macrop` returns `(t)`.
        Some(Value::list(vec![Value::True]))
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// Pure builtins (no evaluator access)
// ---------------------------------------------------------------------------

/// `(subr-name SUBR)` -- return the name of a subroutine as a string.
pub(crate) fn builtin_subr_name(args: Vec<Value>) -> EvalResult {
    expect_args("subr-name", &args, 1)?;
    match &args[0] {
        Value::Subr(name) => Ok(Value::string(name.clone())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("subrp"), other.clone()],
        )),
    }
}

/// `(subr-arity SUBR)` -- return (MIN . MAX) cons cell for argument counts.
///
/// Built-in subrs are dispatched by name and we do not yet have complete
/// per-subr metadata in NeoVM, so this is a partial compatibility table:
/// known shapes are special-cased and other subrs default to `(0 . many)`.
pub(crate) fn builtin_subr_arity(args: Vec<Value>) -> EvalResult {
    expect_args("subr-arity", &args, 1)?;
    match &args[0] {
        Value::Subr(name) => Ok(subr_arity_value(name)),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("subrp"), other.clone()],
        )),
    }
}

/// `(subr-native-elisp-p OBJECT)` -- return t if OBJECT is a native-compiled
/// Elisp subr.
///
/// NeoVM does not currently model native-compiled Elisp subrs, so this always
/// returns nil.
pub(crate) fn builtin_subr_native_elisp_p(args: Vec<Value>) -> EvalResult {
    expect_args("subr-native-elisp-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(native-comp-function-p OBJECT)` -- return t if OBJECT is a native-compiled
/// function object.
///
/// NeoVM does not currently model native-compiled function objects, so this
/// always returns nil.
pub(crate) fn builtin_native_comp_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("native-comp-function-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(subr-primitive-p OBJECT)` -- return t if OBJECT is a primitive subr.
pub(crate) fn builtin_subr_primitive_p(args: Vec<Value>) -> EvalResult {
    expect_args("subr-primitive-p", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::Subr(_))))
}

/// `(interpreted-function-p OBJECT)` -- return t if OBJECT is an interpreted
/// function (a Lambda that is NOT byte-compiled).
///
/// In our VM, any `Value::Lambda` is interpreted (as opposed to
/// `Value::ByteCode`).
pub(crate) fn builtin_interpreted_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("interpreted-function-p", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::Lambda(_))))
}

/// `(special-form-p OBJECT)` -- return t if OBJECT is a symbol that names a
/// special form.
///
/// Accepts a symbol (including nil/t) and checks it against the evaluator's
/// special-form table.
pub(crate) fn builtin_special_form_p(args: Vec<Value>) -> EvalResult {
    expect_args("special-form-p", &args, 1)?;
    let result = match &args[0] {
        Value::Symbol(name) => is_public_special_form_name(name),
        Value::Subr(name) => is_public_special_form_name(name),
        _ => false,
    };
    Ok(Value::bool(result))
}

/// `(macrop OBJECT)` -- return t if OBJECT is a macro.
pub(crate) fn builtin_macrop(args: Vec<Value>) -> EvalResult {
    expect_args("macrop", &args, 1)?;
    if let Some(marker) = autoload_macro_marker(&args[0]) {
        return Ok(marker);
    }
    Ok(Value::bool(is_macro_object(&args[0])))
}

/// `(commandp FUNCTION &optional FOR-CALL-INTERACTIVELY)` -- return t if
/// FUNCTION is an interactive command.
///
/// In our simplified VM, any callable value (lambda, subr, bytecode) is
/// treated as a potential command.  A more complete implementation would
/// check for an `interactive` declaration.
pub(crate) fn builtin_commandp(args: Vec<Value>) -> EvalResult {
    expect_min_args("commandp", &args, 1)?;
    expect_max_args("commandp", &args, 2)?;
    Ok(Value::bool(args[0].is_function()))
}

/// `(func-arity FUNCTION)` -- return (MIN . MAX) for any callable.
///
/// Works for lambdas (reads `LambdaParams`), byte-code (reads `params`),
/// and subrs (returns `(0 . many)` as a conservative default).
pub(crate) fn builtin_func_arity(args: Vec<Value>) -> EvalResult {
    expect_args("func-arity", &args, 1)?;
    if super::autoload::is_autoload_value(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }
    match &args[0] {
        Value::Lambda(l) => {
            let min = l.params.min_arity();
            let max = l.params.max_arity();
            Ok(arity_cons(min, max))
        }
        Value::ByteCode(bc) => {
            let min = bc.params.min_arity();
            let max = bc.params.max_arity();
            Ok(arity_cons(min, max))
        }
        Value::Subr(name) => Ok(subr_arity_value(name)),
        Value::Macro(m) => {
            let min = m.params.min_arity();
            let max = m.params.max_arity();
            Ok(arity_cons(min, max))
        }
        other => Err(signal("invalid-function", vec![other.clone()])),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::value::{LambdaData, LambdaParams};
    use std::sync::Arc;

    fn make_lambda(required: Vec<&str>, optional: Vec<&str>, rest: Option<&str>) -> Value {
        Value::Lambda(Arc::new(LambdaData {
            params: LambdaParams {
                required: required.into_iter().map(String::from).collect(),
                optional: optional.into_iter().map(String::from).collect(),
                rest: rest.map(String::from),
            },
            body: vec![],
            env: None,
            docstring: None,
        }))
    }

    fn make_macro(required: Vec<&str>) -> Value {
        Value::Macro(Arc::new(LambdaData {
            params: LambdaParams::simple(required.into_iter().map(String::from).collect()),
            body: vec![],
            env: None,
            docstring: None,
        }))
    }

    fn make_bytecode(required: Vec<&str>, rest: Option<&str>) -> Value {
        use crate::elisp::bytecode::ByteCodeFunction;
        let params = LambdaParams {
            required: required.into_iter().map(String::from).collect(),
            optional: vec![],
            rest: rest.map(String::from),
        };
        Value::ByteCode(Arc::new(ByteCodeFunction::new(params)))
    }

    // -- subr-name --

    #[test]
    fn subr_name_returns_string() {
        let result = builtin_subr_name(vec![Value::Subr("cons".into())]).unwrap();
        assert_eq!(result.as_str(), Some("cons"));
    }

    #[test]
    fn subr_name_error_for_non_subr() {
        let result = builtin_subr_name(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    // -- subr-arity --

    fn assert_subr_arity(name: &str, min: i64, max: Option<i64>) {
        let result = builtin_subr_arity(vec![Value::Subr(name.to_string())]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(min));
            match max {
                Some(n) => assert_eq!(pair.cdr.as_int(), Some(n)),
                None => assert_eq!(pair.cdr.as_symbol_name(), Some("many")),
            }
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn subr_arity_returns_cons() {
        assert_subr_arity("+", 0, None);
    }

    #[test]
    fn subr_arity_error_for_non_subr() {
        let result = builtin_subr_arity(vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn subr_arity_message_is_one_or_more() {
        assert_subr_arity("message", 1, None);
        assert_subr_arity("message-box", 1, None);
        assert_subr_arity("message-or-box", 1, None);
    }

    #[test]
    fn subr_arity_if_is_unevalled() {
        let result = builtin_subr_arity(vec![Value::Subr("if".into())]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(2));
            assert_eq!(pair.cdr.as_symbol_name(), Some("unevalled"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn subr_arity_core_special_forms_match_oracle_unevalled_shapes() {
        for (name, min) in [
            ("and", 0),
            ("setq", 0),
            ("let", 1),
            ("quote", 1),
            ("catch", 1),
            ("defconst", 2),
            ("condition-case", 2),
            ("unwind-protect", 1),
        ] {
            let result = builtin_subr_arity(vec![Value::Subr(name.into())]).unwrap();
            if let Value::Cons(cell) = &result {
                let pair = cell.lock().unwrap();
                assert_eq!(pair.car.as_int(), Some(min));
                assert_eq!(pair.cdr.as_symbol_name(), Some("unevalled"));
            } else {
                panic!("expected cons cell for {name}");
            }
        }
    }

    #[test]
    fn subr_arity_thread_join_is_one() {
        assert_subr_arity("thread-join", 1, Some(1));
    }

    #[test]
    fn subr_arity_thread_signal_is_three() {
        assert_subr_arity("thread-signal", 3, Some(3));
    }

    #[test]
    fn subr_arity_thread_last_error_optional_cleanup() {
        assert_subr_arity("thread-last-error", 0, Some(1));
    }

    #[test]
    fn subr_arity_make_thread_optional_name() {
        assert_subr_arity("make-thread", 1, Some(2));
    }

    #[test]
    fn subr_arity_current_thread_is_zero() {
        assert_subr_arity("current-thread", 0, Some(0));
    }

    #[test]
    fn subr_arity_condition_notify_optional_all() {
        assert_subr_arity("condition-notify", 1, Some(2));
    }

    #[test]
    fn subr_arity_event_apply_modifier_is_four() {
        assert_subr_arity("event-apply-modifier", 4, Some(4));
    }

    #[test]
    fn subr_arity_thread_primitives_match_oracle() {
        assert_subr_arity("thread-join", 1, Some(1));
        assert_subr_arity("thread-yield", 0, Some(0));
        assert_subr_arity("thread-name", 1, Some(1));
        assert_subr_arity("thread-live-p", 1, Some(1));
        assert_subr_arity("thread-signal", 3, Some(3));
        assert_subr_arity("thread-last-error", 0, Some(1));
        assert_subr_arity("make-thread", 1, Some(2));
        assert_subr_arity("make-mutex", 0, Some(1));
        assert_subr_arity("mutexp", 1, Some(1));
        assert_subr_arity("mutex-name", 1, Some(1));
        assert_subr_arity("mutex-lock", 1, Some(1));
        assert_subr_arity("mutex-unlock", 1, Some(1));
        assert_subr_arity("make-condition-variable", 1, Some(2));
        assert_subr_arity("condition-variable-p", 1, Some(1));
        assert_subr_arity("condition-name", 1, Some(1));
        assert_subr_arity("condition-mutex", 1, Some(1));
        assert_subr_arity("condition-wait", 1, Some(1));
        assert_subr_arity("condition-notify", 1, Some(2));
        assert_subr_arity("current-thread", 0, Some(0));
        assert_subr_arity("all-threads", 0, Some(0));
    }

    #[test]
    fn subr_arity_display_terminal_primitives_match_oracle() {
        assert_subr_arity("display-images-p", 0, Some(1));
        assert_subr_arity("display-graphic-p", 0, Some(1));
        assert_subr_arity("display-color-p", 0, Some(1));
        assert_subr_arity("display-grayscale-p", 0, Some(1));
        assert_subr_arity("display-mouse-p", 0, Some(1));
        assert_subr_arity("display-popup-menus-p", 0, Some(1));
        assert_subr_arity("display-symbol-keys-p", 0, Some(1));
        assert_subr_arity("display-pixel-width", 0, Some(1));
        assert_subr_arity("display-pixel-height", 0, Some(1));
        assert_subr_arity("display-mm-width", 0, Some(1));
        assert_subr_arity("display-mm-height", 0, Some(1));
        assert_subr_arity("display-screens", 0, Some(1));
        assert_subr_arity("display-color-cells", 0, Some(1));
        assert_subr_arity("display-planes", 0, Some(1));
        assert_subr_arity("display-visual-class", 0, Some(1));
        assert_subr_arity("display-backing-store", 0, Some(1));
        assert_subr_arity("display-save-under", 0, Some(1));
        assert_subr_arity("display-selections-p", 0, Some(1));
        assert_subr_arity("gui-get-primary-selection", 0, Some(0));
        assert_subr_arity("gui-get-selection", 0, Some(2));
        assert_subr_arity("gui-select-text", 1, Some(1));
        assert_subr_arity("gui-selection-value", 0, Some(0));
        assert_subr_arity("gui-set-selection", 2, Some(2));
        assert_subr_arity("display-supports-face-attributes-p", 1, Some(2));
        assert_subr_arity("ding", 0, Some(1));
        assert_subr_arity("redraw-display", 0, Some(0));
        assert_subr_arity("redraw-frame", 0, Some(1));
        assert_subr_arity("open-termscript", 1, Some(1));
        assert_subr_arity("send-string-to-terminal", 1, Some(2));
        assert_subr_arity("internal-show-cursor", 2, Some(2));
        assert_subr_arity("internal-show-cursor-p", 0, Some(1));
        assert_subr_arity("display-monitor-attributes-list", 0, Some(1));
        assert_subr_arity("frame-monitor-attributes", 0, Some(1));
        assert_subr_arity("window-system", 0, Some(1));
        assert_subr_arity("frame-edges", 0, Some(2));
        assert_subr_arity("terminal-name", 0, Some(1));
        assert_subr_arity("terminal-list", 0, Some(0));
        assert_subr_arity("terminal-live-p", 1, Some(1));
        assert_subr_arity("frame-terminal", 0, Some(1));
        assert_subr_arity("frame-selected-window", 0, Some(1));
        assert_subr_arity("terminal-parameter", 2, Some(2));
        assert_subr_arity("terminal-parameters", 0, Some(1));
        assert_subr_arity("set-terminal-parameter", 3, Some(3));
        assert_subr_arity("tty-type", 0, Some(1));
        assert_subr_arity("tty-display-color-p", 0, Some(1));
        assert_subr_arity("tty-display-color-cells", 0, Some(1));
        assert_subr_arity("tty-no-underline", 0, Some(1));
        assert_subr_arity("tty-top-frame", 0, Some(1));
        assert_subr_arity("controlling-tty-p", 0, Some(1));
        assert_subr_arity("suspend-tty", 0, Some(1));
        assert_subr_arity("resume-tty", 0, Some(1));
        assert_subr_arity("terminal-coding-system", 0, Some(1));
        assert_subr_arity("x-apply-session-resources", 0, Some(0));
        assert_subr_arity("x-backspace-delete-keys-p", 0, Some(1));
        assert_subr_arity("x-change-window-property", 2, Some(7));
        assert_subr_arity("x-clear-preedit-text", 0, Some(0));
        assert_subr_arity("x-clipboard-yank", 0, Some(0));
        assert_subr_arity("x-delete-window-property", 1, Some(3));
        assert_subr_arity("x-device-class", 1, Some(1));
        assert_subr_arity("x-disown-selection-internal", 1, Some(3));
        assert_subr_arity("x-export-frames", 0, Some(2));
        assert_subr_arity("x-display-list", 0, Some(0));
        assert_subr_arity("x-family-fonts", 0, Some(2));
        assert_subr_arity("x-focus-frame", 1, Some(2));
        assert_subr_arity("x-frame-edges", 0, Some(2));
        assert_subr_arity("x-frame-geometry", 0, Some(1));
        assert_subr_arity("x-frame-list-z-order", 0, Some(1));
        assert_subr_arity("x-frame-restack", 2, Some(3));
        assert_subr_arity("x-get-atom-name", 1, Some(2));
        assert_subr_arity("x-get-clipboard", 0, Some(0));
        assert_subr_arity("x-get-input-coding-system", 1, Some(1));
        assert_subr_arity("x-get-local-selection", 0, Some(2));
        assert_subr_arity("x-get-modifier-masks", 0, Some(1));
        assert_subr_arity("x-get-selection-internal", 2, Some(4));
        assert_subr_arity("x-hide-tip", 0, Some(0));
        assert_subr_arity("x-internal-focus-input-context", 1, Some(1));
        assert_subr_arity("x-mouse-absolute-pixel-position", 0, Some(0));
        assert_subr_arity("x-get-resource", 2, Some(4));
        assert_subr_arity("x-list-fonts", 1, Some(5));
        assert_subr_arity("x-open-connection", 1, Some(3));
        assert_subr_arity("x-own-selection-internal", 2, Some(3));
        assert_subr_arity("x-parse-geometry", 1, Some(1));
        assert_subr_arity("x-preedit-text", 1, Some(1));
        assert_subr_arity("x-popup-dialog", 2, Some(3));
        assert_subr_arity("x-popup-menu", 2, Some(2));
        assert_subr_arity("x-register-dnd-atom", 1, Some(2));
        assert_subr_arity("x-selection-exists-p", 0, Some(2));
        assert_subr_arity("x-selection-owner-p", 0, Some(2));
        assert_subr_arity("x-send-client-message", 6, Some(6));
        assert_subr_arity("x-close-connection", 1, Some(1));
        assert_subr_arity("x-server-version", 0, Some(1));
        assert_subr_arity("x-server-input-extension-version", 0, Some(1));
        assert_subr_arity("x-server-max-request-size", 0, Some(1));
        assert_subr_arity("x-server-vendor", 0, Some(1));
        assert_subr_arity("x-setup-function-keys", 1, Some(1));
        assert_subr_arity("x-show-tip", 1, Some(6));
        assert_subr_arity("x-display-grayscale-p", 0, Some(1));
        assert_subr_arity("x-display-backing-store", 0, Some(1));
        assert_subr_arity("x-display-color-cells", 0, Some(1));
        assert_subr_arity("x-display-mm-height", 0, Some(1));
        assert_subr_arity("x-display-mm-width", 0, Some(1));
        assert_subr_arity("x-display-monitor-attributes-list", 0, Some(1));
        assert_subr_arity("x-display-pixel-width", 0, Some(1));
        assert_subr_arity("x-display-pixel-height", 0, Some(1));
        assert_subr_arity("x-display-planes", 0, Some(1));
        assert_subr_arity("x-display-save-under", 0, Some(1));
        assert_subr_arity("x-display-screens", 0, Some(1));
        assert_subr_arity("x-display-set-last-user-time", 1, Some(2));
        assert_subr_arity("x-display-visual-class", 0, Some(1));
        assert_subr_arity("x-set-mouse-absolute-pixel-position", 2, Some(2));
        assert_subr_arity("x-synchronize", 1, Some(2));
        assert_subr_arity("x-translate-coordinates", 1, Some(6));
        assert_subr_arity("x-uses-old-gtk-dialog", 0, Some(0));
        assert_subr_arity("x-win-suspend-error", 0, Some(0));
        assert_subr_arity("x-window-property", 1, Some(6));
        assert_subr_arity("x-window-property-attributes", 1, Some(3));
        assert_subr_arity("x-wm-set-size-hint", 0, Some(1));
    }

    #[test]
    fn subr_arity_image_font_primitives_match_oracle() {
        assert_subr_arity("clear-face-cache", 0, Some(1));
        assert_subr_arity("clear-font-cache", 0, Some(0));
        assert_subr_arity("clear-image-cache", 0, Some(2));
        assert_subr_arity("clear-string", 1, Some(1));
        assert_subr_arity("create-image", 1, None);
        assert_subr_arity("find-font", 1, Some(2));
        assert_subr_arity("font-family-list", 0, Some(1));
        assert_subr_arity("image-cache-size", 0, Some(0));
        assert_subr_arity("image-flush", 1, Some(2));
        assert_subr_arity("image-mask-p", 1, Some(2));
        assert_subr_arity("image-metadata", 1, Some(2));
        assert_subr_arity("imagep", 1, Some(1));
        assert_subr_arity("image-size", 1, Some(3));
        assert_subr_arity("image-transforms-p", 0, Some(1));
        assert_subr_arity("image-type", 1, Some(3));
        assert_subr_arity("image-type-available-p", 1, Some(1));
        assert_subr_arity("list-fonts", 1, Some(4));
    }

    #[test]
    fn subr_arity_process_primitives_match_oracle() {
        assert_subr_arity("accept-process-output", 0, Some(4));
        assert_subr_arity("backquote-delay-process", 2, Some(2));
        assert_subr_arity("backquote-process", 1, Some(2));
        assert_subr_arity("call-process", 1, None);
        assert_subr_arity("call-process-shell-command", 1, None);
        assert_subr_arity("call-process-region", 3, None);
        assert_subr_arity("clone-process", 1, Some(2));
        assert_subr_arity("continue-process", 0, Some(2));
        assert_subr_arity("delete-process", 0, Some(1));
        assert_subr_arity("format-network-address", 1, Some(2));
        assert_subr_arity("get-buffer-process", 1, Some(1));
        assert_subr_arity("get-process", 1, Some(1));
        assert_subr_arity("getenv", 1, Some(2));
        assert_subr_arity("getenv-internal", 1, Some(2));
        assert_subr_arity("internal-default-interrupt-process", 0, Some(2));
        assert_subr_arity("internal-default-process-filter", 2, Some(2));
        assert_subr_arity("internal-default-process-sentinel", 2, Some(2));
        assert_subr_arity("internal-default-signal-process", 2, Some(3));
        assert_subr_arity("interrupt-process", 0, Some(2));
        assert_subr_arity("isearch-process-search-char", 1, Some(2));
        assert_subr_arity("isearch-process-search-string", 2, Some(2));
        assert_subr_arity("kill-process", 0, Some(2));
        assert_subr_arity("list-processes", 0, Some(2));
        assert_subr_arity("list-processes--refresh", 0, Some(0));
        assert_subr_arity("list-system-processes", 0, Some(0));
        assert_subr_arity("make-process", 0, None);
        assert_subr_arity("make-network-process", 0, None);
        assert_subr_arity("make-pipe-process", 0, None);
        assert_subr_arity("make-serial-process", 0, None);
        assert_subr_arity("minibuffer--sort-preprocess-history", 1, Some(1));
        assert_subr_arity("network-interface-info", 1, Some(1));
        assert_subr_arity("network-interface-list", 0, Some(2));
        assert_subr_arity("network-lookup-address-info", 1, Some(3));
        assert_subr_arity("num-processors", 0, Some(1));
        assert_subr_arity("print--preprocess", 1, Some(1));
        assert_subr_arity("process-attributes", 1, Some(1));
        assert_subr_arity("process-buffer", 1, Some(1));
        assert_subr_arity("process-coding-system", 1, Some(1));
        assert_subr_arity("process-command", 1, Some(1));
        assert_subr_arity("process-contact", 1, Some(3));
        assert_subr_arity("process-datagram-address", 1, Some(1));
        assert_subr_arity("process-exit-status", 1, Some(1));
        assert_subr_arity("process-file", 1, None);
        assert_subr_arity("process-file-shell-command", 1, None);
        assert_subr_arity("process-filter", 1, Some(1));
        assert_subr_arity("process-get", 2, Some(2));
        assert_subr_arity("process-id", 1, Some(1));
        assert_subr_arity("process-inherit-coding-system-flag", 1, Some(1));
        assert_subr_arity("process-kill-buffer-query-function", 0, Some(0));
        assert_subr_arity("process-lines", 1, None);
        assert_subr_arity("process-lines-ignore-status", 1, None);
        assert_subr_arity("process-lines-handling-status", 2, None);
        assert_subr_arity("process-live-p", 1, Some(1));
        assert_subr_arity("process-list", 0, Some(0));
        assert_subr_arity("process-mark", 1, Some(1));
        assert_subr_arity("process-menu-delete-process", 0, Some(0));
        assert_subr_arity("process-menu-mode", 0, Some(0));
        assert_subr_arity("process-menu-visit-buffer", 1, Some(1));
        assert_subr_arity("process-name", 1, Some(1));
        assert_subr_arity("process-plist", 1, Some(1));
        assert_subr_arity("process-put", 3, Some(3));
        assert_subr_arity("process-query-on-exit-flag", 1, Some(1));
        assert_subr_arity("quit-process", 0, Some(2));
        assert_subr_arity("process-running-child-p", 0, Some(1));
        assert_subr_arity("process-send-eof", 0, Some(1));
        assert_subr_arity("process-send-region", 3, Some(3));
        assert_subr_arity("process-send-string", 2, Some(2));
        assert_subr_arity("process-sentinel", 1, Some(1));
        assert_subr_arity("signal-names", 0, Some(0));
        assert_subr_arity("signal-process", 2, Some(3));
        assert_subr_arity("process-status", 1, Some(1));
        assert_subr_arity("stop-process", 0, Some(2));
        assert_subr_arity("process-thread", 1, Some(1));
        assert_subr_arity("process-tty-name", 1, Some(2));
        assert_subr_arity("process-type", 1, Some(1));
        assert_subr_arity("processp", 1, Some(1));
        assert_subr_arity("set-process-buffer", 2, Some(2));
        assert_subr_arity("set-process-coding-system", 1, Some(3));
        assert_subr_arity("set-process-datagram-address", 2, Some(2));
        assert_subr_arity("set-process-filter", 2, Some(2));
        assert_subr_arity("set-process-inherit-coding-system-flag", 2, Some(2));
        assert_subr_arity("set-process-plist", 2, Some(2));
        assert_subr_arity("set-process-query-on-exit-flag", 2, Some(2));
        assert_subr_arity("set-process-sentinel", 2, Some(2));
        assert_subr_arity("set-process-thread", 2, Some(2));
        assert_subr_arity("set-process-window-size", 3, Some(3));
        assert_subr_arity("set-buffer-process-coding-system", 2, Some(2));
        assert_subr_arity("set-binary-mode", 2, Some(2));
        assert_subr_arity("set-network-process-option", 3, Some(4));
        assert_subr_arity("serial-process-configure", 0, None);
        assert_subr_arity("setenv", 1, Some(3));
        assert_subr_arity("start-process", 3, None);
        assert_subr_arity("start-process-shell-command", 3, Some(3));
        assert_subr_arity("start-file-process", 3, None);
        assert_subr_arity("start-file-process-shell-command", 3, Some(3));
        assert_subr_arity("syntax-propertize--in-process-p", 0, Some(0));
        assert_subr_arity("tooltip-process-prompt-regexp", 1, Some(1));
        assert_subr_arity("window--adjust-process-windows", 0, Some(0));
        assert_subr_arity("window--process-window-list", 0, Some(0));
        assert_subr_arity("window-adjust-process-window-size", 2, Some(2));
        assert_subr_arity("window-adjust-process-window-size-largest", 2, Some(2));
        assert_subr_arity("window-adjust-process-window-size-smallest", 2, Some(2));
    }

    #[test]
    fn subr_arity_core_math_primitives_match_oracle() {
        assert_subr_arity("%", 2, Some(2));
        assert_subr_arity("/", 1, None);
        assert_subr_arity("/=", 2, Some(2));
        assert_subr_arity("1+", 1, Some(1));
        assert_subr_arity("1-", 1, Some(1));
        assert_subr_arity("<", 1, None);
        assert_subr_arity("<=", 1, None);
        assert_subr_arity("=", 1, None);
        assert_subr_arity(">", 1, None);
        assert_subr_arity(">=", 1, None);
        assert_subr_arity("abs", 1, Some(1));
        assert_subr_arity("ash", 2, Some(2));
        assert_subr_arity("apply", 1, None);
        assert_subr_arity("always", 0, None);
    }

    #[test]
    fn subr_arity_minibuffer_control_primitives_match_oracle() {
        assert_subr_arity("recursive-edit", 0, Some(0));
        assert_subr_arity("top-level", 0, Some(0));
        assert_subr_arity("exit-recursive-edit", 0, Some(0));
        assert_subr_arity("abort-minibuffers", 0, Some(0));
        assert_subr_arity("abort-recursive-edit", 0, Some(0));
        assert_subr_arity("exit-minibuffer", 0, Some(0));
        assert_subr_arity("minibuffer-depth", 0, Some(0));
        assert_subr_arity("minibufferp", 0, Some(2));
        assert_subr_arity("next-read-file-uses-dialog-p", 0, Some(0));
        assert_subr_arity("minibuffer-prompt", 0, Some(0));
        assert_subr_arity("minibuffer-contents", 0, Some(0));
        assert_subr_arity("minibuffer-contents-no-properties", 0, Some(0));
    }

    #[test]
    fn subr_arity_point_navigation_primitives_match_oracle() {
        assert_subr_arity("beginning-of-line", 0, Some(1));
        assert_subr_arity("end-of-line", 0, Some(1));
        assert_subr_arity("beginning-of-buffer", 0, Some(1));
        assert_subr_arity("end-of-buffer", 0, Some(1));
        assert_subr_arity("forward-char", 0, Some(1));
        assert_subr_arity("backward-char", 0, Some(1));
        assert_subr_arity("forward-word", 0, Some(1));
        assert_subr_arity("backward-word", 0, Some(1));
        assert_subr_arity("forward-line", 0, Some(1));
        assert_subr_arity("goto-char", 1, Some(1));
        assert_subr_arity("point-max", 0, Some(0));
        assert_subr_arity("point-min", 0, Some(0));
        assert_subr_arity("bobp", 0, Some(0));
        assert_subr_arity("eobp", 0, Some(0));
        assert_subr_arity("bolp", 0, Some(0));
        assert_subr_arity("eolp", 0, Some(0));
    }

    #[test]
    fn subr_arity_buffer_point_primitives_match_oracle() {
        assert_subr_arity("current-buffer", 0, Some(0));
        assert_subr_arity("buffer-string", 0, Some(0));
        assert_subr_arity("point", 0, Some(0));
        assert_subr_arity("point-min", 0, Some(0));
        assert_subr_arity("point-max", 0, Some(0));
        assert_subr_arity("erase-buffer", 0, Some(0));
        assert_subr_arity("widen", 0, Some(0));
        assert_subr_arity("barf-if-buffer-read-only", 0, Some(1));
        assert_subr_arity("buffer-file-name", 0, Some(1));
        assert_subr_arity("buffer-base-buffer", 0, Some(1));
        assert_subr_arity("buffer-last-name", 0, Some(1));
        assert_subr_arity("buffer-name", 0, Some(1));
        assert_subr_arity("buffer-size", 0, Some(1));
        assert_subr_arity("buffer-chars-modified-tick", 0, Some(1));
        assert_subr_arity("buffer-modified-p", 0, Some(1));
        assert_subr_arity("buffer-modified-tick", 0, Some(1));
        assert_subr_arity("buffer-list", 0, Some(1));
        assert_subr_arity("other-buffer", 0, Some(3));
        assert_subr_arity("bury-buffer-internal", 1, Some(1));
        assert_subr_arity("buffer-disable-undo", 0, Some(1));
        assert_subr_arity("buffer-enable-undo", 0, Some(1));
        assert_subr_arity("buffer-hash", 0, Some(1));
        assert_subr_arity("buffer-line-statistics", 0, Some(1));
        assert_subr_arity("buffer-local-variables", 0, Some(1));
        assert_subr_arity("buffer-live-p", 1, Some(1));
        assert_subr_arity("buffer-swap-text", 1, Some(1));
        assert_subr_arity("buffer-local-value", 2, Some(2));
        assert_subr_arity("buffer-substring", 2, Some(2));
        assert_subr_arity("buffer-substring-no-properties", 2, Some(2));
        assert_subr_arity("constrain-to-field", 2, Some(5));
        assert_subr_arity("field-beginning", 0, Some(3));
        assert_subr_arity("field-end", 0, Some(3));
        assert_subr_arity("field-string", 0, Some(1));
        assert_subr_arity("field-string-no-properties", 0, Some(1));
        assert_subr_arity("byte-to-position", 1, Some(1));
        assert_subr_arity("position-bytes", 1, Some(1));
    }

    #[test]
    fn subr_arity_mark_marker_primitives_match_oracle() {
        assert_subr_arity("exchange-point-and-mark", 0, Some(1));
        assert_subr_arity("mark", 0, Some(1));
        assert_subr_arity("mark-marker", 0, Some(0));
        assert_subr_arity("point-marker", 0, Some(0));
        assert_subr_arity("point-min-marker", 0, Some(0));
        assert_subr_arity("point-max-marker", 0, Some(0));
        assert_subr_arity("marker-buffer", 1, Some(1));
        assert_subr_arity("marker-insertion-type", 1, Some(1));
        assert_subr_arity("marker-position", 1, Some(1));
        assert_subr_arity("markerp", 1, Some(1));
        assert_subr_arity("push-mark", 0, Some(3));
        assert_subr_arity("pop-mark", 0, Some(0));
        assert_subr_arity("set-mark", 1, Some(1));
        assert_subr_arity("set-mark-command", 1, Some(1));
        assert_subr_arity("set-marker", 2, Some(3));
        assert_subr_arity("set-marker-insertion-type", 2, Some(2));
    }

    #[test]
    fn subr_arity_register_helper_primitives_match_oracle() {
        assert_subr_arity("get-register", 1, Some(1));
        assert_subr_arity("increment-register", 2, Some(2));
        assert_subr_arity("insert-register", 1, Some(2));
        assert_subr_arity("number-to-register", 2, Some(2));
        assert_subr_arity("point-to-register", 1, Some(2));
        assert_subr_arity("set-register", 2, Some(2));
        assert_subr_arity("register-ccl-program", 2, Some(2));
        assert_subr_arity("register-code-conversion-map", 2, Some(2));
    }

    #[test]
    fn subr_arity_list_sequence_primitives_match_oracle() {
        assert_subr_arity("max", 1, None);
        assert_subr_arity("min", 1, None);
        assert_subr_arity("mod", 2, Some(2));
        assert_subr_arity("nreverse", 1, Some(1));
        assert_subr_arity("nth", 2, Some(2));
        assert_subr_arity("nthcdr", 2, Some(2));
        assert_subr_arity("remq", 2, Some(2));
        assert_subr_arity("reverse", 1, Some(1));
        assert_subr_arity("safe-length", 1, Some(1));
        assert_subr_arity("proper-list-p", 1, Some(1));
        assert_subr_arity("make-list", 2, Some(2));
        assert_subr_arity("mapcar", 2, Some(2));
        assert_subr_arity("mapc", 2, Some(2));
        assert_subr_arity("mapcan", 2, Some(2));
        assert_subr_arity("mapconcat", 2, Some(3));
    }

    #[test]
    fn subr_arity_char_charset_primitives_match_oracle() {
        assert_subr_arity("char-after", 0, Some(1));
        assert_subr_arity("char-before", 0, Some(1));
        assert_subr_arity("char-category-set", 1, Some(1));
        assert_subr_arity("char-charset", 1, Some(2));
        assert_subr_arity("char-displayable-p", 1, Some(1));
        assert_subr_arity("char-equal", 2, Some(2));
        assert_subr_arity("char-or-string-p", 1, Some(1));
        assert_subr_arity("char-resolve-modifiers", 1, Some(1));
        assert_subr_arity("char-syntax", 1, Some(1));
        assert_subr_arity("char-width", 1, Some(1));
        assert_subr_arity("char-table-p", 1, Some(1));
        assert_subr_arity("char-table-parent", 1, Some(1));
        assert_subr_arity("char-table-subtype", 1, Some(1));
        assert_subr_arity("char-table-extra-slot", 2, Some(2));
        assert_subr_arity("char-table-range", 2, Some(2));
        assert_subr_arity("charset-after", 0, Some(1));
        assert_subr_arity("charset-id-internal", 0, Some(1));
        assert_subr_arity("charset-plist", 1, Some(1));
        assert_subr_arity("charset-priority-list", 0, Some(1));
        assert_subr_arity("define-charset-alias", 2, Some(2));
        assert_subr_arity("declare-equiv-charset", 4, Some(4));
        assert_subr_arity("decode-big5-char", 1, Some(1));
        assert_subr_arity("decode-sjis-char", 1, Some(1));
        assert_subr_arity("encode-big5-char", 1, Some(1));
        assert_subr_arity("encode-sjis-char", 1, Some(1));
        assert_subr_arity("get-unused-iso-final-char", 2, Some(2));
    }

    #[test]
    fn subr_arity_assoc_predicate_primitives_match_oracle() {
        assert_subr_arity("assoc", 2, Some(3));
        assert_subr_arity("assoc-delete-all", 2, Some(3));
        assert_subr_arity("assoc-string", 2, Some(3));
        assert_subr_arity("assoc-default", 2, Some(4));
        assert_subr_arity("assq", 2, Some(2));
        assert_subr_arity("assq-delete-all", 2, Some(2));
        assert_subr_arity("car-less-than-car", 2, Some(2));
        assert_subr_arity("member", 2, Some(2));
        assert_subr_arity("memq", 2, Some(2));
        assert_subr_arity("memql", 2, Some(2));
        assert_subr_arity("rassoc", 2, Some(2));
        assert_subr_arity("rassq", 2, Some(2));
        assert_subr_arity("bare-symbol", 1, Some(1));
        assert_subr_arity("bare-symbol-p", 1, Some(1));
        assert_subr_arity("bignump", 1, Some(1));
        assert_subr_arity("boundp", 1, Some(1));
        assert_subr_arity("byte-code-function-p", 1, Some(1));
        assert_subr_arity("car-safe", 1, Some(1));
        assert_subr_arity("cdr-safe", 1, Some(1));
    }

    #[test]
    fn subr_arity_navigation_case_primitives_match_oracle() {
        assert_subr_arity("back-to-indentation", 0, Some(0));
        assert_subr_arity("backward-prefix-chars", 0, Some(0));
        assert_subr_arity("backward-sexp", 0, Some(2));
        assert_subr_arity("backward-kill-word", 1, Some(1));
        assert_subr_arity("move-beginning-of-line", 1, Some(1));
        assert_subr_arity("capitalize", 1, Some(1));
        assert_subr_arity("capitalize-word", 1, Some(1));
        assert_subr_arity("capitalize-region", 2, Some(3));
        assert_subr_arity("count-lines", 2, Some(3));
    }

    #[test]
    fn subr_arity_kill_edit_primitives_match_oracle() {
        assert_subr_arity("downcase-region", 2, Some(3));
        assert_subr_arity("downcase-word", 1, Some(1));
        assert_subr_arity("move-end-of-line", 1, Some(1));
        assert_subr_arity("just-one-space", 0, Some(1));
        assert_subr_arity("kill-append", 2, Some(2));
        assert_subr_arity("kill-buffer", 0, Some(1));
        assert_subr_arity("kill-line", 0, Some(1));
        assert_subr_arity("kill-local-variable", 1, Some(1));
        assert_subr_arity("kill-new", 1, Some(2));
        assert_subr_arity("kill-region", 2, Some(3));
        assert_subr_arity("kill-ring-save", 2, Some(3));
        assert_subr_arity("kill-whole-line", 0, Some(1));
        assert_subr_arity("kill-word", 1, Some(1));
    }

    #[test]
    fn subr_arity_hook_advice_primitives_match_oracle() {
        assert_subr_arity("add-face-text-property", 3, Some(5));
        assert_subr_arity("add-hook", 2, Some(4));
        assert_subr_arity("add-name-to-file", 2, Some(3));
        assert_subr_arity("add-text-properties", 3, Some(4));
        assert_subr_arity("add-timeout", 3, Some(4));
        assert_subr_arity("add-variable-watcher", 2, Some(2));
        assert_subr_arity("remove-hook", 2, Some(3));
        assert_subr_arity("advice-add", 3, Some(4));
        assert_subr_arity("advice-remove", 2, Some(2));
        assert_subr_arity("advice-member-p", 2, Some(2));
        assert_subr_arity("autoload", 2, Some(5));
        assert_subr_arity("autoload-do-load", 1, Some(3));
        assert_subr_arity("backtrace--frames-from-thread", 1, Some(1));
        assert_subr_arity("backtrace--locals", 1, Some(2));
        assert_subr_arity("backtrace-debug", 2, Some(3));
        assert_subr_arity("backtrace-eval", 2, Some(3));
        assert_subr_arity("backtrace-frame--internal", 3, Some(3));
        assert_subr_arity("backtrace-frame", 1, Some(2));
        assert_subr_arity("run-hook-with-args", 1, None);
        assert_subr_arity("run-hook-with-args-until-failure", 1, None);
        assert_subr_arity("run-hook-with-args-until-success", 1, None);
        assert_subr_arity("run-hook-query-error-with-timeout", 1, Some(1));
        assert_subr_arity("run-hook-wrapped", 2, None);
        assert_subr_arity("run-mode-hooks", 0, None);
        assert_subr_arity("run-window-configuration-change-hook", 0, Some(1));
        assert_subr_arity("run-window-scroll-functions", 0, Some(1));
    }

    #[test]
    fn subr_arity_doc_helper_primitives_match_oracle() {
        assert_subr_arity("Snarf-documentation", 1, Some(1));
        assert_subr_arity("substitute-command-keys", 1, Some(3));
        assert_subr_arity("documentation", 1, Some(2));
        assert_subr_arity("documentation-stringp", 1, Some(1));
        assert_subr_arity("documentation-property", 2, Some(3));
        assert_subr_arity("help-function-arglist", 1, Some(2));
    }

    #[test]
    fn subr_arity_coding_time_primitives_match_oracle() {
        assert_subr_arity("decode-coding-string", 2, Some(4));
        assert_subr_arity("decode-time", 0, Some(3));
        assert_subr_arity("detect-coding-region", 2, Some(3));
        assert_subr_arity("detect-coding-string", 1, Some(2));
        assert_subr_arity("encode-char", 2, Some(2));
        assert_subr_arity("encode-coding-string", 2, Some(4));
        assert_subr_arity("encode-time", 1, None);
        assert_subr_arity("format-time-string", 1, Some(3));
    }

    #[test]
    fn subr_arity_indent_primitives_match_oracle() {
        assert_subr_arity("indent-according-to-mode", 0, Some(1));
        assert_subr_arity("indent-for-tab-command", 0, Some(1));
        assert_subr_arity("indent-line-to", 1, Some(1));
        assert_subr_arity("indent-region", 2, Some(3));
        assert_subr_arity("indent-rigidly", 3, Some(4));
        assert_subr_arity("indent-to", 1, Some(2));
        assert_subr_arity("tab-to-tab-stop", 0, Some(0));
        assert_subr_arity("move-to-column", 1, Some(2));
    }

    #[test]
    fn subr_arity_text_property_overlay_primitives_match_oracle() {
        assert_subr_arity("put-text-property", 4, Some(5));
        assert_subr_arity("set-text-properties", 3, Some(4));
        assert_subr_arity("remove-text-properties", 3, Some(4));
        assert_subr_arity("remove-list-of-text-properties", 3, Some(4));
        assert_subr_arity("get-text-property", 2, Some(3));
        assert_subr_arity("get-char-property", 2, Some(3));
        assert_subr_arity("get-char-property-and-overlay", 2, Some(3));
        assert_subr_arity("get-display-property", 2, Some(4));
        assert_subr_arity("get-pos-property", 2, Some(3));
        assert_subr_arity("text-properties-at", 1, Some(2));
        assert_subr_arity("next-single-property-change", 2, Some(4));
        assert_subr_arity("next-single-char-property-change", 2, Some(4));
        assert_subr_arity("previous-single-property-change", 2, Some(4));
        assert_subr_arity("previous-single-char-property-change", 2, Some(4));
        assert_subr_arity("next-property-change", 1, Some(3));
        assert_subr_arity("previous-property-change", 1, Some(3));
        assert_subr_arity("next-char-property-change", 1, Some(2));
        assert_subr_arity("previous-char-property-change", 1, Some(2));
        assert_subr_arity("text-property-any", 4, Some(5));
        assert_subr_arity("text-property-not-all", 4, Some(5));
        assert_subr_arity("make-overlay", 2, Some(5));
        assert_subr_arity("move-overlay", 3, Some(4));
        assert_subr_arity("overlay-put", 3, Some(3));
        assert_subr_arity("overlay-get", 2, Some(2));
        assert_subr_arity("next-overlay-change", 1, Some(1));
        assert_subr_arity("previous-overlay-change", 1, Some(1));
        assert_subr_arity("overlay-start", 1, Some(1));
        assert_subr_arity("overlay-end", 1, Some(1));
        assert_subr_arity("overlay-buffer", 1, Some(1));
        assert_subr_arity("overlay-properties", 1, Some(1));
        assert_subr_arity("overlays-at", 1, Some(2));
        assert_subr_arity("overlays-in", 2, Some(2));
        assert_subr_arity("remove-overlays", 0, Some(4));
        assert_subr_arity("overlayp", 1, Some(1));
    }

    #[test]
    fn subr_arity_encoding_bool_vector_primitives_match_oracle() {
        assert_subr_arity("bool-vector", 0, None);
        assert_subr_arity("base64-decode-region", 2, Some(4));
        assert_subr_arity("base64-encode-region", 2, Some(3));
        assert_subr_arity("base64-decode-string", 1, Some(3));
        assert_subr_arity("base64-encode-string", 1, Some(2));
        assert_subr_arity("base64url-encode-region", 2, Some(3));
        assert_subr_arity("base64url-encode-string", 1, Some(2));
        assert_subr_arity("bool-vector-p", 1, Some(1));
        assert_subr_arity("bool-vector-count-population", 1, Some(1));
        assert_subr_arity("bool-vector-count-consecutive", 3, Some(3));
        assert_subr_arity("bool-vector-not", 1, Some(2));
        assert_subr_arity("bool-vector-subsetp", 2, Some(2));
        assert_subr_arity("bool-vector-exclusive-or", 2, Some(3));
        assert_subr_arity("bool-vector-intersection", 2, Some(3));
        assert_subr_arity("bool-vector-set-difference", 2, Some(3));
        assert_subr_arity("bool-vector-union", 2, Some(3));
    }

    #[test]
    fn subr_arity_runtime_covered_primitives_match_oracle() {
        assert_subr_arity("aref", 2, Some(2));
        assert_subr_arity("arrayp", 1, Some(1));
        assert_subr_arity("aset", 3, Some(3));
        assert_subr_arity("atom", 1, Some(1));
        assert_subr_arity("booleanp", 1, Some(1));
        assert_subr_arity("bufferp", 1, Some(1));
        assert_subr_arity("ceiling", 1, Some(2));
        assert_subr_arity("char-to-string", 1, Some(1));
        assert_subr_arity("char-uppercase-p", 1, Some(1));
        assert_subr_arity("characterp", 1, Some(2));
        assert_subr_arity("consp", 1, Some(1));
        assert_subr_arity("downcase", 1, Some(1));
        assert_subr_arity("eq", 2, Some(2));
        assert_subr_arity("eql", 2, Some(2));
        assert_subr_arity("equal", 2, Some(2));
        assert_subr_arity("funcall", 1, None);
        assert_subr_arity("funcall-interactively", 1, None);
        assert_subr_arity("funcall-with-delayed-message", 3, Some(3));
        assert_subr_arity("float", 1, Some(1));
        assert_subr_arity("floatp", 1, Some(1));
        assert_subr_arity("floor", 1, Some(2));
        assert_subr_arity("integer-or-null-p", 1, Some(1));
        assert_subr_arity("integerp", 1, Some(1));
        assert_subr_arity("keywordp", 1, Some(1));
        assert_subr_arity("list-of-strings-p", 1, Some(1));
        assert_subr_arity("listp", 1, Some(1));
        assert_subr_arity("make-vector", 2, Some(2));
        assert_subr_arity("nlistp", 1, Some(1));
        assert_subr_arity("null", 1, Some(1));
        assert_subr_arity("number-to-string", 1, Some(1));
        assert_subr_arity("numberp", 1, Some(1));
        assert_subr_arity("defalias", 2, Some(3));
        assert_subr_arity("provide", 1, Some(2));
        assert_subr_arity("require", 1, Some(3));
        assert_subr_arity("round", 1, Some(2));
        assert_subr_arity("sequencep", 1, Some(1));
        assert_subr_arity("string-equal", 2, Some(2));
        assert_subr_arity("string-lessp", 2, Some(2));
        assert_subr_arity("string-greaterp", 2, Some(2));
        assert_subr_arity("string>", 2, Some(2));
        assert_subr_arity("string-or-null-p", 1, Some(1));
        assert_subr_arity("string-to-char", 1, Some(1));
        assert_subr_arity("string-to-number", 1, Some(2));
        assert_subr_arity("stringp", 1, Some(1));
        assert_subr_arity("substring", 1, Some(3));
        assert_subr_arity("symbolp", 1, Some(1));
        assert_subr_arity("throw", 2, Some(2));
        assert_subr_arity("truncate", 1, Some(2));
        assert_subr_arity("type-of", 1, Some(1));
        assert_subr_arity("upcase", 1, Some(1));
        assert_subr_arity("vectorp", 1, Some(1));
    }

    #[test]
    fn subr_arity_command_timer_primitives_match_oracle() {
        assert_subr_arity("call-interactively", 1, Some(3));
        assert_subr_arity("called-interactively-p", 0, Some(1));
        assert_subr_arity("command-modes", 1, Some(1));
        assert_subr_arity("command-error-default-function", 3, Some(3));
        assert_subr_arity("command-remapping", 1, Some(3));
        assert_subr_arity("commandp", 1, Some(2));
        assert_subr_arity("cancel-timer", 1, Some(1));
        assert_subr_arity("timerp", 1, Some(1));
        assert_subr_arity("timeout-event-p", 1, Some(1));
        assert_subr_arity("run-at-time", 3, None);
        assert_subr_arity("run-with-timer", 3, None);
        assert_subr_arity("run-with-idle-timer", 3, None);
        assert_subr_arity("timer-activate", 1, Some(3));
        assert_subr_arity("sleep-for", 1, Some(2));
        assert_subr_arity("sit-for", 1, Some(2));
        assert_subr_arity("current-cpu-time", 0, Some(0));
        assert_subr_arity("current-idle-time", 0, Some(0));
        assert_subr_arity("current-time", 0, Some(0));
        assert_subr_arity("flush-standard-output", 0, Some(0));
        assert_subr_arity("force-mode-line-update", 0, Some(1));
        assert_subr_arity("force-window-update", 0, Some(1));
        assert_subr_arity("get-internal-run-time", 0, Some(0));
        assert_subr_arity("float-time", 0, Some(1));
    }

    #[test]
    fn subr_arity_command_read_primitives_match_oracle() {
        assert_subr_arity("command-execute", 1, Some(4));
        assert_subr_arity("compare-buffer-substrings", 6, Some(6));
        assert_subr_arity("compare-strings", 6, Some(7));
        assert_subr_arity("comp--compile-ctxt-to-file0", 1, Some(1));
        assert_subr_arity("comp--init-ctxt", 0, Some(0));
        assert_subr_arity("comp--install-trampoline", 2, Some(2));
        assert_subr_arity("comp--late-register-subr", 7, Some(7));
        assert_subr_arity("comp--register-lambda", 7, Some(7));
        assert_subr_arity("comp--register-subr", 7, Some(7));
        assert_subr_arity("comp--release-ctxt", 0, Some(0));
        assert_subr_arity("comp--subr-signature", 1, Some(1));
        assert_subr_arity("comp-el-to-eln-filename", 1, Some(2));
        assert_subr_arity("comp-el-to-eln-rel-filename", 1, Some(1));
        assert_subr_arity("comp-libgccjit-version", 0, Some(0));
        assert_subr_arity("comp-native-compiler-options-effective-p", 0, Some(0));
        assert_subr_arity("comp-native-driver-options-effective-p", 0, Some(0));
        assert_subr_arity("define-fringe-bitmap", 2, Some(5));
        assert_subr_arity("destroy-fringe-bitmap", 1, Some(1));
        assert_subr_arity("display--line-is-continued-p", 0, Some(0));
        assert_subr_arity("display--update-for-mouse-movement", 2, Some(2));
        assert_subr_arity("do-auto-save", 0, Some(2));
        assert_subr_arity("external-debugging-output", 1, Some(1));
        assert_subr_arity("describe-buffer-bindings", 1, Some(3));
        assert_subr_arity("describe-vector", 1, Some(2));
        assert_subr_arity("delete-terminal", 0, Some(2));
        assert_subr_arity("face-attributes-as-vector", 1, Some(1));
        assert_subr_arity("font-at", 1, Some(3));
        assert_subr_arity("font-face-attributes", 1, Some(2));
        assert_subr_arity("font-get-glyphs", 3, Some(4));
        assert_subr_arity("font-get-system-font", 0, Some(0));
        assert_subr_arity("font-get-system-normal-font", 0, Some(0));
        assert_subr_arity("font-has-char-p", 2, Some(3));
        assert_subr_arity("font-info", 1, Some(2));
        assert_subr_arity("font-match-p", 2, Some(2));
        assert_subr_arity("font-shape-gstring", 2, Some(2));
        assert_subr_arity("font-variation-glyphs", 2, Some(2));
        assert_subr_arity("fontset-font", 2, Some(3));
        assert_subr_arity("fontset-info", 1, Some(2));
        assert_subr_arity("fontset-list", 0, Some(0));
        assert_subr_arity("frame--set-was-invisible", 2, Some(2));
        assert_subr_arity("frame-after-make-frame", 2, Some(2));
        assert_subr_arity("frame-ancestor-p", 2, Some(2));
        assert_subr_arity("frame--face-hash-table", 0, Some(1));
        assert_subr_arity("frame-bottom-divider-width", 0, Some(1));
        assert_subr_arity("frame-child-frame-border-width", 0, Some(1));
        assert_subr_arity("frame-focus", 0, Some(1));
        assert_subr_arity("frame-font-cache", 0, Some(1));
        assert_subr_arity("frame-fringe-width", 0, Some(1));
        assert_subr_arity("frame-internal-border-width", 0, Some(1));
        assert_subr_arity("frame-old-selected-window", 0, Some(1));
        assert_subr_arity("frame-or-buffer-changed-p", 0, Some(1));
        assert_subr_arity("frame-parent", 0, Some(1));
        assert_subr_arity("frame-pointer-visible-p", 0, Some(1));
        assert_subr_arity("frame-scale-factor", 0, Some(1));
        assert_subr_arity("frame-scroll-bar-height", 0, Some(1));
        assert_subr_arity("frame-scroll-bar-width", 0, Some(1));
        assert_subr_arity("frame-window-state-change", 0, Some(1));
        assert_subr_arity("frame-right-divider-width", 0, Some(1));
        assert_subr_arity("fringe-bitmaps-at-pos", 0, Some(2));
        assert_subr_arity("gap-position", 0, Some(0));
        assert_subr_arity("gap-size", 0, Some(0));
        assert_subr_arity("gnutls-available-p", 0, Some(0));
        assert_subr_arity("gnutls-asynchronous-parameters", 2, Some(2));
        assert_subr_arity("gnutls-boot", 3, Some(3));
        assert_subr_arity("gnutls-bye", 2, Some(2));
        assert_subr_arity("gnutls-ciphers", 0, Some(0));
        assert_subr_arity("gnutls-deinit", 1, Some(1));
        assert_subr_arity("gnutls-digests", 0, Some(0));
        assert_subr_arity("gnutls-format-certificate", 1, Some(1));
        assert_subr_arity("gnutls-get-initstage", 1, Some(1));
        assert_subr_arity("gnutls-hash-digest", 2, Some(2));
        assert_subr_arity("gnutls-hash-mac", 3, Some(3));
        assert_subr_arity("gnutls-macs", 0, Some(0));
        assert_subr_arity("gnutls-peer-status", 1, Some(1));
        assert_subr_arity("gpm-mouse-start", 0, Some(0));
        assert_subr_arity("gpm-mouse-stop", 0, Some(0));
        assert_subr_arity("sqlite-close", 1, Some(1));
        assert_subr_arity("sqlite-columns", 1, Some(1));
        assert_subr_arity("sqlite-commit", 1, Some(1));
        assert_subr_arity("sqlite-execute", 2, Some(3));
        assert_subr_arity("sqlite-execute-batch", 2, Some(2));
        assert_subr_arity("sqlite-finalize", 1, Some(1));
        assert_subr_arity("sqlite-load-extension", 2, Some(2));
        assert_subr_arity("sqlite-more-p", 1, Some(1));
        assert_subr_arity("sqlite-next", 1, Some(1));
        assert_subr_arity("sqlite-open", 0, Some(1));
        assert_subr_arity("sqlite-pragma", 2, Some(2));
        assert_subr_arity("sqlite-rollback", 1, Some(1));
        assert_subr_arity("sqlite-select", 2, Some(4));
        assert_subr_arity("sqlite-transaction", 1, Some(1));
        assert_subr_arity("sqlite-available-p", 0, Some(0));
        assert_subr_arity("sqlite-version", 0, Some(0));
        assert_subr_arity("sqlitep", 1, Some(1));
        assert_subr_arity("garbage-collect-maybe", 1, Some(1));
        assert_subr_arity("gnutls-error-fatalp", 1, Some(1));
        assert_subr_arity("gnutls-error-string", 1, Some(1));
        assert_subr_arity("gnutls-errorp", 1, Some(1));
        assert_subr_arity("gnutls-peer-status-warning-describe", 1, Some(1));
        assert_subr_arity("gnutls-symmetric-decrypt", 4, Some(5));
        assert_subr_arity("gnutls-symmetric-encrypt", 4, Some(5));
        assert_subr_arity("handle-save-session", 1, Some(1));
        assert_subr_arity("handle-switch-frame", 1, Some(1));
        assert_subr_arity("help--describe-vector", 7, Some(7));
        assert_subr_arity("init-image-library", 1, Some(1));
        assert_subr_arity("inotify-add-watch", 3, Some(3));
        assert_subr_arity("inotify-rm-watch", 1, Some(1));
        assert_subr_arity("inotify-valid-p", 1, Some(1));
        assert_subr_arity("innermost-minibuffer-p", 0, Some(1));
        assert_subr_arity("interactive-form", 1, Some(1));
        assert_subr_arity("local-variable-if-set-p", 1, Some(2));
        assert_subr_arity("lock-buffer", 0, Some(1));
        assert_subr_arity("lock-file", 1, Some(1));
        assert_subr_arity("lossage-size", 0, Some(1));
        assert_subr_arity("unlock-buffer", 0, Some(0));
        assert_subr_arity("unlock-file", 1, Some(1));
        assert_subr_arity("window-at", 2, Some(3));
        assert_subr_arity("window-bottom-divider-width", 0, Some(1));
        assert_subr_arity("window-bump-use-time", 0, Some(1));
        assert_subr_arity("window-combination-limit", 1, Some(1));
        assert_subr_arity("window-left-child", 0, Some(1));
        assert_subr_arity("window-line-height", 0, Some(2));
        assert_subr_arity("window-lines-pixel-dimensions", 0, Some(6));
        assert_subr_arity("window-list-1", 0, Some(3));
        assert_subr_arity("window-new-normal", 0, Some(1));
        assert_subr_arity("window-new-pixel", 0, Some(1));
        assert_subr_arity("window-new-total", 0, Some(1));
        assert_subr_arity("window-next-sibling", 0, Some(1));
        assert_subr_arity("window-normal-size", 0, Some(2));
        assert_subr_arity("window-old-body-pixel-height", 0, Some(1));
        assert_subr_arity("window-old-body-pixel-width", 0, Some(1));
        assert_subr_arity("window-old-pixel-height", 0, Some(1));
        assert_subr_arity("window-old-pixel-width", 0, Some(1));
        assert_subr_arity("window-parent", 0, Some(1));
        assert_subr_arity("window-pixel-left", 0, Some(1));
        assert_subr_arity("window-pixel-top", 0, Some(1));
        assert_subr_arity("window-prev-sibling", 0, Some(1));
        assert_subr_arity("window-resize-apply", 0, Some(2));
        assert_subr_arity("window-resize-apply-total", 0, Some(2));
        assert_subr_arity("window-right-divider-width", 0, Some(1));
        assert_subr_arity("window-scroll-bar-height", 0, Some(1));
        assert_subr_arity("window-scroll-bar-width", 0, Some(1));
        assert_subr_arity("window-tab-line-height", 0, Some(1));
        assert_subr_arity("window-top-child", 0, Some(1));
        assert_subr_arity("treesit-available-p", 0, Some(0));
        assert_subr_arity("treesit-compiled-query-p", 1, Some(1));
        assert_subr_arity("treesit-induce-sparse-tree", 2, Some(4));
        assert_subr_arity("treesit-language-abi-version", 0, Some(1));
        assert_subr_arity("treesit-language-available-p", 1, Some(2));
        assert_subr_arity("treesit-library-abi-version", 0, Some(1));
        assert_subr_arity("treesit-node-check", 2, Some(2));
        assert_subr_arity("treesit-node-child", 2, Some(3));
        assert_subr_arity("treesit-node-child-by-field-name", 2, Some(2));
        assert_subr_arity("treesit-node-child-count", 1, Some(2));
        assert_subr_arity("treesit-node-descendant-for-range", 3, Some(4));
        assert_subr_arity("treesit-node-end", 1, Some(1));
        assert_subr_arity("treesit-node-eq", 2, Some(2));
        assert_subr_arity("treesit-node-field-name-for-child", 2, Some(2));
        assert_subr_arity("treesit-node-first-child-for-pos", 2, Some(3));
        assert_subr_arity("treesit-node-match-p", 2, Some(3));
        assert_subr_arity("treesit-node-next-sibling", 1, Some(2));
        assert_subr_arity("treesit-node-p", 1, Some(1));
        assert_subr_arity("treesit-node-parent", 1, Some(1));
        assert_subr_arity("treesit-node-parser", 1, Some(1));
        assert_subr_arity("treesit-node-prev-sibling", 1, Some(2));
        assert_subr_arity("treesit-node-start", 1, Some(1));
        assert_subr_arity("treesit-node-string", 1, Some(1));
        assert_subr_arity("treesit-node-type", 1, Some(1));
        assert_subr_arity("treesit-parser-add-notifier", 2, Some(2));
        assert_subr_arity("treesit-parser-buffer", 1, Some(1));
        assert_subr_arity("treesit-parser-create", 1, Some(4));
        assert_subr_arity("treesit-parser-delete", 1, Some(1));
        assert_subr_arity("treesit-parser-included-ranges", 1, Some(1));
        assert_subr_arity("treesit-parser-language", 1, Some(1));
        assert_subr_arity("treesit-parser-list", 0, Some(3));
        assert_subr_arity("treesit-parser-notifiers", 1, Some(1));
        assert_subr_arity("treesit-parser-p", 1, Some(1));
        assert_subr_arity("treesit-parser-remove-notifier", 2, Some(2));
        assert_subr_arity("treesit-parser-root-node", 1, Some(1));
        assert_subr_arity("treesit-parser-set-included-ranges", 2, Some(2));
        assert_subr_arity("treesit-parser-tag", 1, Some(1));
        assert_subr_arity("treesit-pattern-expand", 1, Some(1));
        assert_subr_arity("treesit-query-capture", 2, Some(5));
        assert_subr_arity("treesit-query-compile", 2, Some(3));
        assert_subr_arity("treesit-query-expand", 1, Some(1));
        assert_subr_arity("treesit-query-language", 1, Some(1));
        assert_subr_arity("treesit-query-p", 1, Some(1));
        assert_subr_arity("treesit-search-forward", 2, Some(4));
        assert_subr_arity("treesit-search-subtree", 2, Some(5));
        assert_subr_arity("treesit-subtree-stat", 1, Some(1));
        assert_subr_arity("internal--after-save-selected-window", 1, Some(1));
        assert_subr_arity("internal--before-save-selected-window", 0, Some(0));
        assert_subr_arity("internal--define-uninitialized-variable", 1, Some(2));
        assert_subr_arity("internal--labeled-narrow-to-region", 3, Some(3));
        assert_subr_arity("internal--labeled-widen", 1, Some(1));
        assert_subr_arity("internal--obarray-buckets", 1, Some(1));
        assert_subr_arity("internal--set-buffer-modified-tick", 1, Some(2));
        assert_subr_arity("internal--track-mouse", 1, Some(1));
        assert_subr_arity("internal-char-font", 1, Some(2));
        assert_subr_arity("internal-complete-buffer", 3, Some(3));
        assert_subr_arity("internal-describe-syntax-value", 1, Some(1));
        assert_subr_arity("internal-event-symbol-parse-modifiers", 1, Some(1));
        assert_subr_arity("internal-handle-focus-in", 1, Some(1));
        assert_subr_arity("internal-make-var-non-special", 1, Some(1));
        assert_subr_arity("internal-set-lisp-face-attribute-from-resource", 3, Some(4));
        assert_subr_arity("internal-stack-stats", 0, Some(0));
        assert_subr_arity("internal-subr-documentation", 1, Some(1));
        assert_subr_arity("dump-emacs-portable", 1, Some(2));
        assert_subr_arity("dump-emacs-portable--sort-predicate", 2, Some(2));
        assert_subr_arity("dump-emacs-portable--sort-predicate-copied", 2, Some(2));
        assert_subr_arity("malloc-info", 0, Some(0));
        assert_subr_arity("malloc-trim", 0, Some(1));
        assert_subr_arity("marker-last-position", 1, Some(1));
        assert_subr_arity("match-data--translate", 1, Some(1));
        assert_subr_arity("memory-info", 0, Some(0));
        assert_subr_arity("make-frame-invisible", 0, Some(2));
        assert_subr_arity("make-terminal-frame", 1, Some(1));
        assert_subr_arity("menu-bar-menu-at-x-y", 2, Some(3));
        assert_subr_arity("menu-or-popup-active-p", 0, Some(0));
        assert_subr_arity("module-load", 1, Some(1));
        assert_subr_arity("mouse-pixel-position", 0, Some(0));
        assert_subr_arity("mouse-position", 0, Some(0));
        assert_subr_arity("newline-cache-check", 0, Some(1));
        assert_subr_arity("native-comp-available-p", 0, Some(0));
        assert_subr_arity("native-comp-unit-file", 1, Some(1));
        assert_subr_arity("native-comp-unit-set-file", 2, Some(2));
        assert_subr_arity("native-elisp-load", 1, Some(2));
        assert_subr_arity("new-fontset", 2, Some(2));
        assert_subr_arity("object-intervals", 1, Some(1));
        assert_subr_arity("old-selected-frame", 0, Some(0));
        assert_subr_arity("old-selected-window", 0, Some(0));
        assert_subr_arity("open-dribble-file", 1, Some(1));
        assert_subr_arity("open-font", 1, Some(3));
        assert_subr_arity("optimize-char-table", 1, Some(2));
        assert_subr_arity("overlay-lists", 0, Some(0));
        assert_subr_arity("overlay-recenter", 1, Some(1));
        assert_subr_arity("pdumper-stats", 0, Some(0));
        assert_subr_arity("play-sound-internal", 1, Some(1));
        assert_subr_arity("position-symbol", 2, Some(2));
        assert_subr_arity("posn-at-point", 0, Some(2));
        assert_subr_arity("posn-at-x-y", 2, Some(4));
        assert_subr_arity("profiler-cpu-log", 0, Some(0));
        assert_subr_arity("profiler-cpu-running-p", 0, Some(0));
        assert_subr_arity("profiler-cpu-start", 1, Some(1));
        assert_subr_arity("profiler-cpu-stop", 0, Some(0));
        assert_subr_arity("profiler-memory-log", 0, Some(0));
        assert_subr_arity("profiler-memory-running-p", 0, Some(0));
        assert_subr_arity("profiler-memory-start", 0, Some(0));
        assert_subr_arity("profiler-memory-stop", 0, Some(0));
        assert_subr_arity("query-font", 1, Some(1));
        assert_subr_arity("query-fontset", 1, Some(2));
        assert_subr_arity("read-positioning-symbols", 0, Some(1));
        assert_subr_arity("recent-auto-save-p", 0, Some(0));
        assert_subr_arity("record", 1, None);
        assert_subr_arity("recordp", 1, Some(1));
        assert_subr_arity("reconsider-frame-fonts", 1, Some(1));
        assert_subr_arity("redirect-debugging-output", 1, Some(2));
        assert_subr_arity("redirect-frame-focus", 1, Some(2));
        assert_subr_arity("remove-pos-from-symbol", 1, Some(1));
        assert_subr_arity("resize-mini-window-internal", 1, Some(1));
        assert_subr_arity("restore-buffer-modified-p", 1, Some(1));
        assert_subr_arity("set--this-command-keys", 1, Some(1));
        assert_subr_arity("set-buffer-auto-saved", 0, Some(0));
        assert_subr_arity("set-buffer-redisplay", 4, Some(4));
        assert_subr_arity("set-charset-plist", 2, Some(2));
        assert_subr_arity("set-fontset-font", 3, Some(5));
        assert_subr_arity("set-frame-selected-window", 2, Some(3));
        assert_subr_arity("set-frame-window-state-change", 0, Some(2));
        assert_subr_arity("set-fringe-bitmap-face", 1, Some(2));
        assert_subr_arity("set-minibuffer-window", 1, Some(1));
        assert_subr_arity("set-mouse-pixel-position", 3, Some(3));
        assert_subr_arity("set-mouse-position", 3, Some(3));
        assert_subr_arity("set-window-combination-limit", 2, Some(2));
        assert_subr_arity("set-window-new-normal", 1, Some(2));
        assert_subr_arity("set-window-new-pixel", 2, Some(3));
        assert_subr_arity("set-window-new-total", 2, Some(3));
        assert_subr_arity("sort-charsets", 1, Some(1));
        assert_subr_arity("split-char", 1, Some(1));
        assert_subr_arity("string-distance", 2, Some(3));
        assert_subr_arity("subst-char-in-region", 4, Some(5));
        assert_subr_arity("subr-native-comp-unit", 1, Some(1));
        assert_subr_arity("subr-native-lambda-list", 1, Some(1));
        assert_subr_arity("subr-type", 1, Some(1));
        assert_subr_arity("this-single-command-keys", 0, Some(0));
        assert_subr_arity("this-single-command-raw-keys", 0, Some(0));
        assert_subr_arity("thread--blocker", 1, Some(1));
        assert_subr_arity("tool-bar-get-system-style", 0, Some(0));
        assert_subr_arity("tool-bar-pixel-width", 0, Some(1));
        assert_subr_arity("translate-region-internal", 3, Some(3));
        assert_subr_arity("transpose-regions", 4, Some(5));
        assert_subr_arity("tty--output-buffer-size", 0, Some(1));
        assert_subr_arity("tty--set-output-buffer-size", 1, Some(2));
        assert_subr_arity("tty-suppress-bold-inverse-default-colors", 1, Some(1));
        assert_subr_arity("unencodable-char-position", 3, Some(5));
        assert_subr_arity("unicode-property-table-internal", 1, Some(1));
        assert_subr_arity("unify-charset", 1, Some(3));
        assert_subr_arity("unix-sync", 0, Some(0));
        assert_subr_arity("value<", 2, Some(2));
        assert_subr_arity("variable-binding-locus", 1, Some(1));
        assert_subr_arity("byte-code", 3, Some(3));
        assert_subr_arity("decode-coding-region", 3, Some(4));
        assert_subr_arity("defconst-1", 2, Some(3));
        assert_subr_arity("define-coding-system-internal", 13, None);
        assert_subr_arity("defvar-1", 2, Some(3));
        assert_subr_arity("defvaralias", 2, Some(3));
        assert_subr_arity("encode-coding-region", 3, Some(4));
        assert_subr_arity("find-operation-coding-system", 1, None);
        assert_subr_arity("handler-bind-1", 1, None);
        assert_subr_arity("indirect-variable", 1, Some(1));
        assert_subr_arity("insert-and-inherit", 0, None);
        assert_subr_arity("insert-before-markers-and-inherit", 0, None);
        assert_subr_arity("insert-buffer-substring", 1, Some(3));
        assert_subr_arity("iso-charset", 3, Some(3));
        assert_subr_arity("keymap--get-keyelt", 2, Some(2));
        assert_subr_arity("keymap-prompt", 1, Some(1));
        assert_subr_arity("kill-all-local-variables", 0, Some(1));
        assert_subr_arity("kill-emacs", 0, Some(2));
        assert_subr_arity("lower-frame", 0, Some(1));
        assert_subr_arity("lread--substitute-object-in-subtree", 3, Some(3));
        assert_subr_arity("macroexpand", 1, Some(2));
        assert_subr_arity("make-byte-code", 4, None);
        assert_subr_arity("make-char", 1, Some(5));
        assert_subr_arity("make-closure", 1, None);
        assert_subr_arity("make-finalizer", 1, Some(1));
        assert_subr_arity("make-indirect-buffer", 2, Some(4));
        assert_subr_arity("make-interpreted-closure", 3, Some(5));
        assert_subr_arity("make-record", 3, Some(3));
        assert_subr_arity("make-temp-file-internal", 4, Some(4));
        assert_subr_arity("map-charset-chars", 2, Some(5));
        assert_subr_arity("map-keymap", 2, Some(3));
        assert_subr_arity("map-keymap-internal", 2, Some(2));
        assert_subr_arity("mapbacktrace", 1, Some(2));
        assert_subr_arity("minibuffer-innermost-command-loop-p", 0, Some(1));
        assert_subr_arity("minibuffer-prompt-end", 0, Some(0));
        assert_subr_arity("next-frame", 0, Some(2));
        assert_subr_arity("ntake", 2, Some(2));
        assert_subr_arity("obarray-clear", 1, Some(1));
        assert_subr_arity("obarray-make", 0, Some(1));
        assert_subr_arity("previous-frame", 0, Some(2));
        assert_subr_arity("put-unicode-property-internal", 3, Some(3));
        assert_subr_arity("raise-frame", 0, Some(1));
        assert_subr_arity("re--describe-compiled", 1, Some(2));
        assert_subr_arity("redisplay", 0, Some(1));
        assert_subr_arity("rename-buffer", 1, Some(2));
        assert_subr_arity("replace-buffer-contents", 1, Some(3));
        assert_subr_arity("set-buffer-major-mode", 1, Some(1));
        assert_subr_arity("set-buffer-multibyte", 1, Some(1));
        assert_subr_arity("setplist", 2, Some(2));
        assert_subr_arity("split-window-internal", 4, Some(4));
        assert_subr_arity("suspend-emacs", 0, Some(1));
        assert_subr_arity("vertical-motion", 1, Some(3));
        assert_subr_arity("x-begin-drag", 1, Some(6));
        assert_subr_arity("x-create-frame", 1, Some(1));
        assert_subr_arity("x-double-buffered-p", 0, Some(1));
        assert_subr_arity("x-menu-bar-open-internal", 0, Some(1));
        assert_subr_arity("xw-color-defined-p", 1, Some(2));
        assert_subr_arity("xw-color-values", 1, Some(2));
        assert_subr_arity("xw-display-color-p", 0, Some(1));
        assert_subr_arity("get-unicode-property-internal", 2, Some(2));
        assert_subr_arity("get-variable-watchers", 1, Some(1));
        assert_subr_arity("fillarray", 2, Some(2));
        assert_subr_arity("define-hash-table-test", 3, Some(3));
        assert_subr_arity("find-coding-systems-region-internal", 2, Some(3));
        assert_subr_arity("completing-read", 2, Some(8));
        assert_subr_arity("try-completion", 2, Some(3));
        assert_subr_arity("all-completions", 2, Some(4));
        assert_subr_arity("test-completion", 2, Some(3));
        assert_subr_arity("dbus--init-bus", 1, Some(2));
        assert_subr_arity("dbus-get-unique-name", 1, Some(1));
        assert_subr_arity("dbus-message-internal", 4, None);
    }

    #[test]
    fn subr_arity_read_core_primitives_match_oracle() {
        assert_subr_arity("read", 0, Some(1));
        assert_subr_arity("read-char", 0, Some(3));
        assert_subr_arity("read-char-exclusive", 0, Some(3));
        assert_subr_arity("read-event", 0, Some(3));
        assert_subr_arity("read-key", 0, Some(2));
        assert_subr_arity("read-number", 1, Some(3));
        assert_subr_arity("read-string", 1, Some(5));
        assert_subr_arity("read-variable", 1, Some(2));
        assert_subr_arity("read-from-string", 1, Some(3));
        assert_subr_arity("read-command", 1, Some(2));
    }

    #[test]
    fn subr_arity_input_mode_primitives_match_oracle() {
        assert_subr_arity("current-input-mode", 0, Some(0));
        assert_subr_arity("set-input-mode", 3, Some(4));
        assert_subr_arity("set-input-interrupt-mode", 1, Some(1));
        assert_subr_arity("set-input-meta-mode", 1, Some(2));
        assert_subr_arity("set-output-flow-control", 1, Some(2));
        assert_subr_arity("set-quit-char", 1, Some(1));
        assert_subr_arity("input-pending-p", 0, Some(1));
        assert_subr_arity("discard-input", 0, Some(0));
        assert_subr_arity("waiting-for-user-input-p", 0, Some(0));
    }

    #[test]
    fn subr_arity_kmacro_command_primitives_match_oracle() {
        assert_subr_arity("start-kbd-macro", 1, Some(2));
        assert_subr_arity("cancel-kbd-macro-events", 0, Some(0));
        assert_subr_arity("end-kbd-macro", 0, Some(2));
        assert_subr_arity("call-last-kbd-macro", 0, Some(2));
        assert_subr_arity("execute-kbd-macro", 1, Some(3));
        assert_subr_arity("execute-extended-command", 1, Some(3));
        assert_subr_arity("describe-key-briefly", 0, Some(3));
    }

    #[test]
    fn subr_arity_keymap_keyboard_primitives_match_oracle() {
        assert_subr_arity("kbd", 1, Some(1));
        assert_subr_arity("key-binding", 1, Some(4));
        assert_subr_arity("global-key-binding", 1, Some(2));
        assert_subr_arity("local-key-binding", 1, Some(2));
        assert_subr_arity("lookup-key", 2, Some(3));
        assert_subr_arity("key-description", 1, Some(2));
        assert_subr_arity("key-valid-p", 1, Some(1));
        assert_subr_arity("global-set-key", 2, Some(2));
        assert_subr_arity("local-set-key", 2, Some(2));
        assert_subr_arity("keymap-parent", 1, Some(1));
        assert_subr_arity("keymapp", 1, Some(1));
        assert_subr_arity("accessible-keymaps", 1, Some(2));
        assert_subr_arity("keyboard-coding-system", 0, Some(1));
        assert_subr_arity("keyboard-quit", 0, Some(0));
        assert_subr_arity("listify-key-sequence", 1, Some(1));
        assert_subr_arity("make-keymap", 0, Some(1));
        assert_subr_arity("make-sparse-keymap", 0, Some(1));
    }

    #[test]
    fn subr_arity_delete_primitives_match_oracle() {
        assert_subr_arity("delete-char", 1, Some(2));
        assert_subr_arity("delete-all-overlays", 0, Some(1));
        assert_subr_arity("delete-and-extract-region", 2, Some(2));
        assert_subr_arity("delete-field", 0, Some(1));
        assert_subr_arity("delete-region", 2, Some(2));
        assert_subr_arity("delete-horizontal-space", 0, Some(1));
        assert_subr_arity("delete-indentation", 0, Some(3));
        assert_subr_arity("delete-overlay", 1, Some(1));
        assert_subr_arity("delete-window", 0, Some(1));
        assert_subr_arity("delete-window-internal", 1, Some(1));
    }

    #[test]
    fn subr_arity_filesystem_path_primitives_match_oracle() {
        assert_subr_arity("access-file", 2, Some(2));
        assert_subr_arity("delete-directory", 1, Some(3));
        assert_subr_arity("delete-directory-internal", 1, Some(1));
        assert_subr_arity("delete-file", 1, Some(2));
        assert_subr_arity("delete-file-internal", 1, Some(1));
        assert_subr_arity("directory-file-name", 1, Some(1));
        assert_subr_arity("directory-empty-p", 1, Some(1));
        assert_subr_arity("directory-files", 1, Some(5));
        assert_subr_arity("directory-files-and-attributes", 1, Some(6));
        assert_subr_arity("directory-name-p", 1, Some(1));
        assert_subr_arity("expand-file-name", 1, Some(2));
    }

    #[test]
    fn subr_arity_filesystem_create_primitives_match_oracle() {
        assert_subr_arity("make-directory", 1, Some(2));
        assert_subr_arity("make-directory-internal", 1, Some(1));
        assert_subr_arity("make-temp-file", 1, Some(4));
        assert_subr_arity("make-temp-name", 1, Some(1));
        assert_subr_arity("make-nearby-temp-file", 1, Some(3));
        assert_subr_arity("make-symbolic-link", 2, Some(3));
        assert_subr_arity("rename-file", 2, Some(3));
        assert_subr_arity("add-name-to-file", 2, Some(3));
    }

    #[test]
    fn subr_arity_file_load_primitives_match_oracle() {
        assert_subr_arity("find-buffer", 2, Some(2));
        assert_subr_arity("find-file", 1, Some(2));
        assert_subr_arity("find-file-name-handler", 2, Some(2));
        assert_subr_arity("find-file-noselect", 1, Some(4));
        assert_subr_arity("insert-file-contents", 1, Some(5));
        assert_subr_arity("load", 1, Some(5));
        assert_subr_arity("load-file", 1, Some(1));
        assert_subr_arity("locate-file", 2, Some(4));
        assert_subr_arity("locate-file-internal", 2, Some(4));
    }

    #[test]
    fn subr_arity_file_stat_predicate_primitives_match_oracle() {
        assert_subr_arity("file-acl", 1, Some(1));
        assert_subr_arity("file-attributes", 1, Some(2));
        assert_subr_arity("file-accessible-directory-p", 1, Some(1));
        assert_subr_arity("file-directory-p", 1, Some(1));
        assert_subr_arity("file-equal-p", 2, Some(2));
        assert_subr_arity("file-executable-p", 1, Some(1));
        assert_subr_arity("file-exists-p", 1, Some(1));
        assert_subr_arity("file-in-directory-p", 2, Some(2));
        assert_subr_arity("file-locked-p", 1, Some(1));
        assert_subr_arity("file-modes", 1, Some(2));
        assert_subr_arity("file-newer-than-file-p", 2, Some(2));
        assert_subr_arity("file-readable-p", 1, Some(1));
        assert_subr_arity("file-regular-p", 1, Some(1));
        assert_subr_arity("file-selinux-context", 1, Some(1));
        assert_subr_arity("file-system-info", 1, Some(1));
        assert_subr_arity("file-symlink-p", 1, Some(1));
        assert_subr_arity("file-writable-p", 1, Some(1));
    }

    #[test]
    fn subr_arity_file_name_primitives_match_oracle() {
        assert_subr_arity("convert-standard-filename", 1, Some(1));
        assert_subr_arity("file-name-absolute-p", 1, Some(1));
        assert_subr_arity("file-name-base", 0, Some(1));
        assert_subr_arity("backup-file-name-p", 1, Some(1));
        assert_subr_arity("auto-save-file-name-p", 1, Some(1));
        assert_subr_arity("abbreviate-file-name", 1, Some(1));
        assert_subr_arity("file-name-all-completions", 2, Some(2));
        assert_subr_arity("file-name-as-directory", 1, Some(1));
        assert_subr_arity("file-name-case-insensitive-p", 1, Some(1));
        assert_subr_arity("file-name-completion", 2, Some(3));
        assert_subr_arity("file-name-concat", 1, None);
        assert_subr_arity("file-name-directory", 1, Some(1));
        assert_subr_arity("file-name-extension", 1, Some(2));
        assert_subr_arity("file-name-parent-directory", 1, Some(1));
        assert_subr_arity("file-name-split", 1, Some(1));
        assert_subr_arity("file-name-sans-versions", 1, Some(2));
        assert_subr_arity("file-name-nondirectory", 1, Some(1));
        assert_subr_arity("file-local-name", 1, Some(1));
        assert_subr_arity("file-nlinks", 1, Some(1));
        assert_subr_arity("file-remote-p", 1, Some(3));
        assert_subr_arity("file-name-sans-extension", 1, Some(1));
        assert_subr_arity("file-name-with-extension", 2, Some(2));
        assert_subr_arity("file-truename", 1, Some(3));
        assert_subr_arity("get-truename-buffer", 1, Some(1));
        assert_subr_arity("unhandled-file-name-directory", 1, Some(1));
    }

    #[test]
    fn subr_arity_event_error_misc_primitives_match_oracle() {
        assert_subr_arity("event-basic-type", 1, Some(1));
        assert_subr_arity("event-convert-list", 1, Some(1));
        assert_subr_arity("event-modifiers", 1, Some(1));
        assert_subr_arity("eventp", 1, Some(1));
        assert_subr_arity("error-message-string", 1, Some(1));
        assert_subr_arity("user-error", 1, None);
        assert_subr_arity("copysign", 2, Some(2));
        assert_subr_arity("equal-including-properties", 2, Some(2));
        assert_subr_arity("function-equal", 2, Some(2));
        assert_subr_arity("emacs-pid", 0, Some(0));
    }

    #[test]
    fn subr_arity_eval_primitives_match_oracle() {
        assert_subr_arity("eval", 1, Some(2));
        assert_subr_arity("eval-buffer", 0, Some(5));
        assert_subr_arity("eval-expression", 1, Some(4));
        assert_subr_arity("eval-region", 2, Some(4));
    }

    #[test]
    fn subr_arity_define_defaults_primitives_match_oracle() {
        assert_subr_arity("default-file-modes", 0, Some(0));
        assert_subr_arity("define-category", 2, Some(3));
        assert_subr_arity("define-coding-system-alias", 2, Some(2));
        assert_subr_arity("define-key", 3, Some(4));
    }

    #[test]
    fn subr_arity_category_ccl_primitives_match_oracle() {
        assert_subr_arity("category-table", 0, Some(0));
        assert_subr_arity("clear-charset-maps", 0, Some(0));
        assert_subr_arity("case-table-p", 1, Some(1));
        assert_subr_arity("category-set-mnemonics", 1, Some(1));
        assert_subr_arity("category-table-p", 1, Some(1));
        assert_subr_arity("ccl-program-p", 1, Some(1));
        assert_subr_arity("check-coding-system", 1, Some(1));
        assert_subr_arity("clear-abbrev-table", 1, Some(1));
        assert_subr_arity("category-docstring", 1, Some(2));
        assert_subr_arity("ccl-execute", 2, Some(2));
        assert_subr_arity("ccl-execute-on-string", 3, Some(5));
    }

    #[test]
    fn subr_arity_coding_system_primitives_match_oracle() {
        assert_subr_arity("check-coding-systems-region", 3, Some(3));
        assert_subr_arity("coding-system-aliases", 1, Some(1));
        assert_subr_arity("coding-system-base", 1, Some(1));
        assert_subr_arity("coding-system-change-eol-conversion", 2, Some(2));
        assert_subr_arity("coding-system-change-text-conversion", 2, Some(2));
        assert_subr_arity("coding-system-eol-type", 1, Some(1));
        assert_subr_arity("coding-system-get", 2, Some(2));
        assert_subr_arity("coding-system-list", 0, Some(1));
        assert_subr_arity("coding-system-p", 1, Some(1));
        assert_subr_arity("coding-system-plist", 1, Some(1));
        assert_subr_arity("coding-system-priority-list", 0, Some(1));
        assert_subr_arity("coding-system-put", 3, Some(3));
        assert_subr_arity("coding-system-type", 1, Some(1));
    }

    #[test]
    fn subr_arity_color_primitives_match_oracle() {
        assert_subr_arity("defined-colors", 0, Some(1));
        assert_subr_arity("color-distance", 2, Some(4));
        assert_subr_arity("color-defined-p", 1, Some(2));
        assert_subr_arity("color-gray-p", 1, Some(2));
        assert_subr_arity("color-supported-p", 1, Some(3));
        assert_subr_arity("color-values", 1, Some(2));
        assert_subr_arity("color-values-from-color-spec", 1, Some(1));
    }

    #[test]
    fn subr_arity_copy_cons_primitives_match_oracle() {
        assert_subr_arity("cons", 2, Some(2));
        assert_subr_arity("copy-alist", 1, Some(1));
        assert_subr_arity("copy-category-table", 0, Some(1));
        assert_subr_arity("copy-file", 2, Some(6));
        assert_subr_arity("copy-hash-table", 1, Some(1));
        assert_subr_arity("copy-keymap", 1, Some(1));
        assert_subr_arity("copy-marker", 0, Some(2));
        assert_subr_arity("copy-region-as-kill", 2, Some(3));
        assert_subr_arity("copy-sequence", 1, Some(1));
        assert_subr_arity("purecopy", 1, Some(1));
        assert_subr_arity("copy-syntax-table", 0, Some(1));
        assert_subr_arity("copy-to-register", 3, Some(5));
    }

    #[test]
    fn subr_arity_current_state_primitives_match_oracle() {
        assert_subr_arity("bitmap-spec-p", 1, Some(1));
        assert_subr_arity("byte-to-string", 1, Some(1));
        assert_subr_arity("byteorder", 0, Some(0));
        assert_subr_arity("clear-buffer-auto-save-failure", 0, Some(0));
        assert_subr_arity("cl-type-of", 1, Some(1));
        assert_subr_arity("bidi-find-overridden-directionality", 3, Some(4));
        assert_subr_arity("bidi-resolved-levels", 0, Some(1));
        assert_subr_arity("current-active-maps", 0, Some(2));
        assert_subr_arity("current-bidi-paragraph-direction", 0, Some(1));
        assert_subr_arity("current-case-table", 0, Some(0));
        assert_subr_arity("current-column", 0, Some(0));
        assert_subr_arity("current-global-map", 0, Some(0));
        assert_subr_arity("current-indentation", 0, Some(0));
        assert_subr_arity("current-kill", 1, Some(2));
        assert_subr_arity("current-local-map", 0, Some(0));
        assert_subr_arity("current-message", 0, Some(0));
        assert_subr_arity("current-minor-mode-maps", 0, Some(0));
        assert_subr_arity("current-time-string", 0, Some(2));
        assert_subr_arity("current-time-zone", 0, Some(2));
        assert_subr_arity("current-window-configuration", 0, Some(1));
        assert_subr_arity("daemon-initialized", 0, Some(0));
        assert_subr_arity("daemonp", 0, Some(0));
        assert_subr_arity("invocation-directory", 0, Some(0));
        assert_subr_arity("invocation-name", 0, Some(0));
        assert_subr_arity("system-name", 0, Some(0));
        assert_subr_arity("emacs-version", 0, Some(1));
    }

    #[test]
    fn subr_arity_composition_primitives_match_oracle() {
        assert_subr_arity("activate-mark", 0, Some(1));
        assert_subr_arity("auto-composition-mode", 0, Some(1));
        assert_subr_arity("clear-composition-cache", 0, Some(0));
        assert_subr_arity("compose-region-internal", 2, Some(4));
        assert_subr_arity("compose-string-internal", 3, Some(5));
        assert_subr_arity("composition-get-gstring", 4, Some(4));
        assert_subr_arity("composition-sort-rules", 1, Some(1));
        assert_subr_arity("deactivate-mark", 0, Some(1));
    }

    #[test]
    fn subr_arity_predicate_core_primitives_match_oracle() {
        assert_subr_arity("charsetp", 1, Some(1));
        assert_subr_arity("closurep", 1, Some(1));
        assert_subr_arity("compiled-function-p", 1, Some(1));
        assert_subr_arity("custom-variable-p", 1, Some(1));
        assert_subr_arity("decode-char", 2, Some(2));
        assert_subr_arity("default-boundp", 1, Some(1));
        assert_subr_arity("default-toplevel-value", 1, Some(1));
        assert_subr_arity("default-value", 1, Some(1));
        assert_subr_arity("integer-or-marker-p", 1, Some(1));
        assert_subr_arity("module-function-p", 1, Some(1));
        assert_subr_arity("number-or-marker-p", 1, Some(1));
        assert_subr_arity("obarrayp", 1, Some(1));
        assert_subr_arity("special-variable-p", 1, Some(1));
        assert_subr_arity("symbol-with-pos-p", 1, Some(1));
        assert_subr_arity("symbol-with-pos-pos", 1, Some(1));
        assert_subr_arity("user-ptrp", 1, Some(1));
        assert_subr_arity("vector-or-char-table-p", 1, Some(1));
        assert_subr_arity("featurep", 1, Some(2));
    }

    #[test]
    fn subr_arity_abbrev_primitives_match_oracle() {
        assert_subr_arity("abbrev-mode", 0, Some(1));
        assert_subr_arity("abbrev-expansion", 1, Some(2));
        assert_subr_arity("abbrev-table-p", 1, Some(1));
        assert_subr_arity("define-abbrev", 3, None);
        assert_subr_arity("define-abbrev-table", 2, None);
        assert_subr_arity("expand-abbrev", 0, Some(0));
    }

    #[test]
    fn subr_arity_cxr_family_match_oracle() {
        assert_subr_arity("car", 1, Some(1));
        assert_subr_arity("cdr", 1, Some(1));
        assert_subr_arity("caar", 1, Some(1));
        assert_subr_arity("cadr", 1, Some(1));
        assert_subr_arity("cdar", 1, Some(1));
        assert_subr_arity("cddr", 1, Some(1));
        assert_subr_arity("caaar", 1, Some(1));
        assert_subr_arity("caadr", 1, Some(1));
        assert_subr_arity("cadar", 1, Some(1));
        assert_subr_arity("caddr", 1, Some(1));
        assert_subr_arity("cdaar", 1, Some(1));
        assert_subr_arity("cdadr", 1, Some(1));
        assert_subr_arity("cddar", 1, Some(1));
        assert_subr_arity("cdddr", 1, Some(1));
        assert_subr_arity("caaaar", 1, Some(1));
        assert_subr_arity("caaadr", 1, Some(1));
        assert_subr_arity("caadar", 1, Some(1));
        assert_subr_arity("caaddr", 1, Some(1));
        assert_subr_arity("cadaar", 1, Some(1));
        assert_subr_arity("cadadr", 1, Some(1));
        assert_subr_arity("caddar", 1, Some(1));
        assert_subr_arity("cadddr", 1, Some(1));
        assert_subr_arity("cdaaar", 1, Some(1));
        assert_subr_arity("cdaadr", 1, Some(1));
        assert_subr_arity("cdadar", 1, Some(1));
        assert_subr_arity("cdaddr", 1, Some(1));
        assert_subr_arity("cddaar", 1, Some(1));
        assert_subr_arity("cddadr", 1, Some(1));
        assert_subr_arity("cdddar", 1, Some(1));
        assert_subr_arity("cddddr", 1, Some(1));
    }

    #[test]
    fn subr_arity_symbol_state_primitives_match_oracle() {
        assert_subr_arity("fboundp", 1, Some(1));
        assert_subr_arity("func-arity", 1, Some(1));
        assert_subr_arity("native-comp-function-p", 1, Some(1));
        assert_subr_arity("fset", 2, Some(2));
        assert_subr_arity("fmakunbound", 1, Some(1));
        assert_subr_arity("makunbound", 1, Some(1));
        assert_subr_arity("set", 2, Some(2));
        assert_subr_arity("get", 2, Some(2));
        assert_subr_arity("put", 3, Some(3));
        assert_subr_arity("symbol-function", 1, Some(1));
        assert_subr_arity("symbol-value", 1, Some(1));
    }

    #[test]
    fn subr_arity_symbol_obarray_primitives_match_oracle() {
        assert_subr_arity("intern", 1, Some(2));
        assert_subr_arity("intern-soft", 1, Some(2));
        assert_subr_arity("make-symbol", 1, Some(1));
        assert_subr_arity("symbol-name", 1, Some(1));
        assert_subr_arity("symbol-plist", 1, Some(1));
        assert_subr_arity("unintern", 1, Some(2));
        assert_subr_arity("indirect-function", 1, Some(2));
        assert_subr_arity("symbol-file", 1, Some(3));
    }

    #[test]
    fn subr_arity_line_position_primitives_match_oracle() {
        assert_subr_arity("line-beginning-position", 0, Some(1));
        assert_subr_arity("line-end-position", 0, Some(1));
        assert_subr_arity("pos-bol", 0, Some(1));
        assert_subr_arity("pos-eol", 0, Some(1));
        assert_subr_arity("line-number-at-pos", 0, Some(2));
        assert_subr_arity("line-number-display-width", 0, Some(1));
        assert_subr_arity("line-pixel-height", 0, Some(0));
    }

    #[test]
    fn subr_arity_search_match_primitives_match_oracle() {
        assert_subr_arity("looking-at", 1, Some(2));
        assert_subr_arity("posix-looking-at", 1, Some(2));
        assert_subr_arity("match-beginning", 1, Some(1));
        assert_subr_arity("match-end", 1, Some(1));
        assert_subr_arity("match-data", 0, Some(3));
        assert_subr_arity("match-string", 1, Some(2));
        assert_subr_arity("replace-match", 1, Some(5));
        assert_subr_arity("replace-regexp-in-string", 3, Some(7));
        assert_subr_arity("string-match", 2, Some(4));
        assert_subr_arity("posix-string-match", 2, Some(4));
        assert_subr_arity("search-forward", 1, Some(4));
        assert_subr_arity("search-backward", 1, Some(4));
        assert_subr_arity("search-forward-regexp", 1, Some(4));
        assert_subr_arity("search-backward-regexp", 1, Some(4));
        assert_subr_arity("re-search-forward", 1, Some(4));
        assert_subr_arity("re-search-backward", 1, Some(4));
        assert_subr_arity("posix-search-forward", 1, Some(4));
        assert_subr_arity("posix-search-backward", 1, Some(4));
        assert_subr_arity("word-search-forward", 1, Some(4));
        assert_subr_arity("word-search-backward", 1, Some(4));
    }

    #[test]
    fn subr_arity_edit_state_helper_primitives_match_oracle() {
        assert_subr_arity("insert-byte", 2, Some(3));
        assert_subr_arity("insert-char", 1, Some(3));
        assert_subr_arity("newline", 0, Some(2));
        assert_subr_arity("next-line", 0, Some(2));
        assert_subr_arity("newline-and-indent", 0, Some(1));
        assert_subr_arity("open-line", 1, Some(1));
        assert_subr_arity("other-window", 1, Some(3));
        assert_subr_arity("other-window-for-scrolling", 0, Some(0));
        assert_subr_arity("goto-line", 1, Some(3));
        assert_subr_arity("flush-lines", 1, Some(4));
        assert_subr_arity("keep-lines", 1, Some(4));
        assert_subr_arity("how-many", 1, Some(4));
        assert_subr_arity("local-variable-p", 1, Some(2));
        assert_subr_arity("buffer-local-boundp", 2, Some(2));
        assert_subr_arity("locale-info", 1, Some(1));
        assert_subr_arity("max-char", 0, Some(1));
        assert_subr_arity("memory-use-counts", 0, Some(0));
        assert_subr_arity("make-marker", 0, Some(0));
        assert_subr_arity("make-local-variable", 1, Some(1));
        assert_subr_arity("make-variable-buffer-local", 1, Some(1));
        assert_subr_arity("make-syntax-table", 0, Some(1));
        assert_subr_arity("mapatoms", 1, Some(2));
        assert_subr_arity("map-char-table", 2, Some(2));
    }

    #[test]
    fn subr_arity_read_region_helper_primitives_match_oracle() {
        assert_subr_arity("read-buffer", 1, Some(4));
        assert_subr_arity("read-coding-system", 1, Some(2));
        assert_subr_arity("read-directory-name", 1, Some(5));
        assert_subr_arity("read-file-name", 1, Some(6));
        assert_subr_arity("read-from-minibuffer", 1, Some(7));
        assert_subr_arity("read-key-sequence", 1, Some(6));
        assert_subr_arity("read-key-sequence-vector", 1, Some(6));
        assert_subr_arity("read-non-nil-coding-system", 1, Some(1));
        assert_subr_arity("recenter", 0, Some(2));
        assert_subr_arity("recenter-top-bottom", 0, Some(1));
        assert_subr_arity("recursion-depth", 0, Some(0));
        assert_subr_arity("region-beginning", 0, Some(0));
        assert_subr_arity("region-end", 0, Some(0));
        assert_subr_arity("regexp-quote", 1, Some(1));
    }

    #[test]
    fn subr_arity_print_replace_edit_primitives_match_oracle() {
        assert_subr_arity("primitive-undo", 2, Some(2));
        assert_subr_arity("prin1", 1, Some(3));
        assert_subr_arity("prin1-to-string", 1, Some(3));
        assert_subr_arity("princ", 1, Some(2));
        assert_subr_arity("print", 1, Some(2));
        assert_subr_arity("terpri", 0, Some(2));
        assert_subr_arity("write-char", 1, Some(2));
        assert_subr_arity("propertize", 1, None);
        assert_subr_arity("put-image", 2, Some(4));
        assert_subr_arity("query-replace", 2, Some(7));
        assert_subr_arity("query-replace-regexp", 2, Some(7));
        assert_subr_arity("quoted-insert", 1, Some(1));
    }

    #[test]
    fn subr_arity_window_navigation_helpers_match_oracle() {
        assert_subr_arity("delete-frame", 0, Some(2));
        assert_subr_arity("delete-other-windows", 0, Some(2));
        assert_subr_arity("delete-other-windows-internal", 0, Some(2));
        assert_subr_arity("one-window-p", 0, Some(2));
        assert_subr_arity("next-window", 0, Some(3));
        assert_subr_arity("previous-window", 0, Some(3));
        assert_subr_arity("pop-to-buffer", 1, Some(3));
        assert_subr_arity("coordinates-in-window-p", 2, Some(2));
        assert_subr_arity("pos-visible-in-window-p", 0, Some(3));
        assert_subr_arity("move-to-window-line", 1, Some(1));
        assert_subr_arity("move-point-visually", 1, Some(1));
        assert_subr_arity("modify-frame-parameters", 2, Some(2));
        assert_subr_arity("iconify-frame", 0, Some(1));
        assert_subr_arity("make-frame", 0, Some(1));
        assert_subr_arity("make-frame-visible", 0, Some(1));
    }

    #[test]
    fn subr_arity_face_font_helper_primitives_match_oracle() {
        assert_subr_arity("close-font", 1, Some(2));
        assert_subr_arity("face-attribute-relative-p", 2, Some(2));
        assert_subr_arity("face-font", 1, Some(3));
        assert_subr_arity("face-id", 1, Some(2));
        assert_subr_arity("face-list", 0, Some(0));
        assert_subr_arity("font-get", 2, Some(2));
        assert_subr_arity("font-put", 3, Some(3));
        assert_subr_arity("font-xlfd-name", 1, Some(3));
        assert_subr_arity("fontp", 1, Some(2));
        assert_subr_arity("internal-copy-lisp-face", 4, Some(4));
        assert_subr_arity("internal-face-x-get-resource", 2, Some(3));
        assert_subr_arity("internal-get-lisp-face-attribute", 2, Some(3));
        assert_subr_arity("internal-make-lisp-face", 1, Some(2));
        assert_subr_arity("internal-lisp-face-attribute-values", 1, Some(1));
        assert_subr_arity("internal-lisp-face-empty-p", 1, Some(2));
        assert_subr_arity("internal-lisp-face-equal-p", 2, Some(3));
        assert_subr_arity("internal-lisp-face-p", 1, Some(2));
        assert_subr_arity("internal-merge-in-global-face", 2, Some(2));
        assert_subr_arity("internal-set-alternative-font-family-alist", 1, Some(1));
        assert_subr_arity("internal-set-alternative-font-registry-alist", 1, Some(1));
        assert_subr_arity("internal-set-font-selection-order", 1, Some(1));
        assert_subr_arity("internal-set-lisp-face-attribute", 3, Some(4));
    }

    #[test]
    fn subr_arity_syntax_category_plist_helpers_match_oracle() {
        assert_subr_arity("category-set-mnemonics", 1, Some(1));
        assert_subr_arity("file-attributes-lessp", 2, Some(2));
        assert_subr_arity("flatten-tree", 1, Some(1));
        assert_subr_arity("forward-comment", 1, Some(1));
        assert_subr_arity("forward-sexp", 0, Some(2));
        assert_subr_arity("get-unused-category", 0, Some(1));
        assert_subr_arity("make-category-set", 1, Some(1));
        assert_subr_arity("make-category-table", 0, Some(0));
        assert_subr_arity("make-char-table", 1, Some(2));
        assert_subr_arity("modify-category-entry", 2, Some(4));
        assert_subr_arity("modify-syntax-entry", 2, Some(3));
        assert_subr_arity("parse-partial-sexp", 2, Some(6));
        assert_subr_arity("plist-get", 2, Some(3));
        assert_subr_arity("plist-member", 2, Some(3));
        assert_subr_arity("plist-put", 3, Some(4));
        assert_subr_arity("natnump", 1, Some(1));
        assert_subr_arity("preceding-char", 0, Some(0));
        assert_subr_arity("previous-line", 0, Some(2));
    }

    #[test]
    fn subr_arity_set_scan_helpers_match_oracle() {
        assert_subr_arity("set-buffer", 1, Some(1));
        assert_subr_arity("set-buffer-modified-p", 1, Some(1));
        assert_subr_arity("set-case-table", 1, Some(1));
        assert_subr_arity("set-category-table", 1, Some(1));
        assert_subr_arity("set-char-table-extra-slot", 3, Some(3));
        assert_subr_arity("set-char-table-parent", 2, Some(2));
        assert_subr_arity("set-char-table-range", 3, Some(3));
        assert_subr_arity("set-default", 2, Some(2));
        assert_subr_arity("set-default-file-modes", 1, Some(1));
        assert_subr_arity("set-default-toplevel-value", 2, Some(2));
        assert_subr_arity("set-file-acl", 2, Some(2));
        assert_subr_arity("set-file-modes", 2, Some(3));
        assert_subr_arity("set-file-selinux-context", 2, Some(2));
        assert_subr_arity("set-file-times", 1, Some(3));
        assert_subr_arity("set-keyboard-coding-system", 1, Some(2));
        assert_subr_arity("set-keyboard-coding-system-internal", 1, Some(2));
        assert_subr_arity("set-keymap-parent", 2, Some(2));
        assert_subr_arity("set-match-data", 1, Some(2));
        assert_subr_arity("set-safe-terminal-coding-system-internal", 1, Some(1));
        assert_subr_arity("set-standard-case-table", 1, Some(1));
        assert_subr_arity("set-syntax-table", 1, Some(1));
        assert_subr_arity("set-terminal-coding-system", 1, Some(3));
        assert_subr_arity("set-terminal-coding-system-internal", 1, Some(2));
        assert_subr_arity("set-text-conversion-style", 1, Some(2));
        assert_subr_arity("set-time-zone-rule", 1, Some(1));
        assert_subr_arity("set-visited-file-modtime", 0, Some(1));
        assert_subr_arity("set-window-dedicated-p", 2, Some(2));
        assert_subr_arity("setcar", 2, Some(2));
        assert_subr_arity("setcdr", 2, Some(2));
        assert_subr_arity("verify-visited-file-modtime", 0, Some(1));
        assert_subr_arity("visited-file-modtime", 0, Some(0));
        assert_subr_arity("scan-lists", 3, Some(3));
        assert_subr_arity("scan-sexps", 2, Some(2));
    }

    #[test]
    fn subr_arity_string_syntax_helpers_match_oracle() {
        assert_subr_arity("string-as-multibyte", 1, Some(1));
        assert_subr_arity("string-as-unibyte", 1, Some(1));
        assert_subr_arity("string-collate-equalp", 2, Some(4));
        assert_subr_arity("string-collate-lessp", 2, Some(4));
        assert_subr_arity("string-lines", 1, Some(3));
        assert_subr_arity("string-make-multibyte", 1, Some(1));
        assert_subr_arity("string-make-unibyte", 1, Some(1));
        assert_subr_arity("string-replace", 3, Some(3));
        assert_subr_arity("string-search", 2, Some(3));
        assert_subr_arity("string-to-multibyte", 1, Some(1));
        assert_subr_arity("string-to-syntax", 1, Some(1));
        assert_subr_arity("string-to-unibyte", 1, Some(1));
        assert_subr_arity("string-version-lessp", 2, Some(2));
        assert_subr_arity("subst-char-in-string", 3, Some(4));
        assert_subr_arity("substitute-in-file-name", 1, Some(1));
        assert_subr_arity("syntax-after", 1, Some(1));
        assert_subr_arity("syntax-class-to-char", 1, Some(1));
        assert_subr_arity("syntax-ppss", 0, Some(1));
        assert_subr_arity("syntax-ppss-flush-cache", 1, None);
        assert_subr_arity("syntax-table", 0, Some(0));
        assert_subr_arity("syntax-table-p", 1, Some(1));
        assert_subr_arity("standard-case-table", 0, Some(0));
        assert_subr_arity("standard-category-table", 0, Some(0));
        assert_subr_arity("standard-syntax-table", 0, Some(0));
    }

    #[test]
    fn subr_arity_time_user_runtime_helpers_match_oracle() {
        assert_subr_arity("threadp", 1, Some(1));
        assert_subr_arity("time-add", 2, Some(2));
        assert_subr_arity("time-convert", 1, Some(2));
        assert_subr_arity("time-equal-p", 2, Some(2));
        assert_subr_arity("time-less-p", 2, Some(2));
        assert_subr_arity("time-subtract", 2, Some(2));
        assert_subr_arity("system-groups", 0, Some(0));
        assert_subr_arity("system-users", 0, Some(0));
        assert_subr_arity("tab-bar-height", 0, Some(2));
        assert_subr_arity("text-char-description", 1, Some(1));
        assert_subr_arity("tool-bar-height", 0, Some(2));
        assert_subr_arity("user-full-name", 0, Some(1));
        assert_subr_arity("file-user-uid", 0, Some(0));
        assert_subr_arity("user-login-name", 0, Some(1));
        assert_subr_arity("user-real-login-name", 0, Some(0));
        assert_subr_arity("user-real-uid", 0, Some(0));
        assert_subr_arity("user-uid", 0, Some(0));
        assert_subr_arity("y-or-n-p", 1, Some(1));
        assert_subr_arity("yes-or-no-p", 1, Some(1));
        assert_subr_arity("zerop", 1, Some(1));
        assert_subr_arity("zlib-available-p", 0, Some(0));
        assert_subr_arity("zlib-decompress-region", 2, Some(3));
    }

    #[test]
    fn subr_arity_command_edit_runtime_helpers_match_oracle() {
        assert_subr_arity("self-insert-command", 1, Some(2));
        assert_subr_arity("shell-command-to-string", 1, Some(1));
        assert_subr_arity("signal", 2, Some(2));
        assert_subr_arity("single-key-description", 1, Some(2));
        assert_subr_arity("skip-chars-backward", 1, Some(2));
        assert_subr_arity("skip-chars-forward", 1, Some(2));
        assert_subr_arity("skip-syntax-backward", 1, Some(2));
        assert_subr_arity("skip-syntax-forward", 1, Some(2));
        assert_subr_arity("sort", 1, None);
        assert_subr_arity("special-form-p", 1, Some(1));
        assert_subr_arity("split-window", 0, Some(4));
        assert_subr_arity("store-kbd-macro-event", 1, Some(1));
        assert_subr_arity("switch-to-buffer", 1, Some(3));
        assert_subr_arity("take", 2, Some(2));
        assert_subr_arity("reindent-then-newline-and-indent", 0, Some(0));
        assert_subr_arity("clear-this-command-keys", 0, Some(1));
        assert_subr_arity("combine-after-change-execute", 0, Some(0));
        assert_subr_arity("this-command-keys", 0, Some(0));
        assert_subr_arity("this-command-keys-vector", 0, Some(0));
        assert_subr_arity("transient-mark-mode", 0, Some(1));
        assert_subr_arity("transpose-chars", 1, Some(1));
        assert_subr_arity("transpose-lines", 1, Some(1));
        assert_subr_arity("transpose-paragraphs", 1, Some(1));
        assert_subr_arity("transpose-sentences", 1, Some(1));
        assert_subr_arity("transpose-sexps", 1, Some(2));
        assert_subr_arity("transpose-words", 1, Some(1));
        assert_subr_arity("undo", 0, Some(1));
        assert_subr_arity("undo-boundary", 0, Some(0));
        assert_subr_arity("universal-argument", 0, Some(0));
        assert_subr_arity("unibyte-char-to-multibyte", 1, Some(1));
        assert_subr_arity("upcase-initials", 1, Some(1));
        assert_subr_arity("upcase-initials-region", 2, Some(3));
        assert_subr_arity("upcase-region", 2, Some(3));
        assert_subr_arity("upcase-word", 1, Some(1));
        assert_subr_arity("use-global-map", 1, Some(1));
        assert_subr_arity("use-local-map", 1, Some(1));
        assert_subr_arity("region-active-p", 0, Some(0));
        assert_subr_arity("use-region-p", 0, Some(0));
        assert_subr_arity("view-register", 1, Some(1));
        assert_subr_arity("where-is-internal", 1, Some(5));
        assert_subr_arity("widget-apply", 2, None);
        assert_subr_arity("yank", 0, Some(1));
        assert_subr_arity("yank-pop", 0, Some(1));
    }

    #[test]
    fn subr_arity_replace_window_io_helpers_match_oracle() {
        assert_subr_arity("isearch-backward", 0, Some(2));
        assert_subr_arity("isearch-forward", 0, Some(2));
        assert_subr_arity("minor-mode-key-binding", 1, Some(2));
        assert_subr_arity("narrow-to-region", 2, Some(2));
        assert_subr_arity("remove", 2, Some(2));
        assert_subr_arity("remove-images", 2, Some(3));
        assert_subr_arity("remove-variable-watcher", 2, Some(2));
        assert_subr_arity("replace-regexp", 2, Some(7));
        assert_subr_arity("replace-string", 2, Some(7));
        assert_subr_arity("save-buffer", 0, Some(1));
        assert_subr_arity("scroll-down", 0, Some(1));
        assert_subr_arity("scroll-down-command", 0, Some(1));
        assert_subr_arity("scroll-left", 0, Some(2));
        assert_subr_arity("scroll-right", 0, Some(2));
        assert_subr_arity("scroll-up", 0, Some(1));
        assert_subr_arity("scroll-up-command", 0, Some(1));
        assert_subr_arity("select-frame", 1, Some(2));
        assert_subr_arity("select-frame-set-input-focus", 1, Some(2));
        assert_subr_arity("select-window", 1, Some(2));
        assert_subr_arity("selected-frame", 0, Some(0));
        assert_subr_arity("set-charset-priority", 1, None);
        assert_subr_arity("subr-arity", 1, Some(1));
        assert_subr_arity("subr-name", 1, Some(1));
        assert_subr_arity("subrp", 1, Some(1));
        assert_subr_arity("widget-get", 2, Some(2));
        assert_subr_arity("widget-put", 3, Some(3));
        assert_subr_arity("write-region", 3, Some(7));
    }

    #[test]
    fn subr_arity_charset_json_libxml_display_helpers_match_oracle() {
        assert_subr_arity("define-charset-internal", 17, None);
        assert_subr_arity("find-charset-region", 2, Some(3));
        assert_subr_arity("find-charset-string", 1, Some(2));
        assert_subr_arity("find-composition-internal", 4, Some(4));
        assert_subr_arity("format-mode-line", 1, Some(4));
        assert_subr_arity("insert-abbrev-table-description", 1, Some(2));
        assert_subr_arity("insert-image", 1, Some(5));
        assert_subr_arity("json-insert", 1, None);
        assert_subr_arity("json-available-p", 0, Some(0));
        assert_subr_arity("json-parse-string", 1, None);
        assert_subr_arity("json-serialize", 1, None);
        assert_subr_arity("libxml-available-p", 0, Some(0));
        assert_subr_arity("libxml-parse-html-region", 0, Some(4));
        assert_subr_arity("libxml-parse-xml-region", 0, Some(4));
        assert_subr_arity("line-pixel-height", 0, Some(0));
        assert_subr_arity("long-line-optimizations-p", 0, Some(0));
        assert_subr_arity("lookup-image-map", 3, Some(3));
        assert_subr_arity("make-bool-vector", 2, Some(2));
        assert_subr_arity("matching-paren", 1, Some(1));
        assert_subr_arity("md5", 1, Some(5));
        assert_subr_arity("merge-face-attribute", 3, Some(3));
        assert_subr_arity("multibyte-char-to-unibyte", 1, Some(1));
        assert_subr_arity("multibyte-string-p", 1, Some(1));
        assert_subr_arity("secure-hash", 2, Some(5));
        assert_subr_arity("secure-hash-algorithms", 0, Some(0));
    }

    #[test]
    fn subr_arity_hash_table_introspection_primitives_match_oracle() {
        assert_subr_arity("hash-table-test", 1, Some(1));
        assert_subr_arity("hash-table-size", 1, Some(1));
        assert_subr_arity("hash-table-rehash-size", 1, Some(1));
        assert_subr_arity("hash-table-rehash-threshold", 1, Some(1));
        assert_subr_arity("hash-table-weakness", 1, Some(1));
        assert_subr_arity("internal--hash-table-buckets", 1, Some(1));
        assert_subr_arity("internal--hash-table-histogram", 1, Some(1));
        assert_subr_arity("internal--hash-table-index-size", 1, Some(1));
        assert_subr_arity("sxhash-eq", 1, Some(1));
        assert_subr_arity("sxhash-eql", 1, Some(1));
        assert_subr_arity("sxhash-equal", 1, Some(1));
        assert_subr_arity("sxhash-equal-including-properties", 1, Some(1));
    }

    #[test]
    fn subr_arity_hash_table_core_primitives_match_oracle() {
        assert_subr_arity("hash-table-p", 1, Some(1));
        assert_subr_arity("make-hash-table", 0, None);
        assert_subr_arity("gethash", 2, Some(3));
        assert_subr_arity("puthash", 3, Some(3));
        assert_subr_arity("remhash", 2, Some(2));
        assert_subr_arity("clrhash", 1, Some(1));
        assert_subr_arity("hash-table-count", 1, Some(1));
        assert_subr_arity("maphash", 2, Some(2));
    }

    #[test]
    fn subr_arity_buffer_lookup_primitives_match_oracle() {
        assert_subr_arity("get-buffer", 1, Some(1));
        assert_subr_arity("get-buffer-create", 1, Some(2));
        assert_subr_arity("get-file-buffer", 1, Some(1));
        assert_subr_arity("generate-new-buffer-name", 1, Some(2));
        assert_subr_arity("generate-new-buffer", 1, Some(2));
    }

    #[test]
    fn subr_arity_numeric_state_helper_primitives_match_oracle() {
        assert_subr_arity("fceiling", 1, Some(1));
        assert_subr_arity("ffloor", 1, Some(1));
        assert_subr_arity("frexp", 1, Some(1));
        assert_subr_arity("fround", 1, Some(1));
        assert_subr_arity("framep", 1, Some(1));
        assert_subr_arity("ftruncate", 1, Some(1));
        assert_subr_arity("fixnump", 1, Some(1));
        assert_subr_arity("following-char", 0, Some(0));
        assert_subr_arity("garbage-collect", 0, Some(0));
        assert_subr_arity("get-load-suffixes", 0, Some(0));
        assert_subr_arity("get-byte", 0, Some(2));
    }

    #[test]
    fn subr_arity_misc_helper_primitives_match_oracle() {
        assert_subr_arity("format-message", 1, None);
        assert_subr_arity("identity", 1, Some(1));
        assert_subr_arity("prefix-numeric-value", 1, Some(1));
        assert_subr_arity("length", 1, Some(1));
        assert_subr_arity("length<", 2, Some(2));
        assert_subr_arity("length=", 2, Some(2));
        assert_subr_arity("length>", 2, Some(2));
        assert_subr_arity("ldexp", 2, Some(2));
        assert_subr_arity("logb", 1, Some(1));
        assert_subr_arity("logcount", 1, Some(1));
        assert_subr_arity("text-quoting-style", 0, Some(0));
        assert_subr_arity("lognot", 1, Some(1));
        assert_subr_arity("ngettext", 3, Some(3));
        assert_subr_arity("substring-no-properties", 1, Some(3));
        assert_subr_arity("group-name", 1, Some(1));
        assert_subr_arity("file-group-gid", 0, Some(0));
        assert_subr_arity("group-gid", 0, Some(0));
        assert_subr_arity("group-real-gid", 0, Some(0));
        assert_subr_arity("load-average", 0, Some(1));
        assert_subr_arity("last-nonminibuffer-frame", 0, Some(0));
        assert_subr_arity("interactive-p", 0, Some(0));
        assert_subr_arity("interpreted-function-p", 1, Some(1));
        assert_subr_arity("invisible-p", 1, Some(1));
        assert_subr_arity("macrop", 1, Some(1));
        assert_subr_arity("gensym", 0, Some(1));
    }

    #[test]
    fn subr_arity_window_frame_primitives_match_oracle() {
        assert_subr_arity("display-buffer", 1, Some(3));
        assert_subr_arity("active-minibuffer-window", 0, Some(0));
        assert_subr_arity("frame-char-height", 0, Some(1));
        assert_subr_arity("frame-char-width", 0, Some(1));
        assert_subr_arity("frame-first-window", 0, Some(1));
        assert_subr_arity("frame-list", 0, Some(0));
        assert_subr_arity("frame-live-p", 1, Some(1));
        assert_subr_arity("frame-native-height", 0, Some(1));
        assert_subr_arity("frame-native-width", 0, Some(1));
        assert_subr_arity("frame-parameter", 2, Some(2));
        assert_subr_arity("frame-parameters", 0, Some(1));
        assert_subr_arity("frame-position", 0, Some(1));
        assert_subr_arity("frame-root-window", 0, Some(1));
        assert_subr_arity("frame-root-window-p", 1, Some(1));
        assert_subr_arity("frame-visible-p", 1, Some(1));
        assert_subr_arity("frame-text-cols", 0, Some(1));
        assert_subr_arity("frame-text-height", 0, Some(1));
        assert_subr_arity("frame-text-lines", 0, Some(1));
        assert_subr_arity("frame-text-width", 0, Some(1));
        assert_subr_arity("frame-total-cols", 0, Some(1));
        assert_subr_arity("frame-total-lines", 0, Some(1));
        assert_subr_arity("minibuffer-selected-window", 0, Some(0));
        assert_subr_arity("minibuffer-window-active-p", 1, Some(1));
        assert_subr_arity("minibuffer-window", 0, Some(1));
        assert_subr_arity("selected-window", 0, Some(0));
        assert_subr_arity("set-frame-height", 2, Some(4));
        assert_subr_arity("set-frame-width", 2, Some(4));
        assert_subr_arity("set-frame-size", 3, Some(4));
        assert_subr_arity("set-frame-position", 3, Some(3));
        assert_subr_arity("set-window-parameter", 3, Some(3));
        assert_subr_arity("set-window-buffer", 2, Some(3));
        assert_subr_arity("set-window-configuration", 1, Some(3));
        assert_subr_arity("set-window-hscroll", 2, Some(2));
        assert_subr_arity("set-window-display-table", 2, Some(2));
        assert_subr_arity("set-window-cursor-type", 2, Some(2));
        assert_subr_arity("set-window-prev-buffers", 2, Some(2));
        assert_subr_arity("set-window-next-buffers", 2, Some(2));
        assert_subr_arity("set-window-margins", 2, Some(3));
        assert_subr_arity("set-window-point", 2, Some(2));
        assert_subr_arity("set-window-fringes", 2, Some(5));
        assert_subr_arity("set-window-scroll-bars", 1, Some(6));
        assert_subr_arity("set-window-start", 2, Some(3));
        assert_subr_arity("set-window-group-start", 2, Some(3));
        assert_subr_arity("set-window-vscroll", 2, Some(4));
        assert_subr_arity("window-frame", 0, Some(1));
        assert_subr_arity("window-fringes", 0, Some(1));
        assert_subr_arity("window-group-start", 0, Some(1));
        assert_subr_arity("window-header-line-height", 0, Some(1));
        assert_subr_arity("window-hscroll", 0, Some(1));
        assert_subr_arity("window-left-column", 0, Some(1));
        assert_subr_arity("window-margins", 0, Some(1));
        assert_subr_arity("window-mode-line-height", 0, Some(1));
        assert_subr_arity("window-pixel-height", 0, Some(1));
        assert_subr_arity("window-pixel-width", 0, Some(1));
        assert_subr_arity("window-body-edges", 0, Some(1));
        assert_subr_arity("window-body-pixel-edges", 0, Some(1));
        assert_subr_arity("visible-frame-list", 0, Some(0));
        assert_subr_arity("window-pixel-edges", 0, Some(1));
        assert_subr_arity("window-edges", 0, Some(4));
        assert_subr_arity("window-body-height", 0, Some(2));
        assert_subr_arity("window-body-width", 0, Some(2));
        assert_subr_arity("window-text-height", 0, Some(2));
        assert_subr_arity("window-text-width", 0, Some(2));
        assert_subr_arity("window-buffer", 0, Some(1));
        assert_subr_arity("window-configuration-equal-p", 2, Some(2));
        assert_subr_arity("window-configuration-frame", 1, Some(1));
        assert_subr_arity("window-configuration-p", 1, Some(1));
        assert_subr_arity("window-cursor-type", 0, Some(1));
        assert_subr_arity("window-display-table", 0, Some(1));
        assert_subr_arity("window-dedicated-p", 0, Some(1));
        assert_subr_arity("window-end", 0, Some(2));
        assert_subr_arity("window-list", 0, Some(3));
        assert_subr_arity("window-live-p", 1, Some(1));
        assert_subr_arity("window-next-buffers", 0, Some(1));
        assert_subr_arity("window-old-buffer", 0, Some(1));
        assert_subr_arity("window-old-point", 0, Some(1));
        assert_subr_arity("window-parameter", 2, Some(2));
        assert_subr_arity("window-parameters", 0, Some(1));
        assert_subr_arity("window-preserve-size", 0, Some(3));
        assert_subr_arity("window-prev-buffers", 0, Some(1));
        assert_subr_arity("window-resizable", 2, Some(5));
        assert_subr_arity("window-scroll-bars", 0, Some(1));
        assert_subr_arity("window-size-fixed-p", 0, Some(3));
        assert_subr_arity("window-valid-p", 1, Some(1));
        assert_subr_arity("window-minibuffer-p", 0, Some(1));
        assert_subr_arity("window-point", 0, Some(1));
        assert_subr_arity("window-start", 0, Some(1));
        assert_subr_arity("window-use-time", 0, Some(1));
        assert_subr_arity("window-vscroll", 0, Some(2));
        assert_subr_arity("window-top-line", 0, Some(1));
        assert_subr_arity("buffer-text-pixel-size", 0, Some(4));
        assert_subr_arity("compute-motion", 7, Some(7));
        assert_subr_arity("window-text-pixel-size", 0, Some(7));
        assert_subr_arity("windowp", 1, Some(1));
        assert_subr_arity("get-buffer-window", 0, Some(2));
        assert_subr_arity("get-buffer-window-list", 0, Some(3));
        assert_subr_arity("fit-window-to-buffer", 0, Some(6));
        assert_subr_arity("window-total-height", 0, Some(2));
        assert_subr_arity("window-total-width", 0, Some(2));
    }

    #[test]
    fn subr_primitive_and_native_predicates() {
        let primitive = builtin_subr_primitive_p(vec![Value::Subr("car".into())]).unwrap();
        assert!(primitive.is_truthy());

        let non_subr = builtin_subr_primitive_p(vec![Value::Int(1)]).unwrap();
        assert!(non_subr.is_nil());

        let native = builtin_subr_native_elisp_p(vec![Value::Subr("car".into())]).unwrap();
        assert!(native.is_nil());
    }

    // -- interpreted-function-p --

    #[test]
    fn interpreted_function_p_true_for_lambda() {
        let lam = make_lambda(vec!["x"], vec![], None);
        let result = builtin_interpreted_function_p(vec![lam]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn interpreted_function_p_false_for_bytecode() {
        let bc = make_bytecode(vec![], None);
        let result = builtin_interpreted_function_p(vec![bc]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn interpreted_function_p_false_for_subr() {
        let result = builtin_interpreted_function_p(vec![Value::Subr("car".into())]).unwrap();
        assert!(result.is_nil());
    }

    // -- special-form-p --

    #[test]
    fn special_form_p_true_for_if() {
        let result = builtin_special_form_p(vec![Value::symbol("if")]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn special_form_p_true_for_quote() {
        let result = builtin_special_form_p(vec![Value::symbol("quote")]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn special_form_p_true_for_setq() {
        let result = builtin_special_form_p(vec![Value::symbol("setq")]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn special_form_p_true_for_inline() {
        let result = builtin_special_form_p(vec![Value::symbol("inline")]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn special_form_p_false_for_car() {
        let result = builtin_special_form_p(vec![Value::symbol("car")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn special_form_p_false_for_when() {
        let result = builtin_special_form_p(vec![Value::symbol("when")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn special_form_p_false_for_throw() {
        let result = builtin_special_form_p(vec![Value::symbol("throw")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn special_form_p_false_for_int() {
        let result = builtin_special_form_p(vec![Value::Int(42)]).unwrap();
        assert!(result.is_nil());
    }

    // -- macrop --

    #[test]
    fn macrop_true_for_macro() {
        let m = make_macro(vec!["form"]);
        let result = builtin_macrop(vec![m]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn macrop_false_for_lambda() {
        let lam = make_lambda(vec!["x"], vec![], None);
        let result = builtin_macrop(vec![lam]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn macrop_false_for_nil() {
        let result = builtin_macrop(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn macrop_true_for_macro_cons_marker() {
        let marker = Value::cons(Value::symbol("macro"), Value::Int(1));
        let result = builtin_macrop(vec![marker]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn macrop_autoload_macro_returns_macro_marker_list() {
        let autoload_macro = Value::list(vec![
            Value::symbol("autoload"),
            Value::string("dummy-file"),
            Value::Nil,
            Value::Nil,
            Value::symbol("macro"),
        ]);
        let result = builtin_macrop(vec![autoload_macro]).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("macro"), Value::True])
        );
    }

    #[test]
    fn macrop_autoload_function_is_nil() {
        let autoload_function = Value::list(vec![
            Value::symbol("autoload"),
            Value::string("dummy-file"),
            Value::Nil,
            Value::True,
            Value::Nil,
        ]);
        let result = builtin_macrop(vec![autoload_function]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn macrop_autoload_t_marker_returns_single_t_list() {
        let autoload_t_marker = Value::list(vec![
            Value::symbol("autoload"),
            Value::string("dummy-file"),
            Value::Nil,
            Value::Nil,
            Value::True,
        ]);
        let result = builtin_macrop(vec![autoload_t_marker]).unwrap();
        assert_eq!(result, Value::list(vec![Value::True]));
    }

    // -- commandp --

    #[test]
    fn commandp_true_for_subr() {
        let result = builtin_commandp(vec![Value::Subr("car".into())]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn commandp_true_for_lambda() {
        let lam = make_lambda(vec![], vec![], None);
        let result = builtin_commandp(vec![lam]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn commandp_false_for_int() {
        let result = builtin_commandp(vec![Value::Int(42)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn commandp_false_for_nil() {
        let result = builtin_commandp(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn commandp_rejects_overflow_arity() {
        let err = builtin_commandp(vec![Value::symbol("car"), Value::Nil, Value::Nil])
            .expect_err("commandp should reject more than two arguments");
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    // -- func-arity --

    #[test]
    fn func_arity_lambda_required_only() {
        let lam = make_lambda(vec!["a", "b"], vec![], None);
        let result = builtin_func_arity(vec![lam]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(2));
            assert_eq!(pair.cdr.as_int(), Some(2));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_lambda_with_optional() {
        let lam = make_lambda(vec!["a"], vec!["b", "c"], None);
        let result = builtin_func_arity(vec![lam]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_int(), Some(3));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_lambda_with_rest() {
        let lam = make_lambda(vec!["a"], vec![], Some("rest"));
        let result = builtin_func_arity(vec![lam]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_bytecode() {
        let bc = make_bytecode(vec!["x", "y"], Some("rest"));
        let result = builtin_func_arity(vec![bc]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(2));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_subr() {
        let result = builtin_func_arity(vec![Value::Subr("+".into())]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(0));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_subr_uses_compat_overrides() {
        let message = builtin_func_arity(vec![Value::Subr("message".into())]).unwrap();
        if let Value::Cons(cell) = &message {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }

        let car = builtin_func_arity(vec![Value::Subr("car".into())]).unwrap();
        if let Value::Cons(cell) = &car {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_int(), Some(1));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_macro() {
        let m = make_macro(vec!["a", "b"]);
        let result = builtin_func_arity(vec![m]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(2));
            assert_eq!(pair.cdr.as_int(), Some(2));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_defvar_local_preserves_optional_arity() {
        let macro_value = fallback_macro_value("defvar-local").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(2));
            assert_eq!(pair.cdr.as_int(), Some(3));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_save_match_data_is_zero_or_many() {
        let macro_value = fallback_macro_value("save-match-data").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(0));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_save_mark_and_excursion_is_zero_or_many() {
        let macro_value =
            fallback_macro_value("save-mark-and-excursion").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(0));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_save_window_excursion_is_zero_or_many() {
        let macro_value =
            fallback_macro_value("save-window-excursion").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(0));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_save_selected_window_is_zero_or_many() {
        let macro_value =
            fallback_macro_value("save-selected-window").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(0));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_with_local_quit_is_zero_or_many() {
        let macro_value = fallback_macro_value("with-local-quit").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(0));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_with_temp_message_is_one_or_many() {
        let macro_value = fallback_macro_value("with-temp-message").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_bound_and_true_p_is_one_required() {
        let macro_value = fallback_macro_value("bound-and-true-p").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_int(), Some(1));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_with_demoted_errors_is_one_or_many() {
        let macro_value =
            fallback_macro_value("with-demoted-errors").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_pcase_dolist_is_one_or_many() {
        let macro_value = fallback_macro_value("pcase-dolist").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_pcase_let_is_one_or_many() {
        let macro_value = fallback_macro_value("pcase-let").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn fallback_macro_pcase_let_star_is_one_or_many() {
        let macro_value = fallback_macro_value("pcase-let*").expect("fallback macro exists");
        let result = builtin_func_arity(vec![macro_value]).unwrap();
        if let Value::Cons(cell) = &result {
            let pair = cell.lock().unwrap();
            assert_eq!(pair.car.as_int(), Some(1));
            assert_eq!(pair.cdr.as_symbol_name(), Some("many"));
        } else {
            panic!("expected cons cell");
        }
    }

    #[test]
    fn func_arity_error_for_non_callable() {
        let result = builtin_func_arity(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn func_arity_autoload_object_signals_wrong_type_argument_symbolp() {
        let autoload_fn = Value::list(vec![
            Value::symbol("autoload"),
            Value::string("vm-auto-file"),
            Value::Nil,
            Value::True,
            Value::Nil,
        ]);
        let result = builtin_func_arity(vec![autoload_fn.clone()])
            .expect_err("autoload forms should not satisfy func-arity");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("symbolp"), autoload_fn]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    // -- wrong arg count --

    #[test]
    fn subr_name_wrong_args() {
        let result = builtin_subr_name(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn func_arity_wrong_args() {
        let result = builtin_func_arity(vec![]);
        assert!(result.is_err());
    }
}
