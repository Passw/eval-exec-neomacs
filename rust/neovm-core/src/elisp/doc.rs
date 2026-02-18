//! Documentation and help support builtins.
//!
//! Provides:
//! - `documentation` — retrieve docstring from a function
//! - `describe-function` — return description string for a function
//! - `describe-variable` — return description string for a variable
//! - `documentation-property` — retrieve documentation property
//! - `Snarf-documentation` — internal DOC file loader compatibility shim
//! - `substitute-command-keys` — process special sequences in docstrings
//! - `help-function-arglist` — return argument list of a function

use super::error::{signal, EvalResult, Flow};
use super::value::*;

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

fn expect_min_max_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// Eval-dependent builtins
// ---------------------------------------------------------------------------

/// `(documentation FUNCTION &optional RAW)` -- return the docstring of FUNCTION.
///
/// Looks up FUNCTION in the obarray's function cell. If the function cell
/// holds a `Lambda` (or `Macro`) with a docstring, returns it as a string.
/// Otherwise returns nil.  The optional RAW argument is accepted but ignored
/// (in real Emacs it controls whether `substitute-command-keys` is applied).
pub(crate) fn builtin_documentation(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("documentation", &args, 1, 2)?;

    // For symbols, Emacs consults the `function-documentation` property first.
    // This can produce docs even when the function cell is non-callable.
    if let Some(name) = args[0].as_symbol_name() {
        let name = name.to_string();
        if let Some(prop) = eval
            .obarray
            .get_property(&name, "function-documentation")
            .cloned()
        {
            return eval_documentation_property_value(eval, prop);
        }

        let mut func_val =
            super::builtins::builtin_symbol_function(eval, vec![Value::symbol(name.clone())])?;
        if let Some(alias_name) = func_val.as_symbol_name() {
            let indirect =
                super::builtins::builtin_indirect_function(eval, vec![Value::symbol(alias_name)])?;
            if !indirect.is_nil() {
                func_val = indirect;
            }
        }
        if func_val.is_nil() {
            return Err(signal("void-function", vec![Value::symbol(name)]));
        }

        return function_doc_or_error(func_val);
    }

    function_doc_or_error(args[0].clone())
}

fn function_doc_or_error(func_val: Value) -> EvalResult {
    if let Some(result) = quoted_lambda_documentation(&func_val) {
        return result;
    }
    if let Some(result) = quoted_macro_invalid_designator(&func_val) {
        return result;
    }

    match func_val {
        Value::Lambda(data) | Value::Macro(data) => match &data.docstring {
            Some(doc) => Ok(Value::string(doc.clone())),
            None => Ok(Value::Nil),
        },
        Value::Subr(_) => Ok(Value::string("Built-in function.")),
        Value::Str(_) | Value::Vector(_) => Ok(Value::string("Keyboard macro.")),
        Value::ByteCode(bytecode) => Ok(bytecode
            .docstring
            .as_ref()
            .map_or(Value::Nil, |doc| Value::string(doc.clone()))),
        other => Err(signal("invalid-function", vec![other])),
    }
}

fn quoted_lambda_documentation(function: &Value) -> Option<EvalResult> {
    let Value::Cons(cell) = function else {
        return None;
    };

    let pair = cell.lock().ok()?;
    if pair.car.as_symbol_name() != Some("lambda") {
        return None;
    }

    let mut tail = pair.cdr.clone();
    drop(pair);

    let Value::Cons(param_cell) = tail else {
        return Some(Err(signal("invalid-function", vec![function.clone()])));
    };
    let params_and_body = param_cell.lock().ok()?;
    tail = params_and_body.cdr.clone();
    drop(params_and_body);

    match tail {
        Value::Nil => Some(Ok(Value::Nil)),
        Value::Cons(body_cell) => {
            let body = body_cell.lock().ok()?;
            if let Some(doc) = body.car.as_str() {
                Some(Ok(Value::string(doc)))
            } else {
                Some(Ok(Value::Nil))
            }
        }
        other => Some(Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), other],
        ))),
    }
}

fn quoted_macro_invalid_designator(function: &Value) -> Option<EvalResult> {
    let Value::Cons(cell) = function else {
        return None;
    };

    let pair = cell.lock().ok()?;
    if pair.car.as_symbol_name() != Some("macro") {
        return None;
    }

    let payload = pair.cdr.clone();
    if payload.is_nil() {
        return Some(Err(signal("void-function", vec![Value::Nil])));
    }

    Some(Err(signal("invalid-function", vec![payload])))
}

fn eval_documentation_property_value(
    eval: &mut super::eval::Evaluator,
    value: Value,
) -> EvalResult {
    if let Some(text) = value.as_str() {
        return Ok(Value::string(text));
    }

    // Integer doc offsets require DOC-file lookup; return nil when unresolved.
    if matches!(value, Value::Int(_)) {
        return Ok(Value::Nil);
    }

    let expr = super::eval::value_to_expr_pub(&value);
    eval.eval(&expr)
}

fn startup_variable_doc_offset_symbol(sym: &str, prop: &str, value: &Value) -> bool {
    prop == "variable-documentation"
        && matches!(value, Value::Int(_))
        && startup_variable_doc_stub(sym).is_some()
}

pub(crate) static STARTUP_VARIABLE_DOC_STUBS: &[(&str, &str)] = &[
    ("abbrev-mode", "Non-nil if Abbrev mode is enabled."),
    (
        "after-change-functions",
        "List of functions to call after each text change.",
    ),
    (
        "after-delete-frame-functions",
        "Functions run after deleting a frame.",
    ),
    (
        "after-init-time",
        "Value of `current-time' after loading the init files.",
    ),
    (
        "after-insert-file-functions",
        "A list of functions to be called at the end of `insert-file-contents'.",
    ),
    (
        "after-load-alist",
        "An alist of functions to be evalled when particular files are loaded.",
    ),
    (
        "alternate-fontname-alist",
        "Alist of fontname vs list of the alternate fontnames.",
    ),
    (
        "ambiguous-width-chars",
        "A char-table for characters whose width (columns) can be 1 or 2.",
    ),
    (
        "attempt-orderly-shutdown-on-fatal-signal",
        "If non-nil, attempt orderly shutdown on fatal signals.",
    ),
    (
        "attempt-stack-overflow-recovery",
        "If non-nil, attempt to recover from C stack overflows.",
    ),
    (
        "auto-composition-emoji-eligible-codepoints",
        "List of codepoints for which auto-composition will check for an emoji font.",
    ),
    (
        "auto-composition-function",
        "Function to call to compose characters automatically.",
    ),
    (
        "auto-composition-mode",
        "Non-nil if Auto-Composition mode is enabled.",
    ),
    ("auto-fill-function", "Function called (if non-nil) to perform auto-fill."),
    (
        "auto-fill-chars",
        "A char-table for characters which invoke auto-filling.",
    ),
    (
        "auto-hscroll-mode",
        "Allow or disallow automatic horizontal scrolling of windows.",
    ),
    (
        "auto-raise-tab-bar-buttons",
        "Non-nil means raise tab-bar buttons when the mouse moves over them.",
    ),
    (
        "auto-raise-tool-bar-buttons",
        "Non-nil means raise tool-bar buttons when the mouse moves over them.",
    ),
    (
        "auto-resize-tab-bars",
        "Non-nil means automatically resize tab-bars.",
    ),
    (
        "auto-resize-tool-bars",
        "Non-nil means automatically resize tool-bars.",
    ),
    (
        "auto-save-include-big-deletions",
        "If non-nil, auto-save even if a large part of the text is deleted.",
    ),
    ("auto-save-interval", "Number of input events between auto-saves."),
    (
        "auto-save-list-file-name",
        "File name in which to write a list of all auto save file names.",
    ),
    ("auto-save-no-message", "Non-nil means do not print any message when auto-saving."),
    ("auto-save-timeout", "Number of seconds idle time before auto-save."),
    (
        "auto-save-visited-file-name",
        "Non-nil says auto-save a buffer in the file it is visiting, when practical.",
    ),
    (
        "auto-window-vscroll",
        "Non-nil means to automatically adjust `window-vscroll' to view tall lines.",
    ),
    (
        "backtrace-on-error-noninteractive",
        "Non-nil means print backtrace on error in batch mode.",
    ),
    (
        "backtrace-on-redisplay-error",
        "Non-nil means create a backtrace if a lisp error occurs in redisplay.",
    ),
    ("baud-rate", "The output baud rate of the terminal."),
    (
        "before-change-functions",
        "List of functions to call before each text change.",
    ),
    (
        "before-init-time",
        "Value of `current-time' before Emacs begins initialization.",
    ),
    (
        "binary-as-unsigned",
        "Non-nil means `format' %x and %o treat integers as unsigned.",
    ),
    (
        "bidi-display-reordering",
        "Non-nil means reorder bidirectional text for display in the visual order.",
    ),
    (
        "bidi-inhibit-bpa",
        "Non-nil means inhibit the Bidirectional Parentheses Algorithm.",
    ),
    (
        "bidi-paragraph-direction",
        "If non-nil, forces directionality of text paragraphs in the buffer.",
    ),
    (
        "bidi-paragraph-separate-re",
        "If non-nil, a regexp matching a line that separates paragraphs.",
    ),
    (
        "bidi-paragraph-start-re",
        "If non-nil, a regexp matching a line that starts OR separates paragraphs.",
    ),
    (
        "blink-cursor-alist",
        "Alist specifying how to blink the cursor off.",
    ),
    (
        "buffer-access-fontified-property",
        "Property which (if non-nil) indicates text has been fontified.",
    ),
    (
        "buffer-access-fontify-functions",
        "List of functions called by `buffer-substring' to fontify if necessary.",
    ),
    (
        "buffer-auto-save-file-format",
        "Format in which to write auto-save files.",
    ),
    ("buffer-auto-save-file-name", "Name of file for auto-saving current buffer."),
    ("buffer-backed-up", "Non-nil if this buffer's file has been backed up."),
    (
        "buffer-display-count",
        "A number incremented each time this buffer is displayed in a window.",
    ),
    (
        "buffer-display-table",
        "Display table that controls display of the contents of current buffer.",
    ),
    (
        "buffer-display-time",
        "Time stamp updated each time this buffer is displayed in a window.",
    ),
    (
        "buffer-file-coding-system",
        "Coding system to be used for encoding the buffer contents on saving.",
    ),
    (
        "buffer-file-format",
        "List of formats to use when saving this buffer.",
    ),
    (
        "buffer-file-name",
        "Name of file visited in current buffer, or nil if not visiting a file.",
    ),
    (
        "buffer-file-truename",
        "Abbreviated truename of file visited in current buffer, or nil if none.",
    ),
    ("buffer-invisibility-spec", "Invisibility spec of this buffer."),
    ("buffer-list-update-hook", "Hook run when the buffer list changes."),
    ("buffer-read-only", "Non-nil if this buffer is read-only."),
    (
        "buffer-saved-size",
        "Length of current buffer when last read in, saved or auto-saved.",
    ),
    (
        "build-files",
        "A list of files used to build this Emacs binary.",
    ),
    (
        "byte-boolean-vars",
        "List of all DEFVAR_BOOL variables, used by the byte code optimizer.",
    ),
    (
        "bytecomp-version-regexp",
        "Regular expression matching safe to load compiled Lisp files.",
    ),
    ("buffer-undo-list", "List of undo entries in current buffer."),
    (
        "cache-long-scans",
        "Non-nil means that Emacs should use caches in attempt to speedup buffer scans.",
    ),
    ("cairo-version-string", "Version info for cairo."),
    (
        "cannot-suspend",
        "Non-nil means to always spawn a subshell instead of suspending.",
    ),
    ("case-fold-search", "Non-nil if searches and matches should ignore case."),
    (
        "case-symbols-as-words",
        "If non-nil, case functions treat symbol syntax as part of words.",
    ),
    (
        "change-major-mode-hook",
        "Normal hook run before changing the major mode of a buffer.",
    ),
    (
        "char-code-property-alist",
        "Alist of character property name vs char-table containing property values.",
    ),
    (
        "char-property-alias-alist",
        "Alist of alternative properties for properties without a value.",
    ),
    ("char-script-table", "Char table of script symbols."),
    (
        "char-width-table",
        "A char-table for width (columns) of each character.",
    ),
    ("charset-list", "List of all charsets ever defined."),
    (
        "charset-map-path",
        "List of directories to search for charset map files.",
    ),
    (
        "charset-revision-table",
        "Alist of charsets vs revision numbers.",
    ),
    (
        "clear-message-function",
        "If non-nil, function to clear echo-area messages.",
    ),
    (
        "clone-indirect-buffer-hook",
        "Normal hook to run in the new buffer at the end of `make-indirect-buffer'.",
    ),
    (
        "code-conversion-map-vector",
        "Vector of code conversion maps.",
    ),
    (
        "coding-category-list",
        "List of coding-categories (symbols) ordered by priority.",
    ),
    ("coding-system-alist", "Alist of coding system names."),
    (
        "coding-system-for-read",
        "Specify the coding system for read operations.",
    ),
    (
        "coding-system-for-write",
        "Specify the coding system for write operations.",
    ),
    ("coding-system-list", "List of coding systems."),
    ("coding-system-require-warning", "Internal use only."),
    (
        "combine-after-change-calls",
        "Used internally by the function `combine-after-change-calls' macro.",
    ),
    (
        "comment-use-syntax-ppss",
        "Non-nil means `forward-comment' can use `syntax-ppss' internally.",
    ),
    ("comp-abi-hash", "String signing the .eln files ABI."),
    ("comp-ctxt", "The compiler context."),
    (
        "comp-deferred-pending-h",
        "Hash table symbol-name -> function-value.",
    ),
    ("comp-eln-to-el-h", "Hash table eln-filename -> el-filename."),
    (
        "comp-file-preloaded-p",
        "When non-nil, assume the file being compiled to be preloaded.",
    ),
    (
        "comp-installed-trampolines-h",
        "Hash table subr-name -> installed trampoline.",
    ),
    (
        "comp-loaded-comp-units-h",
        "Hash table recording all loaded compilation units, file -> CU.",
    ),
    (
        "comp-native-version-dir",
        "Directory in use to disambiguate eln compatibility.",
    ),
    (
        "comp-no-native-file-h",
        "Files for which no deferred compilation should be performed.",
    ),
    (
        "comp-sanitizer-active",
        "If non-nil, enable runtime execution of native-compiler sanitizer.",
    ),
    (
        "comp-subr-arities-h",
        "Hash table recording the arity of Lisp primitives.",
    ),
    ("comp-subr-list", "List of all defined subrs."),
    (
        "compose-chars-after-function",
        "Function to adjust composition of buffer text.",
    ),
    (
        "composition-break-at-point",
        "If non-nil, prevent auto-composition of characters around point.",
    ),
    (
        "composition-function-table",
        "Char-table of functions for automatic character composition.",
    ),
    (
        "configure-info-directory",
        "For internal use by the build procedure only.",
    ),
    (
        "cons-cells-consed",
        "Number of cons cells that have been consed so far.",
    ),
    (
        "create-lockfiles",
        "Non-nil means use lockfiles to avoid editing collisions.",
    ),
    (
        "cross-disabled-images",
        "Non-nil means always draw a cross over disabled images.",
    ),
    (
        "ctags-program-name",
        "Name of the `ctags' program distributed with Emacs.",
    ),
    (
        "ctl-arrow",
        "Non-nil means display control chars with uparrow '^'.",
    ),
    (
        "command-debug-status",
        "Debugging status of current interactive command.",
    ),
    ("command-error-function", "Function to output error messages."),
    (
        "command-history",
        "List of recent commands that read arguments from terminal.",
    ),
    (
        "command-line-args",
        "Args passed by shell to Emacs, as a list of strings.",
    ),
    (
        "comment-end-can-be-escaped",
        "Non-nil means an escaped ender inside a comment doesn't end the comment.",
    ),
    (
        "completion-ignore-case",
        "Non-nil means don't consider case significant in completion.",
    ),
    (
        "completion-ignored-extensions",
        "Completion ignores file names ending in any string in this list.",
    ),
    (
        "completion-regexp-list",
        "List of regexps that should restrict possible completions.",
    ),
    (
        "current-iso639-language",
        "ISO639 language mnemonic symbol for the current language environment.",
    ),
    (
        "current-key-remap-sequence",
        "The key sequence currently being remap, or nil.",
    ),
    ("current-load-list", "Used for internal purposes by `load'."),
    (
        "current-minibuffer-command",
        "This is like `this-command`, but bound recursively.",
    ),
    (
        "current-prefix-arg",
        "The value of the prefix argument for this editing command.",
    ),
    (
        "current-time-list",
        "Whether `current-time' should return list or (TICKS . HZ) form.",
    ),
    (
        "cursor-in-echo-area",
        "Non-nil means put cursor in minibuffer, at end of any message there.",
    ),
    (
        "cursor-in-non-selected-windows",
        "Non-nil means show a cursor in non-selected windows.",
    ),
    (
        "cursor-type",
        "Cursor to use when this buffer is in the selected window.",
    ),
    (
        "data-directory",
        "Directory of machine-independent files that come with GNU Emacs.",
    ),
    (
        "dbus-compiled-version",
        "The version of D-Bus Emacs is compiled against.",
    ),
    (
        "dbus-debug",
        "If non-nil, debug messages of D-Bus bindings are raised.",
    ),
    (
        "dbus-message-type-error",
        "Message type of an error reply message.",
    ),
    (
        "dbus-message-type-invalid",
        "This value is never a valid message type.",
    ),
    (
        "dbus-message-type-method-call",
        "Message type of a method call message.",
    ),
    (
        "dbus-message-type-method-return",
        "Message type of a method return message.",
    ),
    (
        "dbus-message-type-signal",
        "Message type of a signal message.",
    ),
    (
        "dbus-registered-objects-table",
        "Hash table of registered functions for D-Bus.",
    ),
    (
        "dbus-runtime-version",
        "The version of D-Bus Emacs runs with.",
    ),
    (
        "deactivate-mark",
        "Whether to deactivate the mark after an editing command.",
    ),
    (
        "debug-ignored-errors",
        "List of errors for which the debugger should not be called.",
    ),
    (
        "debug-on-event",
        "Enter debugger on this event.",
    ),
    (
        "debug-on-message",
        "If non-nil, debug if a message matching this regexp is displayed.",
    ),
    (
        "debug-on-next-call",
        "Non-nil means enter debugger before next `eval', `apply' or `funcall'.",
    ),
    (
        "debug-on-error",
        "Non-nil means enter debugger if an error is signaled.",
    ),
    (
        "debug-on-quit",
        "Non-nil means enter debugger if quit is signaled (C-g, for example).",
    ),
    (
        "debug-on-signal",
        "Non-nil means call the debugger regardless of condition handlers.",
    ),
    (
        "debugger",
        "Function to call to invoke debugger.",
    ),
    (
        "debugger-may-continue",
        "Non-nil means debugger may continue execution.",
    ),
    (
        "debugger-stack-frame-as-list",
        "Non-nil means display call stack frames as lists.",
    ),
    (
        "default-directory",
        "Name of default directory of current buffer.",
    ),
    (
        "default-frame-alist",
        "Alist of default values of frame parameters for frame creation.",
    ),
    (
        "default-frame-scroll-bars",
        "Default position of vertical scroll bars on this window-system.",
    ),
    (
        "default-file-name-coding-system",
        "Default coding system for encoding file names.",
    ),
    (
        "default-minibuffer-frame",
        "Minibuffer-less frames by default use this frame's minibuffer.",
    ),
    (
        "default-process-coding-system",
        "Cons of coding systems used for process I/O by default.",
    ),
    (
        "default-text-properties",
        "Property-list used as default values.",
    ),
    (
        "defining-kbd-macro",
        "Non-nil while a keyboard macro is being defined.  Don't set this!",
    ),
    (
        "delayed-warnings-list",
        "List of warnings to be displayed after this command.",
    ),
    (
        "delete-auto-save-files",
        "Non-nil means delete auto-save file when a buffer is saved.",
    ),
    (
        "delete-by-moving-to-trash",
        "Specifies whether to use the system's trash can.",
    ),
    (
        "delete-exited-processes",
        "Non-nil means delete processes immediately when they exit.",
    ),
    (
        "delete-frame-functions",
        "Functions run before deleting a frame.",
    ),
    (
        "delete-terminal-functions",
        "Special hook run when a terminal is deleted.",
    ),
    (
        "describe-bindings-check-shadowing-in-ranges",
        "If non-nil, consider command shadowing when describing ranges of keys.",
    ),
    (
        "disable-ascii-optimization",
        "If non-nil, Emacs does not optimize code decoder for ASCII files.",
    ),
    (
        "disable-inhibit-text-conversion",
        "Don't disable text conversion inside `read-key-sequence`.",
    ),
    (
        "disable-point-adjustment",
        "If non-nil, suppress point adjustment after executing a command.",
    ),
    (
        "display-fill-column-indicator",
        "Non-nil means display the fill column indicator.",
    ),
    (
        "display-fill-column-indicator-character",
        "Character to draw the indicator when `display-fill-column-indicator` is non-nil.",
    ),
    (
        "display-fill-column-indicator-column",
        "Column for indicator when `display-fill-column-indicator` is non-nil.",
    ),
    (
        "display-hourglass",
        "Non-nil means show an hourglass pointer, when Emacs is busy.",
    ),
    (
        "display-line-numbers-current-absolute",
        "Non-nil means display absolute number of current line.",
    ),
    (
        "display-line-numbers-major-tick",
        "If an integer N > 0, highlight line number of every Nth line.",
    ),
    (
        "display-line-numbers-minor-tick",
        "If an integer N > 0, highlight line number of every Nth line.",
    ),
    (
        "display-line-numbers-offset",
        "A signed integer added to each absolute line number.",
    ),
    (
        "display-line-numbers-widen",
        "Non-nil means display line numbers disregarding any narrowing.",
    ),
    (
        "display-line-numbers-width",
        "Minimum width of space reserved for line number display.",
    ),
    (
        "display-monitors-changed-functions",
        "Abnormal hook run when the monitor configuration changes.",
    ),
    (
        "display-pixels-per-inch",
        "Pixels per inch value for non-window system displays.",
    ),
    (
        "display-raw-bytes-as-hex",
        "Non-nil means display raw bytes in hexadecimal format.",
    ),
    (
        "display-line-numbers",
        "Non-nil means display line numbers.",
    ),
    (
        "doc-directory",
        "Directory containing the DOC file that comes with GNU Emacs.",
    ),
    (
        "double-click-fuzz",
        "Maximum mouse movement between clicks to make a double-click.",
    ),
    (
        "double-click-time",
        "Maximum time between mouse clicks to make a double-click.",
    ),
    ("dump-mode", "Non-nil when Emacs is dumping itself."),
    (
        "dynamic-library-alist",
        "Alist of dynamic libraries vs external files implementing them.",
    ),
    (
        "dynamic-library-suffixes",
        "A list of suffixes for loadable dynamic libraries.",
    ),
    (
        "ebrowse-program-name",
        "Name of the `ebrowse` program distributed with Emacs.",
    ),
    (
        "echo-area-clear-hook",
        "Normal hook run when clearing the echo area.",
    ),
    (
        "echo-keystrokes",
        "Nonzero means echo unfinished commands after this many seconds of pause.",
    ),
    (
        "echo-keystrokes-help",
        "Whether to append help text to echoed commands.",
    ),
    (
        "emacs-copyright",
        "Short copyright string for this version of Emacs.",
    ),
    (
        "emacs-version",
        "Version numbers of this version of Emacs.",
    ),
    (
        "emacsclient-program-name",
        "Name of the `emacsclient` program distributed with Emacs.",
    ),
    (
        "emulation-mode-map-alists",
        "List of keymap alists to use for emulation modes.",
    ),
    (
        "enable-character-translation",
        "Non-nil enables character translation while encoding and decoding.",
    ),
    (
        "enable-disabled-menus-and-buttons",
        "If non-nil, don't ignore events produced by disabled menu items and tool-bar.",
    ),
    (
        "enable-multibyte-characters",
        "Non-nil means the buffer contents are regarded as multi-byte characters.",
    ),
    (
        "enable-recursive-minibuffers",
        "Non-nil means to allow minibuffer commands while in the minibuffer.",
    ),
    (
        "eol-mnemonic-dos",
        "String displayed in mode line for DOS-like (CRLF) end-of-line format.",
    ),
    (
        "eol-mnemonic-mac",
        "String displayed in mode line for MAC-like (CR) end-of-line format.",
    ),
    (
        "eol-mnemonic-undecided",
        "String displayed in mode line when end-of-line format is not yet determined.",
    ),
    (
        "eol-mnemonic-unix",
        "String displayed in mode line for UNIX-like (LF) end-of-line format.",
    ),
    (
        "etags-program-name",
        "Name of the `etags` program distributed with Emacs.",
    ),
    (
        "eval-buffer-list",
        "List of buffers being read from by calls to `eval-buffer` and `eval-region`.",
    ),
    ("exec-directory", "Directory for executables for Emacs to invoke."),
    (
        "executing-kbd-macro",
        "Currently executing keyboard macro (string or vector).",
    ),
    (
        "executing-kbd-macro-index",
        "Index in currently executing keyboard macro; undefined if none executing.",
    ),
    ("exec-path", "List of directories to search programs to run in subprocesses."),
    ("exec-suffixes", "List of suffixes to try to find executable file names."),
    (
        "extra-keyboard-modifiers",
        "A mask of additional modifier keys to use with every keyboard character.",
    ),
    (
        "face--new-frame-defaults",
        "Hash table of global face definitions (for internal use only.)",
    ),
    (
        "face-default-stipple",
        "Default stipple pattern used on monochrome displays.",
    ),
    (
        "face-filters-always-match",
        "Non-nil means that face filters are always deemed to match.",
    ),
    (
        "face-font-lax-matched-attributes",
        "Whether to match some face attributes in lax manner when realizing faces.",
    ),
    (
        "face-font-rescale-alist",
        "Alist of fonts vs the rescaling factors.",
    ),
    ("face-ignored-fonts", "List of ignored fonts."),
    (
        "face-near-same-color-threshold",
        "Threshold for using distant-foreground color instead of foreground.",
    ),
    ("face-remapping-alist", "Alist of face remappings."),
    (
        "fast-but-imprecise-scrolling",
        "When non-nil, accelerate scrolling operations.",
    ),
    (
        "fast-read-process-output",
        "Non-nil to optimize the insertion of process output.",
    ),
    ("features", "A list of symbols which are the features of the executing Emacs."),
    (
        "file-coding-system-alist",
        "Alist to decide a coding system to use for a file I/O operation.",
    ),
    (
        "file-name-coding-system",
        "Coding system for encoding file names.",
    ),
    (
        "file-name-handler-alist",
        "Alist of elements (REGEXP . HANDLER) for file names handled specially.",
    ),
    (
        "fill-column",
        "Column beyond which automatic line-wrapping should happen.",
    ),
    (
        "find-word-boundary-function-table",
        "Char table of functions to search for the word boundary.",
    ),
    (
        "first-change-hook",
        "A list of functions to call before changing a buffer which is unmodified.",
    ),
    (
        "float-output-format",
        "The format descriptor string used to print floats.",
    ),
    (
        "floats-consed",
        "Number of floats that have been consed so far.",
    ),
    (
        "focus-follows-mouse",
        "Non-nil if window system changes focus when you move the mouse.",
    ),
    (
        "font-ccl-encoder-alist",
        "Alist of fontname patterns vs corresponding CCL program.",
    ),
    (
        "font-encoding-alist",
        "Alist of fontname patterns vs the corresponding encoding and repertory info.",
    ),
    (
        "font-encoding-charset-alist",
        "Alist of charsets vs the charsets to determine the preferred font encoding.",
    ),
    (
        "font-log",
        "A list that logs font-related actions and results, for debugging.",
    ),
    (
        "font-slant-table",
        "Vector of font slant symbols vs the corresponding numeric values.",
    ),
    (
        "font-use-system-font",
        "Non-nil means to apply the system defined font dynamically.",
    ),
    (
        "font-weight-table",
        "Vector of valid font weight values.",
    ),
    (
        "font-width-table",
        "Alist of font width symbols vs the corresponding numeric values.",
    ),
    (
        "fontification-functions",
        "List of functions to call to fontify regions of text.",
    ),
    (
        "fontset-alias-alist",
        "Alist of fontset names vs the aliases.",
    ),
    (
        "force-load-messages",
        "Non-nil means force printing messages when loading Lisp files.",
    ),
    (
        "frame-alpha-lower-limit",
        "The lower limit of the frame opacity (alpha transparency).",
    ),
    (
        "frame-inhibit-implied-resize",
        "Whether frames should be resized implicitly.",
    ),
    (
        "frame-internal-parameters",
        "Frame parameters specific to every frame.",
    ),
    (
        "frame-resize-pixelwise",
        "Non-nil means resize frames pixelwise.",
    ),
    (
        "frame-size-history",
        "History of frame size adjustments.",
    ),
    (
        "frame-title-format",
        "Template for displaying the title bar of visible frames.",
    ),
    (
        "fringe-bitmaps",
        "List of fringe bitmap symbols.",
    ),
    (
        "fringe-cursor-alist",
        "Mapping from logical to physical fringe cursor bitmaps.",
    ),
    (
        "fringe-indicator-alist",
        "Mapping from logical to physical fringe indicator bitmaps.",
    ),
    (
        "fringes-outside-margins",
        "Non-nil means to display fringes outside display margins.",
    ),
    (
        "function-key-map",
        "The parent keymap of all `local-function-key-map' instances.",
    ),
    (
        "garbage-collection-messages",
        "Non-nil means display messages at start and end of garbage collection.",
    ),
    (
        "gc-cons-percentage",
        "Portion of the heap used for allocation.",
    ),
    (
        "gc-cons-threshold",
        "Number of bytes of consing between garbage collections.",
    ),
    (
        "gc-elapsed",
        "Accumulated time elapsed in garbage collections.",
    ),
    (
        "gcs-done",
        "Accumulated number of garbage collections done.",
    ),
    (
        "global-disable-point-adjustment",
        "If non-nil, always suppress point adjustments.",
    ),
    (
        "global-mode-string",
        "String (or mode line construct) included (normally) in `mode-line-misc-info'.",
    ),
    (
        "glyph-table",
        "Table defining how to output a glyph code to the frame.",
    ),
    (
        "glyphless-char-display",
        "Char-table defining glyphless characters.",
    ),
    (
        "gnutls-log-level",
        "Logging level used by the GnuTLS functions.",
    ),
    (
        "header-line-format",
        "Analogous to `mode-line-format', but controls the header line.",
    ),
    (
        "help-char",
        "Character to recognize as meaning Help.",
    ),
    (
        "help-event-list",
        "List of input events to recognize as meaning Help.",
    ),
    (
        "help-form",
        "Form to execute when character `help-char' is read.",
    ),
    (
        "hexl-program-name",
        "Name of the `hexl' program distributed with Emacs.",
    ),
    (
        "highlight-nonselected-windows",
        "Non-nil means highlight active region even in nonselected windows.",
    ),
    (
        "history-add-new-input",
        "Non-nil means to add new elements in history.",
    ),
    (
        "history-delete-duplicates",
        "Non-nil means to delete duplicates in history.",
    ),
    (
        "history-length",
        "Maximum length of history lists before truncation takes place.",
    ),
    (
        "horizontal-scroll-bar",
        "Position of this buffer's horizontal scroll bar.",
    ),
    (
        "hourglass-delay",
        "Seconds to wait before displaying an hourglass pointer when Emacs is busy.",
    ),
    (
        "hscroll-margin",
        "How many columns away from the window edge point is allowed to get.",
    ),
    (
        "hscroll-step",
        "How many columns to scroll the window when point gets too close to the edge.",
    ),
    (
        "icon-title-format",
        "Template for displaying the title bar of an iconified frame.",
    ),
    (
        "iconify-child-frame",
        "How to handle iconification of child frames.",
    ),
    (
        "ignore-relative-composition",
        "Char table of characters which are not composed relatively.",
    ),
    (
        "image-cache-eviction-delay",
        "Maximum time after which images are removed from the cache.",
    ),
    (
        "image-scaling-factor",
        "When displaying images, apply this scaling factor before displaying.",
    ),
    (
        "image-types",
        "List of potentially supported image types.",
    ),
    (
        "indent-tabs-mode",
        "Indentation can insert tabs if this is non-nil.",
    ),
    (
        "indicate-buffer-boundaries",
        "Visually indicate buffer boundaries and scrolling.",
    ),
    (
        "indicate-empty-lines",
        "Visually indicate unused (\"empty\") screen lines after the buffer end.",
    ),
    (
        "inherit-process-coding-system",
        "Non-nil means process buffer inherits coding system of process output.",
    ),
    (
        "inhibit--record-char",
        "If non-nil, don't record input events.",
    ),
    (
        "inhibit-bidi-mirroring",
        "Non-nil means don't mirror characters even when bidi context requires that.",
    ),
    (
        "inhibit-changing-match-data",
        "Internal use only.",
    ),
    (
        "inhibit-compacting-font-caches",
        "If non-nil, don't compact font caches during GC.",
    ),
    (
        "inhibit-debugger",
        "Non-nil means never enter the debugger.",
    ),
    (
        "inhibit-eol-conversion",
        "Non-nil means always inhibit code conversion of end-of-line format.",
    ),
    (
        "inhibit-eval-during-redisplay",
        "Non-nil means don't eval Lisp during redisplay.",
    ),
    (
        "inhibit-field-text-motion",
        "Non-nil means text motion commands don't notice fields.",
    ),
    (
        "inhibit-file-name-handlers",
        "A list of file name handlers that temporarily should not be used.",
    ),
    (
        "inhibit-file-name-operation",
        "The operation for which `inhibit-file-name-handlers' is applicable.",
    ),
    (
        "inhibit-free-realized-faces",
        "Non-nil means don't free realized faces.  Internal use only.",
    ),
    (
        "inhibit-interaction",
        "Non-nil means any user interaction will signal an error.",
    ),
    (
        "inhibit-iso-escape-detection",
        "If non-nil, Emacs ignores ISO-2022 escape sequences during code detection.",
    ),
    (
        "inhibit-load-charset-map",
        "Inhibit loading of charset maps.  Used when dumping Emacs.",
    ),
    (
        "inhibit-menubar-update",
        "Non-nil means don't update menu bars.  Internal use only.",
    ),
    (
        "inhibit-message",
        "Non-nil means calls to `message' are not displayed.",
    ),
    (
        "inhibit-modification-hooks",
        "Non-nil means don't run any of the hooks that respond to buffer changes.",
    ),
    (
        "inhibit-mouse-event-check",
        "Whether the interactive spec \"e\" requires a mouse gesture event.",
    ),
    (
        "inhibit-null-byte-detection",
        "If non-nil, Emacs ignores null bytes on code detection.",
    ),
    (
        "inhibit-point-motion-hooks",
        "If non-nil, don't run `point-left' and `point-entered' text properties.",
    ),
    (
        "inhibit-quit",
        "Non-nil inhibits C-g quitting from happening immediately.",
    ),
    (
        "inhibit-read-only",
        "Non-nil means disregard read-only status of buffers or characters.",
    ),
    (
        "inhibit-redisplay",
        "Non-nil means don't actually do any redisplay.",
    ),
    (
        "inhibit-x-resources",
        "If non-nil, X resources, Windows Registry settings, and NS defaults are not used.",
    ),
    (
        "initial-environment",
        "List of environment variables inherited from the parent process.",
    ),
    (
        "initial-window-system",
        "Name of the window system that Emacs uses for the first frame.",
    ),
    (
        "input-decode-map",
        "Keymap that decodes input escape sequences.",
    ),
    (
        "input-method-function",
        "If non-nil, the function that implements the current input method.",
    ),
    (
        "input-method-previous-message",
        "When `input-method-function' is called, hold the previous echo area message.",
    ),
    (
        "input-pending-p-filter-events",
        "If non-nil, `input-pending-p' ignores some input events.",
    ),
    (
        "installation-directory",
        "A directory within which to look for the `lib-src' and `etc' directories.",
    ),
    (
        "integer-width",
        "Maximum number N of bits in safely-calculated integers.",
    ),
    (
        "internal--daemon-sockname",
        "Name of external socket passed to Emacs, or nil if none.",
    ),
    (
        "internal--text-quoting-flag",
        "If nil, a nil `text-quoting-style' is treated as `grave'.",
    ),
    (
        "internal--top-level-message",
        "Message displayed by `normal-top-level'.",
    ),
    (
        "internal-doc-file-name",
        "Name of file containing documentation strings of built-in symbols.",
    ),
    (
        "internal-make-interpreted-closure-function",
        "Function to filter the env when constructing a closure.",
    ),
    (
        "internal-when-entered-debugger",
        "The number of keyboard events as of last time `debugger' was called.",
    ),
    (
        "interrupt-process-functions",
        "List of functions to be called for `interrupt-process'.",
    ),
    (
        "intervals-consed",
        "Number of intervals that have been consed so far.",
    ),
    (
        "inverse-video",
        "Non-nil means invert the entire frame display.",
    ),
    (
        "invocation-directory",
        "The directory in which the Emacs executable was found, to run it.",
    ),
    (
        "invocation-name",
        "The program name that was used to run Emacs.",
    ),
    (
        "kbd-macro-termination-hook",
        "Normal hook run whenever a keyboard macro terminates.",
    ),
    (
        "key-translation-map",
        "Keymap of key translations that can override keymaps.",
    ),
    (
        "keyboard-translate-table",
        "Translate table for local keyboard input, or nil.",
    ),
    (
        "kill-buffer-delete-auto-save-files",
        "If non-nil, offer to delete any autosave file when killing a buffer.",
    ),
    (
        "kill-buffer-query-functions",
        "List of functions called with no args to query before killing a buffer.",
    ),
    (
        "kill-emacs-hook",
        "Hook run when `kill-emacs' is called.",
    ),
    (
        "large-hscroll-threshold",
        "Horizontal scroll of truncated lines above which to use redisplay shortcuts.",
    ),
    (
        "last-code-conversion-error",
        "Error status of the last code conversion.",
    ),
    (
        "last-coding-system-used",
        "Coding system used in the latest file or process I/O.",
    ),
    (
        "last-command-event",
        "Last input event of a key sequence that called a command.",
    ),
    (
        "last-event-device",
        "The name of the input device of the most recently read event.",
    ),
    (
        "last-event-frame",
        "The frame in which the most recently read event occurred.",
    ),
    (
        "last-input-event",
        "Last input event.",
    ),
    (
        "last-kbd-macro",
        "Last kbd macro defined, as a string or vector; nil if none defined.",
    ),
    (
        "last-nonmenu-event",
        "Last input event in a command, except for mouse menu events.",
    ),
    (
        "last-prefix-arg",
        "The value of the prefix argument for the previous editing command.",
    ),
    (
        "last-repeatable-command",
        "Last command that may be repeated.",
    ),
    (
        "latin-extra-code-table",
        "Table of extra Latin codes in the range 128..159 (inclusive).",
    ),
    (
        "left-fringe-width",
        "Width of this buffer's left fringe (in pixels).",
    ),
    (
        "left-margin",
        "Column for the default `indent-line-function' to indent to.",
    ),
    (
        "left-margin-width",
        "Width in columns of left marginal area for display of a buffer.",
    ),
    (
        "libgnutls-version",
        "The version of libgnutls that Emacs was compiled with.",
    ),
    (
        "line-number-display-limit",
        "Maximum buffer size for which line number should be displayed.",
    ),
    (
        "line-number-display-limit-width",
        "Maximum line width (in characters) for line number display.",
    ),
    (
        "line-prefix",
        "Prefix prepended to all non-continuation lines at display time.",
    ),
    (
        "line-spacing",
        "Additional space to put between lines when displaying a buffer.",
    ),
    (
        "lisp-eval-depth-reserve",
        "Extra depth that can be allocated to handle errors.",
    ),
    (
        "load-convert-to-unibyte",
        "Non-nil means `read' converts strings to unibyte whenever possible.",
    ),
    (
        "load-dangerous-libraries",
        "Non-nil means load dangerous compiled Lisp files.",
    ),
    (
        "load-file-rep-suffixes",
        "List of suffixes that indicate representations of the same file.",
    ),
    (
        "load-force-doc-strings",
        "Non-nil means `load' should force-load all dynamic doc strings.",
    ),
    (
        "load-in-progress",
        "Non-nil if inside of `load'.",
    ),
    (
        "load-no-native",
        "Non-nil means not to load native code unless explicitly requested.",
    ),
    (
        "load-read-function",
        "Function used for reading expressions.",
    ),
    (
        "load-source-file-function",
        "Function called in `load' to load an Emacs Lisp source file.",
    ),
    (
        "load-suffixes",
        "List of suffixes for Emacs Lisp files and dynamic modules.",
    ),
    (
        "load-true-file-name",
        "Full name of file being loaded by `load'.",
    ),
    (
        "local-abbrev-table",
        "Local (mode-specific) abbrev table of current buffer.",
    ),
    (
        "local-function-key-map",
        "Keymap that translates key sequences to key sequences during input.",
    ),
    (
        "local-minor-modes",
        "Minor modes currently active in the current buffer.",
    ),
    (
        "locale-coding-system",
        "Coding system to use with system messages.",
    ),
    (
        "long-line-optimizations-bol-search-limit",
        "Limit for beginning of line search in buffers with long lines.",
    ),
    (
        "long-line-optimizations-region-size",
        "Region size for narrowing in buffers with long lines.",
    ),
    (
        "long-line-threshold",
        "Line length above which to use redisplay shortcuts.",
    ),
    (
        "lread--unescaped-character-literals",
        "List of deprecated unescaped character literals encountered by `read'.",
    ),
    (
        "lucid--menu-grab-keyboard",
        "If non-nil, grab keyboard during menu operations.",
    ),
    (
        "macroexp--dynvars",
        "List of variables declared dynamic in the current scope.",
    ),
    (
        "main-thread",
        "The main thread of Emacs.",
    ),
    (
        "make-cursor-line-fully-visible",
        "Whether to scroll the window if the cursor line is not fully visible.",
    ),
    (
        "make-pointer-invisible",
        "If non-nil, make mouse pointer invisible while typing.",
    ),
    (
        "make-window-start-visible",
        "Whether to ensure `window-start' position is never invisible.",
    ),
    (
        "mark-active",
        "Non-nil means the mark and region are currently active in this buffer.",
    ),
    (
        "mark-even-if-inactive",
        "Non-nil means you can use the mark even when inactive.",
    ),
    (
        "max-image-size",
        "Maximum size of images.",
    ),
    (
        "max-lisp-eval-depth",
        "Limit on depth in `eval', `apply' and `funcall' before error.",
    ),
    (
        "max-mini-window-height",
        "Maximum height for resizing mini-windows (the minibuffer and the echo area).",
    ),
    (
        "max-redisplay-ticks",
        "Maximum number of redisplay ticks before aborting redisplay of a window.",
    ),
    (
        "maximum-scroll-margin",
        "Maximum effective value of `scroll-margin'.",
    ),
    (
        "memory-full",
        "Non-nil means Emacs cannot get much more Lisp memory.",
    ),
    (
        "memory-signal-data",
        "Precomputed `signal' argument for memory-full error.",
    ),
    (
        "menu-bar-final-items",
        "List of menu bar items to move to the end of the menu bar.",
    ),
    (
        "menu-bar-mode",
        "Non-nil if Menu-Bar mode is enabled.",
    ),
    (
        "menu-bar-update-hook",
        "Normal hook run to update the menu bar definitions.",
    ),
    (
        "menu-prompt-more-char",
        "Character to see next line of menu prompt.",
    ),
    (
        "menu-prompting",
        "Non-nil means prompt with menus when appropriate.",
    ),
    (
        "menu-updating-frame",
        "Frame for which we are updating a menu.",
    ),
    (
        "message-log-max",
        "Maximum number of lines to keep in the message log buffer.",
    ),
    (
        "message-truncate-lines",
        "If non-nil, messages are truncated when displaying the echo area.",
    ),
    (
        "messages-buffer-name",
        "The name of the buffer where messages are logged.",
    ),
    (
        "meta-prefix-char",
        "Meta-prefix character code.",
    ),
    (
        "minibuffer-allow-text-properties",
        "Non-nil means `read-from-minibuffer' should not discard text properties.",
    ),
    (
        "minibuffer-auto-raise",
        "Non-nil means entering the minibuffer raises the minibuffer's frame.",
    ),
    (
        "minibuffer-completing-file-name",
        "Non-nil means completing file names.",
    ),
    (
        "minibuffer-completion-confirm",
        "Whether to demand confirmation of completion before exiting minibuffer.",
    ),
    (
        "minibuffer-completion-predicate",
        "Within call to `completing-read', this holds the PREDICATE argument.",
    ),
    (
        "minibuffer-completion-table",
        "Alist or obarray used for completion in the minibuffer.",
    ),
    (
        "minibuffer-exit-hook",
        "Normal hook run whenever a minibuffer is exited.",
    ),
    (
        "minibuffer-follows-selected-frame",
        "t means the active minibuffer always displays on the selected frame.",
    ),
    (
        "minibuffer-help-form",
        "Value that `help-form' takes on inside the minibuffer.",
    ),
    (
        "minibuffer-history-position",
        "Current position of redoing in the history list.",
    ),
    (
        "minibuffer-history-variable",
        "History list symbol to add minibuffer values to.",
    ),
    (
        "minibuffer-local-map",
        "Default keymap to use when reading from the minibuffer.",
    ),
    (
        "minibuffer-message-timeout",
        "How long to display an echo-area message when the minibuffer is active.",
    ),
    (
        "minibuffer-prompt-properties",
        "Text properties that are added to minibuffer prompts.",
    ),
    (
        "minibuffer-scroll-window",
        "Non-nil means it is the window that C-M-v in minibuffer should scroll.",
    ),
    (
        "minibuffer-setup-hook",
        "Normal hook run just after entry to minibuffer.",
    ),
    (
        "minor-mode-map-alist",
        "Alist of keymaps to use for minor modes.",
    ),
    (
        "minor-mode-overriding-map-alist",
        "Alist of keymaps to use for minor modes, in current major mode.",
    ),
    (
        "mode-line-compact",
        "Non-nil means that mode lines should be compact.",
    ),
    (
        "mode-line-in-non-selected-windows",
        "Non-nil means to use `mode-line-inactive' face in non-selected windows.",
    ),
    (
        "mode-name",
        "Pretty name of current buffer's major mode.",
    ),
    (
        "module-file-suffix",
        "Suffix of loadable module file, or nil if modules are not supported.",
    ),
    (
        "most-negative-fixnum",
        "The least integer that is represented efficiently.",
    ),
    (
        "most-positive-fixnum",
        "The greatest integer that is represented efficiently.",
    ),
    (
        "mouse-autoselect-window",
        "Non-nil means autoselect window with mouse pointer.",
    ),
    (
        "mouse-fine-grained-tracking",
        "Non-nil for pixelwise mouse-movement.",
    ),
    (
        "mouse-highlight",
        "If non-nil, clickable text is highlighted when mouse is over it.",
    ),
    (
        "mouse-leave-buffer-hook",
        "Hook run when the user mouse-clicks in a window.",
    ),
    (
        "mouse-position-function",
        "If non-nil, function to transform normal value of `mouse-position'.",
    ),
    (
        "mouse-prefer-closest-glyph",
        "Non-nil means mouse click position is taken from glyph closest to click.",
    ),
    (
        "move-frame-functions",
        "Functions run after a frame was moved.",
    ),
    (
        "movemail-program-name",
        "Name of the `movemail' program distributed with Emacs.",
    ),
    (
        "multibyte-syntax-as-symbol",
        "Non-nil means `scan-sexps' treats all multibyte characters as symbol.",
    ),
    (
        "multiple-frames",
        "Non-nil if more than one frame is visible on this display.",
    ),
    (
        "mwheel-coalesce-scroll-events",
        "Non-nil means send a wheel event only for scrolling at least one screen line.",
    ),
    (
        "native-comp-eln-load-path",
        "List of directories to look for natively-compiled *.eln files.",
    ),
    (
        "native-comp-enable-subr-trampolines",
        "If non-nil, enable generation of trampolines for calling primitives.",
    ),
    (
        "native-comp-jit-compilation",
        "If non-nil, compile loaded .elc files asynchronously.",
    ),
    (
        "network-coding-system-alist",
        "Alist to decide a coding system to use for a network I/O operation.",
    ),
    (
        "next-screen-context-lines",
        "Number of lines of continuity when scrolling by screenfuls.",
    ),
    (
        "no-redraw-on-reenter",
        "Non-nil means no need to redraw entire frame after suspending.",
    ),
    (
        "nobreak-char-ascii-display",
        "Control display of non-ASCII space and hyphen chars.",
    ),
    (
        "nobreak-char-display",
        "Control highlighting of non-ASCII space and hyphen chars.",
    ),
    (
        "num-input-keys",
        "Number of complete key sequences read as input so far.",
    ),
    (
        "num-nonmacro-input-events",
        "Number of input events read from the keyboard so far.",
    ),
    (
        "obarray",
        "Symbol table for use by ‘intern’ and ‘read’.",
    ),
    (
        "open-paren-in-column-0-is-defun-start",
        "Non-nil means an open paren in column 0 denotes the start of a defun.",
    ),
    (
        "operating-system-release",
        "The kernel version of the operating system on which Emacs is running.",
    ),
    (
        "otf-script-alist",
        "Alist of OpenType script tags vs the corresponding script names.",
    ),
    (
        "other-window-scroll-buffer",
        "If this is a live buffer, C-M-v should scroll its window.",
    ),
    (
        "other-window-scroll-default",
        "Function that provides the window to scroll by C-M-v.",
    ),
    (
        "overflow-newline-into-fringe",
        "Non-nil means that newline may flow into the right fringe.",
    ),
    (
        "overlay-arrow-position",
        "Marker for where to display an arrow on top of the buffer text.",
    ),
    (
        "overlay-arrow-string",
        "String to display as an arrow in text-mode frames.",
    ),
    (
        "overlay-arrow-variable-list",
        "List of variables (symbols) which hold markers for overlay arrows.",
    ),
    (
        "overline-margin",
        "Space between overline and text, in pixels.",
    ),
    (
        "overriding-local-map",
        "Keymap that replaces (overrides) local keymaps.",
    ),
    (
        "overriding-local-map-menu-flag",
        "Non-nil means ‘overriding-local-map’ applies to the menu bar.",
    ),
    (
        "overriding-plist-environment",
        "An alist that overrides the plists of the symbols which it lists.",
    ),
    (
        "overriding-terminal-local-map",
        "Per-terminal keymap that takes precedence over all other keymaps.",
    ),
    (
        "overriding-text-conversion-style",
        "Non-buffer local version of ‘text-conversion-style’.",
    ),
    (
        "overwrite-mode",
        "Non-nil if self-insertion should replace existing text.",
    ),
    (
        "parse-sexp-ignore-comments",
        "Non-nil means ‘forward-sexp’, etc., should treat comments as whitespace.",
    ),
    (
        "parse-sexp-lookup-properties",
        "Non-nil means ‘forward-sexp’, etc., obey ‘syntax-table’ property.",
    ),
    (
        "path-separator",
        "String containing the character that separates directories in",
    ),
    (
        "pdumper-fingerprint",
        "The fingerprint of this Emacs binary.",
    ),
    (
        "point-before-scroll",
        "Value of point before the last series of scroll operations, or nil.",
    ),
    (
        "polling-period",
        "Interval between polling for input during Lisp execution.",
    ),
    (
        "post-command-hook",
        "Normal hook run after each command is executed.",
    ),
    (
        "post-gc-hook",
        "Hook run after garbage collection has finished.",
    ),
    (
        "post-select-region-hook",
        "Abnormal hook run after the region is selected.",
    ),
    (
        "post-self-insert-hook",
        "Hook run at the end of ‘self-insert-command’.",
    ),
    (
        "pre-command-hook",
        "Normal hook run before each command is executed.",
    ),
    (
        "pre-redisplay-function",
        "Function run just before redisplay.",
    ),
    (
        "prefix-arg",
        "The value of the prefix argument for the next editing command.",
    ),
    (
        "prefix-help-command",
        "Command to run when ‘help-char’ character follows a prefix key.",
    ),
    (
        "preloaded-file-list",
        "List of files that were preloaded (when dumping Emacs).",
    ),
    (
        "print-charset-text-property",
        "A flag to control printing of ‘charset’ text property on printing a string.",
    ),
    (
        "print-circle",
        "Non-nil means print recursive structures using #N= and #N# syntax.",
    ),
    (
        "print-continuous-numbering",
        "Non-nil means number continuously across print calls.",
    ),
    (
        "print-escape-control-characters",
        "Non-nil means print control characters in strings as ‘\\OOO’.",
    ),
    (
        "print-escape-multibyte",
        "Non-nil means print multibyte characters in strings as \\xXXXX.",
    ),
    (
        "print-escape-newlines",
        "Non-nil means print newlines in strings as ‘\\n’.",
    ),
    (
        "print-escape-nonascii",
        "Non-nil means print unibyte non-ASCII chars in strings as \\OOO.",
    ),
    (
        "print-gensym",
        "Non-nil means print uninterned symbols so they will read as uninterned.",
    ),
    (
        "print-integers-as-characters",
        "Non-nil means integers are printed using characters syntax.",
    ),
    (
        "print-number-table",
        "A vector used internally to produce ‘#N=’ labels and ‘#N#’ references.",
    ),
    (
        "print-quoted",
        "Non-nil means print quoted forms with reader syntax.",
    ),
    (
        "print-symbols-bare",
        "A flag to control printing of symbols with position.",
    ),
    (
        "print-unreadable-function",
        "If non-nil, a function to call when printing unreadable objects.",
    ),
    (
        "printable-chars",
        "A char-table for each printable character.",
    ),
    (
        "process-adaptive-read-buffering",
        "If non-nil, improve receive buffering by delaying after short reads.",
    ),
    (
        "process-coding-system-alist",
        "Alist to decide a coding system to use for a process I/O operation.",
    ),
    (
        "process-connection-type",
        "Control type of device used to communicate with subprocesses.",
    ),
    (
        "process-environment",
        "List of overridden environment variables for subprocesses to inherit.",
    ),
    (
        "process-error-pause-time",
        "The number of seconds to pause after handling process errors.",
    ),
    (
        "process-prioritize-lower-fds",
        "Whether to start checking for subprocess output from first file descriptor.",
    ),
    (
        "profiler-log-size",
        "Number of distinct call-stacks that can be recorded in a profiler log.",
    ),
    (
        "profiler-max-stack-depth",
        "Number of elements from the call-stack recorded in the log.",
    ),
    (
        "pure-bytes-used",
        "Number of bytes of shareable Lisp data allocated so far.",
    ),
    (
        "purify-flag",
        "Non-nil means loading Lisp code in order to dump an executable.",
    ),
    (
        "query-all-font-backends",
        "If non-nil, attempt to query all available font backends.",
    ),
    (
        "quit-flag",
        "Non-nil causes ‘eval’ to abort, unless ‘inhibit-quit’ is non-nil.",
    ),
    (
        "rcs2log-program-name",
        "Name of the ‘rcs2log’ program distributed with Emacs.",
    ),
    (
        "read-buffer-completion-ignore-case",
        "Non-nil means completion ignores case when reading a buffer name.",
    ),
    (
        "read-buffer-function",
        "If this is non-nil, ‘read-buffer’ does its work by calling this function.",
    ),
    (
        "read-circle",
        "Non-nil means read recursive structures using #N= and #N# syntax.",
    ),
    (
        "read-expression-history",
        "A history list for arguments that are Lisp expressions to evaluate.",
    ),
    (
        "read-hide-char",
        "Whether to hide input characters in noninteractive mode.",
    ),
    (
        "read-minibuffer-restore-windows",
        "Non-nil means restore window configurations on exit from minibuffer.",
    ),
    (
        "read-process-output-max",
        "Maximum number of bytes to read from subprocess in a single chunk.",
    ),
    (
        "read-symbol-shorthands",
        "Alist of known symbol-name shorthands.",
    ),
    (
        "real-last-command",
        "Same as ‘last-command’, but never altered by Lisp code.",
    ),
    (
        "real-this-command",
        "This is like ‘this-command’, except that commands should never modify it.",
    ),
    (
        "recenter-redisplay",
        "Non-nil means ‘recenter’ redraws entire frame.",
    ),
    (
        "record-all-keys",
        "Non-nil means record all keys you type.",
    ),
    (
        "redisplay--all-windows-cause",
        "Code of the cause for redisplaying all windows.",
    ),
    (
        "redisplay--inhibit-bidi",
        "Non-nil means it is not safe to attempt bidi reordering for display.",
    ),
    (
        "redisplay--mode-lines-cause",
        "Code of the cause for redisplaying mode lines.",
    ),
    (
        "redisplay-adhoc-scroll-in-resize-mini-windows",
        "If nil always use normal scrolling in minibuffer windows.",
    ),
    (
        "redisplay-dont-pause",
        "Nil means display update is paused when input is detected.",
    ),
    (
        "redisplay-skip-fontification-on-input",
        "Skip ‘fontification_functions‘ when there is input pending.",
    ),
    (
        "redisplay-skip-initial-frame",
        "Non-nil means skip redisplay of the initial frame.",
    ),
    (
        "region-extract-function",
        "Function to get the region’s content.",
    ),
    (
        "report-emacs-bug-address",
        "Address of mailing list for GNU Emacs bugs.",
    ),
    (
        "resize-mini-frames",
        "Non-nil means resize minibuffer-only frames automatically.",
    ),
    (
        "resize-mini-windows",
        "How to resize mini-windows (the minibuffer and the echo area).",
    ),
    (
        "resume-tty-functions",
        "Functions run after resuming a tty.",
    ),
    (
        "right-fringe-width",
        "Width of this buffer’s right fringe (in pixels).",
    ),
    (
        "right-margin-width",
        "Width in columns of right marginal area for display of a buffer.",
    ),
    (
        "ring-bell-function",
        "Non-nil means call this function to ring the bell.",
    ),
    (
        "saved-region-selection",
        "Contents of active region prior to buffer modification.",
    ),
    (
        "scalable-fonts-allowed",
        "Allowed scalable fonts.",
    ),
    (
        "script-representative-chars",
        "Alist of scripts vs the representative characters.",
    ),
    (
        "scroll-bar-adjust-thumb-portion",
        "Adjust scroll bars for overscrolling for Gtk+, Motif and Haiku.",
    ),
    (
        "scroll-bar-height",
        "Height of this buffer’s horizontal scroll bars in pixels.",
    ),
    (
        "scroll-bar-width",
        "Width of this buffer’s vertical scroll bars in pixels.",
    ),
    (
        "scroll-conservatively",
        "Scroll up to this many lines, to bring point back on screen.",
    ),
    (
        "scroll-down-aggressively",
        "How far to scroll windows downward.",
    ),
    (
        "scroll-margin",
        "Number of lines of margin at the top and bottom of a window.",
    ),
    (
        "scroll-minibuffer-conservatively",
        "Non-nil means scroll conservatively in minibuffer windows.",
    ),
    (
        "scroll-preserve-screen-position",
        "Controls if scroll commands move point to keep its screen position unchanged.",
    ),
    (
        "scroll-step",
        "The number of lines to try scrolling a window by when point moves out.",
    ),
    (
        "scroll-up-aggressively",
        "How far to scroll windows upward.",
    ),
    (
        "search-spaces-regexp",
        "Regexp to substitute for bunches of spaces in regexp search.",
    ),
    (
        "select-safe-coding-system-function",
        "Function to call to select safe coding system for encoding a text.",
    ),
    (
        "selection-converter-alist",
        "An alist associating X Windows selection-types with functions.",
    ),
    (
        "selection-inhibit-update-commands",
        "List of commands which should not update the selection.",
    ),
    (
        "selective-display",
        "Non-nil enables selective display.",
    ),
    (
        "selective-display-ellipses",
        "Non-nil means display ... on previous line when a line is invisible.",
    ),
    (
        "set-auto-coding-function",
        "If non-nil, a function to call to decide a coding system of file.",
    ),
    (
        "set-message-function",
        "If non-nil, function to handle display of echo-area messages.",
    ),
    (
        "shared-game-score-directory",
        "Directory of score files for games which come with GNU Emacs.",
    ),
    (
        "show-help-function",
        "If non-nil, the function that implements the display of help.",
    ),
    (
        "show-trailing-whitespace",
        "Non-nil means highlight trailing whitespace.",
    ),
    (
        "signal-hook-function",
        "If non-nil, this is a function for ‘signal’ to call.",
    ),
    ("kill-ring", "List of killed text sequences."),
    (
        "kill-ring-yank-pointer",
        "The tail of the kill ring whose car is the last thing yanked.",
    ),
    ("last-command", "The last command executed."),
    ("lexical-binding", "Whether to use lexical binding when evaluating code."),
    (
        "load-file-name",
        "Full name of file being loaded by `load'.",
    ),
    (
        "load-history",
        "Alist mapping loaded file names to symbols and features.",
    ),
    (
        "load-path",
        "List of directories to search for files to load.\n\
Each element is a string (directory name) or nil (try default directory).",
    ),
    (
        "load-prefer-newer",
        "Non-nil means `load' prefers the newest version of a file.",
    ),
    ("major-mode", "Symbol for current buffer's major mode."),
    (
        "mode-line-format",
        "Template for displaying mode line for a window's buffer.",
    ),
    (
        "noninteractive",
        "Non-nil means Emacs is running without interactive terminal.",
    ),
    (
        "print-length",
        "Maximum length of list to print before abbreviating.",
    ),
    (
        "print-level",
        "Maximum depth of list nesting to print before abbreviating.",
    ),
    (
        "select-active-regions",
        "If non-nil, any active region automatically sets the primary selection.",
    ),
    (
        "shell-file-name",
        "File name to load inferior shells from.",
    ),
    (
        "standard-output",
        "Output stream `print' uses by default for outputting a character.",
    ),
    ("tab-bar-mode", "Non-nil if Tab-Bar mode is enabled."),
    ("this-command", "The command now being executed."),
    ("tool-bar-mode", "Non-nil if Tool-Bar mode is enabled."),
    (
        "transient-mark-mode",
        "Non-nil if Transient Mark mode is enabled.",
    ),
    (
        "truncate-lines",
        "Non-nil means do not display continuation lines.",
    ),
    (
        "undo-limit",
        "Keep no more undo information once it exceeds this size.",
    ),
    (
        "undo-strong-limit",
        "Don't keep more than this much size of undo information.",
    ),
    (
        "unread-command-events",
        "List of events to be read as the command input.",
    ),
    (
        "unread-input-method-events",
        "List of events to be processed as input by input methods.",
    ),
    ("user-full-name", "The full name of the user logged in."),
    (
        "window-system",
        "Name of window system through which the selected frame is displayed.",
    ),
    (
        "word-wrap",
        "Non-nil means to use word-wrapping for continuation lines.",
    ),
];

fn startup_variable_doc_stub(sym: &str) -> Option<&'static str> {
    STARTUP_VARIABLE_DOC_STUBS
        .iter()
        .find_map(|(name, doc)| (*name == sym).then_some(*doc))
}

/// `(describe-function FUNCTION)` -- return a short description string.
///
/// This is a simplified stub that returns a type description of the function.
/// In real Emacs, `describe-function` opens a *Help* buffer with detailed
/// information.
pub(crate) fn builtin_describe_function(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("describe-function", &args, 1)?;

    let name = match args[0].as_symbol_name() {
        Some(n) => n.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            ));
        }
    };

    // Reuse symbol-function resolution so builtins and wrapper-backed callables
    // are visible here even when not explicitly interned in the function cell.
    let func_val = super::builtins::builtin_symbol_function(eval, vec![Value::symbol(name.clone())])?;
    if func_val.is_nil() {
        return Err(signal("void-function", vec![Value::symbol(&name)]));
    }

    if let Some(alias_name) = func_val.as_symbol_name() {
        if alias_name != name {
            let indirect =
                super::builtins::builtin_indirect_function(eval, vec![Value::symbol(alias_name)]);
            let text = match indirect {
                Ok(value) if !value.is_nil() => {
                    format!("{name} is an alias for `{alias_name}`.")
                }
                _ => format!(
                    "{name} is an alias for `{alias_name}`, which is not known to be defined."
                ),
            };
            return Ok(Value::string(text));
        }
    }

    let description = describe_function_kind(&func_val);

    Ok(Value::string(description))
}

fn describe_function_kind(function: &Value) -> &'static str {
    match function {
        Value::Lambda(_) => "Lisp function",
        Value::Macro(_) => "Lisp macro",
        Value::Subr(name) => {
            if super::subr_info::is_special_form(name) {
                "Special-form"
            } else {
                "Built-in function"
            }
        }
        Value::ByteCode(_) => "Compiled Lisp function",
        Value::Str(_) | Value::Vector(_) => "Keyboard macro",
        Value::Cons(cell) => {
            if super::autoload::is_autoload_value(function) {
                if let Some(items) = list_to_vec(function) {
                    if matches!(items.get(4).and_then(Value::as_symbol_name), Some("macro")) {
                        return "Lisp macro";
                    }
                }
                return "Lisp function";
            }

            let pair = match cell.lock() {
                Ok(pair) => pair,
                Err(_) => return "Lisp function",
            };
            match pair.car.as_symbol_name() {
                Some("macro") => "Lisp macro",
                Some("lambda") => "Lisp function",
                _ => "Lisp function",
            }
        }
        _ => "Lisp function",
    }
}

/// `(describe-variable VARIABLE)` -- return the documentation string for a
/// variable.
///
/// Looks up the `:variable-documentation` property on the symbol's plist.
/// Returns the documentation string if found, or a generic description if the
/// variable is bound but has no documentation, or signals `void-variable` if
/// the variable is not bound.
pub(crate) fn builtin_describe_variable(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("describe-variable", &args, 1, 3)?;

    let name = match args[0].as_symbol_name() {
        Some(n) => n.to_string(),
        None => {
            return Err(signal(
                "user-error",
                vec![Value::string("You didn’t specify a variable")],
            ));
        }
    };

    // Check for :variable-documentation property.
    if let Some(doc_val) = eval
        .obarray
        .get_property(&name, "variable-documentation")
        .cloned()
    {
        match doc_val {
            Value::Str(_) => return Ok(doc_val),
            Value::Int(_) => {
                return Ok(Value::string(format!(
                    "{name} is a variable defined in `C source code`."
                )));
            }
            Value::Symbol(sym) => {
                if !eval.obarray.boundp(&sym) {
                    return Err(signal("void-variable", vec![Value::symbol(sym)]));
                }
                let sym_value = eval.obarray.symbol_value(&sym).cloned().unwrap_or(Value::Nil);
                return match sym_value {
                    Value::Str(_) | Value::Int(_) => Ok(describe_variable_value_or_void(eval, &name)),
                    other => Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("char-or-string-p"), other],
                    )),
                };
            }
            Value::True => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("char-or-string-p"), Value::True],
                ));
            }
            _ => return Ok(describe_variable_value_or_void(eval, &name)),
        }
    }

    Ok(describe_variable_value_or_void(eval, &name))
}

fn describe_variable_value_or_void(eval: &super::eval::Evaluator, name: &str) -> Value {
    let value = if name.starts_with(':') {
        Some(Value::keyword(name))
    } else if eval.obarray.boundp(name) {
        Some(
            eval.obarray
                .symbol_value(name)
                .cloned()
                .unwrap_or(Value::symbol("void")),
        )
    } else {
        None
    };

    if let Some(value) = value {
        let rendered = super::print::print_value(&value);
        let rendered = if matches!(value, Value::Symbol(_) | Value::Keyword(_)) {
            format!("\u{2018}{rendered}\u{2019}")
        } else {
            rendered
        };
        Value::string(format!("{name}\u{2019}s value is {rendered}\n"))
    } else {
        Value::string(format!("{name} is void as a variable.\n"))
    }
}

/// `(documentation-property SYMBOL PROP &optional RAW)` -- return the
/// documentation property PROP of SYMBOL.
///
/// Evaluator-aware implementation:
/// - validates SYMBOL as a symbol designator (`symbolp`)
/// - returns nil when PROP is not a symbol (matching Emacs `get`-like behavior)
/// - unresolved integer doc offsets return nil
/// - non-integer values are evaluated as Lisp and returned
/// - accepts RAW but currently ignores it
pub(crate) fn builtin_documentation_property_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("documentation-property", &args, 2, 3)?;

    let sym = args[0].as_symbol_name().ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        )
    })?;

    let Some(prop) = args[1].as_symbol_name() else {
        return Ok(Value::Nil);
    };

    match eval.obarray.get_property(sym, prop).cloned() {
        Some(value) if startup_variable_doc_offset_symbol(sym, prop, &value) => {
            if let Some(doc) = startup_variable_doc_stub(sym) {
                Ok(Value::string(doc))
            } else {
                Ok(Value::string(format!(
                    "{sym} is a variable defined in `C source code`."
                )))
            }
        }
        Some(value) => eval_documentation_property_value(eval, value),
        _ => Ok(Value::Nil),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(documentation-property SYMBOL PROP &optional RAW)` -- return the
/// documentation property PROP of SYMBOL.
///
/// Stub implementation: always returns nil.  In real Emacs this reads
/// properties like `variable-documentation` and `function-documentation`
/// from the symbol's plist, potentially loading from the DOC file.
pub(crate) fn builtin_documentation_property(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("documentation-property", &args, 2, 3)?;
    // Validate that the first argument is a symbol.
    if args[0].as_symbol_name().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// `(Snarf-documentation FILENAME)` -- load documentation strings from
/// the internal DOC file.
///
/// Compatibility implementation: accepts the canonical `"DOC"` token and
/// preserves observed GNU Emacs error classes for invalid and missing paths.
/// It does not load or parse an on-disk DOC table yet.
fn snarf_doc_path_invalid(filename: &str) -> bool {
    if filename.is_empty() {
        return true;
    }

    let mut segments = filename.split('/').filter(|segment| !segment.is_empty()).peekable();
    if segments.peek().is_none() {
        return true;
    }

    segments.all(|segment| segment == "." || segment == "..")
}

pub(crate) fn builtin_snarf_documentation(args: Vec<Value>) -> EvalResult {
    expect_args("Snarf-documentation", &args, 1)?;
    let filename = match args[0].as_str() {
        Some(name) => name,
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), args[0].clone()],
            ));
        }
    };

    // In batch compatibility mode, allow the canonical DOC token while
    // preserving observable error classes for invalid/missing names.
    if filename == "DOC" {
        return Ok(Value::Nil);
    }

    if filename.starts_with("DOC/") {
        return Err(signal(
            "file-error",
            vec![
                Value::string("Read error"),
                Value::string(format!("/usr/share/emacs/etc/{filename}")),
            ],
        ));
    }

    if snarf_doc_path_invalid(filename) {
        return Err(signal(
            "error",
            vec![Value::string("DOC file invalid at position 0")],
        ));
    }

    Err(signal(
        "file-missing",
        vec![
            Value::string("Opening doc string file"),
            Value::string("No such file or directory"),
            Value::string(format!("/usr/share/emacs/etc/{filename}")),
        ],
    ))
}

/// `(substitute-command-keys STRING)` -- process special documentation
/// sequences in STRING.
///
/// Recognized sequences:
/// - `\\[COMMAND]` — replaced with the key binding for COMMAND (stripped here)
/// - `\\{KEYMAP}` — replaced with a description of the keymap (stripped here)
/// - `\\<KEYMAP>` — sets the keymap for subsequent `\\[...]` (stripped here)
/// - `\\=` — quote the next character (prevents interpretation)
///
/// This implementation strips the special sequences, returning the plain
/// text content.  A full implementation would resolve key bindings and
/// format keymap descriptions.
pub(crate) fn builtin_substitute_command_keys(args: Vec<Value>) -> EvalResult {
    expect_args("substitute-command-keys", &args, 1)?;

    let input = match args[0].as_str() {
        Some(s) => s,
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), args[0].clone()],
            ));
        }
    };

    let mut result = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.peek() {
                Some('[') => {
                    // \\[COMMAND] — skip until closing ']'
                    chars.next(); // consume '['
                    let mut command = String::new();
                    for c in chars.by_ref() {
                        if c == ']' {
                            break;
                        }
                        command.push(c);
                    }
                    // Replace with a placeholder showing the command name.
                    result.push_str(&format!("M-x {}", command));
                }
                Some('{') => {
                    // \\{KEYMAP} — skip until closing '}'
                    chars.next(); // consume '{'
                    for c in chars.by_ref() {
                        if c == '}' {
                            break;
                        }
                    }
                    // Omit keymap description entirely.
                }
                Some('<') => {
                    // \\<KEYMAP> — skip until closing '>'
                    chars.next(); // consume '<'
                    for c in chars.by_ref() {
                        if c == '>' {
                            break;
                        }
                    }
                    // Silently consumed (sets keymap context).
                }
                Some('=') => {
                    // \\= — quote next character literally.
                    chars.next(); // consume '='
                    if let Some(next) = chars.next() {
                        result.push(next);
                    }
                }
                Some('\\') => {
                    // Literal backslash (\\\\).
                    chars.next();
                    result.push('\\');
                }
                _ => {
                    // Not a recognized sequence; keep the backslash.
                    result.push(ch);
                }
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::string(result))
}

/// `(help-function-arglist FUNCTION &optional PRESERVE-NAMES)` -- return
/// the argument list of FUNCTION.
///
/// This returns oracle-compatible arglists for core thread/mutex/condition
/// primitives and for lambda-like objects. For unresolved shapes, Emacs
/// returns `t`, which we mirror here.
pub(crate) fn builtin_help_function_arglist(args: Vec<Value>) -> EvalResult {
    expect_min_max_args("help-function-arglist", &args, 1, 2)?;
    let preserve_names = args.get(1).is_some_and(Value::is_truthy);

    if let Some(result) = help_arglist_from_quoted_designator(&args[0]) {
        return result;
    }

    if let Some(arglist) = help_arglist_from_value(&args[0], preserve_names) {
        return Ok(arglist);
    }

    Ok(Value::True)
}

pub(crate) fn builtin_help_function_arglist_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_max_args("help-function-arglist", &args, 1, 2)?;
    let preserve_names = args.get(1).is_some_and(Value::is_truthy);

    if let Some(name) = args[0].as_symbol_name() {
        let resolved =
            super::builtins::builtin_indirect_function(eval, vec![Value::symbol(name)])?;
        if let Some(result) = help_arglist_from_quoted_designator(&resolved) {
            return result;
        }
        if let Some(arglist) = help_arglist_from_value(&resolved, preserve_names) {
            return Ok(arglist);
        }
        if let Value::Subr(subr_name) = &resolved {
            if let Some(arglist) = help_arglist_from_subr_arity(subr_name) {
                return Ok(arglist);
            }
        }
        if super::autoload::is_autoload_value(&resolved) {
            return Ok(Value::string(
                "[Arg list not available until function definition is loaded.]",
            ));
        }
    }

    if let Some(result) = help_arglist_from_quoted_designator(&args[0]) {
        return result;
    }

    if let Some(arglist) = help_arglist_from_value(&args[0], preserve_names) {
        return Ok(arglist);
    }

    Ok(Value::True)
}

fn help_arglist_from_quoted_designator(function: &Value) -> Option<EvalResult> {
    let Value::Cons(cell) = function else {
        return None;
    };

    let pair = cell.lock().ok()?;
    let Some(head) = pair.car.as_symbol_name() else {
        return None;
    };

    match head {
        "macro" => {
            // GNU Emacs returns nil for the exact quoted shape `(macro lambda)`.
            if let Value::Cons(payload_cell) = pair.cdr.clone() {
                if let Ok(payload) = payload_cell.lock() {
                    if payload.car.as_symbol_name() == Some("lambda") && payload.cdr.is_nil() {
                        return Some(Ok(Value::Nil));
                    }
                }
            }
            Some(Ok(Value::True))
        }
        "lambda" => match pair.cdr.clone() {
            Value::Nil => Some(Ok(Value::Nil)),
            Value::Cons(arg_cell) => {
                let args = arg_cell.lock().ok()?;
                Some(Ok(args.car.clone()))
            }
            other => Some(Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), other],
            ))),
        },
        _ => None,
    }
}

fn help_arglist(required: &[&str], optional: &[&str], rest: Option<&str>) -> Value {
    if required.is_empty() && optional.is_empty() && rest.is_none() {
        return Value::Nil;
    }

    let mut out = Vec::new();
    for name in required {
        out.push(Value::symbol(*name));
    }
    if !optional.is_empty() {
        out.push(Value::symbol("&optional"));
        for name in optional {
            out.push(Value::symbol(*name));
        }
    }
    if let Some(name) = rest {
        out.push(Value::symbol("&rest"));
        out.push(Value::symbol(name));
    }
    Value::list(out)
}

fn help_arglist_from_lambda_params(params: &LambdaParams) -> Value {
    let required: Vec<&str> = params.required.iter().map(String::as_str).collect();
    let optional: Vec<&str> = params.optional.iter().map(String::as_str).collect();
    let rest = params.rest.as_deref();
    help_arglist(&required, &optional, rest)
}

fn help_arglist_from_subr_name(name: &str, preserve_names: bool) -> Option<Value> {
    if name == "car" {
        return Some(if preserve_names {
            help_arglist(&["list"], &[], None)
        } else {
            help_arglist(&["arg1"], &[], None)
        });
    }

    if name == "cdr" {
        return Some(if preserve_names {
            help_arglist(&["list"], &[], None)
        } else {
            help_arglist(&["arg1"], &[], None)
        });
    }

    if name == "cons" {
        return Some(if preserve_names {
            help_arglist(&["car", "cdr"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if name == "list" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("objects"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "if" {
        return Some(help_arglist(&["arg1", "arg2"], &[], Some("rest")));
    }

    if name == "documentation" {
        return Some(if preserve_names {
            help_arglist(&["function"], &["raw"], None)
        } else {
            help_arglist(&["arg1"], &["arg2"], None)
        });
    }

    if name == "eq" {
        return Some(if preserve_names {
            help_arglist(&["obj1", "obj2"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if name == "min" || name == "max" {
        return Some(if preserve_names {
            help_arglist(&["number-or-marker"], &[], Some("numbers-or-markers"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if name == "message" {
        return Some(if preserve_names {
            help_arglist(&["format-string"], &[], Some("args"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if name == "concat" || name == "append" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("sequences"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "nconc" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("lists"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "vector" {
        return Some(if preserve_names {
            help_arglist(&[], &[], Some("objects"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    if name == "format" {
        return Some(if preserve_names {
            help_arglist(&["string"], &[], Some("objects"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if name == "apply" {
        return Some(if preserve_names {
            help_arglist(&["function"], &[], Some("arguments"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if preserve_names && name == "assq" {
        return Some(help_arglist(&["key", "alist"], &[], None));
    }

    if preserve_names && name == "assoc" {
        return Some(help_arglist(&["key", "alist"], &["testfn"], None));
    }

    if preserve_names && (name == "memq" || name == "member") {
        return Some(help_arglist(&["elt", "list"], &[], None));
    }

    if name == "equal" {
        return Some(if preserve_names {
            help_arglist(&["o1", "o2"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if preserve_names && name == "string-match" {
        return Some(help_arglist(
            &["regexp", "string"],
            &["start", "inhibit-modify"],
            None,
        ));
    }

    if name == "substring" {
        return Some(if preserve_names {
            help_arglist(&["string"], &["from", "to"], None)
        } else {
            help_arglist(&["arg1"], &["arg2", "arg3"], None)
        });
    }

    if name == "aref" {
        return Some(if preserve_names {
            help_arglist(&["array", "idx"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2"], &[], None)
        });
    }

    if name == "aset" {
        return Some(if preserve_names {
            help_arglist(&["array", "idx", "newelt"], &[], None)
        } else {
            help_arglist(&["arg1", "arg2", "arg3"], &[], None)
        });
    }

    if name == "make-string" {
        return Some(if preserve_names {
            help_arglist(&["length", "init"], &["multibyte"], None)
        } else {
            help_arglist(&["arg1", "arg2"], &["arg3"], None)
        });
    }

    if preserve_names && name == "read-from-string" {
        return Some(help_arglist(&["string"], &["start", "end"], None));
    }

    if name == "funcall" {
        return Some(if preserve_names {
            help_arglist(&["function"], &[], Some("arguments"))
        } else {
            help_arglist(&["arg1"], &[], Some("rest"))
        });
    }

    if preserve_names && name == "mapcar" {
        return Some(Value::list(vec![
            Value::symbol("function"),
            Value::symbol("sequence"),
        ]));
    }

    if name == "symbol-function" || name == "fboundp" {
        return Some(if preserve_names {
            help_arglist(&["symbol"], &[], None)
        } else {
            help_arglist(&["arg1"], &[], None)
        });
    }

    if name == "substitute-command-keys" {
        return Some(if preserve_names {
            help_arglist(&["string"], &["no-face", "include-menus"], None)
        } else {
            help_arglist(&["arg1"], &["arg2", "arg3"], None)
        });
    }

    if name == "where-is-internal" {
        return Some(if preserve_names {
            help_arglist(
                &["definition"],
                &["keymap", "firstonly", "noindirect", "no-remap"],
                None,
            )
        } else {
            help_arglist(&["arg1"], &["arg2", "arg3", "arg4", "arg5"], None)
        });
    }

    if name == "read" {
        return Some(if preserve_names {
            help_arglist(&[], &["stream"], None)
        } else {
            help_arglist(&[], &["arg1"], None)
        });
    }

    if name == "-" {
        return Some(if preserve_names {
            help_arglist(&[], &["number-or-marker"], Some("more-numbers-or-markers"))
        } else {
            help_arglist(&[], &[], Some("rest"))
        });
    }

    let (required, optional) = match name {
        "thread-join" => (
            if preserve_names {
                vec!["thread"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "thread-yield" => (vec![], vec![]),
        "defining-kbd-macro" => (
            if preserve_names {
                vec!["append"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["no-exec"]
            } else {
                vec!["arg2"]
            },
        ),
        "help-key-description" => (
            if preserve_names {
                vec!["key", "untranslated"]
            } else {
                vec!["arg1", "arg2"]
            },
            vec![],
        ),
        "recent-keys" => (
            vec![],
            if preserve_names {
                vec!["include-cmds"]
            } else {
                vec!["arg1"]
            },
        ),
        "event-apply-modifier" => (
            if preserve_names {
                vec!["event", "symbol", "lshiftby", "prefix"]
            } else {
                vec!["arg1", "arg2", "arg3", "arg4"]
            },
            vec![],
        ),
        "thread-name" => (
            if preserve_names {
                vec!["thread"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "thread-live-p" => (
            if preserve_names {
                vec!["thread"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "thread-signal" => (
            if preserve_names {
                vec!["thread", "error-symbol", "data"]
            } else {
                vec!["arg1", "arg2", "arg3"]
            },
            vec![],
        ),
        "thread-last-error" => (
            vec![],
            if preserve_names {
                vec!["cleanup"]
            } else {
                vec!["arg1"]
            },
        ),
        "make-thread" => (
            if preserve_names {
                vec!["function"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["name"]
            } else {
                vec!["arg2"]
            },
        ),
        "make-mutex" => (
            vec![],
            if preserve_names {
                vec!["name"]
            } else {
                vec!["arg1"]
            },
        ),
        "mutexp" => (
            if preserve_names {
                vec!["object"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "mutex-name" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "mutex-lock" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "mutex-unlock" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "make-condition-variable" => (
            if preserve_names {
                vec!["mutex"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["name"]
            } else {
                vec!["arg2"]
            },
        ),
        "condition-variable-p" => (
            if preserve_names {
                vec!["object"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "condition-wait" => (
            if preserve_names {
                vec!["cond"]
            } else {
                vec!["arg1"]
            },
            vec![],
        ),
        "condition-notify" => (
            if preserve_names {
                vec!["cond"]
            } else {
                vec!["arg1"]
            },
            if preserve_names {
                vec!["all"]
            } else {
                vec!["arg2"]
            },
        ),
        "current-thread" | "all-threads" => (vec![], vec![]),
        _ => return None,
    };

    Some(help_arglist(&required, &optional, None))
}

fn help_arglist_from_subr_arity(name: &str) -> Option<Value> {
    let arity = super::subr_info::builtin_subr_arity(vec![Value::Subr(name.to_string())]).ok()?;
    let Value::Cons(cell) = arity else {
        return None;
    };
    let pair = cell.lock().ok()?;
    let min = pair.car.as_int()?;
    if min < 0 {
        return None;
    }
    let min = min as usize;

    let required: Vec<String> = (1..=min).map(|idx| format!("arg{idx}")).collect();

    if pair.cdr.as_symbol_name() == Some("unevalled") || pair.cdr.as_symbol_name() == Some("many") {
        let required_refs: Vec<&str> = required.iter().map(String::as_str).collect();
        return Some(help_arglist(&required_refs, &[], Some("rest")));
    }

    let max = pair.cdr.as_int()?;
    if max < min as i64 {
        return None;
    }
    let max = max as usize;
    let optional: Vec<String> = ((min + 1)..=max).map(|idx| format!("arg{idx}")).collect();
    let required_refs: Vec<&str> = required.iter().map(String::as_str).collect();
    let optional_refs: Vec<&str> = optional.iter().map(String::as_str).collect();
    Some(help_arglist(&required_refs, &optional_refs, None))
}

fn help_arglist_from_value(function: &Value, preserve_names: bool) -> Option<Value> {
    match function {
        Value::Subr(name) => help_arglist_from_subr_name(name, preserve_names),
        Value::Symbol(name) => help_arglist_from_subr_name(name, preserve_names),
        Value::Lambda(lambda) | Value::Macro(lambda) => {
            Some(help_arglist_from_lambda_params(&lambda.params))
        }
        Value::ByteCode(bytecode) => Some(help_arglist_from_lambda_params(&bytecode.params)),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // =======================================================================
    // substitute-command-keys
    // =======================================================================

    #[test]
    fn substitute_plain_string() {
        let result = builtin_substitute_command_keys(vec![Value::string("hello world")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello world"));
    }

    #[test]
    fn substitute_command_key_binding() {
        let result =
            builtin_substitute_command_keys(vec![Value::string("Press \\[save-buffer] to save.")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        assert!(text.contains("save-buffer"));
        assert!(!text.contains("\\["));
        assert!(!text.contains(']'));
    }

    #[test]
    fn substitute_keymap_description() {
        let result =
            builtin_substitute_command_keys(vec![Value::string("Bindings:\\{foo-mode-map}done")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        // The keymap description is stripped entirely.
        assert_eq!(text, "Bindings:done");
    }

    #[test]
    fn substitute_keymap_context() {
        let result =
            builtin_substitute_command_keys(vec![Value::string("\\<foo-map>Press \\[bar] now")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        assert!(!text.contains("\\<"));
        assert!(!text.contains('>'));
        assert!(text.contains("bar"));
    }

    #[test]
    fn substitute_quote_escape() {
        let result = builtin_substitute_command_keys(vec![Value::string(
            "Use \\=\\[not-a-command] literally",
        )]);
        assert!(result.is_ok());
        let s = result.unwrap();
        let text = s.as_str().unwrap();
        // \\= quotes the next char, so \\[ is literal.
        assert!(text.contains("\\[not-a-command]"));
    }

    #[test]
    fn substitute_literal_backslash() {
        let result = builtin_substitute_command_keys(vec![Value::string("path\\\\name")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert_eq!(s.as_str(), Some("path\\name"));
    }

    #[test]
    fn substitute_wrong_type() {
        let result = builtin_substitute_command_keys(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn substitute_wrong_arity() {
        let result = builtin_substitute_command_keys(vec![]);
        assert!(result.is_err());
    }

    // =======================================================================
    // documentation-property (stub)
    // =======================================================================

    #[test]
    fn documentation_property_returns_nil() {
        let result = builtin_documentation_property(vec![
            Value::symbol("foo"),
            Value::symbol("variable-documentation"),
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_property_with_raw() {
        let result = builtin_documentation_property(vec![
            Value::symbol("foo"),
            Value::symbol("variable-documentation"),
            Value::True,
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_property_wrong_type() {
        let result = builtin_documentation_property(vec![
            Value::Int(42),
            Value::symbol("variable-documentation"),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn documentation_property_wrong_arity() {
        let result = builtin_documentation_property(vec![Value::symbol("foo")]);
        assert!(result.is_err());
    }

    // =======================================================================
    // Snarf-documentation runtime/error semantics
    // =======================================================================

    #[test]
    fn snarf_documentation_returns_nil() {
        let result = builtin_snarf_documentation(vec![Value::string("DOC")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn snarf_documentation_wrong_type() {
        let result = builtin_snarf_documentation(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn snarf_documentation_empty_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_parent_dir_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("../")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_single_dot_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string(".")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_root_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("/")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_doc_dir_path_file_error() {
        let result = builtin_snarf_documentation(vec![Value::string("DOC/")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-error"),
            other => panic!("expected file-error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_doc_subpath_file_error() {
        let result = builtin_snarf_documentation(vec![Value::string("DOC/a")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-error"),
            other => panic!("expected file-error signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_missing_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("NO_SUCH_DOC_FILE")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-missing"),
            other => panic!("expected file-missing signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_missing_dir_path_errors() {
        let result = builtin_snarf_documentation(vec![Value::string("NO_SUCH_DOC_DIR/")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "file-missing"),
            other => panic!("expected file-missing signal, got {other:?}"),
        }
    }

    #[test]
    fn snarf_documentation_wrong_arity() {
        let result = builtin_snarf_documentation(vec![]);
        assert!(result.is_err());
    }

    // =======================================================================
    // help-function-arglist
    // =======================================================================

    fn arglist_names(v: &Value) -> Vec<String> {
        list_to_vec(v)
            .expect("arglist must be list")
            .into_iter()
            .map(|item| {
                item.as_symbol_name()
                    .expect("arglist item must be symbol")
                    .to_string()
            })
            .collect()
    }

    #[test]
    fn help_function_arglist_returns_t_for_unknown() {
        let result = builtin_help_function_arglist(vec![Value::symbol("foo")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn help_function_arglist_thread_join_matches_oracle_shape() {
        let result = builtin_help_function_arglist(vec![Value::symbol("thread-join")]).unwrap();
        assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
    }

    #[test]
    fn help_function_arglist_thread_join_preserve_names() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("thread-join"), Value::True]).unwrap();
        assert_eq!(arglist_names(&result), vec!["thread".to_string()]);
    }

    #[test]
    fn help_function_arglist_sub_non_preserve_names_matches_oracle_shape() {
        let result = builtin_help_function_arglist(vec![Value::symbol("-")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec!["&rest".to_string(), "rest".to_string()]
        );
    }

    #[test]
    fn help_function_arglist_sub_preserve_names_matches_oracle_shape() {
        let result = builtin_help_function_arglist(vec![Value::symbol("-"), Value::True]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "&optional".to_string(),
                "number-or-marker".to_string(),
                "&rest".to_string(),
                "more-numbers-or-markers".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_event_apply_modifier_matches_oracle_shape() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("event-apply-modifier")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "arg1".to_string(),
                "arg2".to_string(),
                "arg3".to_string(),
                "arg4".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_event_apply_modifier_preserve_names() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("event-apply-modifier"), Value::True])
                .unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "event".to_string(),
                "symbol".to_string(),
                "lshiftby".to_string(),
                "prefix".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_condition_notify_optional_arg() {
        let result =
            builtin_help_function_arglist(vec![Value::symbol("condition-notify")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "arg1".to_string(),
                "&optional".to_string(),
                "arg2".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_lambda_params() {
        let result =
            builtin_help_function_arglist(vec![Value::Lambda(std::sync::Arc::new(LambdaData {
                params: LambdaParams {
                    required: vec!["x".to_string()],
                    optional: vec!["y".to_string()],
                    rest: Some("rest".to_string()),
                },
                body: vec![],
                env: None,
                docstring: None,
            }))])
            .unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "x".to_string(),
                "&optional".to_string(),
                "y".to_string(),
                "&rest".to_string(),
                "rest".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_quoted_lambda_params() {
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
            Value::symbol("x"),
        ]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec!["x".to_string(), "y".to_string()]
        );
    }

    #[test]
    fn help_function_arglist_quoted_lambda_symbol_params() {
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::symbol("x"),
            Value::symbol("x"),
        ]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert_eq!(result.as_symbol_name(), Some("x"));
    }

    #[test]
    fn help_function_arglist_quoted_macro_returns_t() {
        let quoted = Value::list(vec![
            Value::symbol("macro"),
            Value::list(vec![Value::symbol("x")]),
            Value::symbol("x"),
        ]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn help_function_arglist_macro_lambda_symbol_payload_returns_nil() {
        let quoted = Value::list(vec![Value::symbol("macro"), Value::symbol("lambda")]);
        let result = builtin_help_function_arglist(vec![quoted]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn help_function_arglist_quoted_lambda_improper_tail_errors() {
        let quoted = Value::cons(Value::symbol("lambda"), Value::Int(1));
        let result = builtin_help_function_arglist(vec![quoted]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn help_function_arglist_wrong_arity() {
        let result = builtin_help_function_arglist(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn help_function_arglist_eval_builtin_car() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("car")]).unwrap();
        assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
    }

    #[test]
    fn help_function_arglist_eval_builtin_car_preserve_names() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("car"), Value::True],
        )
        .unwrap();
        assert_eq!(arglist_names(&result), vec!["list".to_string()]);
    }

    #[test]
    fn help_function_arglist_eval_special_form_if() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("if")]).unwrap();
        assert_eq!(
            arglist_names(&result),
            vec![
                "arg1".to_string(),
                "arg2".to_string(),
                "&rest".to_string(),
                "rest".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_eval_runtime_alias_builtin() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-hfa-alias-builtin", Value::symbol("car"));

        let result = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("vm-hfa-alias-builtin")],
        )
        .unwrap();
        assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
    }

    #[test]
    fn help_function_arglist_eval_autoload_placeholder() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("describe-function")],
        )
        .unwrap();
        assert_eq!(
            result.as_str(),
            Some("[Arg list not available until function definition is loaded.]")
        );
    }

    #[test]
    fn help_function_arglist_eval_subr_arity_fallback_shapes() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let cdr =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("cdr")]).unwrap();
        assert_eq!(arglist_names(&cdr), vec!["arg1".to_string()]);

        let list =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("list")]).unwrap();
        assert_eq!(
            arglist_names(&list),
            vec!["&rest".to_string(), "rest".to_string()]
        );

        let read =
            builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("read")]).unwrap();
        assert_eq!(
            arglist_names(&read),
            vec!["&optional".to_string(), "arg1".to_string()]
        );

        let equal = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("equal")],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&equal),
            vec!["arg1".to_string(), "arg2".to_string()]
        );

        let substring = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("substring")],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&substring),
            vec![
                "arg1".to_string(),
                "&optional".to_string(),
                "arg2".to_string(),
                "arg3".to_string()
            ]
        );

        let funcall = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("funcall")],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&funcall),
            vec![
                "arg1".to_string(),
                "&rest".to_string(),
                "rest".to_string()
            ]
        );
    }

    #[test]
    fn help_function_arglist_eval_preserve_names_core_subrs() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let cdr = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("cdr"), Value::True],
        )
        .unwrap();
        assert_eq!(arglist_names(&cdr), vec!["list".to_string()]);

        let cons = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("cons"), Value::True],
        )
        .unwrap();
        assert_eq!(arglist_names(&cons), vec!["car".to_string(), "cdr".to_string()]);

        let list = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("list"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&list),
            vec!["&rest".to_string(), "objects".to_string()]
        );

        let min = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("min"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&min),
            vec![
                "number-or-marker".to_string(),
                "&rest".to_string(),
                "numbers-or-markers".to_string()
            ]
        );

        let max = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("max"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&max),
            vec![
                "number-or-marker".to_string(),
                "&rest".to_string(),
                "numbers-or-markers".to_string()
            ]
        );

        let message = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("message"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&message),
            vec![
                "format-string".to_string(),
                "&rest".to_string(),
                "args".to_string()
            ]
        );

        let concat = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("concat"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&concat),
            vec!["&rest".to_string(), "sequences".to_string()]
        );

        let nconc = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("nconc"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&nconc),
            vec!["&rest".to_string(), "lists".to_string()]
        );

        let format = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("format"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&format),
            vec![
                "string".to_string(),
                "&rest".to_string(),
                "objects".to_string()
            ]
        );

        let apply = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("apply"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&apply),
            vec![
                "function".to_string(),
                "&rest".to_string(),
                "arguments".to_string()
            ]
        );

        let assq = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("assq"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&assq),
            vec!["key".to_string(), "alist".to_string()]
        );

        let string_match = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("string-match"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&string_match),
            vec![
                "regexp".to_string(),
                "string".to_string(),
                "&optional".to_string(),
                "start".to_string(),
                "inhibit-modify".to_string()
            ]
        );

        let make_string = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("make-string"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&make_string),
            vec![
                "length".to_string(),
                "init".to_string(),
                "&optional".to_string(),
                "multibyte".to_string()
            ]
        );

        let read_from_string = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("read-from-string"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&read_from_string),
            vec![
                "string".to_string(),
                "&optional".to_string(),
                "start".to_string(),
                "end".to_string()
            ]
        );

        let funcall = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("funcall"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&funcall),
            vec![
                "function".to_string(),
                "&rest".to_string(),
                "arguments".to_string()
            ]
        );

        let mapcar = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("mapcar"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&mapcar),
            vec!["function".to_string(), "sequence".to_string()]
        );

        let read = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("read"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&read),
            vec!["&optional".to_string(), "stream".to_string()]
        );

        let documentation = builtin_help_function_arglist_eval(
            &mut evaluator,
            vec![Value::symbol("documentation"), Value::True],
        )
        .unwrap();
        assert_eq!(
            arglist_names(&documentation),
            vec![
                "function".to_string(),
                "&optional".to_string(),
                "raw".to_string()
            ]
        );
    }

    // =======================================================================
    // documentation (eval-dependent)
    // =======================================================================

    #[test]
    fn documentation_lambda_with_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();

        // Set up a lambda with a docstring in the function cell.
        let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
            params: LambdaParams::simple(vec!["x".to_string()]),
            body: vec![],
            env: None,
            docstring: Some("Add one to X.".to_string()),
        }));
        evaluator.obarray.set_symbol_function("my-fn", lambda);

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("my-fn")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Add one to X."));
    }

    #[test]
    fn documentation_lambda_no_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: None,
        }));
        evaluator.obarray.set_symbol_function("no-doc", lambda);

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("no-doc")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_unbound_function() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_err());
    }

    #[test]
    fn documentation_subr() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("plus", Value::Subr("+".to_string()));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("plus")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_string());
    }

    #[test]
    fn documentation_symbol_alias_to_builtin_returns_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("alias-builtin", Value::symbol("car"));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("alias-builtin")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_string());
    }

    #[test]
    fn documentation_prefers_function_documentation_property() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::string("propdoc"),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert_eq!(result.unwrap().as_str(), Some("propdoc"));
    }

    #[test]
    fn documentation_integer_function_documentation_property_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator
            .obarray
            .put_property("doc-prop", "function-documentation", Value::Int(9));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn documentation_list_function_documentation_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert_eq!(result.unwrap().as_str(), Some("doc"));
    }

    #[test]
    fn documentation_symbol_function_documentation_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator
            .obarray
            .put_property("doc-prop", "function-documentation", Value::symbol("t"));

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn documentation_vector_function_documentation_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::vector(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        assert!(result.unwrap().is_vector());
    }

    #[test]
    fn documentation_unbound_symbol_function_documentation_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::symbol("doc-prop-unbound"),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "void-variable"),
            other => panic!("expected void-variable signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_invalid_form_function_documentation_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("doc-prop", Value::Int(7));
        evaluator.obarray.put_property(
            "doc-prop",
            "function-documentation",
            Value::list(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "invalid-function"),
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_quoted_lambda_docstring() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x")]),
            Value::string("d"),
            Value::symbol("x"),
        ]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]).unwrap();
        assert_eq!(result.as_str(), Some("d"));
    }

    #[test]
    fn documentation_quoted_lambda_without_docstring_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![
            Value::symbol("lambda"),
            Value::list(vec![Value::symbol("x")]),
            Value::symbol("x"),
        ]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn documentation_vector_designator_returns_keyboard_macro_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result =
            builtin_documentation(&mut evaluator, vec![Value::vector(vec![Value::Int(1)])]).unwrap();
        assert_eq!(result.as_str(), Some("Keyboard macro."));
    }

    #[test]
    fn documentation_string_designator_returns_keyboard_macro_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(&mut evaluator, vec![Value::string("abc")]).unwrap();
        assert_eq!(result.as_str(), Some("Keyboard macro."));
    }

    #[test]
    fn documentation_quoted_macro_payload_matches_oracle_shape() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![
            Value::symbol("macro"),
            Value::list(vec![Value::symbol("x")]),
            Value::string("md"),
            Value::symbol("x"),
        ]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "invalid-function");
                assert_eq!(
                    sig.data.first(),
                    Some(&Value::list(vec![
                        Value::list(vec![Value::symbol("x")]),
                        Value::string("md"),
                        Value::symbol("x"),
                    ]))
                );
            }
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_empty_quoted_macro_errors_void_function_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let quoted = Value::list(vec![Value::symbol("macro")]);

        let result = builtin_documentation(&mut evaluator, vec![quoted]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "void-function");
                assert!(sig.data.first().is_some_and(Value::is_nil));
            }
            other => panic!("expected void-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_non_symbol_non_function_errors_invalid_function() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(
            &mut evaluator,
            vec![Value::list(vec![Value::Int(1), Value::Int(2)])],
        );
        assert!(result.is_err());
    }

    #[test]
    fn documentation_wrong_arity() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation(&mut evaluator, vec![]);
        assert!(result.is_err());
    }

    // =======================================================================
    // describe-function (eval-dependent)
    // =======================================================================

    #[test]
    fn describe_function_lambda() {
        let mut evaluator = super::super::eval::Evaluator::new();

        let lambda = Value::Lambda(std::sync::Arc::new(LambdaData {
            params: LambdaParams::simple(vec![]),
            body: vec![],
            env: None,
            docstring: None,
        }));
        evaluator.obarray.set_symbol_function("my-fn", lambda);

        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("my-fn")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Lisp function"));
    }

    #[test]
    fn describe_function_subr() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("plus", Value::Subr("+".to_string()));

        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("plus")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Built-in function"));
    }

    #[test]
    fn describe_function_resolves_builtin_name() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("car")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Built-in function"));
    }

    #[test]
    fn describe_function_special_form_if() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("if")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Special-form"));
    }

    #[test]
    fn describe_function_keyboard_macro_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-kmacro-string", Value::string("abc"));

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-kmacro-string")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Keyboard macro"));
    }

    #[test]
    fn describe_function_keyboard_macro_vector() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_function(
            "vm-kmacro-vector",
            Value::vector(vec![Value::Int(97), Value::Int(98)]),
        );

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-kmacro-vector")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Keyboard macro"));
    }

    #[test]
    fn describe_function_macro_marker_list() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let macro_marker = Value::cons(
            Value::symbol("macro"),
            Value::list(vec![Value::symbol("lambda"), Value::Nil, Value::Int(1)]),
        );
        evaluator
            .obarray
            .set_symbol_function("vm-macro-marker", macro_marker);

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-macro-marker")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Lisp macro"));
    }

    #[test]
    fn describe_function_alias_reports_alias_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-alias-car", Value::symbol("car"));

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-alias-car")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("vm-alias-car is an alias for `car`."));
    }

    #[test]
    fn describe_function_alias_to_missing_reports_alias_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .set_symbol_function("vm-alias-missing", Value::symbol("vm-no-such-fn"));

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-alias-missing")]);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().as_str(),
            Some("vm-alias-missing is an alias for `vm-no-such-fn`, which is not known to be defined.")
        );
    }

    #[test]
    fn describe_function_autoload_macro_form() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let autoload_macro = Value::list(vec![
            Value::symbol("autoload"),
            Value::string("files"),
            Value::string("doc"),
            Value::Nil,
            Value::symbol("macro"),
        ]);
        evaluator
            .obarray
            .set_symbol_function("vm-autoload-macro", autoload_macro);

        let result =
            builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-macro")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("Lisp macro"));
    }

    #[test]
    fn describe_function_void() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_err());
    }

    #[test]
    fn describe_function_non_symbol() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_function(&mut evaluator, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    // =======================================================================
    // describe-variable (eval-dependent)
    // =======================================================================

    #[test]
    fn describe_variable_with_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::string("The answer."),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("The answer."));
    }

    #[test]
    fn describe_variable_load_path_uses_c_source_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("load-path")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("defined in"))
        );
    }

    #[test]
    fn describe_variable_list_doc_property_falls_back_to_value_text() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("value is"))
        );
    }

    #[test]
    fn describe_variable_integer_doc_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator
            .obarray
            .put_property("my-var", "variable-documentation", Value::Int(9));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("defined in"))
        );
    }

    #[test]
    fn describe_variable_symbol_doc_property_unbound_errors_void_variable() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::symbol("vm-desc-unbound"),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "void-variable"),
            other => panic!("expected void-variable signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_symbol_doc_property_bound_true_errors_wrong_type_argument() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.set_symbol_value("my-doc", Value::True);
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::symbol("my-doc"),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data.first().and_then(Value::as_symbol_name),
                    Some("char-or-string-p")
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_symbol_doc_property_bound_list_errors_wrong_type_argument() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator.obarray.set_symbol_value(
            "my-doc",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );
        evaluator.obarray.put_property(
            "my-var",
            "variable-documentation",
            Value::symbol("my-doc"),
        );

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_true_doc_property_errors_wrong_type_argument() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
        evaluator
            .obarray
            .put_property("my-var", "variable-documentation", Value::True);

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
        match result {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data.first().and_then(Value::as_symbol_name),
                    Some("char-or-string-p")
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn describe_variable_bound_no_doc() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]);
        assert!(result.is_ok());
        let s = result.unwrap();
        assert!(s.as_str().unwrap().contains("x"));
    }

    #[test]
    fn describe_variable_bound_no_doc_matches_text_shape() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]).unwrap();
        let text = result.as_str().expect("describe-variable must return a string");
        assert!(text.ends_with('\n'));
        assert!(text.contains("x\u{2019}s value is"));
    }

    #[test]
    fn describe_variable_unbound() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.contains("void as a variable"))
        );
    }

    #[test]
    fn describe_variable_unbound_has_trailing_newline() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
        assert!(result.is_ok());
        assert!(
            result
                .unwrap()
                .as_str()
                .is_some_and(|s| s.ends_with('\n'))
        );
    }

    #[test]
    fn describe_variable_non_symbol() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn describe_variable_keyword_is_self_bound() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(&mut evaluator, vec![Value::keyword(":x")]).unwrap();
        let text = result.as_str().expect("describe-variable must return a string");
        assert!(text.contains("value is"));
        assert!(text.contains(":x"));
    }

    #[test]
    fn describe_variable_accepts_optional_second_arg() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result =
            builtin_describe_variable(&mut evaluator, vec![Value::symbol("x"), Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().as_str().is_some());
    }

    #[test]
    fn describe_variable_accepts_optional_third_arg() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.set_symbol_value("x", Value::Int(10));

        let result = builtin_describe_variable(
            &mut evaluator,
            vec![Value::symbol("x"), Value::Nil, Value::Nil],
        );
        assert!(result.is_ok());
        assert!(result.unwrap().as_str().is_some());
    }

    #[test]
    fn describe_variable_rejects_fourth_arg() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_describe_variable(
            &mut evaluator,
            vec![Value::symbol("x"), Value::Nil, Value::Nil, Value::Nil],
        );
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_property_eval_returns_string_property() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::string("doc"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert_eq!(result.as_str(), Some("doc"));
    }

    #[test]
    fn documentation_property_eval_integer_property_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::Int(7));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn documentation_property_eval_load_path_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("load-path"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("List of directories to search for files to load"))
        );
    }

    #[test]
    fn documentation_property_eval_case_fold_search_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("case-fold-search"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("searches and matches should ignore case"))
        );
    }

    #[test]
    fn documentation_property_eval_unread_command_events_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("unread-command-events"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("events to be read as the command input"))
        );
    }

    #[test]
    fn documentation_property_eval_auto_hscroll_mode_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("auto-hscroll-mode"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("automatic horizontal scrolling of windows"))
        );
    }

    #[test]
    fn documentation_property_eval_auto_composition_mode_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("auto-composition-mode"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("Auto-Composition mode is enabled"))
        );
    }

    #[test]
    fn documentation_property_eval_coding_system_alist_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("coding-system-alist"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("Alist of coding system names"))
        );
    }

    #[test]
    fn documentation_property_eval_debug_on_message_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("debug-on-message"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("debug if a message matching this regexp is displayed"))
        );
    }

    #[test]
    fn documentation_property_eval_display_hourglass_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("display-hourglass"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("show an hourglass pointer"))
        );
    }

    #[test]
    fn documentation_property_eval_exec_directory_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("exec-directory"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("Directory for executables for Emacs to invoke"))
        );
    }

    #[test]
    fn documentation_property_eval_frame_title_format_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("frame-title-format"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("Template for displaying the title bar of visible frames"))
        );
    }

    #[test]
    fn documentation_property_eval_header_line_format_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("header-line-format"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("controls the header line"))
        );
    }

    #[test]
    fn documentation_property_eval_input_method_function_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("input-method-function"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("implements the current input method"))
        );
    }

    #[test]
    fn documentation_property_eval_load_suffixes_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("load-suffixes"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("suffixes for Emacs Lisp files and dynamic modules"))
        );
    }

    #[test]
    fn documentation_property_eval_native_comp_eln_load_path_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("native-comp-eln-load-path"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("natively-compiled *.eln files"))
        );
    }

    #[test]
    fn documentation_property_eval_process_environment_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("process-environment"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("environment variables for subprocesses"))
        );
    }

    #[test]
    fn documentation_property_eval_scroll_margin_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("scroll-margin"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("margin at the top and bottom"))
        );
    }

    #[test]
    fn documentation_property_eval_debug_on_error_integer_property_returns_string() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("debug-on-error"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(
            result
                .as_str()
                .is_some_and(|s| s.contains("Non-nil means enter debugger if an error is signaled"))
        );
    }

    #[test]
    fn documentation_property_eval_list_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert_eq!(result.as_str(), Some("doc"));
    }

    #[test]
    fn documentation_property_eval_symbol_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "variable-documentation", Value::symbol("t"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn documentation_property_eval_vector_property_is_evaluated() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::vector(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        )
        .unwrap();
        assert!(result.is_vector());
    }

    #[test]
    fn documentation_property_eval_unbound_symbol_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::symbol("doc-sym-unbound"),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        );
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "void-variable"),
            other => panic!("expected void-variable signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_property_eval_invalid_form_property_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator.obarray.put_property(
            "doc-sym",
            "variable-documentation",
            Value::list(vec![Value::Int(1), Value::Int(2)]),
        );

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![
                Value::symbol("doc-sym"),
                Value::symbol("variable-documentation"),
            ],
        );
        match result {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "invalid-function"),
            other => panic!("expected invalid-function signal, got {other:?}"),
        }
    }

    #[test]
    fn documentation_property_eval_non_symbol_prop_returns_nil() {
        let mut evaluator = super::super::eval::Evaluator::new();
        evaluator
            .obarray
            .put_property("doc-sym", "x", Value::string("v"));

        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![Value::symbol("doc-sym"), Value::Int(1)],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn documentation_property_eval_non_symbol_target_errors() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let result = builtin_documentation_property_eval(
            &mut evaluator,
            vec![Value::Int(1), Value::symbol("variable-documentation")],
        );
        assert!(result.is_err());
    }
}
