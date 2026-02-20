//! Evaluator — special forms, function application, and dispatch.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::abbrev::AbbrevManager;
use super::advice::{AdviceManager, VariableWatcherList};
use super::autoload::AutoloadManager;
use super::bookmark::BookmarkManager;
use super::builtins;
use super::bytecode::Compiler;
use super::category::CategoryManager;
use super::coding::CodingSystemManager;
use super::custom::CustomManager;
use super::doc::{STARTUP_VARIABLE_DOC_STRING_PROPERTIES, STARTUP_VARIABLE_DOC_STUBS};
use super::error::*;
use super::expr::Expr;
use super::interactive::InteractiveRegistry;
use super::keymap::{encode_keymap_handle, KeymapManager};
use super::kill_ring::KillRing;
use super::kmacro::KmacroManager;
use super::mode::ModeRegistry;
use super::process::ProcessManager;
use super::rect::RectangleState;
use super::regex::MatchData;
use super::register::RegisterManager;
use super::symbol::Obarray;
use super::threads::ThreadManager;
use super::timer::TimerManager;
use super::value::*;
use crate::buffer::BufferManager;
use crate::window::FrameManager;

#[derive(Clone, Debug)]
enum NamedCallTarget {
    Obarray(Value),
    EvaluatorCallable,
    Probe,
    Builtin,
    SpecialForm,
    Void,
}

#[derive(Clone, Debug)]
struct NamedCallCache {
    symbol: String,
    function_epoch: u64,
    target: NamedCallTarget,
}

/// Limit for stored recent input events to match GNU Emacs: 300 entries.
pub(crate) const RECENT_INPUT_EVENT_LIMIT: usize = 300;

/// The Elisp evaluator.
pub struct Evaluator {
    /// The obarray — unified symbol table with value cells, function cells, plists.
    pub(crate) obarray: Obarray,
    /// Dynamic binding stack (each frame is one `let`/function call scope).
    pub(crate) dynamic: Vec<HashMap<String, Value>>,
    /// Lexical environment stack (for lexical-binding mode).
    pub(crate) lexenv: Vec<HashMap<String, Value>>,
    /// Features list (for require/provide).
    pub(crate) features: Vec<String>,
    /// Features currently being resolved through `require`.
    require_stack: Vec<String>,
    /// Buffer manager — owns all live buffers and tracks current buffer.
    pub(crate) buffers: BufferManager,
    /// Match data from the last successful search/match operation.
    pub(crate) match_data: Option<MatchData>,
    /// Keymap manager — owns all keymaps.
    pub(crate) keymaps: KeymapManager,
    /// Process manager — owns all tracked processes.
    pub(crate) processes: ProcessManager,
    /// Network manager — owns network connections, filters, and sentinels.
    /// Timer manager — owns all timers.
    pub(crate) timers: TimerManager,
    /// Advice manager — function advice (before/after/around/etc.).
    pub(crate) advice: AdviceManager,
    /// Variable watcher list — callbacks on variable changes.
    pub(crate) watchers: VariableWatcherList,
    /// Current buffer-local keymap id (set by `use-local-map`).
    pub(crate) current_local_map: Option<u64>,
    /// Register manager — quick storage and retrieval of text, positions, etc.
    pub(crate) registers: RegisterManager,
    /// Bookmark manager — persistent named positions.
    pub(crate) bookmarks: BookmarkManager,
    /// Abbreviation manager — text abbreviation expansion.
    pub(crate) abbrevs: AbbrevManager,
    /// Autoload manager — deferred function loading.
    pub(crate) autoloads: AutoloadManager,
    /// Custom variable manager — defcustom/defgroup system.
    pub(crate) custom: CustomManager,
    /// Kill ring — clipboard/kill ring for text editing.
    pub(crate) kill_ring: KillRing,
    /// Rectangle state — stores the last killed rectangle for yank-rectangle.
    pub(crate) rectangle: RectangleState,
    /// Interactive command registry — tracks interactive commands.
    pub(crate) interactive: InteractiveRegistry,
    /// Input events consumed by read* APIs, used by `recent-keys`.
    recent_input_events: Vec<Value>,
    /// Last key sequence captured by read-key/read-key-sequence/read-event paths.
    read_command_keys: Vec<Value>,
    /// Batch-compatible input-mode interrupt flag for `current-input-mode`.
    input_mode_interrupt: bool,
    /// Frame manager — owns all frames and windows.
    pub(crate) frames: FrameManager,
    /// Mode registry — major/minor modes.
    pub(crate) modes: ModeRegistry,
    /// Thread manager — cooperative threading primitives.
    pub(crate) threads: ThreadManager,
    /// Category manager — character category tables.
    pub(crate) category_manager: CategoryManager,
    /// Keyboard macro manager — recording, playback, macro ring.
    pub(crate) kmacro: KmacroManager,
    /// Coding system manager — encoding/decoding registry.
    pub(crate) coding_systems: CodingSystemManager,
    /// Recursion depth counter.
    depth: usize,
    /// Maximum recursion depth.
    max_depth: usize,
    /// Single-entry hot cache for named callable resolution in `funcall`/`apply`.
    named_call_cache: Option<NamedCallCache>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        let mut obarray = Obarray::new();
        let default_directory = std::env::current_dir()
            .ok()
            .and_then(|p| p.to_str().map(|s| s.to_string()))
            .map(|mut s| {
                if !s.ends_with('/') {
                    s.push('/');
                }
                s
            })
            .unwrap_or_else(|| "./".to_string());
        let mut keymaps = KeymapManager::new();
        let completion_in_region_mode_map =
            keymaps.make_sparse_keymap(Some("completion-in-region-mode-map".to_string()));
        let completion_list_mode_map =
            keymaps.make_sparse_keymap(Some("completion-list-mode-map".to_string()));
        let minibuffer_local_map =
            keymaps.make_sparse_keymap(Some("minibuffer-local-map".to_string()));
        let minibuffer_local_completion_map =
            keymaps.make_sparse_keymap(Some("minibuffer-local-completion-map".to_string()));
        let minibuffer_local_filename_completion_map = keymaps
            .make_sparse_keymap(Some("minibuffer-local-filename-completion-map".to_string()));
        let minibuffer_local_must_match_map =
            keymaps.make_sparse_keymap(Some("minibuffer-local-must-match-map".to_string()));
        let minibuffer_local_ns_map =
            keymaps.make_sparse_keymap(Some("minibuffer-local-ns-map".to_string()));
        let minibuffer_local_shell_command_map =
            keymaps.make_sparse_keymap(Some("minibuffer-local-shell-command-map".to_string()));
        let minibuffer_local_isearch_map =
            keymaps.make_sparse_keymap(Some("minibuffer-local-isearch-map".to_string()));
        let minibuffer_inactive_mode_map =
            keymaps.make_sparse_keymap(Some("minibuffer-inactive-mode-map".to_string()));
        let minibuffer_mode_map =
            keymaps.make_sparse_keymap(Some("minibuffer-mode-map".to_string()));
        let minibuffer_visible_completions_map =
            keymaps.make_sparse_keymap(Some("minibuffer-visible-completions-map".to_string()));
        let read_expression_map =
            keymaps.make_sparse_keymap(Some("read-expression-map".to_string()));
        let read_expression_internal_map =
            keymaps.make_sparse_keymap(Some("read--expression-map".to_string()));
        let read_char_from_minibuffer_map =
            keymaps.make_sparse_keymap(Some("read-char-from-minibuffer-map".to_string()));
        let read_extended_command_mode_map =
            keymaps.make_sparse_keymap(Some("read-extended-command-mode-map".to_string()));
        let read_regexp_map = keymaps.make_sparse_keymap(Some("read-regexp-map".to_string()));
        let read_key_empty_map = keymaps.make_sparse_keymap(Some("read-key-empty-map".to_string()));
        let read_key_full_map = keymaps.make_keymap();
        let keymap_handle = |id: u64| Value::Int(encode_keymap_handle(id));

        keymaps.set_keymap_parent(minibuffer_local_completion_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(
            minibuffer_local_filename_completion_map,
            Some(minibuffer_local_completion_map),
        );
        keymaps.set_keymap_parent(
            minibuffer_local_must_match_map,
            Some(minibuffer_local_completion_map),
        );
        keymaps.set_keymap_parent(minibuffer_local_ns_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(
            minibuffer_local_shell_command_map,
            Some(minibuffer_local_map),
        );
        keymaps.set_keymap_parent(minibuffer_local_isearch_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(minibuffer_mode_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(read_expression_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(read_expression_internal_map, Some(read_expression_map));
        keymaps.set_keymap_parent(read_char_from_minibuffer_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(read_extended_command_mode_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(read_regexp_map, Some(minibuffer_local_map));
        keymaps.set_keymap_parent(
            minibuffer_visible_completions_map,
            Some(completion_list_mode_map),
        );

        let standard_syntax_table = super::syntax::builtin_standard_syntax_table(Vec::new())
            .expect("startup seeding requires standard syntax table");

        // Set up standard global variables
        obarray.set_symbol_value("most-positive-fixnum", Value::Int(i64::MAX));
        obarray.set_symbol_value("most-negative-fixnum", Value::Int(i64::MIN));
        obarray.set_symbol_value("emacs-version", Value::string("29.1"));
        obarray.set_symbol_value("system-type", Value::symbol("gnu/linux"));
        obarray.set_symbol_value(
            "default-directory",
            Value::string(default_directory.clone()),
        );
        obarray.set_symbol_value(
            "command-line-default-directory",
            Value::string(default_directory),
        );
        let obarray_object = Value::vector(vec![Value::Nil]);
        obarray.set_symbol_value("obarray", obarray_object.clone());
        obarray.set_symbol_value("neovm--obarray-object", obarray_object);
        obarray.make_special("obarray");
        obarray.set_symbol_value(
            "command-line-args",
            Value::list(vec![
                Value::string("neovm-worker"),
                Value::string("--batch"),
            ]),
        );
        obarray.set_symbol_value("command-line-args-left", Value::Nil);
        obarray.set_symbol_value("command-line-functions", Value::Nil);
        obarray.set_symbol_value("command-line-processed", Value::True);
        obarray.set_symbol_value("command-switch-alist", Value::Nil);
        obarray.set_symbol_value(
            "command-line-ns-option-alist",
            Value::list(vec![Value::list(vec![
                Value::string("-NSOpen"),
                Value::Int(1),
                Value::symbol("ns-handle-nxopen"),
            ])]),
        );
        obarray.set_symbol_value(
            "command-line-x-option-alist",
            Value::list(vec![Value::list(vec![
                Value::string("-display"),
                Value::Int(1),
                Value::symbol("x-handle-display"),
            ])]),
        );
        obarray.set_symbol_value("load-path", Value::Nil);
        obarray.set_symbol_value("load-history", Value::Nil);
        obarray.set_symbol_value("features", Value::Nil);
        obarray.set_symbol_value("debug-on-error", Value::Nil);
        obarray.set_symbol_value("lexical-binding", Value::Nil);
        obarray.set_symbol_value("load-prefer-newer", Value::Nil);
        obarray.set_symbol_value("load-file-name", Value::Nil);
        obarray.set_symbol_value("noninteractive", Value::True);
        obarray.set_symbol_value("inhibit-quit", Value::Nil);
        obarray.set_symbol_value("print-length", Value::Nil);
        obarray.set_symbol_value("print-level", Value::Nil);
        obarray.set_symbol_value("standard-output", Value::True);
        obarray.set_symbol_value("buffer-read-only", Value::Nil);
        obarray.set_symbol_value("kill-ring", Value::Nil);
        obarray.set_symbol_value("kill-ring-yank-pointer", Value::Nil);
        obarray.set_symbol_value("last-command", Value::Nil);
        obarray.set_symbol_value("current-fill-column--has-warned", Value::Nil);
        obarray.set_symbol_value("current-input-method", Value::Nil);
        obarray.set_symbol_value("current-input-method-title", Value::Nil);
        obarray.set_symbol_value("current-iso639-language", Value::Nil);
        obarray.set_symbol_value("current-key-remap-sequence", Value::Nil);
        obarray.set_symbol_value("current-language-environment", Value::string("UTF-8"));
        obarray.set_symbol_value(
            "current-load-list",
            Value::list(vec![
                Value::symbol("comp--no-native-compile"),
                Value::cons(
                    Value::symbol("defun"),
                    Value::symbol("load--fixup-all-elns"),
                ),
                Value::symbol("load--eln-dest-dir"),
                Value::symbol("load--bin-dest-dir"),
            ]),
        );
        obarray.set_symbol_value("current-locale-environment", Value::string("C.UTF-8"));
        obarray.set_symbol_value("current-minibuffer-command", Value::Nil);
        obarray.set_symbol_value("current-time-list", Value::True);
        obarray.set_symbol_value("current-transient-input-method", Value::Nil);
        obarray.set_symbol_value("real-last-command", Value::Nil);
        obarray.set_symbol_value("last-repeatable-command", Value::Nil);
        obarray.set_symbol_value("this-original-command", Value::Nil);
        obarray.set_symbol_value("prefix-arg", Value::Nil);
        obarray.set_symbol_value("defining-kbd-macro", Value::Nil);
        obarray.set_symbol_value("executing-kbd-macro", Value::Nil);
        obarray.set_symbol_value("executing-kbd-macro-index", Value::Int(0));
        obarray.set_symbol_value("command-history", Value::Nil);
        obarray.set_symbol_value("extended-command-history", Value::Nil);
        obarray.set_symbol_value("completion-ignore-case", Value::Nil);
        obarray.set_symbol_value("read-buffer-completion-ignore-case", Value::Nil);
        obarray.set_symbol_value("read-file-name-completion-ignore-case", Value::Nil);
        obarray.set_symbol_value("completion-regexp-list", Value::Nil);
        obarray.set_symbol_value("completion--all-sorted-completions-location", Value::Nil);
        obarray.set_symbol_value("completion--capf-misbehave-funs", Value::Nil);
        obarray.set_symbol_value("completion--capf-safe-funs", Value::Nil);
        obarray.set_symbol_value(
            "completion--embedded-envvar-re",
            Value::string(
                "\\(?:^\\|[^$]\\(?:\\$\\$\\)*\\)\\$\\([[:alnum:]_]*\\|{\\([^}]*\\)\\)\\'",
            ),
        );
        obarray.set_symbol_value("completion--flex-score-last-md", Value::Nil);
        obarray.set_symbol_value("completion-all-sorted-completions", Value::Nil);
        obarray.set_symbol_value(
            "completion--cycling-threshold-type",
            Value::list(vec![Value::symbol("choice")]),
        );
        obarray.set_symbol_value(
            "completion--styles-type",
            Value::list(vec![Value::symbol("repeat")]),
        );
        obarray.set_symbol_value(
            "completion-at-point-functions",
            Value::list(vec![Value::symbol("tags-completion-at-point-function")]),
        );
        obarray.set_symbol_value(
            "completion-setup-hook",
            Value::list(vec![Value::symbol("completion-setup-function")]),
        );
        obarray.set_symbol_value(
            "completion-in-region-mode-map",
            keymap_handle(completion_in_region_mode_map),
        );
        obarray.set_symbol_value(
            "completion-list-mode-map",
            keymap_handle(completion_list_mode_map),
        );
        obarray.set_symbol_value(
            "completion-list-mode-syntax-table",
            standard_syntax_table.clone(),
        );
        obarray.set_symbol_value(
            "completion-list-mode-abbrev-table",
            Value::symbol("completion-list-mode-abbrev-table"),
        );
        obarray.set_symbol_value("completion-list-mode-hook", Value::Nil);
        obarray.set_symbol_value(
            "completion-ignored-extensions",
            Value::list(vec![
                Value::string(".o"),
                Value::string("~"),
                Value::string(".elc"),
            ]),
        );
        obarray.set_symbol_value(
            "completion-styles",
            Value::list(vec![
                Value::symbol("basic"),
                Value::symbol("partial-completion"),
                Value::symbol("emacs22"),
            ]),
        );
        obarray.set_symbol_value(
            "completion-category-defaults",
            Value::list(vec![
                Value::list(vec![
                    Value::symbol("buffer"),
                    Value::list(vec![
                        Value::symbol("styles"),
                        Value::symbol("basic"),
                        Value::symbol("substring"),
                    ]),
                ]),
                Value::list(vec![
                    Value::symbol("unicode-name"),
                    Value::list(vec![
                        Value::symbol("styles"),
                        Value::symbol("basic"),
                        Value::symbol("substring"),
                    ]),
                ]),
                Value::list(vec![
                    Value::symbol("project-file"),
                    Value::list(vec![Value::symbol("styles"), Value::symbol("substring")]),
                ]),
                Value::list(vec![
                    Value::symbol("xref-location"),
                    Value::list(vec![Value::symbol("styles"), Value::symbol("substring")]),
                ]),
                Value::list(vec![
                    Value::symbol("info-menu"),
                    Value::list(vec![
                        Value::symbol("styles"),
                        Value::symbol("basic"),
                        Value::symbol("substring"),
                    ]),
                ]),
                Value::list(vec![
                    Value::symbol("symbol-help"),
                    Value::list(vec![
                        Value::symbol("styles"),
                        Value::symbol("basic"),
                        Value::symbol("shorthand"),
                        Value::symbol("substring"),
                    ]),
                ]),
                Value::list(vec![
                    Value::symbol("calendar-month"),
                    Value::cons(
                        Value::symbol("display-sort-function"),
                        Value::symbol("identity"),
                    ),
                ]),
            ]),
        );
        obarray.set_symbol_value(
            "completion-styles-alist",
            Value::list(vec![
                Value::list(vec![
                    Value::symbol("basic"),
                    Value::symbol("completion-basic-try-completion"),
                    Value::symbol("completion-basic-all-completions"),
                    Value::string(
                        "Completion of the prefix before point and the suffix after point.",
                    ),
                ]),
                Value::list(vec![
                    Value::symbol("partial-completion"),
                    Value::symbol("completion-pcm-try-completion"),
                    Value::symbol("completion-pcm-all-completions"),
                    Value::string("Completion of multiple words, each one taken as a prefix."),
                ]),
                Value::list(vec![
                    Value::symbol("emacs22"),
                    Value::symbol("completion-emacs22-try-completion"),
                    Value::symbol("completion-emacs22-all-completions"),
                    Value::string("Prefix completion that only operates on the text before point."),
                ]),
            ]),
        );
        obarray.set_symbol_value("completion-category-overrides", Value::Nil);
        obarray.set_symbol_value("completion-cycle-threshold", Value::Nil);
        obarray.set_symbol_value("completions-detailed", Value::Nil);
        obarray.set_symbol_value("completions-format", Value::symbol("horizontal"));
        obarray.set_symbol_value("completions-group", Value::Nil);
        obarray.set_symbol_value("completions-group-format", Value::string("     %s  "));
        obarray.set_symbol_value("completions-group-sort", Value::Nil);
        obarray.set_symbol_value(
            "completions-header-format",
            Value::string("%s possible completions:\n"),
        );
        obarray.set_symbol_value(
            "completions-highlight-face",
            Value::symbol("completions-highlight"),
        );
        obarray.set_symbol_value("completions-max-height", Value::Nil);
        obarray.set_symbol_value("completions-sort", Value::symbol("alphabetical"));
        obarray.set_symbol_value("completion-auto-help", Value::True);
        obarray.set_symbol_value("completion-auto-deselect", Value::True);
        obarray.set_symbol_value("completion-auto-select", Value::Nil);
        obarray.set_symbol_value("completion-auto-wrap", Value::True);
        obarray.set_symbol_value("completion-base-position", Value::Nil);
        obarray.set_symbol_value("completion-cycling", Value::Nil);
        obarray.set_symbol_value("completion-extra-properties", Value::Nil);
        obarray.set_symbol_value("completion-fail-discreetly", Value::Nil);
        obarray.set_symbol_value("completion-flex-nospace", Value::Nil);
        obarray.set_symbol_value("completion-in-region--data", Value::Nil);
        obarray.set_symbol_value(
            "completion-in-region-function",
            Value::symbol("completion--in-region"),
        );
        obarray.set_symbol_value("completion-in-region-functions", Value::Nil);
        obarray.set_symbol_value("completion-in-region-mode", Value::Nil);
        obarray.set_symbol_value("completion-in-region-mode--predicate", Value::Nil);
        obarray.set_symbol_value("completion-in-region-mode-hook", Value::Nil);
        obarray.set_symbol_value("completion-in-region-mode-predicate", Value::Nil);
        obarray.set_symbol_value("completion-show-help", Value::True);
        obarray.set_symbol_value("completion-show-inline-help", Value::True);
        obarray.set_symbol_value("completion-lazy-hilit", Value::Nil);
        obarray.set_symbol_value("completion-lazy-hilit-fn", Value::Nil);
        obarray.set_symbol_value(
            "completion-list-insert-choice-function",
            Value::symbol("completion--replace"),
        );
        obarray.set_symbol_value("completion-no-auto-exit", Value::Nil);
        obarray.set_symbol_value(
            "completion-pcm--delim-wild-regex",
            Value::string("[-_./:| *]"),
        );
        obarray.set_symbol_value("completion-pcm--regexp", Value::Nil);
        obarray.set_symbol_value(
            "completion-pcm-complete-word-inserts-delimiters",
            Value::Nil,
        );
        obarray.set_symbol_value("completion-pcm-word-delimiters", Value::string("-_./:| "));
        obarray.set_symbol_value("completion-reference-buffer", Value::Nil);
        obarray.set_symbol_value("completion-tab-width", Value::Nil);
        obarray.set_symbol_value("enable-recursive-minibuffers", Value::Nil);
        obarray.set_symbol_value("history-length", Value::Int(100));
        obarray.set_symbol_value("history-delete-duplicates", Value::Nil);
        obarray.set_symbol_value("history-add-new-input", Value::True);
        obarray.set_symbol_value("read-buffer-function", Value::Nil);
        obarray.set_symbol_value(
            "read-file-name-function",
            Value::symbol("read-file-name-default"),
        );
        obarray.set_symbol_value("read-expression-history", Value::Nil);
        obarray.set_symbol_value("read-number-history", Value::Nil);
        obarray.set_symbol_value("read-char-history", Value::Nil);
        obarray.set_symbol_value("read-answer-short", Value::symbol("auto"));
        obarray.set_symbol_value("read-char-by-name-sort", Value::Nil);
        obarray.set_symbol_value("read-char-choice-use-read-key", Value::Nil);
        obarray.set_symbol_value("read-circle", Value::True);
        obarray.set_symbol_value("read-envvar-name-history", Value::Nil);
        obarray.set_symbol_value("read-face-name-sample-text", Value::string("SAMPLE"));
        obarray.set_symbol_value("read-key-delay", Value::Float(0.01));
        obarray.set_symbol_value(
            "read-answer-map--memoize",
            Value::hash_table(HashTableTest::Equal),
        );
        obarray.set_symbol_value(
            "read-char-from-minibuffer-map",
            keymap_handle(read_char_from_minibuffer_map),
        );
        obarray.set_symbol_value(
            "read-char-from-minibuffer-map-hash",
            Value::hash_table(HashTableTest::Equal),
        );
        obarray.set_symbol_value("read-expression-map", keymap_handle(read_expression_map));
        obarray.set_symbol_value(
            "read--expression-map",
            keymap_handle(read_expression_internal_map),
        );
        obarray.set_symbol_value(
            "read-extended-command-mode-map",
            keymap_handle(read_extended_command_mode_map),
        );
        obarray.set_symbol_value("read-key-empty-map", keymap_handle(read_key_empty_map));
        obarray.set_symbol_value("read-key-full-map", keymap_handle(read_key_full_map));
        obarray.set_symbol_value("read-regexp-map", keymap_handle(read_regexp_map));
        obarray.set_symbol_value("read-extended-command-mode", Value::Nil);
        obarray.set_symbol_value("read-extended-command-mode-hook", Value::Nil);
        obarray.set_symbol_value("read-extended-command-predicate", Value::Nil);
        obarray.set_symbol_value("read-hide-char", Value::Nil);
        obarray.set_symbol_value("read-mail-command", Value::symbol("rmail"));
        obarray.set_symbol_value("read-minibuffer-restore-windows", Value::True);
        obarray.set_symbol_value("read-only-mode-hook", Value::Nil);
        obarray.set_symbol_value("read-process-output-max", Value::Int(65536));
        obarray.set_symbol_value("read-quoted-char-radix", Value::Int(8));
        obarray.set_symbol_value("read-regexp--case-fold", Value::Nil);
        obarray.set_symbol_value("read-regexp-defaults-function", Value::Nil);
        obarray.set_symbol_value("read-symbol-shorthands", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-frame-alist",
            Value::list(vec![
                Value::cons(Value::symbol("width"), Value::Int(80)),
                Value::cons(Value::symbol("height"), Value::Int(2)),
            ]),
        );
        obarray.set_symbol_value(
            "minibuffer-inactive-mode-abbrev-table",
            Value::symbol("minibuffer-inactive-mode-abbrev-table"),
        );
        obarray.set_symbol_value("minibuffer-inactive-mode-hook", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-inactive-mode-map",
            keymap_handle(minibuffer_inactive_mode_map),
        );
        obarray.set_symbol_value(
            "minibuffer-inactive-mode-syntax-table",
            standard_syntax_table.clone(),
        );
        obarray.set_symbol_value(
            "minibuffer-mode-abbrev-table",
            Value::symbol("minibuffer-mode-abbrev-table"),
        );
        obarray.set_symbol_value("minibuffer-mode-hook", Value::Nil);
        obarray.set_symbol_value("minibuffer-mode-map", keymap_handle(minibuffer_mode_map));
        obarray.set_symbol_value("minibuffer-local-map", keymap_handle(minibuffer_local_map));
        obarray.set_symbol_value(
            "minibuffer-local-completion-map",
            keymap_handle(minibuffer_local_completion_map),
        );
        obarray.set_symbol_value(
            "minibuffer-local-filename-completion-map",
            keymap_handle(minibuffer_local_filename_completion_map),
        );
        obarray.set_symbol_value(
            "minibuffer-local-filename-syntax",
            standard_syntax_table.clone(),
        );
        obarray.set_symbol_value(
            "minibuffer-local-isearch-map",
            keymap_handle(minibuffer_local_isearch_map),
        );
        obarray.set_symbol_value(
            "minibuffer-local-must-match-map",
            keymap_handle(minibuffer_local_must_match_map),
        );
        obarray.set_symbol_value(
            "minibuffer-local-ns-map",
            keymap_handle(minibuffer_local_ns_map),
        );
        obarray.set_symbol_value(
            "minibuffer-local-shell-command-map",
            keymap_handle(minibuffer_local_shell_command_map),
        );
        obarray.set_symbol_value("minibuffer-history", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-history-variable",
            Value::symbol("minibuffer-history"),
        );
        obarray.set_symbol_value("minibuffer-history-position", Value::Nil);
        obarray.set_symbol_value("minibuffer-history-isearch-message-overlay", Value::Nil);
        obarray.set_symbol_value("minibuffer-history-search-history", Value::Nil);
        obarray.set_symbol_value("minibuffer-history-sexp-flag", Value::Nil);
        obarray.set_symbol_value("minibuffer-default", Value::Nil);
        obarray.set_symbol_value("minibuffer-default-add-done", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-default-add-function",
            Value::symbol("minibuffer-default-add-completions"),
        );
        obarray.set_symbol_value("minibuffer--original-buffer", Value::Nil);
        obarray.set_symbol_value("minibuffer--regexp-primed", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer--regexp-prompt-regexp",
            Value::string(
                "\\(?:Posix search\\|RE search\\|Search for regexp\\|Query replace regexp\\)",
            ),
        );
        obarray.set_symbol_value("minibuffer--require-match", Value::Nil);
        obarray.set_symbol_value("minibuffer-auto-raise", Value::Nil);
        obarray.set_symbol_value("minibuffer-follows-selected-frame", Value::True);
        obarray.set_symbol_value(
            "minibuffer-exit-hook",
            Value::list(vec![
                Value::symbol("minibuffer--regexp-exit"),
                Value::symbol("minibuffer-exit-on-screen-keyboard"),
                Value::symbol("minibuffer-restore-windows"),
            ]),
        );
        obarray.set_symbol_value("minibuffer-completion-table", Value::Nil);
        obarray.set_symbol_value("minibuffer-completion-predicate", Value::Nil);
        obarray.set_symbol_value("minibuffer-completion-confirm", Value::Nil);
        obarray.set_symbol_value("minibuffer-completion-auto-choose", Value::True);
        obarray.set_symbol_value("minibuffer-completion-base", Value::Nil);
        obarray.set_symbol_value("minibuffer-help-form", Value::Nil);
        obarray.set_symbol_value("minibuffer-completing-file-name", Value::Nil);
        obarray.set_symbol_value("minibuffer-regexp-mode", Value::True);
        obarray.set_symbol_value("minibuffer-regexp-mode-hook", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-regexp-prompts",
            Value::list(vec![
                Value::string("Posix search"),
                Value::string("RE search"),
                Value::string("Search for regexp"),
                Value::string("Query replace regexp"),
            ]),
        );
        obarray.set_symbol_value("minibuffer-message-clear-timeout", Value::Nil);
        obarray.set_symbol_value("minibuffer-message-overlay", Value::Nil);
        obarray.set_symbol_value("minibuffer-message-properties", Value::Nil);
        obarray.set_symbol_value("minibuffer-message-timeout", Value::Int(2));
        obarray.set_symbol_value("minibuffer-message-timer", Value::Nil);
        obarray.set_symbol_value("minibuffer-lazy-count-format", Value::string("%s "));
        obarray.set_symbol_value("minibuffer-text-before-history", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-prompt-properties",
            Value::list(vec![
                Value::symbol("read-only"),
                Value::True,
                Value::symbol("face"),
                Value::symbol("minibuffer-prompt"),
            ]),
        );
        obarray.set_symbol_value("minibuffer-allow-text-properties", Value::Nil);
        obarray.set_symbol_value("minibuffer-scroll-window", Value::Nil);
        obarray.set_symbol_value("minibuffer-visible-completions", Value::Nil);
        obarray.set_symbol_value("minibuffer-visible-completions--always-bind", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-visible-completions-map",
            keymap_handle(minibuffer_visible_completions_map),
        );
        obarray.set_symbol_value("minibuffer-depth-indicate-mode", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-default-prompt-format",
            Value::string(" (default %s)"),
        );
        obarray.set_symbol_value("minibuffer-beginning-of-buffer-movement", Value::Nil);
        obarray.set_symbol_value("minibuffer-electric-default-mode", Value::Nil);
        obarray.set_symbol_value("minibuffer-temporary-goal-position", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-confirm-exit-commands",
            Value::list(vec![
                Value::symbol("completion-at-point"),
                Value::symbol("minibuffer-complete"),
                Value::symbol("minibuffer-complete-word"),
            ]),
        );
        obarray.set_symbol_value("minibuffer-history-case-insensitive-variables", Value::Nil);
        obarray.set_symbol_value("minibuffer-on-screen-keyboard-displayed", Value::Nil);
        obarray.set_symbol_value("minibuffer-on-screen-keyboard-timer", Value::Nil);
        obarray.set_symbol_value(
            "minibuffer-setup-hook",
            Value::list(vec![
                Value::symbol("rfn-eshadow-setup-minibuffer"),
                Value::symbol("minibuffer--regexp-setup"),
                Value::symbol("minibuffer-setup-on-screen-keyboard"),
                Value::symbol("minibuffer-error-initialize"),
                Value::symbol("minibuffer-history-isearch-setup"),
                Value::symbol("minibuffer-history-initialize"),
            ]),
        );
        obarray.set_symbol_value("regexp-search-ring", Value::Nil);
        obarray.set_symbol_value("regexp-search-ring-max", Value::Int(16));
        obarray.set_symbol_value("regexp-search-ring-yank-pointer", Value::Nil);
        obarray.set_symbol_value("search-ring", Value::Nil);
        obarray.set_symbol_value("search-ring-max", Value::Int(16));
        obarray.set_symbol_value("search-ring-update", Value::Nil);
        obarray.set_symbol_value("search-ring-yank-pointer", Value::Nil);
        obarray.set_symbol_value("last-abbrev", Value::Nil);
        obarray.set_symbol_value("last-abbrev-location", Value::Int(0));
        obarray.set_symbol_value("last-abbrev-text", Value::Nil);
        obarray.set_symbol_value("last-command-event", Value::Nil);
        obarray.set_symbol_value("last-event-frame", Value::Nil);
        obarray.set_symbol_value("last-event-device", Value::Nil);
        obarray.set_symbol_value("last-input-event", Value::Nil);
        obarray.set_symbol_value("last-nonmenu-event", Value::Nil);
        obarray.set_symbol_value("last-prefix-arg", Value::Nil);
        obarray.set_symbol_value("last-kbd-macro", Value::Nil);
        obarray.set_symbol_value("last-code-conversion-error", Value::Nil);
        obarray.set_symbol_value("last-coding-system-specified", Value::Nil);
        obarray.set_symbol_value("last-coding-system-used", Value::symbol("undecided-unix"));
        obarray.set_symbol_value("last-next-selection-coding-system", Value::Nil);
        obarray.set_symbol_value("command-debug-status", Value::Nil);
        obarray.set_symbol_value(
            "command-error-function",
            Value::symbol("help-command-error-confusable-suggestions"),
        );
        obarray.set_symbol_value("key-substitution-in-progress", Value::Nil);
        obarray.set_symbol_value("this-command", Value::Nil);
        obarray.set_symbol_value("real-this-command", Value::Nil);
        obarray.set_symbol_value("this-command-keys-shift-translated", Value::Nil);
        obarray.set_symbol_value("current-prefix-arg", Value::Nil);
        obarray.set_symbol_value("track-mouse", Value::Nil);
        obarray.set_symbol_value("throw-on-input", Value::Nil);
        obarray.set_symbol_value(
            "while-no-input-ignore-events",
            Value::list(vec![
                Value::symbol("thread-event"),
                Value::symbol("file-notify"),
                Value::symbol("dbus-event"),
                Value::symbol("select-window"),
                Value::symbol("help-echo"),
                Value::symbol("move-frame"),
                Value::symbol("iconify-frame"),
                Value::symbol("make-frame-visible"),
                Value::symbol("focus-in"),
                Value::symbol("focus-out"),
                Value::symbol("config-changed-event"),
                Value::symbol("selection-request"),
            ]),
        );
        obarray.set_symbol_value("deactivate-mark", Value::True);
        obarray.set_symbol_value("mark-active", Value::Nil);
        obarray.set_symbol_value("mark-even-if-inactive", Value::True);
        obarray.set_symbol_value("mark-ring", Value::Nil);
        obarray.set_symbol_value("mark-ring-max", Value::Int(16));
        obarray.set_symbol_value("saved-region-selection", Value::Nil);
        obarray.set_symbol_value("transient-mark-mode", Value::Nil);
        obarray.set_symbol_value("transient-mark-mode-hook", Value::Nil);
        obarray.set_symbol_value("overriding-local-map", Value::Nil);
        obarray.set_symbol_value("overriding-local-map-menu-flag", Value::Nil);
        obarray.set_symbol_value("overriding-plist-environment", Value::Nil);
        obarray.set_symbol_value("overriding-terminal-local-map", Value::Nil);
        obarray.set_symbol_value("overriding-text-conversion-style", Value::symbol("lambda"));
        obarray.set_symbol_value("unread-input-method-events", Value::Nil);
        obarray.set_symbol_value("unread-post-input-method-events", Value::Nil);
        obarray.set_symbol_value("input-method-alist", Value::Nil);
        obarray.set_symbol_value("input-method-activate-hook", Value::Nil);
        obarray.set_symbol_value("input-method-after-insert-chunk-hook", Value::Nil);
        obarray.set_symbol_value("input-method-deactivate-hook", Value::Nil);
        obarray.set_symbol_value("input-method-exit-on-first-char", Value::Nil);
        obarray.set_symbol_value("input-method-exit-on-invalid-key", Value::Nil);
        obarray.set_symbol_value("input-method-function", Value::symbol("list"));
        obarray.set_symbol_value("input-method-highlight-flag", Value::True);
        obarray.set_symbol_value("input-method-history", Value::Nil);
        obarray.set_symbol_value("input-method-previous-message", Value::Nil);
        obarray.set_symbol_value("input-method-use-echo-area", Value::Nil);
        obarray.set_symbol_value("input-method-verbose-flag", Value::symbol("default"));
        obarray.set_symbol_value("unread-command-events", Value::Nil);
        // GNU Emacs seeds core startup vars with integer
        // `variable-documentation` offsets in the DOC table.
        for &(name, _) in STARTUP_VARIABLE_DOC_STUBS {
            obarray.put_property(name, "variable-documentation", Value::Int(0));
        }
        // Some startup docs are string-valued in GNU Emacs (not integer offsets).
        for &(name, doc) in STARTUP_VARIABLE_DOC_STRING_PROPERTIES {
            obarray.put_property(name, "variable-documentation", Value::string(doc));
        }

        // GNU Emacs exposes `x-display-color-p` as an alias to
        // `display-color-p` in startup state.
        obarray.set_symbol_function("x-display-color-p", Value::symbol("display-color-p"));
        obarray.set_symbol_function("x-color-defined-p", Value::symbol("color-defined-p"));
        obarray.set_symbol_function("x-color-values", Value::symbol("color-values"));
        obarray.set_symbol_function("x-defined-colors", Value::symbol("defined-colors"));
        obarray.set_symbol_function("x-get-selection", Value::symbol("gui-get-selection"));
        obarray.set_symbol_function(
            "x-get-selection-value",
            Value::symbol("gui-get-primary-selection"),
        );
        obarray.set_symbol_function("x-select-text", Value::symbol("gui-select-text"));
        obarray.set_symbol_function("x-selection-value", Value::symbol("gui-selection-value"));
        obarray.set_symbol_function("x-set-selection", Value::symbol("gui-set-selection"));
        // Window size aliases are also preseeded in startup state.
        obarray.set_symbol_function("window-height", Value::symbol("window-total-height"));
        obarray.set_symbol_function("window-width", Value::symbol("window-body-width"));
        obarray.set_symbol_function(
            "window-inside-pixel-edges",
            Value::symbol("window-body-pixel-edges"),
        );
        obarray.set_symbol_function("window-inside-edges", Value::symbol("window-body-edges"));
        // Additional startup aliases exposed as symbol indirections in GNU Emacs.
        obarray.set_symbol_function("count-matches", Value::symbol("how-many"));
        obarray.set_symbol_function("replace-rectangle", Value::symbol("string-rectangle"));
        obarray.set_symbol_function("wholenump", Value::symbol("natnump"));
        obarray.set_symbol_function(
            "subr-native-elisp-p",
            Value::symbol("native-comp-function-p"),
        );
        obarray.set_symbol_function(
            "kmacro-name-last-macro",
            Value::Subr("kmacro-name-last-macro".to_string()),
        );
        obarray.set_symbol_function(
            "name-last-kbd-macro",
            Value::symbol("kmacro-name-last-macro"),
        );
        // GNU Emacs exposes this helper as a Lisp wrapper, not a primitive.
        obarray.set_symbol_function(
            "subr-primitive-p",
            Value::ByteCode(Arc::new(Compiler::new(false).compile_lambda(
                &LambdaParams::simple(vec!["object".to_string()]),
                &[Expr::List(vec![
                    Expr::Symbol("subrp".to_string()),
                    Expr::Symbol("object".to_string()),
                ])],
            ))),
        );
        // Bookmark command wrappers are startup autoloads in GNU Emacs.
        let mut seed_autoload = |name: &str, file: &str, doc: &str| {
            obarray.set_symbol_function(
                name,
                Value::list(vec![
                    Value::symbol("autoload"),
                    Value::string(file),
                    Value::string(doc),
                    Value::True,
                    Value::Nil,
                ]),
            );
        };
        seed_autoload(
            "bookmark-delete",
            "bookmark",
            "Delete BOOKMARK-NAME from the bookmark list.",
        );
        seed_autoload(
            "bookmark-jump",
            "bookmark",
            "Jump to bookmark BOOKMARK (a point in some file).",
        );
        seed_autoload(
            "bookmark-load",
            "bookmark",
            "Load bookmarks from FILE (which must be in bookmark format).",
        );
        seed_autoload(
            "bookmark-rename",
            "bookmark",
            "Change the name of OLD-NAME bookmark to NEW-NAME name.",
        );
        seed_autoload(
            "bookmark-save",
            "bookmark",
            "Save currently defined bookmarks in FILE.",
        );
        seed_autoload(
            "bookmark-set",
            "bookmark",
            "Set a bookmark named NAME at the current location.",
        );
        seed_autoload(
            "format-seconds",
            "time-date",
            "Use format control STRING to format the number SECONDS.",
        );
        seed_autoload(
            "format-spec",
            "format-spec",
            "Return a string based on FORMAT and SPECIFICATION.",
        );
        seed_autoload(
            "string-clean-whitespace",
            "subr-x",
            "Clean up whitespace in STRING.",
        );
        seed_autoload(
            "string-glyph-split",
            "subr-x",
            "Split STRING into a list of strings representing separate glyphs.",
        );
        seed_autoload(
            "upcase-char",
            "misc",
            "Uppercasify ARG chars starting from point.  Point doesn't move.",
        );
        seed_autoload(
            "bounds-of-thing-at-point",
            "thingatpt",
            "Determine the start and end buffer locations for the THING at point.",
        );
        seed_autoload("thing-at-point", "thingatpt", "Return the THING at point.");
        seed_autoload(
            "symbol-at-point",
            "thingatpt",
            "Return the symbol at point, or nil if none is found.",
        );
        seed_autoload(
            "safe-date-to-time",
            "time-date",
            "Parse a string DATE that represents a date-time and return a time value.",
        );
        seed_autoload(
            "read-passwd",
            "auth-source",
            "Read a password, prompting with PROMPT, and return password as a string.",
        );
        seed_autoload(
            "clear-rectangle",
            "rect",
            "Blank out the region-rectangle.",
        );
        seed_autoload(
            "delete-extract-rectangle",
            "rect",
            "Delete the contents of the rectangle with corners at START and END.",
        );
        seed_autoload(
            "delete-rectangle",
            "rect",
            "Delete (don't save) text in the region-rectangle.",
        );
        seed_autoload(
            "describe-function",
            "help-fns",
            "Display the full documentation of FUNCTION (a symbol).",
        );
        seed_autoload(
            "describe-variable",
            "help-fns",
            "Display the full documentation of VARIABLE (a symbol).",
        );
        seed_autoload(
            "extract-rectangle",
            "rect",
            "Return the contents of the rectangle with corners at START and END.",
        );
        seed_autoload(
            "insert-kbd-macro",
            "macros",
            "Insert in buffer the definition of kbd macro MACRONAME, as Lisp code.",
        );
        seed_autoload(
            "insert-rectangle",
            "rect",
            "Insert text of RECTANGLE with upper left corner at point.",
        );
        seed_autoload(
            "kbd-macro-query",
            "macros",
            "Query user during kbd macro execution.",
        );
        seed_autoload(
            "kill-rectangle",
            "rect",
            "Delete the region-rectangle and save it as the last killed one.",
        );
        seed_autoload(
            "open-rectangle",
            "rect",
            "Blank out the region-rectangle, shifting text right.",
        );
        seed_autoload(
            "string-pixel-width",
            "subr-x",
            "Return the width of STRING in pixels.",
        );
        seed_autoload(
            "string-rectangle",
            "rect",
            "Replace rectangle contents with STRING on each line.",
        );
        seed_autoload(
            "yank-rectangle",
            "rect",
            "Yank the last killed rectangle with upper left corner at point.",
        );
        drop(seed_autoload);
        let mut seed_autoload_noninteractive = |name: &str, file: &str, doc: &str| {
            obarray.set_symbol_function(
                name,
                Value::list(vec![
                    Value::symbol("autoload"),
                    Value::string(file),
                    Value::string(doc),
                    Value::Nil,
                    Value::Nil,
                ]),
            );
        };
        // Some helper autoloads are non-interactive in GNU Emacs startup
        // function-cells; override their startup metadata accordingly.
        seed_autoload_noninteractive(
            "bounds-of-thing-at-point",
            "thingatpt",
            "Determine the start and end buffer locations for the THING at point.",
        );
        seed_autoload_noninteractive("thing-at-point", "thingatpt", "Return the THING at point.");
        seed_autoload_noninteractive(
            "symbol-at-point",
            "thingatpt",
            "Return the symbol at point, or nil if none is found.",
        );
        seed_autoload_noninteractive(
            "format-seconds",
            "time-date",
            "Use format control STRING to format the number SECONDS.",
        );
        seed_autoload_noninteractive(
            "format-spec",
            "format-spec",
            "Return a string based on FORMAT and SPECIFICATION.",
        );
        seed_autoload_noninteractive(
            "read-passwd",
            "auth-source",
            "Read a password, prompting with PROMPT, and return password as a string.",
        );
        seed_autoload_noninteractive(
            "safe-date-to-time",
            "time-date",
            "Parse a string DATE that represents a date-time and return a time value.",
        );
        seed_autoload_noninteractive(
            "delete-extract-rectangle",
            "rect",
            "Delete the contents of the rectangle with corners at START and END.",
        );
        seed_autoload_noninteractive(
            "extract-rectangle",
            "rect",
            "Return the contents of the rectangle with corners at START and END.",
        );
        seed_autoload_noninteractive(
            "insert-rectangle",
            "rect",
            "Insert text of RECTANGLE with upper left corner at point.",
        );
        seed_autoload_noninteractive(
            "string-clean-whitespace",
            "subr-x",
            "Clean up whitespace in STRING.",
        );
        seed_autoload_noninteractive(
            "string-glyph-split",
            "subr-x",
            "Split STRING into a list of strings representing separate glyphs.",
        );
        seed_autoload_noninteractive(
            "string-pixel-width",
            "subr-x",
            "Return the width of STRING in pixels.",
        );
        // Keep these as non-interactive autoload wrappers to match GNU Emacs
        // `symbol-function` shape while preserving runtime callability through
        // builtin dispatch.
        drop(seed_autoload_noninteractive);
        obarray.set_symbol_function(
            "string-chop-newline",
            Value::list(vec![
                Value::symbol("autoload"),
                Value::string("subr-x"),
                Value::string("Remove the final newline (if any) from STRING."),
                Value::Nil,
                Value::Nil,
            ]),
        );
        obarray.set_symbol_function(
            "string-pad",
            Value::list(vec![
                Value::symbol("autoload"),
                Value::string("subr-x"),
                Value::string("Pad STRING to LENGTH using PADDING."),
                Value::Nil,
                Value::Nil,
            ]),
        );
        obarray.set_symbol_function(
            "string-fill",
            Value::list(vec![
                Value::symbol("autoload"),
                Value::string("subr-x"),
                Value::string(
                    "Try to word-wrap STRING so that it displays with lines no wider than WIDTH.",
                ),
                Value::Nil,
                Value::Nil,
            ]),
        );
        obarray.set_symbol_function(
            "string-limit",
            Value::list(vec![
                Value::symbol("autoload"),
                Value::string("subr-x"),
                Value::string(
                    "Return a substring of STRING that is (up to) LENGTH characters long.",
                ),
                Value::Nil,
                Value::Nil,
            ]),
        );
        // Some startup helpers are Lisp functions that delegate to primitives.
        // Seed lightweight bytecode wrappers so `symbol-function` shape matches GNU Emacs.
        let seed_function_wrapper = |obarray: &mut Obarray, name: &str| {
            let wrapper = format!("neovm--startup-subr-wrapper-{name}");
            obarray.set_symbol_function(&wrapper, Value::Subr(name.to_string()));

            let params = LambdaParams {
                required: vec![],
                optional: vec![],
                rest: Some("args".to_string()),
            };
            let body = vec![Expr::List(vec![
                Expr::Symbol("apply".to_string()),
                Expr::List(vec![
                    Expr::Symbol("quote".to_string()),
                    Expr::Symbol(wrapper),
                ]),
                Expr::Symbol("args".to_string()),
            ])];
            let bc = Compiler::new(false).compile_lambda(&params, &body);
            obarray.set_symbol_function(name, Value::ByteCode(Arc::new(bc)));
        };
        let seed_fixed_arity_wrapper =
            |obarray: &mut Obarray, name: &str, required: &[&str], optional: &[&str]| {
                let wrapper = format!("neovm--startup-subr-wrapper-{name}");
                obarray.set_symbol_function(&wrapper, Value::Subr(name.to_string()));

                let params = LambdaParams {
                    required: required.iter().map(|s| (*s).to_string()).collect(),
                    optional: optional.iter().map(|s| (*s).to_string()).collect(),
                    rest: None,
                };

                let mut call = Vec::with_capacity(1 + required.len() + optional.len());
                call.push(Expr::Symbol(wrapper));
                call.extend(required.iter().map(|s| Expr::Symbol((*s).to_string())));
                call.extend(optional.iter().map(|s| Expr::Symbol((*s).to_string())));

                let bc = Compiler::new(false).compile_lambda(&params, &[Expr::List(call)]);
                obarray.set_symbol_function(name, Value::ByteCode(Arc::new(bc)));
            };
        for name in [
            "autoloadp",
            "seq-count",
            "seq-concatenate",
            "seq-contains-p",
            "seq-drop",
            "seq-do",
            "seq-empty-p",
            "seq-every-p",
            "seq-into",
            "seq-length",
            "seq-mapn",
            "seq-max",
            "seq-min",
            "seq-position",
            "seq-reduce",
            "seq-reverse",
            "seq-some",
            "seq-sort",
            "seq-subseq",
            "seq-take",
            "seq-uniq",
            "looking-at-p",
            "string-match-p",
            "string-blank-p",
            "string-empty-p",
            "string-equal-ignore-case",
            "string-to-vector",
        ] {
            seed_function_wrapper(&mut obarray, name);
        }

        seed_fixed_arity_wrapper(&mut obarray, "string-join", &["strings"], &["separator"]);
        seed_fixed_arity_wrapper(&mut obarray, "string-to-list", &["string"], &[]);

        // Keep word-at-point unavailable at startup; symbol-at-point lazily
        // materializes it to mirror GNU Emacs thing-at-point bootstrap.
        obarray.fmakunbound("word-at-point");

        // Mark standard variables as special (dynamically bound)
        for name in &[
            "debug-on-error",
            "lexical-binding",
            "load-prefer-newer",
            "load-path",
            "load-history",
            "features",
            "default-directory",
            "load-file-name",
            "noninteractive",
            "inhibit-quit",
            "print-length",
            "print-level",
            "standard-output",
            "buffer-read-only",
            "unread-command-events",
        ] {
            obarray.make_special(name);
        }

        // Initialize the standard error hierarchy (error, user-error, etc.)
        super::errors::init_standard_errors(&mut obarray);

        // Initialize indentation variables (tab-width, indent-tabs-mode, etc.)
        super::indent::init_indent_vars(&mut obarray);

        let mut custom = CustomManager::new();
        custom.make_variable_buffer_local("buffer-read-only");

        Self {
            obarray,
            dynamic: Vec::new(),
            lexenv: Vec::new(),
            features: Vec::new(),
            require_stack: Vec::new(),
            buffers: BufferManager::new(),
            match_data: None,
            keymaps,
            processes: ProcessManager::new(),
            timers: TimerManager::new(),
            advice: AdviceManager::new(),
            watchers: VariableWatcherList::new(),
            current_local_map: None,
            registers: RegisterManager::new(),
            bookmarks: BookmarkManager::new(),
            abbrevs: AbbrevManager::new(),
            autoloads: AutoloadManager::new(),
            custom,
            kill_ring: KillRing::new(),
            rectangle: RectangleState::new(),
            interactive: InteractiveRegistry::new(),
            recent_input_events: Vec::new(),
            read_command_keys: Vec::new(),
            input_mode_interrupt: true,
            frames: FrameManager::new(),
            modes: ModeRegistry::new(),
            threads: ThreadManager::new(),
            category_manager: CategoryManager::new(),
            kmacro: KmacroManager::new(),
            coding_systems: CodingSystemManager::new(),
            depth: 0,
            max_depth: 200,
            named_call_cache: None,
        }
    }

    /// Whether lexical-binding is currently enabled.
    pub fn lexical_binding(&self) -> bool {
        self.obarray
            .symbol_value("lexical-binding")
            .is_some_and(|v| v.is_truthy())
    }

    pub(crate) fn record_input_event(&mut self, event: Value) {
        self.assign("last-input-event", event.clone());
        self.recent_input_events.push(event);
        if self.recent_input_events.len() > RECENT_INPUT_EVENT_LIMIT {
            self.recent_input_events.remove(0);
        }
    }

    pub(crate) fn record_nonmenu_input_event(&mut self, event: Value) {
        self.assign("last-nonmenu-event", event);
    }

    pub(crate) fn recent_input_events(&self) -> &[Value] {
        &self.recent_input_events
    }

    pub(crate) fn clear_recent_input_events(&mut self) {
        self.recent_input_events.clear();
    }

    pub(crate) fn set_read_command_keys(&mut self, keys: Vec<Value>) {
        self.read_command_keys = keys;
    }

    pub(crate) fn clear_read_command_keys(&mut self) {
        self.read_command_keys.clear();
    }

    pub(crate) fn read_command_keys(&self) -> &[Value] {
        &self.read_command_keys
    }

    pub(crate) fn current_input_mode_tuple(&self) -> (bool, bool, bool, i64) {
        // Batch oracle compatibility: flow-control and meta are fixed to
        // nil/t respectively, and quit char is fixed to C-g (7).
        (self.input_mode_interrupt, false, true, 7)
    }

    pub(crate) fn set_input_mode_interrupt(&mut self, interrupt: bool) {
        self.input_mode_interrupt = interrupt;
    }

    pub(crate) fn pop_unread_command_event(&mut self) -> Option<Value> {
        let current = match self.eval_symbol("unread-command-events") {
            Ok(value) => value,
            Err(_) => Value::Nil,
        };
        match current {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let head = pair.car.clone();
                let tail = pair.cdr.clone();
                drop(pair);
                self.assign("unread-command-events", tail);
                self.record_input_event(head.clone());
                Some(head)
            }
            _ => None,
        }
    }

    pub(crate) fn peek_unread_command_event(&self) -> Option<Value> {
        let current = match self.eval_symbol("unread-command-events") {
            Ok(value) => value,
            Err(_) => Value::Nil,
        };
        match current {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                Some(pair.car.clone())
            }
            _ => None,
        }
    }

    /// Enable or disable lexical binding.
    pub fn set_lexical_binding(&mut self, enabled: bool) {
        self.obarray
            .set_symbol_value("lexical-binding", Value::bool(enabled));
    }

    /// Load a file, converting EvalError back to Flow for use in special forms.
    pub(crate) fn load_file_internal(&mut self, path: &std::path::Path) -> EvalResult {
        super::load::load_file(self, path).map_err(|e| match e {
            EvalError::Signal { symbol, data } => signal(&symbol, data),
            EvalError::UncaughtThrow { tag, value } => Flow::Throw { tag, value },
        })
    }

    /// Keep the Lisp-visible `features` variable in sync with the evaluator's
    /// internal feature set.
    fn sync_features_variable(&mut self) {
        let values: Vec<Value> = self
            .features
            .iter()
            .map(|name| Value::symbol(name.clone()))
            .collect();
        self.obarray
            .set_symbol_value("features", Value::list(values));
    }

    fn refresh_features_from_variable(&mut self) {
        let current = self
            .obarray
            .symbol_value("features")
            .cloned()
            .unwrap_or(Value::Nil);
        let mut parsed = Vec::new();
        if let Some(items) = list_to_vec(&current) {
            for item in items {
                if let Some(name) = item.as_symbol_name() {
                    parsed.push(name.to_string());
                }
            }
        }
        self.features = parsed;
    }

    fn has_feature(&mut self, name: &str) -> bool {
        self.refresh_features_from_variable();
        self.features.iter().any(|f| f == name)
    }

    fn add_feature(&mut self, name: &str) {
        self.refresh_features_from_variable();
        if self.features.iter().any(|f| f == name) {
            return;
        }
        // Emacs pushes newly-provided features at the front.
        self.features.insert(0, name.to_string());
        self.sync_features_variable();
    }

    pub(crate) fn feature_present(&mut self, name: &str) -> bool {
        self.has_feature(name)
    }

    /// Access the obarray (for builtins that need it).
    pub fn obarray(&self) -> &Obarray {
        &self.obarray
    }

    /// Access the obarray mutably.
    pub fn obarray_mut(&mut self) -> &mut Obarray {
        &mut self.obarray
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        self.eval(expr).map_err(map_flow)
    }

    pub fn eval_forms(&mut self, forms: &[Expr]) -> Vec<Result<Value, EvalError>> {
        forms.iter().map(|form| self.eval_expr(form)).collect()
    }

    /// Set a global variable.
    pub fn set_variable(&mut self, name: &str, value: Value) {
        self.obarray.set_symbol_value(name, value);
    }

    /// Set a function binding.
    pub fn set_function(&mut self, name: &str, value: Value) {
        self.obarray.set_symbol_function(name, value);
    }

    // -----------------------------------------------------------------------
    // Core eval
    // -----------------------------------------------------------------------

    pub(crate) fn eval(&mut self, expr: &Expr) -> EvalResult {
        self.depth += 1;
        if self.depth > self.max_depth {
            self.depth -= 1;
            return Err(signal(
                "excessive-lisp-nesting",
                vec![Value::Int(self.max_depth as i64)],
            ));
        }
        let result = self.eval_inner(expr);
        self.depth -= 1;
        result
    }

    fn eval_inner(&mut self, expr: &Expr) -> EvalResult {
        match expr {
            Expr::Int(v) => Ok(Value::Int(*v)),
            Expr::Float(v) => Ok(Value::Float(*v)),
            Expr::Str(s) => Ok(Value::string(s.clone())),
            Expr::Char(c) => Ok(Value::Char(*c)),
            Expr::Keyword(s) => Ok(Value::Keyword(s.clone())),
            Expr::Bool(true) => Ok(Value::True),
            Expr::Bool(false) => Ok(Value::Nil),
            Expr::Vector(items) => {
                // Emacs vector literals are self-evaluating constants; elements
                // are not evaluated in the current lexical/dynamic environment.
                let vals = items.iter().map(quote_to_value).collect();
                Ok(Value::vector(vals))
            }
            Expr::Symbol(symbol) => self.eval_symbol(symbol),
            Expr::List(items) => self.eval_list(items),
            Expr::DottedList(items, last) => {
                // Evaluate as a list call, ignoring dotted cdr
                // (This is for `(func a b . rest)` style, which in practice
                //  means the dotted pair is rarely used in function calls)
                let _ = last;
                self.eval_list(items)
            }
        }
    }

    fn eval_symbol(&self, symbol: &str) -> EvalResult {
        if symbol == "nil" {
            return Ok(Value::Nil);
        }
        if symbol == "t" {
            return Ok(Value::True);
        }
        // Keywords evaluate to themselves
        if symbol.starts_with(':') {
            return Ok(Value::Keyword(symbol.to_string()));
        }

        // If lexical binding is on and symbol is NOT special, check lexenv first
        if self.lexical_binding() && !self.obarray.is_special(symbol) {
            for frame in self.lexenv.iter().rev() {
                if let Some(value) = frame.get(symbol) {
                    return Ok(value.clone());
                }
            }
        }

        // Dynamic scope lookup (inner to outer)
        for frame in self.dynamic.iter().rev() {
            if let Some(value) = frame.get(symbol) {
                return Ok(value.clone());
            }
        }

        // Buffer-local binding on current buffer.
        if let Some(buf) = self.buffers.current_buffer() {
            if let Some(value) = buf.get_buffer_local(symbol) {
                return Ok(value.clone());
            }
        }

        // Obarray value cell
        if let Some(value) = self.obarray.symbol_value(symbol) {
            return Ok(value.clone());
        }

        Err(signal("void-variable", vec![Value::symbol(symbol)]))
    }

    fn eval_list(&mut self, items: &[Expr]) -> EvalResult {
        let Some((head, tail)) = items.split_first() else {
            return Ok(Value::Nil);
        };

        if let Expr::Symbol(name) = head {
            // Check for macro expansion first (from obarray function cell)
            if let Some(func) = self.obarray.symbol_function(name).cloned() {
                if func.is_nil() {
                    return Err(signal("void-function", vec![Value::symbol(name)]));
                }
                if let Value::Macro(_) = &func {
                    let expanded = self.expand_macro(func, tail)?;
                    return self.eval(&expanded);
                }

                if let Value::Subr(bound_name) = &func {
                    if bound_name == name && super::subr_info::is_special_form(name) {
                        if let Some(result) = self.try_special_form(name, tail) {
                            return result;
                        }
                    }
                }

                // Explicit function-cell bindings override special-form fallback.
                let mut args = Vec::with_capacity(tail.len());
                for expr in tail {
                    args.push(self.eval(expr)?);
                }
                if super::autoload::is_autoload_value(&func) {
                    let writeback_args = args.clone();
                    let result =
                        self.apply_named_callable(name, args, Value::Subr(name.clone()), false);
                    if let Ok(value) = &result {
                        self.maybe_writeback_mutating_first_arg(
                            name,
                            None,
                            &writeback_args,
                            value,
                        );
                    }
                    return result;
                }
                let function_is_callable = match &func {
                    Value::Lambda(_) | Value::ByteCode(_) | Value::Macro(_) => true,
                    Value::Subr(bound_name) => !super::subr_info::is_special_form(bound_name),
                    _ => false,
                };
                let alias_target = match &func {
                    Value::Symbol(target) => Some(target.clone()),
                    Value::Subr(bound_name) => Some(bound_name.clone()),
                    _ => None,
                };
                let writeback_args = args.clone();
                let result = match self.apply(func.clone(), args) {
                    Err(Flow::Signal(sig))
                        if sig.symbol == "invalid-function" && !function_is_callable =>
                    {
                        if matches!(func, Value::Symbol(_)) {
                            Err(Flow::Signal(sig))
                        } else {
                            Err(signal("invalid-function", vec![Value::symbol(name)]))
                        }
                    }
                    other => other,
                };
                if let Ok(value) = &result {
                    self.maybe_writeback_mutating_first_arg(
                        name,
                        alias_target.as_deref(),
                        &writeback_args,
                        value,
                    );
                }
                return if let Some(target) = alias_target {
                    result.map_err(|flow| {
                        rewrite_wrong_arity_alias_function_object(flow, name, &target)
                    })
                } else {
                    result
                };
            }

            // Special forms
            if !self.obarray.is_function_unbound(name) {
                if let Some(result) = self.try_special_form(name, tail) {
                    return result;
                }
            }

            // Regular function call — evaluate args then dispatch
            let mut args = Vec::with_capacity(tail.len());
            for expr in tail {
                args.push(self.eval(expr)?);
            }

            let writeback_args = args.clone();
            let result = self.apply_named_callable(name, args, Value::Subr(name.clone()), false);
            if let Ok(value) = &result {
                self.maybe_writeback_mutating_first_arg(name, None, &writeback_args, value);
            }
            return result;
        }

        // Head is a list (possibly a lambda expression)
        if let Expr::List(lambda_form) = head {
            if let Some(Expr::Symbol(s)) = lambda_form.first() {
                if s == "lambda" {
                    let func = self.eval_lambda(&lambda_form[1..])?;
                    let mut args = Vec::with_capacity(tail.len());
                    for expr in tail {
                        args.push(self.eval(expr)?);
                    }
                    return self.apply(func, args);
                }
            }
        }

        Err(signal("invalid-function", vec![quote_to_value(head)]))
    }

    fn maybe_writeback_mutating_first_arg(
        &mut self,
        called_name: &str,
        alias_target: Option<&str>,
        call_args: &[Value],
        result: &Value,
    ) {
        let mutates_fillarray =
            called_name == "fillarray" || alias_target.is_some_and(|name| name == "fillarray");
        let mutates_aset =
            called_name == "aset" || alias_target.is_some_and(|name| name == "aset");
        if !mutates_fillarray && !mutates_aset {
            return;
        }
        let Some(first_arg) = call_args.first() else {
            return;
        };
        if !first_arg.is_string() {
            return;
        }

        let replacement = if mutates_fillarray {
            if !result.is_string() || eq_value(first_arg, result) {
                return;
            }
            result.clone()
        } else {
            if call_args.len() < 3 {
                return;
            }
            let Ok(updated) =
                super::builtins::aset_string_replacement(first_arg, &call_args[1], &call_args[2])
            else {
                return;
            };
            if eq_value(first_arg, &updated) {
                return;
            }
            updated
        };

        if first_arg.as_str() == replacement.as_str() {
            return;
        }

        let mut visited = HashSet::new();
        for frame in &mut self.lexenv {
            for value in frame.values_mut() {
                Self::replace_alias_refs_in_value(value, first_arg, &replacement, &mut visited);
            }
        }
        for frame in &mut self.dynamic {
            for value in frame.values_mut() {
                Self::replace_alias_refs_in_value(value, first_arg, &replacement, &mut visited);
            }
        }
        if let Some(buf) = self.buffers.current_buffer_mut() {
            for value in buf.properties.values_mut() {
                Self::replace_alias_refs_in_value(value, first_arg, &replacement, &mut visited);
            }
        }

        let symbols: Vec<String> = self
            .obarray
            .all_symbols()
            .into_iter()
            .map(str::to_string)
            .collect();
        for name in symbols {
            if let Some(symbol) = self.obarray.get_mut(&name) {
                if let Some(value) = symbol.value.as_mut() {
                    Self::replace_alias_refs_in_value(value, first_arg, &replacement, &mut visited);
                }
            }
        }
    }

    fn replace_alias_refs_in_value(
        value: &mut Value,
        from: &Value,
        to: &Value,
        visited: &mut HashSet<usize>,
    ) {
        if eq_value(value, from) {
            *value = to.clone();
            return;
        }

        match value {
            Value::Cons(cell) => {
                let key = (std::sync::Arc::as_ptr(cell) as usize) ^ 0x1;
                if !visited.insert(key) {
                    return;
                }
                let mut pair = cell.lock().expect("poisoned");
                Self::replace_alias_refs_in_value(&mut pair.car, from, to, visited);
                Self::replace_alias_refs_in_value(&mut pair.cdr, from, to, visited);
            }
            Value::Vector(items) => {
                let key = (std::sync::Arc::as_ptr(items) as usize) ^ 0x2;
                if !visited.insert(key) {
                    return;
                }
                let mut values = items.lock().expect("poisoned");
                for item in values.iter_mut() {
                    Self::replace_alias_refs_in_value(item, from, to, visited);
                }
            }
            Value::HashTable(table) => {
                let key = (std::sync::Arc::as_ptr(table) as usize) ^ 0x4;
                if !visited.insert(key) {
                    return;
                }
                let mut guard = table.lock().expect("poisoned");
                let old_ptr = match from {
                    Value::Str(value) => Some(std::sync::Arc::as_ptr(value) as usize),
                    _ => None,
                };
                let new_ptr = match to {
                    Value::Str(value) => Some(std::sync::Arc::as_ptr(value) as usize),
                    _ => None,
                };
                if matches!(guard.test, HashTableTest::Eq | HashTableTest::Eql) {
                    if let (Some(old_ptr), Some(new_ptr)) = (old_ptr, new_ptr) {
                        if let Some(existing) = guard.data.remove(&HashKey::Ptr(old_ptr)) {
                            guard.data.insert(HashKey::Ptr(new_ptr), existing);
                        }
                    }
                }
                for item in guard.data.values_mut() {
                    Self::replace_alias_refs_in_value(item, from, to, visited);
                }
            }
            _ => {}
        }
    }

    // -----------------------------------------------------------------------
    // Special forms
    // -----------------------------------------------------------------------

    fn try_special_form(&mut self, name: &str, tail: &[Expr]) -> Option<EvalResult> {
        Some(match name {
            "quote" => self.sf_quote(tail),
            "function" => self.sf_function(tail),
            "let" => self.sf_let(tail),
            "let*" => self.sf_let_star(tail),
            "setq" => self.sf_setq(tail),
            "setq-local" => self.sf_setq_local(tail),
            "if" => self.sf_if(tail),
            "and" => self.sf_and(tail),
            "or" => self.sf_or(tail),
            "cond" => self.sf_cond(tail),
            "while" => self.sf_while(tail),
            "progn" => self.sf_progn(tail),
            "prog1" => self.sf_prog1(tail),
            "lambda" => self.eval_lambda(tail),
            "defun" => self.sf_defun(tail),
            "defvar" => self.sf_defvar(tail),
            "defconst" => self.sf_defconst(tail),
            "defmacro" => self.sf_defmacro(tail),
            "funcall" => self.sf_funcall(tail),
            "catch" => self.sf_catch(tail),
            "throw" => self.sf_throw(tail),
            "unwind-protect" => self.sf_unwind_protect(tail),
            "condition-case" => self.sf_condition_case(tail),
            "byte-code-literal" => self.sf_byte_code_literal(tail),
            "interactive" => Ok(Value::Nil), // Stub: ignored for now
            "declare" => Ok(Value::Nil),     // Stub: ignored for now
            "when" => self.sf_when(tail),
            "unless" => self.sf_unless(tail),
            "defalias" => self.sf_defalias(tail),
            "provide" => self.sf_provide(tail),
            "require" => self.sf_require(tail),
            "save-excursion" => self.sf_save_excursion(tail),
            "save-mark-and-excursion" => self.sf_save_mark_and_excursion(tail),
            "save-restriction" => self.sf_save_restriction(tail),
            "save-match-data" => self.sf_save_match_data(tail),
            "with-current-buffer" => self.sf_with_current_buffer(tail),
            "ignore-errors" => self.sf_ignore_errors(tail),
            "dotimes" => self.sf_dotimes(tail),
            "dolist" => self.sf_dolist(tail),
            // Custom system special forms
            "defcustom" => super::custom::sf_defcustom(self, tail),
            "defgroup" => super::custom::sf_defgroup(self, tail),
            "setq-default" => super::custom::sf_setq_default(self, tail),
            "defvar-local" => super::custom::sf_defvar_local(self, tail),
            // Autoload special forms
            "autoload" => super::autoload::sf_autoload(self, tail),
            "eval-when-compile" => super::autoload::sf_eval_when_compile(self, tail),
            "eval-and-compile" => super::autoload::sf_eval_and_compile(self, tail),
            "declare-function" => super::autoload::sf_declare_function(self, tail),
            "define-obsolete-function-alias" => {
                super::autoload::sf_define_obsolete_function_alias(self, tail)
            }
            "define-obsolete-variable-alias" => {
                super::autoload::sf_define_obsolete_variable_alias(self, tail)
            }
            "make-obsolete" => super::autoload::sf_make_obsolete(self, tail),
            "make-obsolete-variable" => super::autoload::sf_make_obsolete_variable(self, tail),
            "with-eval-after-load" => super::autoload::sf_with_eval_after_load(self, tail),
            // Error hierarchy
            "define-error" => super::errors::sf_define_error(self, tail),
            // Pattern matching (pcase)
            "pcase" => super::pcase::sf_pcase(self, tail),
            "pcase-let" => super::pcase::sf_pcase_let(self, tail),
            "pcase-let*" => super::pcase::sf_pcase_let_star(self, tail),
            "pcase-dolist" => super::pcase::sf_pcase_dolist(self, tail),
            // Generalized variables (setf)
            "setf" => super::setf::sf_setf(self, tail),
            "push" => super::setf::sf_push(self, tail),
            "pop" => super::setf::sf_pop(self, tail),
            "cl-incf" => super::setf::sf_cl_incf(self, tail),
            "cl-decf" => super::setf::sf_cl_decf(self, tail),
            "gv-define-simple-setter" => super::setf::sf_gv_define_simple_setter(self, tail),
            "gv-define-setter" => super::setf::sf_gv_define_setter(self, tail),
            // CL extended special forms
            "cl-defstruct" => super::cl_extra::sf_cl_defstruct(self, tail),
            "cl-loop" => super::cl_extra::sf_cl_loop(self, tail),
            "cl-destructuring-bind" => super::cl_extra::sf_cl_destructuring_bind(self, tail),
            "cl-push" => super::cl_extra::sf_cl_push(self, tail),
            "cl-pop" => super::cl_extra::sf_cl_pop(self, tail),
            "cl-pushnew" => super::cl_extra::sf_cl_pushnew(self, tail),
            "cl-assert" => super::cl_extra::sf_cl_assert(self, tail),
            "cl-check-type" => super::cl_extra::sf_cl_check_type(self, tail),
            "cl-case" => super::cl_extra::sf_cl_case(self, tail),
            "cl-ecase" => super::cl_extra::sf_cl_ecase(self, tail),
            "cl-typecase" => super::cl_extra::sf_cl_typecase(self, tail),
            "cl-etypecase" => super::cl_extra::sf_cl_etypecase(self, tail),
            "cl-block" => super::cl_extra::sf_cl_block(self, tail),
            "cl-return-from" => super::cl_extra::sf_cl_return_from(self, tail),
            "cl-dotimes" => super::cl_extra::sf_cl_dotimes(self, tail),
            "cl-dolist" => super::cl_extra::sf_cl_dolist(self, tail),
            "cl-flet" => super::cl_extra::sf_cl_flet(self, tail),
            "cl-labels" => super::cl_extra::sf_cl_labels(self, tail),
            "cl-progv" => super::cl_extra::sf_cl_progv(self, tail),
            // Reader/printer special forms
            "with-output-to-string" => super::reader::sf_with_output_to_string(self, tail),
            // Threading
            "with-mutex" => super::threads::sf_with_mutex(self, tail),
            // Misc special forms
            "prog2" => super::misc::sf_prog2(self, tail),
            "with-temp-buffer" => super::misc::sf_with_temp_buffer(self, tail),
            "save-current-buffer" => super::misc::sf_save_current_buffer(self, tail),
            "track-mouse" => super::misc::sf_track_mouse(self, tail),
            "with-syntax-table" => super::misc::sf_with_syntax_table(self, tail),
            // Interactive / mode definition special forms
            "define-minor-mode" => super::interactive::sf_define_minor_mode(self, tail),
            "define-derived-mode" => super::interactive::sf_define_derived_mode(self, tail),
            "define-generic-mode" => super::interactive::sf_define_generic_mode(self, tail),
            _ => return None,
        })
    }

    fn sf_quote(&self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 1 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("quote"), Value::Int(tail.len() as i64)],
            ));
        }
        Ok(quote_to_value(&tail[0]))
    }

    fn sf_function(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 1 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("function"), Value::Int(tail.len() as i64)],
            ));
        }
        match &tail[0] {
            Expr::List(items) => {
                // #'(lambda ...) — create closure
                if let Some(Expr::Symbol(s)) = items.first() {
                    if s == "lambda" {
                        return self.eval_lambda(&items[1..]);
                    }
                }
                Ok(quote_to_value(&tail[0]))
            }
            _ => Ok(quote_to_value(&tail[0])),
        }
    }

    fn sf_let(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("let"), Value::Int(tail.len() as i64)],
            ));
        }

        let mut lexical_bindings = HashMap::new();
        let mut dynamic_bindings = HashMap::new();
        let use_lexical = self.lexical_binding();

        match &tail[0] {
            Expr::List(entries) => {
                for binding in entries {
                    match binding {
                        Expr::Symbol(name) => {
                            if use_lexical && !self.obarray.is_special(name) {
                                lexical_bindings.insert(name.clone(), Value::Nil);
                            } else {
                                dynamic_bindings.insert(name.clone(), Value::Nil);
                            }
                        }
                        Expr::List(pair) if !pair.is_empty() => {
                            let Expr::Symbol(name) = &pair[0] else {
                                return Err(signal(
                                    "wrong-type-argument",
                                    vec![Value::symbol("symbolp"), quote_to_value(&pair[0])],
                                ));
                            };
                            let value = if pair.len() > 1 {
                                self.eval(&pair[1])?
                            } else {
                                Value::Nil
                            };
                            if use_lexical && !self.obarray.is_special(name) {
                                lexical_bindings.insert(name.clone(), value);
                            } else {
                                dynamic_bindings.insert(name.clone(), value);
                            }
                        }
                        _ => return Err(signal("wrong-type-argument", vec![])),
                    }
                }
            }
            Expr::Symbol(s) if s == "nil" => {} // (let nil ...)
            Expr::DottedList(_, last) => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), quote_to_value(last)],
                ))
            }
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), quote_to_value(other)],
                ))
            }
        }

        let pushed_lex = !lexical_bindings.is_empty();
        let pushed_dyn = !dynamic_bindings.is_empty();
        if pushed_lex {
            self.lexenv.push(lexical_bindings);
        }
        if pushed_dyn {
            self.dynamic.push(dynamic_bindings);
        }
        let result = self.sf_progn(&tail[1..]);
        if pushed_dyn {
            self.dynamic.pop();
        }
        if pushed_lex {
            self.lexenv.pop();
        }
        result
    }

    fn sf_let_star(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("let*"), Value::Int(tail.len() as i64)],
            ));
        }

        let entries = match &tail[0] {
            Expr::List(entries) => entries.clone(),
            Expr::Symbol(s) if s == "nil" => Vec::new(),
            Expr::DottedList(_, last) => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), quote_to_value(last)],
                ))
            }
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), quote_to_value(other)],
                ))
            }
        };

        let use_lexical = self.lexical_binding();
        let pushed_lex = use_lexical; // Always push a frame for let* in lexical mode
        let pushed_dyn = true; // Always push a dynamic frame too (for special vars or dynamic mode)

        self.dynamic.push(HashMap::new());
        if use_lexical {
            self.lexenv.push(HashMap::new());
        }

        for binding in &entries {
            match binding {
                Expr::Symbol(name) => {
                    if use_lexical && !self.obarray.is_special(name) {
                        if let Some(frame) = self.lexenv.last_mut() {
                            frame.insert(name.clone(), Value::Nil);
                        }
                    } else if let Some(frame) = self.dynamic.last_mut() {
                        frame.insert(name.clone(), Value::Nil);
                    }
                }
                Expr::List(pair) if !pair.is_empty() => {
                    let Expr::Symbol(name) = &pair[0] else {
                        if pushed_lex {
                            self.lexenv.pop();
                        }
                        self.dynamic.pop();
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("symbolp"), quote_to_value(&pair[0])],
                        ));
                    };
                    let value = if pair.len() > 1 {
                        match self.eval(&pair[1]) {
                            Ok(v) => v,
                            Err(e) => {
                                if pushed_lex {
                                    self.lexenv.pop();
                                }
                                self.dynamic.pop();
                                return Err(e);
                            }
                        }
                    } else {
                        Value::Nil
                    };
                    if use_lexical && !self.obarray.is_special(name) {
                        if let Some(frame) = self.lexenv.last_mut() {
                            frame.insert(name.clone(), value);
                        }
                    } else if let Some(frame) = self.dynamic.last_mut() {
                        frame.insert(name.clone(), value);
                    }
                }
                _ => {
                    if pushed_lex {
                        self.lexenv.pop();
                    }
                    self.dynamic.pop();
                    return Err(signal("wrong-type-argument", vec![]));
                }
            }
        }

        let result = self.sf_progn(&tail[1..]);
        if pushed_dyn {
            self.dynamic.pop();
        }
        if pushed_lex {
            self.lexenv.pop();
        }
        result
    }

    fn sf_setq(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Ok(Value::Nil);
        }
        if tail.len() % 2 != 0 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("setq"), Value::Int(tail.len() as i64)],
            ));
        }

        let mut last = Value::Nil;
        let mut i = 0;
        while i < tail.len() {
            let Expr::Symbol(name) = &tail[i] else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), quote_to_value(&tail[i])],
                ));
            };
            let value = self.eval(&tail[i + 1])?;
            self.assign(name, value.clone());
            last = value;
            i += 2;
        }
        Ok(last)
    }

    fn sf_setq_local(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Ok(Value::Nil);
        }
        if tail.len() % 2 != 0 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("setq-local"), Value::Int(tail.len() as i64)],
            ));
        }

        let mut last = Value::Nil;
        let mut i = 0;
        while i < tail.len() {
            let Expr::Symbol(name) = &tail[i] else {
                return Err(signal("wrong-type-argument", vec![]));
            };

            if name == "nil" || name == "t" {
                return Err(signal("setting-constant", vec![Value::symbol(name)]));
            }

            let value = self.eval(&tail[i + 1])?;
            if let Some(buf) = self.buffers.current_buffer_mut() {
                buf.set_buffer_local(name, value.clone());
            } else {
                self.assign(name, value.clone());
            }
            last = value;
            i += 2;
        }
        Ok(last)
    }

    fn sf_if(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 2 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("if"), Value::Int(tail.len() as i64)],
            ));
        }
        let cond = self.eval(&tail[0])?;
        if cond.is_truthy() {
            self.eval(&tail[1])
        } else {
            self.sf_progn(&tail[2..])
        }
    }

    fn sf_and(&mut self, tail: &[Expr]) -> EvalResult {
        let mut last = Value::True;
        for expr in tail {
            last = self.eval(expr)?;
            if last.is_nil() {
                return Ok(Value::Nil);
            }
        }
        Ok(last)
    }

    fn sf_or(&mut self, tail: &[Expr]) -> EvalResult {
        for expr in tail {
            let val = self.eval(expr)?;
            if val.is_truthy() {
                return Ok(val);
            }
        }
        Ok(Value::Nil)
    }

    fn sf_cond(&mut self, tail: &[Expr]) -> EvalResult {
        for clause in tail {
            let Expr::List(items) = clause else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), quote_to_value(clause)],
                ));
            };
            if items.is_empty() {
                continue;
            }
            let test = self.eval(&items[0])?;
            if test.is_truthy() {
                if items.len() == 1 {
                    return Ok(test);
                }
                return self.sf_progn(&items[1..]);
            }
        }
        Ok(Value::Nil)
    }

    fn sf_while(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("while"), Value::Int(tail.len() as i64)],
            ));
        }
        loop {
            let cond = self.eval(&tail[0])?;
            if cond.is_nil() {
                return Ok(Value::Nil);
            }
            self.sf_progn(&tail[1..])?;
        }
    }

    pub(crate) fn sf_progn(&mut self, forms: &[Expr]) -> EvalResult {
        let mut last = Value::Nil;
        for form in forms {
            last = self.eval(form)?;
        }
        Ok(last)
    }

    fn sf_prog1(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("prog1"), Value::Int(tail.len() as i64)],
            ));
        }
        let first = self.eval(&tail[0])?;
        for form in &tail[1..] {
            self.eval(form)?;
        }
        Ok(first)
    }

    fn sf_when(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::cons(Value::Int(1), Value::Int(1)),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }
        let cond = self.eval(&tail[0])?;
        if cond.is_truthy() {
            self.sf_progn(&tail[1..])
        } else {
            Ok(Value::Nil)
        }
    }

    fn sf_unless(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::cons(Value::Int(1), Value::Int(1)),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }
        let cond = self.eval(&tail[0])?;
        if cond.is_nil() {
            self.sf_progn(&tail[1..])
        } else {
            Ok(Value::Nil)
        }
    }

    fn sf_defun(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 2 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::cons(Value::Int(2), Value::Int(2)),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(&tail[0])],
            ));
        };
        let lambda = self.eval_lambda(&tail[1..])?;
        self.obarray.set_symbol_function(name, lambda);
        Ok(Value::symbol(name.clone()))
    }

    fn sf_defvar(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("defvar"), Value::Int(tail.len() as i64)],
            ));
        }
        if tail.len() > 3 {
            return Err(signal("error", vec![Value::string("Too many arguments")]));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(&tail[0])],
            ));
        };
        // Only set if not already bound. defvar always marks as special.
        if !self.obarray.boundp(name) {
            let value = if tail.len() > 1 {
                self.eval(&tail[1])?
            } else {
                Value::Nil
            };
            self.obarray.set_symbol_value(name, value);
        }
        self.obarray.make_special(name);
        Ok(Value::symbol(name.clone()))
    }

    fn sf_defconst(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 2 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("defconst"), Value::Int(tail.len() as i64)],
            ));
        }
        if tail.len() > 3 {
            return Err(signal("error", vec![Value::string("Too many arguments")]));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(&tail[0])],
            ));
        };
        let value = self.eval(&tail[1])?;
        self.obarray.set_symbol_value(name, value);
        let sym = self.obarray.get_or_intern(name);
        sym.constant = true;
        sym.special = true;
        Ok(Value::symbol(name.clone()))
    }

    fn sf_defmacro(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 2 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::cons(Value::Int(2), Value::Int(2)),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), quote_to_value(&tail[0])],
            ));
        };
        let params = self.parse_lambda_params(&tail[1])?;
        let (docstring, body_start) = match tail.get(2) {
            Some(Expr::Str(s)) => (Some(s.clone()), 3),
            _ => (None, 2),
        };
        let body = tail[body_start..].to_vec();
        let macro_val = Value::Macro(std::sync::Arc::new(LambdaData {
            params,
            body,
            env: None,
            docstring,
        }));
        self.obarray.set_symbol_function(name, macro_val);
        Ok(Value::symbol(name.clone()))
    }

    fn sf_funcall(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("funcall"), Value::Int(tail.len() as i64)],
            ));
        }
        let function = self.eval(&tail[0])?;
        let mut args = Vec::with_capacity(tail.len().saturating_sub(1));
        for expr in &tail[1..] {
            args.push(self.eval(expr)?);
        }
        self.apply(function, args)
    }

    fn sf_catch(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("catch"), Value::Int(tail.len() as i64)],
            ));
        }
        let tag = self.eval(&tail[0])?;
        match self.sf_progn(&tail[1..]) {
            Ok(value) => Ok(value),
            Err(Flow::Throw {
                tag: thrown_tag,
                value,
            }) if eq_value(&tag, &thrown_tag) => Ok(value),
            Err(flow) => Err(flow),
        }
    }

    fn sf_throw(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 2 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("throw"), Value::Int(tail.len() as i64)],
            ));
        }
        let tag = self.eval(&tail[0])?;
        let value = self.eval(&tail[1])?;
        Err(Flow::Throw { tag, value })
    }

    fn sf_unwind_protect(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::symbol("unwind-protect"),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }
        let primary = self.eval(&tail[0]);
        let cleanup = self.sf_progn(&tail[1..]);
        match cleanup {
            Ok(_) => primary,
            Err(flow) => Err(flow),
        }
    }

    fn sf_condition_case(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 3 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::symbol("condition-case"),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }

        let var = match &tail[0] {
            Expr::Symbol(name) => name.clone(),
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), quote_to_value(other)],
                ))
            }
        };
        let body = &tail[1];
        let handlers = &tail[2..];

        // Emacs validates handler shape even when BODY exits normally.
        for handler in handlers {
            match handler {
                Expr::List(_) => {}
                Expr::Symbol(name) if name == "nil" => {}
                _ => {
                    return Err(signal(
                        "error",
                        vec![Value::string(format!(
                            "Invalid condition handler: {}",
                            super::expr::print_expr(handler)
                        ))],
                    ))
                }
            }
        }

        match self.eval(body) {
            Ok(value) => Ok(value),
            Err(Flow::Signal(sig)) => {
                for handler in handlers {
                    if matches!(handler, Expr::Symbol(name) if name == "nil") {
                        continue;
                    }
                    let Expr::List(handler_items) = handler else {
                        return Err(signal("wrong-type-argument", vec![]));
                    };
                    if handler_items.is_empty() {
                        continue;
                    }

                    if signal_matches(&handler_items[0], &sig.symbol) {
                        let mut frame = HashMap::new();
                        if var != "nil" {
                            frame.insert(var.clone(), make_signal_binding_value(&sig));
                        }
                        self.dynamic.push(frame);
                        let result = self.sf_progn(&handler_items[1..]);
                        self.dynamic.pop();
                        return result;
                    }
                }
                Err(Flow::Signal(sig))
            }
            Err(Flow::Throw { tag, value }) => {
                let no_catch = SignalData {
                    symbol: "no-catch".to_string(),
                    data: vec![tag.clone(), value.clone()],
                    raw_data: None,
                };

                for handler in handlers {
                    if matches!(handler, Expr::Symbol(name) if name == "nil") {
                        continue;
                    }
                    let Expr::List(handler_items) = handler else {
                        return Err(signal("wrong-type-argument", vec![]));
                    };
                    if handler_items.is_empty() {
                        continue;
                    }

                    if signal_matches(&handler_items[0], &no_catch.symbol) {
                        let mut frame = HashMap::new();
                        if var != "nil" {
                            frame.insert(var.clone(), make_signal_binding_value(&no_catch));
                        }
                        self.dynamic.push(frame);
                        let result = self.sf_progn(&handler_items[1..]);
                        self.dynamic.pop();
                        return result;
                    }
                }

                Err(Flow::Throw { tag, value })
            }
        }
    }

    fn sf_byte_code_literal(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 1 {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![
                    Value::symbol("byte-code-literal"),
                    Value::Int(tail.len() as i64),
                ],
            ));
        }

        let Expr::Vector(items) = &tail[0] else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("vectorp"), quote_to_value(&tail[0])],
            ));
        };

        let values = items.iter().map(quote_to_value).collect::<Vec<_>>();
        Ok(super::compiled_literal::maybe_coerce_compiled_literal_function(Value::vector(values)))
    }

    fn sf_defalias(&mut self, tail: &[Expr]) -> EvalResult {
        if !(2..=3).contains(&tail.len()) {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("defalias"), Value::Int(tail.len() as i64)],
            ));
        }
        let sym = self.eval(&tail[0])?;
        let def = self.eval(&tail[1])?;
        if tail.len() > 2 {
            let _ = self.eval(&tail[2])?;
        }
        self.defalias_value(sym, def)
    }

    fn sf_provide(&mut self, tail: &[Expr]) -> EvalResult {
        if !(1..=2).contains(&tail.len()) {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("provide"), Value::Int(tail.len() as i64)],
            ));
        }
        let feature = self.eval(&tail[0])?;
        let subfeatures = if tail.len() > 1 {
            Some(self.eval(&tail[1])?)
        } else {
            None
        };
        self.provide_value(feature, subfeatures)
    }

    fn sf_require(&mut self, tail: &[Expr]) -> EvalResult {
        if !(1..=3).contains(&tail.len()) {
            return Err(signal(
                "wrong-number-of-arguments",
                vec![Value::symbol("require"), Value::Int(tail.len() as i64)],
            ));
        }
        let feature = self.eval(&tail[0])?;
        let filename = if tail.len() > 1 {
            Some(self.eval(&tail[1])?)
        } else {
            None
        };
        let noerror = if tail.len() > 2 {
            Some(self.eval(&tail[2])?)
        } else {
            None
        };
        self.require_value(feature, filename, noerror)
    }

    pub(crate) fn defalias_value(&mut self, sym: Value, def: Value) -> EvalResult {
        let def = super::compiled_literal::maybe_coerce_compiled_literal_function(def);
        let name = sym.as_symbol_name().map(str::to_string).ok_or_else(|| {
            signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), sym.clone()],
            )
        })?;
        if name == "nil" {
            return Err(signal("setting-constant", vec![Value::symbol("nil")]));
        }
        if builtins::would_create_function_alias_cycle(self, &name, &def) {
            return Err(signal(
                "cyclic-function-indirection",
                vec![Value::symbol(name.clone())],
            ));
        }
        self.obarray.set_symbol_function(&name, def);
        Ok(sym)
    }

    pub(crate) fn provide_value(
        &mut self,
        feature: Value,
        subfeatures: Option<Value>,
    ) -> EvalResult {
        let name = match &feature {
            Value::Symbol(s) => s.clone(),
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), feature],
                ))
            }
        };
        if let Some(value) = subfeatures {
            self.obarray
                .put_property(&name, "subfeatures", value.clone());
        }
        self.add_feature(&name);
        Ok(feature)
    }

    pub(crate) fn require_value(
        &mut self,
        feature: Value,
        filename: Option<Value>,
        noerror: Option<Value>,
    ) -> EvalResult {
        let name = match &feature {
            Value::Symbol(s) => s.clone(),
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), feature],
                ))
            }
        };
        if self.has_feature(&name) {
            return Ok(Value::symbol(name));
        }

        if self.require_stack.iter().any(|feature| feature == &name) {
            return Err(signal(
                "error",
                vec![Value::string(format!(
                    "Recursive require for feature '{}'",
                    name
                ))],
            ));
        }
        self.require_stack.push(name.clone());

        let result = (|| -> EvalResult {
            let filename = match filename {
                Some(Value::Str(s)) => (*s).clone(),
                Some(_) | None => name.clone(),
            };

            let load_path = super::load::get_load_path(&self.obarray);
            match super::load::find_file_in_load_path(&filename, &load_path) {
                Some(path) => {
                    self.load_file_internal(&path)?;
                    if self.has_feature(&name) {
                        Ok(Value::symbol(name))
                    } else {
                        Err(signal(
                            "error",
                            vec![Value::string(format!(
                                "Required feature '{}' was not provided",
                                name
                            ))],
                        ))
                    }
                }
                None => {
                    if noerror.is_some_and(|value| value.is_truthy()) {
                        return Ok(Value::Nil);
                    }
                    Err(signal(
                        "file-missing",
                        vec![Value::string(format!(
                            "Cannot open load file: no such file or directory, {}",
                            name
                        ))],
                    ))
                }
            }
        })();
        let _ = self.require_stack.pop();
        result
    }

    fn sf_with_current_buffer(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let buf_val = self.eval(&tail[0])?;
        let target_id = match &buf_val {
            Value::Buffer(id) => *id,
            Value::Str(s) => self.buffers.find_buffer_by_name(s).ok_or_else(|| {
                signal("error", vec![Value::string(format!("No buffer named {s}"))])
            })?,
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("bufferp"), other.clone()],
                ))
            }
        };
        // Save current buffer, switch, run body, restore
        let saved = self.buffers.current_buffer().map(|b| b.id);
        self.buffers.set_current(target_id);
        let result = self.sf_progn(&tail[1..]);
        if let Some(saved_id) = saved {
            self.buffers.set_current(saved_id);
        }
        result
    }

    fn sf_save_excursion(&mut self, tail: &[Expr]) -> EvalResult {
        // Save current buffer, point, and mark; restore after body
        let saved_buf = self.buffers.current_buffer().map(|b| b.id);
        let (saved_pt, saved_mark) = match self.buffers.current_buffer() {
            Some(b) => (b.pt, b.mark),
            None => (0, None),
        };
        let result = self.sf_progn(tail);
        // Restore
        if let Some(buf_id) = saved_buf {
            self.buffers.set_current(buf_id);
            if let Some(buf) = self.buffers.get_mut(buf_id) {
                buf.pt = saved_pt;
                buf.mark = saved_mark;
            }
        }
        result
    }

    fn sf_save_mark_and_excursion(&mut self, tail: &[Expr]) -> EvalResult {
        // Save mark-active dynamic/global state in addition to save-excursion state.
        let saved_mark_active = match self.eval_symbol("mark-active") {
            Ok(value) => value,
            Err(Flow::Signal(sig)) if sig.symbol == "void-variable" => Value::Nil,
            Err(flow) => return Err(flow),
        };
        let result = self.sf_save_excursion(tail);
        self.assign("mark-active", saved_mark_active);
        result
    }

    fn sf_save_restriction(&mut self, tail: &[Expr]) -> EvalResult {
        // Save narrowing boundaries; restore after body
        let (saved_begv, saved_zv) = match self.buffers.current_buffer() {
            Some(b) => (b.begv, b.zv),
            None => (0, 0),
        };
        let result = self.sf_progn(tail);
        if let Some(buf) = self.buffers.current_buffer_mut() {
            buf.begv = saved_begv;
            buf.zv = saved_zv;
            buf.pt = buf.pt.clamp(buf.begv, buf.zv);
        }
        result
    }

    fn sf_save_match_data(&mut self, tail: &[Expr]) -> EvalResult {
        // Save global match data; restore after body (including non-local exits).
        let saved_match_data = self.match_data.clone();
        let result = self.sf_progn(tail);
        self.match_data = saved_match_data;
        result
    }

    fn sf_ignore_errors(&mut self, tail: &[Expr]) -> EvalResult {
        match self.sf_progn(tail) {
            Ok(val) => Ok(val),
            Err(Flow::Signal(_)) => Ok(Value::Nil),
            Err(flow) => Err(flow),
        }
    }

    fn sf_dotimes(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::List(spec) = &tail[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        if spec.len() < 2 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::Symbol(var) = &spec[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        let count = self.eval(&spec[1])?;
        let count = match &count {
            Value::Int(n) => *n,
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("integerp"), count],
                ))
            }
        };

        self.dynamic.push(HashMap::new());
        for i in 0..count {
            if let Some(frame) = self.dynamic.last_mut() {
                frame.insert(var.clone(), Value::Int(i));
            }
            self.sf_progn(&tail[1..])?;
        }
        // Result value (third element of spec, or nil)
        let result = if spec.len() > 2 {
            if let Some(frame) = self.dynamic.last_mut() {
                frame.insert(var.clone(), Value::Int(count));
            }
            self.eval(&spec[2])?
        } else {
            Value::Nil
        };
        self.dynamic.pop();
        Ok(result)
    }

    fn sf_dolist(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::List(spec) = &tail[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        if spec.len() < 2 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::Symbol(var) = &spec[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        let list_val = self.eval(&spec[1])?;
        let items = list_to_vec(&list_val).unwrap_or_default();

        self.dynamic.push(HashMap::new());
        for item in items {
            if let Some(frame) = self.dynamic.last_mut() {
                frame.insert(var.clone(), item);
            }
            self.sf_progn(&tail[1..])?;
        }
        let result = if spec.len() > 2 {
            if let Some(frame) = self.dynamic.last_mut() {
                frame.insert(var.clone(), Value::Nil);
            }
            self.eval(&spec[2])?
        } else {
            Value::Nil
        };
        self.dynamic.pop();
        Ok(result)
    }

    // -----------------------------------------------------------------------
    // Lambda / Function application
    // -----------------------------------------------------------------------

    pub(crate) fn eval_lambda(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }

        let params = self.parse_lambda_params(&tail[0])?;

        // Extract docstring if present as the first body element.
        let (docstring, body_start) = match tail.get(1) {
            Some(Expr::Str(s)) => (Some(s.clone()), 2),
            _ => (None, 1),
        };

        // Capture lexical environment for closures (when lexical-binding is on)
        let env = if self.lexical_binding() && !self.lexenv.is_empty() {
            Some(self.lexenv.clone())
        } else {
            None
        };

        Ok(Value::Lambda(std::sync::Arc::new(LambdaData {
            params,
            body: tail[body_start..].to_vec(),
            env,
            docstring,
        })))
    }

    fn parse_lambda_params(&self, expr: &Expr) -> Result<LambdaParams, Flow> {
        match expr {
            Expr::Symbol(s) if s == "nil" => Ok(LambdaParams::simple(vec![])),
            Expr::List(items) => {
                let mut required = Vec::new();
                let mut optional = Vec::new();
                let mut rest = None;
                let mut mode = 0; // 0=required, 1=optional, 2=rest

                for item in items {
                    let Expr::Symbol(name) = item else {
                        return Err(signal("wrong-type-argument", vec![]));
                    };
                    match name.as_str() {
                        "&optional" => {
                            mode = 1;
                            continue;
                        }
                        "&rest" => {
                            mode = 2;
                            continue;
                        }
                        _ => {}
                    }
                    match mode {
                        0 => required.push(name.clone()),
                        1 => optional.push(name.clone()),
                        2 => {
                            rest = Some(name.clone());
                            break;
                        }
                        _ => unreachable!(),
                    }
                }

                Ok(LambdaParams {
                    required,
                    optional,
                    rest,
                })
            }
            _ => Err(signal("wrong-type-argument", vec![])),
        }
    }

    /// Apply a function value to evaluated arguments.
    pub(crate) fn apply(&mut self, function: Value, args: Vec<Value>) -> EvalResult {
        match function {
            Value::ByteCode(bc) => {
                self.refresh_features_from_variable();
                let mut vm = super::bytecode::Vm::new(
                    &mut self.obarray,
                    &mut self.dynamic,
                    &mut self.lexenv,
                    &mut self.features,
                    &mut self.buffers,
                    &mut self.match_data,
                );
                let result = vm.execute(&bc, args);
                self.sync_features_variable();
                result
            }
            Value::Lambda(lambda) | Value::Macro(lambda) => self.apply_lambda(&lambda, args),
            Value::Subr(name) => self.apply_subr_object(&name, args, true),
            Value::Symbol(name) => {
                self.apply_named_callable(&name, args, Value::Subr(name.clone()), true)
            }
            Value::True => self.apply_named_callable("t", args, Value::Subr("t".to_string()), true),
            Value::Keyword(name) => {
                self.apply_named_callable(&name, args, Value::Subr(name.clone()), true)
            }
            Value::Nil => Err(signal("void-function", vec![Value::symbol("nil")])),
            other => {
                if super::autoload::is_autoload_value(&other) {
                    Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("symbolp"), other],
                    ))
                } else {
                    Err(signal("invalid-function", vec![other]))
                }
            }
        }
    }

    #[inline]
    fn apply_subr_object(
        &mut self,
        name: &str,
        args: Vec<Value>,
        rewrite_builtin_wrong_arity: bool,
    ) -> EvalResult {
        if super::subr_info::is_special_form(name) {
            return Err(signal(
                "invalid-function",
                vec![Value::Subr(name.to_string())],
            ));
        }
        if super::subr_info::is_evaluator_callable_name(name) {
            return self.apply_evaluator_callable(name, args);
        }
        if let Some(result) = builtins::dispatch_builtin(self, name, args) {
            if rewrite_builtin_wrong_arity {
                result.map_err(|flow| rewrite_wrong_arity_function_object(flow, name))
            } else {
                result
            }
        } else {
            Err(signal("void-function", vec![Value::symbol(name)]))
        }
    }

    #[inline]
    fn resolve_named_call_target(&mut self, name: &str) -> NamedCallTarget {
        let function_epoch = self.obarray.function_epoch();
        if let Some(cache) = &self.named_call_cache {
            if cache.symbol == name && cache.function_epoch == function_epoch {
                return cache.target.clone();
            }
        }

        let target = if let Some(func) = self.obarray.symbol_function(name).cloned() {
            match &func {
                Value::Nil => NamedCallTarget::Void,
                // `(fset 'foo (symbol-function 'foo))` writes `#<subr foo>` into
                // the function cell. Treat this as a direct builtin/special-form
                // callable, not an obarray indirection cycle.
                Value::Subr(bound_name) if bound_name == name => {
                    if super::subr_info::is_evaluator_callable_name(name) {
                        NamedCallTarget::EvaluatorCallable
                    } else if super::subr_info::is_special_form(name) {
                        NamedCallTarget::SpecialForm
                    } else {
                        NamedCallTarget::Probe
                    }
                }
                _ => NamedCallTarget::Obarray(func),
            }
        } else if self.obarray.is_function_unbound(name) {
            NamedCallTarget::Void
        } else if super::subr_info::is_evaluator_callable_name(name) {
            NamedCallTarget::EvaluatorCallable
        } else if super::subr_info::is_special_form(name) {
            NamedCallTarget::SpecialForm
        } else {
            NamedCallTarget::Probe
        };

        self.named_call_cache = Some(NamedCallCache {
            symbol: name.to_string(),
            function_epoch,
            target: target.clone(),
        });

        target
    }

    #[inline]
    fn apply_named_callable(
        &mut self,
        name: &str,
        args: Vec<Value>,
        invalid_fn: Value,
        rewrite_builtin_wrong_arity: bool,
    ) -> EvalResult {
        match self.resolve_named_call_target(name) {
            NamedCallTarget::Obarray(func) => {
                if super::autoload::is_autoload_value(&func) {
                    return self.apply_named_autoload_callable(
                        name,
                        func,
                        args,
                        rewrite_builtin_wrong_arity,
                    );
                }
                let alias_target = match &func {
                    Value::Symbol(target) => Some(target.clone()),
                    Value::Subr(bound_name) if bound_name != name => Some(bound_name.clone()),
                    _ => None,
                };
                let result = match self.apply(func, args) {
                    Err(Flow::Signal(sig)) if sig.symbol == "invalid-function" => {
                        Err(signal("invalid-function", vec![Value::symbol(name)]))
                    }
                    other => other,
                };
                if let Some(target) = alias_target {
                    if rewrite_builtin_wrong_arity {
                        result
                    } else {
                        result.map_err(|flow| {
                            rewrite_wrong_arity_alias_function_object(flow, name, &target)
                        })
                    }
                } else {
                    result
                }
            }
            NamedCallTarget::EvaluatorCallable => self.apply_evaluator_callable(name, args),
            NamedCallTarget::Probe => {
                if let Some(result) = builtins::dispatch_builtin(self, name, args) {
                    self.named_call_cache = Some(NamedCallCache {
                        symbol: name.to_string(),
                        function_epoch: self.obarray.function_epoch(),
                        target: NamedCallTarget::Builtin,
                    });
                    if rewrite_builtin_wrong_arity {
                        result.map_err(|flow| rewrite_wrong_arity_function_object(flow, name))
                    } else {
                        result
                    }
                } else {
                    self.named_call_cache = Some(NamedCallCache {
                        symbol: name.to_string(),
                        function_epoch: self.obarray.function_epoch(),
                        target: NamedCallTarget::Void,
                    });
                    Err(signal("void-function", vec![Value::symbol(name)]))
                }
            }
            NamedCallTarget::Builtin => {
                if let Some(result) = builtins::dispatch_builtin(self, name, args) {
                    if rewrite_builtin_wrong_arity {
                        result.map_err(|flow| rewrite_wrong_arity_function_object(flow, name))
                    } else {
                        result
                    }
                } else {
                    self.named_call_cache = Some(NamedCallCache {
                        symbol: name.to_string(),
                        function_epoch: self.obarray.function_epoch(),
                        target: NamedCallTarget::Void,
                    });
                    Err(signal("void-function", vec![Value::symbol(name)]))
                }
            }
            NamedCallTarget::SpecialForm => Err(signal("invalid-function", vec![invalid_fn])),
            NamedCallTarget::Void => Err(signal("void-function", vec![Value::symbol(name)])),
        }
    }

    fn apply_named_autoload_callable(
        &mut self,
        name: &str,
        autoload_form: Value,
        args: Vec<Value>,
        rewrite_builtin_wrong_arity: bool,
    ) -> EvalResult {
        // Startup wrappers often expose autoload-shaped function cells for names
        // backed by builtins. Keep the autoload shape while preserving callability.
        if super::builtin_registry::is_dispatch_builtin_name(name) {
            if let Some(result) = builtins::dispatch_builtin(self, name, args.clone()) {
                return if rewrite_builtin_wrong_arity {
                    result.map_err(|flow| rewrite_wrong_arity_function_object(flow, name))
                } else {
                    result
                };
            }
        }

        let loaded = super::autoload::builtin_autoload_do_load(
            self,
            vec![autoload_form, Value::symbol(name)],
        )?;
        match self.apply(loaded, args) {
            Err(Flow::Signal(sig)) if sig.symbol == "invalid-function" => {
                Err(signal("invalid-function", vec![Value::symbol(name)]))
            }
            other => other,
        }
    }

    fn apply_evaluator_callable(&mut self, name: &str, args: Vec<Value>) -> EvalResult {
        match name {
            "throw" => {
                if args.len() != 2 {
                    return Err(signal(
                        "wrong-number-of-arguments",
                        vec![
                            Value::Subr("throw".to_string()),
                            Value::Int(args.len() as i64),
                        ],
                    ));
                }
                Err(Flow::Throw {
                    tag: args[0].clone(),
                    value: args[1].clone(),
                })
            }
            _ => Err(signal("void-function", vec![Value::symbol(name)])),
        }
    }

    fn apply_lambda(&mut self, lambda: &LambdaData, args: Vec<Value>) -> EvalResult {
        let params = &lambda.params;

        // Arity check
        if args.len() < params.min_arity() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        if let Some(max) = params.max_arity() {
            if args.len() > max {
                return Err(signal("wrong-number-of-arguments", vec![]));
            }
        }

        let mut frame = HashMap::new();
        let mut arg_idx = 0;

        // Required params
        for param in &params.required {
            frame.insert(param.clone(), args[arg_idx].clone());
            arg_idx += 1;
        }

        // Optional params
        for param in &params.optional {
            if arg_idx < args.len() {
                frame.insert(param.clone(), args[arg_idx].clone());
                arg_idx += 1;
            } else {
                frame.insert(param.clone(), Value::Nil);
            }
        }

        // Rest param
        if let Some(ref rest_name) = params.rest {
            let rest_args: Vec<Value> = args[arg_idx..].to_vec();
            frame.insert(rest_name.clone(), Value::list(rest_args));
        }

        // If closure has a captured lexenv, restore it
        let saved_lexenv = if let Some(ref env) = lambda.env {
            let old = std::mem::replace(&mut self.lexenv, env.clone());
            // Push param bindings as a new lexical frame on top of captured env
            self.lexenv.push(frame);
            Some(old)
        } else {
            // Dynamic binding (no captured lexenv)
            self.dynamic.push(frame);
            None
        };
        let saved_lexical_mode = if lambda.env.is_some() {
            let old = self.lexical_binding();
            self.set_lexical_binding(true);
            Some(old)
        } else {
            None
        };

        let result = self.sf_progn(&lambda.body);

        if let Some(old_mode) = saved_lexical_mode {
            self.set_lexical_binding(old_mode);
        }
        if let Some(old_lexenv) = saved_lexenv {
            self.lexenv = old_lexenv;
        } else {
            self.dynamic.pop();
        }
        result
    }

    // -----------------------------------------------------------------------
    // Macro expansion
    // -----------------------------------------------------------------------

    fn expand_macro(&mut self, macro_val: Value, args: &[Expr]) -> Result<Expr, Flow> {
        let Value::Macro(lambda) = macro_val else {
            return Err(signal("invalid-macro", vec![]));
        };

        // Convert unevaluated args to values (quoted forms)
        let arg_values: Vec<Value> = args.iter().map(quote_to_value).collect();

        // Apply the macro body
        let expanded_value = self.apply_lambda(&lambda, arg_values)?;

        // Convert value back to expr for re-evaluation
        Ok(value_to_expr(&expanded_value))
    }

    // -----------------------------------------------------------------------
    // Variable assignment
    // -----------------------------------------------------------------------

    pub(crate) fn assign(&mut self, name: &str, value: Value) {
        // If lexical binding and not special, check lexenv first
        if self.lexical_binding() && !self.obarray.is_special(name) {
            for frame in self.lexenv.iter_mut().rev() {
                if frame.contains_key(name) {
                    frame.insert(name.to_string(), value);
                    return;
                }
            }
        }

        // Search dynamic frames (inner to outer)
        for frame in self.dynamic.iter_mut().rev() {
            if frame.contains_key(name) {
                frame.insert(name.to_string(), value);
                return;
            }
        }

        // Update existing buffer-local binding if present.
        if let Some(buf) = self.buffers.current_buffer_mut() {
            if buf.get_buffer_local(name).is_some() {
                buf.set_buffer_local(name, value);
                return;
            }
        }

        // Auto-local variables become local upon assignment.
        if self.custom.is_auto_buffer_local(name) {
            if let Some(buf) = self.buffers.current_buffer_mut() {
                buf.set_buffer_local(name, value);
                return;
            }
        }

        // Fall through to obarray value cell
        self.obarray.set_symbol_value(name, value);
    }
}

fn rewrite_wrong_arity_function_object(flow: Flow, name: &str) -> Flow {
    match flow {
        Flow::Signal(mut sig) => {
            if sig.symbol == "wrong-number-of-arguments"
                && sig.raw_data.is_none()
                && !sig.data.is_empty()
                && sig.data[0].as_symbol_name() == Some(name)
            {
                sig.data[0] = Value::Subr(name.to_string());
            }
            Flow::Signal(sig)
        }
        other => other,
    }
}

fn rewrite_wrong_arity_alias_function_object(flow: Flow, alias: &str, target: &str) -> Flow {
    match flow {
        Flow::Signal(mut sig) => {
            let target_is_payload = sig.data.first().is_some_and(|value| match value {
                Value::Subr(name) => name == target || name == alias,
                _ => {
                    value.as_symbol_name() == Some(target) || value.as_symbol_name() == Some(alias)
                }
            });
            if sig.symbol == "wrong-number-of-arguments"
                && !sig.data.is_empty()
                && target_is_payload
            {
                sig.data[0] = Value::symbol(alias);
            }
            Flow::Signal(sig)
        }
        other => other,
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert an Expr AST node to a Value (for quote).
pub fn quote_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Int(v) => Value::Int(*v),
        Expr::Float(v) => Value::Float(*v),
        Expr::Str(s) => Value::string(s.clone()),
        Expr::Char(c) => Value::Char(*c),
        Expr::Keyword(s) => Value::Keyword(s.clone()),
        Expr::Bool(true) => Value::True,
        Expr::Bool(false) => Value::Nil,
        Expr::Symbol(s) if s == "nil" => Value::Nil,
        Expr::Symbol(s) if s == "t" => Value::True,
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(items) => {
            let quoted = items.iter().map(quote_to_value).collect::<Vec<_>>();
            Value::list(quoted)
        }
        Expr::DottedList(items, last) => {
            let head_vals: Vec<Value> = items.iter().map(quote_to_value).collect();
            let tail_val = quote_to_value(last);
            head_vals
                .into_iter()
                .rev()
                .fold(tail_val, |acc, item| Value::cons(item, acc))
        }
        Expr::Vector(items) => {
            let vals = items.iter().map(quote_to_value).collect();
            Value::vector(vals)
        }
    }
}

/// Public wrapper for value_to_expr (used by builtins::eval).
pub(crate) fn value_to_expr_pub(value: &Value) -> Expr {
    value_to_expr(value)
}

/// Convert a Value back to an Expr (for macro expansion).
fn value_to_expr(value: &Value) -> Expr {
    match value {
        Value::Nil => Expr::Symbol("nil".into()),
        Value::True => Expr::Symbol("t".into()),
        Value::Int(n) => Expr::Int(*n),
        Value::Float(f) => Expr::Float(*f),
        Value::Symbol(s) => Expr::Symbol(s.clone()),
        Value::Keyword(s) => Expr::Keyword(s.clone()),
        Value::Str(s) => Expr::Str((**s).clone()),
        Value::Char(c) => Expr::Char(*c),
        Value::Cons(_) => {
            if let Some(items) = list_to_vec(value) {
                Expr::List(items.iter().map(value_to_expr).collect())
            } else {
                // Improper list — best effort
                Expr::Symbol(format!("{}", value))
            }
        }
        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            Expr::Vector(items.iter().map(value_to_expr).collect())
        }
        _ => Expr::Symbol(format!("{}", value)),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::{format_eval_result, parse_forms};

    fn eval_one(src: &str) -> String {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        let result = ev.eval_expr(&forms[0]);
        format_eval_result(&result)
    }

    fn eval_all(src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        ev.eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect()
    }

    #[test]
    fn basic_arithmetic() {
        assert_eq!(eval_one("(+ 1 2)"), "OK 3");
        assert_eq!(eval_one("(- 10 3)"), "OK 7");
        assert_eq!(eval_one("(* 4 5)"), "OK 20");
        assert_eq!(eval_one("(/ 10 3)"), "OK 3");
        assert_eq!(eval_one("(% 10 3)"), "OK 1");
        assert_eq!(eval_one("(1+ 5)"), "OK 6");
        assert_eq!(eval_one("(1- 5)"), "OK 4");
    }

    #[test]
    fn recent_input_events_are_bounded() {
        let mut ev = Evaluator::new();
        for i in 0..(RECENT_INPUT_EVENT_LIMIT + 1) {
            ev.record_input_event(Value::Int(i as i64));
        }
        let recent = ev.recent_input_events();
        assert_eq!(recent.len(), RECENT_INPUT_EVENT_LIMIT);
        assert_eq!(recent[0], Value::Int(1));
        assert_eq!(
            recent.last(),
            Some(&Value::Int(RECENT_INPUT_EVENT_LIMIT as i64))
        );
    }

    #[test]
    fn float_arithmetic() {
        assert_eq!(eval_one("(+ 1.0 2.0)"), "OK 3.0");
        assert_eq!(eval_one("(+ 1 2.0)"), "OK 3.0"); // int promoted to float
        assert_eq!(eval_one("(/ 10.0 3.0)"), "OK 3.3333333333333335");
    }

    #[test]
    fn comparisons() {
        assert_eq!(eval_one("(< 1 2)"), "OK t");
        assert_eq!(eval_one("(> 1 2)"), "OK nil");
        assert_eq!(eval_one("(= 3 3)"), "OK t");
        assert_eq!(eval_one("(<= 3 3)"), "OK t");
        assert_eq!(eval_one("(>= 5 3)"), "OK t");
        assert_eq!(eval_one("(/= 1 2)"), "OK t");
    }

    #[test]
    fn type_predicates() {
        assert_eq!(eval_one("(integerp 42)"), "OK t");
        assert_eq!(eval_one("(floatp 3.14)"), "OK t");
        assert_eq!(eval_one("(stringp \"hello\")"), "OK t");
        assert_eq!(eval_one("(symbolp 'foo)"), "OK t");
        assert_eq!(eval_one("(consp '(1 2))"), "OK t");
        assert_eq!(eval_one("(null nil)"), "OK t");
        assert_eq!(eval_one("(null t)"), "OK nil");
        assert_eq!(eval_one("(listp nil)"), "OK t");
    }

    #[test]
    fn string_operations() {
        assert_eq!(
            eval_one(r#"(concat "hello" " " "world")"#),
            r#"OK "hello world""#
        );
        assert_eq!(eval_one(r#"(substring "hello" 1 3)"#), r#"OK "el""#);
        assert_eq!(eval_one(r#"(length "hello")"#), "OK 5");
        assert_eq!(eval_one(r#"(upcase "hello")"#), r#"OK "HELLO""#);
        assert_eq!(eval_one(r#"(string-equal "abc" "abc")"#), "OK t");
    }

    #[test]
    fn and_or_cond() {
        assert_eq!(eval_one("(and 1 2 3)"), "OK 3");
        assert_eq!(eval_one("(and 1 nil 3)"), "OK nil");
        assert_eq!(eval_one("(or nil nil 3)"), "OK 3");
        assert_eq!(eval_one("(or nil nil nil)"), "OK nil");
        assert_eq!(eval_one("(cond (nil 1) (t 2))"), "OK 2");
    }

    #[test]
    fn while_loop() {
        assert_eq!(
            eval_one("(let ((x 0)) (while (< x 5) (setq x (1+ x))) x)"),
            "OK 5"
        );
    }

    #[test]
    fn defvar_only_sets_if_unbound() {
        let results = eval_all("(defvar x 42) x (defvar x 99) x");
        assert_eq!(results, vec!["OK x", "OK 42", "OK x", "OK 42"]);
    }

    #[test]
    fn defvar_and_defconst_error_payloads_match_oracle_edges() {
        let results = eval_all(
            "(condition-case err (defvar) (error err))
             (condition-case err (defvar 1) (error err))
             (condition-case err (defvar 'vm-dv) (error err))
             (condition-case err (defvar vm-dv 1 \"doc\" t) (error err))
             (condition-case err (defconst) (error err))
             (condition-case err (defconst vm-dc) (error err))
             (condition-case err (defconst 1 2) (error err))
             (condition-case err (defconst 'vm-dc 1) (error err))
             (condition-case err (defconst vm-dc 1 \"doc\" t) (error err))",
        );
        assert_eq!(results[0], "OK (wrong-number-of-arguments defvar 0)");
        assert_eq!(results[1], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[2], "OK (wrong-type-argument symbolp 'vm-dv)");
        assert_eq!(results[3], "OK (error \"Too many arguments\")");
        assert_eq!(results[4], "OK (wrong-number-of-arguments defconst 0)");
        assert_eq!(results[5], "OK (wrong-number-of-arguments defconst 1)");
        assert_eq!(results[6], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[7], "OK (wrong-type-argument symbolp 'vm-dc)");
        assert_eq!(results[8], "OK (error \"Too many arguments\")");
    }

    #[test]
    fn setq_local_makes_binding_buffer_local() {
        let result = eval_one("(with-temp-buffer (setq-local vm-x 7) vm-x)");
        assert_eq!(result, "OK 7");
    }

    #[test]
    fn defmacro_works() {
        let result = eval_all(
            "(defmacro my-when (cond &rest body)
               (list 'if cond (cons 'progn body)))
             (my-when t 1 2 3)",
        );
        assert_eq!(result[1], "OK 3");
    }

    #[test]
    fn defun_and_defmacro_allow_empty_body() {
        let results = eval_all(
            "(defun vm-empty-f nil)
             (vm-empty-f)
             (defmacro vm-empty-m nil)
             (vm-empty-m)",
        );
        assert_eq!(results[0], "OK vm-empty-f");
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK vm-empty-m");
        assert_eq!(results[3], "OK nil");
    }

    #[test]
    fn defun_and_defmacro_error_payloads_match_oracle_edges() {
        let results = eval_all(
            "(condition-case err (defun) (error err))
             (condition-case err (defun 1 nil) (error err))
             (condition-case err (defun 'vm-df nil 1) (error err))
             (condition-case err (defmacro) (error err))
             (condition-case err (defmacro 1 nil) (error err))
             (condition-case err (defmacro 'vm-dm nil 1) (error err))",
        );
        assert_eq!(results[0], "OK (wrong-number-of-arguments (2 . 2) 0)");
        assert_eq!(results[1], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[2], "OK (wrong-type-argument symbolp 'vm-df)");
        assert_eq!(results[3], "OK (wrong-number-of-arguments (2 . 2) 0)");
        assert_eq!(results[4], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[5], "OK (wrong-type-argument symbolp 'vm-dm)");
    }

    #[test]
    fn optional_and_rest_params() {
        let results = eval_all(
            "(defun f (a &optional b &rest c) (list a b c))
             (f 1)
             (f 1 2)
             (f 1 2 3 4)",
        );
        assert_eq!(results[1], "OK (1 nil nil)");
        assert_eq!(results[2], "OK (1 2 nil)");
        assert_eq!(results[3], "OK (1 2 (3 4))");
    }

    #[test]
    fn when_unless() {
        assert_eq!(eval_one("(when t 1 2 3)"), "OK 3");
        assert_eq!(eval_one("(when nil 1 2 3)"), "OK nil");
        assert_eq!(eval_one("(unless nil 1 2 3)"), "OK 3");
        assert_eq!(eval_one("(unless t 1 2 3)"), "OK nil");
    }

    #[test]
    fn hash_table_ops() {
        let results = eval_all(
            "(let ((ht (make-hash-table :test 'equal)))
               (puthash \"key\" 42 ht)
               (gethash \"key\" ht))",
        );
        assert_eq!(results[0], "OK 42");
    }

    #[test]
    fn vector_ops() {
        assert_eq!(eval_one("(aref [10 20 30] 1)"), "OK 20");
        assert_eq!(eval_one("(length [1 2 3])"), "OK 3");
    }

    #[test]
    fn vector_literals_are_self_evaluating_constants() {
        assert_eq!(eval_one("(aref [f1] 0)"), "OK f1");
        assert_eq!(eval_one("(let ((f1 'shadowed)) (aref [f1] 0))"), "OK f1");
        assert_eq!(eval_one("(aref [(+ 1 2)] 0)"), "OK (+ 1 2)");
        assert_eq!(eval_one("(let ((x 1)) (aref [x] 0))"), "OK x");
    }

    #[test]
    fn format_function() {
        assert_eq!(
            eval_one(r#"(format "hello %s, %d" "world" 42)"#),
            r#"OK "hello world, 42""#
        );
    }

    #[test]
    fn prog1() {
        assert_eq!(eval_one("(prog1 1 2 3)"), "OK 1");
    }

    #[test]
    fn function_special_form() {
        let results = eval_all(
            "(defun add1 (x) (+ x 1))
             (funcall #'add1 5)",
        );
        assert_eq!(results[1], "OK 6");
    }

    #[test]
    fn function_special_form_symbol_and_literal_payloads() {
        assert_eq!(eval_one("#'car"), "OK car");
        assert_eq!(eval_one("#'definitely-missing"), "OK definitely-missing");
        assert_eq!(
            eval_one("(condition-case err #'1 (error (car err)))"),
            "OK 1"
        );
        assert_eq!(eval_one("(equal #''(lambda) ''(lambda))"), "OK t");
    }

    #[test]
    fn lambda_captures_docstring_metadata() {
        let forms = parse_forms("(lambda nil \"lambda-doc\" nil)").expect("parse");
        let mut ev = Evaluator::new();
        let value = ev.eval_expr(&forms[0]).expect("eval");
        let Value::Lambda(data) = value else {
            panic!("expected lambda value");
        };
        assert_eq!(data.docstring.as_deref(), Some("lambda-doc"));
    }

    #[test]
    fn defmacro_captures_docstring_metadata() {
        let forms = parse_forms("(defmacro vm-doc-macro (x) \"macro-doc\" x)").expect("parse");
        let mut ev = Evaluator::new();
        ev.eval_expr(&forms[0]).expect("eval defmacro");
        let macro_val = ev
            .obarray
            .symbol_function("vm-doc-macro")
            .cloned()
            .expect("macro function cell");
        let Value::Macro(data) = macro_val else {
            panic!("expected macro value");
        };
        assert_eq!(data.docstring.as_deref(), Some("macro-doc"));
    }

    #[test]
    fn function_special_form_wrong_arity_signals() {
        assert_eq!(
            eval_one("(condition-case err (function) (error (car err)))"),
            "OK wrong-number-of-arguments"
        );
        assert_eq!(
            eval_one("(condition-case err (function 1 2) (error (car err)))"),
            "OK wrong-number-of-arguments"
        );
    }

    #[test]
    fn special_form_arity_payloads_match_oracle_edges() {
        let results = eval_all(
            "(condition-case err (if) (error err))
             (condition-case err (if t) (error err))
             (condition-case err (when) (error err))
             (condition-case err (unless) (error err))
             (condition-case err (quote) (error err))
             (condition-case err (quote 1 2) (error err))
             (condition-case err (function) (error err))
             (condition-case err (function 1 2) (error err))
             (condition-case err (prog1) (error err))
             (condition-case err (catch) (error err))
             (condition-case err (throw) (error err))
             (condition-case err (condition-case) (error err))
             (condition-case err (let) (error err))
             (condition-case err (let*) (error err))
             (condition-case err (while) (error err))
             (condition-case err (unwind-protect) (error err))",
        );
        assert_eq!(results[0], "OK (wrong-number-of-arguments if 0)");
        assert_eq!(results[1], "OK (wrong-number-of-arguments if 1)");
        assert_eq!(results[2], "OK (wrong-number-of-arguments (1 . 1) 0)");
        assert_eq!(results[3], "OK (wrong-number-of-arguments (1 . 1) 0)");
        assert_eq!(results[4], "OK (wrong-number-of-arguments quote 0)");
        assert_eq!(results[5], "OK (wrong-number-of-arguments quote 2)");
        assert_eq!(results[6], "OK (wrong-number-of-arguments function 0)");
        assert_eq!(results[7], "OK (wrong-number-of-arguments function 2)");
        assert_eq!(results[8], "OK (wrong-number-of-arguments prog1 0)");
        assert_eq!(results[9], "OK (wrong-number-of-arguments catch 0)");
        assert_eq!(results[10], "OK (wrong-number-of-arguments throw 0)");
        assert_eq!(
            results[11],
            "OK (wrong-number-of-arguments condition-case 0)"
        );
        assert_eq!(results[12], "OK (wrong-number-of-arguments let 0)");
        assert_eq!(results[13], "OK (wrong-number-of-arguments let* 0)");
        assert_eq!(results[14], "OK (wrong-number-of-arguments while 0)");
        assert_eq!(
            results[15],
            "OK (wrong-number-of-arguments unwind-protect 0)"
        );
    }

    #[test]
    fn let_dotted_binding_list_reports_listp_tail_payload() {
        assert_eq!(
            eval_one("(condition-case err (let ((x 1) . 2) x) (error err))"),
            "OK (wrong-type-argument listp 2)"
        );
        assert_eq!(
            eval_one("(condition-case err (let* ((x 1) . 2) x) (error err))"),
            "OK (wrong-type-argument listp 2)"
        );
    }

    #[test]
    fn special_form_type_payloads_match_oracle_edges() {
        let results = eval_all(
            "(condition-case err (setq x) (error err))
             (condition-case err (setq 1 2) (error err))
             (condition-case err (let ((1 2)) nil) (error err))
             (condition-case err (let* ((1 2)) nil) (error err))
             (condition-case err (cond 1) (error err))
             (condition-case err (condition-case 1 2 (error 3)) (error err))
             (condition-case err (condition-case err 2 3) (error err))
             (condition-case err (condition-case err 2 ()) (error err))",
        );
        assert_eq!(results[0], "OK (wrong-number-of-arguments setq 1)");
        assert_eq!(results[1], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[2], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[3], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[4], "OK (wrong-type-argument listp 1)");
        assert_eq!(results[5], "OK (wrong-type-argument symbolp 1)");
        assert_eq!(results[6], "OK (error \"Invalid condition handler: 3\")");
        assert_eq!(results[7], "OK 2");
    }

    #[test]
    fn mapcar_works() {
        assert_eq!(eval_one("(mapcar #'1+ '(1 2 3))"), "OK (2 3 4)");
    }

    #[test]
    fn apply_works() {
        assert_eq!(eval_one("(apply #'+ '(1 2 3))"), "OK 6");
        assert_eq!(eval_one("(apply #'+ 1 2 '(3))"), "OK 6");
    }

    #[test]
    fn apply_improper_tail_signals_wrong_type_argument() {
        assert_eq!(
            eval_one(
                "(condition-case err
                     (apply 'list '(1 . 2))
                   (error (list (car err) (nth 2 err))))"
            ),
            "OK (wrong-type-argument 2)"
        );
    }

    #[test]
    fn funcall_and_apply_nil_signal_void_function() {
        let funcall_result = eval_one(
            "(condition-case err
                 (funcall nil)
               (void-function (car err)))",
        );
        assert_eq!(funcall_result, "OK void-function");

        let apply_result = eval_one(
            "(condition-case err
                 (apply nil nil)
               (void-function (car err)))",
        );
        assert_eq!(apply_result, "OK void-function");
    }

    #[test]
    fn funcall_and_apply_non_callable_symbol_edges() {
        assert_eq!(
            eval_one("(condition-case err (funcall t) (error (car err)))"),
            "OK void-function"
        );
        assert_eq!(
            eval_one("(condition-case err (funcall :vm-matrix-keyword) (error (car err)))"),
            "OK void-function"
        );
        assert_eq!(
            eval_one("(condition-case err (funcall 'if) (error (car err)))"),
            "OK invalid-function"
        );
        assert_eq!(
            eval_one(
                "(condition-case err (funcall (symbol-function 'if) t 1 2) (error (car err)))"
            ),
            "OK invalid-function"
        );
        assert_eq!(
            eval_one("(condition-case err (apply t nil) (error (car err)))"),
            "OK void-function"
        );
        assert_eq!(
            eval_one("(condition-case err (apply :vm-matrix-keyword nil) (error (car err)))"),
            "OK void-function"
        );
        assert_eq!(
            eval_one("(condition-case err (apply 'if '(t 1 2)) (error (car err)))"),
            "OK invalid-function"
        );
    }

    #[test]
    fn funcall_throw_is_callable_and_preserves_throw_semantics() {
        assert_eq!(eval_one("(catch 'tag (funcall 'throw 'tag 42))"), "OK 42");
        assert_eq!(
            eval_one("(condition-case err (funcall 'throw 'tag 42) (error err))"),
            "OK (no-catch tag 42)"
        );
        assert_eq!(
            eval_one("(condition-case err (funcall 'throw) (error err))"),
            "OK (wrong-number-of-arguments #<subr throw> 0)"
        );
    }

    #[test]
    fn fmakunbound_masks_builtin_special_and_evaluator_callable_fallbacks() {
        let results = eval_all(
            "(fmakunbound 'car)
             (fboundp 'car)
             (symbol-function 'car)
             (condition-case err (car '(1 2)) (void-function 'void-function))
             (fmakunbound 'if)
             (fboundp 'if)
             (symbol-function 'if)
             (condition-case err (if t 1 2) (void-function 'void-function))
             (fmakunbound 'throw)
             (fboundp 'throw)
             (symbol-function 'throw)
             (condition-case err (throw 'tag 1) (void-function 'void-function))",
        );
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK nil");
        assert_eq!(results[3], "OK void-function");
        assert_eq!(results[5], "OK nil");
        assert_eq!(results[6], "OK nil");
        assert_eq!(results[7], "OK void-function");
        assert_eq!(results[9], "OK nil");
        assert_eq!(results[10], "OK nil");
        assert_eq!(results[11], "OK void-function");
    }

    #[test]
    fn fset_can_override_special_form_name_for_direct_calls() {
        let result = eval_one(
            "(let ((orig (symbol-function 'if)))
               (unwind-protect
                   (progn
                     (fset 'if (lambda (&rest _args) 'ov))
                     (if t 1 2))
                 (fset 'if orig)))",
        );
        assert_eq!(result, "OK ov");
    }

    #[test]
    fn fset_restoring_subr_object_keeps_callability() {
        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function 'car)))
                   (fset 'car orig)
                   (car '(1 2)))"
            ),
            "OK 1"
        );

        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function 'if)))
                   (fset 'if orig)
                   (if t 1 2))"
            ),
            "OK 1"
        );
    }

    #[test]
    fn funcall_subr_object_ignores_symbol_function_rebinding() {
        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function 'car))
                       (snap (symbol-function 'car)))
                   (unwind-protect
                       (progn
                         (fset 'car (lambda (&rest _) 'shadow))
                         (list (funcall snap '(1 2)) (car '(1 2))))
                     (fset 'car orig)))"
            ),
            "OK (1 shadow)"
        );
    }

    #[test]
    fn named_autoload_function_cell_for_builtin_name_remains_callable() {
        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function 'safe-date-to-time)))
                   (unwind-protect
                       (progn
                         (fset 'safe-date-to-time '(autoload \"missing-file\" nil nil nil))
                         (consp (safe-date-to-time \"1970-01-01 00:00:00 +0000\")))
                     (fset 'safe-date-to-time orig)))"
            ),
            "OK t"
        );
    }

    #[test]
    fn funcall_autoload_object_signals_wrong_type_argument_symbolp() {
        assert_eq!(
            eval_one(
                "(condition-case err
                     (funcall '(autoload \"x\" nil nil nil) 3)
                   (wrong-type-argument (list (car err) (nth 1 err) (autoloadp (nth 2 err)))))"
            ),
            "OK (wrong-type-argument symbolp t)"
        );
    }

    #[test]
    fn apply_autoload_object_signals_wrong_type_argument_symbolp() {
        assert_eq!(
            eval_one(
                "(condition-case err
                     (apply '(autoload \"x\" nil nil nil) '(3))
                   (wrong-type-argument (list (car err) (nth 1 err) (autoloadp (nth 2 err)))))"
            ),
            "OK (wrong-type-argument symbolp t)"
        );
    }

    #[test]
    fn fset_nil_reports_symbol_payload_for_void_function_calls() {
        let results = eval_all(
            "(fset 'vm-fsetnil nil)
             (fboundp 'vm-fsetnil)
             (condition-case err (vm-fsetnil) (error err))
             (condition-case err (funcall 'vm-fsetnil) (error err))
             (condition-case err (apply 'vm-fsetnil nil) (error err))
             (fset 'length nil)
             (fboundp 'length)
             (condition-case err (length '(1 2)) (error err))",
        );

        assert_eq!(results[0], "OK nil");
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK (void-function vm-fsetnil)");
        assert_eq!(results[3], "OK (void-function vm-fsetnil)");
        assert_eq!(results[4], "OK (void-function vm-fsetnil)");
        assert_eq!(results[5], "OK nil");
        assert_eq!(results[6], "OK nil");
        assert_eq!(results[7], "OK (void-function length)");
    }

    #[test]
    fn fset_noncallable_reports_symbol_payload_for_invalid_function_calls() {
        let results = eval_all(
            "(fset 'vm-fsetint 1)
             (fboundp 'vm-fsetint)
             (symbol-function 'vm-fsetint)
             (condition-case err (vm-fsetint) (error err))
             (condition-case err (funcall 'vm-fsetint) (error err))
             (condition-case err (apply 'vm-fsetint nil) (error err))",
        );

        assert_eq!(results[0], "OK 1");
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK 1");
        assert_eq!(results[3], "OK (invalid-function vm-fsetint)");
        assert_eq!(results[4], "OK (invalid-function vm-fsetint)");
        assert_eq!(results[5], "OK (invalid-function vm-fsetint)");
    }

    #[test]
    fn fset_t_function_cell_controls_funcall_and_apply_behavior() {
        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function 't)))
                   (unwind-protect
                       (progn
                         (fset 't 'car)
                         (funcall t '(1 2)))
                     (fset 't orig)))"
            ),
            "OK 1"
        );

        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function 't)))
                   (unwind-protect
                       (progn
                         (fset 't 1)
                         (condition-case err (funcall t) (error err)))
                     (fset 't orig)))"
            ),
            "OK (invalid-function t)"
        );
    }

    #[test]
    fn fset_keyword_function_cell_controls_funcall_and_apply_behavior() {
        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function :k)))
                   (unwind-protect
                       (progn
                         (fset :k 'car)
                         (funcall :k '(1 2)))
                     (fset :k orig)))"
            ),
            "OK 1"
        );

        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function :k)))
                   (unwind-protect
                       (progn
                         (fset :k 'car)
                         (apply :k '((1 2))))
                     (fset :k orig)))"
            ),
            "OK 1"
        );

        assert_eq!(
            eval_one(
                "(let ((orig (symbol-function :k)))
                   (unwind-protect
                       (progn
                         (fset :k 1)
                         (condition-case err (funcall :k) (error err)))
                     (fset :k orig)))"
            ),
            "OK (invalid-function :k)"
        );
    }

    #[test]
    fn named_call_cache_invalidates_on_function_cell_mutation() {
        let results = eval_all(
            "(condition-case err
                 (funcall 'vm-cache-target)
               (error (car err)))
             (fset 'vm-cache-target (lambda () 9))
             (funcall 'vm-cache-target)
             (fset 'vm-cache-target (lambda () 11))
             (funcall 'vm-cache-target)",
        );
        assert_eq!(results[0], "OK void-function");
        assert_eq!(results[2], "OK 9");
        assert_eq!(results[4], "OK 11");
    }

    #[test]
    fn funcall_builtin_wrong_arity_uses_subr_object_payload() {
        assert_eq!(
            eval_one("(condition-case err (car) (error (subrp (nth 1 err))))"),
            "OK nil"
        );
        assert_eq!(
            eval_one("(condition-case err (funcall 'car) (error (subrp (nth 1 err))))"),
            "OK t"
        );
    }

    #[test]
    fn condition_case_catches_uncaught_throw_as_no_catch() {
        assert_eq!(
            eval_one("(condition-case err (throw 'tag 42) (error (car err)))"),
            "OK no-catch"
        );
        assert_eq!(
            eval_one("(condition-case err (exit-minibuffer) (error (car err)))"),
            "OK no-catch"
        );
        assert_eq!(
            eval_one("(condition-case err (exit-minibuffer) (no-catch err))"),
            "OK (no-catch exit nil)"
        );
    }

    #[test]
    fn backward_compat_core_forms() {
        // Same tests as original elisp.rs
        let source = r#"
        (+ 1 2)
        (let ((x 1)) (setq x (+ x 2)) x)
        (let ((lst '(1 2))) (setcar lst 9) lst)
        (catch 'tag (throw 'tag 42))
        (condition-case e (/ 1 0) (arith-error 'div-zero))
        (let ((x 1))
          (let ((f (lambda () x)))
            (let ((x 2))
              (funcall f))))
        "#;

        let forms = parse_forms(source).expect("parse");
        let mut ev = Evaluator::new();
        let rendered: Vec<String> = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect();

        assert_eq!(
            rendered,
            vec!["OK 3", "OK 3", "OK (9 2)", "OK 42", "OK div-zero", "OK 2"]
        );
    }

    #[test]
    fn excessive_recursion_detected() {
        let results = eval_all("(defun inf () (inf))\n(inf)");
        // Second form should trigger excessive nesting
        assert!(results[1].contains("excessive-lisp-nesting"));
    }

    #[test]
    fn lexical_binding_closure() {
        // With lexical binding, closures capture the lexical environment
        let forms = parse_forms(
            r#"
            (let ((x 1))
              (let ((f (lambda () x)))
                (let ((x 2))
                  (funcall f))))
        "#,
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        ev.set_lexical_binding(true);
        let result = format_eval_result(&ev.eval_expr(&forms[0]));
        // In lexical binding, the closure captures x=1, not x=2
        assert_eq!(result, "OK 1");
    }

    #[test]
    fn dynamic_binding_closure() {
        // Without lexical binding (default), closures see dynamic scope
        let forms = parse_forms(
            r#"
            (let ((x 1))
              (let ((f (lambda () x)))
                (let ((x 2))
                  (funcall f))))
        "#,
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        let result = format_eval_result(&ev.eval_expr(&forms[0]));
        // In dynamic binding, the lambda sees x=2 (innermost dynamic binding)
        assert_eq!(result, "OK 2");
    }

    #[test]
    fn lexical_binding_special_var_stays_dynamic() {
        // defvar makes a variable special — it stays dynamically scoped
        let forms = parse_forms(
            r#"
            (defvar my-special 10)
            (let ((my-special 20))
              (let ((f (lambda () my-special)))
                (let ((my-special 30))
                  (funcall f))))
        "#,
        )
        .expect("parse");
        let mut ev = Evaluator::new();
        ev.set_lexical_binding(true);
        let results: Vec<String> = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect();
        // my-special is declared special, so even in lexical mode it's dynamic
        assert_eq!(results[1], "OK 30");
    }

    #[test]
    fn defalias_works() {
        let results = eval_all(
            "(defun my-add (a b) (+ a b))
             (defalias 'my-plus 'my-add)
             (my-plus 3 4)",
        );
        assert_eq!(results[2], "OK 7");
    }

    #[test]
    fn defalias_rejects_self_alias_cycle() {
        let result = eval_one(
            "(condition-case err
                 (defalias 'vm-da-self 'vm-da-self)
               (error err))",
        );
        assert_eq!(result, "OK (cyclic-function-indirection vm-da-self)");
    }

    #[test]
    fn defalias_rejects_two_node_alias_cycle() {
        let results = eval_all(
            "(defalias 'vm-da-a 'vm-da-b)
             (condition-case err
                 (defalias 'vm-da-b 'vm-da-a)
               (error err))",
        );
        assert_eq!(results[0], "OK vm-da-a");
        assert_eq!(results[1], "OK (cyclic-function-indirection vm-da-b)");
    }

    #[test]
    fn defalias_nil_signals_setting_constant() {
        let result = eval_one(
            "(condition-case err
                 (defalias nil 'car)
               (error err))",
        );
        assert_eq!(result, "OK (setting-constant nil)");
    }

    #[test]
    fn defalias_t_accepts_symbol_cell_updates() {
        let results = eval_all(
            "(defalias t 'car)
             (symbol-function t)",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK car");
    }

    #[test]
    fn defalias_enforces_argument_count() {
        let results = eval_all(
            "(condition-case err (defalias) (error err))
             (condition-case err (defalias 'vm-da-too-few) (error err))
             (condition-case err (defalias 'vm-da-too-many 'car \"doc\" t) (error err))",
        );
        assert_eq!(results[0], "OK (wrong-number-of-arguments defalias 0)");
        assert_eq!(results[1], "OK (wrong-number-of-arguments defalias 1)");
        assert_eq!(results[2], "OK (wrong-number-of-arguments defalias 4)");
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn defalias_compiled_literal_coerces_to_compiled_function() {
        let results = eval_all(
            "(defalias 'vm-elc-placeholder #[(x) \"\\bT\\207\" [x] 1 (#$ . 83)])
             (compiled-function-p (symbol-function 'vm-elc-placeholder))
             (functionp (symbol-function 'vm-elc-placeholder))",
        );
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK t");
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn fset_compiled_literal_coerces_to_compiled_function() {
        let results = eval_all(
            "(fset 'vm-elc-fset-placeholder #[(x) \"\\bT\\207\" [x] 1 (#$ . 83)])
             (compiled-function-p (symbol-function 'vm-elc-fset-placeholder))
             (functionp (symbol-function 'vm-elc-fset-placeholder))",
        );
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK t");
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn calling_simple_compiled_literal_executes() {
        let results = eval_all(
            "(funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\207\\\" [42] 1]\")))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\207\\\" [x] 1]\")) 'vm-x)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\bT\\\\207\\\" [x] 1]\")) 77)
             (condition-case err
                 (funcall (car (read-from-string \"#[(x) \\\"\\\\bT\\\\207\\\" [x] 1]\")) 'vm-x)
               (error err))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\123\\\\207\\\" [x] 1]\")) 77)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\134\\\\207\\\" [x y] 2]\")) 7 35)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\132\\\\207\\\" [x y] 2]\")) 7 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\137\\\\207\\\" [x y] 2]\")) 7 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\125\\\\207\\\" [x y] 2]\")) 7 7)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\125\\\\207\\\" [x y] 2]\")) 7 8)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\100\\\\207\\\" [x] 1]\")) '(1 2))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\101\\\\207\\\" [x] 1]\")) '(1 2))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\203\\\\6\\\\0\\\\301\\\\207\\\\302\\\\207\\\" [x 1 2] 1]\")) t)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\203\\\\6\\\\0\\\\301\\\\207\\\\302\\\\207\\\" [x 1 2] 1]\")) nil)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\205\\\\5\\\\0\\\\301\\\\207\\\" [x 2] 1]\")) t)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\205\\\\5\\\\0\\\\301\\\\207\\\" [x 2] 1]\")) nil)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\206\\\\5\\\\0\\\\301\\\\207\\\" [x 2] 1]\")) t)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\206\\\\5\\\\0\\\\301\\\\207\\\" [x 2] 1]\")) nil)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\204\\\\6\\\\0\\\\301\\\\207\\\\302\\\\207\\\" [x 1 2] 1]\")) nil)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\204\\\\6\\\\0\\\\301\\\\207\\\\302\\\\207\\\" [x 1 2] 1]\")) t)",
        );
        assert_eq!(results[0], "OK 42");
        assert_eq!(results[1], "OK vm-x");
        assert_eq!(results[2], "OK 78");
        assert_eq!(
            results[3],
            "OK (wrong-type-argument number-or-marker-p vm-x)"
        );
        assert_eq!(results[4], "OK 76");
        assert_eq!(results[5], "OK 42");
        assert_eq!(results[6], "OK 5");
        assert_eq!(results[7], "OK 14");
        assert_eq!(results[8], "OK t");
        assert_eq!(results[9], "OK nil");
        assert_eq!(results[10], "OK 1");
        assert_eq!(results[11], "OK (2)");
        assert_eq!(results[12], "OK 1");
        assert_eq!(results[13], "OK 2");
        assert_eq!(results[14], "OK 2");
        assert_eq!(results[15], "OK nil");
        assert_eq!(results[16], "OK t");
        assert_eq!(results[17], "OK 2");
        assert_eq!(results[18], "OK 1");
        assert_eq!(results[19], "OK 2");
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn calling_extended_compiled_literal_opcodes_executes() {
        let results = eval_all(
            "(funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\127\\\\207\\\" [x y] 2]\")) 1 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\127\\\\207\\\" [x y] 2]\")) 2 1)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\126\\\\207\\\" [x y] 2]\")) 2 1)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\130\\\\207\\\" [x y] 2]\")) 2 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\131\\\\207\\\" [x y] 2]\")) 1 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\135\\\\207\\\" [x y] 2]\")) 7 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\136\\\\207\\\" [x y] 2]\")) 7 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\102\\\\207\\\" [x y] 2]\")) 1 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\104\\\\207\\\" [x y] 2]\")) 1 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\75\\\\207\\\" [x y] 2]\")) 'a 'a)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\232\\\\207\\\" [x y] 2]\")) '(1 2) '(1 2))
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\76\\\\207\\\" [x y] 2]\")) 'b '(a b c))
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\236\\\\207\\\" [x y] 2]\")) 'b '((a . 1) (b . 2)))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\71\\\\207\\\" [x] 1]\")) 'x)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\72\\\\207\\\" [x] 1]\")) '(1 . 2))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\77\\\\207\\\" [x] 1]\")) nil)
             (let ((c (cons 1 2)))
               (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\240\\\\207\\\" [x y] 2]\")) c 9)
               c)
             (let ((c (cons 1 2)))
               (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\241\\\\207\\\" [x y] 2]\")) c 9)
               c)",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK t");
        assert_eq!(results[3], "OK t");
        assert_eq!(results[4], "OK nil");
        assert_eq!(results[5], "OK 7");
        assert_eq!(results[6], "OK 2");
        assert_eq!(results[7], "OK (1 . 2)");
        assert_eq!(results[8], "OK (1 2)");
        assert_eq!(results[9], "OK t");
        assert_eq!(results[10], "OK t");
        assert_eq!(results[11], "OK (b c)");
        assert_eq!(results[12], "OK (b . 2)");
        assert_eq!(results[13], "OK t");
        assert_eq!(results[14], "OK t");
        assert_eq!(results[15], "OK t");
        assert_eq!(results[16], "OK (9 . 2)");
        assert_eq!(results[17], "OK (1 . 9)");
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn calling_sequence_compiled_literal_opcodes_executes() {
        let results = eval_all(
            "(funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\73\\\\207\\\" [x] 1]\")) \"abc\")
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\74\\\\207\\\" [x] 1]\")) '(1 2))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\250\\\\207\\\" [x] 1]\")) 7)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\247\\\\207\\\" [x] 1]\")) 7.5)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\120\\\\207\\\" [x y] 2]\")) \"a\" \"b\")
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\302\\\\117\\\\207\\\" [x y nil] 3]\")) \"abcd\" 1)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\107\\\\207\\\" [x] 1]\")) '(1 2 3))
             (funcall (car (read-from-string \"#[(n x) \\\"\\\\10\\\\11\\\\70\\\\207\\\" [n x] 2]\")) 1 '(a b c))
             (funcall (car (read-from-string \"#[(n x) \\\"\\\\10\\\\11\\\\233\\\\207\\\" [n x] 2]\")) 1 '(a b c))
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\230\\\\207\\\" [x y] 2]\")) \"ab\" \"ab\")
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\231\\\\207\\\" [x y] 2]\")) \"ab\" \"ac\")
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\110\\\\207\\\" [x y] 2]\")) [1 2 3] 1)
             (let ((v [1 2 3]))
               (funcall (car (read-from-string \"#[(x y z) \\\"\\\\10\\\\11\\\\12\\\\111\\\\207\\\" [x y z] 3]\")) v 1 9)
               v)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\245\\\\207\\\" [x y] 2]\")) 7 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\246\\\\207\\\" [x y] 2]\")) 7 2)
             (funcall (car (read-from-string \"#[(x y z) \\\"\\\\10\\\\11\\\\12\\\\303\\\\304\\\\305\\\\257\\\\6\\\\207\\\" [x y z 1 2 3] 6]\")) 'a 'b 'c)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\235\\\\207\\\" [x y] 2]\")) 'b '(a b c))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\237\\\\207\\\" [x] 1]\")) '(1 2 3))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\40\\\\207\\\" [list] 1]\")))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x reverse] 2]\")) '(1 2 3))
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y append] 3]\")) '(1) '(2))
             (funcall (car (read-from-string \"#[(x y z) \\\"\\\\303\\\\10\\\\11\\\\12\\\\43\\\\207\\\" [x y z append] 4]\")) '(1) '(2) '(3))
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y logand] 3]\")) 6 3)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x floor] 2]\")) 3.7)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y vector] 3]\")) 1 2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y assoc] 3]\")) 'b '((a . 1) (b . 2)))
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\244\\\\207\\\" [x y] 2]\")) '(1) '(2))
             (funcall (car (read-from-string \"#[(x y z) \\\"\\\\10\\\\11\\\\244\\\\12\\\\244\\\\207\\\" [x y z] 3]\")) '(1) '(2) '(3))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x string-to-number] 2]\")) \"42.5\")
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x number-to-string] 2]\")) 42)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x char-to-string] 2]\")) 65)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x string-to-char] 2]\")) \"Az\")
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x float] 2]\")) 7)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x truncate] 2]\")) -3.7)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x ceiling] 2]\")) 3.2)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y logior] 3]\")) 6 3)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y logxor] 3]\")) 6 3)
             (funcall (car (read-from-string \"#[(x) \\\"\\\\301\\\\10\\\\41\\\\207\\\" [x lognot] 2]\")) 0)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\302\\\\10\\\\11\\\\42\\\\207\\\" [x y ash] 3]\")) 3 2)
             (funcall (car (read-from-string \"#[(a b c d e f g h) \\\"\\\\10\\\\11\\\\12\\\\13\\\\14\\\\15\\\\16\\\\6\\\\16\\\\7\\\\257\\\\10\\\\207\\\" [a b c d e f g h] 8]\")) 1 2 3 4 5 6 7 8)
             (funcall (car (read-from-string \"#[(a b c d e f g h) \\\"\\\\11\\\\20\\\\10\\\\16\\\\7\\\\26\\\\6\\\\16\\\\6\\\\104\\\\207\\\" [a b c d e f g h] 2]\")) 1 2 3 4 5 6 7 8)
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\1\\\\207\\\" [1 2] 3]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\211\\\\134\\\\207\\\" [7] 2]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\210\\\\207\\\" [1 2] 2]\")))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\133\\\\207\\\" [x] 1]\")) 7)
             (progn
               (set 'x 99)
               (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\112\\\\207\\\" [x] 1]\"))))
             (progn
               (put 'x 'p 42)
               (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\116\\\\207\\\" [x p] 2]\"))))
             (funcall (car (read-from-string \"#[nil \\\"\\\\201\\\\100\\\\0\\\\207\\\" [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64] 1]\")))
             (funcall (car (read-from-string \"#[(a b c d e) \\\"\\\\10\\\\11\\\\12\\\\13\\\\14\\\\260\\\\5\\\\207\\\" [a b c d e] 5]\")) \"a\" \"b\" \"c\" \"d\" \"e\")
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\302\\\\262\\\\1\\\\104\\\\207\\\" [1 2 3] 3]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\302\\\\303\\\\263\\\\2\\\\0\\\\105\\\\207\\\" [1 2 3 4] 4]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\302\\\\266\\\\2\\\\207\\\" [1 2 3] 3]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\301\\\\302\\\\266\\\\202\\\\207\\\" [1 2 3] 3]\")))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\242\\\\10\\\\243\\\\104\\\\207\\\" [x] 2]\")) '(1 2 3))
             (funcall (car (read-from-string \"#[(x) \\\"\\\\10\\\\242\\\\10\\\\243\\\\104\\\\207\\\" [x] 2]\")) 'a)
             (funcall (car (read-from-string \"#[(x y) \\\"\\\\10\\\\11\\\\234\\\\207\\\" [x y] 2]\")) [10 20 30] 1)
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\202\\\\5\\\\0\\\\301\\\\207\\\" [10 20] 1]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\61\\\\6\\\\0\\\\301\\\\60\\\\207\\\" [0 99] 2]\")))
             (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\62\\\\6\\\\0\\\\301\\\\60\\\\207\\\" [0 77] 2]\")))",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK t");
        assert_eq!(results[3], "OK t");
        assert_eq!(results[4], "OK \"ab\"");
        assert_eq!(results[5], "OK \"bcd\"");
        assert_eq!(results[6], "OK 3");
        assert_eq!(results[7], "OK b");
        assert_eq!(results[8], "OK (b c)");
        assert_eq!(results[9], "OK t");
        assert_eq!(results[10], "OK t");
        assert_eq!(results[11], "OK 2");
        assert_eq!(results[12], "OK [1 9 3]");
        assert_eq!(results[13], "OK 3");
        assert_eq!(results[14], "OK 1");
        assert_eq!(results[15], "OK (a b c 1 2 3)");
        assert_eq!(results[16], "OK (b c)");
        assert_eq!(results[17], "OK (3 2 1)");
        assert_eq!(results[18], "OK nil");
        assert_eq!(results[19], "OK (3 2 1)");
        assert_eq!(results[20], "OK (1 2)");
        assert_eq!(results[21], "OK (1 2 3)");
        assert_eq!(results[22], "OK 2");
        assert_eq!(results[23], "OK 3");
        assert_eq!(results[24], "OK [1 2]");
        assert_eq!(results[25], "OK (b . 2)");
        assert_eq!(results[26], "OK (1 2)");
        assert_eq!(results[27], "OK (1 2 3)");
        assert_eq!(results[28], "OK 42.5");
        assert_eq!(results[29], "OK \"42\"");
        assert_eq!(results[30], "OK \"A\"");
        assert_eq!(results[31], "OK 65");
        assert_eq!(results[32], "OK 7.0");
        assert_eq!(results[33], "OK -3");
        assert_eq!(results[34], "OK 4");
        assert_eq!(results[35], "OK 7");
        assert_eq!(results[36], "OK 5");
        assert_eq!(results[37], "OK -1");
        assert_eq!(results[38], "OK 12");
        assert_eq!(results[39], "OK (1 2 3 4 5 6 7 8)");
        assert_eq!(results[40], "OK (2 8)");
        assert_eq!(results[41], "OK 1");
        assert_eq!(results[42], "OK 14");
        assert_eq!(results[43], "OK 1");
        assert_eq!(results[44], "OK -7");
        assert_eq!(results[45], "OK 99");
        assert_eq!(results[46], "OK 42");
        assert_eq!(results[47], "OK 64");
        assert_eq!(results[48], "OK \"abcde\"");
        assert_eq!(results[49], "OK (1 3)");
        assert_eq!(results[50], "OK (1 4 3)");
        assert_eq!(results[51], "OK 1");
        assert_eq!(results[52], "OK 3");
        assert_eq!(results[53], "OK (1 (2 3))");
        assert_eq!(results[54], "OK (nil nil)");
        assert_eq!(results[55], "OK 20");
        assert_eq!(results[56], "OK 10");
        assert_eq!(results[57], "OK 99");
        assert_eq!(results[58], "OK 77");
    }

    #[cfg(feature = "legacy-elc-literal")]
    #[test]
    fn calling_compiled_literal_placeholder_signals_error() {
        let result = eval_one(
            "(progn
               (defalias 'vm-elc-placeholder #[(x) \"\\377\\207\" [x] 1 (#$ . 83)])
               (condition-case err
                   (vm-elc-placeholder 1)
                 (error (car err))))",
        );
        assert_eq!(result, "OK error");
    }

    #[cfg(not(feature = "legacy-elc-literal"))]
    #[test]
    fn compiled_literal_reader_form_is_not_callable_by_default() {
        let result = eval_one(
            "(condition-case err
                 (funcall (car (read-from-string \"#[nil \\\"\\\\300\\\\207\\\" [42] 1]\")))
               (error (car err)))",
        );
        assert_eq!(result, "OK invalid-function");
    }

    #[test]
    fn provide_require() {
        let forms = parse_forms("(provide 'my-feature) (featurep 'my-feature)").expect("parse");
        let mut ev = Evaluator::new();
        let results: Vec<String> = ev
            .eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect();
        assert_eq!(results[0], "OK my-feature");
        assert_eq!(results[1], "OK t");
    }

    #[test]
    fn default_directory_is_bound_to_directory_path() {
        let results = eval_all(
            "(stringp default-directory)
             (file-directory-p default-directory)
             (string-suffix-p \"/\" default-directory)",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK t");
    }

    #[test]
    fn unread_command_events_is_bound_to_nil_at_startup() {
        let results = eval_all(
            "unread-command-events
             (boundp 'unread-command-events)
             (let ((unread-command-events '(97))) unread-command-events)
             unread-command-events",
        );
        assert_eq!(results[0], "OK nil");
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK (97)");
        assert_eq!(results[3], "OK nil");
    }

    #[test]
    fn startup_string_variable_docs_are_seeded_at_startup() {
        let results = eval_all(
            "(stringp (get 'kill-ring 'variable-documentation))
             (integerp (get 'kill-ring 'variable-documentation))
             (stringp (get 'kill-ring-yank-pointer 'variable-documentation))
             (integerp (get 'kill-ring-yank-pointer 'variable-documentation))
             (stringp (get 'after-init-hook 'variable-documentation))
             (integerp (get 'after-init-hook 'variable-documentation))
             (stringp (get 'Buffer-menu-buffer-list 'variable-documentation))
             (integerp (get 'Buffer-menu-buffer-list 'variable-documentation))
             (stringp (get 'Info-default-directory-list 'variable-documentation))
             (integerp (get 'Info-default-directory-list 'variable-documentation))
             (stringp (get 'auto-coding-alist 'variable-documentation))
             (integerp (get 'auto-coding-alist 'variable-documentation))
             (stringp (get 'auto-save--timer 'variable-documentation))
             (integerp (get 'auto-save--timer 'variable-documentation))
             (stringp (get 'backup-directory-alist 'variable-documentation))
             (integerp (get 'backup-directory-alist 'variable-documentation))
             (stringp (get 'before-init-hook 'variable-documentation))
             (integerp (get 'before-init-hook 'variable-documentation))
             (stringp (get 'blink-cursor-mode 'variable-documentation))
             (integerp (get 'blink-cursor-mode 'variable-documentation))
             (stringp (get 'buffer-offer-save 'variable-documentation))
             (integerp (get 'buffer-offer-save 'variable-documentation))
             (stringp (get 'buffer-quit-function 'variable-documentation))
             (integerp (get 'buffer-quit-function 'variable-documentation))
             (stringp (get 'command-line-functions 'variable-documentation))
             (integerp (get 'command-line-functions 'variable-documentation))
             (stringp (get 'comment-start 'variable-documentation))
             (integerp (get 'comment-start 'variable-documentation))
             (stringp (get 'completion-styles 'variable-documentation))
             (integerp (get 'completion-styles 'variable-documentation))
             (stringp (get 'context-menu-mode 'variable-documentation))
             (integerp (get 'context-menu-mode 'variable-documentation))
             (stringp (get 'current-input-method 'variable-documentation))
             (integerp (get 'current-input-method 'variable-documentation))
             (stringp (get 'custom-enabled-themes 'variable-documentation))
             (integerp (get 'custom-enabled-themes 'variable-documentation))
             (stringp (get 'default-input-method 'variable-documentation))
             (integerp (get 'default-input-method 'variable-documentation))
             (stringp (get 'default-korean-keyboard 'variable-documentation))
             (integerp (get 'default-korean-keyboard 'variable-documentation))
             (stringp (get 'delete-selection-mode 'variable-documentation))
             (integerp (get 'delete-selection-mode 'variable-documentation))
             (stringp (get 'display-buffer-alist 'variable-documentation))
             (integerp (get 'display-buffer-alist 'variable-documentation))
             (stringp (get 'eldoc-mode 'variable-documentation))
             (integerp (get 'eldoc-mode 'variable-documentation))
             (stringp (get 'emacs-major-version 'variable-documentation))
             (integerp (get 'emacs-major-version 'variable-documentation))
             (stringp (get 'file-name-shadow-mode 'variable-documentation))
             (integerp (get 'file-name-shadow-mode 'variable-documentation))
             (stringp (get 'fill-prefix 'variable-documentation))
             (integerp (get 'fill-prefix 'variable-documentation))
             (stringp (get 'font-lock-comment-start-skip 'variable-documentation))
             (integerp (get 'font-lock-comment-start-skip 'variable-documentation))
             (stringp (get 'font-lock-mode 'variable-documentation))
             (integerp (get 'font-lock-mode 'variable-documentation))
             (stringp (get 'global-font-lock-mode 'variable-documentation))
             (integerp (get 'global-font-lock-mode 'variable-documentation))
             (stringp (get 'grep-command 'variable-documentation))
             (integerp (get 'grep-command 'variable-documentation))
             (stringp (get 'help-window-select 'variable-documentation))
             (integerp (get 'help-window-select 'variable-documentation))
             (stringp (get 'icomplete-mode 'variable-documentation))
             (integerp (get 'icomplete-mode 'variable-documentation))
             (stringp (get 'indent-line-function 'variable-documentation))
             (integerp (get 'indent-line-function 'variable-documentation))
             (stringp (get 'input-method-history 'variable-documentation))
             (integerp (get 'input-method-history 'variable-documentation))
             (stringp (get 'isearch-mode-hook 'variable-documentation))
             (integerp (get 'isearch-mode-hook 'variable-documentation))
             (stringp (get 'jit-lock-mode 'variable-documentation))
             (integerp (get 'jit-lock-mode 'variable-documentation))
             (stringp (get 'jka-compr-load-suffixes 'variable-documentation))
             (integerp (get 'jka-compr-load-suffixes 'variable-documentation))
             (stringp (get 'keyboard-coding-system 'variable-documentation))
             (integerp (get 'keyboard-coding-system 'variable-documentation))
             (stringp (get 'kill-ring-max 'variable-documentation))
             (integerp (get 'kill-ring-max 'variable-documentation))
             (stringp (get 'line-number-mode 'variable-documentation))
             (integerp (get 'line-number-mode 'variable-documentation))
             (stringp (get 'list-buffers-directory 'variable-documentation))
             (integerp (get 'list-buffers-directory 'variable-documentation))
             (stringp (get 'lock-file-mode 'variable-documentation))
             (integerp (get 'lock-file-mode 'variable-documentation))
             (stringp (get 'mail-user-agent 'variable-documentation))
             (integerp (get 'mail-user-agent 'variable-documentation))
             (stringp (get 'menu-bar-mode-hook 'variable-documentation))
             (integerp (get 'menu-bar-mode-hook 'variable-documentation))
             (stringp (get 'minibuffer-local-completion-map 'variable-documentation))
             (integerp (get 'minibuffer-local-completion-map 'variable-documentation))
             (stringp (get 'mouse-wheel-mode 'variable-documentation))
             (integerp (get 'mouse-wheel-mode 'variable-documentation))
             (stringp (get 'next-error-function 'variable-documentation))
             (integerp (get 'next-error-function 'variable-documentation))
             (stringp (get 'package-user-dir 'variable-documentation))
             (integerp (get 'package-user-dir 'variable-documentation))
             (stringp (get 'prettify-symbols-mode 'variable-documentation))
             (integerp (get 'prettify-symbols-mode 'variable-documentation))
             (stringp (get 'previous-transient-input-method 'variable-documentation))
             (integerp (get 'previous-transient-input-method 'variable-documentation))
             (stringp (get 'process-file-side-effects 'variable-documentation))
             (integerp (get 'process-file-side-effects 'variable-documentation))
             (stringp (get 'process-menu-mode-map 'variable-documentation))
             (integerp (get 'process-menu-mode-map 'variable-documentation))
             (stringp (get 'prog-mode-map 'variable-documentation))
             (integerp (get 'prog-mode-map 'variable-documentation))
             (stringp (get 'query-about-changed-file 'variable-documentation))
             (integerp (get 'query-about-changed-file 'variable-documentation))
             (stringp (get 'read-extended-command-predicate 'variable-documentation))
             (integerp (get 'read-extended-command-predicate 'variable-documentation))
             (stringp (get 'regexp-search-ring-max 'variable-documentation))
             (integerp (get 'regexp-search-ring-max 'variable-documentation))
             (stringp (get 'safe-local-variable-values 'variable-documentation))
             (integerp (get 'safe-local-variable-values 'variable-documentation))
             (stringp (get 'selection-coding-system 'variable-documentation))
             (integerp (get 'selection-coding-system 'variable-documentation))
             (stringp (get 'show-paren-mode 'variable-documentation))
             (integerp (get 'show-paren-mode 'variable-documentation))
             (stringp (get 'tab-bar-format 'variable-documentation))
             (integerp (get 'tab-bar-format 'variable-documentation))
             (stringp (get 'tool-bar-map 'variable-documentation))
             (integerp (get 'tool-bar-map 'variable-documentation))
             (stringp (get 'transient-mark-mode-hook 'variable-documentation))
             (integerp (get 'transient-mark-mode-hook 'variable-documentation))
             (stringp (get 'user-emacs-directory 'variable-documentation))
             (integerp (get 'user-emacs-directory 'variable-documentation))
             (stringp (get 'window-size-fixed 'variable-documentation))
             (integerp (get 'window-size-fixed 'variable-documentation))
             (stringp (get 'yank-transform-functions 'variable-documentation))
             (integerp (get 'yank-transform-functions 'variable-documentation))",
        );
        assert_eq!(results[0], "OK t");
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK t");
        assert_eq!(results[3], "OK nil");
        assert_eq!(results[4], "OK t");
        assert_eq!(results[5], "OK nil");
        assert_eq!(results[6], "OK t");
        assert_eq!(results[7], "OK nil");
        assert_eq!(results[8], "OK t");
        assert_eq!(results[9], "OK nil");
        assert_eq!(results[10], "OK t");
        assert_eq!(results[11], "OK nil");
        assert_eq!(results[12], "OK t");
        assert_eq!(results[13], "OK nil");
        assert_eq!(results[14], "OK t");
        assert_eq!(results[15], "OK nil");
        assert_eq!(results[16], "OK t");
        assert_eq!(results[17], "OK nil");
        assert_eq!(results[18], "OK t");
        assert_eq!(results[19], "OK nil");
        assert_eq!(results[20], "OK t");
        assert_eq!(results[21], "OK nil");
        assert_eq!(results[22], "OK t");
        assert_eq!(results[23], "OK nil");
        assert_eq!(results[24], "OK t");
        assert_eq!(results[25], "OK nil");
        assert_eq!(results[26], "OK t");
        assert_eq!(results[27], "OK nil");
        assert_eq!(results[28], "OK t");
        assert_eq!(results[29], "OK nil");
        assert_eq!(results[30], "OK t");
        assert_eq!(results[31], "OK nil");
        assert_eq!(results[32], "OK t");
        assert_eq!(results[33], "OK nil");
        assert_eq!(results[34], "OK t");
        assert_eq!(results[35], "OK nil");
        assert_eq!(results[36], "OK t");
        assert_eq!(results[37], "OK nil");
        assert_eq!(results[38], "OK t");
        assert_eq!(results[39], "OK nil");
        assert_eq!(results[40], "OK t");
        assert_eq!(results[41], "OK nil");
        assert_eq!(results[42], "OK t");
        assert_eq!(results[43], "OK nil");
        assert_eq!(results[44], "OK t");
        assert_eq!(results[45], "OK nil");
        assert_eq!(results[46], "OK t");
        assert_eq!(results[47], "OK nil");
        assert_eq!(results[48], "OK t");
        assert_eq!(results[49], "OK nil");
        assert_eq!(results[50], "OK t");
        assert_eq!(results[51], "OK nil");
        assert_eq!(results[52], "OK t");
        assert_eq!(results[53], "OK nil");
        assert_eq!(results[54], "OK t");
        assert_eq!(results[55], "OK nil");
        assert_eq!(results[56], "OK t");
        assert_eq!(results[57], "OK nil");
        assert_eq!(results[58], "OK t");
        assert_eq!(results[59], "OK nil");
        assert_eq!(results[60], "OK t");
        assert_eq!(results[61], "OK nil");
        assert_eq!(results[62], "OK t");
        assert_eq!(results[63], "OK nil");
        assert_eq!(results[64], "OK t");
        assert_eq!(results[65], "OK nil");
        assert_eq!(results[66], "OK t");
        assert_eq!(results[67], "OK nil");
        assert_eq!(results[68], "OK t");
        assert_eq!(results[69], "OK nil");
        assert_eq!(results[70], "OK t");
        assert_eq!(results[71], "OK nil");
        assert_eq!(results[72], "OK t");
        assert_eq!(results[73], "OK nil");
        assert_eq!(results[74], "OK t");
        assert_eq!(results[75], "OK nil");
        assert_eq!(results[76], "OK t");
        assert_eq!(results[77], "OK nil");
        assert_eq!(results[78], "OK t");
        assert_eq!(results[79], "OK nil");
        assert_eq!(results[80], "OK t");
        assert_eq!(results[81], "OK nil");
        assert_eq!(results[82], "OK t");
        assert_eq!(results[83], "OK nil");
        assert_eq!(results[84], "OK t");
        assert_eq!(results[85], "OK nil");
        assert_eq!(results[86], "OK t");
        assert_eq!(results[87], "OK nil");
        assert_eq!(results[88], "OK t");
        assert_eq!(results[89], "OK nil");
        assert_eq!(results[90], "OK t");
        assert_eq!(results[91], "OK nil");
        assert_eq!(results[92], "OK t");
        assert_eq!(results[93], "OK nil");
        assert_eq!(results[94], "OK t");
        assert_eq!(results[95], "OK nil");
        assert_eq!(results[96], "OK t");
        assert_eq!(results[97], "OK nil");
        assert_eq!(results[98], "OK t");
        assert_eq!(results[99], "OK nil");
        assert_eq!(results[100], "OK t");
        assert_eq!(results[101], "OK nil");
        assert_eq!(results[102], "OK t");
        assert_eq!(results[103], "OK nil");
        assert_eq!(results[104], "OK t");
        assert_eq!(results[105], "OK nil");
        assert_eq!(results[106], "OK t");
        assert_eq!(results[107], "OK nil");
        assert_eq!(results[108], "OK t");
        assert_eq!(results[109], "OK nil");
        assert_eq!(results[110], "OK t");
        assert_eq!(results[111], "OK nil");
        assert_eq!(results[112], "OK t");
        assert_eq!(results[113], "OK nil");
        assert_eq!(results[114], "OK t");
        assert_eq!(results[115], "OK nil");
        assert_eq!(results[116], "OK t");
        assert_eq!(results[117], "OK nil");
        assert_eq!(results[118], "OK t");
        assert_eq!(results[119], "OK nil");
        assert_eq!(results[120], "OK t");
        assert_eq!(results[121], "OK nil");
        assert_eq!(results[122], "OK t");
        assert_eq!(results[123], "OK nil");
        assert_eq!(results[124], "OK t");
        assert_eq!(results[125], "OK nil");
        assert_eq!(results[126], "OK t");
        assert_eq!(results[127], "OK nil");
        assert_eq!(results[128], "OK t");
        assert_eq!(results[129], "OK nil");
    }

    #[test]
    fn startup_variable_documentation_property_counts_match_oracle_snapshot() {
        let results = eval_all(
            "(list
              (let ((n 0))
                (mapatoms
                 (lambda (s)
                   (let ((d (get s 'variable-documentation)))
                     (when (integerp d) (setq n (1+ n))))))
                n)
              (let ((n 0))
                (mapatoms
                 (lambda (s)
                   (let ((d (get s 'variable-documentation)))
                     (when (stringp d) (setq n (1+ n))))))
                n))",
        );
        assert_eq!(results[0], "OK (761 1904)");
    }

    #[test]
    fn startup_variable_documentation_runtime_resolution_counts_match_oracle_snapshot() {
        let results = eval_all(
            "(list
              (let ((n 0))
                (mapatoms
                 (lambda (s)
                   (let ((d (get s 'variable-documentation)))
                     (when (and (integerp d)
                                (stringp (documentation-property s 'variable-documentation t)))
                       (setq n (1+ n))))))
                n)
              (let ((n 0))
                (mapatoms
                 (lambda (s)
                   (let ((d (get s 'variable-documentation)))
                     (when (and (stringp d)
                                (stringp (documentation-property s 'variable-documentation t)))
                       (setq n (1+ n))))))
                n))",
        );
        assert_eq!(results[0], "OK (761 1904)");
    }

    #[test]
    fn features_variable_controls_featurep_and_require() {
        let results = eval_all(
            "(setq features '(vm-existing))
             (featurep 'vm-existing)
             (require 'vm-existing)",
        );
        assert_eq!(results[0], "OK (vm-existing)");
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK vm-existing");
    }

    #[test]
    fn provide_preserves_features_variable_entries() {
        let results = eval_all(
            "(setq features '(vm-existing))
             (provide 'vm-new)
             features",
        );
        assert_eq!(results[0], "OK (vm-existing)");
        assert_eq!(results[1], "OK vm-new");
        assert_eq!(results[2], "OK (vm-new vm-existing)");
    }

    #[test]
    fn require_recursive_cycle_signals_error() {
        use std::fs;
        use std::time::{SystemTime, UNIX_EPOCH};

        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("neovm-require-recursive-{unique}"));
        fs::create_dir_all(&dir).expect("create fixture dir");
        fs::write(
            dir.join("vm-rec-a.el"),
            "(require 'vm-rec-b)\n(provide 'vm-rec-a)\n",
        )
        .expect("write vm-rec-a");
        fs::write(
            dir.join("vm-rec-b.el"),
            "(require 'vm-rec-a)\n(provide 'vm-rec-b)\n",
        )
        .expect("write vm-rec-b");

        let escaped = dir
            .to_string_lossy()
            .replace('\\', "\\\\")
            .replace('"', "\\\"");
        let script = format!(
            "(progn (setq load-path (cons \"{}\" load-path)) 'ok)\n\
             (condition-case err (require 'vm-rec-a) (error (car err)))\n\
             (featurep 'vm-rec-a)\n\
             (featurep 'vm-rec-b)",
            escaped
        );
        let results = eval_all(&script);
        assert_eq!(results[1], "OK error");
        assert_eq!(results[2], "OK nil");
        assert_eq!(results[3], "OK nil");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn dotimes_loop() {
        let result = eval_one("(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)");
        assert_eq!(result, "OK 10"); // 0+1+2+3+4 = 10
    }

    #[test]
    fn dolist_loop() {
        let result = eval_one(
            "(let ((result nil)) (dolist (x '(a b c)) (setq result (cons x result))) result)",
        );
        assert_eq!(result, "OK (c b a)");
    }

    #[test]
    fn ignore_errors_catches_signal() {
        let result = eval_one("(ignore-errors (/ 1 0) 42)");
        assert_eq!(result, "OK nil"); // error caught, returns nil
    }

    #[test]
    fn math_functions() {
        assert_eq!(eval_one("(expt 2 10)"), "OK 1024");
        assert_eq!(eval_one("(sqrt 4.0)"), "OK 2.0");
    }

    #[test]
    fn hook_system() {
        let results = eval_all(
            "(defvar my-hook nil)
             (defun hook-fn () 42)
             (add-hook 'my-hook 'hook-fn)
             (run-hooks 'my-hook)",
        );
        assert_eq!(results[3], "OK nil"); // run-hooks returns nil
    }

    #[test]
    fn hook_system_runtime_value_shapes() {
        let results = eval_all(
            "(setq hook-count 0)
             (defun hook-inc () (setq hook-count (1+ hook-count)))
             (setq hook-probe-hook 'hook-inc)
             (condition-case err (run-hooks 'hook-probe-hook) (error err))
             hook-count
             (setq hook-count 0)
             (setq hook-probe-hook (cons 'hook-inc 1))
             (condition-case err (run-hooks 'hook-probe-hook) (error err))
             hook-count
             (setq hook-probe-hook t)
             (condition-case err (run-hooks 'hook-probe-hook) (error err))
             (setq hook-probe-hook 42)
             (condition-case err (run-hooks 'hook-probe-hook) (error err))
             (setq hook-probe-hook '(t hook-inc))
             (setq hook-count 0)
             (condition-case err (run-hooks 'hook-probe-hook) (error err))
             hook-count",
        );
        assert_eq!(results[3], "OK nil");
        assert_eq!(results[4], "OK 1");
        assert_eq!(results[7], "OK nil");
        assert_eq!(results[8], "OK 1");
        assert_eq!(results[10], "OK (void-function t)");
        assert_eq!(results[12], "OK (invalid-function 42)");
        assert_eq!(results[15], "OK nil");
        assert_eq!(results[16], "OK 2");
    }

    #[test]
    fn run_hook_with_args_runtime_value_shapes() {
        let results = eval_all(
            "(setq hook-log nil)
             (defun hook-log-fn (&rest args) (setq hook-log (cons args hook-log)))
             (setq hook-probe-hook 'hook-log-fn)
             (condition-case err (run-hook-with-args 'hook-probe-hook 1 2) (error err))
             hook-log
             (setq hook-log nil)
             (setq hook-probe-hook (cons 'hook-log-fn 1))
             (condition-case err (run-hook-with-args 'hook-probe-hook 3) (error err))
             hook-log
             (setq hook-probe-hook t)
             (condition-case err (run-hook-with-args 'hook-probe-hook 4) (error err))
             (setq hook-probe-hook 42)
             (condition-case err (run-hook-with-args 'hook-probe-hook 5) (error err))
             (setq hook-log nil)
             (setq hook-probe-hook '(t hook-log-fn))
             (condition-case err (run-hook-with-args 'hook-probe-hook 6) (error err))
             hook-log",
        );
        assert_eq!(results[3], "OK nil");
        assert_eq!(results[4], "OK ((1 2))");
        assert_eq!(results[7], "OK nil");
        assert_eq!(results[8], "OK ((3))");
        assert_eq!(results[10], "OK (void-function t)");
        assert_eq!(results[12], "OK (invalid-function 42)");
        assert_eq!(results[15], "OK nil");
        assert_eq!(results[16], "OK ((6) (6))");
    }

    #[test]
    fn symbol_operations() {
        let results = eval_all(
            "(defvar x 42)
             (boundp 'x)
             (symbol-value 'x)
             (put 'x 'doc \"A variable\")
             (get 'x 'doc)",
        );
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK 42");
        assert_eq!(results[4], r#"OK "A variable""#);
    }

    // -- Buffer operations -------------------------------------------------

    #[test]
    fn buffer_create_and_switch() {
        let results = eval_all(
            "(get-buffer-create \"test-buf\")
             (set-buffer \"test-buf\")
             (buffer-name)
             (bufferp (current-buffer))",
        );
        assert!(results[0].starts_with("OK #<buffer"));
        assert!(results[1].starts_with("OK #<buffer"));
        assert_eq!(results[2], r#"OK "test-buf""#);
        assert_eq!(results[3], "OK t");
    }

    #[test]
    fn buffer_insert_and_point() {
        let results = eval_all(
            "(get-buffer-create \"ed\")
             (set-buffer \"ed\")
             (insert \"hello\")
             (point)
             (goto-char 1)
             (point)
             (buffer-string)
             (point-min)
             (point-max)",
        );
        assert_eq!(results[3], "OK 6"); // after inserting "hello", point is 6 (1-based)
        assert_eq!(results[5], "OK 1"); // after goto-char 1
        assert_eq!(results[6], r#"OK "hello""#);
        assert_eq!(results[7], "OK 1"); // point-min
        assert_eq!(results[8], "OK 6"); // point-max
    }

    #[test]
    fn buffer_delete_region() {
        let results = eval_all(
            "(get-buffer-create \"del\")
             (set-buffer \"del\")
             (insert \"abcdef\")
             (delete-region 2 5)
             (buffer-string)",
        );
        assert_eq!(results[4], r#"OK "aef""#);
    }

    #[test]
    fn buffer_erase() {
        let results = eval_all(
            "(get-buffer-create \"era\")
             (set-buffer \"era\")
             (insert \"stuff\")
             (erase-buffer)
             (buffer-string)
             (buffer-size)",
        );
        assert_eq!(results[4], r#"OK """#);
        assert_eq!(results[5], "OK 0");
    }

    #[test]
    fn buffer_narrowing() {
        let results = eval_all(
            "(get-buffer-create \"nar\")
             (set-buffer \"nar\")
             (insert \"hello world\")
             (narrow-to-region 7 12)
             (buffer-string)
             (widen)
             (buffer-string)",
        );
        assert_eq!(results[4], r#"OK "world""#);
        assert_eq!(results[6], r#"OK "hello world""#);
    }

    #[test]
    fn buffer_modified_p() {
        let results = eval_all(
            "(get-buffer-create \"mod\")
             (set-buffer \"mod\")
             (buffer-modified-p)
             (insert \"x\")
             (buffer-modified-p)
             (set-buffer-modified-p nil)
             (buffer-modified-p)",
        );
        assert_eq!(results[2], "OK nil");
        assert_eq!(results[4], "OK t");
        assert_eq!(results[6], "OK nil");
    }

    #[test]
    fn buffer_mark() {
        let results = eval_all(
            "(get-buffer-create \"mk\")
             (set-buffer \"mk\")
             (insert \"hello\")
             (set-mark 3)
             (mark)",
        );
        assert_eq!(results[4], "OK 3");
    }

    #[test]
    fn buffer_with_current_buffer() {
        let results = eval_all(
            "(get-buffer-create \"a\")
             (get-buffer-create \"b\")
             (set-buffer \"a\")
             (insert \"in-a\")
             (with-current-buffer \"b\"
               (insert \"in-b\")
               (buffer-string))
             (buffer-name)
             (buffer-string)",
        );
        // with-current-buffer should switch to b, insert, get string, then restore a
        assert_eq!(results[4], r#"OK "in-b""#);
        assert_eq!(results[5], r#"OK "a""#); // current buffer restored
        assert_eq!(results[6], r#"OK "in-a""#); // a's content unchanged
    }

    #[test]
    fn buffer_save_excursion() {
        let results = eval_all(
            "(get-buffer-create \"se\")
             (set-buffer \"se\")
             (insert \"abcdef\")
             (goto-char 3)
             (save-excursion
               (goto-char 1)
               (insert \"X\"))
             (point)",
        );
        // save-excursion restores point to 3
        assert_eq!(results[5], "OK 3");
    }

    #[test]
    fn save_match_data_restores_after_success_and_error() {
        let results = eval_all(
            "(set-match-data '(1 2))
             (save-match-data (set-match-data '(3 4)) (match-data))
             (match-data)
             (condition-case err
                 (save-match-data
                   (set-match-data '(5 6))
                   (error \"boom\"))
               (error (car err)))
             (match-data)",
        );
        assert_eq!(results[1], "OK (3 4)");
        assert_eq!(results[2], "OK (1 2)");
        assert_eq!(results[3], "OK error");
        assert_eq!(results[4], "OK (1 2)");
    }

    #[test]
    fn save_mark_and_excursion_restores_mark_and_mark_active() {
        let results = eval_all(
            "(save-current-buffer
               (let ((b (get-buffer-create \"smx-eval\")))
                 (set-buffer b)
                 (erase-buffer)
                 (insert \"abcdef\")
                 (goto-char 2)
                 (set-mark 5)
                 (setq mark-active nil)
                 (let ((before (list (point) (mark) mark-active)))
                   (save-mark-and-excursion
                     (goto-char 4)
                     (set-mark 3)
                     (setq mark-active t))
                   (list before (point) (mark) mark-active))))",
        );
        assert_eq!(results[0], "OK ((2 5 nil) 2 5 nil)");
    }

    #[test]
    fn buffer_char_after_before() {
        let results = eval_all(
            "(get-buffer-create \"cb\")
             (set-buffer \"cb\")
             (insert \"abc\")
             (goto-char 2)
             (char-after)
             (char-before)",
        );
        assert_eq!(results[4], "OK 98"); // ?b = 98
        assert_eq!(results[5], "OK 97"); // ?a = 97
    }

    #[test]
    fn buffer_list_and_kill() {
        let results = eval_all(
            "(get-buffer-create \"kill-me\")
             (kill-buffer \"kill-me\")
             (get-buffer \"kill-me\")",
        );
        assert_eq!(results[1], "OK t");
        assert_eq!(results[2], "OK nil");
    }

    #[test]
    fn buffer_generate_new_buffer() {
        let results = eval_all(
            "(buffer-name (generate-new-buffer \"test\"))
             (buffer-name (generate-new-buffer \"test\"))",
        );
        assert_eq!(results[0], r#"OK "test""#);
        assert_eq!(results[1], r#"OK "test<2>""#);
    }

    #[test]
    fn fillarray_string_writeback_updates_symbol_binding() {
        let result = eval_one("(let ((s (copy-sequence \"abc\"))) (fillarray s ?x) s)");
        assert_eq!(result, r#"OK "xxx""#);
    }

    #[test]
    fn fillarray_alias_string_writeback_updates_symbol_binding() {
        let result = eval_one(
            "(progn
                (defalias 'vm-fillarray-alias 'fillarray)
                (let ((s (copy-sequence \"abc\")))
                  (vm-fillarray-alias s ?y)
                  s))",
        );
        assert_eq!(result, r#"OK "yyy""#);
    }

    #[test]
    fn fillarray_string_writeback_updates_alias_from_prog1_expression() {
        let result = eval_one("(let ((s (copy-sequence \"abc\"))) (fillarray (prog1 s) ?x) s)");
        assert_eq!(result, r#"OK "xxx""#);
    }

    #[test]
    fn fillarray_string_writeback_updates_alias_from_list_car_expression() {
        let result =
            eval_one("(let ((s (copy-sequence \"abc\"))) (fillarray (car (list s)) ?y) s)");
        assert_eq!(result, r#"OK "yyy""#);
    }

    #[test]
    fn fillarray_string_writeback_updates_vector_alias_element() {
        let result =
            eval_one("(let* ((s (copy-sequence \"abc\")) (v (vector s))) (fillarray s ?x) (aref v 0))");
        assert_eq!(result, r#"OK "xxx""#);
    }

    #[test]
    fn fillarray_string_writeback_updates_cons_alias_element() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (cell (cons s nil))) (fillarray s ?y) (car cell))",
        );
        assert_eq!(result, r#"OK "yyy""#);
    }

    #[test]
    fn fillarray_string_writeback_preserves_eq_hash_key_lookup() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (ht (make-hash-table :test 'eq)))
               (puthash s 'v ht)
               (fillarray s ?x)
               (gethash s ht))",
        );
        assert_eq!(result, "OK v");
    }

    #[test]
    fn fillarray_string_writeback_preserves_eql_hash_key_lookup() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (ht (make-hash-table :test 'eql)))
               (puthash s 'v ht)
               (fillarray s ?y)
               (gethash s ht))",
        );
        assert_eq!(result, "OK v");
    }

    #[test]
    fn fillarray_string_writeback_equal_hash_key_lookup_stays_nil() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (ht (make-hash-table :test 'equal)))
               (puthash s 'v ht)
               (fillarray s ?z)
               (gethash s ht))",
        );
        assert_eq!(result, "OK nil");
    }

    #[test]
    fn aset_string_writeback_updates_symbol_binding() {
        let result = eval_one("(let ((s (copy-sequence \"abc\"))) (aset s 1 ?x) s)");
        assert_eq!(result, r#"OK "axc""#);
    }

    #[test]
    fn aset_alias_string_writeback_updates_symbol_binding() {
        let result = eval_one(
            "(progn
                (defalias 'vm-aset-alias 'aset)
                (let ((s (copy-sequence \"abc\")))
                  (vm-aset-alias s 1 ?y)
                  s))",
        );
        assert_eq!(result, r#"OK "ayc""#);
    }

    #[test]
    fn aset_string_writeback_updates_alias_from_prog1_expression() {
        let result = eval_one("(let ((s (copy-sequence \"abc\"))) (aset (prog1 s) 1 ?x) s)");
        assert_eq!(result, r#"OK "axc""#);
    }

    #[test]
    fn aset_string_writeback_updates_alias_from_list_car_expression() {
        let result = eval_one("(let ((s (copy-sequence \"abc\"))) (aset (car (list s)) 1 ?y) s)");
        assert_eq!(result, r#"OK "ayc""#);
    }

    #[test]
    fn aset_string_writeback_updates_vector_alias_element() {
        let result =
            eval_one("(let* ((s (copy-sequence \"abc\")) (v (vector s))) (aset s 1 ?x) (aref v 0))");
        assert_eq!(result, r#"OK "axc""#);
    }

    #[test]
    fn aset_string_writeback_updates_cons_alias_element() {
        let result =
            eval_one("(let* ((s (copy-sequence \"abc\")) (cell (cons s nil))) (aset s 1 ?y) (car cell))");
        assert_eq!(result, r#"OK "ayc""#);
    }

    #[test]
    fn aset_string_writeback_preserves_eq_hash_key_lookup() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (ht (make-hash-table :test 'eq)))
               (puthash s 'v ht)
               (aset s 1 ?x)
               (gethash s ht))",
        );
        assert_eq!(result, "OK v");
    }

    #[test]
    fn aset_string_writeback_preserves_eql_hash_key_lookup() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (ht (make-hash-table :test 'eql)))
               (puthash s 'v ht)
               (aset s 1 ?y)
               (gethash s ht))",
        );
        assert_eq!(result, "OK v");
    }

    #[test]
    fn aset_string_writeback_equal_hash_key_lookup_stays_nil() {
        let result = eval_one(
            "(let* ((s (copy-sequence \"abc\")) (ht (make-hash-table :test 'equal)))
               (puthash s 'v ht)
               (aset s 1 ?z)
               (gethash s ht))",
        );
        assert_eq!(result, "OK nil");
    }
}
