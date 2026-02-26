//! Buffer-related bootstrap variables.
use crate::elisp::value::Value;

pub fn register_bootstrap_vars(obarray: &mut crate::elisp::symbol::Obarray) {
    obarray.set_symbol_value("kill-buffer-query-functions", Value::Nil);
    obarray.set_symbol_value("kill-buffer-hook", Value::Nil);
    obarray.set_symbol_value("buffer-list-update-hook", Value::Nil);
    obarray.set_symbol_value("change-major-mode-hook", Value::Nil);
    obarray.set_symbol_value("after-change-major-mode-hook", Value::Nil);
    obarray.set_symbol_value("first-change-hook", Value::Nil);
    obarray.set_symbol_value("before-change-functions", Value::Nil);
    obarray.set_symbol_value("after-change-functions", Value::Nil);
    obarray.set_symbol_value("buffer-access-fontify-functions", Value::Nil);
    obarray.set_symbol_value("buffer-access-fontified-property", Value::Nil);
    obarray.set_symbol_value("buffer-file-coding-system", Value::Nil);
    obarray.set_symbol_value("buffer-file-format", Value::Nil);
    obarray.set_symbol_value("buffer-saved-size", Value::Int(0));
    obarray.set_symbol_value(
        "buffer-auto-save-file-format",
        Value::list(vec![Value::symbol("t")]),
    );
    obarray.set_symbol_value("buffer-stale-function", Value::Nil);
    obarray.set_symbol_value("buffer-undo-list", Value::Nil);
    obarray.set_symbol_value("buffer-display-table", Value::Nil);
    obarray.set_symbol_value("enable-multibyte-characters", Value::True);
    obarray.set_symbol_value("default-enable-multibyte-characters", Value::True);
    obarray.set_symbol_value("find-file-hook", Value::Nil);
    obarray.set_symbol_value("find-file-not-found-functions", Value::Nil);
    obarray.set_symbol_value("major-mode", Value::symbol("fundamental-mode"));
    obarray.set_symbol_value("mode-name", Value::string("Fundamental"));
    obarray.set_symbol_value("fill-column", Value::Int(70));
    obarray.set_symbol_value("left-margin", Value::Int(0));
    // tab-width is set by init_indent_vars() with special=true
    obarray.set_symbol_value("ctl-arrow", Value::True);
    obarray.set_symbol_value("truncate-lines", Value::Nil);
    obarray.set_symbol_value("word-wrap", Value::Nil);
    obarray.set_symbol_value("word-wrap-by-category", Value::Nil);
    obarray.set_symbol_value("selective-display", Value::Nil);
    obarray.set_symbol_value("selective-display-ellipses", Value::True);
    obarray.set_symbol_value("indicate-empty-lines", Value::Nil);
    obarray.set_symbol_value("indicate-buffer-boundaries", Value::Nil);
    obarray.set_symbol_value("fringe-indicator-alist", Value::Nil);
    obarray.set_symbol_value("fringe-cursor-alist", Value::Nil);
    obarray.set_symbol_value("scroll-up-aggressively", Value::Nil);
    obarray.set_symbol_value("scroll-down-aggressively", Value::Nil);
    obarray.set_symbol_value("auto-fill-function", Value::Nil);
    obarray.set_symbol_value("buffer-display-count", Value::Int(0));
    obarray.set_symbol_value("buffer-display-time", Value::Nil);
}
