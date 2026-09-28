//! The Elisp every Vertico scenario boots.
//!
//! Fixtures are fixed lists written here in full: a scenario may not read the
//! host's clock, locale, user name, CPU count or installed tools, because a
//! frozen grid that encodes one of those facts drifts on the next machine.

/// The suite's original fixture: three real buffers, a five-candidate window
/// and cycling, so the first scenario's grids stay the ones it was blessed
/// with.
pub(super) const VERTICO_TUI_PRELUDE: &str = r#"
(require 'vertico)
(setq vertico-count 5
      vertico-cycle t)
(vertico-mode 1)
(dolist (fixture '(("project-alpha" . "ALPHA BUFFER\n")
                   ("project-beta" . "BETA BUFFER\n")
                   ("project-notes" . "NOTES BUFFER\n")))
  (with-current-buffer (get-buffer-create (car fixture))
    (erase-buffer)
    (insert (cdr fixture))))
"#;

/// A fixture set: one buffer per name, each holding a line naming itself, so a
/// scenario can see which buffer was selected from the screen alone.
///
/// The names are written out in the prelude rather than derived from the clock,
/// the locale, the file system or anything else the host owns: the candidate
/// order every scenario freezes is a function of these names and the package's
/// sort function, and of nothing else.
pub(super) fn fixture_buffers(names: &[String]) -> String {
    let list = names
        .iter()
        .map(|name| format!("{name:?}"))
        .collect::<Vec<_>>()
        .join(" ");
    format!(
        "(dolist (name (list {list}))\n  \
         (with-current-buffer (get-buffer-create name)\n    \
         (erase-buffer)\n    \
         (insert (concat name \"\\n\"))))\n"
    )
}

/// Uniformly named fixture buffers: `<prefix>-01` up to `<prefix>-<count>`.
///
/// Equal-length names make the order the default sort function produces the
/// names' own order, so a scenario can name the candidate it expects by
/// position.
pub(super) fn numbered_names(prefix: &str, count: usize) -> Vec<String> {
    (1..=count)
        .map(|number| format!("{prefix}-{number:02}"))
        .collect()
}

/// Vertico as the package ships it: `vertico-mode` on, `vertico-count` and
/// `vertico-cycle` left at their defaults, over `fixture`.
///
/// The defaults are the point of the scenarios that use this: the candidate
/// window's height is `vertico-count`'s default of ten, not a configured value.
pub(super) fn default_vertico(fixture: &str) -> String {
    format!("(require 'vertico)\n(vertico-mode 1)\n{fixture}")
}
