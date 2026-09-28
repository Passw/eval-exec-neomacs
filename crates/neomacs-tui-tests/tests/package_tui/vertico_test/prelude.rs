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
