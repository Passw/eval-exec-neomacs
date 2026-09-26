use std::time::Duration;

use expect_test::expect_file;
use neomacs_tui_tests::RawTerminalSnapshot;
use neomacs_tui_tests::git_fixture::{GitCommitSpec, GitFixtureSpec};

use super::{CachedMelpaOracle, MAGIT_MELPA_PIN};

use super::scenario::{DisplayCheckpoint, PackageTuiScenario, PairTimeout, ReadinessCheckpoint};

/// The repository the log screen is rendered from.
///
/// The harness creates it inside each peer's sandbox before that peer boots,
/// and the sandbox's drop removes it.  Branch, contents, identity and dates are
/// all pinned here: the expected grid pins the resulting commit hashes, so
/// nothing may come from the host's git configuration, clock, or timezone.
const MAGIT_LOG_FIXTURE: GitFixtureSpec = GitFixtureSpec {
    directory: "repo",
    branch: "main",
    file: "tracked.txt",
    commits: &[
        GitCommitSpec {
            subject: "short",
            timestamp: "2001-02-03T04:05:06+0000",
            contents: "one\n",
        },
        GitCommitSpec {
            subject: "a deliberately much longer subject",
            timestamp: "2002-03-04T05:06:07+0000",
            contents: "one\ntwo\n",
        },
        GitCommitSpec {
            subject: "medium subject",
            timestamp: "2003-04-05T06:07:08+0000",
            contents: "one\ntwo\nthree\n",
        },
    ],
};

const MAGIT_LOG_TUI_PRELUDE: &str = r#"
(require 'magit)
(defun neomacs-magit-tui-display-same-window (buffer)
  (display-buffer-same-window buffer nil))
(defun neomacs-magit-tui-stabilize-window ()
  (setq-local header-line-format nil
              mode-line-format nil))
(add-hook 'magit-log-mode-hook #'neomacs-magit-tui-stabilize-window)
(setq inhibit-message t
      byte-compile-verbose nil
      magit-display-buffer-function #'neomacs-magit-tui-display-same-window
      magit-log-margin
      '(t "%Y-%m-%d %a %H:%M" magit-log-margin-width t 18))
(let ((repo (getenv "NEOMACS_TUI_GIT_FIXTURE")))
  (unless (and repo (file-directory-p repo))
    (error "NEOMACS_TUI_GIT_FIXTURE is not a directory: %S" repo))
  (setq default-directory (file-name-as-directory repo))
  (find-file (expand-file-name "tracked.txt" default-directory))
  (magit-log-buffer-file)
  (when-let* ((warnings (get-buffer "*Warnings*")))
    (kill-buffer warnings))
  (delete-other-windows))
"#;

#[test]
fn magit_log_buffer_file_margin_columns_match_gnu_full_screen() {
    let oracle = CachedMelpaOracle::new(MAGIT_MELPA_PIN, "magit.el")
        .expect("prepare revision-pinned Magit source")
        .with_prelude(MAGIT_LOG_TUI_PRELUDE);
    let ready = |grid: &[String]| {
        grid.iter().any(|row| {
            row.contains("medium subject") && row.contains("A U Thor") && row.contains("2003-04-05")
        })
    };
    let mut pair = PackageTuiScenario::new("magit-log-margin", oracle.prepared_packages())
        .git_fixture(MAGIT_LOG_FIXTURE)
        .spawn_when_ready(
            ReadinessCheckpoint::new(
                "Magit log rows",
                PairTimeout::per_editor(Duration::from_secs(20), Duration::from_secs(30)),
            ),
            ready,
        )
        .expect("spawn ready package TUI pair");

    let gnu_snapshot = RawTerminalSnapshot::capture_full_screen(pair.gnu.screen());

    let expected_ansi_grid = expect_file!["snapshots/expected_ansi_grid.ansi"];
    expected_ansi_grid.assert_eq(&gnu_snapshot.ansi_grid());
    let expected_plain_grid = expect_file!["snapshots/expected_plain_grid.plain"];
    expected_plain_grid.assert_eq(&gnu_snapshot.plain_grid());

    pair.assert_display(DisplayCheckpoint::new(
        "Magit log full-screen terminal state",
    ));
    pair.assert_display(DisplayCheckpoint::raw_terminal(
        "Magit log full-screen terminal wire state",
    ));
}
