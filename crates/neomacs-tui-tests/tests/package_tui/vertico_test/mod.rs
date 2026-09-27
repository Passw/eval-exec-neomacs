use std::time::Duration;

use expect_test::expect_file;
use neomacs_tui_tests::RawTerminalSnapshot;

use super::{COMPAT_GNU_ELPA_PIN, CachedMelpaOracle, VERTICO_MELPA_PIN};

use super::scenario::{DisplayCheckpoint, PackageTuiScenario, PairTimeout, ReadinessCheckpoint};

const VERTICO_TUI_PRELUDE: &str = r#"
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

fn candidate_rows(grid: &[String]) -> Vec<u16> {
    grid.iter()
        .enumerate()
        .filter_map(|(row, contents)| {
            contents
                .trim_start()
                .starts_with("project-")
                .then_some(row as u16)
        })
        .collect()
}

#[test]
fn vertico_real_minibuffer_candidates_and_selection_match_gnu_grid() {
    let oracle = CachedMelpaOracle::new(VERTICO_MELPA_PIN, "vertico.el")
        .expect("prepare revision-pinned Vertico source")
        .with_gnu_elpa_dependency(COMPAT_GNU_ELPA_PIN)
        .expect("prepare exact Compat dependency")
        .with_prelude(VERTICO_TUI_PRELUDE);
    let ready = |grid: &[String]| grid.iter().any(|row| row.contains("*scratch*"));
    let mut pair = PackageTuiScenario::new("vertico-minibuffer", oracle.prepared_packages())
        .spawn_when_ready(
            ReadinessCheckpoint::new(
                "initial scratch buffer",
                PairTimeout::per_editor(Duration::from_secs(15), Duration::from_secs(20)),
            ),
            ready,
        )
        .expect("spawn ready package TUI pair");

    pair.send_keys_both("C-x b");
    for session in [&mut pair.gnu, &mut pair.neo] {
        session.read_until(Duration::from_secs(8), |grid| {
            grid.iter().any(|row| row.contains("Switch to buffer"))
        });
    }
    pair.send_both(b"project-");
    for session in [&mut pair.gnu, &mut pair.neo] {
        session.read_until(Duration::from_secs(8), |grid| {
            candidate_rows(grid).len() >= 3
        });
    }

    let gnu_rows = candidate_rows(&pair.gnu.text_grid());
    let neo_rows = candidate_rows(&pair.neo.text_grid());
    assert_eq!(neo_rows, gnu_rows, "Vertico candidate rows differ from GNU");
    let gnu_snapshot = RawTerminalSnapshot::capture_full_screen(pair.gnu.screen());

    let expected_ansi_grid = expect_file!["snapshots/expected_ansi_grid.ansi"];
    expected_ansi_grid.assert_eq(&gnu_snapshot.ansi_grid());
    let expected_plain_grid = expect_file!["snapshots/expected_plain_grid.plain"];
    expected_plain_grid.assert_eq(&gnu_snapshot.plain_grid());

    pair.assert_display(DisplayCheckpoint::new("Vertico full-screen terminal state"));

    pair.send_both(b"beta");
    pair.send_key_both("RET");
    for session in [&mut pair.gnu, &mut pair.neo] {
        session.read_until(Duration::from_secs(8), |grid| {
            grid.iter().any(|row| row.contains("BETA BUFFER"))
        });
        assert!(
            session
                .text_grid()
                .iter()
                .any(|row| row.contains("BETA BUFFER")),
            "{} did not select the real project-beta buffer",
            session.name
        );
    }
}
