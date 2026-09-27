//! Gruvbox's two real terminal palette branches and rendered editing surfaces.

use std::any::Any;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::time::Duration;

use expect_test::{Expect, ExpectFile, expect, expect_file};
use neomacs_tui_tests::{RawTerminalSnapshot, Snapshot, TuiSession};

use super::{
    COMPAT_GNU_ELPA_PIN, CachedMelpaOracle, GRUVBOX_THEME_MELPA_PIN, ORDERLESS_MELPA_PIN,
    PreparedPackageSet,
};

use super::scenario::{
    PackageTuiPair, PackageTuiScenario, PairTimeout, ReadinessCheckpoint, TerminalProfile,
};

mod prelude;

use prelude::GRUVBOX_TUI_PRELUDE;

const REPORT_PREFIXES: &[&str] = &[
    "CAP ",
    "THEMES-KNOWN ",
    "GRUVBOX-TUI-BOOT",
    "GRUVBOX-THEME-PAGE ",
    "GRUVBOX-THEME-PAGE-DONE ",
    "THEME ",
    "ENABLED ",
    "MODE ",
    "FACE ",
    "VAR ",
    "GRUVBOX-THEME-READY",
    "PROPERTIES ",
    "PROPERTY-COUNT ",
    "RUN ",
    "GRUVBOX-PROPERTIES-",
    "BOLD ",
    "GRUVBOX-BOLD-READY",
    "ORDERLESS ",
    "GRUVBOX-ORDERLESS-READY",
    "CONSUMER ",
    "GRUVBOX-CONSUMER-READY",
    "CORE-ORG ",
    "GRUVBOX-CORE-ORG-READY",
    "DEFAULT-ORG ",
    "GRUVBOX-DEFAULT-ORG-READY",
];

fn oracle() -> CachedMelpaOracle {
    CachedMelpaOracle::new(GRUVBOX_THEME_MELPA_PIN, "gruvbox.el")
        .expect("prepare exact Gruvbox Theme source below ./tmp")
        .with_installed_autoloads()
        .with_melpa_dependency(ORDERLESS_MELPA_PIN)
        .expect("prepare exact Orderless optional-integration source below ./tmp")
        .with_gnu_elpa_dependency(COMPAT_GNU_ELPA_PIN)
        .expect("prepare exact Compat closure for Orderless below ./tmp")
        .with_prelude(GRUVBOX_TUI_PRELUDE)
}

fn wait_for<F>(session: &mut TuiSession, description: &str, predicate: F)
where
    F: Fn(&[String]) -> bool,
{
    session.read_until(Duration::from_secs(20), |grid| predicate(grid));
    let grid = session.text_grid();
    assert!(
        predicate(&grid),
        "{} timed out waiting for {description}:\n{}",
        session.name,
        grid.join("\n")
    );
}

fn invoke(session: &mut TuiSession, command: &str, ready: &str) {
    session.send_keys("M-x");
    wait_for(session, "M-x prompt", |grid| {
        grid.iter().any(|row| row.contains("M-x"))
    });
    session.send(command.as_bytes());
    session.send_keys("RET");
    wait_for(session, ready, |grid| {
        grid.iter().any(|row| row.contains(ready))
    });
}

fn panic_text(payload: Box<dyn Any + Send>) -> String {
    payload
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| {
            payload
                .downcast_ref::<&str>()
                .map(|value| (*value).to_owned())
        })
        .unwrap_or_else(|| "non-string panic payload".to_owned())
}

fn catch_phase<T>(label: &str, phase: impl FnOnce() -> T) -> Result<T, String> {
    catch_unwind(AssertUnwindSafe(phase))
        .map_err(|payload| format!("{label}: {}", panic_text(payload)))
}

fn invoke_both(pair: &mut PackageTuiPair, command: &str, ready: &str) {
    let gnu = catch_phase(&format!("GNU {command}"), || {
        invoke(&mut pair.gnu, command, ready)
    });
    let neo = catch_phase(&format!("Neo {command}"), || {
        invoke(&mut pair.neo, command, ready)
    });
    let errors = [gnu.err(), neo.err()]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    assert!(
        errors.is_empty(),
        "dual-peer command failed:\n{}",
        errors.join("\n")
    );
}

fn wait_for_boot_both(pair: &mut PackageTuiPair) {
    // Queue the public command into each real command loop.  GNU may display
    // an informational startup warning after the startup file finishes; the
    // explicit command makes the final visible state the owned report.
    invoke_both(pair, "gt357-show-boot", "GRUVBOX-TUI-BOOT");
}

fn report(session: &TuiSession) -> String {
    session
        .text_grid()
        .into_iter()
        .map(|row| row.trim_end().to_owned())
        .filter(|row| REPORT_PREFIXES.iter().any(|prefix| row.starts_with(prefix)))
        .collect::<Vec<_>>()
        .join("\n")
}

fn record_pair(
    pair: &PackageTuiPair,
    label: &str,
    expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) -> String {
    let gnu = report(&pair.gnu);
    let neo = report(&pair.neo);
    if neo != gnu {
        mismatches.push(format!("{label} differs\nGNU:\n{gnu}\nNeo:\n{neo}"));
    }
    expected.assert_snapshot(&gnu);
    gnu
}

fn ansi_rows(session: &TuiSession, needles: &[&str]) -> String {
    let grid = session.text_grid();
    needles
        .iter()
        .map(|needle| {
            let row = grid
                .iter()
                .position(|contents| contents.contains(needle))
                .unwrap_or_else(|| {
                    panic!(
                        "{} never rendered {needle:?}:\n{}",
                        session.name,
                        grid.join("\n")
                    )
                }) as u16;
            let mut snapshot = RawTerminalSnapshot::capture_rows(session.screen(), row..row + 1);
            let meaningful_end = snapshot.rows[0]
                .cells
                .iter()
                .rposition(|cell| {
                    cell.contents()
                        .chars()
                        .any(|character| !character.is_whitespace())
                })
                .unwrap_or(0)
                + 1;
            snapshot.rows[0].cells.truncate(meaningful_end);
            snapshot.ansi_grid()
        })
        .collect::<Vec<_>>()
        .join("")
}

fn record_grid(
    pair: &PackageTuiPair,
    label: &str,
    needles: &[&str],
    expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) -> String {
    let gnu = catch_phase(&format!("GNU {label} grid"), || {
        ansi_rows(&pair.gnu, needles)
    });
    let neo = catch_phase(&format!("Neo {label} grid"), || {
        ansi_rows(&pair.neo, needles)
    });
    let errors = [gnu.as_ref().err(), neo.as_ref().err()]
        .into_iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>();
    assert!(
        errors.is_empty(),
        "dual-peer grid capture failed:\n{}",
        errors.join("\n")
    );
    let gnu = gnu.expect("checked GNU grid result");
    let neo = neo.expect("checked Neo grid result");
    if neo != gnu {
        mismatches.push(format!("{label} differs\nGNU: {gnu:?}\nNeo: {neo:?}"));
    }
    expected.assert_snapshot(&gnu);
    gnu
}

fn record_properties(
    pair: &mut PackageTuiPair,
    label: &str,
    expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) -> String {
    let mut gnu = Vec::new();
    let mut neo = Vec::new();
    for (command, tag) in [
        ("gt357-show-elisp-properties", "E"),
        ("gt357-show-org-properties", "O"),
        ("gt357-show-diff-properties", "D"),
    ] {
        invoke_both(
            pair,
            command,
            &format!("GRUVBOX-PROPERTIES-{tag}-PAGE-DONE 1/"),
        );
        let mut gnu_pages = vec![report(&pair.gnu)];
        let mut neo_pages = vec![report(&pair.neo)];
        let page_total = |editor: &str, report: &str| {
            let line = report
                .lines()
                .find(|line| line.contains(&format!(" {tag} PAGE 1/")))
                .unwrap_or_else(|| {
                    panic!(
                        "{editor} property report omitted the exact {tag} page header:\n{report}"
                    )
                });
            line.rsplit_once('/')
                .and_then(|(_, total)| total.parse::<usize>().ok())
                .filter(|total| *total > 0)
                .unwrap_or_else(|| {
                    panic!("{editor} property report has invalid {tag} page total:\n{report}")
                })
        };
        let gnu_total = page_total("GNU", &gnu_pages[0]);
        let neo_total = page_total("Neo", &neo_pages[0]);
        assert_eq!(
            neo_total, gnu_total,
            "{label} {tag} property page count differs before snapshots"
        );
        for page in 2..=gnu_total {
            let ready = if page == gnu_total {
                format!("GRUVBOX-PROPERTIES-{tag}-READY")
            } else {
                format!("GRUVBOX-PROPERTIES-{tag}-PAGE-DONE {page}/{gnu_total}")
            };
            invoke_both(pair, "gt357-next-property-page", &ready);
            let gnu_page = report(&pair.gnu);
            let neo_page = report(&pair.neo);
            for (editor, page_report) in [("GNU", &gnu_page), ("Neo", &neo_page)] {
                assert!(
                    page_report
                        .lines()
                        .any(|line| line.contains(&format!(" {tag} PAGE {page}/{gnu_total}"))),
                    "{editor} property report omitted exact {tag} page {page}/{gnu_total}:\n{page_report}"
                );
            }
            gnu_pages.push(gnu_page);
            neo_pages.push(neo_page);
        }
        let gnu_report = gnu_pages.join("\n--\n");
        let neo_report = neo_pages.join("\n--\n");
        for (editor, final_page) in [
            ("GNU", gnu_pages.last().expect("GNU property page")),
            ("Neo", neo_pages.last().expect("Neo property page")),
        ] {
            assert!(
                final_page
                    .lines()
                    .any(|line| line == format!("GRUVBOX-PROPERTIES-{tag}-READY")),
                "{editor} property report omitted the trailing {tag} ready marker:\n{final_page}"
            );
        }
        assert!(
            gnu_report.contains(&format!("PROPERTY-COUNT {tag} ")),
            "GNU property report omitted the exact {tag} run count:\n{gnu_report}"
        );
        assert!(
            neo_report.contains(&format!("PROPERTY-COUNT {tag} ")),
            "Neo property report omitted the exact {tag} run count:\n{neo_report}"
        );
        gnu.push(gnu_report);
        neo.push(neo_report);
    }
    let gnu = gnu.join("\n--\n");
    let neo = neo.join("\n--\n");
    if neo != gnu {
        mismatches.push(format!("{label} differs\nGNU:\n{gnu}\nNeo:\n{neo}"));
    }
    expected.assert_snapshot(&gnu);
    gnu
}

fn drive_orderless_completion(session: &mut TuiSession) -> (String, String) {
    session.send_keys("M-x");
    wait_for(session, "M-x before Orderless completion", |grid| {
        grid.iter().any(|row| row.contains("M-x"))
    });
    session.send(b"gt357-orderless-select");
    session.send_keys("RET");
    wait_for(session, "real Orderless minibuffer prompt", |grid| {
        grid.iter().any(|row| row.contains("Gruvbox Orderless:"))
    });
    session.send(b"alp b gam del");
    session.send_keys("TAB");
    wait_for(session, "Orderless four-component completion row", |grid| {
        grid.iter()
            .any(|row| row.contains("alpha beta gamma delta"))
    });
    let grid = ansi_rows(session, &["alpha beta gamma delta"]);
    session.send_keys("C-a");
    session.send_keys("C-k");
    session.send(b"alpha beta gamma delta");
    session.send_keys("RET");
    wait_for(session, "completed Orderless selection", |grid| {
        grid.iter()
            .any(|row| row.contains("GRUVBOX-ORDERLESS-READY"))
    });
    (grid, report(session))
}

fn record_orderless_completion(
    pair: &mut PackageTuiPair,
    grid_expected: impl Snapshot,
    report_expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) {
    let gnu = catch_phase("GNU real Orderless completion", || {
        drive_orderless_completion(&mut pair.gnu)
    });
    let neo = catch_phase("Neo real Orderless completion", || {
        drive_orderless_completion(&mut pair.neo)
    });
    let errors = [gnu.as_ref().err(), neo.as_ref().err()]
        .into_iter()
        .flatten()
        .cloned()
        .collect::<Vec<_>>();
    assert!(
        errors.is_empty(),
        "dual-peer Orderless completion failed:\n{}",
        errors.join("\n")
    );
    let (gnu_grid, gnu_report) = gnu.expect("checked GNU Orderless result");
    let (neo_grid, neo_report) = neo.expect("checked Neo Orderless result");
    if neo_grid != gnu_grid {
        mismatches.push(format!(
            "Orderless rendered completion differs\nGNU: {gnu_grid:?}\nNeo: {neo_grid:?}"
        ));
    }
    if neo_report != gnu_report {
        mismatches.push(format!(
            "Orderless completion report differs\nGNU:\n{gnu_report}\nNeo:\n{neo_report}"
        ));
    }
    grid_expected.assert_snapshot(&gnu_grid);
    report_expected.assert_snapshot(&gnu_report);
}

fn initialize_consumer(
    pair: &mut PackageTuiPair,
    profile: &str,
    expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) {
    invoke_both(pair, "gt357-use-dark-medium", "; comment Ω");
    invoke_both(pair, "gt357-show-consumer-state", "GRUVBOX-CONSUMER-READY");
    record_pair(
        pair,
        &format!("{profile} first lazy compiled consumer"),
        expected,
        mismatches,
    );
}

fn spawn_profile(
    label: &str,
    packages: &PreparedPackageSet,
    terminal_profile: TerminalProfile,
) -> Result<PackageTuiPair, String> {
    PackageTuiScenario::new(label, packages)
        .terminal_profile(terminal_profile)
        .spawn_when_ready(
            ReadinessCheckpoint::new(
                "initial scratch buffer",
                PairTimeout::same(Duration::from_secs(20)),
            ),
            |grid| grid.iter().any(|row| row.contains("*scratch*")),
        )
}

fn capture_matrix_pair(pair: &mut PackageTuiPair) -> (String, String) {
    let mut gnu = Vec::new();
    let mut neo = Vec::new();
    for _ in 0..7 {
        invoke_both(pair, "gt357-next-theme", "GRUVBOX-THEME-PAGE-DONE 1/3");
        gnu.push(report(&pair.gnu));
        neo.push(report(&pair.neo));
        invoke_both(pair, "gt357-next-state-page", "GRUVBOX-THEME-PAGE-DONE 2/3");
        gnu.push(report(&pair.gnu));
        neo.push(report(&pair.neo));
        invoke_both(pair, "gt357-next-state-page", "GRUVBOX-THEME-READY");
        let gnu_final = report(&pair.gnu);
        let neo_final = report(&pair.neo);
        assert!(
            gnu_final
                .lines()
                .any(|line| line == "GRUVBOX-THEME-PAGE 3/3"),
            "GNU final matrix page lacks the exact 3/3 header:\n{gnu_final}"
        );
        assert!(
            neo_final
                .lines()
                .any(|line| line == "GRUVBOX-THEME-PAGE 3/3"),
            "Neo final matrix page lacks the exact 3/3 header:\n{neo_final}"
        );
        gnu.push(gnu_final);
        neo.push(neo_final);
    }
    (gnu.join("\n--\n"), neo.join("\n--\n"))
}

fn assert_matrix(
    pair: &mut PackageTuiPair,
    label: &str,
    expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) {
    let (gnu, neo) = capture_matrix_pair(pair);
    if neo != gnu {
        mismatches.push(format!("{label} differs\nGNU:\n{gnu}\nNeo:\n{neo}"));
    }
    expected.assert_snapshot(&gnu);
}

fn finish(pair: &mut PackageTuiPair, mismatches: &mut Vec<String>) {
    invoke_both(pair, "gt357-finish", "GRUVBOX-TUI-CLEAN");
    let gnu = pair
        .gnu
        .text_grid()
        .into_iter()
        .find(|row| row.contains("GRUVBOX-TUI-CLEAN"))
        .unwrap_or_else(|| panic!("GNU did not report Gruvbox cleanup"));
    let neo = pair
        .neo
        .text_grid()
        .into_iter()
        .find(|row| row.contains("GRUVBOX-TUI-CLEAN"))
        .unwrap_or_else(|| panic!("Neo did not report Gruvbox cleanup"));
    if neo.trim_end() != gnu.trim_end() {
        mismatches.push(format!(
            "final cleanup differs\nGNU: {:?}\nNeo: {:?}",
            gnu.trim_end(),
            neo.trim_end()
        ));
    }
    expect!["GRUVBOX-TUI-CLEAN (:state t :errors nil)"].assert_eq(gnu.trim_end());
}

struct RenderingExpectations {
    dark_elisp: Expect,
    dark_org: Expect,
    dark_diff: Expect,
    dark_properties: ExpectFile,
    dark_state: ExpectFile,
    light_elisp: Expect,
    light_org: Expect,
    light_diff: Expect,
    light_properties: ExpectFile,
    light_state: ExpectFile,
}

struct RenderingSnapshots {
    dark_elisp: String,
    dark_org: String,
    dark_diff: String,
    dark_properties: String,
    dark_state: String,
    light_elisp: String,
    light_org: String,
    light_diff: String,
    light_properties: String,
    light_state: String,
}

fn exercise_rendering(
    pair: &mut PackageTuiPair,
    profile: &str,
    expected: RenderingExpectations,
    mismatches: &mut Vec<String>,
) -> RenderingSnapshots {
    let RenderingExpectations {
        dark_elisp,
        dark_org,
        dark_diff,
        dark_properties,
        dark_state,
        light_elisp,
        light_org,
        light_diff,
        light_properties,
        light_state,
    } = expected;
    invoke_both(pair, "gt357-use-dark-medium", "; comment Ω");
    let dark_elisp = record_grid(
        pair,
        &format!("{profile} dark Elisp"),
        &["; comment Ω", "defun greet", "\"Doc.\"", "Hello %s"],
        dark_elisp,
        mismatches,
    );
    invoke_both(pair, "gt357-show-org", "Plan Ω");
    let dark_org = record_grid(
        pair,
        &format!("{profile} dark Org"),
        &[
            "#+title: Plan Ω",
            "TODO Ship release",
            "DONE Verify rollback",
            "A link and =code=.",
            "#+begin_src",
            "message \"ship\"",
            "#+end_src",
        ],
        dark_org,
        mismatches,
    );
    invoke_both(pair, "gt357-show-diff", "diff --git");
    let dark_diff = record_grid(
        pair,
        &format!("{profile} dark Diff"),
        &[
            "diff --git",
            "--- a/a.el",
            "+++ b/a.el",
            "@@ -1 +1 @@",
            "-(old)",
            "+(new)",
        ],
        dark_diff,
        mismatches,
    );
    let dark_properties = record_properties(
        pair,
        &format!("{profile} dark property runs"),
        dark_properties,
        mismatches,
    );
    let dark_state = record_current_state(
        pair,
        &format!("{profile} dark state"),
        dark_state,
        mismatches,
    );

    invoke_both(pair, "gt357-use-light-medium", "; comment Ω");
    let light_elisp = record_grid(
        pair,
        &format!("{profile} light Elisp"),
        &["; comment Ω", "defun greet", "\"Doc.\"", "Hello %s"],
        light_elisp,
        mismatches,
    );
    invoke_both(pair, "gt357-show-org", "Plan Ω");
    let light_org = record_grid(
        pair,
        &format!("{profile} light Org"),
        &[
            "#+title: Plan Ω",
            "TODO Ship release",
            "DONE Verify rollback",
            "A link and =code=.",
            "#+begin_src",
            "message \"ship\"",
            "#+end_src",
        ],
        light_org,
        mismatches,
    );
    invoke_both(pair, "gt357-show-diff", "diff --git");
    let light_diff = record_grid(
        pair,
        &format!("{profile} light Diff"),
        &[
            "diff --git",
            "--- a/a.el",
            "+++ b/a.el",
            "@@ -1 +1 @@",
            "-(old)",
            "+(new)",
        ],
        light_diff,
        mismatches,
    );
    let light_properties = record_properties(
        pair,
        &format!("{profile} light property runs"),
        light_properties,
        mismatches,
    );
    let light_state = record_current_state(
        pair,
        &format!("{profile} light state"),
        light_state,
        mismatches,
    );

    RenderingSnapshots {
        dark_elisp,
        dark_org,
        dark_diff,
        dark_properties,
        dark_state,
        light_elisp,
        light_org,
        light_diff,
        light_properties,
        light_state,
    }
}

struct StackRenderingExpectations {
    light_elisp: Expect,
    light_org: Expect,
    light_diff: Expect,
    light_properties: ExpectFile,
    light_state: ExpectFile,
    restored_elisp: Expect,
    restored_org: Expect,
    restored_diff: Expect,
    restored_properties: ExpectFile,
    restored_state: ExpectFile,
}

fn record_current_state(
    pair: &mut PackageTuiPair,
    label: &str,
    expected: impl Snapshot,
    mismatches: &mut Vec<String>,
) -> String {
    let mut gnu = Vec::new();
    let mut neo = Vec::new();
    invoke_both(
        pair,
        "gt357-show-current-state",
        "GRUVBOX-THEME-PAGE-DONE 1/3",
    );
    gnu.push(report(&pair.gnu));
    neo.push(report(&pair.neo));
    for page in 2..=3 {
        let ready = if page == 3 {
            "GRUVBOX-THEME-READY".to_owned()
        } else {
            format!("GRUVBOX-THEME-PAGE-DONE {page}/3")
        };
        invoke_both(pair, "gt357-next-state-page", &ready);
        let gnu_page = report(&pair.gnu);
        let neo_page = report(&pair.neo);
        if page == 3 {
            assert!(
                gnu_page
                    .lines()
                    .any(|line| line == "GRUVBOX-THEME-PAGE 3/3"),
                "GNU final state page lacks the exact 3/3 header:\n{gnu_page}"
            );
            assert!(
                neo_page
                    .lines()
                    .any(|line| line == "GRUVBOX-THEME-PAGE 3/3"),
                "Neo final state page lacks the exact 3/3 header:\n{neo_page}"
            );
        }
        gnu.push(gnu_page);
        neo.push(neo_page);
    }
    let gnu = gnu.join("\n--\n");
    let neo = neo.join("\n--\n");
    if neo != gnu {
        mismatches.push(format!("{label} differs\nGNU:\n{gnu}\nNeo:\n{neo}"));
    }
    expected.assert_snapshot(&gnu);
    gnu
}

fn exercise_stack_rendering(
    pair: &mut PackageTuiPair,
    ordinary: &RenderingSnapshots,
    expected: StackRenderingExpectations,
    mismatches: &mut Vec<String>,
) {
    let StackRenderingExpectations {
        light_elisp,
        light_org,
        light_diff,
        light_properties,
        light_state,
        restored_elisp,
        restored_org,
        restored_diff,
        restored_properties,
        restored_state,
    } = expected;

    invoke_both(pair, "gt357-use-light-over-dark", "; comment Ω");
    let light_elisp_actual = record_grid(
        pair,
        "truecolor stacked light Elisp",
        &["; comment Ω", "defun greet", "\"Doc.\"", "Hello %s"],
        light_elisp,
        mismatches,
    );
    invoke_both(pair, "gt357-show-org", "Plan Ω");
    let light_org_actual = record_grid(
        pair,
        "truecolor stacked light Org",
        &[
            "#+title: Plan Ω",
            "TODO Ship release",
            "DONE Verify rollback",
            "A link and =code=.",
            "#+begin_src",
            "message \"ship\"",
            "#+end_src",
        ],
        light_org,
        mismatches,
    );
    invoke_both(pair, "gt357-show-diff", "diff --git");
    let light_diff_actual = record_grid(
        pair,
        "truecolor stacked light Diff",
        &[
            "diff --git",
            "--- a/a.el",
            "+++ b/a.el",
            "@@ -1 +1 @@",
            "-(old)",
            "+(new)",
        ],
        light_diff,
        mismatches,
    );
    let light_properties_actual = record_properties(
        pair,
        "truecolor stacked light property runs",
        light_properties,
        mismatches,
    );
    let light_state_actual = record_current_state(
        pair,
        "truecolor stacked light state",
        light_state,
        mismatches,
    );

    invoke_both(pair, "gt357-disable-stack-light", "; comment Ω");
    let restored_elisp_actual = record_grid(
        pair,
        "truecolor restored dark Elisp",
        &["; comment Ω", "defun greet", "\"Doc.\"", "Hello %s"],
        restored_elisp,
        mismatches,
    );
    invoke_both(pair, "gt357-show-org", "Plan Ω");
    let restored_org_actual = record_grid(
        pair,
        "truecolor restored dark Org",
        &[
            "#+title: Plan Ω",
            "TODO Ship release",
            "DONE Verify rollback",
            "A link and =code=.",
            "#+begin_src",
            "message \"ship\"",
            "#+end_src",
        ],
        restored_org,
        mismatches,
    );
    invoke_both(pair, "gt357-show-diff", "diff --git");
    let restored_diff_actual = record_grid(
        pair,
        "truecolor restored dark Diff",
        &[
            "diff --git",
            "--- a/a.el",
            "+++ b/a.el",
            "@@ -1 +1 @@",
            "-(old)",
            "+(new)",
        ],
        restored_diff,
        mismatches,
    );
    let restored_properties_actual = record_properties(
        pair,
        "truecolor restored dark property runs",
        restored_properties,
        mismatches,
    );
    let restored_state_actual = record_current_state(
        pair,
        "truecolor restored dark state",
        restored_state,
        mismatches,
    );

    for (label, actual, ordinary) in [
        (
            "stacked light Elisp",
            &light_elisp_actual,
            &ordinary.light_elisp,
        ),
        ("stacked light Org", &light_org_actual, &ordinary.light_org),
        (
            "stacked light Diff",
            &light_diff_actual,
            &ordinary.light_diff,
        ),
        (
            "stacked light properties",
            &light_properties_actual,
            &ordinary.light_properties,
        ),
        (
            "restored dark Elisp",
            &restored_elisp_actual,
            &ordinary.dark_elisp,
        ),
        (
            "restored dark Org",
            &restored_org_actual,
            &ordinary.dark_org,
        ),
        (
            "restored dark Diff",
            &restored_diff_actual,
            &ordinary.dark_diff,
        ),
        (
            "restored dark properties",
            &restored_properties_actual,
            &ordinary.dark_properties,
        ),
    ] {
        if actual != ordinary {
            mismatches.push(format!(
                "truecolor {label} differs from its ordinary rendering\nORDINARY:\n{ordinary}\nSTACKED/RESTORED:\n{actual}"
            ));
        }
    }
    let without_enabled = |state: &str| {
        state
            .lines()
            .filter(|line| !line.starts_with("ENABLED "))
            .collect::<Vec<_>>()
            .join("\n")
    };
    for (label, actual, ordinary) in [
        (
            "stacked light state",
            &light_state_actual,
            &ordinary.light_state,
        ),
        (
            "restored dark state",
            &restored_state_actual,
            &ordinary.dark_state,
        ),
    ] {
        if without_enabled(actual) != without_enabled(ordinary) {
            mismatches.push(format!(
                "truecolor {label} differs beyond the intentional enabled stack\nORDINARY:\n{ordinary}\nSTACKED/RESTORED:\n{actual}"
            ));
        }
    }
}

fn run_profile(
    label: &str,
    packages: &PreparedPackageSet,
    terminal_profile: TerminalProfile,
    body: impl FnOnce(&mut PackageTuiPair, &mut Vec<String>),
) -> Result<(), String> {
    let mut pair = spawn_profile(label, packages, terminal_profile)?;
    let mut mismatches = Vec::new();
    let body_result = catch_phase(&format!("{label} body"), || {
        wait_for_boot_both(&mut pair);
        body(&mut pair, &mut mismatches);
    });
    let cleanup_result = catch_phase(&format!("{label} cleanup"), || {
        finish(&mut pair, &mut mismatches)
    });
    let mut failures = [body_result.err(), cleanup_result.err()]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    failures.extend(mismatches);
    if failures.is_empty() {
        Ok(())
    } else {
        Err(failures.join("\n\n"))
    }
}

fn default_org_consumer(packages: &PreparedPackageSet) -> Result<(), String> {
    run_profile(
        "gruvbox-theme-default-org-consumer",
        packages,
        TerminalProfile::TrueColor,
        |pair, mismatches| {
            record_pair(
                pair,
                "default Org consumer capability",
                expect![[r#"
                    CAP TERM "dumb"
                    CAP COLORTERM "truecolor"
                    CAP CELLS 16777216
                    CAP VISUAL static-color
                    CAP DISPLAY color
                    CAP GRAPHIC nil
                    CAP TRUECOLOR t
                    CAP COLOR256 t
                    CAP ORG-COMPILED t
                    CAP GNUS-BEFORE nil
                    CAP LOAD-SUFFIXES (".el")
                    THEMES-KNOWN t
                    GRUVBOX-TUI-BOOT"#]],
                mismatches,
            );
            invoke_both(
                pair,
                "gt357-configure-default-org",
                "GRUVBOX-DEFAULT-ORG-READY",
            );
            record_pair(
                pair,
                "default Org module configuration",
                expect![[r#"
                    DEFAULT-ORG MODULES-1 (ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview)
                    DEFAULT-ORG MODULES-2 (ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
                    DEFAULT-ORG GNUS (nil nil nil)
                    GRUVBOX-DEFAULT-ORG-READY"#]],
                mismatches,
            );
            initialize_consumer(
                pair,
                "default Org",
                expect![[r#"
                CONSUMER MODULES-1 (ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview)
                CONSUMER MODULES-2 (ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
                CONSUMER BEFORE (nil nil nil)
                CONSUMER THEME (gruvbox-dark-medium)
                CONSUMER OUTCOME (:value returned)
                CONSUMER SOURCE "gnus-sum.elc" t
                CONSUMER AFTER (t t t)
                CONSUMER INHERIT (gnus-group-mail-1 gnus-group-news-low)
                CONSUMER SUFFIXES (".el")
                GRUVBOX-CONSUMER-READY"#]],
                mismatches,
            );
            invoke_both(pair, "gt357-show-org", "Plan Ω");
            record_grid(
                pair,
                "default Org rendered consumer",
                &[
                    "#+title: Plan Ω",
                    "TODO Ship release",
                    "DONE Verify rollback",
                    "A link and =code=.",
                    "#+begin_src",
                    "message \"ship\"",
                    "#+end_src",
                ],
                expect![[r#"
                    [0;38;2;124;111;100;48;2;40;40;40m#+title:[0;38;2;235;219;178;48;2;40;40;40m [0;38;2;69;133;136;48;2;40;40;40mPlan Ω[0m
                    [0;38;2;131;165;152;48;2;40;40;40m* [0;1;38;2;251;73;51;48;2;40;40;40mTODO[0;38;2;131;165;152;48;2;40;40;40m Ship release[0m
                    [0;38;2;250;189;47;48;2;40;40;40m** [0;1;38;2;142;192;124;48;2;40;40;40mDONE[0;38;2;250;189;47;48;2;40;40;40m [0;38;2;142;192;124;48;2;40;40;40mVerify rollback[0m
                    [0;38;2;235;219;178;48;2;40;40;40mA [0;4;38;2;104;157;106;48;2;40;40;40mlink[0;38;2;235;219;178;48;2;40;40;40m and [0;38;2;124;111;100;48;2;40;40;40m=code=[0;38;2;235;219;178;48;2;40;40;40m.[0m
                    [0;38;2;235;219;178;48;2;60;56;54m#+begin_src emacs-lisp[0m
                    [0;38;2;235;219;178;48;2;50;48;47m(message [0;38;2;184;187;38;48;2;50;48;47m"ship"[0;38;2;235;219;178;48;2;50;48;47m)[0m
                    [0;38;2;235;219;178;48;2;60;56;54m#+end_src[0m
                "#]],
                mismatches,
            );
        },
    )
}

fn truecolor(packages: &PreparedPackageSet) -> Result<(), String> {
    run_profile(
        "gruvbox-theme-truecolor",
        packages,
        TerminalProfile::TrueColor,
        |pair, mismatches| {
            record_pair(
                pair,
                "truecolor capability",
                expect![[r#"
                    CAP TERM "dumb"
                    CAP COLORTERM "truecolor"
                    CAP CELLS 16777216
                    CAP VISUAL static-color
                    CAP DISPLAY color
                    CAP GRAPHIC nil
                    CAP TRUECOLOR t
                    CAP COLOR256 t
                    CAP ORG-COMPILED t
                    CAP GNUS-BEFORE nil
                    CAP LOAD-SUFFIXES (".el")
                    THEMES-KNOWN t
                    GRUVBOX-TUI-BOOT"#]],
                mismatches,
            );
            invoke_both(pair, "gt357-configure-core-org", "GRUVBOX-CORE-ORG-READY");
            record_pair(
                pair,
                "truecolor core Org configuration",
                expect![[r#"
                    CORE-ORG BEFORE-1 (ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview)
                    CORE-ORG BEFORE-2 (ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
                    CORE-ORG AFTER nil
                    CORE-ORG GNUS (nil nil nil)
                    GRUVBOX-CORE-ORG-READY"#]],
                mismatches,
            );
            initialize_consumer(
                pair,
                "truecolor core",
                expect![[r#"
                CONSUMER MODULES-1 nil
                CONSUMER MODULES-2 nil
                CONSUMER BEFORE (nil nil nil)
                CONSUMER THEME (gruvbox-dark-medium)
                CONSUMER OUTCOME (:value returned)
                CONSUMER SOURCE nil nil
                CONSUMER AFTER (nil nil nil)
                CONSUMER INHERIT nil
                CONSUMER SUFFIXES (".el")
                GRUVBOX-CONSUMER-READY"#]],
                mismatches,
            );
            record_orderless_completion(
                pair,
                expect![[r#"
                [0;1;38;2;102;153;157;48;2;40;40;40malpha[0;38;2;235;219;178;48;2;40;40;40m [0;1;38;2;214;93;14;48;2;40;40;40mb[0;38;2;235;219;178;48;2;40;40;40meta [0;1;38;2;142;192;124;48;2;40;40;40mgam[0;38;2;235;219;178;48;2;40;40;40mma [0;1;38;2;215;153;33;48;2;40;40;40mdel[0;38;2;235;219;178;48;2;40;40;40mta[0m
            "#]],
                expect![[r##"
                ORDERLESS CHOICE "alpha beta gamma delta"
                ORDERLESS FINAL-INPUT "alpha beta gamma delta"
                ORDERLESS HISTORY-HEAD "alpha beta gamma delta"
                ORDERLESS MINIBUFFER nil
                ORDERLESS RUN ("alpha" orderless-match-face-0)
                ORDERLESS RUN (" " nil)
                ORDERLESS RUN ("b" orderless-match-face-1)
                ORDERLESS RUN ("eta " nil)
                ORDERLESS RUN ("gam" orderless-match-face-2)
                ORDERLESS RUN ("ma " nil)
                ORDERLESS RUN ("del" orderless-match-face-3)
                ORDERLESS RUN ("ta" nil)
                ORDERLESS FACE 0 "#66999D" "#66999D" bold bold
                ORDERLESS FACE 1 "#d65d0e" "#d65d0e" bold bold
                ORDERLESS FACE 2 "#8ec07c" "#8ec07c" bold bold
                ORDERLESS FACE 3 "#d79921" "#d79921" bold bold
                GRUVBOX-ORDERLESS-READY"##]],
                mismatches,
            );
            assert_matrix(
                pair,
                "truecolor seven-theme matrix",
                expect_file!["snapshots/truecolor_matrix.txt"],
                mismatches,
            );
            let ordinary = exercise_rendering(
                pair,
                "truecolor",
                RenderingExpectations {
                    dark_elisp: expect![[r#"
                        [0;38;2;124;111;100;48;2;40;40;40m; comment Ω[0m
                        [0;38;2;235;219;178;48;2;40;40;40m([0;38;2;251;73;51;48;2;40;40;40mdefun[0;38;2;235;219;178;48;2;40;40;40m [0;38;2;250;189;47;48;2;40;40;40mgreet[0;38;2;235;219;178;48;2;40;40;40m (name)[0m
                        [0;38;2;235;219;178;48;2;40;40;40m  [0;38;2;184;187;38;48;2;40;40;40m"Doc."[0m
                        [0;38;2;235;219;178;48;2;40;40;40m  ([0;38;2;251;73;51;48;2;40;40;40mif[0;38;2;235;219;178;48;2;40;40;40m name (message [0;38;2;184;187;38;48;2;40;40;40m"Hello %s"[0;38;2;235;219;178;48;2;40;40;40m name) nil))[0m
                    "#]],
                    dark_org: expect![[r#"
                        [0;38;2;124;111;100;48;2;40;40;40m#+title:[0;38;2;235;219;178;48;2;40;40;40m [0;38;2;69;133;136;48;2;40;40;40mPlan Ω[0m
                        [0;38;2;131;165;152;48;2;40;40;40m* [0;1;38;2;251;73;51;48;2;40;40;40mTODO[0;38;2;131;165;152;48;2;40;40;40m Ship release[0m
                        [0;38;2;250;189;47;48;2;40;40;40m** [0;1;38;2;142;192;124;48;2;40;40;40mDONE[0;38;2;250;189;47;48;2;40;40;40m [0;38;2;142;192;124;48;2;40;40;40mVerify rollback[0m
                        [0;38;2;235;219;178;48;2;40;40;40mA [0;4;38;2;104;157;106;48;2;40;40;40mlink[0;38;2;235;219;178;48;2;40;40;40m and [0;38;2;124;111;100;48;2;40;40;40m=code=[0;38;2;235;219;178;48;2;40;40;40m.[0m
                        [0;38;2;235;219;178;48;2;60;56;54m#+begin_src emacs-lisp[0m
                        [0;38;2;235;219;178;48;2;50;48;47m(message [0;38;2;184;187;38;48;2;50;48;47m"ship"[0;38;2;235;219;178;48;2;50;48;47m)[0m
                        [0;38;2;235;219;178;48;2;60;56;54m#+end_src[0m
                    "#]],
                    dark_diff: expect![[r#"
                        [0;38;2;235;219;178;48;2;60;56;54mdiff --git a/a.el b/a.el[0m
                        [0;38;2;235;219;178;48;2;60;56;54m--- [0;38;2;235;219;178;48;2;80;73;69ma/a.el[0m
                        [0;38;2;235;219;178;48;2;60;56;54m+++ [0;38;2;235;219;178;48;2;80;73;69mb/a.el[0m
                        [0;38;2;235;219;178;48;2;80;73;69m@@ -1 +1 @@[0m
                        [0;38;2;251;73;51;48;2;40;40;40m-([0;38;2;235;219;178;48;2;204;36;29mold[0;38;2;251;73;51;48;2;40;40;40m)[0m
                        [0;38;2;184;187;38;48;2;40;40;40m+([0;38;2;235;219;178;48;2;152;151;26mnew[0;38;2;184;187;38;48;2;40;40;40m)[0m
                    "#]],
                    dark_properties: expect_file!["snapshots/dark_properties.txt"],
                    dark_state: expect_file!["snapshots/dark_state.txt"],
                    light_elisp: expect![[r#"
                        [0;38;2;168;153;132;48;2;251;241;199m; comment Ω[0m
                        [0;38;2;60;56;54;48;2;251;241;199m([0;38;2;157;0;6;48;2;251;241;199mdefun[0;38;2;60;56;54;48;2;251;241;199m [0;38;2;181;118;20;48;2;251;241;199mgreet[0;38;2;60;56;54;48;2;251;241;199m (name)[0m
                        [0;38;2;60;56;54;48;2;251;241;199m  [0;38;2;121;116;14;48;2;251;241;199m"Doc."[0m
                        [0;38;2;60;56;54;48;2;251;241;199m  ([0;38;2;157;0;6;48;2;251;241;199mif[0;38;2;60;56;54;48;2;251;241;199m name (message [0;38;2;121;116;14;48;2;251;241;199m"Hello %s"[0;38;2;60;56;54;48;2;251;241;199m name) nil))[0m
                    "#]],
                    light_org: expect![[r#"
                        [0;38;2;168;153;132;48;2;251;241;199m#+title:[0;38;2;60;56;54;48;2;251;241;199m [0;38;2;69;133;136;48;2;251;241;199mPlan Ω[0m
                        [0;38;2;7;102;120;48;2;251;241;199m* [0;1;38;2;157;0;6;48;2;251;241;199mTODO[0;38;2;7;102;120;48;2;251;241;199m Ship release[0m
                        [0;38;2;181;118;20;48;2;251;241;199m** [0;1;38;2;66;123;88;48;2;251;241;199mDONE[0;38;2;181;118;20;48;2;251;241;199m [0;38;2;66;123;88;48;2;251;241;199mVerify rollback[0m
                        [0;38;2;60;56;54;48;2;251;241;199mA [0;4;38;2;104;157;106;48;2;251;241;199mlink[0;38;2;60;56;54;48;2;251;241;199m and [0;38;2;168;153;132;48;2;251;241;199m=code=[0;38;2;60;56;54;48;2;251;241;199m.[0m
                        [0;38;2;60;56;54;48;2;235;219;178m#+begin_src emacs-lisp[0m
                        [0;38;2;60;56;54;48;2;242;229;188m(message [0;38;2;121;116;14;48;2;242;229;188m"ship"[0;38;2;60;56;54;48;2;242;229;188m)[0m
                        [0;38;2;60;56;54;48;2;235;219;178m#+end_src[0m
                    "#]],
                    light_diff: expect![[r#"
                        [0;38;2;60;56;54;48;2;235;219;178mdiff --git a/a.el b/a.el[0m
                        [0;38;2;60;56;54;48;2;235;219;178m--- [0;38;2;60;56;54;48;2;213;196;161ma/a.el[0m
                        [0;38;2;60;56;54;48;2;235;219;178m+++ [0;38;2;60;56;54;48;2;213;196;161mb/a.el[0m
                        [0;38;2;60;56;54;48;2;213;196;161m@@ -1 +1 @@[0m
                        [0;38;2;157;0;6;48;2;251;241;199m-([0;38;2;60;56;54;48;2;204;36;29mold[0;38;2;157;0;6;48;2;251;241;199m)[0m
                        [0;38;2;121;116;14;48;2;251;241;199m+([0;38;2;60;56;54;48;2;152;151;26mnew[0;38;2;121;116;14;48;2;251;241;199m)[0m
                    "#]],
                    light_properties: expect_file!["snapshots/light_properties.txt"],
                    light_state: expect_file!["snapshots/light_state.txt"],
                },
                mismatches,
            );
            exercise_stack_rendering(
                pair,
                &ordinary,
                StackRenderingExpectations {
                    light_elisp: expect![[r#"
                        [0;38;2;168;153;132;48;2;251;241;199m; comment Ω[0m
                        [0;38;2;60;56;54;48;2;251;241;199m([0;38;2;157;0;6;48;2;251;241;199mdefun[0;38;2;60;56;54;48;2;251;241;199m [0;38;2;181;118;20;48;2;251;241;199mgreet[0;38;2;60;56;54;48;2;251;241;199m (name)[0m
                        [0;38;2;60;56;54;48;2;251;241;199m  [0;38;2;121;116;14;48;2;251;241;199m"Doc."[0m
                        [0;38;2;60;56;54;48;2;251;241;199m  ([0;38;2;157;0;6;48;2;251;241;199mif[0;38;2;60;56;54;48;2;251;241;199m name (message [0;38;2;121;116;14;48;2;251;241;199m"Hello %s"[0;38;2;60;56;54;48;2;251;241;199m name) nil))[0m
                    "#]],
                    light_org: expect![[r#"
                        [0;38;2;168;153;132;48;2;251;241;199m#+title:[0;38;2;60;56;54;48;2;251;241;199m [0;38;2;69;133;136;48;2;251;241;199mPlan Ω[0m
                        [0;38;2;7;102;120;48;2;251;241;199m* [0;1;38;2;157;0;6;48;2;251;241;199mTODO[0;38;2;7;102;120;48;2;251;241;199m Ship release[0m
                        [0;38;2;181;118;20;48;2;251;241;199m** [0;1;38;2;66;123;88;48;2;251;241;199mDONE[0;38;2;181;118;20;48;2;251;241;199m [0;38;2;66;123;88;48;2;251;241;199mVerify rollback[0m
                        [0;38;2;60;56;54;48;2;251;241;199mA [0;4;38;2;104;157;106;48;2;251;241;199mlink[0;38;2;60;56;54;48;2;251;241;199m and [0;38;2;168;153;132;48;2;251;241;199m=code=[0;38;2;60;56;54;48;2;251;241;199m.[0m
                        [0;38;2;60;56;54;48;2;235;219;178m#+begin_src emacs-lisp[0m
                        [0;38;2;60;56;54;48;2;242;229;188m(message [0;38;2;121;116;14;48;2;242;229;188m"ship"[0;38;2;60;56;54;48;2;242;229;188m)[0m
                        [0;38;2;60;56;54;48;2;235;219;178m#+end_src[0m
                    "#]],
                    light_diff: expect![[r#"
                        [0;38;2;60;56;54;48;2;235;219;178mdiff --git a/a.el b/a.el[0m
                        [0;38;2;60;56;54;48;2;235;219;178m--- [0;38;2;60;56;54;48;2;213;196;161ma/a.el[0m
                        [0;38;2;60;56;54;48;2;235;219;178m+++ [0;38;2;60;56;54;48;2;213;196;161mb/a.el[0m
                        [0;38;2;60;56;54;48;2;213;196;161m@@ -1 +1 @@[0m
                        [0;38;2;157;0;6;48;2;251;241;199m-([0;38;2;60;56;54;48;2;204;36;29mold[0;38;2;157;0;6;48;2;251;241;199m)[0m
                        [0;38;2;121;116;14;48;2;251;241;199m+([0;38;2;60;56;54;48;2;152;151;26mnew[0;38;2;121;116;14;48;2;251;241;199m)[0m
                    "#]],
                    light_properties: expect_file!["snapshots/truecolor_light_properties.txt"],
                    light_state: expect_file!["snapshots/truecolor_light_state.txt"],
                    restored_elisp: expect![[r#"
                        [0;38;2;124;111;100;48;2;40;40;40m; comment Ω[0m
                        [0;38;2;235;219;178;48;2;40;40;40m([0;38;2;251;73;51;48;2;40;40;40mdefun[0;38;2;235;219;178;48;2;40;40;40m [0;38;2;250;189;47;48;2;40;40;40mgreet[0;38;2;235;219;178;48;2;40;40;40m (name)[0m
                        [0;38;2;235;219;178;48;2;40;40;40m  [0;38;2;184;187;38;48;2;40;40;40m"Doc."[0m
                        [0;38;2;235;219;178;48;2;40;40;40m  ([0;38;2;251;73;51;48;2;40;40;40mif[0;38;2;235;219;178;48;2;40;40;40m name (message [0;38;2;184;187;38;48;2;40;40;40m"Hello %s"[0;38;2;235;219;178;48;2;40;40;40m name) nil))[0m
                    "#]],
                    restored_org: expect![[r#"
                        [0;38;2;124;111;100;48;2;40;40;40m#+title:[0;38;2;235;219;178;48;2;40;40;40m [0;38;2;69;133;136;48;2;40;40;40mPlan Ω[0m
                        [0;38;2;131;165;152;48;2;40;40;40m* [0;1;38;2;251;73;51;48;2;40;40;40mTODO[0;38;2;131;165;152;48;2;40;40;40m Ship release[0m
                        [0;38;2;250;189;47;48;2;40;40;40m** [0;1;38;2;142;192;124;48;2;40;40;40mDONE[0;38;2;250;189;47;48;2;40;40;40m [0;38;2;142;192;124;48;2;40;40;40mVerify rollback[0m
                        [0;38;2;235;219;178;48;2;40;40;40mA [0;4;38;2;104;157;106;48;2;40;40;40mlink[0;38;2;235;219;178;48;2;40;40;40m and [0;38;2;124;111;100;48;2;40;40;40m=code=[0;38;2;235;219;178;48;2;40;40;40m.[0m
                        [0;38;2;235;219;178;48;2;60;56;54m#+begin_src emacs-lisp[0m
                        [0;38;2;235;219;178;48;2;50;48;47m(message [0;38;2;184;187;38;48;2;50;48;47m"ship"[0;38;2;235;219;178;48;2;50;48;47m)[0m
                        [0;38;2;235;219;178;48;2;60;56;54m#+end_src[0m
                    "#]],
                    restored_diff: expect![[r#"
                        [0;38;2;235;219;178;48;2;60;56;54mdiff --git a/a.el b/a.el[0m
                        [0;38;2;235;219;178;48;2;60;56;54m--- [0;38;2;235;219;178;48;2;80;73;69ma/a.el[0m
                        [0;38;2;235;219;178;48;2;60;56;54m+++ [0;38;2;235;219;178;48;2;80;73;69mb/a.el[0m
                        [0;38;2;235;219;178;48;2;80;73;69m@@ -1 +1 @@[0m
                        [0;38;2;251;73;51;48;2;40;40;40m-([0;38;2;235;219;178;48;2;204;36;29mold[0;38;2;251;73;51;48;2;40;40;40m)[0m
                        [0;38;2;184;187;38;48;2;40;40;40m+([0;38;2;235;219;178;48;2;152;151;26mnew[0;38;2;184;187;38;48;2;40;40;40m)[0m
                    "#]],
                    restored_properties: expect_file!["snapshots/restored_properties.txt"],
                    restored_state: expect_file!["snapshots/restored_state.txt"],
                },
                mismatches,
            );
            invoke_both(pair, "gt357-show-bold-cycle", "GRUVBOX-BOLD-READY");
            record_pair(
                pair,
                "truecolor bold reload",
                expect![[r#"
                BOLD PLAIN (normal normal)
                BOLD BEFORE-RELOAD (normal normal)
                BOLD RELOADED (bold bold)
                BOLD PLAIN-AGAIN (normal normal)
                BOLD ORG-RUN ("TODO" (org-todo org-level-1))
                GRUVBOX-BOLD-READY"#]],
                mismatches,
            );
        },
    )
}

fn color256(packages: &PreparedPackageSet) -> Result<(), String> {
    run_profile(
        "gruvbox-theme-color256",
        packages,
        TerminalProfile::Indexed256,
        |pair, mismatches| {
            record_pair(
                pair,
                "256-color capability",
                expect![[r#"
                    CAP TERM "dumb"
                    CAP COLORTERM nil
                    CAP CELLS 256
                    CAP VISUAL static-color
                    CAP DISPLAY color
                    CAP GRAPHIC nil
                    CAP TRUECOLOR nil
                    CAP COLOR256 t
                    CAP ORG-COMPILED t
                    CAP GNUS-BEFORE nil
                    CAP LOAD-SUFFIXES (".el")
                    THEMES-KNOWN t
                    GRUVBOX-TUI-BOOT"#]],
                mismatches,
            );
            invoke_both(pair, "gt357-configure-core-org", "GRUVBOX-CORE-ORG-READY");
            record_pair(
                pair,
                "256-color core Org configuration",
                expect![[r#"
                    CORE-ORG BEFORE-1 (ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview)
                    CORE-ORG BEFORE-2 (ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww)
                    CORE-ORG AFTER nil
                    CORE-ORG GNUS (nil nil nil)
                    GRUVBOX-CORE-ORG-READY"#]],
                mismatches,
            );
            initialize_consumer(
                pair,
                "256-color core",
                expect![[r#"
                CONSUMER MODULES-1 nil
                CONSUMER MODULES-2 nil
                CONSUMER BEFORE (nil nil nil)
                CONSUMER THEME (gruvbox-dark-medium)
                CONSUMER OUTCOME (:value returned)
                CONSUMER SOURCE nil nil
                CONSUMER AFTER (nil nil nil)
                CONSUMER INHERIT nil
                CONSUMER SUFFIXES (".el")
                GRUVBOX-CONSUMER-READY"#]],
                mismatches,
            );
            record_orderless_completion(
                pair,
                expect![[r#"
                [0;1;38;5;73;48;5;235malpha[0;38;5;223;48;5;235m [0;1;38;5;166;48;5;235mb[0;38;5;223;48;5;235meta [0;1;38;5;108;48;5;235mgam[0;38;5;223;48;5;235mma [0;1;38;5;214;48;5;235mdel[0;38;5;223;48;5;235mta[0m
            "#]],
                expect![[r##"
                ORDERLESS CHOICE "alpha beta gamma delta"
                ORDERLESS FINAL-INPUT "alpha beta gamma delta"
                ORDERLESS HISTORY-HEAD "alpha beta gamma delta"
                ORDERLESS MINIBUFFER nil
                ORDERLESS RUN ("alpha" orderless-match-face-0)
                ORDERLESS RUN (" " nil)
                ORDERLESS RUN ("b" orderless-match-face-1)
                ORDERLESS RUN ("eta " nil)
                ORDERLESS RUN ("gam" orderless-match-face-2)
                ORDERLESS RUN ("ma " nil)
                ORDERLESS RUN ("del" orderless-match-face-3)
                ORDERLESS RUN ("ta" nil)
                ORDERLESS FACE 0 "#5fafaf" "#5fafaf" bold bold
                ORDERLESS FACE 1 "#d75f00" "#d75f00" bold bold
                ORDERLESS FACE 2 "#87af87" "#87af87" bold bold
                ORDERLESS FACE 3 "#ffaf00" "#ffaf00" bold bold
                GRUVBOX-ORDERLESS-READY"##]],
                mismatches,
            );
            assert_matrix(
                pair,
                "256-color seven-theme matrix",
                expect_file!["snapshots/color256_matrix.txt"],
                mismatches,
            );
            let _ordinary = exercise_rendering(
                pair,
                "256-color",
                RenderingExpectations {
                    dark_elisp: expect![[r#"
                        [0;38;5;243;48;5;235m; comment Ω[0m
                        [0;38;5;223;48;5;235m([0;38;5;167;48;5;235mdefun[0;38;5;223;48;5;235m [0;38;5;214;48;5;235mgreet[0;38;5;223;48;5;235m (name)[0m
                        [0;38;5;223;48;5;235m  [0;38;5;142;48;5;235m"Doc."[0m
                        [0;38;5;223;48;5;235m  ([0;38;5;167;48;5;235mif[0;38;5;223;48;5;235m name (message [0;38;5;142;48;5;235m"Hello %s"[0;38;5;223;48;5;235m name) nil))[0m
                    "#]],
                    dark_org: expect![[r#"
                        [0;38;5;243;48;5;235m#+title:[0;38;5;223;48;5;235m [0;38;5;109;48;5;235mPlan Ω[0m
                        [0;38;5;109;48;5;235m* [0;1;38;5;167;48;5;235mTODO[0;38;5;109;48;5;235m Ship release[0m
                        [0;38;5;214;48;5;235m** [0;1;38;5;108;48;5;235mDONE[0;38;5;214;48;5;235m [0;38;5;108;48;5;235mVerify rollback[0m
                        [0;38;5;223;48;5;235mA [0;4;38;5;108;48;5;235mlink[0;38;5;223;48;5;235m and [0;38;5;243;48;5;235m=code=[0;38;5;223;48;5;235m.[0m
                        [0;38;5;223;48;5;237m#+begin_src emacs-lisp[0m
                        [0;38;5;223;48;5;236m(message [0;38;5;142;48;5;236m"ship"[0;38;5;223;48;5;236m)[0m
                        [0;38;5;223;48;5;237m#+end_src[0m
                    "#]],
                    dark_diff: expect![[r#"
                        [0;38;5;223;48;5;237mdiff --git a/a.el b/a.el[0m
                        [0;38;5;223;48;5;237m--- [0;38;5;223;48;5;239ma/a.el[0m
                        [0;38;5;223;48;5;237m+++ [0;38;5;223;48;5;239mb/a.el[0m
                        [0;38;5;223;48;5;239m@@ -1 +1 @@[0m
                        [0;38;5;167;48;5;235m-([0;38;5;223;48;5;167mold[0;38;5;167;48;5;235m)[0m
                        [0;38;5;142;48;5;235m+([0;38;5;223;48;5;142mnew[0;38;5;142;48;5;235m)[0m
                    "#]],
                    dark_properties: expect_file!["snapshots/color256_dark_properties.txt"],
                    dark_state: expect_file!["snapshots/color256_dark_state.txt"],
                    light_elisp: expect![[r#"
                        [0;38;5;145;48;5;230m; comment Ω[0m
                        [0;38;5;237;48;5;230m([0;38;5;88;48;5;230mdefun[0;38;5;237;48;5;230m [0;38;5;136;48;5;230mgreet[0;38;5;237;48;5;230m (name)[0m
                        [0;38;5;237;48;5;230m  [0;38;5;100;48;5;230m"Doc."[0m
                        [0;38;5;237;48;5;230m  ([0;38;5;88;48;5;230mif[0;38;5;237;48;5;230m name (message [0;38;5;100;48;5;230m"Hello %s"[0;38;5;237;48;5;230m name) nil))[0m
                    "#]],
                    light_org: expect![[r#"
                        [0;38;5;145;48;5;230m#+title:[0;38;5;237;48;5;230m [0;38;5;109;48;5;230mPlan Ω[0m
                        [0;38;5;24;48;5;230m* [0;1;38;5;88;48;5;230mTODO[0;38;5;24;48;5;230m Ship release[0m
                        [0;38;5;136;48;5;230m** [0;1;38;5;66;48;5;230mDONE[0;38;5;136;48;5;230m [0;38;5;66;48;5;230mVerify rollback[0m
                        [0;38;5;237;48;5;230mA [0;4;38;5;108;48;5;230mlink[0;38;5;237;48;5;230m and [0;38;5;145;48;5;230m=code=[0;38;5;237;48;5;230m.[0m
                        [0;38;5;237;48;5;229m#+begin_src emacs-lisp[0m
                        [0;38;5;237;48;5;230m(message [0;38;5;100;48;5;230m"ship"[0;38;5;237;48;5;230m)[0m
                        [0;38;5;237;48;5;229m#+end_src[0m
                    "#]],
                    light_diff: expect![[r#"
                        [0;38;5;237;48;5;229mdiff --git a/a.el b/a.el[0m
                        [0;38;5;237;48;5;229m--- [0;38;5;237;48;5;187ma/a.el[0m
                        [0;38;5;237;48;5;229m+++ [0;38;5;237;48;5;187mb/a.el[0m
                        [0;38;5;237;48;5;187m@@ -1 +1 @@[0m
                        [0;38;5;88;48;5;230m-([0;38;5;237;48;5;167mold[0;38;5;88;48;5;230m)[0m
                        [0;38;5;100;48;5;230m+([0;38;5;237;48;5;142mnew[0;38;5;100;48;5;230m)[0m
                    "#]],
                    light_properties: expect_file!["snapshots/color256_light_properties.txt"],
                    light_state: expect_file!["snapshots/color256_light_state.txt"],
                },
                mismatches,
            );
        },
    )
}

#[test]
fn gruvbox_theme_real_terminal_profiles_match_gnu() {
    let oracle = oracle();
    let default_org = catch_phase("default Org consumer profile", || {
        default_org_consumer(oracle.prepared_packages())
    })
    .and_then(|result| result);
    let truecolor = catch_phase("truecolor profile", || {
        truecolor(oracle.prepared_packages())
    })
    .and_then(|result| result);
    let color256 = catch_phase("256-color profile", || color256(oracle.prepared_packages()))
        .and_then(|result| result);
    let failures = [default_org.err(), truecolor.err(), color256.err()]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    assert!(
        failures.is_empty(),
        "Gruvbox real terminal profiles failed:\n{}",
        failures.join("\n\n")
    );
}
