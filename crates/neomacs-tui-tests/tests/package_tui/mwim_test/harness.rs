use super::super::scenario::PackageTuiPair;
use neomacs_tui_tests::TuiSession;
use std::any::Any;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::time::Duration;

pub(super) fn wait_for(
    session: &mut TuiSession,
    timeout: Duration,
    description: &str,
    predicate: impl Fn(&[String]) -> bool,
) {
    session.read_until(timeout, |grid| predicate(grid));
    let grid = session.text_grid();
    assert!(
        predicate(&grid),
        "{} timed out waiting for {description}:\n{}",
        session.name,
        grid.join("\n")
    );
}

pub(super) fn invoke(session: &mut TuiSession, command: &str, ready: &str) {
    session.send_keys("M-x");
    wait_for(session, Duration::from_secs(8), "M-x prompt", |grid| {
        grid.iter().any(|row| row.contains("M-x"))
    });
    session.send(command.as_bytes());
    session.send_keys("RET");
    wait_for(session, Duration::from_secs(20), ready, |grid| {
        grid.iter().any(|row| row.contains(ready))
    });
}

pub(super) fn panic_text(payload: Box<dyn Any + Send>) -> String {
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

pub(super) fn catch_phase<T>(label: &str, phase: impl FnOnce() -> T) -> Result<T, String> {
    catch_unwind(AssertUnwindSafe(phase))
        .map_err(|payload| format!("{label}: {}", panic_text(payload)))
}

pub(super) fn both(
    pair: &mut PackageTuiPair,
    label: &str,
    operation: impl Fn(&mut TuiSession) + Copy,
) -> Result<(), String> {
    let gnu = catch_phase(&format!("GNU {label}"), || operation(&mut pair.gnu));
    let neo = catch_phase(&format!("Neo {label}"), || operation(&mut pair.neo));
    let errors = [gnu.err(), neo.err()]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors.join("\n"))
    }
}

pub(super) fn exact_row(session: &TuiSession, marker: &str) -> String {
    session
        .text_grid()
        .into_iter()
        .find(|row| row.contains(marker))
        .unwrap_or_else(|| panic!("{} did not render {marker:?}", session.name))
        .trim()
        .to_owned()
}

pub(super) fn record_rows(
    pair: &PackageTuiPair,
    marker: &str,
    gnu_transcript: &mut Vec<String>,
    neo_transcript: &mut Vec<String>,
) {
    gnu_transcript.push(exact_row(&pair.gnu, marker));
    neo_transcript.push(exact_row(&pair.neo, marker));
}

pub(super) fn visual_grid(session: &TuiSession) -> String {
    let grid = session.text_grid();
    let (_, cols) = session.screen_size();
    [
        "alpha beta",
        "delta epsilon",
        "theta iota",
        "lambda mu",
        "wide 界",
        "maple spruce",
        "willow aspen",
    ]
    .into_iter()
    .map(|needle| {
        let row = grid
            .iter()
            .position(|contents| contents.contains(needle))
            .unwrap_or_else(|| {
                panic!(
                    "{} did not render visual row {needle:?}:\n{}",
                    session.name,
                    grid.join("\n")
                )
            }) as u16;
        session
            .screen()
            .contents_between(row, cols - 24, row, cols)
            .trim_end()
            .to_owned()
    })
    .enumerate()
    .map(|(index, row)| format!("MWIM-GRID-{} {row:?}", index + 1))
    .collect::<Vec<_>>()
    .join("\n")
}
