//! The screen vocabulary the Vertico suite shares.
//!
//! Everything here is about the candidate window Vertico owns: which rows hold
//! candidates, which one carries the highlight, and what the minibuffer's count
//! indicator says. The waits, the symmetric steps and the panic-to-value
//! conversion come from [`neomacs_tui_tests::package_harness`], re-exported so
//! the scenario modules reach them the same way they reach these helpers.

/// The rows holding a candidate whose text starts with `prefix`, in screen
/// order.
///
/// Vertico renders one candidate per row below the prompt, so a row that
/// begins with the fixture's prefix is a candidate row and nothing else on the
/// screen is.
pub(super) fn candidate_rows(grid: &[String], prefix: &str) -> Vec<u16> {
    grid.iter()
        .enumerate()
        .filter_map(|(row, contents)| {
            contents
                .trim_start()
                .starts_with(prefix)
                .then_some(row as u16)
        })
        .collect()
}
