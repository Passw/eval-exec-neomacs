//! Evaluator-thread revision of symbol property-list writes. Layout can read
//! faces and display properties indirectly through overlay/text categories.
use std::cell::Cell;

thread_local! {
    static REVISION: Cell<u64> = const { Cell::new(0) };
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct SymbolPropertyRevision(u64);

impl SymbolPropertyRevision {
    pub fn current() -> Self {
        Self(REVISION.with(Cell::get))
    }

    pub(super) fn changed() {
        REVISION.with(|revision| revision.set(revision.get().wrapping_add(1)));
    }
}
