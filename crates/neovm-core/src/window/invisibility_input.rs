//! Owned inputs to GNU invisible_prop's ordered membership checks.
use crate::emacs_core::value::Value;
use rustc_hash::FxHashSet;
use std::sync::Arc;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LayoutInvisibilityInput(Selection);

#[derive(Clone, Debug, Default, PartialEq, Eq)]
enum Selection {
    #[default]
    None,
    All,
    Entries(Arc<[Entry]>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Entry {
    // GNU first compares the property to the whole entry, including conses.
    identity: usize,
    category: Option<(usize, bool)>,
}

impl LayoutInvisibilityInput {
    pub(super) fn capture(value: Value) -> Self {
        if value == Value::T {
            return Self(Selection::All);
        }
        let mut tail = value;
        let mut seen = FxHashSet::default();
        let mut entries = Vec::new();
        while tail.is_cons() && seen.insert(tail.bits()) {
            let entry = tail.cons_car();
            entries.push(Entry {
                identity: entry.bits(),
                category: entry
                    .is_cons()
                    .then(|| (entry.cons_car().bits(), !entry.cons_cdr().is_nil())),
            });
            tail = tail.cons_cdr();
        }
        if entries.is_empty() {
            Self::default()
        } else {
            Self(Selection::Entries(entries.into()))
        }
    }
}
