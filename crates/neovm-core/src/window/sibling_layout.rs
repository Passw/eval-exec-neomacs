//! Pure sibling-layout arithmetic for one window split.
//!
//! Laying a parent's children out along the split axis needs only four numbers
//! per child -- its current rectangle and its two `window-fixed-size` extents --
//! yet the arithmetic used to be written directly against `&mut [Window]`. That
//! coupling made the hardest part of window layout the part hardest to test, and
//! it is the one place that cannot survive a `children: Vec<Window>` ->
//! `Vec<WindowId>` flip unchanged, because arena children cannot be borrowed
//! mutably while their siblings are read.
//!
//! So the math lives here instead, over [`ChildExtent`] values that a caller
//! gathers before touching the tree. `sibling_bounds` is a total function of its
//! inputs: gather extents, drop the borrow, compute, write the rectangles back.

use super::{Rect, SplitDirection};

/// Everything sibling layout needs to know about one child window.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct ChildExtent {
    /// The child's rectangle before the redistribution.
    pub bounds: Rect,
    /// `window-size-fixed` width in columns; 0 when the width may flex.
    pub fixed_width_cols: usize,
    /// `window-size-fixed` height in lines; 0 when the height may flex.
    pub fixed_height_lines: usize,
}

impl ChildExtent {
    /// The child's extent along `direction`, clamped to a whole non-negative
    /// pixel count.
    fn current(&self, direction: SplitDirection) -> f32 {
        match direction {
            SplitDirection::Horizontal => self.bounds.width,
            SplitDirection::Vertical => self.bounds.height,
        }
        .round()
        .max(0.0)
    }

    /// Whether this child refuses to flex along `direction`.
    fn is_fixed(&self, direction: SplitDirection) -> bool {
        let fixed_cells = match direction {
            SplitDirection::Horizontal => self.fixed_width_cols,
            SplitDirection::Vertical => self.fixed_height_lines,
        };
        fixed_cells > 0
    }
}

/// The axis `children` are already laid out along, inferred from the first two
/// siblings: children that differ in `x` sit side by side, anything else is
/// stacked. `None` when there is no pair to compare.
pub(crate) fn detect_direction(children: &[ChildExtent]) -> Option<SplitDirection> {
    let (first, second) = (children.first()?, children.get(1)?);
    Some(if (first.bounds.x - second.bounds.x).abs() > 0.1 {
        SplitDirection::Horizontal
    } else {
        SplitDirection::Vertical
    })
}

/// The rectangles `children` should occupy inside `parent`, in child order.
///
/// The returned vector always has one rectangle per child, so a caller can zip
/// it straight back over the siblings it gathered the extents from.
pub(crate) fn sibling_bounds(parent: Rect, children: &[ChildExtent]) -> Vec<Rect> {
    let Some(direction) = detect_direction(children) else {
        // No pair to infer an axis from: a lone child inherits the parent's
        // rectangle verbatim, unrounded, and an empty parent lays out nothing.
        return children.iter().map(|_| parent).collect();
    };

    match direction {
        SplitDirection::Horizontal => {
            let widths = sizes_preserving_fixed(parent.width, children, direction);
            let mut edge = parent.x.round();
            widths
                .into_iter()
                .map(|width| {
                    let rect = Rect::new(edge, parent.y.round(), width, parent.height.round());
                    edge += width;
                    rect
                })
                .collect()
        }
        SplitDirection::Vertical => {
            let heights = sizes_preserving_fixed(parent.height, children, direction);
            let mut edge = parent.y.round();
            heights
                .into_iter()
                .map(|height| {
                    let rect = Rect::new(parent.x.round(), edge, parent.width.round(), height);
                    edge += height;
                    rect
                })
                .collect()
        }
    }
}

/// Split `total` into `n` whole pixels as evenly as possible, handing the
/// remainder to the leading children one pixel at a time.
fn distributed_sizes(total: f32, n: usize) -> Vec<f32> {
    let total_px = total.round().max(0.0) as i64;
    let n = n as i64;
    let base = total_px / n;
    let remainder = total_px % n;
    (0..n)
        .map(|idx| (base + i64::from(idx < remainder)) as f32)
        .collect()
}

/// Split `total` along `direction`, holding every fixed-size child at the size
/// it already has and sharing what is left among the rest in proportion to
/// their current sizes.
///
/// Falls back to an even split whenever the fixed children alone would fill
/// `total`, or when nothing is fixed at all.
fn sizes_preserving_fixed(
    total: f32,
    children: &[ChildExtent],
    direction: SplitDirection,
) -> Vec<f32> {
    let total_px = total.round().max(0.0);
    let mut sizes = vec![0.0; children.len()];
    let mut flexible = Vec::new();
    let mut fixed_total = 0.0;
    let mut flexible_current_total = 0.0;

    for (idx, child) in children.iter().enumerate() {
        let current = child.current(direction);
        if child.is_fixed(direction) {
            sizes[idx] = current;
            fixed_total += current;
        } else {
            flexible.push(idx);
            flexible_current_total += current;
        }
    }

    if flexible.is_empty() || fixed_total >= total_px {
        return distributed_sizes(total, children.len());
    }

    let flexible_total = total_px - fixed_total;
    if flexible_current_total <= 0.0 {
        // Nothing to scale in proportion to: share the slack out evenly.
        let flexible_sizes = distributed_sizes(flexible_total, flexible.len());
        for (idx, size) in flexible.into_iter().zip(flexible_sizes) {
            sizes[idx] = size;
        }
        return sizes;
    }

    let mut assigned = 0.0;
    let last_flexible = flexible.len().saturating_sub(1);
    for (flex_idx, idx) in flexible.into_iter().enumerate() {
        let current = children[idx].current(direction);
        let size = if flex_idx == last_flexible {
            // The last flexible child absorbs the rounding drift so the
            // children always tile the parent exactly.
            (flexible_total - assigned).max(0.0)
        } else {
            (flexible_total * (current / flexible_current_total))
                .round()
                .max(0.0)
        };
        sizes[idx] = size;
        assigned += size;
    }
    sizes
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flexible(bounds: Rect) -> ChildExtent {
        ChildExtent {
            bounds,
            fixed_width_cols: 0,
            fixed_height_lines: 0,
        }
    }

    fn fixed_width(bounds: Rect, cols: usize) -> ChildExtent {
        ChildExtent {
            bounds,
            fixed_width_cols: cols,
            fixed_height_lines: 0,
        }
    }

    fn fixed_height(bounds: Rect, lines: usize) -> ChildExtent {
        ChildExtent {
            bounds,
            fixed_width_cols: 0,
            fixed_height_lines: lines,
        }
    }

    #[test]
    fn no_children_lay_out_nothing() {
        assert!(sibling_bounds(Rect::new(0.0, 0.0, 80.0, 24.0), &[]).is_empty());
    }

    #[test]
    fn a_lone_child_inherits_the_parent_rectangle_unrounded() {
        let parent = Rect::new(0.5, 1.5, 80.25, 24.75);
        let children = [flexible(Rect::new(0.0, 0.0, 1.0, 1.0))];
        assert_eq!(sibling_bounds(parent, &children), vec![parent]);
    }

    #[test]
    fn siblings_differing_in_x_are_a_horizontal_split() {
        let children = [
            flexible(Rect::new(0.0, 0.0, 40.0, 24.0)),
            flexible(Rect::new(40.0, 0.0, 40.0, 24.0)),
        ];
        assert_eq!(
            detect_direction(&children),
            Some(SplitDirection::Horizontal)
        );
    }

    #[test]
    fn siblings_sharing_an_x_are_a_vertical_split() {
        let children = [
            flexible(Rect::new(0.0, 0.0, 80.0, 12.0)),
            flexible(Rect::new(0.0, 12.0, 80.0, 12.0)),
        ];
        assert_eq!(detect_direction(&children), Some(SplitDirection::Vertical));
    }

    #[test]
    fn a_single_child_has_no_direction_to_detect() {
        assert_eq!(
            detect_direction(&[flexible(Rect::new(0.0, 0.0, 80.0, 24.0))]),
            None
        );
    }

    #[test]
    fn children_tile_the_parent_without_gaps_or_overlap() {
        let parent = Rect::new(10.0, 20.0, 81.0, 25.0);
        let children = [
            flexible(Rect::new(10.0, 20.0, 27.0, 25.0)),
            flexible(Rect::new(37.0, 20.0, 27.0, 25.0)),
            flexible(Rect::new(64.0, 20.0, 27.0, 25.0)),
        ];
        let laid_out = sibling_bounds(parent, &children);

        let mut edge = parent.x;
        for rect in &laid_out {
            assert_eq!(rect.x, edge, "children must abut: {laid_out:?}");
            edge = rect.right();
        }
        assert_eq!(edge, parent.right(), "children must fill the parent");
    }

    #[test]
    fn an_indivisible_width_gives_the_extra_pixel_to_the_leading_child() {
        // 81 pixels over 2 children: 41 then 40, never 40.5 twice.
        let parent = Rect::new(0.0, 0.0, 81.0, 24.0);
        let children = [
            flexible(Rect::new(0.0, 0.0, 0.0, 24.0)),
            flexible(Rect::new(1.0, 0.0, 0.0, 24.0)),
        ];
        let widths: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.width)
            .collect();
        assert_eq!(widths, vec![41.0, 40.0]);
    }

    #[test]
    fn a_fixed_width_child_keeps_its_width_while_its_sibling_absorbs_the_rest() {
        let parent = Rect::new(0.0, 0.0, 100.0, 24.0);
        let children = [
            fixed_width(Rect::new(0.0, 0.0, 30.0, 24.0), 30),
            flexible(Rect::new(30.0, 0.0, 50.0, 24.0)),
        ];
        let widths: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.width)
            .collect();
        assert_eq!(widths, vec![30.0, 70.0]);
    }

    #[test]
    fn a_fixed_height_child_keeps_its_height_while_its_sibling_absorbs_the_rest() {
        let parent = Rect::new(0.0, 0.0, 80.0, 40.0);
        let children = [
            flexible(Rect::new(0.0, 0.0, 80.0, 20.0)),
            fixed_height(Rect::new(0.0, 20.0, 80.0, 4.0), 4),
        ];
        let heights: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.height)
            .collect();
        assert_eq!(heights, vec![36.0, 4.0]);
    }

    #[test]
    fn flexible_children_keep_their_proportions() {
        // 30:60 of the 90 flexible pixels, held across a grow to 180.
        let parent = Rect::new(0.0, 0.0, 180.0, 24.0);
        let children = [
            flexible(Rect::new(0.0, 0.0, 30.0, 24.0)),
            flexible(Rect::new(30.0, 0.0, 60.0, 24.0)),
        ];
        let widths: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.width)
            .collect();
        assert_eq!(widths, vec![60.0, 120.0]);
    }

    #[test]
    fn a_fixed_child_that_still_fits_leaves_only_the_slack_to_its_sibling() {
        // 60 fixed pixels inside an 80-pixel parent: the fixed child is honored
        // and its sibling is squeezed into what is left, however little.
        let parent = Rect::new(0.0, 0.0, 80.0, 24.0);
        let children = [
            fixed_width(Rect::new(0.0, 0.0, 60.0, 24.0), 60),
            flexible(Rect::new(60.0, 0.0, 60.0, 24.0)),
        ];
        let widths: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.width)
            .collect();
        assert_eq!(widths, vec![60.0, 20.0]);
    }

    #[test]
    fn fixed_children_that_overfill_the_parent_fall_back_to_an_even_split() {
        // Two 60-pixel fixed children cannot both fit in 80 pixels, so neither
        // keeps its size: the parent is shared evenly instead.
        let parent = Rect::new(0.0, 0.0, 80.0, 24.0);
        let children = [
            fixed_width(Rect::new(0.0, 0.0, 60.0, 24.0), 60),
            fixed_width(Rect::new(60.0, 0.0, 60.0, 24.0), 60),
        ];
        let widths: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.width)
            .collect();
        assert_eq!(widths, vec![40.0, 40.0]);
    }

    #[test]
    fn children_with_no_size_yet_are_split_evenly() {
        let parent = Rect::new(0.0, 0.0, 80.0, 24.0);
        let children = [
            flexible(Rect::new(0.0, 0.0, 0.0, 24.0)),
            fixed_width(Rect::new(20.0, 0.0, 20.0, 24.0), 20),
            flexible(Rect::new(40.0, 0.0, 0.0, 24.0)),
        ];
        let widths: Vec<f32> = sibling_bounds(parent, &children)
            .iter()
            .map(|rect| rect.width)
            .collect();
        assert_eq!(widths, vec![30.0, 20.0, 30.0]);
    }

    #[test]
    fn every_child_gets_exactly_one_rectangle() {
        let parent = Rect::new(0.0, 0.0, 80.0, 24.0);
        for count in 0..8usize {
            let children: Vec<ChildExtent> = (0..count)
                .map(|idx| flexible(Rect::new(0.0, idx as f32 * 3.0, 80.0, 3.0)))
                .collect();
            assert_eq!(sibling_bounds(parent, &children).len(), count);
        }
    }
}
