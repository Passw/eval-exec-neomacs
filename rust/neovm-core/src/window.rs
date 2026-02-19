//! Window and frame management for the editor.
//!
//! Implements the Emacs window tree model:
//! - A **frame** contains a root window (which may be split).
//! - A **window** is either a *leaf* (displays a buffer) or an *internal*
//!   node with children (horizontal or vertical split).
//! - The **selected window** is the one receiving input.
//! - The **minibuffer window** is a special single-line window at the bottom.

use crate::buffer::BufferId;
use crate::elisp::value::{eq_value, Value};
use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------------
// IDs
// ---------------------------------------------------------------------------

/// Opaque window identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct WindowId(pub u64);

/// Opaque frame identifier.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FrameId(pub u64);

/// Keep frame and window numeric domains disjoint while both are represented
/// as Lisp integers.
pub(crate) const FRAME_ID_BASE: u64 = 1 << 32;
/// Synthetic window-id domain reserved for per-frame minibuffer windows.
pub(crate) const MINIBUFFER_WINDOW_ID_BASE: u64 = 1 << 48;

// ---------------------------------------------------------------------------
// Window geometry
// ---------------------------------------------------------------------------

/// Pixel-based rectangle for window placement.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Rect {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

impl Rect {
    pub fn new(x: f32, y: f32, width: f32, height: f32) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }

    pub fn right(&self) -> f32 {
        self.x + self.width
    }

    pub fn bottom(&self) -> f32 {
        self.y + self.height
    }

    pub fn contains(&self, px: f32, py: f32) -> bool {
        px >= self.x && px < self.right() && py >= self.y && py < self.bottom()
    }
}

// ---------------------------------------------------------------------------
// Split direction
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SplitDirection {
    Horizontal, // side by side
    Vertical,   // stacked
}

// ---------------------------------------------------------------------------
// Window
// ---------------------------------------------------------------------------

/// A window in the window tree.
#[derive(Clone, Debug)]
pub enum Window {
    /// Leaf window displaying a buffer.
    Leaf {
        id: WindowId,
        buffer_id: BufferId,
        /// Pixel bounds within the frame.
        bounds: Rect,
        /// Character position of the first visible character.
        window_start: usize,
        /// Cursor (point) position in this window.
        point: usize,
        /// Whether this is a dedicated window.
        dedicated: bool,
        /// Window parameters (name -> value).
        parameters: HashMap<String, Value>,
        /// Desired height in lines (for fixed windows, 0 = flexible).
        fixed_height: usize,
        /// Desired width in columns (for fixed windows, 0 = flexible).
        fixed_width: usize,
        /// Horizontal scroll offset (columns).
        hscroll: usize,
        /// Window margins (left, right) in columns.
        margins: (usize, usize),
        /// Fringes (left_width, right_width) in pixels.
        fringes: (u32, u32),
    },

    /// Internal node: contains children split in a direction.
    Internal {
        id: WindowId,
        direction: SplitDirection,
        children: Vec<Window>,
        bounds: Rect,
    },
}

impl Window {
    /// Create a new leaf window.
    pub fn new_leaf(id: WindowId, buffer_id: BufferId, bounds: Rect) -> Self {
        Window::Leaf {
            id,
            buffer_id,
            bounds,
            window_start: 0,
            point: 0,
            dedicated: false,
            parameters: HashMap::new(),
            fixed_height: 0,
            fixed_width: 0,
            hscroll: 0,
            margins: (0, 0),
            fringes: (8, 8),
        }
    }

    /// Window ID.
    pub fn id(&self) -> WindowId {
        match self {
            Window::Leaf { id, .. } | Window::Internal { id, .. } => *id,
        }
    }

    /// Pixel bounds.
    pub fn bounds(&self) -> &Rect {
        match self {
            Window::Leaf { bounds, .. } | Window::Internal { bounds, .. } => bounds,
        }
    }

    /// Set bounds.
    pub fn set_bounds(&mut self, new_bounds: Rect) {
        match self {
            Window::Leaf { bounds, .. } | Window::Internal { bounds, .. } => {
                *bounds = new_bounds;
            }
        }
    }

    /// Whether this is a leaf window.
    pub fn is_leaf(&self) -> bool {
        matches!(self, Window::Leaf { .. })
    }

    /// Buffer displayed in this window (leaf only).
    pub fn buffer_id(&self) -> Option<BufferId> {
        match self {
            Window::Leaf { buffer_id, .. } => Some(*buffer_id),
            Window::Internal { .. } => None,
        }
    }

    /// Set the buffer displayed in this window (leaf only).
    pub fn set_buffer(&mut self, new_id: BufferId) {
        if let Window::Leaf {
            buffer_id,
            window_start,
            point,
            ..
        } = self
        {
            *buffer_id = new_id;
            // Emacs positions are 1-based; switching the displayed buffer resets
            // window-start/point to point-min.
            *window_start = 1;
            *point = 1;
        }
    }

    /// Replace a displayed buffer id in all leaf windows under this node.
    ///
    /// This is used when a buffer is killed; any window still attached to the
    /// dead buffer is moved back to a replacement buffer (typically `*scratch*`).
    pub fn replace_buffer_id(&mut self, old_id: BufferId, new_id: BufferId) {
        match self {
            Window::Leaf { buffer_id, .. } => {
                if *buffer_id == old_id {
                    self.set_buffer(new_id);
                }
            }
            Window::Internal { children, .. } => {
                for child in children {
                    child.replace_buffer_id(old_id, new_id);
                }
            }
        }
    }

    /// Find a leaf window by ID in this subtree.
    pub fn find(&self, target: WindowId) -> Option<&Window> {
        if self.id() == target {
            return Some(self);
        }
        if let Window::Internal { children, .. } = self {
            for child in children {
                if let Some(w) = child.find(target) {
                    return Some(w);
                }
            }
        }
        None
    }

    /// Find a mutable leaf window by ID in this subtree.
    pub fn find_mut(&mut self, target: WindowId) -> Option<&mut Window> {
        if self.id() == target {
            return Some(self);
        }
        if let Window::Internal { children, .. } = self {
            for child in children {
                if let Some(w) = child.find_mut(target) {
                    return Some(w);
                }
            }
        }
        None
    }

    /// Collect all leaf window IDs.
    pub fn leaf_ids(&self) -> Vec<WindowId> {
        let mut result = Vec::new();
        self.collect_leaves(&mut result);
        result
    }

    fn collect_leaves(&self, out: &mut Vec<WindowId>) {
        match self {
            Window::Leaf { id, .. } => out.push(*id),
            Window::Internal { children, .. } => {
                for child in children {
                    child.collect_leaves(out);
                }
            }
        }
    }

    /// Find the window at pixel coordinates.
    pub fn window_at(&self, px: f32, py: f32) -> Option<WindowId> {
        match self {
            Window::Leaf { id, bounds, .. } => {
                if bounds.contains(px, py) {
                    Some(*id)
                } else {
                    None
                }
            }
            Window::Internal {
                children, bounds, ..
            } => {
                if !bounds.contains(px, py) {
                    return None;
                }
                for child in children {
                    if let Some(id) = child.window_at(px, py) {
                        return Some(id);
                    }
                }
                None
            }
        }
    }

    /// Count leaf windows in this subtree.
    pub fn leaf_count(&self) -> usize {
        match self {
            Window::Leaf { .. } => 1,
            Window::Internal { children, .. } => children.iter().map(|c| c.leaf_count()).sum(),
        }
    }
}

// ---------------------------------------------------------------------------
// Frame
// ---------------------------------------------------------------------------

/// A frame (top-level window/screen).
pub struct Frame {
    pub id: FrameId,
    pub name: String,
    /// Root of the window tree.
    pub root_window: Window,
    /// The selected (active) window.
    pub selected_window: WindowId,
    /// Minibuffer window (always a leaf).
    pub minibuffer_window: Option<WindowId>,
    /// Storage for the minibuffer leaf, which is not part of the split tree.
    pub minibuffer_leaf: Option<Window>,
    /// Frame pixel dimensions.
    pub width: u32,
    pub height: u32,
    /// Frame parameters.
    pub parameters: HashMap<String, Value>,
    /// Whether the frame is visible.
    pub visible: bool,
    /// Frame title.
    pub title: String,
    /// Menu bar height in pixels.
    pub menu_bar_height: u32,
    /// Tool bar height in pixels.
    pub tool_bar_height: u32,
    /// Tab bar height in pixels.
    pub tab_bar_height: u32,
    /// Default font size in pixels.
    pub font_pixel_size: f32,
    /// Default character width.
    pub char_width: f32,
    /// Default character height.
    pub char_height: f32,
}

impl Frame {
    pub fn new(id: FrameId, name: String, width: u32, height: u32, root_window: Window) -> Self {
        let minibuffer_window = WindowId(MINIBUFFER_WINDOW_ID_BASE + id.0);
        let minibuffer_buffer_id = root_window.buffer_id().unwrap_or(BufferId(0));
        let mut minibuffer_leaf = Window::new_leaf(
            minibuffer_window,
            minibuffer_buffer_id,
            Rect::new(0.0, height as f32, width as f32, 16.0),
        );
        if let Window::Leaf {
            window_start, point, ..
        } = &mut minibuffer_leaf
        {
            *window_start = 1;
            *point = 1;
        }
        let selected = root_window
            .leaf_ids()
            .first()
            .copied()
            .unwrap_or(WindowId(0));
        Self {
            id,
            name,
            root_window,
            selected_window: selected,
            minibuffer_window: Some(minibuffer_window),
            minibuffer_leaf: Some(minibuffer_leaf),
            width,
            height,
            parameters: HashMap::new(),
            visible: true,
            title: String::new(),
            menu_bar_height: 0,
            tool_bar_height: 0,
            tab_bar_height: 0,
            font_pixel_size: 16.0,
            char_width: 8.0,
            char_height: 16.0,
        }
    }

    /// Get the selected window.
    pub fn selected_window(&self) -> Option<&Window> {
        self.root_window.find(self.selected_window)
    }

    /// Get a mutable reference to the selected window.
    pub fn selected_window_mut(&mut self) -> Option<&mut Window> {
        self.root_window.find_mut(self.selected_window)
    }

    /// Replace all leaf window buffer bindings for `old_id` with `new_id`.
    pub fn replace_buffer_bindings(&mut self, old_id: BufferId, new_id: BufferId) {
        self.root_window.replace_buffer_id(old_id, new_id);
        if let Some(minibuffer_leaf) = self.minibuffer_leaf.as_mut() {
            minibuffer_leaf.replace_buffer_id(old_id, new_id);
        }
    }

    /// Select a window by ID.
    pub fn select_window(&mut self, id: WindowId) -> bool {
        if self.root_window.find(id).is_some() {
            self.selected_window = id;
            true
        } else {
            false
        }
    }

    /// Find a window by ID.
    pub fn find_window(&self, id: WindowId) -> Option<&Window> {
        if let Some(window) = self.root_window.find(id) {
            return Some(window);
        }
        self.minibuffer_leaf.as_ref().and_then(|window| {
            if window.id() == id {
                Some(window)
            } else {
                None
            }
        })
    }

    /// Find a mutable window by ID.
    pub fn find_window_mut(&mut self, id: WindowId) -> Option<&mut Window> {
        if let Some(window) = self.root_window.find_mut(id) {
            return Some(window);
        }
        self.minibuffer_leaf.as_mut().and_then(|window| {
            if window.id() == id {
                Some(window)
            } else {
                None
            }
        })
    }

    /// All leaf window IDs.
    pub fn window_list(&self) -> Vec<WindowId> {
        self.root_window.leaf_ids()
    }

    /// Number of visible windows (leaves).
    pub fn window_count(&self) -> usize {
        self.root_window.leaf_count()
    }

    /// Find which window is at pixel coordinates.
    pub fn window_at(&self, px: f32, py: f32) -> Option<WindowId> {
        self.root_window.window_at(px, py)
    }

    /// Columns (based on default char width).
    pub fn columns(&self) -> u32 {
        (self.width as f32 / self.char_width) as u32
    }

    /// Lines (based on default char height).
    pub fn lines(&self) -> u32 {
        (self.height as f32 / self.char_height) as u32
    }
}

// ---------------------------------------------------------------------------
// FrameManager
// ---------------------------------------------------------------------------

/// Manages all frames and tracks the selected frame.
pub struct FrameManager {
    frames: HashMap<FrameId, Frame>,
    selected: Option<FrameId>,
    next_frame_id: u64,
    next_window_id: u64,
    deleted_windows: HashSet<WindowId>,
    window_parameters: HashMap<WindowId, Vec<(Value, Value)>>,
    window_display_tables: HashMap<WindowId, Value>,
}

impl FrameManager {
    pub fn new() -> Self {
        Self {
            frames: HashMap::new(),
            selected: None,
            next_frame_id: FRAME_ID_BASE,
            next_window_id: 1,
            deleted_windows: HashSet::new(),
            window_parameters: HashMap::new(),
            window_display_tables: HashMap::new(),
        }
    }

    /// Allocate a new window ID.
    pub fn next_window_id(&mut self) -> WindowId {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;
        self.deleted_windows.remove(&id);
        id
    }

    /// Create a new frame with a single window displaying `buffer_id`.
    pub fn create_frame(
        &mut self,
        name: &str,
        width: u32,
        height: u32,
        buffer_id: BufferId,
    ) -> FrameId {
        let frame_id = FrameId(self.next_frame_id);
        self.next_frame_id += 1;

        let window_id = self.next_window_id();
        let bounds = Rect::new(0.0, 0.0, width as f32, height as f32);
        let root = Window::new_leaf(window_id, buffer_id, bounds);

        let frame = Frame::new(frame_id, name.to_string(), width, height, root);
        self.frames.insert(frame_id, frame);

        if self.selected.is_none() {
            self.selected = Some(frame_id);
        }

        frame_id
    }

    /// Get a frame by ID.
    pub fn get(&self, id: FrameId) -> Option<&Frame> {
        self.frames.get(&id)
    }

    /// Get a mutable frame by ID.
    pub fn get_mut(&mut self, id: FrameId) -> Option<&mut Frame> {
        self.frames.get_mut(&id)
    }

    /// Get the selected frame.
    pub fn selected_frame(&self) -> Option<&Frame> {
        self.selected.and_then(|id| self.frames.get(&id))
    }

    /// Get a mutable reference to the selected frame.
    pub fn selected_frame_mut(&mut self) -> Option<&mut Frame> {
        self.selected.and_then(|id| self.frames.get_mut(&id))
    }

    /// Select a frame.
    pub fn select_frame(&mut self, id: FrameId) -> bool {
        if self.frames.contains_key(&id) {
            self.selected = Some(id);
            true
        } else {
            false
        }
    }

    /// Delete a frame.
    pub fn delete_frame(&mut self, id: FrameId) -> bool {
        if let Some(frame) = self.frames.remove(&id) {
            for wid in frame.window_list() {
                self.deleted_windows.insert(wid);
            }
            if let Some(minibuffer_wid) = frame.minibuffer_window {
                self.deleted_windows.insert(minibuffer_wid);
            }
            if self.selected == Some(id) {
                self.selected = self.frames.keys().next().copied();
            }
            true
        } else {
            false
        }
    }

    /// List all frame IDs.
    pub fn frame_list(&self) -> Vec<FrameId> {
        self.frames.keys().copied().collect()
    }

    /// Split a window horizontally or vertically.
    /// Returns the new window's ID, or None if the window wasn't found.
    pub fn split_window(
        &mut self,
        frame_id: FrameId,
        window_id: WindowId,
        direction: SplitDirection,
        new_buffer_id: BufferId,
    ) -> Option<WindowId> {
        let new_id = self.alloc_window_id();
        let frame = self.frames.get_mut(&frame_id)?;

        split_window_in_tree(
            &mut frame.root_window,
            window_id,
            direction,
            new_id,
            new_buffer_id,
        )?;

        Some(new_id)
    }

    /// Delete a window from a frame. Cannot delete the last window.
    pub fn delete_window(&mut self, frame_id: FrameId, window_id: WindowId) -> bool {
        let Some(frame) = self.frames.get_mut(&frame_id) else {
            return false;
        };
        if frame.root_window.leaf_count() <= 1 {
            return false; // Can't delete last window
        }

        let removed = delete_window_in_tree(&mut frame.root_window, window_id);
        if removed {
            self.deleted_windows.insert(window_id);
        }

        if removed && frame.selected_window == window_id {
            // Select the first remaining leaf.
            if let Some(first) = frame.root_window.leaf_ids().first() {
                frame.selected_window = *first;
            }
        }

        removed
    }

    fn alloc_window_id(&mut self) -> WindowId {
        let id = WindowId(self.next_window_id);
        self.next_window_id += 1;
        self.deleted_windows.remove(&id);
        id
    }

    /// Replace dead-buffer bindings in every live frame.
    pub fn replace_buffer_in_windows(&mut self, old_id: BufferId, new_id: BufferId) {
        for frame in self.frames.values_mut() {
            frame.replace_buffer_bindings(old_id, new_id);
        }
    }

    /// Return the frame containing a live window ID, if any.
    pub fn find_window_frame_id(&self, window_id: WindowId) -> Option<FrameId> {
        self.frames.iter().find_map(|(frame_id, frame)| {
            if frame.minibuffer_window == Some(window_id) {
                return Some(*frame_id);
            }
            frame.find_window(window_id).and_then(|window| {
                if window.is_leaf() {
                    Some(*frame_id)
                } else {
                    None
                }
            })
        })
    }

    /// Return true when WINDOW-ID designates a live window in any frame.
    pub fn is_live_window_id(&self, window_id: WindowId) -> bool {
        self.find_window_frame_id(window_id).is_some()
    }

    /// Return true when WINDOW-ID designates a live or stale window object.
    pub fn is_window_object_id(&self, window_id: WindowId) -> bool {
        self.is_live_window_id(window_id) || self.deleted_windows.contains(&window_id)
    }

    /// Return window parameter KEY for WINDOW-ID, or nil when unset.
    pub fn window_parameter(&self, window_id: WindowId, key: &Value) -> Option<Value> {
        self.window_parameters.get(&window_id).and_then(|pairs| {
            pairs
                .iter()
                .find(|(k, _)| eq_value(k, key))
                .map(|(_, v)| v.clone())
        })
    }

    /// Set window parameter KEY on WINDOW-ID to VALUE.
    pub fn set_window_parameter(&mut self, window_id: WindowId, key: Value, value: Value) {
        let params = self.window_parameters.entry(window_id).or_default();
        if let Some((_, existing)) = params.iter_mut().find(|(k, _)| eq_value(k, &key)) {
            *existing = value;
        } else {
            params.push((key, value));
        }
    }

    /// Return window parameters alist for WINDOW-ID.
    pub fn window_parameters_alist(&self, window_id: WindowId) -> Value {
        let Some(params) = self.window_parameters.get(&window_id) else {
            return Value::Nil;
        };
        if params.is_empty() {
            return Value::Nil;
        }
        let alist = params
            .iter()
            .rev()
            .map(|(k, v)| Value::cons(k.clone(), v.clone()))
            .collect::<Vec<_>>();
        Value::list(alist)
    }

    /// Return window display table object for WINDOW-ID, or nil when unset.
    pub fn window_display_table(&self, window_id: WindowId) -> Value {
        self.window_display_tables
            .get(&window_id)
            .cloned()
            .unwrap_or(Value::Nil)
    }

    /// Set window display table object for WINDOW-ID.
    pub fn set_window_display_table(&mut self, window_id: WindowId, table: Value) {
        if table.is_nil() {
            self.window_display_tables.remove(&window_id);
        } else {
            self.window_display_tables.insert(window_id, table);
        }
    }
}

impl Default for FrameManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tree manipulation helpers
// ---------------------------------------------------------------------------

/// Split a window in the tree by wrapping it in an Internal node.
fn split_window_in_tree(
    tree: &mut Window,
    target: WindowId,
    direction: SplitDirection,
    new_id: WindowId,
    new_buffer_id: BufferId,
) -> Option<()> {
    if tree.id() == target {
        // Extract what we need before mutating.
        let old_id = tree.id();
        let old_bounds = *tree.bounds();
        let old_buffer = tree.buffer_id();

        if let Some(buf_id) = old_buffer {
            let (left_bounds, right_bounds) = match direction {
                SplitDirection::Horizontal => {
                    let half = old_bounds.width / 2.0;
                    (
                        Rect::new(old_bounds.x, old_bounds.y, half, old_bounds.height),
                        Rect::new(
                            old_bounds.x + half,
                            old_bounds.y,
                            old_bounds.width - half,
                            old_bounds.height,
                        ),
                    )
                }
                SplitDirection::Vertical => {
                    let half = old_bounds.height / 2.0;
                    (
                        Rect::new(old_bounds.x, old_bounds.y, old_bounds.width, half),
                        Rect::new(
                            old_bounds.x,
                            old_bounds.y + half,
                            old_bounds.width,
                            old_bounds.height - half,
                        ),
                    )
                }
            };

            let old_leaf = Window::new_leaf(old_id, buf_id, left_bounds);
            let new_leaf = Window::new_leaf(new_id, new_buffer_id, right_bounds);

            *tree = Window::Internal {
                id: WindowId(target.0 + 10000), // synthetic internal ID
                direction,
                children: vec![old_leaf, new_leaf],
                bounds: old_bounds,
            };

            return Some(());
        }
    }

    // Recurse into children.
    if let Window::Internal { children, .. } = tree {
        for child in children {
            if split_window_in_tree(child, target, direction, new_id, new_buffer_id).is_some() {
                return Some(());
            }
        }
    }

    None
}

/// Delete a window from the tree. Returns true if found and removed.
fn delete_window_in_tree(tree: &mut Window, target: WindowId) -> bool {
    if let Window::Internal {
        children, bounds, ..
    } = tree
    {
        // Check if any direct child is the target.
        if let Some(idx) = children.iter().position(|c| c.id() == target) {
            children.remove(idx);

            // If only one child remains, replace this internal node with it.
            if children.len() == 1 {
                let mut remaining = children.pop().unwrap();
                remaining.set_bounds(*bounds);
                *tree = remaining;
            } else {
                // Redistribute space among remaining children.
                redistribute_bounds(children, *bounds);
            }
            return true;
        }

        // Recurse.
        for child in children {
            if delete_window_in_tree(child, target) {
                return true;
            }
        }
    }

    false
}

/// Redistribute bounds equally among children.
fn redistribute_bounds(children: &mut [Window], parent: Rect) {
    if children.is_empty() {
        return;
    }

    let n = children.len() as f32;

    // Detect direction from first two children if possible.
    if children.len() >= 2 {
        let first = children[0].bounds();
        let second = children[1].bounds();

        if (first.x - second.x).abs() > 0.1 {
            // Horizontal split
            let w = parent.width / n;
            for (i, child) in children.iter_mut().enumerate() {
                child.set_bounds(Rect::new(
                    parent.x + i as f32 * w,
                    parent.y,
                    w,
                    parent.height,
                ));
            }
        } else {
            // Vertical split
            let h = parent.height / n;
            for (i, child) in children.iter_mut().enumerate() {
                child.set_bounds(Rect::new(
                    parent.x,
                    parent.y + i as f32 * h,
                    parent.width,
                    h,
                ));
            }
        }
    } else {
        // Single child gets full bounds.
        children[0].set_bounds(parent);
    }
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_frame_and_window() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let frame = mgr.get(fid).unwrap();

        assert_eq!(frame.window_count(), 1);
        assert!(frame.selected_window().is_some());
        assert!(frame.selected_window().unwrap().is_leaf());
    }

    #[test]
    fn split_window_horizontal() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let wid = mgr.get(fid).unwrap().window_list()[0];

        let new_wid = mgr.split_window(fid, wid, SplitDirection::Horizontal, BufferId(2));
        assert!(new_wid.is_some());

        let frame = mgr.get(fid).unwrap();
        assert_eq!(frame.window_count(), 2);
    }

    #[test]
    fn split_window_vertical() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let wid = mgr.get(fid).unwrap().window_list()[0];

        let new_wid = mgr.split_window(fid, wid, SplitDirection::Vertical, BufferId(2));
        assert!(new_wid.is_some());

        let frame = mgr.get(fid).unwrap();
        assert_eq!(frame.window_count(), 2);
    }

    #[test]
    fn delete_window() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let wid = mgr.get(fid).unwrap().window_list()[0];

        // Split first.
        let new_wid = mgr
            .split_window(fid, wid, SplitDirection::Horizontal, BufferId(2))
            .unwrap();

        // Delete the new window.
        assert!(mgr.delete_window(fid, new_wid));
        assert_eq!(mgr.get(fid).unwrap().window_count(), 1);
    }

    #[test]
    fn cannot_delete_last_window() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let wid = mgr.get(fid).unwrap().window_list()[0];

        assert!(!mgr.delete_window(fid, wid));
    }

    #[test]
    fn select_window() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let wid = mgr.get(fid).unwrap().window_list()[0];

        let new_wid = mgr
            .split_window(fid, wid, SplitDirection::Horizontal, BufferId(2))
            .unwrap();

        assert!(mgr.get_mut(fid).unwrap().select_window(new_wid));
        assert_eq!(mgr.get(fid).unwrap().selected_window.0, new_wid.0,);
    }

    #[test]
    fn window_at_coordinates() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let wid = mgr.get(fid).unwrap().window_list()[0];

        mgr.split_window(fid, wid, SplitDirection::Horizontal, BufferId(2));

        let frame = mgr.get(fid).unwrap();
        // Left half
        let left = frame.window_at(100.0, 300.0);
        assert!(left.is_some());
        // Right half
        let right = frame.window_at(600.0, 300.0);
        assert!(right.is_some());
        // Should be different windows
        assert_ne!(left, right);
    }

    #[test]
    fn frame_columns_and_lines() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        let frame = mgr.get(fid).unwrap();

        assert_eq!(frame.columns(), 100); // 800/8
        assert_eq!(frame.lines(), 37); // 600/16 = 37
    }

    #[test]
    fn delete_frame() {
        let mut mgr = FrameManager::new();
        let fid = mgr.create_frame("F1", 800, 600, BufferId(1));
        assert!(mgr.delete_frame(fid));
        assert!(mgr.get(fid).is_none());
    }

    #[test]
    fn multiple_frames() {
        let mut mgr = FrameManager::new();
        let f1 = mgr.create_frame("F1", 800, 600, BufferId(1));
        let f2 = mgr.create_frame("F2", 1024, 768, BufferId(2));

        assert_eq!(mgr.frame_list().len(), 2);
        assert!(mgr.select_frame(f2));
        assert_eq!(mgr.selected_frame().unwrap().id, f2);

        mgr.delete_frame(f1);
        assert_eq!(mgr.frame_list().len(), 1);
    }

    #[test]
    fn rect_contains() {
        let r = Rect::new(10.0, 20.0, 100.0, 50.0);
        assert!(r.contains(10.0, 20.0));
        assert!(r.contains(50.0, 40.0));
        assert!(!r.contains(9.0, 20.0));
        assert!(!r.contains(110.0, 70.0));
    }
}
