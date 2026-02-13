use super::args::expect_max_args;
use super::{EvalResult, Value};

pub(crate) fn builtin_display_mouse_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-mouse-p", &args, 1)?;
    Ok(Value::True)
}

/// `(display-graphic-p &optional DISPLAY)` -> t
///
/// Return non-nil if DISPLAY is a graphic display.
/// Neomacs is always graphical, so returns t.
pub(crate) fn builtin_display_graphic_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-graphic-p", &args, 1)?;
    Ok(Value::True)
}

/// `(display-popup-menus-p &optional DISPLAY)` -> t
///
/// Return non-nil if popup menus are supported on DISPLAY.
/// Neomacs is graphical, so returns t.
pub(crate) fn builtin_display_popup_menus_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-popup-menus-p", &args, 1)?;
    Ok(Value::True)
}

/// `(display-selections-p &optional DISPLAY)` -> t
///
/// Return non-nil if selections (clipboard) are supported on DISPLAY.
/// Neomacs is graphical, so returns t.
pub(crate) fn builtin_display_selections_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-selections-p", &args, 1)?;
    Ok(Value::True)
}

/// `(display-screens &optional DISPLAY)` -> 1
///
/// Return the number of screens associated with DISPLAY.
/// Returns 1 as a sensible default.
pub(crate) fn builtin_display_screens(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-screens", &args, 1)?;
    Ok(Value::Int(1))
}

/// `(display-pixel-height &optional DISPLAY)` -> 1080
///
/// Return the height of DISPLAY in pixels.
/// Returns 1080 as a sensible default (1080p).
pub(crate) fn builtin_display_pixel_height(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-pixel-height", &args, 1)?;
    Ok(Value::Int(1080))
}

/// `(display-pixel-width &optional DISPLAY)` -> 1920
///
/// Return the width of DISPLAY in pixels.
/// Returns 1920 as a sensible default (1080p).
pub(crate) fn builtin_display_pixel_width(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-pixel-width", &args, 1)?;
    Ok(Value::Int(1920))
}

/// `(display-mm-height &optional DISPLAY)` -> 286
///
/// Return the height of DISPLAY in millimeters.
/// Returns 286 as a sensible default (~24" monitor).
pub(crate) fn builtin_display_mm_height(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-mm-height", &args, 1)?;
    Ok(Value::Int(286))
}

/// `(display-mm-width &optional DISPLAY)` -> 508
///
/// Return the width of DISPLAY in millimeters.
/// Returns 508 as a sensible default (~24" monitor).
pub(crate) fn builtin_display_mm_width(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-mm-width", &args, 1)?;
    Ok(Value::Int(508))
}

/// `(display-backing-store &optional DISPLAY)` -> 'always
///
/// Return an indication of whether DISPLAY uses a backing store.
/// Returns the symbol `always`.
pub(crate) fn builtin_display_backing_store(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-backing-store", &args, 1)?;
    Ok(Value::symbol("always"))
}

/// `(display-save-under &optional DISPLAY)` -> t
///
/// Return non-nil if DISPLAY supports the SaveUnder feature.
/// Returns t as a sensible default for a modern graphical display.
pub(crate) fn builtin_display_save_under(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-save-under", &args, 1)?;
    Ok(Value::True)
}

/// `(display-planes &optional DISPLAY)` -> 24
///
/// Return the number of bitplanes of DISPLAY.
/// Returns 24 (24-bit color depth).
pub(crate) fn builtin_display_planes(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-planes", &args, 1)?;
    Ok(Value::Int(24))
}

/// `(display-color-cells &optional DISPLAY)` -> 16777216
///
/// Return the number of color cells of DISPLAY.
/// Returns 16777216 (2^24, 24-bit true color).
pub(crate) fn builtin_display_color_cells(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-color-cells", &args, 1)?;
    Ok(Value::Int(16777216))
}

/// `(display-visual-class &optional DISPLAY)` -> 'true-color
///
/// Return the visual class of DISPLAY.
/// Returns the symbol `true-color` for a modern graphical display.
pub(crate) fn builtin_display_visual_class(args: Vec<Value>) -> EvalResult {
    expect_max_args("display-visual-class", &args, 1)?;
    Ok(Value::symbol("true-color"))
}
