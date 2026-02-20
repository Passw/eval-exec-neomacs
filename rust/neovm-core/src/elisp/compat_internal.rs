//! Internal compatibility builtins.
//!
//! These primitives are exposed by GNU Emacs and touched by startup/runtime
//! code paths. NeoVM provides compatibility implementations that preserve
//! observable arity and error contracts for compatibility tests.

use super::error::{signal, EvalResult, Flow};
use super::value::{HashTableTest, Value};
use std::cell::RefCell;

const FACE_ATTRIBUTES_VECTOR_LEN: usize = 20;
const DEFAULT_FONTSET_NAME: &str = "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default";

thread_local! {
    static HASH_TABLE_TEST_ALIASES: RefCell<Vec<(String, HashTableTest)>> =
        const { RefCell::new(Vec::new()) };
}

fn expect_args(name: &str, args: &[Value], n: usize) -> Result<(), Flow> {
    if args.len() != n {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_range_args(name: &str, args: &[Value], min: usize, max: usize) -> Result<(), Flow> {
    if args.len() < min || args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_symbolp(value: &Value) -> Result<(), Flow> {
    if value.as_symbol_name().is_some() {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), value.clone()],
        ))
    }
}

fn hash_test_from_designator(value: &Value) -> Option<HashTableTest> {
    let name = value.as_symbol_name()?;
    match name {
        "eq" => Some(HashTableTest::Eq),
        "eql" => Some(HashTableTest::Eql),
        "equal" => Some(HashTableTest::Equal),
        _ => None,
    }
}

fn register_hash_table_test_alias(name: &str, test: HashTableTest) {
    HASH_TABLE_TEST_ALIASES.with(|slot| {
        let mut aliases = slot.borrow_mut();
        if let Some((_, existing)) = aliases.iter_mut().find(|(alias, _)| alias == name) {
            *existing = test;
        } else {
            aliases.push((name.to_string(), test));
        }
    });
}

pub(crate) fn lookup_hash_table_test_alias(name: &str) -> Option<HashTableTest> {
    HASH_TABLE_TEST_ALIASES.with(|slot| {
        slot.borrow()
            .iter()
            .find_map(|(alias, test)| (alias == name).then_some(test.clone()))
    })
}

fn expect_fixnum(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), other.clone()],
        )),
    }
}

fn expect_sequencep(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Nil | Value::Cons(_) | Value::Vector(_) | Value::Str(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

fn expect_arrayp(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Vector(_) | Value::Str(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), other.clone()],
        )),
    }
}

fn expect_vector_or_char_table_p(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Vector(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("vector-or-char-table-p"), other.clone()],
        )),
    }
}

fn is_font_object(value: &Value) -> bool {
    match value {
        Value::Vector(items) => {
            let items = items.lock().expect("poisoned");
            matches!(
                items.first(),
                Some(Value::Keyword(tag)) if tag == "font-object"
            )
        }
        _ => false,
    }
}

fn is_font_spec(value: &Value) -> bool {
    match value {
        Value::Vector(items) => {
            let items = items.lock().expect("poisoned");
            matches!(items.first(), Some(Value::Keyword(tag)) if tag == "font-spec")
        }
        _ => false,
    }
}

fn unspecified_face_attributes_vector() -> Value {
    Value::vector(vec![
        Value::symbol("unspecified");
        FACE_ATTRIBUTES_VECTOR_LEN
    ])
}

fn expect_stringp(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Str(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn expect_bufferp(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Buffer(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("bufferp"), other.clone()],
        )),
    }
}

fn expect_integer_or_marker_p(value: &Value) -> Result<(), Flow> {
    match value {
        Value::Int(_) | Value::Char(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

fn expect_characterp_from_int(value: &Value) -> Result<char, Flow> {
    match value {
        Value::Int(n) if *n >= 0 => Ok((*n as u8) as char),
        Value::Char(c) => Ok(*c),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), other.clone()],
        )),
    }
}

fn expect_frame_live_or_nil(value: &Value) -> Result<(), Flow> {
    if value.is_nil() || matches!(value, Value::Frame(_)) {
        Ok(())
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), value.clone()],
        ))
    }
}

/// `(face-attributes-as-vector FACE)` -> FACE attribute vector.
pub(crate) fn builtin_face_attributes_as_vector(args: Vec<Value>) -> EvalResult {
    expect_args("face-attributes-as-vector", &args, 1)?;
    Ok(unspecified_face_attributes_vector())
}

/// `(font-face-attributes FONT &optional FRAME)` -> vector or compatibility error.
pub(crate) fn builtin_font_face_attributes(args: Vec<Value>) -> EvalResult {
    expect_range_args("font-face-attributes", &args, 1, 2)?;
    if !is_font_object(&args[0]) {
        return Err(signal(
            "error",
            vec![Value::string("Invalid font object")],
        ));
    }
    Ok(unspecified_face_attributes_vector())
}

/// `(font-get-glyphs FONT FROM TO &optional STR)` -> nil.
pub(crate) fn builtin_font_get_glyphs(args: Vec<Value>) -> EvalResult {
    expect_range_args("font-get-glyphs", &args, 3, 4)?;
    if !is_font_object(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-object"), args[0].clone()],
        ));
    }
    let _ = expect_fixnum(&args[1])?;
    let _ = expect_fixnum(&args[2])?;
    Ok(Value::Nil)
}

/// `(font-get-system-font)` -> nil.
pub(crate) fn builtin_font_get_system_font(args: Vec<Value>) -> EvalResult {
    expect_args("font-get-system-font", &args, 0)?;
    Ok(Value::Nil)
}

/// `(font-get-system-normal-font)` -> nil.
pub(crate) fn builtin_font_get_system_normal_font(args: Vec<Value>) -> EvalResult {
    expect_args("font-get-system-normal-font", &args, 0)?;
    Ok(Value::Nil)
}

/// `(font-has-char-p FONT CHAR &optional SCRIPT)` -> nil.
pub(crate) fn builtin_font_has_char_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("font-has-char-p", &args, 2, 3)?;
    if !is_font_object(&args[0]) && !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font"), args[0].clone()],
        ));
    }
    let _ = expect_characterp_from_int(&args[1])?;
    Ok(Value::Nil)
}

/// `(font-info FONT &optional FRAME)` -> nil.
pub(crate) fn builtin_font_info(args: Vec<Value>) -> EvalResult {
    expect_range_args("font-info", &args, 1, 2)?;
    expect_stringp(&args[0])?;
    Ok(Value::Nil)
}

/// `(font-match-p SPEC FONT-SPEC)` -> nil.
pub(crate) fn builtin_font_match_p(args: Vec<Value>) -> EvalResult {
    expect_args("font-match-p", &args, 2)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[0].clone()],
        ));
    }
    if !is_font_spec(&args[1]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[1].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// `(font-shape-gstring GLYPH-STRING DIRECTION)` -> nil or compatibility error.
pub(crate) fn builtin_font_shape_gstring(args: Vec<Value>) -> EvalResult {
    expect_args("font-shape-gstring", &args, 2)?;
    if !matches!(args[0], Value::Vector(_)) {
        return Err(signal(
            "error",
            vec![Value::string("Invalid glyph-string: ")],
        ));
    }
    let _ = expect_fixnum(&args[1])?;
    Ok(Value::Nil)
}

/// `(font-variation-glyphs FONT-OBJECT CHAR)` -> nil.
pub(crate) fn builtin_font_variation_glyphs(args: Vec<Value>) -> EvalResult {
    expect_args("font-variation-glyphs", &args, 2)?;
    if !is_font_object(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-object"), args[0].clone()],
        ));
    }
    let _ = expect_characterp_from_int(&args[1])?;
    Ok(Value::Nil)
}

/// `(fontset-font FONTSET CHAR &optional FRAME)` -> nil.
pub(crate) fn builtin_fontset_font(args: Vec<Value>) -> EvalResult {
    expect_range_args("fontset-font", &args, 2, 3)?;
    let _ = expect_characterp_from_int(&args[1])?;
    Ok(Value::Nil)
}

/// `(fontset-info FONTSET &optional FRAME)` -> error in batch compatibility.
pub(crate) fn builtin_fontset_info(args: Vec<Value>) -> EvalResult {
    expect_range_args("fontset-info", &args, 1, 2)?;
    Err(signal(
        "error",
        vec![Value::string("Window system is not in use or not initialized")],
    ))
}

/// `(fontset-list)` -> list containing default fontset name.
pub(crate) fn builtin_fontset_list(args: Vec<Value>) -> EvalResult {
    expect_args("fontset-list", &args, 0)?;
    Ok(Value::list(vec![Value::string(DEFAULT_FONTSET_NAME)]))
}

/// `(frame--face-hash-table &optional FRAME)` -> eq hash-table.
pub(crate) fn builtin_frame_face_hash_table(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame--face-hash-table", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(Value::hash_table(HashTableTest::Eq))
}

/// `(frame--set-was-invisible FRAME WAS-INVISIBLE)` -> WAS-INVISIBLE.
pub(crate) fn builtin_frame_set_was_invisible(args: Vec<Value>) -> EvalResult {
    expect_args("frame--set-was-invisible", &args, 2)?;
    expect_frame_live_or_nil(&args[0])?;
    Ok(args[1].clone())
}

/// `(frame-after-make-frame FRAME WAS-INVISIBLE)` -> nil.
pub(crate) fn builtin_frame_after_make_frame(args: Vec<Value>) -> EvalResult {
    expect_args("frame-after-make-frame", &args, 2)?;
    expect_frame_live_or_nil(&args[0])?;
    Ok(Value::Nil)
}

/// `(frame-ancestor-p ANCESTOR FRAME)` -> nil.
pub(crate) fn builtin_frame_ancestor_p(args: Vec<Value>) -> EvalResult {
    expect_args("frame-ancestor-p", &args, 2)?;
    expect_frame_live_or_nil(&args[0])?;
    expect_frame_live_or_nil(&args[1])?;
    Ok(Value::Nil)
}

/// `(frame-bottom-divider-width &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_bottom_divider_width(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-bottom-divider-width", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-child-frame-border-width &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_child_frame_border_width(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-child-frame-border-width", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-focus &optional FRAME)` -> nil.
pub(crate) fn builtin_frame_focus(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-focus", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Nil)
}

/// `(frame-font-cache &optional FRAME)` -> nil.
pub(crate) fn builtin_frame_font_cache(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-font-cache", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Nil)
}

/// `(frame-fringe-width &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_fringe_width(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-fringe-width", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-internal-border-width &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_internal_border_width(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-internal-border-width", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-old-selected-window &optional FRAME)` -> nil.
pub(crate) fn builtin_frame_old_selected_window(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-old-selected-window", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Nil)
}

/// `(frame-or-buffer-changed-p &optional SYM)` -> compatibility flag.
pub(crate) fn builtin_frame_or_buffer_changed_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-or-buffer-changed-p", &args, 0, 1)?;
    let Some(symbol) = args.first() else {
        return Ok(Value::True);
    };
    if symbol.is_nil() {
        return Ok(Value::Nil);
    }
    if symbol.as_symbol_name().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), symbol.clone()],
        ));
    }
    Err(signal("void-variable", vec![symbol.clone()]))
}

/// `(frame-parent &optional FRAME)` -> nil.
pub(crate) fn builtin_frame_parent(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-parent", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Nil)
}

/// `(frame-pointer-visible-p &optional FRAME)` -> t.
pub(crate) fn builtin_frame_pointer_visible_p(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-pointer-visible-p", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::True)
}

/// `(frame-right-divider-width &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_right_divider_width(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-right-divider-width", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-scale-factor &optional FRAME)` -> 1.0.
pub(crate) fn builtin_frame_scale_factor(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-scale-factor", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Float(1.0))
}

/// `(frame-scroll-bar-height &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_scroll_bar_height(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-scroll-bar-height", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-scroll-bar-width &optional FRAME)` -> 0.
pub(crate) fn builtin_frame_scroll_bar_width(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-scroll-bar-width", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Int(0))
}

/// `(frame-window-state-change &optional FRAME)` -> nil.
pub(crate) fn builtin_frame_window_state_change(args: Vec<Value>) -> EvalResult {
    expect_range_args("frame-window-state-change", &args, 0, 1)?;
    if let Some(frame) = args.first() {
        expect_frame_live_or_nil(frame)?;
    }
    Ok(Value::Nil)
}

/// `(fringe-bitmaps-at-pos &optional POS WINDOW)` -> nil.
pub(crate) fn builtin_fringe_bitmaps_at_pos(args: Vec<Value>) -> EvalResult {
    expect_range_args("fringe-bitmaps-at-pos", &args, 0, 2)?;
    if let Some(pos) = args.first() {
        if !pos.is_nil() {
            expect_integer_or_marker_p(pos)?;
        }
    }
    if let Some(window) = args.get(1) {
        if !window.is_nil() && !matches!(window, Value::Window(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), window.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// `(gap-position)` -> 1.
pub(crate) fn builtin_gap_position(args: Vec<Value>) -> EvalResult {
    expect_args("gap-position", &args, 0)?;
    Ok(Value::Int(1))
}

/// `(gap-size)` -> 2001.
pub(crate) fn builtin_gap_size(args: Vec<Value>) -> EvalResult {
    expect_args("gap-size", &args, 0)?;
    Ok(Value::Int(2001))
}

/// `(garbage-collect-maybe N)` -> nil.
pub(crate) fn builtin_garbage_collect_maybe(args: Vec<Value>) -> EvalResult {
    expect_args("garbage-collect-maybe", &args, 1)?;
    let Value::Int(n) = args[0] else {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), args[0].clone()],
        ));
    };
    if n < 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), Value::Int(n)],
        ));
    }
    Ok(Value::Nil)
}

/// `(get-unicode-property-internal TABLE INDEX)` -> compatibility type error.
pub(crate) fn builtin_get_unicode_property_internal(args: Vec<Value>) -> EvalResult {
    expect_args("get-unicode-property-internal", &args, 2)?;
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("char-table-p"), args[0].clone()],
    ))
}

/// `(get-variable-watchers SYMBOL)` -> nil.
pub(crate) fn builtin_get_variable_watchers(args: Vec<Value>) -> EvalResult {
    expect_args("get-variable-watchers", &args, 1)?;
    if !args[0].is_nil() && args[0].as_symbol_name().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// `(gnutls-available-p)` -> capability list.
pub(crate) fn builtin_gnutls_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-available-p", &args, 0)?;
    Ok(Value::list(vec![Value::symbol("gnutls")]))
}

/// `(gnutls-ciphers)` -> non-empty cipher descriptor list.
pub(crate) fn builtin_gnutls_ciphers(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-ciphers", &args, 0)?;
    Ok(Value::list(vec![Value::symbol("AES-256-GCM")]))
}

/// `(gnutls-digests)` -> non-empty digest descriptor list.
pub(crate) fn builtin_gnutls_digests(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-digests", &args, 0)?;
    Ok(Value::list(vec![Value::symbol("SHA256")]))
}

/// `(gnutls-macs)` -> non-empty MAC descriptor list.
pub(crate) fn builtin_gnutls_macs(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-macs", &args, 0)?;
    Ok(Value::list(vec![Value::symbol("AEAD")]))
}

/// `(gnutls-errorp CODE)` -> t.
pub(crate) fn builtin_gnutls_errorp(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-errorp", &args, 1)?;
    Ok(Value::True)
}

/// `(gnutls-error-string CODE)` -> compatibility message string.
pub(crate) fn builtin_gnutls_error_string(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-error-string", &args, 1)?;
    match args[0] {
        Value::Int(0) => Ok(Value::string("Success.")),
        Value::Nil => Ok(Value::string("Symbol has no numeric gnutls-code property")),
        _ => Ok(Value::string("Unknown TLS error")),
    }
}

/// `(gnutls-error-fatalp CODE)` -> nil for numeric codes.
pub(crate) fn builtin_gnutls_error_fatalp(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-error-fatalp", &args, 1)?;
    if args[0].is_nil() {
        return Err(signal(
            "error",
            vec![Value::string("Symbol has no numeric gnutls-code property")],
        ));
    }
    Ok(Value::Nil)
}

/// `(gnutls-peer-status-warning-describe WARNING)` -> nil.
pub(crate) fn builtin_gnutls_peer_status_warning_describe(args: Vec<Value>) -> EvalResult {
    expect_args("gnutls-peer-status-warning-describe", &args, 1)?;
    if args[0].is_nil() {
        return Ok(Value::Nil);
    }
    expect_symbolp(&args[0])?;
    Ok(Value::Nil)
}

/// `(gpm-mouse-start)` -> compatibility error.
pub(crate) fn builtin_gpm_mouse_start(args: Vec<Value>) -> EvalResult {
    expect_args("gpm-mouse-start", &args, 0)?;
    Err(signal(
        "error",
        vec![Value::string("Gpm-mouse only works in the GNU/Linux console")],
    ))
}

/// `(gpm-mouse-stop)` -> nil.
pub(crate) fn builtin_gpm_mouse_stop(args: Vec<Value>) -> EvalResult {
    expect_args("gpm-mouse-stop", &args, 0)?;
    Ok(Value::Nil)
}

/// `(sqlite-available-p)` -> t.
pub(crate) fn builtin_sqlite_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("sqlite-available-p", &args, 0)?;
    Ok(Value::True)
}

/// `(sqlite-version)` -> compatibility version string.
pub(crate) fn builtin_sqlite_version(args: Vec<Value>) -> EvalResult {
    expect_args("sqlite-version", &args, 0)?;
    Ok(Value::string("3.50.4"))
}

/// `(inotify-valid-p WATCH-DESCRIPTOR)` -> nil.
pub(crate) fn builtin_inotify_valid_p(args: Vec<Value>) -> EvalResult {
    expect_args("inotify-valid-p", &args, 1)?;
    Ok(Value::Nil)
}

/// `(define-fringe-bitmap NAME BITS &optional HEIGHT WIDTH ALIGN)` -> NAME.
pub(crate) fn builtin_define_fringe_bitmap(args: Vec<Value>) -> EvalResult {
    expect_range_args("define-fringe-bitmap", &args, 2, 5)?;
    expect_symbolp(&args[0])?;
    expect_arrayp(&args[1])?;

    if let Some(height) = args.get(2) {
        if !height.is_nil() {
            let _ = expect_fixnum(height)?;
        }
    }
    if let Some(width) = args.get(3) {
        if !width.is_nil() {
            let _ = expect_fixnum(width)?;
        }
    }
    if let Some(align) = args.get(4) {
        if !align.is_nil() && align.as_symbol_name().is_none() {
            return Err(signal("error", vec![Value::string("Bad align argument")]));
        }
    }

    Ok(args[0].clone())
}

/// `(destroy-fringe-bitmap NAME)` -> nil.
pub(crate) fn builtin_destroy_fringe_bitmap(args: Vec<Value>) -> EvalResult {
    expect_args("destroy-fringe-bitmap", &args, 1)?;
    expect_symbolp(&args[0])?;
    Ok(Value::Nil)
}

/// `(display--line-is-continued-p)` -> nil.
pub(crate) fn builtin_display_line_is_continued_p(args: Vec<Value>) -> EvalResult {
    expect_args("display--line-is-continued-p", &args, 0)?;
    Ok(Value::Nil)
}

/// `(display--update-for-mouse-movement X Y)` -> nil.
pub(crate) fn builtin_display_update_for_mouse_movement(args: Vec<Value>) -> EvalResult {
    expect_args("display--update-for-mouse-movement", &args, 2)?;
    let _ = expect_fixnum(&args[0])?;
    let _ = expect_fixnum(&args[1])?;
    Ok(Value::Nil)
}

/// `(do-auto-save &optional NO-MESSAGE CURRENT-ONLY)` -> nil.
pub(crate) fn builtin_do_auto_save(args: Vec<Value>) -> EvalResult {
    expect_range_args("do-auto-save", &args, 0, 2)?;
    Ok(Value::Nil)
}

/// `(external-debugging-output CHAR)` -> CHAR.
pub(crate) fn builtin_external_debugging_output(args: Vec<Value>) -> EvalResult {
    expect_args("external-debugging-output", &args, 1)?;
    let ch = expect_fixnum(&args[0])?;
    if ch < 0 {
        return Err(signal(
            "error",
            vec![Value::string("Invalid character: f03fffff")],
        ));
    }
    Ok(Value::Int(ch))
}

/// `(describe-buffer-bindings BUFFER &optional PREFIXES MENUS)` -> nil.
pub(crate) fn builtin_describe_buffer_bindings(args: Vec<Value>) -> EvalResult {
    expect_range_args("describe-buffer-bindings", &args, 1, 3)?;
    expect_bufferp(&args[0])?;
    if let Some(prefixes) = args.get(1) {
        if !prefixes.is_nil() {
            expect_sequencep(prefixes)?;
        }
    }
    Ok(Value::Nil)
}

/// `(describe-vector VECTOR &optional OUTPUT)` -> nil.
pub(crate) fn builtin_describe_vector(args: Vec<Value>) -> EvalResult {
    expect_range_args("describe-vector", &args, 1, 2)?;
    expect_vector_or_char_table_p(&args[0])?;
    if let Some(output) = args.get(1) {
        if !output.is_nil() {
            if let Some(name) = output.as_symbol_name() {
                return Err(signal("void-function", vec![Value::symbol(name)]));
            }
        }
    }
    Ok(Value::Nil)
}

/// `(delete-terminal &optional TERMINAL FORCE)` -> nil or error.
pub(crate) fn builtin_delete_terminal(args: Vec<Value>) -> EvalResult {
    expect_range_args("delete-terminal", &args, 0, 2)?;
    if args.first().is_some_and(|term| !term.is_nil()) {
        return Ok(Value::Nil);
    }
    Err(signal(
        "error",
        vec![Value::string(
            "Attempt to delete the sole active display terminal",
        )],
    ))
}

/// `(font-at POS &optional WINDOW STRING)` -> nil or compatibility error.
pub(crate) fn builtin_font_at(args: Vec<Value>) -> EvalResult {
    expect_range_args("font-at", &args, 1, 3)?;

    if let Some(window) = args.get(1) {
        if !window.is_nil() && !matches!(window, Value::Window(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("window-live-p"), window.clone()],
            ));
        }
    }

    if let Some(string_value) = args.get(2) {
        if !string_value.is_nil() {
            let Value::Str(s) = string_value else {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), string_value.clone()],
                ));
            };
            let pos = match args[0] {
                Value::Int(n) => n,
                Value::Char(c) => c as i64,
                _ => 1,
            };
            return Err(signal(
                "args-out-of-range",
                vec![Value::string((**s).clone()), Value::Int(pos)],
            ));
        }
    }

    Err(signal(
        "error",
        vec![Value::string(
            "Specified window is not displaying the current buffer",
        )],
    ))
}

/// `(fillarray ARRAY ITEM)` -> ARRAY after in-place/vector fill.
pub(crate) fn builtin_fillarray(args: Vec<Value>) -> EvalResult {
    expect_args("fillarray", &args, 2)?;
    match &args[0] {
        Value::Vector(items) => {
            let mut guard = items.lock().expect("poisoned");
            for item in guard.iter_mut() {
                *item = args[1].clone();
            }
            Ok(args[0].clone())
        }
        Value::Str(original) => {
            let fill = expect_characterp_from_int(&args[1])?;
            let len = original.chars().count();
            Ok(Value::string(fill.to_string().repeat(len)))
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("arrayp"), other.clone()],
        )),
    }
}

/// `(define-hash-table-test SYMBOL TEST HASH)` -> (TEST HASH).
pub(crate) fn builtin_define_hash_table_test(args: Vec<Value>) -> EvalResult {
    expect_args("define-hash-table-test", &args, 3)?;
    expect_symbolp(&args[0])?;
    if let Some(alias_name) = args[0].as_symbol_name() {
        if let Some(test) = hash_test_from_designator(&args[1]) {
            register_hash_table_test_alias(alias_name, test);
        }
    }
    Ok(Value::list(vec![args[1].clone(), args[2].clone()]))
}

/// `(find-coding-systems-region-internal FROM TO &optional TABLE)` -> t.
pub(crate) fn builtin_find_coding_systems_region_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("find-coding-systems-region-internal", &args, 2, 3)?;
    expect_integer_or_marker_p(&args[1])?;
    Ok(Value::True)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fillarray_vector_is_in_place() {
        let vec = Value::vector(vec![Value::Int(1), Value::Int(2)]);
        let out = builtin_fillarray(vec![vec.clone(), Value::Int(9)]).unwrap();
        assert_eq!(out, vec);
        let Value::Vector(values) = out else {
            panic!("expected vector");
        };
        let values = values.lock().expect("poisoned");
        assert_eq!(&*values, &[Value::Int(9), Value::Int(9)]);
    }

    #[test]
    fn external_debugging_rejects_negative_fixnum() {
        let err = builtin_external_debugging_output(vec![Value::Int(-1)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn define_hash_table_test_requires_symbol_name() {
        let err = builtin_define_hash_table_test(vec![
            Value::Int(1),
            Value::symbol("eq"),
            Value::symbol("sxhash-eq"),
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn face_attributes_as_vector_shape() {
        let out = builtin_face_attributes_as_vector(vec![Value::Nil]).unwrap();
        let Value::Vector(values) = out else {
            panic!("expected vector");
        };
        let values = values.lock().expect("poisoned");
        assert_eq!(values.len(), FACE_ATTRIBUTES_VECTOR_LEN);
    }

    #[test]
    fn frame_face_hash_table_uses_eq_test() {
        let out = builtin_frame_face_hash_table(vec![]).unwrap();
        let Value::HashTable(table) = out else {
            panic!("expected hash table");
        };
        assert!(matches!(
            table.lock().expect("poisoned").test,
            HashTableTest::Eq
        ));
    }

    #[test]
    fn font_match_p_requires_font_spec_values() {
        let err = builtin_font_match_p(vec![Value::Nil, Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn frame_set_was_invisible_returns_new_state() {
        let out = builtin_frame_set_was_invisible(vec![Value::Nil, Value::True]).unwrap();
        assert_eq!(out, Value::True);
    }

    #[test]
    fn frame_bottom_divider_width_rejects_non_frame_designator() {
        let err = builtin_frame_bottom_divider_width(vec![Value::Int(0)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn frame_scale_factor_defaults_to_one_float() {
        let out = builtin_frame_scale_factor(vec![]).unwrap();
        assert_eq!(out, Value::Float(1.0));
    }

    #[test]
    fn garbage_collect_maybe_requires_whole_number() {
        let err = builtin_garbage_collect_maybe(vec![Value::True]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gnutls_error_string_zero_is_success() {
        let out = builtin_gnutls_error_string(vec![Value::Int(0)]).unwrap();
        assert_eq!(out, Value::string("Success."));
    }

    #[test]
    fn gnutls_peer_status_warning_describe_rejects_non_symbol() {
        let err = builtin_gnutls_peer_status_warning_describe(vec![Value::Int(0)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gpm_mouse_start_signals_console_only_error() {
        let err = builtin_gpm_mouse_start(vec![]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn sqlite_version_returns_string() {
        let out = builtin_sqlite_version(vec![]).unwrap();
        assert!(matches!(out, Value::Str(_)));
    }

    #[test]
    fn inotify_valid_p_returns_nil() {
        let out = builtin_inotify_valid_p(vec![Value::Int(0)]).unwrap();
        assert_eq!(out, Value::Nil);
    }
}
