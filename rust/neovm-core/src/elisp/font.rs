//! Font and face builtins for the Elisp interpreter.
//!
//! Font builtins:
//! - `fontp`, `font-spec`, `font-get`, `font-put`, `list-fonts`, `find-font`,
//!   `clear-font-cache`, `font-family-list`, `font-xlfd-name`
//!
//! Face builtins:
//! - `internal-make-lisp-face`, `internal-lisp-face-p`, `internal-copy-lisp-face`,
//!   `internal-set-lisp-face-attribute`, `internal-get-lisp-face-attribute`,
//!   `internal-merge-in-global-face`, `face-attribute-relative-p`,
//!   `merge-face-attribute`, `face-list`, `color-defined-p`, `color-values`,
//!   `defined-colors`, `face-id`, `face-font`, `internal-face-x-get-resource`,
//!   `internal-set-font-selection-order`,
//!   `internal-set-alternative-font-family-alist`,
//!   `internal-set-alternative-font-registry-alist`

use std::collections::{HashMap, HashSet};
use std::sync::{Mutex, OnceLock};

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use crate::window::{FrameId, FRAME_ID_BASE};

// ---------------------------------------------------------------------------
// Argument helpers (local to this module)
// ---------------------------------------------------------------------------

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

fn expect_min_args(name: &str, args: &[Value], min: usize) -> Result<(), Flow> {
    if args.len() < min {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn expect_max_args(name: &str, args: &[Value], max: usize) -> Result<(), Flow> {
    if args.len() > max {
        Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol(name), Value::Int(args.len() as i64)],
        ))
    } else {
        Ok(())
    }
}

fn live_frame_designator_p(eval: &mut super::eval::Evaluator, value: &Value) -> bool {
    match value {
        Value::Int(id) if *id >= 0 => eval.frames.get(FrameId(*id as u64)).is_some(),
        Value::Frame(id) => eval.frames.get(FrameId(*id)).is_some(),
        _ => false,
    }
}

fn expect_optional_frame_designator_eval(
    eval: &mut super::eval::Evaluator,
    value: Option<&Value>,
) -> Result<(), Flow> {
    if let Some(frame) = value {
        if !frame.is_nil() && !live_frame_designator_p(eval, frame) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(())
}

fn frame_device_designator_p(value: &Value) -> bool {
    match value {
        Value::Int(id) => *id >= FRAME_ID_BASE as i64,
        Value::Frame(id) => *id >= FRAME_ID_BASE,
        _ => false,
    }
}

fn optional_selected_frame_designator_p(value: &Value) -> bool {
    value.is_nil() || frame_device_designator_p(value)
}

// ---------------------------------------------------------------------------
// Font-spec helpers
// ---------------------------------------------------------------------------

/// The tag keyword used to identify font-spec vectors: `:font-spec`.
const FONT_SPEC_TAG: &str = "font-spec";

/// Check whether a Value is a font-spec (a vector whose first element is
/// the keyword `:font-spec`).
fn is_font_spec(val: &Value) -> bool {
    match val {
        Value::Vector(v) => {
            let elems = with_heap(|h| h.get_vector(*v).clone());
            if elems.is_empty() {
                return false;
            }
            matches!(&elems[0], Value::Keyword(k) if k == FONT_SPEC_TAG)
        }
        _ => false,
    }
}

/// Check whether a value is represented as a font-object vector.
fn is_font_object(val: &Value) -> bool {
    match val {
        Value::Vector(v) => {
            let elems = with_heap(|h| h.get_vector(*v).clone());
            matches!(&elems.first(), Some(Value::Keyword(tag)) if tag == "font-object")
        }
        _ => false,
    }
}

/// Extract a property from a font-spec vector.
///
/// Property lookup is strict: keys only match if they are exactly equal to
/// `prop` (keyword vs symbol distinction is preserved).
fn font_spec_get(vec_elems: &[Value], prop: &Value) -> Value {
    // Skip the tag at index 0; scan remaining pairs.
    let mut i = 1;
    while i + 1 < vec_elems.len() {
        if vec_elems[i] == *prop {
            return vec_elems[i + 1].clone();
        }
        i += 2;
    }
    Value::Nil
}

/// Get a property from a font-spec while accepting both `family` and `:family`
/// style keys, and both keyword and symbol keys.
fn font_spec_get_flexible(vec_elems: &[Value], prop: &str) -> Option<Value> {
    let prop_norm = prop.trim_start_matches(':');
    let mut i = 1;
    while i + 1 < vec_elems.len() {
        let key = &vec_elems[i];
        let key_text = match key {
            Value::Keyword(k) => k.as_str(),
            Value::Symbol(k) => k.as_str(),
            _ => {
                i += 2;
                continue;
            }
        };
        let key_norm = key_text.trim_start_matches(':');
        if key_norm == prop_norm {
            return Some(vec_elems[i + 1].clone());
        }
        i += 2;
    }
    None
}

fn font_spec_field_to_string(value: &Value) -> String {
    match value {
        Value::Str(s) => (**s).clone(),
        Value::Symbol(s) | Value::Keyword(s) => s.clone(),
        _ => "*".to_string(),
    }
}

fn xlfd_size_field(size_val: &Value) -> Option<String> {
    match size_val {
        Value::Int(size) => {
            if *size > 0 {
                Some(format!("{}-*", size))
            } else {
                Some("*-*".to_string())
            }
        }
        Value::Float(size) => {
            let scaled = size * 10.0;
            if scaled.is_finite() {
                Some(format!("*-{}", scaled.round() as i64))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn fold_xlfd_wildcards(mut name: String) -> String {
    while let Some(pos) = name.find("-*-*") {
        name.replace_range(pos + 1..pos + 3, "");
    }
    name
}

fn normalize_registry_field(value: &Option<Value>) -> String {
    match value {
        None => "*-*".to_string(),
        Some(Value::Str(s)) => {
            if !s.contains('-') {
                format!("{}-*", s)
            } else {
                (**s).clone()
            }
        }
        Some(Value::Symbol(s)) | Some(Value::Keyword(s)) => {
            if !s.contains('-') {
                format!("{}-*", s)
            } else {
                s.clone()
            }
        }
        _ => "*-*".to_string(),
    }
}

fn sanitize_style_field(value: &Value) -> String {
    match value {
        Value::Symbol(s) => s
            .chars()
            .filter(|ch| *ch != '-' && *ch != '?' && *ch != ',' && *ch != '"')
            .collect(),
        Value::Keyword(s) => s
            .chars()
            .filter(|ch| *ch != '-' && *ch != '?' && *ch != ',' && *ch != '"')
            .collect(),
        Value::Str(s) => s
            .chars()
            .filter(|ch| *ch != '-' && *ch != '?' && *ch != ',' && *ch != '"')
            .collect(),
        _ => "*".to_string(),
    }
}

fn spacing_field(value: Option<&Value>) -> String {
    match value {
        None => "*".to_string(),
        Some(Value::Int(spacing)) => {
            let value = *spacing;
            if value <= 0 {
                "p".to_string()
            } else if value <= 1 {
                "d".to_string()
            } else if value <= 2 {
                "m".to_string()
            } else {
                "c".to_string()
            }
        }
        Some(value) => sanitize_style_field(value),
    }
}

fn avg_width_field(value: Option<&Value>) -> String {
    match value {
        Some(Value::Int(n)) => n.to_string(),
        Some(Value::Str(s)) => (**s).clone(),
        Some(Value::Symbol(s)) | Some(Value::Keyword(s)) => s.clone(),
        _ => "*".to_string(),
    }
}

fn xlfd_pixel_field(size: Option<&Value>) -> String {
    match size {
        Some(value) => xlfd_size_field(value).unwrap_or("*-*".to_string()),
        None => "*-*".to_string(),
    }
}

fn xlfd_resolution_field(dpi: Option<&Value>) -> String {
    match dpi {
        Some(Value::Int(size)) => format!("{}-{}", size, size),
        _ => "*-*".to_string(),
    }
}

fn xlfd_fields_from_font_spec(
    v: &[Value],
) -> (
    String,
    String,
    String,
    String,
    String,
    String,
    String,
    String,
    String,
    String,
    String,
) {
    let foundry = font_spec_get_flexible(v, "foundry")
        .map(|value| font_spec_field_to_string(&value))
        .unwrap_or_else(|| "*".to_string());
    let family = font_spec_get_flexible(v, "family")
        .map(|value| font_spec_field_to_string(&value))
        .unwrap_or_else(|| "*".to_string());
    let weight = font_spec_get_flexible(v, "weight")
        .map(|value| sanitize_style_field(&value))
        .unwrap_or_else(|| "*".to_string());
    let slant = font_spec_get_flexible(v, "slant")
        .map(|value| sanitize_style_field(&value))
        .unwrap_or_else(|| "*".to_string());
    let set_width = font_spec_get_flexible(v, "set-width")
        .or_else(|| font_spec_get_flexible(v, "setwidth"))
        .map(|value| font_spec_field_to_string(&value))
        .unwrap_or_else(|| "*".to_string());
    let adstyle = font_spec_get_flexible(v, "adstyle")
        .map(|value| font_spec_field_to_string(&value))
        .unwrap_or_else(|| "*".to_string());

    let size = font_spec_get_flexible(v, "size");
    let dpi = font_spec_get_flexible(v, "dpi");
    let spacing = font_spec_get_flexible(v, "spacing");
    let avg_width = font_spec_get_flexible(v, "average_width")
        .or_else(|| font_spec_get_flexible(v, "avg_width"))
        .or_else(|| font_spec_get_flexible(v, "avg-width"));
    let registry = font_spec_get_flexible(v, "registry");

    let pixel = xlfd_pixel_field(size.as_ref());
    let resx = xlfd_resolution_field(dpi.as_ref());
    let spacing = spacing_field(spacing.as_ref());
    let avg_width = avg_width_field(avg_width.as_ref());
    let registry = normalize_registry_field(&registry);

    (
        foundry, family, weight, slant, set_width, adstyle, pixel, resx, spacing, avg_width,
        registry,
    )
}

/// Set (or add) a property in a font-spec in place.
fn font_spec_put(vec_elems: &mut Vec<Value>, prop: &Value, val: &Value) {
    let mut i = 1;
    while i + 1 < vec_elems.len() {
        if vec_elems[i] == *prop {
            vec_elems[i + 1] = val.clone();
            return;
        }
        i += 2;
    }
    vec_elems.push(prop.clone());
    vec_elems.push(val.clone());
}

// ===========================================================================
// Font builtins (pure)
// ===========================================================================

/// `(fontp OBJECT &optional EXTRA-TYPE)` -- return t if OBJECT is a font-spec,
/// font-entity, or font-object.  We represent all of these as tagged vectors
/// with `:font-spec` keyword at position 0.
pub(crate) fn builtin_fontp(args: Vec<Value>) -> EvalResult {
    expect_max_args("fontp", &args, 2)?;
    expect_min_args("fontp", &args, 1)?;
    // Ignore EXTRA-TYPE for now; just check the tag.
    Ok(Value::bool(is_font_spec(&args[0])))
}

/// `(font-spec &rest ARGS)` -- create a font spec from keyword args.
///
/// Usage: `(font-spec :family "Monospace" :weight 'normal :size 12)`
///
/// Returns a vector `[:font-spec :family "Monospace" :weight normal :size 12]`.
pub(crate) fn builtin_font_spec(args: Vec<Value>) -> EvalResult {
    let mut elems: Vec<Value> = Vec::with_capacity(1 + args.len());
    elems.push(Value::Keyword(FONT_SPEC_TAG.to_string()));

    for pair_index in (0..args.len()).step_by(2) {
        let key = &args[pair_index];
        let value = args.get(pair_index + 1);

        let Some(value) = value else {
            if matches!(key, Value::Keyword(_) | Value::Symbol(_) | Value::Nil) {
                let key_name = match key {
                    Value::Keyword(k) => format!(":{}", k),
                    Value::Symbol(s) => s.clone(),
                    Value::Nil => "nil".to_string(),
                    _ => "nil".to_string(),
                };
                return Err(signal(
                    "error",
                    vec![Value::string(format!("No value for key ‘{}’", key_name))],
                ));
            }
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), key.clone()],
            ));
        };

        if matches!(key, Value::Nil) {
            return Err(signal(
                "error",
                vec![
                    Value::string("invalid font property"),
                    Value::list(vec![Value::cons(Value::keyword("type"), value.clone())]),
                ],
            ));
        }

        if !matches!(key, Value::Keyword(_) | Value::Symbol(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), key.clone()],
            ));
        }

        elems.push(key.clone());
        elems.push(value.clone());
    }

    Ok(Value::vector(elems))
}

/// `(font-get FONT PROP)` -- get a property value from a font-spec.
pub(crate) fn builtin_font_get(args: Vec<Value>) -> EvalResult {
    expect_args("font-get", &args, 2)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font"), args[0].clone()],
        ));
    }
    if !matches!(&args[1], Value::Keyword(_) | Value::Symbol(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[1].clone()],
        ));
    }

    match &args[0] {
        Value::Vector(v) => {
            let elems = with_heap(|h| h.get_vector(*v).clone());
            Ok(font_spec_get(&elems, &args[1]))
        }
        _ => unreachable!("font-spec check above guarantees vector"),
    }
}

/// `(font-put FONT PROP VAL)` -- set a property in a font-spec and return VAL.
pub(crate) fn builtin_font_put(args: Vec<Value>) -> EvalResult {
    expect_args("font-put", &args, 3)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[0].clone()],
        ));
    }
    match &args[0] {
        Value::Vector(v) => {
            with_heap_mut(|h| {
                let elems = h.get_vector_mut(*v);
                font_spec_put(elems, &args[1], &args[2]);
            });
            Ok(args[2].clone())
        }
        _ => unreachable!("font-spec check above guarantees vector"),
    }
}

/// `(list-fonts FONT-SPEC &optional FRAME MAXNUM PREFER)` -- returns nil in
/// batch-compatible mode.
pub(crate) fn builtin_list_fonts(args: Vec<Value>) -> EvalResult {
    expect_min_args("list-fonts", &args, 1)?;
    expect_max_args("list-fonts", &args, 4)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `list-fonts`.
///
/// Accepts live frame designators in the optional FRAME slot.
pub(crate) fn builtin_list_fonts_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("list-fonts", &args, 1)?;
    expect_max_args("list-fonts", &args, 4)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[0].clone()],
        ));
    }
    expect_optional_frame_designator_eval(eval, args.get(1))?;
    Ok(Value::Nil)
}

/// `(find-font FONT-SPEC &optional FRAME)` -- returns nil in
/// batch-compatible mode.
pub(crate) fn builtin_find_font(args: Vec<Value>) -> EvalResult {
    expect_min_args("find-font", &args, 1)?;
    expect_max_args("find-font", &args, 2)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `find-font`.
///
/// Accepts live frame designators in the optional FRAME slot.
pub(crate) fn builtin_find_font_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("find-font", &args, 1)?;
    expect_max_args("find-font", &args, 2)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-spec"), args[0].clone()],
        ));
    }
    expect_optional_frame_designator_eval(eval, args.get(1))?;
    Ok(Value::Nil)
}

/// `(clear-font-cache)` -- reset internal font/face caches and return nil.
pub(crate) fn builtin_clear_font_cache(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-font-cache", &args, 0)?;
    clear_font_cache_state();
    Ok(Value::Nil)
}

/// `(font-family-list &optional FRAME)` -- returns nil in batch-compatible mode.
pub(crate) fn builtin_font_family_list(args: Vec<Value>) -> EvalResult {
    expect_max_args("font-family-list", &args, 1)?;
    if let Some(frame) = args.first() {
        if !frame.is_nil() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// Evaluator-aware variant of `font-family-list`.
///
/// Accepts live frame designators in the optional FRAME slot.
pub(crate) fn builtin_font_family_list_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("font-family-list", &args, 1)?;
    expect_optional_frame_designator_eval(eval, args.first())?;
    Ok(Value::Nil)
}

/// `(font-xlfd-name FONT &optional FOLD-WILDCARDS)` -- render font-spec fields
/// into an XLFD string; wildcard folding is supported in compatibility mode.
pub(crate) fn builtin_font_xlfd_name(args: Vec<Value>) -> EvalResult {
    expect_min_args("font-xlfd-name", &args, 1)?;
    expect_max_args("font-xlfd-name", &args, 3)?;
    if !is_font_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font"), args[0].clone()],
        ));
    }

    let fields = match &args[0] {
        Value::Vector(v) => {
            let elems = with_heap(|h| h.get_vector(*v).clone());
            xlfd_fields_from_font_spec(&elems)
        }
        _ => (
            "*".to_string(),
            "*".to_string(),
            "*".to_string(),
            "*".to_string(),
            "*".to_string(),
            "*".to_string(),
            "*-*".to_string(),
            "*-*".to_string(),
            "*".to_string(),
            "*".to_string(),
            "*-*".to_string(),
        ),
    };

    let (
        foundry,
        family,
        weight,
        slant,
        set_width,
        adstyle,
        pixel,
        resx,
        spacing,
        avg_width,
        registry,
    ) = fields;
    let rendered = if args.get(1).is_some_and(Value::is_truthy) {
        let name = format!(
            "-{}-{}-{}-{}-{}-{}-{}-{}-{}-{}-{}",
            foundry,
            family,
            weight,
            slant,
            set_width,
            adstyle,
            pixel,
            resx,
            spacing,
            avg_width,
            registry
        );
        fold_xlfd_wildcards(name)
    } else {
        format!(
            "-{}-{}-{}-{}-{}-{}-{}-{}-{}-{}-{}",
            foundry,
            family,
            weight,
            slant,
            set_width,
            adstyle,
            pixel,
            resx,
            spacing,
            avg_width,
            registry
        )
    };
    Ok(Value::string(rendered))
}

/// `(close-font FONT-OBJECT &optional FRAME)` -- close an open font object.
///
/// NeoVM currently has no runtime font-object handles, so this validates the
/// argument shape and returns nil for accepted objects.
pub(crate) fn builtin_close_font(args: Vec<Value>) -> EvalResult {
    expect_min_args("close-font", &args, 1)?;
    expect_max_args("close-font", &args, 2)?;
    if !is_font_object(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("font-object"), args[0].clone()],
        ));
    }
    Ok(Value::Nil)
}

// ===========================================================================
// Face builtins (pure)
// ===========================================================================

/// Well-known face names returned by `face-list` and recognised by
/// `internal-lisp-face-p`.
const KNOWN_FACES: &[&str] = &[
    "default",
    "bold",
    "italic",
    "underline",
    "highlight",
    "region",
    "mode-line",
    "mode-line-inactive",
    "fringe",
    "cursor",
];
const FIRST_DYNAMIC_FACE_ID: i64 = 133;

fn known_face_id(name: &str) -> Option<i64> {
    match name {
        "default" => Some(0),
        "bold" => Some(1),
        "italic" => Some(2),
        "underline" => Some(4),
        "highlight" => Some(12),
        "region" => Some(13),
        "mode-line" => Some(25),
        "mode-line-inactive" => Some(27),
        "fringe" => Some(40),
        "cursor" => Some(43),
        _ => None,
    }
}

const LISP_FACE_VECTOR_LEN: usize = 20;
const VALID_FACE_ATTRIBUTES: &[&str] = &[
    ":family",
    ":foundry",
    ":height",
    ":weight",
    ":slant",
    ":underline",
    ":overline",
    ":strike-through",
    ":box",
    ":inverse-video",
    ":foreground",
    ":distant-foreground",
    ":background",
    ":stipple",
    ":width",
    ":inherit",
    ":extend",
    ":font",
    ":fontset",
];
const LISP_FACE_VECTOR_ATTRIBUTES: &[&str] = &[
    ":family",
    ":foundry",
    ":width",
    ":height",
    ":weight",
    ":slant",
    ":underline",
    ":inverse-video",
    ":foreground",
    ":background",
    ":stipple",
    ":overline",
    ":strike-through",
    ":box",
    ":font",
    ":inherit",
    ":extend",
    ":distant-foreground",
    ":fontset",
];
const DISCRETE_BOOLEAN_FACE_ATTRIBUTES: &[&str] = &[
    ":underline",
    ":overline",
    ":strike-through",
    ":inverse-video",
    ":extend",
];
const SET_ONLY_FACE_ATTRIBUTES: &[&str] = &[":bold", ":italic"];
const VALID_FACE_WEIGHTS: &[&str] = &[
    "ultra-light",
    "extra-light",
    "light",
    "semi-light",
    "normal",
    "semi-bold",
    "bold",
    "extra-bold",
    "ultra-bold",
];
const VALID_FACE_SLANTS: &[&str] = &[
    "normal",
    "italic",
    "oblique",
    "reverse-italic",
    "reverse-oblique",
];
const VALID_FACE_WIDTHS: &[&str] = &[
    "ultra-condensed",
    "extra-condensed",
    "condensed",
    "semi-condensed",
    "normal",
    "semi-expanded",
    "expanded",
    "extra-expanded",
    "ultra-expanded",
];

static CREATED_LISP_FACES: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
static CREATED_FACE_IDS: OnceLock<Mutex<HashMap<String, i64>>> = OnceLock::new();
static NEXT_CREATED_FACE_ID: OnceLock<Mutex<i64>> = OnceLock::new();
#[derive(Default)]
struct FaceAttrState {
    selected_created: HashSet<String>,
    selected_overrides: std::collections::HashMap<String, std::collections::HashMap<String, Value>>,
    defaults_overrides: std::collections::HashMap<String, std::collections::HashMap<String, Value>>,
}

static FACE_ATTR_STATE: OnceLock<Mutex<FaceAttrState>> = OnceLock::new();

fn created_lisp_faces() -> &'static Mutex<HashSet<String>> {
    CREATED_LISP_FACES.get_or_init(|| Mutex::new(HashSet::new()))
}

fn created_face_ids() -> &'static Mutex<HashMap<String, i64>> {
    CREATED_FACE_IDS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn next_created_face_id() -> &'static Mutex<i64> {
    NEXT_CREATED_FACE_ID.get_or_init(|| Mutex::new(FIRST_DYNAMIC_FACE_ID))
}

fn face_attr_state() -> &'static Mutex<FaceAttrState> {
    FACE_ATTR_STATE.get_or_init(|| Mutex::new(FaceAttrState::default()))
}

fn clear_font_cache_state() {
    created_lisp_faces().lock().expect("poisoned").clear();
    created_face_ids().lock().expect("poisoned").clear();
    *next_created_face_id().lock().expect("poisoned") = FIRST_DYNAMIC_FACE_ID;
    *face_attr_state().lock().expect("poisoned") = FaceAttrState::default();
}

fn is_created_lisp_face(name: &str) -> bool {
    created_lisp_faces()
        .lock()
        .expect("poisoned")
        .contains(name)
}

fn mark_created_lisp_face(name: &str) {
    let inserted = created_lisp_faces()
        .lock()
        .expect("poisoned")
        .insert(name.to_string());
    if inserted {
        ensure_dynamic_face_id(name);
    }
}

fn ensure_dynamic_face_id(name: &str) {
    if known_face_id(name).is_some() {
        return;
    }
    let mut ids = created_face_ids().lock().expect("poisoned");
    if ids.contains_key(name) {
        return;
    }
    let mut next = next_created_face_id().lock().expect("poisoned");
    ids.insert(name.to_string(), *next);
    *next += 1;
}

fn dynamic_face_id(name: &str) -> Option<i64> {
    created_face_ids()
        .lock()
        .expect("poisoned")
        .get(name)
        .copied()
}

fn face_id_for_name(name: &str) -> Option<i64> {
    known_face_id(name).or_else(|| dynamic_face_id(name))
}

fn is_selected_created_lisp_face(name: &str) -> bool {
    face_attr_state()
        .lock()
        .expect("poisoned")
        .selected_created
        .contains(name)
}

fn mark_selected_created_lisp_face(name: &str) {
    face_attr_state()
        .lock()
        .expect("poisoned")
        .selected_created
        .insert(name.to_string());
}

fn face_exists_for_domain(name: &str, defaults_frame: bool) -> bool {
    if KNOWN_FACES.contains(&name) {
        return true;
    }
    if defaults_frame {
        is_created_lisp_face(name)
    } else {
        is_selected_created_lisp_face(name)
    }
}

fn get_face_override(face_name: &str, attr: &str, defaults_frame: bool) -> Option<Value> {
    let state = face_attr_state().lock().expect("poisoned");
    let map = if defaults_frame {
        &state.defaults_overrides
    } else {
        &state.selected_overrides
    };
    map.get(face_name)
        .and_then(|attrs| attrs.get(attr))
        .cloned()
}

fn set_face_override(face_name: &str, attr: &str, value: Value, defaults_frame: bool) {
    let mut state = face_attr_state().lock().expect("poisoned");
    let map = if defaults_frame {
        &mut state.defaults_overrides
    } else {
        &mut state.selected_overrides
    };
    map.entry(face_name.to_string())
        .or_default()
        .insert(attr.to_string(), value);
}

fn clear_face_overrides(face_name: &str, defaults_frame: bool) {
    let mut state = face_attr_state().lock().expect("poisoned");
    if defaults_frame {
        state.defaults_overrides.remove(face_name);
    } else {
        state.selected_overrides.remove(face_name);
    }
}

fn copy_defaults_overrides(src: &str, dst: &str) {
    let mut state = face_attr_state().lock().expect("poisoned");
    let copied = state.defaults_overrides.get(src).cloned();
    if let Some(attrs) = copied {
        state.defaults_overrides.insert(dst.to_string(), attrs);
    } else {
        state.defaults_overrides.remove(dst);
    }
}

fn merge_defaults_overrides_into_selected(face_name: &str) {
    let mut state = face_attr_state().lock().expect("poisoned");
    let defaults = state.defaults_overrides.get(face_name).cloned();
    if let Some(attrs) = defaults {
        let selected = state
            .selected_overrides
            .entry(face_name.to_string())
            .or_default();
        for (attr, value) in attrs {
            if matches!(&value, Value::Symbol(s) if s == "unspecified" || s == "relative") {
                continue;
            }
            selected.insert(attr, value);
        }
    }
}

fn symbol_name_for_face_value(face: &Value) -> Option<String> {
    match face {
        Value::Nil => Some("nil".to_string()),
        Value::True => Some("t".to_string()),
        Value::Symbol(name) => Some(name.clone()),
        _ => None,
    }
}

fn require_symbol_face_name(face: &Value) -> Result<String, Flow> {
    symbol_name_for_face_value(face).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), face.clone()],
        )
    })
}

fn known_face_name(face: &Value) -> Option<String> {
    let name = match face {
        Value::Str(name) => name.as_str().to_string(),
        _ => symbol_name_for_face_value(face)?,
    };
    if KNOWN_FACES.contains(&name.as_str()) || is_created_lisp_face(&name) {
        Some(name)
    } else {
        None
    }
}

fn resolve_copy_source_face_symbol(face: &Value) -> Result<String, Flow> {
    let name = symbol_name_for_face_value(face).expect("checked symbol before resolve");
    if KNOWN_FACES.contains(&name.as_str()) || is_created_lisp_face(&name) {
        return Ok(name);
    }
    if face.is_nil() {
        return Err(signal("error", vec![Value::string("Invalid face")]));
    }
    Err(signal(
        "error",
        vec![Value::string("Invalid face"), face.clone()],
    ))
}

fn resolve_face_name_for_domain(face: &Value, defaults_frame: bool) -> Result<String, Flow> {
    match face {
        Value::Str(name) => {
            if face_exists_for_domain(name, defaults_frame) {
                Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), face.clone()],
                ))
            } else {
                Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), Value::symbol(name.as_str())],
                ))
            }
        }
        Value::Nil | Value::True | Value::Symbol(_) => {
            let name = symbol_name_for_face_value(face).expect("symbol-like");
            if face_exists_for_domain(&name, defaults_frame) {
                Ok(name)
            } else if face.is_nil() {
                Err(signal("error", vec![Value::string("Invalid face")]))
            } else {
                Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), face.clone()],
                ))
            }
        }
        _ => Err(signal(
            "error",
            vec![Value::string("Invalid face"), face.clone()],
        )),
    }
}

fn resolve_face_name_for_merge(face: &Value) -> Result<String, Flow> {
    match face {
        Value::Str(name) => {
            if face_exists_for_domain(name, true) {
                Ok((**name).clone())
            } else {
                Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), Value::symbol(name.as_str())],
                ))
            }
        }
        Value::Nil | Value::True | Value::Symbol(_) => {
            let name = symbol_name_for_face_value(face).expect("symbol-like");
            if face_exists_for_domain(&name, true) {
                Ok(name)
            } else if face.is_nil() {
                Err(signal("error", vec![Value::string("Invalid face")]))
            } else {
                Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), face.clone()],
                ))
            }
        }
        _ => Err(signal(
            "error",
            vec![Value::string("Invalid face"), face.clone()],
        )),
    }
}

fn make_lisp_face_vector() -> Value {
    let mut values = Vec::with_capacity(LISP_FACE_VECTOR_LEN);
    values.push(Value::symbol("face"));
    values.extend((1..LISP_FACE_VECTOR_LEN).map(|_| Value::symbol("unspecified")));
    Value::vector(values)
}

fn make_lisp_face_vector_for_domain(face_name: &str, defaults_frame: bool) -> Value {
    let mut values = Vec::with_capacity(LISP_FACE_VECTOR_LEN);
    values.push(Value::symbol("face"));
    values.extend(
        LISP_FACE_VECTOR_ATTRIBUTES
            .iter()
            .map(|attr| lisp_face_attribute_value(face_name, attr, defaults_frame)),
    );
    Value::vector(values)
}

fn normalize_face_attribute_name(attr: &Value) -> Result<String, Flow> {
    let name = match attr {
        Value::Symbol(name) => name.clone(),
        Value::Keyword(name) => {
            if name.starts_with(':') {
                name.clone()
            } else {
                format!(":{name}")
            }
        }
        Value::Nil | Value::True => attr.as_symbol_name().unwrap_or_default().to_string(),
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), attr.clone()],
            ));
        }
    };

    if VALID_FACE_ATTRIBUTES.contains(&name.as_str()) {
        Ok(name)
    } else if attr.is_nil() {
        Err(signal(
            "error",
            vec![Value::string("Invalid face attribute name")],
        ))
    } else {
        Err(signal(
            "error",
            vec![Value::string("Invalid face attribute name"), attr.clone()],
        ))
    }
}

fn normalize_set_face_attribute_name(attr: &Value) -> Result<String, Flow> {
    let name = match attr {
        Value::Symbol(name) => name.clone(),
        Value::Keyword(name) => {
            if name.starts_with(':') {
                name.clone()
            } else {
                format!(":{name}")
            }
        }
        Value::Nil | Value::True => attr.as_symbol_name().unwrap_or_default().to_string(),
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), attr.clone()],
            ));
        }
    };

    if VALID_FACE_ATTRIBUTES.contains(&name.as_str())
        || SET_ONLY_FACE_ATTRIBUTES.contains(&name.as_str())
    {
        Ok(name)
    } else if attr.is_nil() {
        Err(signal(
            "error",
            vec![Value::string("Invalid face attribute name")],
        ))
    } else {
        Err(signal(
            "error",
            vec![Value::string("Invalid face attribute name"), attr.clone()],
        ))
    }
}

fn default_face_attribute_value(attr: &str) -> Value {
    match attr {
        ":family" | ":foundry" => Value::string("default"),
        ":height" => Value::Int(1),
        ":weight" | ":slant" | ":width" => Value::symbol("normal"),
        ":underline" | ":overline" | ":strike-through" | ":box" | ":inverse-video" | ":stipple"
        | ":inherit" | ":extend" | ":fontset" => Value::Nil,
        ":foreground" => Value::string("unspecified-fg"),
        ":background" => Value::string("unspecified-bg"),
        ":distant-foreground" | ":font" => Value::symbol("unspecified"),
        _ => Value::symbol("unspecified"),
    }
}

fn lisp_face_attribute_base_value(face: &str, attr: &str, defaults_frame: bool) -> Value {
    if defaults_frame {
        return Value::symbol("unspecified");
    }
    if face == "default" {
        return default_face_attribute_value(attr);
    }
    match (face, attr) {
        ("bold", ":weight") => Value::symbol("bold"),
        ("italic", ":slant") => Value::symbol("italic"),
        ("underline", ":underline") => Value::True,
        ("highlight", ":inverse-video") => Value::True,
        ("region", ":inverse-video") => Value::True,
        ("mode-line", ":inverse-video") => Value::True,
        ("mode-line-inactive", ":inherit") => Value::symbol("mode-line"),
        ("fringe", ":background") => Value::string("gray"),
        ("cursor", ":background") => Value::string("white"),
        _ => Value::symbol("unspecified"),
    }
}

fn lisp_face_attribute_value(face: &str, attr: &str, defaults_frame: bool) -> Value {
    if let Some(value) = get_face_override(face, attr, defaults_frame) {
        return value;
    }
    lisp_face_attribute_base_value(face, attr, defaults_frame)
}

fn resolve_known_face_name_for_compare(face: &Value, defaults_frame: bool) -> Result<String, Flow> {
    match face {
        Value::Str(name) => {
            if face_exists_for_domain(name, defaults_frame) {
                Ok((**name).clone())
            } else {
                Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), Value::symbol(name.as_str())],
                ))
            }
        }
        _ => resolve_face_name_for_domain(face, defaults_frame),
    }
}

fn face_attr_value_name(attr: &Value) -> Result<String, Flow> {
    match attr {
        Value::Keyword(name) => {
            if name.starts_with(':') {
                Ok(name.clone())
            } else {
                Ok(format!(":{name}"))
            }
        }
        Value::Symbol(name) => Ok(name.clone()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), attr.clone()],
        )),
    }
}

fn frame_defaults_flag(frame: Option<&Value>) -> Result<bool, Flow> {
    match frame {
        None => Ok(false),
        Some(v) if v.is_nil() => Ok(false),
        Some(Value::True) => Ok(true),
        Some(v) if frame_device_designator_p(v) => Ok(false),
        Some(v) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), v.clone()],
        )),
    }
}

fn proper_list_to_vec_or_listp_error(value: &Value) -> Result<Vec<Value>, Flow> {
    let mut out = Vec::new();
    let mut cursor = value.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(out),
            Value::Cons(cell) => {
                let cell = read_cons(cell);
                out.push(cell.car.clone());
                cursor = cell.cdr.clone();
            }
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), other],
                ));
            }
        }
    }
}

fn check_non_empty_string(value: &Value, empty_message: &str) -> Result<(), Flow> {
    match value {
        Value::Str(s) => {
            if s.is_empty() {
                Err(signal(
                    "error",
                    vec![Value::string(empty_message), value.clone()],
                ))
            } else {
                Ok(())
            }
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), value.clone()],
        )),
    }
}

fn symbol_name_or_type_error(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        Value::Symbol(s) => Ok(s.clone()),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), value.clone()],
        )),
    }
}

fn normalize_face_attr_for_set(
    face_name: &str,
    attr: &str,
    value: Value,
) -> Result<(String, Value), Flow> {
    let normalized = match attr {
        ":foreground" | ":background" | ":distant-foreground" if value.is_nil() => {
            Value::symbol("unspecified")
        }
        _ => value,
    };
    let is_reset_like = matches!(&normalized, Value::Symbol(s) if s == "unspecified" || s == ":ignore-defface" || s == "reset");

    match attr {
        ":family" | ":foundry" => {
            if !is_reset_like {
                match &normalized {
                    Value::Str(s) if !s.is_empty() => {}
                    Value::Str(_) => {
                        let msg = if attr == ":family" {
                            "Invalid face family"
                        } else {
                            "Invalid face foundry"
                        };
                        return Err(signal("error", vec![Value::string(msg), normalized]));
                    }
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("stringp"), normalized],
                        ));
                    }
                }
            }
        }
        ":height" => {
            if !is_reset_like {
                if face_name == "default" {
                    match &normalized {
                        Value::Int(n) if *n > 0 => {}
                        _ => {
                            return Err(signal(
                                "error",
                                vec![
                                    Value::string("Default face height not absolute and positive"),
                                    normalized,
                                ],
                            ));
                        }
                    }
                } else {
                    match &normalized {
                        Value::Int(n) if *n > 0 => {}
                        Value::Float(f) if *f > 0.0 => {}
                        _ => {
                            return Err(signal(
                                "error",
                                vec![
                                    Value::string(
                                        "Face height does not produce a positive integer",
                                    ),
                                    normalized,
                                ],
                            ));
                        }
                    }
                }
            }
        }
        ":weight" => {
            if !is_reset_like {
                let sym = symbol_name_or_type_error(&normalized)?;
                if !VALID_FACE_WEIGHTS.contains(&sym.as_str()) {
                    return Err(signal(
                        "error",
                        vec![Value::string("Invalid face weight"), normalized],
                    ));
                }
            }
        }
        ":slant" => {
            if !is_reset_like {
                let sym = symbol_name_or_type_error(&normalized)?;
                if !VALID_FACE_SLANTS.contains(&sym.as_str()) {
                    return Err(signal(
                        "error",
                        vec![Value::string("Invalid face slant"), normalized],
                    ));
                }
            }
        }
        ":width" => {
            if !is_reset_like {
                let sym = symbol_name_or_type_error(&normalized)?;
                if !VALID_FACE_WIDTHS.contains(&sym.as_str()) {
                    return Err(signal(
                        "error",
                        vec![Value::string("Invalid face width"), normalized],
                    ));
                }
            }
        }
        ":foreground" => {
            if !is_reset_like {
                check_non_empty_string(&normalized, "Empty foreground color value")?;
            }
        }
        ":background" => {
            if !is_reset_like {
                check_non_empty_string(&normalized, "Empty background color value")?;
            }
        }
        ":distant-foreground" => {
            if !is_reset_like {
                check_non_empty_string(&normalized, "Empty distant-foreground color value")?;
            }
        }
        ":inverse-video" => {
            if !is_reset_like {
                let sym = symbol_name_or_type_error(&normalized)?;
                if sym != "t" && sym != "nil" {
                    return Err(signal(
                        "error",
                        vec![
                            Value::string("Invalid inverse-video face attribute value"),
                            normalized,
                        ],
                    ));
                }
            }
        }
        ":extend" => {
            if !is_reset_like {
                let sym = symbol_name_or_type_error(&normalized)?;
                if sym != "t" && sym != "nil" {
                    return Err(signal(
                        "error",
                        vec![
                            Value::string("Invalid extend face attribute value"),
                            normalized,
                        ],
                    ));
                }
            }
        }
        ":inherit" => {
            let valid = match &normalized {
                Value::Nil | Value::True | Value::Symbol(_) => true,
                Value::Cons(_) => list_to_vec(&normalized)
                    .map(|vals| vals.iter().all(Value::is_symbol))
                    .unwrap_or(false),
                _ => false,
            };
            if !valid {
                let mut payload = vec![Value::string("Invalid face inheritance")];
                if let Some(vals) = list_to_vec(&normalized) {
                    payload.extend(vals);
                } else {
                    payload.push(normalized);
                }
                return Err(signal("error", payload));
            }
        }
        ":bold" => {
            let mapped = if normalized.is_nil() {
                Value::symbol("normal")
            } else {
                Value::symbol("bold")
            };
            return Ok((":weight".to_string(), mapped));
        }
        ":italic" => {
            let mapped = if normalized.is_nil() {
                Value::symbol("normal")
            } else {
                Value::symbol("italic")
            };
            return Ok((":slant".to_string(), mapped));
        }
        _ => {}
    }

    Ok((attr.to_string(), normalized))
}

/// `(internal-lisp-face-p FACE &optional FRAME)` -- return a face descriptor
/// vector for known faces, nil otherwise.
pub(crate) fn builtin_internal_lisp_face_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-lisp-face-p", &args, 1)?;
    expect_max_args("internal-lisp-face-p", &args, 2)?;
    let frame_designator = if let Some(frame) = args.get(1) {
        if !optional_selected_frame_designator_p(frame) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
        !frame.is_nil()
    } else {
        false
    };
    if let Some(face_name) = known_face_name(&args[0]) {
        if frame_designator {
            Ok(make_lisp_face_vector_for_domain(&face_name, false))
        } else {
            Ok(make_lisp_face_vector())
        }
    } else {
        Ok(Value::Nil)
    }
}

/// `(internal-make-lisp-face FACE &optional FRAME)` -- create/reset FACE as a
/// Lisp face and return its attribute vector.
pub(crate) fn builtin_internal_make_lisp_face(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-make-lisp-face", &args, 1)?;
    expect_max_args("internal-make-lisp-face", &args, 2)?;
    let face_name = require_symbol_face_name(&args[0])?;
    if let Some(frame) = args.get(1) {
        if !optional_selected_frame_designator_p(frame) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    mark_created_lisp_face(&face_name);
    clear_face_overrides(&face_name, true);
    Ok(make_lisp_face_vector())
}

/// `(internal-copy-lisp-face FROM TO FRAME NEW-FRAME)` -- copy defaults overrides to
/// `TO` and return `TO`.
pub(crate) fn builtin_internal_copy_lisp_face(args: Vec<Value>) -> EvalResult {
    expect_args("internal-copy-lisp-face", &args, 4)?;
    let _ = require_symbol_face_name(&args[0])?;
    let to_name = require_symbol_face_name(&args[1])?;
    let copy_defaults_domain = matches!(args[2], Value::True);
    if !copy_defaults_domain && !frame_device_designator_p(&args[2]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), args[2].clone()],
        ));
    }
    if !copy_defaults_domain && !args[3].is_nil() && !frame_device_designator_p(&args[3]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), args[3].clone()],
        ));
    }
    let from_name = resolve_copy_source_face_symbol(&args[0])?;
    mark_created_lisp_face(&to_name);
    copy_defaults_overrides(&from_name, &to_name);
    Ok(args[1].clone())
}

/// `(internal-set-lisp-face-attribute FACE ATTR VALUE &optional FRAME)` --
/// set FACE attribute in selected-frame/default face domains.
pub(crate) fn builtin_internal_set_lisp_face_attribute(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-set-lisp-face-attribute", &args, 3)?;
    expect_max_args("internal-set-lisp-face-attribute", &args, 4)?;
    let face = &args[0];
    let face_name = require_symbol_face_name(face)?;
    let attr_name = normalize_set_face_attribute_name(&args[1])?;
    let value = args[2].clone();

    let apply_set = |defaults_frame: bool| -> Result<(), Flow> {
        if defaults_frame {
            if !face_exists_for_domain(&face_name, true) {
                if face.is_nil() {
                    return Err(signal("error", vec![Value::string("Invalid face")]));
                }
                return Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), face.clone()],
                ));
            }
        } else if !face_exists_for_domain(&face_name, false) {
            mark_selected_created_lisp_face(&face_name);
            mark_created_lisp_face(&face_name);
        }

        let (canonical_attr, canonical_value) =
            normalize_face_attr_for_set(&face_name, &attr_name, value.clone())?;
        set_face_override(&face_name, &canonical_attr, canonical_value, defaults_frame);
        Ok(())
    };

    match args.get(3) {
        None | Some(Value::Nil) => apply_set(false)?,
        Some(Value::True) => apply_set(true)?,
        Some(Value::Int(0)) => {
            apply_set(true)?;
            apply_set(false)?;
        }
        Some(frame) if frame_device_designator_p(frame) => apply_set(false)?,
        Some(other) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), other.clone()],
            ));
        }
    }

    Ok(face.clone())
}

/// `(internal-get-lisp-face-attribute FACE ATTR &optional FRAME)` -- batch
/// semantics-compatible face attribute query for core predefined faces.
pub(crate) fn builtin_internal_get_lisp_face_attribute(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-get-lisp-face-attribute", &args, 2)?;
    expect_max_args("internal-get-lisp-face-attribute", &args, 3)?;
    let defaults_frame = if let Some(frame) = args.get(2) {
        if frame.is_nil() {
            false
        } else if matches!(frame, Value::True) {
            true
        } else if frame_device_designator_p(frame) {
            false
        } else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    } else {
        false
    };

    let face_name = resolve_face_name_for_domain(&args[0], defaults_frame)?;

    let attr_name = normalize_face_attribute_name(&args[1])?;
    Ok(lisp_face_attribute_value(
        &face_name,
        &attr_name,
        defaults_frame,
    ))
}

/// `(internal-lisp-face-attribute-values ATTR)` -- return valid discrete values
/// for known boolean-like face attributes.
pub(crate) fn builtin_internal_lisp_face_attribute_values(args: Vec<Value>) -> EvalResult {
    expect_args("internal-lisp-face-attribute-values", &args, 1)?;
    let attr_name = face_attr_value_name(&args[0])?;
    if DISCRETE_BOOLEAN_FACE_ATTRIBUTES.contains(&attr_name.as_str()) {
        Ok(Value::list(vec![Value::True, Value::Nil]))
    } else {
        Ok(Value::Nil)
    }
}

/// `(internal-lisp-face-equal-p FACE1 FACE2 &optional FRAME)` -- return t if
/// FACE1 and FACE2 resolve to equal face attributes in the selected frame or in
/// default face definitions when FRAME is t.
pub(crate) fn builtin_internal_lisp_face_equal_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-lisp-face-equal-p", &args, 2)?;
    expect_max_args("internal-lisp-face-equal-p", &args, 3)?;
    let defaults_frame = frame_defaults_flag(args.get(2))?;
    let face1 = resolve_known_face_name_for_compare(&args[0], defaults_frame)?;
    let face2 = resolve_known_face_name_for_compare(&args[1], defaults_frame)?;
    for attr in VALID_FACE_ATTRIBUTES {
        let v1 = lisp_face_attribute_value(&face1, attr, defaults_frame);
        let v2 = lisp_face_attribute_value(&face2, attr, defaults_frame);
        if v1 != v2 {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::True)
}

/// `(internal-lisp-face-empty-p FACE &optional FRAME)` -- return t if FACE has
/// only unspecified attributes in selected/default face definitions.
pub(crate) fn builtin_internal_lisp_face_empty_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-lisp-face-empty-p", &args, 1)?;
    expect_max_args("internal-lisp-face-empty-p", &args, 2)?;
    let defaults_frame = frame_defaults_flag(args.get(1))?;
    let face = resolve_known_face_name_for_compare(&args[0], defaults_frame)?;
    for attr in VALID_FACE_ATTRIBUTES {
        let v = lisp_face_attribute_value(&face, attr, defaults_frame);
        if !matches!(v, Value::Symbol(ref name) if name == "unspecified") {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::True)
}

/// `(internal-merge-in-global-face FACE FRAME)` -- merge concrete defaults-face
/// overrides into selected-frame face state.
pub(crate) fn builtin_internal_merge_in_global_face(args: Vec<Value>) -> EvalResult {
    expect_args("internal-merge-in-global-face", &args, 2)?;
    if !frame_device_designator_p(&args[1]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), args[1].clone()],
        ));
    }
    let face_name = resolve_face_name_for_merge(&args[0])?;
    if !KNOWN_FACES.contains(&face_name.as_str()) {
        mark_created_lisp_face(&face_name);
        mark_selected_created_lisp_face(&face_name);
    }
    merge_defaults_overrides_into_selected(&face_name);
    Ok(Value::Nil)
}

/// `(face-attribute-relative-p ATTRIBUTE VALUE)` -- return t if VALUE is the
/// value is a relative form for ATTRIBUTE.
pub(crate) fn builtin_face_attribute_relative_p(args: Vec<Value>) -> EvalResult {
    expect_args("face-attribute-relative-p", &args, 2)?;
    let height_attr = match &args[0] {
        Value::Keyword(name) | Value::Symbol(name) => name == "height" || name == ":height",
        _ => false,
    };
    if !height_attr {
        return Ok(Value::Nil);
    }

    Ok(Value::bool(!matches!(
        &args[1],
        Value::Int(_) | Value::Char(_)
    )))
}

/// `(merge-face-attribute ATTRIBUTE VALUE1 VALUE2)` -- return VALUE1 unless it
/// is the symbol `unspecified`, in which case return VALUE2.
pub(crate) fn builtin_merge_face_attribute(args: Vec<Value>) -> EvalResult {
    expect_args("merge-face-attribute", &args, 3)?;
    let v1_unspecified = match &args[1] {
        Value::Symbol(s) => s == "unspecified",
        _ => false,
    };
    if v1_unspecified {
        Ok(args[2].clone())
    } else {
        Ok(args[1].clone())
    }
}

/// `(face-list &optional FRAME)` -- return list of known face names.
pub(crate) fn builtin_face_list(args: Vec<Value>) -> EvalResult {
    expect_max_args("face-list", &args, 1)?;
    Ok(Value::list(
        KNOWN_FACES.iter().map(|s| Value::symbol(*s)).collect(),
    ))
}

fn expect_color_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn expect_optional_color_frame_arg(args: &[Value], idx: usize) -> Result<(), Flow> {
    if let Some(frame) = args.get(idx) {
        if !frame.is_nil() && !matches!(frame, Value::Frame(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("framep"), frame.clone()],
            ));
        }
    }
    Ok(())
}

fn parse_color_16bit_any(color_name: &str) -> Option<(i64, i64, i64)> {
    let lower = color_name.trim().to_lowercase();
    if let Some(hex) = lower.strip_prefix('#') {
        parse_hex_color_16bit(hex)
    } else {
        parse_named_color_16bit(&lower)
    }
}

/// `(color-defined-p COLOR &optional FRAME)` -- nil if unknown; otherwise truthy
/// for known RGB/hex and supported terminal color names.
pub(crate) fn builtin_color_defined_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("color-defined-p", &args, 1)?;
    expect_max_args("color-defined-p", &args, 2)?;
    expect_optional_color_device_arg(&args, 1)?;
    match &args[0] {
        Value::Str(_) => Ok(Value::bool(
            !builtin_color_values(vec![args[0].clone()])?.is_nil(),
        )),
        _ => Ok(Value::Nil),
    }
}

/// `(color-values COLOR &optional FRAME)` -- resolve COLOR and return a
/// terminal-compatible `(R G B)` list with 16-bit component values.
///
/// In batch/TTY compatibility mode we approximate resolved colors to the
/// nearest entry in the 8-color terminal palette.
pub(crate) fn builtin_color_values(args: Vec<Value>) -> EvalResult {
    expect_min_args("color-values", &args, 1)?;
    expect_max_args("color-values", &args, 2)?;
    expect_optional_color_device_arg(&args, 1)?;
    let color_name = match &args[0] {
        Value::Str(s) => (**s).clone(),
        _ => return Ok(Value::Nil),
    };
    let lower = color_name.trim().to_lowercase();
    let resolved = if let Some(hex) = lower.strip_prefix('#') {
        parse_hex_color_16bit(hex)
    } else {
        parse_named_color_16bit(&lower)
    };
    let Some((r, g, b)) = resolved.map(approximate_tty_color) else {
        return Ok(Value::Nil);
    };
    Ok(Value::list(vec![
        Value::Int(r),
        Value::Int(g),
        Value::Int(b),
    ]))
}

/// `(color-values-from-color-spec COLOR-SPEC)` -- parse hex color spec and
/// return raw `(R G B)` 16-bit channel values.
pub(crate) fn builtin_color_values_from_color_spec(args: Vec<Value>) -> EvalResult {
    expect_args("color-values-from-color-spec", &args, 1)?;
    let color_spec = expect_color_string(&args[0])?;
    let lower = color_spec.trim().to_lowercase();
    let Some(hex) = lower.strip_prefix('#') else {
        return Ok(Value::Nil);
    };
    let Some((r, g, b)) = parse_hex_color_16bit(hex) else {
        return Ok(Value::Nil);
    };
    Ok(Value::list(vec![
        Value::Int(r),
        Value::Int(g),
        Value::Int(b),
    ]))
}

/// `(color-gray-p COLOR &optional FRAME)` -- t if COLOR resolves to equal RGB
/// channels, nil otherwise.
pub(crate) fn builtin_color_gray_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("color-gray-p", &args, 1)?;
    expect_max_args("color-gray-p", &args, 2)?;
    let color = expect_color_string(&args[0])?;
    expect_optional_color_frame_arg(&args, 1)?;
    let Some((r, g, b)) = parse_color_16bit_any(&color) else {
        return Ok(Value::Nil);
    };
    Ok(Value::bool(r == g && g == b))
}

/// `(color-supported-p COLOR &optional FRAME BACKGROUND-P)` -- t if COLOR
/// resolves on this build's color parser.
pub(crate) fn builtin_color_supported_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("color-supported-p", &args, 1)?;
    expect_max_args("color-supported-p", &args, 3)?;
    let color = expect_color_string(&args[0])?;
    expect_optional_color_frame_arg(&args, 1)?;
    let _ = args.get(2);
    Ok(Value::bool(parse_color_16bit_any(&color).is_some()))
}

fn expect_optional_color_distance_frame_arg(args: &[Value], idx: usize) -> Result<(), Flow> {
    if let Some(frame) = args.get(idx) {
        if !frame.is_nil() && !matches!(frame, Value::Frame(_)) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("frame-live-p"), frame.clone()],
            ));
        }
    }
    Ok(())
}

fn invalid_color_error(value: &Value) -> Flow {
    signal("error", vec![Value::string("Invalid color"), value.clone()])
}

fn parse_color_distance_input(value: &Value) -> Result<(i64, i64, i64), Flow> {
    let Value::Str(color) = value else {
        return Err(invalid_color_error(value));
    };
    let Some(rgb) = parse_color_16bit_any(color).map(approximate_tty_color) else {
        return Err(invalid_color_error(value));
    };
    Ok(rgb)
}

fn color_distance_metric(lhs: (i64, i64, i64), rhs: (i64, i64, i64)) -> i64 {
    // Emacs-compatible perceptual approximation (redmean) over 8-bit channels.
    let r1 = lhs.0 / 257;
    let g1 = lhs.1 / 257;
    let b1 = lhs.2 / 257;
    let r2 = rhs.0 / 257;
    let g2 = rhs.1 / 257;
    let b2 = rhs.2 / 257;

    let dr = r1 - r2;
    let dg = g1 - g2;
    let db = b1 - b2;
    let rmean = (r1 + r2) / 2;
    (((512 + rmean) * dr * dr) >> 8) + 4 * dg * dg + (((767 - rmean) * db * db) >> 8)
}

/// `(color-distance COLOR1 COLOR2 &optional FRAME METRIC-FN)` -- return a
/// perceptual distance between colors.
pub(crate) fn builtin_color_distance(args: Vec<Value>) -> EvalResult {
    expect_min_args("color-distance", &args, 2)?;
    expect_max_args("color-distance", &args, 4)?;
    expect_optional_color_distance_frame_arg(&args, 2)?;
    let lhs = parse_color_distance_input(&args[0])?;
    let rhs = parse_color_distance_input(&args[1])?;
    Ok(Value::Int(color_distance_metric(lhs, rhs)))
}

fn parse_hex_color_16bit(hex: &str) -> Option<(i64, i64, i64)> {
    match hex.len() {
        3 => {
            let r = i64::from(hex[0..1].chars().next()?.to_digit(16)? as u16);
            let g = i64::from(hex[1..2].chars().next()?.to_digit(16)? as u16);
            let b = i64::from(hex[2..3].chars().next()?.to_digit(16)? as u16);
            Some((
                r | (r << 4) | (r << 8) | (r << 12),
                g | (g << 4) | (g << 8) | (g << 12),
                b | (b << 4) | (b << 8) | (b << 12),
            ))
        }
        6 => Some((
            i64::from(u16::from_str_radix(&hex[0..2], 16).ok()?) * 257,
            i64::from(u16::from_str_radix(&hex[2..4], 16).ok()?) * 257,
            i64::from(u16::from_str_radix(&hex[4..6], 16).ok()?) * 257,
        )),
        12 => Some((
            i64::from(u16::from_str_radix(&hex[0..4], 16).ok()?),
            i64::from(u16::from_str_radix(&hex[4..8], 16).ok()?),
            i64::from(u16::from_str_radix(&hex[8..12], 16).ok()?),
        )),
        _ => None,
    }
}

fn parse_named_color_16bit(name: &str) -> Option<(i64, i64, i64)> {
    match name {
        "black" => Some((0, 0, 0)),
        "white" => Some((65535, 65535, 65535)),
        "red" => Some((65535, 0, 0)),
        "green" => Some((0, 65535, 0)),
        "blue" => Some((0, 0, 65535)),
        "yellow" => Some((65535, 65535, 0)),
        "cyan" => Some((0, 65535, 65535)),
        "magenta" => Some((65535, 0, 65535)),
        "gray" | "grey" => Some((48573, 48573, 48573)),
        "dark gray" | "dark grey" | "darkgray" | "darkgrey" => Some((43690, 43690, 43690)),
        "light gray" | "light grey" | "lightgray" | "lightgrey" => Some((55512, 55512, 55512)),
        "orange" => Some((65535, 42405, 0)),
        "orange red" | "orangered" => Some((65535, 17990, 0)),
        "pink" => Some((65535, 49344, 52171)),
        "brown" => Some((42405, 10794, 10794)),
        "purple" => Some((32896, 0, 32896)),
        _ => None,
    }
}

fn approximate_tty_color((r, g, b): (i64, i64, i64)) -> (i64, i64, i64) {
    // Emacs batch/TTY behavior is effectively a coarse 8-color quantization.
    // A narrow channel spread is treated as gray, otherwise channels are
    // quantized relative to the local min/max midpoint.
    const GRAY_BAND: i64 = 0x1111;
    const BRIGHT_THRESHOLD: i64 = 0x8888;

    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    if max - min <= GRAY_BAND {
        return if max >= BRIGHT_THRESHOLD {
            (65535, 65535, 65535)
        } else {
            (0, 0, 0)
        };
    }

    let mid = (max + min) / 2;
    (
        if r >= mid { 65535 } else { 0 },
        if g >= mid { 65535 } else { 0 },
        if b >= mid { 65535 } else { 0 },
    )
}

fn invalid_get_device_terminal_error(value: &Value) -> Flow {
    signal(
        "error",
        vec![Value::string(format!(
            "Invalid argument {} in 'get-device-terminal'",
            super::print::print_value(value)
        ))],
    )
}

fn color_device_designator_p(value: &Value) -> bool {
    match value {
        Value::Nil => true,
        other => frame_device_designator_p(other),
    }
}

fn expect_optional_color_device_arg(args: &[Value], idx: usize) -> Result<(), Flow> {
    if let Some(value) = args.get(idx) {
        if !color_device_designator_p(value) {
            return Err(invalid_get_device_terminal_error(value));
        }
    }
    Ok(())
}

/// `(defined-colors &optional FRAME)` -- return a list of defined color names.
pub(crate) fn builtin_defined_colors(args: Vec<Value>) -> EvalResult {
    expect_max_args("defined-colors", &args, 1)?;
    expect_optional_color_device_arg(&args, 0)?;
    let colors = vec![
        "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white",
    ];
    Ok(Value::list(colors.into_iter().map(Value::string).collect()))
}

/// `(face-id FACE &optional FRAME)` -- return numeric face id for known and
/// dynamically created faces.
pub(crate) fn builtin_face_id(args: Vec<Value>) -> EvalResult {
    expect_min_args("face-id", &args, 1)?;
    expect_max_args("face-id", &args, 2)?;
    if matches!(&args[0], Value::Str(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), args[0].clone()],
        ));
    }

    if let Some(name) = symbol_name_for_face_value(&args[0]) {
        if let Some(id) = face_id_for_name(&name) {
            return Ok(Value::Int(id));
        }
        if is_created_lisp_face(&name) {
            ensure_dynamic_face_id(&name);
            if let Some(id) = face_id_for_name(&name) {
                return Ok(Value::Int(id));
            }
        }
    }
    let rendered = super::print::print_value(&args[0]);
    Err(signal(
        "error",
        vec![Value::string(format!("Not a face: {rendered}"))],
    ))
}

/// `(face-font FACE &optional FRAME CHARACTER)` -- returns nil for batch
/// compatibility when the face exists.
pub(crate) fn builtin_face_font(args: Vec<Value>) -> EvalResult {
    expect_min_args("face-font", &args, 1)?;
    expect_max_args("face-font", &args, 3)?;
    match &args[0] {
        Value::Str(name) => {
            if KNOWN_FACES.contains(&name.as_str()) {
                Ok(Value::Nil)
            } else {
                let payload = if name.is_empty() {
                    Value::symbol("")
                } else {
                    Value::symbol(name.as_str())
                };
                Err(signal(
                    "error",
                    vec![Value::string("Invalid face"), payload],
                ))
            }
        }
        Value::Nil => Err(signal("error", vec![Value::string("Invalid face")])),
        Value::True | Value::Symbol(_) => {
            if let Some(name) = symbol_name_for_face_value(&args[0]) {
                if KNOWN_FACES.contains(&name.as_str()) {
                    return Ok(Value::Nil);
                }
            }
            Err(signal(
                "error",
                vec![Value::string("Invalid face"), args[0].clone()],
            ))
        }
        _ => Err(signal(
            "error",
            vec![Value::string("Invalid face"), args[0].clone()],
        )),
    }
}

/// `(internal-face-x-get-resource RESOURCE CLASS FRAME)` -- validate arguments and
/// return nil (font resource lookup is not implemented).
pub(crate) fn builtin_internal_face_x_get_resource(args: Vec<Value>) -> EvalResult {
    expect_min_args("internal-face-x-get-resource", &args, 2)?;
    expect_max_args("internal-face-x-get-resource", &args, 3)?;
    for arg in args.iter().take(2) {
        if !arg.is_string() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), arg.clone()],
            ));
        }
    }
    Ok(Value::Nil)
}

/// `(internal-set-font-selection-order ORDER)` -- validate order list shape and return nil.
pub(crate) fn builtin_internal_set_font_selection_order(args: Vec<Value>) -> EvalResult {
    expect_args("internal-set-font-selection-order", &args, 1)?;
    let order = &args[0];
    if !order.is_nil() && !matches!(order, Value::Cons(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), order.clone()],
        ));
    }

    let valid_keywords = [":width", ":height", ":weight", ":slant"];
    let valid = if let Some(values) = list_to_vec(order) {
        if values.len() == valid_keywords.len() {
            let mut seen = HashSet::new();
            values.iter().all(|value| {
                if let Value::Keyword(name) = value {
                    let key = if name.starts_with(':') {
                        name.clone()
                    } else {
                        format!(":{name}")
                    };
                    valid_keywords.contains(&key.as_str()) && seen.insert(key)
                } else {
                    false
                }
            })
        } else {
            false
        }
    } else {
        false
    };

    if valid {
        return Ok(Value::Nil);
    }

    if let Some(values) = list_to_vec(order) {
        if values.is_empty() {
            return Err(signal(
                "error",
                vec![Value::string("Invalid font sort order")],
            ));
        }
        let mut payload = vec![Value::string("Invalid font sort order")];
        payload.extend(values);
        return Err(signal("error", payload));
    }

    Err(signal(
        "error",
        vec![Value::string("Invalid font sort order"), order.clone()],
    ))
}

/// `(internal-set-alternative-font-family-alist ALIST)` -- normalize string
/// entries to symbols and return the normalized list.
pub(crate) fn builtin_internal_set_alternative_font_family_alist(args: Vec<Value>) -> EvalResult {
    expect_args("internal-set-alternative-font-family-alist", &args, 1)?;
    let entries = proper_list_to_vec_or_listp_error(&args[0])?;
    let mut normalized = Vec::with_capacity(entries.len());
    for entry in entries {
        let members = proper_list_to_vec_or_listp_error(&entry)?;
        let mut converted = Vec::with_capacity(members.len());
        for member in members {
            match member {
                Value::Str(s) => converted.push(Value::symbol((*s).clone())),
                other => {
                    return Err(signal(
                        "wrong-type-argument",
                        vec![Value::symbol("stringp"), other],
                    ));
                }
            }
        }
        normalized.push(Value::list(converted));
    }
    Ok(Value::list(normalized))
}

/// `(internal-set-alternative-font-registry-alist ALIST)` -- validate ALIST shape and
/// return it unchanged.
pub(crate) fn builtin_internal_set_alternative_font_registry_alist(args: Vec<Value>) -> EvalResult {
    expect_args("internal-set-alternative-font-registry-alist", &args, 1)?;
    let entries = proper_list_to_vec_or_listp_error(&args[0])?;
    for entry in entries {
        let _ = proper_list_to_vec_or_listp_error(&entry)?;
    }
    Ok(args[0].clone())
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // Font builtins
    // -----------------------------------------------------------------------

    #[test]
    fn fontp_on_non_font() {
        assert_eq!(builtin_fontp(vec![Value::Int(42)]).unwrap().is_nil(), true);
        assert_eq!(
            builtin_fontp(vec![Value::string("hello")])
                .unwrap()
                .is_nil(),
            true
        );
    }

    #[test]
    fn font_spec_basic() {
        let spec = builtin_font_spec(vec![
            Value::Keyword("family".to_string()),
            Value::string("Monospace"),
            Value::Keyword("size".to_string()),
            Value::Int(12),
        ])
        .unwrap();
        assert!(is_font_spec(&spec));
        assert!(builtin_fontp(vec![spec]).unwrap().is_truthy());
    }

    #[test]
    fn font_spec_odd_args_error() {
        let result = builtin_font_spec(vec![Value::Keyword("family".to_string())]);
        assert!(result.is_err());
    }

    #[test]
    fn font_get_and_put() {
        let spec = builtin_font_spec(vec![
            Value::Keyword("family".to_string()),
            Value::string("Monospace"),
        ])
        .unwrap();

        // Get existing property.
        let family =
            builtin_font_get(vec![spec.clone(), Value::Keyword("family".to_string())]).unwrap();
        assert_eq!(family.as_str(), Some("Monospace"));

        // Get missing property.
        let missing =
            builtin_font_get(vec![spec.clone(), Value::Keyword("size".to_string())]).unwrap();
        assert!(missing.is_nil());

        // Put returns VAL and mutates the original spec.
        let put_size = builtin_font_put(vec![
            spec.clone(),
            Value::Keyword("size".to_string()),
            Value::Int(14),
        ])
        .unwrap();
        assert_eq!(put_size.as_int(), Some(14));
        let size =
            builtin_font_get(vec![spec.clone(), Value::Keyword("size".to_string())]).unwrap();
        assert_eq!(size.as_int(), Some(14));

        // Overwrite existing property.
        let put_family = builtin_font_put(vec![
            spec.clone(),
            Value::Keyword("family".to_string()),
            Value::string("Serif"),
        ])
        .unwrap();
        assert_eq!(put_family.as_str(), Some("Serif"));
        let family2 =
            builtin_font_get(vec![spec.clone(), Value::Keyword("family".to_string())]).unwrap();
        assert_eq!(family2.as_str(), Some("Serif"));
    }

    #[test]
    fn font_get_symbol_key() {
        // Symbol key does not match keyword storage.
        let spec = builtin_font_spec(vec![
            Value::Keyword("weight".to_string()),
            Value::symbol("bold"),
        ])
        .unwrap();
        let weight = builtin_font_get(vec![spec, Value::symbol("weight")]).unwrap();
        assert!(weight.is_nil());
    }

    #[test]
    fn font_get_non_symbol_property_errors() {
        let spec = builtin_font_spec(vec![
            Value::Keyword("weight".to_string()),
            Value::symbol("bold"),
        ])
        .unwrap();
        let result = builtin_font_get(vec![spec, Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn font_get_non_vector() {
        // font-get on a non-font value signals wrong-type-argument.
        let result = builtin_font_get(vec![Value::Int(42), Value::Keyword("family".to_string())]);
        assert!(result.is_err());
    }

    #[test]
    fn list_fonts_returns_list_or_nil() {
        let result = builtin_list_fonts(vec![Value::vector(vec![Value::Keyword(
            FONT_SPEC_TAG.to_string(),
        )])]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn list_fonts_rejects_non_font_spec() {
        let result = builtin_list_fonts(vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_list_fonts_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let result = builtin_list_fonts_eval(
            &mut eval,
            vec![
                Value::vector(vec![Value::Keyword(FONT_SPEC_TAG.to_string())]),
                Value::Int(frame_id),
            ],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn find_font_returns_nil_for_font_spec() {
        let result = builtin_find_font(vec![Value::vector(vec![Value::Keyword(
            FONT_SPEC_TAG.to_string(),
        )])]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn find_font_rejects_non_font_spec() {
        let result = builtin_find_font(vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_find_font_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let result = builtin_find_font_eval(
            &mut eval,
            vec![
                Value::vector(vec![Value::Keyword(FONT_SPEC_TAG.to_string())]),
                Value::Int(frame_id),
            ],
        )
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn clear_font_cache_returns_nil() {
        assert!(builtin_clear_font_cache(vec![]).unwrap().is_nil());
    }

    #[test]
    fn clear_font_cache_rejects_arity() {
        assert!(builtin_clear_font_cache(vec![Value::Nil]).is_err());
    }

    #[test]
    fn clear_font_cache_resets_face_caches() {
        let face = Value::symbol("__neovm_clear_font_cache_unit_test");
        let _ = builtin_internal_make_lisp_face(vec![face.clone()]).unwrap();
        let _ = builtin_internal_set_lisp_face_attribute(vec![
            face.clone(),
            Value::Keyword(":foreground".to_string()),
            Value::string("white"),
        ])
        .unwrap();

        {
            let created = created_lisp_faces().lock().expect("poisoned");
            assert!(created.contains("__neovm_clear_font_cache_unit_test"));
        }
        {
            let state = face_attr_state().lock().expect("poisoned");
            assert!(!state.selected_overrides.is_empty());
        }

        let result = builtin_clear_font_cache(vec![]).unwrap();
        assert!(result.is_nil());

        assert!(created_lisp_faces().lock().expect("poisoned").is_empty());
        assert!(created_face_ids().lock().expect("poisoned").is_empty());
        assert_eq!(
            *next_created_face_id().lock().expect("poisoned"),
            FIRST_DYNAMIC_FACE_ID
        );
        let state = face_attr_state().lock().expect("poisoned");
        assert!(state.selected_overrides.is_empty());
        assert!(state.defaults_overrides.is_empty());
        assert!(state.selected_created.is_empty());
    }

    #[test]
    fn font_family_list_batch_returns_nil() {
        let result = builtin_font_family_list(vec![]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn font_family_list_rejects_non_nil_frame_designator() {
        let result = builtin_font_family_list(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn eval_font_family_list_accepts_live_frame_designator() {
        let mut eval = crate::elisp::Evaluator::new();
        let frame_id = crate::elisp::window_cmds::ensure_selected_frame_id(&mut eval).0 as i64;
        let result = builtin_font_family_list_eval(&mut eval, vec![Value::Int(frame_id)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn font_xlfd_name_returns_xlfd() {
        let result = builtin_font_xlfd_name(vec![Value::vector(vec![Value::Keyword(
            FONT_SPEC_TAG.to_string(),
        )])])
        .unwrap();
        assert_eq!(result.as_str(), Some("-*-*-*-*-*-*-*-*-*-*-*-*-*-*"));
    }

    #[test]
    fn font_xlfd_name_too_many_args() {
        let result = builtin_font_xlfd_name(vec![
            Value::vector(vec![Value::Keyword(FONT_SPEC_TAG.to_string())]),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "wrong-number-of-arguments"
        ));
    }

    #[test]
    fn close_font_requires_font_object() {
        let wrong_nil = builtin_close_font(vec![Value::Nil]).unwrap_err();
        match wrong_nil {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("font-object"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument, got {other:?}"),
        }

        let wrong_spec = builtin_close_font(vec![builtin_font_spec(vec![]).unwrap()]).unwrap_err();
        match wrong_spec {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data[0], Value::symbol("font-object"));
            }
            other => panic!("expected wrong-type-argument, got {other:?}"),
        }
    }

    #[test]
    fn close_font_accepts_tagged_font_object_and_checks_arity() {
        let font_obj = Value::vector(vec![Value::keyword("font-object"), Value::Int(1)]);
        assert!(builtin_close_font(vec![font_obj.clone()]).unwrap().is_nil());
        assert!(builtin_close_font(vec![font_obj, Value::Nil])
            .unwrap()
            .is_nil());

        assert!(builtin_close_font(vec![]).is_err());
        assert!(builtin_close_font(vec![Value::Nil, Value::Nil, Value::Nil]).is_err());
    }

    // -----------------------------------------------------------------------
    // Face builtins
    // -----------------------------------------------------------------------

    #[test]
    fn internal_lisp_face_p_symbol_returns_face_vector() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = builtin_internal_lisp_face_p(vec![Value::symbol("default")]).unwrap();
        let values = match result {
            Value::Vector(v) => with_heap(|h| h.get_vector(v).clone()),
            _ => panic!("expected vector"),
        };
        assert_eq!(values.len(), LISP_FACE_VECTOR_LEN);
        assert_eq!(values[0].as_symbol_name(), Some("face"));
    }

    #[test]
    fn internal_lisp_face_p_non_symbol() {
        let result = builtin_internal_lisp_face_p(vec![Value::Int(42)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_lisp_face_p_nil_returns_nil() {
        let result = builtin_internal_lisp_face_p(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_lisp_face_p_rejects_non_nil_frame_designator() {
        let result = builtin_internal_lisp_face_p(vec![Value::symbol("default"), Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_lisp_face_p_with_frame_designator_returns_resolved_vector() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = builtin_internal_lisp_face_p(vec![
            Value::symbol("default"),
            Value::Frame(FRAME_ID_BASE),
        ])
        .unwrap();
        let values = match result {
            Value::Vector(v) => with_heap(|h| h.get_vector(v).clone()),
            _ => panic!("expected vector"),
        };
        assert_eq!(values[0].as_symbol_name(), Some("face"));
        assert_eq!(values[1].as_str(), Some("default"));
        assert_eq!(values[2].as_str(), Some("default"));
        assert_eq!(values[3].as_symbol_name(), Some("normal"));
        assert_eq!(values[4].as_int(), Some(1));
        assert_eq!(values[5].as_symbol_name(), Some("normal"));
        assert_eq!(values[8].as_symbol_name(), Some("nil"));
        assert!(values[9].as_str().is_some());
        assert!(values[10].as_str().is_some());
    }

    #[test]
    fn internal_make_lisp_face_creates_symbol_visible_to_internal_lisp_face_p() {
        let name = Value::symbol("__neovm_make_face_unit_test");
        let made = builtin_internal_make_lisp_face(vec![name.clone()]).unwrap();
        assert!(matches!(made, Value::Vector(_)));
        let exists = builtin_internal_lisp_face_p(vec![name]).unwrap();
        assert!(matches!(exists, Value::Vector(_)));
    }

    #[test]
    fn internal_make_lisp_face_rejects_non_symbol_and_non_nil_frame() {
        assert!(builtin_internal_make_lisp_face(vec![Value::string("foo")]).is_err());
        assert!(
            builtin_internal_make_lisp_face(vec![Value::symbol("foo"), Value::Int(1)]).is_err()
        );
    }

    #[test]
    fn internal_copy_lisp_face_returns_to_when_frame_t() {
        let result = builtin_internal_copy_lisp_face(vec![
            Value::symbol("bold"),
            Value::symbol("my-face"),
            Value::True,
            Value::Nil,
        ])
        .unwrap();
        assert_eq!(result.as_symbol_name(), Some("my-face"));
    }

    #[test]
    fn internal_copy_lisp_face_rejects_non_t_frame_designator() {
        let result = builtin_internal_copy_lisp_face(vec![
            Value::symbol("default"),
            Value::symbol("my-face"),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_copy_lisp_face_validates_new_frame_when_frame_designator_used() {
        let frame = Value::Frame(FRAME_ID_BASE);
        let err_t = builtin_internal_copy_lisp_face(vec![
            Value::symbol("default"),
            Value::symbol("my-face"),
            frame.clone(),
            Value::True,
        ]);
        assert!(err_t.is_err());

        let err_small_int = builtin_internal_copy_lisp_face(vec![
            Value::symbol("default"),
            Value::symbol("my-face"),
            frame.clone(),
            Value::Int(1),
        ]);
        assert!(err_small_int.is_err());

        let ok = builtin_internal_copy_lisp_face(vec![
            Value::symbol("default"),
            Value::symbol("my-face"),
            frame.clone(),
            frame,
        ])
        .unwrap();
        assert_eq!(ok.as_symbol_name(), Some("my-face"));
    }

    #[test]
    fn internal_copy_lisp_face_uses_symbol_checks_before_frame_checks() {
        let result = builtin_internal_copy_lisp_face(vec![
            Value::Int(1),
            Value::symbol("my-face"),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_set_lisp_face_attribute_returns_value() {
        let face = Value::symbol("__neovm_set_attr_unit_test");
        let result = builtin_internal_set_lisp_face_attribute(vec![
            face.clone(),
            Value::Keyword("foreground".to_string()),
            Value::string("white"),
        ])
        .unwrap();
        assert_eq!(result, face);
    }

    #[test]
    fn internal_get_lisp_face_attribute_default_foreground() {
        let result = builtin_internal_get_lisp_face_attribute(vec![
            Value::symbol("default"),
            Value::Keyword(":foreground".to_string()),
        ])
        .unwrap();
        assert_eq!(result.as_str(), Some("unspecified-fg"));
    }

    #[test]
    fn internal_get_lisp_face_attribute_mode_line_returns_unspecified() {
        let result = builtin_internal_get_lisp_face_attribute(vec![
            Value::symbol("mode-line"),
            Value::Keyword(":foreground".to_string()),
        ])
        .unwrap();
        assert_eq!(result.as_symbol_name(), Some("unspecified"));
    }

    #[test]
    fn internal_get_lisp_face_attribute_defaults_frame_returns_unspecified() {
        let result = builtin_internal_get_lisp_face_attribute(vec![
            Value::symbol("default"),
            Value::Keyword(":foreground".to_string()),
            Value::True,
        ])
        .unwrap();
        assert_eq!(result.as_symbol_name(), Some("unspecified"));
    }

    #[test]
    fn internal_get_lisp_face_attribute_invalid_face_errors() {
        let result = builtin_internal_get_lisp_face_attribute(vec![
            Value::symbol("unknown-face"),
            Value::Keyword(":foreground".to_string()),
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_get_lisp_face_attribute_invalid_attr_errors() {
        let wrong_type =
            builtin_internal_get_lisp_face_attribute(vec![Value::symbol("default"), Value::Int(1)]);
        assert!(wrong_type.is_err());

        let invalid_name = builtin_internal_get_lisp_face_attribute(vec![
            Value::symbol("default"),
            Value::symbol("bogus"),
        ]);
        assert!(invalid_name.is_err());
    }

    #[test]
    fn internal_lisp_face_attribute_values_discrete_boolean_attrs() {
        let result = builtin_internal_lisp_face_attribute_values(vec![Value::Keyword(
            ":underline".to_string(),
        )])
        .unwrap();
        let vals = list_to_vec(&result).expect("list");
        assert_eq!(vals, vec![Value::True, Value::Nil]);
    }

    #[test]
    fn internal_lisp_face_attribute_values_non_discrete_attr_is_nil() {
        let result = builtin_internal_lisp_face_attribute_values(vec![Value::Keyword(
            ":weight".to_string(),
        )])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_lisp_face_attribute_values_rejects_non_symbol() {
        let result = builtin_internal_lisp_face_attribute_values(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_lisp_face_empty_p_selected_frame_default_is_not_empty() {
        let result = builtin_internal_lisp_face_empty_p(vec![Value::symbol("default")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_lisp_face_empty_p_accepts_string_face_name() {
        let result = builtin_internal_lisp_face_empty_p(vec![Value::string("default")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_lisp_face_empty_p_defaults_frame_is_empty() {
        let result =
            builtin_internal_lisp_face_empty_p(vec![Value::symbol("default"), Value::True])
                .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn internal_lisp_face_empty_p_rejects_non_nil_non_t_frame_designator() {
        let result =
            builtin_internal_lisp_face_empty_p(vec![Value::symbol("default"), Value::Int(1)]);
        assert!(result.is_err());
        let frame_result =
            builtin_internal_lisp_face_empty_p(vec![Value::symbol("default"), Value::Frame(1)]);
        assert!(frame_result.is_err());
    }

    #[test]
    fn internal_lisp_face_comparators_accept_frame_handles() {
        let frame = Value::Frame(FRAME_ID_BASE);
        let empty_result =
            builtin_internal_lisp_face_empty_p(vec![Value::symbol("default"), frame.clone()])
                .unwrap();
        assert!(empty_result.is_nil());

        let equal_result = builtin_internal_lisp_face_equal_p(vec![
            Value::symbol("default"),
            Value::symbol("mode-line"),
            frame,
        ])
        .unwrap();
        assert!(equal_result.is_nil());
    }

    #[test]
    fn internal_lisp_face_equal_p_selected_frame_distinguishes_faces() {
        let result = builtin_internal_lisp_face_equal_p(vec![
            Value::symbol("default"),
            Value::symbol("mode-line"),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_lisp_face_equal_p_defaults_frame_treats_faces_as_equal() {
        let result = builtin_internal_lisp_face_equal_p(vec![
            Value::symbol("default"),
            Value::symbol("mode-line"),
            Value::True,
        ])
        .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn internal_lisp_face_equal_p_accepts_string_face_names() {
        let result = builtin_internal_lisp_face_equal_p(vec![
            Value::string("default"),
            Value::string("default"),
        ])
        .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn internal_merge_in_global_face_rejects_non_frame_designator() {
        let result =
            builtin_internal_merge_in_global_face(vec![Value::symbol("default"), Value::Nil]);
        assert!(result.is_err());
        let frame_handle_result =
            builtin_internal_merge_in_global_face(vec![Value::symbol("default"), Value::Frame(1)]);
        assert!(frame_handle_result.is_err());
    }

    #[test]
    fn internal_merge_in_global_face_copies_defaults_into_selected_face() {
        let face = Value::symbol("__neovm_merge_face_unit_test");
        let _ = builtin_internal_make_lisp_face(vec![face.clone()]).unwrap();
        let _ = builtin_internal_set_lisp_face_attribute(vec![
            face.clone(),
            Value::Keyword("foreground".to_string()),
            Value::string("white"),
            Value::True,
        ])
        .unwrap();
        let merged =
            builtin_internal_merge_in_global_face(vec![face.clone(), Value::Frame(FRAME_ID_BASE)])
                .unwrap();
        assert!(merged.is_nil());
        let got = builtin_internal_get_lisp_face_attribute(vec![
            face.clone(),
            Value::Keyword(":foreground".to_string()),
        ])
        .unwrap();
        assert_eq!(got.as_str(), Some("white"));
    }

    #[test]
    fn internal_lisp_face_helpers_accept_frame_handles() {
        let frame = Value::Frame(FRAME_ID_BASE);

        let descriptor =
            builtin_internal_lisp_face_p(vec![Value::symbol("default"), frame.clone()]).unwrap();
        assert!(descriptor.is_vector());

        let face = Value::symbol("__neovm_face_frame_handle_unit_test");
        let made = builtin_internal_make_lisp_face(vec![face.clone(), frame.clone()]).unwrap();
        assert!(made.is_vector());

        let copied = builtin_internal_copy_lisp_face(vec![
            Value::symbol("default"),
            face.clone(),
            frame.clone(),
            Value::Nil,
        ])
        .unwrap();
        assert_eq!(copied, face);

        let set = builtin_internal_set_lisp_face_attribute(vec![
            Value::symbol("default"),
            Value::Keyword("foreground".to_string()),
            Value::string("red"),
            frame.clone(),
        ])
        .unwrap();
        assert_eq!(set, Value::symbol("default"));

        let got = builtin_internal_get_lisp_face_attribute(vec![
            Value::symbol("default"),
            Value::Keyword(":foreground".to_string()),
            frame,
        ])
        .unwrap();
        assert_eq!(got.as_str(), Some("red"));
    }

    #[test]
    fn face_attribute_relative_p_height_non_fixnum_is_relative() {
        let result = builtin_face_attribute_relative_p(vec![
            Value::Keyword("height".to_string()),
            Value::Nil,
        ])
        .unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn face_attribute_relative_p_height_fixnum_is_not_relative() {
        let result = builtin_face_attribute_relative_p(vec![
            Value::Keyword("height".to_string()),
            Value::Int(1),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn face_attribute_relative_p_non_height_attribute_is_nil() {
        let result = builtin_face_attribute_relative_p(vec![
            Value::Keyword("weight".to_string()),
            Value::symbol("foo"),
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn merge_face_attribute_non_unspecified() {
        let result = builtin_merge_face_attribute(vec![
            Value::Keyword("foreground".to_string()),
            Value::string("red"),
            Value::string("blue"),
        ])
        .unwrap();
        assert_eq!(result.as_str(), Some("red"));
    }

    #[test]
    fn merge_face_attribute_unspecified() {
        let result = builtin_merge_face_attribute(vec![
            Value::Keyword("foreground".to_string()),
            Value::symbol("unspecified"),
            Value::string("blue"),
        ])
        .unwrap();
        assert_eq!(result.as_str(), Some("blue"));
    }

    #[test]
    fn face_list_returns_known_faces() {
        let result = builtin_face_list(vec![]).unwrap();
        let faces = list_to_vec(&result).unwrap();
        let names: Vec<&str> = faces.iter().filter_map(|v| v.as_symbol_name()).collect();
        assert!(names.contains(&"default"));
        assert!(names.contains(&"bold"));
        assert!(names.contains(&"cursor"));
        assert!(names.contains(&"mode-line"));
    }

    #[test]
    fn color_defined_p_known_and_unknown() {
        let result = builtin_color_defined_p(vec![Value::string("red")]).unwrap();
        assert!(result.is_truthy());

        let missing = builtin_color_defined_p(vec![Value::string("anything")]).unwrap();
        assert!(missing.is_nil());

        let invalid_hex = builtin_color_defined_p(vec![Value::string("#ggg")]).unwrap();
        assert!(invalid_hex.is_nil());

        let non_string = builtin_color_defined_p(vec![Value::Int(1)]).unwrap();
        assert!(non_string.is_nil());
    }

    #[test]
    fn color_queries_validate_optional_device_arg() {
        assert!(builtin_color_defined_p(vec![Value::string("red"), Value::Int(1)]).is_err());
        assert!(builtin_color_values(vec![Value::string("red"), Value::Int(1)]).is_err());
        assert!(builtin_defined_colors(vec![Value::Int(1)]).is_err());
        assert!(builtin_color_defined_p(vec![Value::string("red"), Value::Frame(1)]).is_err());
        assert!(builtin_color_values(vec![Value::string("red"), Value::Frame(1)]).is_err());
        assert!(builtin_defined_colors(vec![Value::Frame(1)]).is_err());
        assert!(
            builtin_color_defined_p(vec![Value::string("red"), Value::Frame(FRAME_ID_BASE)])
                .is_ok()
        );
        assert!(
            builtin_color_values(vec![Value::string("red"), Value::Frame(FRAME_ID_BASE)]).is_ok()
        );
        assert!(builtin_defined_colors(vec![Value::Frame(FRAME_ID_BASE)]).is_ok());
    }

    #[test]
    fn color_values_named_black() {
        let result = builtin_color_values(vec![Value::string("black")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb.len(), 3);
        assert_eq!(rgb[0].as_int(), Some(0));
        assert_eq!(rgb[1].as_int(), Some(0));
        assert_eq!(rgb[2].as_int(), Some(0));
    }

    #[test]
    fn color_values_named_white() {
        let result = builtin_color_values(vec![Value::string("white")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(65535));
        assert_eq!(rgb[2].as_int(), Some(65535));
    }

    #[test]
    fn color_values_hex_rrggbb() {
        // Hex colors are approximated to terminal palette colors in batch mode.
        let result = builtin_color_values(vec![Value::string("#FF0000")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(0));
        assert_eq!(rgb[2].as_int(), Some(0));
    }

    #[test]
    fn color_values_hex_short() {
        // #F00 resolves and approximates to red.
        let result = builtin_color_values(vec![Value::string("#F00")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(0));
        assert_eq!(rgb[2].as_int(), Some(0));
    }

    #[test]
    fn color_values_hex_12digit() {
        // 12-digit hex resolves and approximates to red.
        let result = builtin_color_values(vec![Value::string("#FFFF00000000")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(0));
        assert_eq!(rgb[2].as_int(), Some(0));
    }

    #[test]
    fn color_values_unknown_returns_nil() {
        let result = builtin_color_values(vec![Value::string("nonexistent-color")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn color_values_wrong_type_returns_nil() {
        let result = builtin_color_values(vec![Value::Int(42)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn defined_colors_returns_list() {
        let result = builtin_defined_colors(vec![]).unwrap();
        assert!(result.is_list());
        assert!(!result.is_nil());
        let colors = list_to_vec(&result).expect("defined-colors list");
        assert_eq!(colors.len(), 8);
        assert_eq!(colors[0].as_str(), Some("black"));
        assert_eq!(colors[7].as_str(), Some("white"));
    }

    #[test]
    fn face_id_rejects_non_symbol_faces() {
        let result = builtin_face_id(vec![Value::symbol("default")]).unwrap();
        assert_eq!(result.as_int(), Some(0));
    }

    #[test]
    fn face_id_known_faces_use_oracle_ids() {
        let bold = builtin_face_id(vec![Value::symbol("bold")]).unwrap();
        assert_eq!(bold.as_int(), Some(1));
        let mode_line = builtin_face_id(vec![Value::symbol("mode-line")]).unwrap();
        assert_eq!(mode_line.as_int(), Some(25));
    }

    #[test]
    fn face_id_accepts_optional_frame_argument() {
        let result = builtin_face_id(vec![Value::symbol("default"), Value::Nil]).unwrap();
        assert_eq!(result.as_int(), Some(0));
    }

    #[test]
    fn face_id_assigns_dynamic_id_for_created_faces() {
        let face = Value::symbol("__neovm_face_id_dynamic_unit_test");
        let _ = builtin_internal_make_lisp_face(vec![face.clone()]).unwrap();
        let first = builtin_face_id(vec![face.clone()]).unwrap();
        let second = builtin_face_id(vec![face]).unwrap();
        assert_eq!(first, second);
    }

    #[test]
    fn face_id_rejects_invalid_face() {
        let result = builtin_face_id(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn face_font_returns_nil_for_known_faces() {
        let result = builtin_face_font(vec![Value::symbol("default")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn face_font_accepts_known_string_face() {
        let result = builtin_face_font(vec![Value::string("default")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn face_font_ignores_optional_arguments_for_known_face() {
        let result =
            builtin_face_font(vec![Value::symbol("default"), Value::Nil, Value::True]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn face_font_rejects_invalid_face() {
        let result = builtin_face_font(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_face_x_get_resource_returns_nil_for_string_args() {
        let result = builtin_internal_face_x_get_resource(vec![
            Value::string("font"),
            Value::string("Font"),
            Value::Nil,
        ])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_face_x_get_resource_validates_string_args_and_arity() {
        assert!(builtin_internal_face_x_get_resource(vec![]).is_err());
        assert!(builtin_internal_face_x_get_resource(vec![Value::Nil]).is_err());
        assert!(
            builtin_internal_face_x_get_resource(vec![Value::Nil, Value::string("Font")]).is_err()
        );
        assert!(
            builtin_internal_face_x_get_resource(vec![Value::string("font"), Value::Nil]).is_err()
        );
    }

    #[test]
    fn internal_set_font_selection_order_accepts_valid_order() {
        let result = builtin_internal_set_font_selection_order(vec![Value::list(vec![
            Value::Keyword(":width".to_string()),
            Value::Keyword(":height".to_string()),
            Value::Keyword(":weight".to_string()),
            Value::Keyword(":slant".to_string()),
        ])])
        .unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_set_font_selection_order_rejects_invalid_order() {
        let result =
            builtin_internal_set_font_selection_order(vec![Value::list(vec![Value::symbol("x")])]);
        assert!(result.is_err());
    }

    #[test]
    fn internal_set_alternative_font_family_alist_returns_converted_list() {
        let result = builtin_internal_set_alternative_font_family_alist(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_set_alternative_font_family_alist_converts_strings_to_symbols() {
        let input = Value::list(vec![Value::list(vec![
            Value::string("Foo"),
            Value::string("Bar"),
        ])]);
        let result = builtin_internal_set_alternative_font_family_alist(vec![input]).unwrap();
        let outer = list_to_vec(&result).expect("outer list");
        let inner = list_to_vec(&outer[0]).expect("inner list");
        assert_eq!(inner[0].as_symbol_name(), Some("Foo"));
        assert_eq!(inner[1].as_symbol_name(), Some("Bar"));
    }

    #[test]
    fn internal_set_alternative_font_registry_alist_returns_nil_or_value() {
        let result =
            builtin_internal_set_alternative_font_registry_alist(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn internal_set_alternative_font_registry_alist_preserves_values() {
        let input = Value::list(vec![Value::list(vec![Value::Int(1), Value::Int(2)])]);
        let result =
            builtin_internal_set_alternative_font_registry_alist(vec![input.clone()]).unwrap();
        assert_eq!(result, input);
    }

    // -----------------------------------------------------------------------
    // Arity checks
    // -----------------------------------------------------------------------

    #[test]
    fn fontp_too_many_args() {
        let result = builtin_fontp(vec![Value::Nil, Value::Nil, Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn fontp_no_args() {
        let result = builtin_fontp(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn font_get_wrong_arity() {
        assert!(builtin_font_get(vec![Value::Nil]).is_err());
        assert!(builtin_font_get(vec![Value::Nil, Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn font_put_wrong_arity() {
        assert!(builtin_font_put(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn face_attribute_relative_p_wrong_arity() {
        assert!(builtin_face_attribute_relative_p(vec![Value::Nil]).is_err());
    }

    #[test]
    fn merge_face_attribute_wrong_arity() {
        assert!(builtin_merge_face_attribute(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn color_values_case_insensitive() {
        let result = builtin_color_values(vec![Value::string("RED")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(0));
        assert_eq!(rgb[2].as_int(), Some(0));
    }

    #[test]
    fn color_values_hex_lowercase() {
        let result = builtin_color_values(vec![Value::string("#ff8000")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        // #ff8000 approximates to yellow in the terminal palette.
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(65535));
        assert_eq!(rgb[2].as_int(), Some(0));
    }

    #[test]
    fn color_values_invalid_hex_returns_nil() {
        let result = builtin_color_values(vec![Value::string("#ggg")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn color_values_from_color_spec_semantics() {
        let rgb_short = list_to_vec(
            &builtin_color_values_from_color_spec(vec![Value::string("#000")]).unwrap(),
        )
        .unwrap();
        assert_eq!(rgb_short, vec![Value::Int(0), Value::Int(0), Value::Int(0)]);

        let rgb_12 = list_to_vec(
            &builtin_color_values_from_color_spec(vec![Value::string("#111122223333")]).unwrap(),
        )
        .unwrap();
        assert_eq!(
            rgb_12,
            vec![Value::Int(4369), Value::Int(8738), Value::Int(13107)]
        );

        assert!(
            builtin_color_values_from_color_spec(vec![Value::string("#abcd")])
                .unwrap()
                .is_nil()
        );
        assert!(
            builtin_color_values_from_color_spec(vec![Value::string("bogus")])
                .unwrap()
                .is_nil()
        );

        let type_err = builtin_color_values_from_color_spec(vec![Value::Int(1)])
            .expect_err("color-values-from-color-spec should enforce stringp");
        match type_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn color_gray_and_supported_semantics() {
        assert!(builtin_color_gray_p(vec![Value::string("#000000")])
            .unwrap()
            .is_truthy());
        assert!(builtin_color_gray_p(vec![Value::string("#808080")])
            .unwrap()
            .is_truthy());
        assert!(builtin_color_gray_p(vec![Value::string("#ff0000")])
            .unwrap()
            .is_nil());
        assert!(
            builtin_color_gray_p(vec![Value::string("#fff"), Value::Nil])
                .unwrap()
                .is_truthy()
        );

        let gray_color_type = builtin_color_gray_p(vec![Value::Int(1)])
            .expect_err("color-gray-p should enforce stringp");
        match gray_color_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let gray_frame_type = builtin_color_gray_p(vec![Value::string("#fff"), Value::Int(0)])
            .expect_err("color-gray-p should validate FRAME");
        match gray_frame_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("framep"), Value::Int(0)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        assert!(builtin_color_supported_p(vec![Value::string("#123456")])
            .unwrap()
            .is_truthy());
        assert!(
            builtin_color_supported_p(vec![Value::string("#fff"), Value::Nil, Value::True])
                .unwrap()
                .is_truthy()
        );
        assert!(
            builtin_color_supported_p(vec![Value::string("bogus"), Value::Nil, Value::Nil])
                .unwrap()
                .is_nil()
        );

        let supported_type = builtin_color_supported_p(vec![Value::Int(1)])
            .expect_err("color-supported-p should enforce stringp");
        match supported_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let supported_frame_type =
            builtin_color_supported_p(vec![Value::string("#fff"), Value::Int(1)])
                .expect_err("color-supported-p should validate FRAME");
        match supported_frame_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("framep"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn color_distance_semantics() {
        let black_white =
            builtin_color_distance(vec![Value::string("#000"), Value::string("#fff")])
                .expect("color-distance should evaluate");
        match black_white {
            Value::Int(n) => assert!(n > 0),
            other => panic!("expected integer distance, got {other:?}"),
        }

        assert_eq!(
            builtin_color_distance(vec![Value::string("#000"), Value::string("#000")]).unwrap(),
            Value::Int(0)
        );

        // Both colors collapse to black in tty-approx mode.
        assert_eq!(
            builtin_color_distance(vec![Value::string("#000"), Value::string("#111")]).unwrap(),
            Value::Int(0)
        );
    }

    #[test]
    fn color_distance_errors_match_oracle_shape() {
        let invalid_left =
            builtin_color_distance(vec![Value::string("#00"), Value::string("#fff")]).unwrap_err();
        match invalid_left {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Invalid color"), Value::string("#00")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let invalid_type = builtin_color_distance(vec![Value::Int(1), Value::string("#fff")])
            .expect_err("color-distance should signal invalid color for non-string args");
        match invalid_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string("Invalid color"), Value::Int(1)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let frame_err = builtin_color_distance(vec![
            Value::string("#000"),
            Value::string("#fff"),
            Value::True,
        ])
        .expect_err("color-distance should validate optional FRAME");
        match frame_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("frame-live-p"), Value::True]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn color_values_dark_gray_approximates_to_white() {
        let result = builtin_color_values(vec![Value::string("DarkGray")]).unwrap();
        let rgb = list_to_vec(&result).unwrap();
        assert_eq!(rgb[0].as_int(), Some(65535));
        assert_eq!(rgb[1].as_int(), Some(65535));
        assert_eq!(rgb[2].as_int(), Some(65535));
    }
}
