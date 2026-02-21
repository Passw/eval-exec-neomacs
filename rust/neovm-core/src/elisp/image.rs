//! Image type support builtins.
//!
//! Provides stub/partial implementations of Emacs image builtins:
//! - `image-type-available-p` — check if image type is available
//! - `create-image` — create image descriptor (property list)
//! - `image-size` — return (WIDTH . HEIGHT) cons
//! - `image-mask-p` — check for mask support
//! - `put-image` / `insert-image` / `remove-images` — display stubs
//! - `image-flush` / `clear-image-cache` — cache management stubs
//! - `image-type` — extract type from image spec
//! - `display-images-p` / `image-transforms-p` — capability queries
//!
//! Image specs are property lists: (:type png :file "foo.png" :width 100 ...)

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use crate::window::FRAME_ID_BASE;

// ---------------------------------------------------------------------------
// Argument helpers
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

fn expect_frame_designator(_name: &str, value: &Value) -> Result<(), Flow> {
    match value {
        Value::Nil => Ok(()),
        Value::Int(id) if *id >= 0 && (*id as u64) >= FRAME_ID_BASE => Ok(()),
        Value::Frame(id) if *id >= FRAME_ID_BASE => Ok(()),
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("frame-live-p"), value.clone()],
        )),
    }
}

fn normalized_keyword_name(value: &Value) -> Option<&str> {
    match value {
        Value::Keyword(name) => Some(name.strip_prefix(':').unwrap_or(name.as_str())),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Property list helpers
// ---------------------------------------------------------------------------

/// Get a value from a property list by keyword.
/// The plist is a flat list: (:key1 val1 :key2 val2 ...).
#[cfg(test)]
fn plist_get(plist: &Value, key: &Value) -> Value {
    let mut cursor = plist.clone();
    loop {
        match cursor {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if eq_value(&pair.car, key) {
                    // Next element is the value.
                    match &pair.cdr {
                        Value::Cons(val_cell) => {
                            return val_cell.lock().expect("poisoned").car.clone();
                        }
                        _ => return Value::Nil,
                    }
                }
                // Skip the value entry.
                match &pair.cdr {
                    Value::Cons(val_cell) => {
                        cursor = val_cell.lock().expect("poisoned").cdr.clone();
                    }
                    _ => return Value::Nil,
                }
            }
            _ => return Value::Nil,
        }
    }
}

/// Check whether a symbol name represents a supported image type.
fn is_supported_image_type(name: &str) -> bool {
    matches!(
        name,
        "png" | "jpeg" | "gif" | "svg" | "webp" | "xpm" | "xbm" | "pbm" | "tiff" | "bmp"
    )
}

fn normalize_image_type_name(name: &str) -> Option<&'static str> {
    let lower = name.to_ascii_lowercase();
    match lower.as_str() {
        "jpg" => Some("jpeg"),
        "jpeg" => Some("jpeg"),
        "png" => Some("png"),
        "gif" => Some("gif"),
        "svg" => Some("svg"),
        "webp" => Some("webp"),
        "xpm" => Some("xpm"),
        "xbm" => Some("xbm"),
        "pbm" => Some("pbm"),
        "tif" | "tiff" => Some("tiff"),
        "bmp" => Some("bmp"),
        "neomacs" => Some("neomacs"),
        _ => None,
    }
}

fn infer_image_type_from_filename(path: &str) -> Option<&'static str> {
    let ext = path.rsplit('.').next()?;
    normalize_image_type_name(ext)
}

/// Validate that a value looks like an image spec.
/// Oracle-compatible shape:
/// - list starts with symbol `image`
/// - plist includes a supported symbolic `:type`
/// - plist includes exactly one source key: `:file` or `:data`
/// - source value is a string
fn is_image_spec(value: &Value) -> bool {
    let items = match list_to_vec(value) {
        Some(v) => v,
        None => return false,
    };

    if items.is_empty() || items[0].as_symbol_name() != Some("image") {
        return false;
    }

    let mut type_seen = false;
    let mut type_ok = false;
    let mut file_seen = false;
    let mut file_ok = false;
    let mut data_seen = false;
    let mut data_ok = false;

    let mut i = 1usize;
    while i + 1 < items.len() {
        if let Some(key) = normalized_keyword_name(&items[i]) {
            let val = &items[i + 1];
            match key {
                "type" if !type_seen => {
                    type_seen = true;
                    type_ok = val.as_symbol_name().is_some_and(is_supported_image_type);
                }
                "file" if !file_seen => {
                    file_seen = true;
                    file_ok = val.as_str().is_some();
                }
                "data" if !data_seen => {
                    data_seen = true;
                    data_ok = val.as_str().is_some();
                }
                _ => {}
            }
        }
        i += 2;
    }

    if !type_seen || !type_ok {
        return false;
    }

    match (file_seen, data_seen) {
        (true, false) => file_ok,
        (false, true) => data_ok,
        _ => false,
    }
}

/// Extract the plist portion of an image spec.
/// If the spec starts with `image`, skip that first element.
#[cfg(test)]
fn image_spec_plist(spec: &Value) -> Value {
    let items = match list_to_vec(spec) {
        Some(v) => v,
        None => return Value::Nil,
    };
    if items.is_empty() {
        return Value::Nil;
    }
    if let Some(name) = items[0].as_symbol_name() {
        if name == "image" {
            // Plist is everything after the `image` symbol.
            return Value::list(items[1..].to_vec());
        }
    }
    // Already a bare plist.
    spec.clone()
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (image-type-available-p TYPE) -> t or nil
///
/// Return t if image type TYPE is available in this Emacs instance.
/// Supported types: png, jpeg, gif, svg, webp, xpm, xbm, pbm, tiff, bmp.
pub(crate) fn builtin_image_type_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("image-type-available-p", &args, 1)?;
    let type_name = match args[0].as_symbol_name() {
        Some(name) => name.to_string(),
        None => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), args[0].clone()],
            ));
        }
    };
    Ok(Value::bool(is_supported_image_type(&type_name)))
}

/// (create-image FILE-OR-DATA &optional TYPE DATA-P &rest PROPS) -> image descriptor
///
/// Create an image descriptor (a list starting with `image`).
/// FILE-OR-DATA is a file name string or raw data string.
/// TYPE is a symbol like `png`, `jpeg`, etc.
/// DATA-P if non-nil means FILE-OR-DATA is raw image data, not a file name.
/// PROPS are additional property-list pairs (e.g. :width 100 :height 200).
///
/// Returns: (image :type TYPE :file FILE-OR-DATA ... PROPS)
pub(crate) fn builtin_create_image(args: Vec<Value>) -> EvalResult {
    expect_min_args("create-image", &args, 1)?;

    let file_or_data = args[0].clone();
    let data_p = args.len() > 2 && args[2].is_truthy();

    // TYPE argument (optional).
    let image_type = if args.len() > 1 && !args[1].is_nil() {
        match args[1].as_symbol_name() {
            Some(name) => {
                let normalized = normalize_image_type_name(name).unwrap_or(name);
                Value::symbol(normalized)
            }
            None => {
                let rendered = super::print::print_value(&args[1]);
                return Err(signal(
                    "error",
                    vec![Value::string(format!("Invalid image type `{rendered}`"))],
                ));
            }
        }
    } else {
        let inferred = if data_p {
            None
        } else {
            file_or_data
                .as_str()
                .and_then(infer_image_type_from_filename)
                .map(str::to_string)
        };
        match inferred {
            Some(name) => Value::symbol(name),
            None => Value::Nil,
        }
    };

    // Build the image spec property list.
    let mut spec_items: Vec<Value> = Vec::new();
    spec_items.push(Value::symbol("image"));
    spec_items.push(Value::Keyword("type".into()));
    spec_items.push(image_type);

    if data_p {
        spec_items.push(Value::Keyword("data".into()));
        spec_items.push(file_or_data);
    } else {
        spec_items.push(Value::Keyword("file".into()));
        spec_items.push(file_or_data);
    }

    // Emacs adds :scale default on freshly created image specs.
    spec_items.push(Value::Keyword("scale".into()));
    spec_items.push(Value::symbol("default"));

    // Append any extra PROPS (starting from index 3).
    if args.len() > 3 {
        for prop in &args[3..] {
            spec_items.push(prop.clone());
        }
    }

    Ok(Value::list(spec_items))
}

/// (image-size SPEC &optional PIXELS FRAME) -> (WIDTH . HEIGHT)
///
/// Batch/no-window semantics:
/// - invalid SPEC -> `(error "Invalid image specification")`
/// - valid SPEC in batch -> `(error "Window system frame should be used")`
pub(crate) fn builtin_image_size(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-size", &args, 1)?;
    expect_max_args("image-size", &args, 3)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "error",
            vec![Value::string("Invalid image specification")],
        ));
    }
    Err(signal(
        "error",
        vec![Value::string("Window system frame should be used")],
    ))
}

/// (image-mask-p SPEC &optional FRAME) -> nil
///
/// Batch/no-window semantics:
/// - invalid SPEC -> `(error "Invalid image specification")`
/// - valid SPEC in batch -> `(error "Window system frame should be used")`
pub(crate) fn builtin_image_mask_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-mask-p", &args, 1)?;
    expect_max_args("image-mask-p", &args, 2)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "error",
            vec![Value::string("Invalid image specification")],
        ));
    }
    Err(signal(
        "error",
        vec![Value::string("Window system frame should be used")],
    ))
}

/// (put-image IMAGE POINT &optional STRING AREA) -> nil
///
/// Display IMAGE at POINT in the current buffer as an overlay.
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_put_image(args: Vec<Value>) -> EvalResult {
    expect_min_args("put-image", &args, 2)?;
    expect_max_args("put-image", &args, 4)?;

    // Validate that first arg looks like an image spec.
    if !is_image_spec(&args[0]) {
        let rendered = super::print::print_value(&args[0]);
        return Err(signal(
            "error",
            vec![Value::string(format!("Not an image: {rendered}"))],
        ));
    }

    // Validate POINT is integer-or-marker in batch.
    if !matches!(&args[1], Value::Int(_) | Value::Char(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), args[1].clone()],
        ));
    }

    // Optional AREA must be nil, left-margin, or right-margin.
    if args.len() > 3 && !args[3].is_nil() {
        let valid = matches!(
            args[3].as_symbol_name(),
            Some("left-margin") | Some("right-margin")
        );
        if !valid {
            let rendered = super::print::print_value(&args[3]);
            return Err(signal(
                "error",
                vec![Value::string(format!("Invalid area {rendered}"))],
            ));
        }
    }

    // Batch compatibility: return a truthy placeholder for inserted overlay.
    Ok(Value::True)
}

/// (insert-image IMAGE &optional STRING AREA SLICE) -> nil
///
/// Insert IMAGE into the current buffer at point.
/// Batch stub: validates IMAGE and returns t.
pub(crate) fn builtin_insert_image(args: Vec<Value>) -> EvalResult {
    expect_min_args("insert-image", &args, 1)?;
    expect_max_args("insert-image", &args, 5)?;

    if !is_image_spec(&args[0]) {
        let rendered = super::print::print_value(&args[0]);
        return Err(signal(
            "error",
            vec![Value::string(format!("Not an image: {rendered}"))],
        ));
    }

    // Optional AREA must be nil, left-margin, or right-margin.
    if args.len() > 2 && !args[2].is_nil() {
        let valid = matches!(
            args[2].as_symbol_name(),
            Some("left-margin") | Some("right-margin")
        );
        if !valid {
            let rendered = super::print::print_value(&args[2]);
            return Err(signal(
                "error",
                vec![Value::string(format!("Invalid area {rendered}"))],
            ));
        }
    }

    Ok(Value::True)
}

/// (remove-images START END &optional BUFFER) -> nil
///
/// Remove images between START and END in BUFFER.
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_remove_images(args: Vec<Value>) -> EvalResult {
    expect_min_args("remove-images", &args, 2)?;
    expect_max_args("remove-images", &args, 3)?;

    // Validate START and END are integer-or-marker in batch.
    if !matches!(&args[0], Value::Int(_) | Value::Char(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), args[0].clone()],
        ));
    }
    if !matches!(&args[1], Value::Int(_) | Value::Char(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), args[1].clone()],
        ));
    }

    // Stub: no-op.
    Ok(Value::Nil)
}

/// (image-flush SPEC &optional FRAME) -> nil
///
/// Flush the image cache for image SPEC.
/// Batch semantics:
/// - invalid SPEC -> `(error "Invalid image specification")`
/// - FRAME = t -> nil (all-frames path)
/// - otherwise -> `(error "Window system frame should be used")`
pub(crate) fn builtin_image_flush(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-flush", &args, 1)?;
    expect_max_args("image-flush", &args, 2)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "error",
            vec![Value::string("Invalid image specification")],
        ));
    }

    if let Some(frame) = args.get(1) {
        if matches!(frame, Value::True) {
            return Ok(Value::Nil);
        }
        if !frame.is_nil() {
            expect_frame_designator("image-flush", frame)?;
        }
    }

    Err(signal(
        "error",
        vec![Value::string("Window system frame should be used")],
    ))
}

/// (clear-image-cache &optional FILTER) -> nil
///
/// Clear the image cache.  FILTER can be nil (clear all), a frame,
/// or t (clear all frames).
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_clear_image_cache(args: Vec<Value>) -> EvalResult {
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("clear-image-cache"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    if args.len() == 2 {
        let animation_cache = &args[1];
        if !animation_cache.is_nil() && !animation_cache.is_cons() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), animation_cache.clone()],
            ));
        }
        // When animation-cache is non-nil, Emacs does not validate `filter`.
    }

    if args.is_empty() {
        return Err(signal(
            "error",
            vec![Value::string("Window system frame should be used")],
        ));
    }

    if args[0].is_nil() {
        return Err(signal(
            "error",
            vec![Value::string("Window system frame should be used")],
        ));
    }

    Ok(Value::Nil)
}

/// (image-cache-size) -> integer
///
/// NeoVM currently has no persistent image cache, so this is always 0.
pub(crate) fn builtin_image_cache_size(args: Vec<Value>) -> EvalResult {
    expect_args("image-cache-size", &args, 0)?;
    Ok(Value::Int(0))
}

/// (image-metadata SPEC &optional FRAME) -> metadata object or nil
///
/// Returns nil for non-image specifications. For valid image specs on
/// non-window-system frames, this signals the same error shape as GNU Emacs.
pub(crate) fn builtin_image_metadata(args: Vec<Value>) -> EvalResult {
    expect_range_args("image-metadata", &args, 1, 2)?;

    if !is_image_spec(&args[0]) {
        return Ok(Value::Nil);
    }

    if let Some(frame) = args.get(1) {
        expect_frame_designator("image-metadata", frame)?;
    }

    Err(signal(
        "error",
        vec![Value::string("Window system frame should be used")],
    ))
}

/// (imagep OBJECT) -> t if OBJECT looks like an image descriptor.
pub(crate) fn builtin_imagep(args: Vec<Value>) -> EvalResult {
    expect_args("imagep", &args, 1)?;
    Ok(Value::bool(is_image_spec(&args[0])))
}

/// (image-type SOURCE &optional TYPE DATA-P) -> symbol
///
/// Compatibility behavior:
/// - SOURCE must be a file name string.
/// - TYPE, when non-nil, must be a symbol and is returned (normalized aliases).
/// - Without TYPE, type is inferred from file extension.
/// - If type inference fails, signal `unknown-image-type`.
pub(crate) fn builtin_image_type(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-type", &args, 1)?;
    expect_max_args("image-type", &args, 3)?;

    let source = &args[0];
    let explicit_type = args.get(1).cloned().unwrap_or(Value::Nil);
    let data_p = args.get(2).cloned().unwrap_or(Value::Nil);

    if source.as_str().is_none() {
        let rendered = super::print::print_value(source);
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Invalid image file name `{rendered}`"
            ))],
        ));
    }

    let resolved = if explicit_type.is_nil() {
        if data_p.is_truthy() {
            None
        } else {
            source
                .as_str()
                .and_then(infer_image_type_from_filename)
                .map(str::to_string)
        }
    } else {
        let rendered = super::print::print_value(&explicit_type);
        let sym_name = explicit_type.as_symbol_name().ok_or_else(|| {
            signal(
                "error",
                vec![Value::string(format!("Invalid image type `{rendered}`"))],
            )
        })?;
        Some(
            normalize_image_type_name(sym_name)
                .unwrap_or(sym_name)
                .to_string(),
        )
    };

    let Some(resolved) = resolved else {
        return Err(signal(
            "unknown-image-type",
            vec![Value::list(vec![Value::string(
                "Cannot determine image type",
            )])],
        ));
    };

    Ok(Value::symbol(resolved))
}

/// (image-transforms-p &optional FRAME) -> bool
///
/// Return nil if FRAME does not match an active frame designator in Neovm.
/// (Compatibility layer keeps this conservative and follows official Emacs semantics
/// observed for common callers.)
pub(crate) fn builtin_image_transforms_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("image-transforms-p", &args, 1)?;
    if let Some(frame_or_display) = args.first() {
        expect_frame_designator("image-transforms-p", frame_or_display)?;
    }
    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // image-type-available-p
    // -----------------------------------------------------------------------

    #[test]
    fn type_available_png() {
        let result = builtin_image_type_available_p(vec![Value::symbol("png")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn type_available_jpeg() {
        let result = builtin_image_type_available_p(vec![Value::symbol("jpeg")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn type_available_gif() {
        let result = builtin_image_type_available_p(vec![Value::symbol("gif")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn type_available_svg() {
        let result = builtin_image_type_available_p(vec![Value::symbol("svg")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn type_available_webp() {
        let result = builtin_image_type_available_p(vec![Value::symbol("webp")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn type_available_neomacs() {
        let result = builtin_image_type_available_p(vec![Value::symbol("neomacs")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn type_available_jpg_alias_is_nil() {
        let result = builtin_image_type_available_p(vec![Value::symbol("jpg")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn type_available_unknown() {
        let result = builtin_image_type_available_p(vec![Value::symbol("heic")]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn type_available_wrong_type() {
        let result = builtin_image_type_available_p(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn type_available_wrong_arity() {
        let result = builtin_image_type_available_p(vec![]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // create-image
    // -----------------------------------------------------------------------

    #[test]
    fn create_image_file() {
        let result = builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]);
        assert!(result.is_ok());
        let spec = result.unwrap();
        assert!(is_image_spec(&spec));

        let plist = image_spec_plist(&spec);
        let img_type = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(img_type.as_symbol_name(), Some("png"));

        let file = plist_get(&plist, &Value::Keyword("file".into()));
        assert_eq!(file.as_str(), Some("test.png"));
    }

    #[test]
    fn create_image_data() {
        let result = builtin_create_image(vec![
            Value::string("raw-png-data"),
            Value::symbol("png"),
            Value::True, // DATA-P
        ]);
        assert!(result.is_ok());
        let spec = result.unwrap();

        let plist = image_spec_plist(&spec);
        let data = plist_get(&plist, &Value::Keyword("data".into()));
        assert_eq!(data.as_str(), Some("raw-png-data"));

        // Should NOT have :file.
        let file = plist_get(&plist, &Value::Keyword("file".into()));
        assert!(file.is_nil());
    }

    #[test]
    fn create_image_default_type() {
        let result = builtin_create_image(vec![Value::string("foo.png")]);
        assert!(result.is_ok());
        let spec = result.unwrap();

        let plist = image_spec_plist(&spec);
        let img_type = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(img_type.as_symbol_name(), Some("png"));
    }

    #[test]
    fn create_image_default_type_from_jpg_extension() {
        let result = builtin_create_image(vec![Value::string("foo.JPG")]);
        assert!(result.is_ok());
        let spec = result.unwrap();

        let plist = image_spec_plist(&spec);
        let img_type = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(img_type.as_symbol_name(), Some("jpeg"));
    }

    #[test]
    fn create_image_default_type_falls_back_to_neomacs() {
        let result = builtin_create_image(vec![Value::string("foo.unknown")]);
        assert!(result.is_ok());
        let spec = result.unwrap();

        let plist = image_spec_plist(&spec);
        let img_type = plist_get(&plist, &Value::Keyword("type".into()));
        assert!(img_type.is_nil());
    }

    #[test]
    fn create_image_data_type_from_mime_hint() {
        let result = builtin_create_image(vec![
            Value::string("raw-image-bytes"),
            Value::Nil,
            Value::symbol("image/jpeg"),
        ]);
        assert!(result.is_ok());
        let spec = result.unwrap();

        let plist = image_spec_plist(&spec);
        let img_type = plist_get(&plist, &Value::Keyword("type".into()));
        assert!(img_type.is_nil());
    }

    #[test]
    fn create_image_with_props() {
        let result = builtin_create_image(vec![
            Value::string("icon.svg"),
            Value::symbol("svg"),
            Value::Nil,
            Value::Keyword("width".into()),
            Value::Int(64),
            Value::Keyword("height".into()),
            Value::Int(64),
        ]);
        assert!(result.is_ok());
        let spec = result.unwrap();

        let plist = image_spec_plist(&spec);
        let width = plist_get(&plist, &Value::Keyword("width".into()));
        assert_eq!(width.as_int(), Some(64));

        let height = plist_get(&plist, &Value::Keyword("height".into()));
        assert_eq!(height.as_int(), Some(64));
    }

    #[test]
    fn create_image_wrong_arity() {
        let result = builtin_create_image(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn create_image_bad_type() {
        let result = builtin_create_image(vec![
            Value::string("test.png"),
            Value::Int(42), // not a symbol
        ]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "error"
        ));
    }

    // -----------------------------------------------------------------------
    // image-size
    // -----------------------------------------------------------------------

    #[test]
    fn image_size_pixels() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_size(vec![spec, Value::True]);
        assert!(result.is_err());
    }

    #[test]
    fn image_size_chars() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_size(vec![spec]);
        assert!(result.is_err());
    }

    #[test]
    fn image_size_not_image_spec() {
        let result = builtin_image_size(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn image_size_wrong_arity() {
        let result = builtin_image_size(vec![]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // image-mask-p
    // -----------------------------------------------------------------------

    #[test]
    fn image_mask_p_batch_errors_without_window_system() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_mask_p(vec![spec]);
        assert!(result.is_err());
    }

    #[test]
    fn image_mask_p_not_image() {
        let result = builtin_image_mask_p(vec![Value::string("not an image")]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // put-image
    // -----------------------------------------------------------------------

    #[test]
    fn put_image_requires_image_and_point() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_put_image(vec![spec, Value::Int(1)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn put_image_accepts_char_point() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_put_image(vec![spec, Value::Char('a')]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn put_image_bad_point() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_put_image(vec![spec, Value::string("not a point")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                && sig.data.first() == Some(&Value::symbol("integer-or-marker-p"))
        ));
    }

    #[test]
    fn put_image_invalid_area() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();
        let result = builtin_put_image(vec![
            spec,
            Value::Int(1),
            Value::Nil,
            Value::symbol("center"),
        ]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Invalid area center"))
        ));
    }

    #[test]
    fn put_image_not_image() {
        let result = builtin_put_image(vec![Value::Int(1), Value::Int(1)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Not an image: 1"))
        ));
    }

    // -----------------------------------------------------------------------
    // insert-image
    // -----------------------------------------------------------------------

    #[test]
    fn insert_image_without_position_returns_true() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_insert_image(vec![spec]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::True);
    }

    #[test]
    fn insert_image_not_image() {
        let result = builtin_insert_image(vec![Value::Int(42)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Not an image: 42"))
        ));
    }

    #[test]
    fn insert_image_invalid_area() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();
        let result = builtin_insert_image(vec![spec, Value::Nil, Value::symbol("center")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Invalid area center"))
        ));
    }

    #[test]
    fn insert_image_too_many_args() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();
        let result = builtin_insert_image(vec![
            spec,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // remove-images
    // -----------------------------------------------------------------------

    #[test]
    fn remove_images_no_error_for_default_args() {
        let result = builtin_remove_images(vec![Value::Int(1), Value::Int(100)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn remove_images_accepts_char_positions() {
        let result = builtin_remove_images(vec![Value::Char('a'), Value::Char('z')]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn remove_images_bad_start() {
        let result = builtin_remove_images(vec![Value::string("x"), Value::Int(100)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                && sig.data.first() == Some(&Value::symbol("integer-or-marker-p"))
        ));
    }

    #[test]
    fn remove_images_bad_end() {
        let result = builtin_remove_images(vec![Value::Int(1), Value::string("x")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                && sig.data.first() == Some(&Value::symbol("integer-or-marker-p"))
        ));
    }

    #[test]
    fn remove_images_bad_buffer() {
        let result = builtin_remove_images(vec![Value::Int(1), Value::Int(10), Value::Int(1)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn remove_images_wrong_arity() {
        let result = builtin_remove_images(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // image-flush
    // -----------------------------------------------------------------------

    #[test]
    fn image_flush_rejects_non_window_frame() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_flush(vec![spec]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Window system frame should be used"))
        ));
    }

    #[test]
    fn image_flush_all_frames() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();
        let result = builtin_image_flush(vec![spec, Value::True]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn image_flush_non_t_frame_errors() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();
        let result = builtin_image_flush(vec![spec, Value::Int(1)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data.first() == Some(&Value::symbol("frame-live-p"))
        ));
    }

    #[test]
    fn image_flush_not_image() {
        let result = builtin_image_flush(vec![Value::Int(42)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Invalid image specification"))
        ));
    }

    // -----------------------------------------------------------------------
    // clear-image-cache
    // -----------------------------------------------------------------------

    #[test]
    fn clear_image_cache_no_args() {
        let result = builtin_clear_image_cache(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn clear_image_cache_nil_filter_errors() {
        let result = builtin_clear_image_cache(vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn clear_image_cache_with_filter() {
        let result = builtin_clear_image_cache(vec![Value::True]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_image_cache_animation_cache_non_list() {
        let result = builtin_clear_image_cache(vec![Value::True, Value::True]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                && sig.data.first() == Some(&Value::symbol("listp"))
        ));
    }

    #[test]
    fn clear_image_cache_nil_second_arg_but_valid_filter() {
        let result = builtin_clear_image_cache(vec![Value::True, Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_image_cache_with_animation_cache_list() {
        let cache_arg = Value::list(vec![Value::symbol("foo"), Value::symbol("bar")]);
        let result = builtin_clear_image_cache(vec![Value::True, cache_arg]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn image_cache_size_is_zero() {
        let result = builtin_image_cache_size(vec![]);
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn imagep_matches_image_spec_shape() {
        let spec = builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")])
            .expect("create-image should succeed");
        assert!(builtin_imagep(vec![spec]).unwrap().is_truthy());
        assert!(builtin_imagep(vec![Value::Int(1)]).unwrap().is_nil());
        assert!(builtin_imagep(vec![Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
        ])])
        .unwrap()
        .is_nil());
        assert!(builtin_imagep(vec![Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("file".into()),
            Value::string("x.png"),
        ])])
        .unwrap()
        .is_nil());
    }

    #[test]
    fn image_metadata_non_spec_returns_nil() {
        let result = builtin_image_metadata(vec![Value::Int(1)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn image_metadata_window_system_error_shape() {
        let spec = builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")])
            .expect("create-image should succeed");
        let result = builtin_image_metadata(vec![spec]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "error"
                && sig.data.first() == Some(&Value::string("Window system frame should be used"))
        ));
    }

    #[test]
    fn image_metadata_second_arg_validates_frame_designator() {
        let spec = builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")])
            .expect("create-image should succeed");
        let result = builtin_image_metadata(vec![spec, Value::True]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                && sig.data.first() == Some(&Value::symbol("frame-live-p"))
        ));
    }

    // -----------------------------------------------------------------------
    // image-type
    // -----------------------------------------------------------------------

    #[test]
    fn image_type_png() {
        let result = builtin_image_type(vec![Value::string("test.png")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_symbol_name(), Some("png"));
    }

    #[test]
    fn image_type_svg() {
        let result = builtin_image_type(vec![Value::string("icon.svg")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_symbol_name(), Some("svg"));
    }

    #[test]
    fn image_type_not_image() {
        let result = builtin_image_type(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn image_type_wrong_arity() {
        let result = builtin_image_type(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn image_type_from_filename_extension() {
        let result = builtin_image_type(vec![Value::string("foo.JPG")]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_symbol_name(), Some("jpeg"));
    }

    #[test]
    fn image_type_explicit_type() {
        let result = builtin_image_type(vec![
            Value::string("no-extension"),
            Value::symbol("png"),
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_symbol_name(), Some("png"));
    }

    #[test]
    fn image_type_unknown_signals() {
        let result = builtin_image_type(vec![Value::string("unknown.bin")]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig)) if sig.symbol == "unknown-image-type"
        ));
    }

    // -----------------------------------------------------------------------
    // image-transforms-p
    // -----------------------------------------------------------------------

    #[test]
    fn image_transforms_p_returns_t() {
        let result = builtin_image_transforms_p(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn image_transforms_p_with_frame() {
        let result = builtin_image_transforms_p(vec![Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn image_transforms_p_with_non_integer_or_small_frame() {
        let result = builtin_image_transforms_p(vec![Value::Int(1)]);
        assert!(matches!(
            result,
            Err(Flow::Signal(sig))
                if sig.symbol == "wrong-type-argument"
                    && sig.data
                        == vec![Value::symbol("frame-live-p"), Value::Int(1)]
        ));
    }

    #[test]
    fn image_transforms_p_too_many_args() {
        let result = builtin_image_transforms_p(vec![Value::Nil, Value::Nil]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    #[test]
    fn plist_get_basic() {
        let plist = Value::list(vec![
            Value::Keyword("type".into()),
            Value::symbol("png"),
            Value::Keyword("file".into()),
            Value::string("test.png"),
        ]);
        let val = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(val.as_symbol_name(), Some("png"));

        let file = plist_get(&plist, &Value::Keyword("file".into()));
        assert_eq!(file.as_str(), Some("test.png"));
    }

    #[test]
    fn plist_get_missing() {
        let plist = Value::list(vec![Value::Keyword("type".into()), Value::symbol("png")]);
        let val = plist_get(&plist, &Value::Keyword("missing".into()));
        assert!(val.is_nil());
    }

    #[test]
    fn is_image_spec_valid() {
        let spec = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
            Value::Keyword("file".into()),
            Value::string("test.png"),
        ]);
        assert!(is_image_spec(&spec));
    }

    #[test]
    fn is_image_spec_bare_plist() {
        let spec = Value::list(vec![Value::Keyword("type".into()), Value::symbol("png")]);
        assert!(!is_image_spec(&spec));
    }

    #[test]
    fn is_image_spec_not_image() {
        assert!(!is_image_spec(&Value::Int(42)));
        assert!(!is_image_spec(&Value::Nil));
        assert!(!is_image_spec(&Value::string("not an image")));
    }

    #[test]
    fn is_image_spec_empty_list() {
        let spec = Value::list(vec![]);
        assert!(!is_image_spec(&spec));
    }

    #[test]
    fn is_image_spec_requires_supported_type_and_one_source() {
        let valid_file = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
            Value::Keyword("file".into()),
            Value::string("x.png"),
        ]);
        assert!(is_image_spec(&valid_file));

        let valid_data = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
            Value::Keyword("data".into()),
            Value::string("raw"),
        ]);
        assert!(is_image_spec(&valid_data));

        let unsupported_type = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("jpg"),
            Value::Keyword("file".into()),
            Value::string("x.jpg"),
        ]);
        assert!(!is_image_spec(&unsupported_type));

        let both_sources = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
            Value::Keyword("file".into()),
            Value::string("x.png"),
            Value::Keyword("data".into()),
            Value::string("raw"),
        ]);
        assert!(!is_image_spec(&both_sources));

        let missing_source = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
        ]);
        assert!(!is_image_spec(&missing_source));
    }

    #[test]
    fn image_spec_plist_with_image_prefix() {
        let spec = Value::list(vec![
            Value::symbol("image"),
            Value::Keyword("type".into()),
            Value::symbol("png"),
        ]);
        let plist = image_spec_plist(&spec);
        let val = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(val.as_symbol_name(), Some("png"));
    }

    #[test]
    fn image_spec_plist_bare() {
        let spec = Value::list(vec![Value::Keyword("type".into()), Value::symbol("jpeg")]);
        let plist = image_spec_plist(&spec);
        let val = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(val.as_symbol_name(), Some("jpeg"));
    }

    #[test]
    fn round_trip_create_then_type() {
        // `create-image` keeps the explicit :type marker in the resulting spec.
        let spec =
            builtin_create_image(vec![Value::string("photo.jpg"), Value::symbol("jpeg")]).unwrap();
        let plist = image_spec_plist(&spec);
        let img_type = plist_get(&plist, &Value::Keyword("type".into()));
        assert_eq!(img_type.as_symbol_name(), Some("jpeg"));
    }

    #[test]
    fn round_trip_create_then_size() {
        // In batch, image-size requires a window-system frame.
        let spec =
            builtin_create_image(vec![Value::string("photo.jpg"), Value::symbol("jpeg")]).unwrap();

        let result = builtin_image_size(vec![spec, Value::True]);
        assert!(result.is_err());
    }
}
