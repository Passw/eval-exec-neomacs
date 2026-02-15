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

// ---------------------------------------------------------------------------
// Property list helpers
// ---------------------------------------------------------------------------

/// Get a value from a property list by keyword.
/// The plist is a flat list: (:key1 val1 :key2 val2 ...).
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
        "png" | "jpeg" | "jpg" | "gif" | "svg" | "webp" | "xpm" | "xbm" | "pbm" | "tiff" | "bmp"
    )
}

/// Validate that a value looks like an image spec.
/// An image spec is a list starting with the symbol `image` followed by
/// property-list key/value pairs, or simply a property list with a `:type` key.
fn is_image_spec(value: &Value) -> bool {
    match value {
        Value::Cons(_) => {
            // Check if it starts with the symbol `image`.
            let items = match list_to_vec(value) {
                Some(v) => v,
                None => return false,
            };
            if items.is_empty() {
                return false;
            }
            // Emacs image descriptors look like: (image :type png :file "x.png" ...)
            if let Some(name) = items[0].as_symbol_name() {
                if name == "image" {
                    return true;
                }
            }
            // Also accept a bare plist with :type.
            !plist_get(value, &Value::Keyword("type".into())).is_nil()
        }
        _ => false,
    }
}

/// Extract the plist portion of an image spec.
/// If the spec starts with `image`, skip that first element.
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
/// TYPE is a symbol like `png`, `jpeg`, etc. (defaults to autodetect stub: `png`).
/// DATA-P if non-nil means FILE-OR-DATA is raw image data, not a file name.
/// PROPS are additional property-list pairs (e.g. :width 100 :height 200).
///
/// Returns: (image :type TYPE :file FILE-OR-DATA ... PROPS)
pub(crate) fn builtin_create_image(args: Vec<Value>) -> EvalResult {
    expect_min_args("create-image", &args, 1)?;

    let file_or_data = args[0].clone();

    // TYPE argument (optional, defaults to png).
    let image_type = if args.len() > 1 && !args[1].is_nil() {
        match args[1].as_symbol_name() {
            Some(name) => Value::symbol(name),
            None => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), args[1].clone()],
                ));
            }
        }
    } else {
        // Auto-detect stub: default to png.
        Value::symbol("png")
    };

    // DATA-P argument (optional).
    let data_p = if args.len() > 2 {
        args[2].is_truthy()
    } else {
        false
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
/// Return the size of image SPEC as a cons (WIDTH . HEIGHT).
/// If PIXELS is non-nil, return pixel sizes; otherwise return
/// sizes in canonical character units.
///
/// Stub implementation: always returns (100 . 100) for pixel mode,
/// or (10 . 5) for character mode.
pub(crate) fn builtin_image_size(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-size", &args, 1)?;
    expect_max_args("image-size", &args, 3)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("imagep"), args[0].clone()],
        ));
    }

    let pixels = args.get(1).map_or(false, |v| v.is_truthy());

    if pixels {
        // Return pixel dimensions stub.
        Ok(Value::cons(Value::Int(100), Value::Int(100)))
    } else {
        // Return character cell dimensions stub.
        Ok(Value::cons(Value::Float(10.0), Value::Float(5.0)))
    }
}

/// (image-mask-p SPEC &optional FRAME) -> nil
///
/// Return non-nil if image SPEC has a mask bitmap.
/// Stub: always returns nil (no mask support yet).
pub(crate) fn builtin_image_mask_p(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-mask-p", &args, 1)?;
    expect_max_args("image-mask-p", &args, 2)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("imagep"), args[0].clone()],
        ));
    }

    Ok(Value::Nil)
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
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("imagep"), args[0].clone()],
        ));
    }

    // Validate POINT is an integer.
    if args[1].as_int().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), args[1].clone()],
        ));
    }

    // Stub: no-op.
    Ok(Value::Nil)
}

/// (insert-image IMAGE &optional STRING AREA SLICE) -> nil
///
/// Insert IMAGE into the current buffer at point.
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_insert_image(args: Vec<Value>) -> EvalResult {
    expect_min_args("insert-image", &args, 1)?;
    expect_max_args("insert-image", &args, 4)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("imagep"), args[0].clone()],
        ));
    }

    // Stub: no-op.
    Ok(Value::Nil)
}

/// (remove-images START END &optional BUFFER) -> nil
///
/// Remove images between START and END in BUFFER.
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_remove_images(args: Vec<Value>) -> EvalResult {
    expect_min_args("remove-images", &args, 2)?;
    expect_max_args("remove-images", &args, 3)?;

    // Validate START and END are integers.
    if args[0].as_int().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), args[0].clone()],
        ));
    }
    if args[1].as_int().is_none() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), args[1].clone()],
        ));
    }

    // Stub: no-op.
    Ok(Value::Nil)
}

/// (image-flush SPEC &optional FRAME) -> nil
///
/// Flush the image cache for image SPEC.
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_image_flush(args: Vec<Value>) -> EvalResult {
    expect_min_args("image-flush", &args, 1)?;
    expect_max_args("image-flush", &args, 2)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("imagep"), args[0].clone()],
        ));
    }

    // Stub: no-op.
    Ok(Value::Nil)
}

/// (clear-image-cache &optional FILTER) -> nil
///
/// Clear the image cache.  FILTER can be nil (clear all), a frame,
/// or t (clear all frames).
/// Stub: does nothing, returns nil.
pub(crate) fn builtin_clear_image_cache(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-image-cache", &args, 1)?;

    // Stub: no-op.
    Ok(Value::Nil)
}

/// (image-type SPEC) -> symbol
///
/// Return the image type of image spec SPEC (e.g. `png`, `jpeg`).
/// Extracts the `:type` property from the image spec.
pub(crate) fn builtin_image_type(args: Vec<Value>) -> EvalResult {
    expect_args("image-type", &args, 1)?;

    if !is_image_spec(&args[0]) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("imagep"), args[0].clone()],
        ));
    }

    let plist = image_spec_plist(&args[0]);
    let type_val = plist_get(&plist, &Value::Keyword("type".into()));

    if type_val.is_nil() {
        // No :type found — signal error.
        Err(signal(
            "error",
            vec![Value::string("Invalid image spec: missing :type")],
        ))
    } else {
        Ok(type_val)
    }
}

/// (image-transforms-p &optional FRAME) -> t
///
/// Return non-nil if FRAME supports image transforms (scaling, rotation).
/// Always returns t since Neomacs supports GPU-based transforms.
pub(crate) fn builtin_image_transforms_p(args: Vec<Value>) -> EvalResult {
    expect_max_args("image-transforms-p", &args, 1)?;
    Ok(Value::True)
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
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // image-size
    // -----------------------------------------------------------------------

    #[test]
    fn image_size_pixels() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_size(vec![spec, Value::True]);
        assert!(result.is_ok());
        let size = result.unwrap();

        match size {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car.as_int(), Some(100));
                assert_eq!(pair.cdr.as_int(), Some(100));
            }
            _ => panic!("expected cons"),
        }
    }

    #[test]
    fn image_size_chars() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_size(vec![spec]);
        assert!(result.is_ok());
        let size = result.unwrap();

        match size {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car.as_float(), Some(10.0));
                assert_eq!(pair.cdr.as_float(), Some(5.0));
            }
            _ => panic!("expected cons"),
        }
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
    fn image_mask_p_returns_nil() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_mask_p(vec![spec]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
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
    fn put_image_stub() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_put_image(vec![spec, Value::Int(1)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn put_image_bad_point() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_put_image(vec![spec, Value::string("not a point")]);
        assert!(result.is_err());
    }

    #[test]
    fn put_image_not_image() {
        let result = builtin_put_image(vec![Value::Int(1), Value::Int(1)]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // insert-image
    // -----------------------------------------------------------------------

    #[test]
    fn insert_image_stub() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_insert_image(vec![spec]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn insert_image_not_image() {
        let result = builtin_insert_image(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // remove-images
    // -----------------------------------------------------------------------

    #[test]
    fn remove_images_stub() {
        let result = builtin_remove_images(vec![Value::Int(1), Value::Int(100)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn remove_images_bad_start() {
        let result = builtin_remove_images(vec![Value::string("x"), Value::Int(100)]);
        assert!(result.is_err());
    }

    #[test]
    fn remove_images_bad_end() {
        let result = builtin_remove_images(vec![Value::Int(1), Value::string("x")]);
        assert!(result.is_err());
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
    fn image_flush_stub() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_flush(vec![spec]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn image_flush_not_image() {
        let result = builtin_image_flush(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // clear-image-cache
    // -----------------------------------------------------------------------

    #[test]
    fn clear_image_cache_no_args() {
        let result = builtin_clear_image_cache(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_image_cache_with_filter() {
        let result = builtin_clear_image_cache(vec![Value::True]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_image_cache_too_many_args() {
        let result = builtin_clear_image_cache(vec![Value::True, Value::True]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // image-type
    // -----------------------------------------------------------------------

    #[test]
    fn image_type_png() {
        let spec =
            builtin_create_image(vec![Value::string("test.png"), Value::symbol("png")]).unwrap();

        let result = builtin_image_type(vec![spec]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_symbol_name(), Some("png"));
    }

    #[test]
    fn image_type_svg() {
        let spec =
            builtin_create_image(vec![Value::string("icon.svg"), Value::symbol("svg")]).unwrap();

        let result = builtin_image_type(vec![spec]);
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

    // -----------------------------------------------------------------------
    // image-transforms-p
    // -----------------------------------------------------------------------

    #[test]
    fn image_transforms_p_returns_t() {
        let result = builtin_image_transforms_p(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn image_transforms_p_with_frame() {
        let result = builtin_image_transforms_p(vec![Value::Nil]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
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
        assert!(is_image_spec(&spec));
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
        // create-image -> image-type should return the same type.
        let spec =
            builtin_create_image(vec![Value::string("photo.jpg"), Value::symbol("jpeg")]).unwrap();

        let img_type = builtin_image_type(vec![spec]).unwrap();
        assert_eq!(img_type.as_symbol_name(), Some("jpeg"));
    }

    #[test]
    fn round_trip_create_then_size() {
        // create-image -> image-size should work.
        let spec =
            builtin_create_image(vec![Value::string("photo.jpg"), Value::symbol("jpeg")]).unwrap();

        let size = builtin_image_size(vec![spec, Value::True]).unwrap();
        match size {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                assert_eq!(pair.car.as_int(), Some(100));
                assert_eq!(pair.cdr.as_int(), Some(100));
            }
            _ => panic!("expected cons"),
        }
    }
}
