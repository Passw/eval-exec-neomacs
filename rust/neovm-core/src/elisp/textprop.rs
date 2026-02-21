//! Text property and overlay builtins for the Elisp interpreter.
//!
//! Bridges the buffer's `TextPropertyTable` and `OverlayList` to Elisp
//! functions like `put-text-property`, `make-overlay`, etc.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use crate::buffer::buffer::BufferId;

// ---------------------------------------------------------------------------
// Helpers (local to this module)
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

fn expect_int(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn expect_integer_or_marker(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

/// Extract a symbol name (for property names).
fn expect_symbol_name(value: &Value) -> Result<String, Flow> {
    match value.as_symbol_name() {
        Some(s) => Ok(s.to_string()),
        None => match value {
            Value::Str(s) => Ok((**s).clone()),
            Value::Keyword(s) => Ok(s.clone()),
            other => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("symbolp"), other.clone()],
            )),
        },
    }
}

/// Convert a 1-based Elisp char position to a 0-based byte position,
/// clamping within the buffer.
fn elisp_pos_to_byte(buf: &crate::buffer::buffer::Buffer, pos: i64) -> usize {
    let char_pos = if pos > 0 { pos as usize - 1 } else { 0 };
    let clamped = char_pos.min(buf.text.char_count());
    buf.text.char_to_byte(clamped)
}

/// Convert a 0-based byte position to a 1-based Elisp char position.
fn byte_to_elisp_pos(buf: &crate::buffer::buffer::Buffer, byte_pos: usize) -> i64 {
    buf.text.byte_to_char(byte_pos) as i64 + 1
}

/// Resolve the optional OBJECT argument to a buffer id.
/// If nil or absent, uses the current buffer.
fn resolve_buffer_id(
    eval: &super::eval::Evaluator,
    object: Option<&Value>,
) -> Result<BufferId, Flow> {
    match object {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")])),
        Some(Value::Buffer(id)) => Ok(*id),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("bufferp"), other.clone()],
        )),
    }
}

/// Iterate a plist (alternating key value key value ...) from a list or vec.
/// Returns pairs of (property-name, value).
fn plist_pairs(plist: &Value) -> Result<Vec<(String, Value)>, Flow> {
    let items = list_to_vec(plist).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), plist.clone()],
        )
    })?;
    if items.len() % 2 != 0 {
        return Err(signal(
            "error",
            vec![Value::string("Odd number of elements in property list")],
        ));
    }
    let mut pairs = Vec::new();
    for chunk in items.chunks(2) {
        let name = expect_symbol_name(&chunk[0])?;
        pairs.push((name, chunk[1].clone()));
    }
    Ok(pairs)
}

/// Convert a HashMap<String, Value> to an Elisp plist (alternating symbols and values).
fn hashmap_to_plist(map: &std::collections::HashMap<String, Value>) -> Value {
    let mut items = Vec::new();
    for (key, val) in map {
        items.push(Value::symbol(key.clone()));
        items.push(val.clone());
    }
    Value::list(items)
}

// ===========================================================================
// Text property builtins
// ===========================================================================

/// (put-text-property BEG END PROP VAL &optional OBJECT)
pub(crate) fn builtin_put_text_property(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("put-text-property", &args, 4)?;
    expect_max_args("put-text-property", &args, 5)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let prop = expect_symbol_name(&args[2])?;
    let val = args[3].clone();
    let buf_id = resolve_buffer_id(eval, args.get(4))?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    buf.text_props.put_property(byte_beg, byte_end, &prop, val);
    Ok(Value::Nil)
}

/// (get-text-property POS PROP &optional OBJECT)
pub(crate) fn builtin_get_text_property(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("get-text-property", &args, 2)?;
    expect_max_args("get-text-property", &args, 3)?;
    let pos = expect_int(&args[0])?;
    let prop = expect_symbol_name(&args[1])?;
    let buf_id = resolve_buffer_id(eval, args.get(2))?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    match buf.text_props.get_property(byte_pos, &prop) {
        Some(v) => {
            let val: Value = v.clone();
            Ok(val)
        }
        None => Ok(Value::Nil),
    }
}

/// (get-char-property POS PROP &optional OBJECT)
/// For now, same as get-text-property (ignores overlays).
pub(crate) fn builtin_get_char_property(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("get-char-property", &args, 2)?;
    expect_max_args("get-char-property", &args, 3)?;
    // Delegate to get-text-property for now.
    builtin_get_text_property(eval, args)
}

/// (add-text-properties BEG END PROPS &optional OBJECT)
pub(crate) fn builtin_add_text_properties(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("add-text-properties", &args, 3)?;
    expect_max_args("add-text-properties", &args, 4)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let pairs = plist_pairs(&args[2])?;
    let buf_id = resolve_buffer_id(eval, args.get(3))?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    for (name, val) in pairs {
        buf.text_props.put_property(byte_beg, byte_end, &name, val);
    }
    // In Emacs, returns t if any property was actually changed; we simplify.
    Ok(Value::True)
}

fn merge_face_property(existing: Option<Value>, new_face: Value, append: bool) -> Value {
    let Some(existing_value) = existing else {
        return new_face;
    };
    if existing_value.is_nil() {
        return new_face;
    }

    if let Some(mut items) = list_to_vec(&existing_value) {
        if append {
            items.push(new_face);
        } else {
            items.insert(0, new_face);
        }
        Value::list(items)
    } else if append {
        Value::list(vec![existing_value, new_face])
    } else {
        Value::list(vec![new_face, existing_value])
    }
}

/// `(add-face-text-property START END FACE &optional APPENDP OBJECT)`
pub(crate) fn builtin_add_face_text_property(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("add-face-text-property", &args, 3)?;
    expect_max_args("add-face-text-property", &args, 5)?;
    let beg = expect_integer_or_marker(&args[0])?;
    let end = expect_integer_or_marker(&args[1])?;
    let new_face = args[2].clone();
    let append = args.get(3).is_some_and(Value::is_truthy);

    let object = args.get(4);
    let buf_id = match object {
        None | Some(Value::Nil) => eval
            .buffers
            .current_buffer()
            .map(|b| b.id)
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")])),
        Some(Value::Buffer(id)) => Ok(*id),
        Some(Value::Str(_)) => return Ok(Value::Nil),
        Some(other) => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("buffer-or-string-p"), other.clone()],
        )),
    }?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;
    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    let existing = buf.text_props.get_property(byte_beg, "face").cloned();
    let merged = merge_face_property(existing, new_face, append);
    buf.text_props
        .put_property(byte_beg, byte_end, "face", merged);
    Ok(Value::Nil)
}

/// (remove-text-properties BEG END PROPS &optional OBJECT)
pub(crate) fn builtin_remove_text_properties(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("remove-text-properties", &args, 3)?;
    expect_max_args("remove-text-properties", &args, 4)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let pairs = plist_pairs(&args[2])?;
    let buf_id = resolve_buffer_id(eval, args.get(3))?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    for (name, _val) in pairs {
        buf.text_props.remove_property(byte_beg, byte_end, &name);
    }
    Ok(Value::True)
}

/// (set-text-properties BEG END PROPS &optional OBJECT)
pub(crate) fn builtin_set_text_properties(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("set-text-properties", &args, 3)?;
    expect_max_args("set-text-properties", &args, 4)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let pairs = plist_pairs(&args[2])?;
    let buf_id = resolve_buffer_id(eval, args.get(3))?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    buf.text_props.remove_all_properties(byte_beg, byte_end);
    for (name, val) in pairs {
        buf.text_props.put_property(byte_beg, byte_end, &name, val);
    }
    Ok(Value::True)
}

/// (remove-list-of-text-properties BEG END LIST &optional OBJECT)
pub(crate) fn builtin_remove_list_of_text_properties(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("remove-list-of-text-properties", &args, 3)?;
    expect_max_args("remove-list-of-text-properties", &args, 4)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let names = list_to_vec(&args[2]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[2].clone()],
        )
    })?;
    let buf_id = resolve_buffer_id(eval, args.get(3))?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);

    let mut changed = false;
    for name_val in names {
        let name = expect_symbol_name(&name_val)?;
        let mut cursor = byte_beg;
        while cursor < byte_end {
            if buf.text_props.get_property(cursor, &name).is_some() {
                changed = true;
                break;
            }
            match buf.text_props.next_property_change(cursor) {
                Some(next) if next > cursor && next < byte_end => cursor = next,
                _ => break,
            }
        }
        buf.text_props.remove_property(byte_beg, byte_end, &name);
    }
    Ok(if changed { Value::True } else { Value::Nil })
}

/// (text-properties-at POS &optional OBJECT)
pub(crate) fn builtin_text_properties_at(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("text-properties-at", &args, 1)?;
    expect_max_args("text-properties-at", &args, 2)?;
    let pos = expect_int(&args[0])?;
    let buf_id = resolve_buffer_id(eval, args.get(1))?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let props = buf.text_props.get_properties(byte_pos);
    Ok(hashmap_to_plist(&props))
}

/// (next-single-property-change POS PROP &optional OBJECT LIMIT)
pub(crate) fn builtin_next_single_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("next-single-property-change", &args, 2)?;
    expect_max_args("next-single-property-change", &args, 4)?;
    let pos = expect_int(&args[0])?;
    let prop = expect_symbol_name(&args[1])?;
    let buf_id = resolve_buffer_id(eval, args.get(2))?;
    let limit = args.get(3);

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let byte_limit = match limit {
        Some(v) if !v.is_nil() => Some(elisp_pos_to_byte(buf, expect_int(v)?)),
        _ => None,
    };

    // Walk forward from byte_pos looking for where the specific property changes.
    let current_val = buf.text_props.get_property(byte_pos, &prop).cloned();
    let buf_len = buf.text.len();
    let mut cursor = byte_pos;

    loop {
        match buf.text_props.next_property_change(cursor) {
            Some(next) => {
                if let Some(lim) = byte_limit {
                    if next > lim {
                        return Ok(Value::Int(byte_to_elisp_pos(buf, lim)));
                    }
                }
                // Check if the specific property changed at `next`.
                let new_val = buf.text_props.get_property(next, &prop).cloned();
                let changed = match (&current_val, &new_val) {
                    (None, None) => false,
                    (Some(a), Some(b)) => !equal_value(a, b, 0),
                    _ => true,
                };
                if changed {
                    return Ok(Value::Int(byte_to_elisp_pos(buf, next)));
                }
                cursor = next;
                if cursor >= buf_len {
                    break;
                }
            }
            None => break,
        }
    }

    // No change found — return LIMIT if given, else nil.
    match byte_limit {
        Some(lim) => Ok(Value::Int(byte_to_elisp_pos(buf, lim))),
        None => Ok(Value::Nil),
    }
}

/// (previous-single-property-change POS PROP &optional OBJECT LIMIT)
pub(crate) fn builtin_previous_single_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("previous-single-property-change", &args, 2)?;
    expect_max_args("previous-single-property-change", &args, 4)?;
    let pos = expect_int(&args[0])?;
    let prop = expect_symbol_name(&args[1])?;
    let buf_id = resolve_buffer_id(eval, args.get(2))?;
    let limit = args.get(3);

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let byte_limit = match limit {
        Some(v) if !v.is_nil() => Some(elisp_pos_to_byte(buf, expect_int(v)?)),
        _ => None,
    };

    // The value of the property just before pos (at pos-1 conceptually).
    let ref_byte = if byte_pos > 0 { byte_pos - 1 } else { 0 };
    let current_val = buf.text_props.get_property(ref_byte, &prop).cloned();
    let mut cursor = byte_pos;

    loop {
        match buf.text_props.previous_property_change(cursor) {
            Some(prev) => {
                if let Some(lim) = byte_limit {
                    if prev < lim {
                        return Ok(Value::Int(byte_to_elisp_pos(buf, lim)));
                    }
                }
                let check = if prev > 0 { prev - 1 } else { 0 };
                let new_val = buf.text_props.get_property(check, &prop).cloned();
                let changed = match (&current_val, &new_val) {
                    (None, None) => false,
                    (Some(a), Some(b)) => !equal_value(a, b, 0),
                    _ => true,
                };
                if changed {
                    return Ok(Value::Int(byte_to_elisp_pos(buf, prev)));
                }
                if prev == 0 {
                    break;
                }
                cursor = if prev < cursor { prev } else { prev - 1 };
            }
            None => break,
        }
    }

    match byte_limit {
        Some(lim) => Ok(Value::Int(byte_to_elisp_pos(buf, lim))),
        None => Ok(Value::Nil),
    }
}

/// (next-property-change POS &optional OBJECT LIMIT)
pub(crate) fn builtin_next_property_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("next-property-change", &args, 1)?;
    expect_max_args("next-property-change", &args, 3)?;
    let pos = expect_int(&args[0])?;
    let buf_id = resolve_buffer_id(eval, args.get(1))?;
    let limit = args.get(2);

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let byte_limit = match limit {
        Some(v) if !v.is_nil() => Some(elisp_pos_to_byte(buf, expect_int(v)?)),
        _ => None,
    };

    match buf.text_props.next_property_change(byte_pos) {
        Some(next) => {
            if let Some(lim) = byte_limit {
                if next > lim {
                    return Ok(Value::Int(byte_to_elisp_pos(buf, lim)));
                }
            }
            Ok(Value::Int(byte_to_elisp_pos(buf, next)))
        }
        None => match byte_limit {
            Some(lim) => Ok(Value::Int(byte_to_elisp_pos(buf, lim))),
            None => Ok(Value::Nil),
        },
    }
}

/// (text-property-any BEG END PROP VAL &optional OBJECT)
pub(crate) fn builtin_text_property_any(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("text-property-any", &args, 4)?;
    expect_max_args("text-property-any", &args, 5)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let prop = expect_symbol_name(&args[2])?;
    let val = &args[3];
    let buf_id = resolve_buffer_id(eval, args.get(4))?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);

    // Walk through the range looking for the property with the matching value.
    let mut cursor = byte_beg;
    while cursor < byte_end {
        if let Some(found) = buf.text_props.get_property(cursor, &prop) {
            if equal_value(found, val, 0) {
                return Ok(Value::Int(byte_to_elisp_pos(buf, cursor)));
            }
        }
        // Advance to next property change.
        match buf.text_props.next_property_change(cursor) {
            Some(next) if next <= byte_end => {
                cursor = next;
            }
            _ => break,
        }
    }
    Ok(Value::Nil)
}

/// (text-property-not-all BEG END PROP VAL &optional OBJECT)
pub(crate) fn builtin_text_property_not_all(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("text-property-not-all", &args, 4)?;
    expect_max_args("text-property-not-all", &args, 5)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let prop = expect_symbol_name(&args[2])?;
    let val = &args[3];
    let buf_id = resolve_buffer_id(eval, args.get(4))?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    let mut cursor = byte_beg;

    while cursor < byte_end {
        let matches = match buf.text_props.get_property(cursor, &prop) {
            Some(found) => equal_value(found, val, 0),
            None => val.is_nil(),
        };
        if !matches {
            return Ok(Value::Int(byte_to_elisp_pos(buf, cursor)));
        }

        match buf.text_props.next_property_change(cursor) {
            Some(next) if next > cursor && next < byte_end => cursor = next,
            _ => break,
        }
    }

    Ok(Value::Nil)
}

/// (get-char-property-and-overlay POS PROP &optional OBJECT)
pub(crate) fn builtin_get_char_property_and_overlay(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("get-char-property-and-overlay", &args, 2)?;
    expect_max_args("get-char-property-and-overlay", &args, 3)?;
    let pos = expect_int(&args[0])?;
    let prop = expect_symbol_name(&args[1])?;
    let buf_id = resolve_buffer_id(eval, args.get(2))?;

    if let Some(buf) = eval.buffers.get(buf_id) {
        let byte_pos = elisp_pos_to_byte(buf, pos);
        let overlay_ids = buf.overlays.overlays_at(byte_pos);
        for ov_id in overlay_ids {
            if let Some(val) = buf.overlays.overlay_get(ov_id, &prop) {
                let overlay = Value::cons(Value::Int(ov_id as i64), Value::Buffer(buf_id));
                return Ok(Value::cons(val.clone(), overlay));
            }
        }
    }

    let value = builtin_get_char_property(eval, args)?;
    Ok(Value::cons(value, Value::Nil))
}

/// (get-display-property POS PROP &optional OBJECT PROPERTIES)
pub(crate) fn builtin_get_display_property(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("get-display-property", &args, 2)?;
    expect_max_args("get-display-property", &args, 4)?;
    let prop = expect_symbol_name(&args[1])?;
    if prop != "display" {
        return Ok(Value::Nil);
    }
    let mut forwarded = vec![args[0].clone(), args[1].clone()];
    if let Some(object) = args.get(2) {
        forwarded.push(object.clone());
    }
    builtin_get_char_property(eval, forwarded)
}

// ===========================================================================
// Overlay builtins
// ===========================================================================

/// (next-overlay-change POS)
pub(crate) fn builtin_next_overlay_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("next-overlay-change", &args, 1)?;
    let pos = expect_integer_or_marker(&args[0])?;
    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let all_ids = buf.overlays.overlays_in(buf.point_min(), buf.point_max());
    let mut best: Option<usize> = None;
    for ov_id in all_ids {
        if let Some(start) = buf.overlays.overlay_start(ov_id) {
            if start > byte_pos {
                best = Some(best.map_or(start, |cur| cur.min(start)));
            }
        }
        if let Some(end) = buf.overlays.overlay_end(ov_id) {
            if end > byte_pos {
                best = Some(best.map_or(end, |cur| cur.min(end)));
            }
        }
    }

    match best {
        Some(next) => Ok(Value::Int(byte_to_elisp_pos(buf, next))),
        None => Ok(Value::Int(byte_to_elisp_pos(buf, buf.point_max()))),
    }
}

/// (previous-overlay-change POS)
pub(crate) fn builtin_previous_overlay_change(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("previous-overlay-change", &args, 1)?;
    let pos = expect_integer_or_marker(&args[0])?;
    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let all_ids = buf.overlays.overlays_in(buf.point_min(), buf.point_max());
    let mut best: Option<usize> = None;
    for ov_id in all_ids {
        if let Some(start) = buf.overlays.overlay_start(ov_id) {
            if start < byte_pos {
                best = Some(best.map_or(start, |cur| cur.max(start)));
            }
        }
        if let Some(end) = buf.overlays.overlay_end(ov_id) {
            if end < byte_pos {
                best = Some(best.map_or(end, |cur| cur.max(end)));
            }
        }
    }

    match best {
        Some(prev) => Ok(Value::Int(byte_to_elisp_pos(buf, prev))),
        None => Ok(Value::Int(byte_to_elisp_pos(buf, buf.point_min()))),
    }
}

/// (make-overlay BEG END &optional BUFFER FRONT-ADVANCE REAR-ADVANCE)
pub(crate) fn builtin_make_overlay(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("make-overlay", &args, 2)?;
    expect_max_args("make-overlay", &args, 5)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;
    let buf_id = resolve_buffer_id(eval, args.get(2))?;
    let front_advance = args.get(3).map_or(false, |v| v.is_truthy());
    let rear_advance = args.get(4).map_or(false, |v| v.is_truthy());

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    let ov_id = buf.overlays.make_overlay(byte_beg, byte_end);
    if front_advance {
        buf.overlays.set_front_advance(ov_id, true);
    }
    if rear_advance {
        buf.overlays.set_rear_advance(ov_id, true);
    }

    // Return a cons (overlay-id . buffer-id) to identify the overlay.
    Ok(Value::cons(Value::Int(ov_id as i64), Value::Buffer(buf_id)))
}

/// Extract overlay id and buffer id from an overlay value (cons of int . buffer).
fn expect_overlay(value: &Value) -> Result<(u64, BufferId), Flow> {
    if let Value::Cons(cell) = value {
        let pair = cell.lock().expect("poisoned");
        if let (Value::Int(ov_id), Value::Buffer(buf_id)) = (&pair.car, &pair.cdr) {
            return Ok((*ov_id as u64, *buf_id));
        }
    }
    Err(signal(
        "wrong-type-argument",
        vec![Value::symbol("overlayp"), value.clone()],
    ))
}

/// (delete-overlay OVERLAY)
pub(crate) fn builtin_delete_overlay(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-overlay", &args, 1)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;

    if let Some(buf) = eval.buffers.get_mut(buf_id) {
        buf.overlays.delete_overlay(ov_id);
    }
    Ok(Value::Nil)
}

/// (overlay-put OVERLAY PROP VAL)
pub(crate) fn builtin_overlay_put(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlay-put", &args, 3)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;
    let prop = expect_symbol_name(&args[1])?;
    let val = args[2].clone();

    if let Some(buf) = eval.buffers.get_mut(buf_id) {
        buf.overlays.overlay_put(ov_id, &prop, val.clone());
    }
    Ok(val)
}

/// (overlay-get OVERLAY PROP)
pub(crate) fn builtin_overlay_get(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlay-get", &args, 2)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;
    let prop = expect_symbol_name(&args[1])?;

    if let Some(buf) = eval.buffers.get(buf_id) {
        match buf.overlays.overlay_get(ov_id, &prop) {
            Some(v) => {
                let val: Value = v.clone();
                return Ok(val);
            }
            None => return Ok(Value::Nil),
        }
    }
    Ok(Value::Nil)
}

/// (overlayp OBJ)
pub(crate) fn builtin_overlayp(_eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("overlayp", &args, 1)?;
    if let Value::Cons(cell) = &args[0] {
        let pair = cell.lock().expect("poisoned");
        if matches!((&pair.car, &pair.cdr), (Value::Int(_), Value::Buffer(_))) {
            return Ok(Value::True);
        }
    }
    Ok(Value::Nil)
}

/// (overlays-at POS &optional SORTED)
pub(crate) fn builtin_overlays_at(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("overlays-at", &args, 1)?;
    expect_max_args("overlays-at", &args, 2)?;
    let pos = expect_int(&args[0])?;

    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_pos = elisp_pos_to_byte(buf, pos);
    let ids = buf.overlays.overlays_at(byte_pos);
    let overlays: Vec<Value> = ids
        .into_iter()
        .map(|id| Value::cons(Value::Int(id as i64), Value::Buffer(buf_id)))
        .collect();
    Ok(Value::list(overlays))
}

/// (overlays-in BEG END)
pub(crate) fn builtin_overlays_in(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlays-in", &args, 2)?;
    let beg = expect_int(&args[0])?;
    let end = expect_int(&args[1])?;

    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    let ids = buf.overlays.overlays_in(byte_beg, byte_end);
    let overlays: Vec<Value> = ids
        .into_iter()
        .map(|id| Value::cons(Value::Int(id as i64), Value::Buffer(buf_id)))
        .collect();
    Ok(Value::list(overlays))
}

/// (move-overlay OVERLAY BEG END &optional BUFFER)
pub(crate) fn builtin_move_overlay(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("move-overlay", &args, 3)?;
    expect_max_args("move-overlay", &args, 4)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;
    let beg = expect_int(&args[1])?;
    let end = expect_int(&args[2])?;
    // Optional BUFFER argument — if given, we'd need to move between buffers.
    // For simplicity, we move within the same buffer.
    let _new_buf = args.get(3);

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = elisp_pos_to_byte(buf, beg);
    let byte_end = elisp_pos_to_byte(buf, end);
    buf.overlays.move_overlay(ov_id, byte_beg, byte_end);
    Ok(args[0].clone())
}

/// (overlay-start OVERLAY)
pub(crate) fn builtin_overlay_start(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlay-start", &args, 1)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    match buf.overlays.overlay_start(ov_id) {
        Some(byte_pos) => Ok(Value::Int(byte_to_elisp_pos(buf, byte_pos))),
        None => Ok(Value::Nil),
    }
}

/// (overlay-end OVERLAY)
pub(crate) fn builtin_overlay_end(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlay-end", &args, 1)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;

    let buf = eval
        .buffers
        .get(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    match buf.overlays.overlay_end(ov_id) {
        Some(byte_pos) => Ok(Value::Int(byte_to_elisp_pos(buf, byte_pos))),
        None => Ok(Value::Nil),
    }
}

/// (overlay-buffer OVERLAY)
pub(crate) fn builtin_overlay_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlay-buffer", &args, 1)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;

    // Check if the overlay still exists in the buffer.
    if let Some(buf) = eval.buffers.get(buf_id) {
        if buf.overlays.get(ov_id).is_some() {
            return Ok(Value::Buffer(buf_id));
        }
    }
    Ok(Value::Nil)
}

/// (overlay-properties OVERLAY)
pub(crate) fn builtin_overlay_properties(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("overlay-properties", &args, 1)?;
    let (ov_id, buf_id) = expect_overlay(&args[0])?;

    if let Some(buf) = eval.buffers.get(buf_id) {
        if let Some(ov) = buf.overlays.get(ov_id) {
            return Ok(hashmap_to_plist(&ov.properties));
        }
    }
    Ok(Value::Nil)
}

/// (remove-overlays &optional BEG END NAME VAL)
pub(crate) fn builtin_remove_overlays(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("remove-overlays", &args, 4)?;
    let buf_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let buf = eval
        .buffers
        .get_mut(buf_id)
        .ok_or_else(|| signal("error", vec![Value::string("Buffer does not exist")]))?;

    let byte_beg = if args.is_empty() || args[0].is_nil() {
        buf.point_min()
    } else {
        elisp_pos_to_byte(buf, expect_int(&args[0])?)
    };

    let byte_end = if args.len() < 2 || args[1].is_nil() {
        buf.point_max()
    } else {
        elisp_pos_to_byte(buf, expect_int(&args[1])?)
    };

    let filter_name = if args.len() >= 3 && !args[2].is_nil() {
        Some(expect_symbol_name(&args[2])?)
    } else {
        None
    };

    let filter_val = if args.len() >= 4 && !args[3].is_nil() {
        Some(args[3].clone())
    } else {
        None
    };

    // Collect overlay ids in range.
    let ids = buf.overlays.overlays_in(byte_beg, byte_end);

    // Filter and delete.
    for ov_id in ids {
        let should_delete = match (&filter_name, &filter_val) {
            (Some(name), Some(val)) => buf
                .overlays
                .overlay_get(ov_id, name)
                .map_or(false, |v| equal_value(v, val, 0)),
            (Some(name), None) => buf.overlays.overlay_get(ov_id, name).is_some(),
            _ => true,
        };
        if should_delete {
            buf.overlays.delete_overlay(ov_id);
        }
    }

    Ok(Value::Nil)
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::super::eval::Evaluator;
    use super::*;

    /// Helper: create an evaluator with a buffer containing the given text.
    fn eval_with_text(text: &str) -> Evaluator {
        let mut eval = Evaluator::new();
        eval.buffers.current_buffer_mut().unwrap().insert(text);
        // Reset point to beginning.
        eval.buffers.current_buffer_mut().unwrap().goto_char(0);
        eval
    }

    // -----------------------------------------------------------------------
    // put-text-property / get-text-property
    // -----------------------------------------------------------------------

    #[test]
    fn put_and_get_text_property() {
        let mut eval = eval_with_text("hello world");
        // Put 'face -> bold on positions 1..6 (1-based, "hello")
        let result = builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        );
        assert!(result.is_ok());

        // Get at position 3 (1-based, 'l')
        let result =
            builtin_get_text_property(&mut eval, vec![Value::Int(3), Value::symbol("face")]);
        match result {
            Ok(Value::Symbol(s)) => assert_eq!(s, "bold"),
            other => panic!("Expected Symbol(bold), got {:?}", other),
        }
    }

    #[test]
    fn get_text_property_returns_nil_when_absent() {
        let mut eval = eval_with_text("hello");
        let result =
            builtin_get_text_property(&mut eval, vec![Value::Int(1), Value::symbol("face")]);
        assert!(matches!(result, Ok(Value::Nil)));
    }

    #[test]
    fn put_text_property_outside_range() {
        let mut eval = eval_with_text("hello");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(3),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        // Position 4 is outside the propertized range.
        let result =
            builtin_get_text_property(&mut eval, vec![Value::Int(4), Value::symbol("face")]);
        assert!(matches!(result, Ok(Value::Nil)));
    }

    // -----------------------------------------------------------------------
    // get-char-property
    // -----------------------------------------------------------------------

    #[test]
    fn get_char_property_delegates() {
        let mut eval = eval_with_text("abcdef");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(5),
                Value::symbol("help-echo"),
                Value::string("tooltip"),
            ],
        )
        .unwrap();

        let result =
            builtin_get_char_property(&mut eval, vec![Value::Int(3), Value::symbol("help-echo")]);
        assert!(matches!(result, Ok(Value::Str(_))));
    }

    #[test]
    fn get_char_property_and_overlay_shape() {
        let mut eval = eval_with_text("abcd");
        let result = builtin_get_char_property_and_overlay(
            &mut eval,
            vec![Value::Int(2), Value::symbol("missing")],
        )
        .unwrap();
        let pair = list_to_vec(&result).unwrap();
        assert_eq!(pair, vec![Value::Nil]);

        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(2), Value::Int(4)]).unwrap();
        builtin_overlay_put(
            &mut eval,
            vec![ov, Value::symbol("foo"), Value::symbol("bar")],
        )
        .unwrap();
        let result = builtin_get_char_property_and_overlay(
            &mut eval,
            vec![Value::Int(3), Value::symbol("foo")],
        )
        .unwrap();
        let Value::Cons(cell) = result else {
            panic!("expected cons");
        };
        let (value, overlay) = {
            let pair = cell.lock().expect("poisoned");
            (pair.car.clone(), pair.cdr.clone())
        };
        assert!(matches!(value, Value::Symbol(s) if s == "bar"));
        let overlayp = builtin_overlayp(&mut eval, vec![overlay]).unwrap();
        assert!(matches!(overlayp, Value::True));
    }

    #[test]
    fn get_display_property_queries_display_only() {
        let mut eval = eval_with_text("abcd");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::symbol("p"),
                Value::symbol("v"),
            ],
        )
        .unwrap();
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::symbol("display"),
                Value::symbol("dv"),
            ],
        )
        .unwrap();
        let non_display = builtin_get_display_property(
            &mut eval,
            vec![Value::Int(2), Value::symbol("p"), Value::Nil, Value::Nil],
        )
        .unwrap();
        assert!(non_display.is_nil());

        let display = builtin_get_display_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::symbol("display"),
                Value::Nil,
                Value::Nil,
            ],
        )
        .unwrap();
        assert!(matches!(display, Value::Symbol(s) if s == "dv"));
    }

    // -----------------------------------------------------------------------
    // add-text-properties
    // -----------------------------------------------------------------------

    #[test]
    fn add_text_properties_multiple() {
        let mut eval = eval_with_text("hello world");
        let props = Value::list(vec![
            Value::symbol("face"),
            Value::symbol("bold"),
            Value::symbol("mouse-face"),
            Value::symbol("highlight"),
        ]);
        let result =
            builtin_add_text_properties(&mut eval, vec![Value::Int(1), Value::Int(6), props]);
        assert!(result.is_ok());

        let face = builtin_get_text_property(&mut eval, vec![Value::Int(2), Value::symbol("face")])
            .unwrap();
        assert!(matches!(face, Value::Symbol(s) if s == "bold"));

        let mouse =
            builtin_get_text_property(&mut eval, vec![Value::Int(2), Value::symbol("mouse-face")])
                .unwrap();
        assert!(matches!(mouse, Value::Symbol(s) if s == "highlight"));
    }

    #[test]
    fn add_text_properties_odd_plist_signals_error() {
        let mut eval = eval_with_text("hello");
        let props = Value::list(vec![Value::symbol("face")]);
        let result =
            builtin_add_text_properties(&mut eval, vec![Value::Int(1), Value::Int(3), props]);
        assert!(result.is_err());
    }

    #[test]
    fn add_face_text_property_basic_and_merge_order() {
        let mut eval = eval_with_text("abc");
        builtin_add_face_text_property(
            &mut eval,
            vec![Value::Int(1), Value::Int(3), Value::symbol("bold")],
        )
        .unwrap();
        let face = builtin_get_text_property(&mut eval, vec![Value::Int(2), Value::symbol("face")])
            .unwrap();
        assert_eq!(face, Value::symbol("bold"));

        let mut eval = eval_with_text("abc");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("face"),
                Value::symbol("italic"),
            ],
        )
        .unwrap();
        builtin_add_face_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("bold"),
                Value::True,
            ],
        )
        .unwrap();
        let appended =
            builtin_get_text_property(&mut eval, vec![Value::Int(1), Value::symbol("face")])
                .unwrap();
        assert_eq!(
            appended,
            Value::list(vec![Value::symbol("italic"), Value::symbol("bold")])
        );

        let mut eval = eval_with_text("abc");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("face"),
                Value::symbol("italic"),
            ],
        )
        .unwrap();
        builtin_add_face_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("bold"),
                Value::Nil,
            ],
        )
        .unwrap();
        let prepended =
            builtin_get_text_property(&mut eval, vec![Value::Int(1), Value::symbol("face")])
                .unwrap();
        assert_eq!(
            prepended,
            Value::list(vec![Value::symbol("bold"), Value::symbol("italic")])
        );
    }

    #[test]
    fn add_face_text_property_argument_contracts() {
        let mut eval = eval_with_text("abc");

        let begin_err = builtin_add_face_text_property(
            &mut eval,
            vec![Value::string("1"), Value::Int(2), Value::symbol("bold")],
        )
        .unwrap_err();
        match begin_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("integer-or-marker-p"), Value::string("1")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let object_err = builtin_add_face_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("bold"),
                Value::Nil,
                Value::True,
            ],
        )
        .unwrap_err();
        match object_err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("buffer-or-string-p"), Value::True]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let string_obj = builtin_add_face_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("bold"),
                Value::Nil,
                Value::string("abc"),
            ],
        )
        .unwrap();
        assert!(string_obj.is_nil());
    }

    // -----------------------------------------------------------------------
    // remove-text-properties
    // -----------------------------------------------------------------------

    #[test]
    fn remove_text_properties_basic() {
        let mut eval = eval_with_text("hello");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        let props = Value::list(vec![Value::symbol("face"), Value::Nil]);
        builtin_remove_text_properties(&mut eval, vec![Value::Int(1), Value::Int(6), props])
            .unwrap();

        let result =
            builtin_get_text_property(&mut eval, vec![Value::Int(3), Value::symbol("face")]);
        assert!(matches!(result, Ok(Value::Nil)));
    }

    #[test]
    fn set_text_properties_replaces_existing_values() {
        let mut eval = eval_with_text("abcd");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::symbol("p"),
                Value::symbol("v"),
            ],
        )
        .unwrap();

        let result = builtin_set_text_properties(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::list(vec![Value::symbol("q"), Value::symbol("z")]),
            ],
        )
        .unwrap();
        assert!(matches!(result, Value::True));

        let q =
            builtin_get_text_property(&mut eval, vec![Value::Int(2), Value::symbol("q")]).unwrap();
        let p =
            builtin_get_text_property(&mut eval, vec![Value::Int(2), Value::symbol("p")]).unwrap();
        assert!(matches!(q, Value::Symbol(s) if s == "z"));
        assert!(p.is_nil());
    }

    #[test]
    fn remove_list_of_text_properties_returns_t_only_when_changed() {
        let mut eval = eval_with_text("abcd");
        builtin_set_text_properties(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::list(vec![Value::symbol("q"), Value::symbol("z")]),
            ],
        )
        .unwrap();

        let first = builtin_remove_list_of_text_properties(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::list(vec![Value::symbol("q")]),
            ],
        )
        .unwrap();
        let second = builtin_remove_list_of_text_properties(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::list(vec![Value::symbol("q")]),
            ],
        )
        .unwrap();
        assert!(matches!(first, Value::True));
        assert!(second.is_nil());
    }

    // -----------------------------------------------------------------------
    // text-properties-at
    // -----------------------------------------------------------------------

    #[test]
    fn text_properties_at_returns_plist() {
        let mut eval = eval_with_text("hello");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        let result = builtin_text_properties_at(&mut eval, vec![Value::Int(2)]).unwrap();
        // Should be a plist with at least 'face 'bold.
        let items = list_to_vec(&result).unwrap();
        assert!(items.len() >= 2);
    }

    #[test]
    fn text_properties_at_empty_returns_nil() {
        let mut eval = eval_with_text("hello");
        let result = builtin_text_properties_at(&mut eval, vec![Value::Int(1)]).unwrap();
        // Empty plist is nil.
        assert!(result.is_nil());
    }

    // -----------------------------------------------------------------------
    // next-property-change
    // -----------------------------------------------------------------------

    #[test]
    fn next_property_change_basic() {
        let mut eval = eval_with_text("hello world");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        // From position 1, next change should be at position 6.
        let result = builtin_next_property_change(&mut eval, vec![Value::Int(1)]).unwrap();
        assert!(matches!(result, Value::Int(6)));
    }

    #[test]
    fn next_property_change_with_limit() {
        let mut eval = eval_with_text("hello world");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        // Limit at 4 — the actual change is at 6, so should return 4.
        let result =
            builtin_next_property_change(&mut eval, vec![Value::Int(1), Value::Nil, Value::Int(4)])
                .unwrap();
        assert!(matches!(result, Value::Int(4)));
    }

    #[test]
    fn next_property_change_no_change() {
        let mut eval = eval_with_text("hello");
        let result = builtin_next_property_change(&mut eval, vec![Value::Int(1)]).unwrap();
        assert!(matches!(result, Value::Nil));
    }

    // -----------------------------------------------------------------------
    // next-single-property-change
    // -----------------------------------------------------------------------

    #[test]
    fn next_single_property_change_basic() {
        let mut eval = eval_with_text("hello world");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        let result = builtin_next_single_property_change(
            &mut eval,
            vec![Value::Int(1), Value::symbol("face")],
        )
        .unwrap();
        assert!(matches!(result, Value::Int(6)));
    }

    #[test]
    fn next_single_property_change_nil_when_none() {
        let mut eval = eval_with_text("hello");
        let result = builtin_next_single_property_change(
            &mut eval,
            vec![Value::Int(1), Value::symbol("face")],
        )
        .unwrap();
        assert!(matches!(result, Value::Nil));
    }

    // -----------------------------------------------------------------------
    // previous-single-property-change
    // -----------------------------------------------------------------------

    #[test]
    fn previous_single_property_change_basic() {
        let mut eval = eval_with_text("hello world");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        // From position 8 (past the propertized region), looking backward for 'face change.
        let result = builtin_previous_single_property_change(
            &mut eval,
            vec![Value::Int(8), Value::symbol("face")],
        )
        .unwrap();
        assert!(matches!(result, Value::Int(6)));
    }

    #[test]
    fn previous_single_property_change_from_interval_end_boundary() {
        let mut eval = eval_with_text("abcd");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::symbol("p"),
                Value::symbol("v"),
            ],
        )
        .unwrap();

        let result = builtin_previous_single_property_change(
            &mut eval,
            vec![Value::Int(4), Value::symbol("p")],
        )
        .unwrap();
        assert!(matches!(result, Value::Int(2)));
    }

    // -----------------------------------------------------------------------
    // text-property-any
    // -----------------------------------------------------------------------

    #[test]
    fn text_property_any_found() {
        let mut eval = eval_with_text("hello world");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(3),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        let result = builtin_text_property_any(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(10),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();
        // Should find it at position 3.
        assert!(matches!(result, Value::Int(3)));
    }

    #[test]
    fn text_property_any_not_found() {
        let mut eval = eval_with_text("hello");
        let result = builtin_text_property_any(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(6),
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();
        assert!(matches!(result, Value::Nil));
    }

    #[test]
    fn text_property_not_all_reports_first_mismatch() {
        let mut eval = eval_with_text("abcd");
        builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::symbol("p"),
                Value::symbol("v"),
            ],
        )
        .unwrap();

        let mismatch = builtin_text_property_not_all(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(5),
                Value::symbol("p"),
                Value::symbol("v"),
            ],
        )
        .unwrap();
        let no_mismatch = builtin_text_property_not_all(
            &mut eval,
            vec![
                Value::Int(2),
                Value::Int(4),
                Value::symbol("p"),
                Value::symbol("v"),
            ],
        )
        .unwrap();
        assert!(matches!(mismatch, Value::Int(1)));
        assert!(no_mismatch.is_nil());
    }

    // -----------------------------------------------------------------------
    // make-overlay / delete-overlay
    // -----------------------------------------------------------------------

    #[test]
    fn make_and_delete_overlay() {
        let mut eval = eval_with_text("hello world");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        // Should be a cons.
        assert!(matches!(ov, Value::Cons(_)));

        // Delete it.
        let result = builtin_delete_overlay(&mut eval, vec![ov]);
        assert!(result.is_ok());
    }

    // -----------------------------------------------------------------------
    // overlay-put / overlay-get
    // -----------------------------------------------------------------------

    #[test]
    fn overlay_put_and_get() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        builtin_overlay_put(
            &mut eval,
            vec![ov.clone(), Value::symbol("face"), Value::symbol("bold")],
        )
        .unwrap();

        let result =
            builtin_overlay_get(&mut eval, vec![ov.clone(), Value::symbol("face")]).unwrap();
        assert!(matches!(result, Value::Symbol(s) if s == "bold"));
    }

    #[test]
    fn overlay_get_absent_property() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        let result = builtin_overlay_get(&mut eval, vec![ov, Value::symbol("missing")]).unwrap();
        assert!(matches!(result, Value::Nil));
    }

    // -----------------------------------------------------------------------
    // overlayp
    // -----------------------------------------------------------------------

    #[test]
    fn overlayp_true() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        let result = builtin_overlayp(&mut eval, vec![ov]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn overlayp_false() {
        let mut eval = Evaluator::new();
        let result = builtin_overlayp(&mut eval, vec![Value::Int(42)]).unwrap();
        assert!(matches!(result, Value::Nil));
    }

    // -----------------------------------------------------------------------
    // overlays-at / overlays-in
    // -----------------------------------------------------------------------

    #[test]
    fn overlays_at_finds_overlay() {
        let mut eval = eval_with_text("hello world");
        let _ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        let result = builtin_overlays_at(&mut eval, vec![Value::Int(3)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 1);
    }

    #[test]
    fn overlays_at_outside() {
        let mut eval = eval_with_text("hello world");
        let _ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(3)]).unwrap();

        let result = builtin_overlays_at(&mut eval, vec![Value::Int(5)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 0);
    }

    #[test]
    fn overlays_in_basic() {
        let mut eval = eval_with_text("hello world");
        builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();
        builtin_make_overlay(&mut eval, vec![Value::Int(4), Value::Int(10)]).unwrap();

        let result = builtin_overlays_in(&mut eval, vec![Value::Int(1), Value::Int(12)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn next_previous_overlay_change_boundaries() {
        let mut eval = eval_with_text("abcd");
        let no_overlay_next = builtin_next_overlay_change(&mut eval, vec![Value::Int(1)]).unwrap();
        let no_overlay_prev =
            builtin_previous_overlay_change(&mut eval, vec![Value::Int(4)]).unwrap();
        assert!(matches!(no_overlay_next, Value::Int(5)));
        assert!(matches!(no_overlay_prev, Value::Int(1)));

        builtin_make_overlay(&mut eval, vec![Value::Int(2), Value::Int(4)]).unwrap();
        let next_from_1 = builtin_next_overlay_change(&mut eval, vec![Value::Int(1)]).unwrap();
        let next_from_2 = builtin_next_overlay_change(&mut eval, vec![Value::Int(2)]).unwrap();
        let prev_from_4 = builtin_previous_overlay_change(&mut eval, vec![Value::Int(4)]).unwrap();
        let prev_from_2 = builtin_previous_overlay_change(&mut eval, vec![Value::Int(2)]).unwrap();
        assert!(matches!(next_from_1, Value::Int(2)));
        assert!(matches!(next_from_2, Value::Int(4)));
        assert!(matches!(prev_from_4, Value::Int(2)));
        assert!(matches!(prev_from_2, Value::Int(1)));
    }

    // -----------------------------------------------------------------------
    // move-overlay
    // -----------------------------------------------------------------------

    #[test]
    fn move_overlay_changes_range() {
        let mut eval = eval_with_text("hello world");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        builtin_move_overlay(&mut eval, vec![ov.clone(), Value::Int(3), Value::Int(8)]).unwrap();

        let start = builtin_overlay_start(&mut eval, vec![ov.clone()]).unwrap();
        let end = builtin_overlay_end(&mut eval, vec![ov]).unwrap();
        assert!(matches!(start, Value::Int(3)));
        assert!(matches!(end, Value::Int(8)));
    }

    // -----------------------------------------------------------------------
    // overlay-start / overlay-end
    // -----------------------------------------------------------------------

    #[test]
    fn overlay_start_and_end() {
        let mut eval = eval_with_text("hello world");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(2), Value::Int(8)]).unwrap();

        let start = builtin_overlay_start(&mut eval, vec![ov.clone()]).unwrap();
        let end = builtin_overlay_end(&mut eval, vec![ov]).unwrap();
        assert!(matches!(start, Value::Int(2)));
        assert!(matches!(end, Value::Int(8)));
    }

    // -----------------------------------------------------------------------
    // overlay-buffer
    // -----------------------------------------------------------------------

    #[test]
    fn overlay_buffer_returns_buffer() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(3)]).unwrap();

        let result = builtin_overlay_buffer(&mut eval, vec![ov]).unwrap();
        assert!(matches!(result, Value::Buffer(_)));
    }

    // -----------------------------------------------------------------------
    // overlay-properties
    // -----------------------------------------------------------------------

    #[test]
    fn overlay_properties_returns_plist() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();

        builtin_overlay_put(
            &mut eval,
            vec![ov.clone(), Value::symbol("face"), Value::symbol("bold")],
        )
        .unwrap();
        builtin_overlay_put(
            &mut eval,
            vec![ov.clone(), Value::symbol("priority"), Value::Int(10)],
        )
        .unwrap();

        let result = builtin_overlay_properties(&mut eval, vec![ov]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 4); // 2 properties * 2 (key+value)
    }

    #[test]
    fn overlay_properties_empty() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(3)]).unwrap();

        let result = builtin_overlay_properties(&mut eval, vec![ov]).unwrap();
        // Empty plist is nil.
        assert!(result.is_nil());
    }

    // -----------------------------------------------------------------------
    // remove-overlays
    // -----------------------------------------------------------------------

    #[test]
    fn remove_overlays_all() {
        let mut eval = eval_with_text("hello world");
        builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();
        builtin_make_overlay(&mut eval, vec![Value::Int(3), Value::Int(10)]).unwrap();

        builtin_remove_overlays(&mut eval, vec![]).unwrap();

        let result = builtin_overlays_in(&mut eval, vec![Value::Int(1), Value::Int(12)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 0);
    }

    #[test]
    fn remove_overlays_by_property() {
        let mut eval = eval_with_text("hello world");
        let ov1 = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(6)]).unwrap();
        let ov2 = builtin_make_overlay(&mut eval, vec![Value::Int(3), Value::Int(10)]).unwrap();

        builtin_overlay_put(
            &mut eval,
            vec![ov1, Value::symbol("face"), Value::symbol("bold")],
        )
        .unwrap();
        builtin_overlay_put(
            &mut eval,
            vec![ov2, Value::symbol("face"), Value::symbol("italic")],
        )
        .unwrap();

        // Remove only overlays with face = bold.
        builtin_remove_overlays(
            &mut eval,
            vec![
                Value::Nil,
                Value::Nil,
                Value::symbol("face"),
                Value::symbol("bold"),
            ],
        )
        .unwrap();

        let result = builtin_overlays_in(&mut eval, vec![Value::Int(1), Value::Int(12)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 1); // only the italic one remains
    }

    // -----------------------------------------------------------------------
    // Wrong argument count tests
    // -----------------------------------------------------------------------

    #[test]
    fn put_text_property_wrong_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_put_text_property(&mut eval, vec![Value::Int(1), Value::Int(3)]);
        assert!(result.is_err());
    }

    #[test]
    fn put_text_property_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_put_text_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("face"),
                Value::symbol("bold"),
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn get_text_property_wrong_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_get_text_property(&mut eval, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn get_text_property_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_get_text_property(
            &mut eval,
            vec![Value::Int(1), Value::symbol("face"), Value::Nil, Value::Nil],
        );
        assert!(result.is_err());
    }

    #[test]
    fn get_char_property_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_get_char_property(
            &mut eval,
            vec![Value::Int(1), Value::symbol("face"), Value::Nil, Value::Nil],
        );
        assert!(result.is_err());
    }

    #[test]
    fn get_char_property_and_overlay_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_get_char_property_and_overlay(
            &mut eval,
            vec![Value::Int(1), Value::symbol("face"), Value::Nil, Value::Nil],
        );
        assert!(result.is_err());
    }

    #[test]
    fn get_display_property_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_get_display_property(
            &mut eval,
            vec![
                Value::Int(1),
                Value::symbol("face"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn overlay_put_wrong_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_overlay_put(&mut eval, vec![Value::Int(42), Value::symbol("face")]);
        assert!(result.is_err());
    }

    #[test]
    fn text_properties_at_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result =
            builtin_text_properties_at(&mut eval, vec![Value::Int(1), Value::Nil, Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn text_property_any_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_text_property_any(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("face"),
                Value::symbol("bold"),
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn text_property_not_all_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_text_property_not_all(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::symbol("face"),
                Value::symbol("bold"),
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn set_text_properties_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_set_text_properties(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn remove_list_of_text_properties_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_remove_list_of_text_properties(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn remove_overlays_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_remove_overlays(
            &mut eval,
            vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil, Value::Nil],
        );
        assert!(result.is_err());
    }

    #[test]
    fn make_overlay_wrong_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_make_overlay(&mut eval, vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn make_overlay_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_make_overlay(
            &mut eval,
            vec![
                Value::Int(1),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn overlays_at_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_overlays_at(&mut eval, vec![Value::Int(1), Value::Nil, Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn next_overlay_change_wrong_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_next_overlay_change(&mut eval, vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn previous_overlay_change_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_previous_overlay_change(&mut eval, vec![Value::Int(1), Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn next_property_change_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_next_property_change(
            &mut eval,
            vec![Value::Int(1), Value::Nil, Value::Nil, Value::Nil],
        );
        assert!(result.is_err());
    }

    #[test]
    fn next_single_property_change_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_next_single_property_change(
            &mut eval,
            vec![
                Value::Int(1),
                Value::symbol("face"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn previous_single_property_change_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_previous_single_property_change(
            &mut eval,
            vec![
                Value::Int(1),
                Value::symbol("face"),
                Value::Nil,
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    #[test]
    fn move_overlay_rejects_too_many_args() {
        let mut eval = eval_with_text("hello");
        let result = builtin_move_overlay(
            &mut eval,
            vec![
                Value::Nil,
                Value::Int(1),
                Value::Int(2),
                Value::Nil,
                Value::Nil,
            ],
        );
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // Integration: overlays with advance flags
    // -----------------------------------------------------------------------

    #[test]
    fn overlay_front_advance() {
        let mut eval = eval_with_text("hello world");
        // Create overlay with front-advance = t
        let ov = builtin_make_overlay(
            &mut eval,
            vec![
                Value::Int(3),
                Value::Int(8),
                Value::Nil,  // buffer
                Value::True, // front-advance
                Value::Nil,  // rear-advance
            ],
        )
        .unwrap();

        // Verify overlay was created.
        let start = builtin_overlay_start(&mut eval, vec![ov]).unwrap();
        assert!(matches!(start, Value::Int(3)));
    }

    #[test]
    fn overlay_rear_advance() {
        let mut eval = eval_with_text("hello world");
        let ov = builtin_make_overlay(
            &mut eval,
            vec![
                Value::Int(3),
                Value::Int(8),
                Value::Nil,
                Value::Nil,
                Value::True, // rear-advance
            ],
        )
        .unwrap();

        let end = builtin_overlay_end(&mut eval, vec![ov]).unwrap();
        assert!(matches!(end, Value::Int(8)));
    }

    // -----------------------------------------------------------------------
    // Edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn text_property_on_empty_buffer() {
        let mut eval = Evaluator::new();
        // Scratch buffer is empty.
        let result =
            builtin_get_text_property(&mut eval, vec![Value::Int(1), Value::symbol("face")]);
        assert!(matches!(result, Ok(Value::Nil)));
    }

    #[test]
    fn overlays_at_empty_buffer() {
        let mut eval = Evaluator::new();
        let result = builtin_overlays_at(&mut eval, vec![Value::Int(1)]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn delete_overlay_twice_is_ok() {
        let mut eval = eval_with_text("hello");
        let ov = builtin_make_overlay(&mut eval, vec![Value::Int(1), Value::Int(3)]).unwrap();

        builtin_delete_overlay(&mut eval, vec![ov.clone()]).unwrap();
        // Second delete should not crash.
        let result = builtin_delete_overlay(&mut eval, vec![ov]);
        assert!(result.is_ok());
    }
}
