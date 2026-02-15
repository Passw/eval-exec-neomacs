use super::super::eval::Evaluator;
use super::args::{
    expect_args, expect_max_args, expect_min_args, expect_string, resolve_buffer_arg,
    resolve_buffer_or_name_opt,
};
use super::{signal, EvalResult, Value};

/// (current-buffer) -> buffer
///
/// Return the current buffer as a Lisp value.
pub(crate) fn builtin_current_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    let _ = args;
    match eval.buffers.current_buffer() {
        Some(buf) => Ok(Value::Buffer(buf.id)),
        None => Ok(Value::Nil),
    }
}

/// (set-buffer BUFFER) -> buffer
///
/// Make BUFFER (a buffer or string name) the current buffer.
pub(crate) fn builtin_set_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set-buffer", &args, 1)?;
    let id = match &args[0] {
        Value::Buffer(id) => *id,
        Value::Str(s) => eval
            .buffers
            .find_buffer_by_name(s)
            .ok_or_else(|| signal("error", vec![Value::string(format!("No buffer named {s}"))]))?,
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ))
        }
    };
    eval.buffers.set_current(id);
    Ok(Value::Buffer(id))
}

/// (get-buffer BUFFER-OR-NAME) -> buffer or nil
///
/// Return the buffer specified by BUFFER-OR-NAME.
/// BUFFER-OR-NAME may be a buffer (returned as-is if live) or a string name.
pub(crate) fn builtin_get_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("get-buffer", &args, 1)?;
    match &args[0] {
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_some() {
                Ok(args[0].clone())
            } else {
                Ok(Value::Nil)
            }
        }
        Value::Str(s) => {
            if let Some(id) = eval.buffers.find_buffer_by_name(s) {
                Ok(Value::Buffer(id))
            } else {
                Ok(Value::Nil)
            }
        }
        _ => Ok(Value::Nil),
    }
}

/// (get-buffer-create NAME &optional INHIBIT-HOOKS) -> buffer
///
/// Return the buffer named NAME, creating it if it does not exist.
pub(crate) fn builtin_get_buffer_create(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("get-buffer-create", &args, 1)?;
    expect_max_args("get-buffer-create", &args, 2)?;
    let name = expect_string(&args[0])?;
    if let Some(id) = eval.buffers.find_buffer_by_name(&name) {
        Ok(Value::Buffer(id))
    } else {
        let id = eval.buffers.create_buffer(&name);
        Ok(Value::Buffer(id))
    }
}

/// (generate-new-buffer-name NAME &optional IGNORE) -> string
///
/// Return a string that is the name of no existing buffer based on NAME.
/// If IGNORE is provided and matches a candidate, that candidate is still
/// considered available.
pub(crate) fn builtin_generate_new_buffer_name(
    eval: &mut Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("generate-new-buffer-name", &args, 1)?;
    expect_max_args("generate-new-buffer-name", &args, 2)?;
    let base = expect_string(&args[0])?;
    let ignore = if args.len() > 1 {
        match &args[1] {
            Value::Str(s) => Some((**s).clone()),
            Value::Nil => None,
            _ => None,
        }
    } else {
        None
    };

    // If the base name is free (or matches IGNORE), return it.
    match eval.buffers.find_buffer_by_name(&base) {
        None => return Ok(Value::string(base)),
        Some(id) => {
            if let Some(ref ign) = ignore {
                if let Some(buf) = eval.buffers.get(id) {
                    if buf.name == *ign {
                        return Ok(Value::string(base));
                    }
                }
            }
        }
    }

    // Otherwise use the standard generation which appends <2>, <3>, ...
    Ok(Value::string(eval.buffers.generate_new_buffer_name(&base)))
}

/// (buffer-list &optional FRAME) -> list
///
/// Return a list of all live buffers.  FRAME is ignored.
pub(crate) fn builtin_buffer_list(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    let _ = args;
    let ids = eval.buffers.buffer_list();
    let vals: Vec<Value> = ids.into_iter().map(Value::Buffer).collect();
    Ok(Value::list(vals))
}

/// (buffer-name &optional BUFFER) -> string or nil
///
/// Return the name of BUFFER (default: current buffer).
pub(crate) fn builtin_buffer_name(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("buffer-name", &args, 1)?;
    let id = resolve_buffer_arg(&eval, args.first())?;
    match eval.buffers.get(id) {
        Some(buf) => Ok(Value::string(&buf.name)),
        None => Ok(Value::Nil),
    }
}

/// (buffer-file-name &optional BUFFER) -> string or nil
///
/// Return the file name of BUFFER, or nil if it has none.
pub(crate) fn builtin_buffer_file_name(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("buffer-file-name", &args, 1)?;
    let id = resolve_buffer_arg(&eval, args.first())?;
    match eval.buffers.get(id) {
        Some(buf) => match &buf.file_name {
            Some(f) => Ok(Value::string(f)),
            None => Ok(Value::Nil),
        },
        None => Ok(Value::Nil),
    }
}

/// (buffer-modified-p &optional BUFFER) -> t or nil
///
/// Return t if BUFFER has been modified since last save.
pub(crate) fn builtin_buffer_modified_p(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("buffer-modified-p", &args, 1)?;
    let id = resolve_buffer_arg(&eval, args.first())?;
    match eval.buffers.get(id) {
        Some(buf) => Ok(Value::bool(buf.is_modified())),
        None => Ok(Value::Nil),
    }
}

/// (set-buffer-modified-p FLAG) -> FLAG
///
/// Set the modified flag of the current buffer to FLAG.
pub(crate) fn builtin_set_buffer_modified_p(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("set-buffer-modified-p", &args, 1)?;
    let flag = args[0].is_truthy();
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.set_modified(flag);
    Ok(args[0].clone())
}

/// (buffer-live-p OBJECT) -> t or nil
///
/// Return t if OBJECT is a buffer that has not been killed.
pub(crate) fn builtin_buffer_live_p(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("buffer-live-p", &args, 1)?;
    match &args[0] {
        Value::Buffer(id) => Ok(Value::bool(eval.buffers.get(*id).is_some())),
        _ => Ok(Value::Nil),
    }
}

/// (rename-buffer NEWNAME &optional UNIQUE) -> string
///
/// Change the name of the current buffer to NEWNAME.
/// If UNIQUE is non-nil and NEWNAME is already taken, generate a unique name.
/// Returns the new name.
pub(crate) fn builtin_rename_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("rename-buffer", &args, 1)?;
    expect_max_args("rename-buffer", &args, 2)?;
    let newname = expect_string(&args[0])?;
    let unique = args.get(1).is_some_and(|v| v.is_truthy());

    let cur_id = eval
        .buffers
        .current_buffer()
        .map(|b| b.id)
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let final_name = if unique {
        // Check if the name is already taken by a different buffer.
        match eval.buffers.find_buffer_by_name(&newname) {
            Some(id) if id != cur_id => eval.buffers.generate_new_buffer_name(&newname),
            _ => newname,
        }
    } else {
        // Check for conflict.
        if let Some(id) = eval.buffers.find_buffer_by_name(&newname) {
            if id != cur_id {
                return Err(signal(
                    "error",
                    vec![Value::string(format!("Buffer name `{newname}' is in use"))],
                ));
            }
        }
        newname
    };

    if let Some(buf) = eval.buffers.get_mut(cur_id) {
        buf.name = final_name.clone();
    }

    Ok(Value::string(final_name))
}

/// (other-buffer &optional BUFFER VISIBLE-OK FRAME) -> buffer
///
/// Return a buffer other than BUFFER.  If no other buffer exists, return
/// the `*scratch*` buffer or the first available buffer.
pub(crate) fn builtin_other_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("other-buffer", &args, 3)?;

    let exclude_id = resolve_buffer_or_name_opt(&eval, args.first());

    let ids = eval.buffers.buffer_list();
    for id in &ids {
        if Some(*id) != exclude_id {
            return Ok(Value::Buffer(*id));
        }
    }
    // All buffers excluded (or only one buffer) -- return current or nil.
    match eval.buffers.current_buffer() {
        Some(buf) => Ok(Value::Buffer(buf.id)),
        None => Ok(Value::Nil),
    }
}

/// (kill-buffer &optional BUFFER-OR-NAME) -> t or nil
///
/// Kill BUFFER-OR-NAME.  Default is the current buffer.
/// Returns t if the buffer was killed, nil otherwise.
pub(crate) fn builtin_kill_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("kill-buffer", &args, 1)?;
    let id = match resolve_buffer_or_name_opt(&eval, args.first()) {
        Some(id) => id,
        None => return Ok(Value::Nil),
    };
    if eval.buffers.kill_buffer(id) {
        Ok(Value::True)
    } else {
        Ok(Value::Nil)
    }
}

/// (erase-buffer) -> nil
///
/// Delete the entire contents of the current buffer.
pub(crate) fn builtin_erase_buffer(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    let _ = args;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let len = buf.text.len();
    buf.delete_region(0, len);
    buf.widen();
    Ok(Value::Nil)
}

/// (buffer-size &optional BUFFER) -> integer
///
/// Return the number of characters in BUFFER (default: current buffer).
pub(crate) fn builtin_buffer_size(eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_max_args("buffer-size", &args, 1)?;
    let id = resolve_buffer_arg(&eval, args.first())?;
    let buf = eval
        .buffers
        .get(id)
        .ok_or_else(|| signal("error", vec![Value::string("No such buffer")]))?;
    Ok(Value::Int(buf.text.char_count() as i64))
}
