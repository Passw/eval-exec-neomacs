use super::args::{expect_args, expect_max_args, expect_min_args};
use super::{EvalResult, Value};

/// (make-indirect-buffer BASE-BUFFER NAME &optional CLONE) -> nil
///
/// Create an indirect buffer sharing the text of BASE-BUFFER.
/// Stub: signals an error (indirect buffers not supported).
pub(crate) fn builtin_make_indirect_buffer(args: Vec<Value>) -> EvalResult {
    expect_min_args("make-indirect-buffer", &args, 2)?;
    expect_max_args("make-indirect-buffer", &args, 3)?;
    // Stub: indirect buffers not supported
    Ok(Value::Nil)
}

/// (buffer-base-buffer &optional BUFFER) -> nil
///
/// Return the base buffer of BUFFER if it is indirect, or nil.
/// Since we do not support indirect buffers, always returns nil.
pub(crate) fn builtin_buffer_base_buffer(args: Vec<Value>) -> EvalResult {
    expect_max_args("buffer-base-buffer", &args, 1)?;
    Ok(Value::Nil)
}
