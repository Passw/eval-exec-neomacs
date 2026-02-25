//! FFI bridge to the neovm-core Rust Evaluator.
//!
//! Provides C-callable functions to initialize, query, and evaluate Elisp
//! via the Rust Evaluator singleton.

use std::ffi::{c_char, c_int, CStr, CString};

/// Global Evaluator instance (lazily initialized via `neomacs_rust_eval_init`).
static mut RUST_EVALUATOR: Option<neovm_core::elisp::Evaluator> = None;

/// Initialize the Rust Evaluator singleton.
///
/// Returns 0 on success, -1 on failure.  Safe to call multiple times —
/// subsequent calls are no-ops that return 0.
///
/// # Safety
/// Must be called from the Emacs main thread before any other eval_bridge
/// functions.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_init() -> c_int {
    if (*std::ptr::addr_of!(RUST_EVALUATOR)).is_some() {
        return 0; // already initialized
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let evaluator = neovm_core::elisp::Evaluator::new();
        *std::ptr::addr_of_mut!(RUST_EVALUATOR) = Some(evaluator);
        log::info!("Rust Evaluator initialized");
    }));

    match result {
        Ok(()) => 0,
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else {
                "unknown panic".to_string()
            };
            log::error!("neomacs_rust_eval_init: panic during initialization: {}", msg);
            -1
        }
    }
}

/// Parse and evaluate an Elisp string, returning the printed result as a
/// newly-allocated C string.
///
/// The caller **must** free the returned pointer with `neomacs_rust_free_string`.
/// Returns `NULL` on error (parse failure, eval error, or uninitialized
/// evaluator).  Error details are logged.
///
/// # Safety
/// `input` must be a valid, NUL-terminated UTF-8 C string.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_string(input: *const c_char) -> *mut c_char {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if input.is_null() {
            log::error!("neomacs_rust_eval_string: null input");
            return std::ptr::null_mut();
        }

        let eval = match (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut() {
            Some(e) => e,
            None => {
                log::error!("neomacs_rust_eval_string: evaluator not initialized");
                return std::ptr::null_mut();
            }
        };

        eval.setup_thread_locals();

        let c_str = match CStr::from_ptr(input).to_str() {
            Ok(s) => s,
            Err(e) => {
                log::error!("neomacs_rust_eval_string: invalid UTF-8: {}", e);
                return std::ptr::null_mut();
            }
        };

        let forms = match neovm_core::elisp::parse_forms(c_str) {
            Ok(f) => f,
            Err(e) => {
                log::error!("neomacs_rust_eval_string: parse error: {}", e);
                return std::ptr::null_mut();
            }
        };

        let mut last_value = None;
        for form in &forms {
            match eval.eval_expr(form) {
                Ok(value) => {
                    last_value = Some(value);
                }
                Err(e) => {
                    log::error!("neomacs_rust_eval_string: eval error: {:?}", e);
                    return std::ptr::null_mut();
                }
            }
        }

        let printed = match last_value {
            Some(ref v) => neovm_core::elisp::print_value_with_eval(eval, v),
            None => "nil".to_string(),
        };

        match CString::new(printed) {
            Ok(cs) => cs.into_raw(),
            Err(e) => {
                log::error!("neomacs_rust_eval_string: result contains NUL byte: {}", e);
                std::ptr::null_mut()
            }
        }
    }));

    match result {
        Ok(ptr) => ptr,
        Err(_) => {
            log::error!("neomacs_rust_eval_string: panic during evaluation");
            std::ptr::null_mut()
        }
    }
}

/// Free a C string previously returned by `neomacs_rust_eval_string`.
///
/// # Safety
/// `s` must be a pointer returned by `neomacs_rust_eval_string`, or NULL.
/// Each pointer must be freed exactly once.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_free_string(s: *mut c_char) {
    if !s.is_null() {
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            drop(CString::from_raw(s));
        }));
        if let Err(_) = result {
            log::error!("neomacs_rust_free_string: panic during drop");
        }
    }
}

/// Check whether the Rust Evaluator has been initialized.
///
/// Returns 1 if initialized, 0 if not.
///
/// # Safety
/// May be called from any thread, but the result is only meaningful on the
/// Emacs main thread (where initialization happens).
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_eval_ready() -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if (*std::ptr::addr_of!(RUST_EVALUATOR)).is_some() {
            1
        } else {
            0
        }
    }));
    match result {
        Ok(v) => v,
        Err(_) => {
            log::error!("neomacs_rust_eval_ready: panic during static read");
            0
        }
    }
}

/// Load an Elisp file through the Rust Evaluator.
///
/// `path` is a NUL-terminated file path string.  The file is loaded via
/// `(load "path")`, which searches `load-path` and handles `.el` suffix
/// resolution and `.neoc` parse caching.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
/// `path` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_load_file(path: *const c_char) -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if path.is_null() {
            log::error!("neomacs_rust_load_file: null path");
            return -1;
        }

        let eval = match (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut() {
            Some(e) => e,
            None => {
                log::error!("neomacs_rust_load_file: evaluator not initialized");
                return -1;
            }
        };

        eval.setup_thread_locals();

        let path_str = match CStr::from_ptr(path).to_str() {
            Ok(s) => s,
            Err(e) => {
                log::error!("neomacs_rust_load_file: invalid UTF-8: {}", e);
                return -1;
            }
        };

        // Build the Lisp expression (load "path") and evaluate it.
        let load_expr = format!("(load \"{}\")", path_str.replace('\\', "\\\\").replace('"', "\\\""));
        let forms = match neovm_core::elisp::parse_forms(&load_expr) {
            Ok(f) => f,
            Err(e) => {
                log::error!("neomacs_rust_load_file: parse error for '{}': {}", path_str, e);
                return -1;
            }
        };

        for form in &forms {
            match eval.eval_expr(form) {
                Ok(_) => {}
                Err(e) => {
                    log::error!("neomacs_rust_load_file: eval error loading '{}': {:?}", path_str, e);
                    return -1;
                }
            }
        }

        log::info!("neomacs_rust_load_file: loaded '{}'", path_str);
        0
    }));

    match result {
        Ok(code) => code,
        Err(_) => {
            log::error!("neomacs_rust_load_file: panic during load");
            -1
        }
    }
}

/// Handle a key event from C's command loop.
///
/// Sets `last-command-event` to the key value and evaluates
/// `(command-execute (key-binding (vector key)))` through the Rust
/// Evaluator — or, for printable ASCII, calls `(self-insert-command 1)`.
///
/// `key` is the Emacs character code (e.g., 97 for 'a', 13 for RET).
/// `modifiers` is the modifier bitmask (ctrl=1, meta=2, shift=4, super=8).
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
/// Must be called from the Emacs main thread.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_handle_key(
    key: c_int,
    modifiers: c_int,
) -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let eval = match (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut() {
            Some(e) => e,
            None => {
                log::error!("neomacs_rust_handle_key: evaluator not initialized");
                return -1;
            }
        };

        eval.setup_thread_locals();

        // Set last-command-event so self-insert-command knows what char to insert
        eval.set_variable(
            "last-command-event",
            neovm_core::elisp::Value::Int(key as i64),
        );

        // For basic printable ASCII with no modifiers, use self-insert-command directly
        if modifiers == 0 && (32..=126).contains(&key) {
            let expr_str = "(self-insert-command 1)";
            match neovm_core::elisp::parse_forms(expr_str) {
                Ok(forms) => {
                    for form in &forms {
                        if let Err(e) = eval.eval_expr(form) {
                            log::error!("neomacs_rust_handle_key: eval error: {:?}", e);
                            return -1;
                        }
                    }
                }
                Err(e) => {
                    log::error!("neomacs_rust_handle_key: parse error: {}", e);
                    return -1;
                }
            }
        }
        // TODO: For non-ASCII and modified keys, look up key-binding and call command-execute.
        // This will be expanded in Phase 2 when keymaps are integrated.

        0
    }));

    match result {
        Ok(code) => code,
        Err(_) => {
            log::error!("neomacs_rust_handle_key: panic");
            -1
        }
    }
}

/// Get a shared reference to the Rust Evaluator.
///
/// # Safety
/// Only call from the main thread. The returned reference is valid until
/// the next mutable access (eval call).
pub(crate) unsafe fn get_evaluator() -> Option<&'static neovm_core::elisp::Evaluator> {
    (*std::ptr::addr_of!(RUST_EVALUATOR)).as_ref()
}

/// Get a mutable reference to the Rust Evaluator.
///
/// # Safety
/// Only call from the main thread.
pub(crate) unsafe fn get_evaluator_mut() -> Option<&'static mut neovm_core::elisp::Evaluator> {
    (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut()
}

/// Set the Evaluator's `load-path` from a colon-separated string of directories.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
/// `paths` must be a valid, NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn neomacs_rust_set_load_path(paths: *const c_char) -> c_int {
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        if paths.is_null() {
            log::error!("neomacs_rust_set_load_path: null paths");
            return -1;
        }

        let eval = match (*std::ptr::addr_of_mut!(RUST_EVALUATOR)).as_mut() {
            Some(e) => e,
            None => {
                log::error!("neomacs_rust_set_load_path: evaluator not initialized");
                return -1;
            }
        };

        eval.setup_thread_locals();

        let paths_str = match CStr::from_ptr(paths).to_str() {
            Ok(s) => s,
            Err(e) => {
                log::error!("neomacs_rust_set_load_path: invalid UTF-8: {}", e);
                return -1;
            }
        };

        // Build a Lisp list of directory strings from the colon-separated input.
        let dirs: Vec<neovm_core::elisp::Value> = paths_str
            .split(':')
            .filter(|s| !s.is_empty())
            .map(|s| neovm_core::elisp::Value::string(s))
            .collect();

        let list = neovm_core::elisp::Value::list(dirs);
        eval.set_variable("load-path", list);

        log::info!("neomacs_rust_set_load_path: set load-path from '{}'", paths_str);
        0
    }));

    match result {
        Ok(code) => code,
        Err(_) => {
            log::error!("neomacs_rust_set_load_path: panic during set");
            -1
        }
    }
}
