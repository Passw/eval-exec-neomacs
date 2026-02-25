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
