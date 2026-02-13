use std::time::Duration;

use super::super::eval::Evaluator;
use super::args::{expect_args_range, expect_min_args, extract_seconds};
use super::{signal, EvalResult, Value};

/// (sleep-for SECONDS &optional MILLISECONDS) -> nil
///
/// Pause execution for SECONDS seconds (may be a float for fractional seconds).
/// If MILLISECONDS is provided, add that many milliseconds to the sleep time.
/// Returns nil.
pub(crate) fn builtin_sleep_for(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("sleep-for", &args, 1)?;
    expect_args_range("sleep-for", &args, 1, 2)?;

    let secs = extract_seconds(&args[0])?;

    let millis = if args.len() > 1 {
        match &args[1] {
            Value::Int(n) => *n as f64,
            Value::Float(f) => *f,
            Value::Nil => 0.0,
            other => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("numberp"), other.clone()],
                ));
            }
        }
    } else {
        0.0
    };

    let total_secs = secs + millis / 1000.0;
    if total_secs > 0.0 {
        std::thread::sleep(Duration::from_secs_f64(total_secs));
    }

    Ok(Value::Nil)
}

/// (sit-for SECONDS &optional NODISP OBSOLETE) -> t
///
/// Wait for SECONDS seconds or until input is available.
/// SECONDS may be a float for fractional seconds.
/// NODISP non-nil means don't redisplay (ignored in this stub).
/// Returns t (in a full implementation, returns nil if interrupted by input).
pub(crate) fn builtin_sit_for(_eval: &mut Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("sit-for", &args, 1)?;
    expect_args_range("sit-for", &args, 1, 3)?;

    let secs = extract_seconds(&args[0])?;

    if secs > 0.0 {
        std::thread::sleep(Duration::from_secs_f64(secs));
    }

    Ok(Value::True)
}
