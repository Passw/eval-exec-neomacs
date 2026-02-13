use super::args::extract_seconds;
use super::*;

// =======================================================================
// redraw-frame
// =======================================================================

#[test]
fn test_redraw_frame_no_args() {
    let result = builtin_redraw_frame(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_redraw_frame_with_frame() {
    let result = builtin_redraw_frame(vec![Value::symbol("some-frame")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_redraw_frame_too_many_args() {
    let result = builtin_redraw_frame(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// redraw-display
// =======================================================================

#[test]
fn test_redraw_display() {
    let result = builtin_redraw_display(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_redraw_display_wrong_args() {
    let result = builtin_redraw_display(vec![Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// open-termscript
// =======================================================================

#[test]
fn test_open_termscript() {
    let result = builtin_open_termscript(vec![Value::string("/tmp/termscript")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_open_termscript_nil() {
    let result = builtin_open_termscript(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_open_termscript_no_args() {
    let result = builtin_open_termscript(vec![]);
    assert!(result.is_err());
}

#[test]
fn test_open_termscript_too_many_args() {
    let result = builtin_open_termscript(vec![Value::string("a"), Value::string("b")]);
    assert!(result.is_err());
}

// =======================================================================
// ding
// =======================================================================

#[test]
fn test_ding_no_args() {
    let result = builtin_ding(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_ding_with_arg() {
    let result = builtin_ding(vec![Value::True]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_ding_too_many_args() {
    let result = builtin_ding(vec![Value::True, Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// send-string-to-terminal
// =======================================================================

#[test]
fn test_send_string_to_terminal() {
    let result = builtin_send_string_to_terminal(vec![Value::string("hello")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_send_string_to_terminal_with_terminal() {
    let result =
        builtin_send_string_to_terminal(vec![Value::string("hello"), Value::symbol("terminal")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_send_string_to_terminal_no_args() {
    let result = builtin_send_string_to_terminal(vec![]);
    assert!(result.is_err());
}

#[test]
fn test_send_string_to_terminal_too_many_args() {
    let result =
        builtin_send_string_to_terminal(vec![Value::string("a"), Value::symbol("t"), Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// internal-show-cursor
// =======================================================================

#[test]
fn test_internal_show_cursor() {
    let result = builtin_internal_show_cursor(vec![Value::Nil, Value::True]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_internal_show_cursor_hide() {
    let result = builtin_internal_show_cursor(vec![Value::symbol("window"), Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_internal_show_cursor_wrong_arity() {
    let result = builtin_internal_show_cursor(vec![Value::Nil]);
    assert!(result.is_err());

    let result = builtin_internal_show_cursor(vec![]);
    assert!(result.is_err());

    let result = builtin_internal_show_cursor(vec![Value::Nil, Value::True, Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// internal-show-cursor-p
// =======================================================================

#[test]
fn test_internal_show_cursor_p_no_args() {
    let result = builtin_internal_show_cursor_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_internal_show_cursor_p_with_window() {
    let result = builtin_internal_show_cursor_p(vec![Value::symbol("window")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_internal_show_cursor_p_too_many_args() {
    let result = builtin_internal_show_cursor_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// last-nonminibuffer-frame
// =======================================================================

#[test]
fn test_last_nonminibuffer_frame() {
    let result = builtin_last_nonminibuffer_frame(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_last_nonminibuffer_frame_wrong_args() {
    let result = builtin_last_nonminibuffer_frame(vec![Value::Nil]);
    assert!(result.is_err());
}

// =======================================================================
// sleep-for
// =======================================================================

#[test]
fn test_sleep_for_zero() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sleep_for(&mut eval, vec![Value::Int(0)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_sleep_for_small_float() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let start = std::time::Instant::now();
    let result = builtin_sleep_for(&mut eval, vec![Value::Float(0.01)]);
    let elapsed = start.elapsed();
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
    assert!(elapsed >= Duration::from_millis(5));
}

#[test]
fn test_sleep_for_with_milliseconds() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let start = std::time::Instant::now();
    let result = builtin_sleep_for(&mut eval, vec![Value::Int(0), Value::Int(10)]);
    let elapsed = start.elapsed();
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
    assert!(elapsed >= Duration::from_millis(5));
}

#[test]
fn test_sleep_for_negative() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // Negative seconds should not sleep (clamped to 0)
    let result = builtin_sleep_for(&mut eval, vec![Value::Int(-1)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_sleep_for_no_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sleep_for(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_sleep_for_too_many_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sleep_for(&mut eval, vec![Value::Int(0), Value::Int(0), Value::Int(0)]);
    assert!(result.is_err());
}

#[test]
fn test_sleep_for_wrong_type() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sleep_for(&mut eval, vec![Value::string("1")]);
    assert!(result.is_err());
}

#[test]
fn test_sleep_for_nil_millis() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // nil as MILLISECONDS should be treated as 0
    let result = builtin_sleep_for(&mut eval, vec![Value::Int(0), Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

// =======================================================================
// sit-for
// =======================================================================

#[test]
fn test_sit_for_zero() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(&mut eval, vec![Value::Int(0)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_sit_for_small_float() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let start = std::time::Instant::now();
    let result = builtin_sit_for(&mut eval, vec![Value::Float(0.01)]);
    let elapsed = start.elapsed();
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
    assert!(elapsed >= Duration::from_millis(5));
}

#[test]
fn test_sit_for_with_nodisp() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(&mut eval, vec![Value::Int(0), Value::True]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_sit_for_with_obsolete() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(&mut eval, vec![Value::Int(0), Value::Nil, Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_sit_for_negative() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    // Negative seconds should not sleep (clamped to 0)
    let result = builtin_sit_for(&mut eval, vec![Value::Float(-0.5)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_sit_for_no_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_sit_for_too_many_args() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(
        &mut eval,
        vec![Value::Int(0), Value::Nil, Value::Nil, Value::Nil],
    );
    assert!(result.is_err());
}

#[test]
fn test_sit_for_wrong_type() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(&mut eval, vec![Value::string("1")]);
    assert!(result.is_err());
}

#[test]
fn test_sit_for_returns_t() {
    use super::super::eval::Evaluator;

    let mut eval = Evaluator::new();
    let result = builtin_sit_for(&mut eval, vec![Value::Int(0)]);
    assert!(result.is_ok());
    match result.unwrap() {
        Value::True => {} // expected
        other => panic!("expected t, got {:?}", other),
    }
}

// =======================================================================
// extract_seconds helper
// =======================================================================

#[test]
fn test_extract_seconds_int() {
    let result = extract_seconds(&Value::Int(5));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 5.0);
}

#[test]
fn test_extract_seconds_float() {
    let result = extract_seconds(&Value::Float(2.5));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 2.5);
}

#[test]
fn test_extract_seconds_negative_int() {
    let result = extract_seconds(&Value::Int(-3));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), -3.0);
}

#[test]
fn test_extract_seconds_zero() {
    let result = extract_seconds(&Value::Int(0));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 0.0);
}

#[test]
fn test_extract_seconds_string() {
    let result = extract_seconds(&Value::string("5"));
    assert!(result.is_err());
}

#[test]
fn test_extract_seconds_nil() {
    let result = extract_seconds(&Value::Nil);
    assert!(result.is_err());
}

#[test]
fn test_extract_seconds_symbol() {
    let result = extract_seconds(&Value::symbol("foo"));
    assert!(result.is_err());
}
