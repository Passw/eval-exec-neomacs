//! Composition builtins (complex script rendering).
//!
//! In real Emacs, the composition system handles combining characters,
//! ligatures, and complex script shaping.  Most of this work is done by the
//! display engine (and in Neomacs, by the Rust layout engine), so here we
//! provide stubs that satisfy Elisp code which queries or manipulates
//! compositions at the Lisp level.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

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

fn expect_integerp(arg: &Value) -> Result<(), Flow> {
    match arg {
        Value::Int(_) | Value::Char(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn expect_integer_or_marker_p(arg: &Value) -> Result<(), Flow> {
    match arg {
        Value::Int(_) | Value::Char(_) => Ok(()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// `(compose-region-internal START END &optional COMPONENTS MODIFICATION-FUNC)`
///
/// Compose text in the current buffer between START and END.
/// COMPONENTS, if given, is a vector or string describing the composition.
/// MODIFICATION-FUNC, if non-nil, is called when the composition is modified.
///
/// Stub: composition is handled by the display/layout engine; return nil.
pub(crate) fn builtin_compose_region_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("compose-region-internal", &args, 2, 4)?;
    expect_integer_or_marker_p(&args[0])?;
    expect_integer_or_marker_p(&args[1])?;
    Ok(Value::Nil)
}

/// `(compose-string-internal STRING START END &optional COMPONENTS MODIFICATION-FUNC)`
///
/// Compose text in STRING between indices START and END.
/// Returns STRING (possibly with composition properties attached).
///
/// Stub: return STRING unchanged.
pub(crate) fn builtin_compose_string_internal(args: Vec<Value>) -> EvalResult {
    expect_range_args("compose-string-internal", &args, 3, 5)?;
    if !args[0].is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0].clone()],
        ));
    }
    expect_integerp(&args[1])?;
    expect_integerp(&args[2])?;
    // Return the string argument unchanged.
    Ok(args[0].clone())
}

/// `(find-composition-internal POS LIMIT STRING DETAIL-P)`
///
/// Find a composition at or near position POS.
/// Returns a list describing the composition, or nil if none found.
///
/// Stub: no compositions exist, always return nil.
pub(crate) fn builtin_find_composition_internal(args: Vec<Value>) -> EvalResult {
    expect_args("find-composition-internal", &args, 4)?;
    expect_integer_or_marker_p(&args[0])?;
    if !args[1].is_nil() {
        expect_integer_or_marker_p(&args[1])?;
    }
    if !args[2].is_nil() && !args[2].is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[2].clone()],
        ));
    }
    Ok(Value::Nil)
}

/// `(composition-get-gstring FROM TO FONT-OBJECT STRING)`
///
/// Return a gstring (grapheme cluster string) for composing characters
/// between FROM and TO with FONT-OBJECT in STRING.
///
/// Stub: return nil (let the display engine handle shaping).
pub(crate) fn builtin_composition_get_gstring(args: Vec<Value>) -> EvalResult {
    expect_args("composition-get-gstring", &args, 4)?;
    expect_integerp(&args[0])?;
    expect_integerp(&args[1])?;
    if !args[3].is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[3].clone()],
        ));
    }
    let from = match &args[0] {
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        _ => unreachable!("validated by expect_integerp"),
    };
    let to = match &args[1] {
        Value::Int(n) => *n,
        Value::Char(c) => *c as i64,
        _ => unreachable!("validated by expect_integerp"),
    };
    let text = args[3].as_str().expect("validated string");
    let chars: Vec<char> = text.chars().collect();
    let len = chars.len() as i64;

    if from > to || from > len || to > len {
        return Err(signal(
            "args-out-of-range",
            vec![Value::string(text), Value::Int(from), Value::Int(to)],
        ));
    }
    if from < 0 || from == to {
        return Err(signal(
            "error",
            vec![Value::string("Attempt to shape zero-length text")],
        ));
    }

    let from_usize = from as usize;
    let to_usize = to as usize;
    if from_usize >= chars.len() || to_usize > chars.len() || from_usize >= to_usize {
        return Err(signal(
            "args-out-of-range",
            vec![Value::string(text), Value::Int(from), Value::Int(to)],
        ));
    }

    let segment = &chars[from_usize..to_usize];
    let mut encoded = vec![Value::symbol("utf-8-unix")];
    encoded.extend(segment.iter().map(|c| Value::Int(*c as i64)));

    let mut gstring = vec![Value::vector(encoded), Value::Nil];
    for ch in segment {
        let code = *ch as i64;
        gstring.push(Value::vector(vec![
            Value::Int(0),
            Value::Int(0),
            Value::Int(code),
            Value::Int(code),
            Value::Int(1),
            Value::Int(0),
            Value::Int(1),
            Value::Int(1),
            Value::Int(0),
            Value::Nil,
        ]));
    }
    while gstring.len() < 10 {
        gstring.push(Value::Nil);
    }

    Ok(Value::vector(gstring))
}

/// `(clear-composition-cache)`
///
/// Clear the internal composition cache.
///
/// Stub: no cache to clear, return nil.
pub(crate) fn builtin_clear_composition_cache(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-composition-cache", &args, 0)?;
    Ok(Value::Nil)
}

/// `(composition-sort-rules RULES)`
///
/// Sort composition rules by priority.
///
/// Batch-compatible subset:
/// - nil RULES => nil
/// - non-list RULES => `(wrong-type-argument listp RULES)`
/// - list entries that are not composition rules => generic invalid-rule error
/// - otherwise return RULES unchanged
pub(crate) fn builtin_composition_sort_rules(args: Vec<Value>) -> EvalResult {
    expect_args("composition-sort-rules", &args, 1)?;
    if args[0].is_nil() {
        return Ok(Value::Nil);
    }

    let items = list_to_vec(&args[0]).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[0].clone()],
        )
    })?;

    for item in items {
        if !matches!(item, Value::Cons(_)) {
            return Err(signal(
                "error",
                vec![Value::string("Invalid composition rule in RULES argument")],
            ));
        }
    }

    Ok(args[0].clone())
}

/// `(auto-composition-mode &optional ARG)`
///
/// Toggle auto-composition mode.  In real Emacs this is a minor mode that
/// controls whether automatic character composition is performed.
///
/// Batch-compatible behavior: return `t`.
pub(crate) fn builtin_auto_composition_mode(args: Vec<Value>) -> EvalResult {
    expect_max_args("auto-composition-mode", &args, 1)?;
    Ok(Value::True)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compose_region_internal_min_args() {
        let result = builtin_compose_region_internal(vec![Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn compose_region_internal_max_args() {
        let result = builtin_compose_region_internal(vec![
            Value::Int(1),
            Value::Int(10),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn compose_region_internal_too_few_args() {
        let result = builtin_compose_region_internal(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn compose_region_internal_too_many_args() {
        let result = builtin_compose_region_internal(vec![
            Value::Int(1),
            Value::Int(10),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_err());
    }

    #[test]
    fn compose_region_internal_rejects_non_integer_positions() {
        let result = builtin_compose_region_internal(vec![Value::symbol("x"), Value::Int(10)]);
        assert!(result.is_err());
        let result = builtin_compose_region_internal(vec![Value::Int(1), Value::symbol("y")]);
        assert!(result.is_err());
    }

    #[test]
    fn compose_string_internal_returns_string() {
        let s = Value::string("hello");
        let result = builtin_compose_string_internal(vec![s.clone(), Value::Int(0), Value::Int(5)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello"));
    }

    #[test]
    fn compose_string_internal_with_optional_args() {
        let s = Value::string("hello");
        let result = builtin_compose_string_internal(vec![
            s.clone(),
            Value::Int(0),
            Value::Int(5),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_str(), Some("hello"));
    }

    #[test]
    fn compose_string_internal_too_few_args() {
        let result = builtin_compose_string_internal(vec![Value::string("hi"), Value::Int(0)]);
        assert!(result.is_err());
    }

    #[test]
    fn compose_string_internal_type_checks() {
        let non_string = builtin_compose_string_internal(vec![Value::Int(1), Value::Int(0), Value::Int(1)]);
        assert!(non_string.is_err());
        let bad_start =
            builtin_compose_string_internal(vec![Value::string("abc"), Value::symbol("x"), Value::Int(1)]);
        assert!(bad_start.is_err());
        let bad_end =
            builtin_compose_string_internal(vec![Value::string("abc"), Value::Int(0), Value::symbol("y")]);
        assert!(bad_end.is_err());
    }

    #[test]
    fn find_composition_internal_returns_nil() {
        let result = builtin_find_composition_internal(vec![
            Value::Int(1),
            Value::Int(100),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn find_composition_internal_wrong_arity() {
        let result = builtin_find_composition_internal(vec![Value::Int(1)]);
        assert!(result.is_err());
    }

    #[test]
    fn find_composition_internal_type_checks() {
        let bad_pos = builtin_find_composition_internal(vec![
            Value::symbol("x"),
            Value::Int(10),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(bad_pos.is_err());

        let bad_limit = builtin_find_composition_internal(vec![
            Value::Int(1),
            Value::symbol("y"),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(bad_limit.is_err());

        let bad_string = builtin_find_composition_internal(vec![
            Value::Int(1),
            Value::Nil,
            Value::Int(1),
            Value::Nil,
        ]);
        assert!(bad_string.is_err());
    }

    #[test]
    fn composition_get_gstring_returns_vector_shape() {
        let result = builtin_composition_get_gstring(vec![
            Value::Int(0),
            Value::Int(1),
            Value::Nil,
            Value::string("ab"),
        ]);
        assert!(result.is_ok());
        let Value::Vector(gs) = result.unwrap() else {
            panic!("expected vector gstring");
        };
        let gs = gs.lock().expect("poisoned");
        assert!(!gs.is_empty());
        assert!(matches!(gs[0], Value::Vector(_)));
    }

    #[test]
    fn composition_get_gstring_wrong_arity() {
        let result = builtin_composition_get_gstring(vec![Value::Int(0)]);
        assert!(result.is_err());
    }

    #[test]
    fn composition_get_gstring_type_checks() {
        let bad_from = builtin_composition_get_gstring(vec![
            Value::symbol("x"),
            Value::Int(5),
            Value::Nil,
            Value::string("hello"),
        ]);
        assert!(bad_from.is_err());

        let bad_to = builtin_composition_get_gstring(vec![
            Value::Int(0),
            Value::symbol("y"),
            Value::Nil,
            Value::string("hello"),
        ]);
        assert!(bad_to.is_err());

        let bad_string =
            builtin_composition_get_gstring(vec![Value::Int(0), Value::Int(5), Value::Nil, Value::Int(1)]);
        assert!(bad_string.is_err());
    }

    #[test]
    fn composition_get_gstring_range_errors() {
        let from_gt_to =
            builtin_composition_get_gstring(vec![Value::Int(2), Value::Int(1), Value::Nil, Value::string("ab")]);
        assert!(from_gt_to.is_err());

        let zero_length =
            builtin_composition_get_gstring(vec![Value::Int(0), Value::Int(0), Value::Nil, Value::string("ab")]);
        assert!(zero_length.is_err());
    }

    #[test]
    fn clear_composition_cache_no_args() {
        let result = builtin_clear_composition_cache(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_nil());
    }

    #[test]
    fn clear_composition_cache_too_many_args() {
        let result = builtin_clear_composition_cache(vec![Value::Nil]);
        assert!(result.is_err());
    }

    #[test]
    fn composition_sort_rules_nil_returns_nil() {
        let result = builtin_composition_sort_rules(vec![Value::Nil]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn composition_sort_rules_rejects_non_lists() {
        let result = builtin_composition_sort_rules(vec![Value::vector(vec![Value::Int(1)])]);
        assert!(result.is_err());
    }

    #[test]
    fn composition_sort_rules_rejects_invalid_rules() {
        let rules = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_composition_sort_rules(vec![rules]);
        assert!(result.is_err());
    }

    #[test]
    fn composition_sort_rules_accepts_cons_rules() {
        let rules = Value::list(vec![Value::cons(Value::Int(1), Value::Int(2))]);
        let result = builtin_composition_sort_rules(vec![rules.clone()]).unwrap();
        assert_eq!(result, rules);
    }

    #[test]
    fn composition_sort_rules_wrong_arity() {
        let result = builtin_composition_sort_rules(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn auto_composition_mode_no_args() {
        let result = builtin_auto_composition_mode(vec![]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn auto_composition_mode_with_arg() {
        let result = builtin_auto_composition_mode(vec![Value::Int(1)]);
        assert!(result.is_ok());
        assert!(result.unwrap().is_truthy());
    }

    #[test]
    fn auto_composition_mode_too_many_args() {
        let result = builtin_auto_composition_mode(vec![Value::Int(1), Value::Int(2)]);
        assert!(result.is_err());
    }
}
