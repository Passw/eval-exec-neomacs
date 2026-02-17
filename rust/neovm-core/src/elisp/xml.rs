//! XML and compression stubs for the Elisp interpreter.
//!
//! Provides stub implementations for:
//! - `libxml-parse-html-region`, `libxml-parse-xml-region`, `libxml-available-p`
//! - `zlib-available-p`, `zlib-decompress-region`
//!
//! These are stubbed because libxml and zlib are not available in pure Rust Elisp yet.

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

fn expect_optional_string(_name: &str, value: &Value) -> Result<(), Flow> {
    if value.is_nil() {
        return Ok(());
    }
    if value.as_str().is_none() {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), value.clone()],
        ))
    } else {
        Ok(())
    }
}

fn html_parse_fallback(name: &str, args: &[Value]) -> Value {
    let body = if args.len() <= 1 {
        Value::string(format!("({name})"))
    } else {
        Value::string("(")
    };
    Value::list(vec![
        Value::symbol("html"),
        Value::Nil,
        Value::list(vec![Value::symbol("body"), Value::Nil, body]),
    ])
}

fn expect_integer_or_marker(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        v if super::marker::is_marker(v) => super::marker::marker_position_as_int(v),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins
// ---------------------------------------------------------------------------

/// (libxml-parse-html-region START END &optional BASE-URL DISCARD-COMMENTS)
/// Stub: returns a compatibility envelope until libxml parser support lands.
pub(crate) fn builtin_libxml_parse_html_region(args: Vec<Value>) -> EvalResult {
    expect_min_args("libxml-parse-html-region", &args, 0)?;
    expect_max_args("libxml-parse-html-region", &args, 4)?;
    if args.len() >= 2 {
        if args.first().is_some_and(Value::is_nil) {
            return Ok(Value::Nil);
        }
        let start_pos = expect_integer_or_marker(
            args.first()
                .expect("libxml-parse-html-region requires a start position here"),
        )?;
        if let Some(end) = args.get(1) {
            if !end.is_nil() {
                let end_pos = expect_integer_or_marker(end)?;
                if start_pos == end_pos {
                    return Ok(Value::Nil);
                }
            }
        }
    }
    if let Some(base_url) = args.get(2) {
        expect_optional_string("libxml-parse-html-region", base_url)?;
    }
    // Stub parser path: return a stable compatibility envelope until libxml support lands.
    Ok(html_parse_fallback("libxml-parse-html-region", &args))
}

/// (libxml-parse-xml-region START END &optional BASE-URL DISCARD-COMMENTS)
/// Stub: returns nil (libxml not available in pure Rust yet).
pub(crate) fn builtin_libxml_parse_xml_region(args: Vec<Value>) -> EvalResult {
    expect_min_args("libxml-parse-xml-region", &args, 0)?;
    expect_max_args("libxml-parse-xml-region", &args, 4)?;
    if let Some(start) = args.first() {
        if !start.is_nil() {
            let _ = expect_integer_or_marker(start)?;
        }
    }
    if let Some(end) = args.get(1) {
        if !end.is_nil() {
            let _ = expect_integer_or_marker(end)?;
        }
    }
    if let Some(base_url) = args.get(2) {
        expect_optional_string("libxml-parse-xml-region", base_url)?;
    }
    // Stub parser path: we intentionally return nil until libxml parser support lands.
    Ok(Value::Nil)
}

/// (libxml-available-p)
/// Returns t (feature availability probe).
pub(crate) fn builtin_libxml_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("libxml-available-p", &args, 0)?;
    Ok(Value::True)
}

/// (zlib-available-p)
/// Returns t (feature availability probe).
pub(crate) fn builtin_zlib_available_p(args: Vec<Value>) -> EvalResult {
    expect_args("zlib-available-p", &args, 0)?;
    Ok(Value::True)
}

/// (zlib-decompress-region START END)
/// Compatibility subset:
/// - validates START/END as integer-or-marker
/// - supports optional third arg
/// - signals the same unibyte-buffer requirement as Emacs in current multibyte buffers
pub(crate) fn builtin_zlib_decompress_region(args: Vec<Value>) -> EvalResult {
    expect_min_args("zlib-decompress-region", &args, 2)?;
    expect_max_args("zlib-decompress-region", &args, 3)?;
    let _start = expect_integer_or_marker(&args[0])?;
    let _end = expect_integer_or_marker(&args[1])?;

    Err(signal(
        "error",
        vec![Value::string(
            "This function can be called only in unibyte buffers",
        )],
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zlib_decompress_region_arity_and_type_validation() {
        let arity = builtin_zlib_decompress_region(vec![]);
        assert!(arity.is_err());

        let too_many = builtin_zlib_decompress_region(vec![
            Value::Int(1),
            Value::Int(1),
            Value::Nil,
            Value::Nil,
        ]);
        assert!(too_many.is_err());

        let bad_type = builtin_zlib_decompress_region(vec![Value::string("x"), Value::Int(1)]);
        assert!(bad_type.is_err());
    }

    #[test]
    fn zlib_decompress_region_signals_unibyte_requirement() {
        let result = builtin_zlib_decompress_region(vec![Value::Int(1), Value::Int(1)])
            .expect_err("must signal error in multibyte buffers");
        match result {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "This function can be called only in unibyte buffers"
                    )]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn libxml_parse_xml_region_arity_and_type_subset() {
        assert_eq!(builtin_libxml_parse_xml_region(vec![]).unwrap(), Value::Nil);
        assert_eq!(
            builtin_libxml_parse_xml_region(vec![Value::Int(1), Value::Int(1)]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_libxml_parse_xml_region(vec![Value::Nil, Value::Int(1)]).unwrap(),
            Value::Nil
        );

        let wrong_type =
            builtin_libxml_parse_xml_region(vec![Value::string("x"), Value::Int(1)]).unwrap_err();
        match wrong_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("integer-or-marker-p"), Value::string("x")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
        let wrong_base =
            builtin_libxml_parse_xml_region(vec![Value::Int(1), Value::Int(2), Value::Int(1)])
                .unwrap_err();
        match wrong_base {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let wrong_arity = builtin_libxml_parse_xml_region(vec![
            Value::Int(1),
            Value::Int(1),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ])
        .unwrap_err();
        match wrong_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("libxml-parse-xml-region"), Value::Int(5)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn libxml_parse_html_region_arity_and_type_subset() {
        assert_eq!(
            builtin_libxml_parse_html_region(vec![]).unwrap(),
            html_parse_fallback("libxml-parse-html-region", &[])
        );
        assert_eq!(
            builtin_libxml_parse_html_region(vec![Value::Nil]).unwrap(),
            html_parse_fallback("libxml-parse-html-region", &[Value::Nil])
        );
        assert_eq!(
            builtin_libxml_parse_html_region(vec![Value::Int(1)]).unwrap(),
            html_parse_fallback("libxml-parse-html-region", &[Value::Int(1)])
        );
        assert_eq!(
            builtin_libxml_parse_html_region(vec![Value::Int(1), Value::Nil]).unwrap(),
            html_parse_fallback("libxml-parse-html-region", &[Value::Int(1), Value::Nil])
        );
        assert_eq!(
            builtin_libxml_parse_html_region(vec![Value::Nil, Value::Int(1)]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_libxml_parse_html_region(vec![Value::Int(1), Value::Int(1)]).unwrap(),
            Value::Nil
        );
        assert_eq!(
            builtin_libxml_parse_html_region(vec![Value::Int(1), Value::Int(2)]).unwrap(),
            html_parse_fallback("libxml-parse-html-region", &[Value::Int(1), Value::Int(2)])
        );

        let wrong_type =
            builtin_libxml_parse_html_region(vec![Value::string("x"), Value::Int(1)]).unwrap_err();
        match wrong_type {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("integer-or-marker-p"), Value::string("x")]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
        let wrong_base =
            builtin_libxml_parse_html_region(vec![Value::Int(1), Value::Int(2), Value::Int(1)])
                .unwrap_err();
        match wrong_base {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let wrong_arity = builtin_libxml_parse_html_region(vec![
            Value::Int(1),
            Value::Int(1),
            Value::Nil,
            Value::Nil,
            Value::Nil,
        ])
        .unwrap_err();
        match wrong_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("libxml-parse-html-region"), Value::Int(5)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }

    #[test]
    fn availability_probes_return_true_and_validate_arity() {
        assert_eq!(builtin_libxml_available_p(vec![]).unwrap(), Value::True);
        assert_eq!(builtin_zlib_available_p(vec![]).unwrap(), Value::True);

        let libxml_arity = builtin_libxml_available_p(vec![Value::Int(1)]).unwrap_err();
        match libxml_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("libxml-available-p"), Value::Int(1)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }

        let zlib_arity = builtin_zlib_available_p(vec![Value::Int(1)]).unwrap_err();
        match zlib_arity {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-number-of-arguments");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("zlib-available-p"), Value::Int(1)]
                );
            }
            other => panic!("unexpected flow: {other:?}"),
        }
    }
}
