use super::*;

#[test]
fn zlib_decompress_region_arity_and_type_validation() {
    let arity = builtin_zlib_decompress_region(vec![]);
    assert!(arity.is_err());

    let too_many =
        builtin_zlib_decompress_region(vec![Value::Int(1), Value::Int(1), Value::Nil, Value::Nil]);
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
            assert_eq!(sig.symbol_name(), "error");
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
        builtin_libxml_parse_xml_region(vec![Value::Nil]).unwrap(),
        Value::Nil
    );
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
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
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
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
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
            assert_eq!(sig.symbol_name(), "wrong-number-of-arguments");
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
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
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
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
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
            assert_eq!(sig.symbol_name(), "wrong-number-of-arguments");
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
            assert_eq!(sig.symbol_name(), "wrong-number-of-arguments");
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
            assert_eq!(sig.symbol_name(), "wrong-number-of-arguments");
            assert_eq!(
                sig.data,
                vec![Value::symbol("zlib-available-p"), Value::Int(1)]
            );
        }
        other => panic!("unexpected flow: {other:?}"),
    }
}
