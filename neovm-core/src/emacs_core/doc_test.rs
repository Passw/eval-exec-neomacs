use super::*;

// =======================================================================
// substitute-command-keys
// =======================================================================

#[test]
fn substitute_plain_string() {
    let result = builtin_substitute_command_keys(vec![Value::string("hello world")]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some("hello world"));
}

#[test]
fn substitute_command_key_binding() {
    let result =
        builtin_substitute_command_keys(vec![Value::string("Press \\[save-buffer] to save.")]);
    assert!(result.is_ok());
    let s = result.unwrap();
    let text = s.as_str().unwrap();
    assert!(text.contains("save-buffer"));
    assert!(!text.contains("\\["));
    assert!(!text.contains(']'));
}

#[test]
fn substitute_keymap_description() {
    let result =
        builtin_substitute_command_keys(vec![Value::string("Bindings:\\{foo-mode-map}done")]);
    assert!(result.is_ok());
    let s = result.unwrap();
    let text = s.as_str().unwrap();
    // The keymap description is stripped entirely.
    assert_eq!(text, "Bindings:done");
}

#[test]
fn substitute_keymap_context() {
    let result =
        builtin_substitute_command_keys(vec![Value::string("\\<foo-map>Press \\[bar] now")]);
    assert!(result.is_ok());
    let s = result.unwrap();
    let text = s.as_str().unwrap();
    assert!(!text.contains("\\<"));
    assert!(!text.contains('>'));
    assert!(text.contains("bar"));
}

#[test]
fn substitute_quote_escape() {
    let result = builtin_substitute_command_keys(vec![Value::string(
        "Use \\=\\[not-a-command] literally",
    )]);
    assert!(result.is_ok());
    let s = result.unwrap();
    let text = s.as_str().unwrap();
    // \\= quotes the next char, so \\[ is literal.
    assert!(text.contains("\\[not-a-command]"));
}

#[test]
fn substitute_literal_backslash() {
    let result = builtin_substitute_command_keys(vec![Value::string("path\\\\name")]);
    assert!(result.is_ok());
    let s = result.unwrap();
    assert_eq!(s.as_str(), Some("path\\name"));
}

#[test]
fn substitute_wrong_type() {
    let result = builtin_substitute_command_keys(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn substitute_wrong_arity() {
    let result = builtin_substitute_command_keys(vec![]);
    assert!(result.is_err());
}

// =======================================================================
// documentation-property (stub)
// =======================================================================

#[test]
fn documentation_property_returns_nil() {
    let result = builtin_documentation_property(vec![
        Value::symbol("foo"),
        Value::symbol("variable-documentation"),
    ]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn documentation_property_with_raw() {
    let result = builtin_documentation_property(vec![
        Value::symbol("foo"),
        Value::symbol("variable-documentation"),
        Value::True,
    ]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn documentation_property_wrong_type() {
    let result = builtin_documentation_property(vec![
        Value::Int(42),
        Value::symbol("variable-documentation"),
    ]);
    assert!(result.is_err());
}

#[test]
fn documentation_property_wrong_arity() {
    let result = builtin_documentation_property(vec![Value::symbol("foo")]);
    assert!(result.is_err());
}

// =======================================================================
// Snarf-documentation runtime/error semantics
// =======================================================================

#[test]
fn snarf_documentation_returns_nil() {
    let result = builtin_snarf_documentation(vec![Value::string("DOC")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn snarf_documentation_wrong_type() {
    let result = builtin_snarf_documentation(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn snarf_documentation_empty_path_errors() {
    let result = builtin_snarf_documentation(vec![Value::string("")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "error"),
        other => panic!("expected error signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_parent_dir_path_errors() {
    let result = builtin_snarf_documentation(vec![Value::string("../")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "error"),
        other => panic!("expected error signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_single_dot_path_errors() {
    let result = builtin_snarf_documentation(vec![Value::string(".")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "error"),
        other => panic!("expected error signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_root_path_errors() {
    let result = builtin_snarf_documentation(vec![Value::string("/")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "error"),
        other => panic!("expected error signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_doc_dir_path_file_error() {
    let result = builtin_snarf_documentation(vec![Value::string("DOC/")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "file-error"),
        other => panic!("expected file-error signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_doc_subpath_file_error() {
    let result = builtin_snarf_documentation(vec![Value::string("DOC/a")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "file-error"),
        other => panic!("expected file-error signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_missing_path_errors() {
    let result = builtin_snarf_documentation(vec![Value::string("NO_SUCH_DOC_FILE")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "file-missing"),
        other => panic!("expected file-missing signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_missing_dir_path_errors() {
    let result = builtin_snarf_documentation(vec![Value::string("NO_SUCH_DOC_DIR/")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "file-missing"),
        other => panic!("expected file-missing signal, got {other:?}"),
    }
}

#[test]
fn snarf_documentation_wrong_arity() {
    let result = builtin_snarf_documentation(vec![]);
    assert!(result.is_err());
}

// =======================================================================
// help-function-arglist
// =======================================================================

fn arglist_names(v: &Value) -> Vec<String> {
    list_to_vec(v)
        .expect("arglist must be list")
        .into_iter()
        .map(|item| {
            item.as_symbol_name()
                .expect("arglist item must be symbol")
                .to_string()
        })
        .collect()
}

#[test]
fn help_function_arglist_returns_t_for_unknown() {
    let result = builtin_help_function_arglist(vec![Value::symbol("foo")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn help_function_arglist_thread_join_matches_oracle_shape() {
    let result = builtin_help_function_arglist(vec![Value::symbol("thread-join")]).unwrap();
    assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
}

#[test]
fn help_function_arglist_thread_join_preserve_names() {
    let result =
        builtin_help_function_arglist(vec![Value::symbol("thread-join"), Value::True]).unwrap();
    assert_eq!(arglist_names(&result), vec!["thread".to_string()]);
}

#[test]
fn help_function_arglist_sub_non_preserve_names_matches_oracle_shape() {
    let result = builtin_help_function_arglist(vec![Value::symbol("-")]).unwrap();
    assert_eq!(
        arglist_names(&result),
        vec!["&rest".to_string(), "rest".to_string()]
    );
}

#[test]
fn help_function_arglist_sub_preserve_names_matches_oracle_shape() {
    let result = builtin_help_function_arglist(vec![Value::symbol("-"), Value::True]).unwrap();
    assert_eq!(
        arglist_names(&result),
        vec![
            "&optional".to_string(),
            "number-or-marker".to_string(),
            "&rest".to_string(),
            "more-numbers-or-markers".to_string()
        ]
    );
}

#[test]
fn help_function_arglist_event_apply_modifier_matches_oracle_shape() {
    let result =
        builtin_help_function_arglist(vec![Value::symbol("event-apply-modifier")]).unwrap();
    assert_eq!(
        arglist_names(&result),
        vec![
            "arg1".to_string(),
            "arg2".to_string(),
            "arg3".to_string(),
            "arg4".to_string()
        ]
    );
}

#[test]
fn help_function_arglist_event_apply_modifier_preserve_names() {
    let result =
        builtin_help_function_arglist(vec![Value::symbol("event-apply-modifier"), Value::True])
            .unwrap();
    assert_eq!(
        arglist_names(&result),
        vec![
            "event".to_string(),
            "symbol".to_string(),
            "lshiftby".to_string(),
            "prefix".to_string()
        ]
    );
}

#[test]
fn help_function_arglist_condition_notify_optional_arg() {
    let result =
        builtin_help_function_arglist(vec![Value::symbol("condition-notify")]).unwrap();
    assert_eq!(
        arglist_names(&result),
        vec![
            "arg1".to_string(),
            "&optional".to_string(),
            "arg2".to_string()
        ]
    );
}

#[test]
fn help_function_arglist_lambda_params() {
    let result =
        builtin_help_function_arglist(vec![Value::make_lambda(LambdaData {
            params: LambdaParams {
                required: vec![intern("x")],
                optional: vec![intern("y")],
                rest: Some(intern("rest")),
            },
            body: vec![].into(),
            env: None,
            docstring: None,
            doc_form: None,
        })])
        .unwrap();
    assert_eq!(
        arglist_names(&result),
        vec![
            "x".to_string(),
            "&optional".to_string(),
            "y".to_string(),
            "&rest".to_string(),
            "rest".to_string()
        ]
    );
}

#[test]
fn help_function_arglist_quoted_lambda_params() {
    let quoted = Value::list(vec![
        Value::symbol("lambda"),
        Value::list(vec![Value::symbol("x"), Value::symbol("y")]),
        Value::symbol("x"),
    ]);
    let result = builtin_help_function_arglist(vec![quoted]).unwrap();
    assert_eq!(
        arglist_names(&result),
        vec!["x".to_string(), "y".to_string()]
    );
}

#[test]
fn help_function_arglist_quoted_lambda_symbol_params() {
    let quoted = Value::list(vec![
        Value::symbol("lambda"),
        Value::symbol("x"),
        Value::symbol("x"),
    ]);
    let result = builtin_help_function_arglist(vec![quoted]).unwrap();
    assert_eq!(result.as_symbol_name(), Some("x"));
}

#[test]
fn help_function_arglist_quoted_macro_returns_t() {
    let quoted = Value::list(vec![
        Value::symbol("macro"),
        Value::list(vec![Value::symbol("x")]),
        Value::symbol("x"),
    ]);
    let result = builtin_help_function_arglist(vec![quoted]).unwrap();
    assert!(result.is_truthy());
}

#[test]
fn help_function_arglist_macro_lambda_symbol_payload_returns_nil() {
    let quoted = Value::list(vec![Value::symbol("macro"), Value::symbol("lambda")]);
    let result = builtin_help_function_arglist(vec![quoted]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn help_function_arglist_quoted_lambda_improper_tail_errors() {
    let quoted = Value::cons(Value::symbol("lambda"), Value::Int(1));
    let result = builtin_help_function_arglist(vec![quoted]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "wrong-type-argument"),
        other => panic!("expected wrong-type-argument signal, got {other:?}"),
    }
}

#[test]
fn help_function_arglist_wrong_arity() {
    let result = builtin_help_function_arglist(vec![]);
    assert!(result.is_err());
}

#[test]
fn help_function_arglist_eval_builtin_car() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result =
        builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("car")]).unwrap();
    assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
}

#[test]
fn help_function_arglist_eval_builtin_car_preserve_names() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("car"), Value::True],
    )
    .unwrap();
    assert_eq!(arglist_names(&result), vec!["list".to_string()]);
}

#[test]
fn help_function_arglist_eval_special_form_if() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result =
        builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("if")]).unwrap();
    assert_eq!(
        arglist_names(&result),
        vec![
            "arg1".to_string(),
            "arg2".to_string(),
            "&rest".to_string(),
            "rest".to_string()
        ]
    );
}

#[test]
fn help_function_arglist_eval_runtime_alias_builtin() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("vm-hfa-alias-builtin", Value::symbol("car"));

    let result = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("vm-hfa-alias-builtin")],
    )
    .unwrap();
    assert_eq!(arglist_names(&result), vec!["arg1".to_string()]);
}

#[test]
fn help_function_arglist_eval_autoload_placeholder() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("describe-function")],
    )
    .unwrap();
    assert_eq!(
        result.as_str(),
        Some("[Arg list not available until function definition is loaded.]")
    );
}

#[test]
fn help_function_arglist_eval_subr_arity_fallback_shapes() {
    let mut evaluator = super::super::eval::Evaluator::new();

    let cdr =
        builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("cdr")]).unwrap();
    assert_eq!(arglist_names(&cdr), vec!["arg1".to_string()]);

    let list = builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("list")])
        .unwrap();
    assert_eq!(
        arglist_names(&list),
        vec!["&rest".to_string(), "rest".to_string()]
    );

    let read = builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("read")])
        .unwrap();
    assert_eq!(
        arglist_names(&read),
        vec!["&optional".to_string(), "arg1".to_string()]
    );

    let equal =
        builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("equal")])
            .unwrap();
    assert_eq!(
        arglist_names(&equal),
        vec!["arg1".to_string(), "arg2".to_string()]
    );

    let substring =
        builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("substring")])
            .unwrap();
    assert_eq!(
        arglist_names(&substring),
        vec![
            "arg1".to_string(),
            "&optional".to_string(),
            "arg2".to_string(),
            "arg3".to_string()
        ]
    );

    let funcall =
        builtin_help_function_arglist_eval(&mut evaluator, vec![Value::symbol("funcall")])
            .unwrap();
    assert_eq!(
        arglist_names(&funcall),
        vec!["arg1".to_string(), "&rest".to_string(), "rest".to_string()]
    );
}

#[test]
fn help_function_arglist_eval_preserve_names_core_subrs() {
    let mut evaluator = super::super::eval::Evaluator::new();

    let cdr = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("cdr"), Value::True],
    )
    .unwrap();
    assert_eq!(arglist_names(&cdr), vec!["list".to_string()]);

    let cons = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("cons"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&cons),
        vec!["car".to_string(), "cdr".to_string()]
    );

    let list = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("list"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&list),
        vec!["&rest".to_string(), "objects".to_string()]
    );

    let min = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("min"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&min),
        vec![
            "number-or-marker".to_string(),
            "&rest".to_string(),
            "numbers-or-markers".to_string()
        ]
    );

    let max = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("max"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&max),
        vec![
            "number-or-marker".to_string(),
            "&rest".to_string(),
            "numbers-or-markers".to_string()
        ]
    );

    let message = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("message"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&message),
        vec![
            "format-string".to_string(),
            "&rest".to_string(),
            "args".to_string()
        ]
    );

    let concat = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("concat"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&concat),
        vec!["&rest".to_string(), "sequences".to_string()]
    );

    let nconc = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("nconc"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&nconc),
        vec!["&rest".to_string(), "lists".to_string()]
    );

    let format = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("format"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&format),
        vec![
            "string".to_string(),
            "&rest".to_string(),
            "objects".to_string()
        ]
    );

    let apply = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("apply"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&apply),
        vec![
            "function".to_string(),
            "&rest".to_string(),
            "arguments".to_string()
        ]
    );

    let assq = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("assq"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&assq),
        vec!["key".to_string(), "alist".to_string()]
    );

    let string_match = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("string-match"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&string_match),
        vec![
            "regexp".to_string(),
            "string".to_string(),
            "&optional".to_string(),
            "start".to_string(),
            "inhibit-modify".to_string()
        ]
    );

    let make_string = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("make-string"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&make_string),
        vec![
            "length".to_string(),
            "init".to_string(),
            "&optional".to_string(),
            "multibyte".to_string()
        ]
    );

    let read_from_string = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("read-from-string"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&read_from_string),
        vec![
            "string".to_string(),
            "&optional".to_string(),
            "start".to_string(),
            "end".to_string()
        ]
    );

    let funcall = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("funcall"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&funcall),
        vec![
            "function".to_string(),
            "&rest".to_string(),
            "arguments".to_string()
        ]
    );

    let mapcar = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("mapcar"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&mapcar),
        vec!["function".to_string(), "sequence".to_string()]
    );

    let read = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("read"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&read),
        vec!["&optional".to_string(), "stream".to_string()]
    );

    let documentation = builtin_help_function_arglist_eval(
        &mut evaluator,
        vec![Value::symbol("documentation"), Value::True],
    )
    .unwrap();
    assert_eq!(
        arglist_names(&documentation),
        vec![
            "function".to_string(),
            "&optional".to_string(),
            "raw".to_string()
        ]
    );
}

// =======================================================================
// documentation (eval-dependent)
// =======================================================================

#[test]
fn documentation_lambda_with_docstring() {
    let mut evaluator = super::super::eval::Evaluator::new();

    // Set up a lambda with a docstring in the function cell.
    let lambda = Value::make_lambda(LambdaData {
        params: LambdaParams::simple(vec![intern("x")]),
        body: vec![].into(),
        env: None,
        docstring: Some("Add one to X.".to_string()),
        doc_form: None,
    });
    evaluator.obarray.set_symbol_function("my-fn", lambda);

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("my-fn")]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some("Add one to X."));
}

#[test]
fn documentation_lambda_no_docstring() {
    let mut evaluator = super::super::eval::Evaluator::new();

    let lambda = Value::make_lambda(LambdaData {
        params: LambdaParams::simple(vec![]),
        body: vec![].into(),
        env: None,
        docstring: None,
        doc_form: None,
    });
    evaluator.obarray.set_symbol_function("no-doc", lambda);

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("no-doc")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn documentation_unbound_function() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("nonexistent")]);
    assert!(result.is_err());
}

#[test]
fn documentation_subr() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("plus", Value::Subr(intern("+")));

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("plus")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_string());
}

#[test]
fn documentation_car_subr_uses_oracle_text_shape() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("car", Value::Subr(intern("car")));

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("car")]).unwrap();
    let text = result
        .as_str()
        .expect("documentation for car should return a string");
    assert!(text.starts_with("Return the car of LIST.  If LIST is nil, return nil."));
    assert_ne!(text, "Built-in function.");
}

#[test]
fn documentation_if_special_form_uses_oracle_text_shape() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("if", Value::Subr(intern("if")));

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("if")]).unwrap();
    let text = result
        .as_str()
        .expect("documentation for if should return a string");
    assert!(text.starts_with("If COND yields non-nil, do THEN, else do ELSE..."));
    assert_ne!(text, "Built-in function.");
}

#[test]
fn documentation_core_subr_stubs_use_oracle_first_line_shapes() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let probes = [
        (
            "cons",
            "Create a new cons, give it CAR and CDR as components, and return it.",
        ),
        (
            "list",
            "Return a newly created list with specified arguments as elements.",
        ),
        ("eq", "Return t if the two args are the same Lisp object."),
        (
            "equal",
            "Return t if two Lisp objects have similar structure and contents.",
        ),
        (
            "length",
            "Return the length of vector, list or string SEQUENCE.",
        ),
        (
            "append",
            "Concatenate all the arguments and make the result a list.",
        ),
        (
            "mapcar",
            "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.",
        ),
        (
            "assoc",
            "Return non-nil if KEY is equal to the car of an element of ALIST.",
        ),
        (
            "member",
            "Return non-nil if ELT is an element of LIST.  Comparison done with ‘equal’.",
        ),
        ("symbol-name", "Return SYMBOL’s name, a string."),
    ];

    for (name, expected_prefix) in probes {
        evaluator
            .obarray
            .set_symbol_function(name, Value::Subr(intern(name)));
        let result = builtin_documentation(&mut evaluator, vec![Value::symbol(name)]).unwrap();
        let text = result
            .as_str()
            .expect("core subr documentation should return a string");
        assert!(
            text.starts_with(expected_prefix),
            "unexpected documentation text for {name}: {text:?}"
        );
        assert_ne!(text, "Built-in function.");
    }
}

#[test]
fn documentation_symbol_alias_to_builtin_returns_docstring() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("alias-builtin", Value::symbol("car"));

    let result =
        builtin_documentation(&mut evaluator, vec![Value::symbol("alias-builtin")]).unwrap();
    let text = result
        .as_str()
        .expect("documentation alias to car should return a string");
    assert!(text.starts_with("Return the car of LIST.  If LIST is nil, return nil."));
    assert_ne!(text, "Built-in function.");
}

#[test]
fn documentation_prefers_function_documentation_property() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator.obarray.put_property(
        "doc-prop",
        "function-documentation",
        Value::string("propdoc"),
    );

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    assert_eq!(result.unwrap().as_str(), Some("propdoc"));
}

#[test]
fn documentation_integer_function_documentation_property_returns_nil() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator
        .obarray
        .put_property("doc-prop", "function-documentation", Value::Int(9));

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    assert!(result.unwrap().is_nil());
}

#[test]
fn documentation_list_function_documentation_property_is_evaluated() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator.obarray.put_property(
        "doc-prop",
        "function-documentation",
        Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
    );

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    assert_eq!(result.unwrap().as_str(), Some("doc"));
}

#[test]
fn documentation_symbol_function_documentation_property_is_evaluated() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator
        .obarray
        .put_property("doc-prop", "function-documentation", Value::symbol("t"));

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    assert!(result.unwrap().is_truthy());
}

#[test]
fn documentation_vector_function_documentation_property_is_evaluated() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator.obarray.put_property(
        "doc-prop",
        "function-documentation",
        Value::vector(vec![Value::Int(1), Value::Int(2)]),
    );

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    assert!(result.unwrap().is_vector());
}

#[test]
fn documentation_unbound_symbol_function_documentation_property_errors() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator.obarray.put_property(
        "doc-prop",
        "function-documentation",
        Value::symbol("doc-prop-unbound"),
    );

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "void-variable"),
        other => panic!("expected void-variable signal, got {other:?}"),
    }
}

#[test]
fn documentation_invalid_form_function_documentation_property_errors() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("doc-prop", Value::Int(7));
    evaluator.obarray.put_property(
        "doc-prop",
        "function-documentation",
        Value::list(vec![Value::Int(1), Value::Int(2)]),
    );

    let result = builtin_documentation(&mut evaluator, vec![Value::symbol("doc-prop")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "invalid-function"),
        other => panic!("expected invalid-function signal, got {other:?}"),
    }
}

#[test]
fn documentation_quoted_lambda_docstring() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let quoted = Value::list(vec![
        Value::symbol("lambda"),
        Value::list(vec![Value::symbol("x")]),
        Value::string("d"),
        Value::symbol("x"),
    ]);

    let result = builtin_documentation(&mut evaluator, vec![quoted]).unwrap();
    assert_eq!(result.as_str(), Some("d"));
}

#[test]
fn documentation_quoted_lambda_without_docstring_returns_nil() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let quoted = Value::list(vec![
        Value::symbol("lambda"),
        Value::list(vec![Value::symbol("x")]),
        Value::symbol("x"),
    ]);

    let result = builtin_documentation(&mut evaluator, vec![quoted]).unwrap();
    assert!(result.is_nil());
}

#[test]
fn documentation_vector_designator_returns_keyboard_macro_doc() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result =
        builtin_documentation(&mut evaluator, vec![Value::vector(vec![Value::Int(1)])])
            .unwrap();
    assert_eq!(result.as_str(), Some("Keyboard macro."));
}

#[test]
fn documentation_string_designator_returns_keyboard_macro_doc() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation(&mut evaluator, vec![Value::string("abc")]).unwrap();
    assert_eq!(result.as_str(), Some("Keyboard macro."));
}

#[test]
fn documentation_quoted_macro_payload_matches_oracle_shape() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let quoted = Value::list(vec![
        Value::symbol("macro"),
        Value::list(vec![Value::symbol("x")]),
        Value::string("md"),
        Value::symbol("x"),
    ]);

    let result = builtin_documentation(&mut evaluator, vec![quoted]);
    match result {
        Err(Flow::Signal(sig)) => {
            assert_eq!(sig.symbol_name(), "invalid-function");
            assert_eq!(
                sig.data.first(),
                Some(&Value::list(vec![
                    Value::list(vec![Value::symbol("x")]),
                    Value::string("md"),
                    Value::symbol("x"),
                ]))
            );
        }
        other => panic!("expected invalid-function signal, got {other:?}"),
    }
}

#[test]
fn documentation_empty_quoted_macro_errors_void_function_nil() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let quoted = Value::list(vec![Value::symbol("macro")]);

    let result = builtin_documentation(&mut evaluator, vec![quoted]);
    match result {
        Err(Flow::Signal(sig)) => {
            assert_eq!(sig.symbol_name(), "void-function");
            assert!(sig.data.first().is_some_and(Value::is_nil));
        }
        other => panic!("expected void-function signal, got {other:?}"),
    }
}

#[test]
fn documentation_non_symbol_non_function_errors_invalid_function() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation(
        &mut evaluator,
        vec![Value::list(vec![Value::Int(1), Value::Int(2)])],
    );
    assert!(result.is_err());
}

#[test]
fn documentation_wrong_arity() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation(&mut evaluator, vec![]);
    assert!(result.is_err());
}

// =======================================================================
// describe-function (eval-dependent)
// =======================================================================

#[test]
fn describe_function_lambda() {
    let mut evaluator = super::super::eval::Evaluator::new();

    let lambda = Value::make_lambda(LambdaData {
        params: LambdaParams::simple(vec![]),
        body: vec![].into(),
        env: None,
        docstring: None,
        doc_form: None,
    });
    evaluator.obarray.set_symbol_function("my-fn", lambda);

    let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("my-fn")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("my-fn is a interpreted-function")));
}

#[test]
fn describe_function_subr() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("plus", Value::Subr(intern("+")));

    let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("plus")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("plus is a primitive-function in ‘C source code’")));
}

#[test]
fn describe_function_resolves_builtin_name() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("car")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("car is a primitive-function in ‘C source code’")));
}

#[test]
fn describe_function_special_form_if() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("if")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("if is a special-form in ‘C source code’")));
}

#[test]
fn describe_function_keyboard_macro_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("vm-kmacro-string", Value::string("abc"));

    let result =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-kmacro-string")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("vm-kmacro-string is a keyboard macro")));
}

#[test]
fn describe_function_keyboard_macro_vector() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_function(
        "vm-kmacro-vector",
        Value::vector(vec![Value::Int(97), Value::Int(98)]),
    );

    let result =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-kmacro-vector")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("vm-kmacro-vector is a keyboard macro")));
}

#[test]
fn describe_function_macro_marker_list() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let macro_marker = Value::cons(
        Value::symbol("macro"),
        Value::list(vec![Value::symbol("lambda"), Value::Nil, Value::Int(1)]),
    );
    evaluator
        .obarray
        .set_symbol_function("vm-macro-marker", macro_marker);

    let result =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-macro-marker")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("vm-macro-marker is a Lisp macro")));
}

#[test]
fn describe_function_alias_reports_alias_text() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("vm-alias-car", Value::symbol("car"));

    let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-alias-car")]);
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap().as_str(),
        Some("vm-alias-car is an alias for ‘car’.")
    );
}

#[test]
fn describe_function_alias_to_missing_reports_alias_text() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .set_symbol_function("vm-alias-missing", Value::symbol("vm-no-such-fn"));

    let result =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-alias-missing")]);
    assert!(result.is_ok());
    assert_eq!(
        result.unwrap().as_str(),
        Some("vm-alias-missing is an alias for ‘vm-no-such-fn’, which is not known to be defined.")
    );
}

#[test]
fn describe_function_autoload_macro_form() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let autoload_macro = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files"),
        Value::string("doc"),
        Value::Nil,
        Value::symbol("macro"),
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-macro", autoload_macro);

    let result =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-macro")]);
    assert!(result.is_ok());
    assert!(result.unwrap().as_str().is_some_and(
        |s| s.contains("vm-autoload-macro is an autoloaded Lisp macro in ‘files.el’")
    ));
}

#[test]
fn describe_function_autoload_file_clause_is_only_used_for_bare_file_names() {
    let mut evaluator = super::super::eval::Evaluator::new();

    let autoload_empty = Value::list(vec![
        Value::symbol("autoload"),
        Value::string(""),
        Value::string("doc"),
        Value::Nil,
        Value::Nil,
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-empty", autoload_empty);

    let autoload_bare = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files"),
        Value::string("doc"),
        Value::Nil,
        Value::Nil,
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-bare", autoload_bare);

    let autoload_el = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files.el"),
        Value::string("doc"),
        Value::Nil,
        Value::Nil,
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-el", autoload_el);

    let autoload_elc = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files.elc"),
        Value::string("doc"),
        Value::Nil,
        Value::Nil,
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-elc", autoload_elc);

    let autoload_path = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("/tmp/files"),
        Value::string("doc"),
        Value::Nil,
        Value::Nil,
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-path", autoload_path);

    let empty =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-empty")]);
    assert!(empty.is_ok());
    assert!(empty.unwrap().as_str().is_some_and(|s| {
        s.contains("autoloaded Lisp function.")
            && !s.contains(" in ‘")
            && !s.contains("image.el")
    }));

    let bare =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-bare")]);
    assert!(bare.is_ok());
    assert!(bare
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("autoloaded Lisp function in ‘files.el’")));

    let el = builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-el")]);
    assert!(el.is_ok());
    assert!(el.unwrap().as_str().is_some_and(
        |s| s.contains("autoloaded Lisp function.") && !s.contains(" in ‘files.el’")
    ));

    let elc = builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-elc")]);
    assert!(elc.is_ok());
    assert!(elc
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("autoloaded Lisp function.")
            && !s.contains("files.elc")
            && !s.contains("files.elc.el")));

    let path =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-path")]);
    assert!(path.is_ok());
    assert!(path
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("autoloaded Lisp function.")
            && !s.contains("/tmp/files")
            && !s.contains("/tmp/files.el")));
}

#[test]
fn describe_function_autoload_kind_shapes_match_oracle() {
    let mut evaluator = super::super::eval::Evaluator::new();

    let autoload_macro_t = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files.elc"),
        Value::string("doc"),
        Value::Nil,
        Value::True,
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-macro-t", autoload_macro_t);

    let autoload_keymap = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files"),
        Value::string("doc"),
        Value::Nil,
        Value::symbol("keymap"),
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-keymap", autoload_keymap);

    let autoload_macro_other = Value::list(vec![
        Value::symbol("autoload"),
        Value::string("files"),
        Value::string("doc"),
        Value::Nil,
        Value::symbol("foo"),
    ]);
    evaluator
        .obarray
        .set_symbol_function("vm-autoload-macro-other", autoload_macro_other);

    let macro_t =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-macro-t")]);
    assert!(macro_t.is_ok());
    assert!(macro_t
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("autoloaded Lisp macro")
            && !s.contains("autoloaded Lisp function")
            && !s.contains("files.elc")
            && !s.contains("files.elc.el")));

    let keymap =
        builtin_describe_function(&mut evaluator, vec![Value::symbol("vm-autoload-keymap")]);
    assert!(keymap.is_ok());
    assert!(keymap
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("autoloaded keymap")
            && !s.contains("autoloaded Lisp function")
            && s.contains(" in ‘files.el’")));

    let macro_other = builtin_describe_function(
        &mut evaluator,
        vec![Value::symbol("vm-autoload-macro-other")],
    );
    assert!(macro_other.is_ok());
    assert!(macro_other
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("autoloaded Lisp macro")
            && !s.contains("autoloaded Lisp function")
            && !s.contains("autoloaded keymap")));
}

#[test]
fn describe_function_void() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_function(&mut evaluator, vec![Value::symbol("nonexistent")]);
    assert!(result.is_err());
}

#[test]
fn describe_function_non_symbol() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_function(&mut evaluator, vec![Value::Int(42)]);
    assert!(result.is_err());
}

// =======================================================================
// describe-variable (eval-dependent)
// =======================================================================

#[test]
fn describe_variable_with_doc() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator.obarray.put_property(
        "my-var",
        "variable-documentation",
        Value::string("The answer."),
    );

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    assert!(result.is_ok());
    let text = result.unwrap();
    let text = text
        .as_str()
        .expect("describe-variable with string doc should return a string");
    assert!(text.contains("my-var\u{2019}s value is 42"));
    assert!(text.contains("The answer."));
}

#[test]
fn describe_variable_load_path_uses_c_source_text() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("load-path")]);
    assert!(result.is_ok());
    let text = result.unwrap();
    let text = text
        .as_str()
        .expect("describe-variable load-path should return a string");
    assert!(text.contains("defined in"));
    assert!(text.contains("‘C source code’."));
    assert!(!text.contains("‘C source code‘."));
    assert!(text.contains("List of directories to search for files to load"));
    assert!(text.contains("‘default-directory’"));
    assert!(text.contains("value is"));
}

#[test]
fn startup_doc_quote_style_display_handles_backtick_pairs() {
    assert_eq!(
        startup_doc_quote_style_display("`C source code`."),
        "‘C source code’."
    );
    assert_eq!(
        startup_doc_quote_style_display("`default-directory'"),
        "‘default-directory’"
    );
    assert_eq!(
        startup_doc_quote_style_display("Keymap for subcommands of \\`C-x 4'."),
        "Keymap for subcommands of C-x 4."
    );
}

#[test]
fn describe_variable_list_doc_property_includes_doc_and_value_text() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator.obarray.put_property(
        "my-var",
        "variable-documentation",
        Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
    );

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("doc") && s.contains("value is")));
}

#[test]
fn describe_variable_integer_doc_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator
        .obarray
        .put_property("my-var", "variable-documentation", Value::Int(9));

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("value is")));
}

#[test]
fn describe_variable_symbol_doc_property_bound_string_includes_doc_and_value_text() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator
        .obarray
        .set_symbol_value("my-doc", Value::string("Doc from indirection."));
    evaluator
        .obarray
        .put_property("my-var", "variable-documentation", Value::symbol("my-doc"));

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("Doc from indirection.") && s.contains("value is")));
}

#[test]
fn describe_variable_symbol_doc_property_unbound_errors_void_variable() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator.obarray.put_property(
        "my-var",
        "variable-documentation",
        Value::symbol("vm-desc-unbound"),
    );

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "void-variable"),
        other => panic!("expected void-variable signal, got {other:?}"),
    }
}

#[test]
fn describe_variable_symbol_doc_property_bound_true_errors_wrong_type_argument() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator.obarray.set_symbol_value("my-doc", Value::True);
    evaluator
        .obarray
        .put_property("my-var", "variable-documentation", Value::symbol("my-doc"));

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    match result {
        Err(Flow::Signal(sig)) => {
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
            assert_eq!(
                sig.data.first().and_then(Value::as_symbol_name),
                Some("char-or-string-p")
            );
        }
        other => panic!("expected wrong-type-argument signal, got {other:?}"),
    }
}

#[test]
fn describe_variable_symbol_doc_property_bound_list_errors_wrong_type_argument() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator.obarray.set_symbol_value(
        "my-doc",
        Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
    );
    evaluator
        .obarray
        .put_property("my-var", "variable-documentation", Value::symbol("my-doc"));

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "wrong-type-argument"),
        other => panic!("expected wrong-type-argument signal, got {other:?}"),
    }
}

#[test]
fn describe_variable_true_doc_property_errors_wrong_type_argument() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("my-var", Value::Int(42));
    evaluator
        .obarray
        .put_property("my-var", "variable-documentation", Value::True);

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("my-var")]);
    match result {
        Err(Flow::Signal(sig)) => {
            assert_eq!(sig.symbol_name(), "wrong-type-argument");
            assert_eq!(
                sig.data.first().and_then(Value::as_symbol_name),
                Some("char-or-string-p")
            );
        }
        other => panic!("expected wrong-type-argument signal, got {other:?}"),
    }
}

#[test]
fn describe_variable_bound_no_doc() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("x", Value::Int(10));

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]);
    assert!(result.is_ok());
    let s = result.unwrap();
    assert!(s.as_str().unwrap().contains("x"));
}

#[test]
fn describe_variable_bound_no_doc_matches_text_shape() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("x", Value::Int(10));

    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("x")]).unwrap();
    let text = result
        .as_str()
        .expect("describe-variable must return a string");
    assert!(text.ends_with('\n'));
    assert!(text.contains("x\u{2019}s value is"));
}

#[test]
fn describe_variable_unbound() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
    assert!(result.is_ok());
    assert!(result
        .unwrap()
        .as_str()
        .is_some_and(|s| s.contains("void as a variable")));
}

#[test]
fn describe_variable_unbound_has_trailing_newline() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_variable(&mut evaluator, vec![Value::symbol("nonexistent")]);
    assert!(result.is_ok());
    assert!(result.unwrap().as_str().is_some_and(|s| s.ends_with('\n')));
}

#[test]
fn describe_variable_non_symbol() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_variable(&mut evaluator, vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn describe_variable_keyword_is_self_bound() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_variable(&mut evaluator, vec![Value::keyword(":x")]).unwrap();
    let text = result
        .as_str()
        .expect("describe-variable must return a string");
    assert!(text.contains("value is"));
    assert!(text.contains(":x"));
}

#[test]
fn describe_variable_accepts_optional_second_arg() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("x", Value::Int(10));

    let result =
        builtin_describe_variable(&mut evaluator, vec![Value::symbol("x"), Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().as_str().is_some());
}

#[test]
fn describe_variable_accepts_optional_third_arg() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.set_symbol_value("x", Value::Int(10));

    let result = builtin_describe_variable(
        &mut evaluator,
        vec![Value::symbol("x"), Value::Nil, Value::Nil],
    );
    assert!(result.is_ok());
    assert!(result.unwrap().as_str().is_some());
}

#[test]
fn describe_variable_rejects_fourth_arg() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_describe_variable(
        &mut evaluator,
        vec![Value::symbol("x"), Value::Nil, Value::Nil, Value::Nil],
    );
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "wrong-number-of-arguments"),
        other => panic!("expected wrong-number-of-arguments signal, got {other:?}"),
    }
}

#[test]
fn documentation_property_eval_returns_string_property() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .put_property("doc-sym", "variable-documentation", Value::string("doc"));

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert_eq!(result.as_str(), Some("doc"));
}

#[test]
fn documentation_property_eval_integer_property_returns_nil() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .put_property("doc-sym", "variable-documentation", Value::Int(7));

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result.is_nil());
}

#[test]
fn documentation_property_eval_load_path_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("load-path"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("List of directories to search for files to load")));
}

#[test]
fn documentation_property_eval_load_path_raw_t_preserves_ascii_quotes() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let display = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("load-path"),
            Value::symbol("variable-documentation"),
            Value::Nil,
        ],
    )
    .unwrap();
    let raw = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("load-path"),
            Value::symbol("variable-documentation"),
            Value::True,
        ],
    )
    .unwrap();
    let display = display
        .as_str()
        .expect("display documentation-property should return a string");
    let raw = raw
        .as_str()
        .expect("raw documentation-property should return a string");

    assert_ne!(display, raw);
    assert!(display.contains("‘default-directory’"));
    assert!(raw.contains("`default-directory'"));
}

#[test]
fn documentation_property_eval_ctl_x_4_map_raw_matches_display_when_no_markup() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let display = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("ctl-x-4-map"),
            Value::symbol("variable-documentation"),
            Value::Nil,
        ],
    )
    .unwrap();
    let raw = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("ctl-x-4-map"),
            Value::symbol("variable-documentation"),
            Value::True,
        ],
    )
    .unwrap();
    let display = display
        .as_str()
        .expect("display documentation-property should return a string");
    let raw = raw
        .as_str()
        .expect("raw documentation-property should return a string");

    assert!(display.contains("C-x 4"));
    assert!(!display.contains("\\`C-x 4'"));
    assert_eq!(raw, display);
}

#[test]
fn documentation_property_eval_case_fold_search_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("case-fold-search"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("searches and matches should ignore case")));
}

#[test]
fn documentation_property_eval_unread_command_events_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("unread-command-events"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("events to be read as the command input")));
}

#[test]
fn documentation_property_eval_auto_hscroll_mode_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("auto-hscroll-mode"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("automatic horizontal scrolling of windows")));
}

#[test]
fn documentation_property_eval_auto_composition_mode_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("auto-composition-mode"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("Auto-Composition mode is enabled")));
}

#[test]
fn documentation_property_eval_coding_system_alist_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("coding-system-alist"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("Alist of coding system names")));
}

#[test]
fn documentation_property_eval_debug_on_message_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("debug-on-message"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("debug if a message matching this regexp is displayed")));
}

#[test]
fn documentation_property_eval_display_hourglass_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("display-hourglass"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("show an hourglass pointer")));
}

#[test]
fn documentation_property_eval_exec_directory_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("exec-directory"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("Directory for executables for Emacs to invoke")));
}

#[test]
fn documentation_property_eval_frame_title_format_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("frame-title-format"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(
        result.as_str().is_some_and(
            |s| s.contains("Template for displaying the title bar of visible frames")
        )
    );
}

#[test]
fn documentation_property_eval_header_line_format_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("header-line-format"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("controls the header line")));
}

#[test]
fn documentation_property_eval_input_method_function_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("input-method-function"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("implements the current input method")));
}

#[test]
fn documentation_property_eval_load_suffixes_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("load-suffixes"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("suffixes for Emacs Lisp files and dynamic modules")));
}

#[test]
fn documentation_property_eval_native_comp_eln_load_path_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("native-comp-eln-load-path"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("natively-compiled *.eln files")));
}

#[test]
fn documentation_property_eval_process_environment_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("process-environment"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("environment variables for subprocesses")));
}

#[test]
fn documentation_property_eval_scroll_margin_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("scroll-margin"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("margin at the top and bottom")));
}

#[test]
fn documentation_property_eval_truncate_partial_width_windows_integer_property_returns_string()
{
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("truncate-partial-width-windows"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("windows narrower than the frame")));
}

#[test]
fn documentation_property_eval_yes_or_no_prompt_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("yes-or-no-prompt"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result.as_str().is_some_and(|s| s.contains("append when")));
}

#[test]
fn documentation_property_eval_debug_on_error_integer_property_returns_string() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("debug-on-error"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result
        .as_str()
        .is_some_and(|s| s.contains("Non-nil means enter debugger if an error is signaled")));
}

#[test]
fn documentation_property_eval_list_property_is_evaluated() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.put_property(
        "doc-sym",
        "variable-documentation",
        Value::list(vec![Value::symbol("identity"), Value::string("doc")]),
    );

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert_eq!(result.as_str(), Some("doc"));
}

#[test]
fn documentation_property_eval_symbol_property_is_evaluated() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .put_property("doc-sym", "variable-documentation", Value::symbol("t"));

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result.is_truthy());
}

#[test]
fn documentation_property_eval_vector_property_is_evaluated() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.put_property(
        "doc-sym",
        "variable-documentation",
        Value::vector(vec![Value::Int(1), Value::Int(2)]),
    );

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    )
    .unwrap();
    assert!(result.is_vector());
}

#[test]
fn documentation_property_eval_unbound_symbol_property_errors() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.put_property(
        "doc-sym",
        "variable-documentation",
        Value::symbol("doc-sym-unbound"),
    );

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    );
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "void-variable"),
        other => panic!("expected void-variable signal, got {other:?}"),
    }
}

#[test]
fn documentation_property_eval_invalid_form_property_errors() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator.obarray.put_property(
        "doc-sym",
        "variable-documentation",
        Value::list(vec![Value::Int(1), Value::Int(2)]),
    );

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![
            Value::symbol("doc-sym"),
            Value::symbol("variable-documentation"),
        ],
    );
    match result {
        Err(Flow::Signal(sig)) => assert_eq!(sig.symbol_name(), "invalid-function"),
        other => panic!("expected invalid-function signal, got {other:?}"),
    }
}

#[test]
fn documentation_property_eval_non_symbol_prop_returns_nil() {
    let mut evaluator = super::super::eval::Evaluator::new();
    evaluator
        .obarray
        .put_property("doc-sym", "x", Value::string("v"));

    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![Value::symbol("doc-sym"), Value::Int(1)],
    )
    .unwrap();
    assert!(result.is_nil());
}

#[test]
fn documentation_property_eval_non_symbol_target_errors() {
    let mut evaluator = super::super::eval::Evaluator::new();
    let result = builtin_documentation_property_eval(
        &mut evaluator,
        vec![Value::Int(1), Value::symbol("variable-documentation")],
    );
    assert!(result.is_err());
}
