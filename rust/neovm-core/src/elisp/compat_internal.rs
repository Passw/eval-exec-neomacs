//! Internal compatibility builtins.
//!
//! These primitives are exposed by GNU Emacs and touched by startup/runtime
//! code paths. NeoVM provides compatibility implementations that preserve
//! observable arity and error contracts for compatibility tests.

#[cfg(test)]
mod tests {
    use crate::elisp::error::Flow;
    use crate::elisp::value::{HashTableTest, Value};

    #[test]
    fn fillarray_vector_is_in_place() {
        let vec = Value::vector(vec![Value::Int(1), Value::Int(2)]);
        let out =
            crate::elisp::builtins::builtin_fillarray(vec![vec.clone(), Value::Int(9)]).unwrap();
        assert_eq!(out, vec);
        let Value::Vector(values) = out else {
            panic!("expected vector");
        };
        let values = values.lock().expect("poisoned");
        assert_eq!(&*values, &[Value::Int(9), Value::Int(9)]);
    }

    #[test]
    fn fillarray_bool_vector_preserves_layout_and_sets_bits() {
        let bv = crate::elisp::chartable::builtin_make_bool_vector(vec![Value::Int(4), Value::Nil])
            .unwrap();
        let out =
            crate::elisp::builtins::builtin_fillarray(vec![bv.clone(), Value::symbol("non-nil")])
                .unwrap();
        assert_eq!(out, bv);
        assert_eq!(
            crate::elisp::chartable::builtin_bool_vector_p(vec![bv.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            crate::elisp::chartable::builtin_bool_vector_count_population(vec![bv.clone()])
                .unwrap(),
            Value::Int(4)
        );

        crate::elisp::builtins::builtin_fillarray(vec![bv.clone(), Value::Nil]).unwrap();
        assert_eq!(
            crate::elisp::chartable::builtin_bool_vector_count_population(vec![bv]).unwrap(),
            Value::Int(0)
        );
    }

    #[test]
    fn fillarray_char_table_preserves_shape_and_updates_default_slot() {
        let table = crate::elisp::chartable::builtin_make_char_table(vec![
            Value::symbol("syntax-table"),
            Value::Int(0),
        ])
        .unwrap();
        crate::elisp::chartable::builtin_set_char_table_range(vec![
            table.clone(),
            Value::Int('a' as i64),
            Value::Int(9),
        ])
        .unwrap();

        let out =
            crate::elisp::builtins::builtin_fillarray(vec![table.clone(), Value::Int(7)]).unwrap();
        assert_eq!(out, table);
        assert_eq!(
            crate::elisp::chartable::builtin_char_table_p(vec![table.clone()]).unwrap(),
            Value::True
        );
        assert_eq!(
            crate::elisp::chartable::builtin_char_table_subtype(vec![table.clone()]).unwrap(),
            Value::symbol("syntax-table")
        );
        assert_eq!(
            crate::elisp::chartable::builtin_char_table_range(vec![
                table.clone(),
                Value::Int('a' as i64)
            ])
            .unwrap(),
            Value::Int(9)
        );
        assert_eq!(
            crate::elisp::chartable::builtin_char_table_range(vec![table, Value::Nil]).unwrap(),
            Value::Int(7)
        );
    }

    #[test]
    fn external_debugging_rejects_negative_fixnum() {
        let err = crate::elisp::builtins::builtin_external_debugging_output(vec![Value::Int(-1)])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn define_hash_table_test_requires_symbol_name() {
        let err = crate::elisp::builtins::builtin_define_hash_table_test(vec![
            Value::Int(1),
            Value::symbol("eq"),
            Value::symbol("sxhash-eq"),
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn face_attributes_as_vector_shape() {
        let out =
            crate::elisp::builtins::builtin_face_attributes_as_vector(vec![Value::Nil]).unwrap();
        let Value::Vector(values) = out else {
            panic!("expected vector");
        };
        let values = values.lock().expect("poisoned");
        assert_eq!(values.len(), 20);
    }

    #[test]
    fn frame_face_hash_table_uses_eq_test() {
        let out = crate::elisp::builtins::builtin_frame_face_hash_table(vec![]).unwrap();
        let Value::HashTable(table) = out else {
            panic!("expected hash table");
        };
        assert!(matches!(
            table.lock().expect("poisoned").test,
            HashTableTest::Eq
        ));
    }

    #[test]
    fn font_match_p_requires_font_spec_values() {
        let err =
            crate::elisp::builtins::builtin_font_match_p(vec![Value::Nil, Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn frame_set_was_invisible_returns_new_state() {
        let out =
            crate::elisp::builtins::builtin_frame_set_was_invisible(vec![Value::Nil, Value::True])
                .unwrap();
        assert_eq!(out, Value::True);
    }

    #[test]
    fn frame_bottom_divider_width_rejects_non_frame_designator() {
        let err = crate::elisp::builtins::builtin_frame_bottom_divider_width(vec![Value::Int(0)])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn frame_scale_factor_defaults_to_one_float() {
        let out = crate::elisp::builtins::builtin_frame_scale_factor(vec![]).unwrap();
        assert_eq!(out, Value::Float(1.0));
    }

    #[test]
    fn garbage_collect_maybe_requires_whole_number() {
        let err =
            crate::elisp::builtins::builtin_garbage_collect_maybe(vec![Value::True]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gnutls_error_string_zero_is_success() {
        let out = crate::elisp::builtins::builtin_gnutls_error_string(vec![Value::Int(0)]).unwrap();
        assert_eq!(out, Value::string("Success."));
    }

    #[test]
    fn gnutls_peer_status_warning_describe_rejects_non_symbol() {
        let err =
            crate::elisp::builtins::builtin_gnutls_peer_status_warning_describe(vec![Value::Int(
                0,
            )])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gpm_mouse_start_signals_console_only_error() {
        let err = crate::elisp::builtins::builtin_gpm_mouse_start(vec![]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn sqlite_version_returns_string() {
        let out = crate::elisp::builtins::builtin_sqlite_version(vec![]).unwrap();
        assert!(matches!(out, Value::Str(_)));
    }

    #[test]
    fn inotify_valid_p_returns_nil() {
        let out = crate::elisp::builtins::builtin_inotify_valid_p(vec![Value::Int(0)]).unwrap();
        assert_eq!(out, Value::Nil);
    }

    #[test]
    fn sqlite_open_and_close_round_trip() {
        let db = crate::elisp::builtins::builtin_sqlite_open(vec![]).unwrap();
        let sqlitep = crate::elisp::builtins::builtin_sqlitep(vec![db.clone()]).unwrap();
        assert_eq!(sqlitep, Value::True);
        let closed = crate::elisp::builtins::builtin_sqlite_close(vec![db]).unwrap();
        assert_eq!(closed, Value::True);
    }

    #[test]
    fn sqlite_execute_rejects_non_handle() {
        let err = crate::elisp::builtins::builtin_sqlite_execute(vec![
            Value::Nil,
            Value::string("select 1"),
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn inotify_watch_lifecycle() {
        let watch = crate::elisp::builtins::builtin_inotify_add_watch(vec![
            Value::string("/tmp"),
            Value::Nil,
            Value::symbol("ignore"),
        ])
        .unwrap();
        let active = crate::elisp::builtins::builtin_inotify_valid_p(vec![watch.clone()]).unwrap();
        assert_eq!(active, Value::True);
        let removed =
            crate::elisp::builtins::builtin_inotify_rm_watch(vec![watch.clone()]).unwrap();
        assert_eq!(removed, Value::True);
        let inactive = crate::elisp::builtins::builtin_inotify_valid_p(vec![watch]).unwrap();
        assert_eq!(inactive, Value::Nil);
    }

    #[test]
    fn inotify_rm_watch_invalid_descriptor_signals() {
        let err =
            crate::elisp::builtins::builtin_inotify_rm_watch(vec![Value::Int(1)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "file-notify-error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gnutls_bye_requires_process() {
        let err =
            crate::elisp::builtins::builtin_gnutls_bye(vec![Value::Nil, Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gnutls_format_certificate_requires_string() {
        let err = crate::elisp::builtins::builtin_gnutls_format_certificate(vec![Value::Nil])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gnutls_hash_digest_nil_method_signals_error() {
        let err = crate::elisp::builtins::builtin_gnutls_hash_digest(vec![
            Value::Nil,
            Value::string("a"),
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn gnutls_hash_mac_symbol_method_returns_string() {
        let out = crate::elisp::builtins::builtin_gnutls_hash_mac(vec![
            Value::symbol("SHA256"),
            Value::string("k"),
            Value::string("a"),
        ])
        .unwrap();
        assert!(matches!(out, Value::Str(_)));
    }

    #[test]
    fn gnutls_symmetric_encrypt_accepts_optional_aad_slot() {
        let out = crate::elisp::builtins::builtin_gnutls_symmetric_encrypt(vec![
            Value::symbol("AES-128-GCM"),
            Value::string("k"),
            Value::string("iv"),
            Value::string("data"),
            Value::string("aad"),
        ])
        .unwrap();
        assert_eq!(out, Value::Nil);
    }

    #[test]
    fn handle_switch_frame_requires_frame_object() {
        let err =
            crate::elisp::builtins::builtin_handle_switch_frame(vec![Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn interactive_form_for_ignore_returns_interactive_list() {
        let out = crate::elisp::builtins::builtin_interactive_form(vec![Value::symbol("ignore")])
            .unwrap();
        assert_eq!(
            out,
            Value::list(vec![Value::symbol("interactive"), Value::Nil])
        );
    }

    #[test]
    fn lock_file_requires_string_argument() {
        let err = crate::elisp::builtins::builtin_lock_file(vec![Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn unlock_file_requires_string_argument() {
        let err = crate::elisp::builtins::builtin_unlock_file(vec![Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn window_combination_limit_requires_window_designator() {
        let err =
            crate::elisp::builtins::builtin_window_combination_limit(vec![Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data.first(), Some(&Value::symbol("window-valid-p")));
            }
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn window_combination_limit_signals_internal_only_for_window_object() {
        let err = crate::elisp::builtins::builtin_window_combination_limit(vec![Value::Window(1)])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn window_resize_apply_rejects_non_frame_designator() {
        let err = crate::elisp::builtins::builtin_window_resize_apply(vec![Value::Window(1)])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data.first(), Some(&Value::symbol("frame-live-p")));
            }
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn window_resize_apply_total_returns_true() {
        let out = crate::elisp::builtins::builtin_window_resize_apply_total(vec![]).unwrap();
        assert_eq!(out, Value::True);
    }

    #[test]
    fn window_bottom_divider_width_rejects_non_window_designator() {
        let err = crate::elisp::builtins::builtin_window_bottom_divider_width(vec![Value::Int(1)])
            .unwrap_err();
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data.first(), Some(&Value::symbol("window-live-p")));
            }
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn treesit_available_p_defaults_to_nil() {
        let out = crate::elisp::builtins::builtin_treesit_available_p(vec![]).unwrap();
        assert_eq!(out, Value::Nil);
    }

    #[test]
    fn treesit_query_compile_validates_arity() {
        let err =
            crate::elisp::builtins::builtin_treesit_query_compile(vec![Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn internal_stack_stats_returns_nil() {
        let out = crate::elisp::builtins::builtin_internal_stack_stats(vec![]).unwrap();
        assert_eq!(out, Value::Nil);
    }

    #[test]
    fn internal_labeled_narrow_to_region_validates_arity() {
        let err = crate::elisp::builtins::builtin_internal_labeled_narrow_to_region(vec![
            Value::Nil,
            Value::Nil,
        ])
        .unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn lossage_size_defaults_to_three_hundred() {
        let out = crate::elisp::builtins::builtin_lossage_size(vec![]).unwrap();
        assert_eq!(out, Value::Int(300));
    }
}
