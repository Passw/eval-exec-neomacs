use super::*;

// -----------------------------------------------------------------------
// Pure builtins
// -----------------------------------------------------------------------

// ----- recent-keys -----

#[test]
fn test_recent_keys_no_args() {
    let result = builtin_recent_keys(vec![]);
    assert!(result.is_ok());
    let val = result.unwrap();
    // Should be an empty vector
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector, got {:?}", val);
    }
}

#[test]
fn test_recent_keys_with_include_cmds() {
    let result = builtin_recent_keys(vec![Value::True]);
    assert!(result.is_ok());
    let val = result.unwrap();
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector");
    }
}

#[test]
fn test_recent_keys_too_many_args() {
    let result = builtin_recent_keys(vec![Value::True, Value::Nil]);
    assert!(result.is_err());
}

// ----- input-pending-p -----

#[test]
fn test_input_pending_p_no_args() {
    let result = builtin_input_pending_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_input_pending_p_with_check_timers() {
    let result = builtin_input_pending_p(vec![Value::True]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_input_pending_p_too_many_args() {
    let result = builtin_input_pending_p(vec![Value::True, Value::Nil]);
    assert!(result.is_err());
}

// ----- discard-input -----

#[test]
fn test_discard_input_no_args() {
    let result = builtin_discard_input(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_discard_input_wrong_args() {
    let result = builtin_discard_input(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ----- recursion-depth -----

#[test]
fn test_recursion_depth_returns_zero() {
    let result = builtin_recursion_depth(vec![]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_int(), Some(0));
}

#[test]
fn test_recursion_depth_wrong_args() {
    let result = builtin_recursion_depth(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ----- current-idle-time -----

#[test]
fn test_current_idle_time_returns_nil() {
    let result = builtin_current_idle_time(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_current_idle_time_wrong_args() {
    let result = builtin_current_idle_time(vec![Value::Int(0)]);
    assert!(result.is_err());
}

// ----- event-convert-list -----

#[test]
fn test_event_convert_list_stub() {
    let list = Value::list(vec![Value::symbol("control"), Value::Char('x')]);
    let result = builtin_event_convert_list(vec![list]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_event_convert_list_wrong_args() {
    let result = builtin_event_convert_list(vec![]);
    assert!(result.is_err());
}

// ----- internal-event-symbol-parse-modifiers -----

#[test]
fn test_internal_event_symbol_parse_modifiers_stub() {
    let result = builtin_internal_event_symbol_parse_modifiers(vec![Value::symbol("C-x")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_internal_event_symbol_parse_modifiers_wrong_args() {
    let result = builtin_internal_event_symbol_parse_modifiers(vec![]);
    assert!(result.is_err());
}

// ----- posn-at-x-y -----

#[test]
fn test_posn_at_x_y_min_args() {
    let result = builtin_posn_at_x_y(vec![Value::Int(10), Value::Int(20)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_posn_at_x_y_all_args() {
    let result = builtin_posn_at_x_y(vec![
        Value::Int(10),
        Value::Int(20),
        Value::Nil,
        Value::True,
    ]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_posn_at_x_y_too_few_args() {
    let result = builtin_posn_at_x_y(vec![Value::Int(10)]);
    assert!(result.is_err());
}

#[test]
fn test_posn_at_x_y_too_many_args() {
    let result = builtin_posn_at_x_y(vec![
        Value::Int(10),
        Value::Int(20),
        Value::Nil,
        Value::True,
        Value::Nil,
    ]);
    assert!(result.is_err());
}

// ----- posn-at-point -----

#[test]
fn test_posn_at_point_no_args() {
    let result = builtin_posn_at_point(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_posn_at_point_with_pos() {
    let result = builtin_posn_at_point(vec![Value::Int(1)]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_posn_at_point_with_pos_and_window() {
    let result = builtin_posn_at_point(vec![Value::Int(1), Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_posn_at_point_too_many_args() {
    let result = builtin_posn_at_point(vec![Value::Int(1), Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// ----- reset-this-command-lengths -----

#[test]
fn test_reset_this_command_lengths() {
    let result = builtin_reset_this_command_lengths(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_reset_this_command_lengths_wrong_args() {
    let result = builtin_reset_this_command_lengths(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ----- clear-this-command-keys -----

#[test]
fn test_clear_this_command_keys() {
    let result = builtin_clear_this_command_keys(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_clear_this_command_keys_wrong_args() {
    let result = builtin_clear_this_command_keys(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ----- top-level -----

#[test]
fn test_top_level() {
    let result = builtin_top_level(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_top_level_wrong_args() {
    let result = builtin_top_level(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ----- exit-recursive-edit -----

#[test]
fn test_exit_recursive_edit() {
    let result = builtin_exit_recursive_edit(vec![]);
    assert!(matches!(
        result,
        Err(Flow::Signal(sig)) if sig.symbol == "user-error"
    ));
}

#[test]
fn test_exit_recursive_edit_wrong_args() {
    let result = builtin_exit_recursive_edit(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// ----- abort-recursive-edit -----

#[test]
fn test_abort_recursive_edit() {
    let result = builtin_abort_recursive_edit(vec![]);
    assert!(matches!(
        result,
        Err(Flow::Signal(sig)) if sig.symbol == "user-error"
    ));
}

#[test]
fn test_abort_recursive_edit_wrong_args() {
    let result = builtin_abort_recursive_edit(vec![Value::Int(1)]);
    assert!(result.is_err());
}

// -----------------------------------------------------------------------
// Eval-dependent builtins
// -----------------------------------------------------------------------

#[test]
fn test_read_key_sequence_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_read_key_sequence(&mut eval, vec![Value::string("Press key: ")]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some(""));
}

#[test]
fn test_read_key_sequence_with_all_optional_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_read_key_sequence(
        &mut eval,
        vec![
            Value::string("Press key: "),
            Value::Nil,  // continue-echo
            Value::Nil,  // dont-downcase-last
            Value::True, // can-return-switch-frame
            Value::Nil,  // command-loop
        ],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some(""));
}

#[test]
fn test_read_key_sequence_too_few_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_read_key_sequence(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_read_key_sequence_too_many_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_read_key_sequence(
        &mut eval,
        vec![
            Value::string("a"),
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil,
            Value::Nil, // 6 args, max is 5
        ],
    );
    assert!(result.is_err());
}

#[test]
fn test_read_key_sequence_vector_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_read_key_sequence_vector(&mut eval, vec![Value::string("Press key: ")]);
    assert!(result.is_ok());
    let val = result.unwrap();
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector, got {:?}", val);
    }
}

#[test]
fn test_read_key_sequence_vector_too_few_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_read_key_sequence_vector(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_this_command_keys_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_command_keys(&mut eval, vec![]);
    assert!(result.is_ok());
    let val = result.unwrap();
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector");
    }
}

#[test]
fn test_this_command_keys_wrong_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_command_keys(&mut eval, vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn test_this_command_keys_vector_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_command_keys_vector(&mut eval, vec![]);
    assert!(result.is_ok());
    let val = result.unwrap();
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector");
    }
}

#[test]
fn test_this_command_keys_vector_wrong_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_command_keys_vector(&mut eval, vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn test_this_single_command_keys_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_single_command_keys(&mut eval, vec![]);
    assert!(result.is_ok());
    let val = result.unwrap();
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector");
    }
}

#[test]
fn test_this_single_command_keys_wrong_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_single_command_keys(&mut eval, vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn test_this_single_command_raw_keys_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_single_command_raw_keys(&mut eval, vec![]);
    assert!(result.is_ok());
    let val = result.unwrap();
    if let Value::Vector(v) = &val {
        let inner = v.lock().expect("poisoned");
        assert!(inner.is_empty());
    } else {
        panic!("expected vector");
    }
}

#[test]
fn test_this_single_command_raw_keys_wrong_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_this_single_command_raw_keys(&mut eval, vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn test_execute_extended_command_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_execute_extended_command(&mut eval, vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_execute_extended_command_with_all_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_execute_extended_command(
        &mut eval,
        vec![
            Value::Nil,
            Value::string("find-file"),
            Value::string("find"),
        ],
    );
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_execute_extended_command_too_few_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_execute_extended_command(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_execute_extended_command_too_many_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_execute_extended_command(
        &mut eval,
        vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil],
    );
    assert!(result.is_err());
}

#[test]
fn test_command_execute_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_command_execute(&mut eval, vec![Value::symbol("find-file")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_command_execute_with_all_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_command_execute(
        &mut eval,
        vec![
            Value::symbol("find-file"),
            Value::True,           // record-flag
            Value::vector(vec![]), // keys
            Value::Nil,            // special
        ],
    );
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_command_execute_too_few_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_command_execute(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_command_execute_too_many_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_command_execute(
        &mut eval,
        vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil, Value::Nil],
    );
    assert!(result.is_err());
}

#[test]
fn test_set_input_mode_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_input_mode(&mut eval, vec![Value::Nil, Value::Nil, Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_set_input_mode_with_quit() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_input_mode(
        &mut eval,
        vec![Value::Nil, Value::Nil, Value::Nil, Value::Int(7)],
    );
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_set_input_mode_too_few_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_input_mode(&mut eval, vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_set_input_mode_too_many_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_input_mode(
        &mut eval,
        vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil, Value::Nil],
    );
    assert!(result.is_err());
}

#[test]
fn test_current_input_mode_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_current_input_mode(&mut eval, vec![]);
    assert!(result.is_ok());
    let val = result.unwrap();
    let items = list_to_vec(&val).unwrap();
    assert_eq!(items.len(), 4);
    // First three are nil
    assert!(items[0].is_nil());
    assert!(items[1].is_nil());
    assert!(items[2].is_nil());
    // Fourth is 7 (C-g)
    assert_eq!(items[3].as_int(), Some(7));
}

#[test]
fn test_current_input_mode_wrong_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_current_input_mode(&mut eval, vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn test_open_dribble_file_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_open_dribble_file(&mut eval, vec![Value::string("/tmp/dribble")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_open_dribble_file_wrong_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_open_dribble_file(&mut eval, vec![]);
    assert!(result.is_err());
}

#[test]
fn test_open_dribble_file_too_many_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_open_dribble_file(&mut eval, vec![Value::string("a"), Value::string("b")]);
    assert!(result.is_err());
}

#[test]
fn test_set_input_interrupt_mode_stub() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_input_interrupt_mode(&mut eval, vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_set_input_interrupt_mode_with_args() {
    let mut eval = super::super::eval::Evaluator::new();
    let result = builtin_set_input_interrupt_mode(&mut eval, vec![Value::True]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}
