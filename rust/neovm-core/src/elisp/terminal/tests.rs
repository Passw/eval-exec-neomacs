use super::*;

// -----------------------------------------------------------------------
// Terminal builtins
// -----------------------------------------------------------------------

#[test]
fn test_terminal_list_no_args() {
    let result = builtin_terminal_list(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_terminal_list_wrong_args() {
    let result = builtin_terminal_list(vec![Value::Int(1)]);
    assert!(result.is_err());
}

#[test]
fn test_terminal_name_no_args() {
    let result = builtin_terminal_name(vec![]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some("neomacs"));
}

#[test]
fn test_terminal_name_with_arg() {
    let result = builtin_terminal_name(vec![Value::Nil]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().as_str(), Some("neomacs"));
}

#[test]
fn test_terminal_name_too_many_args() {
    let result = builtin_terminal_name(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_terminal_live_p() {
    let result = builtin_terminal_live_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_terminal_live_p_wrong_args() {
    let result = builtin_terminal_live_p(vec![]);
    assert!(result.is_err());

    let result = builtin_terminal_live_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_terminal_parameter() {
    let result = builtin_terminal_parameter(vec![Value::Nil, Value::symbol("background-mode")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_terminal_parameter_wrong_args() {
    let result = builtin_terminal_parameter(vec![Value::Nil]);
    assert!(result.is_err());

    let result = builtin_terminal_parameter(vec![]);
    assert!(result.is_err());
}

#[test]
fn test_set_terminal_parameter_returns_value() {
    let value = Value::symbol("dark");
    let result = builtin_set_terminal_parameter(vec![
        Value::Nil,
        Value::symbol("background-mode"),
        value.clone(),
    ]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &value));
}

#[test]
fn test_set_terminal_parameter_wrong_args() {
    let result = builtin_set_terminal_parameter(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());

    let result =
        builtin_set_terminal_parameter(vec![Value::Nil, Value::Nil, Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_terminal_parameters_no_args() {
    let result = builtin_terminal_parameters(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_terminal_parameters_with_arg() {
    let result = builtin_terminal_parameters(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_terminal_parameters_too_many_args() {
    let result = builtin_terminal_parameters(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_frame_terminal_no_args() {
    let result = builtin_frame_terminal(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_frame_terminal_with_arg() {
    let result = builtin_frame_terminal(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_frame_terminal_too_many_args() {
    let result = builtin_frame_terminal(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_delete_terminal_no_args() {
    let result = builtin_delete_terminal(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_delete_terminal_with_terminal() {
    let result = builtin_delete_terminal(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_delete_terminal_with_force() {
    let result = builtin_delete_terminal(vec![Value::Nil, Value::True]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_delete_terminal_too_many_args() {
    let result = builtin_delete_terminal(vec![Value::Nil, Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// -----------------------------------------------------------------------
// TTY builtins
// -----------------------------------------------------------------------

#[test]
fn test_tty_display_color_p_no_args() {
    let result = builtin_tty_display_color_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_tty_display_color_p_with_arg() {
    let result = builtin_tty_display_color_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_tty_display_color_p_too_many_args() {
    let result = builtin_tty_display_color_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_tty_display_color_cells_no_args() {
    let result = builtin_tty_display_color_cells(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(16777216)));
}

#[test]
fn test_tty_display_color_cells_with_arg() {
    let result = builtin_tty_display_color_cells(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(16777216)));
}

#[test]
fn test_tty_display_color_cells_too_many_args() {
    let result = builtin_tty_display_color_cells(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_tty_type_no_args() {
    let result = builtin_tty_type(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_tty_type_with_arg() {
    let result = builtin_tty_type(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_tty_type_too_many_args() {
    let result = builtin_tty_type(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_controlling_tty_p_no_args() {
    let result = builtin_controlling_tty_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_controlling_tty_p_with_arg() {
    let result = builtin_controlling_tty_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_controlling_tty_p_too_many_args() {
    let result = builtin_controlling_tty_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_tty_no_underline_no_args() {
    let result = builtin_tty_no_underline(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_tty_no_underline_with_arg() {
    let result = builtin_tty_no_underline(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_tty_no_underline_too_many_args() {
    let result = builtin_tty_no_underline(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_suspend_tty_no_args() {
    let result = builtin_suspend_tty(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_suspend_tty_with_arg() {
    let result = builtin_suspend_tty(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_suspend_tty_too_many_args() {
    let result = builtin_suspend_tty(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_resume_tty_no_args() {
    let result = builtin_resume_tty(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_resume_tty_with_arg() {
    let result = builtin_resume_tty(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_resume_tty_too_many_args() {
    let result = builtin_resume_tty(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_serial_terminal_p_no_args() {
    let result = builtin_serial_terminal_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_serial_terminal_p_with_arg() {
    let result = builtin_serial_terminal_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_nil());
}

#[test]
fn test_serial_terminal_p_too_many_args() {
    let result = builtin_serial_terminal_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// -----------------------------------------------------------------------
// Display builtins
// -----------------------------------------------------------------------

#[test]
fn test_display_mouse_p_no_args() {
    let result = builtin_display_mouse_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_mouse_p_with_arg() {
    let result = builtin_display_mouse_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_mouse_p_too_many_args() {
    let result = builtin_display_mouse_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_graphic_p_no_args() {
    let result = builtin_display_graphic_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_graphic_p_with_arg() {
    let result = builtin_display_graphic_p(vec![Value::string(":0")]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_graphic_p_too_many_args() {
    let result = builtin_display_graphic_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_popup_menus_p_no_args() {
    let result = builtin_display_popup_menus_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_popup_menus_p_with_arg() {
    let result = builtin_display_popup_menus_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_popup_menus_p_too_many_args() {
    let result = builtin_display_popup_menus_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_selections_p_no_args() {
    let result = builtin_display_selections_p(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_selections_p_with_arg() {
    let result = builtin_display_selections_p(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_selections_p_too_many_args() {
    let result = builtin_display_selections_p(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_screens_no_args() {
    let result = builtin_display_screens(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(1)));
}

#[test]
fn test_display_screens_with_arg() {
    let result = builtin_display_screens(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(1)));
}

#[test]
fn test_display_screens_too_many_args() {
    let result = builtin_display_screens(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_pixel_height_no_args() {
    let result = builtin_display_pixel_height(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(1080)));
}

#[test]
fn test_display_pixel_height_with_arg() {
    let result = builtin_display_pixel_height(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(1080)));
}

#[test]
fn test_display_pixel_height_too_many_args() {
    let result = builtin_display_pixel_height(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_pixel_width_no_args() {
    let result = builtin_display_pixel_width(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(1920)));
}

#[test]
fn test_display_pixel_width_with_arg() {
    let result = builtin_display_pixel_width(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(1920)));
}

#[test]
fn test_display_pixel_width_too_many_args() {
    let result = builtin_display_pixel_width(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_mm_height_no_args() {
    let result = builtin_display_mm_height(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(286)));
}

#[test]
fn test_display_mm_height_with_arg() {
    let result = builtin_display_mm_height(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(286)));
}

#[test]
fn test_display_mm_height_too_many_args() {
    let result = builtin_display_mm_height(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_mm_width_no_args() {
    let result = builtin_display_mm_width(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(508)));
}

#[test]
fn test_display_mm_width_with_arg() {
    let result = builtin_display_mm_width(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(508)));
}

#[test]
fn test_display_mm_width_too_many_args() {
    let result = builtin_display_mm_width(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_backing_store_no_args() {
    let result = builtin_display_backing_store(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::symbol("always")));
}

#[test]
fn test_display_backing_store_with_arg() {
    let result = builtin_display_backing_store(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::symbol("always")));
}

#[test]
fn test_display_backing_store_too_many_args() {
    let result = builtin_display_backing_store(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_save_under_no_args() {
    let result = builtin_display_save_under(vec![]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_save_under_with_arg() {
    let result = builtin_display_save_under(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(result.unwrap().is_truthy());
}

#[test]
fn test_display_save_under_too_many_args() {
    let result = builtin_display_save_under(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_planes_no_args() {
    let result = builtin_display_planes(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(24)));
}

#[test]
fn test_display_planes_with_arg() {
    let result = builtin_display_planes(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(24)));
}

#[test]
fn test_display_planes_too_many_args() {
    let result = builtin_display_planes(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_color_cells_no_args() {
    let result = builtin_display_color_cells(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(16777216)));
}

#[test]
fn test_display_color_cells_with_arg() {
    let result = builtin_display_color_cells(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::Int(16777216)));
}

#[test]
fn test_display_color_cells_too_many_args() {
    let result = builtin_display_color_cells(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

#[test]
fn test_display_visual_class_no_args() {
    let result = builtin_display_visual_class(vec![]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::symbol("true-color")));
}

#[test]
fn test_display_visual_class_with_arg() {
    let result = builtin_display_visual_class(vec![Value::Nil]);
    assert!(result.is_ok());
    assert!(eq_value(&result.unwrap(), &Value::symbol("true-color")));
}

#[test]
fn test_display_visual_class_too_many_args() {
    let result = builtin_display_visual_class(vec![Value::Nil, Value::Nil]);
    assert!(result.is_err());
}

// -----------------------------------------------------------------------
// Return value type checks
// -----------------------------------------------------------------------

#[test]
fn test_terminal_name_returns_string() {
    let result = builtin_terminal_name(vec![]).unwrap();
    assert!(result.is_string());
}

#[test]
fn test_display_backing_store_returns_symbol() {
    let result = builtin_display_backing_store(vec![]).unwrap();
    assert!(result.is_symbol());
}

#[test]
fn test_display_visual_class_returns_symbol() {
    let result = builtin_display_visual_class(vec![]).unwrap();
    assert!(result.is_symbol());
}

#[test]
fn test_display_pixel_dimensions_are_positive() {
    let w = builtin_display_pixel_width(vec![]).unwrap();
    let h = builtin_display_pixel_height(vec![]).unwrap();
    assert!(w.as_int().unwrap() > 0);
    assert!(h.as_int().unwrap() > 0);
}

#[test]
fn test_display_mm_dimensions_are_positive() {
    let w = builtin_display_mm_width(vec![]).unwrap();
    let h = builtin_display_mm_height(vec![]).unwrap();
    assert!(w.as_int().unwrap() > 0);
    assert!(h.as_int().unwrap() > 0);
}

#[test]
fn test_display_planes_is_positive() {
    let p = builtin_display_planes(vec![]).unwrap();
    assert!(p.as_int().unwrap() > 0);
}

#[test]
fn test_display_color_cells_is_power_of_two() {
    let c = builtin_display_color_cells(vec![])
        .unwrap()
        .as_int()
        .unwrap();
    assert_eq!(c, 1 << 24);
}

#[test]
fn test_tty_display_color_cells_matches_display_color_cells() {
    let tty = builtin_tty_display_color_cells(vec![])
        .unwrap()
        .as_int()
        .unwrap();
    let disp = builtin_display_color_cells(vec![])
        .unwrap()
        .as_int()
        .unwrap();
    assert_eq!(tty, disp);
}

// -----------------------------------------------------------------------
// Passing various Value types as optional args (should not error)
// -----------------------------------------------------------------------

#[test]
fn test_optional_args_accept_various_types() {
    // All &optional TERMINAL/DISPLAY builtins should accept any value
    // as the optional argument without signaling wrong-type-argument.
    let values = vec![
        Value::Nil,
        Value::True,
        Value::Int(0),
        Value::string("x11"),
        Value::symbol("some-terminal"),
    ];

    for val in &values {
        assert!(builtin_terminal_name(vec![val.clone()]).is_ok());
        assert!(builtin_terminal_parameters(vec![val.clone()]).is_ok());
        assert!(builtin_frame_terminal(vec![val.clone()]).is_ok());
        assert!(builtin_tty_display_color_p(vec![val.clone()]).is_ok());
        assert!(builtin_tty_display_color_cells(vec![val.clone()]).is_ok());
        assert!(builtin_tty_type(vec![val.clone()]).is_ok());
        assert!(builtin_controlling_tty_p(vec![val.clone()]).is_ok());
        assert!(builtin_tty_no_underline(vec![val.clone()]).is_ok());
        assert!(builtin_suspend_tty(vec![val.clone()]).is_ok());
        assert!(builtin_resume_tty(vec![val.clone()]).is_ok());
        assert!(builtin_serial_terminal_p(vec![val.clone()]).is_ok());
        assert!(builtin_display_mouse_p(vec![val.clone()]).is_ok());
        assert!(builtin_display_graphic_p(vec![val.clone()]).is_ok());
        assert!(builtin_display_popup_menus_p(vec![val.clone()]).is_ok());
        assert!(builtin_display_selections_p(vec![val.clone()]).is_ok());
        assert!(builtin_display_screens(vec![val.clone()]).is_ok());
        assert!(builtin_display_pixel_height(vec![val.clone()]).is_ok());
        assert!(builtin_display_pixel_width(vec![val.clone()]).is_ok());
        assert!(builtin_display_mm_height(vec![val.clone()]).is_ok());
        assert!(builtin_display_mm_width(vec![val.clone()]).is_ok());
        assert!(builtin_display_backing_store(vec![val.clone()]).is_ok());
        assert!(builtin_display_save_under(vec![val.clone()]).is_ok());
        assert!(builtin_display_planes(vec![val.clone()]).is_ok());
        assert!(builtin_display_color_cells(vec![val.clone()]).is_ok());
        assert!(builtin_display_visual_class(vec![val.clone()]).is_ok());
    }
}

// -----------------------------------------------------------------------
// set-terminal-parameter returns exactly the third argument
// -----------------------------------------------------------------------

#[test]
fn test_set_terminal_parameter_returns_int() {
    let result =
        builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("param"), Value::Int(42)])
            .unwrap();
    assert!(eq_value(&result, &Value::Int(42)));
}

#[test]
fn test_set_terminal_parameter_returns_string() {
    let val = Value::string("hello");
    let result =
        builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("param"), val.clone()])
            .unwrap();
    assert!(equal_value(&result, &val, 0));
}

#[test]
fn test_set_terminal_parameter_returns_nil() {
    let result =
        builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("param"), Value::Nil])
            .unwrap();
    assert!(result.is_nil());
}

#[test]
fn test_set_terminal_parameter_returns_list() {
    let val = Value::list(vec![Value::Int(1), Value::Int(2)]);
    let result =
        builtin_set_terminal_parameter(vec![Value::Nil, Value::symbol("param"), val.clone()])
            .unwrap();
    assert!(equal_value(&result, &val, 0));
}
