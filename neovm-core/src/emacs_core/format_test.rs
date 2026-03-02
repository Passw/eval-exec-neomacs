use super::*;

// ===================================================================
// format-spec tests
// ===================================================================

#[test]
fn format_spec_basic() {
    let spec_alist = Value::list(vec![
        Value::cons(Value::Char('n'), Value::string("Bob")),
        Value::cons(Value::Char('a'), Value::string("21")),
    ]);
    let result = builtin_format_spec(vec![Value::string("%n is %a"), spec_alist]);
    assert_eq!(result.unwrap().as_str().unwrap(), "Bob is 21");
}

#[test]
fn format_spec_literal_percent() {
    let result = builtin_format_spec(vec![
        Value::string("100%% done"),
        Value::Nil, // empty alist
    ]);
    assert_eq!(result.unwrap().as_str().unwrap(), "100% done");
}

#[test]
fn format_spec_width_right_align() {
    let spec_alist = Value::list(vec![Value::cons(Value::Char('n'), Value::string("hi"))]);
    let result = builtin_format_spec(vec![Value::string("[%10n]"), spec_alist]);
    assert_eq!(result.unwrap().as_str().unwrap(), "[        hi]");
}

#[test]
fn format_spec_width_left_align() {
    let spec_alist = Value::list(vec![Value::cons(Value::Char('n'), Value::string("hi"))]);
    let result = builtin_format_spec(vec![Value::string("[%-10n]"), spec_alist]);
    assert_eq!(result.unwrap().as_str().unwrap(), "[hi        ]");
}

#[test]
fn format_spec_zero_pad() {
    let spec_alist = Value::list(vec![Value::cons(Value::Char('n'), Value::string("42"))]);
    let result = builtin_format_spec(vec![Value::string("[%05n]"), spec_alist]);
    assert_eq!(result.unwrap().as_str().unwrap(), "[00042]");
}

#[test]
fn format_spec_no_match_passthrough() {
    let result = builtin_format_spec(vec![Value::string("hello %x world"), Value::Nil]);
    assert_eq!(result.unwrap().as_str().unwrap(), "hello %x world");
}

#[test]
fn format_spec_int_keys() {
    // Use integers instead of chars for the spec keys.
    let spec_alist = Value::list(vec![Value::cons(
        Value::Int('n' as i64),
        Value::string("Alice"),
    )]);
    let result = builtin_format_spec(vec![Value::string("Name: %n"), spec_alist]);
    assert_eq!(result.unwrap().as_str().unwrap(), "Name: Alice");
}

#[test]
fn format_spec_wrong_args() {
    let result = builtin_format_spec(vec![Value::string("hi")]);
    assert!(result.is_err());
}

// ===================================================================
// format-time-string tests
// ===================================================================

#[test]
fn format_time_string_epoch() {
    // Unix epoch: 1970-01-01 00:00:00 UTC (Thursday)
    let result =
        builtin_format_time_string(vec![Value::string("%Y-%m-%d %H:%M:%S"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "1970-01-01 00:00:00");
}

#[test]
fn format_time_string_day_name() {
    // 1970-01-01 is a Thursday.
    let result = builtin_format_time_string(vec![Value::string("%A"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "Thursday");
}

#[test]
fn format_time_string_month_name() {
    let result = builtin_format_time_string(vec![Value::string("%B"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "January");
}

#[test]
fn format_time_string_known_date() {
    // 2000-01-01 00:00:00 UTC = 946684800
    let result =
        builtin_format_time_string(vec![Value::string("%Y-%m-%d %A"), Value::Int(946684800)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "2000-01-01 Saturday");
}

#[test]
fn format_time_string_literal_percent() {
    let result = builtin_format_time_string(vec![Value::string("100%%"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "100%");
}

#[test]
fn format_time_string_timezone() {
    let result = builtin_format_time_string(vec![Value::string("%Z"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "UTC");
}

#[test]
fn format_time_string_iso_format() {
    let result =
        builtin_format_time_string(vec![Value::string("%F %T"), Value::Int(946684800)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "2000-01-01 00:00:00");
}

#[test]
fn format_time_string_ampm() {
    // 2000-01-01 15:30:00 UTC = 946684800 + 15*3600 + 30*60 = 946740600
    let result =
        builtin_format_time_string(vec![Value::string("%I:%M %p"), Value::Int(946740600)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "03:30 PM");
}

#[test]
fn format_time_string_no_time_uses_current() {
    // Should not error when TIME is nil.
    let result = builtin_format_time_string(vec![Value::string("%Y"), Value::Nil]);
    assert!(result.is_ok());
    // Should return a 4-digit year.
    let year_str = result.unwrap();
    assert_eq!(year_str.as_str().unwrap().len(), 4);
}

// ===================================================================
// format-seconds tests
// ===================================================================

#[test]
fn format_seconds_basic() {
    // 3661 seconds = 1 hour, 1 minute, 1 second
    let result = builtin_format_seconds(vec![Value::string("%h:%m:%s"), Value::Int(3661)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "1:1:1");
}

#[test]
fn format_seconds_days() {
    // 90061 = 1 day + 1 hour + 1 minute + 1 second
    let result =
        builtin_format_seconds(vec![Value::string("%d days, %h:%m:%s"), Value::Int(90061)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "1 days, 1:1:1");
}

#[test]
fn format_seconds_zero() {
    let result = builtin_format_seconds(vec![Value::string("%h:%m:%s"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "0:0:0");
}

#[test]
fn format_seconds_literal_percent() {
    let result = builtin_format_seconds(vec![Value::string("100%%"), Value::Int(0)]);
    assert_eq!(result.unwrap().as_str().unwrap(), "100%");
}

// ===================================================================
// string-pad tests
// ===================================================================

#[test]
fn string_pad_right_default() {
    let result = builtin_string_pad(vec![Value::string("x"), Value::Int(2)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "x ");
}

#[test]
fn string_pad_left_custom() {
    let result = builtin_string_pad(vec![
        Value::string("x"),
        Value::Int(4),
        Value::Char('0'),
        Value::True,
    ])
    .unwrap();
    assert_eq!(result.as_str().unwrap(), "000x");
}

#[test]
fn string_pad_noop_when_long_enough() {
    let result = builtin_string_pad(vec![Value::string("xyz"), Value::Int(2)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "xyz");
}

#[test]
fn string_pad_type_errors() {
    assert!(builtin_string_pad(vec![Value::Int(1), Value::Int(2)]).is_err());
    assert!(builtin_string_pad(vec![Value::string("x"), Value::Int(-1)]).is_err());
    assert!(
        builtin_string_pad(vec![Value::string("x"), Value::Int(2), Value::string("x")])
            .is_err()
    );
}

// ===================================================================
// string-fill tests
// ===================================================================

#[test]
fn string_fill_no_wrap() {
    let result = builtin_string_fill(vec![Value::string("x"), Value::Int(2)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "x");
}

#[test]
fn string_fill_wraps_words() {
    let result =
        builtin_string_fill(vec![Value::string("aa bb ccc d"), Value::Int(5)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "aa bb\nccc d");
}

#[test]
fn string_fill_preserves_blank_lines() {
    let result =
        builtin_string_fill(vec![Value::string("a b\n\nc d"), Value::Int(10)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "a b\n\nc d");
}

#[test]
fn string_fill_type_errors() {
    assert!(builtin_string_fill(vec![Value::Int(1), Value::Int(2)]).is_err());
    assert!(builtin_string_fill(vec![Value::string("x"), Value::Int(-1)]).is_err());
}

// ===================================================================
// string-limit tests
// ===================================================================

#[test]
fn string_limit_noop() {
    let result = builtin_string_limit(vec![Value::string("x"), Value::Int(2)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "x");
}

#[test]
fn string_limit_truncates_prefix() {
    let result = builtin_string_limit(vec![Value::string("abcd"), Value::Int(2)]).unwrap();
    assert_eq!(result.as_str().unwrap(), "ab");
}

#[test]
fn string_limit_truncates_from_end_with_ellipsis() {
    let result = builtin_string_limit(vec![
        Value::string("abcd"),
        Value::Int(3),
        Value::True,
        Value::string("."),
    ])
    .unwrap();
    assert_eq!(result.as_str().unwrap(), ".cd");
}

#[test]
fn string_limit_type_errors() {
    assert!(builtin_string_limit(vec![Value::Int(1), Value::Int(2)]).is_err());
    assert!(builtin_string_limit(vec![Value::string("x"), Value::Int(-1)]).is_err());
    assert!(builtin_string_limit(vec![
        Value::string("x"),
        Value::Int(1),
        Value::Nil,
        Value::Int(1)
    ])
    .is_err());
}

// ===================================================================
// string-chop-newline tests
// ===================================================================

#[test]
fn string_chop_newline_no_newline() {
    let result = builtin_string_chop_newline(vec![Value::string("x")]).unwrap();
    assert_eq!(result.as_str().unwrap(), "x");
}

#[test]
fn string_chop_newline_lf() {
    let result = builtin_string_chop_newline(vec![Value::string("x\n")]).unwrap();
    assert_eq!(result.as_str().unwrap(), "x");
}

#[test]
fn string_chop_newline_crlf_run() {
    let result = builtin_string_chop_newline(vec![Value::string("x\r\n\n")]).unwrap();
    assert_eq!(result.as_str().unwrap(), "x");
}

#[test]
fn string_chop_newline_wrong_type() {
    assert!(builtin_string_chop_newline(vec![Value::Int(1)]).is_err());
}

// ===================================================================
// string-lines tests
// ===================================================================

#[test]
fn string_lines_basic() {
    let result = builtin_string_lines(vec![Value::string("a\nb\nc")]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(items[0].as_str().unwrap(), "a");
    assert_eq!(items[1].as_str().unwrap(), "b");
    assert_eq!(items[2].as_str().unwrap(), "c");
}

#[test]
fn string_lines_trailing_newline() {
    let result = builtin_string_lines(vec![Value::string("a\nb\n")]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 3); // "a", "b", ""
    assert_eq!(items[2].as_str().unwrap(), "");
}

#[test]
fn string_lines_omit_nulls() {
    let result = builtin_string_lines(vec![Value::string("a\n\nb\n"), Value::True]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 2);
    assert_eq!(items[0].as_str().unwrap(), "a");
    assert_eq!(items[1].as_str().unwrap(), "b");
}

#[test]
fn string_lines_empty_string() {
    let result = builtin_string_lines(vec![Value::string("")]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].as_str().unwrap(), "");
}

// ===================================================================
// string-clean-whitespace tests
// ===================================================================

#[test]
fn string_clean_whitespace_basic() {
    let result = builtin_string_clean_whitespace(vec![Value::string("  hello   world  ")]);
    assert_eq!(result.unwrap().as_str().unwrap(), "hello world");
}

#[test]
fn string_clean_whitespace_tabs_and_newlines() {
    let result = builtin_string_clean_whitespace(vec![Value::string("a\t\tb\n\nc")]);
    assert_eq!(result.unwrap().as_str().unwrap(), "a b c");
}

#[test]
fn string_clean_whitespace_no_change() {
    let result = builtin_string_clean_whitespace(vec![Value::string("hello world")]);
    assert_eq!(result.unwrap().as_str().unwrap(), "hello world");
}

#[test]
fn string_clean_whitespace_empty() {
    let result = builtin_string_clean_whitespace(vec![Value::string("")]);
    assert_eq!(result.unwrap().as_str().unwrap(), "");
}

#[test]
fn string_clean_whitespace_only_spaces() {
    let result = builtin_string_clean_whitespace(vec![Value::string("   ")]);
    assert_eq!(result.unwrap().as_str().unwrap(), "");
}

// ===================================================================
// string-pixel-width tests
// ===================================================================

#[test]
fn string_pixel_width_basic() {
    let result = builtin_string_pixel_width(vec![Value::string("hello")]);
    assert_eq!(result.unwrap().as_int().unwrap(), 5);
}

#[test]
fn string_pixel_width_empty() {
    let result = builtin_string_pixel_width(vec![Value::string("")]);
    assert_eq!(result.unwrap().as_int().unwrap(), 0);
}

#[test]
fn string_pixel_width_tabs_and_wide_chars() {
    assert_eq!(
        builtin_string_pixel_width(vec![Value::string("\t")])
            .unwrap()
            .as_int()
            .unwrap(),
        8
    );
    assert_eq!(
        builtin_string_pixel_width(vec![Value::string("a\t")])
            .unwrap()
            .as_int()
            .unwrap(),
        8
    );
    assert_eq!(
        builtin_string_pixel_width(vec![Value::string("a\tb")])
            .unwrap()
            .as_int()
            .unwrap(),
        9
    );
    assert_eq!(
        builtin_string_pixel_width(vec![Value::string("漢字")])
            .unwrap()
            .as_int()
            .unwrap(),
        4
    );
    assert_eq!(
        builtin_string_pixel_width(vec![Value::string("e\u{0301}")])
            .unwrap()
            .as_int()
            .unwrap(),
        1
    );
}

// ===================================================================
// string-glyph-split tests
// ===================================================================

#[test]
fn string_glyph_split_basic() {
    let result = builtin_string_glyph_split(vec![Value::string("abc")]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(items[0].as_str().unwrap(), "a");
    assert_eq!(items[1].as_str().unwrap(), "b");
    assert_eq!(items[2].as_str().unwrap(), "c");
}

#[test]
fn string_glyph_split_empty() {
    let result = builtin_string_glyph_split(vec![Value::string("")]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 0);
}

#[test]
fn string_glyph_split_unicode() {
    let result = builtin_string_glyph_split(vec![Value::string("\u{1F600}")]);
    let items = list_to_vec(&result.unwrap()).unwrap();
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].as_str().unwrap(), "\u{1F600}");
}

// ===================================================================
// string-equal-ignore-case tests
// ===================================================================

#[test]
fn string_equal_ignore_case_equal() {
    let result =
        builtin_string_equal_ignore_case(vec![Value::string("Hello"), Value::string("hello")]);
    assert!(result.unwrap().is_truthy());
}

#[test]
fn string_equal_ignore_case_not_equal() {
    let result =
        builtin_string_equal_ignore_case(vec![Value::string("Hello"), Value::string("world")]);
    assert!(result.unwrap().is_nil());
}

#[test]
fn string_equal_ignore_case_identical() {
    let result =
        builtin_string_equal_ignore_case(vec![Value::string("abc"), Value::string("abc")]);
    assert!(result.unwrap().is_truthy());
}

#[test]
fn string_equal_ignore_case_empty() {
    let result = builtin_string_equal_ignore_case(vec![Value::string(""), Value::string("")]);
    assert!(result.unwrap().is_truthy());
}

#[test]
fn string_equal_ignore_case_unicode() {
    let result = builtin_string_equal_ignore_case(vec![
        Value::string("\u{00DF}"), // German sharp s
        Value::string("\u{00DF}"),
    ]);
    assert!(result.unwrap().is_truthy());
}

#[test]
fn string_equal_ignore_case_wrong_type() {
    let result = builtin_string_equal_ignore_case(vec![Value::Int(42), Value::string("hello")]);
    assert!(result.is_err());
}

// ===================================================================
// unix_to_broken_down internal tests
// ===================================================================

#[test]
fn broken_down_epoch() {
    let tm = unix_to_broken_down(0);
    assert_eq!(tm.year, 1970);
    assert_eq!(tm.month, 1);
    assert_eq!(tm.day, 1);
    assert_eq!(tm.hour, 0);
    assert_eq!(tm.minute, 0);
    assert_eq!(tm.second, 0);
    assert_eq!(tm.weekday, 4); // Thursday
}

#[test]
fn broken_down_y2k() {
    // 2000-01-01 00:00:00 UTC = 946684800
    let tm = unix_to_broken_down(946684800);
    assert_eq!(tm.year, 2000);
    assert_eq!(tm.month, 1);
    assert_eq!(tm.day, 1);
    assert_eq!(tm.weekday, 6); // Saturday
}

#[test]
fn broken_down_leap_year() {
    // 2000-02-29 00:00:00 UTC = 946684800 + 59*86400 = 946684800 + 5097600 = 951782400
    let tm = unix_to_broken_down(951782400);
    assert_eq!(tm.year, 2000);
    assert_eq!(tm.month, 2);
    assert_eq!(tm.day, 29);
}

#[test]
fn broken_down_end_of_day() {
    // 1970-01-01 23:59:59 = 86399
    let tm = unix_to_broken_down(86399);
    assert_eq!(tm.year, 1970);
    assert_eq!(tm.month, 1);
    assert_eq!(tm.day, 1);
    assert_eq!(tm.hour, 23);
    assert_eq!(tm.minute, 59);
    assert_eq!(tm.second, 59);
}

#[test]
fn broken_down_2024() {
    // 2024-03-15 12:30:45 UTC
    // Compute: days from 1970 to 2024-03-15
    // Using known: 2024-01-01 = 1704067200
    // Jan has 31 days, Feb has 29 (2024 is leap), so Mar 15 = 31 + 29 + 14 = 74 days after Jan 1
    // 1704067200 + 74 * 86400 = 1704067200 + 6393600 = 1710460800
    // + 12*3600 + 30*60 + 45 = 43200 + 1800 + 45 = 45045
    // Total: 1710505845
    let tm = unix_to_broken_down(1710505845);
    assert_eq!(tm.year, 2024);
    assert_eq!(tm.month, 3);
    assert_eq!(tm.day, 15);
    assert_eq!(tm.hour, 12);
    assert_eq!(tm.minute, 30);
    assert_eq!(tm.second, 45);
}
