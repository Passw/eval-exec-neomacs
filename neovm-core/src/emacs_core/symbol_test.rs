use super::super::intern::intern;
use super::*;

#[test]
fn intern_creates_symbol() {
    let mut ob = Obarray::new();
    ob.intern("foo");
    assert!(ob.intern_soft("foo").is_some());
    assert!(ob.intern_soft("bar").is_none());
}

#[test]
fn symbol_value_cell() {
    let mut ob = Obarray::new();
    assert!(!ob.boundp("x"));
    ob.set_symbol_value("x", Value::Int(42));
    assert!(ob.boundp("x"));
    assert_eq!(ob.symbol_value("x").unwrap().as_int(), Some(42));
}

#[test]
fn symbol_function_cell() {
    let mut ob = Obarray::new();
    assert!(!ob.fboundp("f"));
    let start_epoch = ob.function_epoch();
    ob.set_symbol_function("f", Value::Subr(intern("+")));
    assert!(ob.fboundp("f"));
    assert!(ob.function_epoch() > start_epoch);
    let after_set_epoch = ob.function_epoch();
    ob.fmakunbound("f");
    assert!(!ob.fboundp("f"));
    assert!(ob.function_epoch() > after_set_epoch);
}

#[test]
fn fmakunbound_masks_builtin_fallback_name() {
    let mut ob = Obarray::new();
    let start_epoch = ob.function_epoch();
    ob.fmakunbound("car");
    assert!(ob.is_function_unbound("car"));
    assert!(!ob.fboundp("car"));
    assert!(ob.symbol_function("car").is_none());
    assert!(ob.function_epoch() > start_epoch);

    ob.set_symbol_function("car", Value::Subr(intern("car")));
    assert!(!ob.is_function_unbound("car"));
    assert!(ob.fboundp("car"));
}

#[test]
fn symbol_properties() {
    let mut ob = Obarray::new();
    ob.put_property("foo", "doc", Value::string("A function."));
    assert_eq!(
        ob.get_property("foo", "doc").unwrap().as_str(),
        Some("A function.")
    );
}

#[test]
fn special_flag() {
    let mut ob = Obarray::new();
    assert!(!ob.is_special("x"));
    ob.make_special("x");
    assert!(ob.is_special("x"));
}

#[test]
fn indirect_function_follows_chain() {
    let mut ob = Obarray::new();
    ob.set_symbol_function("real-fn", Value::Subr(intern("+")));
    // alias -> real-fn
    ob.set_symbol_function("alias", Value::Symbol(intern("real-fn")));
    let resolved = ob.indirect_function("alias").unwrap();
    assert!(matches!(resolved, Value::Subr(ref id) if resolve_sym(*id) == "+"));
}

#[test]
fn t_and_nil_are_preinterned() {
    let ob = Obarray::new();
    assert!(ob.is_constant("t"));
    assert!(ob.is_constant("nil"));
    assert!(ob.is_constant(":keyword"));
    assert!(ob.is_special("t"));
    assert!(ob.is_special("nil"));
}

#[test]
fn makunbound_doesnt_touch_constants() {
    let mut ob = Obarray::new();
    ob.makunbound("t");
    assert!(ob.boundp("t")); // t is constant, can't unbind
}
