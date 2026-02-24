//! Additional built-in functions to improve Emacs Lisp compatibility.
//!
//! These builtins complement the core set in builtins.rs with:
//! - Advanced list operations (cl-lib compatible)
//! - Sequence operations (seq.el compatible)
//! - String utilities (subr-x compatible)
//! - Window/frame operations
//! - Buffer info queries
//! - Format enhancements
//! - Variable operations

use super::error::{signal, EvalResult, Flow};
use super::intern::resolve_sym;
use super::value::{Value, read_cons, with_heap};
use std::fs;

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

fn expect_string(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Str(id) => Ok(with_heap(|h| h.get_string(*id).clone())),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), *other],
        )),
    }
}

fn expect_int(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), *other],
        )),
    }
}

fn symbol_like_name(value: &Value) -> Option<&str> {
    match value {
        Value::Nil => Some("nil"),
        Value::True => Some("t"),
        Value::Symbol(id) => Some(resolve_sym(*id)),
        Value::Keyword(id) => Some(resolve_sym(*id)),
        _ => None,
    }
}

fn expect_number_or_marker_f64(value: &Value) -> Result<f64, Flow> {
    match value {
        Value::Int(n) => Ok(*n as f64),
        Value::Char(c) => Ok(*c as u32 as f64),
        Value::Float(f) => Ok(*f),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("number-or-marker-p"), *other],
        )),
    }
}

fn list_car_or_signal(value: &Value) -> Result<Value, Flow> {
    match value {
        Value::Cons(cell) => Ok(with_heap(|h| h.cons_car(*cell))),
        Value::Nil => Ok(Value::Nil),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), *other],
        )),
    }
}

fn assoc_string_key_name(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(id) => Ok(with_heap(|h| h.get_string(*id).clone())),
        _ => symbol_like_name(value)
            .map(ToOwned::to_owned)
            .ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("stringp"), *value],
                )
            }),
    }
}

fn assoc_string_entry_name(value: &Value) -> Option<String> {
    match value {
        Value::Str(id) => Some(with_heap(|h| h.get_string(*id).clone())),
        _ => symbol_like_name(value).map(ToOwned::to_owned),
    }
}

fn assoc_string_equal(left: &str, right: &str, fold_case: bool) -> bool {
    if fold_case {
        left.chars()
            .flat_map(char::to_lowercase)
            .eq(right.chars().flat_map(char::to_lowercase))
    } else {
        left == right
    }
}

fn collect_sequence_strict(val: &Value) -> Result<Vec<Value>, Flow> {
    match val {
        Value::Nil => Ok(Vec::new()),
        Value::Cons(_) => {
            let mut result = Vec::new();
            let mut cursor = *val;
            loop {
                match cursor {
                    Value::Nil => return Ok(result),
                    Value::Cons(cell) => {
                        let pair = read_cons(cell);
                        result.push(pair.car);
                        cursor = pair.cdr;
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ));
                    }
                }
            }
        }
        Value::Vector(v) => Ok(with_heap(|h| h.get_vector(*v).clone())),
        Value::Str(id) => {
            let s = with_heap(|h| h.get_string(*id).clone());
            Ok(s.chars().map(|ch| Value::Int(ch as i64)).collect())
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), *other],
        )),
    }
}

// ---------------------------------------------------------------------------
// Advanced list operations
// ---------------------------------------------------------------------------

/// `(cl-remove-if PREDICATE LIST)` — remove elements matching predicate.
/// Since we can't call a predicate here (no eval), this is a stub that
/// works with known predicates like 'null.
pub(crate) fn builtin_remove(args: Vec<Value>) -> EvalResult {
    expect_args("remove", &args, 2)?;
    let target = &args[0];
    let list_val = &args[1];

    let mut result = Vec::new();
    let mut cursor = *list_val;
    loop {
        match cursor {
            Value::Nil => break,
            Value::Cons(cell) => {
                let pair = read_cons(cell);
                if !super::value::equal_value(&pair.car, target, 0) {
                    result.push(pair.car);
                }
                cursor = pair.cdr;
            }
            _ => break,
        }
    }
    Ok(Value::list(result))
}

/// `(remq ITEM LIST)` — remove by eq.
pub(crate) fn builtin_remq(args: Vec<Value>) -> EvalResult {
    expect_args("remq", &args, 2)?;
    let target = &args[0];
    let list_val = &args[1];

    let mut result = Vec::new();
    let mut cursor = *list_val;
    loop {
        match cursor {
            Value::Nil => break,
            Value::Cons(cell) => {
                let pair = read_cons(cell);
                if !super::value::eq_value(&pair.car, target) {
                    result.push(pair.car);
                }
                cursor = pair.cdr;
            }
            _ => break,
        }
    }
    Ok(Value::list(result))
}

/// `(flatten-tree TREE)` — flatten nested lists.
pub(crate) fn builtin_flatten_tree(args: Vec<Value>) -> EvalResult {
    expect_args("flatten-tree", &args, 1)?;
    let mut result = Vec::new();
    flatten_value(&args[0], &mut result);
    Ok(Value::list(result))
}

fn flatten_value(val: &Value, out: &mut Vec<Value>) {
    match val {
        Value::Nil => {}
        Value::Cons(cell) => {
            let pair = read_cons(*cell);
            flatten_value(&pair.car, out);
            flatten_value(&pair.cdr, out);
        }
        other => out.push(*other),
    }
}

/// `(take N LIST)` — first N elements.
pub(crate) fn builtin_take(args: Vec<Value>) -> EvalResult {
    expect_args("take", &args, 2)?;
    let n = expect_int(&args[0])?;
    if n <= 0 {
        return Ok(Value::Nil);
    }
    let list = &args[1];
    if !matches!(list, Value::Nil | Value::Cons(_)) {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), *list],
        ));
    }

    let mut result = Vec::new();
    let mut cursor = *list;
    for _ in 0..(n as usize) {
        match cursor {
            Value::Nil => break,
            Value::Cons(cell) => {
                let pair = read_cons(cell);
                result.push(pair.car);
                cursor = pair.cdr;
            }
            tail => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), tail],
                ));
            }
        }
    }
    Ok(Value::list(result))
}

/// `(seq-uniq SEQ)` — remove duplicates (equal).
pub(crate) fn builtin_seq_uniq(args: Vec<Value>) -> EvalResult {
    expect_args("seq-uniq", &args, 1)?;
    let elements = collect_sequence_strict(&args[0])?;
    let mut result = Vec::new();
    for value in &elements {
        let already = result
            .iter()
            .any(|seen| super::value::equal_value(seen, value, 0));
        if !already {
            result.push(*value);
        }
    }
    Ok(Value::list(result))
}

/// `(seq-length SEQ)` — length of a strict sequence (list/vector/string).
pub(crate) fn builtin_seq_length(args: Vec<Value>) -> EvalResult {
    expect_args("seq-length", &args, 1)?;
    let elements = collect_sequence_strict(&args[0])?;
    Ok(Value::Int(elements.len() as i64))
}

/// `(seq-into SEQ TYPE)` — convert sequence to another type.
pub(crate) fn builtin_seq_into(args: Vec<Value>) -> EvalResult {
    expect_args("seq-into", &args, 2)?;
    let target_type = match &args[1] {
        Value::Symbol(id) => resolve_sym(*id),
        other => {
            return Err(signal(
                "error",
                vec![Value::string(format!(
                    "Not a sequence type name: {}",
                    super::print::print_value(other)
                ))],
            ));
        }
    };
    let elements = collect_sequence_strict(&args[0])?;

    match target_type {
        "list" => Ok(Value::list(elements)),
        "vector" => Ok(Value::vector(elements)),
        "string" => {
            let mut s = String::new();
            for value in &elements {
                let ch = match value {
                    Value::Char(c) => *c,
                    Value::Int(n) => char::from_u32(*n as u32).ok_or_else(|| {
                        signal(
                            "wrong-type-argument",
                            vec![Value::symbol("characterp"), *value],
                        )
                    })?,
                    other => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("characterp"), *other],
                        ));
                    }
                };
                s.push(ch);
            }
            Ok(Value::string(s))
        }
        _ => Err(signal(
            "error",
            vec![Value::string(format!(
                "Not a sequence type name: {}",
                target_type
            ))],
        )),
    }
}

// ---------------------------------------------------------------------------
// String utilities (subr-x compatible)
// ---------------------------------------------------------------------------

/// `(string-empty-p STRING)` -> t or nil.
pub(crate) fn builtin_string_empty_p(args: Vec<Value>) -> EvalResult {
    expect_args("string-empty-p", &args, 1)?;
    let s = expect_string(&args[0])?;
    Ok(Value::bool(s.is_empty()))
}

/// `(string-blank-p STRING)` -> t or nil.
pub(crate) fn builtin_string_blank_p(args: Vec<Value>) -> EvalResult {
    expect_args("string-blank-p", &args, 1)?;
    let s = expect_string(&args[0])?;
    Ok(Value::bool(s.trim().is_empty()))
}

/// `(string-replace FROM TO IN)` — replace all occurrences.
pub(crate) fn builtin_string_replace(args: Vec<Value>) -> EvalResult {
    expect_args("string-replace", &args, 3)?;
    let from = expect_string(&args[0])?;
    let to = expect_string(&args[1])?;
    let input = expect_string(&args[2])?;
    Ok(Value::string(input.replace(&from, &to)))
}

/// `(string-search NEEDLE HAYSTACK &optional START)`.
pub(crate) fn builtin_string_search(args: Vec<Value>) -> EvalResult {
    expect_min_args("string-search", &args, 2)?;
    let needle = expect_string(&args[0])?;
    let haystack = expect_string(&args[1])?;
    let start = if args.len() > 2 {
        expect_int(&args[2])? as usize
    } else {
        0
    };

    let search_in = &haystack[start.min(haystack.len())..];
    match search_in.find(&needle) {
        Some(pos) => Ok(Value::Int((start + pos) as i64)),
        None => Ok(Value::Nil),
    }
}

/// `(string-to-vector STRING)` — convert string to vector of chars.
pub(crate) fn builtin_string_to_vector(args: Vec<Value>) -> EvalResult {
    expect_args("string-to-vector", &args, 1)?;
    let s = expect_string(&args[0])?;
    let chars: Vec<Value> = s.chars().map(Value::Char).collect();
    Ok(Value::vector(chars))
}

// ---------------------------------------------------------------------------
// Predicate additions
// ---------------------------------------------------------------------------

/// `(proper-list-p OBJ)` -> t if OBJ is a proper list.
pub(crate) fn builtin_proper_list_p(args: Vec<Value>) -> EvalResult {
    expect_args("proper-list-p", &args, 1)?;
    Ok(Value::bool(super::value::list_to_vec(&args[0]).is_some()))
}

/// `(subrp OBJ)` -> t if OBJ is a built-in function.
pub(crate) fn builtin_subrp(args: Vec<Value>) -> EvalResult {
    expect_args("subrp", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::Subr(_))))
}

/// `(bare-symbol SYMBOL-OR-SYMBOL-WITH-POS)` -> symbol.
pub(crate) fn builtin_bare_symbol(args: Vec<Value>) -> EvalResult {
    expect_args("bare-symbol", &args, 1)?;
    if symbol_like_name(&args[0]).is_some() {
        Ok(args[0])
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![
                Value::list(vec![
                    Value::symbol("symbolp"),
                    Value::symbol("symbol-with-pos-p"),
                ]),
                args[0],
            ],
        ))
    }
}

/// `(bare-symbol-p OBJECT)` -> t if symbol (including keyword/nil/t).
pub(crate) fn builtin_bare_symbol_p(args: Vec<Value>) -> EvalResult {
    expect_args("bare-symbol-p", &args, 1)?;
    Ok(Value::bool(symbol_like_name(&args[0]).is_some()))
}

/// `(byteorder)` -> `?l` on little-endian, `?B` on big-endian.
pub(crate) fn builtin_byteorder(args: Vec<Value>) -> EvalResult {
    expect_args("byteorder", &args, 0)?;
    let marker = if cfg!(target_endian = "little") {
        'l'
    } else {
        'B'
    };
    Ok(Value::Int(marker as i64))
}

/// `(assoc-string KEY ALIST &optional CASE-FOLD)` -> first matching cell.
pub(crate) fn builtin_assoc_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("assoc-string", &args, 2)?;
    expect_max_args("assoc-string", &args, 3)?;
    let needle = assoc_string_key_name(&args[0])?;
    let fold_case = args.get(2).is_some_and(Value::is_truthy);

    let mut cursor = args[1];
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = read_cons(cell);
                let entry = pair.car;
                cursor = pair.cdr;

                let Value::Cons(entry_cell) = entry else {
                    continue;
                };
                let entry_pair = read_cons(entry_cell);
                let Some(entry_key) = assoc_string_entry_name(&entry_pair.car) else {
                    continue;
                };
                if assoc_string_equal(&needle, &entry_key, fold_case) {
                    return Ok(Value::Cons(entry_cell));
                }
            }
            _ => return Ok(Value::Nil),
        }
    }
}

/// `(car-less-than-car A B)` -> t if `(car A) < (car B)`.
pub(crate) fn builtin_car_less_than_car(args: Vec<Value>) -> EvalResult {
    expect_args("car-less-than-car", &args, 2)?;
    let left = list_car_or_signal(&args[0])?;
    let right = list_car_or_signal(&args[1])?;
    Ok(Value::bool(
        expect_number_or_marker_f64(&left)? < expect_number_or_marker_f64(&right)?,
    ))
}

/// `(byte-code-function-p OBJ)` -> t if compiled.
pub(crate) fn builtin_byte_code_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("byte-code-function-p", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::ByteCode(_))))
}

/// `(compiled-function-p OBJ)` -> t if compiled function.
pub(crate) fn builtin_compiled_function_p(args: Vec<Value>) -> EvalResult {
    expect_args("compiled-function-p", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::ByteCode(_))))
}

/// `(closurep OBJ)` -> t if closure.
pub(crate) fn builtin_closurep(args: Vec<Value>) -> EvalResult {
    expect_args("closurep", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::Lambda(_))))
}

/// `(natnump OBJ)` -> t if natural number (>= 0).
pub(crate) fn builtin_natnump(args: Vec<Value>) -> EvalResult {
    expect_args("natnump", &args, 1)?;
    let is_nat = match &args[0] {
        Value::Int(n) => *n >= 0,
        _ => false,
    };
    Ok(Value::bool(is_nat))
}

/// `(fixnump OBJ)` -> t if fixnum.
pub(crate) fn builtin_fixnump(args: Vec<Value>) -> EvalResult {
    expect_args("fixnump", &args, 1)?;
    Ok(Value::bool(matches!(&args[0], Value::Int(_))))
}

/// `(bignump OBJ)` -> nil (we don't have bignums).
pub(crate) fn builtin_bignump(args: Vec<Value>) -> EvalResult {
    expect_args("bignump", &args, 1)?;
    Ok(Value::Nil)
}

/// `(wholenump OBJ)` -> t if whole number.
pub(crate) fn builtin_wholenump(args: Vec<Value>) -> EvalResult {
    expect_args("wholenump", &args, 1)?;
    let is_whole = match &args[0] {
        Value::Int(n) => *n >= 0,
        _ => false,
    };
    Ok(Value::bool(is_whole))
}

/// `(zerop OBJ)` -> t if zero.
pub(crate) fn builtin_zerop(args: Vec<Value>) -> EvalResult {
    expect_args("zerop", &args, 1)?;
    let is_zero = match &args[0] {
        Value::Int(0) => true,
        Value::Float(f) => *f == 0.0,
        _ => false,
    };
    Ok(Value::bool(is_zero))
}

// ---------------------------------------------------------------------------
// Misc operations
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct PasswdEntry {
    login: String,
    uid: i64,
    gecos: String,
}

fn parse_passwd_entry(line: &str) -> Option<PasswdEntry> {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return None;
    }

    let mut fields = trimmed.split(':');
    let login = fields.next()?.to_string();
    let _passwd = fields.next()?;
    let uid = fields.next()?.parse::<i64>().ok()?;
    let _gid = fields.next()?;
    let gecos = fields.next().unwrap_or("").to_string();
    Some(PasswdEntry { login, uid, gecos })
}

fn load_passwd_entries() -> Vec<PasswdEntry> {
    fs::read_to_string("/etc/passwd")
        .ok()
        .map(|content| content.lines().filter_map(parse_passwd_entry).collect())
        .unwrap_or_default()
}

fn login_name_from_env() -> Option<String> {
    std::env::var("LOGNAME")
        .ok()
        .or_else(|| std::env::var("USER").ok())
        .filter(|name| !name.is_empty())
}

fn current_uid() -> i64 {
    std::process::Command::new("id")
        .arg("-u")
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .and_then(|s| s.trim().parse::<i64>().ok())
        .unwrap_or(1000)
}

fn real_uid() -> i64 {
    std::process::Command::new("id")
        .args(["-ru"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .and_then(|s| s.trim().parse::<i64>().ok())
        .unwrap_or_else(current_uid)
}

fn lookup_login_by_uid(uid: i64) -> Option<String> {
    load_passwd_entries()
        .into_iter()
        .find(|entry| entry.uid == uid)
        .map(|entry| entry.login)
}

fn canonical_full_name(entry: &PasswdEntry) -> String {
    let first_gecos = entry.gecos.split(',').next().unwrap_or("").trim();
    if first_gecos.is_empty() {
        entry.login.clone()
    } else {
        first_gecos.to_string()
    }
}

fn lookup_full_name_by_uid(uid: i64) -> Option<String> {
    load_passwd_entries()
        .into_iter()
        .find(|entry| entry.uid == uid)
        .map(|entry| canonical_full_name(&entry))
}

fn lookup_full_name_by_login(login: &str) -> Option<String> {
    load_passwd_entries()
        .into_iter()
        .find(|entry| entry.login == login)
        .map(|entry| canonical_full_name(&entry))
}

fn expect_uid_arg(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(uid) if *uid >= 0 => Ok(*uid),
        _ => Err(signal(
            "error",
            vec![Value::string(
                "Not an in-range integer, integral float, or cons of integers",
            )],
        )),
    }
}

/// `(user-login-name &optional UID)` -> string or nil.
pub(crate) fn builtin_user_login_name(args: Vec<Value>) -> EvalResult {
    expect_max_args("user-login-name", &args, 1)?;
    if let Some(uid_arg) = args.first() {
        if uid_arg.is_nil() {
            let current = login_name_from_env()
                .or_else(|| lookup_login_by_uid(current_uid()))
                .unwrap_or_else(|| "unknown".to_string());
            return Ok(Value::string(current));
        }
        let uid = expect_uid_arg(uid_arg)?;
        return Ok(match lookup_login_by_uid(uid) {
            Some(name) => Value::string(name),
            None => Value::Nil,
        });
    }

    let current = login_name_from_env()
        .or_else(|| lookup_login_by_uid(current_uid()))
        .unwrap_or_else(|| "unknown".to_string());
    Ok(Value::string(current))
}

/// `(user-real-login-name)` -> string.
pub(crate) fn builtin_user_real_login_name(args: Vec<Value>) -> EvalResult {
    expect_args("user-real-login-name", &args, 0)?;
    let name = lookup_login_by_uid(real_uid())
        .or_else(login_name_from_env)
        .unwrap_or_else(|| "unknown".to_string());
    Ok(Value::string(name))
}

/// `(user-full-name &optional UID-OR-LOGIN)` -> string or nil.
pub(crate) fn builtin_user_full_name(args: Vec<Value>) -> EvalResult {
    expect_max_args("user-full-name", &args, 1)?;
    if let Some(target) = args.first() {
        if target.is_nil() {
            if let Ok(name) = std::env::var("NAME") {
                if !name.is_empty() {
                    return Ok(Value::string(name));
                }
            }
            let fallback = lookup_full_name_by_uid(current_uid())
                .or_else(|| {
                    login_name_from_env()
                        .as_deref()
                        .and_then(lookup_full_name_by_login)
                })
                .or_else(login_name_from_env)
                .unwrap_or_else(|| "unknown".to_string());
            return Ok(Value::string(fallback));
        }

        return Ok(match target {
            Value::Int(uid) => {
                if *uid < 0 {
                    return Err(signal(
                        "error",
                        vec![Value::string(
                            "Not an in-range integer, integral float, or cons of integers",
                        )],
                    ));
                }
                lookup_full_name_by_uid(*uid)
                    .map(Value::string)
                    .unwrap_or(Value::Nil)
            }
            Value::Str(id) => {
                let login = with_heap(|h| h.get_string(*id).clone());
                lookup_full_name_by_login(&login)
                    .map(Value::string)
                    .unwrap_or(Value::Nil)
            }
            _ => {
                return Err(signal(
                    "error",
                    vec![Value::string(
                        "Not an in-range integer, integral float, or cons of integers",
                    )],
                ))
            }
        });
    }

    if let Ok(name) = std::env::var("NAME") {
        if !name.is_empty() {
            return Ok(Value::string(name));
        }
    }

    let fallback = lookup_full_name_by_uid(current_uid())
        .or_else(|| {
            login_name_from_env()
                .as_deref()
                .and_then(lookup_full_name_by_login)
        })
        .or_else(login_name_from_env)
        .unwrap_or_else(|| "unknown".to_string());
    Ok(Value::string(fallback))
}

/// `(system-name)` -> string.
pub(crate) fn builtin_system_name(args: Vec<Value>) -> EvalResult {
    expect_args("system-name", &args, 0)?;
    let name = std::env::var("HOSTNAME").unwrap_or_else(|_| "localhost".to_string());
    Ok(Value::string(name))
}

/// `(emacs-version)` -> string.
pub(crate) fn builtin_emacs_version(args: Vec<Value>) -> EvalResult {
    expect_max_args("emacs-version", &args, 1)?;
    if args.first().is_some_and(|arg| !arg.is_nil()) {
        return Ok(Value::Nil);
    }
    Ok(Value::string("NeoVM 0.1.0 (Neomacs)"))
}

/// `(emacs-pid)` -> integer.
pub(crate) fn builtin_emacs_pid(args: Vec<Value>) -> EvalResult {
    expect_args("emacs-pid", &args, 0)?;
    Ok(Value::Int(std::process::id() as i64))
}

fn gc_bucket(name: &str, counts: &[i64]) -> Value {
    let mut items = Vec::with_capacity(counts.len() + 1);
    items.push(Value::symbol(name));
    items.extend(counts.iter().copied().map(Value::Int));
    Value::list(items)
}

/// `(garbage-collect)` -> GC stats list.
pub(crate) fn builtin_garbage_collect(args: Vec<Value>) -> EvalResult {
    expect_args("garbage-collect", &args, 0)?;
    let counts = Value::memory_use_counts_snapshot();
    let conses = counts[0].max(0);
    let floats = counts[1].max(0);
    let vector_cells = counts[2].max(0);
    let symbols = counts[3].max(0);
    let string_chars = counts[4].max(0);
    let intervals = counts[5].max(0);
    let strings = counts[6].max(0);

    Ok(Value::list(vec![
        gc_bucket("conses", &[16, conses, 0]),
        gc_bucket("symbols", &[48, symbols, 0]),
        gc_bucket("strings", &[32, strings, 0]),
        gc_bucket("string-bytes", &[1, string_chars]),
        gc_bucket("vectors", &[16, vector_cells]),
        gc_bucket("vector-slots", &[8, vector_cells, 0]),
        gc_bucket("floats", &[8, floats, 0]),
        gc_bucket("intervals", &[56, intervals, 0]),
        gc_bucket("buffers", &[992, 0]),
    ]))
}

/// `(memory-use-counts)` -> list of runtime allocation counters:
/// `(CONS FLOATS VECTOR-CELLS SYMBOLS STRING-CHARS INTERVALS STRINGS)`.
pub(crate) fn builtin_memory_use_counts(args: Vec<Value>) -> EvalResult {
    expect_args("memory-use-counts", &args, 0)?;
    let counts = Value::memory_use_counts_snapshot();
    Ok(Value::list(
        counts.iter().map(|count| Value::Int(*count)).collect(),
    ))
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::intern::intern;
    use crate::elisp::value::{LambdaData, LambdaParams};

    #[test]
    fn remove_from_list() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(2),
        ]);
        let result = builtin_remove(vec![Value::Int(2), list]).unwrap();
        let items = super::super::value::list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn flatten_tree() {
        let nested = Value::list(vec![
            Value::Int(1),
            Value::list(vec![Value::Int(2), Value::Int(3)]),
            Value::Int(4),
        ]);
        let result = builtin_flatten_tree(vec![nested]).unwrap();
        let items = super::super::value::list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 4);
    }

    #[test]
    fn take_from_list() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_take(vec![Value::Int(2), list]).unwrap();
        let items = super::super::value::list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn string_empty_blank() {
        assert!(matches!(
            builtin_string_empty_p(vec![Value::string("")]).unwrap(),
            Value::True,
        ));
        assert!(builtin_string_empty_p(vec![Value::string("a")])
            .unwrap()
            .is_nil(),);
        assert!(matches!(
            builtin_string_blank_p(vec![Value::string("  ")]).unwrap(),
            Value::True,
        ));
    }

    #[test]
    fn string_replace() {
        let result = builtin_string_replace(vec![
            Value::string("world"),
            Value::string("rust"),
            Value::string("hello world"),
        ])
        .unwrap();
        assert_eq!(result.as_str(), Some("hello rust"));
    }

    #[test]
    fn string_search() {
        let result =
            builtin_string_search(vec![Value::string("world"), Value::string("hello world")])
                .unwrap();
        assert_eq!(result.as_int(), Some(6));

        let result =
            builtin_string_search(vec![Value::string("xyz"), Value::string("hello")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn proper_list_p() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        assert!(matches!(
            builtin_proper_list_p(vec![list]).unwrap(),
            Value::True,
        ));
        assert!(builtin_proper_list_p(vec![Value::Int(5)]).unwrap().is_nil(),);
    }

    #[test]
    fn closurep_true_for_lambda_values() {
        let lambda = Value::make_lambda(LambdaData {
            params: LambdaParams::simple(vec![intern("x")]),
            body: vec![],
            env: None,
            docstring: None,
        });
        assert!(builtin_closurep(vec![lambda]).unwrap().is_truthy());
        assert!(builtin_closurep(vec![Value::Int(1)]).unwrap().is_nil());
    }

    #[test]
    fn bare_symbol_and_predicate_semantics() {
        assert_eq!(
            builtin_bare_symbol(vec![Value::symbol("alpha")]).unwrap(),
            Value::symbol("alpha")
        );
        assert_eq!(
            builtin_bare_symbol(vec![Value::keyword(":k")]).unwrap(),
            Value::keyword(":k")
        );
        assert_eq!(builtin_bare_symbol(vec![Value::Nil]).unwrap(), Value::Nil);

        assert!(builtin_bare_symbol_p(vec![Value::symbol("alpha")])
            .unwrap()
            .is_truthy());
        assert!(builtin_bare_symbol_p(vec![Value::keyword(":k")])
            .unwrap()
            .is_truthy());
        assert!(builtin_bare_symbol_p(vec![Value::Nil]).unwrap().is_truthy());
        assert!(builtin_bare_symbol_p(vec![Value::Int(1)]).unwrap().is_nil());

        let err = builtin_bare_symbol(vec![Value::Int(1)]).unwrap_err();
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data[1], Value::Int(1));
            }
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn byteorder_shape_and_arity() {
        let byteorder = builtin_byteorder(vec![]).unwrap();
        assert!(matches!(byteorder, Value::Int(108) | Value::Int(66)));

        let err = builtin_byteorder(vec![Value::Nil]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn assoc_string_and_car_less_than_car_semantics() {
        let mut heap = crate::gc::heap::LispHeap::new();
        crate::elisp::value::set_current_heap(&mut heap);

        let result = builtin_assoc_string(vec![
            Value::string("A"),
            Value::list(vec![
                Value::cons(Value::string("a"), Value::Int(1)),
                Value::cons(Value::string("b"), Value::Int(2)),
            ]),
            Value::True,
        ])
        .unwrap();
        let Value::Cons(result_cell) = result else {
            panic!("expected dotted pair result");
        };
        let result_pair = read_cons(result_cell);
        assert_eq!(result_pair.car, Value::string("a"));
        assert_eq!(result_pair.cdr, Value::Int(1));

        let symbol_alist = Value::list(vec![
            Value::cons(Value::symbol("foo"), Value::Int(1)),
            Value::cons(Value::keyword(":k"), Value::Int(2)),
        ]);
        let symbol_hit = builtin_assoc_string(vec![Value::string("foo"), symbol_alist]).unwrap();
        let Value::Cons(symbol_cell) = symbol_hit else {
            panic!("expected dotted pair result");
        };
        let symbol_pair = read_cons(symbol_cell);
        assert_eq!(symbol_pair.car, Value::symbol("foo"));
        assert_eq!(symbol_pair.cdr, Value::Int(1));

        let nil_tail = Value::cons(
            Value::cons(Value::string("x"), Value::Int(1)),
            Value::Int(2),
        );
        assert!(builtin_assoc_string(vec![Value::string("x"), nil_tail])
            .unwrap()
            .is_truthy());
        assert!(
            builtin_assoc_string(vec![Value::string("y"), Value::Int(1)])
                .unwrap()
                .is_nil()
        );

        let key_err = builtin_assoc_string(vec![Value::Int(1), Value::Nil]).unwrap_err();
        match key_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }

        assert!(builtin_car_less_than_car(vec![
            Value::cons(Value::Int(1), Value::symbol("a")),
            Value::cons(Value::Int(2), Value::symbol("b")),
        ])
        .unwrap()
        .is_truthy());
        assert!(builtin_car_less_than_car(vec![
            Value::cons(Value::Float(3.0), Value::symbol("a")),
            Value::cons(Value::Int(2), Value::symbol("b")),
        ])
        .unwrap()
        .is_nil());

        let list_err =
            builtin_car_less_than_car(vec![Value::Int(1), Value::cons(Value::Int(2), Value::Nil)])
                .unwrap_err();
        match list_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }

        let number_err = builtin_car_less_than_car(vec![
            Value::cons(Value::symbol("x"), Value::Nil),
            Value::cons(Value::Int(1), Value::Nil),
        ])
        .unwrap_err();
        match number_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn number_predicates() {
        assert!(matches!(
            builtin_zerop(vec![Value::Int(0)]).unwrap(),
            Value::True
        ));
        assert!(builtin_zerop(vec![Value::Int(1)]).unwrap().is_nil());
        assert!(matches!(
            builtin_natnump(vec![Value::Int(5)]).unwrap(),
            Value::True
        ));
        assert!(builtin_natnump(vec![Value::Int(-1)]).unwrap().is_nil());
    }

    #[test]
    fn seq_uniq() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(1),
            Value::Int(3),
        ]);
        let result = builtin_seq_uniq(vec![list]).unwrap();
        let items = super::super::value::list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn seq_length_list_and_string() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let list_len = builtin_seq_length(vec![list]).unwrap();
        assert_eq!(list_len.as_int(), Some(3));

        let string_len = builtin_seq_length(vec![Value::string("hello")]).unwrap();
        assert_eq!(string_len.as_int(), Some(5));
    }

    #[test]
    fn seq_length_wrong_type_errors() {
        match builtin_seq_length(vec![Value::Int(42)]) {
            Err(Flow::Signal(sig)) => assert_eq!(sig.symbol, "wrong-type-argument"),
            other => panic!("expected wrong-type-argument, got {other:?}"),
        }
    }

    #[test]
    fn user_info() {
        // These should not panic, just return strings.
        assert!(builtin_user_login_name(vec![]).unwrap().is_string());
        assert!(builtin_user_real_login_name(vec![]).unwrap().is_string());
        assert!(builtin_user_full_name(vec![]).unwrap().is_string());
        assert!(builtin_system_name(vec![]).unwrap().is_string());
        assert!(builtin_emacs_version(vec![]).unwrap().is_string());
    }

    #[test]
    fn user_identity_optional_args() {
        let login_for_uid = builtin_user_login_name(vec![Value::Int(current_uid())]).unwrap();
        assert!(login_for_uid.is_nil() || login_for_uid.is_string());

        let by_uid = builtin_user_full_name(vec![Value::Int(current_uid())]).unwrap();
        assert!(by_uid.is_nil() || by_uid.is_string());

        let login = builtin_user_login_name(vec![]).unwrap();
        let by_login = builtin_user_full_name(vec![login]).unwrap();
        assert!(by_login.is_nil() || by_login.is_string());
    }

    #[test]
    fn user_identity_arity_contracts() {
        let login_name_err =
            builtin_user_login_name(vec![Value::Int(1), Value::Int(2)]).unwrap_err();
        match login_name_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }

        let real_login_err = builtin_user_real_login_name(vec![Value::Int(1)]).unwrap_err();
        match real_login_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }

        let full_name_err = builtin_user_full_name(vec![Value::Int(1), Value::Int(2)]).unwrap_err();
        match full_name_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn user_identity_type_contracts() {
        let login_name_err = builtin_user_login_name(vec![Value::string("root")]).unwrap_err();
        match login_name_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }

        let full_name_err =
            builtin_user_full_name(vec![Value::list(vec![Value::Int(1)])]).unwrap_err();
        match full_name_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }

        let negative_uid_login = builtin_user_login_name(vec![Value::Int(-1)]).unwrap_err();
        match negative_uid_login {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }

        let negative_uid_full_name = builtin_user_full_name(vec![Value::Int(-1)]).unwrap_err();
        match negative_uid_full_name {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "error"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn emacs_pid() {
        let pid = builtin_emacs_pid(vec![]).unwrap();
        assert!(matches!(pid, Value::Int(n) if n > 0));
    }

    #[test]
    fn runtime_identity_arity_contracts() {
        let system_name_err = builtin_system_name(vec![Value::Nil]).unwrap_err();
        match system_name_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }

        let version_with_nil = builtin_emacs_version(vec![Value::Nil]).unwrap();
        assert!(version_with_nil.is_string());

        let version_with_non_nil = builtin_emacs_version(vec![Value::True]).unwrap();
        assert!(version_with_non_nil.is_nil());

        let version_err = builtin_emacs_version(vec![Value::Nil, Value::Nil]).unwrap_err();
        match version_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }

        let pid_err = builtin_emacs_pid(vec![Value::Nil]).unwrap_err();
        match pid_err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn garbage_collect_shape_and_arity() {
        let gc = builtin_garbage_collect(vec![]).unwrap();
        let buckets = super::super::value::list_to_vec(&gc).expect("gc list");
        assert_eq!(buckets.len(), 9);
        let names = buckets
            .iter()
            .map(|bucket| {
                let bucket_items = super::super::value::list_to_vec(bucket).expect("bucket list");
                match bucket_items.first() {
                    Some(Value::Symbol(id)) => resolve_sym(*id).to_owned(),
                    other => panic!("expected bucket symbol, got {other:?}"),
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(
            names,
            vec![
                "conses".to_string(),
                "symbols".to_string(),
                "strings".to_string(),
                "string-bytes".to_string(),
                "vectors".to_string(),
                "vector-slots".to_string(),
                "floats".to_string(),
                "intervals".to_string(),
                "buffers".to_string(),
            ]
        );
        for bucket in &buckets {
            let bucket_items = super::super::value::list_to_vec(bucket).expect("bucket list");
            assert!(bucket_items.len() >= 2);
            assert!(matches!(bucket_items[0], Value::Symbol(_)));
            assert!(bucket_items[1..]
                .iter()
                .all(|item| matches!(item, Value::Int(_))));
        }

        let err = builtin_garbage_collect(vec![Value::Int(1)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }

    #[test]
    fn memory_use_counts_shape_and_arity() {
        let counts = builtin_memory_use_counts(vec![]).unwrap();
        let items = super::super::value::list_to_vec(&counts).expect("counts list");
        assert_eq!(items.len(), 7);
        assert!(items.iter().all(|item| matches!(item, Value::Int(_))));

        let err = builtin_memory_use_counts(vec![Value::Int(1)]).unwrap_err();
        match err {
            Flow::Signal(sig) => assert_eq!(sig.symbol, "wrong-number-of-arguments"),
            other => panic!("expected signal, got {other:?}"),
        }
    }
}
