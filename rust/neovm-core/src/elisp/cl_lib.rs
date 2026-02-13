//! CL-lib, seq.el, and JSON built-in functions.
//!
//! Provides Common Lisp compatibility functions, sequence operations,
//! and JSON parsing/serialization for the Elisp interpreter.

use super::error::{signal, EvalResult, Flow};
use super::value::*;
use std::sync::{Arc, Mutex};

// ---------------------------------------------------------------------------
// Argument helpers (local copies for this module)
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

fn expect_int(val: &Value) -> Result<i64, Flow> {
    match val {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integerp"), other.clone()],
        )),
    }
}

fn expect_string(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

/// Collect elements from any sequence type into a Vec.
fn collect_sequence(val: &Value) -> Vec<Value> {
    match val {
        Value::Nil => Vec::new(),
        Value::Cons(_) => list_to_vec(val).unwrap_or_default(),
        Value::Vector(v) => v.lock().expect("poisoned").clone(),
        Value::Str(s) => s.chars().map(Value::Char).collect(),
        _ => vec![val.clone()],
    }
}

/// Convert a type-name symbol to a string for type dispatch.
fn type_name_str(val: &Value) -> &str {
    match val {
        Value::Symbol(s) => s.as_str(),
        Value::Keyword(s) => s.as_str(),
        _ => "",
    }
}

fn seq_position_list_elements(seq: &Value) -> Result<Vec<Value>, Flow> {
    let mut elements = Vec::new();
    let mut cursor = seq.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(elements),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                elements.push(pair.car.clone());
                cursor = pair.cdr.clone();
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

fn seq_position_elements(seq: &Value) -> Result<Vec<Value>, Flow> {
    match seq {
        Value::Nil => Ok(Vec::new()),
        Value::Cons(_) => seq_position_list_elements(seq),
        Value::Vector(v) => Ok(v.lock().expect("poisoned").clone()),
        Value::Str(s) => Ok(s.chars().map(|ch| Value::Int(ch as i64)).collect()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), other.clone()],
        )),
    }
}

fn seq_default_match(left: &Value, right: &Value) -> bool {
    if equal_value(left, right, 0) {
        return true;
    }
    match (left, right) {
        (Value::Char(a), Value::Int(b)) => (*a as i64) == *b,
        (Value::Int(a), Value::Char(b)) => *a == (*b as i64),
        _ => false,
    }
}

// ===========================================================================
// CL-lib pure list operations
// ===========================================================================

/// `(cl-find ITEM SEQ)` — find item in sequence using `equal`.
pub fn builtin_cl_find(args: Vec<Value>) -> EvalResult {
    expect_args("cl-find", &args, 2)?;
    let item = &args[0];
    let elems = collect_sequence(&args[1]);
    for e in &elems {
        if equal_value(item, e, 0) {
            return Ok(e.clone());
        }
    }
    Ok(Value::Nil)
}

/// `(cl-position ITEM SEQ)` — position of item in sequence using `equal`.
pub fn builtin_cl_position(args: Vec<Value>) -> EvalResult {
    expect_args("cl-position", &args, 2)?;
    let item = &args[0];
    let elems = collect_sequence(&args[1]);
    for (i, e) in elems.iter().enumerate() {
        if equal_value(item, e, 0) {
            return Ok(Value::Int(i as i64));
        }
    }
    Ok(Value::Nil)
}

/// `(cl-count ITEM SEQ)` — count occurrences of item using `equal`.
pub fn builtin_cl_count(args: Vec<Value>) -> EvalResult {
    expect_args("cl-count", &args, 2)?;
    let item = &args[0];
    let elems = collect_sequence(&args[1]);
    let count = elems.iter().filter(|e| equal_value(item, e, 0)).count();
    Ok(Value::Int(count as i64))
}

/// `(cl-remove ITEM SEQ)` — remove all occurrences of item using `equal`.
pub fn builtin_cl_remove(args: Vec<Value>) -> EvalResult {
    expect_args("cl-remove", &args, 2)?;
    let item = &args[0];
    let elems = collect_sequence(&args[1]);
    let result: Vec<Value> = elems
        .into_iter()
        .filter(|e| !equal_value(item, e, 0))
        .collect();
    Ok(Value::list(result))
}

/// `(cl-substitute NEW OLD SEQ)` — replace old with new in sequence using `equal`.
pub fn builtin_cl_substitute(args: Vec<Value>) -> EvalResult {
    expect_args("cl-substitute", &args, 3)?;
    let new = &args[0];
    let old = &args[1];
    let elems = collect_sequence(&args[2]);
    let result: Vec<Value> = elems
        .into_iter()
        .map(|e| {
            if equal_value(old, &e, 0) {
                new.clone()
            } else {
                e
            }
        })
        .collect();
    Ok(Value::list(result))
}

/// `(cl-intersection LIST1 LIST2)` — set intersection using `equal`.
pub fn builtin_cl_intersection(args: Vec<Value>) -> EvalResult {
    expect_args("cl-intersection", &args, 2)?;
    let elems1 = collect_sequence(&args[0]);
    let elems2 = collect_sequence(&args[1]);
    let result: Vec<Value> = elems1
        .into_iter()
        .filter(|e| elems2.iter().any(|e2| equal_value(e, e2, 0)))
        .collect();
    Ok(Value::list(result))
}

/// `(cl-union LIST1 LIST2)` — set union using `equal`.
pub fn builtin_cl_union(args: Vec<Value>) -> EvalResult {
    expect_args("cl-union", &args, 2)?;
    let elems1 = collect_sequence(&args[0]);
    let elems2 = collect_sequence(&args[1]);
    let mut result = elems1;
    for e in elems2 {
        if !result.iter().any(|r| equal_value(r, &e, 0)) {
            result.push(e);
        }
    }
    Ok(Value::list(result))
}

/// `(cl-set-difference LIST1 LIST2)` — set difference using `equal`.
pub fn builtin_cl_set_difference(args: Vec<Value>) -> EvalResult {
    expect_args("cl-set-difference", &args, 2)?;
    let elems1 = collect_sequence(&args[0]);
    let elems2 = collect_sequence(&args[1]);
    let result: Vec<Value> = elems1
        .into_iter()
        .filter(|e| !elems2.iter().any(|e2| equal_value(e, e2, 0)))
        .collect();
    Ok(Value::list(result))
}

/// `(cl-subsetp LIST1 LIST2)` — is list1 a subset of list2?
pub fn builtin_cl_subsetp(args: Vec<Value>) -> EvalResult {
    expect_args("cl-subsetp", &args, 2)?;
    let elems1 = collect_sequence(&args[0]);
    let elems2 = collect_sequence(&args[1]);
    let all = elems1
        .iter()
        .all(|e| elems2.iter().any(|e2| equal_value(e, e2, 0)));
    Ok(Value::bool(all))
}

/// `(cl-adjoin ITEM LIST)` — add item if not present using `equal`.
pub fn builtin_cl_adjoin(args: Vec<Value>) -> EvalResult {
    expect_args("cl-adjoin", &args, 2)?;
    let item = &args[0];
    let elems = collect_sequence(&args[1]);
    if elems.iter().any(|e| equal_value(item, e, 0)) {
        Ok(args[1].clone())
    } else {
        Ok(Value::cons(item.clone(), args[1].clone()))
    }
}

/// `(cl-remove-duplicates SEQ)` — remove duplicates using `equal`.
pub fn builtin_cl_remove_duplicates(args: Vec<Value>) -> EvalResult {
    expect_args("cl-remove-duplicates", &args, 1)?;
    let elems = collect_sequence(&args[0]);
    let mut result: Vec<Value> = Vec::new();
    for e in elems {
        if !result.iter().any(|r| equal_value(r, &e, 0)) {
            result.push(e);
        }
    }
    Ok(Value::list(result))
}

/// `(cl-member ITEM LIST)` — like member but using `equal` (same as member).
pub fn builtin_cl_member(args: Vec<Value>) -> EvalResult {
    expect_args("cl-member", &args, 2)?;
    let item = &args[0];
    let mut cursor = args[1].clone();
    loop {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                if equal_value(item, &pair.car, 0) {
                    drop(pair);
                    return Ok(Value::Cons(cell));
                }
                cursor = pair.cdr.clone();
            }
            _ => return Ok(Value::Nil),
        }
    }
}

// ---------------------------------------------------------------------------
// cl-first through cl-tenth, cl-rest
// ---------------------------------------------------------------------------

fn nth_helper(name: &str, args: &[Value], n: usize) -> EvalResult {
    expect_args(name, args, 1)?;
    let mut cursor = args[0].clone();
    for _ in 0..n {
        match cursor {
            Value::Nil => return Ok(Value::Nil),
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                cursor = pair.cdr.clone();
            }
            _ => return Ok(Value::Nil),
        }
    }
    match cursor {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            Ok(pair.car.clone())
        }
        _ => Ok(Value::Nil),
    }
}

pub fn builtin_cl_first(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-first", &args, 0)
}
pub fn builtin_cl_second(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-second", &args, 1)
}
pub fn builtin_cl_third(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-third", &args, 2)
}
pub fn builtin_cl_fourth(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-fourth", &args, 3)
}
pub fn builtin_cl_fifth(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-fifth", &args, 4)
}
pub fn builtin_cl_sixth(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-sixth", &args, 5)
}
pub fn builtin_cl_seventh(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-seventh", &args, 6)
}
pub fn builtin_cl_eighth(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-eighth", &args, 7)
}
pub fn builtin_cl_ninth(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-ninth", &args, 8)
}
pub fn builtin_cl_tenth(args: Vec<Value>) -> EvalResult {
    nth_helper("cl-tenth", &args, 9)
}

/// `(cl-rest LIST)` — alias for cdr.
pub fn builtin_cl_rest(args: Vec<Value>) -> EvalResult {
    expect_args("cl-rest", &args, 1)?;
    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(cell) => {
            let pair = cell.lock().expect("poisoned");
            Ok(pair.cdr.clone())
        }
        _ => Ok(Value::Nil),
    }
}

/// `(cl-subseq SEQ START &optional END)` — subsequence.
pub fn builtin_cl_subseq(args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-subseq", &args, 2)?;
    let elems = collect_sequence(&args[0]);
    let start = expect_int(&args[1])? as usize;
    let end = if args.len() > 2 && !args[2].is_nil() {
        expect_int(&args[2])? as usize
    } else {
        elems.len()
    };
    if start > elems.len() || end > elems.len() || start > end {
        return Err(signal(
            "args-out-of-range",
            vec![
                args[0].clone(),
                Value::Int(start as i64),
                Value::Int(end as i64),
            ],
        ));
    }
    let result: Vec<Value> = elems[start..end].to_vec();
    // Return same type as input
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(result)),
        Value::Str(_) => {
            let s: String = result
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(result)),
    }
}

/// `(cl-concatenate TYPE &rest SEQUENCES)` — concatenate sequences into target type.
pub fn builtin_cl_concatenate(args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-concatenate", &args, 1)?;
    let target = type_name_str(&args[0]);
    let mut combined = Vec::new();
    for arg in &args[1..] {
        combined.extend(collect_sequence(arg));
    }
    match target {
        "vector" => Ok(Value::vector(combined)),
        "string" => {
            let s: String = combined
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    Value::Int(n) => char::from_u32(*n as u32),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(combined)),
    }
}

/// `(cl-coerce OBJ TYPE)` — type coercion.
pub fn builtin_cl_coerce(args: Vec<Value>) -> EvalResult {
    expect_args("cl-coerce", &args, 2)?;
    let obj = &args[0];
    let target = type_name_str(&args[1]);
    match target {
        "list" => {
            let elems = collect_sequence(obj);
            Ok(Value::list(elems))
        }
        "vector" => {
            let elems = collect_sequence(obj);
            Ok(Value::vector(elems))
        }
        "string" => {
            let elems = collect_sequence(obj);
            let s: String = elems
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    Value::Int(n) => char::from_u32(*n as u32),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        "float" => match obj {
            Value::Int(n) => Ok(Value::Float(*n as f64)),
            Value::Float(_) => Ok(obj.clone()),
            _ => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("numberp"), obj.clone()],
            )),
        },
        "integer" => match obj {
            Value::Float(f) => Ok(Value::Int(*f as i64)),
            Value::Int(_) => Ok(obj.clone()),
            Value::Char(c) => Ok(Value::Int(*c as i64)),
            _ => Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("numberp"), obj.clone()],
            )),
        },
        _ => Ok(obj.clone()),
    }
}

// ===========================================================================
// CL-lib eval-dependent operations
// ===========================================================================

/// `(cl-map TYPE FN &rest SEQS)` — map over sequences, return target type.
pub fn builtin_cl_map(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-map", &args, 3)?;
    let target = type_name_str(&args[0]);
    let func = args[1].clone();
    let seqs: Vec<Vec<Value>> = args[2..].iter().map(|s| collect_sequence(s)).collect();
    if seqs.is_empty() {
        return Ok(Value::Nil);
    }
    let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
    let mut results = Vec::new();
    for i in 0..min_len {
        let call_args: Vec<Value> = seqs.iter().map(|s| s[i].clone()).collect();
        results.push(eval.apply(func.clone(), call_args)?);
    }
    match target {
        "vector" => Ok(Value::vector(results)),
        "string" => {
            let s: String = results
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    Value::Int(n) => char::from_u32(*n as u32),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(results)),
    }
}

/// `(cl-every PRED &rest SEQS)` — all elements satisfy predicate.
pub fn builtin_cl_every(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-every", &args, 2)?;
    let pred = args[0].clone();
    let seqs: Vec<Vec<Value>> = args[1..].iter().map(|s| collect_sequence(s)).collect();
    if seqs.is_empty() {
        return Ok(Value::True);
    }
    let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
    for i in 0..min_len {
        let call_args: Vec<Value> = seqs.iter().map(|s| s[i].clone()).collect();
        let result = eval.apply(pred.clone(), call_args)?;
        if result.is_nil() {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::True)
}

/// `(cl-some PRED &rest SEQS)` — some element satisfies predicate.
pub fn builtin_cl_some(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-some", &args, 2)?;
    let pred = args[0].clone();
    let seqs: Vec<Vec<Value>> = args[1..].iter().map(|s| collect_sequence(s)).collect();
    if seqs.is_empty() {
        return Ok(Value::Nil);
    }
    let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
    for i in 0..min_len {
        let call_args: Vec<Value> = seqs.iter().map(|s| s[i].clone()).collect();
        let result = eval.apply(pred.clone(), call_args)?;
        if result.is_truthy() {
            return Ok(result);
        }
    }
    Ok(Value::Nil)
}

/// `(cl-notevery PRED &rest SEQS)` — not all satisfy.
pub fn builtin_cl_notevery(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    let result = builtin_cl_every(eval, args)?;
    Ok(Value::bool(result.is_nil()))
}

/// `(cl-notany PRED &rest SEQS)` — none satisfy.
pub fn builtin_cl_notany(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    let result = builtin_cl_some(eval, args)?;
    Ok(Value::bool(result.is_nil()))
}

/// `(cl-reduce FN SEQ &optional INITIAL-VALUE)` — reduce/fold.
pub fn builtin_cl_reduce(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("cl-reduce", &args, 2)?;
    let func = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let initial = if args.len() > 2 {
        Some(args[2].clone())
    } else {
        None
    };

    let mut iter = elems.into_iter();
    let mut acc = match initial {
        Some(v) => v,
        None => match iter.next() {
            Some(v) => v,
            None => return Ok(Value::Nil),
        },
    };
    for elem in iter {
        acc = eval.apply(func.clone(), vec![acc, elem])?;
    }
    Ok(acc)
}

/// `(cl-remove-if PRED SEQ)` — remove elements matching predicate.
pub fn builtin_cl_remove_if(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("cl-remove-if", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut result = Vec::new();
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e.clone()])?;
        if r.is_nil() {
            result.push(e);
        }
    }
    Ok(Value::list(result))
}

/// `(cl-remove-if-not PRED SEQ)` — keep only elements matching predicate.
pub fn builtin_cl_remove_if_not(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("cl-remove-if-not", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut result = Vec::new();
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e.clone()])?;
        if r.is_truthy() {
            result.push(e);
        }
    }
    Ok(Value::list(result))
}

/// `(cl-find-if PRED SEQ)` — find first element matching predicate.
pub fn builtin_cl_find_if(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("cl-find-if", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e.clone()])?;
        if r.is_truthy() {
            return Ok(e);
        }
    }
    Ok(Value::Nil)
}

/// `(cl-count-if PRED SEQ)` — count elements matching predicate.
pub fn builtin_cl_count_if(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("cl-count-if", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut count = 0i64;
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_truthy() {
            count += 1;
        }
    }
    Ok(Value::Int(count))
}

/// `(cl-sort SEQ PRED)` — sort with predicate (not guaranteed stable).
pub fn builtin_cl_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("cl-sort", &args, 2)?;
    let pred = args[1].clone();
    let mut items = collect_sequence(&args[0]);

    // Insertion sort (stable, supports fallible predicates)
    for i in 1..items.len() {
        let mut j = i;
        while j > 0 {
            let result = eval.apply(pred.clone(), vec![items[j].clone(), items[j - 1].clone()])?;
            if result.is_truthy() {
                items.swap(j, j - 1);
                j -= 1;
            } else {
                break;
            }
        }
    }
    Ok(Value::list(items))
}

/// `(cl-stable-sort SEQ PRED)` — stable sort with predicate.
pub fn builtin_cl_stable_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    // Same as cl-sort since our insertion sort is already stable
    builtin_cl_sort(eval, args)
}

// ===========================================================================
// Seq.el pure operations
// ===========================================================================

/// `(seq-reverse SEQ)` — reverse a sequence.
pub fn builtin_seq_reverse(args: Vec<Value>) -> EvalResult {
    expect_args("seq-reverse", &args, 1)?;
    let mut elems = collect_sequence(&args[0]);
    elems.reverse();
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(elems)),
        Value::Str(_) => {
            let s: String = elems
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(elems)),
    }
}

/// `(seq-drop SEQ N)` — drop first n elements.
pub fn builtin_seq_drop(args: Vec<Value>) -> EvalResult {
    expect_args("seq-drop", &args, 2)?;
    let elems = collect_sequence(&args[0]);
    let n = expect_int(&args[1])? as usize;
    let n = n.min(elems.len());
    let result: Vec<Value> = elems[n..].to_vec();
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(result)),
        _ => Ok(Value::list(result)),
    }
}

/// `(seq-take SEQ N)` — take first n elements.
pub fn builtin_seq_take(args: Vec<Value>) -> EvalResult {
    expect_args("seq-take", &args, 2)?;
    let elems = collect_sequence(&args[0]);
    let n = expect_int(&args[1])? as usize;
    let n = n.min(elems.len());
    let result: Vec<Value> = elems[..n].to_vec();
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(result)),
        _ => Ok(Value::list(result)),
    }
}

/// `(seq-subseq SEQ START &optional END)` — subsequence.
pub fn builtin_seq_subseq(args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-subseq", &args, 2)?;
    let elems = collect_sequence(&args[0]);
    let start = expect_int(&args[1])? as usize;
    let end = if args.len() > 2 && !args[2].is_nil() {
        expect_int(&args[2])? as usize
    } else {
        elems.len()
    };
    let start = start.min(elems.len());
    let end = end.min(elems.len());
    if start > end {
        return Ok(Value::Nil);
    }
    let result: Vec<Value> = elems[start..end].to_vec();
    match &args[0] {
        Value::Vector(_) => Ok(Value::vector(result)),
        _ => Ok(Value::list(result)),
    }
}

/// `(seq-concatenate TYPE &rest SEQS)` — concatenate sequences into target type.
pub fn builtin_seq_concatenate(args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-concatenate", &args, 1)?;
    let target = type_name_str(&args[0]);
    let mut combined = Vec::new();
    for arg in &args[1..] {
        combined.extend(collect_sequence(arg));
    }
    match target {
        "vector" => Ok(Value::vector(combined)),
        "string" => {
            let s: String = combined
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    Value::Int(n) => char::from_u32(*n as u32),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(combined)),
    }
}

/// `(seq-empty-p SEQ)` — is sequence empty?
pub fn builtin_seq_empty_p(args: Vec<Value>) -> EvalResult {
    expect_args("seq-empty-p", &args, 1)?;
    let empty = match &args[0] {
        Value::Nil => true,
        Value::Str(s) => s.is_empty(),
        Value::Vector(v) => v.lock().expect("poisoned").is_empty(),
        Value::Cons(_) => false,
        _ => true,
    };
    Ok(Value::bool(empty))
}

/// `(seq-min SEQ)` — minimum element (numeric).
pub fn builtin_seq_min(args: Vec<Value>) -> EvalResult {
    expect_args("seq-min", &args, 1)?;
    let elems = collect_sequence(&args[0]);
    if elems.is_empty() {
        return Ok(Value::Nil);
    }
    let mut min_val = &elems[0];
    for e in &elems[1..] {
        let a = min_val.as_number_f64();
        let b = e.as_number_f64();
        if let (Some(a_f), Some(b_f)) = (a, b) {
            if b_f < a_f {
                min_val = e;
            }
        }
    }
    Ok(min_val.clone())
}

/// `(seq-max SEQ)` — maximum element (numeric).
pub fn builtin_seq_max(args: Vec<Value>) -> EvalResult {
    expect_args("seq-max", &args, 1)?;
    let elems = collect_sequence(&args[0]);
    if elems.is_empty() {
        return Ok(Value::Nil);
    }
    let mut max_val = &elems[0];
    for e in &elems[1..] {
        let a = max_val.as_number_f64();
        let b = e.as_number_f64();
        if let (Some(a_f), Some(b_f)) = (a, b) {
            if b_f > a_f {
                max_val = e;
            }
        }
    }
    Ok(max_val.clone())
}

/// `(seq-into SEQ TYPE)` — convert sequence type.
pub fn builtin_seq_into(args: Vec<Value>) -> EvalResult {
    expect_args("seq-into", &args, 2)?;
    let elems = collect_sequence(&args[0]);
    let target = type_name_str(&args[1]);
    match target {
        "vector" => Ok(Value::vector(elems)),
        "string" => {
            let s: String = elems
                .iter()
                .filter_map(|v| match v {
                    Value::Char(c) => Some(*c),
                    Value::Int(n) => char::from_u32(*n as u32),
                    _ => None,
                })
                .collect();
            Ok(Value::string(s))
        }
        _ => Ok(Value::list(elems)),
    }
}

// ===========================================================================
// Seq.el eval-dependent operations
// ===========================================================================

/// `(seq-position SEQ ELT &optional TESTFN)` — return first matching index.
pub fn builtin_seq_position(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-position", &args, 2)?;
    let seq = &args[0];
    let target = args[1].clone();
    let test_fn = if args.len() > 2 && !args[2].is_nil() {
        Some(args[2].clone())
    } else {
        None
    };
    let elements = seq_position_elements(seq)?;

    for (idx, element) in elements.into_iter().enumerate() {
        let matches = if let Some(test) = &test_fn {
            eval.apply(test.clone(), vec![element.clone(), target.clone()])?
                .is_truthy()
        } else {
            seq_default_match(&element, &target)
        };
        if matches {
            return Ok(Value::Int(idx as i64));
        }
    }
    Ok(Value::Nil)
}

/// `(seq-mapn FN &rest SEQS)` — map over multiple sequences.
pub fn builtin_seq_mapn(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("seq-mapn", &args, 2)?;
    let func = args[0].clone();
    let seqs: Vec<Vec<Value>> = args[1..].iter().map(|s| collect_sequence(s)).collect();
    if seqs.is_empty() {
        return Ok(Value::Nil);
    }
    let min_len = seqs.iter().map(|s| s.len()).min().unwrap_or(0);
    let mut results = Vec::new();
    for i in 0..min_len {
        let call_args: Vec<Value> = seqs.iter().map(|s| s[i].clone()).collect();
        results.push(eval.apply(func.clone(), call_args)?);
    }
    Ok(Value::list(results))
}

/// `(seq-do FN SEQ)` — apply fn for side effects, return nil.
pub fn builtin_seq_do(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-do", &args, 2)?;
    let func = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        eval.apply(func.clone(), vec![e])?;
    }
    Ok(Value::Nil)
}

/// `(seq-count PRED SEQ)` — count elements matching predicate.
pub fn builtin_seq_count(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-count", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut count = 0i64;
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_truthy() {
            count += 1;
        }
    }
    Ok(Value::Int(count))
}

/// `(seq-reduce FN SEQ INITIAL)` — reduce with initial value.
pub fn builtin_seq_reduce(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-reduce", &args, 3)?;
    let func = args[0].clone();
    let elems = collect_sequence(&args[1]);
    let mut acc = args[2].clone();
    for e in elems {
        acc = eval.apply(func.clone(), vec![acc, e])?;
    }
    Ok(acc)
}

/// `(seq-some PRED SEQ)` — some element matches predicate.
pub fn builtin_seq_some(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-some", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_truthy() {
            return Ok(r);
        }
    }
    Ok(Value::Nil)
}

/// `(seq-every-p PRED SEQ)` — all elements match predicate.
pub fn builtin_seq_every_p(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-every-p", &args, 2)?;
    let pred = args[0].clone();
    let elems = collect_sequence(&args[1]);
    for e in elems {
        let r = eval.apply(pred.clone(), vec![e])?;
        if r.is_nil() {
            return Ok(Value::Nil);
        }
    }
    Ok(Value::True)
}

/// `(seq-sort PRED SEQ)` — sort with predicate.
pub fn builtin_seq_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("seq-sort", &args, 2)?;
    let pred = args[0].clone();
    let mut items = collect_sequence(&args[1]);

    // Insertion sort (stable, supports fallible predicates)
    for i in 1..items.len() {
        let mut j = i;
        while j > 0 {
            let result = eval.apply(pred.clone(), vec![items[j].clone(), items[j - 1].clone()])?;
            if result.is_truthy() {
                items.swap(j, j - 1);
                j -= 1;
            } else {
                break;
            }
        }
    }
    Ok(Value::list(items))
}

// ===========================================================================
// JSON operations
// ===========================================================================

/// Parse a JSON string into a Lisp value.
fn json_to_lisp(json: &str) -> EvalResult {
    // Minimal JSON parser without external dependency
    let trimmed = json.trim();
    if trimmed.is_empty() {
        return Err(signal(
            "json-parse-error",
            vec![Value::string("Empty JSON input")],
        ));
    }
    let (val, _rest) = parse_json_value(trimmed)?;
    Ok(val)
}

fn parse_json_value(s: &str) -> Result<(Value, &str), Flow> {
    let s = s.trim_start();
    if s.is_empty() {
        return Err(signal(
            "json-parse-error",
            vec![Value::string("Unexpected end of JSON")],
        ));
    }
    match s.as_bytes()[0] {
        b'"' => parse_json_string(s),
        b'{' => parse_json_object(s),
        b'[' => parse_json_array(s),
        b't' => {
            if s.starts_with("true") {
                Ok((Value::True, &s[4..]))
            } else {
                Err(signal(
                    "json-parse-error",
                    vec![Value::string("Invalid JSON token")],
                ))
            }
        }
        b'f' => {
            if s.starts_with("false") {
                Ok((Value::Nil, &s[5..]))
            } else {
                Err(signal(
                    "json-parse-error",
                    vec![Value::string("Invalid JSON token")],
                ))
            }
        }
        b'n' => {
            if s.starts_with("null") {
                Ok((Value::Keyword("null".to_string()), &s[4..]))
            } else {
                Err(signal(
                    "json-parse-error",
                    vec![Value::string("Invalid JSON token")],
                ))
            }
        }
        b'-' | b'0'..=b'9' => parse_json_number(s),
        _ => Err(signal(
            "json-parse-error",
            vec![Value::string(format!(
                "Unexpected character: {}",
                s.chars().next().unwrap_or('?')
            ))],
        )),
    }
}

fn parse_json_string(s: &str) -> Result<(Value, &str), Flow> {
    debug_assert!(s.starts_with('"'));
    let s = &s[1..]; // skip opening quote
    let mut result = String::new();
    let mut chars = s.char_indices();
    loop {
        match chars.next() {
            None => {
                return Err(signal(
                    "json-parse-error",
                    vec![Value::string("Unterminated string")],
                ));
            }
            Some((_, '"')) => {
                let rest_offset = chars.as_str();
                return Ok((Value::string(result), rest_offset));
            }
            Some((_, '\\')) => match chars.next() {
                Some((_, '"')) => result.push('"'),
                Some((_, '\\')) => result.push('\\'),
                Some((_, '/')) => result.push('/'),
                Some((_, 'b')) => result.push('\u{0008}'),
                Some((_, 'f')) => result.push('\u{000C}'),
                Some((_, 'n')) => result.push('\n'),
                Some((_, 'r')) => result.push('\r'),
                Some((_, 't')) => result.push('\t'),
                Some((_, 'u')) => {
                    let mut hex = String::new();
                    for _ in 0..4 {
                        match chars.next() {
                            Some((_, c)) => hex.push(c),
                            None => {
                                return Err(signal(
                                    "json-parse-error",
                                    vec![Value::string("Unterminated unicode escape")],
                                ));
                            }
                        }
                    }
                    let code = u32::from_str_radix(&hex, 16).map_err(|_| {
                        signal(
                            "json-parse-error",
                            vec![Value::string("Invalid unicode escape")],
                        )
                    })?;
                    if let Some(c) = char::from_u32(code) {
                        result.push(c);
                    } else {
                        result.push('\u{FFFD}');
                    }
                }
                _ => {
                    return Err(signal(
                        "json-parse-error",
                        vec![Value::string("Invalid escape sequence")],
                    ));
                }
            },
            Some((_, c)) => result.push(c),
        }
    }
}

fn parse_json_number(s: &str) -> Result<(Value, &str), Flow> {
    let mut end = 0;
    let bytes = s.as_bytes();
    let mut is_float = false;

    if end < bytes.len() && bytes[end] == b'-' {
        end += 1;
    }
    while end < bytes.len() && bytes[end].is_ascii_digit() {
        end += 1;
    }
    if end < bytes.len() && bytes[end] == b'.' {
        is_float = true;
        end += 1;
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }
    if end < bytes.len() && (bytes[end] == b'e' || bytes[end] == b'E') {
        is_float = true;
        end += 1;
        if end < bytes.len() && (bytes[end] == b'+' || bytes[end] == b'-') {
            end += 1;
        }
        while end < bytes.len() && bytes[end].is_ascii_digit() {
            end += 1;
        }
    }

    let num_str = &s[..end];
    let rest = &s[end..];

    if is_float {
        let f: f64 = num_str
            .parse()
            .map_err(|_| signal("json-parse-error", vec![Value::string("Invalid number")]))?;
        Ok((Value::Float(f), rest))
    } else {
        match num_str.parse::<i64>() {
            Ok(n) => Ok((Value::Int(n), rest)),
            Err(_) => {
                // Fall back to float for very large integers
                let f: f64 = num_str.parse().map_err(|_| {
                    signal("json-parse-error", vec![Value::string("Invalid number")])
                })?;
                Ok((Value::Float(f), rest))
            }
        }
    }
}

fn parse_json_array(s: &str) -> Result<(Value, &str), Flow> {
    debug_assert!(s.starts_with('['));
    let mut rest = s[1..].trim_start();
    let mut items = Vec::new();

    if rest.starts_with(']') {
        return Ok((Value::vector(items), &rest[1..]));
    }

    loop {
        let (val, r) = parse_json_value(rest)?;
        items.push(val);
        rest = r.trim_start();
        if rest.starts_with(',') {
            rest = rest[1..].trim_start();
        } else if rest.starts_with(']') {
            rest = &rest[1..];
            break;
        } else {
            return Err(signal(
                "json-parse-error",
                vec![Value::string("Expected ',' or ']' in array")],
            ));
        }
    }
    Ok((Value::vector(items), rest))
}

fn parse_json_object(s: &str) -> Result<(Value, &str), Flow> {
    debug_assert!(s.starts_with('{'));
    let mut rest = s[1..].trim_start();
    let ht = LispHashTable::new(HashTableTest::Equal);
    let table = Value::HashTable(Arc::new(Mutex::new(ht)));

    if rest.starts_with('}') {
        return Ok((table, &rest[1..]));
    }

    loop {
        // Parse key (must be string)
        let (key_val, r) = parse_json_string(rest.trim_start())?;
        let key_str = match &key_val {
            Value::Str(s) => (**s).clone(),
            _ => {
                return Err(signal(
                    "json-parse-error",
                    vec![Value::string("Object key must be a string")],
                ));
            }
        };
        rest = r.trim_start();

        // Expect colon
        if !rest.starts_with(':') {
            return Err(signal(
                "json-parse-error",
                vec![Value::string("Expected ':' in object")],
            ));
        }
        rest = rest[1..].trim_start();

        // Parse value
        let (val, r) = parse_json_value(rest)?;
        rest = r.trim_start();

        // Insert into hash table
        if let Value::HashTable(ref ht_arc) = table {
            let mut ht = ht_arc.lock().expect("poisoned");
            let hk = HashKey::Str(key_str);
            ht.data.insert(hk, val);
        }

        if rest.starts_with(',') {
            rest = rest[1..].trim_start();
        } else if rest.starts_with('}') {
            rest = &rest[1..];
            break;
        } else {
            return Err(signal(
                "json-parse-error",
                vec![Value::string("Expected ',' or '}' in object")],
            ));
        }
    }
    Ok((table, rest))
}

/// Serialize a Lisp value to a JSON string.
fn lisp_to_json(val: &Value) -> EvalResult {
    let s = lisp_to_json_str(val)?;
    Ok(Value::string(s))
}

fn lisp_to_json_str(val: &Value) -> Result<String, Flow> {
    match val {
        Value::Nil => Ok("false".to_string()),
        Value::True => Ok("true".to_string()),
        Value::Int(n) => Ok(n.to_string()),
        Value::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                return Err(signal(
                    "json-serialize-error",
                    vec![Value::string(
                        "NaN and Infinity cannot be serialized to JSON",
                    )],
                ));
            }
            Ok(format!("{}", f))
        }
        Value::Str(s) => Ok(json_escape_string(s)),
        Value::Symbol(s) if s == "null" => Ok("null".to_string()),
        Value::Keyword(k) if k == "null" => Ok("null".to_string()),
        Value::Keyword(k) if k == "false" => Ok("false".to_string()),
        Value::Symbol(_) | Value::Keyword(_) => {
            // Serialize symbols as strings
            let name = val.as_symbol_name().unwrap_or("nil");
            Ok(json_escape_string(name))
        }
        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            let mut parts = Vec::new();
            for item in items.iter() {
                parts.push(lisp_to_json_str(item)?);
            }
            Ok(format!("[{}]", parts.join(",")))
        }
        Value::Cons(_) => {
            // Check if it's an alist (list of conses)
            if let Some(items) = list_to_vec(val) {
                // Check if it looks like an alist
                let is_alist = items.iter().all(|item| matches!(item, Value::Cons(_)));
                if is_alist && !items.is_empty() {
                    // Serialize as JSON object
                    let mut parts = Vec::new();
                    for item in &items {
                        if let Value::Cons(cell) = item {
                            let pair = cell.lock().expect("poisoned");
                            let key = match &pair.car {
                                Value::Str(s) => json_escape_string(s),
                                Value::Symbol(s) => json_escape_string(s),
                                Value::Keyword(k) => json_escape_string(k),
                                other => json_escape_string(&format!("{}", other)),
                            };
                            let val_str = lisp_to_json_str(&pair.cdr)?;
                            parts.push(format!("{}:{}", key, val_str));
                        }
                    }
                    return Ok(format!("{{{}}}", parts.join(",")));
                }
                // Otherwise serialize as JSON array
                let mut parts = Vec::new();
                for item in &items {
                    parts.push(lisp_to_json_str(item)?);
                }
                Ok(format!("[{}]", parts.join(",")))
            } else {
                Err(signal(
                    "json-serialize-error",
                    vec![Value::string("Cannot serialize improper list to JSON")],
                ))
            }
        }
        Value::HashTable(ht_arc) => {
            let ht = ht_arc.lock().expect("poisoned");
            let mut parts = Vec::new();
            for (key, value) in &ht.data {
                let key_str = match key {
                    HashKey::Str(s) => json_escape_string(s),
                    HashKey::Symbol(s) => json_escape_string(s),
                    HashKey::Keyword(k) => json_escape_string(k),
                    HashKey::Int(n) => json_escape_string(&n.to_string()),
                    _ => json_escape_string(""),
                };
                let val_str = lisp_to_json_str(value)?;
                parts.push(format!("{}:{}", key_str, val_str));
            }
            Ok(format!("{{{}}}", parts.join(",")))
        }
        Value::Char(c) => Ok(json_escape_string(&c.to_string())),
        _ => Err(signal(
            "json-serialize-error",
            vec![Value::string(format!(
                "Cannot serialize {} to JSON",
                val.type_name()
            ))],
        )),
    }
}

fn json_escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000C}' => out.push_str("\\f"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// `(json-parse-string STRING &rest ARGS)` — parse JSON string to Lisp.
pub fn builtin_json_parse_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("json-parse-string", &args, 1)?;
    let s = expect_string(&args[0])?;
    json_to_lisp(&s)
}

/// `(json-serialize OBJECT &rest ARGS)` — serialize Lisp object to JSON string.
pub fn builtin_json_serialize(args: Vec<Value>) -> EvalResult {
    expect_min_args("json-serialize", &args, 1)?;
    lisp_to_json(&args[0])
}

/// `(json-parse-buffer &rest ARGS)` — parse JSON from current buffer.
pub fn builtin_json_parse_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let _ = args;
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    let text = buf.buffer_string();
    json_to_lisp(&text)
}

/// `(json-insert OBJECT &rest ARGS)` — insert JSON into current buffer.
pub fn builtin_json_insert(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("json-insert", &args, 1)?;
    let json_str = lisp_to_json(&args[0])?;
    let s = match &json_str {
        Value::Str(s) => (**s).clone(),
        _ => return Ok(Value::Nil),
    };
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.insert(&s);
    Ok(Value::Nil)
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // --- CL-lib pure operations ---

    #[test]
    fn cl_find_in_list() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_find(vec![Value::Int(2), list]).unwrap();
        assert_eq!(result.as_int(), Some(2));
    }

    #[test]
    fn cl_find_not_found() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let result = builtin_cl_find(vec![Value::Int(99), list]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_position_found() {
        let list = Value::list(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);
        let result = builtin_cl_position(vec![Value::Int(20), list]).unwrap();
        assert_eq!(result.as_int(), Some(1));
    }

    #[test]
    fn cl_position_not_found() {
        let list = Value::list(vec![Value::Int(10)]);
        let result = builtin_cl_position(vec![Value::Int(99), list]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn cl_count_occurrences() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(1),
            Value::Int(1),
        ]);
        let result = builtin_cl_count(vec![Value::Int(1), list]).unwrap();
        assert_eq!(result.as_int(), Some(3));
    }

    #[test]
    fn cl_remove_items() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(2),
        ]);
        let result = builtin_cl_remove(vec![Value::Int(2), list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_int(), Some(1));
        assert_eq!(items[1].as_int(), Some(3));
    }

    #[test]
    fn cl_substitute_items() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_substitute(vec![Value::Int(99), Value::Int(2), list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items[1].as_int(), Some(99));
    }

    #[test]
    fn cl_intersection_test() {
        let l1 = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let l2 = Value::list(vec![Value::Int(2), Value::Int(3), Value::Int(4)]);
        let result = builtin_cl_intersection(vec![l1, l2]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn cl_union_test() {
        let l1 = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let l2 = Value::list(vec![Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_union(vec![l1, l2]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn cl_set_difference_test() {
        let l1 = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let l2 = Value::list(vec![Value::Int(2), Value::Int(4)]);
        let result = builtin_cl_set_difference(vec![l1, l2]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_int(), Some(1));
        assert_eq!(items[1].as_int(), Some(3));
    }

    #[test]
    fn cl_subsetp_test() {
        let l1 = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let l2 = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert!(matches!(
            builtin_cl_subsetp(vec![l1.clone(), l2.clone()]).unwrap(),
            Value::True
        ));
        assert!(builtin_cl_subsetp(vec![l2, l1]).unwrap().is_nil());
    }

    #[test]
    fn cl_adjoin_already_present() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let result = builtin_cl_adjoin(vec![Value::Int(1), list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn cl_adjoin_new_item() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let result = builtin_cl_adjoin(vec![Value::Int(3), list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn cl_remove_duplicates_test() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(1),
            Value::Int(3),
            Value::Int(2),
        ]);
        let result = builtin_cl_remove_duplicates(vec![list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 3);
    }

    #[test]
    fn cl_first_through_third() {
        let list = Value::list(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);
        assert_eq!(
            builtin_cl_first(vec![list.clone()]).unwrap().as_int(),
            Some(10)
        );
        assert_eq!(
            builtin_cl_second(vec![list.clone()]).unwrap().as_int(),
            Some(20)
        );
        assert_eq!(
            builtin_cl_third(vec![list.clone()]).unwrap().as_int(),
            Some(30)
        );
        assert!(builtin_cl_fourth(vec![list]).unwrap().is_nil());
    }

    #[test]
    fn cl_rest_test() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_rest(vec![list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_int(), Some(2));
    }

    #[test]
    fn cl_subseq_test() {
        let list = Value::list(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
            Value::Int(4),
        ]);
        let result = builtin_cl_subseq(vec![list, Value::Int(1), Value::Int(3)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_int(), Some(2));
        assert_eq!(items[1].as_int(), Some(3));
    }

    #[test]
    fn cl_concatenate_lists() {
        let l1 = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let l2 = Value::list(vec![Value::Int(3), Value::Int(4)]);
        let result = builtin_cl_concatenate(vec![Value::symbol("list"), l1, l2]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 4);
    }

    #[test]
    fn cl_coerce_to_vector() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_coerce(vec![list, Value::symbol("vector")]).unwrap();
        assert!(result.is_vector());
    }

    #[test]
    fn cl_coerce_to_float() {
        let result = builtin_cl_coerce(vec![Value::Int(42), Value::symbol("float")]).unwrap();
        assert!(result.is_float());
    }

    #[test]
    fn cl_member_found() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_member(vec![Value::Int(2), list]).unwrap();
        assert!(result.is_cons());
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].as_int(), Some(2));
    }

    #[test]
    fn cl_member_not_found() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let result = builtin_cl_member(vec![Value::Int(99), list]).unwrap();
        assert!(result.is_nil());
    }

    // --- Seq.el pure operations ---

    #[test]
    fn seq_reverse_list() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_seq_reverse(vec![list]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items[0].as_int(), Some(3));
        assert_eq!(items[2].as_int(), Some(1));
    }

    #[test]
    fn seq_reverse_string() {
        let s = Value::string("abc");
        let result = builtin_seq_reverse(vec![s]).unwrap();
        assert_eq!(result.as_str(), Some("cba"));
    }

    #[test]
    fn seq_drop_test() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_seq_drop(vec![list, Value::Int(2)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].as_int(), Some(3));
    }

    #[test]
    fn seq_take_test() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_seq_take(vec![list, Value::Int(2)]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn seq_subseq_test() {
        let vec = Value::vector(vec![
            Value::Int(10),
            Value::Int(20),
            Value::Int(30),
            Value::Int(40),
        ]);
        let result = builtin_seq_subseq(vec![vec, Value::Int(1), Value::Int(3)]).unwrap();
        if let Value::Vector(v) = result {
            let v = v.lock().unwrap();
            assert_eq!(v.len(), 2);
            assert_eq!(v[0].as_int(), Some(20));
            assert_eq!(v[1].as_int(), Some(30));
        } else {
            panic!("expected vector");
        }
    }

    #[test]
    fn seq_concatenate_test() {
        let l1 = Value::list(vec![Value::Int(1)]);
        let l2 = Value::list(vec![Value::Int(2)]);
        let result = builtin_seq_concatenate(vec![Value::symbol("list"), l1, l2]).unwrap();
        let items = list_to_vec(&result).unwrap();
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn seq_empty_p_test() {
        assert!(matches!(
            builtin_seq_empty_p(vec![Value::Nil]).unwrap(),
            Value::True
        ));
        assert!(matches!(
            builtin_seq_empty_p(vec![Value::string("")]).unwrap(),
            Value::True
        ));
        assert!(builtin_seq_empty_p(vec![Value::list(vec![Value::Int(1)])])
            .unwrap()
            .is_nil());
    }

    #[test]
    fn seq_min_max_test() {
        let list = Value::list(vec![Value::Int(3), Value::Int(1), Value::Int(2)]);
        assert_eq!(
            builtin_seq_min(vec![list.clone()]).unwrap().as_int(),
            Some(1)
        );
        assert_eq!(builtin_seq_max(vec![list]).unwrap().as_int(), Some(3));
    }

    #[test]
    fn seq_into_vector() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let result = builtin_seq_into(vec![list, Value::symbol("vector")]).unwrap();
        assert!(result.is_vector());
    }

    // --- JSON operations ---

    #[test]
    fn json_parse_number() {
        let result = builtin_json_parse_string(vec![Value::string("42")]).unwrap();
        assert_eq!(result.as_int(), Some(42));
    }

    #[test]
    fn json_parse_float() {
        let result = builtin_json_parse_string(vec![Value::string("3.14")]).unwrap();
        assert!(result.is_float());
    }

    #[test]
    fn json_parse_string() {
        let result = builtin_json_parse_string(vec![Value::string("\"hello\"")]).unwrap();
        assert_eq!(result.as_str(), Some("hello"));
    }

    #[test]
    fn json_parse_bool_true() {
        let result = builtin_json_parse_string(vec![Value::string("true")]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn json_parse_bool_false() {
        let result = builtin_json_parse_string(vec![Value::string("false")]).unwrap();
        assert!(result.is_nil());
    }

    #[test]
    fn json_parse_null() {
        let result = builtin_json_parse_string(vec![Value::string("null")]).unwrap();
        assert!(matches!(result, Value::Keyword(ref k) if k == "null"));
    }

    #[test]
    fn json_parse_array() {
        let result = builtin_json_parse_string(vec![Value::string("[1,2,3]")]).unwrap();
        if let Value::Vector(v) = result {
            let v = v.lock().unwrap();
            assert_eq!(v.len(), 3);
            assert_eq!(v[0].as_int(), Some(1));
        } else {
            panic!("expected vector");
        }
    }

    #[test]
    fn json_parse_object() {
        let result =
            builtin_json_parse_string(vec![Value::string("{\"name\":\"test\",\"value\":42}")])
                .unwrap();
        assert!(result.is_hash_table());
        if let Value::HashTable(ht_arc) = &result {
            let ht = ht_arc.lock().unwrap();
            assert_eq!(ht.data.len(), 2);
            let name_key = HashKey::Str("name".to_string());
            let name_val = ht.data.get(&name_key).unwrap();
            assert_eq!(name_val.as_str(), Some("test"));
        }
    }

    #[test]
    fn json_parse_nested() {
        let result =
            builtin_json_parse_string(vec![Value::string("{\"arr\":[1,{\"nested\":true}]}")])
                .unwrap();
        assert!(result.is_hash_table());
    }

    #[test]
    fn json_serialize_int() {
        let result = builtin_json_serialize(vec![Value::Int(42)]).unwrap();
        assert_eq!(result.as_str(), Some("42"));
    }

    #[test]
    fn json_serialize_string() {
        let result = builtin_json_serialize(vec![Value::string("hello")]).unwrap();
        assert_eq!(result.as_str(), Some("\"hello\""));
    }

    #[test]
    fn json_serialize_true() {
        let result = builtin_json_serialize(vec![Value::True]).unwrap();
        assert_eq!(result.as_str(), Some("true"));
    }

    #[test]
    fn json_serialize_nil() {
        let result = builtin_json_serialize(vec![Value::Nil]).unwrap();
        assert_eq!(result.as_str(), Some("false"));
    }

    #[test]
    fn json_serialize_vector() {
        let v = Value::vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_json_serialize(vec![v]).unwrap();
        assert_eq!(result.as_str(), Some("[1,2,3]"));
    }

    #[test]
    fn json_serialize_hash_table() {
        let ht = Value::hash_table(HashTableTest::Equal);
        if let Value::HashTable(ref ht_arc) = ht {
            let mut h = ht_arc.lock().unwrap();
            h.data
                .insert(HashKey::Str("key".to_string()), Value::Int(42));
        }
        let result = builtin_json_serialize(vec![ht]).unwrap();
        let s = result.as_str().unwrap();
        assert!(s.contains("\"key\""));
        assert!(s.contains("42"));
    }

    #[test]
    fn json_parse_string_with_escapes() {
        let result =
            builtin_json_parse_string(vec![Value::string("\"hello\\nworld\\t!\"")]).unwrap();
        assert_eq!(result.as_str(), Some("hello\nworld\t!"));
    }

    #[test]
    fn json_parse_empty_array() {
        let result = builtin_json_parse_string(vec![Value::string("[]")]).unwrap();
        if let Value::Vector(v) = result {
            assert!(v.lock().unwrap().is_empty());
        } else {
            panic!("expected vector");
        }
    }

    #[test]
    fn json_parse_empty_object() {
        let result = builtin_json_parse_string(vec![Value::string("{}")]).unwrap();
        assert!(result.is_hash_table());
        if let Value::HashTable(ht_arc) = &result {
            assert!(ht_arc.lock().unwrap().data.is_empty());
        }
    }

    #[test]
    fn json_roundtrip_vector() {
        let v = Value::vector(vec![
            Value::Int(1),
            Value::string("two"),
            Value::True,
            Value::Nil,
        ]);
        let serialized = builtin_json_serialize(vec![v]).unwrap();
        let parsed = builtin_json_parse_string(vec![serialized]).unwrap();
        if let Value::Vector(arr) = parsed {
            let arr = arr.lock().unwrap();
            assert_eq!(arr.len(), 4);
            assert_eq!(arr[0].as_int(), Some(1));
            assert_eq!(arr[1].as_str(), Some("two"));
            assert!(matches!(arr[2], Value::True));
            assert!(arr[3].is_nil());
        } else {
            panic!("expected vector");
        }
    }

    #[test]
    fn json_parse_negative_number() {
        let result = builtin_json_parse_string(vec![Value::string("-42")]).unwrap();
        assert_eq!(result.as_int(), Some(-42));
    }

    #[test]
    fn json_parse_scientific_notation() {
        let result = builtin_json_parse_string(vec![Value::string("1.5e2")]).unwrap();
        if let Value::Float(f) = result {
            assert!((f - 150.0).abs() < 0.001);
        } else {
            panic!("expected float");
        }
    }

    #[test]
    fn json_serialize_escape_chars() {
        let result = builtin_json_serialize(vec![Value::string("line1\nline2\ttab")]).unwrap();
        let s = result.as_str().unwrap();
        assert!(s.contains("\\n"));
        assert!(s.contains("\\t"));
    }

    // --- Eval-dependent tests (using Evaluator) ---

    #[test]
    fn cl_every_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        // Test cl-every with numberp
        evaluator
            .eval_forms(&super::super::parser::parse_forms("(defun my-pos (x) (> x 0))").unwrap());
        let func = Value::Subr("numberp".to_string());
        let seq = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_every(&mut evaluator, vec![func, seq]).unwrap();
        assert!(matches!(result, Value::True));
    }

    #[test]
    fn cl_some_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr("null".to_string());
        let seq = Value::list(vec![Value::Int(1), Value::Nil, Value::Int(3)]);
        let result = builtin_cl_some(&mut evaluator, vec![func, seq]).unwrap();
        assert!(result.is_truthy());
    }

    #[test]
    fn cl_reduce_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr("+".to_string());
        let seq = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = builtin_cl_reduce(&mut evaluator, vec![func, seq, Value::Int(0)]).unwrap();
        assert_eq!(result.as_int(), Some(6));
    }

    #[test]
    fn seq_reduce_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr("+".to_string());
        let seq = Value::list(vec![Value::Int(10), Value::Int(20)]);
        let result = builtin_seq_reduce(&mut evaluator, vec![func, seq, Value::Int(0)]).unwrap();
        assert_eq!(result.as_int(), Some(30));
    }

    #[test]
    fn seq_count_with_eval() {
        let mut evaluator = super::super::eval::Evaluator::new();
        let func = Value::Subr("numberp".to_string());
        let seq = Value::list(vec![Value::Int(1), Value::string("a"), Value::Int(2)]);
        let result = builtin_seq_count(&mut evaluator, vec![func, seq]).unwrap();
        assert_eq!(result.as_int(), Some(2));
    }
}
