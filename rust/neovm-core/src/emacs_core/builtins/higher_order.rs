use super::*;

pub(crate) fn builtin_apply(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() < 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("apply"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0];
    let last = &args[args.len() - 1];
    let mut call_args: Vec<Value> = args[1..args.len() - 1].to_vec();

    // Last argument must be a list, which gets spread
    match last {
        Value::Nil => {}
        Value::Cons(_) => {
            let mut cursor = *last;
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let pair = read_cons(cell);
                        call_args.push(pair.car);
                        cursor = pair.cdr;
                    }
                    other => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), other],
                        ))
                    }
                }
            }
        }
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), *last],
            ))
        }
    }

    eval.apply(func, call_args)
}

pub(crate) fn builtin_funcall(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_min_args("funcall", &args, 1)?;
    let func = args[0];
    let call_args = args[1..].to_vec();
    eval.apply(func, call_args)
}

pub(crate) fn builtin_funcall_interactively(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("funcall-interactively", &args, 1)?;
    let func = args[0];
    let call_args = args[1..].to_vec();
    eval.apply(func, call_args)
}

pub(crate) fn builtin_funcall_with_delayed_message(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("funcall-with-delayed-message", &args, 3)?;
    let _delay = expect_number(&args[0])?;
    let _message = expect_string(&args[1])?;
    eval.apply(args[2], vec![])
}

// ===========================================================================
// Higher-order
// ===========================================================================

fn for_each_sequence_element<F>(seq: &Value, mut f: F) -> Result<(), Flow>
where
    F: FnMut(Value) -> Result<(), Flow>,
{
    match seq {
        Value::Nil => Ok(()),
        Value::Cons(_) => {
            let mut cursor = *seq;
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let pair = read_cons(cell);
                        let item = pair.car;
                        cursor = pair.cdr;
                        drop(pair);
                        f(item)?;
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ))
                    }
                }
            }
            Ok(())
        }
        Value::Vector(v) | Value::Record(v) => {
            for item in with_heap(|h| h.get_vector(*v).clone()).into_iter() {
                f(item)?;
            }
            Ok(())
        }
        Value::Str(id) => {
            let s = with_heap(|h| h.get_string(*id).clone());
            for cp in decode_storage_char_codes(&s) {
                f(Value::Int(cp as i64))?;
            }
            Ok(())
        }
        _ => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("sequencep"), *seq],
        )),
    }
}

pub(crate) fn builtin_mapcar(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapcar"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0];
    let saved = eval.save_temp_roots();
    eval.push_temp_root(func);
    eval.push_temp_root(args[1]);
    let mut results = Vec::new();
    let map_result = for_each_sequence_element(&args[1], |item| {
        let val = eval.apply(func, vec![item])?;
        eval.push_temp_root(val);
        results.push(val);
        Ok(())
    });
    eval.restore_temp_roots(saved);
    map_result?;
    Ok(Value::list(results))
}

pub(crate) fn builtin_mapc(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapc"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0];
    let seq = args[1];
    let saved = eval.save_temp_roots();
    eval.push_temp_root(func);
    eval.push_temp_root(seq);
    let result = for_each_sequence_element(&seq, |item| {
        eval.apply(func, vec![item])?;
        Ok(())
    });
    eval.restore_temp_roots(saved);
    result?;
    Ok(seq)
}

pub(crate) fn builtin_mapconcat(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_range_args("mapconcat", &args, 2, 3)?;
    let func = args[0];
    let sequence = args[1];
    // Emacs 30: separator is optional, defaults to ""
    let separator = args.get(2).copied().unwrap_or_else(|| Value::string(""));

    let saved = eval.save_temp_roots();
    eval.push_temp_root(func);
    eval.push_temp_root(sequence);
    eval.push_temp_root(separator);
    let mut parts = Vec::new();
    let map_result = for_each_sequence_element(&sequence, |item| {
        let val = eval.apply(func, vec![item])?;
        eval.push_temp_root(val);
        parts.push(val);
        Ok(())
    });
    eval.restore_temp_roots(saved);
    map_result?;

    if parts.is_empty() {
        return Ok(Value::string(""));
    }

    let mut concat_args = Vec::with_capacity(parts.len() * 2 - 1);
    for (index, part) in parts.into_iter().enumerate() {
        if index > 0 {
            concat_args.push(separator);
        }
        concat_args.push(part);
    }
    builtin_concat(concat_args)
}

pub(crate) fn builtin_mapcan(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.len() != 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("mapcan"), Value::Int(args.len() as i64)],
        ));
    }
    let func = args[0];
    let sequence = args[1];
    let saved = eval.save_temp_roots();
    eval.push_temp_root(func);
    eval.push_temp_root(sequence);
    let mut mapped = Vec::new();
    let map_result = for_each_sequence_element(&sequence, |item| {
        let val = eval.apply(func, vec![item])?;
        eval.push_temp_root(val);
        mapped.push(val);
        Ok(())
    });
    eval.restore_temp_roots(saved);
    map_result?;
    builtin_nconc(mapped)
}

pub(crate) fn builtin_sort(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    if args.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("sort"), Value::Int(0)],
        ));
    }

    // Emacs 30 sort: (sort SEQ &key :key :lessp :reverse :in-place)
    // Old form: (sort SEQ PRED) — still supported, always in-place.
    let mut key_fn = Value::Nil;
    let mut lessp_fn = Value::Nil;
    let mut reverse = false;
    let mut _in_place = false;

    if args.len() == 2 && !args[1].is_keyword() {
        // Old-style (sort SEQ PRED) — predicate is the comparison function
        lessp_fn = args[1];
        _in_place = true;
    } else if args.len() > 1 {
        // Keyword argument form
        let mut i = 1;
        while i < args.len() {
            if let Some(kw) = args[i].as_symbol_name() {
                match kw {
                    ":key" => {
                        i += 1;
                        if i < args.len() { key_fn = args[i]; }
                    }
                    ":lessp" => {
                        i += 1;
                        if i < args.len() { lessp_fn = args[i]; }
                    }
                    ":reverse" => {
                        i += 1;
                        if i < args.len() { reverse = args[i].is_truthy(); }
                    }
                    ":in-place" => {
                        i += 1;
                        if i < args.len() { _in_place = args[i].is_truthy(); }
                    }
                    _ => {}
                }
            }
            i += 1;
        }
    }

    // Default predicate: value< (numbers use <, strings use string<)
    let use_default_lessp = lessp_fn.is_nil();

    // Helper: compare two values using key_fn and pred/default
    let compare = |eval: &mut super::eval::Evaluator, a: Value, b: Value| -> Result<bool, Flow> {
        let ka = if key_fn.is_nil() { a } else { eval.apply(key_fn, vec![a])? };
        let kb = if key_fn.is_nil() { b } else { eval.apply(key_fn, vec![b])? };
        let result = if use_default_lessp {
            // Default value<: numbers use <, strings use string<
            match (&ka, &kb) {
                (Value::Int(a), Value::Int(b)) => *a < *b,
                (Value::Float(a), Value::Float(b)) => *a < *b,
                (Value::Int(a), Value::Float(b)) => (*a as f64) < *b,
                (Value::Float(a), Value::Int(b)) => *a < (*b as f64),
                (Value::Str(a), Value::Str(b)) => {
                    let sa = with_heap(|h| h.get_string(*a).clone());
                    let sb = with_heap(|h| h.get_string(*b).clone());
                    sa < sb
                }
                (Value::Symbol(a), Value::Symbol(b)) => {
                    resolve_sym(*a) < resolve_sym(*b)
                }
                _ => false,
            }
        } else {
            eval.apply(lessp_fn, vec![ka, kb])?.is_truthy()
        };
        Ok(if reverse { !result } else { result })
    };

    match &args[0] {
        Value::Nil => Ok(Value::Nil),
        Value::Cons(_) => {
            let mut cons_cells = Vec::new();
            let mut values = Vec::new();
            let mut cursor = args[0];
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        values.push(with_heap(|h| h.cons_car(cell)));
                        cons_cells.push(cell);
                        cursor = with_heap(|h| h.cons_cdr(cell));
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ))
                    }
                }
            }

            let saved = eval.save_temp_roots();
            eval.push_temp_root(lessp_fn);
            eval.push_temp_root(key_fn);
            eval.push_temp_root(args[0]);
            for v in &values {
                eval.push_temp_root(*v);
            }

            // Stable insertion sort
            for i in 1..values.len() {
                let mut j = i;
                while j > 0 {
                    match compare(eval, values[j], values[j - 1]) {
                        Ok(true) => {
                            values.swap(j, j - 1);
                            j -= 1;
                        }
                        Ok(false) => break,
                        Err(err) => {
                            eval.restore_temp_roots(saved);
                            return Err(err);
                        }
                    }
                }
            }

            eval.restore_temp_roots(saved);
            for (cell, value) in cons_cells.iter().zip(values.into_iter()) {
                with_heap_mut(|h| h.set_car(*cell, value));
            }
            Ok(args[0])
        }
        Value::Vector(v) | Value::Record(v) => {
            let mut values = with_heap(|h| h.get_vector(*v).clone());

            let saved = eval.save_temp_roots();
            eval.push_temp_root(lessp_fn);
            eval.push_temp_root(key_fn);
            eval.push_temp_root(args[0]);
            for val in &values {
                eval.push_temp_root(*val);
            }

            for i in 1..values.len() {
                let mut j = i;
                while j > 0 {
                    match compare(eval, values[j], values[j - 1]) {
                        Ok(true) => {
                            values.swap(j, j - 1);
                            j -= 1;
                        }
                        Ok(false) => break,
                        Err(err) => {
                            eval.restore_temp_roots(saved);
                            return Err(err);
                        }
                    }
                }
            }
            eval.restore_temp_roots(saved);
            with_heap_mut(|h| *h.get_vector_mut(*v) = values);
            Ok(args[0])
        }
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("list-or-vector-p"), *other],
        )),
    }
}
