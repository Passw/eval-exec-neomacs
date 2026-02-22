//! Emacs `pcase` pattern matching system.
//!
//! Implements `pcase`, `pcase-let`, `pcase-let*`, and `pcase-dolist` as
//! special forms.  Patterns are compiled from `Expr` AST nodes into an
//! internal `Pattern` enum, then matched against runtime `Value`s.  A
//! successful match produces a set of bindings (symbol name -> value) that
//! are installed into the evaluator's environment before executing the
//! clause body.

use std::collections::HashMap;

use super::error::{signal, EvalResult, Flow};
use super::eval::{quote_to_value, Evaluator};
use super::expr::{print_expr, Expr};
use super::value::*;

// ---------------------------------------------------------------------------
// Pattern representation
// ---------------------------------------------------------------------------

/// A compiled pcase pattern.
#[derive(Clone, Debug)]
enum Pattern {
    /// `_` — matches anything, binds nothing.
    Wildcard,
    /// A symbol name — matches anything and binds the value to that name.
    Bind(String),
    /// A literal value — matches by `equal`.
    Literal(Value),
    /// `(pred FUNC)` — apply FUNC to value; match if result is truthy.
    Pred(Expr),
    /// `(guard EXPR)` — evaluate EXPR in current env; match if truthy.
    Guard(Expr),
    /// `(let PATTERN EXPR)` — evaluate EXPR, then match result against PATTERN.
    Let(Box<Pattern>, Expr),
    /// `(and PAT...)` — all sub-patterns must match.
    And(Vec<Pattern>),
    /// `(or PAT...)` — first matching sub-pattern wins (bindings from that branch).
    Or(Vec<Pattern>),
    /// `(app FUN PAT)` — apply FUN to value, then match result against PAT.
    App(Expr, Box<Pattern>),
    /// Back-quote list pattern — matches a list structurally.  Each element
    /// is either `Unquote(pat)` (match element against pat) or
    /// `Exact(pat)` (literal / recursive quasi-quote).
    BackquoteList(Vec<BqElement>),
    /// Back-quote dotted list — like `BackquoteList` but with a tail pattern
    /// for the cdr of the last cons.
    BackquoteDotted(Vec<BqElement>, Box<Pattern>),
    /// Sequence (vector) pattern — matches a vector element-by-element.
    Vector(Vec<Pattern>),
}

/// An element inside a back-quote list pattern.
#[derive(Clone, Debug)]
enum BqElement {
    /// `,PAT` — match element against pattern.
    Unquote(Pattern),
    /// A literal / nested quasi-quote element that must match via `equal`.
    Exact(Value),
}

// ---------------------------------------------------------------------------
// Pattern compilation (Expr -> Pattern)
// ---------------------------------------------------------------------------

/// Compile an `Expr` AST node (as written in source) into a `Pattern`.
fn compile_pattern(expr: &Expr) -> Result<Pattern, Flow> {
    match expr {
        // `_` wildcard
        Expr::Symbol(s) if s == "_" => Ok(Pattern::Wildcard),

        // `nil` and `t` are literal matches, not bindings
        Expr::Symbol(s) if s == "nil" => Ok(Pattern::Literal(Value::Nil)),
        Expr::Symbol(s) if s == "t" => Ok(Pattern::Literal(Value::True)),

        // Any other bare symbol is a binding
        Expr::Symbol(s) => Ok(Pattern::Bind(s.clone())),

        // Literal integers, floats, strings, chars, keywords, booleans
        Expr::Int(n) => Ok(Pattern::Literal(Value::Int(*n))),
        Expr::Float(f) => Ok(Pattern::Literal(Value::Float(*f))),
        Expr::Str(s) => Ok(Pattern::Literal(Value::string(s.clone()))),
        Expr::Char(c) => Ok(Pattern::Literal(Value::Char(*c))),
        Expr::Keyword(k) => Ok(Pattern::Literal(Value::Keyword(k.clone()))),
        Expr::Bool(true) => Ok(Pattern::Literal(Value::True)),
        Expr::Bool(false) => Ok(Pattern::Literal(Value::Nil)),

        // Bare vectors are not valid pcase patterns; vector destructuring
        // must use backquote syntax like ``[,a ,b]`.
        Expr::Vector(_) => Err(signal(
            "error",
            vec![Value::string(format!("Unknown pattern ‘{}’", print_expr(expr)))],
        )),

        // List forms: quote, pred, guard, let, and, or, app, backquote
        Expr::List(items) if !items.is_empty() => compile_list_pattern(items),

        // Empty list = nil literal
        Expr::List(_) => Ok(Pattern::Literal(Value::Nil)),

        Expr::DottedList(items, _) => {
            let rendered = print_expr(expr);
            let Some(head) = items.first() else {
                return Err(signal("error", vec![Value::string("Unknown pattern ‘nil’")]));
            };
            match head {
                Expr::Symbol(name) => Err(signal(
                    "error",
                    vec![Value::string(format!("Unknown {name} pattern: {rendered}"))],
                )),
                other => Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), quote_to_value(other)],
                )),
            }
        }
    }
}

/// Compile a list-form pattern like `(quote X)`, `(pred F)`, `(and ...)`,
/// backquote patterns, etc.
fn compile_list_pattern(items: &[Expr]) -> Result<Pattern, Flow> {
    let head = &items[0];

    match head {
        // (quote LITERAL) or 'LITERAL
        Expr::Symbol(s) if s == "quote" => {
            let literal = items.get(1).map(quote_to_value).unwrap_or(Value::Nil);
            Ok(Pattern::Literal(literal))
        }

        // (pred FUNC)
        Expr::Symbol(s) if s == "pred" => {
            let predicate = items
                .get(1)
                .cloned()
                .unwrap_or_else(|| Expr::Symbol("nil".to_string()));
            Ok(Pattern::Pred(predicate))
        }

        // (guard EXPR)
        Expr::Symbol(s) if s == "guard" => {
            let guard_expr = items
                .get(1)
                .cloned()
                .unwrap_or_else(|| Expr::Symbol("nil".to_string()));
            Ok(Pattern::Guard(guard_expr))
        }

        // (let PATTERN EXPR)
        Expr::Symbol(s) if s == "let" => {
            let provided = items.len().saturating_sub(1);
            if provided != 2 {
                return Err(signal(
                    "wrong-number-of-arguments",
                    vec![
                        Value::Subr("let--pcase-macroexpander".to_string()),
                        Value::Int(provided as i64),
                    ],
                ));
            }
            let sub = compile_pattern(&items[1])?;
            Ok(Pattern::Let(Box::new(sub), items[2].clone()))
        }

        // (and PAT...)
        Expr::Symbol(s) if s == "and" => {
            let pats: Result<Vec<Pattern>, Flow> = items[1..].iter().map(compile_pattern).collect();
            Ok(Pattern::And(pats?))
        }

        // (or PAT...)
        Expr::Symbol(s) if s == "or" => {
            if items.len() <= 2 {
                return Err(signal("error", vec![Value::string("Please avoid it")]));
            }
            let pats: Result<Vec<Pattern>, Flow> = items[1..].iter().map(compile_pattern).collect();
            Ok(Pattern::Or(pats?))
        }

        // (app FUN PAT)
        Expr::Symbol(s) if s == "app" => {
            if items.len() < 3 {
                return Err(signal(
                    "error",
                    vec![Value::string("Unknown pattern ‘nil’")],
                ));
            }
            let sub = compile_pattern(&items[2])?;
            Ok(Pattern::App(items[1].clone(), Box::new(sub)))
        }

        // Backquote: (\` BODY)
        Expr::Symbol(s) if s == "\\`" => {
            if items.len() != 2 {
                return Err(signal(
                    "error",
                    vec![Value::string(
                        "backquote pattern requires exactly one argument",
                    )],
                ));
            }
            compile_backquote(&items[1])
        }

        // Anything else that looks like a list — not a known pattern combinator.
        // Treat as a quoted literal (for e.g. `(1 2 3)` which should match
        // the list literally).  This matches Emacs behaviour where unknown
        // list patterns signal an error.
        _ => {
            let rendered = super::expr::print_expr(&Expr::List(items.to_vec()));
            match head {
                Expr::Symbol(name) => {
                    let message = format!("Unknown {name} pattern: {rendered}");
                    Err(signal("error", vec![Value::string(message)]))
                }
                other => Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("symbolp"), quote_to_value(other)],
                )),
            }
        }
    }
}

/// Compile the body of a backquote pattern.
fn compile_backquote(expr: &Expr) -> Result<Pattern, Flow> {
    match expr {
        // (\, PAT) — unquote: compile the inner thing as a pattern
        Expr::List(items)
            if items.len() == 2 && matches!(&items[0], Expr::Symbol(s) if s == "\\,") =>
        {
            compile_pattern(&items[1])
        }

        // A proper list inside backquote — each element may contain unquotes
        Expr::List(items) => {
            let elems: Result<Vec<BqElement>, Flow> =
                items.iter().map(compile_bq_element).collect();
            Ok(Pattern::BackquoteList(elems?))
        }

        // Dotted list inside backquote — head elements + tail pattern
        Expr::DottedList(heads, tail) => {
            let elems: Result<Vec<BqElement>, Flow> =
                heads.iter().map(compile_bq_element).collect();
            let tail_pat = compile_bq_tail(tail)?;
            Ok(Pattern::BackquoteDotted(elems?, Box::new(tail_pat)))
        }

        // Vector inside backquote
        Expr::Vector(items) => {
            let pats: Result<Vec<Pattern>, Flow> = items
                .iter()
                .map(|e| compile_bq_element_to_pattern(e))
                .collect();
            Ok(Pattern::Vector(pats?))
        }

        // Anything else (symbol, int, string, ...) is a literal
        _ => Ok(Pattern::Literal(quote_to_value(expr))),
    }
}

/// Compile one element of a backquote list.
fn compile_bq_element(expr: &Expr) -> Result<BqElement, Flow> {
    match expr {
        // (\, PAT) — unquote
        Expr::List(items)
            if items.len() == 2 && matches!(&items[0], Expr::Symbol(s) if s == "\\,") =>
        {
            let pat = compile_pattern(&items[1])?;
            Ok(BqElement::Unquote(pat))
        }

        // Nested list inside backquote — recursively compile as a backquote pattern,
        // but wrap it as an Unquote since the result is itself a Pattern.
        Expr::List(_) | Expr::DottedList(_, _) => {
            let pat = compile_backquote(expr)?;
            Ok(BqElement::Unquote(pat))
        }

        // Literal element
        _ => Ok(BqElement::Exact(quote_to_value(expr))),
    }
}

/// Convert a backquote element to a plain Pattern (used for vectors inside bq).
fn compile_bq_element_to_pattern(expr: &Expr) -> Result<Pattern, Flow> {
    match expr {
        Expr::List(items)
            if items.len() == 2 && matches!(&items[0], Expr::Symbol(s) if s == "\\,") =>
        {
            compile_pattern(&items[1])
        }
        _ => Ok(Pattern::Literal(quote_to_value(expr))),
    }
}

/// Compile the tail of a dotted backquote pattern.
fn compile_bq_tail(expr: &Expr) -> Result<Pattern, Flow> {
    match expr {
        Expr::List(items)
            if items.len() == 2 && matches!(&items[0], Expr::Symbol(s) if s == "\\,") =>
        {
            compile_pattern(&items[1])
        }
        _ => Ok(Pattern::Literal(quote_to_value(expr))),
    }
}

// ---------------------------------------------------------------------------
// Pattern matching
// ---------------------------------------------------------------------------

/// Resolve a function expression for use in `pred` and `app` patterns.
///
/// If `expr` is a bare symbol, resolve it as a function name (via the
/// obarray function cell or as a built-in Subr).  Otherwise evaluate
/// the expression normally (e.g. a lambda form).
fn resolve_function(eval: &mut Evaluator, expr: &Expr) -> Result<Value, Flow> {
    match expr {
        Expr::Symbol(name) => {
            // Try obarray function cell first.
            if let Some(func) = eval.obarray().symbol_function(name).cloned() {
                Ok(func)
            } else {
                // Treat as a built-in (Subr) name.
                Ok(Value::Subr(name.clone()))
            }
        }
        // For anything else (lambda expression, #'func, etc.), evaluate normally.
        _ => eval.eval(expr),
    }
}

/// Attempt to match `value` against `pattern`.  On success return the
/// accumulated bindings (symbol -> value).  On failure return `None`.
///
/// The `eval` parameter is needed for `pred`, `guard`, `let`, and `app`
/// patterns that must call back into the evaluator.
fn match_pattern(
    eval: &mut Evaluator,
    pattern: &Pattern,
    value: &Value,
) -> Result<Option<HashMap<String, Value>>, Flow> {
    match pattern {
        Pattern::Wildcard => Ok(Some(HashMap::new())),

        Pattern::Bind(name) => {
            let mut m = HashMap::new();
            m.insert(name.clone(), value.clone());
            Ok(Some(m))
        }

        Pattern::Literal(expected) => {
            if equal_value(expected, value, 0) {
                Ok(Some(HashMap::new()))
            } else {
                Ok(None)
            }
        }

        Pattern::Pred(func_expr) => {
            let func = resolve_function(eval, func_expr)?;
            let result = eval.apply(func, vec![value.clone()])?;
            if result.is_truthy() {
                Ok(Some(HashMap::new()))
            } else {
                Ok(None)
            }
        }

        Pattern::Guard(expr) => {
            let result = eval.eval(expr)?;
            if result.is_truthy() {
                Ok(Some(HashMap::new()))
            } else {
                Ok(None)
            }
        }

        Pattern::Let(sub_pat, expr) => {
            let val = eval.eval(expr)?;
            match_pattern(eval, sub_pat, &val)
        }

        Pattern::And(pats) => {
            // We need to install bindings incrementally so that later
            // sub-patterns (especially `guard`) can reference variables
            // bound by earlier sub-patterns within the same `and`.
            let mut combined = HashMap::new();
            let mut frames_pushed = 0usize;

            for pat in pats {
                match match_pattern(eval, pat, value) {
                    Ok(Some(bindings)) => {
                        // Install these bindings so subsequent sub-patterns
                        // (e.g. guard) can see them.
                        if !bindings.is_empty() {
                            eval.dynamic.push(bindings.clone());
                            frames_pushed += 1;
                        }
                        combined.extend(bindings);
                    }
                    Ok(None) => {
                        // Unwind any frames we pushed.
                        for _ in 0..frames_pushed {
                            eval.dynamic.pop();
                        }
                        return Ok(None);
                    }
                    Err(e) => {
                        for _ in 0..frames_pushed {
                            eval.dynamic.pop();
                        }
                        return Err(e);
                    }
                }
            }

            // Unwind the temporary frames — the caller will install the
            // final combined bindings properly via `with_bindings`.
            for _ in 0..frames_pushed {
                eval.dynamic.pop();
            }
            Ok(Some(combined))
        }

        Pattern::Or(pats) => {
            for pat in pats {
                if let Some(bindings) = match_pattern(eval, pat, value)? {
                    return Ok(Some(bindings));
                }
            }
            Ok(None)
        }

        Pattern::App(func_expr, sub_pat) => {
            let func = resolve_function(eval, func_expr)?;
            let result = eval.apply(func, vec![value.clone()])?;
            match_pattern(eval, sub_pat, &result)
        }

        Pattern::BackquoteList(elems) => match_backquote_list(eval, elems, value),

        Pattern::BackquoteDotted(elems, tail_pat) => {
            match_backquote_dotted(eval, elems, tail_pat, value)
        }

        Pattern::Vector(pats) => match_vector(eval, pats, value),
    }
}

/// Match a back-quote list pattern against a value.
fn match_backquote_list(
    eval: &mut Evaluator,
    elems: &[BqElement],
    value: &Value,
) -> Result<Option<HashMap<String, Value>>, Flow> {
    // Collect the value into a vec — must be a proper list of exactly the
    // right length.
    let items = match list_to_vec(value) {
        Some(v) => v,
        None => return Ok(None),
    };
    if items.len() != elems.len() {
        return Ok(None);
    }

    let mut combined = HashMap::new();
    for (elem, val) in elems.iter().zip(items.iter()) {
        match elem {
            BqElement::Unquote(pat) => match match_pattern(eval, pat, val)? {
                Some(bindings) => combined.extend(bindings),
                None => return Ok(None),
            },
            BqElement::Exact(expected) => {
                if !equal_value(expected, val, 0) {
                    return Ok(None);
                }
            }
        }
    }
    Ok(Some(combined))
}

/// Match a back-quote dotted list pattern against a value.
fn match_backquote_dotted(
    eval: &mut Evaluator,
    elems: &[BqElement],
    tail_pat: &Pattern,
    value: &Value,
) -> Result<Option<HashMap<String, Value>>, Flow> {
    let mut combined = HashMap::new();
    let mut cursor = value.clone();

    for elem in elems {
        match &cursor {
            Value::Cons(cell) => {
                let pair = cell.lock().expect("poisoned");
                let car = pair.car.clone();
                let cdr = pair.cdr.clone();
                drop(pair);

                match elem {
                    BqElement::Unquote(pat) => match match_pattern(eval, pat, &car)? {
                        Some(bindings) => combined.extend(bindings),
                        None => return Ok(None),
                    },
                    BqElement::Exact(expected) => {
                        if !equal_value(expected, &car, 0) {
                            return Ok(None);
                        }
                    }
                }
                cursor = cdr;
            }
            _ => return Ok(None),
        }
    }

    // Match the remaining cdr against the tail pattern
    match match_pattern(eval, tail_pat, &cursor)? {
        Some(bindings) => combined.extend(bindings),
        None => return Ok(None),
    }
    Ok(Some(combined))
}

/// Match a vector pattern against a value.
fn match_vector(
    eval: &mut Evaluator,
    pats: &[Pattern],
    value: &Value,
) -> Result<Option<HashMap<String, Value>>, Flow> {
    let Value::Vector(v) = value else {
        return Ok(None);
    };
    let items = v.lock().expect("poisoned");
    if items.len() != pats.len() {
        return Ok(None);
    }

    let mut combined = HashMap::new();
    for (pat, val) in pats.iter().zip(items.iter()) {
        match match_pattern(eval, pat, val)? {
            Some(bindings) => combined.extend(bindings),
            None => return Ok(None),
        }
    }
    Ok(Some(combined))
}

// ---------------------------------------------------------------------------
// Helper: install bindings and evaluate body forms
// ---------------------------------------------------------------------------

/// Push bindings into the evaluator environment, run `body`, then pop.
fn with_bindings(
    eval: &mut Evaluator,
    bindings: HashMap<String, Value>,
    body: &[Expr],
) -> EvalResult {
    if bindings.is_empty() {
        return eval.sf_progn(body);
    }

    let use_lexical = eval.lexical_binding();
    if use_lexical {
        // Split bindings into lexical vs dynamic (special) sets.
        let mut lex = HashMap::new();
        let mut dyn_bindings = HashMap::new();
        for (name, val) in bindings {
            if eval.obarray().is_special(&name) {
                dyn_bindings.insert(name, val);
            } else {
                lex.insert(name, val);
            }
        }
        let pushed_lex = !lex.is_empty();
        let pushed_dyn = !dyn_bindings.is_empty();
        if pushed_lex {
            eval.lexenv.push(lex);
        }
        if pushed_dyn {
            eval.dynamic.push(dyn_bindings);
        }
        let result = eval.sf_progn(body);
        if pushed_dyn {
            eval.dynamic.pop();
        }
        if pushed_lex {
            eval.lexenv.pop();
        }
        result
    } else {
        eval.dynamic.push(bindings);
        let result = eval.sf_progn(body);
        eval.dynamic.pop();
        result
    }
}

fn collect_pattern_binding_names(pattern: &Pattern, names: &mut Vec<String>) {
    match pattern {
        Pattern::Bind(name) => {
            if !names.contains(name) {
                names.push(name.clone());
            }
        }
        Pattern::Let(inner, _) | Pattern::App(_, inner) => {
            collect_pattern_binding_names(inner, names);
        }
        Pattern::And(items) | Pattern::Or(items) | Pattern::Vector(items) => {
            for item in items {
                collect_pattern_binding_names(item, names);
            }
        }
        Pattern::BackquoteList(items) => {
            for item in items {
                if let BqElement::Unquote(inner) = item {
                    collect_pattern_binding_names(inner, names);
                }
            }
        }
        Pattern::BackquoteDotted(items, tail) => {
            for item in items {
                if let BqElement::Unquote(inner) = item {
                    collect_pattern_binding_names(inner, names);
                }
            }
            collect_pattern_binding_names(tail, names);
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::Pred(_) | Pattern::Guard(_) => {}
    }
}

fn fallback_nil_bindings_for_pattern(pattern: &Pattern) -> HashMap<String, Value> {
    let mut names = Vec::new();
    collect_pattern_binding_names(pattern, &mut names);
    let mut fallback = HashMap::with_capacity(names.len());
    for name in names {
        fallback.insert(name, Value::Nil);
    }
    fallback
}

fn fallback_fill_pattern_bindings(
    pattern: &Pattern,
    value: &Value,
    bindings: &mut HashMap<String, Value>,
) {
    match pattern {
        Pattern::Bind(name) => {
            bindings.insert(name.clone(), value.clone());
        }
        Pattern::And(items) => {
            for item in items {
                fallback_fill_pattern_bindings(item, value, bindings);
            }
        }
        Pattern::Or(items) => {
            if let Some(first) = items.first() {
                fallback_fill_pattern_bindings(first, value, bindings);
            }
        }
        Pattern::BackquoteList(items) => {
            let mut cursor = value.clone();
            for item in items {
                let current = match cursor.clone() {
                    Value::Cons(cell) => {
                        let pair = cell.lock().expect("poisoned");
                        let car = pair.car.clone();
                        cursor = pair.cdr.clone();
                        car
                    }
                    _ => {
                        cursor = Value::Nil;
                        Value::Nil
                    }
                };
                if let BqElement::Unquote(inner) = item {
                    fallback_fill_pattern_bindings(inner, &current, bindings);
                }
            }
        }
        Pattern::BackquoteDotted(items, tail) => {
            let mut cursor = value.clone();
            for item in items {
                let current = match cursor.clone() {
                    Value::Cons(cell) => {
                        let pair = cell.lock().expect("poisoned");
                        let car = pair.car.clone();
                        cursor = pair.cdr.clone();
                        car
                    }
                    _ => {
                        cursor = Value::Nil;
                        Value::Nil
                    }
                };
                if let BqElement::Unquote(inner) = item {
                    fallback_fill_pattern_bindings(inner, &current, bindings);
                }
            }
            fallback_fill_pattern_bindings(tail, &cursor, bindings);
        }
        Pattern::Vector(items) => {
            let vector_items = if let Value::Vector(values) = value {
                Some(values.lock().expect("poisoned").clone())
            } else {
                None
            };
            for (idx, item) in items.iter().enumerate() {
                let current = vector_items
                    .as_ref()
                    .and_then(|vals| vals.get(idx))
                    .cloned()
                    .unwrap_or(Value::Nil);
                fallback_fill_pattern_bindings(item, &current, bindings);
            }
        }
        Pattern::Wildcard
        | Pattern::Literal(_)
        | Pattern::Pred(_)
        | Pattern::Guard(_)
        | Pattern::Let(_, _)
        | Pattern::App(_, _) => {}
    }
}

fn fallback_bindings_for_pattern(pattern: &Pattern, value: &Value) -> HashMap<String, Value> {
    let mut bindings = fallback_nil_bindings_for_pattern(pattern);
    fallback_fill_pattern_bindings(pattern, value, &mut bindings);
    bindings
}

// ---------------------------------------------------------------------------
// Special form implementations
// ---------------------------------------------------------------------------

/// `(pcase EXPR (PATTERN BODY...) ...)`
///
/// Evaluate EXPR once, then try each clause in order.  The first clause
/// whose PATTERN matches gets its BODY forms evaluated with any pattern
/// bindings in scope.  Returns the result of the matching clause body, or
/// nil if no clause matches.
pub(crate) fn sf_pcase(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("pcase"), Value::Int(tail.len() as i64)],
        ));
    }

    // Evaluate the expression being matched.
    let subject = eval.eval(&tail[0])?;

    // Iterate over clauses.
    for clause_expr in &tail[1..] {
        let Expr::List(clause) = clause_expr else {
            return Err(signal(
                "error",
                vec![Value::string("pcase clause must be a list")],
            ));
        };
        if clause.is_empty() {
            continue;
        }

        let pattern = compile_pattern(&clause[0])?;
        if let Some(bindings) = match_pattern(eval, &pattern, &subject)? {
            return with_bindings(eval, bindings, &clause[1..]);
        }
    }

    // No clause matched.
    Ok(Value::Nil)
}

/// `(pcase-let ((PATTERN VAL) ...) BODY...)`
///
/// Like `let` but each binding uses pattern destructuring.  All VAL
/// expressions are evaluated first (parallel binding), then all patterns
/// are matched and bindings installed.
pub(crate) fn sf_pcase_let(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("pcase-let"), Value::Int(tail.len() as i64)],
        ));
    }

    let bindings_expr = match &tail[0] {
        Expr::List(entries) => entries.clone(),
        Expr::Symbol(s) if s == "nil" => Vec::new(),
        other => return Err(signal("wrong-type-argument", vec![Value::symbol("listp"), quote_to_value(other)])),
    };

    // Validate binding entry shapes first. Oracle reports list-shape errors
    // before evaluating patterns/values, even if an earlier pattern would fail.
    let mut binding_pairs: Vec<Vec<Expr>> = Vec::with_capacity(bindings_expr.len());
    for entry in &bindings_expr {
        let Expr::List(pair) = entry else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), quote_to_value(entry)],
            ));
        };
        if pair.len() < 2 {
            return Err(signal(
                "error",
                vec![Value::string(
                    "pcase-let binding must have at least PATTERN and VAL",
                )],
            ));
        }
        binding_pairs.push(pair.clone());
    }

    // Oracle shortcut: with no body forms, `pcase-let` returns nil after
    // structural validation, without evaluating binding patterns/values.
    if tail.len() == 1 {
        return Ok(Value::Nil);
    }

    // Phase 1: evaluate all values.
    let mut pairs: Vec<(Pattern, Value)> = Vec::with_capacity(binding_pairs.len());
    for pair in &binding_pairs {
        let pat = compile_pattern(&pair[0])?;
        let val = eval.eval(&pair[1])?;
        pairs.push((pat, val));
    }

    // Phase 2: match all patterns and collect bindings.
    let mut combined = HashMap::new();
    for (pat, val) in &pairs {
        let bindings = match match_pattern(eval, pat, val)? {
            Some(bindings) => bindings,
            None => fallback_bindings_for_pattern(pat, val),
        };
        combined.extend(bindings);
    }

    // Phase 3: evaluate body with bindings.
    with_bindings(eval, combined, &tail[1..])
}

/// `(pcase-let* ((PATTERN VAL) ...) BODY...)`
///
/// Like `let*` but with pattern destructuring.  Each (PATTERN VAL) is
/// evaluated and matched sequentially, so later bindings can refer to
/// earlier ones.
pub(crate) fn sf_pcase_let_star(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("pcase-let*"), Value::Int(tail.len() as i64)],
        ));
    }

    // Oracle shortcut: with no body forms, `pcase-let*` returns nil and does
    // not validate/evaluate bindings.
    if tail.len() == 1 {
        return Ok(Value::Nil);
    }

    let bindings_expr = match &tail[0] {
        Expr::List(entries) => entries.clone(),
        Expr::Symbol(s) if s == "nil" => Vec::new(),
        other => return Err(signal("wrong-type-argument", vec![Value::symbol("listp"), quote_to_value(other)])),
    };

    let mut binding_pairs: Vec<Vec<Expr>> = Vec::with_capacity(bindings_expr.len());
    for entry in &bindings_expr {
        let Expr::List(pair) = entry else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), quote_to_value(entry)],
            ));
        };
        if pair.len() < 2 {
            return Err(signal(
                "error",
                vec![Value::string(
                    "pcase-let* binding must have PATTERN and VAL",
                )],
            ));
        }
        binding_pairs.push(pair.clone());
    }

    // Process each binding sequentially, pushing one env frame per binding.
    let mut frames_pushed = 0usize;
    let use_lexical = eval.lexical_binding();

    for pair in &binding_pairs {
        let pat = match compile_pattern(&pair[0]) {
            Ok(p) => p,
            Err(e) => {
                for _ in 0..frames_pushed {
                    if use_lexical {
                        eval.lexenv.pop();
                    } else {
                        eval.dynamic.pop();
                    }
                }
                return Err(e);
            }
        };

        let val = match eval.eval(&pair[1]) {
            Ok(v) => v,
            Err(e) => {
                for _ in 0..frames_pushed {
                    if use_lexical {
                        eval.lexenv.pop();
                    } else {
                        eval.dynamic.pop();
                    }
                }
                return Err(e);
            }
        };

        let bindings = match match_pattern(eval, &pat, &val) {
            Ok(Some(b)) => b,
            Ok(None) => fallback_bindings_for_pattern(&pat, &val),
            Err(e) => {
                for _ in 0..frames_pushed {
                    if use_lexical {
                        eval.lexenv.pop();
                    } else {
                        eval.dynamic.pop();
                    }
                }
                return Err(e);
            }
        };

        if !bindings.is_empty() {
            if use_lexical {
                eval.lexenv.push(bindings);
            } else {
                eval.dynamic.push(bindings);
            }
            frames_pushed += 1;
        }
    }

    let result = eval.sf_progn(&tail[1..]);

    for _ in 0..frames_pushed {
        if use_lexical {
            eval.lexenv.pop();
        } else {
            eval.dynamic.pop();
        }
    }
    result
}

/// `(pcase-dolist (PATTERN LIST) BODY...)`
///
/// Iterate over LIST.  For each element, match it against PATTERN and
/// evaluate BODY with the resulting bindings.
pub(crate) fn sf_pcase_dolist(eval: &mut Evaluator, tail: &[Expr]) -> EvalResult {
    if tail.is_empty() {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::cons(Value::Int(1), Value::Int(1)), Value::Int(0)],
        ));
    }

    let spec = match &tail[0] {
        Expr::List(spec) => spec,
        Expr::DottedList(_, last) => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), quote_to_value(last)],
            ))
        }
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), quote_to_value(other)],
            ))
        }
    };
    if !(2..=3).contains(&spec.len()) {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::cons(Value::Int(2), Value::Int(3)),
                Value::Int(spec.len() as i64),
            ],
        ));
    }

    let pattern = compile_pattern(&spec[0])?;
    let mut list_val = eval.eval(&spec[1])?;
    let body = &tail[1..];

    while list_val.is_truthy() {
        let Value::Cons(cell) = list_val.clone() else {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("listp"), list_val],
            ));
        };
        let pair = cell.lock().expect("poisoned");
        let item = pair.car.clone();
        let next = pair.cdr.clone();
        drop(pair);

        let bindings = match match_pattern(eval, &pattern, &item)? {
            Some(bindings) => bindings,
            None => fallback_bindings_for_pattern(&pattern, &item),
        };
        with_bindings(eval, bindings, body)?;
        list_val = next;
    }

    if spec.len() == 3 {
        return eval.eval(&spec[2]);
    }

    Ok(Value::Nil)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::{format_eval_result, parse_forms};

    /// Helper: parse source, evaluate all forms, return formatted results.
    fn eval_all(src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        ev.eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect()
    }

    /// Helper: evaluate and return the last result.
    fn eval_last(src: &str) -> String {
        let results = eval_all(src);
        results.into_iter().last().unwrap()
    }

    // =======================================================================
    // 1. Wildcard pattern
    // =======================================================================

    #[test]
    fn pcase_wildcard_matches_anything() {
        assert_eq!(eval_last("(pcase 42 (_ 'matched))"), "OK matched");
    }

    #[test]
    fn pcase_wildcard_matches_nil() {
        assert_eq!(eval_last("(pcase nil (_ 'matched))"), "OK matched");
    }

    // =======================================================================
    // 2. Symbol binding pattern
    // =======================================================================

    #[test]
    fn pcase_symbol_binds_value() {
        assert_eq!(eval_last("(pcase 42 (x x))"), "OK 42");
    }

    #[test]
    fn pcase_symbol_binds_string() {
        assert_eq!(
            eval_last(r#"(pcase "hello" (s (concat s " world")))"#),
            r#"OK "hello world""#
        );
    }

    // =======================================================================
    // 3. Quoted literal pattern
    // =======================================================================

    #[test]
    fn pcase_quoted_symbol() {
        assert_eq!(eval_last("(pcase 'foo ('foo 1) ('bar 2))"), "OK 1");
    }

    #[test]
    fn pcase_quoted_symbol_no_match() {
        assert_eq!(eval_last("(pcase 'baz ('foo 1) ('bar 2))"), "OK nil");
    }

    #[test]
    fn pcase_quoted_list() {
        assert_eq!(
            eval_last("(pcase '(1 2 3) ('(1 2 3) 'yes) (_ 'no))"),
            "OK yes"
        );
    }

    #[test]
    fn pcase_quote_pattern_missing_arg_defaults_to_nil() {
        assert_eq!(
            eval_last("(pcase 1 ((quote) 'x) (_ 'y))"),
            "OK y"
        );
    }

    #[test]
    fn pcase_quote_pattern_ignores_extra_args() {
        assert_eq!(
            eval_last("(pcase 1 ((quote 1 2 3) 'x) (_ 'y))"),
            "OK x"
        );
    }

    // =======================================================================
    // 4. Integer literal pattern
    // =======================================================================

    #[test]
    fn pcase_integer_match() {
        assert_eq!(
            eval_last("(pcase 1 (1 'one) (2 'two) (_ 'other))"),
            "OK one"
        );
    }

    #[test]
    fn pcase_integer_no_match_falls_through() {
        assert_eq!(
            eval_last("(pcase 3 (1 'one) (2 'two) (_ 'other))"),
            "OK other"
        );
    }

    // =======================================================================
    // 5. String literal pattern
    // =======================================================================

    #[test]
    fn pcase_string_match() {
        assert_eq!(
            eval_last(r#"(pcase "hello" ("hello" 'yes) (_ 'no))"#),
            "OK yes"
        );
    }

    #[test]
    fn pcase_string_no_match() {
        assert_eq!(
            eval_last(r#"(pcase "world" ("hello" 'yes) (_ 'no))"#),
            "OK no"
        );
    }

    // =======================================================================
    // 6. Character literal pattern
    // =======================================================================

    #[test]
    fn pcase_char_match() {
        assert_eq!(eval_last("(pcase ?a (?a 'yes) (_ 'no))"), "OK yes");
    }

    // =======================================================================
    // 7. nil / t literal patterns
    // =======================================================================

    #[test]
    fn pcase_nil_literal() {
        assert_eq!(
            eval_last("(pcase nil (nil 'is-nil) (_ 'other))"),
            "OK is-nil"
        );
    }

    #[test]
    fn pcase_t_literal() {
        assert_eq!(eval_last("(pcase t (t 'is-t) (_ 'other))"), "OK is-t");
    }

    // =======================================================================
    // 8. pred pattern
    // =======================================================================

    #[test]
    fn pcase_pred_integerp() {
        assert_eq!(
            eval_last("(pcase 42 ((pred integerp) 'int) (_ 'other))"),
            "OK int"
        );
    }

    #[test]
    fn pcase_pred_stringp() {
        assert_eq!(
            eval_last(r#"(pcase "hi" ((pred stringp) 'str) (_ 'other))"#),
            "OK str"
        );
    }

    #[test]
    fn pcase_pred_no_match() {
        assert_eq!(
            eval_last(r#"(pcase "hi" ((pred integerp) 'int) (_ 'other))"#),
            "OK other"
        );
    }

    #[test]
    fn pcase_pred_pattern_missing_arg_uses_nil_callable() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((pred) 'x) (_ 'y)) (error err))"),
            "OK (void-function nil)"
        );
    }

    #[test]
    fn pcase_pred_pattern_ignores_extra_args() {
        assert_eq!(
            eval_last("(pcase 1 ((pred integerp 2) 'x) (_ 'y))"),
            "OK x"
        );
    }

    #[test]
    fn pcase_pred_pattern_ignores_multiple_extra_args() {
        assert_eq!(
            eval_last("(pcase 1 ((pred integerp 2 3) 'x) (_ 'y))"),
            "OK x"
        );
    }

    #[test]
    fn pcase_pred_pattern_binder_and_dolist_behavior_matches_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((pred) 1)) 'ok) (error err))"),
            "OK (void-function nil)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((pred integerp 2 3) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((pred) 1)) 'ok) (error err))"),
            "OK (void-function nil)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((pred integerp 2 3) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((pred) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (void-function nil)"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((pred integerp 2 3) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (1 1)"
        );
    }

    // =======================================================================
    // 9. guard pattern
    // =======================================================================

    #[test]
    fn pcase_guard_true() {
        assert_eq!(
            eval_last("(pcase 42 ((guard (> 10 5)) 'yes) (_ 'no))"),
            "OK yes"
        );
    }

    #[test]
    fn pcase_guard_false() {
        assert_eq!(
            eval_last("(pcase 42 ((guard (< 10 5)) 'yes) (_ 'no))"),
            "OK no"
        );
    }

    #[test]
    fn pcase_guard_pattern_missing_arg_defaults_to_nil() {
        assert_eq!(
            eval_last("(pcase 1 ((guard) 'x) (_ 'y))"),
            "OK y"
        );
    }

    #[test]
    fn pcase_guard_pattern_ignores_extra_args() {
        assert_eq!(
            eval_last("(pcase 1 ((guard t 2) 'x) (_ 'y))"),
            "OK x"
        );
    }

    #[test]
    fn pcase_guard_pattern_binder_and_dolist_behavior_matches_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((guard) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((guard t 2) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((guard) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((guard t 2) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((guard) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (1 1)"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((guard t 2) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (1 1)"
        );
    }

    // =======================================================================
    // 10. let pattern
    // =======================================================================

    #[test]
    fn pcase_let_pattern() {
        // (let PATTERN EXPR) — match EXPR against PATTERN
        assert_eq!(eval_last("(pcase 42 ((let x (+ 1 2)) x))"), "OK 3");
    }

    #[test]
    fn pcase_let_pattern_arity_errors_match_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((let) 'x) (_ 'y)) (error err))"),
            "OK (wrong-number-of-arguments #<subr let--pcase-macroexpander> 0)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((let x) 'x) (_ 'y)) (error err))"),
            "OK (wrong-number-of-arguments #<subr let--pcase-macroexpander> 1)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((let x 2 3) x) (_ 'y)) (error err))"),
            "OK (wrong-number-of-arguments #<subr let--pcase-macroexpander> 3)"
        );
    }

    #[test]
    fn pcase_unknown_symbol_pattern_error_shape_matches_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((foo bar) 'x) (_ 'y)) (error err))"),
            "OK (error \"Unknown foo pattern: (foo bar)\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((foo bar) 1)) 'ok) (error err))"),
            "OK (error \"Unknown foo pattern: (foo bar)\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((foo bar) 1)) 'ok) (error err))"),
            "OK (error \"Unknown foo pattern: (foo bar)\")"
        );
    }

    #[test]
    fn pcase_non_symbol_pattern_head_signals_symbol_type_error() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 (((1 2) 3) 'x) (_ 'y)) (error err))"),
            "OK (wrong-type-argument symbolp (1 2))"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let ((((1 2) 3) 1)) 'ok) (error err))"),
            "OK (wrong-type-argument symbolp (1 2))"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* ((((1 2) 3) 1)) 'ok) (error err))"),
            "OK (wrong-type-argument symbolp (1 2))"
        );
    }

    #[test]
    fn pcase_dotted_symbol_head_pattern_error_shape_matches_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase '(1 . 2) ((a . b) (list a b)) (_ 'no)) (error err))"),
            "OK (error \"Unknown a pattern: (a . b)\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((a . b) '(1 . 2))) (list a b)) (error err))"),
            "OK (error \"Unknown a pattern: (a . b)\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((a . b) '(1 . 2))) (list a b)) (error err))"),
            "OK (error \"Unknown a pattern: (a . b)\")"
        );
    }

    #[test]
    fn pcase_dotted_non_symbol_head_signals_symbol_type_error() {
        assert_eq!(
            eval_last(
                "(condition-case err (pcase '(1 . 2) (((1 2) . 3) 'x) (_ 'y)) (error err))"
            ),
            "OK (wrong-type-argument symbolp (1 2))"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (pcase-let ((((1 2) . 3) '(1 . 2))) 'ok) (error err))"
            ),
            "OK (wrong-type-argument symbolp (1 2))"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (pcase-let* ((((1 2) . 3) '(1 . 2))) 'ok) (error err))"
            ),
            "OK (wrong-type-argument symbolp (1 2))"
        );
    }

    // =======================================================================
    // 11. and pattern
    // =======================================================================

    #[test]
    fn pcase_and_all_match() {
        assert_eq!(eval_last("(pcase 42 ((and (pred integerp) x) x))"), "OK 42");
    }

    #[test]
    fn pcase_and_fails_on_first() {
        assert_eq!(
            eval_last(r#"(pcase "hi" ((and (pred integerp) x) x) (_ 'nope))"#),
            "OK nope"
        );
    }

    #[test]
    fn pcase_and_pattern_empty_and_singleton_behavior_matches_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((and) 'x) (_ 'y)) (error err))"),
            "OK x"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((and 1) 'x) (_ 'y)) (error err))"),
            "OK x"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((and 1 2) 'x) (_ 'y)) (error err))"),
            "OK y"
        );
    }

    // =======================================================================
    // 12. or pattern
    // =======================================================================

    #[test]
    fn pcase_or_first_matches() {
        assert_eq!(
            eval_last("(pcase 1 ((or 1 2 3) 'found) (_ 'nope))"),
            "OK found"
        );
    }

    #[test]
    fn pcase_or_second_matches() {
        assert_eq!(
            eval_last("(pcase 2 ((or 1 2 3) 'found) (_ 'nope))"),
            "OK found"
        );
    }

    #[test]
    fn pcase_or_none_matches() {
        assert_eq!(
            eval_last("(pcase 5 ((or 1 2 3) 'found) (_ 'nope))"),
            "OK nope"
        );
    }

    #[test]
    fn pcase_or_pattern_requires_at_least_two_alternatives() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((or) 'x) (_ 'y)) (error err))"),
            "OK (error \"Please avoid it\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((or 1) 'x) (_ 'y)) (error err))"),
            "OK (error \"Please avoid it\")"
        );
    }

    #[test]
    fn pcase_or_two_alternatives_behavior_matches_oracle() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((or 1 2) 'x) (_ 'y)) (error err))"),
            "OK x"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 3 ((or 1 2) 'x) (_ 'y)) (error err))"),
            "OK y"
        );
    }

    #[test]
    fn pcase_or_arity_errors_propagate_to_let_and_dolist() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((or) 1)) 'ok) (error err))"),
            "OK (error \"Please avoid it\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((or 1) 1)) 'ok) (error err))"),
            "OK (error \"Please avoid it\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((or) 1)) 'ok) (error err))"),
            "OK (error \"Please avoid it\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((or 1) 1)) 'ok) (error err))"),
            "OK (error \"Please avoid it\")"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((or) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (error \"Please avoid it\")"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((or 1) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (error \"Please avoid it\")"
        );
    }

    #[test]
    fn pcase_or_and_empty_patterns_work_in_let_and_dolist() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((or 1 2) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((or 1 2) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((and) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((and) 1)) 'ok) (error err))"),
            "OK ok"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((or 1 2) '(1 2 3)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (1 1 1)"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (let ((acc nil)) (pcase-dolist ((and) '(1 2)) (push 1 acc)) (nreverse acc)) (error err))"
            ),
            "OK (1 1)"
        );
    }

    // =======================================================================
    // 13. app pattern
    // =======================================================================

    #[test]
    fn pcase_app_car() {
        assert_eq!(
            eval_last("(pcase '(1 2 3) ((app car 1) 'yes) (_ 'no))"),
            "OK yes"
        );
    }

    #[test]
    fn pcase_app_with_binding() {
        assert_eq!(eval_last("(pcase '(1 2 3) ((app car x) x))"), "OK 1");
    }

    #[test]
    fn pcase_app_length() {
        assert_eq!(
            eval_last("(pcase '(1 2 3) ((app length 3) 'three) (_ 'other))"),
            "OK three"
        );
    }

    #[test]
    fn pcase_app_pattern_missing_args_reports_unknown_nil_pattern() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((app) 'x) (_ 'y)) (error err))"),
            "OK (error \"Unknown pattern ‘nil’\")"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((app car) 'x) (_ 'y)) (error err))"),
            "OK (error \"Unknown pattern ‘nil’\")"
        );
    }

    #[test]
    fn pcase_app_pattern_ignores_extra_args() {
        assert_eq!(
            eval_last("(condition-case err (pcase 1 ((app car x y) 'x) (_ 'y)) (error err))"),
            "OK (wrong-type-argument listp 1)"
        );
    }

    // =======================================================================
    // 14. Backquote list pattern
    // =======================================================================

    #[test]
    fn pcase_backquote_simple_list() {
        assert_eq!(eval_last("(pcase '(1 2 3) (`(1 ,x 3) x) (_ 'no))"), "OK 2");
    }

    #[test]
    fn pcase_backquote_all_unquoted() {
        assert_eq!(eval_last("(pcase '(10 20) (`(,a ,b) (+ a b)))"), "OK 30");
    }

    #[test]
    fn pcase_backquote_wrong_length() {
        assert_eq!(
            eval_last("(pcase '(1 2) (`(,a ,b ,c) (list a b c)) (_ 'wrong-len))"),
            "OK wrong-len"
        );
    }

    #[test]
    fn pcase_backquote_nested() {
        assert_eq!(eval_last("(pcase '(1 (2 3)) (`(1 (2 ,x)) x))"), "OK 3");
    }

    // =======================================================================
    // 15. Vector pattern
    // =======================================================================

    #[test]
    fn pcase_vector_match() {
        assert_eq!(eval_last("(pcase [1 2 3] (`[,a ,b ,c] (+ a b c)))"), "OK 6");
    }

    #[test]
    fn pcase_vector_wrong_length() {
        assert_eq!(
            eval_last("(pcase [1 2] (`[,a ,b ,c] 'three) (_ 'other))"),
            "OK other"
        );
    }

    #[test]
    fn pcase_bare_vector_pattern_is_unknown() {
        assert_eq!(
            eval_last(
                "(condition-case err (pcase [1 2 3] ([a b c] (+ a b c))) (error err))"
            ),
            "OK (error \"Unknown pattern ‘[a b c]’\")"
        );
    }

    // =======================================================================
    // 16. Multiple body forms in a clause
    // =======================================================================

    #[test]
    fn pcase_clause_with_multiple_body_forms() {
        assert_eq!(eval_last("(pcase 42 (x (+ x 1) (+ x 2)))"), "OK 44");
    }

    // =======================================================================
    // 17. No clause matches -> nil
    // =======================================================================

    #[test]
    fn pcase_no_match_returns_nil() {
        assert_eq!(eval_last("(pcase 42 (1 'one) (2 'two))"), "OK nil");
    }

    // =======================================================================
    // 18. pcase-let basic
    // =======================================================================

    #[test]
    fn pcase_let_basic_binding() {
        assert_eq!(eval_last("(pcase-let ((x 42)) x)"), "OK 42");
    }

    #[test]
    fn pcase_let_destructure() {
        assert_eq!(eval_last("(pcase-let ((`(,a ,b) '(1 2))) (+ a b))"), "OK 3");
    }

    #[test]
    fn pcase_let_multiple_bindings() {
        assert_eq!(eval_last("(pcase-let ((x 10) (y 20)) (+ x y))"), "OK 30");
    }

    #[test]
    fn pcase_let_mismatch_binds_pattern_vars_to_nil() {
        assert_eq!(eval_last("(pcase-let ((`(,a ,b) 1)) (list a b))"), "OK (nil nil)");
    }

    #[test]
    fn pcase_let_partial_mismatch_preserves_available_bindings() {
        assert_eq!(
            eval_last("(pcase-let ((`(,a ,b) '(1 . 2))) (list a b))"),
            "OK (1 nil)"
        );
    }

    #[test]
    fn pcase_let_predicate_mismatch_still_evaluates_body() {
        assert_eq!(eval_last("(pcase-let (((pred numberp) \"x\")) 'ok)"), "OK ok");
    }

    #[test]
    fn pcase_let_non_list_binding_entry_signals_list_type_error() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let ((x 1) y) y) (error err))"),
            "OK (wrong-type-argument listp y)"
        );
        assert_eq!(
            eval_last(
                "(condition-case err (pcase-let (((app car x y) '(1 2)) x)) (error err))"
            ),
            "OK (wrong-type-argument listp x)"
        );
    }

    #[test]
    fn pcase_let_non_list_bindings_form_signals_list_type_error() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let 1 'ok) (error err))"),
            "OK (wrong-type-argument listp 1)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let t 'ok) (error err))"),
            "OK (wrong-type-argument listp t)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let nil 'ok) (error err))"),
            "OK ok"
        );
    }

    #[test]
    fn pcase_let_no_body_skips_binding_pattern_evaluation() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((pred) 1))) (error err))"),
            "OK nil"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let (((guard) 1))) (error err))"),
            "OK nil"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let ((x 1) y)) (error err))"),
            "OK (wrong-type-argument listp y)"
        );
    }

    // =======================================================================
    // 19. pcase-let* sequential binding
    // =======================================================================

    #[test]
    fn pcase_let_star_sequential() {
        assert_eq!(eval_last("(pcase-let* ((x 10) (y (+ x 5))) y)"), "OK 15");
    }

    #[test]
    fn pcase_let_star_destructure() {
        assert_eq!(
            eval_last("(pcase-let* ((`(,a ,b) '(3 4)) (c (+ a b))) c)"),
            "OK 7"
        );
    }

    #[test]
    fn pcase_let_star_mismatch_binds_pattern_vars_to_nil() {
        assert_eq!(
            eval_last("(pcase-let* ((`(,a ,b) 1)) (list a b))"),
            "OK (nil nil)"
        );
    }

    #[test]
    fn pcase_let_star_partial_mismatch_preserves_available_bindings() {
        assert_eq!(
            eval_last("(pcase-let* ((`(,a ,b) '(1 . 2))) (list a b))"),
            "OK (1 nil)"
        );
    }

    #[test]
    fn pcase_let_star_predicate_mismatch_still_evaluates_body() {
        assert_eq!(eval_last("(pcase-let* (((pred numberp) \"x\")) 'ok)"), "OK ok");
    }

    #[test]
    fn pcase_let_star_non_list_binding_entry_signals_list_type_error() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (x) x) (error err))"),
            "OK (wrong-type-argument listp x)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* ((x 1) y) y) (error err))"),
            "OK (wrong-type-argument listp y)"
        );
    }

    #[test]
    fn pcase_let_star_non_list_bindings_form_signals_list_type_error() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let* 1 'ok) (error err))"),
            "OK (wrong-type-argument listp 1)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* t 'ok) (error err))"),
            "OK (wrong-type-argument listp t)"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* nil 'ok) (error err))"),
            "OK ok"
        );
    }

    #[test]
    fn pcase_let_star_no_body_ignores_bindings_and_returns_nil() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let* 1) (error err))"),
            "OK nil"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* t) (error err))"),
            "OK nil"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (x)) (error err))"),
            "OK nil"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((pred) 1))) (error err))"),
            "OK nil"
        );
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((pred) 1) y)) (error err))"),
            "OK nil"
        );
    }

    #[test]
    fn pcase_let_star_body_validates_binding_shape_before_matching() {
        assert_eq!(
            eval_last("(condition-case err (pcase-let* (((pred) 1) y) y) (error err))"),
            "OK (wrong-type-argument listp y)"
        );
    }

    // =======================================================================
    // 20. pcase-dolist
    // =======================================================================

    #[test]
    fn pcase_dolist_basic() {
        assert_eq!(
            eval_last(
                "(let ((result 0))
                   (pcase-dolist (x '(1 2 3 4))
                     (setq result (+ result x)))
                   result)"
            ),
            "OK 10"
        );
    }

    #[test]
    fn pcase_dolist_destructure() {
        assert_eq!(
            eval_last(
                "(let ((result nil))
                   (pcase-dolist (`(,k ,v) '((a 1) (b 2) (c 3)))
                     (setq result (cons v result)))
                   result)"
            ),
            "OK (3 2 1)"
        );
    }

    // =======================================================================
    // 21. Combined and + pred + binding
    // =======================================================================

    #[test]
    fn pcase_and_pred_bind() {
        assert_eq!(
            eval_last(
                "(pcase 42
                   ((and (pred integerp) (pred (lambda (n) (> n 10))) x)
                    (+ x 1)))"
            ),
            "OK 43"
        );
    }

    // =======================================================================
    // 22. or with bindings from first match
    // =======================================================================

    #[test]
    fn pcase_or_binds_from_first_match() {
        assert_eq!(
            eval_last(
                "(pcase 2
                   ((or 1 2 3) 'found)
                   (_ 'other))"
            ),
            "OK found"
        );
    }

    // =======================================================================
    // 23. Nested pcase
    // =======================================================================

    #[test]
    fn pcase_nested() {
        assert_eq!(
            eval_last(
                "(pcase '(1 2)
                   (`(,a ,b)
                    (pcase a
                      (1 (+ b 100))
                      (_ 0))))"
            ),
            "OK 102"
        );
    }

    // =======================================================================
    // 24. pred with lambda
    // =======================================================================

    #[test]
    fn pcase_pred_lambda() {
        assert_eq!(
            eval_last(
                "(pcase 42
                   ((pred (lambda (x) (= x 42))) 'bingo)
                   (_ 'nope))"
            ),
            "OK bingo"
        );
    }

    // =======================================================================
    // 25. Keyword literal pattern
    // =======================================================================

    #[test]
    fn pcase_keyword_literal() {
        assert_eq!(eval_last("(pcase :foo (:foo 'yes) (_ 'no))"), "OK yes");
    }

    // =======================================================================
    // 26. Float literal pattern
    // =======================================================================

    #[test]
    fn pcase_float_literal() {
        assert_eq!(eval_last("(pcase 3.14 (3.14 'pi) (_ 'other))"), "OK pi");
    }

    // =======================================================================
    // 27. Backquote with literal head and binding tail
    // =======================================================================

    #[test]
    fn pcase_backquote_mixed() {
        assert_eq!(
            eval_last("(pcase '(add 3 4) (`(add ,a ,b) (+ a b)))"),
            "OK 7"
        );
    }

    // =======================================================================
    // 28. pcase with evaluated expression
    // =======================================================================

    #[test]
    fn pcase_evaluates_expression() {
        assert_eq!(
            eval_last("(pcase (+ 1 2) (3 'three) (_ 'other))"),
            "OK three"
        );
    }

    // =======================================================================
    // 29. pcase-let with empty bindings
    // =======================================================================

    #[test]
    fn pcase_let_empty_bindings() {
        assert_eq!(eval_last("(pcase-let () 42)"), "OK 42");
    }

    // =======================================================================
    // 30. pcase-let* with empty bindings
    // =======================================================================

    #[test]
    fn pcase_let_star_empty_bindings() {
        assert_eq!(eval_last("(pcase-let* () 42)"), "OK 42");
    }

    // =======================================================================
    // 31. pcase-dolist returns nil
    // =======================================================================

    #[test]
    fn pcase_dolist_returns_nil() {
        assert_eq!(eval_last("(pcase-dolist (x '(1 2 3)) x)"), "OK nil");
    }

    #[test]
    fn pcase_dolist_returns_result_expression() {
        assert_eq!(
            eval_last(
                "(let ((acc nil))
                   (pcase-dolist (x '(1 2) (nreverse acc))
                     (setq acc (cons x acc))))"
            ),
            "OK (1 2)"
        );
    }

    #[test]
    fn pcase_dolist_runtime_errors_match_oracle_edges() {
        let results = eval_all(
            "(condition-case err (pcase-dolist (x) x) (error err))
             (condition-case err (pcase-dolist (x '(1 2) :r :extra) x) (error err))
             (condition-case err (pcase-dolist (x '(1) . z) x) (error err))
             (condition-case err (pcase-dolist (x 1) x) (error err))
             (condition-case err (pcase-dolist (x '(1 . 2)) x) (error err))",
        );
        assert_eq!(
            results,
            vec![
                "OK (wrong-number-of-arguments (2 . 3) 1)",
                "OK (wrong-number-of-arguments (2 . 3) 4)",
                "OK (wrong-type-argument listp z)",
                "OK (wrong-type-argument listp 1)",
                "OK (wrong-type-argument listp 2)",
            ]
        );
    }

    #[test]
    fn pcase_dolist_mismatch_still_executes_body_with_nil_bindings() {
        assert_eq!(
            eval_last(
                "(let ((vals nil))
                   (pcase-dolist (`(,a ,b) '(1 (2 3)) vals)
                     (setq vals (cons (list a b) vals))))"
            ),
            "OK ((2 3) (nil nil))"
        );
    }

    #[test]
    fn pcase_dolist_partial_mismatch_preserves_available_bindings() {
        assert_eq!(
            eval_last(
                "(let ((vals nil))
                   (pcase-dolist (`(,a ,b) '((1 . 2)) vals)
                     (setq vals (cons (list a b) vals))))"
            ),
            "OK ((1 nil))"
        );
    }

    #[test]
    fn pcase_dolist_predicate_mismatch_still_executes_body() {
        assert_eq!(
            eval_last("(let ((n 0)) (pcase-dolist ((pred numberp) '(a 1) n) (setq n (1+ n))))"),
            "OK 2"
        );
    }

    // =======================================================================
    // 32. Complex nested backquote with mixed literals and bindings
    // =======================================================================

    #[test]
    fn pcase_backquote_complex_nested() {
        assert_eq!(
            eval_last(
                "(pcase '(defun foo (x) (+ x 1))
                   (`(defun ,name ,args . ,body)
                    (list name args body)))"
            ),
            "OK (foo (x) ((+ x 1)))"
        );
    }

    // =======================================================================
    // 33. guard pattern referencing previously bound variable
    // =======================================================================

    #[test]
    fn pcase_and_bind_then_guard() {
        assert_eq!(
            eval_last(
                "(pcase 42
                   ((and x (guard (> x 40))) 'big)
                   (x 'small))"
            ),
            "OK big"
        );
    }

    #[test]
    fn pcase_and_bind_then_guard_fails() {
        assert_eq!(
            eval_last(
                "(pcase 3
                   ((and x (guard (> x 40))) 'big)
                   (x 'small))"
            ),
            "OK small"
        );
    }

    // =======================================================================
    // 34. app with cdr
    // =======================================================================

    #[test]
    fn pcase_app_cdr_binding() {
        assert_eq!(
            eval_last("(pcase '(1 2 3) ((app cdr rest) rest))"),
            "OK (2 3)"
        );
    }

    // =======================================================================
    // 35. pcase-let with vector destructuring
    // =======================================================================

    #[test]
    fn pcase_let_vector_destructure() {
        assert_eq!(
            eval_last("(pcase-let ((`[,a ,b ,c] [10 20 30])) (+ a b c))"),
            "OK 60"
        );
    }
}
