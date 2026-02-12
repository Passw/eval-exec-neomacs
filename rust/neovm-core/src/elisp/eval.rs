//! Evaluator — special forms, function application, and dispatch.

use std::collections::HashMap;

use super::builtins;
use super::error::*;
use super::expr::Expr;
use super::value::*;

/// The Elisp evaluator.
pub struct Evaluator {
    /// Global variable bindings.
    pub(crate) globals: HashMap<String, Value>,
    /// Function namespace (separate from variables, like Lisp-2).
    pub(crate) functions: HashMap<String, Value>,
    /// Dynamic binding stack (each frame is one `let`/function call scope).
    pub(crate) dynamic: Vec<HashMap<String, Value>>,
    /// Macro namespace.
    pub(crate) macros: HashMap<String, Value>,
    /// Recursion depth counter.
    depth: usize,
    /// Maximum recursion depth.
    max_depth: usize,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("t".to_string(), Value::True);
        globals.insert("nil".to_string(), Value::Nil);

        Self {
            globals,
            functions: HashMap::new(),
            dynamic: Vec::new(),
            macros: HashMap::new(),
            depth: 0,
            max_depth: 200,
        }
    }

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, EvalError> {
        self.eval(expr).map_err(map_flow)
    }

    pub fn eval_forms(&mut self, forms: &[Expr]) -> Vec<Result<Value, EvalError>> {
        forms.iter().map(|form| self.eval_expr(form)).collect()
    }

    /// Set a global variable.
    pub fn set_variable(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    /// Set a function binding.
    pub fn set_function(&mut self, name: &str, value: Value) {
        self.functions.insert(name.to_string(), value);
    }

    // -----------------------------------------------------------------------
    // Core eval
    // -----------------------------------------------------------------------

    pub(crate) fn eval(&mut self, expr: &Expr) -> EvalResult {
        self.depth += 1;
        if self.depth > self.max_depth {
            self.depth -= 1;
            return Err(signal("excessive-lisp-nesting", vec![Value::Int(self.max_depth as i64)]));
        }
        let result = self.eval_inner(expr);
        self.depth -= 1;
        result
    }

    fn eval_inner(&mut self, expr: &Expr) -> EvalResult {
        match expr {
            Expr::Int(v) => Ok(Value::Int(*v)),
            Expr::Float(v) => Ok(Value::Float(*v)),
            Expr::Str(s) => Ok(Value::string(s.clone())),
            Expr::Char(c) => Ok(Value::Char(*c)),
            Expr::Keyword(s) => Ok(Value::Keyword(s.clone())),
            Expr::Bool(true) => Ok(Value::True),
            Expr::Bool(false) => Ok(Value::Nil),
            Expr::Vector(items) => {
                let mut vals = Vec::with_capacity(items.len());
                for item in items {
                    vals.push(self.eval(item)?);
                }
                Ok(Value::vector(vals))
            }
            Expr::Symbol(symbol) => self.eval_symbol(symbol),
            Expr::List(items) => self.eval_list(items),
            Expr::DottedList(items, last) => {
                // Evaluate as a list call, ignoring dotted cdr
                // (This is for `(func a b . rest)` style, which in practice
                //  means the dotted pair is rarely used in function calls)
                let _ = last;
                self.eval_list(items)
            }
        }
    }

    fn eval_symbol(&self, symbol: &str) -> EvalResult {
        if symbol == "nil" {
            return Ok(Value::Nil);
        }
        if symbol == "t" {
            return Ok(Value::True);
        }
        // Keywords evaluate to themselves
        if symbol.starts_with(':') {
            return Ok(Value::Keyword(symbol.to_string()));
        }

        // Dynamic scope lookup (inner to outer)
        for frame in self.dynamic.iter().rev() {
            if let Some(value) = frame.get(symbol) {
                return Ok(value.clone());
            }
        }

        if let Some(value) = self.globals.get(symbol) {
            return Ok(value.clone());
        }

        Err(signal("void-variable", vec![Value::symbol(symbol)]))
    }

    fn eval_list(&mut self, items: &[Expr]) -> EvalResult {
        let Some((head, tail)) = items.split_first() else {
            return Ok(Value::Nil);
        };

        if let Expr::Symbol(name) = head {
            // Check for macro expansion first
            if let Some(macro_val) = self.macros.get(name).cloned() {
                let expanded = self.expand_macro(macro_val, tail)?;
                return self.eval(&expanded);
            }

            // Special forms
            if let Some(result) = self.try_special_form(name, tail) {
                return result;
            }

            // Regular function call — evaluate args then dispatch
            let mut args = Vec::with_capacity(tail.len());
            for expr in tail {
                args.push(self.eval(expr)?);
            }

            // Try builtin dispatch first
            if let Some(result) = builtins::dispatch_builtin(self, name, args.clone()) {
                return result;
            }

            // Try function namespace
            if let Some(func) = self.functions.get(name).cloned() {
                return self.apply(func, args);
            }

            return Err(signal("void-function", vec![Value::symbol(name.clone())]));
        }

        // Head is a list (possibly a lambda expression)
        if let Expr::List(lambda_form) = head {
            if let Some(Expr::Symbol(s)) = lambda_form.first() {
                if s == "lambda" {
                    let func = self.eval_lambda(&lambda_form[1..])?;
                    let mut args = Vec::with_capacity(tail.len());
                    for expr in tail {
                        args.push(self.eval(expr)?);
                    }
                    return self.apply(func, args);
                }
            }
        }

        Err(signal("invalid-function", vec![quote_to_value(head)]))
    }

    // -----------------------------------------------------------------------
    // Special forms
    // -----------------------------------------------------------------------

    fn try_special_form(&mut self, name: &str, tail: &[Expr]) -> Option<EvalResult> {
        Some(match name {
            "quote" => self.sf_quote(tail),
            "function" => self.sf_function(tail),
            "let" => self.sf_let(tail),
            "let*" => self.sf_let_star(tail),
            "setq" => self.sf_setq(tail),
            "if" => self.sf_if(tail),
            "and" => self.sf_and(tail),
            "or" => self.sf_or(tail),
            "cond" => self.sf_cond(tail),
            "while" => self.sf_while(tail),
            "progn" => self.sf_progn(tail),
            "prog1" => self.sf_prog1(tail),
            "lambda" => self.eval_lambda(tail),
            "defun" => self.sf_defun(tail),
            "defvar" => self.sf_defvar(tail),
            "defconst" => self.sf_defconst(tail),
            "defmacro" => self.sf_defmacro(tail),
            "funcall" => self.sf_funcall(tail),
            "catch" => self.sf_catch(tail),
            "throw" => self.sf_throw(tail),
            "unwind-protect" => self.sf_unwind_protect(tail),
            "condition-case" => self.sf_condition_case(tail),
            "interactive" => Ok(Value::Nil), // Stub: ignored for now
            "declare" => Ok(Value::Nil),     // Stub: ignored for now
            "when" => self.sf_when(tail),
            "unless" => self.sf_unless(tail),
            _ => return None,
        })
    }

    fn sf_quote(&self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 1 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        Ok(quote_to_value(&tail[0]))
    }

    fn sf_function(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 1 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        match &tail[0] {
            Expr::Symbol(name) => {
                // #'symbol — look up in function namespace
                if let Some(func) = self.functions.get(name).cloned() {
                    Ok(func)
                } else if builtins::dispatch_builtin(self, name, vec![]).is_some() {
                    // It's a known builtin — return as Subr
                    Ok(Value::Subr(name.clone()))
                } else {
                    Ok(Value::Subr(name.clone()))
                }
            }
            Expr::List(items) => {
                // #'(lambda ...) — create closure
                if let Some(Expr::Symbol(s)) = items.first() {
                    if s == "lambda" {
                        return self.eval_lambda(&items[1..]);
                    }
                }
                Err(signal("invalid-function", vec![]))
            }
            _ => Err(signal("invalid-function", vec![])),
        }
    }

    fn sf_let(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }

        let mut bindings = HashMap::new();
        match &tail[0] {
            Expr::List(entries) => {
                for binding in entries {
                    match binding {
                        Expr::Symbol(name) => {
                            bindings.insert(name.clone(), Value::Nil);
                        }
                        Expr::List(pair) if !pair.is_empty() => {
                            let Expr::Symbol(name) = &pair[0] else {
                                return Err(signal("wrong-type-argument", vec![]));
                            };
                            let value = if pair.len() > 1 {
                                self.eval(&pair[1])?
                            } else {
                                Value::Nil
                            };
                            bindings.insert(name.clone(), value);
                        }
                        _ => return Err(signal("wrong-type-argument", vec![])),
                    }
                }
            }
            Expr::Symbol(s) if s == "nil" => {} // (let nil ...)
            _ => return Err(signal("wrong-type-argument", vec![])),
        }

        self.dynamic.push(bindings);
        let result = self.sf_progn(&tail[1..]);
        self.dynamic.pop();
        result
    }

    fn sf_let_star(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }

        let entries = match &tail[0] {
            Expr::List(entries) => entries.clone(),
            Expr::Symbol(s) if s == "nil" => Vec::new(),
            _ => return Err(signal("wrong-type-argument", vec![])),
        };

        self.dynamic.push(HashMap::new());
        for binding in &entries {
            match binding {
                Expr::Symbol(name) => {
                    if let Some(frame) = self.dynamic.last_mut() {
                        frame.insert(name.clone(), Value::Nil);
                    }
                }
                Expr::List(pair) if !pair.is_empty() => {
                    let Expr::Symbol(name) = &pair[0] else {
                        self.dynamic.pop();
                        return Err(signal("wrong-type-argument", vec![]));
                    };
                    let value = if pair.len() > 1 {
                        match self.eval(&pair[1]) {
                            Ok(v) => v,
                            Err(e) => {
                                self.dynamic.pop();
                                return Err(e);
                            }
                        }
                    } else {
                        Value::Nil
                    };
                    if let Some(frame) = self.dynamic.last_mut() {
                        frame.insert(name.clone(), value);
                    }
                }
                _ => {
                    self.dynamic.pop();
                    return Err(signal("wrong-type-argument", vec![]));
                }
            }
        }

        let result = self.sf_progn(&tail[1..]);
        self.dynamic.pop();
        result
    }

    fn sf_setq(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Ok(Value::Nil);
        }
        if tail.len() % 2 != 0 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }

        let mut last = Value::Nil;
        let mut i = 0;
        while i < tail.len() {
            let Expr::Symbol(name) = &tail[i] else {
                return Err(signal("wrong-type-argument", vec![]));
            };
            let value = self.eval(&tail[i + 1])?;
            self.assign(name, value.clone());
            last = value;
            i += 2;
        }
        Ok(last)
    }

    fn sf_if(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 2 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let cond = self.eval(&tail[0])?;
        if cond.is_truthy() {
            self.eval(&tail[1])
        } else {
            self.sf_progn(&tail[2..])
        }
    }

    fn sf_and(&mut self, tail: &[Expr]) -> EvalResult {
        let mut last = Value::True;
        for expr in tail {
            last = self.eval(expr)?;
            if last.is_nil() {
                return Ok(Value::Nil);
            }
        }
        Ok(last)
    }

    fn sf_or(&mut self, tail: &[Expr]) -> EvalResult {
        for expr in tail {
            let val = self.eval(expr)?;
            if val.is_truthy() {
                return Ok(val);
            }
        }
        Ok(Value::Nil)
    }

    fn sf_cond(&mut self, tail: &[Expr]) -> EvalResult {
        for clause in tail {
            let Expr::List(items) = clause else {
                return Err(signal("wrong-type-argument", vec![]));
            };
            if items.is_empty() {
                continue;
            }
            let test = self.eval(&items[0])?;
            if test.is_truthy() {
                if items.len() == 1 {
                    return Ok(test);
                }
                return self.sf_progn(&items[1..]);
            }
        }
        Ok(Value::Nil)
    }

    fn sf_while(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        loop {
            let cond = self.eval(&tail[0])?;
            if cond.is_nil() {
                return Ok(Value::Nil);
            }
            self.sf_progn(&tail[1..])?;
        }
    }

    pub(crate) fn sf_progn(&mut self, forms: &[Expr]) -> EvalResult {
        let mut last = Value::Nil;
        for form in forms {
            last = self.eval(form)?;
        }
        Ok(last)
    }

    fn sf_prog1(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let first = self.eval(&tail[0])?;
        for form in &tail[1..] {
            self.eval(form)?;
        }
        Ok(first)
    }

    fn sf_when(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let cond = self.eval(&tail[0])?;
        if cond.is_truthy() {
            self.sf_progn(&tail[1..])
        } else {
            Ok(Value::Nil)
        }
    }

    fn sf_unless(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let cond = self.eval(&tail[0])?;
        if cond.is_nil() {
            self.sf_progn(&tail[1..])
        } else {
            Ok(Value::Nil)
        }
    }

    fn sf_defun(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 3 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        let lambda = self.eval_lambda(&tail[1..])?;
        self.functions.insert(name.clone(), lambda);
        Ok(Value::symbol(name.clone()))
    }

    fn sf_defvar(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        // Only set if not already bound
        if !self.globals.contains_key(name) {
            let value = if tail.len() > 1 {
                self.eval(&tail[1])?
            } else {
                Value::Nil
            };
            self.globals.insert(name.clone(), value);
        }
        Ok(Value::symbol(name.clone()))
    }

    fn sf_defconst(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 2 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        let value = self.eval(&tail[1])?;
        self.globals.insert(name.clone(), value);
        Ok(Value::symbol(name.clone()))
    }

    fn sf_defmacro(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 3 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let Expr::Symbol(name) = &tail[0] else {
            return Err(signal("wrong-type-argument", vec![]));
        };
        let params = self.parse_lambda_params(&tail[1])?;
        let body = tail[2..].to_vec();
        let macro_val = Value::Macro(std::sync::Arc::new(LambdaData {
            params,
            body,
            env: None,
            docstring: None,
        }));
        self.macros.insert(name.clone(), macro_val);
        Ok(Value::symbol(name.clone()))
    }

    fn sf_funcall(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let function = self.eval(&tail[0])?;
        let mut args = Vec::with_capacity(tail.len().saturating_sub(1));
        for expr in &tail[1..] {
            args.push(self.eval(expr)?);
        }
        self.apply(function, args)
    }

    fn sf_catch(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let tag = self.eval(&tail[0])?;
        match self.sf_progn(&tail[1..]) {
            Ok(value) => Ok(value),
            Err(Flow::Throw {
                tag: thrown_tag,
                value,
            }) if eq_value(&tag, &thrown_tag) => Ok(value),
            Err(flow) => Err(flow),
        }
    }

    fn sf_throw(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() != 2 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let tag = self.eval(&tail[0])?;
        let value = self.eval(&tail[1])?;
        Err(Flow::Throw { tag, value })
    }

    fn sf_unwind_protect(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        let primary = self.eval(&tail[0]);
        let cleanup = self.sf_progn(&tail[1..]);
        match cleanup {
            Ok(_) => primary,
            Err(flow) => Err(flow),
        }
    }

    fn sf_condition_case(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.len() < 3 {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }

        let var = match &tail[0] {
            Expr::Symbol(name) => name.clone(),
            _ => return Err(signal("wrong-type-argument", vec![])),
        };
        let body = &tail[1];
        let handlers = &tail[2..];

        match self.eval(body) {
            Ok(value) => Ok(value),
            Err(Flow::Signal(sig)) => {
                for handler in handlers {
                    let Expr::List(handler_items) = handler else {
                        return Err(signal("wrong-type-argument", vec![]));
                    };
                    if handler_items.is_empty() {
                        return Err(signal("wrong-type-argument", vec![]));
                    }

                    if signal_matches(&handler_items[0], &sig.symbol) {
                        let mut frame = HashMap::new();
                        if var != "nil" {
                            frame.insert(var.clone(), make_signal_binding_value(&sig));
                        }
                        self.dynamic.push(frame);
                        let result = self.sf_progn(&handler_items[1..]);
                        self.dynamic.pop();
                        return result;
                    }
                }
                Err(Flow::Signal(sig))
            }
            Err(flow) => Err(flow),
        }
    }

    // -----------------------------------------------------------------------
    // Lambda / Function application
    // -----------------------------------------------------------------------

    pub(crate) fn eval_lambda(&mut self, tail: &[Expr]) -> EvalResult {
        if tail.is_empty() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }

        let params = self.parse_lambda_params(&tail[0])?;

        // Skip docstring if present
        let body_start = if tail.len() > 2 {
            if let Expr::Str(_) = &tail[1] { 2 } else { 1 }
        } else {
            1
        };

        Ok(Value::Lambda(std::sync::Arc::new(LambdaData {
            params,
            body: tail[body_start..].to_vec(),
            env: None,
            docstring: None,
        })))
    }

    fn parse_lambda_params(&self, expr: &Expr) -> Result<LambdaParams, Flow> {
        match expr {
            Expr::Symbol(s) if s == "nil" => Ok(LambdaParams::simple(vec![])),
            Expr::List(items) => {
                let mut required = Vec::new();
                let mut optional = Vec::new();
                let mut rest = None;
                let mut mode = 0; // 0=required, 1=optional, 2=rest

                for item in items {
                    let Expr::Symbol(name) = item else {
                        return Err(signal("wrong-type-argument", vec![]));
                    };
                    match name.as_str() {
                        "&optional" => { mode = 1; continue; }
                        "&rest" => { mode = 2; continue; }
                        _ => {}
                    }
                    match mode {
                        0 => required.push(name.clone()),
                        1 => optional.push(name.clone()),
                        2 => { rest = Some(name.clone()); break; }
                        _ => unreachable!(),
                    }
                }

                Ok(LambdaParams { required, optional, rest })
            }
            _ => Err(signal("wrong-type-argument", vec![])),
        }
    }

    /// Apply a function value to evaluated arguments.
    pub(crate) fn apply(&mut self, function: Value, args: Vec<Value>) -> EvalResult {
        match function {
            Value::Lambda(lambda) | Value::Macro(lambda) => {
                self.apply_lambda(&lambda, args)
            }
            Value::Subr(name) => {
                // Try function namespace first to avoid consuming args
                if let Some(func) = self.functions.get(&name).cloned() {
                    return self.apply(func, args);
                }
                if let Some(result) = builtins::dispatch_builtin(self, &name, args) {
                    result
                } else {
                    Err(signal("void-function", vec![Value::symbol(name)]))
                }
            }
            Value::Symbol(name) => {
                // Symbol used as function — look up in function namespace
                if let Some(func) = self.functions.get(&name).cloned() {
                    self.apply(func, args)
                } else if let Some(result) = builtins::dispatch_builtin(self, &name, args) {
                    result
                } else {
                    Err(signal("void-function", vec![Value::symbol(name)]))
                }
            }
            _ => Err(signal("invalid-function", vec![function])),
        }
    }

    fn apply_lambda(&mut self, lambda: &LambdaData, args: Vec<Value>) -> EvalResult {
        let params = &lambda.params;

        // Arity check
        if args.len() < params.min_arity() {
            return Err(signal("wrong-number-of-arguments", vec![]));
        }
        if let Some(max) = params.max_arity() {
            if args.len() > max {
                return Err(signal("wrong-number-of-arguments", vec![]));
            }
        }

        let mut frame = HashMap::new();
        let mut arg_idx = 0;

        // Required params
        for param in &params.required {
            frame.insert(param.clone(), args[arg_idx].clone());
            arg_idx += 1;
        }

        // Optional params
        for param in &params.optional {
            if arg_idx < args.len() {
                frame.insert(param.clone(), args[arg_idx].clone());
                arg_idx += 1;
            } else {
                frame.insert(param.clone(), Value::Nil);
            }
        }

        // Rest param
        if let Some(ref rest_name) = params.rest {
            let rest_args: Vec<Value> = args[arg_idx..].to_vec();
            frame.insert(rest_name.clone(), Value::list(rest_args));
        }

        self.dynamic.push(frame);
        let result = self.sf_progn(&lambda.body);
        self.dynamic.pop();
        result
    }

    // -----------------------------------------------------------------------
    // Macro expansion
    // -----------------------------------------------------------------------

    fn expand_macro(&mut self, macro_val: Value, args: &[Expr]) -> Result<Expr, Flow> {
        let Value::Macro(lambda) = macro_val else {
            return Err(signal("invalid-macro", vec![]));
        };

        // Convert unevaluated args to values (quoted forms)
        let arg_values: Vec<Value> = args.iter().map(quote_to_value).collect();

        // Apply the macro body
        let expanded_value = self.apply_lambda(&lambda, arg_values)?;

        // Convert value back to expr for re-evaluation
        Ok(value_to_expr(&expanded_value))
    }

    // -----------------------------------------------------------------------
    // Variable assignment
    // -----------------------------------------------------------------------

    pub(crate) fn assign(&mut self, name: &str, value: Value) {
        // Search dynamic frames (inner to outer)
        for frame in self.dynamic.iter_mut().rev() {
            if frame.contains_key(name) {
                frame.insert(name.to_string(), value);
                return;
            }
        }
        self.globals.insert(name.to_string(), value);
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert an Expr AST node to a Value (for quote).
pub fn quote_to_value(expr: &Expr) -> Value {
    match expr {
        Expr::Int(v) => Value::Int(*v),
        Expr::Float(v) => Value::Float(*v),
        Expr::Str(s) => Value::string(s.clone()),
        Expr::Char(c) => Value::Char(*c),
        Expr::Keyword(s) => Value::Keyword(s.clone()),
        Expr::Bool(true) => Value::True,
        Expr::Bool(false) => Value::Nil,
        Expr::Symbol(s) if s == "nil" => Value::Nil,
        Expr::Symbol(s) if s == "t" => Value::True,
        Expr::Symbol(s) => Value::Symbol(s.clone()),
        Expr::List(items) => {
            let quoted = items.iter().map(quote_to_value).collect::<Vec<_>>();
            Value::list(quoted)
        }
        Expr::DottedList(items, last) => {
            let head_vals: Vec<Value> = items.iter().map(quote_to_value).collect();
            let tail_val = quote_to_value(last);
            head_vals.into_iter().rev().fold(tail_val, |acc, item| {
                Value::cons(item, acc)
            })
        }
        Expr::Vector(items) => {
            let vals = items.iter().map(quote_to_value).collect();
            Value::vector(vals)
        }
    }
}

/// Convert a Value back to an Expr (for macro expansion).
fn value_to_expr(value: &Value) -> Expr {
    match value {
        Value::Nil => Expr::Symbol("nil".into()),
        Value::True => Expr::Symbol("t".into()),
        Value::Int(n) => Expr::Int(*n),
        Value::Float(f) => Expr::Float(*f),
        Value::Symbol(s) => Expr::Symbol(s.clone()),
        Value::Keyword(s) => Expr::Keyword(s.clone()),
        Value::Str(s) => Expr::Str((**s).clone()),
        Value::Char(c) => Expr::Char(*c),
        Value::Cons(_) => {
            if let Some(items) = list_to_vec(value) {
                Expr::List(items.iter().map(value_to_expr).collect())
            } else {
                // Improper list — best effort
                Expr::Symbol(format!("{}", value))
            }
        }
        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            Expr::Vector(items.iter().map(value_to_expr).collect())
        }
        _ => Expr::Symbol(format!("{}", value)),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::{parse_forms, format_eval_result};

    fn eval_one(src: &str) -> String {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        let result = ev.eval_expr(&forms[0]);
        format_eval_result(&result)
    }

    fn eval_all(src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        ev.eval_forms(&forms).iter().map(format_eval_result).collect()
    }

    #[test]
    fn basic_arithmetic() {
        assert_eq!(eval_one("(+ 1 2)"), "OK 3");
        assert_eq!(eval_one("(- 10 3)"), "OK 7");
        assert_eq!(eval_one("(* 4 5)"), "OK 20");
        assert_eq!(eval_one("(/ 10 3)"), "OK 3");
        assert_eq!(eval_one("(% 10 3)"), "OK 1");
        assert_eq!(eval_one("(1+ 5)"), "OK 6");
        assert_eq!(eval_one("(1- 5)"), "OK 4");
    }

    #[test]
    fn float_arithmetic() {
        assert_eq!(eval_one("(+ 1.0 2.0)"), "OK 3.0");
        assert_eq!(eval_one("(+ 1 2.0)"), "OK 3.0"); // int promoted to float
        assert_eq!(eval_one("(/ 10.0 3.0)"), "OK 3.3333333333333335");
    }

    #[test]
    fn comparisons() {
        assert_eq!(eval_one("(< 1 2)"), "OK t");
        assert_eq!(eval_one("(> 1 2)"), "OK nil");
        assert_eq!(eval_one("(= 3 3)"), "OK t");
        assert_eq!(eval_one("(<= 3 3)"), "OK t");
        assert_eq!(eval_one("(>= 5 3)"), "OK t");
        assert_eq!(eval_one("(/= 1 2)"), "OK t");
    }

    #[test]
    fn type_predicates() {
        assert_eq!(eval_one("(integerp 42)"), "OK t");
        assert_eq!(eval_one("(floatp 3.14)"), "OK t");
        assert_eq!(eval_one("(stringp \"hello\")"), "OK t");
        assert_eq!(eval_one("(symbolp 'foo)"), "OK t");
        assert_eq!(eval_one("(consp '(1 2))"), "OK t");
        assert_eq!(eval_one("(null nil)"), "OK t");
        assert_eq!(eval_one("(null t)"), "OK nil");
        assert_eq!(eval_one("(listp nil)"), "OK t");
    }

    #[test]
    fn string_operations() {
        assert_eq!(eval_one(r#"(concat "hello" " " "world")"#), r#"OK "hello world""#);
        assert_eq!(eval_one(r#"(substring "hello" 1 3)"#), r#"OK "el""#);
        assert_eq!(eval_one(r#"(length "hello")"#), "OK 5");
        assert_eq!(eval_one(r#"(upcase "hello")"#), r#"OK "HELLO""#);
        assert_eq!(eval_one(r#"(string-equal "abc" "abc")"#), "OK t");
    }

    #[test]
    fn and_or_cond() {
        assert_eq!(eval_one("(and 1 2 3)"), "OK 3");
        assert_eq!(eval_one("(and 1 nil 3)"), "OK nil");
        assert_eq!(eval_one("(or nil nil 3)"), "OK 3");
        assert_eq!(eval_one("(or nil nil nil)"), "OK nil");
        assert_eq!(eval_one("(cond (nil 1) (t 2))"), "OK 2");
    }

    #[test]
    fn while_loop() {
        assert_eq!(
            eval_one("(let ((x 0)) (while (< x 5) (setq x (1+ x))) x)"),
            "OK 5"
        );
    }

    #[test]
    fn defvar_only_sets_if_unbound() {
        let results = eval_all("(defvar x 42) x (defvar x 99) x");
        assert_eq!(results, vec!["OK x", "OK 42", "OK x", "OK 42"]);
    }

    #[test]
    fn defmacro_works() {
        let result = eval_all(
            "(defmacro my-when (cond &rest body)
               (list 'if cond (cons 'progn body)))
             (my-when t 1 2 3)",
        );
        assert_eq!(result[1], "OK 3");
    }

    #[test]
    fn optional_and_rest_params() {
        let results = eval_all(
            "(defun f (a &optional b &rest c) (list a b c))
             (f 1)
             (f 1 2)
             (f 1 2 3 4)"
        );
        assert_eq!(results[1], "OK (1 nil nil)");
        assert_eq!(results[2], "OK (1 2 nil)");
        assert_eq!(results[3], "OK (1 2 (3 4))");
    }

    #[test]
    fn when_unless() {
        assert_eq!(eval_one("(when t 1 2 3)"), "OK 3");
        assert_eq!(eval_one("(when nil 1 2 3)"), "OK nil");
        assert_eq!(eval_one("(unless nil 1 2 3)"), "OK 3");
        assert_eq!(eval_one("(unless t 1 2 3)"), "OK nil");
    }

    #[test]
    fn hash_table_ops() {
        let results = eval_all(
            "(let ((ht (make-hash-table :test 'equal)))
               (puthash \"key\" 42 ht)
               (gethash \"key\" ht))"
        );
        assert_eq!(results[0], "OK 42");
    }

    #[test]
    fn vector_ops() {
        assert_eq!(eval_one("(aref [10 20 30] 1)"), "OK 20");
        assert_eq!(eval_one("(length [1 2 3])"), "OK 3");
    }

    #[test]
    fn format_function() {
        assert_eq!(eval_one(r#"(format "hello %s, %d" "world" 42)"#), r#"OK "hello world, 42""#);
    }

    #[test]
    fn prog1() {
        assert_eq!(eval_one("(prog1 1 2 3)"), "OK 1");
    }

    #[test]
    fn function_special_form() {
        let results = eval_all(
            "(defun add1 (x) (+ x 1))
             (funcall #'add1 5)"
        );
        assert_eq!(results[1], "OK 6");
    }

    #[test]
    fn mapcar_works() {
        assert_eq!(eval_one("(mapcar #'1+ '(1 2 3))"), "OK (2 3 4)");
    }

    #[test]
    fn apply_works() {
        assert_eq!(eval_one("(apply #'+ '(1 2 3))"), "OK 6");
        assert_eq!(eval_one("(apply #'+ 1 2 '(3))"), "OK 6");
    }

    #[test]
    fn backward_compat_core_forms() {
        // Same tests as original elisp.rs
        let source = r#"
        (+ 1 2)
        (let ((x 1)) (setq x (+ x 2)) x)
        (let ((lst '(1 2))) (setcar lst 9) lst)
        (catch 'tag (throw 'tag 42))
        (condition-case e (/ 1 0) (arith-error 'div-zero))
        (let ((x 1))
          (let ((f (lambda () x)))
            (let ((x 2))
              (funcall f))))
        "#;

        let forms = parse_forms(source).expect("parse");
        let mut ev = Evaluator::new();
        let rendered: Vec<String> = ev.eval_forms(&forms).iter().map(format_eval_result).collect();

        assert_eq!(
            rendered,
            vec!["OK 3", "OK 3", "OK (9 2)", "OK 42", "OK div-zero", "OK 2"]
        );
    }

    #[test]
    fn excessive_recursion_detected() {
        let results = eval_all("(defun inf () (inf))\n(inf)");
        // Second form should trigger excessive nesting
        assert!(results[1].contains("excessive-lisp-nesting"));
    }
}
