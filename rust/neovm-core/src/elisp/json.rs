//! JSON serialization and parsing builtins.
//!
//! Implements the Emacs JSON interface:
//! - `json-serialize` — convert Lisp value to JSON string
//! - `json-parse-string` — parse JSON string to Lisp value
//!
//! Key mapping (Emacs convention):
//! - Lisp nil / :null → JSON null
//! - Lisp t → JSON true
//! - Lisp :false / :json-false → JSON false
//! - Lisp integer/float → JSON number
//! - Lisp string → JSON string
//! - Lisp hash-table → JSON object
//! - Lisp vector → JSON array
//! - Lisp alist/plist → JSON object (when :object-type specifies)
//!
//! No external crate (serde_json etc.) is used — the parser and serializer
//! are implemented from scratch with simple recursive descent.

use super::error::{signal, EvalResult, Flow};
use super::value::*;

// ---------------------------------------------------------------------------
// Argument helpers
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Keyword argument parsing
// ---------------------------------------------------------------------------

/// Options that control how Lisp values are serialized to JSON.
#[derive(Clone, Debug)]
struct SerializeOpts {
    /// How nil is serialized.  In Emacs, `json-serialize` always maps nil to
    /// JSON `null`, but the keyword arg can override (:null-object).
    null_object: Value,
    /// The Lisp value that maps to JSON false.
    false_object: Value,
}

impl Default for SerializeOpts {
    fn default() -> Self {
        Self {
            null_object: Value::Nil,
            false_object: Value::Keyword(":false".to_string()),
        }
    }
}

/// Options that control how a JSON string is parsed into Lisp values.
#[derive(Clone, Debug)]
struct ParseOpts {
    /// How JSON objects are represented.
    object_type: ObjectType,
    /// How JSON arrays are represented.
    array_type: ArrayType,
    /// Lisp value for JSON null.
    null_object: Value,
    /// Lisp value for JSON false.
    false_object: Value,
}

impl Default for ParseOpts {
    fn default() -> Self {
        Self {
            object_type: ObjectType::HashTable,
            array_type: ArrayType::Vector,
            null_object: Value::Keyword(":null".to_string()),
            false_object: Value::Keyword(":false".to_string()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum ObjectType {
    HashTable,
    Alist,
    Plist,
}

#[derive(Clone, Debug, PartialEq)]
enum ArrayType {
    Vector,
    List,
}

/// Parse keyword arguments from the &rest tail (starting at `start_index`).
/// Returns `ParseOpts`.  Unknown keywords signal `json-error`.
fn parse_parse_kwargs(args: &[Value], start_index: usize) -> Result<ParseOpts, Flow> {
    let mut opts = ParseOpts::default();
    let rest = &args[start_index..];
    if rest.len() % 2 != 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("plistp"), Value::list(rest.to_vec())],
        ));
    }

    let mut i = 0;
    while i + 1 < rest.len() {
        let key = &rest[i];
        let value = &rest[i + 1];
        match key {
            Value::Keyword(k) if k == ":object-type" => match value {
                Value::Symbol(s) if s == "hash-table" => opts.object_type = ObjectType::HashTable,
                Value::Symbol(s) if s == "alist" => opts.object_type = ObjectType::Alist,
                Value::Symbol(s) if s == "plist" => opts.object_type = ObjectType::Plist,
                _ => {
                    return Err(signal(
                        "error",
                        vec![
                            Value::string("One of hash-table, alist or plist should be specified"),
                            value.clone(),
                        ],
                    ));
                }
            },
            Value::Keyword(k) if k == ":array-type" => match value {
                Value::Symbol(s) if s == "array" => opts.array_type = ArrayType::Vector,
                Value::Symbol(s) if s == "list" => opts.array_type = ArrayType::List,
                _ => {
                    return Err(signal(
                        "error",
                        vec![
                            Value::string("One of array or list should be specified"),
                            value.clone(),
                        ],
                    ));
                }
            },
            Value::Keyword(k) if k == ":null-object" => {
                opts.null_object = value.clone();
            }
            Value::Keyword(k) if k == ":false-object" => {
                opts.false_object = value.clone();
            }
            _ => {
                return Err(signal(
                    "error",
                    vec![
                        Value::string(
                            "One of :object-type, :array-type, :null-object or :false-object should be specified",
                        ),
                        value.clone(),
                    ],
                ));
            }
        }
        i += 2;
    }
    Ok(opts)
}

/// Parse keyword arguments relevant to `json-serialize` / `json-insert`.
fn parse_serialize_kwargs(args: &[Value], start_index: usize) -> Result<SerializeOpts, Flow> {
    let mut opts = SerializeOpts::default();
    let rest = &args[start_index..];
    if rest.len() % 2 != 0 {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("plistp"), Value::list(rest.to_vec())],
        ));
    }

    let mut i = 0;
    while i + 1 < rest.len() {
        let key = &rest[i];
        let value = &rest[i + 1];
        match key {
            Value::Keyword(k) if k == ":null-object" => {
                opts.null_object = value.clone();
            }
            Value::Keyword(k) if k == ":false-object" => {
                opts.false_object = value.clone();
            }
            _ => {
                return Err(signal(
                    "error",
                    vec![
                        Value::string("One of :null-object or :false-object should be specified"),
                        value.clone(),
                    ],
                ));
            }
        }
        i += 2;
    }
    Ok(opts)
}

// ===========================================================================
// JSON Serializer (Lisp → JSON string)
// ===========================================================================

/// Check if two Values are equivalent for the purpose of matching the
/// null/false sentinel objects.  Uses structural equality for simple types.
fn value_matches(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Nil, Value::Nil) => true,
        (Value::True, Value::True) => true,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x.to_bits() == y.to_bits(),
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Keyword(x), Value::Keyword(y)) => x == y,
        (Value::Str(x), Value::Str(y)) => **x == **y,
        (Value::Char(x), Value::Char(y)) => x == y,
        _ => false,
    }
}

/// Serialize a Lisp value to a JSON string.
fn serialize_to_json(value: &Value, opts: &SerializeOpts, depth: usize) -> Result<String, Flow> {
    if depth > 512 {
        return Err(signal(
            "json-serialize-error",
            vec![Value::string("Nesting too deep")],
        ));
    }

    // Check for null sentinel.
    if value_matches(value, &opts.null_object) {
        return Ok("null".to_string());
    }

    // Check for false sentinel.
    if value_matches(value, &opts.false_object) {
        return Ok("false".to_string());
    }

    match value {
        // t → true (checked after false sentinel, which is usually :false not t)
        Value::True => Ok("true".to_string()),

        Value::Int(n) => Ok(n.to_string()),

        Value::Float(f) => {
            if f.is_nan() || f.is_infinite() {
                return Err(signal(
                    "json-serialize-error",
                    vec![Value::string("Not a finite number")],
                ));
            }
            // JSON numbers must not have trailing dot, use full representation.
            if f.fract() == 0.0 && f.abs() < (i64::MAX as f64) {
                // Emit as integer-looking float (e.g. 1.0 → 1.0, not 1)
                // Actually JSON allows both; Emacs json-serialize emits "1.0"
                // for float 1.0.  We follow that convention.
                Ok(format!("{:.1}", f))
            } else {
                Ok(format!("{}", f))
            }
        }

        Value::Str(s) => Ok(json_encode_string(s)),

        Value::Vector(v) => {
            let items = v.lock().expect("poisoned");
            let mut parts = Vec::with_capacity(items.len());
            for item in items.iter() {
                parts.push(serialize_to_json(item, opts, depth + 1)?);
            }
            Ok(format!("[{}]", parts.join(",")))
        }

        Value::HashTable(ht) => {
            let table = ht.lock().expect("poisoned");
            let mut parts = Vec::with_capacity(table.data.len());
            for (key, val) in &table.data {
                let key_str = hash_key_to_string(key)?;
                let val_json = serialize_to_json(val, opts, depth + 1)?;
                parts.push(format!("{}:{}", json_encode_string(&key_str), val_json));
            }
            Ok(format!("{{{}}}", parts.join(",")))
        }

        // Alist: list of (KEY . VALUE) cons cells → JSON object.
        Value::Cons(_) => {
            let items = list_to_vec(value).ok_or_else(|| {
                signal(
                    "wrong-type-argument",
                    vec![Value::symbol("listp"), value.clone()],
                )
            })?;
            let mut parts = Vec::with_capacity(items.len());
            for item in &items {
                match item {
                    Value::Cons(cell) => {
                        let pair = cell.lock().expect("poisoned");
                        let key_str = symbol_object_key(&pair.car)?;
                        let val_json = serialize_to_json(&pair.cdr, opts, depth + 1)?;
                        parts.push(format!("{}:{}", json_encode_string(&key_str), val_json));
                    }
                    _ => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("consp"), item.clone()],
                        ));
                    }
                }
            }
            Ok(format!("{{{}}}", parts.join(",")))
        }

        // Nil was already checked as null_object above. If we reach here,
        // it means nil is NOT the null sentinel (user provided a custom
        // :null-object).  Emacs treats nil as JSON null regardless, but we
        // follow the sentinel logic.  Since nil is also an empty list, treat
        // it as an empty JSON object when it wasn't matched as null.
        Value::Nil => Ok("null".to_string()),

        // Keywords that were not matched as false/null sentinels.
        Value::Keyword(k) if k == ":json-false" => Ok("false".to_string()),
        Value::Keyword(k) if k == ":null" || k == ":json-null" => Ok("null".to_string()),

        other => Err(signal(
            "json-serialize-error",
            vec![Value::string(format!(
                "Value cannot be serialized to JSON: {}",
                super::print::print_value(other)
            ))],
        )),
    }
}

/// Convert a HashKey to a string suitable as a JSON object key.
fn hash_key_to_string(key: &HashKey) -> Result<String, Flow> {
    match key {
        HashKey::Str(s) => Ok(s.clone()),
        HashKey::Symbol(s) => Ok(s.clone()),
        HashKey::Keyword(s) => {
            // Strip leading colon if present.
            if let Some(stripped) = s.strip_prefix(':') {
                Ok(stripped.to_string())
            } else {
                Ok(s.clone())
            }
        }
        HashKey::Int(n) => Ok(n.to_string()),
        HashKey::Nil => Ok("nil".to_string()),
        HashKey::True => Ok("t".to_string()),
        _ => Err(signal(
            "json-serialize-error",
            vec![Value::string(
                "Hash table key cannot be converted to JSON object key",
            )],
        )),
    }
}

/// Convert a Lisp value to a string key for a JSON object (used when
/// serializing alists).
///
/// Emacs `json-serialize` expects symbol keys in alists.
fn symbol_object_key(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Symbol(s) => Ok(s.clone()),
        Value::Keyword(s) => Ok(s.clone()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("symbolp"), other.clone()],
        )),
    }
}

/// Encode a Rust string as a JSON string with proper escaping.
fn json_encode_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\x08' => out.push_str("\\b"),
            '\x0C' => out.push_str("\\f"),
            c if (c as u32) < 0x20 => {
                // Control characters: emit \u00XX.
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

// ===========================================================================
// JSON Parser (JSON string → Lisp value)
// ===========================================================================

/// Parser state: a cursor over the input bytes.
struct JsonParser<'a> {
    input: &'a [u8],
    pos: usize,
    opts: ParseOpts,
}

impl<'a> JsonParser<'a> {
    fn new(input: &'a str, opts: ParseOpts) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
            opts,
        }
    }

    /// Current byte (or None if at end).
    fn peek(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    /// Advance by one byte.
    fn advance(&mut self) {
        self.pos += 1;
    }

    /// Skip whitespace.
    fn skip_ws(&mut self) {
        while let Some(b) = self.peek() {
            if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Consume a specific byte, or signal error.
    fn expect_byte(&mut self, expected: u8) -> Result<(), Flow> {
        self.skip_ws();
        match self.peek() {
            Some(b) if b == expected => {
                self.advance();
                Ok(())
            }
            Some(b) => Err(signal(
                "json-parse-error",
                vec![Value::string(format!(
                    "Expected '{}', got '{}' at position {}",
                    expected as char, b as char, self.pos
                ))],
            )),
            None => Err(signal(
                "json-parse-error",
                vec![Value::string(format!(
                    "Unexpected end of input, expected '{}'",
                    expected as char
                ))],
            )),
        }
    }

    /// Parse one JSON value.
    fn parse_value(&mut self) -> Result<Value, Flow> {
        self.skip_ws();
        match self.peek() {
            None => Err(signal(
                "json-parse-error",
                vec![Value::string("Unexpected end of input")],
            )),
            Some(b'"') => self.parse_string(),
            Some(b'{') => self.parse_object(),
            Some(b'[') => self.parse_array(),
            Some(b't') => self.parse_true(),
            Some(b'f') => self.parse_false(),
            Some(b'n') => self.parse_null(),
            Some(b) if b == b'-' || b.is_ascii_digit() => self.parse_number(),
            Some(b) => Err(signal(
                "json-parse-error",
                vec![Value::string(format!(
                    "Unexpected character '{}' at position {}",
                    b as char, self.pos
                ))],
            )),
        }
    }

    /// Parse `true`.
    fn parse_true(&mut self) -> Result<Value, Flow> {
        self.expect_literal(b"true")?;
        Ok(Value::True)
    }

    /// Parse `false`.
    fn parse_false(&mut self) -> Result<Value, Flow> {
        self.expect_literal(b"false")?;
        Ok(self.opts.false_object.clone())
    }

    /// Parse `null`.
    fn parse_null(&mut self) -> Result<Value, Flow> {
        self.expect_literal(b"null")?;
        Ok(self.opts.null_object.clone())
    }

    /// Expect an exact byte sequence.
    fn expect_literal(&mut self, literal: &[u8]) -> Result<(), Flow> {
        for &expected in literal {
            match self.peek() {
                Some(b) if b == expected => self.advance(),
                _ => {
                    return Err(signal(
                        "json-parse-error",
                        vec![Value::string(format!(
                            "Expected '{}' at position {}",
                            std::str::from_utf8(literal).unwrap_or("?"),
                            self.pos
                        ))],
                    ));
                }
            }
        }
        Ok(())
    }

    /// Parse a JSON string (opening `"` has not been consumed yet).
    fn parse_string(&mut self) -> Result<Value, Flow> {
        let s = self.parse_string_raw()?;
        Ok(Value::string(s))
    }

    /// Parse a JSON string and return the raw Rust String.
    fn parse_string_raw(&mut self) -> Result<String, Flow> {
        self.expect_byte(b'"')?;
        let mut result = String::new();
        loop {
            match self.peek() {
                None => {
                    return Err(signal(
                        "json-parse-error",
                        vec![Value::string("Unterminated string")],
                    ));
                }
                Some(b'"') => {
                    self.advance();
                    return Ok(result);
                }
                Some(b'\\') => {
                    self.advance();
                    match self.peek() {
                        Some(b'"') => {
                            self.advance();
                            result.push('"');
                        }
                        Some(b'\\') => {
                            self.advance();
                            result.push('\\');
                        }
                        Some(b'/') => {
                            self.advance();
                            result.push('/');
                        }
                        Some(b'n') => {
                            self.advance();
                            result.push('\n');
                        }
                        Some(b'r') => {
                            self.advance();
                            result.push('\r');
                        }
                        Some(b't') => {
                            self.advance();
                            result.push('\t');
                        }
                        Some(b'b') => {
                            self.advance();
                            result.push('\x08');
                        }
                        Some(b'f') => {
                            self.advance();
                            result.push('\x0C');
                        }
                        Some(b'u') => {
                            self.advance();
                            let cp = self.parse_unicode_escape()?;
                            // Handle UTF-16 surrogate pairs.
                            if (0xD800..=0xDBFF).contains(&cp) {
                                // High surrogate — expect \uXXXX low surrogate.
                                if self.peek() == Some(b'\\') {
                                    self.advance();
                                    if self.peek() == Some(b'u') {
                                        self.advance();
                                        let low = self.parse_unicode_escape()?;
                                        if (0xDC00..=0xDFFF).contains(&low) {
                                            let combined = 0x10000
                                                + ((cp as u32 - 0xD800) << 10)
                                                + (low as u32 - 0xDC00);
                                            if let Some(c) = char::from_u32(combined) {
                                                result.push(c);
                                            } else {
                                                result.push(char::REPLACEMENT_CHARACTER);
                                            }
                                        } else {
                                            result.push(char::REPLACEMENT_CHARACTER);
                                            result.push(char::REPLACEMENT_CHARACTER);
                                        }
                                    } else {
                                        result.push(char::REPLACEMENT_CHARACTER);
                                    }
                                } else {
                                    result.push(char::REPLACEMENT_CHARACTER);
                                }
                            } else if let Some(c) = char::from_u32(cp as u32) {
                                result.push(c);
                            } else {
                                result.push(char::REPLACEMENT_CHARACTER);
                            }
                        }
                        Some(b) => {
                            return Err(signal(
                                "json-parse-error",
                                vec![Value::string(format!(
                                    "Invalid escape '\\{}' at position {}",
                                    b as char, self.pos
                                ))],
                            ));
                        }
                        None => {
                            return Err(signal(
                                "json-parse-error",
                                vec![Value::string("Unterminated escape in string")],
                            ));
                        }
                    }
                }
                Some(b) => {
                    // Regular byte — we need proper UTF-8 handling.
                    // Since the input was a valid Rust &str, we can safely
                    // decode the UTF-8 from the byte slice.
                    if b < 0x80 {
                        self.advance();
                        result.push(b as char);
                    } else {
                        // Multi-byte UTF-8 character.  Find its length and
                        // decode using std::str.
                        let start = self.pos;
                        let remaining = &self.input[start..];
                        if let Ok(s) = std::str::from_utf8(remaining) {
                            if let Some(ch) = s.chars().next() {
                                let len = ch.len_utf8();
                                self.pos += len;
                                result.push(ch);
                            } else {
                                self.advance();
                                result.push(char::REPLACEMENT_CHARACTER);
                            }
                        } else {
                            // Try char by char with lossy decoding.
                            self.advance();
                            result.push(char::REPLACEMENT_CHARACTER);
                        }
                    }
                }
            }
        }
    }

    /// Parse 4 hex digits for a \uXXXX escape.
    fn parse_unicode_escape(&mut self) -> Result<u16, Flow> {
        let mut value: u16 = 0;
        for _ in 0..4 {
            match self.peek() {
                Some(b) if b.is_ascii_hexdigit() => {
                    self.advance();
                    let digit = match b {
                        b'0'..=b'9' => (b - b'0') as u16,
                        b'a'..=b'f' => (b - b'a' + 10) as u16,
                        b'A'..=b'F' => (b - b'A' + 10) as u16,
                        _ => unreachable!(),
                    };
                    value = value * 16 + digit;
                }
                _ => {
                    return Err(signal(
                        "json-parse-error",
                        vec![Value::string(format!(
                            "Invalid unicode escape at position {}",
                            self.pos
                        ))],
                    ));
                }
            }
        }
        Ok(value)
    }

    /// Parse a JSON number.
    fn parse_number(&mut self) -> Result<Value, Flow> {
        let start = self.pos;
        let mut is_float = false;

        // Optional leading minus.
        if self.peek() == Some(b'-') {
            self.advance();
        }

        // Integer part.
        match self.peek() {
            Some(b'0') => {
                self.advance();
            }
            Some(b) if b >= b'1' && b <= b'9' => {
                self.advance();
                while let Some(b) = self.peek() {
                    if b.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
            _ => {
                return Err(signal(
                    "json-parse-error",
                    vec![Value::string(format!(
                        "Invalid number at position {}",
                        self.pos
                    ))],
                ));
            }
        }

        // Fractional part.
        if self.peek() == Some(b'.') {
            is_float = true;
            self.advance();
            let frac_start = self.pos;
            while let Some(b) = self.peek() {
                if b.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            if self.pos == frac_start {
                return Err(signal(
                    "json-parse-error",
                    vec![Value::string(format!(
                        "Expected digit after decimal point at position {}",
                        self.pos
                    ))],
                ));
            }
        }

        // Exponent part.
        if let Some(b'e') | Some(b'E') = self.peek() {
            is_float = true;
            self.advance();
            if let Some(b'+') | Some(b'-') = self.peek() {
                self.advance();
            }
            let exp_start = self.pos;
            while let Some(b) = self.peek() {
                if b.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            if self.pos == exp_start {
                return Err(signal(
                    "json-parse-error",
                    vec![Value::string(format!(
                        "Expected digit in exponent at position {}",
                        self.pos
                    ))],
                ));
            }
        }

        let num_str = std::str::from_utf8(&self.input[start..self.pos]).unwrap_or("0");

        if is_float {
            let f: f64 = num_str.parse().map_err(|_| {
                signal(
                    "json-parse-error",
                    vec![Value::string(format!("Invalid float: {}", num_str))],
                )
            })?;
            Ok(Value::Float(f))
        } else {
            // Try parsing as i64 first; fall back to f64 for very large numbers.
            match num_str.parse::<i64>() {
                Ok(n) => Ok(Value::Int(n)),
                Err(_) => {
                    let f: f64 = num_str.parse().map_err(|_| {
                        signal(
                            "json-parse-error",
                            vec![Value::string(format!("Invalid number: {}", num_str))],
                        )
                    })?;
                    Ok(Value::Float(f))
                }
            }
        }
    }

    /// Parse a JSON array: `[` value { `,` value } `]`.
    fn parse_array(&mut self) -> Result<Value, Flow> {
        self.expect_byte(b'[')?;
        self.skip_ws();
        let mut items: Vec<Value> = Vec::new();

        if self.peek() == Some(b']') {
            self.advance();
        } else {
            loop {
                let val = self.parse_value()?;
                items.push(val);
                self.skip_ws();
                match self.peek() {
                    Some(b',') => {
                        self.advance();
                    }
                    Some(b']') => {
                        self.advance();
                        break;
                    }
                    _ => {
                        return Err(signal(
                            "json-parse-error",
                            vec![Value::string(format!(
                                "Expected ',' or ']' at position {}",
                                self.pos
                            ))],
                        ));
                    }
                }
            }
        }

        match self.opts.array_type {
            ArrayType::Vector => Ok(Value::vector(items)),
            ArrayType::List => Ok(Value::list(items)),
        }
    }

    /// Parse a JSON object: `{` string `:` value { `,` string `:` value } `}`.
    fn parse_object(&mut self) -> Result<Value, Flow> {
        self.expect_byte(b'{')?;
        self.skip_ws();

        match self.opts.object_type {
            ObjectType::HashTable => self.parse_object_hash_table(),
            ObjectType::Alist => self.parse_object_alist(),
            ObjectType::Plist => self.parse_object_plist(),
        }
    }

    fn parse_object_hash_table(&mut self) -> Result<Value, Flow> {
        let ht = Value::hash_table(HashTableTest::Equal);
        if self.peek() == Some(b'}') {
            self.advance();
            return Ok(ht);
        }

        if let Value::HashTable(ref table_arc) = ht {
            loop {
                self.skip_ws();
                let key = self.parse_string_raw()?;
                self.expect_byte(b':')?;
                let val = self.parse_value()?;

                {
                    let mut table = table_arc.lock().expect("poisoned");
                    let hash_key = HashKey::Str(key.clone());
                    table.data.insert(hash_key.clone(), val);
                    table.key_snapshots.insert(hash_key, Value::string(key));
                }

                self.skip_ws();
                match self.peek() {
                    Some(b',') => {
                        self.advance();
                    }
                    Some(b'}') => {
                        self.advance();
                        break;
                    }
                    _ => {
                        return Err(signal(
                            "json-parse-error",
                            vec![Value::string(format!(
                                "Expected ',' or '}}' at position {}",
                                self.pos
                            ))],
                        ));
                    }
                }
            }
        }

        Ok(ht)
    }

    fn parse_object_alist(&mut self) -> Result<Value, Flow> {
        let mut pairs: Vec<Value> = Vec::new();

        if self.peek() == Some(b'}') {
            self.advance();
            return Ok(Value::Nil);
        }

        loop {
            self.skip_ws();
            let key = self.parse_string_raw()?;
            self.expect_byte(b':')?;
            let val = self.parse_value()?;

            pairs.push(Value::cons(Value::symbol(key), val));

            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.advance();
                }
                Some(b'}') => {
                    self.advance();
                    break;
                }
                _ => {
                    return Err(signal(
                        "json-parse-error",
                        vec![Value::string(format!(
                            "Expected ',' or '}}' at position {}",
                            self.pos
                        ))],
                    ));
                }
            }
        }

        Ok(Value::list(pairs))
    }

    fn parse_object_plist(&mut self) -> Result<Value, Flow> {
        let mut items: Vec<Value> = Vec::new();

        if self.peek() == Some(b'}') {
            self.advance();
            return Ok(Value::Nil);
        }

        loop {
            self.skip_ws();
            let key = self.parse_string_raw()?;
            self.expect_byte(b':')?;
            let val = self.parse_value()?;

            // Plist keys are keywords (symbols with leading colon).
            items.push(Value::Keyword(format!(":{}", key)));
            items.push(val);

            self.skip_ws();
            match self.peek() {
                Some(b',') => {
                    self.advance();
                }
                Some(b'}') => {
                    self.advance();
                    break;
                }
                _ => {
                    return Err(signal(
                        "json-parse-error",
                        vec![Value::string(format!(
                            "Expected ',' or '}}' at position {}",
                            self.pos
                        ))],
                    ));
                }
            }
        }

        Ok(Value::list(items))
    }
}

// ===========================================================================
// Public builtin functions
// ===========================================================================

/// `(json-serialize VALUE &rest ARGS)` — serialize a Lisp value to a JSON string.
///
/// ARGS are keyword arguments:
/// - `:null-object VALUE` — Lisp value to serialize as JSON null (default: nil)
/// - `:false-object VALUE` — Lisp value to serialize as JSON false (default: :false)
pub(crate) fn builtin_json_serialize(args: Vec<Value>) -> EvalResult {
    expect_min_args("json-serialize", &args, 1)?;
    let opts = parse_serialize_kwargs(&args, 1)?;
    let json = serialize_to_json(&args[0], &opts, 0)?;
    Ok(Value::string(json))
}

/// `(json-parse-string STRING &rest ARGS)` — parse a JSON string into a Lisp value.
///
/// ARGS are keyword arguments:
/// - `:object-type SYMBOL` — `hash-table` (default), `alist`, or `plist`
/// - `:array-type SYMBOL` — `array` (default, yields vector) or `list`
/// - `:null-object VALUE` — Lisp value for JSON null (default: :null)
/// - `:false-object VALUE` — Lisp value for JSON false (default: :false)
pub(crate) fn builtin_json_parse_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("json-parse-string", &args, 1)?;
    let input = match &args[0] {
        Value::Str(s) => (**s).clone(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("stringp"), other.clone()],
            ));
        }
    };
    let opts = parse_parse_kwargs(&args, 1)?;
    let mut parser = JsonParser::new(&input, opts);
    parser.skip_ws();
    if parser.pos >= parser.input.len() {
        let p = parser.pos as i64;
        return Err(signal(
            "json-end-of-file",
            vec![Value::Int(1), Value::Int(p), Value::Int(p)],
        ));
    }
    let result = parser.parse_value()?;

    // Ensure there is no trailing non-whitespace.
    parser.skip_ws();
    if parser.pos < parser.input.len() {
        return Err(signal(
            "json-trailing-content",
            vec![Value::string(format!(
                "Trailing content after JSON value at position {}",
                parser.pos
            ))],
        ));
    }

    Ok(result)
}

/// `(json-parse-buffer &rest ARGS)` — parse one JSON value from point.
///
/// Unlike `json-parse-string`, this parses a single JSON value starting at the
/// current point (after leading whitespace), leaves trailing buffer content
/// untouched, and advances point to just after the parsed value.
pub(crate) fn builtin_json_parse_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    let opts = parse_parse_kwargs(&args, 0)?;
    let (input, point_base) = {
        let buf = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        (
            buf.buffer_substring(buf.point(), buf.point_max()),
            buf.point(),
        )
    };

    let mut parser = JsonParser::new(&input, opts);
    parser.skip_ws();
    if parser.pos >= parser.input.len() {
        let p = parser.pos as i64;
        return Err(signal(
            "json-end-of-file",
            vec![Value::Int(1), Value::Int(p), Value::Int(p)],
        ));
    }

    let result = parser.parse_value()?;
    let new_point = point_base + parser.pos;
    if let Some(buf) = eval.buffers.current_buffer_mut() {
        buf.goto_char(new_point);
    }
    Ok(result)
}

/// `(json-insert VALUE &rest ARGS)` — insert JSON text at point.
///
/// Keyword arguments mirror `json-serialize` (`:null-object`, `:false-object`).
pub(crate) fn builtin_json_insert(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("json-insert", &args, 1)?;
    let opts = parse_serialize_kwargs(&args, 1)?;
    let json = serialize_to_json(&args[0], &opts, 0)?;
    let buf = eval
        .buffers
        .current_buffer_mut()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
    buf.insert(&json);
    Ok(Value::Nil)
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // Serializer tests
    // -----------------------------------------------------------------------

    #[test]
    fn serialize_null() {
        let result = builtin_json_serialize(vec![Value::Nil]);
        assert_eq!(result.unwrap().as_str(), Some("null"));
    }

    #[test]
    fn serialize_true() {
        let result = builtin_json_serialize(vec![Value::True]);
        assert_eq!(result.unwrap().as_str(), Some("true"));
    }

    #[test]
    fn serialize_false_keyword() {
        let result = builtin_json_serialize(vec![Value::Keyword(":false".to_string())]);
        assert_eq!(result.unwrap().as_str(), Some("false"));
    }

    #[test]
    fn serialize_json_false_keyword() {
        let result = builtin_json_serialize(vec![Value::Keyword(":json-false".to_string())]);
        assert_eq!(result.unwrap().as_str(), Some("false"));
    }

    #[test]
    fn serialize_integer() {
        let result = builtin_json_serialize(vec![Value::Int(42)]);
        assert_eq!(result.unwrap().as_str(), Some("42"));
    }

    #[test]
    fn serialize_negative_integer() {
        let result = builtin_json_serialize(vec![Value::Int(-7)]);
        assert_eq!(result.unwrap().as_str(), Some("-7"));
    }

    #[test]
    fn serialize_float() {
        let result = builtin_json_serialize(vec![Value::Float(3.14)]);
        assert_eq!(result.unwrap().as_str(), Some("3.14"));
    }

    #[test]
    fn serialize_float_whole() {
        let result = builtin_json_serialize(vec![Value::Float(1.0)]);
        assert_eq!(result.unwrap().as_str(), Some("1.0"));
    }

    #[test]
    fn serialize_nan_errors() {
        let result = builtin_json_serialize(vec![Value::Float(f64::NAN)]);
        assert!(result.is_err());
    }

    #[test]
    fn serialize_inf_errors() {
        let result = builtin_json_serialize(vec![Value::Float(f64::INFINITY)]);
        assert!(result.is_err());
    }

    #[test]
    fn serialize_string() {
        let result = builtin_json_serialize(vec![Value::string("hello")]);
        assert_eq!(result.unwrap().as_str(), Some("\"hello\""));
    }

    #[test]
    fn serialize_string_with_escapes() {
        let result = builtin_json_serialize(vec![Value::string("a\"b\\c\ndef")]);
        assert_eq!(result.unwrap().as_str(), Some("\"a\\\"b\\\\c\\ndef\""));
    }

    #[test]
    fn serialize_empty_vector() {
        let result = builtin_json_serialize(vec![Value::vector(vec![])]);
        assert_eq!(result.unwrap().as_str(), Some("[]"));
    }

    #[test]
    fn serialize_vector() {
        let result = builtin_json_serialize(vec![Value::vector(vec![
            Value::Int(1),
            Value::string("two"),
            Value::True,
            Value::Nil,
        ])]);
        assert_eq!(result.unwrap().as_str(), Some("[1,\"two\",true,null]"));
    }

    #[test]
    fn serialize_hash_table() {
        let ht = Value::hash_table(HashTableTest::Equal);
        if let Value::HashTable(ref table_arc) = ht {
            let mut table = table_arc.lock().unwrap();
            table
                .data
                .insert(HashKey::Str("name".to_string()), Value::string("Alice"));
        }
        let result = builtin_json_serialize(vec![ht]);
        assert_eq!(result.unwrap().as_str(), Some("{\"name\":\"Alice\"}"));
    }

    #[test]
    fn serialize_alist() {
        let alist = Value::list(vec![
            Value::cons(Value::symbol("a"), Value::Int(1)),
            Value::cons(Value::symbol("b"), Value::Int(2)),
        ]);
        let result = builtin_json_serialize(vec![alist]);
        assert_eq!(result.unwrap().as_str(), Some("{\"a\":1,\"b\":2}"));
    }

    #[test]
    fn serialize_nested() {
        let inner = Value::vector(vec![Value::Int(1), Value::Int(2)]);
        let alist = Value::list(vec![Value::cons(Value::symbol("arr"), inner)]);
        let result = builtin_json_serialize(vec![alist]);
        assert_eq!(result.unwrap().as_str(), Some("{\"arr\":[1,2]}"));
    }

    #[test]
    fn serialize_alist_string_key_type_error() {
        let alist = Value::list(vec![Value::cons(Value::string("a"), Value::Int(1))]);
        match builtin_json_serialize(vec![alist]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data.first(), Some(&Value::symbol("symbolp")));
            }
            other => panic!("expected wrong-type-argument signal, got {:?}", other),
        }
    }

    #[test]
    fn serialize_custom_false_object() {
        // Use nil as the false-object.
        let result = builtin_json_serialize(vec![
            Value::Nil,
            Value::Keyword(":false-object".to_string()),
            Value::Nil,
        ]);
        // nil matches both null_object (default) and false_object (nil).
        // null_object is checked first, so it becomes "null".
        assert_eq!(result.unwrap().as_str(), Some("null"));
    }

    #[test]
    fn serialize_wrong_no_args() {
        let result = builtin_json_serialize(vec![]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // Parser tests
    // -----------------------------------------------------------------------

    #[test]
    fn parse_null() {
        let result = builtin_json_parse_string(vec![Value::string("null")]);
        let val = result.unwrap();
        assert!(matches!(val, Value::Keyword(ref k) if k == ":null"));
    }

    #[test]
    fn parse_true() {
        let result = builtin_json_parse_string(vec![Value::string("true")]);
        assert!(matches!(result.unwrap(), Value::True));
    }

    #[test]
    fn parse_false() {
        let result = builtin_json_parse_string(vec![Value::string("false")]);
        let val = result.unwrap();
        assert!(matches!(val, Value::Keyword(ref k) if k == ":false"));
    }

    #[test]
    fn parse_integer() {
        let result = builtin_json_parse_string(vec![Value::string("42")]);
        assert!(matches!(result.unwrap(), Value::Int(42)));
    }

    #[test]
    fn parse_negative_integer() {
        let result = builtin_json_parse_string(vec![Value::string("-7")]);
        assert!(matches!(result.unwrap(), Value::Int(-7)));
    }

    #[test]
    fn parse_float() {
        let result = builtin_json_parse_string(vec![Value::string("3.14")]);
        match result.unwrap() {
            Value::Float(f) => assert!((f - 3.14).abs() < 1e-10),
            other => panic!("expected float, got {:?}", other),
        }
    }

    #[test]
    fn parse_float_exponent() {
        let result = builtin_json_parse_string(vec![Value::string("1.5e2")]);
        match result.unwrap() {
            Value::Float(f) => assert!((f - 150.0).abs() < 1e-10),
            other => panic!("expected float, got {:?}", other),
        }
    }

    #[test]
    fn parse_zero() {
        let result = builtin_json_parse_string(vec![Value::string("0")]);
        assert!(matches!(result.unwrap(), Value::Int(0)));
    }

    #[test]
    fn parse_string_simple() {
        let result = builtin_json_parse_string(vec![Value::string("\"hello\"")]);
        assert_eq!(result.unwrap().as_str(), Some("hello"));
    }

    #[test]
    fn parse_string_with_escapes() {
        let result = builtin_json_parse_string(vec![Value::string("\"a\\\"b\\\\c\\nd\"")]);
        assert_eq!(result.unwrap().as_str(), Some("a\"b\\c\nd"));
    }

    #[test]
    fn parse_string_unicode_escape() {
        let result = builtin_json_parse_string(vec![Value::string("\"\\u0041\"")]);
        assert_eq!(result.unwrap().as_str(), Some("A"));
    }

    #[test]
    fn parse_string_surrogate_pair() {
        // U+1F600 (grinning face) = \uD83D\uDE00
        let result = builtin_json_parse_string(vec![Value::string("\"\\uD83D\\uDE00\"")]);
        let val = result.unwrap();
        assert_eq!(val.as_str(), Some("\u{1F600}"));
    }

    #[test]
    fn parse_empty_array() {
        let result = builtin_json_parse_string(vec![Value::string("[]")]);
        match result.unwrap() {
            Value::Vector(v) => assert!(v.lock().unwrap().is_empty()),
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn parse_array() {
        let result = builtin_json_parse_string(vec![Value::string("[1, 2, 3]")]);
        match result.unwrap() {
            Value::Vector(v) => {
                let items = v.lock().unwrap();
                assert_eq!(items.len(), 3);
                assert!(matches!(items[0], Value::Int(1)));
                assert!(matches!(items[1], Value::Int(2)));
                assert!(matches!(items[2], Value::Int(3)));
            }
            other => panic!("expected vector, got {:?}", other),
        }
    }

    #[test]
    fn parse_array_as_list() {
        let result = builtin_json_parse_string(vec![
            Value::string("[1, 2]"),
            Value::Keyword(":array-type".to_string()),
            Value::symbol("list"),
        ]);
        let val = result.unwrap();
        let items = list_to_vec(&val).expect("should be a list");
        assert_eq!(items.len(), 2);
        assert!(matches!(items[0], Value::Int(1)));
        assert!(matches!(items[1], Value::Int(2)));
    }

    #[test]
    fn parse_empty_object() {
        let result = builtin_json_parse_string(vec![Value::string("{}")]);
        match result.unwrap() {
            Value::HashTable(ht) => {
                let table = ht.lock().unwrap();
                assert!(table.data.is_empty());
            }
            other => panic!("expected hash-table, got {:?}", other),
        }
    }

    #[test]
    fn parse_object_hash_table() {
        let result = builtin_json_parse_string(vec![Value::string("{\"a\": 1, \"b\": 2}")]);
        match result.unwrap() {
            Value::HashTable(ht) => {
                let table = ht.lock().unwrap();
                assert_eq!(table.data.len(), 2);
                assert_eq!(table.key_snapshots.len(), 2);
                assert!(matches!(
                    table.data.get(&HashKey::Str("a".to_string())),
                    Some(Value::Int(1))
                ));
                assert!(matches!(
                    table.data.get(&HashKey::Str("b".to_string())),
                    Some(Value::Int(2))
                ));
                assert!(matches!(
                    table.key_snapshots.get(&HashKey::Str("a".to_string())),
                    Some(key) if key.as_str() == Some("a")
                ));
                assert!(matches!(
                    table.key_snapshots.get(&HashKey::Str("b".to_string())),
                    Some(key) if key.as_str() == Some("b")
                ));
            }
            other => panic!("expected hash-table, got {:?}", other),
        }
    }

    #[test]
    fn parse_object_as_alist() {
        let result = builtin_json_parse_string(vec![
            Value::string("{\"x\": 10}"),
            Value::Keyword(":object-type".to_string()),
            Value::symbol("alist"),
        ]);
        let val = result.unwrap();
        let items = list_to_vec(&val).expect("should be a list");
        assert_eq!(items.len(), 1);
        // Each item should be (key . value).
        match &items[0] {
            Value::Cons(cell) => {
                let pair = cell.lock().unwrap();
                assert_eq!(pair.car, Value::symbol("x"));
                assert!(matches!(pair.cdr, Value::Int(10)));
            }
            other => panic!("expected cons, got {:?}", other),
        }
    }

    #[test]
    fn parse_object_as_plist() {
        let result = builtin_json_parse_string(vec![
            Value::string("{\"key\": 42}"),
            Value::Keyword(":object-type".to_string()),
            Value::symbol("plist"),
        ]);
        let val = result.unwrap();
        let items = list_to_vec(&val).expect("should be a list");
        assert_eq!(items.len(), 2);
        assert!(matches!(&items[0], Value::Keyword(k) if k == ":key"));
        assert!(matches!(items[1], Value::Int(42)));
    }

    #[test]
    fn parse_nested() {
        let json = r#"{"arr": [1, {"nested": true}], "val": null}"#;
        let result = builtin_json_parse_string(vec![Value::string(json)]);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_custom_null_object() {
        let result = builtin_json_parse_string(vec![
            Value::string("null"),
            Value::Keyword(":null-object".to_string()),
            Value::Nil,
        ]);
        assert!(matches!(result.unwrap(), Value::Nil));
    }

    #[test]
    fn parse_custom_false_object() {
        let result = builtin_json_parse_string(vec![
            Value::string("false"),
            Value::Keyword(":false-object".to_string()),
            Value::Keyword(":json-false".to_string()),
        ]);
        let val = result.unwrap();
        assert!(matches!(val, Value::Keyword(ref k) if k == ":json-false"));
    }

    #[test]
    fn parse_trailing_content_error() {
        let result = builtin_json_parse_string(vec![Value::string("42 extra")]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_empty_string_error() {
        match builtin_json_parse_string(vec![Value::string("")]) {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "json-end-of-file");
            }
            other => panic!("expected json-end-of-file signal, got {:?}", other),
        }
    }

    #[test]
    fn parse_invalid_json_error() {
        let result = builtin_json_parse_string(vec![Value::string("{bad}")]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_wrong_type_argument() {
        let result = builtin_json_parse_string(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn parse_no_args() {
        let result = builtin_json_parse_string(vec![]);
        assert!(result.is_err());
    }

    // -----------------------------------------------------------------------
    // Round-trip tests
    // -----------------------------------------------------------------------

    #[test]
    fn round_trip_integer() {
        let serialized = builtin_json_serialize(vec![Value::Int(123)]).unwrap();
        let parsed = builtin_json_parse_string(vec![serialized]).unwrap();
        assert!(matches!(parsed, Value::Int(123)));
    }

    #[test]
    fn round_trip_string() {
        let original = Value::string("hello \"world\"\ntest");
        let serialized = builtin_json_serialize(vec![original]).unwrap();
        let parsed = builtin_json_parse_string(vec![serialized]).unwrap();
        assert_eq!(parsed.as_str(), Some("hello \"world\"\ntest"));
    }

    #[test]
    fn round_trip_array() {
        let original = Value::vector(vec![Value::Int(1), Value::string("two"), Value::True]);
        let serialized = builtin_json_serialize(vec![original]).unwrap();
        let parsed = builtin_json_parse_string(vec![serialized]).unwrap();
        match parsed {
            Value::Vector(v) => {
                let items = v.lock().unwrap();
                assert_eq!(items.len(), 3);
                assert!(matches!(items[0], Value::Int(1)));
                assert_eq!(items[1].as_str(), Some("two"));
                assert!(matches!(items[2], Value::True));
            }
            _ => panic!("expected vector"),
        }
    }

    #[test]
    fn round_trip_object() {
        let ht = Value::hash_table(HashTableTest::Equal);
        if let Value::HashTable(ref table_arc) = ht {
            let mut table = table_arc.lock().unwrap();
            table
                .data
                .insert(HashKey::Str("key".to_string()), Value::Int(99));
        }
        let serialized = builtin_json_serialize(vec![ht]).unwrap();
        let parsed = builtin_json_parse_string(vec![serialized]).unwrap();
        match parsed {
            Value::HashTable(ht) => {
                let table = ht.lock().unwrap();
                assert!(matches!(
                    table.data.get(&HashKey::Str("key".to_string())),
                    Some(Value::Int(99))
                ));
            }
            _ => panic!("expected hash-table"),
        }
    }

    // -----------------------------------------------------------------------
    // String encoding edge cases
    // -----------------------------------------------------------------------

    #[test]
    fn encode_control_chars() {
        let s = "a\x00b\x01c";
        let encoded = json_encode_string(s);
        assert_eq!(encoded, "\"a\\u0000b\\u0001c\"");
    }

    #[test]
    fn encode_backspace_formfeed() {
        let s = "\x08\x0C";
        let encoded = json_encode_string(s);
        assert_eq!(encoded, "\"\\b\\f\"");
    }

    #[test]
    fn parse_large_number_as_float() {
        // Number too large for i64.
        let result = builtin_json_parse_string(vec![Value::string("99999999999999999999")]);
        match result.unwrap() {
            Value::Float(_) => {} // OK — fell back to f64
            other => panic!("expected float for large number, got {:?}", other),
        }
    }

    #[test]
    fn serialize_symbol_key_in_alist() {
        let alist = Value::list(vec![Value::cons(
            Value::symbol("name"),
            Value::string("test"),
        )]);
        let result = builtin_json_serialize(vec![alist]);
        assert_eq!(result.unwrap().as_str(), Some("{\"name\":\"test\"}"));
    }

    #[test]
    fn parse_whitespace_around_values() {
        let result = builtin_json_parse_string(vec![Value::string("  {  \"a\"  :  1  }  ")]);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_deeply_nested() {
        let json = "[[[[[[1]]]]]]";
        let result = builtin_json_parse_string(vec![Value::string(json)]);
        assert!(result.is_ok());
    }
}
