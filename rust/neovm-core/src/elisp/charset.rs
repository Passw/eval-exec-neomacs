//! Charset builtins for the Elisp interpreter.
//!
//! Charsets in Emacs define sets of characters with encoding properties.
//! For neovm we primarily support Unicode; other charsets are registered
//! for compatibility but map through to the Unicode code-point space.
//!
//! The `CharsetRegistry` stores known charset names, IDs, and plists.
//! It is initialized with the standard charsets: ascii, unicode,
//! unicode-bmp, latin-iso8859-1, emacs, and eight-bit.

use super::error::{signal, EvalResult, Flow};
use super::intern::resolve_sym;
use super::value::*;
use std::collections::{HashMap, HashSet};

const RAW_BYTE_SENTINEL_MIN: u32 = 0xE080;
const RAW_BYTE_SENTINEL_MAX: u32 = 0xE0FF;
const UNIBYTE_BYTE_SENTINEL_MIN: u32 = 0xE300;
const UNIBYTE_BYTE_SENTINEL_MAX: u32 = 0xE3FF;

// ---------------------------------------------------------------------------
// Charset data types
// ---------------------------------------------------------------------------

/// Information about a single charset.
struct CharsetInfo {
    id: i64,
    name: String,
    plist: Vec<(String, Value)>,
}

/// Registry of known charsets, keyed by name.
pub(crate) struct CharsetRegistry {
    charsets: HashMap<String, CharsetInfo>,
    /// Priority-ordered list of charset names.
    priority: Vec<String>,
}

impl CharsetRegistry {
    /// Create a new registry pre-populated with the standard charsets.
    pub fn new() -> Self {
        let mut reg = Self {
            charsets: HashMap::new(),
            priority: Vec::new(),
        };
        reg.init_standard_charsets();
        reg
    }

    fn init_standard_charsets(&mut self) {
        self.register(CharsetInfo {
            id: 0,
            name: "ascii".to_string(),
            plist: vec![],
        });
        self.register(CharsetInfo {
            id: 2,
            name: "unicode".to_string(),
            plist: vec![],
        });
        self.register(CharsetInfo {
            id: 144,
            name: "unicode-bmp".to_string(),
            plist: vec![],
        });
        self.register(CharsetInfo {
            id: 5,
            name: "latin-iso8859-1".to_string(),
            plist: vec![],
        });
        self.register(CharsetInfo {
            id: 3,
            name: "emacs".to_string(),
            plist: vec![],
        });
        self.register(CharsetInfo {
            id: 4,
            name: "eight-bit".to_string(),
            plist: vec![],
        });

        // Default priority order.
        self.priority = vec![
            "unicode".to_string(),
            "emacs".to_string(),
            "ascii".to_string(),
            "unicode-bmp".to_string(),
            "latin-iso8859-1".to_string(),
            "eight-bit".to_string(),
        ];
    }

    fn register(&mut self, info: CharsetInfo) {
        self.charsets.insert(info.name.clone(), info);
    }

    /// Return true if a charset with the given name exists.
    pub fn contains(&self, name: &str) -> bool {
        self.charsets.contains_key(name)
    }

    /// Return the list of all charset names (unordered).
    #[cfg(test)]
    pub fn names(&self) -> Vec<String> {
        self.charsets.keys().cloned().collect()
    }

    /// Return the priority-ordered list of charset names.
    pub fn priority_list(&self) -> &[String] {
        &self.priority
    }

    /// Move the requested charset names to the front of the priority list
    /// (deduplicated, preserving relative order for remaining entries).
    pub fn set_priority(&mut self, requested: &[String]) {
        let mut seen = HashSet::with_capacity(self.priority.len() + requested.len());
        let mut reordered = Vec::with_capacity(self.priority.len() + requested.len());

        for name in requested {
            if seen.insert(name.clone()) {
                reordered.push(name.clone());
            }
        }

        for name in &self.priority {
            if seen.insert(name.clone()) {
                reordered.push(name.clone());
            }
        }

        self.priority = reordered;
    }

    /// Return the plist for a charset, or None if not found.
    pub fn plist(&self, name: &str) -> Option<&[(String, Value)]> {
        self.charsets.get(name).map(|info| info.plist.as_slice())
    }

    /// Return the internal ID for a charset, if known.
    pub fn id(&self, name: &str) -> Option<i64> {
        self.charsets.get(name).map(|info| info.id)
    }

    /// Register ALIAS as another name for TARGET.
    pub fn define_alias(&mut self, alias: &str, target: &str) {
        let Some(target_info) = self.charsets.get(target) else {
            return;
        };
        self.charsets.insert(
            alias.to_string(),
            CharsetInfo {
                id: target_info.id,
                name: alias.to_string(),
                plist: target_info.plist.clone(),
            },
        );
    }
}

// ---------------------------------------------------------------------------
// Singleton registry
// ---------------------------------------------------------------------------

use std::sync::{Mutex, OnceLock};

/// Global charset registry (initialized once, accessed from builtins).
fn global_registry() -> &'static Mutex<CharsetRegistry> {
    static REGISTRY: OnceLock<Mutex<CharsetRegistry>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(CharsetRegistry::new()))
}

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

fn expect_int_or_marker(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), *other],
        )),
    }
}

fn require_known_charset(value: &Value) -> Result<String, Flow> {
    let name = match value {
        Value::Symbol(id) => resolve_sym(*id).to_owned(),
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("charsetp"), *other],
            ))
        }
    };
    let reg = global_registry().lock().expect("poisoned");
    if reg.contains(&name) {
        Ok(name)
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("charsetp"), Value::symbol(name)],
        ))
    }
}

fn decode_char_codepoint_arg(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) if *n >= 0 => Ok(*n),
        Value::Float(f)
            if f.is_finite() && *f >= 0.0 && f.fract() == 0.0 && *f <= i64::MAX as f64 =>
        {
            Ok(*f as i64)
        }
        _ => Err(signal(
            "error",
            vec![Value::string(
                "Not an in-range integer, integral float, or cons of integers",
            )],
        )),
    }
}

fn expect_wholenump(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) if *n >= 0 => Ok(*n),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("wholenump"), *other],
        )),
    }
}

fn expect_fixnump(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("fixnump"), *other],
        )),
    }
}

fn encode_char_input(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Char(c) => Ok(*c as i64),
        Value::Int(n) if (0..=0x3FFFFF).contains(n) => Ok(*n),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("characterp"), *other],
        )),
    }
}

// ---------------------------------------------------------------------------
// Pure builtins (Vec<Value> -> EvalResult)
// ---------------------------------------------------------------------------

/// `(charsetp OBJECT)` -- return t if OBJECT names a known charset.
pub(crate) fn builtin_charsetp(args: Vec<Value>) -> EvalResult {
    expect_args("charsetp", &args, 1)?;
    let name = match &args[0] {
        Value::Symbol(id) => resolve_sym(*id).to_owned(),
        _ => return Ok(Value::Nil),
    };
    let reg = global_registry().lock().expect("poisoned");
    Ok(Value::bool(reg.contains(&name)))
}

/// `(charset-list)` -- return charset symbols in priority order.
#[cfg(test)]
pub(crate) fn builtin_charset_list(args: Vec<Value>) -> EvalResult {
    expect_args("charset-list", &args, 0)?;
    let reg = global_registry().lock().expect("poisoned");
    let names: Vec<Value> = reg
        .priority_list()
        .iter()
        .map(|name| Value::symbol(name.clone()))
        .collect();
    Ok(Value::list(names))
}

/// `(unibyte-charset)` -- return the charset used for unibyte strings.
#[cfg(test)]
pub(crate) fn builtin_unibyte_charset(args: Vec<Value>) -> EvalResult {
    expect_args("unibyte-charset", &args, 0)?;
    Ok(Value::symbol("eight-bit"))
}

/// `(charset-priority-list &optional HIGHESTP)` -- return list of charsets
/// in priority order.  If HIGHESTP is non-nil, return only the highest
/// priority charset.
pub(crate) fn builtin_charset_priority_list(args: Vec<Value>) -> EvalResult {
    expect_max_args("charset-priority-list", &args, 1)?;
    let highestp = args.first().map(|v| v.is_truthy()).unwrap_or(false);
    let reg = global_registry().lock().expect("poisoned");
    let priority = reg.priority_list();
    if highestp {
        if let Some(first) = priority.first() {
            Ok(Value::list(vec![Value::symbol(first.clone())]))
        } else {
            Ok(Value::Nil)
        }
    } else {
        let syms: Vec<Value> = priority.iter().map(|s| Value::symbol(s.clone())).collect();
        Ok(Value::list(syms))
    }
}

/// `(set-charset-priority &rest CHARSETS)` -- set charset detection priority.
pub(crate) fn builtin_set_charset_priority(args: Vec<Value>) -> EvalResult {
    expect_min_args("set-charset-priority", &args, 1)?;

    let mut reg = global_registry().lock().expect("poisoned");
    let mut requested = Vec::with_capacity(args.len());
    for arg in &args {
        let name = match arg {
            Value::Symbol(id) => resolve_sym(*id).to_owned(),
            _ => {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("charsetp"), *arg],
                ));
            }
        };
        if !reg.contains(&name) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("charsetp"), *arg],
            ));
        }
        requested.push(name);
    }
    reg.set_priority(&requested);
    Ok(Value::Nil)
}

/// `(char-charset CH &optional RESTRICTION)` -- return charset for character.
/// Always returns 'unicode for now.
pub(crate) fn builtin_char_charset(args: Vec<Value>) -> EvalResult {
    expect_min_args("char-charset", &args, 1)?;
    expect_max_args("char-charset", &args, 2)?;
    // Validate that the first argument is a character (int or char).
    match &args[0] {
        Value::Int(_) | Value::Char(_) => {}
        other => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("characterp"), *other],
            ));
        }
    }
    Ok(Value::symbol("unicode"))
}

/// `(charset-plist CHARSET)` -- return property list for CHARSET.
pub(crate) fn builtin_charset_plist(args: Vec<Value>) -> EvalResult {
    expect_args("charset-plist", &args, 1)?;
    let name = require_known_charset(&args[0])?;
    let reg = global_registry().lock().expect("poisoned");
    if let Some(pairs) = reg.plist(&name) {
        let mut elems = Vec::with_capacity(pairs.len() * 2);
        for (key, val) in pairs {
            elems.push(Value::symbol(key.clone()));
            elems.push(*val);
        }
        Ok(Value::list(elems))
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("charsetp"), Value::symbol(name)],
        ))
    }
}

/// `(charset-id-internal &optional CHARSET)` -- return internal charset id.
pub(crate) fn builtin_charset_id_internal(args: Vec<Value>) -> EvalResult {
    expect_max_args("charset-id-internal", &args, 1)?;
    let arg = args.first().cloned().unwrap_or(Value::Nil);
    let name = match &arg {
        Value::Symbol(id) => resolve_sym(*id),
        _ => {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("charsetp"), arg],
            ))
        }
    };

    let reg = global_registry().lock().expect("poisoned");
    if let Some(id) = reg.id(name) {
        Ok(Value::Int(id))
    } else {
        Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("charsetp"), Value::symbol(name)],
        ))
    }
}

/// `(define-charset-internal ARG1 ... ARG17)` -- internal charset initializer.
///
/// NeoVM keeps a compatibility stub body but mirrors Emacs arity behavior:
/// this builtin accepts exactly 17 arguments.
pub(crate) fn builtin_define_charset_internal(args: Vec<Value>) -> EvalResult {
    expect_args("define-charset-internal", &args, 17)?;

    // Emacs validates early "array" arguments before deeper charset setup.
    // We keep the stub body but mirror this front-of-function type contract.
    for value in [&args[1], &args[2]] {
        if !(value.is_vector() || value.is_string()) {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("arrayp"), (*value)],
            ));
        }
    }

    if let Value::Vector(v) = &args[2] {
        let vec = with_heap(|h| h.get_vector(*v).clone());
        if vec.len() <= 1 {
            return Err(signal(
                "args-out-of-range",
                vec![args[2], Value::Int(vec.len() as i64)],
            ));
        }
    }

    Ok(Value::Nil)
}

/// `(find-charset-region BEG END &optional TABLE)` -- returns a list of charsets
/// present in the buffer slice.
pub(crate) fn builtin_find_charset_region(args: Vec<Value>) -> EvalResult {
    expect_min_args("find-charset-region", &args, 2)?;
    expect_max_args("find-charset-region", &args, 3)?;
    Ok(Value::list(vec![Value::symbol("ascii")]))
}

/// Evaluator-aware variant of `(find-charset-region BEG END &optional TABLE)`.
///
/// Returns charset symbols present in the region `[BEG, END)` where BEG/END are
/// Emacs 1-based character positions inside the accessible region.
pub(crate) fn builtin_find_charset_region_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("find-charset-region", &args, 2)?;
    expect_max_args("find-charset-region", &args, 3)?;
    let beg = expect_int_or_marker(&args[0])?;
    let end = expect_int_or_marker(&args[1])?;

    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    if beg < point_min || beg > point_max || end < point_min || end > point_max {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Int(beg), Value::Int(end)],
        ));
    }

    let mut a = beg;
    let mut b = end;
    if a > b {
        std::mem::swap(&mut a, &mut b);
    }

    let start_byte = buf.text.char_to_byte((a - 1).max(0) as usize);
    let end_byte = buf.text.char_to_byte((b - 1).max(0) as usize);
    if start_byte == end_byte {
        return Ok(Value::list(vec![Value::symbol("ascii")]));
    }

    let text = buf.buffer_substring(start_byte, end_byte);
    let charsets = classify_string_charsets(&text);
    if charsets.is_empty() {
        return Ok(Value::list(vec![Value::symbol("ascii")]));
    }
    Ok(Value::list(
        charsets.into_iter().map(Value::symbol).collect::<Vec<_>>(),
    ))
}

/// `(encode-big5-char CH)` -- encode character CH in BIG5 space.
pub(crate) fn builtin_encode_big5_char(args: Vec<Value>) -> EvalResult {
    expect_args("encode-big5-char", &args, 1)?;
    let ch = encode_char_input(&args[0])?;
    Ok(Value::Int(ch))
}

/// `(decode-big5-char CODE)` -- decode BIG5 code to Emacs character code.
pub(crate) fn builtin_decode_big5_char(args: Vec<Value>) -> EvalResult {
    expect_args("decode-big5-char", &args, 1)?;
    let code = expect_wholenump(&args[0])?;
    Ok(Value::Int(code))
}

/// `(encode-sjis-char CH)` -- encode character CH in Shift-JIS space.
pub(crate) fn builtin_encode_sjis_char(args: Vec<Value>) -> EvalResult {
    expect_args("encode-sjis-char", &args, 1)?;
    let ch = encode_char_input(&args[0])?;
    Ok(Value::Int(ch))
}

/// `(decode-sjis-char CODE)` -- decode Shift-JIS code to Emacs character code.
pub(crate) fn builtin_decode_sjis_char(args: Vec<Value>) -> EvalResult {
    expect_args("decode-sjis-char", &args, 1)?;
    let code = expect_wholenump(&args[0])?;
    Ok(Value::Int(code))
}

/// `(get-unused-iso-final-char DIMENSION CHARS)` -- return an available ISO
/// final-char code for the requested DIMENSION/CHARS class.
pub(crate) fn builtin_get_unused_iso_final_char(args: Vec<Value>) -> EvalResult {
    expect_args("get-unused-iso-final-char", &args, 2)?;
    let dimension = expect_fixnump(&args[0])?;
    let chars = expect_fixnump(&args[1])?;
    if !matches!(dimension, 1..=3) {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Invalid DIMENSION {dimension}, it should be 1, 2, or 3"
            ))],
        ));
    }
    if !matches!(chars, 94 | 96) {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Invalid CHARS {chars}, it should be 94 or 96"
            ))],
        ));
    }
    let final_char = match (dimension, chars) {
        (1, 94) => 54,
        (1, 96) => 51,
        (2, 94) => 50,
        (2, 96) | (3, 94) | (3, 96) => 48,
        _ => 48,
    };
    Ok(Value::Int(final_char))
}

/// `(declare-equiv-charset DIMENSION CHARS CH CHARSET)` -- declare an
/// equivalent charset mapping tuple.
pub(crate) fn builtin_declare_equiv_charset(args: Vec<Value>) -> EvalResult {
    expect_args("declare-equiv-charset", &args, 4)?;
    let _charset = require_known_charset(&args[3])?;
    let dimension = expect_fixnump(&args[0])?;
    let chars = expect_fixnump(&args[1])?;
    let _ch = encode_char_input(&args[2])?;
    if !matches!(dimension, 1..=3) {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Invalid DIMENSION {dimension}, it should be 1, 2, or 3"
            ))],
        ));
    }
    if !matches!(chars, 94 | 96) {
        return Err(signal(
            "error",
            vec![Value::string(format!(
                "Invalid CHARS {chars}, it should be 94 or 96"
            ))],
        ));
    }
    Ok(Value::Nil)
}

/// `(define-charset-alias ALIAS CHARSET)` -- add ALIAS for CHARSET.
pub(crate) fn builtin_define_charset_alias(args: Vec<Value>) -> EvalResult {
    expect_args("define-charset-alias", &args, 2)?;
    let target = require_known_charset(&args[1])?;
    if let Value::Symbol(id) = &args[0] {
        let alias = resolve_sym(*id);
        let mut reg = global_registry().lock().expect("poisoned");
        reg.define_alias(alias, &target);
    }
    Ok(Value::Nil)
}

/// `(find-charset-string STR &optional TABLE)` -- returns a list of charsets
/// present in STR.
pub(crate) fn builtin_find_charset_string(args: Vec<Value>) -> EvalResult {
    expect_min_args("find-charset-string", &args, 1)?;
    expect_max_args("find-charset-string", &args, 2)?;
    if !args[0].is_string() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), args[0]],
        ));
    }
    let s_ref = args[0].as_str().unwrap();

    let charsets = classify_string_charsets(s_ref);
    if charsets.is_empty() {
        Ok(Value::Nil)
    } else {
        Ok(Value::list(
            charsets.into_iter().map(Value::symbol).collect::<Vec<_>>(),
        ))
    }
}

/// `(decode-char CHARSET CODE-POINT)` -- decode code-point in CHARSET space.
pub(crate) fn builtin_decode_char(args: Vec<Value>) -> EvalResult {
    expect_args("decode-char", &args, 2)?;
    let name = require_known_charset(&args[0])?;
    let code_point = decode_char_codepoint_arg(&args[1])?;

    let decoded = match name.as_str() {
        "ascii" => (code_point <= 0x7F).then_some(code_point),
        "eight-bit" => (0x80..=0xFF)
            .contains(&code_point)
            .then_some(0x3FFF00 + code_point),
        "latin-iso8859-1" => (0x20..=0x7F)
            .contains(&code_point)
            .then_some(code_point + 0x80),
        "unicode" => (code_point <= 0x10FFFF).then_some(code_point),
        "unicode-bmp" => (code_point <= 0xFFFF).then_some(code_point),
        "emacs" => (code_point <= 0x3FFF7F).then_some(code_point),
        _ => None,
    };

    Ok(decoded.map_or(Value::Nil, Value::Int))
}

/// `(encode-char CH CHARSET)` -- encode CH in CHARSET space.
pub(crate) fn builtin_encode_char(args: Vec<Value>) -> EvalResult {
    expect_args("encode-char", &args, 2)?;
    let ch = encode_char_input(&args[0])?;
    let name = require_known_charset(&args[1])?;

    let encoded = match name.as_str() {
        "ascii" => (ch <= 0x7F).then_some(ch),
        "eight-bit" => (0x3FFF80..=0x3FFFFF).contains(&ch).then_some(ch - 0x3FFF00),
        "latin-iso8859-1" => (0xA0..=0xFF).contains(&ch).then_some(ch - 0x80),
        "unicode" => (ch <= 0x10FFFF).then_some(ch),
        "unicode-bmp" => (ch <= 0xFFFF).then_some(ch),
        "emacs" => (ch <= 0x3FFF7F).then_some(ch),
        _ => None,
    };

    Ok(encoded.map_or(Value::Nil, Value::Int))
}

/// `(clear-charset-maps)` -- clear charset-related caches (currently no cache
/// state is stored) and return nil.
pub(crate) fn builtin_clear_charset_maps(args: Vec<Value>) -> EvalResult {
    expect_max_args("clear-charset-maps", &args, 0)?;
    Ok(Value::Nil)
}

/// `(charset-after &optional POS)` -- currently returns 'unicode for compatibility.
pub(crate) fn builtin_charset_after(args: Vec<Value>) -> EvalResult {
    expect_max_args("charset-after", &args, 1)?;
    Ok(Value::symbol("unicode"))
}

/// Evaluator-aware variant of `(charset-after &optional POS)`.
///
/// Returns the charset of the character at POS (1-based), or the character
/// after point when POS is omitted. Returns nil at end-of-buffer or for
/// out-of-range numeric positions.
pub(crate) fn builtin_charset_after_eval(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_max_args("charset-after", &args, 1)?;
    let buf = eval
        .buffers
        .current_buffer()
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    let target_byte = if let Some(pos) = args.first() {
        let pos = expect_int_or_marker(pos)?;
        let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
        let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
        if pos < point_min || pos > point_max {
            return Ok(Value::Nil);
        }
        buf.text.char_to_byte((pos - 1).max(0) as usize)
    } else {
        buf.point()
    };

    let point_max_byte = buf.point_max();
    if target_byte >= point_max_byte {
        return Ok(Value::Nil);
    }

    let Some(ch) = buf.char_after(target_byte) else {
        return Ok(Value::Nil);
    };
    let cp = ch as u32;
    let charset = if (RAW_BYTE_SENTINEL_MIN..=RAW_BYTE_SENTINEL_MAX).contains(&cp) {
        "eight-bit"
    } else if (UNIBYTE_BYTE_SENTINEL_MIN..=UNIBYTE_BYTE_SENTINEL_MAX).contains(&cp) {
        let byte = cp - UNIBYTE_BYTE_SENTINEL_MIN;
        if byte <= 0x7F {
            "ascii"
        } else {
            "eight-bit"
        }
    } else if cp <= 0x7F {
        "ascii"
    } else if cp <= 0xFFFF {
        "unicode-bmp"
    } else {
        "unicode"
    };
    Ok(Value::symbol(charset))
}

fn classify_string_charsets(s: &str) -> Vec<&'static str> {
    if s.is_empty() {
        return Vec::new();
    }

    let mut has_ascii = false;
    let mut has_unicode = false;
    let mut has_eight_bit = false;
    let mut has_unicode_bmp = false;

    for ch in s.chars() {
        let cp = ch as u32;
        if (RAW_BYTE_SENTINEL_MIN..=RAW_BYTE_SENTINEL_MAX).contains(&cp) {
            has_eight_bit = true;
            continue;
        }
        if (UNIBYTE_BYTE_SENTINEL_MIN..=UNIBYTE_BYTE_SENTINEL_MAX).contains(&cp) {
            let byte = cp - UNIBYTE_BYTE_SENTINEL_MIN;
            if byte <= 0x7F {
                has_ascii = true;
            } else {
                has_eight_bit = true;
            }
            continue;
        }

        if cp <= 0x7F {
            has_ascii = true;
        } else if cp <= 0xFFFF {
            has_unicode_bmp = true;
        } else {
            has_unicode = true;
        }
    }

    // Match Emacs ordering observed for find-charset-string:
    // ascii, unicode, eight-bit, unicode-bmp.
    let mut out = Vec::new();
    if has_ascii {
        out.push("ascii");
    }
    if has_unicode {
        out.push("unicode");
    }
    if has_eight_bit {
        out.push("eight-bit");
    }
    if has_unicode_bmp {
        out.push("unicode-bmp");
    }
    out
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -----------------------------------------------------------------------
    // CharsetRegistry unit tests
    // -----------------------------------------------------------------------

    #[test]
    fn registry_has_standard_charsets() {
        let reg = CharsetRegistry::new();
        assert!(reg.contains("ascii"));
        assert!(reg.contains("unicode"));
        assert!(reg.contains("unicode-bmp"));
        assert!(reg.contains("latin-iso8859-1"));
        assert!(reg.contains("emacs"));
        assert!(reg.contains("eight-bit"));
        assert!(!reg.contains("nonexistent"));
    }

    #[test]
    fn registry_names_returns_all() {
        let reg = CharsetRegistry::new();
        let names = reg.names();
        assert_eq!(names.len(), 6);
        assert!(names.contains(&"ascii".to_string()));
        assert!(names.contains(&"unicode".to_string()));
    }

    #[test]
    fn registry_priority_list() {
        let reg = CharsetRegistry::new();
        let prio = reg.priority_list();
        assert!(!prio.is_empty());
        // unicode should be the highest priority.
        assert_eq!(prio[0], "unicode");
    }

    #[test]
    fn registry_plist_returns_empty_for_standard() {
        let reg = CharsetRegistry::new();
        let plist = reg.plist("ascii").unwrap();
        assert!(plist.is_empty());
    }

    #[test]
    fn registry_plist_none_for_unknown() {
        let reg = CharsetRegistry::new();
        assert!(reg.plist("nonexistent").is_none());
    }

    // -----------------------------------------------------------------------
    // Builtin tests: charsetp
    // -----------------------------------------------------------------------

    #[test]
    fn charsetp_known() {
        let r = builtin_charsetp(vec![Value::symbol("ascii")]).unwrap();
        assert!(matches!(r, Value::True));
    }

    #[test]
    fn charsetp_unknown() {
        let r = builtin_charsetp(vec![Value::symbol("nonexistent")]).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn charsetp_string_arg() {
        let r = builtin_charsetp(vec![Value::string("unicode")]).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn charsetp_non_symbol() {
        let r = builtin_charsetp(vec![Value::Int(42)]).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn charsetp_wrong_arg_count() {
        assert!(builtin_charsetp(vec![]).is_err());
        assert!(builtin_charsetp(vec![Value::Nil, Value::Nil]).is_err());
    }

    #[test]
    fn charset_list_returns_priority_order() {
        let r = builtin_charset_list(vec![]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "unicode"));
        assert!(items.len() >= 2);
    }

    #[test]
    fn unibyte_charset_returns_eight_bit() {
        let r = builtin_unibyte_charset(vec![]).unwrap();
        assert!(matches!(r, Value::Symbol(id) if resolve_sym(id) == "eight-bit"));
    }

    // -----------------------------------------------------------------------
    // Builtin tests: charset-priority-list
    // -----------------------------------------------------------------------

    #[test]
    fn charset_priority_list_full() {
        let r = builtin_charset_priority_list(vec![]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert!(!items.is_empty());
        // First should be unicode.
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "unicode"));
    }

    #[test]
    fn charset_priority_list_highestp() {
        let r = builtin_charset_priority_list(vec![Value::True]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "unicode"));
    }

    #[test]
    fn charset_priority_list_highestp_nil() {
        let r = builtin_charset_priority_list(vec![Value::Nil]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert!(items.len() > 1);
    }

    // -----------------------------------------------------------------------
    // Builtin tests: set-charset-priority
    // -----------------------------------------------------------------------

    #[test]
    fn registry_set_priority_reorders_and_dedups() {
        let mut reg = CharsetRegistry::new();
        reg.set_priority(&[
            "ascii".to_string(),
            "unicode".to_string(),
            "ascii".to_string(),
        ]);
        assert_eq!(reg.priority[0], "ascii");
        assert_eq!(reg.priority[1], "unicode");
    }

    #[test]
    fn set_charset_priority_requires_at_least_one_arg() {
        assert!(builtin_set_charset_priority(vec![]).is_err());
    }

    #[test]
    fn set_charset_priority_rejects_unknown_charset() {
        let r = builtin_set_charset_priority(vec![Value::symbol("vm-no-such-charset")]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![
                        Value::symbol("charsetp"),
                        Value::symbol("vm-no-such-charset")
                    ]
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    // -----------------------------------------------------------------------
    // Builtin tests: char-charset
    // -----------------------------------------------------------------------

    #[test]
    fn char_charset_int() {
        let r = builtin_char_charset(vec![Value::Int(65)]).unwrap();
        assert!(matches!(r, Value::Symbol(id) if resolve_sym(id) == "unicode"));
    }

    #[test]
    fn char_charset_char() {
        let r = builtin_char_charset(vec![Value::Char('A')]).unwrap();
        assert!(matches!(r, Value::Symbol(id) if resolve_sym(id) == "unicode"));
    }

    #[test]
    fn char_charset_with_restriction() {
        let r = builtin_char_charset(vec![Value::Int(65), Value::Nil]).unwrap();
        assert!(matches!(r, Value::Symbol(id) if resolve_sym(id) == "unicode"));
    }

    #[test]
    fn char_charset_wrong_type() {
        assert!(builtin_char_charset(vec![Value::string("not a char")]).is_err());
    }

    #[test]
    fn char_charset_wrong_arg_count() {
        assert!(builtin_char_charset(vec![]).is_err());
        assert!(builtin_char_charset(vec![Value::Int(65), Value::Nil, Value::Nil]).is_err());
    }

    // -----------------------------------------------------------------------
    // Builtin tests: charset-plist
    // -----------------------------------------------------------------------

    #[test]
    fn charset_plist_known() {
        let r = builtin_charset_plist(vec![Value::symbol("ascii")]).unwrap();
        // Standard charsets have empty plists.
        assert!(r.is_nil());
    }

    #[test]
    fn charset_plist_unknown() {
        let r = builtin_charset_plist(vec![Value::symbol("nonexistent")]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("charsetp"), Value::symbol("nonexistent")]
                );
            }
            other => panic!("expected wrong-type-argument charsetp, got {other:?}"),
        }
    }

    #[test]
    fn charset_plist_wrong_arg_count() {
        assert!(builtin_charset_plist(vec![]).is_err());
    }

    // -----------------------------------------------------------------------
    // Builtin tests: charset-id-internal
    // -----------------------------------------------------------------------

    #[test]
    fn charset_id_internal_requires_charset() {
        let r = builtin_charset_id_internal(vec![]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("charsetp"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn charset_id_internal_with_ascii() {
        let r = builtin_charset_id_internal(vec![Value::symbol("ascii")]).unwrap();
        assert!(matches!(r, Value::Int(0)));
    }

    #[test]
    fn charset_id_internal_with_unicode() {
        let r = builtin_charset_id_internal(vec![Value::symbol("unicode")]).unwrap();
        assert!(matches!(r, Value::Int(2)));
    }

    #[test]
    fn charset_id_internal_unknown_is_type_error() {
        let r = builtin_charset_id_internal(vec![Value::symbol("vm-no-such")]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("charsetp"), Value::symbol("vm-no-such")]
                );
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn charset_id_internal_wrong_arg_count() {
        assert!(builtin_charset_id_internal(vec![Value::Nil, Value::Nil]).is_err());
    }

    // -----------------------------------------------------------------------
    // Builtin tests: define-charset-internal
    // -----------------------------------------------------------------------

    #[test]
    fn define_charset_internal_requires_exact_arity() {
        assert!(builtin_define_charset_internal(vec![]).is_err());
        assert!(builtin_define_charset_internal(vec![Value::Nil; 16]).is_err());
        assert!(builtin_define_charset_internal(vec![Value::Nil; 18]).is_err());
    }

    #[test]
    fn define_charset_internal_exact_arity_validates_array_args() {
        let err = builtin_define_charset_internal(vec![Value::Nil; 17]).unwrap_err();
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("arrayp"), Value::Nil]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn define_charset_internal_exact_arity_returns_nil() {
        let mut args = vec![Value::Nil; 17];
        args[1] = Value::string("dimension");
        args[2] = Value::vector(vec![Value::Int(0), Value::Int(1)]);
        let r = builtin_define_charset_internal(args).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn define_charset_internal_short_dimension_vector_signals_args_out_of_range() {
        let mut args = vec![Value::Nil; 17];
        args[1] = Value::vector(vec![Value::Int(0)]);
        args[2] = Value::vector(vec![Value::Int(0)]);
        let err = builtin_define_charset_internal(args).unwrap_err();
        match err {
            Flow::Signal(sig) => {
                assert_eq!(sig.symbol, "args-out-of-range");
                assert_eq!(
                    sig.data,
                    vec![Value::vector(vec![Value::Int(0)]), Value::Int(1)]
                );
            }
            other => panic!("expected args-out-of-range signal, got {other:?}"),
        }
    }

    // -----------------------------------------------------------------------
    // Builtin tests: find-charset-region
    // -----------------------------------------------------------------------

    #[test]
    fn find_charset_region_ascii_default() {
        let r = builtin_find_charset_region(vec![Value::Int(1), Value::Int(100)]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "ascii"));
    }

    #[test]
    fn find_charset_region_with_table() {
        let r =
            builtin_find_charset_region(vec![Value::Int(1), Value::Int(100), Value::Nil]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
    }

    #[test]
    fn find_charset_region_wrong_arg_count() {
        assert!(builtin_find_charset_region(vec![Value::Int(1)]).is_err());
        assert!(builtin_find_charset_region(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Nil,
            Value::Nil,
        ])
        .is_err());
    }

    #[test]
    fn find_charset_region_eval_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("aé😀");
        }

        let all = builtin_find_charset_region_eval(&mut eval, vec![Value::Int(1), Value::Int(4)])
            .expect("find-charset-region all");
        assert_eq!(
            all,
            Value::list(vec![
                Value::symbol("ascii"),
                Value::symbol("unicode"),
                Value::symbol("unicode-bmp"),
            ])
        );

        let bmp = builtin_find_charset_region_eval(&mut eval, vec![Value::Int(2), Value::Int(3)])
            .expect("find-charset-region bmp");
        assert_eq!(bmp, Value::list(vec![Value::symbol("unicode-bmp")]));

        let empty = builtin_find_charset_region_eval(&mut eval, vec![Value::Int(4), Value::Int(4)])
            .expect("find-charset-region empty");
        assert_eq!(empty, Value::list(vec![Value::symbol("ascii")]));
    }

    #[test]
    fn find_charset_region_eval_out_of_range_errors() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("abc");
        }
        assert!(
            builtin_find_charset_region_eval(&mut eval, vec![Value::Int(0), Value::Int(2)])
                .is_err()
        );
        assert!(
            builtin_find_charset_region_eval(&mut eval, vec![Value::Int(1), Value::Int(5)])
                .is_err()
        );
    }

    // -----------------------------------------------------------------------
    // Builtin tests: find-charset-string
    // -----------------------------------------------------------------------

    #[test]
    fn find_charset_string_ascii() {
        let r = builtin_find_charset_string(vec![Value::string("hello")]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "ascii"));
    }

    #[test]
    fn find_charset_string_empty_is_nil() {
        let r = builtin_find_charset_string(vec![Value::string("")]).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn find_charset_string_bmp() {
        let r = builtin_find_charset_string(vec![Value::string("é")]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "unicode-bmp"));
    }

    #[test]
    fn find_charset_string_unicode_supplementary() {
        let r = builtin_find_charset_string(vec![Value::string("😀")]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
        assert!(matches!(&items[0], Value::Symbol(id) if resolve_sym(*id) == "unicode"));
    }

    #[test]
    fn find_charset_string_mixed_order_matches_oracle() {
        let mut s = String::from("a😀é");
        s.push(char::from_u32(0xE3FF).expect("valid unibyte sentinel"));
        let r = builtin_find_charset_string(vec![Value::string(s)]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 4);
        assert!(matches!(&items[0], Value::Symbol(v) if resolve_sym(*v) == "ascii"));
        assert!(matches!(&items[1], Value::Symbol(v) if resolve_sym(*v) == "unicode"));
        assert!(matches!(&items[2], Value::Symbol(v) if resolve_sym(*v) == "eight-bit"));
        assert!(matches!(&items[3], Value::Symbol(v) if resolve_sym(*v) == "unicode-bmp"));
    }

    #[test]
    fn find_charset_string_unibyte_ascii_and_eight_bit() {
        let mut s = String::new();
        s.push(char::from_u32(0xE341).expect("valid unibyte ascii sentinel"));
        s.push(char::from_u32(0xE3FF).expect("valid unibyte 255 sentinel"));
        let r = builtin_find_charset_string(vec![Value::string(s)]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 2);
        assert!(matches!(&items[0], Value::Symbol(v) if resolve_sym(*v) == "ascii"));
        assert!(matches!(&items[1], Value::Symbol(v) if resolve_sym(*v) == "eight-bit"));
    }

    #[test]
    fn find_charset_string_with_table() {
        let r = builtin_find_charset_string(vec![Value::string("hello"), Value::Nil]).unwrap();
        let items = list_to_vec(&r).unwrap();
        assert_eq!(items.len(), 1);
    }

    #[test]
    fn find_charset_string_wrong_type() {
        let r = builtin_find_charset_string(vec![Value::Int(1)]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(sig.data, vec![Value::symbol("stringp"), Value::Int(1)]);
            }
            other => panic!("expected wrong-type-argument signal, got {other:?}"),
        }
    }

    #[test]
    fn find_charset_string_wrong_arg_count() {
        assert!(builtin_find_charset_string(vec![]).is_err());
        assert!(
            builtin_find_charset_string(vec![Value::string("a"), Value::Nil, Value::Nil,]).is_err()
        );
    }

    // -----------------------------------------------------------------------
    // Builtin tests: decode-char
    // -----------------------------------------------------------------------

    #[test]
    fn decode_char_ascii() {
        let r = builtin_decode_char(vec![Value::symbol("ascii"), Value::Int(65)]).unwrap();
        assert!(matches!(r, Value::Int(65)));
    }

    #[test]
    fn decode_char_unicode() {
        let r = builtin_decode_char(vec![Value::symbol("unicode"), Value::Int(0x1F600)]).unwrap();
        assert!(matches!(r, Value::Int(0x1F600)));
    }

    #[test]
    fn decode_char_eight_bit_maps_to_raw_byte_range() {
        let r = builtin_decode_char(vec![Value::symbol("eight-bit"), Value::Int(255)]).unwrap();
        assert!(matches!(r, Value::Int(0x3FFFFF)));
    }

    #[test]
    fn decode_char_invalid_code_point() {
        let r = builtin_decode_char(vec![Value::symbol("unicode"), Value::Int(0xD800)]).unwrap();
        assert!(matches!(r, Value::Int(0xD800)));
    }

    #[test]
    fn decode_char_negative() {
        let r = builtin_decode_char(vec![Value::symbol("unicode"), Value::Int(-1)]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "Not an in-range integer, integral float, or cons of integers"
                    )]
                );
            }
            other => panic!("expected decode-char error signal, got {other:?}"),
        }
    }

    #[test]
    fn decode_char_out_of_range() {
        let r = builtin_decode_char(vec![Value::symbol("unicode"), Value::Int(0x110000)]).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn decode_char_unknown_charset() {
        let r = builtin_decode_char(vec![Value::symbol("nonexistent"), Value::Int(65)]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("charsetp"), Value::symbol("nonexistent")]
                );
            }
            other => panic!("expected wrong-type-argument charsetp, got {other:?}"),
        }
    }

    #[test]
    fn decode_char_wrong_type() {
        let r = builtin_decode_char(vec![Value::symbol("ascii"), Value::string("not an int")]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "error");
                assert_eq!(
                    sig.data,
                    vec![Value::string(
                        "Not an in-range integer, integral float, or cons of integers"
                    )]
                );
            }
            other => panic!("expected decode-char type error, got {other:?}"),
        }
    }

    #[test]
    fn decode_char_wrong_arg_count() {
        assert!(builtin_decode_char(vec![Value::symbol("ascii")]).is_err());
        assert!(
            builtin_decode_char(vec![Value::symbol("ascii"), Value::Int(65), Value::Nil]).is_err()
        );
    }

    // -----------------------------------------------------------------------
    // Builtin tests: encode-char
    // -----------------------------------------------------------------------

    #[test]
    fn encode_char_basic() {
        let r = builtin_encode_char(vec![Value::Int(65), Value::symbol("ascii")]).unwrap();
        assert!(matches!(r, Value::Int(65)));
    }

    #[test]
    fn encode_char_unicode() {
        let r = builtin_encode_char(vec![Value::Int(0x1F600), Value::symbol("unicode")]).unwrap();
        assert!(matches!(r, Value::Int(0x1F600)));
    }

    #[test]
    fn encode_char_eight_bit_raw_byte_maps_back_to_byte() {
        let r =
            builtin_encode_char(vec![Value::Int(0x3FFFFF), Value::symbol("eight-bit")]).unwrap();
        assert!(matches!(r, Value::Int(255)));
    }

    #[test]
    fn encode_char_with_char_value() {
        let r = builtin_encode_char(vec![Value::Char('Z'), Value::symbol("unicode")]).unwrap();
        assert!(matches!(r, Value::Int(90)));
    }

    #[test]
    fn encode_char_unknown_charset() {
        let r = builtin_encode_char(vec![Value::Int(65), Value::symbol("nonexistent")]);
        match r {
            Err(Flow::Signal(sig)) => {
                assert_eq!(sig.symbol, "wrong-type-argument");
                assert_eq!(
                    sig.data,
                    vec![Value::symbol("charsetp"), Value::symbol("nonexistent")]
                );
            }
            other => panic!("expected wrong-type-argument charsetp, got {other:?}"),
        }
    }

    #[test]
    fn encode_char_wrong_type() {
        assert!(
            builtin_encode_char(vec![Value::string("not a char"), Value::symbol("ascii")]).is_err()
        );
    }

    #[test]
    fn encode_char_wrong_arg_count() {
        assert!(builtin_encode_char(vec![Value::Int(65)]).is_err());
    }

    #[test]
    fn encode_decode_big5_sjis_basic_identity() {
        assert_eq!(
            builtin_encode_big5_char(vec![Value::Int(65)]).expect("encode-big5-char"),
            Value::Int(65)
        );
        assert_eq!(
            builtin_decode_big5_char(vec![Value::Int(65)]).expect("decode-big5-char"),
            Value::Int(65)
        );
        assert_eq!(
            builtin_encode_sjis_char(vec![Value::Int(65)]).expect("encode-sjis-char"),
            Value::Int(65)
        );
        assert_eq!(
            builtin_decode_sjis_char(vec![Value::Int(65)]).expect("decode-sjis-char"),
            Value::Int(65)
        );
    }

    #[test]
    fn get_unused_iso_final_char_known_values() {
        assert_eq!(
            builtin_get_unused_iso_final_char(vec![Value::Int(1), Value::Int(94)]).expect("1/94"),
            Value::Int(54)
        );
        assert_eq!(
            builtin_get_unused_iso_final_char(vec![Value::Int(1), Value::Int(96)]).expect("1/96"),
            Value::Int(51)
        );
        assert_eq!(
            builtin_get_unused_iso_final_char(vec![Value::Int(2), Value::Int(94)]).expect("2/94"),
            Value::Int(50)
        );
        assert_eq!(
            builtin_get_unused_iso_final_char(vec![Value::Int(2), Value::Int(96)]).expect("2/96"),
            Value::Int(48)
        );
        assert_eq!(
            builtin_get_unused_iso_final_char(vec![Value::Int(3), Value::Int(94)]).expect("3/94"),
            Value::Int(48)
        );
    }

    #[test]
    fn get_unused_iso_final_char_validates_dimension_and_chars() {
        let bad_dimension = builtin_get_unused_iso_final_char(vec![Value::Int(0), Value::Int(94)])
            .expect_err("dimension 0 should error");
        assert!(matches!(bad_dimension, Flow::Signal(_)));

        let bad_chars = builtin_get_unused_iso_final_char(vec![Value::Int(1), Value::Int(0)])
            .expect_err("chars 0 should error");
        assert!(matches!(bad_chars, Flow::Signal(_)));
    }

    #[test]
    fn declare_equiv_charset_validates_and_accepts_valid_tuple() {
        assert!(builtin_declare_equiv_charset(vec![
            Value::Int(1),
            Value::Int(94),
            Value::Int(65),
            Value::symbol("ascii"),
        ])
        .is_ok());

        assert!(builtin_declare_equiv_charset(vec![
            Value::Int(0),
            Value::Int(94),
            Value::Int(65),
            Value::symbol("ascii"),
        ])
        .is_err());
        assert!(builtin_declare_equiv_charset(vec![
            Value::Int(1),
            Value::Int(0),
            Value::Int(65),
            Value::symbol("ascii"),
        ])
        .is_err());
        assert!(builtin_declare_equiv_charset(vec![
            Value::Int(1),
            Value::Int(94),
            Value::symbol("A"),
            Value::symbol("ascii"),
        ])
        .is_err());
    }

    #[test]
    fn define_charset_alias_adds_symbol_alias_only() {
        assert!(builtin_define_charset_alias(vec![
            Value::symbol("latin-1"),
            Value::symbol("latin-iso8859-1"),
        ])
        .is_ok());
        assert!(builtin_charsetp(vec![Value::symbol("latin-1")])
            .expect("charsetp latin-1")
            .is_truthy());
        assert_eq!(
            builtin_charset_id_internal(vec![Value::symbol("latin-1")]).expect("id latin-1"),
            Value::Int(5)
        );

        // Non-symbol aliases are accepted but do not register a new symbol alias.
        assert!(builtin_define_charset_alias(vec![Value::Int(1), Value::symbol("ascii")]).is_ok());
    }

    // -----------------------------------------------------------------------
    // Builtin tests: clear-charset-maps
    // -----------------------------------------------------------------------

    #[test]
    fn clear_charset_maps_returns_nil() {
        let r = builtin_clear_charset_maps(vec![]).unwrap();
        assert!(r.is_nil());
    }

    #[test]
    fn clear_charset_maps_wrong_arg_count() {
        assert!(builtin_clear_charset_maps(vec![Value::Nil]).is_err());
    }

    // -----------------------------------------------------------------------
    // Builtin tests: charset-after
    // -----------------------------------------------------------------------

    #[test]
    fn charset_after_default_returns_unicode() {
        let r = builtin_charset_after(vec![]).unwrap();
        assert!(matches!(r, Value::Symbol(id) if resolve_sym(id) == "unicode"));
    }

    #[test]
    fn charset_after_with_pos() {
        let r = builtin_charset_after(vec![Value::Int(42)]).unwrap();
        assert!(matches!(r, Value::Symbol(id) if resolve_sym(id) == "unicode"));
    }

    #[test]
    fn charset_after_wrong_arg_count() {
        assert!(builtin_charset_after(vec![Value::Int(1), Value::Int(2)]).is_err());
    }

    #[test]
    fn charset_after_eval_semantics() {
        let mut eval = super::super::eval::Evaluator::new();
        {
            let buf = eval
                .buffers
                .current_buffer_mut()
                .expect("current buffer must exist");
            buf.insert("aé😀");
        }

        // No arg uses char after point; after insert point is at EOB.
        assert!(builtin_charset_after_eval(&mut eval, vec![])
            .expect("charset-after no arg")
            .is_nil());

        assert_eq!(
            builtin_charset_after_eval(&mut eval, vec![Value::Int(1)]).expect("pos 1"),
            Value::symbol("ascii")
        );
        assert_eq!(
            builtin_charset_after_eval(&mut eval, vec![Value::Int(2)]).expect("pos 2"),
            Value::symbol("unicode-bmp")
        );
        assert_eq!(
            builtin_charset_after_eval(&mut eval, vec![Value::Int(3)]).expect("pos 3"),
            Value::symbol("unicode")
        );
        assert!(builtin_charset_after_eval(&mut eval, vec![Value::Int(4)])
            .expect("pos 4")
            .is_nil());
        assert!(builtin_charset_after_eval(&mut eval, vec![Value::Int(0)])
            .expect("pos 0")
            .is_nil());
        assert!(builtin_charset_after_eval(&mut eval, vec![Value::Int(10)])
            .expect("pos 10")
            .is_nil());
        assert!(builtin_charset_after_eval(&mut eval, vec![Value::string("x")]).is_err());
    }

    // -----------------------------------------------------------------------
    // Round-trip tests
    // -----------------------------------------------------------------------

    #[test]
    fn decode_encode_round_trip() {
        // decode-char then encode-char should give the same code-point.
        let code = 0x00E9_i64; // e-acute
        let decoded =
            builtin_decode_char(vec![Value::symbol("unicode"), Value::Int(code)]).unwrap();
        let cp = decoded.as_int().unwrap();
        let encoded = builtin_encode_char(vec![Value::Int(cp), Value::symbol("unicode")]).unwrap();
        assert!(matches!(encoded, Value::Int(n) if n == code));
    }

    #[test]
    fn charsetp_all_standard() {
        for name in &[
            "ascii",
            "unicode",
            "unicode-bmp",
            "latin-iso8859-1",
            "emacs",
            "eight-bit",
        ] {
            let r = builtin_charsetp(vec![Value::symbol(*name)]).unwrap();
            assert!(
                matches!(r, Value::True),
                "charsetp should return t for {}",
                name
            );
        }
    }
}
