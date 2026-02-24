//! Lisp value representation and fundamental operations.

use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::sync::{
    atomic::{AtomicU64, Ordering},
    Mutex, OnceLock,
};

use super::intern::{intern, resolve_sym, SymId};
use crate::gc::heap::LispHeap;
use crate::gc::types::ObjId;

const ZERO_COUNT: u64 = 0;

static CONS_CELLS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static FLOATS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static VECTOR_CELLS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static SYMBOLS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static STRING_CHARS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static INTERVALS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static STRINGS_CONSED: AtomicU64 = AtomicU64::new(ZERO_COUNT);
static STRING_TEXT_PROPS: OnceLock<Mutex<HashMap<usize, Vec<StringTextPropertyRun>>>> =
    OnceLock::new();

fn add_wrapping(counter: &AtomicU64, delta: u64) {
    counter.fetch_add(delta, Ordering::Relaxed);
}

fn as_neovm_int(value: u64) -> i64 {
    value as i64
}

fn string_text_props() -> &'static Mutex<HashMap<usize, Vec<StringTextPropertyRun>>> {
    STRING_TEXT_PROPS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Clear all string text properties (must be called when heap changes,
/// e.g. when creating a new Evaluator for test isolation).
pub fn reset_string_text_properties() {
    string_text_props().lock().expect("string text props poisoned").clear();
}

// ---------------------------------------------------------------------------
// Thread-local heap access
// ---------------------------------------------------------------------------

thread_local! {
    static CURRENT_HEAP: Cell<*mut LispHeap> = const { Cell::new(std::ptr::null_mut()) };
    /// Auto-allocated heap for tests that construct Values without an Evaluator.
    #[cfg(test)]
    static TEST_FALLBACK_HEAP: std::cell::RefCell<Option<Box<LispHeap>>> = const { std::cell::RefCell::new(None) };
}

/// Set the current thread-local heap pointer.
/// Must be called before any Value constructors that allocate on the heap.
pub fn set_current_heap(heap: &mut LispHeap) {
    CURRENT_HEAP.with(|h| h.set(heap as *mut LispHeap));
}

/// Clear the thread-local heap pointer.
pub fn clear_current_heap() {
    CURRENT_HEAP.with(|h| h.set(std::ptr::null_mut()));
}

/// Returns true if a thread-local heap is currently set.
pub fn has_current_heap() -> bool {
    CURRENT_HEAP.with(|h| !h.get().is_null())
}

/// Save and restore the current heap pointer around a closure.
/// Used when a temporary Evaluator is created that would overwrite the thread-local.
pub(crate) fn with_saved_heap<R>(f: impl FnOnce() -> R) -> R {
    let saved = CURRENT_HEAP.with(|h| h.get());
    let result = f();
    CURRENT_HEAP.with(|h| h.set(saved));
    result
}

/// Get raw pointer to the current heap. Panics if not set (unless in test mode,
/// where a fallback heap is auto-created).
#[inline]
pub(crate) fn current_heap_ptr() -> *mut LispHeap {
    CURRENT_HEAP.with(|h| {
        let ptr = h.get();
        if !ptr.is_null() {
            return ptr;
        }
        #[cfg(test)]
        {
            // Auto-create a heap for tests that don't use Evaluator.
            TEST_FALLBACK_HEAP.with(|fb| {
                let mut borrow = fb.borrow_mut();
                if borrow.is_none() {
                    *borrow = Some(Box::new(LispHeap::new()));
                }
                let heap_ref: &mut LispHeap = borrow.as_mut().unwrap();
                let ptr = heap_ref as *mut LispHeap;
                h.set(ptr);
                ptr
            })
        }
        #[cfg(not(test))]
        {
            panic!("current heap not set — call set_current_heap() first");
        }
    })
}

/// Immutable access to the current thread-local heap.
///
/// # Safety
/// The returned reference is valid only for the duration of `f`.
/// Do NOT call `with_heap` or `with_heap_mut` from within `f`.
#[inline]
pub(crate) fn with_heap<R>(f: impl FnOnce(&LispHeap) -> R) -> R {
    let ptr = current_heap_ptr();
    f(unsafe { &*ptr })
}

/// Mutable access to the current thread-local heap.
///
/// # Safety
/// The returned reference is valid only for the duration of `f`.
/// Do NOT call `with_heap` or `with_heap_mut` from within `f`.
#[inline]
pub(crate) fn with_heap_mut<R>(f: impl FnOnce(&mut LispHeap) -> R) -> R {
    let ptr = current_heap_ptr();
    f(unsafe { &mut *ptr })
}

/// Snapshot of a cons cell's car and cdr values.
///
/// Returned by `read_cons()`. Used as a drop-in replacement for the
/// old `MutexGuard<ConsCell>` pattern: `pair.car` / `pair.cdr` just work.
pub struct ConsSnapshot {
    pub car: Value,
    pub cdr: Value,
}

/// A string text property run used by printed propertized-string literals.
#[derive(Clone, Debug, PartialEq)]
pub struct StringTextPropertyRun {
    pub start: usize,
    pub end: usize,
    pub plist: Value,
}

pub fn set_string_text_properties(
    id: ObjId,
    runs: Vec<StringTextPropertyRun>,
) {
    let key = obj_id_to_key(id);
    let mut props = string_text_props().lock().expect("string text props poisoned");
    if runs.is_empty() {
        props.remove(&key);
    } else {
        props.insert(key, runs);
    }
}

pub fn get_string_text_properties(id: ObjId) -> Option<Vec<StringTextPropertyRun>> {
    let key = obj_id_to_key(id);
    let props = string_text_props().lock().expect("string text props poisoned");
    props.get(&key).cloned()
}

fn obj_id_to_key(id: ObjId) -> usize {
    ((id.index as usize) << 32) | (id.generation as usize)
}

/// Read car and cdr from a cons cell on the heap.
///
/// Drop-in replacement for `cell.lock().expect("poisoned")`.
#[inline]
pub fn read_cons(id: ObjId) -> ConsSnapshot {
    with_heap(|h| ConsSnapshot {
        car: h.cons_car(id),
        cdr: h.cons_cdr(id),
    })
}

// ---------------------------------------------------------------------------
// Core value types
// ---------------------------------------------------------------------------

/// Runtime Lisp value.
///
/// All heap-allocated types use `ObjId` handles into a thread-local `LispHeap`.
/// Symbol, Keyword, and Subr names use `SymId` handles into a thread-local
/// `StringInterner`, making Value `Copy` and 16 bytes.
#[derive(Clone, Copy, Debug)]
pub enum Value {
    Nil,
    /// `t` — the canonical true value.
    True,
    Int(i64),
    Float(f64),
    Symbol(SymId),
    Keyword(SymId),
    Str(ObjId),
    Cons(ObjId),
    Vector(ObjId),
    HashTable(ObjId),
    Lambda(ObjId),
    Macro(ObjId),
    Char(char),
    /// Subr = built-in function reference (name).  Dispatched by the evaluator.
    Subr(SymId),
    /// Compiled bytecode function.
    ByteCode(ObjId),
    /// Buffer reference (opaque id into the BufferManager).
    Buffer(crate::buffer::BufferId),
    /// Window reference (opaque id into the FrameManager).
    Window(u64),
    /// Frame reference (opaque id into the FrameManager).
    Frame(u64),
    /// Timer reference (opaque id into the TimerManager).
    Timer(u64),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        equal_value(self, other, 0)
    }
}

/// Shared representation for lambda and macro bodies.
#[derive(Clone, Debug)]
pub struct LambdaData {
    pub params: LambdaParams,
    pub body: Vec<super::expr::Expr>,
    /// For lexical closures: captured environment snapshot.
    pub env: Option<Vec<HashMap<SymId, Value>>>,
    pub docstring: Option<String>,
}

/// Describes a lambda parameter list including &optional and &rest.
#[derive(Clone, Debug)]
pub struct LambdaParams {
    pub required: Vec<String>,
    pub optional: Vec<String>,
    pub rest: Option<String>,
}

impl LambdaParams {
    pub fn simple(names: Vec<String>) -> Self {
        Self {
            required: names,
            optional: Vec::new(),
            rest: None,
        }
    }

    /// Total minimum arity.
    pub fn min_arity(&self) -> usize {
        self.required.len()
    }

    /// Total maximum arity (None = unbounded due to &rest).
    pub fn max_arity(&self) -> Option<usize> {
        if self.rest.is_some() {
            None
        } else {
            Some(self.required.len() + self.optional.len())
        }
    }
}

/// Hash table with configurable test function.
#[derive(Clone, Debug)]
pub struct LispHashTable {
    pub test: HashTableTest,
    /// Symbol name provided via `:test` at construction time.
    /// For user-defined tests this preserves the alias returned by
    /// `hash-table-test`.
    pub test_name: Option<String>,
    pub size: i64,
    pub weakness: Option<HashTableWeakness>,
    pub rehash_size: f64,
    pub rehash_threshold: f64,
    pub data: HashMap<HashKey, Value>,
    /// Original key objects for diagnostics/iteration where pointer-identity
    /// keys cannot be reconstructed from `HashKey`.
    pub key_snapshots: HashMap<HashKey, Value>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HashTableTest {
    Eq,
    Eql,
    Equal,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HashTableWeakness {
    Key,
    Value,
    KeyOrValue,
    KeyAndValue,
}

/// Key type that supports hashing for `eq`, `eql`, and `equal` tests.
/// For simplicity, we normalize keys to a hashable representation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HashKey {
    Nil,
    True,
    Int(i64),
    Float(u64), // bits
    Symbol(SymId),
    Keyword(SymId),
    Str(String),
    Char(char),
    Window(u64),
    Frame(u64),
    /// Pointer identity for eq hash tables (legacy, unused with ObjId migration).
    Ptr(usize),
    /// Object identity for eq hash tables (heap-allocated types).
    ObjId(u32, u32),
}

impl LispHashTable {
    pub fn new(test: HashTableTest) -> Self {
        Self::new_with_options(test, 0, None, 1.5, 0.8125)
    }

    pub fn new_with_options(
        test: HashTableTest,
        size: i64,
        weakness: Option<HashTableWeakness>,
        rehash_size: f64,
        rehash_threshold: f64,
    ) -> Self {
        Self {
            test,
            test_name: None,
            size,
            weakness,
            rehash_size,
            rehash_threshold,
            data: HashMap::with_capacity(size.max(0) as usize),
            key_snapshots: HashMap::with_capacity(size.max(0) as usize),
        }
    }
}

// ---------------------------------------------------------------------------
// Value constructors
// ---------------------------------------------------------------------------

impl Value {
    pub fn t() -> Self {
        Value::True
    }

    pub fn bool(b: bool) -> Self {
        if b {
            Value::True
        } else {
            Value::Nil
        }
    }

    pub fn symbol(s: impl AsRef<str>) -> Self {
        let s = s.as_ref();
        if s == "nil" {
            Value::Nil
        } else if s == "t" {
            Value::True
        } else {
            add_wrapping(&SYMBOLS_CONSED, 1);
            Value::Symbol(intern(s))
        }
    }

    pub fn keyword(s: impl AsRef<str>) -> Self {
        add_wrapping(&SYMBOLS_CONSED, 1);
        Value::Keyword(intern(s.as_ref()))
    }

    pub fn string(s: impl Into<String>) -> Self {
        let s = s.into();
        add_wrapping(&STRINGS_CONSED, 1);
        add_wrapping(&STRING_CHARS_CONSED, s.len() as u64);
        let id = with_heap_mut(|heap| heap.alloc_string(s));
        Value::Str(id)
    }

    pub fn string_with_text_properties(
        s: impl Into<String>,
        runs: Vec<StringTextPropertyRun>,
    ) -> Self {
        let value = Self::string(s);
        if let Value::Str(id) = &value {
            set_string_text_properties(*id, runs);
        }
        value
    }

    pub fn make_lambda(data: LambdaData) -> Self {
        let id = with_heap_mut(|heap| heap.alloc_lambda(data));
        Value::Lambda(id)
    }

    pub fn make_macro(data: LambdaData) -> Self {
        let id = with_heap_mut(|heap| heap.alloc_macro(data));
        Value::Macro(id)
    }

    pub fn make_bytecode(bc: super::bytecode::ByteCodeFunction) -> Self {
        let id = with_heap_mut(|heap| heap.alloc_bytecode(bc));
        Value::ByteCode(id)
    }

    pub fn cons(car: Value, cdr: Value) -> Self {
        add_wrapping(&CONS_CELLS_CONSED, 1);
        let id = with_heap_mut(|heap| heap.alloc_cons(car, cdr));
        Value::Cons(id)
    }

    pub fn list(values: Vec<Value>) -> Self {
        values
            .into_iter()
            .rev()
            .fold(Value::Nil, |acc, item| Value::cons(item, acc))
    }

    pub fn vector(values: Vec<Value>) -> Self {
        add_wrapping(&VECTOR_CELLS_CONSED, values.len() as u64);
        let id = with_heap_mut(|heap| heap.alloc_vector(values));
        Value::Vector(id)
    }

    pub fn hash_table(test: HashTableTest) -> Self {
        add_wrapping(&VECTOR_CELLS_CONSED, 1);
        let id = with_heap_mut(|heap| heap.alloc_hash_table(test));
        Value::HashTable(id)
    }

    pub fn hash_table_with_options(
        test: HashTableTest,
        size: i64,
        weakness: Option<HashTableWeakness>,
        rehash_size: f64,
        rehash_threshold: f64,
    ) -> Self {
        add_wrapping(&VECTOR_CELLS_CONSED, 1);
        let id = with_heap_mut(|heap| {
            heap.alloc_hash_table_with_options(test, size, weakness, rehash_size, rehash_threshold)
        });
        Value::HashTable(id)
    }

    pub(crate) fn memory_use_counts_snapshot() -> [i64; 7] {
        [
            as_neovm_int(CONS_CELLS_CONSED.load(Ordering::Relaxed)),
            as_neovm_int(FLOATS_CONSED.load(Ordering::Relaxed)),
            as_neovm_int(VECTOR_CELLS_CONSED.load(Ordering::Relaxed)),
            as_neovm_int(SYMBOLS_CONSED.load(Ordering::Relaxed)),
            as_neovm_int(STRING_CHARS_CONSED.load(Ordering::Relaxed)),
            as_neovm_int(INTERVALS_CONSED.load(Ordering::Relaxed)),
            as_neovm_int(STRINGS_CONSED.load(Ordering::Relaxed)),
        ]
    }

    // -----------------------------------------------------------------------
    // Heap accessor methods (via thread-local)
    // -----------------------------------------------------------------------

    /// Get the car of a cons cell.
    pub fn cons_car(&self) -> Value {
        match self {
            Value::Cons(id) => with_heap(|h| h.cons_car(*id)),
            _ => panic!("cons_car on non-cons: {}", self.type_name()),
        }
    }

    /// Get the cdr of a cons cell.
    pub fn cons_cdr(&self) -> Value {
        match self {
            Value::Cons(id) => with_heap(|h| h.cons_cdr(*id)),
            _ => panic!("cons_cdr on non-cons: {}", self.type_name()),
        }
    }

    /// Set the car of a cons cell.
    pub fn set_car(&self, val: Value) {
        match self {
            Value::Cons(id) => with_heap_mut(|h| h.set_car(*id, val)),
            _ => panic!("set_car on non-cons: {}", self.type_name()),
        }
    }

    /// Set the cdr of a cons cell.
    pub fn set_cdr(&self, val: Value) {
        match self {
            Value::Cons(id) => with_heap_mut(|h| h.set_cdr(*id, val)),
            _ => panic!("set_cdr on non-cons: {}", self.type_name()),
        }
    }

    // -----------------------------------------------------------------------
    // Type predicates
    // -----------------------------------------------------------------------

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_nil()
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Value::Nil | Value::Cons(_))
    }

    pub fn is_cons(&self) -> bool {
        matches!(self, Value::Cons(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Int(_) | Value::Float(_))
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Value::Float(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Value::Nil | Value::True | Value::Symbol(_))
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self, Value::Keyword(_))
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Value::Char(_))
    }

    pub fn is_hash_table(&self) -> bool {
        matches!(self, Value::HashTable(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Lambda(_) | Value::Subr(_) | Value::ByteCode(_))
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "symbol",
            Value::True => "symbol",
            Value::Int(_) => "integer",
            Value::Float(_) => "float",
            Value::Symbol(_) => "symbol",
            Value::Keyword(_) => "symbol",
            Value::Str(_) => "string",
            Value::Cons(_) => "cons",
            Value::Vector(_) => "vector",
            Value::HashTable(_) => "hash-table",
            Value::Lambda(_) => "function",
            Value::Macro(_) => "macro",
            Value::Char(_) => "integer", // Emacs chars are integers
            Value::Subr(_) => "subr",
            Value::ByteCode(_) => "byte-code-function",
            Value::Buffer(_) => "buffer",
            Value::Window(_) => "window",
            Value::Frame(_) => "frame",
            Value::Timer(_) => "timer",
        }
    }

    /// Extract as number (int or float).  Promotes int → float if needed.
    pub fn as_number_f64(&self) -> Option<f64> {
        match self {
            Value::Int(n) => Some(*n as f64),
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            Value::Char(c) => Some(*c as i64),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Borrow the string contents from the heap.
    ///
    /// # Safety
    /// The returned reference borrows from the thread-local heap.  It is valid
    /// as long as no GC collection occurs (which would free/move objects).
    /// This is safe at normal call sites because GC only runs at explicit safe
    /// points (`gc_safe_point`), never during a borrow.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::Str(id) => {
                let ptr = current_heap_ptr();
                let heap = unsafe { &*ptr };
                Some(heap.get_string(*id).as_str())
            }
            _ => None,
        }
    }

    /// Get an owned copy of the string contents.
    pub fn as_str_owned(&self) -> Option<String> {
        self.as_str().map(|s| s.to_owned())
    }

    /// Access the heap string via a closure.
    pub fn with_str<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        self.as_str().map(f)
    }

    pub fn as_symbol_name(&self) -> Option<&str> {
        match self {
            Value::Nil => Some("nil"),
            Value::True => Some("t"),
            Value::Symbol(id) => Some(resolve_sym(*id)),
            Value::Keyword(id) => Some(resolve_sym(*id)),
            _ => None,
        }
    }

    /// Check if this value is a symbol with the given name.
    /// Convenience for the common `if s == "foo"` pattern in match guards.
    pub fn is_symbol_named(&self, name: &str) -> bool {
        self.as_symbol_name() == Some(name)
    }

    /// Borrow the LambdaData from a Lambda or Macro value on the heap.
    pub fn get_lambda_data(&self) -> Option<&LambdaData> {
        let ptr = current_heap_ptr();
        let heap = unsafe { &*ptr };
        match self {
            Value::Lambda(id) => Some(heap.get_lambda(*id)),
            Value::Macro(id) => Some(heap.get_macro_data(*id)),
            _ => None,
        }
    }

    /// Borrow the ByteCodeFunction from a ByteCode value on the heap.
    pub fn get_bytecode_data(&self) -> Option<&super::bytecode::ByteCodeFunction> {
        let ptr = current_heap_ptr();
        let heap = unsafe { &*ptr };
        match self {
            Value::ByteCode(id) => Some(heap.get_bytecode(*id)),
            _ => None,
        }
    }

    /// Get the ObjId of a string value (for text property operations).
    pub fn str_id(&self) -> Option<ObjId> {
        match self {
            Value::Str(id) => Some(*id),
            _ => None,
        }
    }

    /// Convert to hash key based on the hash table test.
    pub fn to_hash_key(&self, test: &HashTableTest) -> HashKey {
        match test {
            HashTableTest::Eq => self.to_eq_key(),
            HashTableTest::Eql => self.to_eql_key(),
            HashTableTest::Equal => self.to_equal_key(),
        }
    }

    fn to_eq_key(&self) -> HashKey {
        match self {
            Value::Nil => HashKey::Nil,
            Value::True => HashKey::True,
            Value::Int(n) => HashKey::Int(*n),
            Value::Float(f) => HashKey::Float(f.to_bits()),
            Value::Symbol(id) => HashKey::Symbol(*id),
            Value::Keyword(id) => HashKey::Keyword(*id),
            // Emacs chars are integers for equality/hash semantics.
            Value::Char(c) => HashKey::Int(*c as i64),
            // All heap-allocated types: use ObjId for identity
            Value::Cons(id) | Value::Vector(id) | Value::HashTable(id)
            | Value::Str(id) | Value::Lambda(id) | Value::Macro(id) | Value::ByteCode(id)
                => HashKey::ObjId(id.index, id.generation),
            Value::Subr(id) => HashKey::Symbol(*id),
            Value::Buffer(id) => HashKey::Int(id.0 as i64),
            Value::Window(id) => HashKey::Window(*id),
            Value::Frame(id) => HashKey::Frame(*id),
            Value::Timer(id) => HashKey::Int(*id as i64),
        }
    }

    fn to_eql_key(&self) -> HashKey {
        match self {
            // eql is like eq but also does value-equality for numbers
            Value::Int(n) => HashKey::Int(*n),
            Value::Float(f) => HashKey::Float(f.to_bits()),
            Value::Char(c) => HashKey::Int(*c as i64),
            other => other.to_eq_key(),
        }
    }

    fn to_equal_key(&self) -> HashKey {
        match self {
            Value::Nil => HashKey::Nil,
            Value::True => HashKey::True,
            Value::Int(n) => HashKey::Int(*n),
            Value::Float(f) => HashKey::Float(f.to_bits()),
            Value::Symbol(id) => HashKey::Symbol(*id),
            Value::Keyword(id) => HashKey::Keyword(*id),
            Value::Str(id) => HashKey::Str(with_heap(|h| h.get_string(*id).clone())),
            Value::Char(c) => HashKey::Int(*c as i64),
            Value::Window(id) => HashKey::Window(*id),
            Value::Frame(id) => HashKey::Frame(*id),
            // For compound types, fall back to eq identity
            other => other.to_eq_key(),
        }
    }
}

// ---------------------------------------------------------------------------
// Equality
// ---------------------------------------------------------------------------

/// `eq` — identity comparison.
pub fn eq_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::True, Value::True) => true,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Int(a), Value::Char(b)) => *a == *b as i64,
        (Value::Char(a), Value::Int(b)) => *a as i64 == *b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Keyword(a), Value::Keyword(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Cons(a), Value::Cons(b)) => a == b,
        (Value::Vector(a), Value::Vector(b)) => a == b,
        (Value::Lambda(a), Value::Lambda(b)) => a == b,
        (Value::Macro(a), Value::Macro(b)) => a == b,
        (Value::HashTable(a), Value::HashTable(b)) => a == b,
        (Value::Subr(a), Value::Subr(b)) => a == b,
        (Value::ByteCode(a), Value::ByteCode(b)) => a == b,
        (Value::Buffer(a), Value::Buffer(b)) => a == b,
        (Value::Window(a), Value::Window(b)) => a == b,
        (Value::Frame(a), Value::Frame(b)) => a == b,
        (Value::Timer(a), Value::Timer(b)) => a == b,
        _ => false,
    }
}

/// `eql` — like `eq` but also value-equality for numbers of same type.
pub fn eql_value(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
        _ => eq_value(left, right),
    }
}

/// `equal` — structural comparison.
pub fn equal_value(left: &Value, right: &Value, depth: usize) -> bool {
    if depth > 4096 {
        return false;
    }
    match (left, right) {
        (Value::Nil, Value::Nil) => true,
        (Value::True, Value::True) => true,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Int(a), Value::Char(b)) => *a == *b as i64,
        (Value::Char(a), Value::Int(b)) => *a as i64 == *b,
        (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Keyword(a), Value::Keyword(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => {
            if a == b { return true; }
            with_heap(|h| h.get_string(*a) == h.get_string(*b))
        }
        (Value::Cons(a), Value::Cons(b)) => {
            if a == b {
                return true;
            }
            let a_car = with_heap(|h| h.cons_car(*a));
            let a_cdr = with_heap(|h| h.cons_cdr(*a));
            let b_car = with_heap(|h| h.cons_car(*b));
            let b_cdr = with_heap(|h| h.cons_cdr(*b));
            equal_value(&a_car, &b_car, depth + 1) && equal_value(&a_cdr, &b_cdr, depth + 1)
        }
        (Value::Vector(a), Value::Vector(b)) => {
            if a == b {
                return true;
            }
            let av = with_heap(|h| h.get_vector(*a).clone());
            let bv = with_heap(|h| h.get_vector(*b).clone());
            av.len() == bv.len()
                && av
                    .iter()
                    .zip(bv.iter())
                    .all(|(x, y)| equal_value(x, y, depth + 1))
        }
        (Value::Lambda(a), Value::Lambda(b)) => a == b,
        (Value::Macro(a), Value::Macro(b)) => a == b,
        (Value::Subr(a), Value::Subr(b)) => a == b,
        (Value::ByteCode(a), Value::ByteCode(b)) => a == b,
        (Value::Buffer(a), Value::Buffer(b)) => a == b,
        (Value::Window(a), Value::Window(b)) => a == b,
        (Value::Frame(a), Value::Frame(b)) => a == b,
        (Value::Timer(a), Value::Timer(b)) => a == b,
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// List iteration helpers
// ---------------------------------------------------------------------------

/// Collect a proper list into a Vec.  Returns None if not a proper list.
pub fn list_to_vec(value: &Value) -> Option<Vec<Value>> {
    let mut result = Vec::new();
    let mut cursor = *value;
    loop {
        match cursor {
            Value::Nil => return Some(result),
            Value::Cons(id) => {
                result.push(with_heap(|h| h.cons_car(id)));
                cursor = with_heap(|h| h.cons_cdr(id));
            }
            _ => return None,
        }
    }
}

/// Length of a list (counts cons cells).  Returns None if improper list detected.
pub fn list_length(value: &Value) -> Option<usize> {
    let mut len = 0;
    let mut cursor = *value;
    loop {
        match cursor {
            Value::Nil => return Some(len),
            Value::Cons(id) => {
                len += 1;
                cursor = with_heap(|h| h.cons_cdr(id));
            }
            _ => return None,
        }
    }
}

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", super::print::print_value(self))
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: set up a temporary heap for tests that use Value constructors.
    fn with_test_heap<R>(f: impl FnOnce() -> R) -> R {
        let mut heap = LispHeap::new();
        set_current_heap(&mut heap);
        let result = f();
        clear_current_heap();
        result
    }

    #[test]
    fn value_constructors() {
        with_test_heap(|| {
            assert!(Value::Nil.is_nil());
            assert!(Value::t().is_truthy());
            assert!(Value::Int(42).is_integer());
            assert!(Value::Float(3.14).is_float());
            assert!(Value::string("hello").is_string());
            assert!(Value::Char('a').is_char());
            assert!(Value::symbol("foo").is_symbol());
            assert!(Value::keyword(":bar").is_keyword());
        });
    }

    #[test]
    fn list_round_trip() {
        with_test_heap(|| {
            let lst = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
            let vec = list_to_vec(&lst).unwrap();
            assert_eq!(vec.len(), 3);
        });
    }

    #[test]
    fn eq_identity() {
        with_test_heap(|| {
            assert!(eq_value(&Value::Nil, &Value::Nil));
            assert!(eq_value(&Value::Int(42), &Value::Int(42)));
            assert!(!eq_value(&Value::Int(1), &Value::Int(2)));
            assert!(eq_value(&Value::Char('a'), &Value::Int(97)));
            assert!(eq_value(&Value::Int(97), &Value::Char('a')));
            assert!(eq_value(&Value::symbol("foo"), &Value::symbol("foo")));
        });
    }

    #[test]
    fn equal_structural() {
        with_test_heap(|| {
            let a = Value::list(vec![Value::Int(1), Value::Int(2)]);
            let b = Value::list(vec![Value::Int(1), Value::Int(2)]);
            assert!(equal_value(&a, &b, 0));
            assert!(!eq_value(&a, &b));
        });
    }

    #[test]
    fn string_equality() {
        with_test_heap(|| {
            let a = Value::string("hello");
            let b = Value::string("hello");
            assert!(equal_value(&a, &b, 0));
            // eq compares ObjId identity — different allocations
            assert!(!eq_value(&a, &b));
        });
    }

    #[test]
    fn hash_key_char_int_equivalence() {
        for test in [HashTableTest::Eq, HashTableTest::Eql, HashTableTest::Equal] {
            let char_key = Value::Char('a').to_hash_key(&test);
            let int_key = Value::Int(97).to_hash_key(&test);
            assert_eq!(char_key, int_key);
        }
    }

    #[test]
    fn lambda_params_arity() {
        let p = LambdaParams {
            required: vec!["a".into(), "b".into()],
            optional: vec!["c".into()],
            rest: None,
        };
        assert_eq!(p.min_arity(), 2);
        assert_eq!(p.max_arity(), Some(3));

        let p2 = LambdaParams {
            required: vec!["a".into()],
            optional: vec![],
            rest: Some("rest".into()),
        };
        assert_eq!(p2.min_arity(), 1);
        assert_eq!(p2.max_arity(), None);
    }

    #[test]
    fn cons_accessors() {
        with_test_heap(|| {
            let c = Value::cons(Value::Int(1), Value::Int(2));
            assert_eq!(c.cons_car(), Value::Int(1));
            assert_eq!(c.cons_cdr(), Value::Int(2));
            c.set_car(Value::Int(10));
            assert_eq!(c.cons_car(), Value::Int(10));
        });
    }

    #[test]
    fn value_is_copy_and_16_bytes() {
        // Value is Copy — this assignment would fail to compile if not.
        let a = Value::Int(42);
        let b = a; // copy, not move
        let _ = a; // still usable after copy
        let _ = b;

        assert_eq!(
            std::mem::size_of::<Value>(),
            16,
            "Value should be 16 bytes (discriminant + largest variant)"
        );
    }
}
