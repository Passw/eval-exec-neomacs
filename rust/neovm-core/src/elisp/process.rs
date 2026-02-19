//! Process/subprocess management for the Elisp VM.
//!
//! Provides process abstractions: creating, killing, querying, and
//! communicating with subprocesses.  `start-process` creates a tracked
//! record; `call-process` and `shell-command-to-string` run real OS
//! commands via `std::process::Command`.

use std::collections::HashMap;
use std::fs::OpenOptions;
use std::process::{Command, Stdio};

use super::error::{signal, EvalResult, Flow};
use super::value::{list_to_vec, Value};

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Unique identifier for a process.
pub type ProcessId = u64;

/// Status of a managed process.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProcessStatus {
    Run,
    Stop,
    Exit(i32),
    Signal(i32),
}

/// A tracked process record.
#[derive(Clone, Debug)]
pub struct Process {
    pub id: ProcessId,
    pub name: String,
    pub command: String,
    pub args: Vec<String>,
    pub status: ProcessStatus,
    pub buffer_name: Option<String>,
    /// Queued input (sent via `process-send-string`).
    pub stdin_queue: String,
    /// Captured stdout.
    pub stdout: String,
    /// Captured stderr.
    pub stderr: String,
    /// Query-on-exit flag state.
    pub query_on_exit_flag: bool,
    /// Process filter callback (or default marker symbol).
    pub filter: Value,
    /// Process sentinel callback (or default marker symbol).
    pub sentinel: Value,
    /// Process plist state.
    pub plist: Value,
}

/// Manages the set of live processes.
#[derive(Clone, Debug)]
pub struct ProcessManager {
    processes: HashMap<ProcessId, Process>,
    next_id: ProcessId,
    /// Environment variable overrides (for `setenv`/`getenv`).
    env_overrides: HashMap<String, Option<String>>,
}

impl Default for ProcessManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ProcessManager {
    pub fn new() -> Self {
        Self {
            processes: HashMap::new(),
            next_id: 1,
            env_overrides: HashMap::new(),
        }
    }

    /// Create a new process record.  Returns the process id.
    pub fn create_process(
        &mut self,
        name: String,
        buffer_name: Option<String>,
        command: String,
        args: Vec<String>,
    ) -> ProcessId {
        let id = self.next_id;
        self.next_id += 1;
        let proc = Process {
            id,
            name,
            command,
            args,
            status: ProcessStatus::Run,
            buffer_name,
            stdin_queue: String::new(),
            stdout: String::new(),
            stderr: String::new(),
            query_on_exit_flag: true,
            filter: Value::symbol(DEFAULT_PROCESS_FILTER_SYMBOL),
            sentinel: Value::symbol(DEFAULT_PROCESS_SENTINEL_SYMBOL),
            plist: Value::Nil,
        };
        self.processes.insert(id, proc);
        id
    }

    /// Kill (remove) a process by id.  Returns true if found.
    pub fn kill_process(&mut self, id: ProcessId) -> bool {
        if let Some(proc) = self.processes.get_mut(&id) {
            proc.status = ProcessStatus::Signal(9);
            true
        } else {
            false
        }
    }

    /// Delete a process entirely.
    pub fn delete_process(&mut self, id: ProcessId) -> bool {
        self.processes.remove(&id).is_some()
    }

    /// Get process status.
    pub fn process_status(&self, id: ProcessId) -> Option<&ProcessStatus> {
        self.processes.get(&id).map(|p| &p.status)
    }

    /// Get a process by id.
    pub fn get(&self, id: ProcessId) -> Option<&Process> {
        self.processes.get(&id)
    }

    /// Get a mutable process by id.
    pub fn get_mut(&mut self, id: ProcessId) -> Option<&mut Process> {
        self.processes.get_mut(&id)
    }

    /// List all process ids.
    pub fn list_processes(&self) -> Vec<ProcessId> {
        self.processes.keys().copied().collect()
    }

    /// Find a process by name.
    pub fn find_by_name(&self, name: &str) -> Option<ProcessId> {
        self.processes
            .values()
            .find(|p| p.name == name)
            .map(|p| p.id)
    }

    /// Find a process associated with BUFFER-NAME.
    pub fn find_by_buffer_name(&self, buffer_name: &str) -> Option<ProcessId> {
        self.processes
            .values()
            .find(|p| p.buffer_name.as_deref() == Some(buffer_name))
            .map(|p| p.id)
    }

    /// Queue input for a process.
    pub fn send_input(&mut self, id: ProcessId, input: &str) -> bool {
        if let Some(proc) = self.processes.get_mut(&id) {
            proc.stdin_queue.push_str(input);
            true
        } else {
            false
        }
    }

    /// Get stdout output from a process.
    pub fn get_output(&self, id: ProcessId) -> Option<&str> {
        self.processes.get(&id).map(|p| p.stdout.as_str())
    }

    /// Get an environment variable (checking overrides first, then OS).
    pub fn getenv(&self, name: &str) -> Option<String> {
        if let Some(override_val) = self.env_overrides.get(name) {
            return override_val.clone();
        }
        std::env::var(name).ok()
    }

    /// Set an environment variable override.  If value is None, unset it.
    pub fn setenv(&mut self, name: String, value: Option<String>) {
        self.env_overrides.insert(name, value);
    }
}

const DEFAULT_PROCESS_FILTER_SYMBOL: &str = "internal-default-process-filter";
const DEFAULT_PROCESS_SENTINEL_SYMBOL: &str = "internal-default-process-sentinel";

// ---------------------------------------------------------------------------
// Helpers
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

fn expect_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        Value::Symbol(s) => Ok(s.clone()),
        Value::Nil => Ok("nil".to_string()),
        Value::True => Ok("t".to_string()),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("stringp"), other.clone()],
        )),
    }
}

fn signal_wrong_type_sequence(value: Value) -> Flow {
    signal(
        "wrong-type-argument",
        vec![Value::symbol("sequencep"), value],
    )
}

fn signal_wrong_type_character(value: Value) -> Flow {
    signal(
        "wrong-type-argument",
        vec![Value::symbol("characterp"), value],
    )
}

fn char_from_codepoint_value(value: &Value) -> Result<char, Flow> {
    match value {
        Value::Char(c) => Ok(*c),
        Value::Int(n) if *n >= 0 => {
            char::from_u32(*n as u32).ok_or_else(|| signal_wrong_type_character(value.clone()))
        }
        _ => Err(signal_wrong_type_character(value.clone())),
    }
}

fn sequence_value_to_env_string(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        Value::Vector(items) => {
            let chars = items
                .lock()
                .expect("poisoned")
                .iter()
                .cloned()
                .map(|item| char_from_codepoint_value(&item))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(chars.into_iter().collect())
        }
        Value::Cons(_) | Value::Nil => {
            let mut out = String::new();
            let mut cursor = value.clone();
            loop {
                match cursor {
                    Value::Nil => break,
                    Value::Cons(cell) => {
                        let (car, cdr) = {
                            let pair = cell.lock().expect("poisoned");
                            (pair.car.clone(), pair.cdr.clone())
                        };
                        out.push(char_from_codepoint_value(&car)?);
                        cursor = cdr;
                    }
                    tail => {
                        return Err(signal(
                            "wrong-type-argument",
                            vec![Value::symbol("listp"), tail],
                        ))
                    }
                }
            }
            Ok(out)
        }
        other => Err(signal_wrong_type_sequence(other.clone())),
    }
}

fn expect_int_or_marker(value: &Value) -> Result<i64, Flow> {
    match value {
        Value::Int(n) => Ok(*n),
        Value::Char(c) => Ok(*c as i64),
        v if super::marker::is_marker(v) => super::marker::marker_position_as_int(v),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("integer-or-marker-p"), other.clone()],
        )),
    }
}

fn checked_region_bytes(
    buf: &crate::buffer::Buffer,
    start: i64,
    end: i64,
) -> Result<(usize, usize), Flow> {
    let point_min = buf.text.byte_to_char(buf.point_min()) as i64 + 1;
    let point_max = buf.text.byte_to_char(buf.point_max()) as i64 + 1;
    if start < point_min || start > point_max || end < point_min || end > point_max {
        return Err(signal(
            "args-out-of-range",
            vec![Value::Buffer(buf.id), Value::Int(start), Value::Int(end)],
        ));
    }

    let start_byte = buf.text.char_to_byte((start - 1) as usize);
    let end_byte = buf.text.char_to_byte((end - 1) as usize);
    Ok(if start_byte <= end_byte {
        (start_byte, end_byte)
    } else {
        (end_byte, start_byte)
    })
}

fn file_error_symbol(kind: std::io::ErrorKind) -> &'static str {
    match kind {
        std::io::ErrorKind::NotFound => "file-missing",
        std::io::ErrorKind::AlreadyExists => "file-already-exists",
        std::io::ErrorKind::PermissionDenied => "permission-denied",
        _ => "file-error",
    }
}

fn signal_process_io(action: &str, target: Option<&str>, err: std::io::Error) -> Flow {
    let mut data = vec![Value::string(action), Value::string(err.to_string())];
    if let Some(target) = target {
        data.push(Value::string(target));
    }
    signal(file_error_symbol(err.kind()), data)
}

#[derive(Clone, Debug)]
enum OutputTarget {
    Discard,
    Buffer(Value),
    File(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StderrTarget {
    Discard,
    ToStdoutTarget,
    File,
}

#[derive(Clone, Debug)]
struct DestinationSpec {
    stdout: OutputTarget,
    stderr: StderrTarget,
    stderr_file: Option<String>,
    no_wait: bool,
}

fn signal_wrong_type_string(value: Value) -> Flow {
    signal("wrong-type-argument", vec![Value::symbol("stringp"), value])
}

fn expect_string_strict(value: &Value) -> Result<String, Flow> {
    match value {
        Value::Str(s) => Ok((**s).clone()),
        other => Err(signal_wrong_type_string(other.clone())),
    }
}

fn is_file_keyword(value: &Value) -> bool {
    matches!(value, Value::Keyword(k) if k == ":file" || k == "file")
}

fn parse_file_target(items: &[Value]) -> Result<OutputTarget, Flow> {
    let file_value = items.get(1).cloned().unwrap_or(Value::Nil);
    let file = expect_string_strict(&file_value)?;
    Ok(OutputTarget::File(file))
}

fn parse_real_buffer_destination(
    eval: &super::eval::Evaluator,
    value: &Value,
) -> Result<(OutputTarget, bool), Flow> {
    match value {
        Value::Int(_) => Ok((OutputTarget::Discard, true)),
        Value::Nil => Ok((OutputTarget::Discard, false)),
        Value::True | Value::Str(_) => Ok((OutputTarget::Buffer(value.clone()), false)),
        Value::Buffer(id) => {
            if eval.buffers.get(*id).is_none() {
                Err(signal(
                    "error",
                    vec![Value::string("Selecting deleted buffer")],
                ))
            } else {
                Ok((OutputTarget::Buffer(value.clone()), false))
            }
        }
        Value::Cons(_) => {
            let items =
                list_to_vec(value).ok_or_else(|| signal_wrong_type_string(value.clone()))?;
            let first = items.first().cloned().unwrap_or(Value::Nil);
            if is_file_keyword(&first) {
                Ok((parse_file_target(&items)?, false))
            } else {
                Err(signal_wrong_type_string(first))
            }
        }
        other => Err(signal_wrong_type_string(other.clone())),
    }
}

fn parse_stderr_destination(value: &Value) -> Result<(StderrTarget, Option<String>), Flow> {
    match value {
        Value::Nil => Ok((StderrTarget::Discard, None)),
        Value::True => Ok((StderrTarget::ToStdoutTarget, None)),
        Value::Str(s) => Ok((StderrTarget::File, Some((**s).clone()))),
        other => Err(signal_wrong_type_string(other.clone())),
    }
}

fn parse_call_process_destination(
    eval: &super::eval::Evaluator,
    destination: &Value,
) -> Result<DestinationSpec, Flow> {
    if let Value::Cons(_) = destination {
        let items = list_to_vec(destination)
            .ok_or_else(|| signal_wrong_type_string(destination.clone()))?;
        let first = items.first().cloned().unwrap_or(Value::Nil);
        if is_file_keyword(&first) {
            let stdout = parse_file_target(&items)?;
            return Ok(DestinationSpec {
                stdout,
                stderr: StderrTarget::ToStdoutTarget,
                stderr_file: None,
                no_wait: false,
            });
        }
        let second = items.get(1).cloned().unwrap_or(Value::Nil);
        let (stdout, no_wait) = parse_real_buffer_destination(eval, &first)?;
        let (stderr, stderr_file) = parse_stderr_destination(&second)?;
        return Ok(DestinationSpec {
            stdout,
            stderr,
            stderr_file,
            no_wait,
        });
    }

    let (stdout, no_wait) = parse_real_buffer_destination(eval, destination)?;
    let stderr = match destination {
        Value::Nil | Value::Int(_) => StderrTarget::Discard,
        _ => StderrTarget::ToStdoutTarget,
    };
    Ok(DestinationSpec {
        stdout,
        stderr,
        stderr_file: None,
        no_wait,
    })
}

fn insert_process_output(
    eval: &mut super::eval::Evaluator,
    destination: &Value,
    output: &str,
) -> Result<(), Flow> {
    match destination {
        Value::Str(name) => {
            let id = eval
                .buffers
                .find_buffer_by_name(name)
                .unwrap_or_else(|| eval.buffers.create_buffer(name));
            let buf = eval.buffers.get_mut(id).ok_or_else(|| {
                signal(
                    "error",
                    vec![Value::string("No such live buffer for process output")],
                )
            })?;
            buf.insert(output);
            Ok(())
        }
        Value::Buffer(id) => {
            let buf = eval
                .buffers
                .get_mut(*id)
                .ok_or_else(|| signal("error", vec![Value::string("Selecting deleted buffer")]))?;
            buf.insert(output);
            Ok(())
        }
        _ => {
            if let Some(buf) = eval.buffers.current_buffer_mut() {
                buf.insert(output);
            }
            Ok(())
        }
    }
}

fn write_output_target(
    eval: &mut super::eval::Evaluator,
    target: &OutputTarget,
    output: &[u8],
    append: bool,
) -> Result<(), Flow> {
    match target {
        OutputTarget::Discard => Ok(()),
        OutputTarget::Buffer(destination) => {
            let text = String::from_utf8_lossy(output).into_owned();
            insert_process_output(eval, destination, &text)
        }
        OutputTarget::File(path) => {
            if append {
                use std::io::Write;
                let mut file = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(path)
                    .map_err(|e| signal_process_io("Writing process output", Some(path), e))?;
                file.write_all(output)
                    .map_err(|e| signal_process_io("Writing process output", Some(path), e))
            } else {
                std::fs::write(path, output)
                    .map_err(|e| signal_process_io("Writing process output", Some(path), e))
            }
        }
    }
}

fn route_captured_output(
    eval: &mut super::eval::Evaluator,
    destination: &DestinationSpec,
    stdout: &[u8],
    stderr: &[u8],
) -> Result<(), Flow> {
    write_output_target(eval, &destination.stdout, stdout, false)?;
    match destination.stderr {
        StderrTarget::Discard => Ok(()),
        StderrTarget::ToStdoutTarget => {
            write_output_target(eval, &destination.stdout, stderr, true)
        }
        StderrTarget::File => {
            let path = destination
                .stderr_file
                .as_ref()
                .ok_or_else(|| signal("error", vec![Value::string("Missing stderr file target")]))?
                .clone();
            write_output_target(eval, &OutputTarget::File(path), stderr, false)
        }
    }
}

fn configure_call_process_stdin(command: &mut Command, infile: Option<&str>) -> Result<(), Flow> {
    match infile {
        None => {
            command.stdin(Stdio::null());
            Ok(())
        }
        Some(path) => {
            let file = std::fs::File::open(path)
                .map_err(|e| signal_process_io("Opening process input file", Some(path), e))?;
            command.stdin(Stdio::from(file));
            Ok(())
        }
    }
}

/// Resolve a process argument: either a ProcessId integer or a name string.
fn resolve_process(eval: &super::eval::Evaluator, value: &Value) -> Result<ProcessId, Flow> {
    match value {
        Value::Int(n) => {
            let id = *n as ProcessId;
            if eval.processes.get(id).is_some() {
                Ok(id)
            } else {
                Err(signal(
                    "error",
                    vec![Value::string(format!("No process {}", n))],
                ))
            }
        }
        Value::Str(s) => eval.processes.find_by_name(s).ok_or_else(|| {
            signal(
                "error",
                vec![Value::string(format!("No process named {}", s))],
            )
        }),
        Value::Symbol(s) => eval.processes.find_by_name(s).ok_or_else(|| {
            signal(
                "error",
                vec![Value::string(format!("No process named {}", s))],
            )
        }),
        other => Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("processp"), other.clone()],
        )),
    }
}

/// Resolve a live process designator for compatibility builtins.
///
/// NeoVM currently models process handles as integer ids.  These helpers treat
/// a live process id as a process designator for runtime parity surfaces.
fn resolve_live_process_designator(eval: &super::eval::Evaluator, value: &Value) -> Option<ProcessId> {
    match value {
        Value::Int(n) if *n >= 0 => {
            let id = *n as ProcessId;
            eval.processes.get(id).map(|_| id)
        }
        _ => None,
    }
}

fn resolve_live_process_or_wrong_type(
    eval: &super::eval::Evaluator,
    value: &Value,
) -> Result<ProcessId, Flow> {
    resolve_live_process_designator(eval, value).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("processp"), value.clone()],
        )
    })
}

fn resolve_optional_process_or_current_buffer(
    eval: &super::eval::Evaluator,
    value: Option<&Value>,
) -> Result<ProcessId, Flow> {
    if let Some(v) = value {
        if !v.is_nil() {
            return resolve_live_process_or_wrong_type(eval, v);
        }
    }

    let current_buffer_name = eval
        .buffers
        .current_buffer()
        .map(|buffer| buffer.name.clone())
        .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;

    eval.processes
        .find_by_buffer_name(&current_buffer_name)
        .ok_or_else(|| {
            signal(
                "error",
                vec![Value::string(format!(
                    "Buffer  {} has no process",
                    current_buffer_name
                ))],
            )
        })
}

fn process_live_status_value(status: &ProcessStatus) -> Value {
    match status {
        ProcessStatus::Run => Value::list(vec![
            Value::symbol("run"),
            Value::symbol("open"),
            Value::symbol("listen"),
            Value::symbol("connect"),
            Value::symbol("stop"),
        ]),
        ProcessStatus::Stop => Value::list(vec![Value::symbol("stop")]),
        ProcessStatus::Exit(_) | ProcessStatus::Signal(_) => Value::Nil,
    }
}

fn process_tty_stream_selector_p(value: &Value) -> bool {
    match value {
        Value::Nil => true,
        Value::Symbol(sym) => matches!(sym.as_str(), "stdin" | "stdout" | "stderr"),
        _ => false,
    }
}

// ---------------------------------------------------------------------------
// Builtins (eval-dependent)
// ---------------------------------------------------------------------------

/// (start-process NAME BUFFER PROGRAM &rest ARGS) -> process-id
pub(crate) fn builtin_start_process(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("start-process", &args, 3)?;
    let name = expect_string(&args[0])?;
    let buffer = if args[1].is_nil() {
        None
    } else {
        Some(expect_string(&args[1])?)
    };
    let program = expect_string(&args[2])?;
    let proc_args: Vec<String> = args[3..]
        .iter()
        .map(expect_string)
        .collect::<Result<Vec<_>, _>>()?;

    let id = eval
        .processes
        .create_process(name, buffer, program, proc_args);
    Ok(Value::Int(id as i64))
}

/// (call-process PROGRAM &optional INFILE DESTINATION DISPLAY &rest ARGS)
///
/// Runs the command synchronously using `std::process::Command`, captures
/// output.  Returns the exit code as an integer.
pub(crate) fn builtin_call_process(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("call-process", &args, 1)?;
    let program = expect_string(&args[0])?;

    let infile = if args.len() > 1 && !args[1].is_nil() {
        Some(expect_string_strict(&args[1])?)
    } else {
        None
    };

    let destination = if args.len() > 2 {
        &args[2]
    } else {
        &Value::Nil
    };
    let destination_spec = parse_call_process_destination(eval, destination)?;

    // DISPLAY (arg index 3): ignored in this implementation.

    let cmd_args: Vec<String> = if args.len() > 4 {
        args[4..]
            .iter()
            .map(expect_string)
            .collect::<Result<Vec<_>, _>>()?
    } else {
        Vec::new()
    };

    if destination_spec.no_wait {
        let mut command = Command::new(&program);
        command.args(&cmd_args).stdout(Stdio::null());
        configure_call_process_stdin(&mut command, infile.as_deref())?;
        match destination_spec.stderr {
            StderrTarget::Discard | StderrTarget::ToStdoutTarget => {
                command.stderr(Stdio::null());
            }
            StderrTarget::File => {
                let path = destination_spec.stderr_file.as_ref().ok_or_else(|| {
                    signal("error", vec![Value::string("Missing stderr file target")])
                })?;
                let file = OpenOptions::new()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open(path)
                    .map_err(|e| signal_process_io("Writing process output", Some(path), e))?;
                command.stderr(Stdio::from(file));
            }
        };

        let mut child = command
            .spawn()
            .map_err(|e| signal_process_io("Searching for program", Some(&program), e))?;
        std::thread::spawn(move || {
            let _ = child.wait();
        });
        return Ok(Value::Nil);
    }

    let mut command = Command::new(&program);
    command
        .args(&cmd_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    configure_call_process_stdin(&mut command, infile.as_deref())?;
    let output = command
        .output()
        .map_err(|e| signal_process_io("Searching for program", Some(&program), e))?;

    let exit_code = output.status.code().unwrap_or(-1);
    route_captured_output(eval, &destination_spec, &output.stdout, &output.stderr)?;
    Ok(Value::Int(exit_code as i64))
}

/// (call-process-region START END PROGRAM &optional DELETE DESTINATION DISPLAY &rest ARGS)
///
/// Pipes buffer region from START to END through PROGRAM.
pub(crate) fn builtin_call_process_region(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("call-process-region", &args, 3)?;
    let program = expect_string(&args[2])?;

    let delete = args.len() > 3 && args[3].is_truthy();
    let destination = if args.len() > 4 {
        &args[4]
    } else {
        &Value::Nil
    };
    let destination_spec = parse_call_process_destination(eval, destination)?;
    // DISPLAY (arg index 5): ignored.

    let cmd_args: Vec<String> = if args.len() > 6 {
        args[6..]
            .iter()
            .map(expect_string)
            .collect::<Result<Vec<_>, _>>()?
    } else {
        Vec::new()
    };

    // START semantics:
    // - nil => use whole buffer contents, ignore END
    // - string => use that string as stdin, ignore END
    // - integer/marker => use region START..END
    let region_text = match &args[0] {
        Value::Nil => {
            let (text, maybe_delete_range) = {
                let buf = eval
                    .buffers
                    .current_buffer()
                    .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
                let len = buf.text.len();
                (buf.text.text_range(0, len), (0usize, len))
            };
            if delete {
                let buf = eval
                    .buffers
                    .current_buffer_mut()
                    .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
                buf.delete_region(maybe_delete_range.0, maybe_delete_range.1);
            }
            text
        }
        Value::Str(s) => {
            if delete {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("integer-or-marker-p"), args[0].clone()],
                ));
            }
            (**s).clone()
        }
        _ => {
            let start = expect_int_or_marker(&args[0])?;
            let end = expect_int_or_marker(&args[1])?;
            let (text, region_beg, region_end) = {
                let buf = eval
                    .buffers
                    .current_buffer()
                    .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
                let (region_beg, region_end) = checked_region_bytes(buf, start, end)?;
                (
                    buf.text.text_range(region_beg, region_end),
                    region_beg,
                    region_end,
                )
            };

            if delete {
                let buf = eval
                    .buffers
                    .current_buffer_mut()
                    .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
                buf.delete_region(region_beg, region_end);
            }

            text
        }
    };

    use std::io::Write;
    if destination_spec.no_wait {
        let mut command = Command::new(&program);
        command
            .args(&cmd_args)
            .stdin(Stdio::piped())
            .stdout(Stdio::null());
        match destination_spec.stderr {
            StderrTarget::Discard | StderrTarget::ToStdoutTarget => {
                command.stderr(Stdio::null());
            }
            StderrTarget::File => {
                let path = destination_spec.stderr_file.as_ref().ok_or_else(|| {
                    signal("error", vec![Value::string("Missing stderr file target")])
                })?;
                let file = OpenOptions::new()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open(path)
                    .map_err(|e| signal_process_io("Writing process output", Some(path), e))?;
                command.stderr(Stdio::from(file));
            }
        };

        let mut child = command
            .spawn()
            .map_err(|e| signal_process_io("Searching for program", Some(&program), e))?;

        if let Some(mut stdin) = child.stdin.take() {
            let _ = stdin.write_all(region_text.as_bytes());
        }

        std::thread::spawn(move || {
            let _ = child.wait();
        });

        return Ok(Value::Nil);
    }

    let mut child = Command::new(&program)
        .args(&cmd_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| signal_process_io("Searching for program", Some(&program), e))?;

    if let Some(mut stdin) = child.stdin.take() {
        let _ = stdin.write_all(region_text.as_bytes());
    }

    let output = child
        .wait_with_output()
        .map_err(|e| signal_process_io("Process error", None, e))?;

    let exit_code = output.status.code().unwrap_or(-1);
    route_captured_output(eval, &destination_spec, &output.stdout, &output.stderr)?;
    Ok(Value::Int(exit_code as i64))
}

/// (delete-process PROCESS) -> nil
pub(crate) fn builtin_delete_process(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("delete-process", &args, 1)?;
    let id = resolve_process(eval, &args[0])?;
    eval.processes.delete_process(id);
    Ok(Value::Nil)
}

/// (process-send-string PROCESS STRING) -> nil
pub(crate) fn builtin_process_send_string(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-send-string", &args, 2)?;
    let id = resolve_process(eval, &args[0])?;
    let input = expect_string(&args[1])?;
    if !eval.processes.send_input(id, &input) {
        return Err(signal("error", vec![Value::string("Process not found")]));
    }
    Ok(Value::Nil)
}

/// (process-status PROCESS) -> symbol
pub(crate) fn builtin_process_status(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-status", &args, 1)?;
    let id = resolve_process(eval, &args[0])?;
    match eval.processes.process_status(id) {
        Some(ProcessStatus::Run) => Ok(Value::symbol("run")),
        Some(ProcessStatus::Stop) => Ok(Value::symbol("stop")),
        Some(ProcessStatus::Exit(_)) => Ok(Value::symbol("exit")),
        Some(ProcessStatus::Signal(_)) => Ok(Value::symbol("signal")),
        None => Err(signal("error", vec![Value::string("Process not found")])),
    }
}

/// (process-exit-status PROCESS) -> integer
pub(crate) fn builtin_process_exit_status(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-exit-status", &args, 1)?;
    let id = resolve_process(eval, &args[0])?;
    match eval.processes.process_status(id) {
        Some(ProcessStatus::Exit(code)) => Ok(Value::Int(*code as i64)),
        Some(ProcessStatus::Signal(sig)) => Ok(Value::Int(*sig as i64)),
        Some(_) => Ok(Value::Int(0)),
        None => Err(signal("error", vec![Value::string("Process not found")])),
    }
}

/// (process-list) -> list of process ids
pub(crate) fn builtin_process_list(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-list", &args, 0)?;
    let ids = eval.processes.list_processes();
    let values: Vec<Value> = ids.iter().map(|id| Value::Int(*id as i64)).collect();
    Ok(Value::list(values))
}

/// (process-name PROCESS) -> string
pub(crate) fn builtin_process_name(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-name", &args, 1)?;
    let id = resolve_process(eval, &args[0])?;
    match eval.processes.get(id) {
        Some(proc) => Ok(Value::string(proc.name.clone())),
        None => Err(signal("error", vec![Value::string("Process not found")])),
    }
}

/// (process-buffer PROCESS) -> string or nil
pub(crate) fn builtin_process_buffer(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-buffer", &args, 1)?;
    let id = resolve_process(eval, &args[0])?;
    match eval.processes.get(id) {
        Some(proc) => match &proc.buffer_name {
            Some(name) => Ok(Value::string(name.clone())),
            None => Ok(Value::Nil),
        },
        None => Err(signal("error", vec![Value::string("Process not found")])),
    }
}

/// (process-coding-system PROCESS) -> (decode . encode)
pub(crate) fn builtin_process_coding_system(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-coding-system", &args, 1)?;
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::cons(
        Value::symbol("utf-8-unix"),
        Value::symbol("utf-8-unix"),
    ))
}

/// (process-datagram-address PROCESS) -> address-or-nil
pub(crate) fn builtin_process_datagram_address(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-datagram-address", &args, 1)?;
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::Nil)
}

/// (process-inherit-coding-system-flag PROCESS) -> bool
pub(crate) fn builtin_process_inherit_coding_system_flag(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-inherit-coding-system-flag", &args, 1)?;
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::Nil)
}

/// (process-kill-buffer-query-function) -> bool
pub(crate) fn builtin_process_kill_buffer_query_function(args: Vec<Value>) -> EvalResult {
    expect_args("process-kill-buffer-query-function", &args, 0)?;
    Ok(Value::True)
}

/// (process-tty-name PROCESS &optional STREAM) -> string
pub(crate) fn builtin_process_tty_name(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("process-tty-name", &args, 1)?;
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("process-tty-name"), Value::Int(args.len() as i64)],
        ));
    }
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let stream = args.get(1).cloned().unwrap_or(Value::Nil);
    if !process_tty_stream_selector_p(&stream) {
        return Err(signal(
            "error",
            vec![Value::string("Unknown stream"), stream],
        ));
    }
    Ok(Value::string("/dev/pts/0"))
}

/// (process-mark PROCESS) -> marker
pub(crate) fn builtin_process_mark(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-mark", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval.processes.get(id).ok_or_else(|| {
        signal(
            "wrong-type-argument",
            vec![Value::symbol("processp"), args[0].clone()],
        )
    })?;
    Ok(super::marker::make_marker_value(
        proc.buffer_name.as_deref(),
        None,
        false,
    ))
}

/// (process-type PROCESS) -> symbol
pub(crate) fn builtin_process_type(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-type", &args, 1)?;
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::symbol("real"))
}

/// (process-thread PROCESS) -> object-or-nil
pub(crate) fn builtin_process_thread(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-thread", &args, 1)?;
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::Nil)
}

/// (process-send-region PROCESS START END) -> nil
pub(crate) fn builtin_process_send_region(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-send-region", &args, 3)?;
    let id = resolve_optional_process_or_current_buffer(eval, Some(&args[0]))?;
    let start = expect_int_or_marker(&args[1])?;
    let end = expect_int_or_marker(&args[2])?;

    let region_text = {
        let buf = eval
            .buffers
            .current_buffer()
            .ok_or_else(|| signal("error", vec![Value::string("No current buffer")]))?;
        let (region_beg, region_end) = checked_region_bytes(buf, start, end)?;
        buf.text.text_range(region_beg, region_end)
    };

    if !eval.processes.send_input(id, &region_text) {
        return Err(signal("error", vec![Value::string("Process not found")]));
    }
    Ok(Value::Nil)
}

/// (process-send-eof &optional PROCESS) -> process
pub(crate) fn builtin_process_send_eof(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 1 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("process-send-eof"), Value::Int(args.len() as i64)],
        ));
    }
    let id = resolve_optional_process_or_current_buffer(eval, args.first())?;
    Ok(Value::Int(id as i64))
}

/// (process-running-child-p &optional PROCESS) -> bool
pub(crate) fn builtin_process_running_child_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 1 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("process-running-child-p"),
                Value::Int(args.len() as i64),
            ],
        ));
    }
    let _id = resolve_optional_process_or_current_buffer(eval, args.first())?;
    Ok(Value::Nil)
}

/// (accept-process-output &optional PROCESS SECONDS MILLISECS JUST-THIS-ONE) -> bool
///
/// Batch/runtime compatibility path: validates arguments, then returns nil.
pub(crate) fn builtin_accept_process_output(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    if args.len() > 4 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![
                Value::symbol("accept-process-output"),
                Value::Int(args.len() as i64),
            ],
        ));
    }

    if let Some(process) = args.first() {
        if !process.is_nil() && resolve_live_process_designator(eval, process).is_none() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("processp"), process.clone()],
            ));
        }
    }

    if let Some(seconds) = args.get(1) {
        if args.get(2).is_some() {
            if !seconds.is_nil() && !matches!(seconds, Value::Int(_)) {
                return Err(signal(
                    "wrong-type-argument",
                    vec![Value::symbol("fixnump"), seconds.clone()],
                ));
            }
        } else if !seconds.is_nil() && !seconds.is_number() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("numberp"), seconds.clone()],
            ));
        }
    }

    Ok(Value::Nil)
}

/// (get-process NAME) -> process-or-nil
pub(crate) fn builtin_get_process(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("get-process", &args, 1)?;
    let name = expect_string_strict(&args[0])?;
    match eval.processes.find_by_name(&name) {
        Some(id) => Ok(Value::Int(id as i64)),
        None => Ok(Value::Nil),
    }
}

/// (processp OBJECT) -> bool
pub(crate) fn builtin_processp(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("processp", &args, 1)?;
    Ok(Value::bool(
        resolve_live_process_designator(eval, &args[0]).is_some(),
    ))
}

/// (process-live-p PROCESS) -> list-or-nil
pub(crate) fn builtin_process_live_p(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-live-p", &args, 1)?;
    let Some(id) = resolve_live_process_designator(eval, &args[0]) else {
        return Ok(Value::Nil);
    };
    match eval.processes.process_status(id) {
        Some(status) => Ok(process_live_status_value(status)),
        None => Ok(Value::Nil),
    }
}

/// (process-id PROCESS) -> integer
pub(crate) fn builtin_process_id(eval: &mut super::eval::Evaluator, args: Vec<Value>) -> EvalResult {
    expect_args("process-id", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::Int(id as i64))
}

/// (process-query-on-exit-flag PROCESS) -> bool
pub(crate) fn builtin_process_query_on_exit_flag(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-query-on-exit-flag", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    Ok(Value::bool(proc.query_on_exit_flag))
}

/// (set-process-query-on-exit-flag PROCESS FLAG) -> FLAG
pub(crate) fn builtin_set_process_query_on_exit_flag(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-process-query-on-exit-flag", &args, 2)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let flag = args[1].is_truthy();
    let proc = eval
        .processes
        .get_mut(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    proc.query_on_exit_flag = flag;
    Ok(args[1].clone())
}

/// (process-command PROCESS) -> list
pub(crate) fn builtin_process_command(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-command", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    let mut items = Vec::with_capacity(proc.args.len() + 1);
    items.push(Value::string(proc.command.clone()));
    items.extend(proc.args.iter().cloned().map(Value::string));
    Ok(Value::list(items))
}

/// (process-contact PROCESS &optional KEY NO-BLOCK) -> value
pub(crate) fn builtin_process_contact(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_min_args("process-contact", &args, 1)?;
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("process-contact"), Value::Int(args.len() as i64)],
        ));
    }
    let _id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    Ok(Value::True)
}

/// (process-filter PROCESS) -> function
pub(crate) fn builtin_process_filter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-filter", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    Ok(proc.filter.clone())
}

/// (set-process-filter PROCESS FILTER) -> FILTER
pub(crate) fn builtin_set_process_filter(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-process-filter", &args, 2)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let stored = if args[1].is_nil() {
        Value::symbol(DEFAULT_PROCESS_FILTER_SYMBOL)
    } else {
        args[1].clone()
    };
    let proc = eval
        .processes
        .get_mut(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    proc.filter = stored.clone();
    Ok(stored)
}

/// (process-sentinel PROCESS) -> function
pub(crate) fn builtin_process_sentinel(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-sentinel", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    Ok(proc.sentinel.clone())
}

/// (set-process-sentinel PROCESS SENTINEL) -> SENTINEL
pub(crate) fn builtin_set_process_sentinel(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-process-sentinel", &args, 2)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let stored = if args[1].is_nil() {
        Value::symbol(DEFAULT_PROCESS_SENTINEL_SYMBOL)
    } else {
        args[1].clone()
    };
    let proc = eval
        .processes
        .get_mut(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    proc.sentinel = stored.clone();
    Ok(stored)
}

/// (process-plist PROCESS) -> plist
pub(crate) fn builtin_process_plist(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-plist", &args, 1)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    Ok(proc.plist.clone())
}

/// (set-process-plist PROCESS PLIST) -> plist
pub(crate) fn builtin_set_process_plist(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("set-process-plist", &args, 2)?;
    if !args[1].is_list() {
        return Err(signal(
            "wrong-type-argument",
            vec![Value::symbol("listp"), args[1].clone()],
        ));
    }
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let proc = eval
        .processes
        .get_mut(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    proc.plist = args[1].clone();
    Ok(proc.plist.clone())
}

/// (process-put PROCESS PROP VALUE) -> plist
pub(crate) fn builtin_process_put(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-put", &args, 3)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let current_plist = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?
        .plist
        .clone();
    let new_plist =
        super::builtins::builtin_plist_put(vec![current_plist, args[1].clone(), args[2].clone()])?;
    let proc = eval
        .processes
        .get_mut(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?;
    proc.plist = new_plist.clone();
    Ok(new_plist)
}

/// (process-get PROCESS PROP) -> value
pub(crate) fn builtin_process_get(
    eval: &mut super::eval::Evaluator,
    args: Vec<Value>,
) -> EvalResult {
    expect_args("process-get", &args, 2)?;
    let id = resolve_live_process_or_wrong_type(eval, &args[0])?;
    let plist = eval
        .processes
        .get(id)
        .ok_or_else(|| signal("wrong-type-argument", vec![Value::symbol("processp"), args[0].clone()]))?
        .plist
        .clone();
    super::builtins::builtin_plist_get(vec![plist, args[1].clone()])
}

// ---------------------------------------------------------------------------
// Builtins (pure — no evaluator needed)
// ---------------------------------------------------------------------------

/// (shell-command-to-string COMMAND) -> string
///
/// Runs COMMAND via the system shell and returns captured stdout.
pub(crate) fn builtin_shell_command_to_string(args: Vec<Value>) -> EvalResult {
    expect_args("shell-command-to-string", &args, 1)?;
    let command = expect_string(&args[0])?;

    let shell = std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string());

    let output = Command::new(&shell)
        .arg("-c")
        .arg(&command)
        .output()
        .map_err(|e| signal_process_io("Shell command failed", Some(&shell), e))?;

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    Ok(Value::string(stdout))
}

/// (getenv VARIABLE) -> string or nil
pub(crate) fn builtin_getenv(args: Vec<Value>) -> EvalResult {
    expect_min_args("getenv", &args, 1)?;
    if args.len() > 2 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("getenv"), Value::Int(args.len() as i64)],
        ));
    }
    if let Some(frame) = args.get(1) {
        if !frame.is_nil() {
            return Err(signal(
                "wrong-type-argument",
                vec![Value::symbol("framep"), frame.clone()],
            ));
        }
    }
    let name = expect_string_strict(&args[0])?;
    match std::env::var(&name) {
        Ok(val) => Ok(Value::string(val)),
        Err(_) => Ok(Value::Nil),
    }
}

/// (setenv VARIABLE &optional VALUE) -> string or nil
///
/// Sets the environment variable VARIABLE to VALUE.  If VALUE is nil
/// or omitted, removes the variable.
pub(crate) fn builtin_setenv(args: Vec<Value>) -> EvalResult {
    expect_min_args("setenv", &args, 1)?;
    if args.len() > 3 {
        return Err(signal(
            "wrong-number-of-arguments",
            vec![Value::symbol("setenv"), Value::Int(args.len() as i64)],
        ));
    }
    let name = expect_string_strict(&args[0])?;

    if args.len() > 1 && !args[1].is_nil() {
        let env_value = if args.len() > 2 && args[2].is_truthy() {
            let substituted =
                super::fileio::builtin_substitute_in_file_name(vec![args[1].clone()])?;
            expect_string_strict(&substituted)?
        } else {
            sequence_value_to_env_string(&args[1])?
        };
        // Safety: this is single-threaded for the Elisp VM, so setting env
        // vars is acceptable.
        unsafe {
            std::env::set_var(&name, &env_value);
        }
        Ok(args[1].clone())
    } else {
        unsafe {
            std::env::remove_var(&name);
        }
        Ok(Value::Nil)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::elisp::{format_eval_result, parse_forms, Evaluator};

    fn eval_one(src: &str) -> String {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        let result = ev.eval_expr(&forms[0]);
        format_eval_result(&result)
    }

    fn eval_all(src: &str) -> Vec<String> {
        let forms = parse_forms(src).expect("parse");
        let mut ev = Evaluator::new();
        ev.eval_forms(&forms)
            .iter()
            .map(format_eval_result)
            .collect()
    }

    /// Find the path of a binary, trying /bin, /usr/bin, and PATH lookup.
    fn find_bin(name: &str) -> String {
        for dir in &["/bin", "/usr/bin", "/run/current-system/sw/bin"] {
            let path = format!("{}/{}", dir, name);
            if std::path::Path::new(&path).exists() {
                return path;
            }
        }
        // Fallback: try to find via `which`
        if let Ok(output) = std::process::Command::new("which").arg(name).output() {
            if output.status.success() {
                return String::from_utf8_lossy(&output.stdout).trim().to_string();
            }
        }
        // Last resort: return the bare name and let Command search PATH
        name.to_string()
    }

    fn tmp_file(label: &str) -> String {
        let nonce = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("time should be monotonic")
            .as_nanos();
        format!("/tmp/neovm-{label}-{}-{nonce}.txt", std::process::id())
    }

    // -- ProcessManager unit tests ------------------------------------------

    #[test]
    fn process_manager_create_and_query() {
        let mut pm = ProcessManager::new();
        let id = pm.create_process(
            "test".into(),
            Some("*test*".into()),
            "/bin/echo".into(),
            vec!["hello".into()],
        );
        assert!(id > 0);
        assert!(pm.get(id).is_some());
        assert_eq!(pm.get(id).unwrap().name, "test");
        assert_eq!(pm.get(id).unwrap().command, "/bin/echo");
        assert_eq!(pm.process_status(id), Some(&ProcessStatus::Run));
    }

    #[test]
    fn process_manager_kill() {
        let mut pm = ProcessManager::new();
        let id = pm.create_process("p".into(), None, "prog".into(), vec![]);
        assert!(pm.kill_process(id));
        assert_eq!(pm.process_status(id), Some(&ProcessStatus::Signal(9)));
    }

    #[test]
    fn process_manager_delete() {
        let mut pm = ProcessManager::new();
        let id = pm.create_process("p".into(), None, "prog".into(), vec![]);
        assert!(pm.delete_process(id));
        assert!(pm.get(id).is_none());
    }

    #[test]
    fn process_manager_send_input() {
        let mut pm = ProcessManager::new();
        let id = pm.create_process("p".into(), None, "prog".into(), vec![]);
        assert!(pm.send_input(id, "hello "));
        assert!(pm.send_input(id, "world"));
        assert_eq!(pm.get(id).unwrap().stdin_queue, "hello world");
    }

    #[test]
    fn process_manager_find_by_name() {
        let mut pm = ProcessManager::new();
        let id = pm.create_process("my-proc".into(), None, "prog".into(), vec![]);
        assert_eq!(pm.find_by_name("my-proc"), Some(id));
        assert_eq!(pm.find_by_name("nonexistent"), None);
    }

    #[test]
    fn process_manager_list() {
        let mut pm = ProcessManager::new();
        let id1 = pm.create_process("a".into(), None, "p".into(), vec![]);
        let id2 = pm.create_process("b".into(), None, "q".into(), vec![]);
        let ids = pm.list_processes();
        assert!(ids.contains(&id1));
        assert!(ids.contains(&id2));
        assert_eq!(ids.len(), 2);
    }

    #[test]
    fn process_manager_env() {
        let mut pm = ProcessManager::new();
        pm.setenv("NEOVM_TEST_VAR".into(), Some("hello".into()));
        assert_eq!(pm.getenv("NEOVM_TEST_VAR"), Some("hello".into()));
        pm.setenv("NEOVM_TEST_VAR".into(), None);
        assert_eq!(pm.getenv("NEOVM_TEST_VAR"), None);
    }

    // -- Elisp-level tests --------------------------------------------------

    #[test]
    fn start_process_and_query() {
        let echo = find_bin("echo");
        let results = eval_all(&format!(
            r#"(start-process "my-proc" nil "{echo}" "hello")
               (process-status 1)
               (process-name 1)
               (process-buffer 1)"#,
        ));
        assert_eq!(results[0], "OK 1");
        assert_eq!(results[1], "OK run");
        assert_eq!(results[2], r#"OK "my-proc""#);
        assert_eq!(results[3], "OK nil");
    }

    #[test]
    fn start_process_with_buffer() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(start-process "p" "*output*" "{cat}")
               (process-buffer 1)"#,
        ));
        assert_eq!(results[1], r#"OK "*output*""#);
    }

    #[test]
    fn delete_process_removes() {
        let echo = find_bin("echo");
        let results = eval_all(&format!(
            r#"(start-process "p" nil "{echo}")
               (delete-process 1)
               (process-list)"#,
        ));
        assert_eq!(results[2], "OK nil");
    }

    #[test]
    fn process_send_string_test() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(start-process "p" nil "{cat}")
               (process-send-string 1 "hello")"#,
        ));
        assert_eq!(results[1], "OK nil");
    }

    #[test]
    fn process_exit_status_initial() {
        let echo = find_bin("echo");
        let results = eval_all(&format!(
            r#"(start-process "p" nil "{echo}")
               (process-exit-status 1)"#,
        ));
        assert_eq!(results[1], "OK 0");
    }

    #[test]
    fn process_list_test() {
        let echo = find_bin("echo");
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(start-process "a" nil "{echo}")
               (start-process "b" nil "{cat}")
               (process-list)"#,
        ));
        // Process list contains two entries.  Order may vary.
        let list_str = &results[2];
        assert!(list_str.contains("1"));
        assert!(list_str.contains("2"));
    }

    #[test]
    fn call_process_echo() {
        let echo = find_bin("echo");
        // call-process with echo, inserting into current buffer
        let results = eval_all(&format!(
            r#"(get-buffer-create "cp-test")
               (set-buffer "cp-test")
               (call-process "{echo}" nil t nil "hello" "world")
               (buffer-string)"#,
        ));
        // Exit code should be 0.
        assert_eq!(results[2], "OK 0");
        // Buffer should contain "hello world\n".
        assert_eq!(results[3], r#"OK "hello world\n""#);
    }

    #[test]
    fn call_process_no_destination() {
        let echo = find_bin("echo");
        // call-process with nil destination discards output
        let results = eval_all(&format!(
            r#"(get-buffer-create "cp-nil")
               (set-buffer "cp-nil")
               (call-process "{echo}" nil nil nil "hello")
               (buffer-string)"#,
        ));
        assert_eq!(results[2], "OK 0");
        assert_eq!(results[3], r#"OK """#);
    }

    #[test]
    fn call_process_infile_feeds_stdin() {
        let cat = find_bin("cat");
        let infile = tmp_file("cp-infile");
        std::fs::write(&infile, "infile-data").expect("write infile");
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (list
                   (call-process "{cat}" "{infile}" t nil)
                   (buffer-string)))"#
        ));
        assert_eq!(results[0], r#"OK (0 "infile-data")"#);
        let _ = std::fs::remove_file(&infile);
    }

    #[test]
    fn call_process_destination_buffer_name_inserts_there() {
        let echo = find_bin("echo");
        let results = eval_all(&format!(
            r#"(get-buffer-create "cp-src")
               (get-buffer-create "cp-dst")
               (set-buffer "cp-src")
               (erase-buffer)
               (set-buffer "cp-dst")
               (erase-buffer)
               (set-buffer "cp-src")
               (call-process "{echo}" nil "cp-dst" nil "hello")
               (list
                 (with-current-buffer "cp-src" (buffer-string))
                 (with-current-buffer "cp-dst" (buffer-string)))"#,
        ));
        assert_eq!(results[7], "OK 0");
        assert_eq!(results[8], r#"OK ("" "hello\n")"#);
    }

    #[test]
    fn call_process_file_destination_collects_stdout_and_stderr() {
        let sh = find_bin("sh");
        let out = tmp_file("cp-file");
        let _ = std::fs::remove_file(&out);
        let results = eval_all(&format!(
            r#"(call-process "{sh}" nil '(:file "{out}") nil "-c" "echo out; echo err >&2")
               (with-temp-buffer (insert-file-contents "{out}") (buffer-string))"#
        ));
        assert_eq!(results[0], "OK 0");
        assert!(results[1].contains("out"));
        assert!(results[1].contains("err"));
        let _ = std::fs::remove_file(&out);
    }

    #[test]
    fn call_process_pair_destination_splits_stderr_to_file() {
        let sh = find_bin("sh");
        let out = tmp_file("cp-pair-out");
        let err = tmp_file("cp-pair-err");
        let _ = std::fs::remove_file(&out);
        let _ = std::fs::remove_file(&err);
        let results = eval_all(&format!(
            r#"(call-process "{sh}" nil '((:file "{out}") "{err}") nil "-c" "echo out; echo err >&2")
               (with-temp-buffer (insert-file-contents "{out}") (buffer-string))
               (with-temp-buffer (insert-file-contents "{err}") (buffer-string))"#
        ));
        assert_eq!(results[0], "OK 0");
        assert!(results[1].contains("out"));
        assert!(!results[1].contains("err"));
        assert!(results[2].contains("err"));
        let _ = std::fs::remove_file(&out);
        let _ = std::fs::remove_file(&err);
    }

    #[test]
    fn call_process_integer_destination_returns_nil() {
        let echo = find_bin("echo");
        // Any integer destination behaves like 0: discard and return nil.
        let results = eval_all(&format!(
            r#"(get-buffer-create "cp-int")
               (set-buffer "cp-int")
               (call-process "{echo}" nil 2 nil "hello")
               (buffer-string)"#,
        ));
        assert_eq!(results[2], "OK nil");
        assert_eq!(results[3], r#"OK """#);
    }

    #[test]
    fn call_process_false() {
        let false_bin = find_bin("false");
        // false exits with code 1
        let result = eval_one(&format!(r#"(call-process "{false_bin}")"#));
        assert_eq!(result, "OK 1");
    }

    #[test]
    fn call_process_region_test() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(get-buffer-create "cpr-test")
               (set-buffer "cpr-test")
               (insert "hello world")
               (call-process-region 1 12 "{cat}" nil t)
               (buffer-string)"#,
        ));
        // exit code 0
        assert_eq!(results[3], "OK 0");
        // Buffer should contain original text plus piped output
        assert!(results[4].contains("hello world"));
    }

    #[test]
    fn call_process_region_destination_buffer_name_inserts_there() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(get-buffer-create "cpr-src")
               (get-buffer-create "cpr-dst")
               (with-current-buffer "cpr-src" (erase-buffer) (insert "abc"))
               (with-current-buffer "cpr-dst" (erase-buffer))
               (with-current-buffer "cpr-src"
                 (call-process-region (point-min) (point-max) "{cat}" nil "cpr-dst" nil))
               (list
                 (with-current-buffer "cpr-src" (buffer-string))
                 (with-current-buffer "cpr-dst" (buffer-string)))"#,
        ));
        assert_eq!(results[4], "OK 0");
        assert_eq!(results[5], r#"OK ("abc" "abc")"#);
    }

    #[test]
    fn call_process_region_file_destination_writes_file() {
        let cat = find_bin("cat");
        let out = tmp_file("cpr-file");
        let _ = std::fs::remove_file(&out);
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (call-process-region (point-min) (point-max) "{cat}" nil '(:file "{out}") nil))
               (with-temp-buffer (insert-file-contents "{out}") (buffer-string))"#
        ));
        assert_eq!(results[0], "OK 0");
        assert_eq!(results[1], r#"OK "abc""#);
        let _ = std::fs::remove_file(&out);
    }

    #[test]
    fn call_process_region_start_nil_uses_whole_buffer() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (list (call-process-region nil nil "{cat}" nil t nil)
                       (buffer-string)))"#
        ));
        assert_eq!(results[0], r#"OK (0 "abcabc")"#);
    }

    #[test]
    fn call_process_region_start_string_uses_string_input() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (list (call-process-region "xyz" nil "{cat}" nil t nil)
                       (buffer-string)))"#
        ));
        assert_eq!(results[0], r#"OK (0 "abcxyz")"#);
    }

    #[test]
    fn call_process_region_start_string_with_delete_signals_wrong_type() {
        let cat = find_bin("cat");
        let result = eval_one(&format!(
            r#"(condition-case err
                   (call-process-region "xyz" nil "{cat}" t t nil)
                 (error (car err)))"#
        ));
        assert_eq!(result, "OK wrong-type-argument");
    }

    #[test]
    fn call_process_region_accepts_marker_positions() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (insert "abcdef")
                 (goto-char 3)
                 (let ((m (copy-marker (point))))
                   (list (call-process-region m (point-max) "{cat}" nil t nil)
                         (buffer-string))))"#
        ));
        assert_eq!(results[0], r#"OK (0 "abcdefcdef")"#);
    }

    #[test]
    fn call_process_region_reversed_bounds_are_accepted() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (list (call-process-region (point-max) (point-min) "{cat}" nil t nil)
                       (buffer-string)))"#
        ));
        assert_eq!(results[0], r#"OK (0 "abcabc")"#);
    }

    #[test]
    fn call_process_region_reversed_bounds_with_delete_delete_region() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (list (call-process-region (point-max) (point-min) "{cat}" t t nil)
                       (buffer-string)))"#
        ));
        assert_eq!(results[0], r#"OK (0 "abc")"#);
    }

    #[test]
    fn call_process_region_negative_start_signals_args_out_of_range() {
        let cat = find_bin("cat");
        let result = eval_one(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (condition-case err
                     (call-process-region -1 2 "{cat}" nil t nil)
                   (error (car err))))"#
        ));
        assert_eq!(result, "OK args-out-of-range");
    }

    #[test]
    fn call_process_region_huge_end_signals_args_out_of_range() {
        let cat = find_bin("cat");
        let result = eval_one(&format!(
            r#"(with-temp-buffer
                 (insert "abc")
                 (condition-case err
                     (call-process-region 1 999999 "{cat}" nil t nil)
                   (error (car err))))"#
        ));
        assert_eq!(result, "OK args-out-of-range");
    }

    #[test]
    fn call_process_region_integer_destination_returns_nil() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(get-buffer-create "cpr-int")
               (set-buffer "cpr-int")
               (erase-buffer)
               (insert "abc")
               (call-process-region 1 4 "{cat}" nil 3 nil)
               (buffer-string)"#,
        ));
        assert_eq!(results[4], "OK nil");
        assert_eq!(results[5], r#"OK "abc""#);
    }

    #[test]
    fn shell_command_to_string_test() {
        let result = eval_one(r#"(shell-command-to-string "echo -n hello")"#);
        assert_eq!(result, r#"OK "hello""#);
    }

    #[test]
    fn shell_command_to_string_with_pipe() {
        let result = eval_one(r#"(shell-command-to-string "echo hello | tr a-z A-Z")"#);
        assert_eq!(result, r#"OK "HELLO\n""#);
    }

    #[test]
    fn getenv_path() {
        // PATH should always be set
        let result = eval_one(r#"(getenv "PATH")"#);
        assert!(result.starts_with("OK \""));
    }

    #[test]
    fn getenv_nonexistent() {
        let result = eval_one(r#"(getenv "NEOVM_DEFINITELY_NOT_SET_12345")"#);
        assert_eq!(result, "OK nil");
    }

    #[test]
    fn getenv_name_must_be_string() {
        let result = eval_one(r#"(condition-case err (getenv nil) (error err))"#);
        assert_eq!(result, "OK (wrong-type-argument stringp nil)");
    }

    #[test]
    fn getenv_accepts_optional_nil_frame_arg() {
        let result = eval_one(
            r#"(condition-case err
                   (let ((v (getenv "HOME" nil)))
                     (if (stringp v) 'string v))
                 (error err))"#,
        );
        assert_eq!(result, "OK string");
    }

    #[test]
    fn getenv_rejects_non_nil_frame_arg_before_variable_type_check() {
        let result = eval_one(r#"(condition-case err (getenv 1 '(x)) (error err))"#);
        assert_eq!(result, "OK (wrong-type-argument framep (x))");
    }

    #[test]
    fn getenv_rejects_more_than_two_args() {
        let result = eval_one(r#"(condition-case err (getenv "HOME" nil nil) (error (car err)))"#);
        assert_eq!(result, "OK wrong-number-of-arguments");
    }

    #[test]
    fn setenv_and_getenv() {
        let results = eval_all(
            r#"(setenv "NEOVM_TEST_SETENV" "myvalue")
               (getenv "NEOVM_TEST_SETENV")"#,
        );
        assert_eq!(results[0], r#"OK "myvalue""#);
        assert_eq!(results[1], r#"OK "myvalue""#);
    }

    #[test]
    fn setenv_unset() {
        let results = eval_all(
            r#"(setenv "NEOVM_TEST_UNSET" "val")
               (setenv "NEOVM_TEST_UNSET")
               (getenv "NEOVM_TEST_UNSET")"#,
        );
        assert_eq!(results[2], "OK nil");
    }

    #[test]
    fn setenv_name_must_be_string() {
        let result = eval_one(r#"(condition-case err (setenv nil "v") (error err))"#);
        assert_eq!(result, "OK (wrong-type-argument stringp nil)");
    }

    #[test]
    fn setenv_accepts_sequence_value_and_sets_environment() {
        let vector_result = eval_one(
            r#"(let ((old (getenv "NEOVM_TEST_SETENV_SEQ")))
                 (unwind-protect
                     (progn
                       (setenv "NEOVM_TEST_SETENV_SEQ" [118 97 108])
                       (getenv "NEOVM_TEST_SETENV_SEQ"))
                   (setenv "NEOVM_TEST_SETENV_SEQ" old)))"#,
        );
        assert_eq!(vector_result, r#"OK "val""#);

        let list_result = eval_one(
            r#"(let ((old (getenv "NEOVM_TEST_SETENV_SEQ")))
                 (unwind-protect
                     (progn
                       (setenv "NEOVM_TEST_SETENV_SEQ" '(118 97 108))
                       (getenv "NEOVM_TEST_SETENV_SEQ"))
                   (setenv "NEOVM_TEST_SETENV_SEQ" old)))"#,
        );
        assert_eq!(list_result, r#"OK "val""#);
    }

    #[test]
    fn setenv_substitute_flag_controls_expansion_and_requires_string() {
        let unsubstituted = eval_one(
            r#"(let ((old (getenv "NEOVM_TEST_SETENV_SEQ")))
                 (unwind-protect
                     (progn
                       (setenv "NEOVM_TEST_SETENV_SEQ" "$HOME")
                       (getenv "NEOVM_TEST_SETENV_SEQ"))
                   (setenv "NEOVM_TEST_SETENV_SEQ" old)))"#,
        );
        assert_eq!(unsubstituted, r#"OK "$HOME""#);

        let substituted = eval_one(
            r#"(let ((old (getenv "NEOVM_TEST_SETENV_SEQ")))
                 (unwind-protect
                     (progn
                       (setenv "NEOVM_TEST_SETENV_SEQ" "$HOME" t)
                       (getenv "NEOVM_TEST_SETENV_SEQ"))
                   (setenv "NEOVM_TEST_SETENV_SEQ" old)))"#,
        );
        assert!(substituted.starts_with("OK \""));
        assert_ne!(substituted, r#"OK "$HOME""#);

        let type_err = eval_one(
            r#"(condition-case err (setenv "NEOVM_TEST_SETENV_SEQ" [118 97 108] t) (error err))"#,
        );
        assert_eq!(type_err, "OK (wrong-type-argument stringp [118 97 108])");
    }

    #[test]
    fn setenv_rejects_non_sequence_value() {
        let result =
            eval_one(r#"(condition-case err (setenv "NEOVM_TEST_SETENV_SEQ" 1) (error err))"#);
        assert_eq!(result, "OK (wrong-type-argument sequencep 1)");
    }

    #[test]
    fn setenv_rejects_too_many_args() {
        let result = eval_one(
            r#"(condition-case err (setenv "NEOVM_TEST_SETENV_SEQ" "v" nil nil) (error (car err)))"#,
        );
        assert_eq!(result, "OK wrong-number-of-arguments");
    }

    #[test]
    fn call_process_bad_program() {
        let result = eval_one(r#"(call-process "/nonexistent/program_xyz")"#);
        assert!(result.contains("ERR"));
    }

    #[test]
    fn call_process_bad_program_signals_file_missing() {
        let result = eval_one(
            r#"(condition-case err (call-process "/nonexistent/program_xyz") (error (car err)))"#,
        );
        assert_eq!(result, "OK file-missing");
    }

    #[test]
    fn call_process_missing_infile_signals_file_missing() {
        let cat = find_bin("cat");
        let result = eval_one(&format!(
            r#"(condition-case err (call-process "{cat}" "/nonexistent/neovm-process-infile") (error (car err)))"#
        ));
        assert_eq!(result, "OK file-missing");
    }

    #[test]
    fn call_process_region_bad_program_signals_file_missing() {
        let result = eval_one(
            r#"(condition-case err (call-process-region 1 1 "/nonexistent/program_xyz") (error (car err)))"#,
        );
        assert_eq!(result, "OK file-missing");
    }

    #[test]
    fn call_process_symbol_destination_signals_wrong_type_argument() {
        let echo = find_bin("echo");
        let result = eval_one(&format!(
            r#"(condition-case err (call-process "{echo}" nil 'foo nil "x") (error err))"#
        ));
        assert_eq!(result, "OK (wrong-type-argument stringp foo)");
    }

    #[test]
    fn call_process_bad_stderr_target_signals_wrong_type_argument() {
        let echo = find_bin("echo");
        let result = eval_one(&format!(
            r#"(condition-case err (call-process "{echo}" nil '(t 99) nil "x") (error err))"#
        ));
        assert_eq!(result, "OK (wrong-type-argument stringp 99)");
    }

    #[test]
    fn process_status_wrong_arg_type() {
        let result = eval_one(r#"(process-status 999)"#);
        assert!(result.contains("ERR"));
    }

    #[test]
    fn start_process_multiple_args() {
        let echo = find_bin("echo");
        let results = eval_all(&format!(
            r#"(start-process "echo" nil "{echo}" "a" "b" "c")
               (process-name 1)"#,
        ));
        assert_eq!(results[0], "OK 1");
        assert_eq!(results[1], r#"OK "echo""#);
    }

    #[test]
    fn process_runtime_introspection_controls() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(let ((p (start-process "proc-introspect" nil "{cat}")))
                 (list
                  (processp p)
                  (equal (process-live-p p) '(run open listen connect stop))
                  (integerp (process-id p))
                  (process-contact p t)
                  (process-filter p)
                  (set-process-filter p nil)
                  (set-process-filter p 'ignore)
                  (process-filter p)
                  (process-sentinel p)
                  (set-process-sentinel p nil)
                  (set-process-sentinel p 'ignore)
                  (process-sentinel p)
                  (set-process-plist p '(a 1))
                  (process-get p 'a)
                  (process-put p 'k 2)
                  (process-get p 'k)
                  (process-query-on-exit-flag p)
                  (set-process-query-on-exit-flag p nil)
                  (process-query-on-exit-flag p)
                  (delete-process p)
                  (process-live-p p)))"#,
        ));
        assert_eq!(
            results[0],
            "OK (t t t t internal-default-process-filter internal-default-process-filter ignore ignore internal-default-process-sentinel internal-default-process-sentinel ignore ignore (a 1 k 2) 1 (a 1 k 2) 2 t nil nil nil nil)"
        );
    }

    #[test]
    fn accept_process_output_and_get_process_runtime_surface() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(condition-case err (accept-process-output) (error err))
               (condition-case err (accept-process-output nil 0.01) (error err))
               (condition-case err (accept-process-output 1) (error err))
               (condition-case err (accept-process-output nil "x") (error err))
               (let ((p (start-process "proc-get-probe" nil "{cat}")))
                 (list
                  (processp (get-process "proc-get-probe"))
                  (eq p (get-process "proc-get-probe"))
                  (accept-process-output p 0.0)
                  (delete-process p)
                  (get-process "proc-get-probe")))
               (condition-case err (get-process 'proc-get-probe) (error err))"#,
        ));
        assert_eq!(results[0], "OK nil");
        assert_eq!(results[1], "OK nil");
        assert_eq!(results[2], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[3], r#"OK (wrong-type-argument numberp "x")"#);
        assert_eq!(results[4], "OK (t t nil nil nil)");
        assert_eq!(
            results[5],
            "OK (wrong-type-argument stringp proc-get-probe)"
        );
    }

    #[test]
    fn process_mark_type_thread_send_and_running_child_runtime_surface() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(let ((p (start-process "proc-mark-type-thread-send" nil "{cat}")))
                 (unwind-protect
                     (list
                      (processp p)
                      (eq (process-type p) 'real)
                      (not (processp (process-thread p)))
                      (markerp (process-mark p))
                      (marker-buffer (process-mark p))
                      (marker-position (process-mark p))
                      (process-running-child-p p)
                      (processp (process-send-eof p))
                      (with-temp-buffer
                        (insert "abc")
                        (process-send-region p (point-min) (point-max)))
                      (delete-process p)
                      (process-live-p p))
                   (ignore-errors (delete-process p))))
               (condition-case err (process-send-eof) (error (car err)))
               (condition-case err (process-running-child-p) (error (car err)))
               (condition-case err (process-mark 1) (error err))
               (condition-case err (process-type 1) (error err))
               (condition-case err (process-thread 1) (error err))
               (condition-case err (process-send-region 1 1 1) (error err))
               (condition-case err (process-send-eof 1) (error err))
               (condition-case err (process-running-child-p 1) (error err))
               (condition-case err (process-send-eof nil nil) (error (car err)))
               (condition-case err (process-running-child-p nil nil) (error (car err)))"#,
        ));
        assert_eq!(results[0], "OK (t t t t nil nil nil t nil nil nil)");
        assert_eq!(results[1], "OK error");
        assert_eq!(results[2], "OK error");
        assert_eq!(results[3], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[4], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[5], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[6], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[7], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[8], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[9], "OK wrong-number-of-arguments");
        assert_eq!(results[10], "OK wrong-number-of-arguments");
    }

    #[test]
    fn process_coding_tty_and_kill_buffer_query_runtime_surface() {
        let cat = find_bin("cat");
        let results = eval_all(&format!(
            r#"(let ((p (start-process "proc-coding-tty-query" nil "{cat}")))
                 (unwind-protect
                     (list
                      (equal (process-coding-system p) '(utf-8-unix . utf-8-unix))
                      (process-datagram-address p)
                      (process-inherit-coding-system-flag p)
                      (process-kill-buffer-query-function)
                      (stringp (process-tty-name p))
                      (stringp (process-tty-name p 'stdin))
                      (stringp (process-tty-name p 'stdout))
                      (stringp (process-tty-name p 'stderr))
                      (condition-case err (process-tty-name p 0) (error err))
                      (delete-process p)
                      (process-live-p p))
                   (ignore-errors (delete-process p))))
               (condition-case err (process-coding-system 1) (error err))
               (condition-case err (process-datagram-address 1) (error err))
               (condition-case err (process-inherit-coding-system-flag 1) (error err))
               (condition-case err (process-tty-name 1) (error err))
               (condition-case err (process-tty-name nil) (error err))
               (condition-case err (process-tty-name 1 t) (error err))
               (condition-case err (process-kill-buffer-query-function nil) (error (car err)))
               (condition-case err (process-coding-system) (error (car err)))
               (condition-case err (process-datagram-address) (error (car err)))
               (condition-case err (process-inherit-coding-system-flag) (error (car err)))
               (condition-case err (process-tty-name) (error (car err)))"#,
        ));
        assert_eq!(
            results[0],
            "OK (t nil nil t t t t t (error \"Unknown stream\" 0) nil nil)"
        );
        assert_eq!(results[1], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[2], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[3], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[4], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[5], "OK (wrong-type-argument processp nil)");
        assert_eq!(results[6], "OK (wrong-type-argument processp 1)");
        assert_eq!(results[7], "OK wrong-number-of-arguments");
        assert_eq!(results[8], "OK wrong-number-of-arguments");
        assert_eq!(results[9], "OK wrong-number-of-arguments");
        assert_eq!(results[10], "OK wrong-number-of-arguments");
        assert_eq!(results[11], "OK wrong-number-of-arguments");
    }
}
