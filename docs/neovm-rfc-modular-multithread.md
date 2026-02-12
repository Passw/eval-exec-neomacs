# RFC: Modular, Multi-Threaded NeoMacs VM Architecture

Status: Draft
Audience: NeoMacs VM, editor host, runtime, and performance engineering contributors
Scope: Defines the VM/host split, concurrency model, ABI contract, and phased acceptance criteria

## Decision Record

This RFC records an explicit architecture decision:

- NeoMacs default threading model is isolate-first.
- NeoMacs default compatibility mode does not implement Common Lisp-style shared-heap multi-threaded Elisp.
- Any future shared-heap execution model is experimental and must be opt-in.

## 1. Problem Statement

NeoMacs needs to deliver all of the following at once:

- GNU Emacs package/config compatibility
- Multi-core performance gains
- Low interactive latency
- A clear modular architecture where the editor and Elisp VM evolve independently

Historically, Emacs tightly couples evaluator/runtime/editor state. That improves direct access but makes concurrency and architectural evolution hard. This RFC defines a boundary that preserves compatibility while enabling modern performance work.

## 2. Goals

- Preserve observable Elisp behavior for existing packages by default.
- Split VM and editor host into independently testable crates.
- Use isolate-first concurrency for safe parallel speedups.
- Enable tiered execution (interpreter, quickened bytecode, JIT) behind compatibility guards.
- Make rollout measurable through explicit phase gates.

## 3. Non-Goals (Initial Phases)

- Full shared-mutable Elisp heap across arbitrary threads.
- Parallel execution of arbitrary user Elisp in a single shared environment.
- Compatibility-breaking language changes.
- Common Lisp-style shared-heap multi-threaded Elisp as the default runtime mode.

## 4. High-Level Architecture

Runtime layout:

- `neovm-core`: value model, reader/compiler, interpreter/bytecode/JIT, GC, `specpdl`, error system.
- `neovm-host-abi`: typed boundary between VM and editor host (primitives, handles, snapshots, events).
- `neomacs-host`: editor implementation (buffers/windows/redisplay/processes/filesystem/input/timers).
- `neovm-worker`: isolate runtime for parallel tasks using message passing.

Execution layout:

- Main isolate: compatibility authority, owns editor-visible mutation.
- Worker isolates: parallel compute tasks with explicit data transfer/snapshots.
- Background runtime threads: JIT compilation, GC sweep tasks, telemetry aggregation.

## 4.1 Current Code Status (February 12, 2026)

The initial scaffolding is now implemented in Rust:

- `rust/neovm-host-abi`: host boundary and metadata model.
- `rust/neovm-core`: VM shell with scheduler trait boundary (`TaskScheduler`) and default noop scheduler.
- `rust/neovm-core/src/elisp.rs`: initial interpreter slice and parser for a starter compatibility corpus.
- `rust/neovm-core/examples/compat_runner.rs`: emits oracle-style TSV for differential checks.
- `rust/neovm-worker`: multi-thread worker runtime scaffold with:
  - bounded priority queues
  - cancellation state and task status tracking
  - pluggable task executor and awaitable result/error propagation
  - Elisp executor wiring (`with_elisp_executor`) for source payload evaluation
  - bounded channels and timeout-aware `select`
  - condvar-based wait/wakeup for task completion and channel readiness
  - finished-task reaping hook for long-running process memory hygiene
  - runtime counters for enqueue/dequeue/rejection/cancel/complete

This remains a partial evaluator implementation. Bytecode execution, JIT, GC, and full compatibility behavior remain future phases.

## 5. Dependency Rules

Dependency direction is one-way:

- `neovm-core` -> `neovm-host-abi`
- `neomacs-host` -> `neovm-host-abi`
- `neovm-worker` -> `neovm-core` + `neovm-host-abi`

Forbidden:

- `neovm-core` importing host/editor internals
- Raw pointers crossing VM/host boundary
- Host mutating VM internals directly

## 6. VM/Host ABI Contract

The VM only sees opaque handles and typed requests.

```rust
pub type VmHandleId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Affinity {
    MainOnly,
    WorkerSafe,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EffectClass {
    PureRead,
    StateRead,
    StateWrite,
    BlockingIo,
}

#[derive(Clone, Debug)]
pub struct PrimitiveDescriptor {
    pub name: &'static str,
    pub affinity: Affinity,
    pub effect: EffectClass,
    pub can_trigger_gc: bool,
    pub can_reenter_elisp: bool,
    pub deterministic: bool,
}

pub trait HostAbi {
    fn primitive_descriptor(&self, prim: PrimitiveId) -> PrimitiveDescriptor;
    fn call_primitive(
        &mut self,
        isolate: IsolateId,
        prim: PrimitiveId,
        args: &[LispValue],
    ) -> Result<LispValue, Signal>;
    fn clone_snapshot(&self, req: SnapshotRequest) -> Result<SnapshotBlob, HostError>;
    fn submit_patch(&mut self, req: PatchRequest) -> Result<PatchResult, HostError>;
}
```

Key rule:

- `StateWrite` with `MainOnly` cannot execute on worker isolates.

## 7. Primitive Metadata Schema

Every primitive must declare metadata used by scheduler/JIT/safety checks.

Required fields:

- `affinity`: controls where primitive may run.
- `effect`: side-effect category.
- `can_trigger_gc`: whether call can enter VM safepoint/GC-sensitive path.
- `can_reenter_elisp`: whether primitive may invoke evaluator callbacks.
- `deterministic`: enables memoization/speculation candidates.

Registration policy:

- New primitives fail CI unless metadata is complete.
- Runtime asserts affinity/effect violations in debug mode.

## 8. Concurrency Model

### 8.1 Isolate-First Semantics

- Main isolate runs compatibility-critical command execution.
- Worker isolates have separate heaps, stacks, handlers, and `specpdl`.
- Workers communicate by message passing.

### 8.2 Transfer Modes

- `Copy`: deep copy mutable values crossing isolates.
- `Move`: ownership transfer for immutable blobs/snapshots.
- `Borrow`: forbidden across isolate boundary.

### 8.3 Shared State Policy

- Shared mutable editor state is host-owned and main-thread committed.
- Workers may compute deltas/patch proposals.
- Main isolate applies proposals with conflict checks/version stamps.

### 8.4 Go-Inspired Scheduler Model

NeoMacs adopts Go-inspired runtime scheduling ideas, adapted for Elisp compatibility constraints.

Core direction:

- many lightweight VM tasks multiplexed over a bounded OS worker pool
- message passing and channels as the default coordination model
- structured cancellation and deadlines as first-class runtime controls

Not adopted:

- thread-per-task spawning model
- unrestricted shared mutable state between tasks

### 8.4.1 Runtime Entities

- `Task`: unit of work that runs Elisp or host callbacks.
- `Isolate`: owns VM state (heap, stack, `specpdl`, handlers).
- `Worker`: OS thread executing runnable tasks.
- `Scheduler`: maps runnable tasks to workers, enforces affinity/effect rules.

### 8.4.2 Scheduling Topology

- per-worker local run queue for cache locality
- global overflow queue for load balancing
- work-stealing between workers when local queue drains
- dedicated main queue for `MainOnly` tasks tied to compatibility-critical execution

### 8.4.3 Preemption and Safepoints

Preemption is cooperative, not arbitrary signal interruption.

Safepoints required at:

- bytecode loop backedges
- primitive call boundaries
- allocation pressure checkpoints
- explicit runtime `yield` points

Rules:

- long-running tasks must periodically hit safepoints
- scheduler can park/resume tasks only at safe VM boundaries

### 8.4.4 Cancellation and Deadlines

Each task carries a cancellation context:

- cancellation token
- optional deadline
- optional timeout

Behavior:

- cancellation propagates to child tasks unless explicitly detached
- blocking waits must return cancellation-aware status
- interactive quit (`C-g`) maps to cancellation for cancellable background tasks

### 8.4.5 Channels and Select

Channel model requirements:

- bounded capacity by default (backpressure first)
- blocking and non-blocking send/recv variants
- timeout and cancellation-aware operations
- close semantics with deterministic receiver behavior

`select` requirements:

- wait on multiple channel operations and timeout branch
- deterministic fairness policy (documented and tested)
- no busy-poll spin loops in default runtime path

### 8.4.6 Go-Inspired API Sketch

Runtime API sketch:

```rust
pub struct TaskHandle(TaskId);

pub struct TaskOptions {
    pub name: Option<String>,
    pub priority: TaskPriority,
    pub deadline: Option<Instant>,
    pub affinity: Affinity,
}

pub fn spawn_task(form: LispValue, opts: TaskOptions) -> Result<TaskHandle, Signal>;
pub fn task_await(handle: TaskHandle, timeout: Option<Duration>) -> Result<LispValue, TaskError>;
pub fn task_cancel(handle: TaskHandle) -> bool;
pub fn select(ops: &[SelectOp], timeout: Option<Duration>) -> SelectResult;
```

Elisp-facing sketch:

```elisp
(spawn-task FORM &key name priority deadline-ms)
(task-await TASK &optional timeout-ms)
(task-cancel TASK)
(make-channel &optional capacity)
(channel-send CH VALUE &optional timeout-ms)
(channel-recv CH &optional timeout-ms)
(select (recv ch1) (recv ch2) (timeout 50))
```

## 9. VM Execution Tiers

Tier pipeline:

- Tier 0: interpreter (always correct fallback)
- Tier 1: quickened/register bytecode + inline caches
- Tier 2: baseline JIT (low-risk lowering)
- Tier 3: optimizing JIT (guarded, fully deoptimizable)

Deopt requirement:

- Any optimized frame must reconstruct interpreter state exactly:
  - virtual registers
  - program counter
  - `specpdl` depth
  - condition handler stack
  - backtrace/debug info

## 10. Invalidations and Redefinition

No global epoch flush.

Use targeted versioned cells:

- function cell version
- value cell version
- context-local version (buffer/frame locals)
- optional shape/type guard versions

Any guard miss:

- falls back to interpreter/baseline path
- updates feedback slot
- may reoptimize later

## 11. GC Architecture

Per-isolate memory management:

- young generation: bump allocation + copying minor GC
- old generation: incremental marking + concurrent sweep where safe
- card marking for old->young references

Requirements:

- precise roots from interpreter and JIT stack maps
- safepoints at primitive calls and loop back-edges
- isolate-local pause budgets with p99 telemetry

## 12. Compatibility Contract

Default mode must match GNU Emacs observable semantics:

- dynamic and lexical binding behavior
- error/throw/unwind behavior (`condition-case`, `unwind-protect`)
- symbol/value/function cell behavior with runtime redefinition
- buffer-local and context-local lookup behavior
- module-facing behavior expected by package ecosystem

Optional experimental modes must be explicitly opt-in and off by default.

Verification path:

- GNU Emacs batch oracle corpus under `test/neovm/vm-compat/`
- NeoVM must run the same forms and match `OK/ERR` class and value printout semantics
- CI should fail on compatibility drift for enabled suites

## 13. Testing and Validation

### 13.1 Differential Harness

For each test form, execute in:

- `emacs -Q --batch`
- NeoMacs Tier 0
- NeoMacs Tier 1/2/3 (when enabled)

Compare:

- return value
- signal symbol + signal data
- selected state snapshots (buffer text, point/mark, variable values)

### 13.2 Suite Classes

- upstream Emacs test imports
- property/fuzz generation of valid Elisp forms
- package matrix (popular community packages)
- long-session stress (GC/timers/process output)

### 13.3 Regression Handling

- auto-minimize failing forms
- store dual traces (GNU Emacs result + NeoMacs path)
- tag by subsystem (binding, gc, bytecode, jit, host-abi)

## 14. Observability

Mandatory runtime metrics:

- IC hit/miss by opcode and site
- JIT compile/deopt counts and reasons
- GC pause p50/p95/p99 per isolate
- allocation rate by object type
- worker queue depth and task latency
- main-thread frame/command latency

Mandatory controls:

- interpreter-only mode
- disable JIT per function
- dump bytecode/quickening/IR for a function

## 15. Security and Robustness

- No host pointer exposure through Elisp values.
- ABI handle validation on every cross-boundary call in debug/profile builds.
- Panic boundaries at worker task entry points.
- Hard timeouts/cancellation for background tasks.

## 16. Rollout Plan With Acceptance Criteria

### Phase 1: ABI and Compatibility Core

Deliverables:

- `neovm-core` + `neovm-host-abi` skeleton
- interpreter + reader + `specpdl` + signal/unwind basics
- host primitives routed only through ABI

Acceptance criteria:

- 0 direct editor-struct access from VM in static checks
- differential smoke suite pass rate >= 99.5%
- no main-thread latency regression > 10% on baseline commands

### Phase 2: Register Bytecode + IC

Deliverables:

- register bytecode format
- quickening
- mono/poly/mega ICs

Acceptance criteria:

- differential parity with Tier 0 on full CI suite
- IC hit rate >= 80% on benchmark corpus hot call sites
- command latency p95 improves >= 20% vs Phase 1 baseline

### Phase 3: Isolate Workers

Deliverables:

- worker isolate runtime
- snapshot + patch protocol
- scheduler with affinity enforcement
- bounded worker pool (no thread-per-task default path)
- work-stealing local/global run queue implementation
- cancellation/deadline propagation and cancellation-aware wait paths
- channel/select primitives with backpressure and timeout support

Acceptance criteria:

- no compatibility diffs in default mode
- at least 2 production workloads running on workers (for example indexing, parser tasks)
- main-thread p99 latency not worse than Phase 2
- no busy-poll scheduler loops in profile traces for default task runtime
- scheduler telemetry available: queue depth, wait time, run time, cancellation rate

### Phase 4: Generational Incremental GC

Deliverables:

- young/old generation
- barriers + root maps
- pause telemetry

Acceptance criteria:

- major pause p99 <= 5 ms on interactive benchmark profile
- memory safety suite and fuzz suite pass
- no GC-induced semantic mismatch in differential suite

### Phase 5: Baseline JIT

Deliverables:

- Tier 2 baseline JIT
- deopt metadata + recovery path
- background compilation workers

Acceptance criteria:

- deopt correctness tests 100% pass
- hot function throughput >= 2x vs Tier 1 median
- whole-workload latency improvement >= 15% on compute-heavy corpus

### Phase 6: Optimizing JIT + Advanced Parallelism

Deliverables:

- guarded optimizations and selective inlining
- profile-guided heuristics and backoff
- optional controlled shared-state experiments behind feature flags

Acceptance criteria:

- default compatibility mode unchanged
- no unresolved high-severity miscompilation issues in release gate
- measurable wins on targeted workloads without p99 latency regressions

## 17. Open Questions

- Should bytecode compatibility prioritize GNU Emacs opcodes initially or move directly to a NeoMacs register format with translation?
- Which host primitives are first-class worker-safe in Phase 3?
- What snapshot granularity is best for buffer-heavy workloads (full text, chunks, or rope/gap slices)?
- How should external modules be isolated: in-process ABI only, or optional process boundary?

## 18. Immediate Next Steps

- Create crate scaffolding for `neovm-core`, `neovm-host-abi`, `neovm-worker`.
- Implement primitive descriptor registry and affinity/effect checks.
- Build minimal differential harness runner for batch forms.
- Select first two worker tasks and define snapshot/patch payload schemas.
- Prototype scheduler run queues and task context cancellation in `neovm-worker`.
