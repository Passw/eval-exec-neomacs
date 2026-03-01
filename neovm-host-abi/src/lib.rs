use std::error::Error;
use std::fmt::{Display, Formatter};
use std::time::Duration;

pub type VmHandleId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IsolateId(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PrimitiveId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ChannelId(pub u64);

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimitiveDescriptor {
    pub name: &'static str,
    pub affinity: Affinity,
    pub effect: EffectClass,
    pub can_trigger_gc: bool,
    pub can_reenter_elisp: bool,
    pub deterministic: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct LispValue {
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signal {
    pub symbol: String,
    pub data: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostError {
    pub message: String,
}

impl Display for HostError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl Error for HostError {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SnapshotRequest {
    pub isolate: IsolateId,
    pub handle: VmHandleId,
    pub revision_hint: Option<u64>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SnapshotBlob {
    pub revision: u64,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatchRequest {
    pub isolate: IsolateId,
    pub target: VmHandleId,
    pub expected_revision: u64,
    pub patch: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatchResult {
    Applied { new_revision: u64 },
    Rejected { current_revision: u64 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskPriority {
    Interactive,
    Default,
    Background,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TaskOptions {
    pub name: Option<String>,
    pub priority: TaskPriority,
    pub affinity: Affinity,
    pub timeout: Option<Duration>,
}

impl Default for TaskOptions {
    fn default() -> Self {
        Self {
            name: None,
            priority: TaskPriority::Default,
            affinity: Affinity::WorkerSafe,
            timeout: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TaskError {
    Cancelled,
    TimedOut,
    Failed(Signal),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectOp {
    Recv(ChannelId),
    Send(ChannelId, LispValue),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectResult {
    Ready {
        op_index: usize,
        value: Option<LispValue>,
    },
    TimedOut,
    Cancelled,
}

pub trait HostAbi {
    fn primitive_descriptor(&self, primitive: PrimitiveId) -> PrimitiveDescriptor;

    fn call_primitive(
        &mut self,
        isolate: IsolateId,
        primitive: PrimitiveId,
        args: &[LispValue],
    ) -> Result<LispValue, Signal>;

    fn clone_snapshot(&self, request: SnapshotRequest) -> Result<SnapshotBlob, HostError>;

    fn submit_patch(&mut self, request: PatchRequest) -> Result<PatchResult, HostError>;
}
