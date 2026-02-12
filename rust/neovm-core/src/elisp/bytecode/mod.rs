//! Bytecode compiler and virtual machine.
//!
//! Provides:
//! - `opcode::Op` — bytecode instruction set
//! - `chunk::ByteCodeFunction` — compiled function representation
//! - `compiler::Compiler` — AST to bytecode compiler
//! - `vm::Vm` — stack-based bytecode interpreter

pub mod opcode;
pub mod chunk;
pub mod compiler;
pub mod vm;

// Re-export main types
pub use chunk::ByteCodeFunction;
pub use compiler::Compiler;
pub use opcode::Op;
pub use vm::Vm;
