//! Kraken Intermediate Representation (IR)
//!
//! This module defines the compiler's intermediate representation, which sits
//! between the AST and LLVM codegen. The IR enables:
//! - Clean separation between frontend and backend
//! - Future transformation passes (async state machines, optimizations)
//! - Backend-agnostic code representation

pub mod desugar;
pub mod lower;
pub mod state_machine;
pub mod types;

#[allow(unused_imports)]
pub use desugar::Desugar;
pub use lower::IrLowering;
#[allow(unused_imports)]
pub use state_machine::{AsyncAnalyzer, StateMachineLowering};
