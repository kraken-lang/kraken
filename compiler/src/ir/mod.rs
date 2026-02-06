//! Kraken Intermediate Representation (IR)
//!
//! This module defines the compiler's intermediate representation, which sits
//! between the AST and LLVM codegen. The IR enables:
//! - Clean separation between frontend and backend
//! - Future transformation passes (async state machines, optimizations)
//! - Backend-agnostic code representation

/// IR desugaring: `for`→`while` transformation and `defer` insertion at exit points.
pub mod desugar;
/// AST-to-IR lowering: converts parsed AST into the compiler's intermediate representation.
pub mod lower;
#[cfg(test)]
mod lower_tests;
/// Async state machine lowering: transforms async functions into state machine IR.
pub mod state_machine;
/// IR type definitions: instructions, values, types, blocks, functions, and programs.
pub mod types;

#[allow(unused_imports)]
pub use desugar::Desugar;
pub use lower::IrLowering;
#[allow(unused_imports)]
pub use state_machine::{AsyncAnalyzer, StateMachineLowering};
