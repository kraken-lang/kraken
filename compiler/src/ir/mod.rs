//! Kraken Intermediate Representation (IR)
//!
//! This module defines the compiler's intermediate representation, which sits
//! between the AST and LLVM codegen. The IR enables:
//! - Clean separation between frontend and backend
//! - Future transformation passes (async state machines, optimizations)
//! - Backend-agnostic code representation

pub mod lower;
pub mod types;

pub use lower::IrLowering;
