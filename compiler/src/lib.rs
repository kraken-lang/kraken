//! Kraken Compiler
//!
//! A high-performance systems programming language compiler that combines
//! C's raw power with Rust's safety and Go's simplicity.

pub mod analyzer;
pub mod codegen;
pub mod diagnostic_registry;
pub mod diagnostics;
pub mod error;
pub mod error_helpers;
pub mod ffi;
pub mod ir;
pub mod lexer;
pub mod modules;
pub mod optimizer;
pub mod parser;

pub use error::{CompilerError, CompilerResult};
