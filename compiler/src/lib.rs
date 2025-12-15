//! Kraken Compiler
//!
//! A high-performance systems programming language compiler that combines
//! C's raw power with Rust's safety and Go's simplicity.

pub mod analyzer;
pub mod codegen;
pub mod error;
pub mod ffi;
pub mod lexer;
pub mod parser;

pub use error::{CompilerError, CompilerResult};
