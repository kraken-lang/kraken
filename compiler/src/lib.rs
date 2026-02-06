//! Kraken Compiler
//!
//! A high-performance systems programming language compiler that combines
//! C's raw power with Rust's safety and Go's simplicity.

/// Semantic analysis: type checking, closure analysis, monomorphization, macros, and traits.
pub mod analyzer;
/// Compiler benchmark harness: pipeline instrumentation, regression detection, baseline persistence.
pub mod bench_harness;
#[cfg(test)]
mod bench_harness_tests;
/// Command-line interface: build, run, test, bench, check, fmt, doc, clean, init, new.
pub mod cli;
/// LLVM code generation: IR emission, optimization passes, and object file output.
pub mod codegen;
/// Debugger integration: DWARF debug info generation for GDB/LLDB.
pub mod debugger;
/// Diagnostic code registry: KRA-prefixed error/warning code lookup and documentation.
pub mod diagnostic_registry;
/// Diagnostic system: structured error/warning codes with severity, messages, and suggestions.
pub mod diagnostics;
/// Documentation metadata generator: DocGraph JSON conforming to docgraph-v1 schema.
pub mod docgen;
/// Compiler error types, source locations, spans, and diagnostic hints.
pub mod error;
/// Error helper utilities: fuzzy name matching, "did you mean" suggestions.
pub mod error_helpers;
/// FFI support: C ABI types, stdlib function signatures, and boundary validation.
pub mod ffi;
/// Source code formatter: whitespace normalization and indentation-aware formatting.
pub mod formatter;
/// Intermediate representation: IR types, AST-to-IR lowering, desugaring, and state machines.
pub mod ir;
/// Lexer: tokenization of Kraken source code into token streams.
pub mod lexer;
/// Module system: file-based module resolution, imports, and symbol visibility.
pub mod modules;
/// Compiler optimizations: constant folding, dead code elimination, loop and memory optimization.
pub mod optimizer;
/// Parser: token stream to AST conversion, expression/statement parsing.
pub mod parser;
/// Semantic versioning enforcement: API surface tracking, diff detection, and bump validation.
///
/// See [`semver::ApiSurface`] and [`semver::ApiDiff`] for details.
pub mod semver;
/// Cross-platform target support: target triples, architecture detection, LLVM target generation.
pub mod target;

pub use error::{CompilerError, CompilerResult};
