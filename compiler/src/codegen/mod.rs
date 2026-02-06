/// IR-based code generation: translates compiler IR to LLVM IR.
pub mod ir_codegen;
/// LLVM backend: direct AST-to-LLVM-IR code generation, struct layout, and object file emission.
pub mod llvm_backend;
/// LLVM optimization passes: configurable optimization levels and pass management.
pub mod optimization;

// LLVM global state is not thread-safe across context creation/destruction.
// This shared lock serializes all codegen tests to prevent SIGSEGV.
#[cfg(test)]
pub(crate) static LLVM_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

#[cfg(test)]
mod codegen_tests;

#[allow(unused_imports)]
pub use ir_codegen::IrCodegen;
pub use llvm_backend::LLVMCodegen;
#[allow(unused_imports)]
pub use optimization::{OptimizationLevel, Optimizer};
