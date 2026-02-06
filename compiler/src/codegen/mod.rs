/// IR-based code generation: translates compiler IR to LLVM IR.
pub mod ir_codegen;
/// LLVM backend: direct AST-to-LLVM-IR code generation, struct layout, and object file emission.
pub mod llvm_backend;
/// LLVM optimization passes: configurable optimization levels and pass management.
pub mod optimization;

#[cfg(test)]
mod codegen_tests;

#[allow(unused_imports)]
pub use ir_codegen::IrCodegen;
pub use llvm_backend::LLVMCodegen;
#[allow(unused_imports)]
pub use optimization::{OptimizationLevel, Optimizer};
