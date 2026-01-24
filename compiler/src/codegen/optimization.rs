//! LLVM optimization passes and optimization level control.

#![allow(dead_code)]

use llvm_sys::core::*;
use llvm_sys::prelude::*;

/// Optimization level for LLVM compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptimizationLevel {
    /// No optimizations (-O0)
    None,
    /// Basic optimizations (-O1)
    Less,
    /// Default optimizations (-O2)
    Default,
    /// Aggressive optimizations (-O3)
    Aggressive,
}

impl OptimizationLevel {
    /// Get LLVM optimization level value
    pub fn to_llvm_level(self) -> u32 {
        match self {
            OptimizationLevel::None => 0,
            OptimizationLevel::Less => 1,
            OptimizationLevel::Default => 2,
            OptimizationLevel::Aggressive => 3,
        }
    }

    /// Parse optimization level from string
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "0" | "O0" => Some(OptimizationLevel::None),
            "1" | "O1" => Some(OptimizationLevel::Less),
            "2" | "O2" => Some(OptimizationLevel::Default),
            "3" | "O3" => Some(OptimizationLevel::Aggressive),
            _ => None,
        }
    }
}

/// Optimizer for LLVM modules
pub struct Optimizer {
    optimization_level: OptimizationLevel,
}

impl Optimizer {
    pub fn new(optimization_level: OptimizationLevel) -> Self {
        Self { optimization_level }
    }

    /// Apply optimization passes to an LLVM module
    ///
    /// # Safety
    /// Caller must ensure the module is a valid LLVM module reference
    pub unsafe fn optimize_module(&self, module: LLVMModuleRef) {
        // Create module pass manager
        let module_pm = LLVMCreatePassManager();

        // Run optimization passes based on level
        if self.optimization_level != OptimizationLevel::None {
            LLVMRunPassManager(module_pm, module);
        }

        // Cleanup
        LLVMDisposePassManager(module_pm);
    }

    /// Apply function-level optimization passes
    ///
    /// # Safety
    /// Caller must ensure function and module are valid LLVM references
    pub unsafe fn optimize_function(&self, function: LLVMValueRef, module: LLVMModuleRef) {
        // Create function pass manager
        let function_pm = LLVMCreateFunctionPassManagerForModule(module);

        // Initialize pass manager
        LLVMInitializeFunctionPassManager(function_pm);

        // Run optimization passes on function
        if self.optimization_level != OptimizationLevel::None {
            LLVMRunFunctionPassManager(function_pm, function);
        }

        // Finalize pass manager
        LLVMFinalizeFunctionPassManager(function_pm);

        // Cleanup
        LLVMDisposePassManager(function_pm);
    }

    /// Enable specific optimization passes
    pub fn enable_dead_code_elimination(&self) -> bool {
        self.optimization_level != OptimizationLevel::None
    }

    /// Enable constant folding
    pub fn enable_constant_folding(&self) -> bool {
        self.optimization_level != OptimizationLevel::None
    }

    /// Enable inlining
    pub fn enable_inlining(&self) -> bool {
        matches!(
            self.optimization_level,
            OptimizationLevel::Default | OptimizationLevel::Aggressive
        )
    }

    /// Enable loop optimizations
    pub fn enable_loop_optimizations(&self) -> bool {
        matches!(
            self.optimization_level,
            OptimizationLevel::Default | OptimizationLevel::Aggressive
        )
    }

    /// Enable tail call optimization
    pub fn enable_tail_call_optimization(&self) -> bool {
        self.optimization_level == OptimizationLevel::Aggressive
    }
}

impl Default for Optimizer {
    fn default() -> Self {
        Self::new(OptimizationLevel::Default)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_optimization_level_to_llvm() {
        assert_eq!(OptimizationLevel::None.to_llvm_level(), 0);
        assert_eq!(OptimizationLevel::Less.to_llvm_level(), 1);
        assert_eq!(OptimizationLevel::Default.to_llvm_level(), 2);
        assert_eq!(OptimizationLevel::Aggressive.to_llvm_level(), 3);
    }

    #[test]
    fn test_optimization_level_parse() {
        assert_eq!(OptimizationLevel::parse("0"), Some(OptimizationLevel::None));
        assert_eq!(
            OptimizationLevel::parse("O1"),
            Some(OptimizationLevel::Less)
        );
        assert_eq!(
            OptimizationLevel::parse("2"),
            Some(OptimizationLevel::Default)
        );
        assert_eq!(
            OptimizationLevel::parse("O3"),
            Some(OptimizationLevel::Aggressive)
        );
        assert_eq!(OptimizationLevel::parse("invalid"), None);
    }

    #[test]
    fn test_optimizer_creation() {
        let optimizer = Optimizer::new(OptimizationLevel::Aggressive);
        assert_eq!(optimizer.optimization_level, OptimizationLevel::Aggressive);
    }

    #[test]
    fn test_optimization_flags() {
        let opt_none = Optimizer::new(OptimizationLevel::None);
        assert!(!opt_none.enable_dead_code_elimination());
        assert!(!opt_none.enable_inlining());

        let opt_aggressive = Optimizer::new(OptimizationLevel::Aggressive);
        assert!(opt_aggressive.enable_dead_code_elimination());
        assert!(opt_aggressive.enable_inlining());
        assert!(opt_aggressive.enable_tail_call_optimization());
    }
}
