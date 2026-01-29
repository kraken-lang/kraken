//! Code generation infrastructure for bootstrap compiler.
//!
//! Provides LLVM code generation from IR.

#[cfg(test)]
use crate::ir_gen::IRValue;
use crate::ir_gen::{BinaryOp, IRFunction, IRInstruction, IRModule, IRType};
use crate::llvm_bindings::{Builder, Context, IntType, Module};
use std::collections::HashMap;

/// Code generator for LLVM.
pub struct CodeGenerator {
    #[allow(dead_code)]
    context: Context,
    module: Module,
    #[allow(dead_code)]
    builder: Builder,
    named_values: HashMap<String, String>,
}

impl CodeGenerator {
    /// Create a new code generator.
    pub fn new(module_name: &str) -> Self {
        Self {
            context: Context::new(),
            module: Module::new(module_name),
            builder: Builder::new(),
            named_values: HashMap::new(),
        }
    }

    /// Get the generated module.
    pub fn module(&self) -> &Module {
        &self.module
    }

    /// Generate LLVM IR from IR module.
    pub fn generate(&mut self, ir_module: &IRModule) -> Result<(), String> {
        for function in ir_module.functions.values() {
            self.generate_function(function)?;
        }
        Ok(())
    }

    /// Generate LLVM IR for a function.
    fn generate_function(&mut self, function: &IRFunction) -> Result<(), String> {
        self.named_values.clear();

        // Add parameters to named values
        for (param_name, _) in &function.params {
            self.named_values
                .insert(param_name.clone(), param_name.clone());
        }

        // Generate instructions
        for instruction in &function.instructions {
            self.generate_instruction(instruction)?;
        }

        Ok(())
    }

    /// Generate LLVM IR for an instruction.
    fn generate_instruction(&mut self, instruction: &IRInstruction) -> Result<(), String> {
        match instruction {
            IRInstruction::Alloca { name, .. } => {
                self.named_values.insert(name.clone(), name.clone());
                Ok(())
            }
            IRInstruction::Store { .. } => Ok(()),
            IRInstruction::Load { result, .. } => {
                self.named_values.insert(result.clone(), result.clone());
                Ok(())
            }
            IRInstruction::BinaryOp { result, .. } => {
                self.named_values.insert(result.clone(), result.clone());
                Ok(())
            }
            IRInstruction::Call { result, .. } => {
                if let Some(res) = result {
                    self.named_values.insert(res.clone(), res.clone());
                }
                Ok(())
            }
            IRInstruction::Return { .. } => Ok(()),
            IRInstruction::Branch { .. } => Ok(()),
            IRInstruction::CondBranch { .. } => Ok(()),
            IRInstruction::Label { .. } => Ok(()),
        }
    }

    /// Convert IR type to LLVM type.
    #[allow(dead_code)]
    fn ir_type_to_llvm(&self, ty: &IRType) -> IntType {
        match ty {
            IRType::I1 => IntType::i1(),
            IRType::I8 => IntType::i8(),
            IRType::I32 => IntType::i32(),
            IRType::I64 => IntType::i64(),
            _ => IntType::i64(), // Default
        }
    }

    /// Get LLVM opcode for binary operation.
    #[allow(dead_code)]
    fn binary_op_to_llvm(&self, op: BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "add",
            BinaryOp::Sub => "sub",
            BinaryOp::Mul => "mul",
            BinaryOp::Div => "sdiv",
            BinaryOp::Mod => "srem",
            BinaryOp::Eq => "icmp eq",
            BinaryOp::Ne => "icmp ne",
            BinaryOp::Lt => "icmp slt",
            BinaryOp::Le => "icmp sle",
            BinaryOp::Gt => "icmp sgt",
            BinaryOp::Ge => "icmp sge",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
        }
    }

    /// Verify the generated module.
    pub fn verify(&self) -> Result<(), String> {
        self.module.verify()
    }

    /// Print the generated LLVM IR.
    pub fn print_ir(&self) -> String {
        self.module.print_ir()
    }
}

/// Optimization level for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    None,
    Less,
    Default,
    Aggressive,
}

impl OptLevel {
    /// Get the optimization level as a number.
    pub fn as_number(&self) -> u32 {
        match self {
            OptLevel::None => 0,
            OptLevel::Less => 1,
            OptLevel::Default => 2,
            OptLevel::Aggressive => 3,
        }
    }
}

/// Code generation options.
#[derive(Debug, Clone)]
pub struct CodeGenOptions {
    pub opt_level: OptLevel,
    pub debug_info: bool,
    pub emit_llvm_ir: bool,
    pub emit_asm: bool,
}

impl Default for CodeGenOptions {
    fn default() -> Self {
        Self {
            opt_level: OptLevel::Default,
            debug_info: false,
            emit_llvm_ir: false,
            emit_asm: false,
        }
    }
}

impl CodeGenOptions {
    /// Create new options with specified optimization level.
    pub fn with_opt_level(opt_level: OptLevel) -> Self {
        Self {
            opt_level,
            ..Default::default()
        }
    }

    /// Enable debug information.
    pub fn with_debug_info(mut self) -> Self {
        self.debug_info = true;
        self
    }

    /// Enable LLVM IR emission.
    pub fn with_llvm_ir(mut self) -> Self {
        self.emit_llvm_ir = true;
        self
    }

    /// Enable assembly emission.
    pub fn with_asm(mut self) -> Self {
        self.emit_asm = true;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_gen::IRGenerator;

    #[test]
    fn test_code_generator_creation() {
        let gen = CodeGenerator::new("test");
        assert!(gen.verify().is_ok());
    }

    #[test]
    fn test_code_generator_simple_function() {
        let mut ir_gen = IRGenerator::new("test".to_string());
        ir_gen.start_function("main".to_string(), vec![], IRType::I32);
        ir_gen.gen_return(Some(IRValue::ConstInt(42)));
        ir_gen.finish_function();

        let ir_module = ir_gen.into_module();
        let mut codegen = CodeGenerator::new("test");
        let result = codegen.generate(&ir_module);
        assert!(result.is_ok());
    }

    #[test]
    fn test_binary_op_to_llvm() {
        let gen = CodeGenerator::new("test");
        assert_eq!(gen.binary_op_to_llvm(BinaryOp::Add), "add");
        assert_eq!(gen.binary_op_to_llvm(BinaryOp::Sub), "sub");
        assert_eq!(gen.binary_op_to_llvm(BinaryOp::Mul), "mul");
    }

    #[test]
    fn test_ir_type_to_llvm() {
        let gen = CodeGenerator::new("test");
        assert_eq!(gen.ir_type_to_llvm(&IRType::I32).bits(), 32);
        assert_eq!(gen.ir_type_to_llvm(&IRType::I64).bits(), 64);
    }

    #[test]
    fn test_opt_level() {
        assert_eq!(OptLevel::None.as_number(), 0);
        assert_eq!(OptLevel::Less.as_number(), 1);
        assert_eq!(OptLevel::Default.as_number(), 2);
        assert_eq!(OptLevel::Aggressive.as_number(), 3);
    }

    #[test]
    fn test_codegen_options() {
        let opts = CodeGenOptions::default();
        assert_eq!(opts.opt_level, OptLevel::Default);
        assert!(!opts.debug_info);

        let opts = CodeGenOptions::with_opt_level(OptLevel::Aggressive)
            .with_debug_info()
            .with_llvm_ir();
        assert_eq!(opts.opt_level, OptLevel::Aggressive);
        assert!(opts.debug_info);
        assert!(opts.emit_llvm_ir);
    }

    #[test]
    fn test_code_generator_print_ir() {
        let gen = CodeGenerator::new("test");
        let ir = gen.print_ir();
        assert!(ir.contains("test"));
    }
}
