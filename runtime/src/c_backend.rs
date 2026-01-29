//! C backend for maximum portability.
//!
//! Provides C code generation from IR for alternative bootstrap path.

use crate::ir_gen::{BinaryOp, IRFunction, IRInstruction, IRModule, IRType, IRValue};
use std::fmt::Write;

/// C code generator.
pub struct CBackend {
    output: String,
    indent_level: usize,
}

impl CBackend {
    /// Create a new C backend.
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
        }
    }

    /// Generate C code from IR module.
    pub fn generate(&mut self, module: &IRModule) -> Result<String, String> {
        self.output.clear();
        self.indent_level = 0;

        // Header
        self.emit_line("#include <stdint.h>");
        self.emit_line("#include <stdbool.h>");
        self.emit_line("#include <stdio.h>");
        self.emit_line("");

        // Forward declarations
        for function in module.functions.values() {
            self.emit_function_declaration(function)?;
        }
        self.emit_line("");

        // Function definitions
        for function in module.functions.values() {
            self.emit_function(function)?;
            self.emit_line("");
        }

        Ok(self.output.clone())
    }

    /// Emit a function declaration.
    fn emit_function_declaration(&mut self, function: &IRFunction) -> Result<(), String> {
        let return_type = self.type_to_c(&function.return_type);
        write!(self.output, "{} {}(", return_type, function.name).unwrap();

        for (i, (param_name, param_type)) in function.params.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ").unwrap();
            }
            write!(self.output, "{} {}", self.type_to_c(param_type), param_name).unwrap();
        }

        writeln!(self.output, ");").unwrap();
        Ok(())
    }

    /// Emit a function definition.
    fn emit_function(&mut self, function: &IRFunction) -> Result<(), String> {
        let return_type = self.type_to_c(&function.return_type);
        write!(self.output, "{} {}(", return_type, function.name).unwrap();

        for (i, (param_name, param_type)) in function.params.iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ").unwrap();
            }
            write!(self.output, "{} {}", self.type_to_c(param_type), param_name).unwrap();
        }

        writeln!(self.output, ") {{").unwrap();
        self.indent_level += 1;

        // Emit instructions
        for instruction in &function.instructions {
            self.emit_instruction(instruction)?;
        }

        self.indent_level -= 1;
        self.emit_line("}");
        Ok(())
    }

    /// Emit an instruction.
    fn emit_instruction(&mut self, instruction: &IRInstruction) -> Result<(), String> {
        match instruction {
            IRInstruction::Alloca { name, ty } => {
                self.emit_line(&format!("{} {};", self.type_to_c(ty), name));
            }
            IRInstruction::Store { value, ptr } => {
                self.emit_line(&format!(
                    "{} = {};",
                    self.value_to_c(ptr),
                    self.value_to_c(value)
                ));
            }
            IRInstruction::Load { ptr, result } => {
                self.emit_line(&format!("auto {} = {};", result, self.value_to_c(ptr)));
            }
            IRInstruction::BinaryOp {
                op,
                left,
                right,
                result,
            } => {
                let op_str = self.binary_op_to_c(*op);
                self.emit_line(&format!(
                    "auto {} = {} {} {};",
                    result,
                    self.value_to_c(left),
                    op_str,
                    self.value_to_c(right)
                ));
            }
            IRInstruction::Call {
                function,
                args,
                result,
            } => {
                let args_str = args
                    .iter()
                    .map(|arg| self.value_to_c(arg))
                    .collect::<Vec<_>>()
                    .join(", ");

                if let Some(res) = result {
                    self.emit_line(&format!("auto {res} = {function}({args_str});"));
                } else {
                    self.emit_line(&format!("{function}({args_str});"));
                }
            }
            IRInstruction::Return { value } => {
                if let Some(val) = value {
                    self.emit_line(&format!("return {};", self.value_to_c(val)));
                } else {
                    self.emit_line("return;");
                }
            }
            IRInstruction::Branch { target } => {
                self.emit_line(&format!("goto {target};"));
            }
            IRInstruction::CondBranch {
                condition,
                true_label,
                false_label,
            } => {
                self.emit_line(&format!("if ({}) {{", self.value_to_c(condition)));
                self.indent_level += 1;
                self.emit_line(&format!("goto {true_label};"));
                self.indent_level -= 1;
                self.emit_line("} else {");
                self.indent_level += 1;
                self.emit_line(&format!("goto {false_label};"));
                self.indent_level -= 1;
                self.emit_line("}");
            }
            IRInstruction::Label { name } => {
                self.emit_line(&format!("{name}:"));
            }
        }
        Ok(())
    }

    /// Convert IR type to C type.
    #[allow(clippy::only_used_in_recursion)]
    fn type_to_c(&self, ty: &IRType) -> String {
        match ty {
            IRType::Void => "void".to_string(),
            IRType::I1 => "bool".to_string(),
            IRType::I8 => "int8_t".to_string(),
            IRType::I32 => "int32_t".to_string(),
            IRType::I64 => "int64_t".to_string(),
            IRType::F64 => "double".to_string(),
            IRType::Pointer(inner) => format!("{}*", self.type_to_c(inner)),
            IRType::Function { .. } => "void*".to_string(),
        }
    }

    /// Convert IR value to C expression.
    fn value_to_c(&self, value: &IRValue) -> String {
        match value {
            IRValue::ConstInt(n) => n.to_string(),
            IRValue::ConstFloat(f) => f.to_string(),
            IRValue::ConstBool(b) => if *b { "true" } else { "false" }.to_string(),
            IRValue::Variable(name) => name.clone(),
            IRValue::Register(name) => name.clone(),
        }
    }

    /// Convert binary operation to C operator.
    fn binary_op_to_c(&self, op: BinaryOp) -> &'static str {
        match op {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }

    /// Emit a line with proper indentation.
    fn emit_line(&mut self, line: &str) {
        for _ in 0..self.indent_level {
            self.output.push_str("    ");
        }
        writeln!(self.output, "{line}").unwrap();
    }

    /// Get the generated C code.
    pub fn output(&self) -> &str {
        &self.output
    }
}

impl Default for CBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_gen::IRGenerator;

    #[test]
    fn test_c_backend_creation() {
        let backend = CBackend::new();
        assert!(backend.output().is_empty());
    }

    #[test]
    fn test_type_to_c() {
        let backend = CBackend::new();
        assert_eq!(backend.type_to_c(&IRType::I32), "int32_t");
        assert_eq!(backend.type_to_c(&IRType::I64), "int64_t");
        assert_eq!(backend.type_to_c(&IRType::F64), "double");
        assert_eq!(backend.type_to_c(&IRType::Void), "void");
    }

    #[test]
    fn test_value_to_c() {
        let backend = CBackend::new();
        assert_eq!(backend.value_to_c(&IRValue::ConstInt(42)), "42");
        assert_eq!(backend.value_to_c(&IRValue::ConstBool(true)), "true");
        assert_eq!(backend.value_to_c(&IRValue::Variable("x".to_string())), "x");
    }

    #[test]
    fn test_binary_op_to_c() {
        let backend = CBackend::new();
        assert_eq!(backend.binary_op_to_c(BinaryOp::Add), "+");
        assert_eq!(backend.binary_op_to_c(BinaryOp::Sub), "-");
        assert_eq!(backend.binary_op_to_c(BinaryOp::Mul), "*");
        assert_eq!(backend.binary_op_to_c(BinaryOp::Eq), "==");
    }

    #[test]
    fn test_c_backend_simple_function() {
        let mut ir_gen = IRGenerator::new("test".to_string());
        ir_gen.start_function("main".to_string(), vec![], IRType::I32);
        ir_gen.gen_return(Some(IRValue::ConstInt(42)));
        ir_gen.finish_function();

        let ir_module = ir_gen.into_module();
        let mut backend = CBackend::new();
        let result = backend.generate(&ir_module);

        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("#include <stdint.h>"));
        assert!(code.contains("int32_t main()"));
        assert!(code.contains("return 42;"));
    }

    #[test]
    fn test_c_backend_with_params() {
        let mut ir_gen = IRGenerator::new("test".to_string());
        ir_gen.start_function(
            "add".to_string(),
            vec![
                ("a".to_string(), IRType::I32),
                ("b".to_string(), IRType::I32),
            ],
            IRType::I32,
        );
        let result = ir_gen.gen_binary_op(
            BinaryOp::Add,
            IRValue::Variable("a".to_string()),
            IRValue::Variable("b".to_string()),
        );
        ir_gen.gen_return(Some(result));
        ir_gen.finish_function();

        let ir_module = ir_gen.into_module();
        let mut backend = CBackend::new();
        let result = backend.generate(&ir_module);

        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("int32_t add(int32_t a, int32_t b)"));
    }

    #[test]
    fn test_c_backend_output() {
        let backend = CBackend::new();
        assert_eq!(backend.output(), "");
    }
}
