use crate::error::{CompilerError, CompilerResult, SourceLocation};
use crate::parser::ast::*;
use crate::lexer::token::Operator;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::analysis::*;
use std::collections::HashMap;
use std::ffi::{CString, CStr};
use std::path::PathBuf;
use std::ptr;

/// LLVM code generator for Kraken.
/// 
/// Generates executable binaries from type-checked AST using LLVM.
pub struct LLVMCodegen {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    named_values: HashMap<String, LLVMValueRef>,
    file_path: PathBuf,
}

impl LLVMCodegen {
    /// Create a new LLVM code generator.
    /// 
    /// # Arguments
    /// * `module_name` - Name of the LLVM module to generate
    /// * `file_path` - Source file path for error reporting
    pub fn new(module_name: String, file_path: PathBuf) -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let module_name_cstr = CString::new(module_name.as_str()).expect("CString conversion failed");
            let module = LLVMModuleCreateWithNameInContext(module_name_cstr.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            Self {
                context,
                module,
                builder,
                named_values: HashMap::new(),
                file_path,
            }
        }
    }

    /// Generate LLVM IR and compile to object file.
    /// 
    /// # Arguments
    /// * `program` - The AST program to compile
    /// * `output_path` - Path for the output object file
    /// 
    /// # Errors
    /// Returns `CompilerError::CodegenError` if code generation fails
    pub fn compile(&mut self, program: &Program, output_path: &PathBuf) -> CompilerResult<()> {
        unsafe {
            // Initialize LLVM targets
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            // Generate code for all statements
            for statement in &program.statements {
                self.codegen_statement(statement)?;
            }

            // Verify the module
            let mut error_msg: *mut i8 = ptr::null_mut();
            if LLVMVerifyModule(
                self.module,
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                &mut error_msg,
            ) != 0
            {
                let error_str = if !error_msg.is_null() {
                    CStr::from_ptr(error_msg).to_string_lossy().into_owned()
                } else {
                    "Unknown verification error".to_string()
                };
                LLVMDisposeMessage(error_msg);
                return Err(CompilerError::codegen_error(format!(
                    "Module verification failed: {error_str}"
                )));
            }

            // Get target triple
            let target_triple = LLVMGetDefaultTargetTriple();
            LLVMSetTarget(self.module, target_triple);

            // Get target
            let mut target: LLVMTargetRef = ptr::null_mut();
            if LLVMGetTargetFromTriple(target_triple, &mut target, &mut error_msg) != 0 {
                let error_str = if !error_msg.is_null() {
                    CStr::from_ptr(error_msg).to_string_lossy().into_owned()
                } else {
                    "Unknown target error".to_string()
                };
                LLVMDisposeMessage(error_msg);
                return Err(CompilerError::codegen_error(format!(
                    "Failed to get target: {error_str}"
                )));
            }

            // Create target machine
            let cpu = CString::new("generic").expect("CString failed");
            let features = CString::new("").expect("CString failed");
            let target_machine = LLVMCreateTargetMachine(
                target,
                target_triple,
                cpu.as_ptr(),
                features.as_ptr(),
                LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                LLVMRelocMode::LLVMRelocPIC,
                LLVMCodeModel::LLVMCodeModelDefault,
            );

            if target_machine.is_null() {
                return Err(CompilerError::codegen_error("Failed to create target machine"));
            }

            // Emit object file
            let output_cstr = CString::new(output_path.to_str().expect("Invalid path"))
                .expect("CString failed");
            
            if LLVMTargetMachineEmitToFile(
                target_machine,
                self.module,
                output_cstr.as_ptr() as *mut i8,
                LLVMCodeGenFileType::LLVMObjectFile,
                &mut error_msg,
            ) != 0
            {
                let error_str = if !error_msg.is_null() {
                    CStr::from_ptr(error_msg).to_string_lossy().into_owned()
                } else {
                    "Unknown emit error".to_string()
                };
                LLVMDisposeMessage(error_msg);
                return Err(CompilerError::codegen_error(format!(
                    "Failed to emit object file: {error_str}"
                )));
            }

            // Cleanup
            LLVMDisposeTargetMachine(target_machine);
            LLVMDisposeMessage(target_triple);

            Ok(())
        }
    }

    /// Generate code for a statement.
    fn codegen_statement(&mut self, statement: &Statement) -> CompilerResult<()> {
        match statement {
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                is_async: _,
                is_public: _,
            } => {
                self.codegen_function(name, parameters, return_type.as_ref().unwrap_or(&Type::Void), body)?;
                Ok(())
            }

            Statement::Return { value } => {
                unsafe {
                    if let Some(expr) = value {
                        let val = self.codegen_expression(expr)?;
                        LLVMBuildRet(self.builder, val);
                    } else {
                        LLVMBuildRetVoid(self.builder);
                    }
                }
                Ok(())
            }

            Statement::VariableDeclaration {
                name,
                type_annotation: _,
                initializer,
                is_mutable: _,
            } => {
                if let Some(init_expr) = initializer {
                    let init_val = self.codegen_expression(init_expr)?;
                    self.named_values.insert(name.clone(), init_val);
                }
                Ok(())
            }

            Statement::Expression(expr) => {
                self.codegen_expression(expr)?;
                Ok(())
            }

            _ => {
                // Other statements not yet implemented
                Ok(())
            }
        }
    }

    /// Generate code for a function.
    fn codegen_function(
        &mut self,
        name: &str,
        parameters: &[Parameter],
        return_type: &Type,
        body: &Block,
    ) -> CompilerResult<LLVMValueRef> {
        unsafe {
            // Build parameter types
            let mut param_types: Vec<LLVMTypeRef> = parameters
                .iter()
                .map(|p| self.get_llvm_type(&p.param_type))
                .collect();

            // Create function type
            let ret_type = self.get_llvm_type(return_type);
            let func_type = LLVMFunctionType(
                ret_type,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0, // not vararg
            );

            // Create function
            let func_name = CString::new(name).expect("CString failed");
            let function = LLVMAddFunction(self.module, func_name.as_ptr(), func_type);

            // Create entry block
            let entry_name = CString::new("entry").expect("CString failed");
            let entry_block = LLVMAppendBasicBlockInContext(self.context, function, entry_name.as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, entry_block);

            // Add parameters to named values
            self.named_values.clear();
            for (i, param) in parameters.iter().enumerate() {
                let param_val = LLVMGetParam(function, i as u32);
                let param_name = CString::new(param.name.as_str()).expect("CString failed");
                LLVMSetValueName2(param_val, param_name.as_ptr(), param.name.len());
                self.named_values.insert(param.name.clone(), param_val);
            }

            // Generate body
            let mut has_terminator = false;
            for stmt in &body.statements {
                if matches!(stmt, Statement::Return { .. }) {
                    has_terminator = true;
                }
                self.codegen_statement(stmt)?;
            }

            // Add default return if needed
            if !has_terminator {
                if return_type == &Type::Void {
                    LLVMBuildRetVoid(self.builder);
                } else {
                    // Return zero/default value
                    let zero = LLVMConstInt(ret_type, 0, 0);
                    LLVMBuildRet(self.builder, zero);
                }
            }

            Ok(function)
        }
    }

    /// Generate code for an expression.
    fn codegen_expression(&mut self, expression: &Expression) -> CompilerResult<LLVMValueRef> {
        unsafe {
            match expression {
                Expression::IntLiteral(value) => {
                    let int_type = LLVMInt64TypeInContext(self.context);
                    Ok(LLVMConstInt(int_type, *value as u64, 0))
                }

                Expression::FloatLiteral(value) => {
                    let float_type = LLVMDoubleTypeInContext(self.context);
                    Ok(LLVMConstReal(float_type, *value))
                }

                Expression::BoolLiteral(value) => {
                    let bool_type = LLVMInt1TypeInContext(self.context);
                    Ok(LLVMConstInt(bool_type, if *value { 1 } else { 0 }, 0))
                }

                Expression::StringLiteral(value) => {
                    let str_cstring = CString::new(value.as_str()).expect("CString failed");
                    Ok(LLVMBuildGlobalStringPtr(
                        self.builder,
                        str_cstring.as_ptr(),
                        CString::new("str").expect("CString failed").as_ptr(),
                    ))
                }

                Expression::Identifier(name) => {
                    self.named_values
                        .get(name)
                        .copied()
                        .ok_or_else(|| {
                            CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Undefined variable: {name}"),
                            )
                        })
                }

                Expression::Binary { left, operator, right } => {
                    let lhs = self.codegen_expression(left)?;
                    let rhs = self.codegen_expression(right)?;

                    let result = match operator {
                        Operator::Plus => {
                            let name = CString::new("addtmp").expect("CString failed");
                            LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::Minus => {
                            let name = CString::new("subtmp").expect("CString failed");
                            LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::Star => {
                            let name = CString::new("multmp").expect("CString failed");
                            LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::Slash => {
                            let name = CString::new("divtmp").expect("CString failed");
                            LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::Less => {
                            let name = CString::new("cmptmp").expect("CString failed");
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntSLT,
                                lhs,
                                rhs,
                                name.as_ptr(),
                            )
                        }
                        Operator::Greater => {
                            let name = CString::new("cmptmp").expect("CString failed");
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntSGT,
                                lhs,
                                rhs,
                                name.as_ptr(),
                            )
                        }
                        Operator::Equal => {
                            let name = CString::new("cmptmp").expect("CString failed");
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                                lhs,
                                rhs,
                                name.as_ptr(),
                            )
                        }
                        _ => {
                            return Err(CompilerError::codegen_error(format!(
                                "Unsupported binary operator: {operator}"
                            )));
                        }
                    };

                    Ok(result)
                }

                _ => {
                    Err(CompilerError::codegen_error("Unsupported expression type"))
                }
            }
        }
    }

    /// Convert Kraken type to LLVM type.
    fn get_llvm_type(&self, kraken_type: &Type) -> LLVMTypeRef {
        unsafe {
            match kraken_type {
                Type::Int => LLVMInt64TypeInContext(self.context),
                Type::Float => LLVMDoubleTypeInContext(self.context),
                Type::Bool => LLVMInt1TypeInContext(self.context),
                Type::String => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Array { element_type, size } => {
                    let elem_type = self.get_llvm_type(element_type);
                    if let Some(s) = size {
                        LLVMArrayType(elem_type, *s as u32)
                    } else {
                        LLVMPointerType(elem_type, 0)
                    }
                }
                Type::Reference { inner_type, .. } => {
                    let inner = self.get_llvm_type(inner_type);
                    LLVMPointerType(inner, 0)
                }
                Type::Custom(_) => {
                    // Struct types would be handled here
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
                Type::Generic { .. } => {
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
            }
        }
    }
}

impl Drop for LLVMCodegen {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_llvm_type_conversion() {
        let codegen = LLVMCodegen::new("test".to_string(), PathBuf::from("test.kr"));
        
        unsafe {
            let int_type = codegen.get_llvm_type(&Type::Int);
            let float_type = codegen.get_llvm_type(&Type::Float);
            let bool_type = codegen.get_llvm_type(&Type::Bool);
            let void_type = codegen.get_llvm_type(&Type::Void);
            
            assert!(!int_type.is_null());
            assert!(!float_type.is_null());
            assert!(!bool_type.is_null());
            assert!(!void_type.is_null());
        }
    }

    #[test]
    fn test_compile_empty_program() {
        let mut codegen = LLVMCodegen::new("test".to_string(), PathBuf::from("test.kr"));
        let program = Program::new(vec![]);
        let output = PathBuf::from("/tmp/test.o");
        
        let result = codegen.compile(&program, &output);
        assert!(result.is_ok());
    }
}
