//! IR to LLVM Code Generation
//!
//! Translates Kraken IR to LLVM IR for native code generation.
//!
//! NOTE: This module is reserved for future IR-based codegen pipeline.
#![allow(dead_code)]

use crate::error::{CompilerError, CompilerResult};
use crate::ir::types::*;
use crate::lexer::token::Operator;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMIntPredicate;
use std::collections::HashMap;
use std::ffi::CString;
use std::path::PathBuf;

/// LLVM code generator that consumes Kraken IR.
pub struct IrCodegen {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    /// Map from ValueId to LLVM value
    values: HashMap<u32, LLVMValueRef>,
    /// Map from variable name to alloca pointer
    variables: HashMap<String, LLVMValueRef>,
    /// Map from BlockId to LLVM basic block
    blocks: HashMap<u32, LLVMBasicBlockRef>,
    /// Map from function name to LLVM function
    functions: HashMap<String, LLVMValueRef>,
    /// Current function being generated
    current_function: Option<LLVMValueRef>,
    /// Source file path for error reporting
    #[allow(dead_code)]
    file_path: PathBuf,
}

impl IrCodegen {
    /// Create a new IR code generator.
    pub fn new(module_name: &str, file_path: PathBuf) -> Self {
        unsafe {
            let context = LLVMContextCreate();
            let module_cstr = CString::new(module_name).expect("CString failed");
            let module = LLVMModuleCreateWithNameInContext(module_cstr.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            Self {
                context,
                module,
                builder,
                values: HashMap::new(),
                variables: HashMap::new(),
                blocks: HashMap::new(),
                functions: HashMap::new(),
                current_function: None,
                file_path,
            }
        }
    }

    /// Generate LLVM IR from a Kraken IR program.
    pub fn generate(&mut self, program: &IrProgram) -> CompilerResult<()> {
        // First pass: declare all functions
        for func in &program.functions {
            self.declare_function(func)?;
        }

        // Second pass: generate function bodies
        for func in &program.functions {
            self.generate_function(func)?;
        }

        Ok(())
    }

    /// Get the LLVM module.
    pub fn get_module(&self) -> LLVMModuleRef {
        self.module
    }

    /// Convert IrType to LLVM type.
    fn ir_type_to_llvm(&self, ty: &IrType) -> LLVMTypeRef {
        unsafe {
            match ty {
                IrType::Void => LLVMVoidTypeInContext(self.context),
                IrType::Int => LLVMInt64TypeInContext(self.context),
                IrType::Float => LLVMDoubleTypeInContext(self.context),
                IrType::Bool => LLVMInt1TypeInContext(self.context),
                IrType::String | IrType::Bytes => {
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
                IrType::Str => {
                    // str is a fat pointer: { ptr: *i8, len: i64 }
                    let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let mut fields = [i8_ptr, i64_ty];
                    LLVMStructTypeInContext(self.context, fields.as_mut_ptr(), 2, 0)
                }
                IrType::VecInt | IrType::VecString | IrType::VecBytes => {
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
                IrType::MapStringInt | IrType::MapStringString => {
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
                IrType::SliceInt | IrType::SliceString | IrType::SliceBytes => {
                    // Slice is { ptr: *i8, len: i64 }
                    let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let mut fields = [i8_ptr, i64_ty];
                    LLVMStructTypeInContext(self.context, fields.as_mut_ptr(), 2, 0)
                }
                IrType::Array { element, size } => {
                    let elem_ty = self.ir_type_to_llvm(element);
                    LLVMArrayType2(elem_ty, size.unwrap_or(0) as u64)
                }
                IrType::Pointer(inner) => {
                    let inner_ty = self.ir_type_to_llvm(inner);
                    LLVMPointerType(inner_ty, 0)
                }
                IrType::Struct(_name) => {
                    // For now, treat structs as opaque pointers
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
            }
        }
    }

    /// Declare a function (create signature without body).
    fn declare_function(&mut self, func: &IrFunction) -> CompilerResult<()> {
        unsafe {
            let ret_ty = self.ir_type_to_llvm(&func.return_type);
            let mut param_types: Vec<LLVMTypeRef> = func
                .params
                .iter()
                .map(|p| self.ir_type_to_llvm(&p.ty))
                .collect();

            let fn_ty = LLVMFunctionType(
                ret_ty,
                param_types.as_mut_ptr(),
                param_types.len() as u32,
                0,
            );
            let fn_name = CString::new(func.name.as_str()).expect("CString failed");
            let llvm_fn = LLVMAddFunction(self.module, fn_name.as_ptr(), fn_ty);

            self.functions.insert(func.name.clone(), llvm_fn);
            Ok(())
        }
    }

    /// Generate code for a function.
    fn generate_function(&mut self, func: &IrFunction) -> CompilerResult<()> {
        let llvm_fn = *self.functions.get(&func.name).ok_or_else(|| {
            CompilerError::codegen_error(format!("Function not declared: {}", func.name))
        })?;

        self.current_function = Some(llvm_fn);
        self.values.clear();
        self.variables.clear();
        self.blocks.clear();

        unsafe {
            // Create basic blocks
            for block in &func.blocks {
                let block_name = CString::new(block.name.as_str()).expect("CString failed");
                let llvm_block =
                    LLVMAppendBasicBlockInContext(self.context, llvm_fn, block_name.as_ptr());
                self.blocks.insert(block.id.0, llvm_block);
            }

            // Set up function parameters
            for (i, param) in func.params.iter().enumerate() {
                let llvm_param = LLVMGetParam(llvm_fn, i as u32);
                // Create alloca for parameter
                let first_block = *self
                    .blocks
                    .get(&0)
                    .ok_or_else(|| CompilerError::codegen_error("Function has no entry block"))?;
                LLVMPositionBuilderAtEnd(self.builder, first_block);

                let param_ty = self.ir_type_to_llvm(&param.ty);
                let param_name = CString::new(param.name.as_str()).expect("CString failed");
                let alloca = LLVMBuildAlloca(self.builder, param_ty, param_name.as_ptr());
                LLVMBuildStore(self.builder, llvm_param, alloca);
                self.variables.insert(param.name.clone(), alloca);
            }

            // Generate code for each block
            for block in &func.blocks {
                self.generate_block(block)?;
            }
        }

        self.current_function = None;
        Ok(())
    }

    /// Generate code for a basic block.
    fn generate_block(&mut self, block: &IrBlock) -> CompilerResult<()> {
        let llvm_block = *self.blocks.get(&block.id.0).ok_or_else(|| {
            CompilerError::codegen_error(format!("Block not found: {}", block.id))
        })?;

        unsafe {
            LLVMPositionBuilderAtEnd(self.builder, llvm_block);

            for instr in &block.instructions {
                self.generate_instruction(instr)?;
            }
        }

        Ok(())
    }

    /// Generate code for a single instruction.
    fn generate_instruction(&mut self, instr: &IrInstruction) -> CompilerResult<()> {
        unsafe {
            match instr {
                IrInstruction::Alloca { dest, ty, name } => {
                    let llvm_ty = self.ir_type_to_llvm(ty);
                    let name_cstr = CString::new(name.as_str()).expect("CString failed");
                    let alloca = LLVMBuildAlloca(self.builder, llvm_ty, name_cstr.as_ptr());
                    self.values.insert(dest.0, alloca);
                    self.variables.insert(name.clone(), alloca);
                }

                IrInstruction::Store { value, ptr } => {
                    let llvm_value = self.ir_value_to_llvm(value)?;
                    let llvm_ptr = self.ir_value_to_llvm(ptr)?;
                    LLVMBuildStore(self.builder, llvm_value, llvm_ptr);
                }

                IrInstruction::Load { dest, ptr, ty } => {
                    let llvm_ptr = self.ir_value_to_llvm(ptr)?;
                    let llvm_ty = self.ir_type_to_llvm(ty);
                    let load = LLVMBuildLoad2(self.builder, llvm_ty, llvm_ptr, c"".as_ptr());
                    self.values.insert(dest.0, load);
                }

                IrInstruction::BinaryOp {
                    dest,
                    op,
                    left,
                    right,
                    ty,
                } => {
                    let llvm_left = self.ir_value_to_llvm(left)?;
                    let llvm_right = self.ir_value_to_llvm(right)?;
                    let result = self.generate_binary_op(*op, llvm_left, llvm_right, ty)?;
                    self.values.insert(dest.0, result);
                }

                IrInstruction::UnaryOp {
                    dest,
                    op,
                    operand,
                    ty,
                } => {
                    let llvm_operand = self.ir_value_to_llvm(operand)?;
                    let result = self.generate_unary_op(*op, llvm_operand, ty)?;
                    self.values.insert(dest.0, result);
                }

                IrInstruction::Call {
                    dest,
                    func,
                    args,
                    ret_ty: _,
                } => {
                    let llvm_fn = *self.functions.get(func).ok_or_else(|| {
                        CompilerError::codegen_error(format!("Unknown function: {func}"))
                    })?;
                    let mut llvm_args: Vec<LLVMValueRef> = args
                        .iter()
                        .map(|a| self.ir_value_to_llvm(a))
                        .collect::<CompilerResult<Vec<_>>>()?;

                    let fn_ty = LLVMGlobalGetValueType(llvm_fn);
                    let call = LLVMBuildCall2(
                        self.builder,
                        fn_ty,
                        llvm_fn,
                        llvm_args.as_mut_ptr(),
                        llvm_args.len() as u32,
                        c"".as_ptr(),
                    );

                    if let Some(d) = dest {
                        self.values.insert(d.0, call);
                    }
                }

                IrInstruction::Return { value } => {
                    if let Some(v) = value {
                        let llvm_value = self.ir_value_to_llvm(v)?;
                        LLVMBuildRet(self.builder, llvm_value);
                    } else {
                        LLVMBuildRetVoid(self.builder);
                    }
                }

                IrInstruction::Branch { target } => {
                    let llvm_block = *self.blocks.get(&target.0).ok_or_else(|| {
                        CompilerError::codegen_error(format!("Unknown block: {target}"))
                    })?;
                    LLVMBuildBr(self.builder, llvm_block);
                }

                IrInstruction::CondBranch {
                    cond,
                    then_block,
                    else_block,
                } => {
                    let llvm_cond = self.ir_value_to_llvm(cond)?;
                    let llvm_then = *self.blocks.get(&then_block.0).ok_or_else(|| {
                        CompilerError::codegen_error(format!("Unknown block: {then_block}"))
                    })?;
                    let llvm_else = *self.blocks.get(&else_block.0).ok_or_else(|| {
                        CompilerError::codegen_error(format!("Unknown block: {else_block}"))
                    })?;
                    LLVMBuildCondBr(self.builder, llvm_cond, llvm_then, llvm_else);
                }

                IrInstruction::Phi { dest, ty, incoming } => {
                    let llvm_ty = self.ir_type_to_llvm(ty);
                    let phi = LLVMBuildPhi(self.builder, llvm_ty, c"".as_ptr());

                    for (val, block_id) in incoming {
                        let llvm_val = self.ir_value_to_llvm(val)?;
                        let llvm_block = *self.blocks.get(&block_id.0).ok_or_else(|| {
                            CompilerError::codegen_error(format!("Unknown block: {block_id}"))
                        })?;
                        LLVMAddIncoming(phi, [llvm_val].as_mut_ptr(), [llvm_block].as_mut_ptr(), 1);
                    }

                    self.values.insert(dest.0, phi);
                }

                IrInstruction::GetElementPtr {
                    dest,
                    ptr,
                    indices,
                    ty,
                } => {
                    let llvm_ptr = self.ir_value_to_llvm(ptr)?;
                    let llvm_ty = self.ir_type_to_llvm(ty);
                    let mut llvm_indices: Vec<LLVMValueRef> = indices
                        .iter()
                        .map(|i| self.ir_value_to_llvm(i))
                        .collect::<CompilerResult<Vec<_>>>()?;

                    let gep = LLVMBuildGEP2(
                        self.builder,
                        llvm_ty,
                        llvm_ptr,
                        llvm_indices.as_mut_ptr(),
                        llvm_indices.len() as u32,
                        c"".as_ptr(),
                    );
                    self.values.insert(dest.0, gep);
                }

                IrInstruction::ExtractValue {
                    dest,
                    ptr,
                    field_idx,
                    ty: _,
                } => {
                    let llvm_ptr = self.ir_value_to_llvm(ptr)?;
                    let extract =
                        LLVMBuildExtractValue(self.builder, llvm_ptr, *field_idx, c"".as_ptr());
                    self.values.insert(dest.0, extract);
                }

                IrInstruction::InsertValue {
                    dest,
                    struct_val,
                    value,
                    field_idx,
                } => {
                    let llvm_struct = self.ir_value_to_llvm(struct_val)?;
                    let llvm_value = self.ir_value_to_llvm(value)?;
                    let insert = LLVMBuildInsertValue(
                        self.builder,
                        llvm_struct,
                        llvm_value,
                        *field_idx,
                        c"".as_ptr(),
                    );
                    self.values.insert(dest.0, insert);
                }
            }
        }

        Ok(())
    }

    /// Convert IrValue to LLVM value.
    fn ir_value_to_llvm(&self, value: &IrValue) -> CompilerResult<LLVMValueRef> {
        unsafe {
            match value {
                IrValue::Register(id) => self
                    .values
                    .get(&id.0)
                    .copied()
                    .ok_or_else(|| CompilerError::codegen_error(format!("Unknown value: {id}"))),
                IrValue::ConstInt(v) => Ok(LLVMConstInt(
                    LLVMInt64TypeInContext(self.context),
                    *v as u64,
                    1,
                )),
                IrValue::ConstFloat(v) => {
                    Ok(LLVMConstReal(LLVMDoubleTypeInContext(self.context), *v))
                }
                IrValue::ConstBool(v) => Ok(LLVMConstInt(
                    LLVMInt1TypeInContext(self.context),
                    *v as u64,
                    0,
                )),
                IrValue::ConstString(s) => {
                    let cstr = CString::new(s.as_str()).expect("CString failed");
                    Ok(LLVMBuildGlobalStringPtr(
                        self.builder,
                        cstr.as_ptr(),
                        c"str".as_ptr(),
                    ))
                }
                IrValue::Null => {
                    let ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    Ok(LLVMConstNull(ptr_ty))
                }
                IrValue::Variable(name) => self.variables.get(name).copied().ok_or_else(|| {
                    CompilerError::codegen_error(format!("Unknown variable: {name}"))
                }),
                IrValue::Function(name) => self.functions.get(name).copied().ok_or_else(|| {
                    CompilerError::codegen_error(format!("Unknown function: {name}"))
                }),
            }
        }
    }

    /// Generate binary operation.
    fn generate_binary_op(
        &self,
        op: Operator,
        left: LLVMValueRef,
        right: LLVMValueRef,
        ty: &IrType,
    ) -> CompilerResult<LLVMValueRef> {
        unsafe {
            let is_float = matches!(ty, IrType::Float);

            let result = match op {
                Operator::Plus => {
                    if is_float {
                        LLVMBuildFAdd(self.builder, left, right, c"".as_ptr())
                    } else {
                        LLVMBuildAdd(self.builder, left, right, c"".as_ptr())
                    }
                }
                Operator::Minus => {
                    if is_float {
                        LLVMBuildFSub(self.builder, left, right, c"".as_ptr())
                    } else {
                        LLVMBuildSub(self.builder, left, right, c"".as_ptr())
                    }
                }
                Operator::Star => {
                    if is_float {
                        LLVMBuildFMul(self.builder, left, right, c"".as_ptr())
                    } else {
                        LLVMBuildMul(self.builder, left, right, c"".as_ptr())
                    }
                }
                Operator::Slash => {
                    if is_float {
                        LLVMBuildFDiv(self.builder, left, right, c"".as_ptr())
                    } else {
                        LLVMBuildSDiv(self.builder, left, right, c"".as_ptr())
                    }
                }
                Operator::Percent => LLVMBuildSRem(self.builder, left, right, c"".as_ptr()),
                Operator::Equal => {
                    if is_float {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealOEQ,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    } else {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntPredicate::LLVMIntEQ,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    }
                }
                Operator::NotEqual => {
                    if is_float {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealONE,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    } else {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntPredicate::LLVMIntNE,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    }
                }
                Operator::Less => {
                    if is_float {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealOLT,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    } else {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntPredicate::LLVMIntSLT,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    }
                }
                Operator::LessEqual => {
                    if is_float {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealOLE,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    } else {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntPredicate::LLVMIntSLE,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    }
                }
                Operator::Greater => {
                    if is_float {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealOGT,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    } else {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntPredicate::LLVMIntSGT,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    }
                }
                Operator::GreaterEqual => {
                    if is_float {
                        LLVMBuildFCmp(
                            self.builder,
                            llvm_sys::LLVMRealPredicate::LLVMRealOGE,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    } else {
                        LLVMBuildICmp(
                            self.builder,
                            LLVMIntPredicate::LLVMIntSGE,
                            left,
                            right,
                            c"".as_ptr(),
                        )
                    }
                }
                Operator::And => LLVMBuildAnd(self.builder, left, right, c"".as_ptr()),
                Operator::Or => LLVMBuildOr(self.builder, left, right, c"".as_ptr()),
                _ => {
                    return Err(CompilerError::codegen_error(format!(
                        "Unsupported binary operator: {op:?}"
                    )));
                }
            };

            Ok(result)
        }
    }

    /// Generate unary operation.
    fn generate_unary_op(
        &self,
        op: Operator,
        operand: LLVMValueRef,
        ty: &IrType,
    ) -> CompilerResult<LLVMValueRef> {
        unsafe {
            let is_float = matches!(ty, IrType::Float);

            let result = match op {
                Operator::Minus => {
                    if is_float {
                        LLVMBuildFNeg(self.builder, operand, c"".as_ptr())
                    } else {
                        LLVMBuildNeg(self.builder, operand, c"".as_ptr())
                    }
                }
                Operator::Not => LLVMBuildNot(self.builder, operand, c"".as_ptr()),
                _ => {
                    return Err(CompilerError::codegen_error(format!(
                        "Unsupported unary operator: {op:?}"
                    )));
                }
            };

            Ok(result)
        }
    }
}

impl Drop for IrCodegen {
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

    #[test]
    fn test_ir_codegen_empty_program() {
        let mut codegen = IrCodegen::new("test", PathBuf::from("test.kr"));
        let program = IrProgram::new();
        assert!(codegen.generate(&program).is_ok());
    }

    #[test]
    fn test_ir_codegen_simple_function() {
        let mut codegen = IrCodegen::new("test", PathBuf::from("test.kr"));

        let mut program = IrProgram::new();
        let mut func = IrFunction::new("main".to_string(), vec![], IrType::Int, true);

        let mut block = IrBlock::new(BlockId(0), "entry".to_string());
        block.instructions.push(IrInstruction::Return {
            value: Some(IrValue::ConstInt(0)),
        });
        func.blocks.push(block);
        program.functions.push(func);

        assert!(codegen.generate(&program).is_ok());
    }
}
