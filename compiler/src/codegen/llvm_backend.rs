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
    array_variables: HashMap<String, bool>, // Track which variables are arrays
    struct_variables: HashMap<String, String>, // Track which variables are structs (var name -> struct name)
    struct_types: HashMap<String, (LLVMTypeRef, Vec<String>, Vec<LLVMTypeRef>)>, // struct name -> (LLVM type, field names, field types)
    functions: HashMap<String, LLVMValueRef>,
    current_function: Option<LLVMValueRef>,
    loop_exit_blocks: Vec<LLVMBasicBlockRef>,
    loop_continue_blocks: Vec<LLVMBasicBlockRef>,
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
                array_variables: HashMap::new(),
                struct_variables: HashMap::new(),
                struct_types: HashMap::new(),
                functions: HashMap::new(),
                current_function: None,
                loop_exit_blocks: Vec::new(),
                loop_continue_blocks: Vec::new(),
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

            // Declare standard library functions
            self.declare_stdlib_functions()?;

            // Two-pass compilation:
            // Pass 1: Declare all functions (so they can call each other)
            for statement in &program.statements {
                if let Statement::FunctionDeclaration { name, parameters, return_type, .. } = statement {
                    self.declare_function(name, parameters, return_type.as_ref().unwrap_or(&Type::Void))?;
                }
            }

            // Pass 2: Generate function bodies
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

            Statement::StructDeclaration { name, fields, is_public: _ } => {
                unsafe {
                    // Create LLVM struct type
                    let struct_name = CString::new(name.as_str()).expect("CString failed");
                    let struct_type = LLVMStructCreateNamed(self.context, struct_name.as_ptr());

                    // Get field types
                    let mut field_types: Vec<LLVMTypeRef> = Vec::new();
                    let mut field_names: Vec<String> = Vec::new();
                    
                    for field in fields {
                        field_types.push(self.get_llvm_type(&field.field_type));
                        field_names.push(field.name.clone());
                    }

                    // Set struct body
                    LLVMStructSetBody(
                        struct_type,
                        field_types.as_mut_ptr(),
                        field_types.len() as u32,
                        0, // not packed
                    );

                    // Store struct type, field names, and field types
                    self.struct_types.insert(name.clone(), (struct_type, field_names, field_types));
                }
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
                type_annotation,
                initializer,
                is_mutable: _,
            } => {
                unsafe {
                    // Check if this is an array or struct type
                    let is_array = if let Some(Type::Array { .. }) = type_annotation {
                        true
                    } else {
                        false
                    };
                    
                    let struct_name = if let Some(Type::Custom(sname)) = type_annotation {
                        Some(sname.clone())
                    } else {
                        None
                    };

                    // For array/struct literals without type annotation, generate them first
                    let pregenerated_value = if type_annotation.is_none() {
                        if let Some(init_expr) = initializer {
                            if matches!(init_expr, Expression::StructLiteral { .. } | Expression::Array { .. }) {
                                Some(self.codegen_expression(init_expr)?)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    // Allocate stack space for the variable
                    let var_type = if let Some(ty) = type_annotation {
                        self.get_llvm_type(ty)
                    } else if let Some(pregen) = pregenerated_value {
                        // Use the type from the pregenerated value
                        LLVMGetAllocatedType(pregen)
                    } else if let Some(init_expr) = initializer {
                        // Infer type from initializer (for non-array/struct)
                        if let Expression::StructLiteral { name: sname, .. } = init_expr {
                            // Track this as a struct variable
                            self.struct_variables.insert(name.clone(), sname.clone());
                            let (st, _, _) = self.struct_types.get(sname)
                                .cloned()
                                .ok_or_else(|| {
                                    CompilerError::codegen_error(format!("Undefined struct: {sname}"))
                                })?;
                            st
                        } else {
                            let init_val = self.codegen_expression(init_expr)?;
                            LLVMTypeOf(init_val)
                        }
                    } else {
                        return Err(CompilerError::codegen_error(
                            "Variable must have type annotation or initializer"
                        ));
                    };

                    // Create alloca at the entry block
                    let alloca = self.create_entry_block_alloca(var_type, name)?;

                    // Store initial value if provided
                    if let Some(init_expr) = initializer {
                        let init_val = if let Some(pregen) = pregenerated_value {
                            pregen
                        } else {
                            self.codegen_expression(init_expr)?
                        };
                        
                        // Check if this is a struct literal or array - if so, we need to copy it
                        if matches!(init_expr, Expression::StructLiteral { .. } | Expression::Array { .. }) {
                            // init_val is a pointer to the struct/array, we need to copy the data
                            // Use memcpy to copy the data
                            let size = LLVMSizeOf(var_type);
                            LLVMBuildMemCpy(
                                self.builder,
                                alloca,
                                0, // dest align
                                init_val,
                                0, // src align
                                size,
                            );
                            
                            // Track arrays
                            if matches!(init_expr, Expression::Array { .. }) {
                                self.array_variables.insert(name.clone(), true);
                            }
                        } else {
                            LLVMBuildStore(self.builder, init_val, alloca);
                        }
                    }

                    // Store the alloca pointer in named_values
                    self.named_values.insert(name.clone(), alloca);
                    
                    // Track if this is an array (from type annotation)
                    if is_array {
                        self.array_variables.insert(name.clone(), true);
                    }
                    
                    // Track if this is a struct (from type annotation)
                    if let Some(sname) = struct_name {
                        self.struct_variables.insert(name.clone(), sname);
                    }
                }
                Ok(())
            }

            Statement::Expression(expr) => {
                self.codegen_expression(expr)?;
                Ok(())
            }

            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                unsafe {
                    let cond_val = self.codegen_expression(condition)?;

                    let function = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("No current function for if statement")
                    })?;

                    // Create blocks
                    let then_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("then").expect("CString failed").as_ptr(),
                    );
                    let else_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("else").expect("CString failed").as_ptr(),
                    );
                    let merge_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("ifcont").expect("CString failed").as_ptr(),
                    );

                    // Branch based on condition
                    LLVMBuildCondBr(self.builder, cond_val, then_bb, else_bb);

                    // Generate then block
                    LLVMPositionBuilderAtEnd(self.builder, then_bb);
                    for stmt in &then_branch.statements {
                        self.codegen_statement(stmt)?;
                    }
                    // Check current block for terminator (might have changed during codegen)
                    let current_then_bb = LLVMGetInsertBlock(self.builder);
                    let then_has_terminator = !LLVMGetBasicBlockTerminator(current_then_bb).is_null();
                    if !then_has_terminator {
                        LLVMBuildBr(self.builder, merge_bb);
                    }

                    // Generate else block
                    LLVMPositionBuilderAtEnd(self.builder, else_bb);
                    if let Some(else_blk) = else_branch {
                        for stmt in &else_blk.statements {
                            self.codegen_statement(stmt)?;
                        }
                    }
                    // Check current block for terminator
                    let current_else_bb = LLVMGetInsertBlock(self.builder);
                    let else_has_terminator = !LLVMGetBasicBlockTerminator(current_else_bb).is_null();
                    if !else_has_terminator {
                        LLVMBuildBr(self.builder, merge_bb);
                    }

                    // Continue at merge block (only if at least one branch reaches it)
                    if !then_has_terminator || !else_has_terminator {
                        LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                    } else {
                        // Both branches have terminators, merge block is unreachable
                        // Delete the unreachable merge block
                        LLVMDeleteBasicBlock(merge_bb);
                        // Don't position builder anywhere - caller must check if current block has terminator
                    }
                }
                Ok(())
            }

            Statement::While { condition, body } => {
                unsafe {
                    let function = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("No current function for while loop")
                    })?;

                    // Create blocks
                    let cond_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("while.cond").expect("CString failed").as_ptr(),
                    );
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("while.body").expect("CString failed").as_ptr(),
                    );
                    let after_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("while.end").expect("CString failed").as_ptr(),
                    );

                    // Push loop blocks for break/continue
                    self.loop_exit_blocks.push(after_bb);
                    self.loop_continue_blocks.push(cond_bb);

                    // Branch to condition
                    LLVMBuildBr(self.builder, cond_bb);

                    // Generate condition block
                    LLVMPositionBuilderAtEnd(self.builder, cond_bb);
                    let cond_val = self.codegen_expression(condition)?;
                    LLVMBuildCondBr(self.builder, cond_val, loop_bb, after_bb);

                    // Generate loop body
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    for stmt in &body.statements {
                        self.codegen_statement(stmt)?;
                    }
                    // Branch back to condition if no terminator
                    if LLVMGetBasicBlockTerminator(loop_bb).is_null() {
                        LLVMBuildBr(self.builder, cond_bb);
                    }

                    // Pop loop blocks
                    self.loop_exit_blocks.pop();
                    self.loop_continue_blocks.pop();

                    // Continue after loop
                    LLVMPositionBuilderAtEnd(self.builder, after_bb);
                }
                Ok(())
            }

            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                unsafe {
                    let function = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("No current function for for loop")
                    })?;

                    // Generate initializer if present
                    if let Some(init) = initializer {
                        self.codegen_statement(init)?;
                    }

                    // Create blocks
                    let cond_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("for.cond").expect("CString failed").as_ptr(),
                    );
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("for.body").expect("CString failed").as_ptr(),
                    );
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("for.inc").expect("CString failed").as_ptr(),
                    );
                    let after_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        function,
                        CString::new("for.end").expect("CString failed").as_ptr(),
                    );

                    // Push loop blocks for break/continue
                    self.loop_exit_blocks.push(after_bb);
                    self.loop_continue_blocks.push(inc_bb);

                    // Branch to condition
                    LLVMBuildBr(self.builder, cond_bb);

                    // Generate condition block
                    LLVMPositionBuilderAtEnd(self.builder, cond_bb);
                    if let Some(cond) = condition {
                        let cond_val = self.codegen_expression(cond)?;
                        LLVMBuildCondBr(self.builder, cond_val, loop_bb, after_bb);
                    } else {
                        // No condition means infinite loop
                        LLVMBuildBr(self.builder, loop_bb);
                    }

                    // Generate loop body
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    for stmt in &body.statements {
                        self.codegen_statement(stmt)?;
                        // Stop generating if we hit a terminator
                        let current_bb = LLVMGetInsertBlock(self.builder);
                        if !LLVMGetBasicBlockTerminator(current_bb).is_null() {
                            break;
                        }
                    }
                    // Branch to increment if no terminator
                    let current_bb = LLVMGetInsertBlock(self.builder);
                    if LLVMGetBasicBlockTerminator(current_bb).is_null() {
                        LLVMBuildBr(self.builder, inc_bb);
                    }

                    // Generate increment block
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    if let Some(inc) = increment {
                        self.codegen_expression(inc)?;
                    }
                    // Branch back to condition
                    LLVMBuildBr(self.builder, cond_bb);

                    // Pop loop blocks
                    self.loop_exit_blocks.pop();
                    self.loop_continue_blocks.pop();

                    // Continue after loop
                    LLVMPositionBuilderAtEnd(self.builder, after_bb);
                }
                Ok(())
            }

            Statement::Break => {
                unsafe {
                    if let Some(&exit_bb) = self.loop_exit_blocks.last() {
                        LLVMBuildBr(self.builder, exit_bb);
                    } else {
                        return Err(CompilerError::codegen_error("Break outside of loop"));
                    }
                }
                Ok(())
            }

            Statement::Continue => {
                unsafe {
                    if let Some(&continue_bb) = self.loop_continue_blocks.last() {
                        LLVMBuildBr(self.builder, continue_bb);
                    } else {
                        return Err(CompilerError::codegen_error("Continue outside of loop"));
                    }
                }
                Ok(())
            }

            Statement::Match { expression, arms } => {
                unsafe {
                    let match_val = self.codegen_expression(expression)?;
                    
                    let function = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("Match outside of function")
                    })?;
                    
                    // Create basic blocks for each arm and the merge block
                    let mut arm_blocks = Vec::new();
                    let mut next_check_blocks = Vec::new();
                    
                    for _ in 0..arms.len() {
                        let arm_name = CString::new("match.arm").expect("CString failed");
                        let arm_bb = LLVMAppendBasicBlockInContext(self.context, function, arm_name.as_ptr());
                        arm_blocks.push(arm_bb);
                        
                        let next_name = CString::new("match.next").expect("CString failed");
                        let next_bb = LLVMAppendBasicBlockInContext(self.context, function, next_name.as_ptr());
                        next_check_blocks.push(next_bb);
                    }
                    
                    let merge_name = CString::new("match.merge").expect("CString failed");
                    let merge_bb = LLVMAppendBasicBlockInContext(self.context, function, merge_name.as_ptr());
                    
                    // Generate code for each arm
                    for (i, arm) in arms.iter().enumerate() {
                        // Check pattern
                        match &arm.pattern {
                            Pattern::Literal(lit_expr) => {
                                let lit_val = self.codegen_expression(lit_expr)?;
                                let cmp_name = CString::new("match.cmp").expect("CString failed");
                                let cond = LLVMBuildICmp(
                                    self.builder,
                                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                                    match_val,
                                    lit_val,
                                    cmp_name.as_ptr(),
                                );
                                
                                // Branch to arm or next check
                                LLVMBuildCondBr(self.builder, cond, arm_blocks[i], next_check_blocks[i]);
                            }
                            Pattern::Identifier(_name) => {
                                // Bind the value and execute arm
                                // For now, just jump to the arm (binding would need scope management)
                                LLVMBuildBr(self.builder, arm_blocks[i]);
                            }
                            Pattern::Wildcard => {
                                // Always matches
                                LLVMBuildBr(self.builder, arm_blocks[i]);
                            }
                        }
                        
                        // Generate arm body
                        LLVMPositionBuilderAtEnd(self.builder, arm_blocks[i]);
                        for stmt in &arm.body.statements {
                            self.codegen_statement(stmt)?;
                            // Check if we hit a terminator
                            if !LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder)).is_null() {
                                break;
                            }
                        }
                        
                        // Branch to merge if no terminator
                        if LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder)).is_null() {
                            LLVMBuildBr(self.builder, merge_bb);
                        }
                        
                        // Position at next check block
                        if i < arms.len() - 1 {
                            LLVMPositionBuilderAtEnd(self.builder, next_check_blocks[i]);
                        }
                    }
                    
                    // Last next_check block should jump to merge (no match case)
                    if let Some(&last_next) = next_check_blocks.last() {
                        LLVMPositionBuilderAtEnd(self.builder, last_next);
                        LLVMBuildBr(self.builder, merge_bb);
                    }
                    
                    // Position at merge block
                    LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                }
                Ok(())
            }

            _ => {
                // Other statements not yet implemented
                Ok(())
            }
        }
    }

    /// Declare standard library functions (printf, etc.).
    fn declare_stdlib_functions(&mut self) -> CompilerResult<()> {
        unsafe {
            // Declare printf: int printf(const char* format, ...)
            let i8_ptr_type = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let printf_type = LLVMFunctionType(
                LLVMInt32TypeInContext(self.context),
                [i8_ptr_type].as_mut_ptr(),
                1,
                1, // vararg
            );
            let printf_name = CString::new("printf").expect("CString failed");
            let printf_func = LLVMAddFunction(self.module, printf_name.as_ptr(), printf_type);
            self.functions.insert("printf".to_string(), printf_func);

            // Declare puts: int puts(const char* s)
            let puts_type = LLVMFunctionType(
                LLVMInt32TypeInContext(self.context),
                [i8_ptr_type].as_mut_ptr(),
                1,
                0, // not vararg
            );
            let puts_name = CString::new("puts").expect("CString failed");
            let puts_func = LLVMAddFunction(self.module, puts_name.as_ptr(), puts_type);
            self.functions.insert("puts".to_string(), puts_func);

            Ok(())
        }
    }

    /// Declare a function (without body).
    fn declare_function(
        &mut self,
        name: &str,
        parameters: &[Parameter],
        return_type: &Type,
    ) -> CompilerResult<LLVMValueRef> {
        unsafe {
            // Check if already declared
            if let Some(&func) = self.functions.get(name) {
                return Ok(func);
            }

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

            // Store in function table
            self.functions.insert(name.to_string(), function);

            Ok(function)
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
            // Get the already-declared function
            let function = *self.functions.get(name).ok_or_else(|| {
                CompilerError::codegen_error(format!("Function {name} not declared"))
            })?;

            self.current_function = Some(function);

            // Create entry block
            let entry_name = CString::new("entry").expect("CString failed");
            let entry_block = LLVMAppendBasicBlockInContext(self.context, function, entry_name.as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, entry_block);

            // Add parameters to named values (allocate on stack for mutability)
            self.named_values.clear();
            for (i, param) in parameters.iter().enumerate() {
                let param_val = LLVMGetParam(function, i as u32);
                let param_name = CString::new(param.name.as_str()).expect("CString failed");
                LLVMSetValueName2(param_val, param_name.as_ptr(), param.name.len());
                
                // Allocate stack space for parameter
                let param_type = self.get_llvm_type(&param.param_type);
                let alloca = self.create_entry_block_alloca(param_type, &param.name)?;
                
                // Store parameter value into alloca
                LLVMBuildStore(self.builder, param_val, alloca);
                
                // Store alloca in named_values
                self.named_values.insert(param.name.clone(), alloca);
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
                    let ret_type = self.get_llvm_type(return_type);
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
                    let alloca = self.named_values
                        .get(name)
                        .copied()
                        .ok_or_else(|| {
                            CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Undefined variable: {name}"),
                            )
                        })?;

                    // Check if this variable is an array or struct - if so, return pointer directly
                    if self.array_variables.get(name).copied().unwrap_or(false) || self.struct_variables.contains_key(name) {
                        return Ok(alloca);
                    }

                    // Load the value from the alloca (original working code)
                    let load_name = CString::new(format!("{name}.load")).expect("CString failed");
                    unsafe {
                        Ok(LLVMBuildLoad2(
                            self.builder,
                            LLVMGetAllocatedType(alloca),
                            alloca,
                            load_name.as_ptr(),
                        ))
                    }
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
                        Operator::NotEqual => {
                            let name = CString::new("cmptmp").expect("CString failed");
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntNE,
                                lhs,
                                rhs,
                                name.as_ptr(),
                            )
                        }
                        Operator::LessEqual => {
                            let name = CString::new("cmptmp").expect("CString failed");
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntSLE,
                                lhs,
                                rhs,
                                name.as_ptr(),
                            )
                        }
                        Operator::GreaterEqual => {
                            let name = CString::new("cmptmp").expect("CString failed");
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntSGE,
                                lhs,
                                rhs,
                                name.as_ptr(),
                            )
                        }
                        Operator::Percent => {
                            let name = CString::new("modtmp").expect("CString failed");
                            LLVMBuildSRem(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::And => {
                            let name = CString::new("andtmp").expect("CString failed");
                            LLVMBuildAnd(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::Or => {
                            let name = CString::new("ortmp").expect("CString failed");
                            LLVMBuildOr(self.builder, lhs, rhs, name.as_ptr())
                        }
                        _ => {
                            return Err(CompilerError::codegen_error(format!(
                                "Unsupported binary operator: {operator}"
                            )));
                        }
                    };

                    Ok(result)
                }

                Expression::Call { callee, arguments } => {
                    // For now, only support direct function calls (identifier)
                    if let Expression::Identifier(name) = &**callee {
                        // Look up the function
                        let function = self.functions.get(name).copied().ok_or_else(|| {
                            CompilerError::type_error(
                                SourceLocation::new(self.file_path.clone(), 0, 0),
                                format!("Undefined function: {name}"),
                            )
                        })?;

                        // Generate code for arguments
                        let mut arg_values: Vec<LLVMValueRef> = Vec::new();
                        for arg in arguments {
                            arg_values.push(self.codegen_expression(arg)?);
                        }

                        // Build the call
                        let call_name = CString::new("calltmp").expect("CString failed");
                        let func_type = LLVMGlobalGetValueType(function);
                        Ok(LLVMBuildCall2(
                            self.builder,
                            func_type,
                            function,
                            arg_values.as_mut_ptr(),
                            arg_values.len() as u32,
                            call_name.as_ptr(),
                        ))
                    } else {
                        Err(CompilerError::codegen_error("Only direct function calls supported"))
                    }
                }

                Expression::Unary { operator, operand } => {
                    let operand_val = self.codegen_expression(operand)?;
                    
                    let result = match operator {
                        Operator::Not => {
                            let name = CString::new("nottmp").expect("CString failed");
                            // Logical NOT: compare with 0 and invert
                            let zero = LLVMConstInt(LLVMTypeOf(operand_val), 0, 0);
                            LLVMBuildICmp(
                                self.builder,
                                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                                operand_val,
                                zero,
                                name.as_ptr(),
                            )
                        }
                        Operator::Minus => {
                            let name = CString::new("negtmp").expect("CString failed");
                            LLVMBuildNeg(self.builder, operand_val, name.as_ptr())
                        }
                        _ => {
                            return Err(CompilerError::codegen_error(format!(
                                "Unsupported unary operator: {operator}"
                            )));
                        }
                    };

                    Ok(result)
                }

                Expression::Array { elements } => {
                    unsafe {
                        if elements.is_empty() {
                            return Err(CompilerError::codegen_error("Empty array literals not supported"));
                        }

                        // Generate code for all elements
                        let mut element_values: Vec<LLVMValueRef> = Vec::new();
                        for elem in elements {
                            element_values.push(self.codegen_expression(elem)?);
                        }

                        // Get element type from first element
                        let elem_type = LLVMTypeOf(element_values[0]);
                        let array_type = LLVMArrayType(elem_type, elements.len() as u32);

                        // Allocate array on stack
                        let array_name = CString::new("array").expect("CString failed");
                        let array_alloca = LLVMBuildAlloca(self.builder, array_type, array_name.as_ptr());

                        // Store each element
                        for (i, &val) in element_values.iter().enumerate() {
                            let idx_name = CString::new(format!("idx{i}")).expect("CString failed");
                            let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                            let idx = LLVMConstInt(LLVMInt32TypeInContext(self.context), i as u64, 0);
                            
                            let mut indices = [zero, idx];
                            let elem_ptr = LLVMBuildInBoundsGEP2(
                                self.builder,
                                array_type,
                                array_alloca,
                                indices.as_mut_ptr(),
                                2,
                                idx_name.as_ptr(),
                            );
                            LLVMBuildStore(self.builder, val, elem_ptr);
                        }

                        Ok(array_alloca)
                    }
                }

                Expression::Index { array, index } => {
                    unsafe {
                        let array_val = self.codegen_expression(array)?;
                        let index_val = self.codegen_expression(index)?;

                        // Get array type (array_val is a pointer to the array)
                        let array_type = LLVMGetAllocatedType(array_val);

                        // Build GEP to get element pointer
                        let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                        let elem_ptr_name = CString::new("elemptr").expect("CString failed");
                        let mut indices = [zero, index_val];
                        let elem_ptr = LLVMBuildInBoundsGEP2(
                            self.builder,
                            array_type,
                            array_val,
                            indices.as_mut_ptr(),
                            2,
                            elem_ptr_name.as_ptr(),
                        );

                        // Get element type from array type
                        let elem_type = LLVMGetElementType(array_type);
                        let load_name = CString::new("elem").expect("CString failed");
                        Ok(LLVMBuildLoad2(self.builder, elem_type, elem_ptr, load_name.as_ptr()))
                    }
                }

                Expression::StructLiteral { name, fields } => {
                    unsafe {
                        // Get the struct type (clone to avoid borrow issues)
                        let (struct_type, field_names, _) = self.struct_types.get(name)
                            .cloned()
                            .ok_or_else(|| {
                                CompilerError::codegen_error(format!("Undefined struct: {name}"))
                            })?;
                        
                        // Allocate struct on stack
                        let struct_name = CString::new(format!("{name}.tmp")).expect("CString failed");
                        let struct_alloca = LLVMBuildAlloca(self.builder, struct_type, struct_name.as_ptr());
                        
                        // Store each field
                        for (field_name, field_expr) in fields {
                            // Find field index
                            let field_idx = field_names.iter().position(|f| f == field_name)
                                .ok_or_else(|| {
                                    CompilerError::codegen_error(format!("Field {field_name} not found in struct {name}"))
                                })? as u32;
                            
                            // Generate field value
                            let field_val = self.codegen_expression(field_expr)?;
                            
                            // Get pointer to field
                            let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                            let idx = LLVMConstInt(LLVMInt32TypeInContext(self.context), field_idx as u64, 0);
                            let mut indices = [zero, idx];
                            
                            let field_ptr_name = CString::new(format!("{field_name}.ptr")).expect("CString failed");
                            let field_ptr = LLVMBuildInBoundsGEP2(
                                self.builder,
                                struct_type,
                                struct_alloca,
                                indices.as_mut_ptr(),
                                2,
                                field_ptr_name.as_ptr(),
                            );
                            
                            // Store field value
                            LLVMBuildStore(self.builder, field_val, field_ptr);
                        }
                        
                        Ok(struct_alloca)
                    }
                }

                Expression::MemberAccess { object, member } => {
                    unsafe {
                        // Get the object (should be a struct pointer)
                        let obj_val = self.codegen_expression(object)?;
                        
                        // Get struct name from the object expression
                        let struct_name = if let Expression::Identifier(var_name) = &**object {
                            self.struct_variables.get(var_name).cloned()
                        } else {
                            None
                        }.ok_or_else(|| {
                            CompilerError::codegen_error("Member access only supported on struct variables")
                        })?;
                        
                        // Get struct type, field names, and field types
                        let (struct_type, field_names, field_types) = self.struct_types.get(&struct_name)
                            .cloned()
                            .ok_or_else(|| {
                                CompilerError::codegen_error(format!("Undefined struct: {struct_name}"))
                            })?;
                        
                        // Find field index
                        let field_idx = field_names.iter().position(|f| f == member)
                            .ok_or_else(|| {
                                CompilerError::codegen_error(format!("Field {member} not found in struct {struct_name}"))
                            })?;
                        
                        // Get pointer to field using GEP
                        let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                        let idx = LLVMConstInt(LLVMInt32TypeInContext(self.context), field_idx as u64, 0);
                        let mut indices = [zero, idx];
                        
                        let field_ptr_name = CString::new(format!("{member}.ptr")).expect("CString failed");
                        let field_ptr = LLVMBuildInBoundsGEP2(
                            self.builder,
                            struct_type,
                            obj_val,
                            indices.as_mut_ptr(),
                            2,
                            field_ptr_name.as_ptr(),
                        );
                        
                        // Load the field value using the stored field type
                        let field_type = field_types[field_idx];
                        let load_name = CString::new(format!("{member}.load")).expect("CString failed");
                        Ok(LLVMBuildLoad2(self.builder, field_type, field_ptr, load_name.as_ptr()))
                    }
                }

                Expression::Assignment { target, value } => {
                    // Get the target variable (must be an identifier for now)
                    if let Expression::Identifier(var_name) = &**target {
                        let alloca = self.named_values
                            .get(var_name)
                            .copied()
                            .ok_or_else(|| {
                                CompilerError::type_error(
                                    SourceLocation::new(self.file_path.clone(), 0, 0),
                                    format!("Undefined variable: {var_name}"),
                                )
                            })?;

                        // Generate the value to assign
                        let val = self.codegen_expression(value)?;

                        // Store the value
                        LLVMBuildStore(self.builder, val, alloca);

                        // Return the value (for chained assignments)
                        Ok(val)
                    } else {
                        Err(CompilerError::codegen_error("Assignment target must be a variable"))
                    }
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
                Type::Custom(name) => {
                    // Look up struct type
                    if let Some((struct_type, _, _)) = self.struct_types.get(name) {
                        *struct_type
                    } else {
                        // Unknown type, use i8* as fallback
                        LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                    }
                }
                Type::Generic { .. } => {
                    LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
                }
            }
        }
    }

    /// Create an alloca instruction in the entry block of the function.
    /// This ensures all allocas are at the start for better optimization.
    fn create_entry_block_alloca(
        &self,
        var_type: LLVMTypeRef,
        var_name: &str,
    ) -> CompilerResult<LLVMValueRef> {
        unsafe {
            let function = self.current_function.ok_or_else(|| {
                CompilerError::codegen_error("No current function for alloca")
            })?;

            // Save current position
            let current_block = LLVMGetInsertBlock(self.builder);

            // Get entry block
            let entry_block = LLVMGetEntryBasicBlock(function);

            // Position at the start of entry block
            let first_instruction = LLVMGetFirstInstruction(entry_block);
            if !first_instruction.is_null() {
                LLVMPositionBuilderBefore(self.builder, first_instruction);
            } else {
                LLVMPositionBuilderAtEnd(self.builder, entry_block);
            }

            // Create alloca
            let var_name_cstr = CString::new(var_name).expect("CString failed");
            let alloca = LLVMBuildAlloca(self.builder, var_type, var_name_cstr.as_ptr());

            // Restore position
            if !current_block.is_null() {
                LLVMPositionBuilderAtEnd(self.builder, current_block);
            }

            Ok(alloca)
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
