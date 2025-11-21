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

            // String functions from libc
            let int_type = LLVMInt64TypeInContext(self.context); // Use i64 to match Kraken's int type

            // strlen: int strlen(const char* s)
            let strlen_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let strlen_name = CString::new("strlen").expect("CString failed");
            let strlen_func = LLVMAddFunction(self.module, strlen_name.as_ptr(), strlen_type);
            self.functions.insert("strlen".to_string(), strlen_func);

            // strcmp: int strcmp(const char* s1, const char* s2)
            let strcmp_type = LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strcmp_name = CString::new("strcmp").expect("CString failed");
            let strcmp_func = LLVMAddFunction(self.module, strcmp_name.as_ptr(), strcmp_type);
            self.functions.insert("strcmp".to_string(), strcmp_func);

            // strcpy: char* strcpy(char* dest, const char* src)
            let strcpy_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strcpy_name = CString::new("strcpy").expect("CString failed");
            let strcpy_func = LLVMAddFunction(self.module, strcpy_name.as_ptr(), strcpy_type);
            self.functions.insert("strcpy".to_string(), strcpy_func);

            // strcat: char* strcat(char* dest, const char* src)
            let strcat_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strcat_name = CString::new("strcat").expect("CString failed");
            let strcat_func = LLVMAddFunction(self.module, strcat_name.as_ptr(), strcat_type);
            self.functions.insert("strcat".to_string(), strcat_func);

            // strstr: char* strstr(const char* haystack, const char* needle)
            let strstr_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strstr_name = CString::new("strstr").expect("CString failed");
            let strstr_func = LLVMAddFunction(self.module, strstr_name.as_ptr(), strstr_type);
            self.functions.insert("strstr".to_string(), strstr_func);

            // strchr: char* strchr(const char* s, int c)
            let strchr_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, int_type].as_mut_ptr(), 2, 0);
            let strchr_name = CString::new("strchr").expect("CString failed");
            let strchr_func = LLVMAddFunction(self.module, strchr_name.as_ptr(), strchr_type);
            self.functions.insert("strchr".to_string(), strchr_func);

            // strncpy: char* strncpy(char* dest, const char* src, int n)
            let strncpy_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type, int_type].as_mut_ptr(), 3, 0);
            let strncpy_name = CString::new("strncpy").expect("CString failed");
            let strncpy_func = LLVMAddFunction(self.module, strncpy_name.as_ptr(), strncpy_type);
            self.functions.insert("strncpy".to_string(), strncpy_func);

            // strncmp: int strncmp(const char* s1, const char* s2, int n)
            let strncmp_type = LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type, int_type].as_mut_ptr(), 3, 0);
            let strncmp_name = CString::new("strncmp").expect("CString failed");
            let strncmp_func = LLVMAddFunction(self.module, strncmp_name.as_ptr(), strncmp_type);
            self.functions.insert("strncmp".to_string(), strncmp_func);

            // Memory functions
            // malloc: void* malloc(int size)
            let void_ptr_type = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let malloc_type = LLVMFunctionType(void_ptr_type, [int_type].as_mut_ptr(), 1, 0);
            let malloc_name = CString::new("malloc").expect("CString failed");
            let malloc_func = LLVMAddFunction(self.module, malloc_name.as_ptr(), malloc_type);
            self.functions.insert("malloc".to_string(), malloc_func);

            // free: void free(void* ptr)
            let void_type = LLVMVoidTypeInContext(self.context);
            let free_type = LLVMFunctionType(void_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let free_name = CString::new("free").expect("CString failed");
            let free_func = LLVMAddFunction(self.module, free_name.as_ptr(), free_type);
            self.functions.insert("free".to_string(), free_func);

            // realloc: void* realloc(void* ptr, int size)
            let realloc_type = LLVMFunctionType(void_ptr_type, [void_ptr_type, int_type].as_mut_ptr(), 2, 0);
            let realloc_name = CString::new("realloc").expect("CString failed");
            let realloc_func = LLVMAddFunction(self.module, realloc_name.as_ptr(), realloc_type);
            self.functions.insert("realloc".to_string(), realloc_func);

            // memcpy: void* memcpy(void* dest, const void* src, int n)
            let memcpy_type = LLVMFunctionType(void_ptr_type, [void_ptr_type, void_ptr_type, int_type].as_mut_ptr(), 3, 0);
            let memcpy_name = CString::new("memcpy").expect("CString failed");
            let memcpy_func = LLVMAddFunction(self.module, memcpy_name.as_ptr(), memcpy_type);
            self.functions.insert("memcpy".to_string(), memcpy_func);

            // Math functions from libm
            let float_type = LLVMDoubleTypeInContext(self.context);
            
            // sqrt: double sqrt(double x)
            let sqrt_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let sqrt_name = CString::new("sqrt").expect("CString failed");
            let sqrt_func = LLVMAddFunction(self.module, sqrt_name.as_ptr(), sqrt_type);
            self.functions.insert("sqrt".to_string(), sqrt_func);
            
            // pow: double pow(double x, double y)
            let pow_type = LLVMFunctionType(float_type, [float_type, float_type].as_mut_ptr(), 2, 0);
            let pow_name = CString::new("pow").expect("CString failed");
            let pow_func = LLVMAddFunction(self.module, pow_name.as_ptr(), pow_type);
            self.functions.insert("pow".to_string(), pow_func);
            
            // abs: int abs(int x)
            let abs_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let abs_name = CString::new("abs").expect("CString failed");
            let abs_func = LLVMAddFunction(self.module, abs_name.as_ptr(), abs_type);
            self.functions.insert("abs".to_string(), abs_func);
            
            // fabs: double fabs(double x)
            let fabs_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let fabs_name = CString::new("fabs").expect("CString failed");
            let fabs_func = LLVMAddFunction(self.module, fabs_name.as_ptr(), fabs_type);
            self.functions.insert("fabs".to_string(), fabs_func);
            
            // floor: double floor(double x)
            let floor_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let floor_name = CString::new("floor").expect("CString failed");
            let floor_func = LLVMAddFunction(self.module, floor_name.as_ptr(), floor_type);
            self.functions.insert("floor".to_string(), floor_func);
            
            // ceil: double ceil(double x)
            let ceil_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let ceil_name = CString::new("ceil").expect("CString failed");
            let ceil_func = LLVMAddFunction(self.module, ceil_name.as_ptr(), ceil_type);
            self.functions.insert("ceil".to_string(), ceil_func);
            
            // round: double round(double x)
            let round_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let round_name = CString::new("round").expect("CString failed");
            let round_func = LLVMAddFunction(self.module, round_name.as_ptr(), round_type);
            self.functions.insert("round".to_string(), round_func);
            
            // sin: double sin(double x)
            let sin_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let sin_name = CString::new("sin").expect("CString failed");
            let sin_func = LLVMAddFunction(self.module, sin_name.as_ptr(), sin_type);
            self.functions.insert("sin".to_string(), sin_func);
            
            // cos: double cos(double x)
            let cos_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let cos_name = CString::new("cos").expect("CString failed");
            let cos_func = LLVMAddFunction(self.module, cos_name.as_ptr(), cos_type);
            self.functions.insert("cos".to_string(), cos_func);
            
            // tan: double tan(double x)
            let tan_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let tan_name = CString::new("tan").expect("CString failed");
            let tan_func = LLVMAddFunction(self.module, tan_name.as_ptr(), tan_type);
            self.functions.insert("tan".to_string(), tan_func);
            
            // log: double log(double x)
            let log_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let log_name = CString::new("log").expect("CString failed");
            let log_func = LLVMAddFunction(self.module, log_name.as_ptr(), log_type);
            self.functions.insert("log".to_string(), log_func);
            
            // log10: double log10(double x)
            let log10_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let log10_name = CString::new("log10").expect("CString failed");
            let log10_func = LLVMAddFunction(self.module, log10_name.as_ptr(), log10_type);
            self.functions.insert("log10".to_string(), log10_func);
            
            // exp: double exp(double x)
            let exp_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let exp_name = CString::new("exp").expect("CString failed");
            let exp_func = LLVMAddFunction(self.module, exp_name.as_ptr(), exp_type);
            self.functions.insert("exp".to_string(), exp_func);
            
            // Random number functions
            // rand: int rand()
            let rand_type = LLVMFunctionType(int_type, [].as_mut_ptr(), 0, 0);
            let rand_name = CString::new("rand").expect("CString failed");
            let rand_func = LLVMAddFunction(self.module, rand_name.as_ptr(), rand_type);
            self.functions.insert("rand".to_string(), rand_func);
            
            // srand: void srand(unsigned int seed)
            let srand_type = LLVMFunctionType(void_type, [int_type].as_mut_ptr(), 1, 0);
            let srand_name = CString::new("srand").expect("CString failed");
            let srand_func = LLVMAddFunction(self.module, srand_name.as_ptr(), srand_type);
            self.functions.insert("srand".to_string(), srand_func);
            
            // Time functions
            // time: int time(void* tloc)
            let time_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let time_name = CString::new("time").expect("CString failed");
            let time_func = LLVMAddFunction(self.module, time_name.as_ptr(), time_type);
            self.functions.insert("time".to_string(), time_func);

            // File I/O functions
            // FILE* is represented as void* (i8*)
            
            // fopen: FILE* fopen(const char* filename, const char* mode)
            let fopen_type = LLVMFunctionType(void_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let fopen_name = CString::new("fopen").expect("CString failed");
            let fopen_func = LLVMAddFunction(self.module, fopen_name.as_ptr(), fopen_type);
            self.functions.insert("fopen".to_string(), fopen_func);
            
            // fclose: int fclose(FILE* stream)
            let fclose_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let fclose_name = CString::new("fclose").expect("CString failed");
            let fclose_func = LLVMAddFunction(self.module, fclose_name.as_ptr(), fclose_type);
            self.functions.insert("fclose".to_string(), fclose_func);
            
            // fread: int fread(void* ptr, int size, int count, FILE* stream)
            let fread_type = LLVMFunctionType(int_type, [void_ptr_type, int_type, int_type, void_ptr_type].as_mut_ptr(), 4, 0);
            let fread_name = CString::new("fread").expect("CString failed");
            let fread_func = LLVMAddFunction(self.module, fread_name.as_ptr(), fread_type);
            self.functions.insert("fread".to_string(), fread_func);
            
            // fwrite: int fwrite(const void* ptr, int size, int count, FILE* stream)
            let fwrite_type = LLVMFunctionType(int_type, [void_ptr_type, int_type, int_type, void_ptr_type].as_mut_ptr(), 4, 0);
            let fwrite_name = CString::new("fwrite").expect("CString failed");
            let fwrite_func = LLVMAddFunction(self.module, fwrite_name.as_ptr(), fwrite_type);
            self.functions.insert("fwrite".to_string(), fwrite_func);
            
            // fgets: char* fgets(char* str, int n, FILE* stream)
            let fgets_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, int_type, void_ptr_type].as_mut_ptr(), 3, 0);
            let fgets_name = CString::new("fgets").expect("CString failed");
            let fgets_func = LLVMAddFunction(self.module, fgets_name.as_ptr(), fgets_type);
            self.functions.insert("fgets".to_string(), fgets_func);
            
            // fputs: int fputs(const char* str, FILE* stream)
            let fputs_type = LLVMFunctionType(int_type, [i8_ptr_type, void_ptr_type].as_mut_ptr(), 2, 0);
            let fputs_name = CString::new("fputs").expect("CString failed");
            let fputs_func = LLVMAddFunction(self.module, fputs_name.as_ptr(), fputs_type);
            self.functions.insert("fputs".to_string(), fputs_func);
            
            // fgetc: int fgetc(FILE* stream)
            let fgetc_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let fgetc_name = CString::new("fgetc").expect("CString failed");
            let fgetc_func = LLVMAddFunction(self.module, fgetc_name.as_ptr(), fgetc_type);
            self.functions.insert("fgetc".to_string(), fgetc_func);
            
            // fputc: int fputc(int c, FILE* stream)
            let fputc_type = LLVMFunctionType(int_type, [int_type, void_ptr_type].as_mut_ptr(), 2, 0);
            let fputc_name = CString::new("fputc").expect("CString failed");
            let fputc_func = LLVMAddFunction(self.module, fputc_name.as_ptr(), fputc_type);
            self.functions.insert("fputc".to_string(), fputc_func);
            
            // fseek: int fseek(FILE* stream, int offset, int whence)
            let fseek_type = LLVMFunctionType(int_type, [void_ptr_type, int_type, int_type].as_mut_ptr(), 3, 0);
            let fseek_name = CString::new("fseek").expect("CString failed");
            let fseek_func = LLVMAddFunction(self.module, fseek_name.as_ptr(), fseek_type);
            self.functions.insert("fseek".to_string(), fseek_func);
            
            // ftell: int ftell(FILE* stream)
            let ftell_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let ftell_name = CString::new("ftell").expect("CString failed");
            let ftell_func = LLVMAddFunction(self.module, ftell_name.as_ptr(), ftell_type);
            self.functions.insert("ftell".to_string(), ftell_func);
            
            // rewind: void rewind(FILE* stream)
            let rewind_type = LLVMFunctionType(void_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let rewind_name = CString::new("rewind").expect("CString failed");
            let rewind_func = LLVMAddFunction(self.module, rewind_name.as_ptr(), rewind_type);
            self.functions.insert("rewind".to_string(), rewind_func);
            
            // fflush: int fflush(FILE* stream)
            let fflush_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let fflush_name = CString::new("fflush").expect("CString failed");
            let fflush_func = LLVMAddFunction(self.module, fflush_name.as_ptr(), fflush_type);
            self.functions.insert("fflush".to_string(), fflush_func);
            
            // feof: int feof(FILE* stream)
            let feof_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let feof_name = CString::new("feof").expect("CString failed");
            let feof_func = LLVMAddFunction(self.module, feof_name.as_ptr(), feof_type);
            self.functions.insert("feof".to_string(), feof_func);
            
            // ferror: int ferror(FILE* stream)
            let ferror_type = LLVMFunctionType(int_type, [void_ptr_type].as_mut_ptr(), 1, 0);
            let ferror_name = CString::new("ferror").expect("CString failed");
            let ferror_func = LLVMAddFunction(self.module, ferror_name.as_ptr(), ferror_type);
            self.functions.insert("ferror".to_string(), ferror_func);
            
            // remove: int remove(const char* filename)
            let remove_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let remove_name = CString::new("remove").expect("CString failed");
            let remove_func = LLVMAddFunction(self.module, remove_name.as_ptr(), remove_type);
            self.functions.insert("remove".to_string(), remove_func);
            
            // rename: int rename(const char* old, const char* new)
            let rename_type = LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let rename_name = CString::new("rename").expect("CString failed");
            let rename_func = LLVMAddFunction(self.module, rename_name.as_ptr(), rename_type);
            self.functions.insert("rename".to_string(), rename_func);

            // System & Process functions
            // exit: void exit(int status)
            let exit_type = LLVMFunctionType(void_type, [int_type].as_mut_ptr(), 1, 0);
            let exit_name = CString::new("exit").expect("CString failed");
            let exit_func = LLVMAddFunction(self.module, exit_name.as_ptr(), exit_type);
            self.functions.insert("exit".to_string(), exit_func);
            
            // system: int system(const char* command)
            let system_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let system_name = CString::new("system").expect("CString failed");
            let system_func = LLVMAddFunction(self.module, system_name.as_ptr(), system_type);
            self.functions.insert("system".to_string(), system_func);
            
            // getenv: char* getenv(const char* name)
            let getenv_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let getenv_name = CString::new("getenv").expect("CString failed");
            let getenv_func = LLVMAddFunction(self.module, getenv_name.as_ptr(), getenv_type);
            self.functions.insert("getenv".to_string(), getenv_func);
            
            // setenv: int setenv(const char* name, const char* value, int overwrite)
            let setenv_type = LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type, int_type].as_mut_ptr(), 3, 0);
            let setenv_name = CString::new("setenv").expect("CString failed");
            let setenv_func = LLVMAddFunction(self.module, setenv_name.as_ptr(), setenv_type);
            self.functions.insert("setenv".to_string(), setenv_func);
            
            // unsetenv: int unsetenv(const char* name)
            let unsetenv_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let unsetenv_name = CString::new("unsetenv").expect("CString failed");
            let unsetenv_func = LLVMAddFunction(self.module, unsetenv_name.as_ptr(), unsetenv_type);
            self.functions.insert("unsetenv".to_string(), unsetenv_func);
            
            // Additional string conversion functions
            // atoi: int atoi(const char* str)
            let atoi_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let atoi_name = CString::new("atoi").expect("CString failed");
            let atoi_func = LLVMAddFunction(self.module, atoi_name.as_ptr(), atoi_type);
            self.functions.insert("atoi".to_string(), atoi_func);
            
            // atof: double atof(const char* str)
            let atof_type = LLVMFunctionType(float_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let atof_name = CString::new("atof").expect("CString failed");
            let atof_func = LLVMAddFunction(self.module, atof_name.as_ptr(), atof_type);
            self.functions.insert("atof".to_string(), atof_func);
            
            // More advanced math
            // asin: double asin(double x)
            let asin_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let asin_name = CString::new("asin").expect("CString failed");
            let asin_func = LLVMAddFunction(self.module, asin_name.as_ptr(), asin_type);
            self.functions.insert("asin".to_string(), asin_func);
            
            // acos: double acos(double x)
            let acos_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let acos_name = CString::new("acos").expect("CString failed");
            let acos_func = LLVMAddFunction(self.module, acos_name.as_ptr(), acos_type);
            self.functions.insert("acos".to_string(), acos_func);
            
            // atan: double atan(double x)
            let atan_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let atan_name = CString::new("atan").expect("CString failed");
            let atan_func = LLVMAddFunction(self.module, atan_name.as_ptr(), atan_type);
            self.functions.insert("atan".to_string(), atan_func);
            
            // atan2: double atan2(double y, double x)
            let atan2_type = LLVMFunctionType(float_type, [float_type, float_type].as_mut_ptr(), 2, 0);
            let atan2_name = CString::new("atan2").expect("CString failed");
            let atan2_func = LLVMAddFunction(self.module, atan2_name.as_ptr(), atan2_type);
            self.functions.insert("atan2".to_string(), atan2_func);
            
            // sinh: double sinh(double x)
            let sinh_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let sinh_name = CString::new("sinh").expect("CString failed");
            let sinh_func = LLVMAddFunction(self.module, sinh_name.as_ptr(), sinh_type);
            self.functions.insert("sinh".to_string(), sinh_func);
            
            // cosh: double cosh(double x)
            let cosh_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let cosh_name = CString::new("cosh").expect("CString failed");
            let cosh_func = LLVMAddFunction(self.module, cosh_name.as_ptr(), cosh_type);
            self.functions.insert("cosh".to_string(), cosh_func);
            
            // tanh: double tanh(double x)
            let tanh_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let tanh_name = CString::new("tanh").expect("CString failed");
            let tanh_func = LLVMAddFunction(self.module, tanh_name.as_ptr(), tanh_type);
            self.functions.insert("tanh".to_string(), tanh_func);
            
            // fmod: double fmod(double x, double y)
            let fmod_type = LLVMFunctionType(float_type, [float_type, float_type].as_mut_ptr(), 2, 0);
            let fmod_name = CString::new("fmod").expect("CString failed");
            let fmod_func = LLVMAddFunction(self.module, fmod_name.as_ptr(), fmod_type);
            self.functions.insert("fmod".to_string(), fmod_func);
            
            // Sleep function (platform-specific, using usleep for microseconds)
            // usleep: int usleep(int usec)
            let usleep_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let usleep_name = CString::new("usleep").expect("CString failed");
            let usleep_func = LLVMAddFunction(self.module, usleep_name.as_ptr(), usleep_type);
            self.functions.insert("usleep".to_string(), usleep_func);

            // Character classification functions (ctype.h)
            // isalpha: int isalpha(int c)
            let isalpha_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let isalpha_name = CString::new("isalpha").expect("CString failed");
            let isalpha_func = LLVMAddFunction(self.module, isalpha_name.as_ptr(), isalpha_type);
            self.functions.insert("isalpha".to_string(), isalpha_func);
            
            // isdigit: int isdigit(int c)
            let isdigit_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let isdigit_name = CString::new("isdigit").expect("CString failed");
            let isdigit_func = LLVMAddFunction(self.module, isdigit_name.as_ptr(), isdigit_type);
            self.functions.insert("isdigit".to_string(), isdigit_func);
            
            // isalnum: int isalnum(int c)
            let isalnum_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let isalnum_name = CString::new("isalnum").expect("CString failed");
            let isalnum_func = LLVMAddFunction(self.module, isalnum_name.as_ptr(), isalnum_type);
            self.functions.insert("isalnum".to_string(), isalnum_func);
            
            // isspace: int isspace(int c)
            let isspace_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let isspace_name = CString::new("isspace").expect("CString failed");
            let isspace_func = LLVMAddFunction(self.module, isspace_name.as_ptr(), isspace_type);
            self.functions.insert("isspace".to_string(), isspace_func);
            
            // isupper: int isupper(int c)
            let isupper_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let isupper_name = CString::new("isupper").expect("CString failed");
            let isupper_func = LLVMAddFunction(self.module, isupper_name.as_ptr(), isupper_type);
            self.functions.insert("isupper".to_string(), isupper_func);
            
            // islower: int islower(int c)
            let islower_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let islower_name = CString::new("islower").expect("CString failed");
            let islower_func = LLVMAddFunction(self.module, islower_name.as_ptr(), islower_type);
            self.functions.insert("islower".to_string(), islower_func);
            
            // toupper: int toupper(int c)
            let toupper_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let toupper_name = CString::new("toupper").expect("CString failed");
            let toupper_func = LLVMAddFunction(self.module, toupper_name.as_ptr(), toupper_type);
            self.functions.insert("toupper".to_string(), toupper_func);
            
            // tolower: int tolower(int c)
            let tolower_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let tolower_name = CString::new("tolower").expect("CString failed");
            let tolower_func = LLVMAddFunction(self.module, tolower_name.as_ptr(), tolower_type);
            self.functions.insert("tolower".to_string(), tolower_func);
            
            // Additional string utilities
            // strdup: char* strdup(const char* s)
            let strdup_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let strdup_name = CString::new("strdup").expect("CString failed");
            let strdup_func = LLVMAddFunction(self.module, strdup_name.as_ptr(), strdup_type);
            self.functions.insert("strdup".to_string(), strdup_func);
            
            // strtok: char* strtok(char* str, const char* delim)
            let strtok_type = LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strtok_name = CString::new("strtok").expect("CString failed");
            let strtok_func = LLVMAddFunction(self.module, strtok_name.as_ptr(), strtok_type);
            self.functions.insert("strtok".to_string(), strtok_func);
            
            // memset: void* memset(void* ptr, int value, int num)
            let memset_type = LLVMFunctionType(void_ptr_type, [void_ptr_type, int_type, int_type].as_mut_ptr(), 3, 0);
            let memset_name = CString::new("memset").expect("CString failed");
            let memset_func = LLVMAddFunction(self.module, memset_name.as_ptr(), memset_type);
            self.functions.insert("memset".to_string(), memset_func);
            
            // memcmp: int memcmp(const void* ptr1, const void* ptr2, int num)
            let memcmp_type = LLVMFunctionType(int_type, [void_ptr_type, void_ptr_type, int_type].as_mut_ptr(), 3, 0);
            let memcmp_name = CString::new("memcmp").expect("CString failed");
            let memcmp_func = LLVMAddFunction(self.module, memcmp_name.as_ptr(), memcmp_type);
            self.functions.insert("memcmp".to_string(), memcmp_func);
            
            // Assertion and error handling
            // abort: void abort()
            let abort_type = LLVMFunctionType(void_type, [].as_mut_ptr(), 0, 0);
            let abort_name = CString::new("abort").expect("CString failed");
            let abort_func = LLVMAddFunction(self.module, abort_name.as_ptr(), abort_type);
            self.functions.insert("abort".to_string(), abort_func);
            
            // Additional I/O
            // putchar: int putchar(int c)
            let putchar_type = LLVMFunctionType(int_type, [int_type].as_mut_ptr(), 1, 0);
            let putchar_name = CString::new("putchar").expect("CString failed");
            let putchar_func = LLVMAddFunction(self.module, putchar_name.as_ptr(), putchar_type);
            self.functions.insert("putchar".to_string(), putchar_func);
            
            // getchar: int getchar()
            let getchar_type = LLVMFunctionType(int_type, [].as_mut_ptr(), 0, 0);
            let getchar_name = CString::new("getchar").expect("CString failed");
            let getchar_func = LLVMAddFunction(self.module, getchar_name.as_ptr(), getchar_type);
            self.functions.insert("getchar".to_string(), getchar_func);
            
            // sprintf: int sprintf(char* str, const char* format, ...)
            let sprintf_type = LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 1);
            let sprintf_name = CString::new("sprintf").expect("CString failed");
            let sprintf_func = LLVMAddFunction(self.module, sprintf_name.as_ptr(), sprintf_type);
            self.functions.insert("sprintf".to_string(), sprintf_func);
            
            // sscanf: int sscanf(const char* str, const char* format, ...)
            let sscanf_type = LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 1);
            let sscanf_name = CString::new("sscanf").expect("CString failed");
            let sscanf_func = LLVMAddFunction(self.module, sscanf_name.as_ptr(), sscanf_type);
            self.functions.insert("sscanf".to_string(), sscanf_func);

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
