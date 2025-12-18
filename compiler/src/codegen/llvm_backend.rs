use crate::error::{CompilerError, CompilerResult, SourceLocation};
use crate::ffi::stdlib::{AbiType, CIntWidening, StdlibFnSig};
use crate::lexer::token::Operator;
use crate::parser::ast::*;
use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::LLVMIntPredicate;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::path::Path;
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
    enum_types: HashMap<String, Vec<(String, u32)>>, // enum name -> [(variant_name, tag)]
    functions: HashMap<String, LLVMValueRef>,
    current_function: Option<LLVMValueRef>,
    loop_exit_blocks: Vec<LLVMBasicBlockRef>,
    loop_continue_blocks: Vec<LLVMBasicBlockRef>,
    file_path: PathBuf,
    debug_bounds_checks: bool, // Enable bounds checking when KRAKEN_DEBUG_BOUNDS=1
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
            let module_name_cstr =
                CString::new(module_name.as_str()).expect("CString conversion failed");
            let module = LLVMModuleCreateWithNameInContext(module_name_cstr.as_ptr(), context);
            let builder = LLVMCreateBuilderInContext(context);

            let debug_bounds_checks = std::env::var("KRAKEN_DEBUG_BOUNDS")
                .map(|v| v == "1")
                .unwrap_or(false);

            Self {
                context,
                module,
                builder,
                named_values: HashMap::new(),
                array_variables: HashMap::new(),
                struct_variables: HashMap::new(),
                struct_types: HashMap::new(),
                enum_types: HashMap::new(),
                functions: HashMap::new(),
                current_function: None,
                loop_exit_blocks: Vec::new(),
                loop_continue_blocks: Vec::new(),
                file_path,
                debug_bounds_checks,
            }
        }
    }

    fn maybe_trap_on_null_stdlib_result(
        &mut self,
        name: &str,
        value: LLVMValueRef,
    ) -> CompilerResult<LLVMValueRef> {
        let Some(sig) = self.stdlib_sig(name) else {
            return Ok(value);
        };

        let should_trap = sig.c_abi_return == AbiType::I8Ptr
            && sig.c_abi_return_nullability == crate::ffi::types::Nullability::Nullable
            && sig.c_abi_return_ownership == crate::ffi::types::Ownership::Owned
            && sig.errno == crate::ffi::types::ErrnoConvention::ReturnsNull;

        if !should_trap {
            return Ok(value);
        }

        unsafe {
            let current_bb = LLVMGetInsertBlock(self.builder);
            let current_fn = LLVMGetBasicBlockParent(current_bb);

            let trap_bb =
                LLVMAppendBasicBlockInContext(self.context, current_fn, c"stdlib_null".as_ptr());
            let cont_bb =
                LLVMAppendBasicBlockInContext(self.context, current_fn, c"stdlib_ok".as_ptr());

            let is_null = LLVMBuildICmp(
                self.builder,
                LLVMIntPredicate::LLVMIntEQ,
                value,
                LLVMConstNull(LLVMTypeOf(value)),
                c"isnull".as_ptr(),
            );

            LLVMBuildCondBr(self.builder, is_null, trap_bb, cont_bb);

            LLVMPositionBuilderAtEnd(self.builder, trap_bb);
            let abort = *self
                .functions
                .get("abort")
                .ok_or_else(|| CompilerError::codegen_error("Missing stdlib function: abort"))?;
            let abort_ty = LLVMGlobalGetValueType(abort);
            LLVMBuildCall2(
                self.builder,
                abort_ty,
                abort,
                [].as_mut_ptr(),
                0,
                c"".as_ptr(),
            );
            LLVMBuildUnreachable(self.builder);

            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
            Ok(value)
        }
    }

    fn stdlib_sig(&self, name: &str) -> Option<&'static StdlibFnSig> {
        crate::ffi::stdlib::stdlib_sig(name)
    }

    fn abi_type_to_llvm(&self, ty: AbiType) -> LLVMTypeRef {
        unsafe {
            match ty {
                AbiType::Void => LLVMVoidTypeInContext(self.context),
                AbiType::I32 => LLVMInt32TypeInContext(self.context),
                AbiType::I64 => LLVMInt64TypeInContext(self.context),
                AbiType::I8Ptr => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
            }
        }
    }

    fn coerce_stdlib_call_args(
        &mut self,
        name: &str,
        args: &mut [LLVMValueRef],
    ) -> CompilerResult<()> {
        let Some(sig) = self.stdlib_sig(name) else {
            return Ok(());
        };

        if sig.c_abi_params.len() > args.len() {
            return Err(CompilerError::codegen_error(format!(
                "Stdlib call {name} missing args: expected >= {}, got {}",
                sig.c_abi_params.len(),
                args.len()
            )));
        }

        unsafe {
            for (idx, abi_ty) in sig.c_abi_params.iter().copied().enumerate() {
                let expected = self.abi_type_to_llvm(abi_ty);
                let actual = LLVMTypeOf(args[idx]);
                if actual == expected {
                    continue;
                }

                let cast_name = CString::new(format!("arg_cast_{idx}")).expect("CString failed");
                args[idx] = match abi_ty {
                    AbiType::I32 => {
                        LLVMBuildTrunc(self.builder, args[idx], expected, cast_name.as_ptr())
                    }
                    AbiType::I64 => {
                        // Currently Kraken int is i64; accept i32 by widening if it appears.
                        let i32_ty = LLVMInt32TypeInContext(self.context);
                        if actual == i32_ty {
                            LLVMBuildSExt(self.builder, args[idx], expected, cast_name.as_ptr())
                        } else {
                            LLVMBuildIntCast2(
                                self.builder,
                                args[idx],
                                expected,
                                1,
                                cast_name.as_ptr(),
                            )
                        }
                    }
                    AbiType::I8Ptr => {
                        LLVMBuildBitCast(self.builder, args[idx], expected, cast_name.as_ptr())
                    }
                    AbiType::Void => args[idx],
                };
            }
        }

        Ok(())
    }

    fn maybe_widen_stdlib_result(&mut self, name: &str, value: LLVMValueRef) -> LLVMValueRef {
        let Some(sig) = self.stdlib_sig(name) else {
            return value;
        };

        let Some(widening) = sig.c_int_widening else {
            return value;
        };

        if sig.kraken_return != Type::Int {
            return value;
        }

        unsafe {
            let value_ty = LLVMTypeOf(value);
            let i32_ty = LLVMInt32TypeInContext(self.context);
            if value_ty != i32_ty {
                return value;
            }

            let i64_ty = LLVMInt64TypeInContext(self.context);
            let cast_name = CString::new("cint_widen").expect("CString failed");
            match widening {
                CIntWidening::Signed => {
                    LLVMBuildSExt(self.builder, value, i64_ty, cast_name.as_ptr())
                }
                CIntWidening::Unsigned => {
                    LLVMBuildZExt(self.builder, value, i64_ty, cast_name.as_ptr())
                }
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
    pub fn compile(&mut self, program: &Program, output_path: &Path) -> CompilerResult<()> {
        unsafe {
            // Initialize LLVM targets
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmParsers();
            LLVM_InitializeAllAsmPrinters();

            // Declare standard library functions
            self.declare_stdlib_functions()?;

            for statement in &program.statements {
                if matches!(statement, Statement::StructDeclaration { .. }) {
                    self.codegen_statement(statement)?;
                }
            }

            // Two-pass compilation:
            // Pass 1: Declare all functions (so they can call each other)
            for statement in &program.statements {
                if let Statement::FunctionDeclaration {
                    name,
                    parameters,
                    return_type,
                    ..
                } = statement
                {
                    self.declare_function(
                        name,
                        parameters,
                        return_type.as_ref().unwrap_or(&Type::Void),
                    )?;
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
                return Err(CompilerError::codegen_error(
                    "Failed to create target machine",
                ));
            }

            // Emit object file
            let output_cstr =
                CString::new(output_path.to_str().expect("Invalid path")).expect("CString failed");

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
                self.codegen_function(
                    name,
                    parameters,
                    return_type.as_ref().unwrap_or(&Type::Void),
                    body,
                )?;
                Ok(())
            }

            Statement::StructDeclaration {
                name,
                fields,
                is_public: _,
            } => {
                unsafe {
                    if self.struct_types.contains_key(name) {
                        return Ok(());
                    }

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
                    self.struct_types
                        .insert(name.clone(), (struct_type, field_names, field_types));
                }
                Ok(())
            }

            Statement::EnumDeclaration {
                name,
                variants,
                is_public: _,
            } => {
                // Register enum variants with their tag values
                let variants_with_tags: Vec<(String, u32)> = variants
                    .iter()
                    .enumerate()
                    .map(|(i, (variant_name, _payload))| (variant_name.clone(), i as u32))
                    .collect();
                self.enum_types.insert(name.clone(), variants_with_tags);
                Ok(())
            }

            Statement::InterfaceDeclaration { .. } => {
                // Interface declarations don't generate code
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
                    let is_array = matches!(type_annotation, Some(Type::Array { .. }));

                    let struct_name = if let Some(Type::Custom(sname)) = type_annotation {
                        Some(sname.clone())
                    } else {
                        None
                    };

                    // For array/struct literals without type annotation, generate them first
                    let pregenerated_value = if type_annotation.is_none() {
                        if let Some(init_expr) = initializer {
                            if matches!(
                                init_expr,
                                Expression::StructLiteral { .. } | Expression::Array { .. }
                            ) {
                                if let Expression::StructLiteral { name: sname, .. } = init_expr {
                                    self.struct_variables.insert(name.clone(), sname.clone());
                                }
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
                    // Cache codegen result to avoid double evaluation of expressions with side effects
                    let (var_type, cached_init_val) = if let Some(ty) = type_annotation {
                        (self.get_llvm_type(ty), None)
                    } else if let Some(pregen) = pregenerated_value {
                        // Use the type from the pregenerated value
                        (LLVMGetAllocatedType(pregen), Some(pregen))
                    } else if let Some(init_expr) = initializer {
                        // Infer type from initializer (for non-array/struct)
                        if let Expression::StructLiteral { name: sname, .. } = init_expr {
                            // Track this as a struct variable
                            self.struct_variables.insert(name.clone(), sname.clone());
                            let (st, _, _) =
                                self.struct_types.get(sname).cloned().ok_or_else(|| {
                                    CompilerError::codegen_error(format!(
                                        "Undefined struct: {sname}"
                                    ))
                                })?;
                            (st, None)
                        } else {
                            // Evaluate once and cache the result
                            let init_val = self.codegen_expression(init_expr)?;
                            (LLVMTypeOf(init_val), Some(init_val))
                        }
                    } else {
                        return Err(CompilerError::codegen_error(
                            "Variable must have type annotation or initializer",
                        ));
                    };

                    // Create alloca at the entry block
                    let alloca = self.create_entry_block_alloca(var_type, name)?;

                    // Store initial value if provided
                    if let Some(init_expr) = initializer {
                        let init_val = if let Some(pregen) = pregenerated_value {
                            pregen
                        } else if let Some(cached) = cached_init_val {
                            cached
                        } else {
                            self.codegen_expression(init_expr)?
                        };

                        // Check if this is a struct literal or array - if so, we need to copy it
                        if matches!(
                            init_expr,
                            Expression::StructLiteral { .. } | Expression::Array { .. }
                        ) {
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
                    let then_has_terminator =
                        !LLVMGetBasicBlockTerminator(current_then_bb).is_null();
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
                    let else_has_terminator =
                        !LLVMGetBasicBlockTerminator(current_else_bb).is_null();
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

                    let function = self
                        .current_function
                        .ok_or_else(|| CompilerError::codegen_error("Match outside of function"))?;

                    // Create basic blocks for each arm and the merge block
                    let mut arm_blocks = Vec::new();
                    let mut next_check_blocks = Vec::new();

                    for _ in 0..arms.len() {
                        let arm_name = CString::new("match.arm").expect("CString failed");
                        let arm_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            arm_name.as_ptr(),
                        );
                        arm_blocks.push(arm_bb);

                        let next_name = CString::new("match.next").expect("CString failed");
                        let next_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            function,
                            next_name.as_ptr(),
                        );
                        next_check_blocks.push(next_bb);
                    }

                    let merge_name = CString::new("match.merge").expect("CString failed");
                    let merge_bb =
                        LLVMAppendBasicBlockInContext(self.context, function, merge_name.as_ptr());

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
                                LLVMBuildCondBr(
                                    self.builder,
                                    cond,
                                    arm_blocks[i],
                                    next_check_blocks[i],
                                );
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
                            Pattern::EnumVariant { enum_name, variant_name, .. } => {
                                // Look up the tag value for this variant
                                if let Some(variants) = self.enum_types.get(enum_name) {
                                    if let Some((_, tag)) = variants.iter().find(|(name, _)| name == variant_name) {
                                        // Compare match value (assumed to be tag) against expected tag
                                        let i64_ty = LLVMInt64TypeInContext(self.context);
                                        let expected_tag = LLVMConstInt(i64_ty, *tag as u64, 0);
                                        let cmp_name = CString::new(format!("enum.cmp.{}", variant_name)).expect("CString failed");
                                        let cond = LLVMBuildICmp(
                                            self.builder,
                                            LLVMIntPredicate::LLVMIntEQ,
                                            match_val,
                                            expected_tag,
                                            cmp_name.as_ptr(),
                                        );
                                        LLVMBuildCondBr(self.builder, cond, arm_blocks[i], next_check_blocks[i]);
                                    } else {
                                        // Variant not found, just branch (will error at runtime)
                                        LLVMBuildBr(self.builder, arm_blocks[i]);
                                    }
                                } else {
                                    // Enum not found, just branch
                                    LLVMBuildBr(self.builder, arm_blocks[i]);
                                }
                            }
                        }

                        // Generate arm body
                        LLVMPositionBuilderAtEnd(self.builder, arm_blocks[i]);
                        for stmt in &arm.body.statements {
                            self.codegen_statement(stmt)?;
                            // Check if we hit a terminator
                            if !LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(self.builder))
                                .is_null()
                            {
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
            let i8_ptr_type = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let void_ptr_type = i8_ptr_type;
            let void_type = LLVMVoidTypeInContext(self.context);

            for sig in crate::ffi::stdlib::stdlib_functions() {
                let mut params: Vec<LLVMTypeRef> = sig
                    .c_abi_params
                    .iter()
                    .copied()
                    .map(|t| self.abi_type_to_llvm(t))
                    .collect();

                let ret_type = self.abi_type_to_llvm(sig.c_abi_return);
                let fn_type = LLVMFunctionType(
                    ret_type,
                    params.as_mut_ptr(),
                    params.len() as u32,
                    if sig.is_vararg { 1 } else { 0 },
                );

                let fn_name = CString::new(sig.name).expect("CString failed");
                let func = LLVMAddFunction(self.module, fn_name.as_ptr(), fn_type);
                self.functions.insert(sig.name.to_string(), func);
            }

            // String functions from libc
            let int_type = LLVMInt64TypeInContext(self.context); // Use i64 to match Kraken's int type

            // strcpy: char* strcpy(char* dest, const char* src)
            let strcpy_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strcpy_name = CString::new("strcpy").expect("CString failed");
            let strcpy_func = LLVMAddFunction(self.module, strcpy_name.as_ptr(), strcpy_type);
            self.functions.insert("strcpy".to_string(), strcpy_func);

            // strcat: char* strcat(char* dest, const char* src)
            let strcat_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strcat_name = CString::new("strcat").expect("CString failed");
            let strcat_func = LLVMAddFunction(self.module, strcat_name.as_ptr(), strcat_type);
            self.functions.insert("strcat".to_string(), strcat_func);

            // strstr: char* strstr(const char* haystack, const char* needle)
            let strstr_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strstr_name = CString::new("strstr").expect("CString failed");
            let strstr_func = LLVMAddFunction(self.module, strstr_name.as_ptr(), strstr_type);
            self.functions.insert("strstr".to_string(), strstr_func);

            // strchr: char* strchr(const char* s, int c)
            let strchr_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, int_type].as_mut_ptr(), 2, 0);
            let strchr_name = CString::new("strchr").expect("CString failed");
            let strchr_func = LLVMAddFunction(self.module, strchr_name.as_ptr(), strchr_type);
            self.functions.insert("strchr".to_string(), strchr_func);

            // strncpy: char* strncpy(char* dest, const char* src, int n)
            let strncpy_type = LLVMFunctionType(
                i8_ptr_type,
                [i8_ptr_type, i8_ptr_type, int_type].as_mut_ptr(),
                3,
                0,
            );
            let strncpy_name = CString::new("strncpy").expect("CString failed");
            let strncpy_func = LLVMAddFunction(self.module, strncpy_name.as_ptr(), strncpy_type);
            self.functions.insert("strncpy".to_string(), strncpy_func);

            // strncmp: int strncmp(const char* s1, const char* s2, int n)
            let strncmp_type = LLVMFunctionType(
                int_type,
                [i8_ptr_type, i8_ptr_type, int_type].as_mut_ptr(),
                3,
                0,
            );
            let strncmp_name = CString::new("strncmp").expect("CString failed");
            let strncmp_func = LLVMAddFunction(self.module, strncmp_name.as_ptr(), strncmp_type);
            self.functions.insert("strncmp".to_string(), strncmp_func);

            // strcmp: int strcmp(const char* s1, const char* s2) - returns C int (i32)
            // Use LLVMGetNamedFunction first to avoid duplicate declaration
            let strcmp_name = CString::new("strcmp").expect("CString failed");
            let strcmp_func = LLVMGetNamedFunction(self.module, strcmp_name.as_ptr());
            let strcmp_func = if strcmp_func.is_null() {
                let i32_type = LLVMInt32TypeInContext(self.context);
                let strcmp_type =
                    LLVMFunctionType(i32_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
                LLVMAddFunction(self.module, strcmp_name.as_ptr(), strcmp_type)
            } else {
                strcmp_func
            };
            self.functions.insert("strcmp".to_string(), strcmp_func);

            // Memory functions
            // Core memory functions are declared via the shared stdlib table above.

            // Math functions from libm
            let float_type = LLVMDoubleTypeInContext(self.context);

            // sqrt: double sqrt(double x)
            let sqrt_type = LLVMFunctionType(float_type, [float_type].as_mut_ptr(), 1, 0);
            let sqrt_name = CString::new("sqrt").expect("CString failed");
            let sqrt_func = LLVMAddFunction(self.module, sqrt_name.as_ptr(), sqrt_type);
            self.functions.insert("sqrt".to_string(), sqrt_func);

            // pow: double pow(double x, double y)
            let pow_type =
                LLVMFunctionType(float_type, [float_type, float_type].as_mut_ptr(), 2, 0);
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
            let atan2_type =
                LLVMFunctionType(float_type, [float_type, float_type].as_mut_ptr(), 2, 0);
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
            let fmod_type =
                LLVMFunctionType(float_type, [float_type, float_type].as_mut_ptr(), 2, 0);
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
            let strtok_type =
                LLVMFunctionType(i8_ptr_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 0);
            let strtok_name = CString::new("strtok").expect("CString failed");
            let strtok_func = LLVMAddFunction(self.module, strtok_name.as_ptr(), strtok_type);
            self.functions.insert("strtok".to_string(), strtok_func);

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
            let sprintf_type =
                LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 1);
            let sprintf_name = CString::new("sprintf").expect("CString failed");
            let sprintf_func = LLVMAddFunction(self.module, sprintf_name.as_ptr(), sprintf_type);
            self.functions.insert("sprintf".to_string(), sprintf_func);

            // sscanf: int sscanf(const char* str, const char* format, ...)
            let sscanf_type =
                LLVMFunctionType(int_type, [i8_ptr_type, i8_ptr_type].as_mut_ptr(), 2, 1);
            let sscanf_name = CString::new("sscanf").expect("CString failed");
            let sscanf_func = LLVMAddFunction(self.module, sscanf_name.as_ptr(), sscanf_type);
            self.functions.insert("sscanf".to_string(), sscanf_func);

            // Process control
            // exit: void exit(int status)
            let i32_type = LLVMInt32TypeInContext(self.context);
            let exit_type = LLVMFunctionType(void_type, [i32_type].as_mut_ptr(), 1, 0);
            let exit_name = CString::new("exit").expect("CString failed");
            let exit_func = LLVMAddFunction(self.module, exit_name.as_ptr(), exit_type);
            self.functions.insert("exit".to_string(), exit_func);

            // Time functions
            // sleep: unsigned int sleep(unsigned int seconds)
            let sleep_type = LLVMFunctionType(i32_type, [i32_type].as_mut_ptr(), 1, 0);
            let sleep_name = CString::new("sleep").expect("CString failed");
            let sleep_func = LLVMAddFunction(self.module, sleep_name.as_ptr(), sleep_type);
            self.functions.insert("sleep".to_string(), sleep_func);

            // usleep: int usleep(useconds_t usec)
            let usleep_type = LLVMFunctionType(i32_type, [i32_type].as_mut_ptr(), 1, 0);
            let usleep_name = CString::new("usleep").expect("CString failed");
            let usleep_func = LLVMAddFunction(self.module, usleep_name.as_ptr(), usleep_type);
            self.functions.insert("usleep".to_string(), usleep_func);

            // time: time_t time(time_t *tloc)
            let time_type = LLVMFunctionType(int_type, [i8_ptr_type].as_mut_ptr(), 1, 0);
            let time_name = CString::new("time").expect("CString failed");
            let time_func = LLVMAddFunction(self.module, time_name.as_ptr(), time_type);
            self.functions.insert("time".to_string(), time_func);

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
            let entry_block =
                LLVMAppendBasicBlockInContext(self.context, function, entry_name.as_ptr());
            LLVMPositionBuilderAtEnd(self.builder, entry_block);

            // Add parameters to named values (allocate on stack for mutability)
            self.named_values.clear();
            self.array_variables.clear();
            self.struct_variables.clear();
            for (i, param) in parameters.iter().enumerate() {
                let param_val = LLVMGetParam(function, i as u32);
                let param_name = CString::new(param.name.as_str()).expect("CString failed");
                LLVMSetValueName2(param_val, param_name.as_ptr(), param.name.len());

                if let Type::Custom(struct_name) = &param.param_type {
                    self.struct_variables
                        .insert(param.name.clone(), struct_name.clone());
                }

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
                    let alloca = self.named_values.get(name).copied().ok_or_else(|| {
                        CompilerError::type_error(
                            SourceLocation::new(self.file_path.clone(), 0, 0),
                            format!("Undefined variable: {name}"),
                        )
                    })?;

                    // Check if this variable is an array or struct - if so, return pointer directly
                    if self.array_variables.get(name).copied().unwrap_or(false)
                        || self.struct_variables.contains_key(name)
                    {
                        return Ok(alloca);
                    }

                    // Load the value from the alloca (original working code)
                    let load_name = CString::new(format!("{name}.load")).expect("CString failed");
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        LLVMGetAllocatedType(alloca),
                        alloca,
                        load_name.as_ptr(),
                    ))
                }

                Expression::Binary {
                    left,
                    operator,
                    right,
                } => {
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
                        Operator::BitAnd => {
                            let name = CString::new("bitandtmp").expect("CString failed");
                            LLVMBuildAnd(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::BitOr => {
                            let name = CString::new("bitortmp").expect("CString failed");
                            LLVMBuildOr(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::BitXor => {
                            let name = CString::new("bitxortmp").expect("CString failed");
                            LLVMBuildXor(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::LeftShift => {
                            let name = CString::new("shltmp").expect("CString failed");
                            LLVMBuildShl(self.builder, lhs, rhs, name.as_ptr())
                        }
                        Operator::RightShift => {
                            let name = CString::new("shrtmp").expect("CString failed");
                            LLVMBuildAShr(self.builder, lhs, rhs, name.as_ptr())
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
                        if name == "cstr" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "cstr expects exactly 1 argument",
                                ));
                            }

                            let val = self.codegen_expression(&arguments[0])?;
                            let i8_ptr_type =
                                LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                            let cast_name = CString::new("cstr.cast").expect("CString failed");
                            return Ok(LLVMBuildBitCast(
                                self.builder,
                                val,
                                i8_ptr_type,
                                cast_name.as_ptr(),
                            ));
                        }

                        if name == "from_cstr" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "from_cstr expects exactly 1 argument",
                                ));
                            }

                            let val = self.codegen_expression(&arguments[0])?;
                            let i8_ptr_type =
                                LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                            let cast_name = CString::new("from_cstr.cast").expect("CString failed");
                            let val = LLVMBuildBitCast(
                                self.builder,
                                val,
                                i8_ptr_type,
                                cast_name.as_ptr(),
                            );

                            // Trap on null pointer.
                            let current_bb = LLVMGetInsertBlock(self.builder);
                            let current_fn = LLVMGetBasicBlockParent(current_bb);
                            let trap_bb = LLVMAppendBasicBlockInContext(
                                self.context,
                                current_fn,
                                c"from_cstr_null".as_ptr(),
                            );
                            let cont_bb = LLVMAppendBasicBlockInContext(
                                self.context,
                                current_fn,
                                c"from_cstr_ok".as_ptr(),
                            );

                            let is_null = LLVMBuildICmp(
                                self.builder,
                                LLVMIntPredicate::LLVMIntEQ,
                                val,
                                LLVMConstNull(i8_ptr_type),
                                c"isnull".as_ptr(),
                            );
                            LLVMBuildCondBr(self.builder, is_null, trap_bb, cont_bb);

                            LLVMPositionBuilderAtEnd(self.builder, trap_bb);
                            let abort = *self.functions.get("abort").ok_or_else(|| {
                                CompilerError::codegen_error("Missing stdlib function: abort")
                            })?;
                            let abort_ty = LLVMGlobalGetValueType(abort);
                            LLVMBuildCall2(
                                self.builder,
                                abort_ty,
                                abort,
                                [].as_mut_ptr(),
                                0,
                                c"".as_ptr(),
                            );
                            LLVMBuildUnreachable(self.builder);

                            LLVMPositionBuilderAtEnd(self.builder, cont_bb);
                            return Ok(val);
                        }

                        // Concurrency intrinsics
                        if name == "join" {
                            // For now, join is a no-op since spawn executes inline
                            // Full implementation will wait for the spawned task
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "join expects exactly 1 argument (handle)",
                                ));
                            }
                            // Evaluate the handle argument (for side effects)
                            let _handle = self.codegen_expression(&arguments[0])?;
                            // Return void (represented as undef)
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "join_all" {
                            // For now, join_all is a no-op since spawn executes inline
                            // Full implementation will wait for all spawned tasks
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "join_all expects exactly 1 argument (VecBytes of handles)",
                                ));
                            }
                            // Evaluate the handles argument (for side effects)
                            let _handles = self.codegen_expression(&arguments[0])?;
                            // Return void (represented as undef)
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "block_on" {
                            // For now, block_on just returns 0 since async executes inline
                            // Full implementation will run the executor until completion
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "block_on expects exactly 1 argument (future/handle)",
                                ));
                            }
                            // Evaluate the future argument (for side effects)
                            let _future = self.codegen_expression(&arguments[0])?;
                            // Return 0 as placeholder result
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            return Ok(LLVMConstInt(i64_ty, 0, 0));
                        }

                        // Mutex intrinsics - spinlock implementation using LLVM atomics
                        if name == "mutex_new" {
                            // Allocate 8 bytes for atomic lock state (0 = unlocked, 1 = locked)
                            let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                                CompilerError::codegen_error("Missing malloc")
                            })?;
                            let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let mutex_ptr = LLVMBuildCall2(
                                self.builder,
                                malloc_ty,
                                malloc_fn,
                                [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                                1,
                                c"mutex".as_ptr(),
                            );
                            // Initialize to 0 (unlocked)
                            let zero = LLVMConstInt(i64_ty, 0, 0);
                            LLVMBuildStore(self.builder, zero, mutex_ptr);
                            return Ok(mutex_ptr);
                        }

                        if name == "mutex_lock" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "mutex_lock expects 1 argument (mutex handle)",
                                ));
                            }
                            let mutex = self.codegen_expression(&arguments[0])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let zero = LLVMConstInt(i64_ty, 0, 0);
                            let one = LLVMConstInt(i64_ty, 1, 0);
                            
                            // Spinlock: atomically try to set 0 -> 1, loop until success
                            let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                            let spin_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"mutex.spin".as_ptr());
                            let acquired_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"mutex.acquired".as_ptr());
                            
                            LLVMBuildBr(self.builder, spin_bb);
                            LLVMPositionBuilderAtEnd(self.builder, spin_bb);
                            
                            // Atomic compare-and-swap: if *mutex == 0, set to 1
                            let result = LLVMBuildAtomicCmpXchg(
                                self.builder,
                                mutex,
                                zero,
                                one,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingMonotonic,
                                0, // single-threaded = false
                            );
                            let success = LLVMBuildExtractValue(self.builder, result, 1, c"cas.success".as_ptr());
                            LLVMBuildCondBr(self.builder, success, acquired_bb, spin_bb);
                            
                            LLVMPositionBuilderAtEnd(self.builder, acquired_bb);
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "mutex_unlock" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "mutex_unlock expects 1 argument (mutex handle)",
                                ));
                            }
                            let mutex = self.codegen_expression(&arguments[0])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let zero = LLVMConstInt(i64_ty, 0, 0);
                            
                            // Atomic store: set lock to 0 (release)
                            LLVMBuildStore(self.builder, zero, mutex);
                            // Add memory fence for release semantics
                            LLVMBuildFence(
                                self.builder,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingRelease,
                                0,
                                c"".as_ptr(),
                            );
                            
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "mutex_free" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "mutex_free expects 1 argument (mutex handle)",
                                ));
                            }
                            let mutex = self.codegen_expression(&arguments[0])?;
                            
                            // Free the allocated memory
                            let free_fn = *self.functions.get("free").ok_or_else(|| {
                                CompilerError::codegen_error("Missing free")
                            })?;
                            let free_ty = LLVMGlobalGetValueType(free_fn);
                            LLVMBuildCall2(
                                self.builder,
                                free_ty,
                                free_fn,
                                [mutex].as_mut_ptr(),
                                1,
                                c"".as_ptr(),
                            );
                            
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // Channel intrinsics (placeholder - simple queue for now)
                        if name == "channel_new" {
                            // Allocate a channel struct: { data_ptr, capacity, head, tail, count }
                            let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                                CompilerError::codegen_error("Missing malloc")
                            })?;
                            let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            // 40 bytes: ptr(8) + cap(8) + head(8) + tail(8) + count(8)
                            let channel_ptr = LLVMBuildCall2(
                                self.builder,
                                malloc_ty,
                                malloc_fn,
                                [LLVMConstInt(i64_ty, 40, 0)].as_mut_ptr(),
                                1,
                                c"channel".as_ptr(),
                            );
                            return Ok(channel_ptr);
                        }

                        if name == "channel_send" {
                            // Placeholder: just evaluates args
                            if arguments.len() != 2 {
                                return Err(CompilerError::codegen_error(
                                    "channel_send expects 2 arguments (channel, value)",
                                ));
                            }
                            let _channel = self.codegen_expression(&arguments[0])?;
                            let _value = self.codegen_expression(&arguments[1])?;
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "channel_recv" {
                            // Placeholder: returns 0
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "channel_recv expects 1 argument (channel)",
                                ));
                            }
                            let _channel = self.codegen_expression(&arguments[0])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            return Ok(LLVMConstInt(i64_ty, 0, 0));
                        }

                        if name == "channel_close" {
                            // Placeholder: no-op
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "channel_close expects 1 argument (channel)",
                                ));
                            }
                            let _channel = self.codegen_expression(&arguments[0])?;
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // AtomicInt intrinsics
                        if name == "atomic_new" {
                            // Allocate 8 bytes for an i64 atomic value
                            let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                                CompilerError::codegen_error("Missing malloc")
                            })?;
                            let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let atomic_ptr = LLVMBuildCall2(
                                self.builder,
                                malloc_ty,
                                malloc_fn,
                                [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                                1,
                                c"atomic".as_ptr(),
                            );
                            // Store initial value
                            if arguments.len() == 1 {
                                let init_val = self.codegen_expression(&arguments[0])?;
                                let ptr_typed = LLVMBuildBitCast(
                                    self.builder,
                                    atomic_ptr,
                                    LLVMPointerType(i64_ty, 0),
                                    c"".as_ptr(),
                                );
                                LLVMBuildStore(self.builder, init_val, ptr_typed);
                            }
                            return Ok(atomic_ptr);
                        }

                        if name == "atomic_load" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "atomic_load expects 1 argument",
                                ));
                            }
                            let atomic_ptr = self.codegen_expression(&arguments[0])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                atomic_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            // Use atomic load with acquire ordering
                            let load = LLVMBuildLoad2(self.builder, i64_ty, ptr_typed, c"atomic.load".as_ptr());
                            LLVMSetOrdering(load, llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingAcquire);
                            LLVMSetAlignment(load, 8);
                            return Ok(load);
                        }

                        if name == "atomic_store" {
                            if arguments.len() != 2 {
                                return Err(CompilerError::codegen_error(
                                    "atomic_store expects 2 arguments",
                                ));
                            }
                            let atomic_ptr = self.codegen_expression(&arguments[0])?;
                            let value = self.codegen_expression(&arguments[1])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                atomic_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            // Use atomic store with release ordering
                            let store = LLVMBuildStore(self.builder, value, ptr_typed);
                            LLVMSetOrdering(store, llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingRelease);
                            LLVMSetAlignment(store, 8);
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "atomic_add" {
                            if arguments.len() != 2 {
                                return Err(CompilerError::codegen_error(
                                    "atomic_add expects 2 arguments",
                                ));
                            }
                            let atomic_ptr = self.codegen_expression(&arguments[0])?;
                            let delta = self.codegen_expression(&arguments[1])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                atomic_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            // AtomicRMW Add
                            let prev = LLVMBuildAtomicRMW(
                                self.builder,
                                llvm_sys::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
                                ptr_typed,
                                delta,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
                                0, // not single-threaded
                            );
                            return Ok(prev);
                        }

                        if name == "atomic_sub" {
                            if arguments.len() != 2 {
                                return Err(CompilerError::codegen_error(
                                    "atomic_sub expects 2 arguments",
                                ));
                            }
                            let atomic_ptr = self.codegen_expression(&arguments[0])?;
                            let delta = self.codegen_expression(&arguments[1])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                atomic_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            // AtomicRMW Sub
                            let prev = LLVMBuildAtomicRMW(
                                self.builder,
                                llvm_sys::LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
                                ptr_typed,
                                delta,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
                                0,
                            );
                            return Ok(prev);
                        }

                        if name == "atomic_cas" {
                            if arguments.len() != 3 {
                                return Err(CompilerError::codegen_error(
                                    "atomic_cas expects 3 arguments (atomic, expected, new)",
                                ));
                            }
                            let atomic_ptr = self.codegen_expression(&arguments[0])?;
                            let expected = self.codegen_expression(&arguments[1])?;
                            let new_val = self.codegen_expression(&arguments[2])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                atomic_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            // AtomicCmpXchg
                            let result = LLVMBuildAtomicCmpXchg(
                                self.builder,
                                ptr_typed,
                                expected,
                                new_val,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
                                llvm_sys::LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
                                0,
                            );
                            // Extract success flag (second element of { i64, i1 })
                            let success = LLVMBuildExtractValue(self.builder, result, 1, c"cas.success".as_ptr());
                            // Zero-extend i1 to i64
                            let success_i64 = LLVMBuildZExt(self.builder, success, i64_ty, c"".as_ptr());
                            return Ok(success_i64);
                        }

                        // Timing intrinsics
                        if name == "sleep_ms" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "sleep_ms expects 1 argument (milliseconds)",
                                ));
                            }
                            let ms = self.codegen_expression(&arguments[0])?;
                            
                            // Convert milliseconds to microseconds (usleep takes microseconds)
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let thousand = LLVMConstInt(i64_ty, 1000, 0);
                            let us = LLVMBuildMul(self.builder, ms, thousand, c"us".as_ptr());
                            
                            // Call usleep(microseconds)
                            let usleep_fn = *self.functions.get("usleep").ok_or_else(|| {
                                CompilerError::codegen_error("Missing usleep")
                            })?;
                            let usleep_ty = LLVMGlobalGetValueType(usleep_fn);
                            
                            // usleep takes u32 on some platforms, truncate if needed
                            let i32_ty = LLVMInt32TypeInContext(self.context);
                            let us_32 = LLVMBuildTrunc(self.builder, us, i32_ty, c"us32".as_ptr());
                            
                            LLVMBuildCall2(
                                self.builder,
                                usleep_ty,
                                usleep_fn,
                                [us_32].as_mut_ptr(),
                                1,
                                c"".as_ptr(),
                            );
                            
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // Thread pool intrinsics
                        if name == "pool_new" {
                            // Allocate pool struct: { num_workers, queue_ptr, running }
                            let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                                CompilerError::codegen_error("Missing malloc")
                            })?;
                            let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let pool_ptr = LLVMBuildCall2(
                                self.builder,
                                malloc_ty,
                                malloc_fn,
                                [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                                1,
                                c"pool".as_ptr(),
                            );
                            // Store num_workers if provided
                            if arguments.len() == 1 {
                                let num_workers = self.codegen_expression(&arguments[0])?;
                                let ptr_typed = LLVMBuildBitCast(
                                    self.builder,
                                    pool_ptr,
                                    LLVMPointerType(i64_ty, 0),
                                    c"".as_ptr(),
                                );
                                LLVMBuildStore(self.builder, num_workers, ptr_typed);
                            }
                            return Ok(pool_ptr);
                        }

                        if name == "pool_spawn" || name == "pool_shutdown" {
                            // Placeholder implementations
                            for arg in arguments {
                                let _ = self.codegen_expression(arg)?;
                            }
                            if name == "pool_spawn" {
                                // Return dummy handle
                                let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                                return Ok(LLVMConstNull(i8_ptr_ty));
                            }
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // Executor intrinsics
                        if name == "executor_new" {
                            // Allocate executor struct
                            let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                                CompilerError::codegen_error("Missing malloc")
                            })?;
                            let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let exec_ptr = LLVMBuildCall2(
                                self.builder,
                                malloc_ty,
                                malloc_fn,
                                [LLVMConstInt(i64_ty, 32, 0)].as_mut_ptr(),
                                1,
                                c"executor".as_ptr(),
                            );
                            return Ok(exec_ptr);
                        }

                        if name == "executor_spawn" || name == "executor_run" || name == "executor_shutdown" {
                            // Placeholder implementations
                            for arg in arguments {
                                let _ = self.codegen_expression(arg)?;
                            }
                            if name == "executor_spawn" {
                                let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                                return Ok(LLVMConstNull(i8_ptr_ty));
                            }
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        // Cancellation intrinsics
                        if name == "cancel_token_new" {
                            // Allocate token: single i64 flag (0 = not cancelled, 1 = cancelled)
                            let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                                CompilerError::codegen_error("Missing malloc")
                            })?;
                            let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let token_ptr = LLVMBuildCall2(
                                self.builder,
                                malloc_ty,
                                malloc_fn,
                                [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                                1,
                                c"cancel_token".as_ptr(),
                            );
                            // Initialize to 0 (not cancelled)
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                token_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), ptr_typed);
                            return Ok(token_ptr);
                        }

                        if name == "cancel_token_cancel" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "cancel_token_cancel expects 1 argument",
                                ));
                            }
                            let token_ptr = self.codegen_expression(&arguments[0])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                token_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            // Atomic store 1 to mark as cancelled
                            LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 1, 0), ptr_typed);
                            let void_ty = LLVMVoidTypeInContext(self.context);
                            return Ok(LLVMGetUndef(void_ty));
                        }

                        if name == "cancel_token_is_cancelled" {
                            if arguments.len() != 1 {
                                return Err(CompilerError::codegen_error(
                                    "cancel_token_is_cancelled expects 1 argument",
                                ));
                            }
                            let token_ptr = self.codegen_expression(&arguments[0])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            let ptr_typed = LLVMBuildBitCast(
                                self.builder,
                                token_ptr,
                                LLVMPointerType(i64_ty, 0),
                                c"".as_ptr(),
                            );
                            let val = LLVMBuildLoad2(self.builder, i64_ty, ptr_typed, c"cancelled".as_ptr());
                            return Ok(val);
                        }

                        // Timeout intrinsic
                        if name == "timeout" {
                            // Placeholder: always returns 0 (completed, not timed out)
                            // Full implementation would check elapsed time
                            if arguments.len() != 2 {
                                return Err(CompilerError::codegen_error(
                                    "timeout expects 2 arguments (future, milliseconds)",
                                ));
                            }
                            let _future = self.codegen_expression(&arguments[0])?;
                            let _ms = self.codegen_expression(&arguments[1])?;
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            return Ok(LLVMConstInt(i64_ty, 0, 0)); // 0 = completed
                        }

                        // VecInt intrinsics
                        if let Some(result) = self.codegen_vec_int_intrinsic(name, arguments)? {
                            return Ok(result);
                        }

                        // VecString intrinsics
                        if let Some(result) = self.codegen_vec_string_intrinsic(name, arguments)? {
                            return Ok(result);
                        }

                        // VecBytes intrinsics
                        if let Some(result) = self.codegen_vec_bytes_intrinsic(name, arguments)? {
                            return Ok(result);
                        }

                        // MapStringInt intrinsics
                        if let Some(result) =
                            self.codegen_map_string_int_intrinsic(name, arguments)?
                        {
                            return Ok(result);
                        }

                        // MapStringString intrinsics
                        if let Some(result) =
                            self.codegen_map_string_string_intrinsic(name, arguments)?
                        {
                            return Ok(result);
                        }

                        // String manipulation intrinsics
                        if let Some(result) = self.codegen_string_intrinsic(name, arguments)? {
                            return Ok(result);
                        }

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
                            let arg_val = self.codegen_expression(arg)?;

                            let arg_val = match arg {
                                Expression::Identifier(var_name)
                                    if self.struct_variables.contains_key(var_name) =>
                                {
                                    let load_name = CString::new(format!("{var_name}.load"))
                                        .expect("CString failed");
                                    LLVMBuildLoad2(
                                        self.builder,
                                        LLVMGetAllocatedType(arg_val),
                                        arg_val,
                                        load_name.as_ptr(),
                                    )
                                }
                                Expression::StructLiteral { .. } => {
                                    let load_name =
                                        CString::new("struct.load").expect("CString failed");
                                    LLVMBuildLoad2(
                                        self.builder,
                                        LLVMGetAllocatedType(arg_val),
                                        arg_val,
                                        load_name.as_ptr(),
                                    )
                                }
                                _ => arg_val,
                            };

                            arg_values.push(arg_val);
                        }

                        self.coerce_stdlib_call_args(name, &mut arg_values)?;

                        // Build the call
                        let func_type = LLVMGlobalGetValueType(function);
                        let ret_type = LLVMGetReturnType(func_type);
                        let void_type = LLVMVoidTypeInContext(self.context);
                        let call_name = if ret_type == void_type {
                            CString::new("").expect("CString failed")
                        } else {
                            CString::new("calltmp").expect("CString failed")
                        };
                        let call = LLVMBuildCall2(
                            self.builder,
                            func_type,
                            function,
                            arg_values.as_mut_ptr(),
                            arg_values.len() as u32,
                            call_name.as_ptr(),
                        );

                        let call = self.maybe_trap_on_null_stdlib_result(name, call)?;
                        Ok(self.maybe_widen_stdlib_result(name, call))
                    } else {
                        Err(CompilerError::codegen_error(
                            "Only direct function calls are supported",
                        ))
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
                        Operator::BitNot => {
                            let name = CString::new("bitnottmp").expect("CString failed");
                            LLVMBuildNot(self.builder, operand_val, name.as_ptr())
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
                    if elements.is_empty() {
                        return Err(CompilerError::codegen_error(
                            "Empty array literals not supported",
                        ));
                    }

                    // Generate code for all elements
                    let mut element_values: Vec<LLVMValueRef> = Vec::new();
                    for elem in elements {
                        element_values.push(self.codegen_expression(elem)?);
                    }

                    // Get element type from first element
                    let elem_type = LLVMTypeOf(element_values[0]);
                    let array_type = LLVMArrayType2(elem_type, elements.len() as u64);

                    // Allocate array on stack
                    let array_name = CString::new("array").expect("CString failed");
                    let array_alloca =
                        LLVMBuildAlloca(self.builder, array_type, array_name.as_ptr());

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

                Expression::Index { array, index } => {
                    let array_val = self.codegen_expression(array)?;
                    let index_val = self.codegen_expression(index)?;

                    // array_val can be either:
                    // - an alloca (stack allocated arrays / array literals)
                    // - an i8* pointer (bytes/string pointers)
                    if !LLVMIsAAllocaInst(array_val).is_null() {
                        // Array indexing: *(array_ptr + [0, idx])
                        let array_type = LLVMGetAllocatedType(array_val);
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

                        let elem_type = LLVMGetElementType(array_type);
                        let load_name = CString::new("elem").expect("CString failed");
                        return Ok(LLVMBuildLoad2(
                            self.builder,
                            elem_type,
                            elem_ptr,
                            load_name.as_ptr(),
                        ));
                    }

                    // Byte indexing on bytes/string: *(i8* + idx) -> zext to i64
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    // Debug bounds checking for string indexing
                    if self.debug_bounds_checks {
                        let strlen_fn = *self.functions.get("strlen").ok_or_else(|| {
                            CompilerError::codegen_error("Missing strlen for bounds check")
                        })?;
                        let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                        let len = LLVMBuildCall2(
                            self.builder,
                            strlen_ty,
                            strlen_fn,
                            [array_val].as_mut_ptr(),
                            1,
                            c"str.len".as_ptr(),
                        );

                        // Check: index < len
                        let in_bounds = LLVMBuildICmp(
                            self.builder,
                            llvm_sys::LLVMIntPredicate::LLVMIntULT,
                            index_val,
                            len,
                            c"bounds.ok".as_ptr(),
                        );

                        let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                        let ok_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            current_fn,
                            c"str.idx.ok".as_ptr(),
                        );
                        let trap_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            current_fn,
                            c"str.idx.trap".as_ptr(),
                        );

                        LLVMBuildCondBr(self.builder, in_bounds, ok_bb, trap_bb);

                        // Trap block
                        LLVMPositionBuilderAtEnd(self.builder, trap_bb);
                        let trap_fn = *self.functions.get("abort").ok_or_else(|| {
                            CompilerError::codegen_error("Missing abort")
                        })?;
                        let trap_ty = LLVMGlobalGetValueType(trap_fn);
                        LLVMBuildCall2(
                            self.builder,
                            trap_ty,
                            trap_fn,
                            std::ptr::null_mut(),
                            0,
                            c"".as_ptr(),
                        );
                        LLVMBuildUnreachable(self.builder);

                        // Continue in ok block
                        LLVMPositionBuilderAtEnd(self.builder, ok_bb);
                    }

                    let byte_ptr_name = CString::new("byteptr").expect("CString failed");
                    let mut indices = [index_val];
                    let byte_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        array_val,
                        indices.as_mut_ptr(),
                        1,
                        byte_ptr_name.as_ptr(),
                    );

                    let byte_name = CString::new("byte").expect("CString failed");
                    let byte_val =
                        LLVMBuildLoad2(self.builder, i8_ty, byte_ptr, byte_name.as_ptr());

                    let zext_name = CString::new("byte.zext").expect("CString failed");
                    Ok(LLVMBuildZExt(
                        self.builder,
                        byte_val,
                        i64_ty,
                        zext_name.as_ptr(),
                    ))
                }

                Expression::Slice { array, start, end } => {
                    // String slicing: use str_slice intrinsic
                    let array_val = self.codegen_expression(array)?;
                    let start_val = self.codegen_expression(start)?;
                    let end_val = self.codegen_expression(end)?;

                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let _i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);

                    // Get malloc, memcpy functions
                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let memcpy_fn = *self.functions.get("memcpy").ok_or_else(|| {
                        CompilerError::codegen_error("Missing memcpy")
                    })?;
                    let memcpy_ty = LLVMGlobalGetValueType(memcpy_fn);

                    // Calculate length: end - start
                    let len = LLVMBuildSub(self.builder, end_val, start_val, c"slice.len".as_ptr());

                    // Allocate: malloc(len + 1) for null terminator
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size = LLVMBuildAdd(self.builder, len, one, c"alloc.size".as_ptr());
                    let new_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [alloc_size].as_mut_ptr(),
                        1,
                        c"slice.ptr".as_ptr(),
                    );

                    // Get source pointer: array + start
                    let src_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        LLVMInt8TypeInContext(self.context),
                        array_val,
                        [start_val].as_mut_ptr(),
                        1,
                        c"src.ptr".as_ptr(),
                    );

                    // memcpy(new_str, src_ptr, len)
                    LLVMBuildCall2(
                        self.builder,
                        memcpy_ty,
                        memcpy_fn,
                        [new_str, src_ptr, len].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    // Null terminate: new_str[len] = 0
                    let null_pos = LLVMBuildInBoundsGEP2(
                        self.builder,
                        LLVMInt8TypeInContext(self.context),
                        new_str,
                        [len].as_mut_ptr(),
                        1,
                        c"null.pos".as_ptr(),
                    );
                    let zero_byte = LLVMConstInt(LLVMInt8TypeInContext(self.context), 0, 0);
                    LLVMBuildStore(self.builder, zero_byte, null_pos);

                    Ok(new_str)
                }

                Expression::StructLiteral { name, fields } => {
                    // Get the struct type (clone to avoid borrow issues)
                    let (struct_type, field_names, _) =
                        self.struct_types.get(name).cloned().ok_or_else(|| {
                            CompilerError::codegen_error(format!("Undefined struct: {name}"))
                        })?;

                    // Allocate struct on stack
                    let struct_name = CString::new(format!("{name}.tmp")).expect("CString failed");
                    let struct_alloca =
                        LLVMBuildAlloca(self.builder, struct_type, struct_name.as_ptr());

                    // Store each field
                    for (field_name, field_expr) in fields {
                        // Find field index
                        let field_idx = field_names
                            .iter()
                            .position(|f| f == field_name)
                            .ok_or_else(|| {
                                CompilerError::codegen_error(format!(
                                    "Field {field_name} not found in struct {name}"
                                ))
                            })? as u32;

                        // Generate field value
                        let field_val = self.codegen_expression(field_expr)?;

                        // Get pointer to field
                        let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                        let idx =
                            LLVMConstInt(LLVMInt32TypeInContext(self.context), field_idx as u64, 0);
                        let mut indices = [zero, idx];

                        let field_ptr_name =
                            CString::new(format!("{field_name}.ptr")).expect("CString failed");
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

                Expression::MemberAccess { object, member } => {
                    // Get the object (should be a struct pointer)
                    let obj_val = self.codegen_expression(object)?;

                    // Get struct name from the object expression
                    let struct_name = if let Expression::Identifier(var_name) = &**object {
                        self.struct_variables.get(var_name).cloned()
                    } else {
                        None
                    }
                    .ok_or_else(|| {
                        CompilerError::codegen_error(
                            "Member access only supported on named struct variables",
                        )
                    })?;

                    // Get struct type info
                    let (struct_type, field_names, field_types) = self
                        .struct_types
                        .get(&struct_name)
                        .cloned()
                        .ok_or_else(|| {
                            CompilerError::codegen_error(format!(
                                "Undefined struct type: {struct_name}"
                            ))
                        })?;

                    // Find field index
                    let field_idx =
                        field_names
                            .iter()
                            .position(|f| f == member)
                            .ok_or_else(|| {
                                CompilerError::codegen_error(format!(
                                    "Field {member} not found in struct {struct_name}"
                                ))
                            })? as u32;

                    // Get pointer to field
                    let zero = LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0);
                    let idx =
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), field_idx as u64, 0);
                    let mut indices = [zero, idx];

                    let field_ptr_name =
                        CString::new(format!("{member}.ptr")).expect("CString failed");
                    let field_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        struct_type,
                        obj_val,
                        indices.as_mut_ptr(),
                        2,
                        field_ptr_name.as_ptr(),
                    );

                    // Load field value
                    let field_type = field_types[field_idx as usize];
                    let load_name = CString::new(format!("{member}.load")).expect("CString failed");
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        field_type,
                        field_ptr,
                        load_name.as_ptr(),
                    ))
                }

                Expression::Assignment { target, value } => {
                    // Get the target variable (must be an identifier for now)
                    if let Expression::Identifier(var_name) = &**target {
                        let alloca = self.named_values.get(var_name).copied().ok_or_else(|| {
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
                        Err(CompilerError::codegen_error(
                            "Assignment target must be a variable",
                        ))
                    }
                }

                Expression::Reference { expression } => {
                    // For references, we want to return the address, not the value
                    // If it's an identifier, return its alloca pointer
                    if let Expression::Identifier(var_name) = &**expression {
                        self.named_values.get(var_name).copied().ok_or_else(|| {
                            CompilerError::codegen_error(format!("Undefined variable: {var_name}"))
                        })
                    } else {
                        // For other expressions, we need to evaluate them first
                        // This is a simplified implementation
                        self.codegen_expression(expression)
                    }
                }

                Expression::Dereference { expression } => {
                    // Get the pointer value
                    let ptr_val = self.codegen_expression(expression)?;

                    // Load the value from the pointer
                    let ptr_type = LLVMTypeOf(ptr_val);
                    let pointee_type = LLVMGetElementType(ptr_type);

                    let load_name = CString::new("deref").expect("CString failed");
                    Ok(LLVMBuildLoad2(
                        self.builder,
                        pointee_type,
                        ptr_val,
                        load_name.as_ptr(),
                    ))
                }

                Expression::Await { expression } => {
                    // For now, await just evaluates the expression
                    // Full implementation will integrate with runtime executor
                    self.codegen_expression(expression)
                }

                Expression::Spawn { body } => {
                    // For now, spawn executes the body inline and returns a dummy handle
                    // Full implementation will spawn a task on the runtime
                    for stmt in &body.statements {
                        self.codegen_statement(stmt)?;
                    }
                    // Return a null handle for now
                    let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    Ok(LLVMConstNull(i8_ptr_ty))
                }

                Expression::EnumVariant { enum_name, variant_name, payload: _ } => {
                    // Get the tag value for this variant
                    if let Some(variants) = self.enum_types.get(enum_name) {
                        if let Some((_, tag)) = variants.iter().find(|(name, _)| name == variant_name) {
                            // Return the tag value as an i64
                            let i64_ty = LLVMInt64TypeInContext(self.context);
                            Ok(LLVMConstInt(i64_ty, *tag as u64, 0))
                        } else {
                            Err(CompilerError::codegen_error(format!(
                                "Unknown variant '{}' for enum '{}'", variant_name, enum_name
                            )))
                        }
                    } else {
                        Err(CompilerError::codegen_error(format!(
                            "Unknown enum '{}'", enum_name
                        )))
                    }
                }

                _ => Err(CompilerError::codegen_error("Unsupported expression type")),
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
                Type::Str => {
                    // str is a fat pointer: { ptr: *i8, len: i64 }
                    let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let mut fields = [i8_ptr, i64_ty];
                    LLVMStructTypeInContext(self.context, fields.as_mut_ptr(), 2, 0)
                }
                Type::Bytes => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::Void => LLVMVoidTypeInContext(self.context),
                Type::Array { element_type, size } => {
                    let elem_type = self.get_llvm_type(element_type);
                    if let Some(s) = size {
                        LLVMArrayType2(elem_type, *s as u64)
                    } else {
                        LLVMPointerType(elem_type, 0)
                    }
                }
                Type::Reference { inner_type, .. } => {
                    let inner = self.get_llvm_type(inner_type);
                    LLVMPointerType(inner, 0)
                }
                Type::Pointer { inner_type, .. } => {
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
                Type::Generic { .. } => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::VecInt
                | Type::VecString
                | Type::VecBytes
                | Type::MapStringInt
                | Type::MapStringString => LLVMPointerType(LLVMInt8TypeInContext(self.context), 0),
                Type::SliceInt | Type::SliceString | Type::SliceBytes => {
                    // Slice is { ptr: *i8, len: i64 }
                    let i8_ptr = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let mut fields = [i8_ptr, i64_ty];
                    LLVMStructTypeInContext(self.context, fields.as_mut_ptr(), 2, 0)
                }
            }
        }
    }

    /// Handle VecInt intrinsics
    fn codegen_vec_int_intrinsic(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<Option<LLVMValueRef>> {
        unsafe {
            let i64_ty = LLVMInt64TypeInContext(self.context);
            let i64_ptr_ty = LLVMPointerType(i64_ty, 0);
            let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);

            match name {
                "vec_int_new" => {
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let struct_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                        1,
                        c"vec".as_ptr(),
                    );
                    let array_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 32, 0)].as_mut_ptr(),
                        1,
                        c"data".as_ptr(),
                    );
                    let array_typed =
                        LLVMBuildBitCast(self.builder, array_ptr, i64_ptr_ty, c"".as_ptr());
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        struct_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, array_typed, ptr_field);
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        struct_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        struct_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 4, 0), cap_field);
                    Ok(Some(struct_ptr))
                }
                "vec_int_len" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i64_ty,
                        len_field,
                        c"len".as_ptr(),
                    )))
                }
                "vec_int_push" => {
                    // Simplified v1: no branching, always check and grow if needed using select
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let value = self.codegen_expression(&arguments[1])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let cap_val = LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"".as_ptr());

                    // Use select to compute new capacity (double if full, else keep same)
                    let needs_grow = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGE,
                        len_val,
                        cap_val,
                        c"".as_ptr(),
                    );
                    let double_cap = LLVMBuildMul(
                        self.builder,
                        cap_val,
                        LLVMConstInt(i64_ty, 2, 0),
                        c"".as_ptr(),
                    );
                    let new_cap = LLVMBuildSelect(
                        self.builder,
                        needs_grow,
                        double_cap,
                        cap_val,
                        c"".as_ptr(),
                    );
                    let new_size = LLVMBuildMul(
                        self.builder,
                        new_cap,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"".as_ptr(),
                    );

                    // Always realloc (no-op if size unchanged)
                    let old_ptr = LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"".as_ptr());
                    let old_ptr_i8 =
                        LLVMBuildBitCast(self.builder, old_ptr, i8_ptr_ty, c"".as_ptr());
                    let realloc_fn = *self
                        .functions
                        .get("realloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing realloc"))?;
                    let new_ptr = LLVMBuildCall2(
                        self.builder,
                        LLVMGlobalGetValueType(realloc_fn),
                        realloc_fn,
                        [old_ptr_i8, new_size].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let new_ptr_typed =
                        LLVMBuildBitCast(self.builder, new_ptr, i64_ptr_ty, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_ptr_typed, ptr_field);
                    LLVMBuildStore(self.builder, new_cap, cap_field);

                    // Store value at len index and increment len
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, elem_ptr);
                    let new_len = LLVMBuildAdd(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_len, len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_get" => {
                    // v1: no bounds checking (unsafe like C arrays)
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i64_ty,
                        elem_ptr,
                        c"".as_ptr(),
                    )))
                }
                "vec_int_set" => {
                    // v1: no bounds checking (unsafe like C arrays)
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let value = self.codegen_expression(&arguments[2])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, elem_ptr);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_pop" => {
                    // Pop: read last element, decrement len, return element
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    
                    // Get length field (offset 8 bytes from struct start)
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    
                    // Compute last index (len - 1)
                    let last_idx = LLVMBuildSub(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    
                    // Get data pointer and read element at last_idx FIRST
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        data_ptr,
                        [last_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let val = LLVMBuildLoad2(self.builder, i64_ty, elem_ptr, c"".as_ptr());
                    
                    // Now store decremented length
                    LLVMBuildStore(self.builder, last_idx, len_field);
                    
                    Ok(Some(val))
                }
                "vec_int_clear" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_capacity" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let cap = LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"cap".as_ptr());
                    Ok(Some(cap))
                }
                "vec_int_reserve" => {
                    // Ensure capacity >= new_cap, reallocate if needed
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let new_cap = self.codegen_expression(&arguments[1])?;
                    
                    // Get current capacity
                    let cap_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, vec_ptr, [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let cap_field = LLVMBuildBitCast(self.builder, cap_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    let old_cap = LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"old_cap".as_ptr());
                    
                    // Check if reallocation needed: new_cap > old_cap
                    let needs_realloc = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSGT, new_cap, old_cap, c"needs_realloc".as_ptr());
                    
                    // Get current function for block creation
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let realloc_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"reserve.realloc".as_ptr());
                    let done_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"reserve.done".as_ptr());
                    
                    LLVMBuildCondBr(self.builder, needs_realloc, realloc_bb, done_bb);
                    
                    // Realloc block
                    LLVMPositionBuilderAtEnd(self.builder, realloc_bb);
                    
                    let realloc_fn = *self.functions.get("realloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing realloc")
                    })?;
                    let realloc_ty = LLVMGlobalGetValueType(realloc_fn);
                    
                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(self.builder, vec_ptr, LLVMPointerType(i64_ptr_ty, 0), c"".as_ptr());
                    let old_data = LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"old_data".as_ptr());
                    let old_data_i8 = LLVMBuildBitCast(self.builder, old_data, i8_ptr_ty, c"".as_ptr());
                    
                    // New size = new_cap * 8
                    let new_size = LLVMBuildMul(self.builder, new_cap, LLVMConstInt(i64_ty, 8, 0), c"new_size".as_ptr());
                    
                    // Realloc
                    let new_data = LLVMBuildCall2(self.builder, realloc_ty, realloc_fn, [old_data_i8, new_size].as_mut_ptr(), 2, c"new_data".as_ptr());
                    let new_data_typed = LLVMBuildBitCast(self.builder, new_data, i64_ptr_ty, c"".as_ptr());
                    
                    // Store new data ptr and capacity
                    LLVMBuildStore(self.builder, new_data_typed, ptr_field);
                    LLVMBuildStore(self.builder, new_cap, cap_field);
                    
                    LLVMBuildBr(self.builder, done_bb);
                    
                    // Done block
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_shrink_to_fit" => {
                    // Shrink capacity to match length
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    
                    // Get len
                    let len_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, vec_ptr, [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let len_field = LLVMBuildBitCast(self.builder, len_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());
                    
                    // Get capacity
                    let cap_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, vec_ptr, [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let cap_field = LLVMBuildBitCast(self.builder, cap_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    let cap = LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"cap".as_ptr());
                    
                    // Check if shrink needed: cap > len
                    let needs_shrink = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSGT, cap, len, c"needs_shrink".as_ptr());
                    
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let shrink_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"shrink.do".as_ptr());
                    let done_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"shrink.done".as_ptr());
                    
                    LLVMBuildCondBr(self.builder, needs_shrink, shrink_bb, done_bb);
                    
                    // Shrink block
                    LLVMPositionBuilderAtEnd(self.builder, shrink_bb);
                    
                    let realloc_fn = *self.functions.get("realloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing realloc")
                    })?;
                    let realloc_ty = LLVMGlobalGetValueType(realloc_fn);
                    
                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(self.builder, vec_ptr, LLVMPointerType(i64_ptr_ty, 0), c"".as_ptr());
                    let old_data = LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"old_data".as_ptr());
                    let old_data_i8 = LLVMBuildBitCast(self.builder, old_data, i8_ptr_ty, c"".as_ptr());
                    
                    // New size = len * 8 (minimum 8 bytes to avoid zero alloc)
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let min_len = LLVMBuildSelect(self.builder, 
                        LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSLT, len, one, c"".as_ptr()),
                        one, len, c"min_len".as_ptr());
                    let new_size = LLVMBuildMul(self.builder, min_len, LLVMConstInt(i64_ty, 8, 0), c"new_size".as_ptr());
                    
                    // Realloc
                    let new_data = LLVMBuildCall2(self.builder, realloc_ty, realloc_fn, [old_data_i8, new_size].as_mut_ptr(), 2, c"new_data".as_ptr());
                    let new_data_typed = LLVMBuildBitCast(self.builder, new_data, i64_ptr_ty, c"".as_ptr());
                    
                    // Store new data ptr and capacity
                    LLVMBuildStore(self.builder, new_data_typed, ptr_field);
                    LLVMBuildStore(self.builder, min_len, cap_field);
                    
                    LLVMBuildBr(self.builder, done_bb);
                    
                    // Done block
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_with_capacity" => {
                    let capacity = self.codegen_expression(&arguments[0])?;
                    let malloc_fn = *self.functions.get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    
                    // Allocate struct (24 bytes: ptr + len + cap)
                    let struct_ptr = LLVMBuildCall2(
                        self.builder, malloc_ty, malloc_fn,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(), 1, c"vec".as_ptr(),
                    );
                    
                    // Allocate data array (capacity * 8 bytes for i64)
                    let data_size = LLVMBuildMul(self.builder, capacity, LLVMConstInt(i64_ty, 8, 0), c"size".as_ptr());
                    let array_ptr = LLVMBuildCall2(
                        self.builder, malloc_ty, malloc_fn,
                        [data_size].as_mut_ptr(), 1, c"data".as_ptr(),
                    );
                    let array_typed = LLVMBuildBitCast(self.builder, array_ptr, i64_ptr_ty, c"".as_ptr());
                    
                    // Store ptr
                    let ptr_field = LLVMBuildBitCast(self.builder, struct_ptr, LLVMPointerType(i64_ptr_ty, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, array_typed, ptr_field);
                    
                    // Store len = 0
                    let len_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, struct_ptr, [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let len_field = LLVMBuildBitCast(self.builder, len_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    
                    // Store cap = capacity
                    let cap_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, struct_ptr, [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let cap_field = LLVMBuildBitCast(self.builder, cap_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, capacity, cap_field);
                    
                    Ok(Some(struct_ptr))
                }
                "vec_int_swap_remove" => {
                    // O(1) remove: swap element at index with last element, then pop
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    
                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(self.builder, vec_ptr, LLVMPointerType(i64_ptr_ty, 0), c"".as_ptr());
                    let data_ptr = LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"data".as_ptr());
                    
                    // Get len
                    let len_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, vec_ptr, [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let len_field = LLVMBuildBitCast(self.builder, len_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());
                    
                    // Get element at index (to return)
                    let elem_ptr = LLVMBuildGEP2(self.builder, i64_ty, data_ptr, [index].as_mut_ptr(), 1, c"elem".as_ptr());
                    let removed_val = LLVMBuildLoad2(self.builder, i64_ty, elem_ptr, c"removed".as_ptr());
                    
                    // Get last index
                    let last_idx = LLVMBuildSub(self.builder, len, LLVMConstInt(i64_ty, 1, 0), c"last".as_ptr());
                    
                    // Get last element
                    let last_ptr = LLVMBuildGEP2(self.builder, i64_ty, data_ptr, [last_idx].as_mut_ptr(), 1, c"last_elem".as_ptr());
                    let last_val = LLVMBuildLoad2(self.builder, i64_ty, last_ptr, c"last_val".as_ptr());
                    
                    // Store last element at index position
                    LLVMBuildStore(self.builder, last_val, elem_ptr);
                    
                    // Decrement len
                    LLVMBuildStore(self.builder, last_idx, len_field);
                    
                    Ok(Some(removed_val))
                }
                "vec_int_insert" => {
                    // O(n) insert: shift elements right, insert at index
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let value = self.codegen_expression(&arguments[2])?;
                    
                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(self.builder, vec_ptr, LLVMPointerType(i64_ptr_ty, 0), c"".as_ptr());
                    let data_ptr = LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"data".as_ptr());
                    
                    // Get len
                    let len_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, vec_ptr, [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let len_field = LLVMBuildBitCast(self.builder, len_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());
                    
                    // Calculate bytes to move: (len - index) * 8
                    let elements_to_move = LLVMBuildSub(self.builder, len, index, c"tomove".as_ptr());
                    let bytes_to_move = LLVMBuildMul(self.builder, elements_to_move, LLVMConstInt(i64_ty, 8, 0), c"bytes".as_ptr());
                    
                    // Source: data[index], Dest: data[index+1]
                    let src_ptr = LLVMBuildGEP2(self.builder, i64_ty, data_ptr, [index].as_mut_ptr(), 1, c"src".as_ptr());
                    let index_plus_one = LLVMBuildAdd(self.builder, index, LLVMConstInt(i64_ty, 1, 0), c"idx1".as_ptr());
                    let dst_ptr = LLVMBuildGEP2(self.builder, i64_ty, data_ptr, [index_plus_one].as_mut_ptr(), 1, c"dst".as_ptr());
                    
                    // memmove(dst, src, bytes)
                    let memmove_fn = *self.functions.get("memmove").ok_or_else(|| {
                        CompilerError::codegen_error("Missing memmove")
                    })?;
                    let memmove_ty = LLVMGlobalGetValueType(memmove_fn);
                    let src_i8 = LLVMBuildBitCast(self.builder, src_ptr, i8_ptr_ty, c"".as_ptr());
                    let dst_i8 = LLVMBuildBitCast(self.builder, dst_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(self.builder, memmove_ty, memmove_fn, [dst_i8, src_i8, bytes_to_move].as_mut_ptr(), 3, c"".as_ptr());
                    
                    // Store value at index
                    LLVMBuildStore(self.builder, value, src_ptr);
                    
                    // Increment len
                    let new_len = LLVMBuildAdd(self.builder, len, LLVMConstInt(i64_ty, 1, 0), c"newlen".as_ptr());
                    LLVMBuildStore(self.builder, new_len, len_field);
                    
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_int_remove" => {
                    // O(n) remove: shift elements left after removing
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    
                    // Get data ptr
                    let ptr_field = LLVMBuildBitCast(self.builder, vec_ptr, LLVMPointerType(i64_ptr_ty, 0), c"".as_ptr());
                    let data_ptr = LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"data".as_ptr());
                    
                    // Get len
                    let len_addr = LLVMBuildGEP2(self.builder, i8_ptr_ty, vec_ptr, [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(), 1, c"".as_ptr());
                    let len_field = LLVMBuildBitCast(self.builder, len_addr, LLVMPointerType(i64_ty, 0), c"".as_ptr());
                    let len = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"len".as_ptr());
                    
                    // Get element at index (to return)
                    let elem_ptr = LLVMBuildGEP2(self.builder, i64_ty, data_ptr, [index].as_mut_ptr(), 1, c"elem".as_ptr());
                    let removed_val = LLVMBuildLoad2(self.builder, i64_ty, elem_ptr, c"removed".as_ptr());
                    
                    // Calculate bytes to move: (len - index - 1) * 8
                    let index_plus_one = LLVMBuildAdd(self.builder, index, LLVMConstInt(i64_ty, 1, 0), c"idx1".as_ptr());
                    let elements_to_move = LLVMBuildSub(self.builder, len, index_plus_one, c"tomove".as_ptr());
                    let bytes_to_move = LLVMBuildMul(self.builder, elements_to_move, LLVMConstInt(i64_ty, 8, 0), c"bytes".as_ptr());
                    
                    // Source: data[index+1], Dest: data[index]
                    let src_ptr = LLVMBuildGEP2(self.builder, i64_ty, data_ptr, [index_plus_one].as_mut_ptr(), 1, c"src".as_ptr());
                    
                    // memmove(dst, src, bytes)
                    let memmove_fn = *self.functions.get("memmove").ok_or_else(|| {
                        CompilerError::codegen_error("Missing memmove")
                    })?;
                    let memmove_ty = LLVMGlobalGetValueType(memmove_fn);
                    let src_i8 = LLVMBuildBitCast(self.builder, src_ptr, i8_ptr_ty, c"".as_ptr());
                    let dst_i8 = LLVMBuildBitCast(self.builder, elem_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(self.builder, memmove_ty, memmove_fn, [dst_i8, src_i8, bytes_to_move].as_mut_ptr(), 3, c"".as_ptr());
                    
                    // Decrement len
                    let new_len = LLVMBuildSub(self.builder, len, LLVMConstInt(i64_ty, 1, 0), c"newlen".as_ptr());
                    LLVMBuildStore(self.builder, new_len, len_field);
                    
                    Ok(Some(removed_val))
                }
                "vec_int_free" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, ptr_field, c"".as_ptr());
                    let data_ptr_i8 =
                        LLVMBuildBitCast(self.builder, data_ptr, i8_ptr_ty, c"".as_ptr());
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [data_ptr_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [vec_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                _ => Ok(None),
            }
        }
    }

    /// Handle VecString intrinsics
    fn codegen_vec_string_intrinsic(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<Option<LLVMValueRef>> {
        unsafe {
            let i64_ty = LLVMInt64TypeInContext(self.context);
            let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let str_ptr_ty = LLVMPointerType(i8_ptr_ty, 0); // pointer to string pointer

            match name {
                "vec_string_new" => {
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    // Struct: { ptr: **i8, len: i64, cap: i64 } = 24 bytes
                    let struct_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                        1,
                        c"vec".as_ptr(),
                    );
                    // Initial array: 4 string pointers = 32 bytes
                    let array_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 32, 0)].as_mut_ptr(),
                        1,
                        c"data".as_ptr(),
                    );
                    let array_typed =
                        LLVMBuildBitCast(self.builder, array_ptr, str_ptr_ty, c"".as_ptr());
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        struct_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, array_typed, ptr_field);
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        struct_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        struct_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 4, 0), cap_field);
                    Ok(Some(struct_ptr))
                }
                "vec_string_len" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i64_ty,
                        len_field,
                        c"len".as_ptr(),
                    )))
                }
                "vec_string_push" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let value = self.codegen_expression(&arguments[1])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let cap_val = LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"".as_ptr());
                    let needs_grow = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGE,
                        len_val,
                        cap_val,
                        c"".as_ptr(),
                    );
                    let double_cap = LLVMBuildMul(
                        self.builder,
                        cap_val,
                        LLVMConstInt(i64_ty, 2, 0),
                        c"".as_ptr(),
                    );
                    let new_cap = LLVMBuildSelect(
                        self.builder,
                        needs_grow,
                        double_cap,
                        cap_val,
                        c"".as_ptr(),
                    );
                    let new_size = LLVMBuildMul(
                        self.builder,
                        new_cap,
                        LLVMConstInt(i64_ty, 8, 0),
                        c"".as_ptr(),
                    );
                    let old_ptr = LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"".as_ptr());
                    let old_ptr_i8 =
                        LLVMBuildBitCast(self.builder, old_ptr, i8_ptr_ty, c"".as_ptr());
                    let realloc_fn = *self
                        .functions
                        .get("realloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing realloc"))?;
                    let new_ptr = LLVMBuildCall2(
                        self.builder,
                        LLVMGlobalGetValueType(realloc_fn),
                        realloc_fn,
                        [old_ptr_i8, new_size].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let new_ptr_typed =
                        LLVMBuildBitCast(self.builder, new_ptr, str_ptr_ty, c"".as_ptr());
                    LLVMBuildStore(self.builder, new_ptr_typed, ptr_field);
                    LLVMBuildStore(self.builder, new_cap, cap_field);
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, elem_ptr);
                    let new_len = LLVMBuildAdd(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_len, len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_string_get" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        elem_ptr,
                        c"".as_ptr(),
                    )))
                }
                "vec_string_set" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let value = self.codegen_expression(&arguments[2])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, elem_ptr);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_string_pop" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let last_idx = LLVMBuildSub(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        data_ptr,
                        [last_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let val = LLVMBuildLoad2(self.builder, i8_ptr_ty, elem_ptr, c"".as_ptr());
                    LLVMBuildStore(self.builder, last_idx, len_field);
                    Ok(Some(val))
                }
                "vec_string_clear" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_string_free" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, ptr_field, c"".as_ptr());
                    let data_ptr_i8 =
                        LLVMBuildBitCast(self.builder, data_ptr, i8_ptr_ty, c"".as_ptr());
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [data_ptr_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [vec_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                _ => Ok(None),
            }
        }
    }

    /// Handle VecBytes intrinsics (single-byte elements stored as i8)
    fn codegen_vec_bytes_intrinsic(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<Option<LLVMValueRef>> {
        unsafe {
            let i8_ty = LLVMInt8TypeInContext(self.context);
            let i64_ty = LLVMInt64TypeInContext(self.context);
            let i8_ptr_ty = LLVMPointerType(i8_ty, 0);

            match name {
                "vec_bytes_new" => {
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    // Struct: { ptr: *i8, len: i64, cap: i64 } = 24 bytes
                    let struct_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                        1,
                        c"vec".as_ptr(),
                    );
                    // Initial array: 16 bytes
                    let array_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"data".as_ptr(),
                    );
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        struct_ptr,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, array_ptr, ptr_field);
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        struct_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        struct_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 16, 0), cap_field);
                    Ok(Some(struct_ptr))
                }
                "vec_bytes_len" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i64_ty,
                        len_field,
                        c"len".as_ptr(),
                    )))
                }
                "vec_bytes_push" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let value = self.codegen_expression(&arguments[1])?;
                    let value_i8 = LLVMBuildTrunc(self.builder, value, i8_ty, c"".as_ptr());
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let cap_val = LLVMBuildLoad2(self.builder, i64_ty, cap_field, c"".as_ptr());
                    let needs_grow = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSGE,
                        len_val,
                        cap_val,
                        c"".as_ptr(),
                    );
                    let double_cap = LLVMBuildMul(
                        self.builder,
                        cap_val,
                        LLVMConstInt(i64_ty, 2, 0),
                        c"".as_ptr(),
                    );
                    let new_cap = LLVMBuildSelect(
                        self.builder,
                        needs_grow,
                        double_cap,
                        cap_val,
                        c"".as_ptr(),
                    );
                    let old_ptr = LLVMBuildLoad2(self.builder, i8_ptr_ty, ptr_field, c"".as_ptr());
                    let realloc_fn = *self
                        .functions
                        .get("realloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing realloc"))?;
                    let new_ptr = LLVMBuildCall2(
                        self.builder,
                        LLVMGlobalGetValueType(realloc_fn),
                        realloc_fn,
                        [old_ptr, new_cap].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_ptr, ptr_field);
                    LLVMBuildStore(self.builder, new_cap, cap_field);
                    let data_ptr = LLVMBuildLoad2(self.builder, i8_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        data_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value_i8, elem_ptr);
                    let new_len = LLVMBuildAdd(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_len, len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_bytes_get" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr = LLVMBuildLoad2(self.builder, i8_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let val_i8 = LLVMBuildLoad2(self.builder, i8_ty, elem_ptr, c"".as_ptr());
                    Ok(Some(LLVMBuildZExt(
                        self.builder,
                        val_i8,
                        i64_ty,
                        c"".as_ptr(),
                    )))
                }
                "vec_bytes_set" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let index = self.codegen_expression(&arguments[1])?;
                    let value = self.codegen_expression(&arguments[2])?;
                    let value_i8 = LLVMBuildTrunc(self.builder, value, i8_ty, c"".as_ptr());
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr = LLVMBuildLoad2(self.builder, i8_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        data_ptr,
                        [index].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value_i8, elem_ptr);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_bytes_pop" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let last_idx = LLVMBuildSub(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr = LLVMBuildLoad2(self.builder, i8_ptr_ty, ptr_field, c"".as_ptr());
                    let elem_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ty,
                        data_ptr,
                        [last_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let val_i8 = LLVMBuildLoad2(self.builder, i8_ty, elem_ptr, c"".as_ptr());
                    LLVMBuildStore(self.builder, last_idx, len_field);
                    Ok(Some(LLVMBuildZExt(
                        self.builder,
                        val_i8,
                        i64_ty,
                        c"".as_ptr(),
                    )))
                }
                "vec_bytes_clear" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vec_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "vec_bytes_free" => {
                    let vec_ptr = self.codegen_expression(&arguments[0])?;
                    let ptr_field = LLVMBuildBitCast(
                        self.builder,
                        vec_ptr,
                        LLVMPointerType(i8_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let data_ptr = LLVMBuildLoad2(self.builder, i8_ptr_ty, ptr_field, c"".as_ptr());
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [data_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [vec_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                _ => Ok(None),
            }
        }
    }

    /// Handle MapStringInt intrinsics (v1: simple linear search, no hashing)
    fn codegen_map_string_int_intrinsic(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<Option<LLVMValueRef>> {
        unsafe {
            let i64_ty = LLVMInt64TypeInContext(self.context);
            let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let str_ptr_ty = LLVMPointerType(i8_ptr_ty, 0);
            let i64_ptr_ty = LLVMPointerType(i64_ty, 0);

            match name {
                "map_string_int_new" => {
                    // Struct: { keys: **i8, values: *i64, len: i64, cap: i64 } = 32 bytes
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let map_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 32, 0)].as_mut_ptr(),
                        1,
                        c"map".as_ptr(),
                    );
                    // Initial capacity: 8 entries
                    let keys_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 64, 0)].as_mut_ptr(),
                        1,
                        c"keys".as_ptr(),
                    );
                    let vals_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 64, 0)].as_mut_ptr(),
                        1,
                        c"vals".as_ptr(),
                    );
                    let keys_typed =
                        LLVMBuildBitCast(self.builder, keys_ptr, str_ptr_ty, c"".as_ptr());
                    let vals_typed =
                        LLVMBuildBitCast(self.builder, vals_ptr, i64_ptr_ty, c"".as_ptr());
                    // Store keys pointer
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, keys_typed, keys_field);
                    // Store values pointer at offset 8
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, vals_typed, vals_field);
                    // Store len=0 at offset 16
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    // Store cap=8 at offset 24
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 8, 0), cap_field);
                    Ok(Some(map_ptr))
                }
                "map_string_int_len" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i64_ty,
                        len_field,
                        c"len".as_ptr(),
                    )))
                }
                "map_string_int_clear" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "map_string_int_free" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    // Free keys array
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let keys_i8 = LLVMBuildBitCast(self.builder, keys_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [keys_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    // Free values array
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let vals_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, vals_field, c"".as_ptr());
                    let vals_i8 = LLVMBuildBitCast(self.builder, vals_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [vals_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    // Free map struct
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [map_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "map_string_int_has" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let key = self.codegen_expression(&arguments[1])?;
                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    // Load keys array and len
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    // Create loop blocks
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_loop".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_found".as_ptr(),
                    );
                    let notfound_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_notfound".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_done".as_ptr(),
                    );
                    // Entry: alloca for index, init to 0, branch to loop
                    let idx_ptr = LLVMBuildAlloca(self.builder, i64_ty, c"idx".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    // Loop: check idx < len, if not goto notfound
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let cmp = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        idx,
                        len_val,
                        c"".as_ptr(),
                    );
                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_body".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cmp, body_bb, notfound_bb);
                    // Body: compare keys[idx] with key
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let stored_key = LLVMBuildLoad2(self.builder, i8_ptr_ty, key_ptr, c"".as_ptr());
                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [stored_key, key].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let is_match = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_match, found_bb, loop_bb);
                    // Before continuing loop, increment idx (in loop_bb predecessor)
                    // Actually we need to increment before branching back. Let me fix this.
                    // Add increment block
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_inc".as_ptr(),
                    );
                    // Fix body to branch to inc instead of loop
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    LLVMMoveBasicBlockAfter(inc_bb, body_bb);
                    // Rebuild body's conditional branch
                    let term = LLVMGetBasicBlockTerminator(body_bb);
                    if !term.is_null() {
                        LLVMInstructionEraseFromParent(term);
                    }
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    LLVMBuildCondBr(self.builder, is_match, found_bb, inc_bb);
                    // Inc block: increment and branch to loop
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    let idx2 = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let next_idx =
                        LLVMBuildAdd(self.builder, idx2, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, next_idx, idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    // Found: return 1
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    LLVMBuildBr(self.builder, done_bb);
                    // Not found: return 0
                    LLVMPositionBuilderAtEnd(self.builder, notfound_bb);
                    LLVMBuildBr(self.builder, done_bb);
                    // Done: phi node for result
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    let phi = LLVMBuildPhi(self.builder, i64_ty, c"result".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let zero = LLVMConstInt(i64_ty, 0, 0);
                    LLVMAddIncoming(phi, [one].as_mut_ptr(), [found_bb].as_mut_ptr(), 1);
                    LLVMAddIncoming(phi, [zero].as_mut_ptr(), [notfound_bb].as_mut_ptr(), 1);
                    Ok(Some(phi))
                }
                "map_string_int_get" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let key = self.codegen_expression(&arguments[1])?;
                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let vals_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, vals_field, c"".as_ptr());
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_loop".as_ptr(),
                    );
                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_body".as_ptr(),
                    );
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_inc".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_found".as_ptr(),
                    );
                    let trap_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_trap".as_ptr(),
                    );
                    let idx_ptr = LLVMBuildAlloca(self.builder, i64_ty, c"idx".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let cmp = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        idx,
                        len_val,
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cmp, body_bb, trap_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let stored_key = LLVMBuildLoad2(self.builder, i8_ptr_ty, key_ptr, c"".as_ptr());
                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [stored_key, key].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let is_match = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_match, found_bb, inc_bb);
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    let idx2 = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let next_idx =
                        LLVMBuildAdd(self.builder, idx2, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, next_idx, idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, trap_bb);
                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    LLVMBuildCall2(
                        self.builder,
                        LLVMGlobalGetValueType(abort_fn),
                        abort_fn,
                        [].as_mut_ptr(),
                        0,
                        c"".as_ptr(),
                    );
                    LLVMBuildUnreachable(self.builder);
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    let found_idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let val_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        vals_ptr,
                        [found_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let val = LLVMBuildLoad2(self.builder, i64_ty, val_ptr, c"".as_ptr());
                    Ok(Some(val))
                }
                "map_string_int_set" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let key = self.codegen_expression(&arguments[1])?;
                    let value = self.codegen_expression(&arguments[2])?;
                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(i64_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let vals_ptr =
                        LLVMBuildLoad2(self.builder, i64_ptr_ty, vals_field, c"".as_ptr());
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_loop".as_ptr(),
                    );
                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_body".as_ptr(),
                    );
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_inc".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_found".as_ptr(),
                    );
                    let append_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_append".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_done".as_ptr(),
                    );
                    let idx_ptr = LLVMBuildAlloca(self.builder, i64_ty, c"idx".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let cmp = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        idx,
                        len_val,
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cmp, body_bb, append_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let stored_key = LLVMBuildLoad2(self.builder, i8_ptr_ty, key_ptr, c"".as_ptr());
                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [stored_key, key].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let is_match = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_match, found_bb, inc_bb);
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    let idx2 = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let next_idx =
                        LLVMBuildAdd(self.builder, idx2, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, next_idx, idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    let found_idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let val_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        vals_ptr,
                        [found_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, val_ptr);
                    LLVMBuildBr(self.builder, done_bb);
                    LLVMPositionBuilderAtEnd(self.builder, append_bb);
                    // Append new key-value pair at len position
                    let append_key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, key, append_key_ptr);
                    let append_val_ptr = LLVMBuildGEP2(
                        self.builder,
                        i64_ty,
                        vals_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, append_val_ptr);
                    let new_len = LLVMBuildAdd(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_len, len_field);
                    LLVMBuildBr(self.builder, done_bb);
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "map_string_int_delete" => {
                    // For v1, delete just sets len to 0 for simplicity (clears the map)
                    // Full delete with element shifting deferred
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let _ = self.codegen_expression(&arguments[1])?; // key - unused in v1
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                _ => Ok(None),
            }
        }
    }

    /// Handle MapStringString intrinsics (v1: simple linear search, no hashing)
    fn codegen_map_string_string_intrinsic(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<Option<LLVMValueRef>> {
        unsafe {
            let i64_ty = LLVMInt64TypeInContext(self.context);
            let i8_ptr_ty = LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);
            let str_ptr_ty = LLVMPointerType(i8_ptr_ty, 0);

            match name {
                "map_string_string_new" => {
                    // Struct: { keys: **i8, values: **i8, len: i64, cap: i64 } = 32 bytes
                    let malloc_fn = *self
                        .functions
                        .get("malloc")
                        .ok_or_else(|| CompilerError::codegen_error("Missing malloc"))?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let map_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 32, 0)].as_mut_ptr(),
                        1,
                        c"map".as_ptr(),
                    );
                    let keys_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 64, 0)].as_mut_ptr(),
                        1,
                        c"keys".as_ptr(),
                    );
                    let vals_ptr = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [LLVMConstInt(i64_ty, 64, 0)].as_mut_ptr(),
                        1,
                        c"vals".as_ptr(),
                    );
                    let keys_typed =
                        LLVMBuildBitCast(self.builder, keys_ptr, str_ptr_ty, c"".as_ptr());
                    let vals_typed =
                        LLVMBuildBitCast(self.builder, vals_ptr, str_ptr_ty, c"".as_ptr());
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, keys_typed, keys_field);
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, vals_typed, vals_field);
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    let cap_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 24, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let cap_field = LLVMBuildBitCast(
                        self.builder,
                        cap_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 8, 0), cap_field);
                    Ok(Some(map_ptr))
                }
                "map_string_string_len" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i64_ty,
                        len_field,
                        c"len".as_ptr(),
                    )))
                }
                "map_string_string_clear" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "map_string_string_free" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let free_fn = *self
                        .functions
                        .get("free")
                        .ok_or_else(|| CompilerError::codegen_error("Missing free"))?;
                    let free_ty = LLVMGlobalGetValueType(free_fn);
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let keys_i8 = LLVMBuildBitCast(self.builder, keys_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [keys_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let vals_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, vals_field, c"".as_ptr());
                    let vals_i8 = LLVMBuildBitCast(self.builder, vals_ptr, i8_ptr_ty, c"".as_ptr());
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [vals_i8].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildCall2(
                        self.builder,
                        free_ty,
                        free_fn,
                        [map_ptr].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "map_string_string_has" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let key = self.codegen_expression(&arguments[1])?;
                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_loop".as_ptr(),
                    );
                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_body".as_ptr(),
                    );
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_inc".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_found".as_ptr(),
                    );
                    let notfound_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_notfound".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"has_done".as_ptr(),
                    );
                    let idx_ptr = LLVMBuildAlloca(self.builder, i64_ty, c"idx".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let cmp = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        idx,
                        len_val,
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cmp, body_bb, notfound_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let stored_key = LLVMBuildLoad2(self.builder, i8_ptr_ty, key_ptr, c"".as_ptr());
                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [stored_key, key].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let is_match = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_match, found_bb, inc_bb);
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    let idx2 = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let next_idx =
                        LLVMBuildAdd(self.builder, idx2, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, next_idx, idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    LLVMBuildBr(self.builder, done_bb);
                    LLVMPositionBuilderAtEnd(self.builder, notfound_bb);
                    LLVMBuildBr(self.builder, done_bb);
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    let phi = LLVMBuildPhi(self.builder, i64_ty, c"result".as_ptr());
                    LLVMAddIncoming(
                        phi,
                        [LLVMConstInt(i64_ty, 1, 0)].as_mut_ptr(),
                        [found_bb].as_mut_ptr(),
                        1,
                    );
                    LLVMAddIncoming(
                        phi,
                        [LLVMConstInt(i64_ty, 0, 0)].as_mut_ptr(),
                        [notfound_bb].as_mut_ptr(),
                        1,
                    );
                    Ok(Some(phi))
                }
                "map_string_string_get" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let key = self.codegen_expression(&arguments[1])?;
                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let vals_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, vals_field, c"".as_ptr());
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_loop".as_ptr(),
                    );
                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_body".as_ptr(),
                    );
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_inc".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_found".as_ptr(),
                    );
                    let trap_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"get_trap".as_ptr(),
                    );
                    let idx_ptr = LLVMBuildAlloca(self.builder, i64_ty, c"idx".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let cmp = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        idx,
                        len_val,
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cmp, body_bb, trap_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let stored_key = LLVMBuildLoad2(self.builder, i8_ptr_ty, key_ptr, c"".as_ptr());
                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [stored_key, key].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let is_match = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_match, found_bb, inc_bb);
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    let idx2 = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let next_idx =
                        LLVMBuildAdd(self.builder, idx2, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, next_idx, idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, trap_bb);
                    let abort_fn = *self
                        .functions
                        .get("abort")
                        .ok_or_else(|| CompilerError::codegen_error("Missing abort"))?;
                    LLVMBuildCall2(
                        self.builder,
                        LLVMGlobalGetValueType(abort_fn),
                        abort_fn,
                        [].as_mut_ptr(),
                        0,
                        c"".as_ptr(),
                    );
                    LLVMBuildUnreachable(self.builder);
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    let found_idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let val_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vals_ptr,
                        [found_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    Ok(Some(LLVMBuildLoad2(
                        self.builder,
                        i8_ptr_ty,
                        val_ptr,
                        c"".as_ptr(),
                    )))
                }
                "map_string_string_set" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let key = self.codegen_expression(&arguments[1])?;
                    let value = self.codegen_expression(&arguments[2])?;
                    let strcmp_fn = *self
                        .functions
                        .get("strcmp")
                        .ok_or_else(|| CompilerError::codegen_error("Missing strcmp"))?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let keys_field = LLVMBuildBitCast(
                        self.builder,
                        map_ptr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let keys_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, keys_field, c"".as_ptr());
                    let vals_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 8, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let vals_field = LLVMBuildBitCast(
                        self.builder,
                        vals_addr,
                        LLVMPointerType(str_ptr_ty, 0),
                        c"".as_ptr(),
                    );
                    let vals_ptr =
                        LLVMBuildLoad2(self.builder, str_ptr_ty, vals_field, c"".as_ptr());
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    let len_val = LLVMBuildLoad2(self.builder, i64_ty, len_field, c"".as_ptr());
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_loop".as_ptr(),
                    );
                    let body_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_body".as_ptr(),
                    );
                    let inc_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_inc".as_ptr(),
                    );
                    let found_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_found".as_ptr(),
                    );
                    let append_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_append".as_ptr(),
                    );
                    let done_bb = LLVMAppendBasicBlockInContext(
                        self.context,
                        current_fn,
                        c"set_done".as_ptr(),
                    );
                    let idx_ptr = LLVMBuildAlloca(self.builder, i64_ty, c"idx".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let cmp = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntSLT,
                        idx,
                        len_val,
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, cmp, body_bb, append_bb);
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let stored_key = LLVMBuildLoad2(self.builder, i8_ptr_ty, key_ptr, c"".as_ptr());
                    let cmp_result = LLVMBuildCall2(
                        self.builder,
                        strcmp_ty,
                        strcmp_fn,
                        [stored_key, key].as_mut_ptr(),
                        2,
                        c"".as_ptr(),
                    );
                    let is_match = LLVMBuildICmp(
                        self.builder,
                        LLVMIntPredicate::LLVMIntEQ,
                        cmp_result,
                        LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildCondBr(self.builder, is_match, found_bb, inc_bb);
                    LLVMPositionBuilderAtEnd(self.builder, inc_bb);
                    let idx2 = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let next_idx =
                        LLVMBuildAdd(self.builder, idx2, LLVMConstInt(i64_ty, 1, 0), c"".as_ptr());
                    LLVMBuildStore(self.builder, next_idx, idx_ptr);
                    LLVMBuildBr(self.builder, loop_bb);
                    LLVMPositionBuilderAtEnd(self.builder, found_bb);
                    let found_idx = LLVMBuildLoad2(self.builder, i64_ty, idx_ptr, c"".as_ptr());
                    let val_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vals_ptr,
                        [found_idx].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, val_ptr);
                    LLVMBuildBr(self.builder, done_bb);
                    LLVMPositionBuilderAtEnd(self.builder, append_bb);
                    let append_key_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        keys_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, key, append_key_ptr);
                    let append_val_ptr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        vals_ptr,
                        [len_val].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, value, append_val_ptr);
                    let new_len = LLVMBuildAdd(
                        self.builder,
                        len_val,
                        LLVMConstInt(i64_ty, 1, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, new_len, len_field);
                    LLVMBuildBr(self.builder, done_bb);
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                "map_string_string_delete" => {
                    let map_ptr = self.codegen_expression(&arguments[0])?;
                    let _ = self.codegen_expression(&arguments[1])?;
                    let len_addr = LLVMBuildGEP2(
                        self.builder,
                        i8_ptr_ty,
                        map_ptr,
                        [LLVMConstInt(i64_ty, 16, 0)].as_mut_ptr(),
                        1,
                        c"".as_ptr(),
                    );
                    let len_field = LLVMBuildBitCast(
                        self.builder,
                        len_addr,
                        LLVMPointerType(i64_ty, 0),
                        c"".as_ptr(),
                    );
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), len_field);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }
                _ => Ok(None),
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
            let function = self
                .current_function
                .ok_or_else(|| CompilerError::codegen_error("No current function for alloca"))?;

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

impl LLVMCodegen {
    /// Emit a null pointer check that traps if the pointer is null
    fn emit_null_check(&mut self, ptr: LLVMValueRef, _msg: &str) -> CompilerResult<()> {
        unsafe {
            let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
            let trap_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"null.trap".as_ptr());
            let ok_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"null.ok".as_ptr());

            let is_null = LLVMBuildICmp(
                self.builder,
                llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                ptr,
                LLVMConstNull(LLVMTypeOf(ptr)),
                c"is.null".as_ptr(),
            );

            LLVMBuildCondBr(self.builder, is_null, trap_bb, ok_bb);

            // Trap block - call abort
            LLVMPositionBuilderAtEnd(self.builder, trap_bb);
            let abort_fn = *self.functions.get("abort").ok_or_else(|| {
                CompilerError::codegen_error("Missing abort")
            })?;
            let abort_ty = LLVMGlobalGetValueType(abort_fn);
            LLVMBuildCall2(self.builder, abort_ty, abort_fn, std::ptr::null_mut(), 0, c"".as_ptr());
            LLVMBuildUnreachable(self.builder);

            // Continue in ok block
            LLVMPositionBuilderAtEnd(self.builder, ok_bb);
            Ok(())
        }
    }

    /// Handle string manipulation intrinsics
    fn codegen_string_intrinsic(
        &mut self,
        name: &str,
        arguments: &[Expression],
    ) -> CompilerResult<Option<LLVMValueRef>> {
        unsafe {
            match name {
                "str_len" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    
                    // Trap on null pointer
                    self.emit_null_check(s, "str_len: null string")?;
                    
                    let strlen_fn = *self.functions.get("strlen").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strlen")
                    })?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let result = LLVMBuildCall2(
                        self.builder,
                        strlen_ty,
                        strlen_fn,
                        [s].as_mut_ptr(),
                        1,
                        c"str.len".as_ptr(),
                    );
                    Ok(Some(result))
                }

                "str_char_at" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let idx = self.codegen_expression(&arguments[1])?;
                    
                    // Trap on null pointer
                    self.emit_null_check(s, "str_char_at: null string")?;
                    
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let byte_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [idx].as_mut_ptr(),
                        1,
                        c"char.ptr".as_ptr(),
                    );
                    let byte_val = LLVMBuildLoad2(self.builder, i8_ty, byte_ptr, c"char".as_ptr());
                    let result = LLVMBuildZExt(self.builder, byte_val, i64_ty, c"char.int".as_ptr());
                    Ok(Some(result))
                }

                "str_slice" => {
                    let s = self.codegen_expression(&arguments[0])?;
                    let start = self.codegen_expression(&arguments[1])?;
                    let end = self.codegen_expression(&arguments[2])?;

                    // Trap on null pointer
                    self.emit_null_check(s, "str_slice: null string")?;

                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let memcpy_fn = *self.functions.get("memcpy").ok_or_else(|| {
                        CompilerError::codegen_error("Missing memcpy")
                    })?;
                    let memcpy_ty = LLVMGlobalGetValueType(memcpy_fn);

                    let len = LLVMBuildSub(self.builder, end, start, c"slice.len".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size = LLVMBuildAdd(self.builder, len, one, c"alloc.size".as_ptr());
                    let new_str = LLVMBuildCall2(
                        self.builder,
                        malloc_ty,
                        malloc_fn,
                        [alloc_size].as_mut_ptr(),
                        1,
                        c"slice.ptr".as_ptr(),
                    );

                    let src_ptr = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        s,
                        [start].as_mut_ptr(),
                        1,
                        c"src.ptr".as_ptr(),
                    );

                    LLVMBuildCall2(
                        self.builder,
                        memcpy_ty,
                        memcpy_fn,
                        [new_str, src_ptr, len].as_mut_ptr(),
                        3,
                        c"".as_ptr(),
                    );

                    let null_pos = LLVMBuildInBoundsGEP2(
                        self.builder,
                        i8_ty,
                        new_str,
                        [len].as_mut_ptr(),
                        1,
                        c"null.pos".as_ptr(),
                    );
                    let zero_byte = LLVMConstInt(i8_ty, 0, 0);
                    LLVMBuildStore(self.builder, zero_byte, null_pos);

                    Ok(Some(new_str))
                }

                "str_concat" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "str_concat: null first string")?;
                    self.emit_null_check(b, "str_concat: null second string")?;

                    let i64_ty = LLVMInt64TypeInContext(self.context);

                    let strlen_fn = *self.functions.get("strlen").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strlen")
                    })?;
                    let strlen_ty = LLVMGlobalGetValueType(strlen_fn);
                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let strcpy_fn = *self.functions.get("strcpy").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strcpy")
                    })?;
                    let strcpy_ty = LLVMGlobalGetValueType(strcpy_fn);
                    let strcat_fn = *self.functions.get("strcat").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strcat")
                    })?;
                    let strcat_ty = LLVMGlobalGetValueType(strcat_fn);

                    let len_a = LLVMBuildCall2(self.builder, strlen_ty, strlen_fn, [a].as_mut_ptr(), 1, c"len.a".as_ptr());
                    let len_b = LLVMBuildCall2(self.builder, strlen_ty, strlen_fn, [b].as_mut_ptr(), 1, c"len.b".as_ptr());

                    let total = LLVMBuildAdd(self.builder, len_a, len_b, c"total.len".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let alloc_size = LLVMBuildAdd(self.builder, total, one, c"alloc.size".as_ptr());

                    let new_str = LLVMBuildCall2(self.builder, malloc_ty, malloc_fn, [alloc_size].as_mut_ptr(), 1, c"concat.ptr".as_ptr());
                    LLVMBuildCall2(self.builder, strcpy_ty, strcpy_fn, [new_str, a].as_mut_ptr(), 2, c"".as_ptr());
                    LLVMBuildCall2(self.builder, strcat_ty, strcat_fn, [new_str, b].as_mut_ptr(), 2, c"".as_ptr());

                    Ok(Some(new_str))
                }

                "str_eq" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "str_eq: null first string")?;
                    self.emit_null_check(b, "str_eq: null second string")?;

                    let strcmp_fn = *self.functions.get("strcmp").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strcmp")
                    })?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let i32_ty = LLVMInt32TypeInContext(self.context);

                    let cmp_result = LLVMBuildCall2(self.builder, strcmp_ty, strcmp_fn, [a, b].as_mut_ptr(), 2, c"strcmp".as_ptr());
                    let zero = LLVMConstInt(i32_ty, 0, 0);
                    let result = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntEQ, cmp_result, zero, c"str.eq".as_ptr());
                    Ok(Some(result))
                }

                "str_ne" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "str_ne: null first string")?;
                    self.emit_null_check(b, "str_ne: null second string")?;

                    let strcmp_fn = *self.functions.get("strcmp").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strcmp")
                    })?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let i32_ty = LLVMInt32TypeInContext(self.context);

                    let cmp_result = LLVMBuildCall2(self.builder, strcmp_ty, strcmp_fn, [a, b].as_mut_ptr(), 2, c"strcmp".as_ptr());
                    let zero = LLVMConstInt(i32_ty, 0, 0);
                    let result = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntNE, cmp_result, zero, c"str.ne".as_ptr());
                    Ok(Some(result))
                }

                "bytes_eq" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;

                    // Trap on null pointers
                    self.emit_null_check(a, "bytes_eq: null first bytes")?;
                    self.emit_null_check(b, "bytes_eq: null second bytes")?;

                    let strcmp_fn = *self.functions.get("strcmp").ok_or_else(|| {
                        CompilerError::codegen_error("Missing strcmp")
                    })?;
                    let strcmp_ty = LLVMGlobalGetValueType(strcmp_fn);
                    let i32_ty = LLVMInt32TypeInContext(self.context);

                    let cmp_result = LLVMBuildCall2(self.builder, strcmp_ty, strcmp_fn, [a, b].as_mut_ptr(), 2, c"strcmp".as_ptr());
                    let zero = LLVMConstInt(i32_ty, 0, 0);
                    let result = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntEQ, cmp_result, zero, c"bytes.eq".as_ptr());
                    Ok(Some(result))
                }

                // ============================================================
                // Math Stdlib: math_sqrt, math_pow, math_abs, etc.
                // ============================================================

                "math_sqrt" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let sqrt_fn = *self.functions.get("sqrt").ok_or_else(|| {
                        CompilerError::codegen_error("Missing sqrt")
                    })?;
                    let sqrt_ty = LLVMGlobalGetValueType(sqrt_fn);
                    let result = LLVMBuildCall2(self.builder, sqrt_ty, sqrt_fn, [x].as_mut_ptr(), 1, c"math_sqrt".as_ptr());
                    Ok(Some(result))
                }

                "math_pow" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let y = self.codegen_expression(&arguments[1])?;
                    let pow_fn = *self.functions.get("pow").ok_or_else(|| {
                        CompilerError::codegen_error("Missing pow")
                    })?;
                    let pow_ty = LLVMGlobalGetValueType(pow_fn);
                    let result = LLVMBuildCall2(self.builder, pow_ty, pow_fn, [x, y].as_mut_ptr(), 2, c"math_pow".as_ptr());
                    Ok(Some(result))
                }

                "math_abs" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let abs_fn = *self.functions.get("abs").ok_or_else(|| {
                        CompilerError::codegen_error("Missing abs")
                    })?;
                    let abs_ty = LLVMGlobalGetValueType(abs_fn);
                    let result = LLVMBuildCall2(self.builder, abs_ty, abs_fn, [x].as_mut_ptr(), 1, c"math_abs".as_ptr());
                    Ok(Some(result))
                }

                "math_floor" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let floor_fn = *self.functions.get("floor").ok_or_else(|| {
                        CompilerError::codegen_error("Missing floor")
                    })?;
                    let floor_ty = LLVMGlobalGetValueType(floor_fn);
                    let result = LLVMBuildCall2(self.builder, floor_ty, floor_fn, [x].as_mut_ptr(), 1, c"math_floor".as_ptr());
                    Ok(Some(result))
                }

                "math_ceil" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let ceil_fn = *self.functions.get("ceil").ok_or_else(|| {
                        CompilerError::codegen_error("Missing ceil")
                    })?;
                    let ceil_ty = LLVMGlobalGetValueType(ceil_fn);
                    let result = LLVMBuildCall2(self.builder, ceil_ty, ceil_fn, [x].as_mut_ptr(), 1, c"math_ceil".as_ptr());
                    Ok(Some(result))
                }

                "math_round" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let round_fn = *self.functions.get("round").ok_or_else(|| {
                        CompilerError::codegen_error("Missing round")
                    })?;
                    let round_ty = LLVMGlobalGetValueType(round_fn);
                    let result = LLVMBuildCall2(self.builder, round_ty, round_fn, [x].as_mut_ptr(), 1, c"math_round".as_ptr());
                    Ok(Some(result))
                }

                "math_sin" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let sin_fn = *self.functions.get("sin").ok_or_else(|| {
                        CompilerError::codegen_error("Missing sin")
                    })?;
                    let sin_ty = LLVMGlobalGetValueType(sin_fn);
                    let result = LLVMBuildCall2(self.builder, sin_ty, sin_fn, [x].as_mut_ptr(), 1, c"math_sin".as_ptr());
                    Ok(Some(result))
                }

                "math_cos" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let cos_fn = *self.functions.get("cos").ok_or_else(|| {
                        CompilerError::codegen_error("Missing cos")
                    })?;
                    let cos_ty = LLVMGlobalGetValueType(cos_fn);
                    let result = LLVMBuildCall2(self.builder, cos_ty, cos_fn, [x].as_mut_ptr(), 1, c"math_cos".as_ptr());
                    Ok(Some(result))
                }

                "math_tan" => {
                    let x = self.codegen_expression(&arguments[0])?;
                    let tan_fn = *self.functions.get("tan").ok_or_else(|| {
                        CompilerError::codegen_error("Missing tan")
                    })?;
                    let tan_ty = LLVMGlobalGetValueType(tan_fn);
                    let result = LLVMBuildCall2(self.builder, tan_ty, tan_fn, [x].as_mut_ptr(), 1, c"math_tan".as_ptr());
                    Ok(Some(result))
                }

                "math_min" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;
                    // min(a, b) = a < b ? a : b
                    let cond = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSLT, a, b, c"min.cmp".as_ptr());
                    let result = LLVMBuildSelect(self.builder, cond, a, b, c"math_min".as_ptr());
                    Ok(Some(result))
                }

                "math_max" => {
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;
                    // max(a, b) = a > b ? a : b
                    let cond = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSGT, a, b, c"max.cmp".as_ptr());
                    let result = LLVMBuildSelect(self.builder, cond, a, b, c"math_max".as_ptr());
                    Ok(Some(result))
                }

                // ============================================================
                // Random Stdlib: rand_int, rand_float, rand_seed
                // ============================================================

                "rand_seed" => {
                    let seed = self.codegen_expression(&arguments[0])?;
                    let srand_fn = *self.functions.get("srand").ok_or_else(|| {
                        CompilerError::codegen_error("Missing srand")
                    })?;
                    let srand_ty = LLVMGlobalGetValueType(srand_fn);
                    LLVMBuildCall2(self.builder, srand_ty, srand_fn, [seed].as_mut_ptr(), 1, c"".as_ptr());
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "rand_int" => {
                    let min = self.codegen_expression(&arguments[0])?;
                    let max = self.codegen_expression(&arguments[1])?;
                    let rand_fn = *self.functions.get("rand").ok_or_else(|| {
                        CompilerError::codegen_error("Missing rand")
                    })?;
                    let rand_ty = LLVMGlobalGetValueType(rand_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    
                    // rand() returns int, convert to i64
                    let rand_val = LLVMBuildCall2(self.builder, rand_ty, rand_fn, [].as_mut_ptr(), 0, c"rand".as_ptr());
                    let rand_i64 = LLVMBuildSExt(self.builder, rand_val, i64_ty, c"rand.ext".as_ptr());
                    
                    // result = min + (rand % (max - min + 1))
                    let range = LLVMBuildSub(self.builder, max, min, c"range.sub".as_ptr());
                    let one = LLVMConstInt(i64_ty, 1, 0);
                    let range_plus_one = LLVMBuildAdd(self.builder, range, one, c"range.add".as_ptr());
                    let mod_val = LLVMBuildSRem(self.builder, rand_i64, range_plus_one, c"rand.mod".as_ptr());
                    let result = LLVMBuildAdd(self.builder, min, mod_val, c"rand_int".as_ptr());
                    Ok(Some(result))
                }

                "rand_float" => {
                    let rand_fn = *self.functions.get("rand").ok_or_else(|| {
                        CompilerError::codegen_error("Missing rand")
                    })?;
                    let rand_ty = LLVMGlobalGetValueType(rand_fn);
                    let f64_ty = LLVMDoubleTypeInContext(self.context);
                    
                    // rand() / RAND_MAX -> 0.0 to 1.0
                    let rand_val = LLVMBuildCall2(self.builder, rand_ty, rand_fn, [].as_mut_ptr(), 0, c"rand".as_ptr());
                    let rand_f64 = LLVMBuildSIToFP(self.builder, rand_val, f64_ty, c"rand.fp".as_ptr());
                    let rand_max = LLVMConstReal(f64_ty, 2147483647.0); // RAND_MAX
                    let result = LLVMBuildFDiv(self.builder, rand_f64, rand_max, c"rand_float".as_ptr());
                    Ok(Some(result))
                }

                "rand_bytes" => {
                    // Allocate n bytes and fill with random data
                    let n = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let i8_ty = LLVMInt8TypeInContext(self.context);
                    let _i8_ptr_ty = LLVMPointerType(i8_ty, 0);
                    
                    // Allocate buffer
                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let buf = LLVMBuildCall2(self.builder, malloc_ty, malloc_fn, [n].as_mut_ptr(), 1, c"rand_buf".as_ptr());
                    
                    // Get rand function
                    let rand_fn = *self.functions.get("rand").ok_or_else(|| {
                        CompilerError::codegen_error("Missing rand")
                    })?;
                    let rand_ty = LLVMGlobalGetValueType(rand_fn);
                    
                    // Create loop to fill buffer
                    let current_fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder));
                    let loop_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"rand.loop".as_ptr());
                    let done_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"rand.done".as_ptr());
                    
                    // Alloca for loop counter
                    let counter = LLVMBuildAlloca(self.builder, i64_ty, c"i".as_ptr());
                    LLVMBuildStore(self.builder, LLVMConstInt(i64_ty, 0, 0), counter);
                    LLVMBuildBr(self.builder, loop_bb);
                    
                    // Loop block
                    LLVMPositionBuilderAtEnd(self.builder, loop_bb);
                    let i = LLVMBuildLoad2(self.builder, i64_ty, counter, c"i.val".as_ptr());
                    let cond = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntSLT, i, n, c"cond".as_ptr());
                    
                    let body_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"rand.body".as_ptr());
                    LLVMBuildCondBr(self.builder, cond, body_bb, done_bb);
                    
                    // Body block
                    LLVMPositionBuilderAtEnd(self.builder, body_bb);
                    let rand_val = LLVMBuildCall2(self.builder, rand_ty, rand_fn, [].as_mut_ptr(), 0, c"rand".as_ptr());
                    let rand_byte = LLVMBuildTrunc(self.builder, rand_val, i8_ty, c"byte".as_ptr());
                    let ptr = LLVMBuildGEP2(self.builder, i8_ty, buf, [i].as_mut_ptr(), 1, c"ptr".as_ptr());
                    LLVMBuildStore(self.builder, rand_byte, ptr);
                    
                    // Increment counter
                    let next_i = LLVMBuildAdd(self.builder, i, LLVMConstInt(i64_ty, 1, 0), c"next_i".as_ptr());
                    LLVMBuildStore(self.builder, next_i, counter);
                    LLVMBuildBr(self.builder, loop_bb);
                    
                    // Done block
                    LLVMPositionBuilderAtEnd(self.builder, done_bb);
                    
                    Ok(Some(buf))
                }

                // ============================================================
                // Log Stdlib: log_debug, log_info, log_warn, log_error
                // ============================================================

                "log_debug" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[DEBUG] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(self.builder, prefix.as_ptr(), c"log.prefix".as_ptr());
                    
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, prefix_ptr, msg].as_mut_ptr(), 3, c"".as_ptr());
                    
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_info" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[INFO] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(self.builder, prefix.as_ptr(), c"log.prefix".as_ptr());
                    
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, prefix_ptr, msg].as_mut_ptr(), 3, c"".as_ptr());
                    
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_warn" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[WARN] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(self.builder, prefix.as_ptr(), c"log.prefix".as_ptr());
                    
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, prefix_ptr, msg].as_mut_ptr(), 3, c"".as_ptr());
                    
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_error" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let prefix = CString::new("[ERROR] ").expect("CString failed");
                    let prefix_ptr = LLVMBuildGlobalStringPtr(self.builder, prefix.as_ptr(), c"log.prefix".as_ptr());
                    
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("%s%s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"log.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, prefix_ptr, msg].as_mut_ptr(), 3, c"".as_ptr());
                    
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "log_set_level" => {
                    // Log levels: 0=DEBUG, 1=INFO, 2=WARN, 3=ERROR, 4=OFF
                    // For now, just accept the level but don't filter (no-op)
                    // Full implementation would require global state
                    let _level = self.codegen_expression(&arguments[0])?;
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Format Stdlib: fmt_int, fmt_float, fmt_bool, fmt_hex
                // ============================================================

                "fmt_int" => {
                    let n = self.codegen_expression(&arguments[0])?;
                    
                    // Allocate buffer and use sprintf
                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let buf_size = LLVMConstInt(i64_ty, 32, 0);
                    let buf = LLVMBuildCall2(self.builder, malloc_ty, malloc_fn, [buf_size].as_mut_ptr(), 1, c"fmt.buf".as_ptr());
                    
                    let sprintf_fn = *self.functions.get("sprintf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing sprintf")
                    })?;
                    let sprintf_ty = LLVMGlobalGetValueType(sprintf_fn);
                    let fmt = CString::new("%ld").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"fmt.int".as_ptr());
                    LLVMBuildCall2(self.builder, sprintf_ty, sprintf_fn, [buf, fmt_ptr, n].as_mut_ptr(), 3, c"".as_ptr());
                    
                    Ok(Some(buf))
                }

                "fmt_hex" => {
                    let n = self.codegen_expression(&arguments[0])?;
                    
                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let buf_size = LLVMConstInt(i64_ty, 32, 0);
                    let buf = LLVMBuildCall2(self.builder, malloc_ty, malloc_fn, [buf_size].as_mut_ptr(), 1, c"fmt.buf".as_ptr());
                    
                    let sprintf_fn = *self.functions.get("sprintf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing sprintf")
                    })?;
                    let sprintf_ty = LLVMGlobalGetValueType(sprintf_fn);
                    let fmt = CString::new("0x%lx").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"fmt.hex".as_ptr());
                    LLVMBuildCall2(self.builder, sprintf_ty, sprintf_fn, [buf, fmt_ptr, n].as_mut_ptr(), 3, c"".as_ptr());
                    
                    Ok(Some(buf))
                }

                "fmt_bool" => {
                    let b = self.codegen_expression(&arguments[0])?;
                    let _i1_ty = LLVMInt1TypeInContext(self.context);
                    
                    // Convert to i1 if needed
                    let cond = if LLVMGetTypeKind(LLVMTypeOf(b)) == llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind 
                        && LLVMGetIntTypeWidth(LLVMTypeOf(b)) != 1 {
                        let zero = LLVMConstInt(LLVMTypeOf(b), 0, 0);
                        LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntNE, b, zero, c"fmt.cond".as_ptr())
                    } else {
                        b
                    };
                    
                    let true_str = CString::new("true").expect("CString failed");
                    let false_str = CString::new("false").expect("CString failed");
                    let true_ptr = LLVMBuildGlobalStringPtr(self.builder, true_str.as_ptr(), c"fmt.true".as_ptr());
                    let false_ptr = LLVMBuildGlobalStringPtr(self.builder, false_str.as_ptr(), c"fmt.false".as_ptr());
                    
                    let result = LLVMBuildSelect(self.builder, cond, true_ptr, false_ptr, c"fmt_bool".as_ptr());
                    Ok(Some(result))
                }

                "fmt_float" => {
                    let f = self.codegen_expression(&arguments[0])?;
                    let precision = self.codegen_expression(&arguments[1])?;
                    
                    let malloc_fn = *self.functions.get("malloc").ok_or_else(|| {
                        CompilerError::codegen_error("Missing malloc")
                    })?;
                    let malloc_ty = LLVMGlobalGetValueType(malloc_fn);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    let buf_size = LLVMConstInt(i64_ty, 64, 0);
                    let buf = LLVMBuildCall2(self.builder, malloc_ty, malloc_fn, [buf_size].as_mut_ptr(), 1, c"fmt.buf".as_ptr());
                    
                    let sprintf_fn = *self.functions.get("sprintf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing sprintf")
                    })?;
                    let sprintf_ty = LLVMGlobalGetValueType(sprintf_fn);
                    let fmt = CString::new("%.*f").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"fmt.float".as_ptr());
                    
                    // Truncate precision to i32 for printf
                    let i32_ty = LLVMInt32TypeInContext(self.context);
                    let prec_i32 = LLVMBuildTrunc(self.builder, precision, i32_ty, c"prec.trunc".as_ptr());
                    LLVMBuildCall2(self.builder, sprintf_ty, sprintf_fn, [buf, fmt_ptr, prec_i32, f].as_mut_ptr(), 4, c"".as_ptr());
                    
                    Ok(Some(buf))
                }

                // ============================================================
                // Test Framework: assert, assert_eq, assert_ne
                // ============================================================

                "assert" => {
                    // assert(cond) - abort with message if condition is false
                    let cond = self.codegen_expression(&arguments[0])?;
                    let i1_ty = LLVMInt1TypeInContext(self.context);
                    
                    // Convert to i1 if needed (non-zero = true)
                    let cond_i1 = if LLVMGetTypeKind(LLVMTypeOf(cond)) == llvm_sys::LLVMTypeKind::LLVMIntegerTypeKind 
                        && LLVMGetIntTypeWidth(LLVMTypeOf(cond)) != 1 {
                        let zero = LLVMConstInt(LLVMTypeOf(cond), 0, 0);
                        LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntNE, cond, zero, c"assert.cond".as_ptr())
                    } else {
                        cond
                    };

                    // Create blocks for pass/fail
                    let current_fn = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("assert: no current function")
                    })?;
                    let pass_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"assert.pass".as_ptr());
                    let fail_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"assert.fail".as_ptr());

                    LLVMBuildCondBr(self.builder, cond_i1, pass_bb, fail_bb);

                    // Fail block: print message and abort
                    LLVMPositionBuilderAtEnd(self.builder, fail_bb);
                    let msg_str = CString::new("Assertion failed").expect("CString failed");
                    let msg = LLVMBuildGlobalStringPtr(self.builder, msg_str.as_ptr(), c"assert.msg".as_ptr());
                    let puts_fn = *self.functions.get("puts").ok_or_else(|| {
                        CompilerError::codegen_error("Missing puts")
                    })?;
                    let puts_ty = LLVMGlobalGetValueType(puts_fn);
                    LLVMBuildCall2(self.builder, puts_ty, puts_fn, [msg].as_mut_ptr(), 1, c"".as_ptr());
                    
                    let abort_fn = *self.functions.get("abort").ok_or_else(|| {
                        CompilerError::codegen_error("Missing abort")
                    })?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(self.builder, abort_ty, abort_fn, [].as_mut_ptr(), 0, c"".as_ptr());
                    LLVMBuildUnreachable(self.builder);

                    // Pass block: continue execution
                    LLVMPositionBuilderAtEnd(self.builder, pass_bb);

                    // Return void (0)
                    Ok(Some(LLVMConstInt(i1_ty, 0, 0)))
                }

                "assert_eq" => {
                    // assert_eq(a, b) - abort if a != b
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;
                    let i1_ty = LLVMInt1TypeInContext(self.context);

                    // Compare for equality
                    let cond = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntEQ, a, b, c"assert_eq.cmp".as_ptr());

                    // Create blocks for pass/fail
                    let current_fn = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("assert_eq: no current function")
                    })?;
                    let pass_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"assert_eq.pass".as_ptr());
                    let fail_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"assert_eq.fail".as_ptr());

                    LLVMBuildCondBr(self.builder, cond, pass_bb, fail_bb);

                    // Fail block: print message and abort
                    LLVMPositionBuilderAtEnd(self.builder, fail_bb);
                    let msg_str = CString::new("Assertion failed: values not equal").expect("CString failed");
                    let msg = LLVMBuildGlobalStringPtr(self.builder, msg_str.as_ptr(), c"assert_eq.msg".as_ptr());
                    let puts_fn = *self.functions.get("puts").ok_or_else(|| {
                        CompilerError::codegen_error("Missing puts")
                    })?;
                    let puts_ty = LLVMGlobalGetValueType(puts_fn);
                    LLVMBuildCall2(self.builder, puts_ty, puts_fn, [msg].as_mut_ptr(), 1, c"".as_ptr());
                    
                    let abort_fn = *self.functions.get("abort").ok_or_else(|| {
                        CompilerError::codegen_error("Missing abort")
                    })?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(self.builder, abort_ty, abort_fn, [].as_mut_ptr(), 0, c"".as_ptr());
                    LLVMBuildUnreachable(self.builder);

                    // Pass block: continue execution
                    LLVMPositionBuilderAtEnd(self.builder, pass_bb);

                    // Return void (0)
                    Ok(Some(LLVMConstInt(i1_ty, 0, 0)))
                }

                "assert_ne" => {
                    // assert_ne(a, b) - abort if a == b
                    let a = self.codegen_expression(&arguments[0])?;
                    let b = self.codegen_expression(&arguments[1])?;
                    let i1_ty = LLVMInt1TypeInContext(self.context);

                    // Compare for inequality
                    let cond = LLVMBuildICmp(self.builder, llvm_sys::LLVMIntPredicate::LLVMIntNE, a, b, c"assert_ne.cmp".as_ptr());

                    // Create blocks for pass/fail
                    let current_fn = self.current_function.ok_or_else(|| {
                        CompilerError::codegen_error("assert_ne: no current function")
                    })?;
                    let pass_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"assert_ne.pass".as_ptr());
                    let fail_bb = LLVMAppendBasicBlockInContext(self.context, current_fn, c"assert_ne.fail".as_ptr());

                    LLVMBuildCondBr(self.builder, cond, pass_bb, fail_bb);

                    // Fail block: print message and abort
                    LLVMPositionBuilderAtEnd(self.builder, fail_bb);
                    let msg_str = CString::new("Assertion failed: values are equal").expect("CString failed");
                    let msg = LLVMBuildGlobalStringPtr(self.builder, msg_str.as_ptr(), c"assert_ne.msg".as_ptr());
                    let puts_fn = *self.functions.get("puts").ok_or_else(|| {
                        CompilerError::codegen_error("Missing puts")
                    })?;
                    let puts_ty = LLVMGlobalGetValueType(puts_fn);
                    LLVMBuildCall2(self.builder, puts_ty, puts_fn, [msg].as_mut_ptr(), 1, c"".as_ptr());
                    
                    let abort_fn = *self.functions.get("abort").ok_or_else(|| {
                        CompilerError::codegen_error("Missing abort")
                    })?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(self.builder, abort_ty, abort_fn, [].as_mut_ptr(), 0, c"".as_ptr());
                    LLVMBuildUnreachable(self.builder);

                    // Pass block: continue execution
                    LLVMPositionBuilderAtEnd(self.builder, pass_bb);

                    // Return void (0)
                    Ok(Some(LLVMConstInt(i1_ty, 0, 0)))
                }

                // ============================================================
                // Test Harness Helpers
                // ============================================================

                "test_pass" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[32m[PASS]\x1b[0m %s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"test.pass.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, msg].as_mut_ptr(), 2, c"".as_ptr());
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "test_fail" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[31m[FAIL]\x1b[0m %s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"test.fail.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, msg].as_mut_ptr(), 2, c"".as_ptr());
                    let abort_fn = *self.functions.get("abort").ok_or_else(|| {
                        CompilerError::codegen_error("Missing abort")
                    })?;
                    let abort_ty = LLVMGlobalGetValueType(abort_fn);
                    LLVMBuildCall2(self.builder, abort_ty, abort_fn, [].as_mut_ptr(), 0, c"".as_ptr());
                    LLVMBuildUnreachable(self.builder);
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "test_skip" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[33m[SKIP]\x1b[0m %s\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"test.skip.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, msg].as_mut_ptr(), 2, c"".as_ptr());
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "test_section" => {
                    let msg = self.codegen_expression(&arguments[0])?;
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\n\x1b[1m=== %s ===\x1b[0m\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"test.section.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, msg].as_mut_ptr(), 2, c"".as_ptr());
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                // ============================================================
                // Runtime Benchmark Helpers
                // ============================================================

                "bench_start" => {
                    // Placeholder: returns 0 (actual timing would use clock_gettime)
                    // This stub allows the API to work while full timing is deferred
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                "bench_end" => {
                    // Prints benchmark info (timing is placeholder)
                    let _start_time = self.codegen_expression(&arguments[0])?;
                    let name = self.codegen_expression(&arguments[1])?;
                    let iterations = self.codegen_expression(&arguments[2])?;
                    
                    let i64_ty = LLVMInt64TypeInContext(self.context);
                    
                    // Print result (timing placeholder shows 0 for now)
                    let printf_fn = *self.functions.get("printf").ok_or_else(|| {
                        CompilerError::codegen_error("Missing printf")
                    })?;
                    let printf_ty = LLVMGlobalGetValueType(printf_fn);
                    let fmt = CString::new("\x1b[36m[BENCH]\x1b[0m %s: completed (%ld iterations)\n").expect("CString failed");
                    let fmt_ptr = LLVMBuildGlobalStringPtr(self.builder, fmt.as_ptr(), c"bench.fmt".as_ptr());
                    LLVMBuildCall2(self.builder, printf_ty, printf_fn, [fmt_ptr, name, iterations].as_mut_ptr(), 3, c"".as_ptr());
                    
                    Ok(Some(LLVMConstInt(i64_ty, 0, 0)))
                }

                _ => Ok(None),
            }
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

        let int_type = codegen.get_llvm_type(&Type::Int);
        let float_type = codegen.get_llvm_type(&Type::Float);
        let bool_type = codegen.get_llvm_type(&Type::Bool);
        let void_type = codegen.get_llvm_type(&Type::Void);

        assert!(!int_type.is_null());
        assert!(!float_type.is_null());
        assert!(!bool_type.is_null());
        assert!(!void_type.is_null());
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
